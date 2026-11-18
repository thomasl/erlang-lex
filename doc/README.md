# Erlang-Lex

A table-driven lexer generator for Erlang. You give a list of rules
(regular expression → action); `lex` builds a deterministic finite
automaton (DFA) from them at run time and uses it to split input into
tokens, choosing the longest match like Unix `lex`/`flex`.

The automaton is built with the classic algorithms of the "Dragon book"
(see [References](#references)): regular expressions are translated
to an NFA, the NFA to a DFA by the subset construction, the DFA is
minimized, and the result is stored as a transition table indexed by
state and input byte.

The lexer reads **bytes**. Besides plain bytes it handles text in
**UTF-8, UTF-16 (LE/BE), UTF-32 (LE/BE)** and **Latin-1**: regexps are
then written in Unicode code points and can use **named Unicode classes**
in Perl syntax (`\p{L}`, `\p{Greek}`, `\p{XID_Start}`, ...), **POSIX
classes** (`[:alpha:]`) and **case-insensitive matching** (`(?i)`).
Malformed or truncated input is reported, never silently accepted.

Original author: Thomas Lindgren (2004-2005, 2021). Unicode support,
case-insensitive matching, POSIX classes and DFA minimization: 2026.

Contents:

- [Quick start](#quick-start)
- [Concepts](#concepts)
- [Encodings](#encodings)
- [API](#api)
- [Errors](#errors)
- [Examples](#examples)
- [Yecc compatibility](#yecc-compatibility)
- [How it works](#how-it-works)
- [Performance](#performance)
- [Lexing corner cases](#lexing-corner-cases)
- [Unicode data](#unicode-data)
- [Testing](#testing)
- [References](#references)
- [Limitations and future work](#limitations-and-future-work)

The regular expression syntax is described in a separate document:
**[syntax.md](syntax.md)**.

## Quick start

```sh
make          # compiles src/*.erl into ebin/
make test     # runs the test suites
erl -pa ebin
```

```erlang
1> Rules = [{regexp, "\\p{XID_Start}\\p{XID_Continue}*", lex:item(ident)},
            {regexp, "\\d+", lex:item(num)},
            {regexp, "\\s+", lex:whitespace()}].
2> lex:with(Rules, <<"λx 42 中文"/utf8>>, [{encoding, utf8}]).
[{ident,1,"λx"},{num,1,"42"},{ident,1,"中文"}]
3> lex:with([{regexp, "[a-z]+", lex:item(word)},
             {regexp, " ", lex:whitespace()}], "hello world").
[{word,1,"hello"},{word,1,"world"}]
```

(Start `erl` with `+pc unicode` to see `"λx"` rather than `[955,120]`.)

## Concepts

**Rules.** A specification is a list of rules
`{regexp, Regexp, Action}` or `{regexp, Priority, Regexp, Action}`; see
[syntax.md](syntax.md#1-rules) for the details.

**Longest match.** At each position the lexer finds the longest prefix
of the remaining input that some rule matches. If several rules match
that longest prefix, the highest `Priority` wins (default 0), then the
earliest rule. So `"iffy"` is an identifier even if `"if"` is a keyword,
and `"if"` is a keyword if the keyword rule comes first or has a
higher priority.

**Actions.** When a token is found, its rule's action is called with
the token text reversed and returns `{token, T}` (emit `T`) or
`no_token` (skip it, e.g. white space). `lex` has
[helpers](#token-helpers) for the common cases.

**Tables.** `lex:regexps_to_table/1,2` compiles the rules into a lex
table, an Erlang term that you can keep and reuse; `lex:with/2,3` does
both steps at once. Building a table takes milliseconds (see
[Performance](#performance)), so it is fine to build it at start-up,
or dynamically: you can extend a lexer by adding rules and rebuilding
it.

**No match.** If no rule matches at some position, lexing stops with an
exception (see [Errors](#errors)). There is no default rule that skips
characters as in `lex`/`flex`; if you want one, add a catch-all such
as `{regexp, ".", Action}` at the end of the list (an earlier rule wins
a tie of the same length).

## Encodings

The option `{encoding, Enc}` decides how regexps and input are read:

| `Enc` | Regexps | Input | Tokens (action argument) |
|---|---|---|---|
| `bytes` (default) | bytes; non-ASCII characters in a regexp are converted to their UTF-8 bytes | list of 0-255 or binary | bytes |
| `latin1` | code points 0-255 | Latin-1 bytes | code points |
| `utf8` | code points | UTF-8 ([RFC 3629]) | code points |
| `utf16be`, `utf16le` | code points | UTF-16 ([RFC 2781]), big or little endian | code points |
| `utf32be`, `utf32le` | code points | UTF-32, big or little endian | code points |

Aliases: `unicode` = `utf8`; `utf16` = `{utf16, big}` = `utf16be`;
`{utf16, little}` = `utf16le`; likewise for `utf32`. These are the
names Erlang's `unicode` module uses.

`bytes` is how the lexer always worked, and is kept as the default so
existing specifications behave exactly as before. In every other
encoding, regexps are strings of code points and may use the Unicode
features of [syntax.md](syntax.md#8-bytes-mode-versus-the-unicode-encodings).

**Input** is always bytes: a binary, or a list of integers 0-255. To lex
an Erlang string (a list of code points), encode it first, e.g.
`unicode:characters_to_binary(Str)` for UTF-8. Passing the string itself
is an error (`not_a_byte`) as soon as it contains a code point above 255.

**Byte order marks** are not interpreted: U+FEFF is an ordinary
character. Choose `utf16le`/`utf16be` yourself, and skip a BOM either by
stripping it or with a rule (see [example 5](#5-a-utf-16le-file-with-a-byte-order-mark)).

## API

All functions are in module `lex` unless stated otherwise.

### Lexing

```erlang
with(Rules, Input) -> [Token]
with(Rules, Input, Options) -> [Token]
```

Build a table from `Rules` and lex all of `Input` (a binary or a list).
Resets the line count to 1 first. Exits on errors (see [Errors](#errors)).

```erlang
regexps_to_table(Rules) -> LexTable
regexps_to_table(Rules, Options) -> LexTable
```

Compile the rules. `Options` is a list of:

- `{encoding, Enc}`: see [Encodings](#encodings); default `bytes`
- `case_insensitive` or `{case_insensitive, Bool}`: make all rules
  case-insensitive, as if each started with `(?i)`; default `false`
  (see [syntax.md](syntax.md#7-case-insensitive-matching))

Errors in the rules exit here, see [Errors](#errors). The table is
`{lex, Start, AcceptLimit, Table, Actions, Enc}`; treat it as opaque.

```erlang
one(LexTable, Input) -> {token, Token, Rest} | end_of_stream
```

Return the next token and the rest of the input (as a list of bytes),
or `end_of_stream` when only skipped text (`no_token`) remains. This is
the interface for a parser that pulls tokens one at a time
([example 7](#7-pulling-tokens-one-at-a-time)). `one/2` does not reset
the line count: call `no_lines()` before lexing a new input.

```erlang
o_with(Rules, String) -> [Token]
o_longest(LexTable, String) -> [Token]
```

The older, non-incremental driver, kept for compatibility (`String` must
be a list). Equivalent to `with/2`; `o_longest/2` also resets the line
count.

### Token helpers

Functions that return ready-made actions. `Line` is `current_line()` at
the time of the match, `Text` the token text (not reversed).

| Helper | Emits |
|---|---|
| `item(Key)` | `{Key, Line, Text}` |
| `token(Key)` | `{Key, Line}` |
| `raw_token(Key)` | `Key` |
| `op(Key)` | `{op, Line, Key}` |
| `enclosed_token(Char, Tag)` | `{Tag, Text}` without the first and last character, which must both be `Char` |
| `enclosed_token(Start, End, Tag)` | as above, with different start and end characters |
| `string()` | `{string, Line, Text}` without the enclosing `"`; counts the newlines inside (escape sequences are not processed) |
| `quoted_atom()` | `{atom, Line, Text}` without the enclosing `'`; counts the newlines inside |
| `no_token()`, `whitespace()`, `comment()` | nothing |
| `newline()` | nothing; increments the line count |

### Line counting

The line count is kept in the process dictionary, so it is per process
and not safe to share between concurrent lexers in one process.

| Function | |
|---|---|
| `no_lines()` | reset the line count to 1 |
| `inc_lines()`, `inc_lines(N)` | add 1 or `N` |
| `current_line()` | the current line |
| `count_lines(Text)` | the number of `$\n` in `Text` |

Lines are counted by your actions, typically with a rule
`{regexp, "\n", lex:newline()}`, or `lex:inc_lines(lex:count_lines(...))`
for tokens that span lines. Only `\n` counts: CR, U+2028 LINE SEPARATOR
etc. are not lines unless your rules make them so.

### Other utilities

| Function | |
|---|---|
| `safe_literal(String)` | escape the regexp special characters in `String`, so `lex:safe_literal("a+b")` matches the text `a+b` |
| `strip_c_comments(String)` | remove (nestable) `/* ... */` comments; a pre-pass for input that is hard to describe with regexps |
| `resolve_line(String)` | replace `?LINE` with the line number |

### Building a table step by step

`regexps_to_table/2` is the composition of these steps, which are
exported for experiments and debugging. The rules are first converted to
an internal form (`{regexp, Id, Tree, Action, Prio}`, made by
`regexps_to_internal/2`, available since the modules are compiled with
`export_all`).

```erlang
regexps_to_nfa(InternalRules) -> NFA
nfa_to_dfa(NFA) -> DFA
minimize_dfa(DFA) -> DFA
dfa_to_table(DFA, InternalRules) -> LexTable          % bytes
dfa_to_table(DFA, InternalRules, Enc) -> LexTable
```

### Unicode functions (`lex_unicode`)

Useful on their own, e.g. to inspect a class. A code point set is a
sorted list of disjoint `{From, To}` ranges.

| Function | |
|---|---|
| `property(Name)` | `{ok, Set}` or `{error, Reason}` for a `\p{Name}` name, e.g. `property("Greek")`, `property("gc=Lu")`, `property("^L")` |
| `posix_class(Name, unicode \| ascii)` | `{ok, Set}` for a POSIX class name |
| `case_closure(Set)` | `Set` plus all simple case folding equivalents |
| `ascii_case_closure(Set)` | `Set` plus ASCII case equivalents |
| `union/2`, `intersect/2`, `subtract/2`, `complement/1`, `normalize/1` | set operations (complement is within 0-10FFFF) |
| `scalar_values()` | all code points except surrogates |
| `byte_seqs(Enc, Set)` | the byte-range sequences encoding `Set`, see [How it works](#how-it-works) |
| `normalize_encoding(Enc)` | the canonical form of an encoding name |
| `decode_prefix(Enc, Bytes)` | `{CodePoints, Rest}`: decode as far as the bytes are well-formed |
| `first_error(Enc, Bytes, StopAt)` | the first malformed sequence, if it starts at or before byte offset `StopAt` |
| `encode(Enc, CodePoints)` | a list of bytes |

`lex_unicode_data:version()` returns the Unicode version of the tables.

## Errors

### In the rules

Raised (as `exit`) by `regexps_to_table/1,2` and `with/2,3`:

| Exit reason | Cause |
|---|---|
| `{regexp_error, Error, Regexp}` | syntax error, Unicode encodings (bytes mode: `{error, Error}`) |
| `{regexp_not_handled, What}` | valid syntax that is not supported, e.g. `bos`/`eos` for `^`/`$`, `epsilon` for an empty regexp |
| `{unknown_property, Name}`, `{bad_property_value, Name}` | bad `\p{...}` |
| `{unknown_posix_class, Name}` | bad `[:name:]` |
| `{unencodable_char, CodePoint, Enc}` | a literal the encoding cannot represent (a surrogate, or above 255 in `latin1`) |
| `{unknown_encoding, Enc}` | bad `{encoding, Enc}` |

Syntax errors (`Error` above) include `{illegal, Chars}`,
`{unterminated, What}`, `{char_class, Range}` (bad range such as
`z-a`), `{illegal_flags, Chars}` and `{misplaced_flags, Message}`.

### While lexing

Raised (as `exit`) by `with/2,3`, `one/2` and `o_longest/2`:

```erlang
{lex_no_match, {line, Line}, {accumulated, Text}, {remainder, Rest}}
```

No rule matches. `Text` is what was read of the failed token before
the lexer gave up, `Rest` the rest of the input up to the end of the
line. In the Unicode encodings both are code points.

```erlang
{lex_bad_encoding, Enc, Kind, {line, Line}, {accumulated, Text}, {bytes, Bytes}}
```

The input is not well-formed in encoding `Enc` (only for `latin1`,
`utf8`, `utf16*`, `utf32*`). `Kind` is

- `invalid`: a malformed sequence. In UTF-8 a byte that cannot start a
  character, a missing continuation byte, an overlong form (`C0 AF`),
  an encoded surrogate (`ED A0 80`) or a value above U+10FFFF
  (`F4 90 80 80`), following the Unicode Standard, Table 3-7; in UTF-16
  an unpaired surrogate; in UTF-32 a surrogate or a value above
  10FFFF.
- `incomplete`: the input ends in the middle of a character.
- `not_a_byte`: a list element is not an integer 0-255.

`Bytes` are the offending bytes, and `Text` the code points of the
current token before them.

How errors are told apart: a token always starts at a character
boundary, and the table only accepts well-formed sequences, so when the
DFA gets stuck the lexer decodes the input from the token start. If a
malformed sequence starts at or before the byte where the DFA got stuck,
the input is to blame (`lex_bad_encoding`); otherwise no rule matches
(`lex_no_match`).

See [example 8](#8-handling-errors) for a way to turn these into error
tuples.

## Examples

All examples are in [`src/lex_examples.erl`](../src/lex_examples.erl)
and are run by `make test`, so they are known to work as shown.

### 1. A calculator in bytes mode

```erlang
calc(String) ->
    Rules = [{regexp, "[0-9]+", lex:item(num)},
             {regexp, "\\+", lex:token('+')},
             {regexp, "-", lex:token('-')},
             {regexp, "\\*", lex:token('*')},
             {regexp, "/", lex:token('/')},
             {regexp, "\\(", lex:token('(')},
             {regexp, "\\)", lex:token(')')},
             {regexp, "[ \t]+", lex:whitespace()},
             {regexp, "\n", lex:newline()}],
    lex:with(Rules, String).

%% calc("12 + (3*4)")
%% => [{num,1,"12"},{'+',1},{'(',1},{num,1,"3"},{'*',1},{num,1,"4"},{')',1}]
```

### 2. POSIX classes: an ini-style config file

```erlang
config(String) ->
    Rules = [{regexp, "\\[[[:alnum:]_]+\\]", fun section/1},
             {regexp, "[[:alpha:]_][[:alnum:]_.]*", fun key/1},
             {regexp, "=", lex:token('=')},
             {regexp, "#[^\n]*", lex:comment()},
             {regexp, "[[:blank:]]+", lex:whitespace()},
             {regexp, "\n", lex:newline()}],
    lex:with(Rules, String).

%% An action gets the matched characters in reverse order.
key(Acc) -> {token, {key, lex:current_line(), lists:reverse(Acc)}}.

section([$] | Acc]) ->
    [$[ | Name] = lists:reverse(Acc),
    {token, {section, lex:current_line(), Name}}.

%% config("[main]\nname = demo # comment\n")
%% => [{section,1,"main"},{key,2,"name"},{'=',2},{key,2,"demo"}]
```

### 3. Unicode identifiers, numbers and strings in UTF-8

Identifiers as recommended by [UAX #31]; `\d` is any decimal digit,
e.g. Arabic-Indic `٣٤`.

```erlang
idents(Utf8) ->
    lex:with(ident_rules(), Utf8, [{encoding, utf8}]).

ident_rules() ->
    [{regexp, "\\p{XID_Start}\\p{XID_Continue}*", lex:item(ident)},
     {regexp, "\\d+", lex:item(num)},
     {regexp, "\"([^\"\\\\\n]|\\\\.)*\"", lex:string()},
     {regexp, "=", lex:token('=')},
     {regexp, "\\+", lex:token('+')},
     {regexp, "[\\p{Zs}\t]+", lex:whitespace()},
     {regexp, "\n", lex:newline()}].

%% idents(<<"λx = \"naïve\" + ٣٤"/utf8>>)
%% => [{ident,1,"λx"},{'=',1},{string,1,"naïve"},{'+',1},{num,1,"٣٤"}]
```

### 4. Case-insensitive keywords

```erlang
sql(Input) ->
    Kw = fun(Word) -> {regexp, 1, "(?i)" ++ Word, lex:raw_token(list_to_atom(Word))} end,
    Rules = [Kw("select"), Kw("from"), Kw("where"),
             {regexp, "[[:alpha:]_][[:alnum:]_]*", fun(Acc) -> {token, {ident, lists:reverse(Acc)}} end},
             {regexp, "[[:space:]]+", lex:whitespace()}],
    lex:with(Rules, Input, [{encoding, utf8}]).

%% sql(<<"Select name FROM t WHERE selected">>)
%% => [select,{ident,"name"},from,{ident,"t"},where,{ident,"selected"}]
```

`Select` is matched by both the keyword and the identifier rule with
the same length; priority 1 makes it a keyword. `selected` is longer
than any keyword match, so it is an identifier.

### 5. A UTF-16LE file with a byte order mark

```erlang
utf16_file(Bin) ->
    Rules = [{regexp, "\\x{FEFF}", lex:no_token()},
             {regexp, "\\w+", lex:item(word)},
             {regexp, "\\s+", lex:whitespace()}],
    lex:with(Rules, Bin, [{encoding, utf16le}]).

%% utf16_file(unicode:characters_to_binary([16#FEFF | "Grüße Welt"],
%%                                         unicode, {utf16, little}))
%% => [{word,1,"Grüße"},{word,1,"Welt"}]
```

To read a file: `{ok, Bin} = file:read_file(Name), utf16_file(Bin)`.

### 6. Scripts and blocks

```erlang
scripts(Utf8) ->
    Word = fun(Tag) -> fun(Acc) -> {token, {Tag, lists:reverse(Acc)}} end end,
    Rules = [{regexp, "\\p{sc=Greek}+", Word(greek)},
             {regexp, "\\p{Latin}+", Word(latin)},
             {regexp, "\\p{Script=Cyrillic}+", Word(cyrillic)},
             {regexp, "[\\p{InKatakana}\\p{Hiragana}]+", Word(kana)},
             {regexp, " ", lex:whitespace()}],
    lex:with(Rules, Utf8, [{encoding, utf8}]).

%% scripts(<<"Ἀθῆναι Athens Афины アテネ"/utf8>>)
%% => [{greek,"Ἀθῆναι"},{latin,"Athens"},{cyrillic,"Афины"},{kana,"アテネ"}]
```

### 7. Pulling tokens one at a time

Build the table once, then call `one/2` for each token:

```erlang
tokens_one_by_one(Input) ->
    Lex = lex:regexps_to_table(ident_rules(), [{encoding, utf8}]),
    lex:no_lines(),
    pull(Lex, Input, []).

pull(Lex, Input, Acc) ->
    case lex:one(Lex, Input) of
        {token, T, Rest} -> pull(Lex, Rest, [T | Acc]);
        end_of_stream -> lists:reverse(Acc)
    end.

%% tokens_one_by_one(<<"a1 b2">>) => [{ident,1,"a1"},{ident,1,"b2"}]
```

### 8. Handling errors

```erlang
safe_lex(Utf8) ->
    try
        {ok, lex:with(ident_rules(), Utf8, [{encoding, utf8}])}
    catch
        exit:{lex_bad_encoding, utf8, invalid, Line, _Acc, {bytes, Bytes}} ->
            {error, {invalid_utf8, Line, Bytes}};
        exit:{lex_bad_encoding, utf8, incomplete, Line, _Acc, {bytes, Bytes}} ->
            {error, {truncated, Line, Bytes}};
        exit:{lex_no_match, Line, _Acc, {remainder, Rest}} ->
            {error, {unexpected, Line, Rest}}
    end.

%% safe_lex(<<"ok", 16#FF>>)            => {error,{invalid_utf8,{line,1},[255]}}
%% safe_lex(<<"ok ", 16#E2, 16#82>>)    => {error,{truncated,{line,1},[226,130]}}
%% safe_lex(<<"ok ?">>)                 => {error,{unexpected,{line,1},"?"}}
```

### Larger specifications

[`src/lex_test.erl`](../src/lex_test.erl) has two Erlang lexers,
`erlang_lex()` and `real_erlang_lex()`, the latter returning tokens in
the form `erl_parse` expects:

```erlang
lex:with(lex_test:real_erlang_lex(), "foo(X) -> zap(\"hi\", 42).\n")
%% => [{atom,1,"foo"},{'(',1},{var,1,"X"},{')',1},{'->',1},{atom,1,"zap"},
%%     {'(',1},{string,1,"hi"},{',',1},{integer,1,"42"},{')',1},{'.',1}]
```

They are demonstrations rather than complete Erlang lexers (e.g.
character literals such as `$\)` and escapes in strings are not fully
handled).

## Yecc compatibility

Yecc expects the tokenizer to return tuples of the form

```erlang
{Category, LineNumber, Symbol}
{Category, LineNumber}
```

the latter if there is just one member of the `Category`. For example,
all `+` operators are the same while variables differ, so we would
return `{var, 42, "VarName"}` and `{'+', 17}`. The list of tokens
should end with `{End_symbol, LineNumber}`, where `End_symbol` is
declared in the yecc file.

`lex:item/1` and `lex:token/1` produce exactly these forms; append the
end symbol yourself.

## How it works

1. **Parse** each regexp (`regexp_parse.erl`, a recursive-descent parser
   derived from the old OTP `regexp` module).
2. **Characters to sets.** Every character, class, `\p{...}`, POSIX class
   and `.` becomes a set of code points (bytes in `bytes` mode). With
   case-insensitivity the set is closed under case folding; then
   complements are taken.
3. **Sets to bytes.** Each set is *lowered* to a regexp over bytes that
   matches exactly the well-formed encodings of its members in the chosen
   encoding (`lex_unicode:to_regexp/2`):
   - The set is cut into ranges whose members have the same encoded
     length, and each range into sub-ranges whose encodings form a
     cartesian product of byte ranges: every byte position varies
     independently. This is the range splitting used by RE2 and by the
     `utf8` module of Rust's `regex-syntax` ([References](#references)),
     generalized here to "N digits of B bits": 6-bit digits for UTF-8
     continuation bytes, 8-bit digits for UTF-16 code units and UTF-32,
     and two 10-bit digits for UTF-16 surrogate pairs. For example,
     `[\x{80}-\x{7FF}]` in UTF-8 is `[C2-DF][80-BF]`, and U+10000-U+10FFFF
     in UTF-16BE is `[D8-DB][00-FF][DC-DF][00-FF]`.
   - Little-endian encodings reverse the bytes of each code unit.
   - Surrogates are removed first, so they are never matched.
   - The sequences are merged into a trie on their leading byte ranges,
     so that a class like `\p{L}` (hundreds of ranges) does not give one
     NFA branch per range.
4. **NFA.** Each rule is translated to an NFA ending in its own accepting
   state (Thompson's construction), and the rules are joined by epsilon
   transitions from a common start state.
5. **DFA.** The subset construction merges epsilon-reachable NFA states.
   Overlapping byte ranges are split so that each DFA state has disjoint
   outgoing ranges. A DFA state that contains several accepting NFA
   states accepts the rule with the highest priority, then the lowest
   rule number.
6. **Minimization.** States are partitioned by what they accept, then
   split repeatedly by which partitions their transitions lead to, until
   nothing changes (the Dragon book's DFA minimization). This matters a
   lot for Unicode, where the subset construction leaves many equivalent
   states.
7. **Table.** States are renumbered so that the accepting states are
   `1..AcceptLimit` (checking for acceptance is a comparison), and each
   state becomes a 256-element tuple mapping a byte to the next state or
   to "no match".
8. **Driving.** The driver reads one byte at a time, remembers the last
   accepting position, and on getting stuck emits the longest match and
   restarts after it (backtracking if needed, see below). In the Unicode
   encodings the action gets the token decoded into code points.

Steps 4-8 are independent of the encoding: Unicode is entirely handled
in steps 2 and 3, and by the decoding of tokens and errors in step 8.

## Performance

Measured in 2026 on an Apple M4 with Erlang/OTP 29:

| Specification | Encoding | DFA states | Build time |
|---|---|---|---|
| `lex_test:real_erlang_lex()` (68 rules) | `bytes` | 136 | 3 ms |
| same | `utf8` | 164 | 1 ms |
| same | `utf16le` | 286 | 3 ms |
| same | `utf32le` | 555 | 8 ms |
| identifier (`\p{XID_Start}\p{XID_Continue}*`), `\d+`, `\s+` | `utf8` | 520 | 11 ms |
| same | `utf16be` | 316 | 4 ms |
| same | `utf16le` | 634 | 41 ms |
| same | `utf32be` | 253 | 3 ms |
| same | `utf32le` | 575 | 133 ms |

Lexing 660 KB of mixed Greek, CJK and ASCII UTF-8 text into 100,000
tokens with the identifier lexer took 51 ms (about 13 MB/s).

A table takes about 257 words (2 KB) per state, so the tables above
are 0.3-1.3 MB. Little-endian encodings give bigger tables, since the
DFA must remember the low byte of a code unit while it reads the high
byte. Large Unicode classes in UTF-8 cost a few hundred states.

(For history: in 2004-2005, `erlang_lex()` gave 142 DFA states and was
built in 55 ms on an Athlon 1300+, and `real_erlang_lex()` in 66-68 ms
on a 1.6 GHz laptop. The DFA was not minimized then.)

## Lexing corner cases

The driver tries to match a maximally long token ("maximal munch"),
which means there can be *backtracking* if a long match ultimately
fails. In that case, the driver emits the longest known match and
continues from the saved point. This means characters can be traversed
several times. Consider the regexp `(a|a*b)`, which matches a single
`a` or any number of `a` followed by a `b`.

For the string `"aaaaa"`, the driver will traverse it all to try to
match `a*b`, then see there is no match and emit `a`. Then the same
thing is done with the remaining `"aaaa"`, `"aaa"`, `"aa"` and `"a"`.
So lexing can be quadratic in the worst case, since each token takes
linear time and may consume just one character. This sort of overlap
is not common in real specifications (you need nasty regexps and nasty
input), but be aware that it may happen.

## Unicode data

The Unicode tables are generated into
[`src/lex_unicode_data.erl`](../src/lex_unicode_data.erl) (about 380 KB)
by [`tools/gen_unicode_data.escript`](../tools/gen_unicode_data.escript)
from these files of the Unicode Character Database, version **17.0.0**:

| File | Used for |
|---|---|
| `extracted/DerivedGeneralCategory.txt` | General_Category (including `Cn`) |
| `Scripts.txt` | Script |
| `ScriptExtensions.txt` | Script_Extensions |
| `Blocks.txt` | Block |
| `PropList.txt`, `DerivedCoreProperties.txt` | binary properties |
| `PropertyAliases.txt`, `PropertyValueAliases.txt` | property and value names and aliases |
| `CaseFolding.txt` | case-insensitive matching (status C and S) |

To update to another Unicode version:

```sh
make unicode-data UCD_VERSION=18.0.0
make test
```

This downloads the files from `https://www.unicode.org/Public/<version>/ucd/`
into `ucd-<version>/` (unless they are already there) and regenerates
the module.

## Testing

`make test` runs two EUnit suites:

- `lex_unicode_tests`: for every encoding, the tables of several classes
  (any character, `.`, `\p{L}`, `[^\p{Greek}\d]`, `(?i)\p{Lu}`, POSIX
  classes, ranges across every encoding boundary) are checked against
  all 1,114,112 code points. The number of byte strings each table
  accepts is also counted and must equal the size of the class, which
  proves the table accepts nothing else, in particular no malformed
  sequence. Then: property names, case folding, POSIX classes, error
  reporting for every kind of malformed input in every encoding, and
  the legacy bytes mode.
- `lex_examples_tests`: the examples above.

## References

The algorithms:

- A. V. Aho, R. Sethi, J. D. Ullman, *Compilers: Principles, Techniques,
  and Tools*, 1st ed., Addison-Wesley, 1986 ("the Dragon book"),
  chapter 3: regular expression to NFA (Thompson's construction), NFA
  to DFA (subset construction), and DFA minimization (section 3.9).
- The UTF-8 range splitting of RE2
  ([`re2/compile.cc`](https://github.com/google/re2/blob/main/re2/compile.cc))
  and of the Rust `regex-syntax` crate's
  [`utf8` module](https://docs.rs/regex-syntax/latest/regex_syntax/utf8/index.html),
  which explains the technique.

Unicode, version 17.0.0:

- The Unicode Standard, Version 17.0,
  <https://www.unicode.org/versions/Unicode17.0.0/>; in particular
  [chapter 3](https://www.unicode.org/versions/Unicode17.0.0/core-spec/chapter-3/),
  section 3.9 "Unicode Encoding Forms" (UTF-8, UTF-16, UTF-32, and
  Table 3-7, the well-formed UTF-8 byte sequences).
- The Unicode Character Database, <https://www.unicode.org/Public/17.0.0/ucd/>.
- [UAX #44], Unicode Character Database: file formats, property and value
  aliases, general category values (section 5.7.1), loose matching of
  names (UAX44-LM3).
- [UTS #18], Unicode Regular Expressions: properties (RL1.2), simple
  loose matching for case-insensitivity (RL1.5); see the table under
  [Limitations](#limitations-and-future-work) for what is covered.
- [UAX #24], Unicode Script Property: Script and Script_Extensions.
- [UAX #31], Unicode Identifiers and Syntax: `XID_Start`/`XID_Continue`.
- [RFC 3629] (UTF-8) and [RFC 2781] (UTF-16).

Syntax and semantics of the classes:

- Perl: [perlunicode], [perluniprops] (the `\p{...}` syntax and names,
  Script_Extensions for bare script names, `\p{Word}` and the XPosix
  classes), [perlrecharclass] (`\d \s \w`, POSIX classes in Unicode,
  negation and case-insensitivity), [perlre] (`(?i)`, `(?i:...)`).
- POSIX.1-2024 (IEEE Std 1003.1), Base Definitions,
  [chapter 7](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap07.html)
  (LC_CTYPE character classes in the POSIX locale) and
  [chapter 9](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap09.html)
  (bracket expressions, `[:name:]`).

## Limitations and future work

### Unicode

- **Normalization.** Text is matched code point by code point, with no
  canonical equivalence ([UTS #18] RL2.1, [UAX #15]): `é` (U+00E9) and
  `e` followed by U+0301 COMBINING ACUTE ACCENT are different, and a
  rule for one does not match the other. Workaround: normalize input
  (and literals in rules) beforehand, e.g. with
  `unicode:characters_to_nfc_binary/1`. Future work: an option that
  makes literals match all canonically equivalent sequences.
- **Grapheme clusters.** `.` and classes match one code point, not a
  user-perceived character ([UAX #29], [UTS #18] RL2.2); there is no
  `\X`. `[^"]` happily matches a lone combining mark.
- **Case folding** is simple folding only: no multi-character folds
  (`ß` / `SS`, `ﬁ` / `fi`), no Turkic or other locale-specific mappings
  ([UTS #18] RL2.4 is not met). `(?i)` is only allowed at the start of
  a regexp; Perl allows it anywhere.
- **Properties.** Only General_Category, Script, Script_Extensions,
  Block and the binary properties of `PropList.txt` and
  `DerivedCoreProperties.txt`. That covers the minimal list of
  [UTS #18] RL1.2, but not the emoji properties of `emoji-data.txt`
  (`Emoji`, `Extended_Pictographic`, ...). Missing: other enumerated and numeric
  properties (`Bidi_Class`, `Line_Break`, `East_Asian_Width`,
  `Numeric_Value`, `Age`, ...), character names (`\N{GREEK SMALL LETTER
  ALPHA}`, RL2.5), wildcards in property values (RL2.6), string
  properties such as `RGI_Emoji`, and set operations in classes
  (`[\p{L}--\p{Lu}]`, RL1.3).
- **Boundaries.** No word boundaries (`\b`, RL1.4) or line boundaries
  (RL1.6), and no anchors at all (`^`, `$`).
- **Encodings.** No automatic detection of the encoding or BOM handling;
  no UTF-16 or UTF-32 without a declared byte order; no CESU-8 or
  Java's "modified UTF-8"; no legacy encodings other than Latin-1.
- **Unicode version.** The tables are fixed at generation time (17.0.0
  now) and have to be regenerated for new versions.

[UTS #18] level 1 at a glance (Unicode 17.0.0):

| Requirement | Status |
|---|---|
| RL1.1 Hex Notation | yes: `\x{...}` |
| RL1.2 Properties | yes: all the required properties, aliases and loose matching |
| RL1.2a Compatibility Properties | yes, following Perl: as the Standard Recommendation of Annex C, except `[:punct:]` (adds the ASCII symbols) and `[:xdigit:]` (`Hex_Digit` only, without non-ASCII decimal digits) |
| RL1.3 Subtraction and Intersection | no |
| RL1.4 Simple Word Boundaries | no |
| RL1.5 Simple Loose Matches | yes: `(?i)` with simple case folding |
| RL1.6 Line Boundaries | no |
| RL1.7 Supplementary Code Points | yes: all code points, surrogate pairs in UTF-16 |

### The lexer in general

- **Regexp language**: no anchors, empty alternatives, counted
  repetition `{n,m}`, or `"quoted strings"`; see
  [syntax.md](syntax.md#9-not-supported).
- **Input** is converted to a list of bytes, so a large binary takes
  about 16 bytes of memory per input byte while lexing. Lexing directly
  on binaries would be faster and smaller.
- **Incremental input.** `one/2` returns one token at a time but needs
  the whole input: there is no resumable state for input that arrives
  in chunks. A chunk that ends in the middle of a character (or a
  token) is an error. Future work: return a continuation.
- **No match** stops lexing with an exception; there is no built-in
  recovery (`lex`/`flex` echo and skip the character). Add a catch-all
  rule if needed.
- **Error positions** are given as a line and the surrounding text, not
  as a byte offset or column.
- **Line counting** uses the process dictionary, counts only `\n`, and
  is done by the actions.
- **Table size.** Every state is a 256-element tuple. Sparse tables,
  equivalence classes of bytes (as in `flex`), or generating Erlang code
  from the DFA (as `leex` does) would make large and little-endian
  tables smaller and possibly faster.
- **Code generation.** The table holds closures, so it cannot be written
  to a module and loaded later; it is rebuilt at run time.
- **Bytes mode** keeps some legacy quirks for compatibility: non-ASCII
  characters in regexps are matched as their UTF-8 bytes, and `\d`/`\s`
  mean DEL/space.
- **Backtracking** can make lexing quadratic for pathological rule sets,
  see [Lexing corner cases](#lexing-corner-cases).

[RFC 3629]: https://www.rfc-editor.org/rfc/rfc3629
[RFC 2781]: https://www.rfc-editor.org/rfc/rfc2781
[UAX #15]: https://www.unicode.org/reports/tr15/
[UAX #24]: https://www.unicode.org/reports/tr24/
[UAX #29]: https://www.unicode.org/reports/tr29/
[UAX #31]: https://www.unicode.org/reports/tr31/
[UAX #44]: https://www.unicode.org/reports/tr44/
[UTS #18]: https://www.unicode.org/reports/tr18/
[perlunicode]: https://perldoc.perl.org/perlunicode
[perluniprops]: https://perldoc.perl.org/perluniprops
[perlrecharclass]: https://perldoc.perl.org/perlrecharclass
[perlre]: https://perldoc.perl.org/perlre
