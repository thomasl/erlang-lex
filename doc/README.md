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

**Input** is always bytes, as iodata: a binary, or a list of bytes
(integers 0-255), binaries and such lists, nested to any depth (see
[Iolist input](#iolist-input)). To lex an Erlang string (a list of
code points), encode it first, e.g. `unicode:characters_to_binary(Str)`
for UTF-8. Passing the string itself is an error (`not_a_byte`) as soon
as it contains a code point above 255.

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

Build a table from `Rules` and lex all of `Input`, which is iodata. A
binary is lexed directly by the [binary driver](#binary-input), a list
(flat or nested) by the [iolist driver](#iolist-input); they give the
same tokens. Resets the position to line 1, column 1 first. Exits on
errors (see [Errors](#errors)).

```erlang
regexps_to_table(Rules) -> LexTable
regexps_to_table(Rules, Options) -> LexTable
```

Compile the rules. `Options` is a list of:

- `{encoding, Enc}`: see [Encodings](#encodings); default `bytes`
- `case_insensitive` or `{case_insensitive, Bool}`: make all rules
  case-insensitive, as if each started with `(?i)`; default `false`
  (see [syntax.md](syntax.md#7-case-insensitive-matching))
- `{newline, unicode | ascii | lf}`: which characters end a line, see
  [Line and column counting](#line-and-column-counting); default
  `unicode`, or `ascii` in the `bytes` encoding
- `{position, process | argument | unboxed}`: whether the drivers keep
  the line and column in the process dictionary (default) or pass them
  as arguments, see [Position as an argument](#position-as-an-argument)
- `{table, tuple | binary | {binary, 8 | 16 | 32}}`: how the DFA table
  is stored; default `tuple`. `binary` uses the smallest entry width
  that holds the number of states. The binary is smaller and free to
  share between processes, but lexing is slower; see
  [Table representation](#table-representation)

Errors in the rules exit here, see [Errors](#errors). The table is a
`#lex{}` record, defined in [src/lex.hrl](../src/lex.hrl); treat it as
opaque.

```erlang
one(LexTable, Input) -> {token, Token, Rest} | end_of_stream
```

Return the next token and the rest of the input, or `end_of_stream`
when only skipped text (`no_token`) remains. The rest has the same form
as `Input`: for a binary, a sub-binary of it (nothing is copied); for a
list, iodata made of what remains of its parts (for a flat list of
bytes, simply its tail). This is the interface for a parser that pulls tokens one
at a time ([example 7](#7-pulling-tokens-one-at-a-time)). `one/2`
continues from the current position and does not reset it: call
`no_lines()` before lexing a new input.

```erlang
start(LexTable) -> Cont
feed(Cont, Data | eof) -> Cont
next(Cont) -> {token, Token, Cont} | {more, Cont} | end_of_stream
tokens(Cont) -> {more, [Token], Cont} | {done, [Token]}
one(LexTable, Data, More) -> {token, Token, Rest, More1} | end_of_stream
with(Rules, Data, More, Options) -> [Token]
```

Lex a stream that arrives in chunks, either by feeding the chunks to a
continuation as they arrive (non-blocking), or by giving a function
`More()` that the lexer calls for the next chunk. See
[Incremental input](#incremental-input).

```erlang
o_with(Rules, String) -> [Token]
o_longest(LexTable, String) -> [Token]
```

The older, non-incremental driver, kept for compatibility (`String` must
be a list). Equivalent to `with/2`; `o_longest/2` also resets the
position.

### Binary input

A binary is lexed in place, without converting it to a list (which
takes 16 bytes of memory per input byte). The binary driver reads the
input a byte at a time through a binary match that the compiler turns
into a reused match context, so reading a byte allocates nothing; only
the bytes of the token being read are collected, as a list, which is
what actions get. Tokens, positions and errors are the same as for a
list (the tests check this on thousands of random inputs).

**Garbage collection.** For a large binary, two settings of the
process doing the lexing matter:

- `min_bin_vheap_size`: a binary larger than this (default 46422 words,
  371 KB) that the process keeps referring to, as the lexer does with
  its input, makes nearly every garbage collection a full one, which
  copies all the tokens made so far. For a 1 MB input that made lexing
  about 3 times slower.
- `min_heap_size`: the heap otherwise has to grow step by step as the
  token list grows.

`with/2,3` raises both for the duration of the call, to twice the size
of the binary and to 2 words per input byte (each at most 16M words, or
128 MB), and then restores them. `one/2` cannot do this, since it returns
after each token; when pulling tokens from a large binary, raise the
limits around the loop yourself:

```erlang
Words = byte_size(Bin) div erlang:system_info(wordsize),
Old = process_flag(min_bin_vheap_size, 2 * Words),
try pull(Lex, Bin, []) after process_flag(min_bin_vheap_size, Old) end
```

or lex in a process spawned with
`spawn_opt(Fun, [{min_bin_vheap_size, 2 * Words}])`.

### Iolist input

A list is lexed as iodata, the type of `iolist_to_binary/1` and of most
I/O functions: bytes, binaries and nested lists of them, where a binary
may also be the tail of a list (`[$a, $b | <<"cd">>]`). It is read in
place, piece by piece, without flattening it or copying it into a
binary, so input assembled from many parts (file chunks, the output of
other functions) can be lexed as it is:

```erlang
%% with the rules of example 3
lex:with(ident_rules(), [<<"λx = "/utf8>>, ["42", $\s], [[<<"+ 1">>]]], [{encoding, utf8}])
%% => [{ident,1,"λx"},{'=',1},{num,1,"42"},{'+',1},{num,1,"1"}]
```

(This is `lex_examples:iodata/1`.)

Tokens and characters may span pieces: a token, or a UTF-8 character,
can start in one binary and end in the next.

The iolist driver keeps a cursor: the piece being read and a stack of
what comes after it. Reading a byte from a list allocates nothing, and a
binary piece is read like in the binary driver. A flat list of bytes is
the simplest iolist, and lexes as fast as with the original list driver
(still available as `lex:one_list/2` and `lex:lex_string/2`, for flat
lists only). With small pieces (1-64 bytes) a nested iolist takes a
fifth to a half longer than the same bytes as one flat list.

For `one/2`, the rest is again iodata, built from what remains of the
input's parts, so it can be passed back to `one/2` or to any function
that takes iodata.

An element that is not a byte, a binary or a list (an integer above
255, an atom, a bitstring that is not a whole number of bytes) is an
error when the lexer reaches it: `not_a_byte` in the encodings other
than `bytes`, `lex_no_match` in `bytes`.

`with/2,3` raises `min_bin_vheap_size` (see [Binary input](#binary-input))
to the size of the input while lexing a list, since binaries in it have
the same effect on garbage collection as a single binary does.

### Incremental input

For data that arrives in chunks, e.g. from a socket or a file read a
piece at a time, there are two interfaces: a non-blocking one, where you
feed the lexer the chunks as they arrive, and one where the lexer calls a
function for the next chunk.

**Non-blocking.** A continuation holds the lexer's state between chunks:

| Function | |
|---|---|
| `start(LexTable)` | a new continuation, at line 1, column 1 |
| `feed(Cont, Data)` | add a chunk (iodata, may be empty) |
| `feed(Cont, eof)` | the stream has ended; feeding more exits with `lex_feed_after_eof` |
| `next(Cont)` | `{token, Token, Cont1}`, or `{more, Cont1}` when it needs data that has not been fed yet, or `end_of_stream` |
| `tokens(Cont)` | all the tokens it can get now: `{more, Tokens, Cont1}` (feed more, then continue) or `{done, Tokens}` |
| `rest(Cont)` | the data fed but not yet lexed, as iodata |
| `position(Cont)` | `{Line, Column}` where the next token starts |

`next/1` never blocks, so the lexer fits a process that gets its data as
messages, such as a `gen_server` with an active socket:

```erlang
init(Socket) ->
    ok = inet:setopts(Socket, [{active, true}]),
    {ok, #state{socket = Socket, lex = lex:start(LexTable)}}.

handle_info({tcp, _Socket, Data}, State = #state{lex = C}) ->
    {more, Tokens, C1} = lex:tokens(lex:feed(C, Data)),
    handle_tokens(Tokens),
    {noreply, State#state{lex = C1}};
handle_info({tcp_closed, _Socket}, State = #state{lex = C}) ->
    {done, Tokens} = lex:tokens(lex:feed(C, eof)),
    handle_tokens(Tokens),
    {stop, normal, State}.
```

(The tests run this loop on a real socket, in `active_socket_test`.)

Each continuation keeps its own line and column, so one process can lex
several streams at once, e.g. one per connection; `next/1` puts the
position in the process dictionary while it runs, so actions use
`current_line()` etc. as usual.

**With a function for more data.** Pass the data you have and a function
`More`:

```erlang
lex:with(Rules, Data, More, Options) -> [Token]
lex:one(LexTable, Data, More) -> {token, Token, Rest, More1} | end_of_stream
```

When the lexer runs out of data, it calls `More()`, which returns

| Result | Meaning |
|---|---|
| `{more, Data, Cont}` | `Data` is the next chunk (iodata); more data may follow, and `Cont` is the function to call when this chunk runs out too |
| `{last, Data}` | `Data` is the last chunk |
| `ended` | there is no more data |

Anything else exits with `{bad_more_result, Result}`. After `{last, Data}`
or `ended` the lexer never calls a function again. `Data` may be empty.

`one/3` returns the rest of the data it already has, `Rest`, and the
function to call when that runs out, `More1`; pass both to the next call
(after the stream has ended, `More1` just returns `ended`). As with
`one/2`, call `no_lines()` before a new stream; `with/4` does that.

`More()` may block (e.g. in `gen_tcp:recv/2`); this interface is built on
the non-blocking one, calling `More()` whenever `next/1` asks for more.

Reading a socket:

```erlang
More = fun M() ->
               case gen_tcp:recv(Socket, 0) of
                   {ok, Data} -> {more, Data, M};
                   {error, closed} -> ended
               end
       end,
Tokens = lex:with(Rules, <<>>, More, [{encoding, utf8}])
```

Reading a file in 4 KB chunks (`lex_examples:from_file/1`):

```erlang
from_file(Name) ->
    {ok, Fd} = file:open(Name, [read, binary, raw]),
    More = fun M() ->
                   case file:read(Fd, 4096) of
                       {ok, Data} -> {more, Data, M};
                       eof -> ended
                   end
           end,
    try lex:with(ident_rules(), <<>>, More, [{encoding, utf8}])
    after file:close(Fd)
    end.
```

**When more data is needed** (`next/1` returns `{more, Cont}`, or the
lexer calls `More()`). Only when the lexer has no data left. That may be
in the middle of a token, or of a character: the lexer resumes where it
stopped, so tokens, UTF-8 characters and the backtracking of longest
match can all span chunks, and give exactly the same tokens, positions
and errors as lexing the whole input at once (the tests check this on
tens of thousands of random splits, down to 1 byte per chunk).

By the longest-match rule, a token that is complete at the end of the
available data might still continue in the next chunk, so the lexer asks
for more data before returning it. With an interactive source, a word or
a number at the end of a line is therefore returned when the next data
arrives (or the stream ends). There is one exception: if no further byte
can extend the token (its DFA state has no transitions), as for `;` or
`)`, the token is returned at once. Ending lines with a newline token
rule (e.g. `{regexp, "\n", ...}`) also helps, since a newline is
usually one of those.

When lexing fails in a Unicode encoding at a character that looks
incomplete so far, whether it is invalid or merely incomplete depends on
the bytes after it, so the lexer asks for more data (up to 4 bytes)
before reporting the error.

The incremental driver does not adjust the garbage collection settings
(see [Binary input](#binary-input)), since it does not know the size of
the input in advance. Chunks are usually small, so this matters only if
they are larger than a few hundred KB.

### Token helpers

Functions that return ready-made actions. `Line` is the line where the
token starts (`current_line()` while the action runs), `Text` the token
text (not reversed).

| Helper | Emits |
|---|---|
| `item(Key)` | `{Key, Line, Text}` |
| `token(Key)` | `{Key, Line}` |
| `raw_token(Key)` | `Key` |
| `op(Key)` | `{op, Line, Key}` |
| `enclosed_token(Char, Tag)` | `{Tag, Text}` without the first and last character, which must both be `Char` |
| `enclosed_token(Start, End, Tag)` | as above, with different start and end characters |
| `string()` | `{string, Line, Text}` without the enclosing `"` (escape sequences are not processed) |
| `quoted_atom()` | `{atom, Line, Text}` without the enclosing `'` |
| `no_token()`, `whitespace()`, `comment()`, `newline()` | nothing |

`newline()` used to increment the line count; now the lexer counts
lines itself, and `newline()` is the same as `whitespace()`.

### Line and column counting

The lexer keeps track of the current position, a line and a column,
both counted from 1. Every match, whether it gives a token or not,
advances the position over the matched text:

- a **line terminator** moves to column 1 of the next line;
- any other character moves to the next column. Columns count
  **characters**: code points in the Unicode encodings and `latin1`,
  bytes in `bytes` mode. A tab is one column; there is no notion of tab
  stops or display width.

The line terminators depend on the option `{newline, NL}`:

| `NL` | Line terminators | Default for |
|---|---|---|
| `unicode` | LF, VT, FF, CR, CR LF, NEL (U+0085), LINE SEPARATOR (U+2028), PARAGRAPH SEPARATOR (U+2029) | `latin1`, `utf8`, `utf16*`, `utf32*` |
| `ascii` | LF, VT, FF, CR, CR LF | `bytes` |
| `lf` | LF only (how `lex` used to count) | |

The `unicode` set is the one of [UTS #18] RL1.6 and the Unicode
Standard's newline guidelines (section 5.8). CR LF is one line break,
also when the CR and the LF are matched by different rules. `unicode` is
not allowed with `bytes`, since there the input may be UTF-8, in which
the byte 16#85 is a continuation byte, not NEL.

While an action runs, the position is that of the **start of its
token**, so actions can include it in their tokens:

```erlang
positions(Utf8) ->
    lex:with([{regexp, "\\p{L}+", at(word)}, {regexp, "\\s+", lex:whitespace()}],
             Utf8, [{encoding, utf8}]).

at(Tag) -> fun(Acc) -> {token, {Tag, lex:current_position(), lists:reverse(Acc)}} end.

%% positions(<<"ab\ncd λ"/utf8>>)
%% => [{word,{1,1},"ab"},{word,{2,1},"cd"},{word,{2,4},"λ"}]
```

(This is `lex_examples:positions/1`.)

Actions do not need to count lines; tokens that span lines (strings,
comments) are counted correctly by the lexer.

| Function | |
|---|---|
| `current_position()` | `{Line, Column}` |
| `current_line()`, `current_column()` | the line and the column |
| `no_lines()` | reset the position to line 1, column 1 |
| `inc_lines()`, `inc_lines(N)` | add 1 or `N` to the line, e.g. for a directive that changes the line number; do not use it for line terminators, which are already counted |
| `count_lines(Text)` | the number of line breaks in `Text` by the `ascii` rules (correct for both bytes and code points) |
| `count_lines(Text, NL)` | the same with `NL` = `unicode`, `ascii` or `lf` |

The position is kept in the process dictionary, so with `with/2,3`,
`one/2,3` it belongs to the process: two such lexers interleaved in one
process share it. The non-blocking interface keeps a position per stream
(see [Incremental input](#incremental-input)).

#### Position as an argument

With the option `{position, argument}`, the drivers do not keep the
position in the process dictionary: they pass `{Line, Column}` to each
action and get the new position back, as arguments and results.

An action can take the position as a second argument,
`fun(Acc, {Line, Column}) -> ...`. This works in both modes (in process
mode the driver reads the position from the dictionary for it). A
1-argument action, such as the token helpers, also works in argument
mode: the driver puts the position in the dictionary for the call, so
that `current_line()` and `inc_lines/1` work as usual.

```erlang
Rules = [{regexp, "[a-z]+", fun(Acc, {Line, Col}) ->
                                    {token, {word, {Line, Col}, lists:reverse(Acc)}}
                            end},
         {regexp, "[ \n]+", fun(_Acc, _Pos) -> no_token end}],
lex:with(Rules, <<"ab\n cd">>, [{position, argument}])
%% => [{word,{1,1},"ab"},{word,{2,2},"cd"}]
```

(This is `lex_examples:position_argument/1`.)

With only 2-argument actions, argument mode does not touch the process
dictionary at all; `current_line()` etc. are then not updated. `one/2,3`
continue from the dictionary's position and leave the new one there, as
in process mode.

It is not faster. Lexing about 1 MB in a new process (Apple M4, OTP 29,
best of 9, three rounds):

| Input | Process mode | Argument mode, 2-argument actions | Argument mode, 1-argument actions |
|---|---|---|---|
| `bytes`, binary | 36 ms | 36 ms | 41 ms |
| `bytes`, flat list | 39 ms | 39 ms | 53 ms |
| `utf8`, binary | 41 ms | 38 ms | 40 ms |
| `utf8`, 4 KB chunks | 140 ms | 145 ms | 148 ms |

A dictionary `put` and `get` per token cost about as much as returning
the new position in a tuple. 1-argument actions in argument mode pay for
both. The reason to use argument mode is that the position is then plain
data rather than state of the process. (The non-blocking interface keeps
a position per stream in either mode.)

#### Unboxed position: 3-argument actions

With `{position, unboxed}`, the whole-input drivers (`with/2,3`, for a
binary or an iolist) keep the line, the column and the CR flag as three
separate arguments of their loop, and call 3-argument actions directly:

```erlang
fun(Acc, Line, Column) -> {token, ...} | no_token
```

Nothing is allocated for the position: the position after a token is
computed from the reversed token itself, by counting the characters
after its last line terminator, and only when it has one, its line
breaks. (The other modes reverse every token to advance the position.)
2- and 1-argument actions work in this mode too, and 3-argument actions
work in the other two modes.

The drivers that return after each token (`one/2,3` and the
[non-blocking interface](#incremental-input)) return a position anyway,
so in unboxed mode they work as in argument mode.

This is faster for whole inputs, most for `bytes`, where the token does
not have to be decoded (and so is never reversed). The same measurements
as above:

| Input | Process mode | Argument mode, 2-argument actions | Unboxed mode, 3-argument actions | Unboxed mode, 1-argument actions |
|---|---|---|---|---|
| `bytes`, binary | 36 ms | 36 ms | 27 ms | 41 ms |
| `bytes`, flat list | 38 ms | 38 ms | 36 ms | 40 ms |
| `utf8`, binary | 40 ms | 37 ms | 37 ms | 40 ms |
| `utf8`, 4 KB chunks | 137 ms | 141 ms | 142 ms | 145 ms |

(`bytes`, binary: 25% faster. The 4 KB chunks go through the
non-blocking interface, so unboxed mode does not apply.)

Using the same reverse-walk position update in the other modes was
tried, and made them slower (`bytes`, binary: 36 to 43 ms in process
mode; flat list: 37 to 46 ms in argument mode), although it is about as
fast in isolation: tokens are short, so reversing them costs little. It
pays off only together with the unboxed loop.

### Other utilities

| Function | |
|---|---|
| `safe_literal(String)` | escape the regexp special characters in `String`, so `lex:safe_literal("a+b")` matches the text `a+b` |
| `strip_c_comments(String)` | remove (nestable) `/* ... */` comments; a pre-pass for input that is hard to describe with regexps |
| `resolve_line(String)` | replace `?LINE` with the line number |
| `transition(State, Byte, Table)` | the next state of the DFA, or -1; `Table` is the `table` field of the `#lex{}` from `regexps_to_table/2`, in either [representation](#table-representation) |

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
dfa_to_table(DFA, InternalRules, Enc, NL) -> LexTable
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
| `{unknown_newline, NL}` | bad `{newline, NL}` |
| `{unicode_newline_needs_encoding, bytes}` | `{newline, unicode}` with the `bytes` encoding |
| `{unknown_table_option, Rep}` | bad `{table, Rep}` |
| `{table_too_large, States, {binary, Bits}}` | `{table, {binary, Bits}}` with too many states for `Bits`-bit entries |

Syntax errors (`Error` above) include `{illegal, Chars}`,
`{unterminated, What}`, `{char_class, Range}` (bad range such as
`z-a`), `{illegal_flags, Chars}` and `{misplaced_flags, Message}`.

### While lexing

Raised (as `exit`) by `with/2,3`, `one/2` and `o_longest/2`. Positions
are counted as in [Line and column counting](#line-and-column-counting).

```erlang
{lex_no_match, {line, Line}, {column, Column}}
```

No rule matches the input at `Line`, `Column`: the position where the
token that could not be matched starts. For an unterminated string, for
example, that is the opening quote.

```erlang
{lex_bad_encoding, Enc, Kind, {line, Line}, {column, Column}, {bytes, Bytes}}
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
- `not_a_byte`: an element of an input list is not a byte, a binary or a
  list (for example a code point above 255 in a string that was not
  encoded).

`Bytes` are the offending bytes, and `Line`, `Column` their position
(also inside a token that spans several lines).

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
        exit:{lex_bad_encoding, utf8, invalid, {line, L}, {column, C}, {bytes, Bytes}} ->
            {error, {invalid_utf8, {L, C}, Bytes}};
        exit:{lex_bad_encoding, utf8, incomplete, {line, L}, {column, C}, {bytes, Bytes}} ->
            {error, {truncated, {L, C}, Bytes}};
        exit:{lex_no_match, {line, L}, {column, C}} ->
            {error, {unexpected, {L, C}}}
    end.

%% safe_lex(<<"ok", 16#FF>>)                  => {error,{invalid_utf8,{1,3},[255]}}
%% safe_lex(<<"ok\nλ "/utf8, 16#E2, 16#82>>)  => {error,{truncated,{2,3},[226,130]}}
%% safe_lex(<<"ok ?">>)                       => {error,{unexpected,{1,4}}}
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

1. **Parse** each regexp (`lex_spec_parse.erl`, a recursive-descent
   parser; the grammar is at the top of the file).
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
   restarts after it (backtracking if needed, see below). There are
   drivers with the same behaviour for binaries and for iolists (see
   [Binary input](#binary-input) and [Iolist input](#iolist-input)). In the Unicode encodings the action
   gets the token decoded into code points.

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

Lexing about 1 MB with `with/3` in a new process, including building
the table, with the identifier lexer above (or, for `bytes`, an ASCII
version of it), as a binary and as a list (`binary_to_list/1` of the
same binary):

| Input | Bytes | Tokens | Binary | List |
|---|---|---|---|---|
| `bytes`, ASCII text | 1,000,000 | 250,000 | 35 ms, 39 MB | 47 ms, 47 MB |
| `utf8`, ASCII text | 1,000,000 | 250,000 | 60 ms, 63 MB | 68 ms, 49 MB |
| `utf8`, Greek, CJK and ASCII | 990,000 | 150,000 | 40 ms, 35 MB | 49 ms, 33 MB |
| `utf16le`, Greek, CJK and ASCII | 1,320,000 | 150,000 | 89 ms, 50 MB | 93 ms, 46 MB |

(The memory is that of the lexing process afterwards; most of it is the
tokens, plus the list of the input for a list.) That is 10-30 MB/s. The
time is mostly spent per token, on the actions and on keeping track of
the position, rather than per byte. The same bytes as a nested iolist
of pieces of 1-64 bytes (binaries, byte lists and nested lists) took
57 ms (`bytes`) and 53 ms (`utf8`, Greek, CJK and ASCII).

A table takes about 257 words (2 KB) per state, so the tables above
are 0.3-1.3 MB. Little-endian encodings give bigger tables, since the
DFA must remember the low byte of a code unit while it reads the high
byte. Large Unicode classes in UTF-8 cost a few hundred states.

(For history: in 2004-2005, `erlang_lex()` gave 142 DFA states and was
built in 55 ms on an Athlon 1300+, and `real_erlang_lex()` in 66-68 ms
on a 1.6 GHz laptop. The DFA was not minimized then.)

### Table representation

By default the DFA table is a tuple with one row per state, and each row
is a tuple of 256 next states, so a transition is two `element/2` calls.
With the option `{table, binary}` it is instead one binary of
`States * 256` unsigned entries. The entry for `(State, Byte)` is at
index `(State - 1) * 256 + Byte`, and 0 means no match (states are
numbered from 1). The entries are 8, 16 or 32 bits wide, the smallest
that holds the number of states: 8 bits for up to 255 states, 16 for up
to 65,535. `{table, {binary, Bits}}` sets the width, and exits with
`{table_too_large, States, {binary, Bits}}` if it is too small.

The scan loops exist once for tuples and once for each width, so that
the width is a constant in the bit-syntax match. A width read at run
time made each lookup 40-60% slower. The drivers choose the loop when
they start each token. The tuple loops are the same as before, and the
benchmark below showed no difference in tuple timings from the extra
dispatch. `lex:transition(State, Byte, Table)` reads either kind of
table.

`make bench-table` ([bench/lex_table_bench.erl](../bench/lex_table_bench.erl))
compares the representations on an Erlang lexer: keywords, atoms,
variables, integers in every base, floats, strings, quoted atoms, `$`
characters, comments and all operators, 44 rules. It comes in an
ASCII/Latin-1 version (`bytes`) and one with UAX #31 identifiers in
UTF-8. The input is this repository's Erlang source repeated to
1.22 MB: 238,665 tokens, lexed as a binary, a string, and an iolist
with one binary per line. Each time is the median of 9 runs, each in a
new process, with the table built beforehand. Measured on an Apple M4
with Erlang/OTP 29 (run-to-run noise is about 3%):

| | tuple | binary, 8 bits | binary, 16 bits | binary, 32 bits |
|---|---|---|---|---|
| `bytes`: 174 states | | (chosen by `binary`) | | |
| table in memory | 359 KB | 45 KB | 89 KB | 178 KB |
| `term_to_binary` size | 179 KB | 45 KB | 89 KB | 178 KB |
| give the table to a new process | 68 µs | 0.6 µs | 0.7 µs | 0.7 µs |
| lex a binary | 35 ms | 46 ms | 46 ms | 46-47 ms |
| lex a string | 32-33 ms | 46 ms | 46 ms | 47-48 ms |
| lex an iolist of lines | 34-35 ms | 81-87 ms | 81-82 ms | 81-82 ms |
| `utf8`: 1,147 states | | too few states | (chosen by `binary`) | |
| table in memory | 2.37 MB | | 587 KB | 1.17 MB |
| `term_to_binary` size | 1.31 MB | | 587 KB | 1.17 MB |
| give the table to a new process | 455-465 µs | | 0.8 µs | 0.9 µs |
| lex a binary | 48-49 ms | | 52-55 ms | 52-53 ms |
| lex a string | 47-48 ms | | 51-52 ms | 52 ms |
| lex an iolist of lines | 73-75 ms | | 114-117 ms | 114-115 ms |

The binary is 4 to 8 times smaller than the tuples. Since it is a
large, reference-counted binary, it is never copied, so giving it to
another process is free. But lexing is slower: 8-42% longer on binaries
and strings, and 55-140% longer on iolists of short binaries. A tuple
lookup allocates nothing. Each binary lookup starts a new match on the
table (`bs_start_match3` in the BEAM code), which creates a match
context on the heap for every byte lexed. That cost is the same at
every width, so 8-bit entries only save space. `binary:at/2`, which
allocates nothing, was slower still in a micro-benchmark. Inlining the
lookup made no difference.

The entries are big-endian (`integer-unsigned-big`), so a table has
the same bytes on every machine and can be stored with `term_to_binary`
or sent to another node. `native` byte order was also tried, on this
little-endian Apple M4. The BEAM code was identical except for one flag
on the instruction that reads the entry. In the lexer, the difference
was within 1% (run-to-run noise is about 3%):

| Lex with a 16-bit table (ms) | big | native |
|---|---|---|
| `bytes`: binary / string / iolist | 46 / 46-47 / 84-85 | 47 / 47-48 / 83-85 |
| `utf8`: binary / string / iolist | 51 / 50-51 / 114-118 | 52 / 51-52 / 114-117 |

32-bit tables gave the same picture. In a micro-benchmark of 20 million
chained lookups alone, the difference went both ways: `big` was about
10% faster at 16 bits, and `native` 15-20% faster at 32 bits. These
results also depended on the order the variants ran in. Since `native`
gains nothing in the lexer and would make tables differ between
machines, the entries stay big-endian. 8-bit entries have no byte order.

So `tuple` stays the default. `binary` pays off when one table is
shared by many processes or stored, where it saves memory and copying.
A tuple table can also avoid being copied by keeping it in
`persistent_term`.

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
  Table 3-7, the well-formed UTF-8 byte sequences), and
  [chapter 5](https://www.unicode.org/versions/Unicode17.0.0/core-spec/chapter-5/),
  section 5.8 "Newline Guidelines" (line terminators).
- The Unicode Character Database, <https://www.unicode.org/Public/17.0.0/ucd/>.
- [UAX #44], Unicode Character Database: file formats, property and value
  aliases, general category values (section 5.7.1), loose matching of
  names (UAX44-LM3).
- [UTS #18], Unicode Regular Expressions: properties (RL1.2), simple
  loose matching for case-insensitivity (RL1.5), line terminators
  (RL1.6); see the table under
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
  `\X`. `[^"]` happily matches a lone combining mark, and a column is a
  code point, not a grapheme cluster.
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
- **Boundaries.** No word boundaries (`\b`, RL1.4). All the line
  terminators of RL1.6 are recognized when counting lines, but there are
  no anchors (`^`, `$`), and `.` excludes only LF, not the other line
  terminators.
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
| RL1.6 Line Boundaries | partly: line counting recognizes all the line terminators, but there are no `^`/`$` anchors, and `.` excludes only LF |
| RL1.7 Supplementary Code Points | yes: all code points, surrogate pairs in UTF-16 |

### The lexer in general

- **Regexp language**: no anchors, empty alternatives, counted
  repetition `{n,m}`, or `"quoted strings"`; see
  [syntax.md](syntax.md#9-not-supported).
- **Large binaries and garbage collection**: `with/2,3` adjusts the
  process's GC settings while it lexes a binary, see
  [Binary input](#binary-input); with `one/2` that is up to the caller.
- **Incremental input**: a token that could still grow is only returned
  when more data arrives (or the stream ends), as longest match
  requires; see [Incremental input](#incremental-input).
- **No match** stops lexing with an exception; there is no built-in
  recovery (`lex`/`flex` echo and skip the character). Add a catch-all
  rule if needed.
- **Positions** are a line and a column in characters (code points, or
  bytes in `bytes` mode). There is no byte offset, and columns do not
  account for tab stops, display width (wide CJK characters, combining
  marks) or the UTF-16 code units that some editors and the Language
  Server Protocol use. Except in the non-blocking incremental interface
  (one position per continuation), the position is kept in the process
  dictionary, so it is per process; actions always read it from there.
- **Table size.** Every state is a 256-element tuple (or, with
  `{table, binary}`, 256-1,024 bytes of a binary, which is smaller but
  slower; see [Table representation](#table-representation)). Sparse tables,
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
