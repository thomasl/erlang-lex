# Erlang-Lex specification syntax

This is the reference for the rules and regular expressions accepted by
`lex:regexps_to_table/1,2` and `lex:with/2,3`. For the API, usage and
background, see [README.md](README.md).

Contents:

1. [Rules](#1-rules)
2. [Regular expressions](#2-regular-expressions)
3. [Escapes](#3-escapes)
4. [Character classes](#4-character-classes)
5. [POSIX classes](#5-posix-classes)
6. [Named Unicode classes (`\p{...}`)](#6-named-unicode-classes-p)
7. [Case-insensitive matching](#7-case-insensitive-matching)
8. [Bytes mode versus the Unicode encodings](#8-bytes-mode-versus-the-unicode-encodings)
9. [Not supported](#9-not-supported)

Regexps are usually given as Erlang strings, so every backslash in this
document is written twice in Erlang source: the regexp `\p{L}+` is the
Erlang string `"\\p{L}+"`, and `\.` is `"\\."`. They can also be
binaries or iolists, see [Rules](#1-rules).

## 1. Rules

A specification is a list of rules:

```erlang
{regexp, Regexp, Action}
{regexp, Priority, Regexp, Action}
```

- `Regexp` is a string, a binary or an iolist of both, in the syntax
  below. See [How regexp characters are read](#how-regexp-characters-are-read).
- `Priority` is an integer `>= 0`; the default is 0.
- `Action` is a fun of one argument, or any other term (see below).

**Which rule wins.** At each position the lexer takes the *longest*
match ("maximal munch"). If several rules match that longest token, the
one with the highest `Priority` wins, and among equal priorities the
rule that comes first in the list. So keywords that should beat a
general identifier rule can come first or get a higher priority:

```erlang
[{regexp, 1, "if", lex:raw_token('if')},       % "if"  -> 'if'
 {regexp, "[a-z]+", lex:item(atom)}]           % "iffy" -> {atom,1,"iffy"}
```

**Actions.** `Action(Acc)` is called with the matched text *reversed*:
a list of bytes in the `bytes` encoding, otherwise a list of code points.
An action may also take the position where the token starts, as
`Action(Acc, {Line, Column})` or `Action(Acc, Line, Column)`. It returns `{token, Token}` to
emit `Token`, or `no_token` to drop the match (whitespace, comments). Use `lists:reverse(Acc)` for the text. If
`Action` is not a fun, the match is printed as `rule Action: Text` and
dropped, which is handy while developing a specification.

`lex` provides ready-made actions, see
[README.md, "Token helpers"](README.md#token-helpers).

### How regexp characters are read

With the default option `{regexp_encoding, utf8}`, these are all the
same regexp:

```erlang
"λ+"                                 % an Erlang string literal: [955, $+]
<<"λ+"/utf8>>                        % a UTF-8 binary
["λ", <<"+">>]                       % an iolist
binary_to_list(<<"λ+"/utf8>>)        % a string of UTF-8 bytes: [206, 187, $+]
```

- A binary is UTF-8. Write `<<"λ"/utf8>>`: plain `<<"λ">>` keeps only the
  low byte of each character, which is not valid UTF-8 and gives the
  error `{bad_characters, Regexp}`.
- A string (or iolist) whose elements are all bytes is read as UTF-8 if
  it is valid UTF-8. That covers a regexp read from a file or a socket
  as a list, or source code compiled as Latin-1.
- Otherwise the elements of a string are code points, as in an Erlang
  string literal in a UTF-8 source file. `"é"` is `[233]`, which is not
  valid UTF-8 on its own, so it stays U+00E9. So does `[<<"é"/utf8>>,
  "é"]`, which is not valid UTF-8 as a whole: its binary is still UTF-8
  and its list part code points.
- ASCII reads the same either way, and so do escapes such as `\x{3BB}`.

The guess has one ambiguous case: a string of code points 128-255 that
happens to be valid UTF-8, such as `"Ã©"` (`[195, 169]`), reads as `é`.
Two other settings avoid guessing:

| `{regexp_encoding, E}` | Strings | Binaries |
|---|---|---|
| `utf8` (default) | UTF-8 if all bytes and valid UTF-8, otherwise code points | UTF-8 |
| `unicode` | code points (Erlang `unicode:chardata()`) | UTF-8 |
| `latin1` | one character 0-255 per element | one character per byte |

In `bytes` mode a non-ASCII character then matches its UTF-8 bytes,
as a whole character (see
[section 8](#8-bytes-mode-versus-the-unicode-encodings)). With
`latin1`, each character is instead the byte to match, so
`{regexp_encoding, latin1}` is the way to write raw bytes in `bytes`
mode without `\xHH` escapes.

`lex:safe_literal/1` returns its result in the form of its argument (a
string, binary or iolist), so it works with all of these.

## 2. Regular expressions

| Syntax | Meaning |
|---|---|
| `c` | the character `c` (any character that is not special) |
| `\c` | escaped character, see [Escapes](#3-escapes) |
| `.` | any character except newline (`\n`) |
| `[...]` | character class, see [Character classes](#4-character-classes) |
| `[^...]` | complemented character class |
| `r1r2` | `r1` followed by `r2` |
| `r1\|r2` | `r1` or `r2` |
| `r*` | zero or more `r` |
| `r+` | one or more `r` |
| `r?` | zero or one `r` |
| `(r)` | grouping |
| `(?i)r` | `r` case-insensitive; only at the start of the regexp |
| `(?-i)r` | `r` case-sensitive (overrides the `case_insensitive` option); only at the start |
| `(?i:r)` | `r` case-insensitive |
| `(?-i:r)` | `r` case-sensitive |

Operators bind as usual: postfix `*`, `+`, `?` (which may be stacked,
as in `a*?`) bind tightest, then concatenation, then `|`.

The special characters outside classes are `( ) | * + ? . [ ] \` and,
reserved for future use, `^` and `$`. `{`, `}`, `-`, `"` and `/` are
ordinary characters. Escape special characters with `\`, or use
`lex:safe_literal(String)` to escape a whole literal string.

## 3. Escapes

Outside and inside classes:

| Escape | Bytes mode | Unicode encodings |
|---|---|---|
| `\n` `\r` `\t` `\v` `\f` | newline, CR, tab, VT, form feed | same |
| `\b` | backspace (8) | same |
| `\e` | escape (27) | same |
| `\NNN` | the octal byte `NNN` (exactly three digits, at most `\377`) | the code point `NNN` octal |
| `\xHH` | the byte `HH` (exactly two hex digits) | the code point `U+00HH` |
| `\x{H...}` | the byte `H...` (at most `FF`) | the code point `U+H...` (at most `10FFFF`, not a surrogate) |
| `\d` | DEL (127) | a decimal digit, `\p{Nd}` |
| `\D` | `D` | not a decimal digit, `\P{Nd}` |
| `\s` | space (32) | white space, `\p{White_Space}` |
| `\S` | `S` | `\P{White_Space}` |
| `\w` | `w` | a word character, `\p{Word}` |
| `\W` | `W` | `\P{Word}` |
| `\p{Name}`, `\pX` | `p{Name}`, `pX` (no meaning) | named class, see [section 6](#6-named-unicode-classes-p) |
| `\P{Name}`, `\PX` | as above | complement of a named class |
| `\c` (any other) | the character `c` | the character `c` |

The bytes-mode meanings of `\d` and `\s` come from the AWK-style regexp
syntax the lexer started with and are kept for compatibility.

Note that `\xHH` in the Unicode encodings is a code point, not a byte:
in UTF-8, `\xE9` matches `é` (the bytes `C3 A9`). Raw bytes cannot be
matched in the Unicode encodings, by design.

## 4. Character classes

`[...]` matches one character from the class; `[^...]` one character
*not* in it. The elements are:

| Element | Meaning |
|---|---|
| `c` or an escape | a single character |
| `a-z` | a range; the first character must be lower than the second (`[a-a]` is an error) |
| `[:name:]`, `[:^name:]` | a POSIX class or its complement, see [section 5](#5-posix-classes) |
| `\p{...}`, `\P{...}`, `\d`, `\s`, `\w` (and complements) | named classes; Unicode encodings only |

- `]` is literal if it is the first element: `[]a]`, `[^]a]`.
- `-` is literal at the end (`[a-]`) or next to a named class
  (`[\p{L}-]`, as in Perl).
- `[` is literal unless it starts a POSIX class: `[[:al]` matches `[`,
  `:`, `a`, `l`.
- A named class cannot be a range endpoint.

In the Unicode encodings, `[^...]` and `.` match any *character* (Unicode
scalar value) outside the class, never a part of one: in UTF-8 `[^a]`
matches `λ` as its two bytes `CE BB`, and never a lone `CE`.

In `bytes` mode a class matches one byte, or one whole non-ASCII
character written literally in it: `[a-zé]` matches a byte `a`-`z` or
the two bytes of `é`, and `[α-ω]` any Greek small letter as two bytes. A
range with a non-ASCII end is a range of characters, so its other end
must also be a character (ASCII, or literal non-ASCII). A byte escape
above `\x7F` there is an error: `[\x80-λ]`. See
[section 8](#8-bytes-mode-versus-the-unicode-encodings) for negated
classes.

## 5. POSIX classes

Inside brackets: `[[:alpha:]_]`, `[[:^digit:]]`. Outside brackets,
`[:alpha:]` is an ordinary class of the characters `:alph`.

| Name | Bytes mode (ASCII, POSIX "C" locale) | Unicode encodings (Perl's XPosix classes) |
|---|---|---|
| `alpha` | `A-Za-z` | `\p{Alphabetic}` |
| `digit` | `0-9` | `\p{Nd}` |
| `alnum` | `0-9A-Za-z` | `\p{Alphabetic}` + `\p{Nd}` |
| `upper` | `A-Z` | `\p{Uppercase}` |
| `lower` | `a-z` | `\p{Lowercase}` |
| `space` | tab, LF, VT, FF, CR, space | `\p{White_Space}` |
| `blank` | tab, space | tab + `\p{Zs}` |
| `cntrl` | 0-31, 127 | `\p{Cc}` |
| `punct` | ``!"#$%&'()*+,-./:;<=>?@[\]^_`{\|}~`` | `\p{P}` + the ASCII symbols `\p{S}` |
| `graph` | 33-126 | everything except white space, `\p{Cc}`, `\p{Cs}`, `\p{Cn}` |
| `print` | 32-126 | `graph` + `blank` - `\p{Cc}` |
| `xdigit` | `0-9A-Fa-f` | `\p{Hex_Digit}` |
| `word` | `0-9A-Za-z_` (Perl extension) | `\p{Word}` (as `\w`) |

The bytes-mode classes are the Unicode classes restricted to ASCII; the
tests check that these equal the POSIX "C" locale classes ([POSIX]
chapter 7, LC_CTYPE). `latin1` uses the same ASCII classes, the 12
classes of the POSIX standard plus `word` (see the table in
[regular-expressions.info](https://www.regular-expressions.info/posixbrackets.html)).
So a Latin-1 letter such as `é` is not `[:alpha:]`; use `\p{L}` for
that. Unknown names are an error: `{unknown_posix_class, Name}`.

The Unicode definitions are Perl's ([perlrecharclass]). They are the
"Standard Recommendation" of [UTS #18] Annex C, except for `punct`
(UTS #18: `\p{P}` only) and `xdigit` (UTS #18: also `\p{Nd}`).

## 6. Named Unicode classes (`\p{...}`)

In the Unicode encodings (and `latin1`), `\p{Name}` matches one character
with the named property and `\P{Name}` (or `\p{^Name}`) one without it.
A one-letter name can be written without braces: `\pL`, `\PN`. The
syntax follows Perl ([perlunicode], [perluniprops]), which follows
[UTS #18] RL1.2.

**Loose matching.** Names are compared ignoring case, spaces, `_` and
`-` ([UAX #44], UAX44-LM3): `\p{Uppercase_Letter}`,
`\p{uppercase letter}` and `\p{UPPERCASE-LETTER}` are the same. An
`Is` prefix is allowed: `\p{IsL}`, `\p{IsGreek}`.

**Forms.**

| Form | Property |
|---|---|
| `\p{gc=V}`, `\p{General_Category=V}`, `\p{Category=V}` | General_Category |
| `\p{sc=V}`, `\p{Script=V}` | Script |
| `\p{scx=V}`, `\p{Script_Extensions=V}` | Script_Extensions |
| `\p{blk=V}`, `\p{Block=V}`, `\p{InV}` | Block |
| `\p{B}`, `\p{B=Yes}`, `\p{B=No}` (also `Y`/`N`/`T`/`F`/`True`/`False`) | binary property `B` |
| `\p{V}` | a bare name, resolved in this order: the special names below, General_Category, Script_Extensions, binary property; then without an `Is` prefix; then `In...` as a block |
| `\p{^V}` | the complement of any of the above |

`:` can be used instead of `=`. Note that a bare script name means
Script_Extensions, as in Perl: `\p{Greek}` includes U+00B7 MIDDLE DOT,
which is `Common` but used by Greek, while `\p{sc=Greek}` does not
([UAX #24]).

**General categories** ([UAX #44], section 5.7.1): `L` (`Letter`) =
`Lu Ll Lt Lm Lo`; `LC` (`Cased_Letter`, also `L&`) = `Lu Ll Lt`;
`M` (`Mark`) = `Mn Mc Me`; `N` (`Number`) = `Nd Nl No`;
`P` (`Punctuation`) = `Pc Pd Ps Pe Pi Pf Po`; `S` (`Symbol`) =
`Sm Sc Sk So`; `Z` (`Separator`) = `Zs Zl Zp`; `C` (`Other`) =
`Cc Cf Cs Co Cn`. Long names and aliases come from
`PropertyValueAliases.txt` (`Lu` = `Uppercase_Letter`, `Nd` =
`Decimal_Number` = `digit`, `Cc` = `Control` = `cntrl`, `P` = `punct`,
...).

**Scripts**: all Script values of Unicode 17.0, by long or short
name (`Greek`/`Grek`, `Han`/`Hani`, `Common`/`Zyyy`,
`Inherited`/`Zinh`, `Unknown`/`Zzzz`, ...).

**Blocks**: all blocks, by name (`InBasic_Latin`, `\p{Block=CJK
Unified Ideographs}`) or alias (`\p{blk=ASCII}`), and `No_Block`.

**Binary properties** (from `PropList.txt` and
`DerivedCoreProperties.txt`): `ASCII_Hex_Digit`, `Alphabetic`,
`Bidi_Control`, `Case_Ignorable`, `Cased`, `Changes_When_Casefolded`,
`Changes_When_Casemapped`, `Changes_When_Lowercased`,
`Changes_When_Titlecased`, `Changes_When_Uppercased`, `Dash`,
`Default_Ignorable_Code_Point`, `Deprecated`, `Diacritic`, `Extender`,
`Grapheme_Base`, `Grapheme_Extend`, `Grapheme_Link`, `Hex_Digit`,
`Hyphen`, `IDS_Binary_Operator`, `IDS_Trinary_Operator`,
`IDS_Unary_Operator`, `ID_Compat_Math_Continue`, `ID_Compat_Math_Start`,
`ID_Continue`, `ID_Start`, `Ideographic`, `Join_Control`,
`Logical_Order_Exception`, `Lowercase`, `Math`,
`Modifier_Combining_Mark`, `Noncharacter_Code_Point`,
`Other_Alphabetic`, `Other_Default_Ignorable_Code_Point`,
`Other_Grapheme_Extend`, `Other_ID_Continue`, `Other_ID_Start`,
`Other_Lowercase`, `Other_Math`, `Other_Uppercase`, `Pattern_Syntax`,
`Pattern_White_Space`, `Prepended_Concatenation_Mark`, `Quotation_Mark`,
`Radical`, `Regional_Indicator`, `Sentence_Terminal`, `Soft_Dotted`,
`Terminal_Punctuation`, `Unified_Ideograph`, `Uppercase`,
`Variation_Selector`, `White_Space`, `XID_Continue`, `XID_Start`, with
their short aliases from `PropertyAliases.txt` (`WSpace` and `space`,
`Alpha`, `Upper`, `Lower`, `IDS`, `XIDC`, `Hex`, ...).

**Special names** (from Perl):

| Name | Matches |
|---|---|
| `Any`, `All`, `Unicode` | any character |
| `Assigned` | any assigned character (`\P{Cn}`) |
| `ASCII` | U+0000-U+007F |
| `L&` | `LC` |
| `Word` | `\w`: `Alphabetic` + `M` + `Nd` + `Pc` + `Join_Control` |
| `Alnum`, `Blank`, `XDigit`, `Graph`, `Print` | as the POSIX classes in [section 5](#5-posix-classes) |
| `HorizSpace` | `Blank` |
| `XPosixAlpha`, `XPosixDigit`, ... (any POSIX name) | the Unicode POSIX class |
| `PosixAlpha`, `PosixDigit`, ... | the ASCII POSIX class |

Unknown names give `{unknown_property, Name}`; a bad binary value
(`\p{Alpha=maybe}`) gives `{bad_property_value, Name}`.

Surrogates (`\p{Cs}`) are never matched: they are not characters and
have no well-formed encoding. A class that ends up empty never matches.

Identifier rules: [UAX #31] recommends
`\p{XID_Start}\p{XID_Continue}*` (or `ID_` versions) for identifiers;
see [example 3](README.md#examples).

## 7. Case-insensitive matching

Three ways, in every encoding:

```erlang
{regexp, "(?i)select", Action}                         % whole regexp
{regexp, "(?i:k)eep", Action}                          % part: Keep, keep
lex:regexps_to_table(Rules, [case_insensitive])        % all rules
```

`{case_insensitive, true}` is the same as `case_insensitive`. With the
option set, `(?-i)` or `(?-i:...)` makes (part of) a rule case-sensitive
again. `(?i)` is only allowed as the very first thing in a regexp (else
`{misplaced_flags, ...}`; use `(?i:...)`), and `i` is the only flag
(else `{illegal_flags, ...}`).

**Unicode encodings and latin1.** Each character and class is extended
with every character that has the same *simple case folding*
(`CaseFolding.txt`, status C and S); this is the "simple loose
matching" of [UTS #18] RL1.5. Examples:

| Regexp | Also matches |
|---|---|
| `(?i)k` | `K`, U+212A KELVIN SIGN |
| `(?i)σ` | `Σ`, `ς` (final sigma) |
| `(?i)ß` | U+1E9E `ẞ` |
| `(?i)[à-é]` | `À`-`É` |
| `(?i)\p{Lu}` | lower case letters that have an upper case form |

Negation is applied after folding (as in Perl): `(?i)[^a]`,
`(?i)\P{Lu}` and `(?i)[[:^upper:]]` exclude both cases. In `latin1`,
folds outside 0-255 are dropped (`ÿ` does not match U+0178 `Ÿ`, which
`latin1` cannot represent).

Not done: multi-character (full) case folding, so `(?i)ss` does not
match `ß` and `(?i)ß` does not match `SS`; and the Turkic mappings
(status T), so `(?i)i` matches `I` but not `İ`.

**Bytes mode.** Only `A-Z` and `a-z` are folded. Other bytes may be
parts of UTF-8 characters, so `(?i)café` matches `CAFé` but not
`CAFÉ`.

## 8. Bytes mode versus the Unicode encodings

The encoding is chosen with `{encoding, Enc}` (default `bytes`); see
[README.md](README.md#encodings).

| | `bytes` (default) | `latin1` | `utf8`, `utf16le/be`, `utf32le/be` |
|---|---|---|---|
| regexp characters | ASCII characters and escapes are bytes; a non-ASCII character is its UTF-8 bytes, as a unit | latin1 only (0-255); others are an error | code points 0-10FFFF |
| `\x{...}` | up to `FF` | up to `FF` | up to `10FFFF`, no surrogates |
| `\d \s \w` | DEL, space, `w` | Unicode classes | Unicode classes |
| `\p{...}` | not available | its latin1 members; none: an error | yes |
| POSIX classes | ASCII | ASCII | Unicode |
| case folding | ASCII | Unicode, restricted to 0-255 | Unicode |
| `.`, `[^...]` | one byte | one byte | one whole character |
| input | bytes (string of chars 0-255, or binary) | bytes | bytes in that encoding |
| action argument | reversed bytes | reversed code points (= bytes) | reversed code points |
| malformed input | not checked | not applicable | rejected, see [README.md](README.md#errors) |

A character that the encoding cannot represent is an error:
`{unencodable_char, CodePoint, Enc}`, for example a surrogate in the
Unicode encodings.

**`latin1` regexps use latin1 only.** In `{encoding, latin1}`, every
character in a regexp must be in Latin-1 (0-255), and every class must
have characters in Latin-1. See [Classes in latin1](#classes-in-latin1).

**Non-ASCII characters in `bytes` mode.** The lexer reads bytes, so a
non-ASCII character in a regexp stands for its UTF-8 bytes. They form
one unit: a quantifier applies to all of them (`λ+` is `(CE BB)+` and
matches `λλλ`), and `[λμ]` matches either character. Byte escapes stay
single bytes (`\xE9+` is one or more bytes `E9`), and with
`{regexp_encoding, latin1}` every character is one byte. Case folding
still covers ASCII only. The input is not checked, so a regexp of bytes,
such as `.` or `\xC3`, can match part of a character.

**Negated classes in `bytes` mode.** A negated class such as `[^é]`
matches one byte, so it cannot mean "any character but `é`" when `é` is
two bytes. It could mean "any byte but the first byte of `é`" or "any
byte but its bytes", but neither is what the regexp says. So a non-ASCII
character in a negated class is an error,
`{non_ascii_in_negated_class, Chars}`. ASCII members and byte escapes
are allowed (`[^a\xE9]`). This choice may be revisited.

The other encodings do not have this problem, because the lexer
decodes the input and a negated class matches one whole character. In
`utf8`, `utf16le/be` and `utf32le/be`, `[^é]` matches any character
except `é`, as all of its bytes and never part of one (see
[section 4](#4-character-classes)). In `latin1`, every character is one
byte, so `[^é]` matches any byte except `E9`. To lex UTF-8 text with
negated classes of non-ASCII characters, use `{encoding, utf8}` rather
than `bytes`.

### Classes in latin1

With `{encoding, latin1}` the lexer reads one byte per character, and a
regexp may only use the 256 Latin-1 characters (U+0000-U+00FF). Classes
work as in the Unicode encodings, cut down to those characters, with
two kinds of error for what latin1 cannot express. Each error carries
a `Note` saying to use `{encoding, utf8}` (or `utf16`, `utf32`) instead.

**Characters and bracket classes.** A character written in a regexp,
on its own, in `[...]` or `[^...]`, or as a range end, must be at most
255. Escapes work the same way: `\xHH`, `\NNN` and `\x{...}` up to
`\x{FF}`.

| Regexp | Matches |
|---|---|
| `[a-zé]` | `a`-`z` or `é` |
| `[^é]` | any byte except `E9` |
| `[\xC0-\xFF]` | `À`-`ÿ` |
| `.` | any byte except `\n` |
| `λ`, `[λ]`, `[^λ]`, `[a-λ]`, `[^\x{100}]` | error: `{unencodable_char, CodePoint, latin1, Note}` |

**POSIX classes** are the ASCII ones, as in `bytes` mode: the 12
classes of the POSIX standard, and `word`. They hold no Latin-1 letters,
so `[[:alpha:]]` is `[A-Za-z]`. A negated POSIX class holds every other
byte, so `[[:^alpha:]]` includes `é`. The definitions are those of
[regular-expressions.info](https://www.regular-expressions.info/posixbrackets.html),
and the tests check each one over all 256 bytes:

| Class | Matches | Class | Matches |
|---|---|---|---|
| `[:alnum:]` | `[a-zA-Z0-9]` | `[:lower:]` | `[a-z]` |
| `[:alpha:]` | `[a-zA-Z]` | `[:print:]` | `[\x20-\x7E]` |
| `[:blank:]` | space, tab | `[:punct:]` | the 32 ASCII punctuation and symbol characters: `!` to `/`, `:` to `@`, `[` to `` ` ``, `{` to `~` |
| `[:cntrl:]` | `[\x00-\x1F\x7F]` | `[:space:]` | space, `\t\r\n\v\f` |
| `[:digit:]` | `[0-9]` | `[:upper:]` | `[A-Z]` |
| `[:graph:]` | `[\x21-\x7E]` | `[:xdigit:]` | `[A-Fa-f0-9]` |
| `[:word:]` | `[A-Za-z0-9_]` (not POSIX) | | |

**Perl and named classes** (`\d \s \w`, `\p{...}`) use the Unicode
definitions (section 6), cut down to their Latin-1 members:

| Class | Latin-1 members |
|---|---|
| `\p{L}` | `A`-`Z`, `a`-`z`, `ª`, `µ`, `º`, `À`-`ÿ` except `×` and `÷` |
| `\p{Lu}` | `A`-`Z`, `À`-`Þ` except `×` |
| `\d` | `0`-`9` |
| `\s` | tab, LF, VT, FF, CR, space, NEL (`\x85`), no-break space (`\xA0`) |
| `\w` | `\p{L}`, `0`-`9` and `_` |
| `\p{Latin}` | the letters of `\p{L}` except `µ` (whose script is Common), and `·` (Script_Extensions) |
| `\p{InBasicLatin}`, `\p{InLatin1}` | `\x00`-`\x7F`, `\x80`-`\xFF` |

A named class with **no characters of its own** in Latin-1 is an
error: `{unencodable_class, Class, latin1, Note}`. Examples are
`\p{Greek}`, `\p{Han}`, `\p{Cyrillic}` and `\p{InCyrillic}`, and they
are rejected inside brackets too. A script is judged by its own
characters (`sc`, the Script property). A bare script name means
Script_Extensions ([section 6](#6-named-unicode-classes-p)), which adds
characters shared with other scripts, such as `·` (U+00B7) for Greek and
Han. Those shared characters do not count, so `\p{Greek}` is an error
rather than a class of `·` alone.

How the check is made depends on the class:

- **On its own, and in a positive bracket class**, each named class must
  have Latin-1 characters of its own, so `[a\p{Greek}]` is an error even
  though `a` is fine. `\P{Greek}` and `\p{^Greek}` are allowed: they
  have Latin-1 members, everything except `·`.
- **In a negated bracket class** the elements say what to exclude, so
  the class is checked as a whole, after the `^`. `[^\p{Greek}]` is
  allowed and means the same as `\P{Greek}`. `[^\P{Greek}]` would match
  only Greek characters, so it is an error, like `\p{Greek}`.
- A class with no characters in any encoding, such as
  `[^[:alpha:][:^alpha:]]`, is not a latin1 error. Like any empty class,
  it never matches.

**Case-insensitive matching** uses Unicode case folding, cut down to
Latin-1 ([section 7](#7-case-insensitive-matching)). A character whose
other case is outside Latin-1 matches only itself:

| Regexp | Matches |
|---|---|
| `(?i)é` | `é`, `É` |
| `(?i)k` | `k`, `K` (not U+212A KELVIN SIGN) |
| `(?i)ÿ` | `ÿ` only (`Ÿ` is U+0178) |
| `(?i)ß`, `(?i)µ` | themselves only |
| `(?i)[[:upper:]]` | `A`-`Z`, `a`-`z` (POSIX classes are ASCII) |
| `(?i)\p{Lu}` | the letters of `\p{L}` except `ª` and `º`, which have no case |

Folding comes before the check, so `(?i)\p{Greek}` is allowed: Greek
`μ` folds to `µ` (U+00B5, MICRO SIGN), which is in Latin-1. It matches
`µ` and `·`.

## 9. Not supported

These parse as errors or are simply not in the language:

- anchors `^` and `$` (parsed, then rejected with
  `{regexp_not_handled, bos | eos}`)
- empty regexps and empty alternatives: `""`, `"a|"`, `"()"`, `"(|a)"`,
  `"a||b"` (rejected with `{regexp_not_handled, epsilon}`)
- counted repetition `r{n,m}` (`{` is an ordinary character)
- lazy and possessive quantifiers, backreferences, lookahead and
  lookbehind, capture groups (a lexer has no use for captures: the
  action gets the whole token)
- inline flags other than `i`; `(?i)` other than at the start
- `\b`/`\B` word boundaries (`\b` is backspace), `\X` grapheme clusters,
  `\R`, `\h`, `\N{NAME}`, `\N{U+...}` (these are literal letters)
- set operations in classes (`[\p{L}--\p{Lu}]`, `[\p{L}&&\p{Greek}]`,
  [UTS #18] RL1.3)
- other properties: non-binary ones besides General_Category, Script,
  Script_Extensions and Block (e.g. `Bidi_Class`, `Line_Break`,
  `East_Asian_Width`, `Numeric_Value`, `Age`), emoji properties
  (`Emoji`, `Extended_Pictographic`), string properties (`RGI_Emoji`),
  character names
- canonical equivalence: `é` (U+00E9) and `e` + U+0301 are different

See [README.md, "Limitations and future work"](README.md#limitations-and-future-work).

[perlunicode]: https://perldoc.perl.org/perlunicode
[perluniprops]: https://perldoc.perl.org/perluniprops
[perlrecharclass]: https://perldoc.perl.org/perlrecharclass
[UTS #18]: https://www.unicode.org/reports/tr18/
[UAX #44]: https://www.unicode.org/reports/tr44/
[UAX #24]: https://www.unicode.org/reports/tr24/
[UAX #31]: https://www.unicode.org/reports/tr31/
[POSIX]: https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap07.html
