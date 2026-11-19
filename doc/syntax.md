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

Regexps are given as Erlang strings, so every backslash in this document
is written twice in Erlang source: the regexp `\p{L}+` is the Erlang
string `"\\p{L}+"`, and `\.` is `"\\."`.

## 1. Rules

A specification is a list of rules:

```erlang
{regexp, Regexp, Action}
{regexp, Priority, Regexp, Action}
```

- `Regexp` is a string (or, in the Unicode encodings, also a UTF-8
  binary) in the syntax below.
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
chapter 7, LC_CTYPE). In `latin1`, the Unicode
definitions are used for the code points 0-255. Unknown names are an
error: `{unknown_posix_class, Name}`.

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
| regexp characters | bytes; non-ASCII characters in the regexp are converted to their UTF-8 bytes | code points 0-255 | code points 0-10FFFF |
| `\x{...}` | up to `FF` | up to `10FFFF` (above `FF` is an error) | up to `10FFFF`, no surrogates |
| `\d \s \w` | DEL, space, `w` | Unicode classes | Unicode classes |
| `\p{...}` | not available | yes (restricted to 0-255) | yes |
| POSIX classes | ASCII | Unicode, restricted to 0-255 | Unicode |
| case folding | ASCII | Unicode, restricted to 0-255 | Unicode |
| `.`, `[^...]` | one byte | one byte | one whole character |
| input | bytes (string of chars 0-255, or binary) | bytes | bytes in that encoding |
| action argument | reversed bytes | reversed code points (= bytes) | reversed code points |
| malformed input | not checked | not applicable | rejected, see [README.md](README.md#errors) |

A literal that the encoding cannot represent is an error:
`{unencodable_char, CodePoint, Enc}`.

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
