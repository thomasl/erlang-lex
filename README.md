# Erlang-Lex

A table-driven lexer generator in Erlang.

Give it a list of rules (regular expression → action); it builds a DFA
at run time (regexp → NFA → DFA → minimized DFA → table, as in the
Dragon book) and splits input into tokens by longest match, like Unix
`lex`/`flex`. If several rules match the same longest token, the rule
with the highest priority wins, then the earliest rule.

```erlang
1> lex:with([{regexp, "\\p{L}+", lex:item(word)},
             {regexp, "\\s+", lex:whitespace()}],
            <<"Grüße мир"/utf8>>, [{encoding, utf8}]).
[{word,1,"Grüße"},{word,1,"мир"}]
```

## Features

- Input as bytes, Latin-1, UTF-8, UTF-16 LE/BE or UTF-32 LE/BE. The
  lexer reads one byte at a time and rejects malformed or truncated
  input with a precise error.
- Binaries and iolists (nested lists of bytes and binaries) are lexed in
  place, without flattening or converting them.
- Incremental lexing of data that arrives in chunks (sockets, files):
  feed chunks to a non-blocking continuation as they arrive (e.g. in a
  `gen_server`), or let the lexer call a function for more data.
- Named Unicode classes in Perl syntax (`\p{L}`, `\p{Greek}`,
  `\p{sc=Grek}`, `\p{InCyrillic}`, `\p{XID_Start}`, `\P{...}`), `\d \s \w`,
  `\x{1F600}`.
- POSIX classes (`[[:alpha:]]`, `[[:^digit:]]`).
- Case-insensitive matching (`(?i)`, `(?i:...)`, option `case_insensitive`).
- Line and column counting by the lexer, with the Unicode line
  terminators (CR LF, NEL, U+2028, ...); errors report the line and
  column.
- Unicode 17.0.0 data, regenerated from the UCD with `make unicode-data`.

## Build and test

```sh
make
make test
```

## Documentation

- [doc/README.md](doc/README.md): API, usage, examples, how it works,
  references, limitations and future work
- [doc/syntax.md](doc/syntax.md): the rule and regular expression syntax
- [src/lex_examples.erl](src/lex_examples.erl): the documented examples
  (tested)

Building the table dynamically is fast enough for most uses (a few
milliseconds for a programming language lexer); use
`lex:with(Rules, Input)` or keep the result of `lex:regexps_to_table/2`.

## License

MIT; see [LICENSE](LICENSE).
