%%% Examples for doc/README.md and doc/syntax.md.
%%%
%%% Every example here is run by lex_examples_tests (make test), so the
%%% documentation cannot drift from what the code does.

-module(lex_examples).

-export([calc/1,
         config/1,
         idents/1,
         sql/1,
         utf16_file/1,
         scripts/1,
         tokens_one_by_one/1,
         safe_lex/1,
         positions/1,
         iodata/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. A calculator lexer in the default (bytes) encoding.
%%
%%   lex_examples:calc("12 + (3*4)")
%%   => [{num,1,"12"},{'+',1},{'(',1},{num,1,"3"},{'*',1},{num,1,"4"},{')',1}]

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. POSIX classes in bytes mode: an ini-style config file.
%%
%%   lex_examples:config("[main]\nname = demo # comment\n")
%%   => [{section,1,"main"},{key,2,"name"},{'=',2},{key,2,"demo"}]

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. Unicode identifiers (UAX #31), numbers and strings in UTF-8.
%%
%%   lex_examples:idents(<<"λx = \"naïve\" + ٣٤"/utf8>>)
%%   => [{ident,1,"λx"},{'=',1},{string,1,"naïve"},{'+',1},{num,1,"٣٤"}]
%%
%% Tokens are code point lists, whatever the input encoding.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4. Case-insensitive keywords that win over identifiers.
%%
%%   lex_examples:sql(<<"Select name FROM t WHERE selected">>)
%%   => [select,{ident,"name"},from,{ident,"t"},where,{ident,"selected"}]
%%
%% "Select" matches both the keyword rule and the identifier rule with the
%% same length; the keyword has priority 1, so it wins. "selected" is
%% longer than any keyword match, so it is an identifier (longest match).

sql(Input) ->
    Kw = fun(Word) -> {regexp, 1, "(?i)" ++ Word, lex:raw_token(list_to_atom(Word))} end,
    Rules = [Kw("select"), Kw("from"), Kw("where"),
             {regexp, "[[:alpha:]_][[:alnum:]_]*", fun(Acc) -> {token, {ident, lists:reverse(Acc)}} end},
             {regexp, "[[:space:]]+", lex:whitespace()}],
    lex:with(Rules, Input, [{encoding, utf8}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5. A UTF-16LE file, as written by many Windows tools, with a byte
%%    order mark. The BOM is U+FEFF, an ordinary character to the lexer,
%%    so we skip it with a rule (or strip it from the binary first).
%%
%%   lex_examples:utf16_file(unicode:characters_to_binary(
%%       [16#FEFF | "Grüße Welt"], unicode, {utf16, little}))
%%   => [{word,1,"Grüße"},{word,1,"Welt"}]

utf16_file(Bin) ->
    Rules = [{regexp, "\\x{FEFF}", lex:no_token()},
             {regexp, "\\w+", lex:item(word)},
             {regexp, "\\s+", lex:whitespace()}],
    lex:with(Rules, Bin, [{encoding, utf16le}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6. Scripts and blocks.
%%
%%   lex_examples:scripts(<<"Ἀθῆναι Athens Афины アテネ"/utf8>>)
%%   => [{greek,"Ἀθῆναι"},{latin,"Athens"},{cyrillic,"Афины"},{kana,"アテネ"}]

scripts(Utf8) ->
    Word = fun(Tag) -> fun(Acc) -> {token, {Tag, lists:reverse(Acc)}} end end,
    Rules = [{regexp, "\\p{sc=Greek}+", Word(greek)},
             {regexp, "\\p{Latin}+", Word(latin)},
             {regexp, "\\p{Script=Cyrillic}+", Word(cyrillic)},
             {regexp, "[\\p{InKatakana}\\p{Hiragana}]+", Word(kana)},
             {regexp, " ", lex:whitespace()}],
    lex:with(Rules, Utf8, [{encoding, utf8}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7. Pulling tokens one at a time with one/2, e.g. from a parser.
%%    Build the table once and reuse it.
%%
%%   lex_examples:tokens_one_by_one(<<"a1 b2">>)
%%   => [{ident,1,"a1"},{ident,1,"b2"}]

tokens_one_by_one(Input) ->
    Lex = lex:regexps_to_table(ident_rules(), [{encoding, utf8}]),
    lex:no_lines(),
    pull(Lex, Input, []).

pull(Lex, Input, Acc) ->
    case lex:one(Lex, Input) of
        {token, T, Rest} -> pull(Lex, Rest, [T | Acc]);
        end_of_stream -> lists:reverse(Acc)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8. Handling errors. Errors give the line and column, counted in
%%    characters from 1.
%%
%%   lex_examples:safe_lex(<<"ok", 16#FF>>)
%%   => {error, {invalid_utf8, {1, 3}, [255]}}
%%   lex_examples:safe_lex(<<"ok\nλ ", 16#E2, 16#82>>)   % truncated "€"
%%   => {error, {truncated, {2, 3}, [226,130]}}
%%   lex_examples:safe_lex(<<"ok ?">>)
%%   => {error, {unexpected, {1, 4}}}

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9. Tokens with their position {Line, Column}, as in doc/README.md,
%%    "Line and column counting".
%%
%%   lex_examples:positions(<<"ab\ncd λ"/utf8>>)
%%   => [{word,{1,1},"ab"},{word,{2,1},"cd"},{word,{2,4},"λ"}]

positions(Utf8) ->
    lex:with([{regexp, "\\p{L}+", at(word)}, {regexp, "\\s+", lex:whitespace()}],
             Utf8, [{encoding, utf8}]).

at(Tag) -> fun(Acc) -> {token, {Tag, lex:current_position(), lists:reverse(Acc)}} end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10. Iodata input: nested lists of bytes and binaries are lexed as
%%     they are, as in doc/README.md, "Iolist input".
%%
%%   lex_examples:iodata([<<"λx = "/utf8>>, ["42", $\s], [[<<"+ 1">>]]])
%%   => [{ident,1,"λx"},{'=',1},{num,1,"42"},{'+',1},{num,1,"1"}]

iodata(IoData) ->
    lex:with(ident_rules(), IoData, [{encoding, utf8}]).
