%%% The documentation examples (lex_examples.erl), checked. Run with
%%% `make test`.

-module(lex_examples_tests).
-include_lib("eunit/include/eunit.hrl").

-import(lex_examples, [calc/1, config/1, idents/1, sql/1, utf16_file/1,
                       scripts/1, tokens_one_by_one/1, safe_lex/1]).

calc_test() ->
    ?assertEqual([{num, 1, "12"}, {'+', 1}, {'(', 1}, {num, 1, "3"}, {'*', 1},
                  {num, 1, "4"}, {')', 1}],
                 calc("12 + (3*4)")).

config_test() ->
    ?assertEqual([{section, 1, "main"}, {key, 2, "name"}, {'=', 2}, {key, 2, "demo"}],
                 config("[main]\nname = demo # comment\n")).

idents_test() ->
    ?assertEqual([{ident, 1, "λx"}, {'=', 1}, {string, 1, "naïve"}, {'+', 1}, {num, 1, "٣٤"}],
                 idents(<<"λx = \"naïve\" + ٣٤"/utf8>>)).

sql_test() ->
    ?assertEqual([select, {ident, "name"}, from, {ident, "t"}, where, {ident, "selected"}],
                 sql(<<"Select name FROM t WHERE selected">>)).

utf16_file_test() ->
    Bin = unicode:characters_to_binary([16#FEFF | "Grüße Welt"], unicode, {utf16, little}),
    ?assertEqual([{word, 1, "Grüße"}, {word, 1, "Welt"}], utf16_file(Bin)).

scripts_test() ->
    ?assertEqual([{greek, "Ἀθῆναι"}, {latin, "Athens"}, {cyrillic, "Афины"}, {kana, "アテネ"}],
                 scripts(<<"Ἀθῆναι Athens Афины アテネ"/utf8>>)).

one_by_one_test() ->
    ?assertEqual([{ident, 1, "a1"}, {ident, 1, "b2"}], tokens_one_by_one(<<"a1 b2">>)).

safe_lex_test() ->
    ?assertEqual({error, {invalid_utf8, {line, 1}, [255]}}, safe_lex(<<"ok", 16#FF>>)),
    ?assertEqual({error, {truncated, {line, 1}, [226, 130]}}, safe_lex(<<"ok ", 16#E2, 16#82>>)),
    ?assertEqual({error, {unexpected, {line, 1}, "?"}}, safe_lex(<<"ok ?">>)),
    ?assertEqual({ok, [{ident, 1, "ok"}]}, safe_lex(<<"ok">>)).
