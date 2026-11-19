%%% Tests for lex_spec_parse (the regexp parser).

-module(lex_spec_parse_tests).

-include_lib("eunit/include/eunit.hrl").

-define(B(S), lex_spec_parse:string(S, bytes)).
-define(U(S), lex_spec_parse:string(S, unicode)).

operators_test_() ->
    [?_assertEqual({ok, $a}, ?B("a")),
     ?_assertEqual({ok, {concat, {concat, $a, $b}, $c}}, ?B("abc")),
     ?_assertEqual({ok, {'or', {'or', $a, $b}, $c}}, ?B("a|b|c")),
     ?_assertEqual({ok, {'or', {concat, $a, $b}, $c}}, ?B("ab|c")),
     ?_assertEqual({ok, {optional, {pclosure, {kclosure, $a}}}}, ?B("a*+?")),
     ?_assertEqual({ok, {concat, {kclosure, {'or', $a, $b}}, $c}}, ?B("(a|b)*c")),
     ?_assertEqual({ok, {comp_class, "\n"}}, ?B(".")),
     ?_assertEqual({ok, {concat, {concat, bos, $a}, eos}}, ?B("^a$")),
     ?_assertEqual({ok, {concat, {concat, $a, ${}, $}}}, ?B("a{}"))].

empty_test_() ->
    [?_assertEqual({ok, epsilon}, ?B("")),
     ?_assertEqual({ok, {'or', $a, epsilon}}, ?B("a|")),
     ?_assertEqual({ok, {'or', epsilon, $a}}, ?B("(|a)")),
     ?_assertEqual({ok, epsilon}, ?B("()"))].

escapes_test_() ->
    [?_assertEqual({ok, {concat, $\n, $\t}}, ?B("\\n\\t")),
     ?_assertEqual({ok, $A}, ?B("\\101")),
     ?_assertEqual({ok, $A}, ?B("\\x41")),
     ?_assertEqual({ok, 16#3BB}, ?U("\\x{3bb}")),
     ?_assertEqual({ok, $*}, ?B("\\*")),
     %% bytes mode keeps the AWK meanings
     ?_assertEqual({ok, {concat, {concat, 127, $\s}, $w}}, ?B("\\d\\s\\w")),
     ?_assertEqual({ok, {concat, $p, $L}}, ?B("\\pL")),
     ?_assertEqual({ok, {concat, {prop, false, "Nd"}, {prop, true, "Word"}}}, ?U("\\d\\W")),
     ?_assertEqual({ok, {concat, {prop, false, "L"}, {prop, true, "Greek"}}}, ?U("\\pL\\P{Greek}"))].

bytes_mode_is_utf8_test_() ->
    [?_assertEqual({ok, {concat, 16#CE, 16#BB}}, ?B([16#3BB])),
     ?_assertEqual({ok, 16#3BB}, ?U([16#3BB])),
     ?_assertEqual({ok, 16#3BB}, ?U(<<"λ"/utf8>>))].

classes_test_() ->
    [?_assertEqual({ok, {char_class, [{$a, $z}, $_]}}, ?B("[a-z_]")),
     ?_assertEqual({ok, {comp_class, [$], $a]}}, ?B("[^]a]")),
     ?_assertEqual({ok, {char_class, [$a, $-]}}, ?B("[a-]")),
     ?_assertEqual({ok, {char_class, [{posix, false, "alpha"}, {posix, true, "digit"}]}},
                   ?B("[[:alpha:][:^digit:]]")),
     ?_assertEqual({ok, {char_class, "[:al"}}, ?B("[[:al]")),
     ?_assertEqual({ok, {char_class, [{prop, false, "L"}, $-, $x]}}, ?U("[\\p{L}-x]")),
     ?_assertEqual({ok, {char_class, [{$\0, 16#FF}]}}, ?B("[\\x00-\\x{ff}]"))].

flags_test_() ->
    [?_assertEqual({ok, {caseless, true, {concat, $a, $b}}}, ?B("(?i)ab")),
     ?_assertEqual({ok, {caseless, false, $a}}, ?B("(?-i)a")),
     ?_assertEqual({ok, {concat, {caseless, true, $a}, $b}}, ?B("(?i:a)b"))].

errors_test_() ->
    E = fun(S) -> element(1, element(2, ?U(S))) end,
    [?_assertEqual({error, {illegal, ")"}}, ?B("a)")),
     ?_assertEqual({error, {illegal, ")"}}, ?B(")")),
     ?_assertEqual({error, {illegal, "*"}}, ?B("*a")),
     ?_assertEqual({error, {illegal, "]"}}, ?B("]")),
     ?_assertEqual({error, {unterminated, "("}}, ?B("(a")),
     ?_assertEqual({error, {unterminated, "["}}, ?B("[a")),
     ?_assertEqual({error, {unterminated, "\\"}}, ?B("a\\")),
     ?_assertEqual({error, {char_class, "b-a"}}, ?B("[b-a]")),
     ?_assertEqual({error, {char_class, "a-a"}}, ?B("[a-a]")),
     ?_assertEqual({error, {illegal, "\\777"}}, ?B("\\777")),
     ?_assertEqual({error, {illegal, "}"}}, ?B("\\x{100}")),
     ?_assertEqual({error, {illegal, "}"}}, ?U("\\x{110000}")),
     ?_assertEqual({error, {illegal, "}"}}, ?U("\\x{}")),
     ?_assertEqual({error, {unterminated, "\\x{"}}, ?U("\\x{12")),
     ?_assertEqual({error, {illegal, "x"}}, ?U("\\xg")),
     ?_assertEqual({error, {illegal, "p{}"}}, ?U("\\p{}")),
     ?_assertEqual({error, {unterminated, "\\p{"}}, ?U("\\p{L")),
     ?_assertEqual({error, {unterminated, "\\p"}}, ?U("\\p")),
     ?_assertEqual({error, {unterminated, "(?"}}, ?U("(?i:a")),
     ?_assertEqual(misplaced_flags, E("a(?i)b")),
     ?_assertEqual(illegal_flags, E("(?x)a")),
     ?_assertEqual(illegal_flags, E("a(?s:b)")),
     ?_assertMatch({error, {bad_characters, _}}, ?U([16#110000]))].

format_error_test() ->
    Reasons = [element(2, R) || R <- [?B("a)"), ?B("(a"), ?B("[a-a]"), ?B("a(?i)b"),
                                      ?B("(?x)"), ?U([16#110000])]],
    [?assert(is_list(lists:flatten(lex_spec_parse:format_error(R)))) || R <- Reasons].
