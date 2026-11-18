%%% Tests for the Unicode support in lex. Run with `make test`.

-module(lex_unicode_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ENCODINGS, [utf8, {utf16, big}, {utf16, little}, {utf32, big}, {utf32, little}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

table(Regexp, Enc) ->
    lex:regexps_to_table([{regexp, Regexp, lex:item(t)}], [{encoding, Enc}]).

%% Run the DFA over Bytes; true if it ends in an accepting state.
accepts({lex, Start, AccLim, Table, _Actions, _Enc, _NL}, Bytes) ->
    walk(Start, AccLim, Table, Bytes).

walk(S, AccLim, _Table, []) ->
    S =< AccLim;
walk(S, AccLim, Table, [B | Bs]) ->
    case element(B + 1, element(S, Table)) of
        -1 -> false;
        N -> walk(N, AccLim, Table, Bs)
    end.

%% Number of distinct byte strings accepted by an acyclic table.
count_accepted({lex, Start, AccLim, Table, _Actions, _Enc, _NL}) ->
    {N, _} = count(Start, AccLim, Table, #{}),
    N.

count(S, AccLim, Table, Memo) ->
    case Memo of
        #{S := N} -> {N, Memo};
        _ ->
            Row = tuple_to_list(element(S, Table)),
            {N, Memo1} =
                lists:foldl(fun(-1, Acc) -> Acc;
                               (Nxt, {K, M}) ->
                                    {K1, M1} = count(Nxt, AccLim, Table, M),
                                    {K + K1, M1}
                            end,
                            {case S =< AccLim of true -> 1; false -> 0 end, Memo},
                            Row),
            {N, Memo1#{S => N}}
    end.

enc(latin1, Cp) -> [Cp];
enc(Enc, Cp) -> binary_to_list(unicode:characters_to_binary([Cp], unicode, Enc)).

in_set(Cp, [{F, T} | _]) when Cp >= F, Cp =< T -> true;
in_set(Cp, [{_, T} | Rs]) when Cp > T -> in_set(Cp, Rs);
in_set(_, _) -> false.

set_size(Set) -> lists:sum([ T - F + 1 || {F, T} <- Set ]).

%% The table for a single-character class accepts exactly the encodings
%% of the scalar values in Set:
%% - every scalar value is accepted iff it is in Set (exhaustive), and
%% - the table accepts exactly as many byte strings as there are such
%%   scalar values, so it accepts nothing else (no malformed input).
check_class(Regexp, Set0, Enc) ->
    Universe = case Enc of
                   latin1 -> [{0, 255}];
                   _ -> lex_unicode:scalar_values()
               end,
    Set = lex_unicode:intersect(lex_unicode:normalize(Set0), Universe),
    Lex = table(Regexp, Enc),
    Wrong = [ Cp || {F, T} <- Universe, Cp <- lists:seq(F, T),
                    accepts(Lex, enc(Enc, Cp)) =/= in_set(Cp, Set) ],
    ?assertEqual({Regexp, Enc, []}, {Regexp, Enc, lists:sublist(Wrong, 10)}),
    ?assertEqual({Regexp, Enc, set_size(Set)}, {Regexp, Enc, count_accepted(Lex)}).

prop(Name) ->
    {ok, Set} = lex_unicode:property(Name),
    Set.

rules() ->
    [{regexp, "\\p{XID_Start}\\p{XID_Continue}*", lex:item(ident)},
     {regexp, "\\d+", lex:item(num)},
     {regexp, "\\p{Sm}|\\p{So}", lex:item(sym)},
     {regexp, "\"[^\"]*\"", lex:item(str)},
     {regexp, "\\s+", lex:whitespace()}].

lex(Bytes, Enc) ->
    caught(fun() -> lex:with(rules(), Bytes, [{encoding, Enc}]) end).

caught(F) ->
    try F() catch exit:Reason -> {'EXIT', Reason} end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lowering: exhaustive over all code points

any_char_test_() ->
    {timeout, 120,
     [ fun() -> check_class("[\\x{0}-\\x{10FFFF}]", [{0, 16#10FFFF}], Enc) end
       || Enc <- [latin1 | ?ENCODINGS] ]}.

dot_test_() ->
    {timeout, 120,
     [ fun() -> check_class(".", lex_unicode:complement([{$\n, $\n}]), Enc) end
       || Enc <- ?ENCODINGS ]}.

letter_test_() ->
    {timeout, 120,
     [ fun() -> check_class("\\p{L}", prop("L"), Enc) end
       || Enc <- [latin1 | ?ENCODINGS] ]}.

not_greek_test_() ->
    {timeout, 120,
     [ fun() -> check_class("[^\\p{Greek}\\d]", lex_unicode:complement(
                                                  lex_unicode:union(prop("Greek"), prop("Nd"))), Enc) end
       || Enc <- ?ENCODINGS ]}.

%% Ranges that straddle every encoding boundary
boundary_ranges_test_() ->
    Ranges = [{16#7E, 16#81}, {16#7FE, 16#801}, {16#D7FE, 16#E001},
              {16#FFFE, 16#10001}, {16#103FE, 16#10401}, {16#10FFFE, 16#10FFFF}],
    {timeout, 120,
     [ fun() ->
               Re = lists:flatten([ io_lib:format("[\\x{~.16b}-\\x{~.16b}]", [F, T]) ]),
               check_class(Re, [{F, T}], Enc)
       end || {F, T} <- Ranges, Enc <- ?ENCODINGS ]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Named classes

property_names_test() ->
    Greek = prop("sc=Greek"),
    ?assertEqual(Greek, prop("Script=Greek")),
    ?assertEqual(Greek, prop("script:grek")),
    ?assertEqual(prop("L"), prop("Letter")),
    ?assertEqual(prop("L"), prop("gc=L")),
    ?assertEqual(prop("L"), prop("General_Category = letter")),
    ?assertEqual(prop("L"), prop("IsL")),
    ?assertEqual(prop("Lu"), prop("Uppercase-Letter")),
    ?assertEqual(prop("LC"), prop("L&")),
    ?assertEqual(prop("White_Space"), prop("space")),
    ?assertEqual(prop("White_Space"), prop("WSpace")),
    ?assertEqual(prop("Alphabetic"), prop("Alpha=Yes")),
    ?assertEqual(lex_unicode:complement(prop("Alphabetic")), prop("Alpha=No")),
    ?assertEqual(lex_unicode:complement(prop("L")), prop("^L")),
    ?assertEqual(prop("InGreek_and_Coptic"), prop("Block=Greek")),
    ?assertEqual([{16#370, 16#3FF}], prop("InGreek")),
    ?assertEqual([{0, 127}], prop("ASCII")),
    ?assertEqual({error, {unknown_property, "NoSuchThing"}},
                 lex_unicode:property("NoSuchThing")),
    ?assertEqual({error, {bad_property_value, "Alpha=maybe"}},
                 lex_unicode:property("Alpha=maybe")).

property_data_test() ->
    Has = fun(Cp, Name) -> in_set(Cp, prop(Name)) end,
    ?assert(Has($A, "Lu")),
    ?assert(Has(16#E9, "Ll")),             % é
    ?assert(Has(16#3BB, "Greek")),         % λ
    ?assert(Has(16#4E2D, "Han")),          % 中
    ?assert(Has(16#663, "Nd")),            % ARABIC-INDIC DIGIT THREE
    ?assert(Has(16#378, "Cn")),            % unassigned
    ?assert(Has(16#1F600, "So")),          % 😀
    ?assert(Has(16#D800, "Cs")),
    ?assert(Has($_, "Word")),
    ?assertNot(Has($-, "Word")),
    %% U+00B7 MIDDLE DOT is Common, with Greek among its extensions;
    %% like Perl, \p{Greek} means Script_Extensions
    ?assert(Has(16#B7, "Greek")),
    ?assert(Has(16#B7, "scx=Greek")),
    ?assertNot(Has(16#B7, "sc=Greek")),
    ?assert(Has(16#B7, "sc=Common")).

%% \P{X}, \p{^X} and [^\p{X}] are the same class
negation_forms_test() ->
    T = fun(Re) -> element(4, table(Re, utf8)) end,
    ?assertEqual(T("\\P{L}"), T("\\p{^L}")),
    ?assertEqual(T("\\P{L}"), T("[^\\p{L}]")),
    ?assertEqual(T("\\PL"), T("\\P{L}")),
    ?assertEqual(T("\\D"), T("\\P{Nd}")).

regexp_errors_test() ->
    ?assertMatch({'EXIT', {unknown_property, "Klingon"}}, caught(fun() -> table("\\p{Klingon}", utf8) end)),
    ?assertMatch({'EXIT', {unencodable_char, 16#D800, utf8}}, caught(fun() -> table("\\x{D800}", utf8) end)),
    ?assertMatch({'EXIT', {unencodable_char, 16#100, latin1}}, caught(fun() -> table("\\x{100}", latin1) end)),
    ?assertMatch({'EXIT', {regexp_error, {unterminated, "\\p{"}, _}}, caught(fun() -> table("\\p{L", utf8) end)),
    ?assertMatch({'EXIT', {regexp_error, {illegal, "}"}, _}}, caught(fun() -> table("\\x{110000}", utf8) end)),
    ?assertMatch({'EXIT', {unknown_encoding, utf7}}, caught(fun() -> table("a", utf7) end)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lexing

-define(TEXT, "λx → 42 Ωmega 😀 ٣٤ 中文\n").
-define(TOKENS, [{ident, 1, "λx"}, {sym, 1, "→"}, {num, 1, "42"},
                 {ident, 1, "Ωmega"}, {sym, 1, "😀"}, {num, 1, "٣٤"},
                 {ident, 1, "中文"}]).

tokens_in_all_encodings_test_() ->
    [ ?_assertEqual(?TOKENS, lex(unicode:characters_to_binary(?TEXT, unicode, Enc), Enc))
      || Enc <- ?ENCODINGS ].

encoding_aliases_test() ->
    Bin = unicode:characters_to_binary(?TEXT, unicode, {utf16, little}),
    ?assertEqual(?TOKENS, lex(Bin, utf16le)),
    ?assertEqual(?TOKENS, lex(unicode:characters_to_binary(?TEXT, unicode, {utf32, big}), utf32)).

byte_list_input_test() ->
    ?assertEqual(?TOKENS, lex(binary_to_list(unicode:characters_to_binary(?TEXT)), utf8)).

%% one/2 hands back the rest as bytes
one_test() ->
    Lex = lex:regexps_to_table(rules(), [{encoding, utf8}]),
    Bin = unicode:characters_to_binary("λx 1"),
    {token, {ident, _, "λx"}, Rest} = lex:one(Lex, Bin),
    ?assertEqual(<<" 1">>, Rest),
    %% a list in, a list out
    {token, {ident, _, "λx"}, RestL} = lex:one(Lex, binary_to_list(Bin)),
    ?assertEqual(" 1", RestL).

%% Byte 0 is an ordinary byte (UTF-16/32 need it; latin1 allows NUL)
nul_test() ->
    ?assertEqual([{ident, 1, "A"}], lex(<<"A", 0>>, {utf16, little})),
    ?assertEqual([{t, 1, [0, 1]}], lex:with([{regexp, "[\\x{0}-\\x{1f}]+", lex:item(t)}],
                                          <<0, 1>>, [{encoding, latin1}])).

latin1_test() ->
    Rules = [{regexp, "\\p{L}+", lex:item(w)}, {regexp, " ", lex:whitespace()}],
    ?assertEqual([{w, 1, "café"}, {w, 1, "ÿ"}],
                 lex:with(Rules, <<"caf", 16#E9, " ", 16#FF>>, [{encoding, latin1}])).

no_match_test() ->
    ?assertEqual({'EXIT', {lex_no_match, {line, 1}, {column, 3}}},
                 lex(unicode:characters_to_binary("λ !λ\nzz"), utf8)),
    ?assertEqual({'EXIT', {lex_no_match, {line, 3}, {column, 4}}},
                 lex(unicode:characters_to_binary("λ\n中文\n😀λ !"), utf8)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Malformed input is reported, never accepted

%% The position is that of the first bad byte, on line 1 unless given.
bad(Enc, Kind, Col, Bytes) ->
    bad(Enc, Kind, 1, Col, Bytes).

bad(Enc, Kind, Line, Col, Bytes) ->
    {'EXIT', {lex_bad_encoding, Enc, Kind, {line, Line}, {column, Col}, {bytes, Bytes}}}.

utf8_errors_test_() ->
    [?_assertEqual(bad(utf8, invalid, 4, [16#FF]), lex(<<"ab ", 16#FF, "cd">>, utf8)),
     %% overlong '/'
     ?_assertEqual(bad(utf8, invalid, 1, [16#C0]), lex(<<16#C0, 16#AF>>, utf8)),
     %% overlong 3-byte
     ?_assertEqual(bad(utf8, invalid, 1, [16#E0, 16#80]), lex(<<16#E0, 16#80, 16#80>>, utf8)),
     %% encoded surrogate U+D800
     ?_assertEqual(bad(utf8, invalid, 1, [16#ED, 16#A0]), lex(<<16#ED, 16#A0, 16#80>>, utf8)),
     %% above U+10FFFF
     ?_assertEqual(bad(utf8, invalid, 1, [16#F4, 16#90]), lex(<<16#F4, 16#90, 16#80, 16#80>>, utf8)),
     %% stray continuation byte after a token ...
     ?_assertEqual(bad(utf8, invalid, 3, [16#80]), lex(<<"ab", 16#80>>, utf8)),
     %% ... and inside one
     ?_assertEqual(bad(utf8, invalid, 4, [16#80]), lex(<<"\"ab", 16#80, "\"">>, utf8)),
     ?_assertEqual(bad(utf8, incomplete, 4, [16#E2, 16#82]), lex(<<"\"ab", 16#E2, 16#82>>, utf8)),
     %% truncated character at the end of the input, alone ...
     ?_assertEqual(bad(utf8, incomplete, 4, [16#CE]), lex(<<"ab ", 16#CE>>, utf8)),
     %% ... and right after a token that could continue with it
     ?_assertEqual(bad(utf8, incomplete, 3, [16#CE]), lex(<<"ab", 16#CE>>, utf8)),
     ?_assertEqual(bad(utf8, incomplete, 3, [16#F0, 16#9F, 16#98]),
                   lex(<<"x ", 16#F0, 16#9F, 16#98>>, utf8)),
     %% truncated, then more input: invalid, not incomplete
     ?_assertEqual(bad(utf8, invalid, 1, [16#CE, $a]), lex(<<16#CE, "a">>, utf8)),
     %% a list element that is not a byte
     ?_assertEqual(bad(utf8, not_a_byte, 2, [16#3BB]), lex([$a, 16#3BB], utf8)),
     ?_assertEqual(bad(utf8, not_a_byte, 3, [16#3BB]), lex([$", $a, 16#3BB, $"], utf8)),
     %% inside a token that spans lines: the position of the bad byte
     ?_assertEqual(bad(utf8, invalid, 3, 2, [16#FF]),
                   lex(<<"x\n\"λ\nμ"/utf8, 16#FF, "\"">>, utf8))].

utf16_errors_test_() ->
    [%% lone high surrogate followed by 'a'
     ?_assertEqual(bad({utf16, little}, invalid, 1, [16#00, 16#D8, $a, 0]),
                   lex(<<16#00, 16#D8, $a, 0>>, {utf16, little})),
     %% lone low surrogate
     ?_assertEqual(bad({utf16, big}, invalid, 2, [16#DC, 16#00]),
                   lex(<<0, $a, 16#DC, 16#00>>, {utf16, big})),
     %% odd number of bytes
     ?_assertEqual(bad({utf16, big}, incomplete, 3, [0]), lex(<<0, $a, 0, $\s, 0>>, {utf16, big})),
     %% high surrogate at the end
     ?_assertEqual(bad({utf16, little}, incomplete, 3, [16#3D, 16#D8]),
                   lex(<<$a, 0, $\s, 0, 16#3D, 16#D8>>, {utf16, little}))].

utf32_errors_test_() ->
    [?_assertEqual(bad({utf32, big}, invalid, 1, [0, 16#11, 0, 0]),
                   lex(<<0, 16#11, 0, 0>>, {utf32, big})),
     ?_assertEqual(bad({utf32, little}, invalid, 1, [0, 16#D8, 0, 0]),
                   lex(<<0, 16#D8, 0, 0>>, {utf32, little})),
     ?_assertEqual(bad({utf32, little}, incomplete, 1, [$a, 0, 0]),
                   lex(<<$a, 0, 0>>, {utf32, little}))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The legacy (bytes) mode is unchanged

legacy_test() ->
    ?assertEqual([{atom, 1, "foo"}, {'(', 1}, {var, 1, "X"}, {')', 1}, {'->', 1},
                  {string, 1, "hi"}, {'.', 1}, {integer, 3, "42"}],
                 lex:with(lex_test:real_erlang_lex(), "foo(X) -> \"hi\".\n\n42")).

%% The whole Erlang lexer also works in a Unicode encoding
erlang_lex_utf16_test() ->
    Src = "foo(X) -> \"hi\".\n\n42",
    ?assertEqual(lex:with(lex_test:real_erlang_lex(), Src),
                 lex:with(lex_test:real_erlang_lex(),
                          unicode:characters_to_binary(Src, unicode, {utf16, little}),
                          [{encoding, utf16le}])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Case-insensitive matching

closure(Name) -> lex_unicode:case_closure(prop(Name)).

caseless_class_test_() ->
    {timeout, 120,
     [ fun() -> check_class("(?i)\\p{Lu}", closure("Lu"), Enc) end
       || Enc <- [latin1 | ?ENCODINGS] ] ++
     [ fun() -> check_class("(?i)[^k\\x{3C3}]",
                            lex_unicode:complement(
                              lex_unicode:case_closure([{$k, $k}, {16#3C3, 16#3C3}])), Enc) end
       || Enc <- ?ENCODINGS ]}.

case_closure_test() ->
    C = fun(Cp) -> lex_unicode:case_closure([{Cp, Cp}]) end,
    ?assertEqual([{$K, $K}, {$k, $k}, {16#212A, 16#212A}], C($k)),
    ?assertEqual([{16#3A3, 16#3A3}, {16#3C2, 16#3C3}], C(16#3C2)),   % Σ ς σ
    ?assertEqual([{16#DF, 16#DF}, {16#1E9E, 16#1E9E}], C(16#DF)),    % ß ẞ
    ?assertEqual([{$1, $1}], C($1)),
    %% Turkic dotted/dotless i (status T) are not folded with i/I
    ?assertEqual([{$I, $I}, {$i, $i}], C($i)),
    %% every set it returns is closed
    L = closure("L"),
    ?assertEqual(L, lex_unicode:case_closure(L)),
    ?assertEqual([{$A, $C}, {$a, $c}], lex_unicode:ascii_case_closure([{$a, $c}])).

caseless_lex_test_() ->
    W = fun(Re, In, Opts) ->
                caught(fun() -> lex:with([{regexp, Re, lex:item(t)},
                                          {regexp, " ", lex:whitespace()}], In, Opts) end)
        end,
    U = [{encoding, utf8}],
    [?_assertEqual([{t, 1, "SELECT"}, {t, 1, "SeLeCt"}], W("(?i)select", <<"SELECT SeLeCt">>, [])),
     ?_assertEqual([{t, 1, "SELECT"}], W("select", <<"SELECT">>, [case_insensitive])),
     ?_assertEqual([{t, 1, "SELECT"}], W("select", <<"SELECT">>, [{case_insensitive, true}])),
     ?_assertMatch({'EXIT', {lex_no_match, _, _}}, W("(?-i)select", <<"SELECT">>, [case_insensitive])),
     ?_assertEqual([{t, 1, "Keep"}], W("(?i:k)eep", <<"Keep">>, [])),
     ?_assertEqual({'EXIT', {lex_no_match, {line, 1}, {column, 1}}}, W("(?i:k)eep", <<"KEEP">>, [])),
     ?_assertEqual([{t, 1, "ΣσςΣ"}], W("(?i)σ+", <<"ΣσςΣ"/utf8>>, U)),
     ?_assertEqual([{t, 1, "K"}, {t, 1, [16#212A]}], W("(?i)k", <<"K"/utf8, 16#212A/utf8>>, U)),
     ?_assertEqual([{t, 1, "ÀÉ"}], W("(?i)[à-é]+", <<"ÀÉ"/utf8>>, U)),
     ?_assertEqual([{t, 1, "a"}], W("(?i)\\p{Lu}", <<"a">>, U)),
     %% negation after folding
     ?_assertMatch({'EXIT', {lex_no_match, _, _}}, W("(?i)[^a]", <<"A">>, [])),
     ?_assertMatch({'EXIT', {lex_no_match, _, _}}, W("(?i)\\P{Lu}", <<"a">>, U)),
     %% no multi-character folds
     ?_assertMatch({'EXIT', {lex_no_match, _, _}}, W("(?i)ss", <<"ß"/utf8>>, U)),
     %% bytes mode only folds ASCII, and never splits a UTF-8 literal
     ?_assertEqual([{t, 1, [$C, $A, $F, 16#C3, 16#A9]}], W("(?i)café", <<"CAFé"/utf8>>, [])),
     ?_assertMatch({'EXIT', {lex_no_match, _, _}}, W("(?i)café", <<"CAFÉ"/utf8>>, []))].

flag_syntax_test_() ->
    E = fun(Re) -> caught(fun() -> table(Re, utf8) end) end,
    [?_assertMatch({'EXIT', {regexp_error, {misplaced_flags, _}, _}}, E("a(?i)b")),
     ?_assertMatch({'EXIT', {regexp_error, {illegal_flags, _}, _}}, E("(?x)a")),
     ?_assertMatch({'EXIT', {regexp_error, {unterminated, "(?"}, _}}, E("(?i:a")),
     ?_assertMatch({'EXIT', {error, {illegal_flags, _}}}, caught(fun() -> table("(?s:a)", bytes) end))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% POSIX classes

posix_unicode_test_() ->
    {timeout, 120,
     [ fun() -> check_class("[[:alpha:]]", prop("Alphabetic"), utf8) end,
       fun() -> check_class("[[:^digit:]]", lex_unicode:complement(prop("Nd")), {utf16, little}) end,
       fun() -> check_class("[[:word:]-]", lex_unicode:union(prop("Word"), [{$-, $-}]), utf8) end,
       fun() -> check_class("[[:punct:]]", prop("XPosixPunct"), {utf32, big}) end ]}.

%% In bytes mode the POSIX classes are the ASCII ("C" locale) ones
posix_ascii_test() ->
    Expect = [{"alpha", "A-Za-z"}, {"digit", "0-9"}, {"alnum", "0-9A-Za-z"},
              {"upper", "A-Z"}, {"lower", "a-z"}, {"space", "\t\n\v\f\r "},
              {"blank", "\t "}, {"xdigit", "0-9A-Fa-f"}, {"word", "0-9A-Za-z_"},
              {"cntrl", lists:seq(0, 31) ++ [127]},
              {"punct", "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"},
              {"graph", lists:seq(33, 126)}, {"print", lists:seq(32, 126)}],
    [ begin
          Set = lex_unicode:normalize([ {C, C} || C <- expand(Chars) ]),
          Lex = table("[[:" ++ Name ++ ":]]", bytes),
          Got = [ B || B <- lists:seq(0, 255), accepts(Lex, [B]) ],
          ?assertEqual({Name, [ C || {F, T} <- Set, C <- lists:seq(F, T) ]}, {Name, Got})
      end || {Name, Chars} <- Expect ],
    ?assertEqual([{0, 127}], lex_unicode:union(prop("PosixPrint"), prop("PosixCntrl"))),
    ?assertMatch({'EXIT', {unknown_posix_class, "bogus"}},
                 caught(fun() -> table("[[:bogus:]]", bytes) end)),
    %% not a POSIX class: '[' and friends are ordinary characters
    Lex = table("[[:al]+", bytes),
    ?assert(accepts(Lex, "[:la")).

expand([A, $-, B | Rest]) -> lists:seq(A, B) ++ expand(Rest);
expand([C | Rest]) -> [C | expand(Rest)];
expand([]) -> [].

caseless_posix_test() ->
    Lex = lex:regexps_to_table([{regexp, "[[:upper:]]+", lex:item(t)}],
                               [{encoding, utf8}, case_insensitive]),
    ?assert(accepts(Lex, "aBc")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Line and column counting

%% An action that records where its token starts
at(Tag) ->
    fun(Acc) -> {token, {Tag, lex:current_position(), lists:reverse(Acc)}} end.

%% Words; every other character is skipped one at a time, so that line
%% terminators (and the two halves of a CR LF) are separate matches.
pos_rules(Enc) when Enc == bytes ->
    [{regexp, "[a-z]+", at(w)}, {regexp, "[^a-z]", lex:whitespace()}];
pos_rules(_Enc) ->
    [{regexp, "\\p{L}+", at(w)}, {regexp, "\\P{L}", lex:whitespace()}].

positions(Text, Opts) ->
    Enc = proplists:get_value(encoding, Opts, bytes),
    Bin = case Enc of
              bytes -> list_to_binary(Text);
              latin1 -> list_to_binary(Text);
              _ -> unicode:characters_to_binary(Text, unicode, Enc)
          end,
    [ P || {w, P, _} <- lex:with(pos_rules(Enc), Bin, Opts) ].

%% LF, VT, FF, CR, CR LF, NEL, LS, PS, in every Unicode encoding
unicode_lines_test_() ->
    Text = "a\nb\vc\fd\re\r\nf" ++ [16#85] ++ "g" ++ [16#2028] ++ "h" ++ [16#2029] ++ "i",
    Expect = [ {L, 1} || L <- lists:seq(1, 9) ],
    [ ?_assertEqual(Expect, positions(Text, [{encoding, Enc}])) || Enc <- ?ENCODINGS ].

latin1_lines_test() ->
    ?assertEqual([{1, 1}, {2, 1}, {3, 1}],
                 positions("a" ++ [16#85] ++ "b\r\nc", [{encoding, latin1}])).

ascii_lines_test() ->
    %% the default in bytes mode
    ?assertEqual([{1, 1}, {2, 1}, {3, 1}, {4, 1}, {5, 1}],
                 positions("a\nb\rc\r\nd\fe", [])),
    ?assertEqual([{1, 1}, {2, 1}, {2, 3}],
                 positions("a\r\nb" ++ [16#85] ++ "c", [{encoding, utf8}, {newline, ascii}])).

lf_lines_test() ->
    ?assertEqual([{1, 1}, {1, 3}, {2, 1}, {2, 3}],
                 positions("a\rb\nc\fd", [{newline, lf}])),
    ?assertEqual([{1, 1}, {1, 3}],
                 positions("a" ++ [16#2028] ++ "b", [{encoding, utf8}, {newline, lf}])).

%% a CR LF is one line break, also when one match ends with the CR and
%% the next one starts with the LF
crlf_test() ->
    Rules = [{regexp, "[a-z]+", at(w)}, {regexp, "[a-z]*\r", at(cr)},
             {regexp, "\n[a-z]*", at(lf)}, {regexp, "\r\n", lex:newline()},
             {regexp, " ", lex:whitespace()}],
    ?assertEqual([{cr, {1, 1}, "a\r"}, {lf, {2, 1}, "\nb"}, {cr, {3, 1}, "c\r"},
                  {lf, {4, 1}, "\nd"}, {w, {4, 3}, "e"}],
                 lex:with(Rules, <<"a\r\nb\r\nc\r\nd e">>)).

%% columns count characters, not bytes or UTF-16 code units
columns_test_() ->
    Text = "ab λμ 中文 😀x y",
    Expect = [{1, 1}, {1, 4}, {1, 7}, {1, 11}, {1, 13}],
    [ ?_assertEqual(Expect, positions(Text, [{encoding, Enc}])) || Enc <- ?ENCODINGS ].

%% a tab is one column; bytes mode counts bytes
bytes_columns_test() ->
    ?assertEqual([{1, 1}, {1, 3}, {1, 6}], positions("a\tb  c", [])),
    Lex = lex:regexps_to_table([{regexp, "[a-z]+", at(w)}, {regexp, "[^a-z]+", lex:whitespace()}]),
    ?assertEqual([{w, {1, 1}, "a"}, {w, {1, 4}, "b"}],
                 lex:o_longest(Lex, binary_to_list(<<"a", "λ"/utf8, "b">>))).

%% tokens that span lines, and the helpers that used to count lines
multiline_tokens_test() ->
    Rules = [{regexp, "\"[^\"]*\"", lex:string()},
             {regexp, "'[^']*'", lex:quoted_atom()},
             {regexp, "[a-z]+", lex:item(w)},
             {regexp, " ", lex:whitespace()},
             {regexp, "\n", lex:newline()}],
    ?assertEqual([{string, 1, "a\nb"}, {w, 2, "c"}, {atom, 3, "d\n\ne"},
                  {w, 5, "f"}],
                 lex:with(Rules, <<"\"a\nb\" c\n'd\n\ne' f">>)).

%% inc_lines/1 still adds to the line count (and nothing else)
inc_lines_test() ->
    Rules = [{regexp, "#skip", fun(_) -> lex:inc_lines(10), no_token end},
             {regexp, "[a-z]+", at(w)}, {regexp, " ", lex:whitespace()}],
    ?assertEqual([{w, {1, 1}, "a"}, {w, {11, 9}, "b"}],
                 lex:with(Rules, <<"a #skip b">>)).

%% one/2 continues from the current position; no_lines/0 resets it
one_position_test() ->
    Lex = lex:regexps_to_table(pos_rules(utf8), [{encoding, utf8}]),
    lex:no_lines(),
    {token, {w, {1, 1}, "λ"}, R1} = lex:one(Lex, <<"λ\n μ"/utf8>>),
    {token, {w, {2, 2}, "μ"}, <<>>} = lex:one(Lex, R1),
    ?assertEqual({2, 3}, lex:current_position()),
    ?assertEqual(2, lex:current_line()),
    ?assertEqual(3, lex:current_column()),
    lex:no_lines(),
    ?assertEqual({1, 1}, lex:current_position()).

newline_option_test() ->
    ?assertEqual({'EXIT', {unicode_newline_needs_encoding, bytes}},
                 caught(fun() -> lex:regexps_to_table(pos_rules(bytes), [{newline, unicode}]) end)),
    ?assertEqual({'EXIT', {unknown_newline, crlf}},
                 caught(fun() -> lex:regexps_to_table(pos_rules(bytes), [{newline, crlf}]) end)).

count_lines_test() ->
    ?assertEqual(4, lex:count_lines("a\nb\r\nc\rd\fe")),
    ?assertEqual(2, lex:count_lines("a\nb\r\nc\rd\fe", lf)),
    ?assertEqual(0, lex:count_lines([$a, 16#2028, $b])),
    ?assertEqual(2, lex:count_lines([$a, 16#2028, $b, 16#85], unicode)),
    ?assertEqual(0, lex:count_lines("")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The binary driver and the list driver give the same results

%% Rules with backtracking ("a*b" vs "a"), multi-line tokens, and room
%% for both no-match and encoding errors.
driver_rules(bytes) ->
    [{regexp, "a", at(a)}, {regexp, "a*b", at(ab)}, {regexp, "[c-e]+", at(w)},
     {regexp, "\"[^\"]*\"", at(s)}, {regexp, "[ \n\r]", lex:whitespace()}];
driver_rules(_Enc) ->
    [{regexp, "a", at(a)}, {regexp, "a*b", at(ab)}, {regexp, "[c-eλ😀]+", at(w)},
     {regexp, "\"[^\"]*\"", at(s)}, {regexp, "[ \n\r\\x{2028}]", lex:whitespace()}].

%% Random text: mostly characters the rules know, some they do not ('?'),
%% and for the Unicode encodings, some broken bytes.
random_input(Enc) ->
    Chars = "aaab cde\"\n\r?" ++ [16#3BB, 16#1F600, 16#2028],
    N = rand:uniform(30) - 1,
    Text = [ lists:nth(rand:uniform(length(Chars)), Chars) || _ <- lists:seq(1, N) ],
    Bin = case Enc of
              _ when Enc == bytes; Enc == latin1 -> list_to_binary([ C || C <- Text, C < 256 ]);
              _ -> unicode:characters_to_binary(Text, unicode, Enc)
          end,
    case Enc =/= bytes andalso rand:uniform(4) == 1 of
        true ->
            Cut = rand:uniform(byte_size(Bin) + 1) - 1,
            <<Pre:Cut/binary, Post/binary>> = Bin,
            Junk = lists:nth(rand:uniform(3), [<<16#FF>>, <<16#C3>>, <<16#D8, 16#00>>]),
            case rand:uniform(2) of
                1 -> <<Pre/binary, Junk/binary, Post/binary>>;
                2 -> Pre
            end;
        false ->
            Bin
    end.

%% Four drivers, one result: the binary driver on the binary, the iolist
%% driver on a random iolist of the same bytes and on the flat list, and
%% the original list driver (lex_string/2) on the flat list.
driver_equivalence_test_() ->
    {timeout, 120,
     fun() ->
             rand:seed(exsss, {17, 42, 4711}),
             [ begin
                   Rules = driver_rules(Enc),
                   Opts = [{encoding, Enc}],
                   Bin = random_input(Enc),
                   IoList = random_iolist(Bin),
                   ?assertEqual(Bin, iolist_to_binary(IoList)),
                   FromBin = caught(fun() -> lex:with(Rules, Bin, Opts) end),
                   FromIoList = caught(fun() -> lex:with(Rules, IoList, Opts) end),
                   FromList = caught(fun() -> lex:with(Rules, binary_to_list(Bin), Opts) end),
                   FromOld = caught(fun() ->
                                            Lex = lex:regexps_to_table(Rules, Opts),
                                            lex:no_lines(),
                                            lex:lex_string(Lex, binary_to_list(Bin))
                                    end),
                   ?assertEqual({Enc, Bin, FromBin}, {Enc, Bin, FromIoList}),
                   ?assertEqual({Enc, Bin, FromBin}, {Enc, Bin, FromList}),
                   ?assertEqual({Enc, Bin, FromBin}, {Enc, Bin, FromOld})
               end || Enc <- [bytes, latin1 | ?ENCODINGS], _ <- lists:seq(1, 500) ]
     end}.

%% The bytes of Bin as a random iolist: binaries, byte lists, nested
%% lists, empty pieces, and binaries as improper list tails.
random_iolist(Bin) ->
    Pieces = split_randomly(Bin),
    nest(Pieces).

split_randomly(<<>>) ->
    [];
split_randomly(Bin) ->
    N = rand:uniform(min(byte_size(Bin), 6)),
    <<P:N/binary, Rest/binary>> = Bin,
    [P | split_randomly(Rest)].

nest([]) ->
    case rand:uniform(3) of
        1 -> [];
        2 -> [<<>>];
        3 -> [[]]
    end;
nest([P]) ->
    case rand:uniform(4) of
        1 -> [[] | P];                                   % a binary as the tail
        _ -> [piece(P) | nest([])]
    end;
nest([P | Ps]) ->
    case rand:uniform(5) of
        1 -> binary_to_list(P) ++ nest(Ps);              % bytes in this list
        _ -> [piece(P) | nest(Ps)]
    end.

piece(P) ->
    case rand:uniform(5) of
        1 -> P;                                          % a binary
        2 -> binary_to_list(P);                          % a nested byte list
        3 -> [binary_to_list(P)];                        % nested deeper
        4 -> [[<<>>, P]];                                % with an empty binary
        5 -> <<B, Rest/binary>> = P, [B | Rest]          % improper tail inside
    end.


%% one/2 on a binary: the rest is a sub-binary, and lexing can continue
one_binary_test() ->
    Lex = lex:regexps_to_table(driver_rules(utf8), [{encoding, utf8}]),
    lex:no_lines(),
    In = <<"aaa ab \"λ\n\" c"/utf8>>,
    Toks = (fun Loop(B, Acc) ->
                    case lex:one(Lex, B) of
                        {token, T, R} when is_binary(R) -> Loop(R, [T | Acc]);
                        end_of_stream -> lists:reverse(Acc)
                    end
            end)(In, []),
    ?assertEqual([{a, {1, 1}, "a"}, {a, {1, 2}, "a"}, {a, {1, 3}, "a"},
                  {ab, {1, 5}, "ab"}, {s, {1, 8}, "\"λ\n\""}, {w, {2, 3}, "c"}],
                 Toks),
    ?assertEqual(end_of_stream, lex:one(Lex, <<>>)),
    ?assertEqual(end_of_stream, lex:one(Lex, <<"  \n ">>)).

%% with/3 on a binary raises the process's GC limits while it runs and
%% restores them afterwards, also when lexing fails
gc_limits_restored_test() ->
    Self = self(),
    spawn(fun() ->
                  Flags = fun() -> process_info(self(), [min_heap_size, min_bin_vheap_size]) end,
                  Before = Flags(),
                  Big = binary:copy(<<"ab ">>, 200000),
                  Ok = length(lex:with(driver_rules(utf8), Big, [{encoding, utf8}])),
                  AfterOk = Flags(),
                  Err = caught(fun() -> lex:with(driver_rules(utf8), <<Big/binary, "?">>, [{encoding, utf8}]) end),
                  AfterErr = Flags(),
                  Self ! {result, Before, Ok, AfterOk, Err, AfterErr}
          end),
    receive
        {result, Before, Ok, AfterOk, Err, AfterErr} ->
            ?assertEqual(200000, Ok),          % "ab" is one token (a*b)
            ?assertEqual(Before, AfterOk),
            ?assertEqual({'EXIT', {lex_no_match, {line, 1}, {column, 600001}}}, Err),
            ?assertEqual(Before, AfterErr)
    after 30000 ->
            error(timeout)
    end.


%% one/2 on an iolist returns the rest as iodata, sharing the input
one_iolist_test() ->
    Lex = lex:regexps_to_table(driver_rules(utf8), [{encoding, utf8}]),
    lex:no_lines(),
    In = [<<"aa">>, [$a, $\s | <<"ab">>], [[<<" \"λ"/utf8>>], "\n\" "], <<"c">>],
    Pull = fun Loop(I, Acc) ->
                   case lex:one(Lex, I) of
                       {token, T, R} -> Loop(R, [{T, iolist_to_binary(R)} | Acc]);
                       end_of_stream -> lists:reverse(Acc)
                   end
           end,
    ?assertEqual([{{a, {1, 1}, "a"}, <<"aa ab \"λ\n\" c"/utf8>>},
                  {{a, {1, 2}, "a"}, <<"a ab \"λ\n\" c"/utf8>>},
                  {{a, {1, 3}, "a"}, <<" ab \"λ\n\" c"/utf8>>},
                  {{ab, {1, 5}, "ab"}, <<" \"λ\n\" c"/utf8>>},
                  {{s, {1, 8}, "\"λ\n\""}, <<" c">>},
                  {{w, {2, 3}, "c"}, <<>>}],
                 Pull(In, [])),
    %% a flat list's rest is its tail
    {token, _, Rest} = lex:one(Lex, "cd ab"),
    ?assertEqual(" ab", Rest).

iolist_shapes_test_() ->
    W = fun(In) -> caught(fun() -> lex:with(driver_rules(utf8), In, [{encoding, utf8}]) end) end,
    [?_assertEqual([], W([])),
     ?_assertEqual([], W([[], <<>>, [[<<>>]]])),
     %% improper tails
     ?_assertEqual([{w, {1, 1}, "cdde"}], W([$c, $d | <<"de">>])),
     ?_assertEqual([{w, {1, 1}, "cd"}, {w, {1, 4}, "ed"}], W([[$c | <<"d">>], $\s | <<"ed">>])),
     %% a multibyte character split over pieces
     ?_assertEqual([{w, {1, 1}, "cλd"}], W([<<"c", 16#CE>>, [16#BB], <<"d">>])),
     %% deep nesting
     ?_assertEqual([{w, {1, 1}, "cde"}],
                   W(lists:foldl(fun(_, Acc) -> [Acc] end, [<<"cde">>], lists:seq(1, 1000))))].

%% elements that are not bytes, binaries or lists
iolist_bad_elements_test_() ->
    U = fun(In) -> caught(fun() -> lex:with(driver_rules(utf8), In, [{encoding, utf8}]) end) end,
    B = fun(In) -> caught(fun() -> lex:with(driver_rules(bytes), In, []) end) end,
    [?_assertEqual(bad(utf8, not_a_byte, 3, [300]), U([<<"cd">>, 300])),
     ?_assertEqual(bad(utf8, not_a_byte, 2, 2, [foo]), U([<<"cd\n ">>, [foo]])),
     ?_assertEqual(bad(utf8, not_a_byte, 4, [<<1:3>>]), U([<<"\"cd">>, <<1:3>>])),
     ?_assertEqual(bad(utf8, not_a_byte, 3, [bar]), U([$c, $d | bar])),
     ?_assertEqual({'EXIT', {lex_no_match, {line, 1}, {column, 3}}}, B([<<"cd">>, 300])),
     ?_assertEqual({'EXIT', {lex_no_match, {line, 1}, {column, 1}}}, B([foo]))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Incremental driver

%% A More function giving Chunks, then ending with {last, C} or ended.
%% Each function fails if it is called twice, and the last one if it is
%% called at all after the stream ended.
more_from(Chunks, Ending) ->
    once(fun() -> more_result(Chunks, Ending) end).

more_result([], _Ending) -> ended;
more_result([C], last) -> {last, C};
more_result([C | Cs], Ending) -> {more, C, more_from(Cs, Ending)}.

once(F) ->
    Ref = make_ref(),
    fun() ->
            case get(Ref) of
                undefined -> put(Ref, called), F();
                called -> exit(more_called_twice)
            end
    end.

%% Random chunks of 0-7 bytes, each as a binary or a random iolist
random_chunks(<<>>) ->
    case rand:uniform(3) of
        1 -> [];
        _ -> [<<>>]
    end;
random_chunks(Bin) ->
    N = min(byte_size(Bin), rand:uniform(8) - 1),
    <<C:N/binary, Rest/binary>> = Bin,
    Chunk = case rand:uniform(2) of 1 -> C; 2 -> random_iolist(C) end,
    [Chunk | random_chunks(Rest)].

pull_more(Lex, Data, More, Acc) ->
    case lex:one(Lex, Data, More) of
        {token, T, Rest, More1} -> pull_more(Lex, Rest, More1, [T | Acc]);
        end_of_stream -> lists:reverse(Acc)
    end.

%% Lexing in chunks gives the same as lexing the whole input: tokens,
%% positions and errors, with tokens, characters and backtracking
%% spanning chunks
incremental_equivalence_test_() ->
    {timeout, 120,
     fun() ->
             rand:seed(exsss, {3, 14, 15}),
             [ begin
                   Rules = driver_rules(Enc),
                   Opts = [{encoding, Enc}],
                   Bin = random_input(Enc),
                   Whole = caught(fun() -> lex:with(Rules, Bin, Opts) end),
                   Ending = lists:nth(rand:uniform(2), [last, ended]),
                   {First, Chunks} = case random_chunks(Bin) of
                                         [C | Cs] when Ending == ended; Cs =/= [] -> {C, Cs};
                                         Cs -> {[], Cs}
                                     end,
                   Stream = caught(fun() -> lex:with(Rules, First, more_from(Chunks, Ending), Opts) end),
                   ?assertEqual({Enc, Bin, Chunks, Whole}, {Enc, Bin, Chunks, Stream}),
                   Pulled = caught(fun() ->
                                           Lex = lex:regexps_to_table(Rules, Opts),
                                           lex:no_lines(),
                                           pull_more(Lex, First, more_from(Chunks, Ending), [])
                                   end),
                   ?assertEqual({Enc, Bin, Chunks, Whole}, {Enc, Bin, Chunks, Pulled})
               end || Enc <- [bytes, latin1 | ?ENCODINGS], _ <- lists:seq(1, 500) ]
     end}.

incremental_more_test_() ->
    Rules = [{regexp, "\\p{L}+", lex:item(w)}, {regexp, ";", lex:token(semi)},
             {regexp, "\\s+", lex:whitespace()}],
    Lex = lex:regexps_to_table(Rules, [{encoding, utf8}]),
    Boom = fun() -> exit(more_called) end,
    One = fun(Data, More) -> lex:no_lines(), caught(fun() -> lex:one(Lex, Data, More) end) end,
    [%% a token that cannot grow is returned without calling More
     ?_assertMatch({token, {semi, 1}, <<>>, _}, One(<<";">>, Boom)),
     ?_assertMatch({token, {w, 1, "a"}, <<";">>, _}, One(<<"a;">>, Boom)),
     %% one that could grow needs More
     ?_assertEqual({'EXIT', more_called}, One(<<"a">>, Boom)),
     ?_assertMatch({token, {w, 1, "ab"}, <<>>, _}, One(<<"a">>, fun() -> {last, <<"b">>} end)),
     %% so does whitespace at the end (after "a" and ";" were returned
     %% without More: each is followed by a byte that ends it)
     ?_assertMatch({token, {semi, 1}, <<" ">>, _}, One(<<"; ">>, Boom)),
     ?_assertEqual({'EXIT', more_called}, One(<<" ">>, Boom)),
     %% an empty start
     ?_assertEqual(end_of_stream, One(<<>>, fun() -> ended end)),
     ?_assertEqual(end_of_stream, One([[], <<>>], fun() -> {last, <<"  ">>} end)),
     %% empty chunks are fine
     ?_assertMatch({token, {w, 1, "ab"}, _, _},
                   One(<<"a">>, more_from([<<>>, [], <<"b">>, <<>>], ended))),
     %% the continuation after the stream ended returns end
     ?_assertEqual(ended, begin
                              {token, _, _, M} = One(<<"a">>, fun() -> {last, <<" b">>} end),
                              M()
                          end),
     %% a More function must return one of the three forms
     ?_assertEqual({'EXIT', {bad_more_result, eof}}, One(<<"a">>, fun() -> eof end)),
     %% positions and errors continue across chunks
     ?_assertEqual({'EXIT', {lex_bad_encoding, utf8, invalid, {line, 2}, {column, 2}, {bytes, [16#FF]}}},
                   caught(fun() -> lex:with(Rules, <<"a\n">>, more_from([<<"b">>, <<16#FF>>], ended),
                                            [{encoding, utf8}]) end)),
     %% a bad character at the end of a chunk: the next chunk decides
     %% whether it is invalid or incomplete
     ?_assertEqual({'EXIT', {lex_bad_encoding, utf8, invalid, {line, 1}, {column, 1}, {bytes, [16#D8, 0]}}},
                   caught(fun() -> lex:with(Rules, [16#D8], more_from([<<0>>], ended),
                                            [{encoding, utf8}]) end)),
     ?_assertEqual({'EXIT', {lex_bad_encoding, utf8, incomplete, {line, 1}, {column, 2}, {bytes, [16#E2, 16#82]}}},
                   caught(fun() -> lex:with(Rules, <<"a", 16#E2>>, more_from([<<16#82>>, <<>>], last),
                                            [{encoding, utf8}]) end)),
     ?_assertEqual({'EXIT', {lex_no_match, {line, 1}, {column, 4}}},
                   caught(fun() -> lex:with(Rules, <<"ab">>, more_from([<<" ">>, <<"?">>], last),
                                            [{encoding, utf8}]) end))].

%% A real stream: lexing from a TCP socket, as the data arrives in
%% separate writes (with a word and a UTF-8 character split between them)
socket_test() ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {ip, loopback}]),
    {ok, Port} = inet:port(L),
    Writes = [<<"hello wo">>, <<"rld; ">>, <<"λ"/utf8, 16#CE>>, <<16#BC, ";\n">>, <<"x">>],
    spawn_link(fun() ->
                       {ok, S} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary]),
                       [ begin ok = gen_tcp:send(S, W), timer:sleep(20) end || W <- Writes ],
                       ok = gen_tcp:close(S)
               end),
    {ok, Sock} = gen_tcp:accept(L, 5000),
    More = fun M() ->
                   case gen_tcp:recv(Sock, 0, 5000) of
                       {ok, Data} -> {more, Data, M};
                       {error, closed} -> ended
                   end
           end,
    Rules = [{regexp, "\\p{L}+", fun(Acc) -> {token, {w, lex:current_position(), lists:reverse(Acc)}} end},
             {regexp, ";", fun(_) -> {token, {semi, lex:current_position()}} end},
             {regexp, "\\s+", lex:whitespace()}],
    Tokens = lex:with(Rules, <<>>, More, [{encoding, utf8}]),
    gen_tcp:close(Sock),
    gen_tcp:close(L),
    ?assertEqual([{w, {1, 1}, "hello"}, {w, {1, 7}, "world"}, {semi, {1, 12}},
                  {w, {1, 14}, "λμ"}, {semi, {1, 16}}, {w, {2, 1}, "x"}],
                 Tokens).
