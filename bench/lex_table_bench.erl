%%% Benchmark: the DFA table as a tuple of tuples versus a binary.
%%%
%%% lex.erl is compiled once per representation (see the bench-table
%%% target in the Makefile); this module is run against each build in a
%%% separate VM and prints one result line per specification and input.
%%%
%%% The specification is a lexer for Erlang (keywords, atoms, variables,
%%% numbers in all forms, strings, quoted atoms, characters, comments and
%%% all operators), in two versions:
%%%   erlang_bytes:  ASCII/Latin-1 identifiers, bytes encoding
%%%   erlang_utf8:   UAX #31 identifiers (XID_Start/XID_Continue), UTF-8
%%% The corpus is the Erlang source in src/ (without the generated
%%% Unicode tables), repeated to just over 1 MB.

-module(lex_table_bench).

-export([run/1, rules/1]).

-define(RUNS, 9).
-define(CORPUS_BYTES, 1000000).

run([Variant]) ->
    Corpus = corpus(),
    io:format("~s: corpus ~w bytes~n", [Variant, byte_size(Corpus)]),
    [spec(Variant, Spec, Corpus) || Spec <- [erlang_bytes, erlang_utf8]],
    ok.

spec(Variant, Spec, Corpus) ->
    Opts = opts(Spec),
    {BuildUs, LEX} = timer:tc(fun() -> lex:regexps_to_table(rules(Spec), Opts) end),
    {lex, _Start, AcceptLimit, Table, _Actions, _Enc, _NL, _Pos} = LEX,
    States = states(Table),
    Tokens = length(lex_binary(LEX, Corpus)),
    io:format("~s ~s: ~w states (~w accepting), ~w tokens~n",
              [Variant, Spec, States, AcceptLimit, Tokens]),
    io:format("~s ~s: table ~w bytes in memory, ~w bytes external, built in ~.1f ms~n",
              [Variant, Spec, memory_bytes(Table), byte_size(term_to_binary(Table)),
               BuildUs / 1000]),
    io:format("~s ~s: send table to a new process: ~.1f us~n",
              [Variant, Spec, send_us(Table)]),
    Inputs = [{binary, Corpus},
              {string, binary_to_list(Corpus)},
              {iolist, binary:split(Corpus, <<"\n">>, [global])}],
    [io:format("~s ~s: ~-6s ~6.1f ms (median of ~w)~n",
               [Variant, Spec, Kind, median_ms(LEX, Input), ?RUNS])
     || {Kind, Input} <- Inputs],
    ok.

opts(erlang_bytes) -> [];
opts(erlang_utf8) -> [{encoding, utf8}].

%% The number of states, whatever the representation.
states(Table) when is_tuple(Table) -> tuple_size(Table);
states(Table) when is_binary(Table) ->
    byte_size(Table) div (256 * table_bytes(Table)).

%% Entry width: find it from a known-good lookup (only to count states).
table_bytes(Table) ->
    hd([W || W <- [1, 2, 4], byte_size(Table) rem (256 * W) =:= 0,
             valid(Table, W)]).

valid(Table, W) ->
    States = byte_size(Table) div (256 * W),
    Bits = W * 8,
    lists:all(fun(N) -> N =:= -1 orelse (N >= 1 andalso N =< States) end,
              [N || <<N:Bits/signed>> <= Table]).

memory_bytes(Table) when is_binary(Table) ->
    byte_size(Table);                   % off-heap, refcounted
memory_bytes(Table) ->
    erts_debug:flat_size(Table) * erlang:system_info(wordsize).

%% Time to give the table to a new process (copied if it is a tuple,
%% a reference if it is a large binary).
send_us(Table) ->
    N = 200,
    Self = self(),
    {Us, _} = timer:tc(
                fun() ->
                        [begin
                             P = spawn(fun() -> Self ! {done, self(), is_tuple(Table)} end),
                             receive {done, P, _} -> ok end
                         end || _ <- lists:seq(1, N)]
                end),
    Us / N.

%% Each run in a fresh process with the same GC settings as lex:with/3.
median_ms(LEX, Input) ->
    Times = [run_once(LEX, Input) || _ <- lists:seq(1, ?RUNS)],
    lists:nth((?RUNS + 1) div 2, lists:sort(Times)) / 1000.

run_once(LEX, Input) ->
    {Pid, Ref} =
        spawn_monitor(
          fun() ->
                  {Us, _} = timer:tc(fun() -> lex_any(LEX, Input) end),
                  exit({time, Us})
          end),
    receive {'DOWN', Ref, process, Pid, {time, Us}} -> Us end.

lex_any(LEX, Bin) when is_binary(Bin) -> lex_binary(LEX, Bin);
lex_any(LEX, List) ->
    lex:with_gc_limits([vheap], iolist_size(List),
                       fun() -> lex:reset_pos(LEX), lex:lex_iolist(LEX, List) end).

lex_binary(LEX, Bin) ->
    lex:with_gc_limits([vheap, heap], byte_size(Bin),
                       fun() -> lex:reset_pos(LEX), lex:lex_binary(LEX, Bin) end).

corpus() ->
    Files = [F || F <- filelib:wildcard("src/*.erl"),
                  filename:basename(F) =/= "lex_unicode_data.erl"],
    One = iolist_to_binary([element(2, file:read_file(F)) || F <- lists:sort(Files)]),
    binary:copy(One, ?CORPUS_BYTES div byte_size(One) + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Erlang lexer

rules(Spec) ->
    {Lower, Upper, Cont} =
        case Spec of
            erlang_bytes -> {"[a-z\\xDF-\\xF6\\xF8-\\xFF]",
                             "[A-Z_\\xC0-\\xD6\\xD8-\\xDE]",
                             "[A-Za-z0-9_@\\xC0-\\xD6\\xD8-\\xF6\\xF8-\\xFF]"};
            erlang_utf8 -> {"[\\p{Ll}\\p{Lo}]",
                            "[\\p{Lu}\\p{Lt}_]",
                            "[\\p{XID_Continue}@]"}
        end,
    Keywords = ["after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl",
                "bsr", "bxor", "case", "catch", "cond", "div", "else", "end",
                "fun", "if", "let", "maybe", "not", "of", "or", "orelse",
                "receive", "rem", "try", "when", "xor"],
    Ops = ["->", "<-", "<=", "=>", "<:-", "<:=", "&&", "::", ":=", "||", "<<", ">>",
           "++", "--", "==", "/=", "=<", ">=", "=:=", "=/=", "?=", "...", "..",
           "(", ")", "[", "]", "{", "}", ",", ";", ":", "|", "#", ".", "+", "-",
           "*", "/", "<", ">", "=", "!", "?", "^"],
    Digits = "[0-9]+(_[0-9]+)*",
    Exp = "([eE][-+]?" ++ Digits ++ ")",
    [{regexp, 2, Kw, lex:raw_token(list_to_atom(Kw))} || Kw <- Keywords] ++
    [{regexp, 1, Re, Action} || {regexp, Re, Action} <-
    [{regexp, Lower ++ Cont ++ "*", lex:item(atom)},
     {regexp, "'([^'\\\\]|\\\\.)*'", lex:item(atom)},
     {regexp, Upper ++ Cont ++ "*", lex:item(var)},
     {regexp, Digits, lex:item(integer)},
     {regexp, "[0-9]+#[0-9A-Za-z]+(_[0-9A-Za-z]+)*", lex:item(integer)},
     {regexp, Digits ++ "\\." ++ Digits ++ Exp ++ "?", lex:item(float)},
     {regexp, "\\$([^\\\\\n]|\\\\(x\\{[0-9A-Fa-f]+\\}|x[0-9A-Fa-f][0-9A-Fa-f]|[0-7][0-7]?[0-7]?|\\^.|[^x0-7^\n]))",
      lex:item(char)},
     {regexp, "\"([^\"\\\\]|\\\\.)*\"", lex:item(string)},
     {regexp, "%[^\n]*", lex:comment()},
     {regexp, "[ \t\r\f]+", lex:whitespace()},
     {regexp, "\n", lex:newline()}] ++
    [{regexp, lex:safe_literal(Op), lex:raw_token(list_to_atom(Op))} || Op <- Ops]] ++
    %% anything else (priority 0), so that the benchmark never stops early
    [{regexp, ".", lex:item(other)}].
