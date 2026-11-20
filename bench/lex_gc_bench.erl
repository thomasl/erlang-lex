%%% Benchmark: garbage collection when lexing a binary.
%%%
%%% A binary over 64 bytes lives outside the process heap, and a process
%%% that keeps referring to a large one gets nearly only full GCs (see
%%% doc/README.md, "Binary input"). lex:with/3 works around that by
%%% raising min_bin_vheap_size and min_heap_size. This compares that with
%%% getting the input away from one large binary:
%%%
%%%   binary_default   lex the binary, default GC settings
%%%   binary_tuned     lex the binary with the GC limits of with/3 (current)
%%%   byte_list        binary_to_list/1 first, then lex the list
%%%   heap_binaries    split into 64-byte binaries (heap binaries, on the
%%%                    process heap) first, then lex that iolist
%%%   copy_F           incremental API, fed fragments of F bytes, each a
%%%                    binary:copy/1 of a part of the input (F = 64 gives
%%%                    heap binaries; larger fragments are off-heap)
%%%   producer_F       the same, but the fragments come from another
%%%                    process holding the input, as from a file or socket,
%%%                    so the lexing process never refers to the whole input
%%%   subbin_4K        incremental API, fed sub-binaries of 4 KB without
%%%                    copying (the cost of the incremental API alone)
%%%
%%% Run with `make bench-gc`. Each run is a new process that gets the
%%% input binary in a message and the table built beforehand.

-module(lex_gc_bench).

-export([run/0]).

-include("lex.hrl").

-define(SIZES, [4096, 16384, 65536, 262144, 524288, 1048576]).

run() ->
    LEX = lex:regexps_to_table(lex_table_bench:rules(erlang_bytes), []),
    Corpus = corpus(lists:max(?SIZES)),
    io:format("Erlang lexer, bytes encoding, ~w states; ~s~n",
              [tuple_size(LEX#lex.table), erlang:system_info(otp_release)]),
    %% copy, so that the input is a binary of its own (a sub-binary would
    %% keep the whole corpus alive in every worker)
    [size(LEX, binary:copy(binary:part(Corpus, 0, Size))) || Size <- ?SIZES],
    ok.

variants() ->
    [{binary_default, fun binary_default/2},
     {binary_tuned, fun binary_tuned/2},
     {byte_list, fun byte_list/2},
     {heap_binaries, fun heap_binaries/2}]
    ++ [{name(copy, F), fun(LEX, Bin) -> copy_fragments(LEX, Bin, F) end}
        || F <- [64, 1024, 4096, 8192, 16384, 65536]]
    ++ [{name(producer, F), fun(LEX, Bin) -> producer_fragments(LEX, Bin, F) end}
        || F <- [1024, 4096, 8192, 65536]]
    ++ [{subbin_4K, fun(LEX, Bin) -> subbin_fragments(LEX, Bin, 4096) end}].

name(Kind, F) when F >= 1024 -> list_to_atom(lists:concat([Kind, "_", F div 1024, "K"]));
name(Kind, F) -> list_to_atom(lists:concat([Kind, "_", F])).

size(LEX, Input) ->
    Size = byte_size(Input),
    Runs = max(11, min(51, 4000000 div Size)),
    io:format("~n~w bytes (~w timing runs each)~n", [Size, Runs]),
    io:format("~-15s ~9s ~7s ~7s ~11s ~10s ~10s ~10s~n",
              ["variant", "time", "minor", "major", "MB copied", "peak heap", "old bin", "end mem"]),
    {_, Ref, _} = run_once(LEX, Input, fun binary_default/2),
    [begin
         Times = [element(1, run_once(LEX, Input, F)) || _ <- lists:seq(1, Runs)],
         {_, Check, GC} = run_traced(LEX, Input, F),
         Check == Ref orelse exit({different_tokens, Name, Size}),
         io:format("~-15s ~6.2f ms ~7w ~7w ~11.2f ~7w KB ~7w KB ~7w KB~n",
                   [Name, median(Times) / 1000,
                    maps:get(minor, GC), maps:get(major, GC),
                    maps:get(copied, GC) * 8 / 1048576,
                    maps:get(peak_heap, GC) * 8 div 1024,
                    maps:get(peak_bin, GC) * 8 div 1024,
                    maps:get(end_mem, GC) div 1024])
     end || {Name, F} <- variants()],
    ok.

median(Xs) -> lists:nth((length(Xs) + 1) div 2, lists:sort(Xs)).

%% One run in a new process: {Microseconds, {NTokens, Hash}, EndMemory}.
run_once(LEX, Input, F) ->
    Self = self(),
    Pid = spawn(fun() -> worker(Self, F) end),
    Pid ! {go, LEX, Input},
    receive {done, Pid, Us, Check, EndMem} -> {Us, Check, EndMem} end.

worker(Parent, F) ->
    receive {go, LEX, Input} -> ok end,
    T0 = erlang:monotonic_time(microsecond),
    Tokens = F(LEX, Input),
    T1 = erlang:monotonic_time(microsecond),
    {memory, Mem} = process_info(self(), memory),
    Parent ! {done, self(), T1 - T0, {length(Tokens), erlang:phash2(Tokens)}, Mem}.

%% A run with GC tracing: counts, words copied, peaks.
run_traced(LEX, Input, F) ->
    Self = self(),
    Pid = spawn(fun() -> worker(Self, F) end),
    erlang:trace(Pid, true, [garbage_collection, {tracer, Self}]),
    Pid ! {go, LEX, Input},
    receive {done, Pid, Us, Check, EndMem} -> ok end,
    Ref = erlang:trace_delivered(Pid),
    receive {trace_delivered, Pid, Ref} -> ok end,
    GC = gc_events(Pid, undefined, #{minor => 0, major => 0, copied => 0,
                                     peak_heap => 0, peak_bin => 0}),
    {Us, Check, GC#{end_mem => EndMem}}.

gc_events(Pid, Start, Acc) ->
    receive
        {trace, Pid, Event, Info} when Event == gc_minor_start; Event == gc_major_start ->
            gc_events(Pid, Info, Acc);
        {trace, Pid, Event, Info} when Event == gc_minor_end; Event == gc_major_end ->
            G = fun(K, L) -> proplists:get_value(K, L, 0) end,
            Copied = case Event of
                         gc_minor_end ->
                             G(heap_size, Info) + max(0, G(old_heap_size, Info) - G(old_heap_size, Start));
                         gc_major_end ->
                             G(heap_size, Info) + G(old_heap_size, Info)
                     end,
            Kind = case Event of gc_minor_end -> minor; gc_major_end -> major end,
            Heap = G(heap_block_size, Info) + G(old_heap_block_size, Info),
            %% off-heap binaries on the old heap after the GC: above
            %% min_bin_vheap_size they make GCs full sweeps
            Bin = G(bin_old_vheap_size, Info),
            gc_events(Pid, undefined,
                      Acc#{Kind := maps:get(Kind, Acc) + 1,
                           copied := maps:get(copied, Acc) + Copied,
                           peak_heap := max(maps:get(peak_heap, Acc), Heap),
                           peak_bin := max(maps:get(peak_bin, Acc), Bin)})
    after 0 ->
            Acc
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The variants. Each returns the list of tokens.

binary_default(LEX, Bin) ->
    lex:reset_pos(LEX),
    lex:lex_binary(LEX, Bin).

binary_tuned(LEX, Bin) ->
    lex:with_gc_limits([vheap, heap], byte_size(Bin), fun() -> binary_default(LEX, Bin) end).

byte_list(LEX, Bin) ->
    List = binary_to_list(Bin),
    lex:reset_pos(LEX),
    lex:lex_iolist(LEX, List).

heap_binaries(LEX, Bin) ->
    Chunks = copies(Bin, 64),
    lex:reset_pos(LEX),
    lex:lex_iolist(LEX, Chunks).

%% The whole binary as a list of copies of at most F bytes.
copies(Bin, F) when byte_size(Bin) =< F -> [binary:copy(Bin)];
copies(Bin, F) ->
    <<Part:F/binary, Rest/binary>> = Bin,
    [binary:copy(Part) | copies(Rest, F)].

copy_fragments(LEX, Bin, F) ->
    incremental(LEX, fun() -> next_part(Bin, F, true) end).

subbin_fragments(LEX, Bin, F) ->
    incremental(LEX, fun() -> next_part(Bin, F, false) end).

next_part(Bin, F, Copy) ->
    case split(Bin, F) of
        none -> eof;
        {Part, Rest} ->
            {case Copy of true -> binary:copy(Part); false -> Part end,
             fun() -> next_part(Rest, F, Copy) end}
    end.

%% The first F bytes (or fewer, at the end) and the rest
split(<<>>, _F) -> none;
split(Bin, F) when byte_size(Bin) =< F -> {Bin, <<>>};
split(Bin, F) -> <<Part:F/binary, Rest/binary>> = Bin, {Part, Rest}.

producer_fragments(LEX, Bin, F) ->
    Producer = spawn_link(fun() -> producer(Bin, F) end),
    incremental(LEX, fun() -> ask(Producer) end).

producer(Bin, F) ->
    receive
        {next, From} ->
            case split(Bin, F) of
                none -> From ! {part, self(), eof};
                {Part, Rest} ->
                    From ! {part, self(), binary:copy(Part)},
                    producer(Rest, F)
            end
    end.

ask(Producer) ->
    Producer ! {next, self()},
    receive
        {part, Producer, eof} -> eof;
        {part, Producer, Part} -> {Part, fun() -> ask(Producer) end}
    end.

%% Lex with the non-blocking incremental API, getting fragments from
%% More() -> {Fragment, More1} | eof.
incremental(LEX, More) ->
    incremental(lex:start(LEX), More, []).

incremental(C, More, Acc) ->
    case lex:tokens(C) of
        {more, Tokens, C1} ->
            case More() of
                {Part, More1} -> incremental(lex:feed(C1, Part), More1, [Tokens | Acc]);
                eof -> incremental(lex:feed(C1, eof), fun() -> eof end, [Tokens | Acc])
            end;
        {done, Tokens} ->
            lists:append(lists:reverse([Tokens | Acc]))
    end.

corpus(Bytes) ->
    Files = [F || F <- filelib:wildcard("src/*.erl"),
                  filename:basename(F) =/= "lex_unicode_data.erl"],
    One = iolist_to_binary([element(2, file:read_file(F)) || F <- lists:sort(Files)]),
    binary:copy(One, Bytes div byte_size(One) + 1).
