%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% erlang-lex: a table-driven lexer generator.
%%
%% See doc/README.md for documentation, discussion, usage, etc.

-module(lex).
-author('thomasl_erlang@yahoo.com').
-export(
   [one/2,
    one/3,
    with/2,
    with/3,
    with/4
   ]).

%% non-blocking incremental lexing
-export(
   [start/1,
    feed/2,
    next/1,
    tokens/1,
    rest/1,
    position/1
   ]).
-export([o_with/2,
	 regexps_to_table/1,
	 regexps_to_table/2,
	 regexps_to_nfa/1,
	 nfa_to_dfa/1,
	 minimize_dfa/1,
	 dfa_to_table/2,
	 dfa_to_table/3,
	 dfa_to_table/4,
	 dfa_to_table/5
	]).

%% library functions for using the generated lexer
-export([o_longest/2]).
%-export([longest/2]).

%% useful token-related exports ("library")
-export([item/1,
	 op/1,
	 token/1,
	 raw_token/1,
	 enclosed_token/2,
	 enclosed_token/3,
	 quoted_atom/0,
	 string/0,
	 no_token/0,
	 comment/0,
	 whitespace/0,
	 newline/0
	]).
-export(
   %% position counting, currently not used
   %% (not the same as line counting)
   [
    start_pos/0,
    inc_pos/1,
    curr_char/2
   ]).
-export([strip_c_comments/1,
	 resolve_line/1]).

%% library function for safely quoting REGEXP LITERALS that may 
%% contain reserved chars

-export(
   [safe_literal/1]
  ).

%% library functions for counting the number of lines
-export([no_lines/0,
	 inc_lines/0,
	 inc_lines/1,
	 current_line/0,
	 current_column/0,
	 current_position/0,
	 count_lines/1,
	 count_lines/2
	]).

%% The DFA table, in either representation (for tests and tools)
-export([transition/3]).

%% ?max_char is currently used to decide the "charset size"
%% we permit characters 0-?maxchar (excepting ?no_match,)
%%
%% The table always works on bytes. Unicode encodings are handled by
%% lowering each character (class) to byte sequences, see lex_unicode.erl.

-define(max_char, 255).

%% Byte 0 is needed for UTF-16/UTF-32 (and is a legal latin1 char).
%% Rows are tuples, so next_state/2 indexes with Char+1.

-define(min_char, 0).

%% process dictionary key for the current position, see
%% "Line and column counting"

-define(pos_key, lex_position).

%% ?no_match is a reserved state indicating error/no match possible.
%% It is -1 in tuple tables; binary tables (see "Binary tables") use 0.

-define(no_match, -1).

%% For compact tables, the value must be unsigned (which is a drawback).

-define(compact_no_match, 255).

%% Binary tables (option {table, binary}), see "Binary tables" below:
%% the table is {bin8 | bin16 | bin32, Binary}.

-define(is_bin_table_tag(Tag), (Tag == bin8 orelse Tag == bin16 orelse Tag == bin32)).

-ifdef(DEBUG).
-define(log(Str, Xs), io:format("Log: " ++ Str, Xs)).
-define(warning(Str, Xs), io:format("Warning: " ++ Str, Xs)).
-else.
-define(log(Str, Xs), ok).
-define(warning(Str, Xs), ok).
-endif.

%% quick patch
%-define(dbg(Str, Xs), io:format("~p:~p: " Str, [?MODULE, ?LINE | Xs])).
-define(dbg(Str, Xs), ok).

%% Test for accepting state

-define(is_accepting(N, AccLim), 
	((N) =< (AccLim))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This provides lexing of an entire stream/list, similar to lex:with/2,
%% though using one/2
%% 
%% A parser that incrementally calls the lexer should call one/2 directly
%% when another token is desired.
%%
%% NB: Current behavior on no match is very strict - it is possible we would
%%  prefer just doing like unix lex and 'emit the character' and restart lexing
%%  on the next position

with(Regexp_rules, Str) ->
    with(Regexp_rules, Str, []).

%% Options: see regexps_to_table/2. Input is a binary or a list of bytes
%% (in the legacy 'bytes' encoding, a list of chars 0..255 as before).
%% A binary is lexed by the binary driver (see "Binary driver" below),
%% a list by the list driver; both give the same tokens.

with(Regexp_rules, Bin, Opts) when is_binary(Bin) ->
    %% see "Garbage collection" under "Binary driver"
    with_gc_limits(
      [vheap, heap], byte_size(Bin),
      fun() ->
	      LEX = regexps_to_table(Regexp_rules, Opts),
	      reset_pos(LEX),
	      lex_binary(LEX, Bin)
      end);
with(Regexp_rules, IoList, Opts) when is_list(IoList) ->
    %% A list is on the heap already, so only binaries in it matter.
    with_gc_limits(
      [vheap], io_size(IoList),
      fun() ->
	      LEX = regexps_to_table(Regexp_rules, Opts),
	      reset_pos(LEX),
	      lex_iolist(LEX, IoList)
      end).

%% the size of iodata; 0 if it holds anything else (which the lexer then
%% reports when it gets there)

io_size(IoList) ->
    try iolist_size(IoList)
    catch error:badarg -> 0
    end.

%% (testing: convert file to string)
f(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

lex_string(LEX, Str) ->
    lex_string(tuple_table(LEX), Str, []).

lex_string(LEX, Str, Acc) ->
    case one_list(LEX, Str) of
	{token, T, RestStr} ->
	    lex_string(LEX, RestStr, [T|Acc]);
	{no_match, _Line, _AccToken, _Remains} = NOMATCH ->
	    exit(NOMATCH);
	end_of_stream ->
	    %% finished cleanly, emit the accumulated tokens
	    lists:reverse(Acc);
	Other ->
	    exit({unknown_return_value, Other})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Adapted from lex tokenization loop, BUT emits one token + next state
%% (or error or end of stream).
%%
%% This provides a more usual interface to lexing, like what the original
%% Unix lex/flex/... did. The parser should invoke this to get more tokens.
%%
%% NB: If a token is returned, we return {Token, RestString}.

%% Returns one of 
%%  {token, T, RestString}
%%  {no_match, Line, AccToken, RestString}
%%  end_of_stream

%%  ... or exits with {lex_no_match, ...} or (for the Unicode encodings)
%%  {lex_bad_encoding, ...}, see report_no_match/3.
%%
%% The input is iodata: a binary (see "Binary driver") or a list of
%% bytes, binaries and such lists (see "Iolist driver"). The rest is
%% returned in the same form: a sub-binary of a binary, or iodata for a
%% list (for a flat list of bytes, its tail). In the Unicode encodings
%% the action receives the token as a (reversed) list of code points.

one(LEX, Bin) when is_binary(Bin) ->
    one_binary(LEX, Bin);
one(LEX, IoList) when is_list(IoList) ->
    one_iolist(LEX, IoList).

%% The original list driver, for flat lists of bytes only. one/2 and
%% with/2,3 now use the iolist driver for lists, which gives the same
%% results (the tests check this against one_list/2 and lex_string/2).

one_list({lex, _, _, {Tag, _}, _, _, _, _} = LEX, String) when ?is_bin_table_tag(Tag) ->
    %% the list drivers only handle tuple tables (converting costs a
    %% whole table per token; lex_string/2 converts once)
    one_list(tuple_table(LEX), String);
one_list(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, _PosMode}, String) ->
    try longest_string(LEX, String)
    catch
	throw:{?MODULE, no_match, RevAcc, Remainder} ->
	    report_no_match(Enc, NL, RevAcc, Remainder)
    end.

%% Longest match applied to string (list of chars)

%% NB: one/2 does not reset the line count (that would restart the count
%%  at every token); call no_lines() before lexing a new input.

longest_string({lex, Start, AcceptLimit, Table, Actions, _Enc, _NL, _PosMode}, String) 
  when is_list(String) -> 
    longest_string(Start, Table, AcceptLimit, Actions, String);
longest_string(_LEX, String) ->
    exit({string_expected, String}).

longest_string(_Start, _Table, _AcceptLimit, _Actions, "") ->
    end_of_stream;
longest_string(Start, Table, AcceptLimit, Actions, String) ->
    Row = transition_table(Start, Table),
    pre_accept(String, none_acc(), Row, 
	       Table, AcceptLimit, Start, Actions).

%% [C|Cs] string to be lexed
%% Acc = accumulated possible match so far
%% State = current state
%% Nxt = state transition vector
%% Table = state transition table
%% AcceptLimit = max. # for accepting state (up to this are accepting)
%% Start = starting state
%%
%% If we reach an error state or end of string, stop with error. No match
%%  was possible.
%%
%% If we enter an accepting state, then go to accept
%%
%% Otherwise, keep going in pre_acc.
%%
%% *** UNFINISHED ***
%% - parameter State is dead? or something missing?

pre_accept([C|Cs], Acc, Nxt, Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% no match possible, fail
	    no_match(Acc, [C|Cs]);
        N when ?is_accepting(N, AcceptLimit) ->
	    %% move into accepting state N
	    NewNxt = transition_table(N, Table),
	    accept(Cs, acc(C, Acc), none_acc(), N, NewNxt, 
		   Table, AcceptLimit, Start, Actions);
	N ->
	    NewNxt = transition_table(N, Table),
	    pre_accept(Cs, acc(C, Acc), NewNxt, 
		       Table, AcceptLimit, Start, Actions)
    end;
pre_accept("", Acc, _Nxt, _Table, _AcceptLimit, _Start, _Actions) ->
    %% no match
    no_match(Acc, "").

%% [C|Cs] = lexed string
%% Acc = accumulated token
%% AccSt = previous accepted token
%% N = FSM state number
%% Nxt = state transition
%% Table = lex table
%% AcceptLimit = all states less than this are accepting
%%
%% If we enter an error state or the string ends, emit the current token
%%  and match next token (if possible)
%%
%% If we stay in an accepting state, go on.
%%
%% If we get into a non-accepting state, save the current match and
%%  go into post_accept

accept([C|Cs]=Lst, Acc, AccSt, State, Nxt, 
       Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% error state, so no further matching possible
	    %% - emit current token, reset
	    %% (note that AccSt is forgotten, since we now have a longer match)
	    %%
	    case emit_token(Actions, State, Acc) of
		no_token ->
		    longest_string(Start, Table, AcceptLimit, Actions, Lst);
		{token, T} ->
		    token(T, Lst)
	    end;
	N when ?is_accepting(N, AcceptLimit) ->
	    %% still in accepting state, keep accumulating
	    NewNxt = transition_table(N, Table),
	    accept(Cs, acc(C, Acc), AccSt, N, NewNxt, 
		   Table, AcceptLimit, Start, Actions);
	N ->
	    %% leaving accepting state, so save current token
	    NewNxt = transition_table(N, Table),
	    post_accept(Cs, acc(C, Acc), save_match(State, Acc, Lst), 
			N, NewNxt,
			Table, AcceptLimit, Start, Actions)
    end;
accept("", Acc, _AccSt, State, _Nxt, _Table, _AcceptLimit, _Start, Actions) ->
    %% no more to lex and we are in an accepting state => emit token
    %% and end
    case emit_token(Actions, State, Acc) of
	no_token ->
	    end_of_stream;
	{token, T} ->
	    token(T, "")
    end.

%% We have been in an accepting state (stored in AccSt) and are
%% now in a non-accepting state.
%%
%% If we enter an error state, or the string ends, emit the saved state 
%% and continue from there. 
%%
%% If we enter an accepting state, jolly good. Go back to accept.
%%
%% Otherwise, keep going.

post_accept([C|Cs]=_Lst, Acc, AccSt, _State, Nxt, 
	    Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% no more matching possible, reset to saved token
	    {State0, Acc0, Cs0} = reset_match(AccSt),
	    case emit_token(Actions, State0, Acc0) of
		no_token ->
		    %% In this case, resume getting a token
		    %% UNFINISHED - rename + rewrite params?
		    longest_string(Start, Table, AcceptLimit, Actions, Cs0);
		{token, T} ->
		    token(T, Cs0)
	    end;
	N when ?is_accepting(N, AcceptLimit) ->
	    %% we re-enter an accepting state
	    NewNxt = transition_table(N, Table),
	    accept(Cs, acc(C, Acc), AccSt, N, NewNxt,
		   Table, AcceptLimit, Start, Actions);
	N ->
	    %% still in non-accepting state, keep searching
	    NewNxt = transition_table(N, Table),
	    post_accept(Cs, acc(C, Acc), AccSt, N, NewNxt,
			Table, AcceptLimit, Start, Actions)
    end;
post_accept("", _Acc, AccSt, _State, _Nxt, Table, 
	    AcceptLimit, Start, Actions) ->
    %% no more matching, reset to saved token and continue from there
    {State0, Acc0, Cs0} = reset_match(AccSt),
    case emit_token(Actions, State0, Acc0) of
	no_token ->
	    longest_string(Start, Table, AcceptLimit, Actions, Cs0);
	{token, T} ->
	    token(T, Cs0)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Binary driver
%%
%% Lexes a single binary directly, without converting it to a list.
%% Same longest-match semantics, actions, positions and errors as the
%% list driver:
%%
%%   one_binary(LEX, Bin) -> {token, T, Rest} | end_of_stream
%%     Rest is a sub-binary of Bin (no copying).
%%   lex_binary(LEX, Bin) -> [T]
%%
%% scan/9 walks the binary a byte at a time with a binary match on its
%% tail, which the compiler turns into a match context that is reused
%% from byte to byte. Like the list driver, it collects the bytes read
%% so far as a reversed list (which is what actions get), and remembers
%% the state, length and byte list of the longest accepting prefix so
%% far. When the DFA gets stuck or the input ends, that prefix is the
%% token; the input is then restarted after it, which is the
%% backtracking of the list driver. (Taking the token as a sub-binary and
%% converting it afterwards was measured to be slower, in particular for
%% UTF-16 and UTF-32.)

one_binary(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, process}, Bin) ->
    try bin_longest(LEX, Bin)
    catch
	throw:{?MODULE, bin_no_match, Bin1, Len, Pos} ->
	    bin_report_no_match(Enc, NL, Pos, Bin1, Len)
    end;
one_binary(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, _Enc, _NL, unboxed}, Bin) ->
    one_binary(to_argument(LEX), Bin);
one_binary(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, argument}, Bin) ->
    try bin_longest_a(LEX, Bin, entry_pos(argument)) of
	{token, T, Rest, Pos1} ->
	    exit_pos(Pos1),
	    {token, T, Rest};
	{end_of_stream, Pos1} ->
	    exit_pos(Pos1),
	    end_of_stream
    catch
	throw:{?MODULE, bin_no_match, Bin1, Len, Pos} ->
	    bin_report_no_match(Enc, NL, Pos, Bin1, Len)
    end.

%% Garbage collection. Lexing a whole binary builds a list of tokens
%% about as large as the input, with many garbage collections on the way.
%% Two process settings matter, and with/2,3 raises both for the
%% duration of the call, including building the table (then restores
%% them), see with_gc_limits/3. (For a list, only min_bin_vheap_size:
%% the list itself is already on the heap.)
%%
%% - min_bin_vheap_size: the input binary is referenced from the old
%%   heap, and counts against its "virtual binary heap". If the binary is
%%   larger than that limit (default 46422 words, 371 KB), nearly every
%%   collection becomes a full one, copying all tokens so far: for a 1 MB
%%   input this made lexing about 3 times slower. Raised to twice the
%%   size of the binary.
%% - min_heap_size: raised to 2 words per input byte, so the heap does not
%%   have to grow step by step (the list driver gets this for free, since
%%   its input list is already that large).
%%
%% Both are capped at 16M words (128 MB). With them, lexing a 1 MB binary
%% in a new process is as fast as or faster than lexing it as a list, and
%% uses less memory. one/2 cannot do this, since it returns after each
%% token; see doc/README.md for doing it around a one/2 loop.

-define(heap_words_per_byte, 2).
-define(max_gc_hint, 16#1000000).

with_gc_limits(Which, Size, Fun) ->
    Words = Size div erlang:system_info(wordsize) + 1,
    Limits = [ L || {Kind, L} <- [{vheap, {min_bin_vheap_size, 2 * Words}},
				  {heap, {min_heap_size, ?heap_words_per_byte * Size}}],
		    lists:member(Kind, Which) ],
    Restore = raise_gc_limits(Limits),
    try Fun()
    after
	[ process_flag(Flag, Old) || {Flag, Old} <- Restore ]
    end.

lex_binary(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, PosMode}, Bin) ->
    try
	case PosMode of
	    process -> lex_binary(LEX, Bin, []);
	    argument -> lex_binary_a(LEX, Bin, {1, 1, false}, []);
	    unboxed -> lex_binary_u(LEX, Bin, 1, 1, false, [])
	end
    catch
	throw:{?MODULE, bin_no_match, Bin1, Len, Pos} ->
	    bin_report_no_match(Enc, NL, Pos, Bin1, Len)
    end.

%% with/2,3,4 start at line 1: in process mode, reset the process
%% dictionary; in argument mode the position starts in the driver, and
%% the dictionary is not used at all.

reset_pos({lex, _, _, _, _, _, _, process}) -> no_lines();
reset_pos({lex, _, _, _, _, _, _, _ArgumentOrUnboxed}) -> ok.

%% An unboxed table as an argument mode one: for the drivers that return
%% after each token (one/2,3, the incremental interface), which hand the
%% position back to their caller anyway.

to_argument({lex, Start, AcceptLimit, Table, {unboxed, _Unboxed, Wrappers}, Enc, NL, unboxed}) ->
    {lex, Start, AcceptLimit, Table, Wrappers, Enc, NL, argument};
to_argument(LEX) ->
    LEX.

%% Raise each process flag to at least Value (capped); returns the
%% [{Flag, OldValue}] to restore.

raise_gc_limits(Limits) ->
    [ {Flag, process_flag(Flag, New)}
      || {Flag, Value} <- Limits,
	 New <- [min(Value, ?max_gc_hint)],
	 New > element(2, process_info(self(), Flag)) ].

lex_binary(LEX, Bin, Acc) ->
    case bin_longest(LEX, Bin) of
	{token, T, Rest} ->
	    lex_binary(LEX, Rest, [T|Acc]);
	end_of_stream ->
	    lists:reverse(Acc)
    end.

%% argument mode: the position is passed along

lex_binary_a(LEX, Bin, Pos, Acc) ->
    case bin_longest_a(LEX, Bin, Pos) of
	{token, T, Rest, Pos1} ->
	    lex_binary_a(LEX, Rest, Pos1, [T|Acc]);
	{end_of_stream, _Pos} ->
	    lists:reverse(Acc)
    end.

%% Find the next token, skipping no_token matches.

%% Process mode: the actions keep the position in the process
%% dictionary. Returns {token, T, Rest} or end_of_stream.

bin_longest(_LEX, <<>>) ->
    end_of_stream;
bin_longest(LEX = {lex, Start, AcceptLimit, Table, Actions, _Enc, _NL, _PosMode}, Bin) ->
    case scan_table(Bin, Start, Table, AcceptLimit) of
	{accept, State, Len, RevAcc} ->
	    <<_:Len/binary, Rest/binary>> = Bin,
	    case emit_token(Actions, State, RevAcc) of
		no_token ->
		    bin_longest(LEX, Rest);
		{token, T} ->
		    token(T, Rest)
	    end;
	{fail, Len} ->
	    throw({?MODULE, bin_no_match, Bin, Len, process})
    end.

%% Argument mode: Pos = {Line, Col, AfterCR} is passed to the actions,
%% which return the new one. Returns {token, T, Rest, Pos} or
%% {end_of_stream, Pos}.

bin_longest_a(_LEX, <<>>, Pos) ->
    {end_of_stream, Pos};
bin_longest_a(LEX = {lex, Start, AcceptLimit, Table, Actions, _Enc, _NL, _PosMode}, Bin, Pos) ->
    case scan_table(Bin, Start, Table, AcceptLimit) of
	{accept, State, Len, RevAcc} ->
	    <<_:Len/binary, Rest/binary>> = Bin,
	    case (element(State, Actions))(RevAcc, Pos) of
		{no_token, Pos1} ->
		    bin_longest_a(LEX, Rest, Pos1);
		{{token, T}, Pos1} ->
		    {token, T, Rest, Pos1}
	    end;
	{fail, Len} ->
	    throw({?MODULE, bin_no_match, Bin, Len, Pos})
    end.

%% scan(Bin, State, Table, AcceptLimit, N, RevAcc,
%%      LastState, LastLen, LastRevAcc)
%%   N = bytes read so far, RevAcc = those bytes reversed;
%%   LastState/LastLen/LastRevAcc = the longest accepting prefix so far
%%   (LastLen = 0: none yet; rules never match the empty string).
%%
%% Returns {accept, State, Len, RevAcc}, or {fail, N} with N the number
%% of bytes read before the DFA got stuck (or the input ended).

scan(<<C, Rest/binary>>, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    case element(C+1, element(State, Table)) of
	?no_match ->
	    scan_done(N, LastState, LastLen, LastRevAcc);
	Next when ?is_accepting(Next, AcceptLimit) ->
	    Acc = [C|RevAcc],
	    scan(Rest, Next, Table, AcceptLimit, N+1, Acc, Next, N+1, Acc);
	Next ->
	    scan(Rest, Next, Table, AcceptLimit, N+1, [C|RevAcc], LastState, LastLen, LastRevAcc)
    end;
scan(<<>>, _State, _Table, _AcceptLimit, N, _RevAcc, LastState, LastLen, LastRevAcc) ->
    scan_done(N, LastState, LastLen, LastRevAcc).

scan_done(N, _LastState, 0, _LastRevAcc) ->
    {fail, N};
scan_done(_N, LastState, LastLen, LastRevAcc) ->
    {accept, LastState, LastLen, LastRevAcc}.

%% Errors are reported as by the list driver. report_no_match/4 needs the
%% bytes of the failed token and a few more: a malformed character that
%% starts at or before the failing byte is at most 4 bytes long.

bin_report_no_match(Enc, NL, Pos, Bin, Len) ->
    <<Acc:Len/binary, Rest/binary>> = Bin,
    More = binary:part(Rest, 0, min(byte_size(Rest), 8)),
    report_no_match(Enc, NL, report_pos(Pos), lists:reverse(binary_to_list(Acc)),
		    binary_to_list(More)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Iolist driver
%%
%% Lexes iodata: a binary, or a possibly nested list of bytes and
%% binaries, possibly with a binary as the tail of a list
%% ([..., Bin1 | Bin2]), without flattening it or copying it to a binary.
%% Same longest-match semantics, actions, positions and errors as the
%% other drivers:
%%
%%   one_iolist(LEX, IoData) -> {token, T, Rest} | end_of_stream
%%     Rest is iodata: what remains of IoData, sharing its parts.
%%   lex_iolist(LEX, IoData) -> [T]
%%
%% The input is consumed through a cursor {Cur, Stack}: Cur is the
%% iodata being read, and Stack the iodata still to read after it (the
%% tails of the lists Cur came from), innermost first. Reading from a
%% list of bytes allocates nothing; a binary is read by io_scan_b/10
%% with a binary match (a reused match context, as in the binary
%% driver); a nested list or a binary in a list pushes the rest of the
%% list on Stack. As for binaries, the scan only remembers the length of
%% the longest accepting prefix; the cursor after the token is found
%% again by skipping that many bytes from the token start (io_skip/3).
%%
%% An element that is neither a byte, a binary nor a list (an integer
%% above 255, an atom, a bitstring that is not a binary) cannot be part
%% of a token; it is reported like a non-byte in a list (not_a_byte in
%% the Unicode encodings, lex_no_match in bytes mode).

one_iolist(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, process}, IoData) ->
    try io_longest(LEX, IoData, []) of
	{token, T, {Cur, Stack}} ->
	    {token, T, io_join(Cur, Stack)};
	end_of_stream ->
	    end_of_stream
    catch
	throw:{?MODULE, io_no_match, Cur, Stack, Len, Pos} ->
	    io_report_no_match(Enc, NL, Pos, Cur, Stack, Len)
    end;
one_iolist(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, _Enc, _NL, unboxed}, IoData) ->
    one_iolist(to_argument(LEX), IoData);
one_iolist(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, argument}, IoData) ->
    try io_longest_a(LEX, IoData, [], entry_pos(argument)) of
	{token, T, {Cur, Stack}, Pos1} ->
	    exit_pos(Pos1),
	    {token, T, io_join(Cur, Stack)};
	{end_of_stream, Pos1} ->
	    exit_pos(Pos1),
	    end_of_stream
    catch
	throw:{?MODULE, io_no_match, Cur, Stack, Len, Pos} ->
	    io_report_no_match(Enc, NL, Pos, Cur, Stack, Len)
    end.

lex_iolist(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, PosMode}, IoData) ->
    try
	case PosMode of
	    process -> lex_iolist(LEX, IoData, [], []);
	    argument -> lex_iolist_a(LEX, IoData, [], {1, 1, false}, []);
	    unboxed -> lex_iolist_u(LEX, IoData, [], 1, 1, false, [])
	end
    catch
	throw:{?MODULE, io_no_match, Cur, Stack, Len, Pos} ->
	    io_report_no_match(Enc, NL, Pos, Cur, Stack, Len)
    end.

lex_iolist(LEX, Cur, Stack, Acc) ->
    case io_longest(LEX, Cur, Stack) of
	{token, T, {Cur1, Stack1}} ->
	    lex_iolist(LEX, Cur1, Stack1, [T|Acc]);
	end_of_stream ->
	    lists:reverse(Acc)
    end.

lex_iolist_a(LEX, Cur, Stack, Pos, Acc) ->
    case io_longest_a(LEX, Cur, Stack, Pos) of
	{token, T, {Cur1, Stack1}, Pos1} ->
	    lex_iolist_a(LEX, Cur1, Stack1, Pos1, [T|Acc]);
	{end_of_stream, _Pos} ->
	    lists:reverse(Acc)
    end.

%% Find the next token, skipping no_token matches.

%% process mode

io_longest(LEX = {lex, Start, AcceptLimit, Table, Actions, _Enc, _NL, _PosMode}, Cur, Stack) ->
    case io_eof(io_scan_table(Cur, Stack, Start, Table, AcceptLimit, 0, [], 0, 0, [])) of
	{accept, State, Len, RevAcc} ->
	    {Cur1, Stack1} = io_skip(Cur, Stack, Len),
	    case emit_token(Actions, State, RevAcc) of
		no_token ->
		    io_longest(LEX, Cur1, Stack1);
		{token, T} ->
		    token(T, {Cur1, Stack1})
	    end;
	{fail, 0, at_end} ->
	    end_of_stream;
	{fail, Len, _} ->
	    throw({?MODULE, io_no_match, Cur, Stack, Len, process})
    end.

%% argument mode

io_longest_a(LEX = {lex, Start, AcceptLimit, Table, Actions, _Enc, _NL, _PosMode}, Cur, Stack, Pos) ->
    case io_eof(io_scan_table(Cur, Stack, Start, Table, AcceptLimit, 0, [], 0, 0, [])) of
	{accept, State, Len, RevAcc} ->
	    {Cur1, Stack1} = io_skip(Cur, Stack, Len),
	    case (element(State, Actions))(RevAcc, Pos) of
		{no_token, Pos1} ->
		    io_longest_a(LEX, Cur1, Stack1, Pos1);
		{{token, T}, Pos1} ->
		    {token, T, {Cur1, Stack1}, Pos1}
	    end;
	{fail, 0, at_end} ->
	    {end_of_stream, Pos};
	{fail, Len, _} ->
	    throw({?MODULE, io_no_match, Cur, Stack, Len, Pos})
    end.

%% io_scan(Cur, Stack, State, Table, AcceptLimit, N, RevAcc,
%%         LastState, LastLen, LastRevAcc)
%%   as scan/9 of the binary driver, reading from the cursor.
%%
%% Returns {accept, State, Len, RevAcc}, or {fail, N, stuck}: N bytes
%% were read before the DFA got stuck or met an element that is not a
%% byte, or at the end of the data
%%   {eof, State, N, RevAcc, LastState, LastLen, LastRevAcc}
%% which io_eof/1 turns into one of the others (the incremental driver
%% may instead get more data and resume the scan).

io_scan([B | T], Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)
  when is_integer(B), B >= 0, B =< 255 ->
    case element(B+1, element(State, Table)) of
	?no_match ->
	    io_done(stuck, N, LastState, LastLen, LastRevAcc);
	Next when ?is_accepting(Next, AcceptLimit) ->
	    Acc = [B|RevAcc],
	    io_scan(T, Stack, Next, Table, AcceptLimit, N+1, Acc, Next, N+1, Acc);
	Next ->
	    io_scan(T, Stack, Next, Table, AcceptLimit, N+1, [B|RevAcc], LastState, LastLen, LastRevAcc)
    end;
io_scan([H | T], Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)
  when is_list(H); is_binary(H) ->
    io_scan(H, [T|Stack], State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
io_scan([], [Next | Stack], State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    io_scan(Next, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
io_scan([], [], State, _Table, _AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    %% the end of the data: the incremental driver may get more and resume
    {eof, State, N, RevAcc, LastState, LastLen, LastRevAcc};
io_scan(Bin, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)
  when is_binary(Bin) ->
    io_scan_b(Bin, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
io_scan(_NotIoData, _Stack, _State, _Table, _AcceptLimit, N, _RevAcc, LastState, LastLen, LastRevAcc) ->
    io_done(stuck, N, LastState, LastLen, LastRevAcc).

io_scan_b(<<C, Rest/binary>>, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    case element(C+1, element(State, Table)) of
	?no_match ->
	    io_done(stuck, N, LastState, LastLen, LastRevAcc);
	Next when ?is_accepting(Next, AcceptLimit) ->
	    Acc = [C|RevAcc],
	    io_scan_b(Rest, Stack, Next, Table, AcceptLimit, N+1, Acc, Next, N+1, Acc);
	Next ->
	    io_scan_b(Rest, Stack, Next, Table, AcceptLimit, N+1, [C|RevAcc], LastState, LastLen, LastRevAcc)
    end;
io_scan_b(<<>>, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    io_scan([], Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Binary tables
%%
%% With {table, binary}, the table is {Tag, Bin}: one binary of
%% States*256 unsigned entries, the next state for (State, Byte) at index
%% (State-1)*256 + Byte, and 0 for no match (states start at 1). The
%% entry width is the smallest of 8, 16 and 32 bits that holds the
%% number of states (or the one given with {table, {binary, Bits}}),
%% and Tag is bin8, bin16 or bin32.
%%
%% The scan loops below are the tuple loops scan/9, io_scan/10 and
%% io_scan_b/10 with the lookup replaced, generated once per width so
%% that the width is a constant in the bit syntax (a width read at run
%% time made each lookup 40-60% slower). The drivers choose the loop
%% when they start a token: scan_table/4 and io_scan_table/10.
%% The tuple loops are unchanged. The old list drivers (one_list/2,
%% lex_string/2, o_longest/2) take tuple tables only and convert.

binary_table(LEX, tuple) ->
    LEX;
binary_table({lex, Start, AcceptLimit, Table, Actions, Enc, NL, PosMode}, Rep) ->
    States = tuple_size(Table),
    Bits = case Rep of
	       binary -> hd([B || B <- [8, 16, 32], States < 1 bsl B]);
	       {binary, B} when States < 1 bsl B -> B;
	       {binary, B} -> exit({table_too_large, States, {binary, B}})
	   end,
    Bin = << <<(case Next of ?no_match -> 0; _ -> Next end):Bits>>
	     || Row <- tuple_to_list(Table), Next <- tuple_to_list(Row) >>,
    {lex, Start, AcceptLimit, {bin_table_tag(Bits), Bin}, Actions, Enc, NL, PosMode}.

bin_table_tag(8) -> bin8;
bin_table_tag(16) -> bin16;
bin_table_tag(32) -> bin32.

bin_table_bits(bin8) -> 8;
bin_table_bits(bin16) -> 16;
bin_table_bits(bin32) -> 32.

%% A binary table back to tuples (for the old list drivers)
tuple_table({lex, Start, AcceptLimit, {Tag, Bin}, Actions, Enc, NL, PosMode})
  when ?is_bin_table_tag(Tag) ->
    Bits = bin_table_bits(Tag),
    Rows = [list_to_tuple([case Next of 0 -> ?no_match; _ -> Next end
			   || <<Next:Bits>> <= Row])
	    || <<Row:(256 * Bits)/bits>> <= Bin],
    {lex, Start, AcceptLimit, list_to_tuple(Rows), Actions, Enc, NL, PosMode};
tuple_table(LEX) ->
    LEX.

scan_table(Bin, Start, {bin8, T}, AcceptLimit) ->
    scan8(Bin, Start, T, AcceptLimit, 0, [], 0, 0, []);
scan_table(Bin, Start, {bin16, T}, AcceptLimit) ->
    scan16(Bin, Start, T, AcceptLimit, 0, [], 0, 0, []);
scan_table(Bin, Start, {bin32, T}, AcceptLimit) ->
    scan32(Bin, Start, T, AcceptLimit, 0, [], 0, 0, []);
scan_table(Bin, Start, Table, AcceptLimit) ->
    scan(Bin, Start, Table, AcceptLimit, 0, [], 0, 0, []).

io_scan_table(Cur, Stack, State, {bin8, T}, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    io_scan8(Cur, Stack, State, T, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
io_scan_table(Cur, Stack, State, {bin16, T}, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    io_scan16(Cur, Stack, State, T, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
io_scan_table(Cur, Stack, State, {bin32, T}, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    io_scan32(Cur, Stack, State, T, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
io_scan_table(Cur, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    io_scan(Cur, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc).

%% The next state of State on byte C, in a table of Bits-bit entries:
%% binds Next (a macro, so that Bits is a literal in the pattern).
-define(bin_next(Next, State, C, Table, Bits),
	<<_:(((State - 1) bsl 8) bor C)/binary-unit:Bits, Next:Bits, _/binary>> = Table).

-define(BIN_SCAN(Scan, Bits),
Scan(<<C, Rest/binary>>, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    ?bin_next(Next, State, C, Table, Bits),
    case Next of
	0 ->
	    scan_done(N, LastState, LastLen, LastRevAcc);
	_ when ?is_accepting(Next, AcceptLimit) ->
	    Acc = [C|RevAcc],
	    Scan(Rest, Next, Table, AcceptLimit, N+1, Acc, Next, N+1, Acc);
	_ ->
	    Scan(Rest, Next, Table, AcceptLimit, N+1, [C|RevAcc], LastState, LastLen, LastRevAcc)
    end;
Scan(<<>>, _State, _Table, _AcceptLimit, N, _RevAcc, LastState, LastLen, LastRevAcc) ->
    scan_done(N, LastState, LastLen, LastRevAcc)).

-define(BIN_IO_SCAN(IoScan, IoScanB, Bits),
IoScan([B | T], Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)
  when is_integer(B), B >= 0, B =< 255 ->
    ?bin_next(Next, State, B, Table, Bits),
    case Next of
	0 ->
	    io_done(stuck, N, LastState, LastLen, LastRevAcc);
	_ when ?is_accepting(Next, AcceptLimit) ->
	    Acc = [B|RevAcc],
	    IoScan(T, Stack, Next, Table, AcceptLimit, N+1, Acc, Next, N+1, Acc);
	_ ->
	    IoScan(T, Stack, Next, Table, AcceptLimit, N+1, [B|RevAcc], LastState, LastLen, LastRevAcc)
    end;
IoScan([H | T], Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)
  when is_list(H); is_binary(H) ->
    IoScan(H, [T|Stack], State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
IoScan([], [Next | Stack], State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    IoScan(Next, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
IoScan([], [], State, _Table, _AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    {eof, State, N, RevAcc, LastState, LastLen, LastRevAcc};
IoScan(Bin, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)
  when is_binary(Bin) ->
    IoScanB(Bin, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc);
IoScan(_NotIoData, _Stack, _State, _Table, _AcceptLimit, N, _RevAcc, LastState, LastLen, LastRevAcc) ->
    io_done(stuck, N, LastState, LastLen, LastRevAcc)).

-define(BIN_IO_SCAN_B(IoScanB, IoScan, Bits),
IoScanB(<<C, Rest/binary>>, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    ?bin_next(Next, State, C, Table, Bits),
    case Next of
	0 ->
	    io_done(stuck, N, LastState, LastLen, LastRevAcc);
	_ when ?is_accepting(Next, AcceptLimit) ->
	    Acc = [C|RevAcc],
	    IoScanB(Rest, Stack, Next, Table, AcceptLimit, N+1, Acc, Next, N+1, Acc);
	_ ->
	    IoScanB(Rest, Stack, Next, Table, AcceptLimit, N+1, [C|RevAcc], LastState, LastLen, LastRevAcc)
    end;
IoScanB(<<>>, Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc) ->
    IoScan([], Stack, State, Table, AcceptLimit, N, RevAcc, LastState, LastLen, LastRevAcc)).

?BIN_SCAN(scan8, 8).
?BIN_SCAN(scan16, 16).
?BIN_SCAN(scan32, 32).
?BIN_IO_SCAN(io_scan8, io_scan_b8, 8).
?BIN_IO_SCAN(io_scan16, io_scan_b16, 16).
?BIN_IO_SCAN(io_scan32, io_scan_b32, 32).
?BIN_IO_SCAN_B(io_scan_b8, io_scan8, 8).
?BIN_IO_SCAN_B(io_scan_b16, io_scan16, 16).
?BIN_IO_SCAN_B(io_scan_b32, io_scan32, 32).

io_eof({eof, _State, N, _RevAcc, LastState, LastLen, LastRevAcc}) ->
    io_done(at_end, N, LastState, LastLen, LastRevAcc);
io_eof(Result) ->
    Result.

io_done(How, N, _LastState, 0, _LastRevAcc) ->
    {fail, N, How};
io_done(_How, _N, LastState, LastLen, LastRevAcc) ->
    {accept, LastState, LastLen, LastRevAcc}.

%% io_skip(Cur, Stack, N) -> {Cur, Stack} after N bytes. Only used on
%% bytes that io_scan/10 has read, so they are all there and all bytes.

io_skip(Cur, Stack, 0) ->
    {Cur, Stack};
io_skip([B | T], Stack, N) when is_integer(B) ->
    io_skip(T, Stack, N-1);
io_skip([H | T], Stack, N) ->
    io_skip(H, [T|Stack], N);
io_skip([], [Next | Stack], N) ->
    io_skip(Next, Stack, N);
io_skip(Bin, Stack, N) when is_binary(Bin) ->
    case byte_size(Bin) of
	Size when Size >= N ->
	    <<_:N/binary, Rest/binary>> = Bin,
	    {Rest, Stack};
	Size ->
	    io_skip([], Stack, N - Size)
    end.

%% The cursor as iodata: Cur followed by the elements of Stack.

io_join(Cur, []) ->
    Cur;
io_join(Cur, Stack) ->
    [Cur | Stack].

%% Errors are reported as by the list driver, from the bytes of the
%% failed token and a few more (see bin_report_no_match/4). An element
%% that is not a byte ends the list, so that it is reported as such.

io_report_no_match(Enc, NL, Pos, Cur, Stack, Len) ->
    Elements = io_take(Cur, Stack, Len + 8, []),
    {Acc, Rest} = lists:split(min(Len, length(Elements)), Elements),
    report_no_match(Enc, NL, report_pos(Pos), lists:reverse(Acc), Rest).

io_take(_Cur, _Stack, 0, Acc) ->
    lists:reverse(Acc);
io_take([B | T], Stack, K, Acc) when is_integer(B) ->
    io_take(T, Stack, K-1, [B|Acc]);
io_take([H | T], Stack, K, Acc) when is_list(H); is_binary(H) ->
    io_take(H, [T|Stack], K, Acc);
io_take([], [Next | Stack], K, Acc) ->
    io_take(Next, Stack, K, Acc);
io_take([], [], _K, Acc) ->
    lists:reverse(Acc);
io_take(<<C, Rest/binary>>, Stack, K, Acc) ->
    io_take(Rest, Stack, K-1, [C|Acc]);
io_take(<<>>, Stack, K, Acc) ->
    io_take([], Stack, K, Acc);
io_take([Bad | _], _Stack, _K, Acc) ->
    lists:reverse([Bad|Acc]);
io_take(Bad, _Stack, _K, Acc) ->
    lists:reverse([Bad|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Incremental driver
%%
%% Lexes a stream of data that arrives in chunks, e.g. from a socket. The
%% core does not block: it is a continuation that the caller feeds with
%% data as it arrives, and asks for tokens.
%%
%%   start(LEX) -> Cont
%%   feed(Cont, Data) -> Cont          Data is iodata, may be empty
%%   feed(Cont, eof) -> Cont           no more data will come
%%   next(Cont) -> {token, T, Cont} | {more, Cont} | end_of_stream
%%   tokens(Cont) -> {more, [T], Cont} | {done, [T]}
%%   rest(Cont) -> iodata              the data not yet lexed
%%   position(Cont) -> {Line, Column}  where the next token starts
%%
%% next/1 returns {more, Cont} when it needs data that has not been fed
%% yet; feed the next chunk (or eof) and call next/1 again. This suits a
%% gen_server that gets data as messages ({tcp, Socket, Data}).
%%
%% The blocking interface, with a function More() that the lexer calls
%% for data, is built on the core:
%%
%%   one(LEX, Data, More) -> {token, T, Rest, More1} | end_of_stream
%%   with(Rules, Data, More, Opts) -> [T]
%%
%% More() returns {more, Data, Cont} (Cont is called next time),
%% {last, Data} or ended; it is never called again after the last two.
%%
%% How it works: this is the iolist driver, with one difference. When
%% io_scan/10 gets to the end of the data, the scan has not failed yet:
%% its state is kept in the continuation, and when data is fed, the scan
%% resumes where it stopped, so a token (or a UTF-8 character) can span
%% chunks. The cursor at the start of the token is extended with each
%% chunk, so that the rest after the token, and the bytes of a failed
%% token for error reports, can still be found; backtracking can thus
%% also cross chunks. Chunks fed during a token are kept in reverse
%% (tail) and joined once per token, so feeding is O(1).
%%
%% By the longest-match rule a token that is complete at the end of the
%% data may still grow, so next/1 asks for more data before returning it.
%% Not if the DFA state at the end of the token has no transitions at
%% all: then no data can extend it, and the token is returned at once.
%% That way e.g. ";" or ")" at the end of a line are returned right away.
%%
%% When lexing fails in a Unicode encoding, the error report needs the 4
%% bytes after the failure (the longest character) to tell an invalid
%% character from one that is merely incomplete so far. Until they have
%% been fed (or eof), next/1 returns {more, Cont}, then it exits with the
%% error.
%%
%% Positions: each continuation has its own position, so one process can
%% lex several streams at once (e.g. a gen_server with many
%% connections). next/1 puts it in the process dictionary while it runs,
%% so that actions can use current_line/0 etc. as usual.

-record(lexc,
	{lex,			% the lex table
	 cur = [],		% cursor at the start of the token: Cur and
	 stack = [],		% Stack, followed by
	 tail = [],		% the chunks fed since, reversed
	 scan = none,		% none, or the suspended scan:
				%   {State, N, RevAcc, LastState, LastLen, LastRevAcc}
	 new = [],		% chunks fed since the scan was suspended, reversed
	 ended = false,		% true when eof has been fed
	 fail = none,		% none, or Len of a failed token whose error
				% report waits for more bytes
	 pos = {1, 1, false}	% {Line, Column, AfterCR}
	}).

start(LEX = {lex, _Start, _AcceptLimit, _Table, _Actions, _Enc, _NL, _PosMode}) ->
    #lexc{lex = to_argument(LEX)}.

feed(#lexc{ended = true}, _Data) ->
    exit(lex_feed_after_eof);
feed(C, eof) ->
    C#lexc{ended = true};
feed(C = #lexc{scan = none, tail = Tail}, Data) ->
    C#lexc{tail = [Data | Tail]};
feed(C = #lexc{tail = Tail, new = New}, Data) ->
    C#lexc{tail = [Data | Tail], new = [Data | New]}.

rest(#lexc{cur = Cur, stack = Stack, tail = Tail}) ->
    io_join(Cur, Stack ++ lists:reverse(Tail)).

position(#lexc{pos = {Line, Col, _AfterCR}}) ->
    {Line, Col}.

next(C = #lexc{lex = {lex, _, _, _, _, _, _, process}, pos = Pos}) ->
    %% process mode: the actions keep the position in the dictionary
    put(?pos_key, Pos),
    case next1(C) of
	{token, T, C1} ->
	    {token, T, C1#lexc{pos = position()}};
	{more, C1} ->
	    {more, C1#lexc{pos = position()}};
	end_of_stream ->
	    end_of_stream
    end;
next(C) ->
    %% argument mode: the position is passed to and from the actions
    next1(C).

tokens(C) ->
    tokens(C, []).

tokens(C, Acc) ->
    case next(C) of
	{token, T, C1} -> tokens(C1, [T|Acc]);
	{more, C1} -> {more, lists:reverse(Acc), C1};
	end_of_stream -> {done, lists:reverse(Acc)}
    end.

%% next1/1 runs with the position in the process dictionary

next1(C = #lexc{fail = Len}) when Len =/= none ->
    check_fail(C);
next1(C = #lexc{lex = {lex, Start, AcceptLimit, Table, _, _, _, _PosMode},
		cur = Cur, stack = Stack, tail = Tail, scan = none}) ->
    %% a new token: scan from its start, through all the data
    Stack1 = Stack ++ lists:reverse(Tail),
    C1 = C#lexc{stack = Stack1, tail = []},
    scanned(C1, io_scan_table(Cur, Stack1, Start, Table, AcceptLimit, 0, [], 0, 0, []));
next1(C = #lexc{lex = {lex, _Start, AcceptLimit, Table, _, _, _, _PosMode},
		scan = {State, N, RevAcc, LastState, LastLen, LastRevAcc}, new = New}) ->
    %% resume the scan with the chunks fed since it stopped
    C1 = C#lexc{scan = none, new = []},
    scanned(C1, io_scan_table(lists:reverse(New), [], State, Table, AcceptLimit,
			N, RevAcc, LastState, LastLen, LastRevAcc)).

scanned(C = #lexc{lex = {lex, _Start, _AcceptLimit, Table, _, _, _, _PosMode}, ended = Ended},
	{eof, State, N, RevAcc, LastState, LastLen, LastRevAcc} = Eof) ->
    case Ended orelse (LastLen == N andalso N > 0 andalso dead_state(State, Table)) of
	true ->
	    %% the end of the stream, or a token that cannot grow
	    finish(C, io_eof(Eof));
	false ->
	    {more, C#lexc{scan = {State, N, RevAcc, LastState, LastLen, LastRevAcc}}}
    end;
scanned(C, Result) ->
    finish(C, Result).

finish(C = #lexc{lex = {lex, _Start, _AcceptLimit, _Table, Actions, _, _, _PosMode},
		 cur = Cur, stack = Stack, tail = Tail}, Result) ->
    case Result of
	{accept, State, Len, RevAcc} ->
	    {Cur1, Stack1} = io_skip(Cur, Stack ++ lists:reverse(Tail), Len),
	    C1 = C#lexc{cur = Cur1, stack = Stack1, tail = []},
	    emitted(emit_c(C1, Actions, State, RevAcc));
	{fail, 0, at_end} ->
	    end_of_stream;
	{fail, Len, _} ->
	    check_fail(C#lexc{fail = Len})
    end.

%% Run the action. Process mode: the action keeps the position in the
%% process dictionary, and the result is used directly. Argument mode:
%% the position is in the continuation, and passed to the action.

emit_c(C = #lexc{lex = {lex, _, _, _, _, _, _, process}}, Actions, State, RevAcc) ->
    case emit_token(Actions, State, RevAcc) of
	no_token -> next1(C);
	{token, T} -> {token, T, C}
    end;
emit_c(C = #lexc{pos = Pos}, Actions, State, RevAcc) ->
    {Result, Pos1} = (element(State, Actions))(RevAcc, Pos),
    {emitted, Result, C#lexc{pos = Pos1}}.

emitted({emitted, no_token, C}) -> next1(C);
emitted({emitted, {token, T}, C}) -> {token, T, C};
emitted(Result) -> Result.

c_report_pos(#lexc{lex = {lex, _, _, _, _, _, _, process}}) -> position();
c_report_pos(#lexc{pos = Pos}) -> Pos.

%% A token of Len bytes failed. In the Unicode encodings, the error may
%% be a malformed character at or before the failing byte; with the bytes
%% fed so far, such a character may look incomplete, which more data can
%% turn into invalid (or show to be fine). So while it looks incomplete
%% and the stream has not ended, ask for more; otherwise report.

check_fail(C = #lexc{lex = {lex, _Start, _AcceptLimit, _Table, _Actions, Enc, NL, _PosMode},
		     cur = Cur, stack = Stack, tail = Tail, ended = Ended, fail = Len}) ->
    Stack1 = Stack ++ lists:reverse(Tail),
    case Ended orelse not looks_incomplete(Enc, Cur, Stack1, Len) of
	true ->
	    io_report_no_match(Enc, NL, c_report_pos(C), Cur, Stack1, Len);
	false ->
	    {more, C#lexc{stack = Stack1, tail = []}}
    end.

%% A character that starts at or before byte Len is at most 4 bytes long,
%% so Len + 4 bytes are enough to decide it.

looks_incomplete(Enc, _Cur, _Stack, _Len) when Enc == bytes; Enc == latin1 ->
    false;
looks_incomplete(Enc, Cur, Stack, Len) ->
    Bytes = io_take(Cur, Stack, Len + 4, []),
    case lex_unicode:first_error(Enc, Bytes, Len) of
	{incomplete, _Offset, _Bad} -> true;
	_ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The blocking interface: a function More() is called for data

one(LEX0 = {lex, _Start, _AcceptLimit, _Table, _Actions, _Enc, _NL, _PosMode}, Data, More)
  when is_function(More, 0) ->
    %% continue from the position in the process dictionary, as one/2
    LEX = {lex, _, _, _, _, _, _, PosMode} = to_argument(LEX0),
    C = feed(#lexc{lex = LEX, pos = position()}, Data),
    case drive(C, More) of
	{token, T, C1 = #lexc{pos = Pos1}, More1} ->
	    PosMode == argument andalso exit_pos(Pos1),
	    {token, T, rest(C1), More1};
	end_of_stream ->
	    end_of_stream
    end.

with(Regexp_rules, Data, More, Opts) when is_function(More, 0) ->
    LEX = regexps_to_table(Regexp_rules, Opts),
    reset_pos(LEX),
    lex_more(feed(start(LEX), Data), More, []).

lex_more(C, More, Acc) ->
    case drive(C, More) of
	{token, T, C1, More1} ->
	    lex_more(C1, More1, [T|Acc]);
	end_of_stream ->
	    lists:reverse(Acc)
    end.

%% Get the next token, calling More() for data when it is needed.
%% Returns {token, T, Cont, More} or end_of_stream.

drive(C, More) ->
    case next(C) of
	{token, T, C1} ->
	    {token, T, C1, More};
	{more, C1} ->
	    case next_data(More) of
		{Data, More1} ->
		    drive(feed(C1, Data), More1);
		no_more ->
		    drive(feed(C1, eof), fun no_more/0)
	    end;
	end_of_stream ->
	    end_of_stream
    end.

%% Call More() for the next chunk: {Data, NextMore} or no_more.

next_data(More) ->
    case More() of
	{more, Data, Cont} when is_function(Cont, 0) ->
	    {Data, Cont};
	{last, Data} ->
	    {Data, fun no_more/0};
	ended ->
	    no_more;
	Other ->
	    exit({bad_more_result, Other})
    end.

no_more() ->
    ended.

%% true if State has no transitions: an accepted token ending in it
%% cannot be extended

dead_state(State, {Tag, Bin}) when ?is_bin_table_tag(Tag) ->
    Size = 256 * bin_table_bits(Tag),
    <<_:((State - 1) * Size)/bits, Row:Size/bits, _/bits>> = Bin,
    Row =:= <<0:Size>>;
dead_state(State, Table) ->
    dead_row(element(State, Table), ?max_char + 1).

dead_row(_Row, 0) ->
    true;
dead_row(Row, I) ->
    element(I, Row) == ?no_match andalso dead_row(Row, I-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Unboxed whole-input drivers ({position, unboxed})
%%
%% The binary and iolist drivers for with/2,3, with the position as three
%% loop arguments, Line, Col and AfterCR, rather than a tuple. A
%% 3-argument action is called directly as Action(RevChars, Line, Col).
%% The position after the token is computed from the reversed token,
%% without reversing it: rev_cols/3 counts the characters after the last
%% line terminator (all of them if there is none), and only if there is
%% a terminator, rev_lines/4 counts the line breaks. Both return
%% integers, so nothing is allocated for the position.

lex_binary_u(_LEX, <<>>, _Line, _Col, _AfterCR, Acc) ->
    lists:reverse(Acc);
lex_binary_u(LEX = {lex, Start, AcceptLimit, Table, {unboxed, Actions, _}, Enc, _NL, _},
	     Bin, Line, Col, AfterCR, Acc) ->
    case scan_table(Bin, Start, Table, AcceptLimit) of
	{accept, State, Len, RevAcc} ->
	    <<_:Len/binary, Rest/binary>> = Bin,
	    RevChars = rev_chars(Enc, RevAcc),
	    case element(State, Actions) of
		{dict, Action} ->
		    put(?pos_key, {Line, Col, AfterCR}),
		    Result = Action(RevChars),
		    {Line0, Col0, AfterCR0} = position(),
		    u_next(binary, LEX, Rest, Result, RevChars, Line0, Col0, AfterCR0, Acc);
		Action ->
		    Result = Action(RevChars, Line, Col),
		    u_next(binary, LEX, Rest, Result, RevChars, Line, Col, AfterCR, Acc)
	    end;
	{fail, Len} ->
	    throw({?MODULE, bin_no_match, Bin, Len, {Line, Col, AfterCR}})
    end.

lex_iolist_u(LEX = {lex, Start, AcceptLimit, Table, {unboxed, Actions, _}, Enc, _NL, _},
	     Cur, Stack, Line, Col, AfterCR, Acc) ->
    case io_eof(io_scan_table(Cur, Stack, Start, Table, AcceptLimit, 0, [], 0, 0, [])) of
	{accept, State, Len, RevAcc} ->
	    Rest = io_skip(Cur, Stack, Len),
	    RevChars = rev_chars(Enc, RevAcc),
	    case element(State, Actions) of
		{dict, Action} ->
		    put(?pos_key, {Line, Col, AfterCR}),
		    Result = Action(RevChars),
		    {Line0, Col0, AfterCR0} = position(),
		    u_next(iolist, LEX, Rest, Result, RevChars, Line0, Col0, AfterCR0, Acc);
		Action ->
		    Result = Action(RevChars, Line, Col),
		    u_next(iolist, LEX, Rest, Result, RevChars, Line, Col, AfterCR, Acc)
	    end;
	{fail, 0, at_end} ->
	    lists:reverse(Acc);
	{fail, Len, _} ->
	    throw({?MODULE, io_no_match, Cur, Stack, Len, {Line, Col, AfterCR}})
    end.

%% advance over the token, then continue with the next one

u_next(Kind, LEX = {lex, _, _, _, _, _, NL, _}, Rest, Result, RevChars, Line, Col, AfterCR, Acc) ->
    case rev_cols(RevChars, NL, 0) of
	K when K < 0 ->
	    %% no line terminator: -K-1 more columns on the same line
	    u_cont(Kind, LEX, Rest, Result, Line, Col - K - 1, false, Acc);
	K ->
	    u_cont(Kind, LEX, Rest, Result, Line + rev_lines(RevChars, AfterCR, NL, 0), K + 1,
		   ends_in_cr(RevChars, NL), Acc)
    end.

u_cont(binary, LEX, Rest, no_token, Line, Col, AfterCR, Acc) ->
    lex_binary_u(LEX, Rest, Line, Col, AfterCR, Acc);
u_cont(binary, LEX, Rest, {token, T}, Line, Col, AfterCR, Acc) ->
    lex_binary_u(LEX, Rest, Line, Col, AfterCR, [T|Acc]);
u_cont(iolist, LEX, {Cur, Stack}, no_token, Line, Col, AfterCR, Acc) ->
    lex_iolist_u(LEX, Cur, Stack, Line, Col, AfterCR, Acc);
u_cont(iolist, LEX, {Cur, Stack}, {token, T}, Line, Col, AfterCR, Acc) ->
    lex_iolist_u(LEX, Cur, Stack, Line, Col, AfterCR, [T|Acc]).

%% the token's characters, reversed (bytes in bytes mode)

rev_chars(Enc, RevAcc) when Enc == bytes; Enc == latin1 ->
    RevAcc;
rev_chars(Enc, RevBytes) ->
    {Chars, []} = lex_unicode:decode_prefix(Enc, lists:reverse(RevBytes)),
    lists:reverse(Chars).

%% rev_cols(RevChars, NL, 0): the number of characters after the last
%% line terminator, or -(N+1) if there is none (N = all characters).

rev_cols([C | Cs], NL, K) when C > $\r, C < 16#85 ->
    rev_cols(Cs, NL, K+1);
rev_cols([C | Cs], NL, K) ->
    case is_line_terminator(C, NL) of
	true -> K;
	false -> rev_cols(Cs, NL, K+1)
    end;
rev_cols([], _NL, K) ->
    -K - 1.

%% rev_lines(RevChars, AfterCR, NL, 0): the number of line breaks, as
%% advance/3 counts them: the LF of a CR LF does not count, nor an LF
%% that starts the token right after a CR (AfterCR).

rev_lines([C | Cs], AfterCR, NL, N) when C > $\r, C < 16#85 ->
    rev_lines(Cs, AfterCR, NL, N);
rev_lines([$\n], true, _NL, N) ->
    N;
rev_lines([$\n, $\r | Cs], AfterCR, NL, N) when NL =/= lf ->
    rev_lines([$\r | Cs], AfterCR, NL, N);
rev_lines([C | Cs], AfterCR, NL, N) ->
    case is_line_terminator(C, NL) of
	true -> rev_lines(Cs, AfterCR, NL, N+1);
	false -> rev_lines(Cs, AfterCR, NL, N)
    end;
rev_lines([], _AfterCR, _NL, N) ->
    N.

ends_in_cr([$\r | _], NL) -> NL =/= lf;
ends_in_cr(_RevChars, _NL) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Quick entrypoint - old version

o_with(Regexp_rules, String) ->
    Tab = regexps_to_table(Regexp_rules),
    o_longest(Tab, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

regexps_to_table(Regexp_rules0) ->
    regexps_to_table(Regexp_rules0, []).

%% Options:
%%   {encoding, Enc}  how regexps and input are interpreted, one of
%%     bytes   (default) legacy: regexp chars are bytes; a non-ASCII
%%             literal in the regexp is matched as its UTF-8 bytes
%%     latin1  code points 0..255, one byte each
%%     utf8
%%     utf16be | utf16le | {utf16, big | little}  (utf16 = big endian)
%%     utf32be | utf32le | {utf32, big | little}  (utf32 = big endian)
%%
%%   case_insensitive | {case_insensitive, Bool}  (default false)
%%     all rules are case-insensitive, as if they started with (?i);
%%     (?-i) and (?-i:...) still switch it off
%%
%%   {position, process | argument | unboxed}  how the drivers keep the
%%     line and column (default process); see "Line and column counting"
%%     below
%%
%%   {table, tuple | binary | {binary, 8 | 16 | 32}}  how the DFA table
%%     is represented (default tuple); binary picks the smallest entry
%%     width that holds the states; see "Binary tables" below
%%
%%   {newline, NL}  which characters end a line, for line and column
%%     counting (see "Line and column counting" below):
%%     unicode  LF, VT, FF, CR, CR LF, NEL, LS, PS (UTS #18 RL1.6);
%%              the default, except in bytes mode
%%     ascii    LF, VT, FF, CR, CR LF; the default in bytes mode
%%     lf       LF only (as lex did before)
%%
%% In every encoding except 'bytes', the regexp is read as a string of
%% code points and may use named Unicode classes (\\p{...}, \\P{...};
%% see lex_unicode:property/1) and \\x{HHHHHH}. Each character or class
%% is compiled into the byte sequences that encode it, so the lexer
%% still runs one byte at a time but only accepts well-formed input.

regexps_to_table(Regexp_rules0, Opts) ->
    Enc = lex_unicode:normalize_encoding(
	    proplists:get_value(encoding, Opts, bytes)),
    Fold = proplists:get_bool(case_insensitive, Opts),
    NL = newline_option(Enc, proplists:get_value(newline, Opts, default)),
    PosMode = position_option(proplists:get_value(position, Opts, process)),
    TableRep = table_option(proplists:get_value(table, Opts, tuple)),
    Regexp_rules = regexps_to_internal(Regexp_rules0, {Enc, Fold}),
    NFA = regexps_to_nfa(Regexp_rules),
    DFA = minimize_dfa(nfa_to_dfa(NFA)),
    Lex_table = dfa_to_table(DFA, Regexp_rules, Enc, NL, PosMode),
    binary_table(Lex_table, TableRep).

%% In bytes mode the input may be UTF-8, where 16#85 (NEL in Unicode)
%% is a continuation byte, so only the ASCII terminators are allowed.

newline_option(bytes, default) -> ascii;
newline_option(_Enc, default) -> unicode;
newline_option(bytes, unicode) -> exit({unicode_newline_needs_encoding, bytes});
newline_option(_Enc, NL) when NL == unicode; NL == ascii; NL == lf -> NL;
newline_option(_Enc, NL) -> exit({unknown_newline, NL}).

%% How the drivers keep the position, see "Line and column counting"

position_option(Mode) when Mode == process; Mode == argument; Mode == unboxed -> Mode;
position_option(Mode) -> exit({unknown_position_mode, Mode}).

table_option(tuple) -> tuple;
table_option(binary) -> binary;
table_option({binary, Bits} = Rep) when Bits == 8; Bits == 16; Bits == 32 -> Rep;
table_option(Other) -> exit({unknown_table_option, Other}).

%% Convert all regexp rules to internal form
%%
%% Ctx = {Enc, Fold}: the encoding, and whether matching is
%% case-insensitive by default (option case_insensitive).

regexps_to_internal(Regexp_rules0, Ctx) ->
    [ regexp_to_internal(N, Rule, Ctx)
      || {N, Rule} <- lists:zip(lists:seq(1,length(Regexp_rules0)), Regexp_rules0) ].

%% Convert a single regexp rule to internal form. A rule is
%% {regexp, Regexp, Action} or {regexp, Prio, Regexp, Action}.

regexp_to_internal(N, {regexp, Regexp_str, Action}, Ctx) ->
    Prio = 0,
    {regexp, N, parse_regexp(Regexp_str, Ctx), Action, Prio};
regexp_to_internal(N, {regexp, Prio, Regexp_str, Action}, Ctx)
  when is_integer(Prio), Prio >= 0 ->
    {regexp, N, parse_regexp(Regexp_str, Ctx), Action, Prio}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% We currently handle a SUBSET of the regexps of Erlang/OTP:s regexp
%% matching (regexp(3)), which is a subset of AWKs.
%%  [Perhaps we should handle a subset of the regexps of Unix lex/flex?]
%%
%% c       char c
%% \c      escaped c [note! when inside string "\\c", this is a common case)
%% .       any char
%% ^       beginning of string
%% $       end of string
%% [abc]   characters abc
%% [^abc]  complement
%% [[:alpha:]]  POSIX class (also [:^alpha:]), inside [...]
%% r*      zero or more r
%% r+      one or more r
%% r?      zero or one r
%% r1r2    sequencing
%% (?i)r   case-insensitive r (only at the start of the regexp)
%% (?i:r)  case-insensitive r; (?-i:r) case-sensitive r
%%
%% {optional, RE}        r?
%% {comp_class, Chars}   complementation of chars
%% {pclosure, RE}        r+
%% {caseless, Bool, RE}  (?i:r), (?-i:r)
%% bos                   "beginning of string", ^
%% eos                   "end of string", $

parse_regexp(Regexp) ->
    parse_regexp(Regexp, {bytes, false}).

parse_regexp(Regexp, {bytes, Fold}) ->
    case lex_spec_parse:string(Regexp) of
  	{ok, RE} ->
  	    internal_form(RE, {bytes, Fold});
  	Err ->
  	    exit(Err)
    end;
parse_regexp(Regexp, Ctx) ->
    case lex_spec_parse:string(Regexp, unicode) of
  	{ok, RE} ->
  	    internal_form(RE, Ctx);
  	{error, Err} ->
  	    exit({regexp_error, Err, Regexp})
    end.

%% Convert to internal form. 
%%
%% Characters and classes are first sets of code points (in bytes mode:
%% bytes), then lowered to byte-level regexps by lower_set/2. For the
%% Unicode encodings, that gives the byte sequences encoding the set (see
%% lex_unicode:to_regexp/2), so the NFA/DFA construction is the same for
%% all encodings.
%%
%% Case-insensitive matching closes each set under case folding before
%% any complement is taken: [^a], \P{Lu} and [:^upper:] exclude both
%% cases (as in Perl). In bytes mode, only A-Z/a-z are folded.
%%
%% *** UNFINISHED ***
%% - does not handle beginning/end of string
%% - (why do we have two internal forms?)

internal_form(RE) ->
    internal_form(RE, {bytes, false}).

internal_form({kclosure, RE}, Ctx) ->
    {iter, internal_form(RE, Ctx)};
internal_form({'or', RE1, RE2}, Ctx) ->
    flatten_disj([internal_form(RE1, Ctx), internal_form(RE2, Ctx)]);
internal_form({optional, RE}, Ctx) ->
    {disj, [{seq, ""}, internal_form(RE, Ctx)]};
internal_form({concat, RE1, RE2}, Ctx) ->
    flatten_seq([internal_form(RE1, Ctx), internal_form(RE2, Ctx)]);
internal_form({pclosure, RE}, Ctx) ->
    RegExp = internal_form(RE, Ctx),
    {seq, [RegExp, {iter, RegExp}]};
internal_form({caseless, Fold, RE}, {Enc, _Fold}) ->
    internal_form(RE, {Enc, Fold});
internal_form({char_class, Elems}, {Enc, _} = Ctx) ->
    lower_set(Enc, class_set(Elems, Ctx));
internal_form({comp_class, Elems}, {Enc, _} = Ctx) ->
    lower_set(Enc, lex_unicode:complement(class_set(Elems, Ctx)));
internal_form({prop, _Neg, _Name} = Prop, {Enc, _} = Ctx) ->
    lower_set(Enc, class_set([Prop], Ctx));
internal_form(C, {Enc, _} = Ctx) when is_integer(C) ->
    case Enc =/= bytes andalso lex_unicode:byte_seqs(Enc, [{C, C}]) == [] of
	true -> exit({unencodable_char, C, Enc});
	false -> lower_set(Enc, elem_set(C, Ctx))
    end;
internal_form(Other, _Ctx) ->
    exit({regexp_not_handled, Other}).

lower_set(bytes, Set) ->
    case lex_unicode:intersect(Set, [{?min_char, ?max_char}]) of
	[{From, To}] -> {range, From, To};
	Ranges -> {disj, [ {range, From, To} || {From, To} <- Ranges ]}
    end;
lower_set(Enc, Set) ->
    lex_unicode:to_regexp(Enc, Set).

%% The set of a class: chars, {From, To}, {prop, Neg, Name} and
%% {posix, Neg, Name}. Every element set is closed under case folding
%% (if Fold), so the union is too.

class_set(Elems, Ctx) ->
    lex_unicode:normalize(lists:append([ elem_set(E, Ctx) || E <- Elems ])).

elem_set(C, Ctx) when is_integer(C) ->
    fold_set([{C, C}], Ctx);
elem_set({From, To}, Ctx) when is_integer(From), is_integer(To) ->
    fold_set([{From, To}], Ctx);
elem_set({prop, Neg, Name}, Ctx) ->
    negate(Neg, fold_set(named_set(lex_unicode:property(Name)), Ctx));
elem_set({posix, Neg, Name}, {Enc, _} = Ctx) ->
    Charset = case lex_unicode:is_unicode(Enc) of
		  true -> unicode;
		  false -> ascii
	      end,
    negate(Neg, fold_set(named_set(lex_unicode:posix_class(Name, Charset)), Ctx)).

named_set({ok, Set}) -> Set;
named_set({error, Reason}) -> exit(Reason).

negate(true, Set) -> lex_unicode:complement(Set);
negate(false, Set) -> Set.

fold_set(Set, {_Enc, false}) -> Set;
fold_set(Set, {bytes, true}) -> lex_unicode:ascii_case_closure(Set);
fold_set(Set, {_Enc, true}) -> lex_unicode:case_closure(Set).

internal_range({From, To}) 
  when is_integer(From), is_integer(To), From =< To ->
    {range, From, To};
internal_range(N) when is_integer(N) ->
    {range, N, N}.

%% Convert a complemented range into a non-complemented one

complement_char_ranges(MinChar, MaxChar, Char_ranges) ->
    complement(MinChar, MaxChar, [ internal_range(R) || R <- Char_ranges ]).

complement(MinChar, MaxChar, Ranges) ->
    compl(MinChar, MaxChar, sort_distinct_ranges(Ranges)).

%%

sort_distinct_ranges(Ranges) ->
    lists:sort(
      fun({range, From0, _To0}, {range, From1, _To1}) ->
	      From0 < From1
      end,
      Ranges
     ).

%% compl/3 generates new ranges spanning M..N but not the parameter Ranges
%%
%% E.g., compl(1, 255, [{range, 10, 10}]) -> [{range, 1, 9}, {range, 11, 255}]

compl(M, N, [{range, From, To}|Ranges]) when M =< N, From =< N ->
    if
	M < From ->
	    [{range, M, From-1}|compl(To+1, N, Ranges)];
	true ->
	    compl(To+1, N, Ranges)
    end;
compl(M, N, []) ->
    if
	M =< N ->
	    [{range, M, N}];
	true ->
	    []
    end.

%% Flatten a disjunction of disjunctions

flatten_disj(Xs) ->
    {disj, collect_disj(Xs)}.

collect_disj([{disj, Xs}|Rest]) ->
    collect_disj(Xs) ++ collect_disj(Rest);
collect_disj([X|Xs]) ->
    [X|collect_disj(Xs)];
collect_disj([]) ->
    [].

%% Flatten a sequence of sequences

flatten_seq(Xs) ->
    {seq, collect_seq(Xs)}.
    
collect_seq([{seq, Xs}|Rest]) ->
    collect_seq(Xs) ++ collect_seq(Rest);
collect_seq([X|Xs]) ->
    [X|collect_seq(Xs)];
collect_seq([]) ->
    [].

%% The abstract syntax tree of regexps:
%%
%%  {seq, [R]} | {disj, [R]} | {range, From, To} | {iter, R}
%%
%% Other regexps can (probably) be compiled into these. Check what flex is
%% capable of.

%% A finite automaton is represented as
%%  [{State, Accept, [Arc]}]
%%
%%  where State, NextState are integers
%%        Accept is non_accept | {accept, ID}
%%        Arc is an arc {range, From, To, NextState} | {epsilon, NextState}
%%  such that no two non-epsilon Ranges overlap
%%
%% A character range is {range, From, To} | Char | epsilon
%%  (possibly also {not, From, To}?)
%%
%% An automaton without epsilon-transitions is called deterministic (DFA).
%% One with epsilon-transitions is an NFA.
%%

%% Convert a collection of regexps into an NFA
%%
%% Translation system:
%%
%%  r1 ... rn => -> (r1) -> (r2) -> ... -> (rn) -> (accept)
%%   where (r) is the automaton for r and -> is an epsilon transition
%%   from the accepting state of automaton A to the entry state of B.
%%
%%  [range] => -> () -R-> (accept)
%%    where () is a new state and -R-> is an arc labelled with range R
%%
%%  (r1 | ... | rn) =>  () -> (ri) -> (accept)
%%    that is, an empty start state with epsilon transitions to ALL the
%%    automata for the regexps, which then epsilon-transit to the accept
%%    state
%%    * note: the toplevel NFAs must be handled differently, since each
%%      must end up in a different accepting state!
%%
%%  (r)* => -> (r)<- -> (accept)
%%    where <- is an arc from (r) back to (r) and -> (accept) is an
%%    epsilon arc to the accepting state
%%
%%
%% The automata are built up from end to start, so that every automaton
%% always has an existing "next state" to go to, the "accept state" which
%% is the parameter.
%%
%% NOTE: we assume that each NFA state has non-overlapping outgoing ranges.
%%  If you want to express c:(A -> B | A -> C), that we transition from
%%  A to B or C when c is found, then you MUST use epsilon transitions:
%%   (A eps-> S1, A eps-> S2, S1 -c-> B, S2 -c-> C)
%%
%% This is _equivalent_to_ having overlapping ranges, but is less terse
%% (for good or bad).

%% Regexps = [{regexp, RuleID, Regexp_syntree, Action, Prio}]
%%
%% The automaton for a regexp is
%%   () -> (ri) -> (accepti)
%% where
%% () is the starting state, (ri) the automaton for ri and (accepti)
%% the accepting state for ri (invoking its action)
%% and -> here is an epsilon transition.
%%
%% For all regexps, we have a single start state that (conceptually
%% at least) epsilon-transitions to each of the individual start states.
%%
%% NOTE: we ensure/assume that the accepting states are low-numbered.
%%  This is implicit in calling init_accepting_states/1 to get the FA.

regexps_to_nfa(Regexps) ->
    {AccStates, FA1} = init_accepting_states(Regexps),
    {StartStates, FA2} = nfa_states(Regexps, AccStates, FA1),
    {St, FA3} = add_epsilon_transitions(StartStates, FA2),
    set_fa_start_state(St, FA3).

%% Add initial accepting states, returns acc.states + FA

init_accepting_states(Regexps) ->
    lists:mapfoldl(
      fun({regexp, AccID, _Regexp, _Action, Prio}, FA0) ->
	      add_accepting_state(AccID, Prio, [], FA0)
      end,
      empty_fa(),
      Regexps
     ).

%% Add NFAs ending in accepting states, returns start states + FA

nfa_states(Regexps, AccStates, FA1) ->
    lists:mapfoldl(
      fun({{regexp, _AccID, Regexp, _Action, _Prio}, AccState}, FA_in) ->
	      regexp_to_nfa(Regexp, AccState, FA_in)
      end,
      FA1,
      lists:zip(Regexps, AccStates)
     ).

%% Add epsilon transition to all the start states

add_epsilon_transitions(StartStates, FA2) ->
    add_state([ {epsilon, Start} || Start <- StartStates ], FA2).

%% takes (Regexp, ExitState, FA) as arguments
%% returns {EntryState, NewFA}

regexp_to_nfa({seq, Rs}, Accept, FA) ->
    lists:foldr(
      fun(Regexp, {AccState, FA0}) ->
	      regexp_to_nfa(Regexp, AccState, FA0)
      end,
      { Accept, FA},
      Rs
     );
regexp_to_nfa({disj, Rs}, Accept, FA0) ->
    {StartStates, FA1} =
	lists:mapfoldl(
	  fun(Regexp, FA_in) ->
		  regexp_to_nfa(Regexp, Accept, FA_in)
	  end,
	  FA0,
	  Rs
	 ),
    add_state([ {epsilon, Start} || Start <- StartStates ], FA1);
regexp_to_nfa({range, From, To}, Accept, FA) ->
    add_state([{range, From, To, Accept}], FA);
regexp_to_nfa({iter, R}, Accept, FA0) ->
    {Last, FA1} = add_state([], FA0),
    {StartState, FA2} = regexp_to_nfa(R, Last, FA1),
    FA3 = add_epsilon(Last, StartState, FA2),
    {StartState, add_epsilon(StartState, Accept, FA3)}.

%% regexp_to_nfa({iter, R}, Accept, FA0) ->
%%  {StartState, FA1} = regexp_to_nfa(R, Accept, FA0)
%%  {StartState, add_epsilon(Accept, StartState, FA1)}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Converting an NFA to a DFA:
%%
%% There are two main issues
%% 1. getting rid of epsilon-transitions
%% 2. ensuring that an accepting state has a unique rule associated with it
%%    (or rather, the transition out of it)
%%
%% Getting rid of epsilon: (starting with start state)
%% - merge states accessible by epsilon transitions
%% - outgoing args are merged wrt their ranges
%% ... until none remain
%%
%% *** UNFINISHED ***
%% - the accepting states should be named 1..maxaccept to simplify
%%   checking if we are in one
%%   * this should probably be done by renaming them ...

nfa_to_dfa(NFA) ->
    Start = start_state(NFA),
    {NewStateName, Pre_DFA} = merge_nfa_states([Start], NFA, empty_pre_dfa()),
    DFA = pre_dfa_to_dfa(NewStateName, Pre_DFA),
    ?log("NFA states: ~w~nDFA states: ~w~n", 
	 [length(states_of(NFA)), length(states_of(DFA))]),
    DFA.

%% list of States to be merged and inserted into Done
%% - if already done, do nothing
%% - otherwise, merge it, then merge successors
%%
%% return {MergedStateName, Done_states}

%%
%% The epsilon closure is expensive for big NFAs (Unicode classes), so
%% the name is also cached under the (sorted) set States0 itself.

merge_nfa_states(States0, NFA, Done0) ->
    Raw = {raw, lists:usort(States0)},
    case states_present(Raw, Done0) of
	{found, Name} ->
	    {Name, Done0};
	not_found ->
	    merge_nfa_closure(Raw, States0, NFA, Done0)
    end.

merge_nfa_closure(Raw, States0, NFA, Done0) ->
    States = epsilon_reachable_all(States0, NFA),
    %% io:format("merged states = ~w~n", [States]),
    case states_present(States, Done0) of
	{found, Name} ->
	    {Name, alias_dfa_state(Raw, Name, Done0)};
	not_found ->
	    {DFA_name, Done1a} = name_dfa_state(States, Done0),
	    Done1 = alias_dfa_state(Raw, DFA_name, Done1a),
	    {state, Acc, Succs} = merge_epsilon_reachable_states(States, NFA),
	    {NewSuccs, Done2} = merge_nfa_succ_list(Succs, NFA, Done1),
	    {DFA_name, 
	     set_dfa_state(
	       DFA_name,
	       {state, Acc, NewSuccs},
	       Done2
	      )
	    }
    end.

merge_nfa_succ_list(Ranges, NFA, Done0) ->
    lists:mapfoldl(
      fun(Range, Done) ->
	      merge_nfa_succ(Range, NFA, Done)
      end,
      Done0,
      Ranges
     ).

merge_nfa_succ({From, To, Next}, NFA, Done0) ->
    {NewNext, Done1} = merge_nfa_states(Next, NFA, Done0),
    {{range, From, To, NewNext}, Done1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% How do we merge two (or more) states into one?
%% (a) epsilon transitions: ignore, we assume we will merge all
%%     epsilon-reachable states
%% (b) non-overlapping ranges: just keep
%% (c) overlapping ranges:
%%     - overlapping range to states s1,...,sn => {s1,...,sn} is a new state
%%       to be merged too
%%     (note: there may furthermore be PARTIAL and MULTIPLE overlaps.)

%% Merge epsilon-reachable states by merging their ranges
%%
%% Returns a new semi-state such that:
%% - name is union of states' names (= not a proper state name)
%% - accepting if precisely one of the states is accepting
%% - successors according to unions taken (these are pseudo-names, since
%%   the states as such DO NOT EXIST yet)

merge_epsilon_reachable_states(StateIDs, NFA) ->
    StatesInfo = [ state(StateID, NFA) || StateID <- StateIDs ],
    Acceptance =
	lists:foldl(
	  fun({state, Acc0, _Succs0}, Acc1) ->
		  lub_acceptance(Acc0, Acc1)
	  end,
	  non_accepting,
	  StatesInfo
	 ),
    %% should printout StatesInfo, Succs_list, Acceptance for debugging ...
    Succs_list = 
	[ ranges_to_set_form(Succs) || {state, _Acc0, Succs} <- StatesInfo ],
    case Succs_list of
	[] ->
	    {state, Acceptance, []};
	_ ->
	    MergedSuccs = lists:foldl(
			    fun merge_ranges/2,
			    hd(Succs_list),
			    tl(Succs_list)
			   ),
	    {state, Acceptance, MergedSuccs}
    end.

%% Two rules can match in the same accepting state. This function chooses
%% which rule is the actual match:
%% 1. best priority wins
%% 2. if same priority, compare rule IDs (least-numbered rule wins)
%%    and warn that this happened
%%
%% The warning is emitted because the rule selection may not have been
%% intended by the user.

lub_acceptance(non_accepting, X) ->
    X;
lub_acceptance(X, non_accepting) ->
    X;
lub_acceptance({accepting, Rule1, Prio1}, {accepting, Rule2, _Prio2}) 
  when Rule1 == Rule2 ->
    %% *** CHECK ***
    %% - should Prio2 be used?
    {accepting, Rule1, Prio1};
lub_acceptance({accepting, Rule1, Prio1}, {accepting, Rule2, Prio2}) 
  when Rule1 =/= Rule2 ->
    if
	Prio1 > Prio2 ->
	    {accepting, Rule1, Prio1};
	Prio1 < Prio2 ->
	    {accepting, Rule2, Prio2};
	Prio1 == Prio2 ->
	    %% in this case, disambiguate by comparing rule IDs
	    %% (arbitrary)
	    {Rule, Prio, _ShadowedRule} =
		if
		    Rule1 < Rule2 ->
			{Rule1, Prio1, Rule2};
		    Rule1 > Rule2 ->
			{Rule2, Prio2, Rule1};
		    Rule1 == Rule2 ->
			%% can never happen (handled by previous clauses)
			{Rule1, Prio1, Rule2}
		end,
	    ?warning(
	       "both ~w and ~w can be accepted at once,"
	       " ~w shadowed~n", 
	       [Rule1, Rule2, _ShadowedRule]),
	    {accepting, Rule, Prio}
    end.

-record(ranges, {r0, r1}).

%% This is the entry point: we have two states with successor-ranges
%% Ranges0 and Ranges1, merge them.
%%
%% Note: successor ranges must have LISTS of states in successor position.

merge_ranges(Ranges0, Ranges1) ->
    merge_ranges_with_ranges(Ranges0, Ranges1).

%%
      
ranges_to_set_form(Ranges0) ->
    lists:flatten([ convert_to_set_form(R0) || R0 <- Ranges0 ]).

%% Convert a range arc into suitable form for range-merging

convert_to_set_form({range, From, To, Next}) ->
    {From, To, singleton(Next)};
convert_to_set_form({epsilon, _Next}) ->
    %% discarded when list flattened
    [].

%% Merge the active range (From0, To0) with a list of "passive" ranges
%%
%% returns a new list of ranges

merge_range_with_ranges(From0, To0, S0, []) ->
    [{From0, To0, S0}];
merge_range_with_ranges(From0, To0, S0, [{From1, To1, S1}|Ranges]) ->
    #ranges{r0=Actives, r1=Passives} = 
	range_overlap(From0, To0, S0, From1, To1, S1),
    Passives ++ merge_ranges_with_ranges(Actives, Ranges).

%% Merge a ("active") list of ranges into a list of ("passive")
%% non-overlapping ranges.
%%
%% note: this is inefficient, since we insert Active into the list
%% then traverse it -- yet we are guaranteed that the Actives NEVER overlap

merge_ranges_with_ranges(Actives, Ranges0) ->
    lists:foldl(
      fun({From0, To0, S0}, Ranges) ->
	      merge_range_with_ranges(From0, To0, S0, Ranges)
      end,
      Ranges0,
      Actives
     ).

%% The relationship between two ranges R0=(From0, To0) and R1=(From1, To1)
%% where R0 is an _active_ range (being merged with a list of passive ranges)
%% and R1 is a passive range.
%%
%% We have a large number of cases:
%%   R0 == R1
%%   R0 and R1 do not overlap
%%   R0 includes R1 (on both sides, or one side) = 3 cases
%%   R1 includes R0 (on both sides, or one side) = 3 cases
%%   R0 and R1 partially overlap = 2 cases
%% sum: 10 cases
%%
%% We return #ranges{r0, r1}
%%  where r0 = new active ranges
%%        r1 = new passive ranges 

range_overlap(From0, To0, S0, From1, To1, S1) ->
    if
	From0 == From1,
	To0 == To1 ->
	    %% intervals are the same, so r0 is covered and the
	    %% new range in r1 leads to s0+s1
	    #ranges{r0 = [],
		    r1 = [{From0, To0, union(S0, S1)}]};
	To0 < From1 ; To1 < From0 ->
	    %% no overlap, just return both ranges
	    #ranges{r0=[{From0, To0, S0}],
		    r1=[{From1, To1, S1}]};
	From0 == From1,
	To0 < To1 ->
	    %% R0 inside R1, lower bound the same
	    %% - r0 range entirely covered (since inside r1)
	    %% - the r1 range splits into two, one leading to s0+s1
	    %%   the other just to s1
	    #ranges{r0=[],
		    r1=[{From0, To0, union(S0, S1)},
			{To0+1, To1, S1}]};
	From0 == From1,
	To0 > To1 ->
	    %% R1 inside R0, lower bound the same
	    %% - r0: the uncovered part returned, gets s0
	    %% - r1: add s0+s1 as states
	    #ranges{r0=[{To1+1, To0, S0}],
		    r1=[{From1, To1, union(S0, S1)}]};
	From0 > From1,
	To0 == To1 ->
	    %% R0 inside R1, upper bound the same
	    %% - r0 entirely covered
	    %% - r1 splits into two ranges, one covering r0, one not
	    %%   the covering one gets s0+s1, the other just s1
	    #ranges{r0=[],
		    r1=[{From1, From0-1, S1},
			{From0, To0, union(S0, S1)}]};
	From1 > From0,
	To0 == To1 ->
	    %% R1 inside R0, upper bound the same
	    %% - uncovered part of r0 gets s0
	    %% - overlapping part of r0 and r1 get s0+s1
	    #ranges{r0=[{From0, From1-1, S0}],
		    r1=[{From1, To1, union(S0, S1)}]};
	From0 > From1, To0 < To1 ->
	    %% R0 included in R1
	    %% - r0 entirely covered
	    %% - r1 splits into three intervals, middle one s0+s1, others s1
	    #ranges{r0=[],
		    r1=[{From1, From0-1, S1},
			{From0, To0, union(S0, S1)},
			{To0+1, To1, S1}]};
	From0 < From1, To0 > To1 ->
	    %% R1 included in R0
	    %% - r0 returns two intervals, states s0
	    %% - r1 gets middle interval, states s0+s1
	    #ranges{r0=[{From0, From1-1, S0},
			{To1+1, To0, S0}],
		    r1=[{From1, To1, union(S0, S1)}]};
	From0 < From1,
	To0 < To1 ->
	    %% R0 overlaps R1 (in lower range)
	    %% - R0 alone gets s0
	    %% - R0, R1 overlap gets s0+s1
	    %% - R1 alone gets s1
	    #ranges{r0=[{From0, From1-1, S0}],
		    r1=[{From1, To0, union(S0, S1)},
			{To0+1, To1, S1}]};
	From1 < From0,
	To1 < To0 ->
	    %% R1 overlaps R0 (in lower range)
	    %% - R1 alone gets s1 => r1
	    %% - R1, R0 overlap gets s0+s1 => r1
	    %% - R0 alone gets s0 => r0
	    #ranges{r0=[{To1+1, To0, S0}],
		    r1=[{From1, From0-1, S1},
			{From0, To1, union(S0, S1)}]};
	true ->
	    %% error case -- should never happen, right?
	    exit({range_compare_bug, {From0, To0}, {From1, To1}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% State = integer()
%% FA = finite_automaton()

epsilon_neighbours(State, FA) ->
    [ NextState || {epsilon, NextState} <- successors(State, FA) ].

% epsilon_reachable(State, FA) ->
%    All_seen = epsilon_reach(State, FA, none_seen()),
%    seen_list(All_seen).

epsilon_reachable_all(States, FA) ->
    All_seen = epsilon_reach_all(States, FA, none_seen()),
    seen_list(All_seen).

%% depth-first search for all epsilon-reachable states;
%% keep track of already seen states
%%
%% returns Seen

epsilon_reach(State, FA, Seen) ->
    case seen(State, Seen) of
	false ->
	    %% visit all epsilon-neighbours depth-first
	    epsilon_reach_all(
	      epsilon_neighbours(State, FA), 
	      FA, 
	      see(State, Seen)
	     );
	true ->
	    Seen
    end.

%% collect all epsilon-reachable states beginning from States

epsilon_reach_all(States, FA, Seen) ->
    lists:foldl(
      fun(State0, Seen0) ->
	      epsilon_reach(State0, FA, Seen0)
      end,
      Seen,
      States
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Minimizing the DFA (Dragon book 1st ed, section 3.9, algorithm 3.6)
%%
%% Partition the states, initially by acceptance (non-accepting, or
%% accepting a given rule), then repeatedly split each group by which
%% groups its transitions lead to, until no group splits. Each group
%% becomes one state. Missing transitions (no match) count as a
%% transition to the same implicit dead state.
%%
%% This matters for Unicode: the subset construction leaves many
%% equivalent states, in particular for the little-endian encodings,
%% where the low byte comes first.

minimize_dfa(DFA) ->
    IDs = states_of(DFA),
    States = maps:from_list([ {ID, state(ID, DFA)} || ID <- IDs ]),
    {Block0, N0} = number_blocks([ {ID, Acc} || ID <- IDs,
						{state, Acc, _} <- [maps:get(ID, States)] ]),
    {Block, N} = refine_blocks(IDs, States, Block0, N0),
    Reps = maps:from_list([ {B, ID} || ID <- lists:reverse(IDs),
				       B <- [maps:get(ID, Block)] ]),
    FA = lists:foldl(
	   fun(B, FA0) ->
		   {state, Acc, Succs} = maps:get(maps:get(B, Reps), States),
		   NewSuccs = [ {range, F, T, NB}
				|| {F, T, NB} <- block_signature(Succs, Block) ],
		   set_state(B, {state, Acc, NewSuccs}, FA0)
	   end,
	   empty_fa(),
	   lists:seq(1, N)),
    ?log("DFA states: ~w, minimized: ~w~n", [length(IDs), N]),
    set_next_id(N+1, set_start_state(maps:get(start_state(DFA), Block), FA)).

refine_blocks(IDs, States, Block, N) ->
    Keys = [ {ID, {maps:get(ID, Block), block_signature(Succs, Block)}}
	     || ID <- IDs, {state, _Acc, Succs} <- [maps:get(ID, States)] ],
    case number_blocks(Keys) of
	{_NewBlock, N} ->
	    {Block, N};
	{NewBlock, NewN} ->
	    refine_blocks(IDs, States, NewBlock, NewN)
    end.

%% [{ID, Key}] -> {#{ID => BlockNo}, NumBlocks}, blocks numbered from 1
%% in order of first appearance

number_blocks(Keys) ->
    {Block, _Seen, N} =
	lists:foldl(
	  fun({ID, Key}, {Bl, Seen, K}) ->
		  case Seen of
		      #{Key := B} -> {Bl#{ID => B}, Seen, K};
		      _ -> {Bl#{ID => K+1}, Seen#{Key => K+1}, K+1}
		  end
	  end,
	  {#{}, #{}, 0},
	  Keys),
    {Block, N}.

%% The transitions of a state in terms of blocks: sorted, with adjacent
%% ranges to the same block merged.

block_signature(Succs, Block) ->
    merge_block_ranges(
      lists:sort([ {F, T, maps:get(Nxt, Block)} || {range, F, T, Nxt} <- Succs ])).

merge_block_ranges([{F1, T1, B}, {F2, T2, B} | Rs]) when F2 == T1+1 ->
    merge_block_ranges([{F1, T2, B} | Rs]);
merge_block_ranges([R | Rs]) ->
    [R | merge_block_ranges(Rs)];
merge_block_ranges([]) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Converting a DFA to a suitable table + using the table for parsing.
%%
%% The code must do two things:
%% 1. generate a suitable structure for lexing
%% 2. invoke the correct action when a match is found
%%
%% NOTES:
%% We can represent the DFA as an array with rows = states and columns
%% = chars (e.g., 128 columns for 7-bit ASCII). When in state k and getting
%% character c, the next state is array[k,c].
%%
%% (In Erlang, the best representation of the array when states are
%% few, e.g., 16-bit or less, may be a binary.)
%%
%% *** UNFINISHED ***
%% * A number of possible methods:
%% - generate direct code
%% - generate a table (array)
%% - generate a binary, index into it (faster?)
%% - generate a decision-tree per state?
%% - generate an ets-table (?)

dfa_to_table(DFA, Regexp_rules) ->
    dfa_to_table(DFA, Regexp_rules, bytes).

dfa_to_table(DFA, Regexp_rules, Enc) ->
    dfa_to_table(DFA, Regexp_rules, Enc, newline_option(Enc, default)).

dfa_to_table(DFA, Regexp_rules, Enc, NL) ->
    dfa_to_table(DFA, Regexp_rules, Enc, NL, process).

dfa_to_table(DFA, Regexp_rules, Enc, NL, PosMode) ->
    {AcceptLimit, NewDFA} = group_accepting_states(DFA),
    ?log("~p accepting states~n", [AcceptLimit]),
    Actions = action_table(Regexp_rules, NewDFA, AcceptLimit, Enc, NL, PosMode),
    generate_code(NewDFA, AcceptLimit, Actions, Enc, NL, PosMode).

generate_code(DFA, AcceptLimit, Actions, Enc, NL, PosMode) ->
    Table = generate_table(DFA),
    Start = start_state(DFA),
    {lex, Start, AcceptLimit, Table, Actions, Enc, NL, PosMode}.

%% The table is a tuple of one row/element per state
%%
%% (Other approach: index as element(This_state*?chars + Char, Table)
%% - one extra multiply-add (2 BIFs)
%% - current approach: extra load + extra element/3
%% so, is it worth it?)
%%
%% *** UNFINISHED ***
%% I think we should check that states are contiguous and starting at 1
%% as well, since we kind of rely on this.

generate_table(DFA) ->
    States = lists:keysort(
	       1,
	       [ {ID, state(ID, DFA)} || ID <- states_of(DFA) ]),
    list_to_tuple([ generate_row(State) || State <- States ]).

%% each row is the transition function for the corresponding state,
%% a tuple of ?maxchar elements

generate_row({_ID, {state, _Acc, Succs}}) ->
    list_to_tuple(ranges_to_list(sort_successors(Succs))).

sort_successors(Succs) ->
    Sorts =
	lists:sort(
	  fun({range, From0, _To0, _St0}, {range, From1, _To1, _St1}) ->
		  From0 < From1
	  end,
	  Succs
	 ),
    %% io:format("sorted successors: ~w~n",[Sorts]),
    Sorts.

%% Generate a list of [St1, St2, ..., StN] where each Sti is the next state
%% to use

ranges_to_list(Ranges) ->
    lists:flatten(
      [ lists:duplicate(To-(From-1), NxtSt) 
	|| {From, To, NxtSt} <- rtol(?min_char, ?max_char, ?no_match, Ranges) ]
     ).

%% rtol "ranges to list of ranges"
%%
%% M: current "index"/Mth element of final list
%% N: max index
%% St: current state to emit
%% third argument is the ranges to emit

rtol(M, N, St, [{range, From, To, NxtSt}|Rs]) when M =< N ->
    [{M, From-1, St}, {From, To, NxtSt} | rtol(To+1, N, St, Rs)];
rtol(M, N, St, []) ->
    if M =< N ->
	    [{M, N, St}];
       true ->
	    []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% In order to quickly check that a state is accepting, ensure that the
%% accepting states are 1..maxaccept. If so, the test is a simple compare.
%%
%% This is not guaranteed by our NFA->DFA conversion (and is difficult
%% to do, since the accepting states may be many). We do it in a separate
%% pass instead.

group_accepting_states(FA) ->
    State_info =
	[ {ID, accepting_state(ID, FA)} || ID <- states_of(FA) ],
    Acc = [ ID || {ID, true} <- State_info ],
    N = length(Acc),
    Non_Acc = [ ID || {ID, false} <- State_info ],
    NumStates =
	length(State_info),
    Map = rename_map(Acc ++ Non_Acc, 1),
    NewStart = rename_state(start_state(FA), Map),
    NewFA = set_next_id(
	      NumStates+1,
	      set_start_state(
		NewStart, 
		rename_states(Map, FA))),
    {N, NewFA}.

rename_map([ID|IDs], NewID) ->
    [{ID, NewID} | rename_map(IDs, NewID+1)];
rename_map([], _NewID) ->
    [].

%% create a new FA with renamed states
%%
%% Note: we explicitly set the state NewID, though I think we could just
%% add a new state. But just adding states means we do not _ensure_ that
%% the NewID is correct, we just assume it is. So I avoid that.

rename_states(Map, FA) ->
    lists:foldl(
      fun({ID, NewID}, NewFA) ->
	      {state, Acc, Succ} = state(ID, FA),
	      NewSucc = [ {range, From, To, rename_state(St, Map)} 
			  || {range, From, To, St} <- Succ ],
	      NewState = {state, Acc, NewSucc},
	      set_state(NewID, NewState, NewFA)
      end,
      empty_fa(),
      Map
     ).

rename_state(X, Map) ->
    rename_state(X, Map, Map).

rename_state(X, [{X, Y}|_], _Map) ->
    Y;
rename_state(X, [_|Xs], _Map) ->
    rename_state(X, Xs);
rename_state(_X, [], Map) ->
    exit({state_not_found, Map}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OLD VERSION - lexing of entire list, returns list of tokens
%%  rather than the classic token-at-a-time.
%%
%% Driving the lexical analysis
%%
%% We provide functions that performs tokenization over a string or a
%% binary given a table to work with.
%%
%% There are three kinds of states:
%% - accepting
%% - non-accepting
%% - error (this means no match possible)
%%
%% Longest match: emit latest token when transiting into error state;
%%  save token when transiting from accepting to non-accepting state
%%  since we may reenter an accepting state later
%%  (this means more work and some backtracking, but is probably what
%%  is intended)

%% longest(Lex_table, Sequence) -> [Token]
%%
%% A sequence is a string (list of char) or a binary

o_longest({lex, _, _, {Tag, _}, _, _, _, _} = Lex, Seq) when ?is_bin_table_tag(Tag) ->
    o_longest(tuple_table(Lex), Seq);
o_longest(Lex = {lex, _, _, _, _, Enc, NL, _PosMode}, Seq) when is_list(Seq) ->
    try o_longest_string(Lex, Seq)
    catch
	throw:{?MODULE, no_match, RevAcc, Remainder} ->
	    report_no_match(Enc, NL, RevAcc, Remainder)
    end.

%% Longest match applied to string (list of chars)

o_longest_string({lex, Start, AcceptLimit, Table, Actions, _Enc, _NL, _PosMode}, String) 
  when is_list(String) -> 
    no_lines(),
    o_longest_string(Start, Table, AcceptLimit, Actions, String).

o_longest_string(Start, Table, AcceptLimit, Actions, String) ->
    Row = transition_table(Start, Table),
    o_pre_accept(String, none_acc(), Start, Row, 
	       Table, AcceptLimit, Start, Actions).

%% [C|Cs] string to be lexed
%% Acc = accumulated possible match so far
%% State = current state
%% Nxt = state transition vector
%% Table = state transition table
%% AcceptLimit = max. # for accepting state (up to this are accepting)
%% Start = starting state
%%
%% If we reach an error state or end of string, stop with error. No match
%%  was possible.
%%
%% If we enter an accepting state, then go to accept
%%
%% Otherwise, keep going in pre_acc.
%%
%% *** UNFINISHED ***
%% - parameter State is dead? or something missing?

o_pre_accept([C|Cs], Acc, _State, Nxt, Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% no match possible, fail
	    no_match(Acc, [C|Cs]);
        N when ?is_accepting(N, AcceptLimit) ->
	    %% move into accepting state N
	    NewNxt = transition_table(N, Table),
	    o_accept(Cs, acc(C, Acc), none_acc(), N, NewNxt, 
		   Table, AcceptLimit, Start, Actions);
	N ->
	    NewNxt = transition_table(N, Table),
	    o_pre_accept(Cs, acc(C, Acc), N, NewNxt, 
		    Table, AcceptLimit, Start, Actions)
    end;
o_pre_accept("", Acc, _State, _Nxt, _Table, _AcceptLimit, _Start, _Actions) ->
    %% no match
    no_match(Acc, "").

%% [C|Cs] = lexed string
%% Acc = accumulated token
%% AccSt = previous accepted token
%% N = state
%% Nxt = state transition
%% Table = lex table
%% AcceptLimit = all states less than this are accepting
%%
%% If we enter an error state or the string ends, emit the current token
%%  and match next token (if possible)
%%
%% If we stay in an accepting state, go on.
%%
%% If we get into a non-accepting state, save the current match and
%%  go into post_accept

o_accept([C|Cs]=Lst, Acc, AccSt, State, Nxt, 
       Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% error state, so no further matching possible
	    %% - emit current token, reset
	    %% (note that AccSt is forgotten, since we now have a better match)
	    %%
	    case emit_token(Actions, State, Acc) of
		no_token ->
		    o_longest_string(Start, Table, AcceptLimit, Actions, Lst);
		{token, T} ->
		    [T | o_longest_string(Start, Table, AcceptLimit, 
					Actions, Lst) ]
	    end;
	N when ?is_accepting(N, AcceptLimit) ->
	    %% still in accepting state, keep accumulating
	    NewNxt = transition_table(N, Table),
	    o_accept(Cs, acc(C, Acc), AccSt, N, NewNxt, 
		   Table, AcceptLimit, Start, Actions);
	N ->
	    %% leaving accepting state, so save current token
	    NewNxt = transition_table(N, Table),
	    o_post_accept(Cs, acc(C, Acc), save_match(State, Acc, Lst), 
			  N, NewNxt,
			  Table, AcceptLimit, Start, Actions)
    end;
o_accept("", Acc, _AccSt, State, _Nxt, _Table, _AcceptLimit, _Start, Actions) ->
    %% no more to lex and we are in an accepting state => emit token
    %% and end
    case emit_token(Actions, State, Acc) of
	no_token ->
	    [];
	{token, T} ->
	    [T]
    end.

%% We have been in an accepting state (stored in AccSt) and are
%% now in a non-accepting state.
%%
%% If we enter an error state, or the string ends, emit the saved state 
%% and continue from there. 
%%
%% If we enter an accepting state, jolly good. Go back to accept.
%%
%% Otherwise, keep going.

o_post_accept([C|Cs]=_Lst, Acc, AccSt, _State, Nxt, 
	    Table, AcceptLimit, Start, Actions) ->
    case next_state(C, Nxt) of
	?no_match ->
	    %% no more matching possible, reset to saved token
	    {State0, Acc0, Cs0} = reset_match(AccSt),
	    case emit_token(Actions, State0, Acc0) of
		no_token ->
		    o_longest_string(Start, Table, AcceptLimit, Actions, Cs0);
		{token, T} ->
		    [ T | o_longest_string(Start, Table, AcceptLimit, 
					   Actions, Cs0) ]
	    end;
	N when ?is_accepting(N, AcceptLimit) ->
	    %% we re-enter an accepting state
	    NewNxt = transition_table(N, Table),
	    o_accept(Cs, acc(C, Acc), AccSt, N, NewNxt,
		     Table, AcceptLimit, Start, Actions);
	N ->
	    %% still in non-accepting state, keep searching
	    NewNxt = transition_table(N, Table),
	    o_post_accept(Cs, acc(C, Acc), AccSt, N, NewNxt,
			  Table, AcceptLimit, Start, Actions)
    end;
o_post_accept("", _Acc, AccSt, _State, _Nxt, Table, 
	      AcceptLimit, Start, Actions) ->
    %% no more matching, reset to saved token and continue from there
    {State0, Acc0, Cs0} = reset_match(AccSt),
    case emit_token(Actions, State0, Acc0) of
	no_token ->
	    o_longest_string(Start, Table, AcceptLimit, Actions, Cs0);
	{token, T} ->
	    [ T | o_longest_string(Start, Table, AcceptLimit, Actions, Cs0) ]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% transition(State, Byte, Table) -> NextState | -1 (no match), where
%% Table is element 4 of a {lex, ...} table, in either representation.

transition(State, Byte, {Tag, Bin}) when ?is_bin_table_tag(Tag) ->
    Bits = bin_table_bits(Tag),
    <<_:(((State - 1) * 256 + Byte) * Bits)/bits, Next:Bits, _/bits>> = Bin,
    case Next of
	0 -> ?no_match;
	_ -> Next
    end;
transition(State, Byte, Table) ->
    element(Byte + 1, element(State, Table)).

%% Anything that is not a byte (e.g., a code point > 255 in a list) can
%% never match; it is reported by report_no_match/3.

next_state(Char, Trans_table)
  when is_integer(Char), Char >= ?min_char, Char =< ?max_char ->
    element(Char+1, Trans_table);
next_state(_Char, _Trans_table) ->
    ?no_match.

%%

transition_table(State, Table) ->
    element(State, Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Accumulated characters so far

none_acc() ->
    [].

acc(C, Cs) ->
    [C|Cs].

%%

emit_token(Actions, State, Acc) ->
    Action = lookup_action(Actions, State),
    Action(Acc).

%% Save a longest match (so far)

save_match(State, Acc, Cs) ->
    {match, State, Acc, Cs}.

%% Reset to a previously saved longest match

reset_match({match, State, Acc, Cs}) ->
    {State, Acc, Cs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% What to do when no regexp matched
%%
%% Current - exit with somewhat informative message
%%
%% Could also return a [{no_match, ...}] element, this was
%% done previously.
%%
%% The error gives the line and column (see "Line and column counting").

%% The drivers throw, and the entry points (one/2, o_longest/2) know the
%% encoding and turn the throw into an exit via report_no_match/4.

no_match(RevAcc, Remainder) ->
    throw({?MODULE, no_match, RevAcc, Remainder}).

%% When no rule matches, the position is where the unmatched token
%% starts, which is the current position (positions are advanced after
%% each match):
%%
%%   exit({lex_no_match, {line, L}, {column, C}})
%%
%% In the encodings other than bytes, we first check if the failure is
%% due to malformed input. The token started at a character boundary,
%% so we decode from there; if a malformed sequence starts at or before
%% the byte where the DFA failed, the input is at fault:
%%
%%   exit({lex_bad_encoding, Enc, Kind, {line, L}, {column, C},
%%         {bytes, BadBytes}})
%%
%%   Kind = invalid      malformed sequence (overlong UTF-8, unpaired
%%                       surrogate, code point > 10FFFF, ...)
%%        | incomplete   the input ends in the middle of a character
%%        | not_a_byte   the input list holds something other than 0..255
%%   L, C = the position of BadBytes

report_no_match(Enc, NL, RevAcc, Remainder) ->
    report_no_match(Enc, NL, position(), RevAcc, Remainder).

%% Pos = {Line, Col, AfterCR}: where the failed token starts

report_no_match(bytes, _NL, Pos, _RevAcc, _Remainder) ->
    no_match_exit(Pos);
report_no_match(Enc, NL, Pos, RevAcc, Remainder) ->
    Acc = lists:reverse(RevAcc),
    Bytes = Acc ++ Remainder,
    case lex_unicode:first_error(Enc, Bytes, length(Acc)) of
	{Kind, Offset, Bad} ->
	    {Good, _} = lex_unicode:decode_prefix(Enc, lists:sublist(Bytes, Offset)),
	    {Line, Col, _} = advance(Pos, Good, NL),
	    exit({lex_bad_encoding, Enc, Kind,
		  {line, Line},
		  {column, Col},
		  {bytes, Bad}});
	none ->
	    no_match_exit(Pos)
    end.

no_match_exit({Line, Col, _AfterCR}) ->
    exit({lex_no_match, {line, Line}, {column, Col}}).

%% The position to report: in process mode the drivers pass the atom
%% process, and the position is in the process dictionary.

report_pos(process) -> position();
report_pos(Pos) -> Pos.

%% For one/2,3: they continue from the position in the process
%% dictionary (as in process mode) and leave the new one there.

entry_pos(process) -> process;
entry_pos(argument) -> position().

exit_pos(process) -> ok;
exit_pos(Pos) -> put(?pos_key, Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

token(T, Cs0) ->
    ?dbg("Token ~p\n", [T]),
    {token, T, Cs0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The character to be read is indicated by the "current position" in
%% the binary. This position is an abstract value, only to be used with
%% curr_char/2 and inc_pos/1.
%%
%% - see the match expression in curr_char/2 for an explanation why
%%   we really store CurrPos-1 instead of CurrPos

%% This is the abstract "start of sequence" position.
%%
%% if start_pos() is changed, also change ?least_pos above

start_pos() ->
    0.

%% Increment position by one.
%%
%% = should be inlined

inc_pos(Pos) ->
    Pos+1.

%% Select the current character. Note that since binaries are 1-indexed,
%% and we use PrevPos as 0-indexed, we get a relatively simple match 
%% expression (should be compilable into nice code).
%%
%% = should be inlined

curr_char(Bin, PrevPos) ->
    case Bin of
	<<_:PrevPos/binary, C, _/binary>> ->
	    {found, C};
	_ ->
	    not_found
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Action table ADT
%%
%% We map (AccStateID -> Action)

action_table(Regexp_rules, DFA, AcceptLimit, Enc, NL, unboxed) ->
    %% the actions for the unboxed whole-input drivers, and the argument
    %% mode wrappers for the other drivers (see to_argument/1)
    Unboxed = dict:from_list(
		[ {ID, unboxed_action(the_action(Action))}
		  || {regexp, ID, _Regexp_str, Action, _Prio} <- Regexp_rules ]),
    {unboxed,
     state_actions(AcceptLimit, DFA, Unboxed),
     state_actions(AcceptLimit, DFA, regexp_actions(Regexp_rules, Enc, NL, argument))};
action_table(Regexp_rules, DFA, AcceptLimit, Enc, NL, PosMode) ->
    Acts0 = regexp_actions(Regexp_rules, Enc, NL, PosMode),
    state_actions(AcceptLimit, DFA, Acts0).

regexp_actions(Regexp_rules, Enc, NL, process) ->
    dict:from_list(
      [ {ID, position_action(Enc, NL, process_action(the_action(Action)))} 
	|| {regexp, ID, _Regexp_str, Action, _Prio} <- Regexp_rules ]
     );
regexp_actions(Regexp_rules, Enc, NL, argument) ->
    dict:from_list(
      [ {ID, argument_action(Enc, NL, the_action(Action))}
	|| {regexp, ID, _Regexp_str, Action, _Prio} <- Regexp_rules ]
     ).

%% An action may take the token alone, Action(RevChars), or the token and
%% where it starts, Action(RevChars, {Line, Column}).
%%
%% In process mode, the position is in the process dictionary: a
%% 2-argument action gets it from there.

process_action(Action) when is_function(Action, 3) ->
    fun(RevChars) -> {Line, Col} = current_position(), Action(RevChars, Line, Col) end;
process_action(Action) when is_function(Action, 2) ->
    fun(RevChars) -> Action(RevChars, current_position()) end;
process_action(Action) ->
    Action.

%% In argument mode, the drivers pass the position {Line, Col, AfterCR}
%% to the wrapper, which returns {Result, NewPos}. A 2-argument action
%% gets {Line, Col} directly; for a 1-argument action (e.g. the helpers
%% item/1 etc., which call current_line/0) the position is put in the
%% process dictionary for the call, and read back after it, so that
%% inc_lines/1 still works.

argument_action(Enc, NL, Action) when is_function(Action, 3) ->
    argument_action(Enc, NL, fun(RevChars, {Line, Col}) -> Action(RevChars, Line, Col) end);
argument_action(Enc, NL, Action) when is_function(Action, 2) ->
    fun(RevAcc, Pos = {Line, Col, _AfterCR}) ->
	    {RevChars, Chars} = token_chars(Enc, RevAcc),
	    Result = Action(RevChars, {Line, Col}),
	    {Result, advance(Pos, Chars, NL)}
    end;
argument_action(Enc, NL, Action) ->
    fun(RevAcc, Pos) ->
	    {RevChars, Chars} = token_chars(Enc, RevAcc),
	    put(?pos_key, Pos),
	    Result = Action(RevChars),
	    {Result, advance(position(), Chars, NL)}
    end.

%% In unboxed mode, the whole-input drivers call a 3-argument action
%% Action(RevChars, Line, Col) directly; a 2-argument action is adapted;
%% a 1-argument action is marked {dict, Action}: the driver puts the
%% position in the process dictionary for it (and reads it back).

unboxed_action(Action) when is_function(Action, 3) ->
    Action;
unboxed_action(Action) when is_function(Action, 2) ->
    fun(RevChars, Line, Col) -> Action(RevChars, {Line, Col}) end;
unboxed_action(Action) ->
    {dict, Action}.

%% The token as {RevChars, Chars}: characters (bytes in bytes mode)
%% reversed and in order

token_chars(Enc, RevAcc) when Enc == bytes; Enc == latin1 ->
    {RevAcc, lists:reverse(RevAcc)};
token_chars(Enc, RevBytes) ->
    {Chars, []} = lex_unicode:decode_prefix(Enc, lists:reverse(RevBytes)),
    {lists:reverse(Chars), Chars}.

%% Every match, token or not, goes through exactly one action, so the
%% action is wrapped to keep the position up to date: the action runs
%% with the position at the start of the token (current_line/0 etc.),
%% then the position is advanced over the token text.
%%
%% In the multibyte encodings, the driver accumulates bytes but the
%% action gets the token as a reversed list of code points, just like
%% a reversed string in bytes/latin1 mode. A token is always
%% well-formed (the DFA only accepts complete, valid sequences).

%%
%% Both drivers pass the token as a reversed list of bytes.

position_action(Enc, NL, Action) when Enc == bytes; Enc == latin1 ->
    fun(RevChars) ->
	    Result = Action(RevChars),
	    put(?pos_key, advance(position(), lists:reverse(RevChars), NL)),
	    Result
    end;
position_action(Enc, NL, Action) ->
    fun(RevBytes) ->
	    {Chars, []} = lex_unicode:decode_prefix(Enc, lists:reverse(RevBytes)),
	    Result = Action(lists:reverse(Chars)),
	    put(?pos_key, advance(position(), Chars, NL)),
	    Result
    end.

%% for accepting states (1..AcceptLimit), get the corresponding Action
%%
%% returns a tuple -- index into this tuple using St to get the Action

state_actions(AcceptLimit, DFA, Actions) ->
    list_to_tuple(
      [ begin
	    {state, {accepting, R, _Prio}, _Succ} = state(St, DFA),
	    rule_action(Actions, R)
	end || St <- lists:seq(1, AcceptLimit) ]
     ).

%%

the_action(Fun) when is_function(Fun) ->
    Fun;
the_action(A) ->
    fun(Acc) ->
	    io:format("rule ~w: ~ts~n", [A, lists:reverse(Acc)]),
	    no_token
    end.

%%

rule_action(Actions, RuleID) ->
    case dict:find(RuleID, Actions) of
	{ok, Act} ->
	    Act;
	_ ->
	    exit({action_not_found, RuleID})
    end.

%% This is done while lexing. Note that Actions is a tuple of closures,
%%  { Action(Acc) -> {token, T} | no_token }

lookup_action(Actions, St) ->
    element(St, Actions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Line and column counting
%%
%% The drivers keep the current position, {Line, Column}, both counted
%% from 1. Each match (whether it gives a token or not) advances the
%% position over the matched characters: a line terminator moves to
%% column 1 of the next line, anything else to the next column. Columns
%% count characters: code points in the Unicode encodings and latin1,
%% bytes in bytes mode (so a tab is one column).
%%
%% While an action runs, the position is that of the start of its
%% token, so actions can call current_line/0, current_column/0 and
%% current_position/0. Actions do not have to count lines themselves.
%%
%% Line terminators, by the newline option of regexps_to_table/2:
%%   lf       LF
%%   ascii    LF, VT, FF, CR, and CR LF (one line break)
%%   unicode  as ascii, plus NEL (U+0085), LINE SEPARATOR (U+2028)
%%            and PARAGRAPH SEPARATOR (U+2029); UTS #18 RL1.6
%% A CR LF split over two matches is still one line break.
%%
%% The position is kept in the process dictionary. with/2,3 and
%% o_longest/2 reset it; call no_lines() before lexing new input with
%% one/2.

%% {Line, Column, AfterCR}; AfterCR is true if the last character was a
%% CR that ended a line, so that a following LF does not start another.

position() ->
    case get(?pos_key) of
	undefined -> {1, 1, false};
	Pos -> Pos
    end.

no_lines() ->
    put(?pos_key, {1, 1, false}).

%% Add N to the line count, e.g. for a directive that changes the line
%% number. Ordinary line terminators are already counted by the driver,
%% so actions should not call this for them.

inc_lines() ->
    inc_lines(1).

inc_lines(N) when N >= 0 ->
    {Line, Col, CR} = position(),
    put(?pos_key, {Line+N, Col, CR}).

current_line() ->
    element(1, position()).

current_column() ->
    element(2, position()).

current_position() ->
    {Line, Col, _CR} = position(),
    {Line, Col}.

%% advance({Line, Col, AfterCR}, Chars, NL) -> {Line, Col, AfterCR}
%%
%% This runs over every character of the input, so adv/5 keeps the
%% position in arguments rather than a tuple, and the first clause
%% handles the common case: no line terminator is above CR and below NEL.

advance({Line, Col, AfterCR}, Chars, NL) ->
    adv(Chars, Line, Col, AfterCR, NL).

adv([C | Cs], Line, Col, _AfterCR, NL) when C > $\r, C < 16#85 ->
    adv(Cs, Line, Col+1, false, NL);
adv([$\n | Cs], Line, Col, true, NL) ->
    %% the LF of a CR LF
    adv(Cs, Line, Col, false, NL);
adv([C | Cs], Line, Col, _AfterCR, NL) ->
    case is_line_terminator(C, NL) of
	true -> adv(Cs, Line+1, 1, C == $\r, NL);
	false -> adv(Cs, Line, Col+1, false, NL)
    end;
adv([], Line, Col, AfterCR, _NL) ->
    {Line, Col, AfterCR}.

is_line_terminator($\n, _NL) -> true;
is_line_terminator(_C, lf) -> false;
is_line_terminator(C, _NL) when C >= $\v, C =< $\r -> true;   % VT FF CR
is_line_terminator(_C, ascii) -> false;
is_line_terminator(16#85, unicode) -> true;
is_line_terminator(16#2028, unicode) -> true;
is_line_terminator(16#2029, unicode) -> true;
is_line_terminator(_C, unicode) -> false.

%% count_lines(Chars) -> number of line breaks in Chars, by the ascii
%% rules (LF, VT, FF, CR, CR LF), which work both for bytes and for
%% code points. count_lines(Chars, NL) with NL = lf | ascii | unicode.

count_lines(Chars) ->
    count_lines(Chars, ascii).

count_lines(Chars, NL) ->
    {Lines, _Col, _CR} = advance({0, 1, false}, Chars, NL),
    Lines.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Finite automaton ADT

%%

add_state(Succ, FA) ->
    add_fa_state({state, non_accepting, Succ}, FA).

%%

add_accepting_state(AccRule, Prio, Succ, FA) ->
    add_fa_state({state, {accepting, AccRule, Prio}, Succ}, FA).

%%

add_epsilon(From, To, FA) ->
    Succ = successors(From, FA),
    update_fa_successors(From, Succ ++ [{epsilon, To}], FA).

%%

set_state(ID, State, FA) ->
    set_fa_state(ID, State, FA).

%%

state(ID, FA) ->
    fa_state(ID, FA).

%%

successors(ID, FA) ->
    {state, _, Succs} = state(ID, FA),
    Succs.

%%

accepting_state(ID, FA) ->
    {state, Acc, _Succs} = state(ID, FA),
    case Acc of
	non_accepting ->
	    false;
	{accepting, _Rule, _Prio} ->
	    true
    end.

%%

set_start_state(ID, FA) ->
    set_fa_start_state(ID, FA).

%%

start_state(FA) ->
    fa_start_state(FA).

%%

states_of(FA) ->
    fa_states_of(FA).

%%

% num_states(FA) ->
%    fa_num_states(FA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% next=1 since least element in tuple is 1
%% use dict to represent dictionaries (or gb_tree?)

-record(fa, 
	{next=1, 
	 start_state, 
	 states=dict:new()
	}
       ).

%%

empty_fa() ->
    #fa{}.

%%

add_fa_state(State, FA) ->
    ID = FA#fa.next,
    {ID, FA#fa{next=ID+1, states=dict:store(ID, State, FA#fa.states)}}.

%% NOTE: the resulting FA does _not_ have a correct "next" field.

set_fa_state(ID, State, FA) ->
    FA#fa{states=dict:store(ID, State, FA#fa.states)}.

%% Update successors of state to new list

update_fa_successors(ID, NewSuccs, FA) ->
    {state, Acc, _OldSuccs} = fa_state(ID, FA),
    NewState = {state, Acc, NewSuccs},
    FA#fa{states=dict:store(ID, NewState, FA#fa.states)}.

%% Enumerate the states of the FA
%%
%% (the convoluted code needed because lists:seq/2 is buggy)

fa_states_of(FA) ->
    N = FA#fa.next-1,
    if
	N < 1 ->
	    [];
	true ->
	    lists:seq(1, N)
    end.

% fa_num_states(FA) ->
%    FA#fa.next-1.

%%

fa_state(ID, FA) ->
    case dict:find(ID, FA#fa.states) of
	{ok, State} ->
	    State;
	Err ->
	    exit({invalid_state, Err, {state, ID}, fa_to_list(FA)})
    end.

%%

set_fa_start_state(ID, FA) ->
    FA#fa{start_state=ID}.

%% NOTE: use only with care

set_next_id(ID, FA) ->    
    FA#fa{next=ID}.

%%

fa_start_state(FA) ->
    FA#fa.start_state.

%% (generate a pseudo-FA for output)

fa_to_list(FA) ->
    FA#fa{next={next, FA#fa.next},
	  start_state={start, FA#fa.start_state},
	  states=dict:to_list(FA#fa.states)}.

%% (a pseudo-FA used as input)

-ifdef(DEBUG).
fa_from_list(FA) ->
    {next, Nxt} = FA#fa.next,
    {start, Start} = FA#fa.start_state,
    FA#fa{next=Nxt, start_state=Start, states=dict:from_list(FA#fa.states)}.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ADT for handling what states have been visited already
%%
%% A map, since Unicode classes give epsilon closures of thousands of
%% states. seen_list/1 returns a sorted list, so that the same set of
%% NFA states always gets the same name (= the same DFA state).

none_seen() ->
    #{}.

seen(X, Xs) ->
    is_map_key(X, Xs).

see(X, Xs) ->
    Xs#{X => true}.

seen_list(Xs) ->
    lists:sort(maps:keys(Xs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simple set handling (might be unconscionably slow!)

singleton(X) ->
    [X].

union(Xs, Ys) ->
    lists:sort(union0(Xs, Ys)).

union0([X|Xs], Ys) ->
    case lists:member(X, Ys) of
	true ->
	    union0(Xs, Ys);
	false ->
	    [X|union0(Xs,Ys)]
    end;
union0([], Ys) ->
    Ys.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% *** UNFINISHED ***
%% - which is start state?
%% - which are accepting states?
%%   * low numbering of acc.states?

empty_pre_dfa() ->
    { 1, dict:new(), dict:new() }.

name_dfa_state(StateSet, {Next, State_names, DFA_states}) ->
    {Next, {Next+1, dict:store(StateSet, Next, State_names), DFA_states}}.

alias_dfa_state(Key, Name, {Next, State_names, DFA_states}) ->
    {Next, dict:store(Key, Name, State_names), DFA_states}.

states_present(StateSet, {_Next, State_names, _DFA_states}) ->
    case dict:find(StateSet, State_names) of
	{ok, Name} ->
	    {found, Name};
	_ ->
	    not_found
    end.

set_dfa_state(ID, State, {Next, State_names, DFA_states}) ->
    {Next, State_names, dict:store(ID, State, DFA_states)}.

-ifdef(DEBUG).
pre_dfa_to_list({Next, State_names, DFA_states}) ->
    {Next, dict:to_list(State_names), dict:to_list(DFA_states)}.

pre_dfa_state_names_list(Pre_DFA) ->
    dict:to_list(pre_dfa_state_names(Pre_DFA)).

pre_dfa_state_names({_Next, State_names, _DFA_states}) ->
    State_names.
-endif.

pre_dfa_to_dfa(Start, {Next, _State_names, DFA_states}=_Pre_DFA) ->
    #fa{next=Next,
	start_state=Start,
	states=DFA_states}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These functions emit action functions for most tokens (better than
%% writing them explicitly in the spec above)

item(Key) ->
    fun(Acc) ->
	    {token, {Key, lex:current_line(), lists:reverse(Acc)}}
    end.

%%

op(Key) ->
    fun(_Acc) ->
	    {token, {op, lex:current_line(), Key}}
    end.

%% NB: some quoted atoms might be defined not to contain newlines

quoted_atom() ->
    fun([$'|Acc]) ->
	    [$'|Atm] = lists:reverse(Acc),
	    {token, {atom, lex:current_line(), Atm}}
    end.

%% NB: Some strings may be defined not to contain newlines

string() ->
    fun([$"|Acc]) ->
	    [$"|Str] = lists:reverse(Acc),
	    {token, {string, lex:current_line(), Str}}
    end.

%% recall that accumulator is reversed when passed to the action

char() ->
    fun([C,$$]) ->
	    {token, {char, lex:current_line(), C}}
    end.

%% Use this if you want to emit token with line number, e.g., {'(', 42} 
	
token(Key) ->    
    fun(_Acc) ->
	    {token, {Key, lex:current_line()}}
    end.

%% If you only want to emit '(' or similar, use this.

raw_token(Key) ->    
    fun(_Acc) ->
	    {token, Key}
    end.

%%

enclosed_token(Char, Tag) ->
    fun(Acc) ->
	    Str = drop_required_quote(Char, Acc),
	    {token, {Tag, Str}}
    end.

%%

enclosed_token(StartChar, EndChar, Tag) ->
    fun(Acc) ->
	    Str = drop_required_quote(StartChar, EndChar, Acc),
	    {token, {Tag, Str}}
    end.

%% Char is an enclosing citation mark that must start and end the string.
%% The first version assumes the common case when start and end are the same
%% character.
%%
%% If string does not start/end properly, will fail with uninformative error

drop_required_quote(Char, Acc) ->
    drop_required_quote(Char, Char, Acc).

drop_required_quote(StartChar, EndChar, [EndChar|Acc]) ->
    [StartChar|Str] = lists:reverse(Acc),
    Str.

%%

no_token() ->
    fun(_) ->
	    no_token
    end.

%%

comment() ->
    fun(_Acc) ->
	    %% io:format("comment '~s'~n", [lists:reverse(Acc)]),
	    no_token
    end.

%%

whitespace() ->
    fun(_Acc) ->
	    %% io:format("whitespace '~s'~n", [lists:reverse(_Acc)]),
	    no_token
    end.

%% Lines are counted by the driver, so this is the same as
%% whitespace(); kept for existing specifications.

newline() ->
    fun(_Acc) ->
	    no_token
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% If you want to safely match a literal string, this function makes sure
%% any reserved chars are escaped properly.

safe_literal(LitChars) ->
    lists:flatten(
      [ quote_char(LitChar) || LitChar <- LitChars ]
     ).

quote_char($+) -> "\\+";
quote_char($*) -> "\\*";
quote_char($?) -> "\\?";
quote_char($^) -> "\\^";
quote_char($$) -> "\\$";
quote_char($.) -> "\\.";
quote_char($\\) -> "\\\\";
quote_char($() -> "\\(";
quote_char($)) -> "\\)";
quote_char($[) -> "\\[";
quote_char($]) -> "\\]";
quote_char($|) -> "\\|";
quote_char(C) -> C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This can be run as a pre-pass over the input, either flat or nested.
%% - more expensive than integrated with lexer, but also a lot easier
%%   than writing a useful lex spec

strip_c_comments(Str) ->
    strip_c_comments(Str, []).

%% Note: encountering '*/' before any starting '/*' is treated as any
%% string.
%%
%% Note: Escaping is NOT handled, see the first clause for how to do it
%% though. We skip this because it's a corner case that may not be relevant.
%%
%% Note: Unclosed comments at end of string are acceptable.

-define(start_level, 0).

%strip_c_comments([$\\, C |Cs], Acc) ->
%    strip_c_comments(Cs, [C, $\\ | Acc]);
strip_c_comments("/*" ++ Str, Acc) ->
    Level = ?start_level,
    NewStr = drop_comments(Str, Level),
    strip_c_comments(NewStr, Acc);
strip_c_comments([C|Cs], Acc) ->
    strip_c_comments(Cs, [C|Acc]);
strip_c_comments([], Acc) ->
    lists:reverse(Acc).

drop_comments("/*" ++ Str, Lvl) ->
    drop_comments(Str, Lvl+1);
drop_comments("*/" ++ Str, Lvl) ->
    if
	Lvl > ?start_level ->
	    %% some nested /* */
	    drop_comments(Str, Lvl-1);
	true ->
	    %% outermost /* */
	    Str
    end;
drop_comments([_|Cs], Lvl) ->
    drop_comments(Cs, Lvl);
drop_comments([], _Lvl) ->
    %% unclosed comments at end are accepted
    [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Library function
%%
%% Count number of lines and replace occurrences of "?LINE" with the
%% current line number as a string.
%%
%% This operates on the raw string representation, so run it before
%% 
%%
%% UNFINISHED
%% - should parametrize wrt macro name
%% - assumes \\n is escaped newline = doesn't count
%%   (no good treatment of multiline strings at this point)

resolve_line(Str) ->
    StartLine = 1,
    resolve_line(Str, StartLine, []).

resolve_line("?LINE" ++ Str, CurrLine, Xs) ->
    NewXs = lists:reverse(integer_to_list(CurrLine)) ++ Xs,
    resolve_line(Str, CurrLine, NewXs);
resolve_line([$\\,C|Str], CurrLine, Xs) ->
    resolve_line(Str, CurrLine, [C,$\\|Xs]);
resolve_line([$\n|Str], CurrLine, Xs) ->
    NxtLine = CurrLine+1,
    resolve_line(Str, NxtLine, [$\n|Xs]);
resolve_line([C|Cs], CurrLine, Xs) ->
    resolve_line(Cs, CurrLine, [C|Xs]);
resolve_line([], _CurrLine, Xs) ->
    lists:reverse(Xs).

