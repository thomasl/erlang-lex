%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%% thomasl note:
%% - salvaged the original regexp parser from regexp.erl (obsoleted by R15)
%%
%% *** UNFINISHED ***
%% - extend it to handle UTF-8 literals

-module(regexp_parse).
-export([string/1, format_error/1]).

-import(string, [substr/2,substr/3]).
-import(lists, [reverse/1]).

-type errordesc() :: term().
-opaque regexp() :: term().

%% parse(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

-spec string(RegExp) -> ParseRes when
      RegExp :: string(),
      ParseRes :: {ok, RE} | {error, Error},
      RE :: regexp(),
      Error :: errordesc().

string(S) ->
    case catch reg(S) of
	{R,[]} -> {ok,R};
	{_R,[C|_]} -> {error,{illegal,[C]}};
	{error,E} -> {error,E}
    end.

%% format_error(Error) -> String.

-spec format_error(ErrorDescriptor) -> Chars when
      ErrorDescriptor :: errordesc(),
      Chars :: io_lib:chars().

format_error({illegal,What}) -> ["illegal character `",What,"'"];
format_error({unterminated,What}) -> ["unterminated `",What,"'"];
format_error({char_class,What}) ->
    ["illegal character class ",io_lib:write_string(What)].

%% This is the regular expression grammar used. It is equivalent to the
%% one used in AWK, except that we allow ^ $ to be used anywhere and fail
%% in the matching.
%%
%% reg -> reg1 : '$1'.
%% reg1 -> reg1 "|" reg2 : {'or','$1','$2'}.
%% reg1 -> reg2 : '$1'.
%% reg2 -> reg2 reg3 : {concat,'$1','$2'}.
%% reg2 -> reg3 : '$1'.
%% reg3 -> reg3 "*" : {kclosure,'$1'}.
%% reg3 -> reg3 "+" : {pclosure,'$1'}.
%% reg3 -> reg3 "?" : {optional,'$1'}.
%% reg3 -> reg4 : '$1'.
%% reg4 -> "(" reg ")" : '$2'.
%% reg4 -> "\\" char : '$2'.
%% reg4 -> "^" : bos.
%% reg4 -> "$" : eos.
%% reg4 -> "." : char.
%% reg4 -> "[" class "]" : {char_class,char_class('$2')}
%% reg4 -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% reg4 -> "\"" chars "\"" : char_string('$2')
%% reg4 -> char : '$1'.
%% reg4 -> empty : epsilon.
%%  The grammar of the current regular expressions. The actual parser
%%  is a recursive descent implementation of the grammar.

reg(S) -> reg1(S).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2
%% reg1' -> empty

reg1(S0) ->
    {L,S1} = reg2(S0),
    reg1p(S1, L).

reg1p([$||S0], L) ->
    {R,S1} = reg2(S0),
    reg1p(S1, {'or',L,R});
reg1p(S, L) -> {L,S}.

%% reg2 -> reg3 reg2'
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0) ->
    {L,S1} = reg3(S0),
    reg2p(S1, L).

reg2p([C|S0], L) when C =/= $|, C =/= $) ->
    {R,S1} = reg3([C|S0]),
    reg2p(S1, {concat,L,R});
reg2p(S, L) -> {L,S}.

%% reg3 -> reg4 reg3'
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> empty

reg3(S0) ->
    {L,S1} = reg4(S0),
    reg3p(S1, L).

reg3p([$*|S], L) -> reg3p(S, {kclosure,L});
reg3p([$+|S], L) -> reg3p(S, {pclosure,L});
reg3p([$?|S], L) -> reg3p(S, {optional,L});
reg3p(S, L) -> {L,S}.

-define(HEX(C), C >= $0 andalso C =< $9 orelse 
                C >= $A andalso C =< $F orelse 
                C >= $a andalso C =< $f).

reg4([$(|S0]) ->
    case reg(S0) of
	{R,[$)|S1]} -> {R,S1};
	{_R,_S} -> throw({error,{unterminated,"("}})
    end;
reg4([$\\,O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
reg4([$\\,$x,H1,H2|S]) when ?HEX(H1), ?HEX(H2) ->
    {erlang:list_to_integer([H1,H2], 16),S};
reg4([$\\,$x,${|S]) ->
    hex(S, []);
reg4([$\\,$x|_]) ->
    throw({error,{illegal,[$x]}});
reg4([$\\,C|S]) -> {escape_char(C),S};
reg4([$\\]) -> throw({error,{unterminated,"\\"}});
reg4([$^|S]) -> {bos,S};
reg4([$$|S]) -> {eos,S};
reg4([$.|S]) -> {{comp_class,"\n"},S};
reg4("[^" ++ S0) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{comp_class,Cc},S1};
	{_Cc,_S} -> throw({error,{unterminated,"["}})
    end;
reg4([$[|S0]) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{char_class,Cc},S1};
	{_Cc,_S1} -> throw({error,{unterminated,"["}})
    end;
%reg4([$"|S0]) ->
%    case char_string(S0) of
%	{St,[$"|S1]} -> {St,S1};
%	{St,S1} -> throw({error,{unterminated,"\""}})
%    end;
reg4([C|S]) when C =/= $*, C =/= $+, C =/= $?, C =/= $] -> {C,S};
reg4([C|_S]) -> throw({error,{illegal,[C]}});
reg4([]) -> {epsilon,[]}.

hex([C|Cs], L) when ?HEX(C) ->
    hex(Cs, [C|L]);
hex([$}|S], L) ->
    case catch erlang:list_to_integer(lists:reverse(L), 16) of
        V when V =< 16#FF ->
            {V,S};
        _ ->
            throw({error,{illegal,[$}]}})
    end;
hex(_S, _) ->
    throw({error,{unterminated,"\\x{"}}).

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPACE
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

char_class([$]|S]) -> char_class(S, [$]]);
char_class(S) -> char_class(S, []).

char($\\, [O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [$x,H1,H2|S]) when ?HEX(H1), ?HEX(H2) ->
    {erlang:list_to_integer([H1,H2], 16),S};
char($\\,[$x,${|S]) ->
    hex(S, []);
char($\\,[$x|_]) ->
    throw({error,{illegal,[$x]}});
char($\\, [C|S]) -> {escape_char(C),S};
char(C, S) -> {C,S}.

char_class([C1|S0], Cc) when C1 =/= $] ->
    case char(C1, S0) of
	{Cf,[$-,C2|S1]} when C2 =/= $] ->
	    case char(C2, S1) of
		{Cl,S2} when Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc]); 
		{Cl,_S2} -> throw({error,{char_class,[Cf,$-,Cl]}})
	    end;
	{C,S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc,S}.

