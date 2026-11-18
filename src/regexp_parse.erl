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
%% thomasl notes:
%% - salvaged the original regexp parser from regexp.erl (obsoleted by R15)
%% - conversion from unicode to UTF-8 is a bit unintuitive, and needs to
%%   be tested in some real situation (for example, what is actually submitted
%%   when we use this? Latin-1? UTF-8? Unicode?)

-module(regexp_parse).
-export([string/1, string/2, format_error/1]).

-import(string, [substr/2,substr/3]).
-import(lists, [reverse/1]).

-type errordesc() :: term().
-opaque regexp() :: term().
-type mode() :: bytes | unicode.

%% parse(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

-spec string(RegExp) -> ParseRes when
      RegExp :: string(),
      ParseRes :: {ok, RE} | {error, Error},
      RE :: regexp(),
      Error :: errordesc().

%% thomasl - convert the regexp from unicode to UTF-8 before
%%  parsing
%%
%% *** UNFINISHED ***
%% - Need to investigate how this really works:
%%   in the toploop, we input 2 chinese chars and get
%%    6> X = "æ é". 
%%    [32,38395]
%%   and so on with more chars (note that this looks different in my emacs,
%%   perhaps because it's still interpreted as Latin-1?)
%%   But ... strings should be UTF-8 according to the docs.
%%
%% string(S, unicode) is the code point parser used for the Unicode
%% encodings (see lex_unicode.erl). Differences from bytes mode:
%% - the regexp is NOT converted to UTF-8; every element is a code point
%% - \x{HHHHHH} accepts code points up to 10FFFF
%% - \p{Name}, \pX, \P{Name}, \PX are named classes (Perl syntax),
%%   both on their own and inside [...]; they parse as {prop, Neg, Name}
%% - \d \s \w (and \D \S \W) are Perl's Unicode classes: \p{Nd},
%%   \p{White_Space}, \p{Word} (in bytes mode they still mean DEL, SPACE
%%   and 'w')

string(S) -> string(S, bytes).

-spec string(RegExp, Mode) -> ParseRes when
      RegExp :: unicode:chardata(),
      Mode :: mode(),
      ParseRes :: {ok, RE} | {error, Error},
      RE :: regexp(),
      Error :: errordesc().

string(S0, Mode) ->
    S = case Mode of
            bytes -> binary_to_list(unicode:characters_to_binary(S0));
            unicode -> unicode:characters_to_list(S0)
        end,
    case catch reg(S, Mode) of
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

reg(S, M) -> reg1(S, M).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2
%% reg1' -> empty

reg1(S0, M) ->
    {L,S1} = reg2(S0, M),
    reg1p(S1, L, M).

reg1p([$||S0], L, M) ->
    {R,S1} = reg2(S0, M),
    reg1p(S1, {'or',L,R}, M);
reg1p(S, L, _M) -> {L,S}.

%% reg2 -> reg3 reg2'
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0, M) ->
    {L,S1} = reg3(S0, M),
    reg2p(S1, L, M).

reg2p([C|S0], L, M) when C =/= $|, C =/= $) ->
    {R,S1} = reg3([C|S0], M),
    reg2p(S1, {concat,L,R}, M);
reg2p(S, L, _M) -> {L,S}.

%% reg3 -> reg4 reg3'
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> empty

reg3(S0, M) ->
    {L,S1} = reg4(S0, M),
    reg3p(S1, L).

reg3p([$*|S], L) -> reg3p(S, {kclosure,L});
reg3p([$+|S], L) -> reg3p(S, {pclosure,L});
reg3p([$?|S], L) -> reg3p(S, {optional,L});
reg3p(S, L) -> {L,S}.

-define(HEX(C), C >= $0 andalso C =< $9 orelse 
                C >= $A andalso C =< $F orelse 
                C >= $a andalso C =< $f).

reg4([$(|S0], M) ->
    case reg(S0, M) of
	{R,[$)|S1]} -> {R,S1};
	{_R,_S} -> throw({error,{unterminated,"("}})
    end;
reg4([$\\,O1,O2,O3|S], _M) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
reg4([$\\,$x,H1,H2|S], _M) when ?HEX(H1), ?HEX(H2) ->
    {erlang:list_to_integer([H1,H2], 16),S};
reg4([$\\,$x,${|S], M) ->
    hex(S, [], M);
reg4([$\\,$x|_], _M) ->
    throw({error,{illegal,[$x]}});
reg4([$\\,C|S], unicode) when C == $p; C == $P ->
    prop(C, S);
reg4([$\\,C|S], unicode) when C == $d; C == $D; C == $s; C == $S;
                               C == $w; C == $W ->
    {perl_class(C),S};
reg4([$\\,C|S], _M) -> {escape_char(C),S};
reg4([$\\], _M) -> throw({error,{unterminated,"\\"}});
reg4([$^|S], _M) -> {bos,S};
reg4([$$|S], _M) -> {eos,S};
reg4([$.|S], _M) -> {{comp_class,"\n"},S};
reg4("[^" ++ S0, M) ->
    case char_class(S0, M) of
	{Cc,[$]|S1]} -> {{comp_class,Cc},S1};
	{_Cc,_S} -> throw({error,{unterminated,"["}})
    end;
reg4([$[|S0], M) ->
    case char_class(S0, M) of
	{Cc,[$]|S1]} -> {{char_class,Cc},S1};
	{_Cc,_S1} -> throw({error,{unterminated,"["}})
    end;
%reg4([$"|S0]) ->
%    case char_string(S0) of
%	{St,[$"|S1]} -> {St,S1};
%	{St,S1} -> throw({error,{unterminated,"\""}})
%    end;
reg4([C|S], _M) when C =/= $*, C =/= $+, C =/= $?, C =/= $] -> {C,S};
reg4([C|_S], _M) -> throw({error,{illegal,[C]}});
reg4([], _M) -> {epsilon,[]}.

hex([C|Cs], L, M) when ?HEX(C) ->
    hex(Cs, [C|L], M);
hex([$}|S], L, M) ->
    Max = case M of
              bytes -> 16#FF;
              unicode -> 16#10FFFF
          end,
    try erlang:list_to_integer(lists:reverse(L), 16) of
        V when V =< Max ->
            {V,S};
        _ ->
            throw({error,{illegal,[$}]}})
    catch
        error:badarg ->
            throw({error,{illegal,[$}]}})
    end;
hex(_S, _, _M) ->
    throw({error,{unterminated,"\\x{"}}).

%% \p{Name} \pX \P{Name} \PX  ->  {prop, Negated, Name}
%% (the name is resolved later, by lex_unicode:property/1)

prop(P, [${|S0]) ->
    case lists:splitwith(fun(C) -> C =/= $} end, S0) of
        {Name, [$}|S1]} when Name =/= [] -> {{prop, P == $P, Name}, S1};
        {[], [$}|_]} -> throw({error,{illegal,[P,${,$}]}});
        _ -> throw({error,{unterminated,[$\\,P,${]}})
    end;
prop(P, [C|S]) when C =/= ${ -> {{prop, P == $P, [C]}, S};
prop(P, []) -> throw({error,{unterminated,[$\\,P]}}).

perl_class($d) -> {prop, false, "Nd"};
perl_class($D) -> {prop, true, "Nd"};
perl_class($s) -> {prop, false, "White_Space"};
perl_class($S) -> {prop, true, "White_Space"};
perl_class($w) -> {prop, false, "Word"};
perl_class($W) -> {prop, true, "Word"}.

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

char_class([$]|S], M) -> char_class(S, [$]], M);
char_class(S, M) -> char_class(S, [], M).

char($\\, [O1,O2,O3|S], _M) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [$x,H1,H2|S], _M) when ?HEX(H1), ?HEX(H2) ->
    {erlang:list_to_integer([H1,H2], 16),S};
char($\\,[$x,${|S], M) ->
    hex(S, [], M);
char($\\,[$x|_], _M) ->
    throw({error,{illegal,[$x]}});
char($\\, [C|S], unicode) when C == $p; C == $P ->
    prop(C, S);
char($\\, [C|S], unicode) when C == $d; C == $D; C == $s; C == $S;
                              C == $w; C == $W ->
    {perl_class(C),S};
char($\\, [C|S], _M) -> {escape_char(C),S};
char(C, S, _M) -> {C,S}.

%% A class element is a character, a range {From,To} or (unicode mode)
%% a named class {prop, Neg, Name}. A named class cannot be a range
%% endpoint; "[\\p{L}-x]" means \p{L}, '-' or 'x' (as in Perl).

char_class([C1|S0], Cc, M) when C1 =/= $] ->
    case char(C1, S0, M) of
	{Cf,[$-,C2|S1]} when C2 =/= $], is_integer(Cf) ->
	    case char(C2, S1, M) of
		{Cl,S2} when is_integer(Cl), Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc], M);
		{Cl,_S2} -> throw({error,{char_class,[Cf,$-,Cl]}})
	    end;
	{C,S1} -> char_class(S1, [C|Cc], M)
    end;
char_class(S, Cc, _M) -> {Cc,S}.

