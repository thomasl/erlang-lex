%%% File    : regexp_parse2.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created : 10 Mar 2019 by Thomas Lindgren <>

%% New lex regexp format
%% - first of all, note that this is a LIMITED regexp format for tokenization
%%   a la flex, while perl and such have far more flexibility (at a greater cost)
%% - the existing regexp format uses erlang re, but leads to a lot of
%%   quoting problems in practice (e.g., using \\ and \\\\ and so on in strings)
%% - less free-form but easier to get right
%% 
%% NB: POSIX has defined classes [:name:] with a collection of standard names.
%%  These are sometimes a bit obscure, so we will extend them a bit.
%%
%% We focus on character classes, but also permit various composition operations,
%% which have tended to get messed up.
%%
%% Here we translate long-form regexps into the lex internal format.
%%  [:range c1 c2]                 a-z
%%  [:class]                       predefined classes
%%  [:not[:class]]                  ^c
%%  [:or[:class]...[:class:]]       (c1|...|cn)
%%  [:bos], [:eos]                beginning and end of string
%%   including classes for all meta characters like (,),[,],\,+,?,. etc
%%   to avoid too much quoting
%%
%% For example: 
%%  [a-zA-Z0-9_]*  =>
%%  [:star[:or[:lowercase][:uppercase][:digits][:lit _]]]
%% or (POSIX-style, though _ might not be part of this predefined class):
%%  [:alnum:]*
%%
%% We then ALSO define short forms for these for convenience.

%% Control characters: + ? . * ^ $ ( ) [ ] { } | \

%% UNFINISHED
%% - see lex:internal_form/1 [should be refactored into regexp_parse.erl]
%%   * do we want to emit the old regexp_parse format or go directly to the
%%     internal one? somewhat unclear, actually
%% - also permit defining classes; I think flex does that
%%   something like [:define name defexpr]
%%   and then [:name:] is subsequently available
%% - some numbers are restricted to \xFF but probably should be \xFF...FF
%%   to handle unicode

-module(regexp_parse2).
-export(
   [string/1]
  ).

%-define(debug(Str, Xs), io:format(Str, Xs)).
-define(debug(Str, Xs), ok).

%% Returns the regexp AST which then is processed by lex
%%
%% Internal form is the erlang-lex internal form for regexps.
%%
%% Internal form RE:
%%   {iter, RE}
%%   {disj, REs}
%%   {seq, REs}
%%   {range, From, To}
%%
%% We currently return the following format:
%% - an RE is a list, a sequence of sub-REs
%%
%%   {lit, C}                   exactly character C (integer)
%%   {range, From, To}          characters in range From-To
%%   {zero_or_more, RE}         RE repeated zero or more times
%%   {one_or_more, RE}          RE repeated one or more times
%%   {'or', RE_list}            one of the REs matches (at least one)
%%   {'not', RE}                RE does not match
%%   {optional, RE}             0-1
%%   {from_to, From, To, RE}    From-To repetitions of RE
%%
%% (Not sure if all of these are relevant?) Basic translation scheme
%%
%%  {lit, C} => {range, C, C}
%%  {range, From, To} => {range, From, To}
%%  {zero_or_more, RE} => {iter, RE}
%%  {one_or_more, RE} => {seq, [RE, {iter, RE}]}
%%  {'or', REs} => {disj, REs}
%%  {'not', RE} => ... ??
%%  {optional, RE} => {disj, [RE, empty]}
%%  {from_to, From, To, RE} => {seq, [RE,...,Re, {optional, RE}, ... {optional, RE}]}

string(Str) ->
    parse_re(Str).

%% A regexp re is an implicit sequence (list) of class exprs.

parse_re(Str) ->
    parse_re(Str, []).

parse_re("", RevAcc) ->
    lists:reverse(RevAcc);
parse_re(Str, RevAcc) ->
    {ClassE, RestStr} = parse_class_expr(Str),
    parse_re(RestStr, [ClassE|RevAcc]).

%% Parse a class until the final "]". Note that there may be nested elements.
%%
%% Should we also parse the optional iterator, ?*+{M,N}? no, since these do not
%% occur inside class definitions. See parse_class_expr/1 for that.
%%
%% NB: this form is NOT identical to POSIX [:name:] classes, but named classes
%% have the same syntax (and a broader set of classes).
%%
%% UNFINISHED
%% - translate named classes to direct exprs

parse_class("[:or " ++ Str0) ->
    %% parse a sequence of classes, then ]
    Str1 = drop_ws(Str0),
    {Classes, Str2} = parse_classes(Str1),
    Str2a = drop_ws(Str2),
    case Str2a of
	"]" ++ Str3 ->
	    {{'or', Classes}, Str3};
	_ ->
	    exit({expected_end_bracket_when_parsing_OR_CLASS, Str2a})
    end;
parse_class("[:not " ++ Str0) ->
    %% parse a nested class, then ]
    Str1 = drop_ws(Str0),
    {Class, Str2} = parse_class(Str1),
    Str2a = drop_ws(Str2),
    case Str2a of
	"]" ++ Str3 ->
	    {{'not', Class}, Str3};
	_ ->
	    exit({expected_end_bracket_when_parsing_NOT_CLASS, Str2a})
    end;
parse_class("[:range " ++ Str0) ->
    %% parse two characters
    Str1 = drop_ws(Str0),
    {FromCh, Str2} = get_char_spec(Str1),
    Str3 = drop_ws(Str2),
    {ToCh, Str4} = get_char_spec(Str3),
    Str4a = drop_ws(Str4),
    case Str4a of
	"]" ++ Str5 ->
	    {{range, FromCh, ToCh}, Str5};
	_ ->
	    exit({expected_end_bracket_when_parsing_CHAR_RANGE, Str4a})
    end;
parse_class("[:def " ++ Str0) ->
    %% [:def name class]
    Str1 = drop_ws(Str0),
    {ClassName, Str2} = get_class_name(Str1),
    Str2a = drop_ws(Str2),
    {Class, Str3} = parse_class(Str2a),
    case Str3 of
	"]" ++ Str4 ->
	    {{'def', ClassName, Class}, Str4};
	_ ->
	    exit({expected_end_bracket_when_parsing_DEF_CLASS, Str3})
    end;
parse_class("[:lit " ++ Str0) ->
    %% get a single literal character (or hex spec)
    %% - this may be too weak to be practical?
    Str1 = drop_ws(Str0),
    {Ch, Str2} = get_char_spec(Str1),
    Str2a = drop_ws(Str2),
    case Str2a of
	"]" ++ Str3 ->
	    {{lit, Ch}, Str3};
	_ ->
	    exit({expected_end_bracket_when_parsing_LITERAL_CLASS, Str2a})
    end;
parse_class("[:alt " ++ Str0) ->
    %% disjunction of characters
    %% note: use named classes to capture difficult chars!
    %% - like whitespace, backslash etc
    %% parse a sequence of classes, then ]
    Str1 = drop_ws(Str0),
    {Chs, Str2} = get_chars(Str1),
    Str2a = drop_ws(Str2),
    case Str2a of
	"]" ++ Str3 ->
	    %% UNFINISHED
	    %% - should be {or, [Lit1, ..., LitN]}
	    {{'alt', Chs}, Str3};
	_ ->
	    exit({expected_end_bracket_when_parsing_SEQ_CLASS, Str2a})
    end;
parse_class("[:seq " ++ Str0) ->
    %% parse a sequence of classes, then ]
    Str1 = drop_ws(Str0),
    {Classes, Str2} = parse_classes(Str1),
    Str2a = drop_ws(Str2),
    case Str2a of
	"]" ++ Str3 ->
	    {{'seq', Classes}, Str3};
	_ ->
	    exit({expected_end_bracket_when_parsing_SEQ_CLASS, Str2a})
    end;
parse_class("[:" ++ Str0) ->
    %% parse a named class [:name:]
    {ClassName, Str1} = get_class_name(Str0),
    case Str1 of
	":]" ++ Str2 ->
	    %% resolve ClassName into something better
	    Class = name_to_class(ClassName),
	    {Class, Str2};
	_ ->
	    exit({expected_end_of_named_class, Str1})
    end;
%% allow single char_spec? 
%% allow [:seq ...]
parse_class(Str0) ->
    exit({expected_start_bracket_colon_when_parsing_CLASS, Str0}).

%% Parse sequence of classes, perhaps empty

parse_classes(Lst) ->
    NxtLst = drop_ws(Lst),
    RevAcc0 = [],
    parse_classes(NxtLst, RevAcc0).

parse_classes(Lst = ("[:" ++ _), RevAcc) ->
    {Class, TmpLst} = parse_class(Lst),
    NxtLst = drop_ws(TmpLst),
    parse_classes(NxtLst, [Class|RevAcc]);
parse_classes(Lst, RevAcc) ->
    {lists:reverse(RevAcc), Lst}.

%% Parse class followed by iterator
%%
%% UNFINISHED
%% - no semantic checking that {M,N} has M =< N yet

parse_class_expr(Str0) ->
    {Class, Str1} = parse_class(Str0),
    case Str1 of
	"*" ++ Str2 ->
	    {{zero_or_more, Class}, Str2};
	"+" ++ Str2 ->
	    {{one_or_more, Class}, Str2};
	"?" ++ Str2 ->
	    {{optional, Class}, Str2};
	"{" ++ Str2 ->
	    {From, Str3} = get_int(Str2),
	    case Str3 of
		"," ++ Str4 ->
		    {To, Str4a} = get_int(Str4),
		    case Str4a of
			"}" ++ Str5 ->
			    {{from_to, From, To, Class}, Str5};
			_ ->
			    exit({expected_right_brace_while_parsing_FROM_TO, Str4a})
		    end;
		"}" ++ Str4 ->
		    {{from_to, From, From, Class}, Str4};
		_ ->
		    exit({expected_comma_or_right_brace_while_parsing_FROM_TO, Str3})
	    end;
	_ ->
	    %% no recognized modifier = match class once
	    {Class, Str1}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract various literals

%%

get_class_name(Lst) ->
    get_class_name(Lst, "").

get_class_name([C|Cs]=Lst, RevAcc) ->
    if
	C >= $A, C =< $Z ;
	C >= $a, C =< $z ;
	C >= $0, C =< $9 ;
	C == $_ ->
	    get_class_name(Cs, [C|RevAcc]);
	true ->
	    {lists:reverse(RevAcc), Lst}
    end;
get_class_name("", RevAcc) ->
    {lists:reverse(RevAcc), ""}.

%% NB: will fail if RevAcc is empty

get_int(Lst) ->
    get_int(Lst, 0).

get_int([C|Cs]=Lst, Acc) ->
    if
	C >= $0, C =< $9 ->
	    NewAcc = Acc*10 + (C-$0),
	    get_int(Cs, NewAcc);
	true ->
	    {Acc, Lst}
    end;
get_int("", Acc) ->
    {Acc, ""}.

%% Convert a list of hex digits "0da334f..." into an integer
%%
%% NB: will fail if RevAcc is empty

get_hex_int(Lst) ->
    ?debug("get_hex_int ~s\n", [Lst]),
    get_hex_int(Lst, 0).

get_hex_int([C|Cs]=Lst, Acc) ->
    ?debug("Acc is ~p, next is ~s (~p)\n", [Acc, [C], C]),
    if
	C >= $0, C =< $9 ->
	    NewAcc = (Acc bsl 4) bor (C-$0),
	    get_hex_int(Cs, NewAcc);
	C >= $A, C =< $Z ->
	    NewAcc = (Acc bsl 4) bor (10+(C-$A)),
	    get_hex_int(Cs, NewAcc);
	C >= $a, C =< $z ->
	    NewAcc = (Acc bsl 4) bor (10+(C-$a)),
	    get_hex_int(Cs, NewAcc);
	true ->
	    {Acc, Lst}
    end;
get_hex_int("", Acc) ->
    {Acc, ""}.

%% get a character spec, which is either a raw character or a hex number 0xff..ff
%%
%% NB: we furthermore restrict the range of values to bytes; unicode is for later

get_char_spec("0x" ++ Str0) ->
    get_hex(Str0);
get_char_spec([C|Cs]) ->
    %% C is taken to be a literal character
    {C, Cs}.

get_hex(Cs) ->
    get_hex(Cs, 0).

get_hex([C|Cs]=Lst, Acc) ->
    if
	C >= $0, C =< $9 ->
	    NewAcc = (Acc bsl 4) bor (C - $0),
	    get_hex(Cs, NewAcc);
	C >= $A, C =< $F ->
	    NewAcc = (Acc bsl 4) bor ((C - $A) + 10),
	    get_hex(Cs, NewAcc);
	C >= $a, C =< $f ->
	    NewAcc = (Acc bsl 4) bor ((C - $a) + 10),
	    get_hex(Cs, NewAcc);
	true ->
	    {Acc, Lst}
    end;
get_hex("", Acc) ->
    {Acc, ""}.
	 
%% Skip a sequence of whitespace

drop_ws("\s" ++ Str) ->
    drop_ws(Str);
drop_ws("\t" ++ Str) ->
    drop_ws(Str);
drop_ws("\n" ++ Str) ->
    drop_ws(Str);
drop_ws(Str) ->
    Str.

%% Take a base operation, then a suffix (?,*,+), then possible 'rest of sequence'
%%
%% The base operation is
%%  - a range [r1-r2...rm-rn]
%%  - a class [:name:]  <-- POSIX character classes
%%  - a disjunction (of N arms) SYNTAX??
%%  - complemented base operation (^:RE)
%%  - optional (?:RE)
%%  - repetition (*:RE)
%%    (could add (*,M,N:RE) for M to N repetition
%%
%% OK, is this great?? well, try it ...

%% Specify POSIX-style character classes
%%
%% UNFINISHED
%% - not sure if this is the right thing?

get_chars(Str) ->
    get_chars(Str, []).

%% Parse character classes until ] is found
%%
get_chars(Lst = ("]" ++ _), RevAcc) ->
    {lists:reverse(RevAcc), Lst};
get_chars("", RevAcc) ->
    %% NB: this probably means an error in the enclosing parse
    {lists:reverse(RevAcc), ""};
get_chars(Str0, RevAcc) ->
    {Range, Str1} = get_char_range(Str0),
    NewRevAcc = [Range|RevAcc],
    get_chars(Str1, NewRevAcc).

%% Should ensure that From and To are printable ASCII
%%
%% No, like this:
%% 1/ get \xFF...FF
%%    or just one character
%% 2/ then get -char
%%    OR 

get_char("\\x" ++ Str0) ->
    get_hex_int(Str0);
get_char("\\n" ++ Str0) ->
    [X] = "\n",
    {X, Str0};
get_char("\\t" ++ Str0) ->
    [X] = "\t",
    {X, Str0};
get_char("\\r" ++ Str0) ->
    [X] = "\r",
    {X, Str0};
get_char("\\v" ++ Str0) ->
    [X] = "\v",
    {X, Str0};
get_char("\\f" ++ Str0) ->
    [X] = "\f",
    {X, Str0};
get_char([C|Cs]) ->
    %% should check that C is printable and not a bad thing
    {C, Cs}.

get_char_range(Str0) ->
    {A, Str1} = get_char(Str0),
    case Str1 of
	"-" ++ Str2 ->
	    {B, Str3} = get_char(Str2),
	    {{range, A, B}, Str3};
	_ ->
	    {{lit, A}, Str1}
    end.

%% UNFINISHED
%% - dumb wrapper to interface with posix_cc, should get rid of posix_cc wraps
%%   instead

name_to_class(ClassName) ->
    CName = lists:flatten(["[:", ClassName, ":]"]),
    posix_cc(CName).
    
%% POSIX character classes
%%  from https://www.regular-expressions.info/posixbrackets.html
%%
%% NB: we need more classes, e.g., 'reserved characters'
%%
%% UNFINISHED
%% - dumb solution, we should of course expand these classes without the
%%   extra parsing step (every time!) ... but let's leave it for now

posix_cc("[:alnum:]" ++ _Str) ->
    %% [a-zA-Z0-9]
    pc("[:alt a-zA-Z0-9]");
posix_cc("[:alpha:]" ++ _Str) ->
    %% [a-zA-Z]
    pc("[:alt a-zA-Z]");
posix_cc("[:ascii:]" ++ _Str) ->
    %% [\x00-\x7F]
    pc("[:alt \\x00-\\x7F]");
posix_cc("[:blank:]" ++ _Str) ->
    %% [\s\t]
    pc("[:alt \\x20\\x09]");
posix_cc("[:cntrl:]" ++ _Str) ->
    %% [\x00-\x1F\x7F]
    pc("[:alt \\x00-\\x1F\\x7F]");
posix_cc("[:digit:]" ++ _Str) ->
    %% [0-9]
    pc("[:alt 0-9]");
posix_cc("[:graph:]" ++ _Str) ->
    %% [\x21-\x7E] "visible chars"
    pc("[:alt \\x21-\\x7E]");
posix_cc("[:lower:]" ++ _Str) ->
    %% [a-z]
    pc("[:alt a-z]");
posix_cc("[:print:]" ++ _Str) ->
    %% [\x20-\x7E]
    pc("[:alt \\x20-\\x7E]");
posix_cc("[:punct:]" ++ _Str) ->
    %% !"#$%&'()*+,-./:;<=>?@[]^_`{}|~
    pc("[:alt !#$%&'()*+,-./:;<=>?@^_`|~]\\x22\\x5b\\x5d\\x7b\\x7d");  %% "[]{}
posix_cc("[:space:]" ++ _Str) ->
    %% [ \t\r\n\v\f]
    pc("[:alt \t\r\n\v\f]");
posix_cc("[:upper:]" ++ _Str) ->
    %% [A-Z]
    pc("[:alt A-Z]");
posix_cc("[:word:]" ++ _Str) ->
    %% [A-Za-z0-9_]
    pc("[:alt A-Za-z0-9_]");
posix_cc("[:xdigit:]" ++ _Str) ->
    %% [A-Fa-f0-9]
    pc("[:alt A-Fa-f0-9]").

%% tmp wrapper

pc(Str) ->
    {Class, []} = parse_class(Str),
    Class.

