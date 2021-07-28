%%% File    : lex_test.erl
%%% Author  : Thomas Lindgren <tl@Thomass-MacBook-Pro.local>
%%% Description : 
%%% Created : 28 Jul 2021 by Thomas Lindgren <tl@Thomass-MacBook-Pro.local>

-module(lex_test).
-compile(export_all).

-import(lex, 
	[item/1,
	 token/1,
	 op/1,
	 comment/0,
	 newline/0,
	 whitespace/0,
	 quoted_atom/0,
	 string/0,
	 char/0
	]).
-import(lex,
	[parse_regexp/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Testing stuff beyond this

test_exp(1) ->
    "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?";
test_exp(2) ->
    "[a-z][0-9A-Za-z_]*";
test_exp(3) ->
    "[A-Z][0-9A-Za-z_]*";
test_exp(N) ->
    exit({no_such_test, N}).

test_all(N) ->
    [ {regexp, test_exp(R), {rule, R}} || R <- lists:seq(1,N) ].

test(Regexp) ->
    Regexp_rules = [ {regexp, parse_regexp(Regexp), {rule, 1}} ],
    lex:regexps_to_table(Regexp_rules).

test(1, String) ->
    Lex = lex:regexps_to_table(
	    [{regexp, "[a-z][A-Za-z0-9_]*", atom}, 
	     {regexp, "[A-Z][A-Za-z0-9_]*", var}, 
	     {regexp, "[1-9][0-9]*", int}, 
	     {regexp, "[\n\t\ ]*", fun(_) -> no_token end}]
	   ),
    lex:longest(Lex, String).

%% this is a reasonable lexing grammar, somewhat sparse with  reserved words
%%
%% the lexer seems to handle it quite well. Note: quoted atoms are sort of
%% handled, but I haven't really thought through what policy should be used for
%% quoted characters
%%
%% (currently: 112 NFA states, 36 DFA states with 28 accepting states)

test_lexer() ->
    [{regexp, ",", comma},
     {regexp, "\\(", lpar},     
     {regexp, "\\)", rpar},     
     {regexp, "\\[", lbrack},
     {regexp, "\\]", lbrack},
     {regexp, "&&", op_andand},
     {regexp, "&", op_and},
     {regexp, "\\|\\|", op_oror},
     {regexp, "\\|", op_or},
     {regexp, "\\+", plus},
     {regexp, "\\+\\+", plusplus},
     {regexp, "-", minus},
     {regexp, "--", minusminus},
     {regexp, "\\*", times},
     {regexp, "/", divide},
     {regexp, "[A-Z][A-Za-z0-9_]*", var},
     {regexp, "[a-z][A-Za-z0-9_]*", atom}, 
     {regexp, "'([^\n']|\\')+'", atom},     %% quoted atom
     {regexp, "(-|\\+)?[1-9][0-9]*", int},
     {regexp, "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?", float},
     {regexp, "[\ \n\t]*", fun(_) -> no_token end}
    ].

%% this one inspired by the terminals of erl_parse.yrl
%%
%% - conflicts between atoms and reserved words (currently, reserved
%%   words by some quirk win; we should be able to handle that explicitly)
%% - NFA has 296 states, becomes a DFA with 142 states (129 accepting)
%%
%% Size is 142*255 + 129 words (+ closures, and some extra stuff)
%% or 36339 words (<146 KB) for this lexer table. Not too awful.

erlang_lex() ->
    [{regexp, "\\$.", char},
     {regexp, "[A-Z][A-Za-z0-9_]*", var},
     {regexp, "[a-z][A-Za-z0-9_]*", atom}, 
     {regexp, "'([^\n']|\\')+'", atom},     %% quoted atom
     {regexp, "(-|\\+)?[1-9][0-9]*", int},
     {regexp, "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?", float},

     {regexp, "\"[^\"\n]\"", string},
     {regexp, "\\(", '(' },
     {regexp, "\\)", ')' },
     {regexp, "\\[", '[' },
     {regexp, "\\]", ']' },
     {regexp, "\\{", '{' },
     {regexp, "\\}", '}' },
     {regexp, ",", ',' },
     {regexp, "->", '->' },
     {regexp, ":-", ':-' },
     {regexp, "\\|", '|' },
     {regexp, "\\|\\|", '||' },
     {regexp, ";", ';' },
     {regexp, ":", ':' },
     {regexp, "#", '#' },
     {regexp, "\\.", '.' },

     {regexp, "after", 'after' },
     {regexp, "begin", 'begin' },
     {regexp, "case", 'case' },
     {regexp, "try", 'try' },
     {regexp, "catch", 'catch' },
     {regexp, "end", 'end' },
     {regexp, "fun", 'fun' },
     {regexp, "if", 'if' },
     {regexp, "of", 'of' },
     {regexp, "receive", 'receive' },
     {regexp, "when", 'when' },
     {regexp, "andalso", 'andalso' },
     {regexp, "orelse", 'orelse' },
     {regexp, "query", 'query' },
     {regexp, "bnot", 'bnot' },
     {regexp, "not", 'not' },

     {regexp, "\\*", '*' },
     {regexp, "/", '/' },
     {regexp, "div", 'div' },
     {regexp, "rem", 'rem' },
     {regexp, "band", 'band' },
     {regexp, "and", 'and' },

     {regexp, "\\+", '+' },
     {regexp, "-", '-' },
     {regexp, "bor", 'bor' },
     {regexp, "bxor", 'bxor' },
     {regexp, "bsl", 'bsl' },
     {regexp, "bsr", 'bsr' },
     {regexp, "or", 'or' },
     {regexp, "xor", 'xor' },

     {regexp, "\\+\\+", '++' },
     {regexp, "--", '--' },

     {regexp, "==", '==' },
     {regexp, "/=", '/=' },
     {regexp, "=<", '=<' },
     {regexp, "<", '<' },
     {regexp, ">=", '>=' },
     {regexp, ">", '>' },
     {regexp, "=:=", '=:=' },
     {regexp, "=/=", '=/=' },

     {regexp, "<<", '<<' },
     {regexp, ">>", '>>' },

     {regexp, "!", '!' },
     {regexp, "=", '=' },

     {regexp, "%[^\n]*", fun(_) -> no_token end},
     {regexp, "[\ \n\t]*", fun(_) -> no_token end}
     ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% A more realistic erlang lexer, which returns "actual tokens". Some
%% tokens are just returned as atoms, while others are wrapped into
%% 'items'/tuples.
%%
%% Note: not quite compatible with the standard erlang lexer:
%% - emits a lot of non-operators as operators (easily fixed)
%% - I couldn't find a good way for quoted atoms to contain newlines
%%   (how to specify? "'[^']*'" ?)
%% - strings currently cannot contain " or \n, which is a weakness
%%   (also, should we convert control chars into character codes?)
%%   (how to respecify?)
%% - quoted characters inside quoted atoms and strings not handled
%%   e.g., \', \"
%%
%% All of these should be fixable with more involved regexps.

real_erlang_lex() ->
    [{regexp, "\\$.", char()},
     {regexp, 0, "[A-Z_][A-Za-z0-9_]*", item(var)},
     {regexp, 0, "[a-z][A-Za-z0-9_]*", item(atom)},
     {regexp, 0, "\\?[A-Za-z][A-Za-z0-9_]*", item(macro)},
     {regexp, 0, "'(\\\'|[^'\n])*'", quoted_atom()},
     {regexp, 0, "(-|\\+)?[0-9][0-9]*", item(integer)},
     {regexp, 0, 
      "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?",item(float)},

     {regexp, 0, "\"(\\\"|[^\"\n])*\"", string()},
     %% {regexp, 0, "\"[^\"\n]*\"", item(string)},
     {regexp, "\\(", token('(') },
     {regexp, "\\)", token(')') },
     {regexp, "\\[", token('[') },
     {regexp, "\\]", token(']') },
     {regexp, "\\{", token('{') },
     {regexp, "\\}", token('}') },
     {regexp, ",", token(',') },
     {regexp, "->", token('->') },
     {regexp, ":-", token(':-') },
     {regexp, "\\|", token('|') },
     {regexp, "\\|\\|", token('||') },
     {regexp, ";", token(';') },
     {regexp, ":", token(':') },
     {regexp, "#", token('#') },
     {regexp, "\\.", token('.') },

     {regexp, 1, "after", token('after') },
     {regexp, 1, "begin", token('begin') },
     {regexp, 1, "case", token('case') },
     {regexp, 1, "try", token('try') },
     {regexp, 1, "catch", token('catch') },
     {regexp, 1, "end", token('end') },
     {regexp, 1, "fun", token('fun') },
     {regexp, 1, "if", token('if') },    % rule 30
     {regexp, 1, "of", token('of') },
     {regexp, 1, "receive", token('receive') },
     {regexp, 1, "when", token('when') },
     {regexp, 1, "andalso", token('andalso') },
     {regexp, 1, "orelse", token('orelse') },
     {regexp, 1, "query", token('query') },
     {regexp, 1, "bnot", token('bnot') },
     {regexp, 1, "not", token('not') },

     {regexp, 1, "\\*", op('*') },
     {regexp, 1, "/", op('/') },
     {regexp, 1, "div", op('div') },
     {regexp, 1, "rem", op('rem') },
     {regexp, 1, "band", op('band') },
     {regexp, 1, "and", op('and') },

     {regexp, 1, "\\+", op('+') },
     {regexp, 1, "-", op('-') },
     {regexp, 1, "bor", op('bor') },
     {regexp, 1, "bxor", op('bxor') },
     {regexp, 1, "bsl", op('bsl') },
     {regexp, 1, "bsr", op('bsr') }, % rule 50
     {regexp, 1, "or", op('or') },
     {regexp, 1, "xor", op('xor') },

     {regexp, 1, "\\+\\+", op('++') },
     {regexp, 1, "--", op('--') },

     {regexp, 1, "==", op('==') },
     {regexp, 1, "/=", op('/=') },
     {regexp, 1, "=<", op('=<') },
     {regexp, 1, "<", op('<') },
     {regexp, 1, ">=", op('>=') },
     {regexp, 1, ">", op('>') },
     {regexp, 1, "=:=", op('=:=') },
     {regexp, 1, "=/=", op('=/=') },

     {regexp, 1, "<<", op('<<') },
     {regexp, 1, ">>", op('>>') },

     {regexp, 1, "!", op('!') },
     {regexp, 1, "=", op('=') },

     {regexp, 1, "%[^\n]*", comment()},
     {regexp, 2, "\n", newline()},
     {regexp, 1, "[\ \t]*", whitespace()}
     ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here is a rule for floats that also handles various IEEE special numbers
%%
%% UNFINISHED
%% - should be moved to library

float_rules() ->
    [{regexp, 0, 
      "((\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)?|NAN|NaN|(\\+|-)?(INF|Inf|inf))",item(float)}
    ].

