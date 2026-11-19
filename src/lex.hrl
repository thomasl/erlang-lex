%%% The compiled lexer, as returned by lex:regexps_to_table/1,2.
%%%
%%% Treat it as opaque outside lex.erl; the tests and benchmarks look
%%% inside. The field order is that of the tuple {lex, ...} used before
%%% the record, so the term is unchanged.

-record(lex,
        {start,           % the start state of the DFA
         accept_limit,    % states 1..accept_limit are accepting
         table,           % the DFA: a tuple of rows, one per state, each
                          % a tuple of 256 next states (-1: no match); or,
                          % with {table, binary}, {bin8 | bin16 | bin32, Binary}
         actions,         % a tuple of actions, one per accepting state; in
                          % {position, unboxed} mode {unboxed, Actions, Wrappers}
         encoding,        % bytes | latin1 | utf8 | {utf16 | utf32, big | little}
         newline,         % unicode | ascii | lf: the line terminators
         position         % process | argument | unboxed: how drivers keep it
        }).
