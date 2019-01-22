
# Erlang-Lex documentation

(This has been collected from the `src/lex.erl` file.)

Author: Thomas Lindgren (040422-040425; 041125; 051007-051016)

Lexical analysis of strings: convert a sequence into a list of tokens
according to regexp rules.

# USAGE

````erlang
  lex:with([Regexp_rule], Sequence) -> [Token]
    %% Regexp_rule is same as for regexps_to_table/2
  lex:file_with([Regexp_rule], File) -> [Token]
    %% As above but lexes a whole file.
````

A sequence is a list of characters, a binary, or a pair of binary
and position. Positions are an abstract datatype, see `start_pos/0`
and its friends below. (You normally SHOULD NOT need to generate positions
yourself.)

## EXAMPLES

````erlang
    lex:with(lex:real_erlang_lex(), "foo() -> zap().")
    lex:file_with(lex:real_erlang_lex(), "file.erl")
````

## EXAMPLE REGEXP SPECIFICATIONS

There are two functions that return realistic example specifications
for lexing. Both are found in `src/lex.erl`.

- `erlang_lex()`
- `real_erlang_lex()`

## PERFORMANCE

Performance was measured most recently in 2004-2005. 

A lex table for `erlang_lex()` becomes 142 DFA states and is generated
(from regexps to ready to use) in 54.8 msec on an Athlon 1300+. 

Likewise, `real_erlang_lex()` was generated in 66-68
milliseconds on a 1.6 GHz laptop.

This is approximately as complex as a programming language lexer gets.

## MORE API:

````erlang
  regexps_to_table([{regexp, [Prio,] Regexp_string, Action [,Prio]}]) -> 
                   Lex_table
    %% where Action(AccToken) -> {token, Token} | no_token
    %%      Regexp_string is a regexp according to the regexp module
    %%      Prio is an integer rule priority (default: 0)
    %%       that chooses the rule to accept if there are several possible
  longest(Lex_table, String) -> [Token]
    %% Use a generated lex table to convert a string into a list of tokens.
   regexps_to_nfa([{regexp, Regexp_string, Action}]) -> NFA
    %% where Action(AccToken) -> {token, Token} | no_token
  nfa_to_dfa(NFA) -> DFA
  dfa_to_table(DFA) -> Lex_table
````

See also below for help in specifying your lexers.

1. Specify (regexp -> action) rules
2. Translate regexp -> NFA
3. Translate NFA -> DFA (+ check for uniqueness)
4. Generate table representing DFA

This code is a simple subset of lex/flex. When there are two matching tokens,
the longest is always chosen. (I.e., if you are in an accepting state and
more characters arrive, you continue.)

We also provide a library for keeping track of the number of lines
seen so far.

````erlang
  no_lines()                      %% reset line count to zero
  inc_lines(), inc_lines(N)       %% increment line count by 1 or N
  current_line() -> N             %% returns current line number
  count_lines(Str) -> N           %% count newlines in string
````

The available lex driver sets `no_lines()` before lexing.

Note that the line count is global per-process at this
time.

Using the line library in your lexer is done like this.
See also `erlang_lex()` and `real_erlang_lex()`.

````erlang
   {regexp, "\n", fun(_) -> lex:inc_lines(), no_token end}
   {regexp, "...", fun(Acc) -> lex:count_lines(Acc), no_token end}
   {regexp, "<", fun(_) -> {token, {op, lex:current_line(), '<'}} end}
````

## YECC COMPATIBILITY

Yecc expects the tokenizer to return tuples on the form

````erlang
   {Category, LineNumber, Symbol}
   {Category, LineNumber}
````

if there is just one member of the `Category`. For example, all operators
are the same while variables normally are different, so we would return
something like

````erlang
   {var, 42, "VarName"}
````

or

````erlang
   {'+', 17}
````

Furthermore, the list of tokens should end with

````erlang
  {End_symbol, LineNumber}
````

(where `End_symbol` is declared in the yecc file)

It is up to you to return tokens suitable for yecc, but as you can see
it's not too hard. Line numbers can be added using the library above.
See also the example lexer specifications in `src/lex.erl`.

## STATUS

No known bugs per se, but some questionable design decisions.

The error returned when there is no match is incomprehensible. This is
probably the main usage issue.

Some missing functionality for more modern languages. In particular,
UTF-8 and suchlike is not handled. The current way of lexing is not
suitable for this -- we currently emit the equivalent of arrays for
representing DFA transition tables, which is impractical for huge
character sets. (The code also assumes byte values rather than wider
characters, so this would need substantial cleanup.)

See `src/lex.erl` for some further notes on what could be done better.

To understand the code, you should know about (deterministic
and nondeterministic) finite automata on the level of the Dragon Book.
It's basically the same algorithms.

## LEXING CORNER CASES

 The driver tries to match a maximally long token (aka "maximal munch")
 which means there can be _backtracking_ if a long match ultimately fails.
 In that case, the driver emits the longest known match and continues from
 the saved point. This means characters can be traversed several times.
 Consider the regexp `(a|a*b)`, which matches a single 'a' or any number of
 'a' followed by a 'b'. 

 For the string "aaaaa", the driver will traverse it all to try to
 match a*b, then see there is no match and emit 'a'. Then the same
 thing is done with the remaining "aaaa", "aaa", "aa" and "a". So
 lexing can be slow in this respect (quadratic at worst, I think, as
 shown, since you need to match at least one character every time and
 each match takes linear time).

 I'm not sure what is done in flex etc. for the corresponding situation.
 Note that this sort of overlap is not so common in real use (you need
 nasty regexps and nasty input), but be aware that it _may_ happen.

##  RANDOM NOTES

- An application: composing collections of regexps is simple, you
  just set their priorities and concatenate => you can easily extend
  your tokenizer dynamically! (just regenerate the lexer)



