%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parser for the regular expressions in lex rule specifications.
%%
%% The language is described in doc/syntax.md. In short:
%%
%%   regexp  ::= ["(?i)" | "(?-i)"] alt
%%   alt     ::= seq ("|" seq)*
%%   seq     ::= postfix*                  (empty: epsilon)
%%   postfix ::= atom ("*" | "+" | "?")*
%%   atom    ::= char | escape | "." | "^" | "$" | class
%%             | "(" alt ")" | "(?i:" alt ")" | "(?-i:" alt ")"
%%   class   ::= "[" ["^"] ["]"] item* "]"
%%   item    ::= "[:" ["^"] name ":]" | single ["-" single]
%%
%% There are two modes. In `bytes` mode the regexp is first converted to
%% UTF-8 and every element is a byte; `\d` `\s` `\w` keep their old AWK
%% meanings (DEL, space, w) and `\p` is a literal p. In `unicode` mode
%% every element is a code point, and `\p{..}`, `\P{..}`, `\d` `\s` `\w`
%% (and complements) are named classes.
%%
%% The regexp itself may be a string, a binary or an iolist of both
%% (unicode:chardata()). How its characters are read is set by the regexp
%% encoding (see "Input" below); by default binaries are UTF-8, and so is
%% a string of bytes that is valid UTF-8, so "λ+", <<"λ+"/utf8>> and
%% binary_to_list(<<"λ+"/utf8>>) are the same regexp.
%%
%% The result is a parse tree (regexp/0 below). Names of properties and
%% POSIX classes are not checked here; lex_unicode resolves them.

-module(lex_spec_parse).

-export([string/1, string/2, string/3, format_error/1]).

-export_type([regexp/0, class_item/0, mode/0, regexp_encoding/0, error_reason/0]).

-type mode() :: bytes | unicode.

-type regexp_encoding() :: utf8 | unicode | latin1.

-type named_class() :: {prop, Negated :: boolean(), Name :: string()}
                     | {posix, Negated :: boolean(), Name :: string()}.

-type class_item() :: char()
                    | {First :: char(), Last :: char()}
                    | named_class().

-type regexp() :: char()
                | epsilon
                | bos
                | eos
                | named_class()
                | {char_class, [class_item()]}
                | {comp_class, [class_item()]}
                | {concat, regexp(), regexp()}
                | {'or', regexp(), regexp()}
                | {kclosure, regexp()}
                | {pclosure, regexp()}
                | {optional, regexp()}
                | {caseless, boolean(), regexp()}.

-type error_reason() :: {illegal, string()}
                      | {unterminated, string()}
                      | {char_class, list()}
                      | {illegal_flags, string()}
                      | {misplaced_flags, string()}
                      | {bad_characters, unicode:chardata()}.

-define(FAIL(Reason), throw({?MODULE, Reason})).

-define(is_octal(C), (C >= $0 andalso C =< $7)).
-define(is_hex(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

-spec string(unicode:chardata()) -> {ok, regexp()} | {error, error_reason()}.
string(Regexp) ->
    string(Regexp, bytes).

-spec string(unicode:chardata(), mode()) ->
          {ok, regexp()} | {error, error_reason()}.
string(Regexp, Mode) ->
    string(Regexp, Mode, utf8).

-spec string(unicode:chardata(), mode(), regexp_encoding()) ->
          {ok, regexp()} | {error, error_reason()}.
string(Regexp, Mode, RegexpEnc)
  when (Mode =:= bytes orelse Mode =:= unicode),
       (RegexpEnc =:= utf8 orelse RegexpEnc =:= unicode orelse RegexpEnc =:= latin1) ->
    try
        Chars = to_chars(Regexp, Mode, RegexpEnc),
        {ok, parse(Chars, Mode)}
    catch
        throw:{?MODULE, Reason} -> {error, Reason}
    end.

-spec format_error(error_reason()) -> io_lib:chars().
format_error({illegal, What}) ->
    io_lib:format("illegal character `~ts'", [What]);
format_error({unterminated, What}) ->
    io_lib:format("unterminated `~ts'", [What]);
format_error({char_class, What}) ->
    io_lib:format("illegal character class range ~tp", [What]);
format_error({illegal_flags, What}) ->
    io_lib:format("unsupported flags `(?~ts'; only i and -i are allowed", [What]);
format_error({misplaced_flags, Hint}) ->
    io_lib:format("misplaced flags: ~ts", [Hint]);
format_error({bad_characters, What}) ->
    io_lib:format("regexp is not valid character data (a binary must be UTF-8, "
                  "e.g. <<\"...\"/utf8>>): ~tp", [What]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input

%% The regexp encoding says how the regexp's characters are read:
%%
%%   utf8 (default)  binaries are UTF-8. A string (or iolist) whose
%%                   elements are all bytes is read as UTF-8 if it is
%%                   valid UTF-8, so a string of UTF-8 bytes works;
%%                   otherwise its elements are code points, as in an
%%                   Erlang string literal ("é" is [233], which is not
%%                   valid UTF-8 on its own, so it stays U+00E9). ASCII
%%                   reads the same either way.
%%   unicode         unicode:chardata(): list elements are code points,
%%                   binaries UTF-8 (no guessing).
%%   latin1          every element and every byte of a binary is one
%%                   character 0..255. In bytes mode these are the bytes
%%                   to match, not converted to UTF-8.
%%
%% Bytes mode then works on the UTF-8 bytes of the characters (except
%% with latin1), so "é" in a regexp matches the two bytes of an UTF-8 é
%% in the input.

to_chars(Regexp, Mode, RegexpEnc) ->
    CodePoints = code_points(Regexp, RegexpEnc),
    case {Mode, RegexpEnc} of
        {unicode, _} -> CodePoints;
        {bytes, latin1} -> CodePoints;
        {bytes, _} -> binary_to_list(unicode:characters_to_binary(CodePoints))
    end.

code_points(Regexp, utf8) ->
    try iolist_to_binary(Regexp) of
        Bytes ->
            case unicode:characters_to_list(Bytes, utf8) of
                List when is_list(List) -> List;
                _ -> code_points(Regexp, unicode)    % not UTF-8: code points
            end
    catch
        error:badarg -> code_points(Regexp, unicode) % has code points > 255
    end;
code_points(Regexp, Enc) ->
    case unicode:characters_to_list(Regexp, Enc) of
        List when is_list(List) -> List;
        _ -> ?FAIL({bad_characters, Regexp})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regexps
%%
%% Each function takes the remaining input and returns {Tree, Rest}.

parse(Chars, Mode) ->
    {Tree, Rest} = case leading_flags(Chars) of
                       {Caseless, Body} ->
                           {RE, R} = alt(Body, Mode),
                           {{caseless, Caseless, RE}, R};
                       none ->
                           alt(Chars, Mode)
                   end,
    case Rest of
        [] -> Tree;
        [C | _] -> ?FAIL({illegal, [C]})        % an unmatched ")"
    end.

%% "(?i)" or "(?-i)" at the very start applies to the whole regexp.
leading_flags("(?" ++ After) ->
    case flag(After) of
        {Caseless, ")" ++ Body} -> {Caseless, Body};
        {_, _} -> none                          % "(?i:" is a group
    end;
leading_flags(_) ->
    none.

%% Only the i flag exists.
flag("i" ++ Rest) -> {true, Rest};
flag("-i" ++ Rest) -> {false, Rest};
flag(Other) -> ?FAIL({illegal_flags, lists:sublist(Other, 3)}).

alt(Chars, Mode) ->
    {First, Rest} = seq(Chars, Mode),
    more_alts(Rest, First, Mode).

more_alts("|" ++ Chars, Left, Mode) ->
    {Right, Rest} = seq(Chars, Mode),
    more_alts(Rest, {'or', Left, Right}, Mode);
more_alts(Rest, Tree, _Mode) ->
    {Tree, Rest}.

%% A sequence ends at "|", ")" or the end. An empty one is epsilon,
%% which lex rejects later (empty regexps and alternatives).
seq(Chars, Mode) ->
    seq(Chars, Mode, epsilon).

seq([C | _] = Rest, _Mode, Tree) when C =:= $|; C =:= $) ->
    {Tree, Rest};
seq([], _Mode, Tree) ->
    {Tree, []};
seq(Chars, Mode, Tree) ->
    {Item, Rest} = postfix(Chars, Mode),
    seq(Rest, Mode, append(Tree, Item)).

append(epsilon, Item) -> Item;
append(Tree, Item) -> {concat, Tree, Item}.

postfix(Chars, Mode) ->
    {Atom, Rest} = atom(Chars, Mode),
    quantifiers(Rest, Atom).

quantifiers("*" ++ Rest, Tree) -> quantifiers(Rest, {kclosure, Tree});
quantifiers("+" ++ Rest, Tree) -> quantifiers(Rest, {pclosure, Tree});
quantifiers("?" ++ Rest, Tree) -> quantifiers(Rest, {optional, Tree});
quantifiers(Rest, Tree) -> {Tree, Rest}.

atom("(?" ++ After, Mode) ->
    case flag(After) of
        {Caseless, ":" ++ Body} ->
            {RE, Rest} = close_group(alt(Body, Mode), "(?"),
            {{caseless, Caseless, RE}, Rest};
        {_, ")" ++ _} ->
            ?FAIL({misplaced_flags,
                   "(?i) is only allowed at the start; use (?i:...)"});
        {_, _} ->
            ?FAIL({illegal_flags, lists:sublist(After, 3)})
    end;
atom("(" ++ Body, Mode) ->
    close_group(alt(Body, Mode), "(");
atom("[" ++ Body, Mode) ->
    class(Body, Mode);
atom("\\" ++ Escape, Mode) ->
    escape(Escape, Mode);
atom("." ++ Rest, _Mode) -> {{comp_class, "\n"}, Rest};
atom("^" ++ Rest, _Mode) -> {bos, Rest};
atom("$" ++ Rest, _Mode) -> {eos, Rest};
atom([C | _], _Mode) when C =:= $*; C =:= $+; C =:= $?; C =:= $] ->
    ?FAIL({illegal, [C]});
atom([C | Rest], _Mode) ->
    {C, Rest}.

close_group({RE, ")" ++ Rest}, _Open) -> {RE, Rest};
close_group({_RE, _Rest}, Open) -> ?FAIL({unterminated, Open}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Escapes (the part after the backslash), the same in and out of
%% classes. The result is a character or, in unicode mode, a named class.

escape([D1, D2, D3 | Rest], Mode)
  when ?is_octal(D1), ?is_octal(D2), ?is_octal(D3) ->
    {check_max(list_to_integer([D1, D2, D3], 8), Mode, [$\\, D1, D2, D3]), Rest};
escape([$x, ${ | Rest], Mode) ->
    braced_hex(Rest, Mode);
escape([$x, H1, H2 | Rest], _Mode) when ?is_hex(H1), ?is_hex(H2) ->
    {list_to_integer([H1, H2], 16), Rest};
escape([$x | _], _Mode) ->
    ?FAIL({illegal, "x"});
escape([P | Rest], unicode) when P =:= $p; P =:= $P ->
    property(P, Rest);
escape([C | Rest], unicode) when C =:= $d; C =:= $D; C =:= $s; C =:= $S;
                                 C =:= $w; C =:= $W ->
    {perl_class(C), Rest};
escape([C | Rest], _Mode) ->
    {control_char(C), Rest};
escape([], _Mode) ->
    ?FAIL({unterminated, "\\"}).

%% \x{H...}: one or more hex digits.
braced_hex(Chars, Mode) ->
    {Digits, Rest} = lists:splitwith(fun(C) -> ?is_hex(C) end, Chars),
    case Rest of
        "}" ++ After when Digits =/= [] ->
            {check_max(list_to_integer(Digits, 16), Mode, "}"), After};
        "}" ++ _ ->
            ?FAIL({illegal, "}"});
        _ ->
            ?FAIL({unterminated, "\\x{"})
    end.

check_max(Value, bytes, _What) when Value =< 16#FF -> Value;
check_max(Value, unicode, _What) when Value =< 16#10FFFF -> Value;
check_max(_Value, _Mode, What) -> ?FAIL({illegal, What}).

%% \p{Name}, \pX and the complements \P{Name}, \PX.
property(P, "{" ++ Chars) ->
    case lists:splitwith(fun(C) -> C =/= $} end, Chars) of
        {[], _} when Chars =/= [] -> ?FAIL({illegal, [P, ${, $}]});
        {Name, "}" ++ Rest} -> {{prop, P =:= $P, Name}, Rest};
        {_, []} -> ?FAIL({unterminated, [$\\, P, ${]})
    end;
property(P, [X | Rest]) ->
    {{prop, P =:= $P, [X]}, Rest};
property(P, []) ->
    ?FAIL({unterminated, [$\\, P]}).

perl_class($d) -> {prop, false, "Nd"};
perl_class($D) -> {prop, true, "Nd"};
perl_class($s) -> {prop, false, "White_Space"};
perl_class($S) -> {prop, true, "White_Space"};
perl_class($w) -> {prop, false, "Word"};
perl_class($W) -> {prop, true, "Word"}.

%% In bytes mode \d and \s are DEL and space, as in the AWK syntax the
%% lexer started from (unicode mode handles them in escape/2).
control_char($n) -> $\n;
control_char($r) -> $\r;
control_char($t) -> $\t;
control_char($v) -> $\v;
control_char($f) -> $\f;
control_char($b) -> $\b;
control_char($e) -> $\e;
control_char($d) -> 127;
control_char($s) -> $\s;
control_char(C) -> C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Character classes, after the "[".

class("^" ++ Chars, Mode) ->
    {Items, Rest} = class_items(Chars, Mode),
    {{comp_class, Items}, Rest};
class(Chars, Mode) ->
    {Items, Rest} = class_items(Chars, Mode),
    {{char_class, Items}, Rest}.

%% A "]" first is literal.
class_items("]" ++ Chars, Mode) ->
    class_items(Chars, Mode, [$]]);
class_items(Chars, Mode) ->
    class_items(Chars, Mode, []).

class_items("]" ++ Rest, _Mode, Acc) ->
    {lists:reverse(Acc), Rest};
class_items([], _Mode, _Acc) ->
    ?FAIL({unterminated, "["});
class_items("[:" ++ After = Chars, Mode, Acc) ->
    case posix_class(After) of
        {Posix, Rest} -> class_items(Rest, Mode, [Posix | Acc]);
        none -> class_range(Chars, Mode, Acc)    % "[" is literal
    end;
class_items(Chars, Mode, Acc) ->
    class_range(Chars, Mode, Acc).

%% A single item, or a range "a-z" of two characters with a < z.
%% "-" is literal at the end and after a named class.
class_range(Chars, Mode, Acc) ->
    case class_single(Chars, Mode) of
        {First, [$-, C | After]} when is_integer(First), C =/= $] ->
            case class_single([C | After], Mode) of
                {Last, Rest} when is_integer(Last), First < Last ->
                    class_items(Rest, Mode, [{First, Last} | Acc]);
                {Last, _Rest} ->
                    ?FAIL({char_class, [First, $-, Last]})
            end;
        {Item, Rest} ->
            class_items(Rest, Mode, [Item | Acc])
    end.

class_single("\\" ++ Escape, Mode) -> escape(Escape, Mode);
class_single([C | Rest], _Mode) -> {C, Rest};
class_single([], _Mode) -> ?FAIL({unterminated, "["}).

%% After "[:": an optional "^", a lower-case name and ":]".
posix_class("^" ++ Chars) -> posix_class(Chars, true);
posix_class(Chars) -> posix_class(Chars, false).

posix_class(Chars, Negated) ->
    case lists:splitwith(fun(C) -> C >= $a andalso C =< $z end, Chars) of
        {[_ | _] = Name, ":]" ++ Rest} -> {{posix, Negated, Name}, Rest};
        _ -> none
    end.
