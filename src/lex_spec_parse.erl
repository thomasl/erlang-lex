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
%% There are two modes. In `bytes` mode the regexp matches bytes: an
%% ASCII character or an escape such as \xHH is one byte, and a non-ASCII
%% character is its UTF-8 bytes as a unit, so "λ+" is (CE BB)+ and a class
%% "[λμ]" holds whole characters ({utf8, C} and {utf8, First, Last} items,
%% which lex lowers to byte sequences). A negated class cannot hold
%% non-ASCII characters, since it matches one byte. With the latin1 regexp
%% encoding every character is one byte instead. `\d` `\s` `\w` keep their
%% old AWK meanings (DEL, space, w) and `\p` is a literal p. In `unicode`
%% mode every element is a code point, and `\p{..}`, `\P{..}`, `\d` `\s`
%% `\w` (and complements) are named classes.
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
                    | named_class()
                    %% bytes mode: whole non-ASCII characters
                    | {utf8, char()}
                    | {utf8, First :: char(), Last :: char()}.

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
                      | {non_ascii_in_negated_class, string()}
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
    %% internally, bytes mode is utf8_bytes (non-ASCII characters are
    %% whole UTF-8 characters) or, with the latin1 regexp encoding, bytes
    ParseMode = case {Mode, RegexpEnc} of
                    {unicode, _} -> unicode;
                    {bytes, latin1} -> bytes;
                    {bytes, _} -> utf8_bytes
                end,
    try
        {ok, parse(code_points(Regexp, RegexpEnc), ParseMode)}
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
format_error({non_ascii_in_negated_class, What}) ->
    io_lib:format("a negated class cannot hold the non-ASCII characters `~ts' in bytes "
                  "mode, since it matches one byte; use {encoding, utf8}", [What]);
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
%% In bytes mode a non-ASCII character then stands for its UTF-8 bytes
%% (except with latin1), so "é" in a regexp matches the two bytes of an
%% UTF-8 é in the input.

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
    whole_char(escape(Escape, Mode));
atom("." ++ Rest, _Mode) -> {{comp_class, "\n"}, Rest};
atom("^" ++ Rest, _Mode) -> {bos, Rest};
atom("$" ++ Rest, _Mode) -> {eos, Rest};
atom([C | _], _Mode) when C =:= $*; C =:= $+; C =:= $?; C =:= $] ->
    ?FAIL({illegal, [C]});
atom([C | Rest], utf8_bytes) when C >= 128 ->
    {utf8_seq(C), Rest};
atom([C | Rest], _Mode) ->
    {C, Rest}.

%% Bytes mode: a non-ASCII character is its UTF-8 bytes in sequence, as
%% one atom, so that a quantifier after it applies to all of them.
whole_char({{utf8, C}, Rest}) -> {utf8_seq(C), Rest};
whole_char(Other) -> Other.

utf8_seq(C) ->
    [B | Bs] = binary_to_list(unicode:characters_to_binary([C])),
    lists:foldl(fun(Byte, Seq) -> {concat, Seq, Byte} end, B, Bs).

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
escape([C | Rest], utf8_bytes) when C >= 128 ->
    {{utf8, C}, Rest};                          % an escaped non-ASCII character
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
check_max(Value, utf8_bytes, _What) when Value =< 16#FF -> Value;
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
    %% bytes mode: a negated class matches one byte, which cannot be "any
    %% character but é" when é is two bytes
    case [Item || Item <- Items, is_whole_char(Item)] of
        [] -> {{comp_class, Items}, Rest};
        NonAscii -> ?FAIL({non_ascii_in_negated_class, item_chars(NonAscii)})
    end;
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
        {First, [$-, C | After]} when C =/= $], (is_integer(First) orelse
                                                 element(1, First) =:= utf8) ->
            {Last, Rest} = class_single([C | After], Mode),
            class_items(Rest, Mode, [range(First, Last) | Acc]);
        {Item, Rest} ->
            class_items(Rest, Mode, [Item | Acc])
    end.

%% Two bytes (or two code points, in unicode mode) give a range of them.
%% In bytes mode, if either end is a whole non-ASCII character, the range
%% is of characters; the other end must then be ASCII (as a byte escape
%% above 127 would be neither a character nor a byte of one).
range(First, Last) when is_integer(First), is_integer(Last), First < Last ->
    {First, Last};
range(First, Last) when is_integer(First), is_integer(Last) ->
    ?FAIL({char_class, [First, $-, Last]});
range(First, Last) ->
    F = range_char(First), L = range_char(Last),
    case is_integer(F) andalso is_integer(L) andalso F < L of
        true -> {utf8, F, L};
        false -> ?FAIL({char_class, item_chars([First]) ++ "-" ++ item_chars([Last])})
    end.

range_char({utf8, C}) -> C;
range_char(C) when is_integer(C), C < 128 -> C;
range_char(_) -> none.

is_whole_char({utf8, _}) -> true;
is_whole_char({utf8, _, _}) -> true;
is_whole_char(_) -> false.

%% The characters of class items, for error messages
item_chars(Items) ->
    lists:append([case Item of
                      {utf8, C} -> [C];
                      {utf8, F, L} -> [F, $-, L];
                      C when is_integer(C) -> [C];
                      _ -> []
                  end || Item <- Items]).

class_single("\\" ++ Escape, Mode) -> escape(Escape, Mode);
class_single([C | Rest], utf8_bytes) when C >= 128 -> {{utf8, C}, Rest};
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
