%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unicode support for lex.
%%
%% The lexer proper still works on bytes 0..255. Unicode is handled by
%% the regexp compiler: every character and character class is first
%% a set of code points, and is then LOWERED into a regexp over byte
%% ranges that matches exactly the well-formed encodings of those code
%% points in the chosen encoding. The NFA -> DFA -> table pipeline is
%% unchanged.
%%
%% Because only well-formed sequences are ever accepted, a token can
%% never contain or end in a partial character. When lexing fails, the
%% driver calls first_error/3 to tell an encoding error (invalid or
%% incomplete sequence) from an ordinary "no rule matches".
%%
%% This module provides:
%% - encodings:      normalize_encoding/1, is_unicode/1
%% - named classes:  property/1 (Perl-style \p{...} names),
%%                   posix_class/2 ([:alpha:] etc.)
%% - case folding:   case_closure/1, ascii_case_closure/1
%% - code point sets: normalize/1, union/2, complement/1, intersect/2,
%%                   subtract/2, scalar_values/0
%% - lowering:       byte_seqs/2, to_regexp/2
%% - decoding:       decode_char/2, decode_prefix/2, first_error/3,
%%                   encode/2
%%
%% A code point set is a sorted list of disjoint, non-adjacent
%% {From, To} pairs.

-module(lex_unicode).

-export([normalize_encoding/1,
         is_unicode/1,
         property/1,
         posix_class/2,
         case_closure/1,
         ascii_case_closure/1,
         normalize/1,
         union/2,
         complement/1,
         intersect/2,
         subtract/2,
         scalar_values/0,
         byte_seqs/2,
         to_regexp/2,
         decode_char/2,
         decode_prefix/2,
         first_error/3,
         encode/2]).

-define(MAX_CP, 16#10FFFF).
-define(IS_BYTE(B), (is_integer(B) andalso B >= 0 andalso B =< 255)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encodings
%%
%% bytes          legacy mode: regexps are bytes (non-ASCII literals in
%%                the regexp are converted to UTF-8), no \p{...}
%% latin1         code points 0..255, one byte each
%% utf8
%% {utf16, big | little}   (also utf16be, utf16le; utf16 = big)
%% {utf32, big | little}   (also utf32be, utf32le; utf32 = big)

normalize_encoding(bytes) -> bytes;
normalize_encoding(latin1) -> latin1;
normalize_encoding(utf8) -> utf8;
normalize_encoding(unicode) -> utf8;
normalize_encoding(utf16) -> {utf16, big};
normalize_encoding(utf16be) -> {utf16, big};
normalize_encoding(utf16le) -> {utf16, little};
normalize_encoding({utf16, E} = Enc) when E == big; E == little -> Enc;
normalize_encoding(utf32) -> {utf32, big};
normalize_encoding(utf32be) -> {utf32, big};
normalize_encoding(utf32le) -> {utf32, little};
normalize_encoding({utf32, E} = Enc) when E == big; E == little -> Enc;
normalize_encoding(Other) -> exit({unknown_encoding, Other}).

%% true if regexps are interpreted as code points
is_unicode(bytes) -> false;
is_unicode(_) -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Named classes, following Perl's \p{...} syntax (perlunicode,
%% perluniprops). Names are matched loosely: case, spaces, '_' and '-'
%% are ignored.
%%
%%   \p{L}  \p{Lu}  \p{Letter}  \p{Uppercase_Letter}   general category
%%   \p{gc=Lu}  \p{General_Category=Lu}  \p{Category=Lu}
%%   \p{L&}  \p{LC}  \p{Cased_Letter}
%%   \p{Greek}  \p{Grek}               script extensions (as Perl)
%%   \p{sc=Greek}  \p{Script=Greek}    script
%%   \p{scx=Greek}  \p{Script_Extensions=Greek}
%%   \p{InGreek_and_Coptic}  \p{Block=Greek}  \p{blk=Greek}
%%   \p{White_Space}  \p{Alphabetic}  \p{ID_Start}  ...  binary props
%%   \p{Alphabetic=No}  \p{Alpha=F}    negated binary prop
%%   \p{IsGreek}  \p{IsL}              optional "Is" prefix
%%   \p{Any} \p{All} \p{Assigned} \p{ASCII}
%%   \p{Word} \p{Alnum} \p{Blank} \p{XDigit}  (Perl's XPosix classes)
%%   \p{^Name}                         complement
%%
%% Returns {ok, Set} or {error, Reason}. The set may contain surrogates
%% (\p{Cs}); the lowering step removes them.

property(Name0) ->
    Name = string:trim(Name0),
    case Name of
        [$^ | Rest] ->
            case property(Rest) of
                {ok, Set} -> {ok, complement(Set)};
                Err -> Err
            end;
        _ ->
            case split_prop(Name) of
                {Prop, Value} -> prop_value(loose(Prop), Value, Name0);
                none -> bare(loose(Name), Name0)
            end
    end.

split_prop(Name) ->
    case string:split(Name, "=") of
        [P, V] -> {P, V};
        [_] ->
            case string:split(Name, ":") of
                [P, V] -> {P, V};
                [_] -> none
            end
    end.

prop_value(P, V, Orig) when P == "gc"; P == "generalcategory"; P == "category" ->
    lookup(gc, loose(V), Orig);
prop_value(P, V, Orig) when P == "sc"; P == "script" ->
    lookup(sc, loose(V), Orig);
prop_value(P, V, Orig) when P == "scx"; P == "scriptextensions" ->
    lookup(scx, loose(V), Orig);
prop_value(P, V, Orig) when P == "blk"; P == "block" ->
    lookup(blk, loose(V), Orig);
prop_value(P, V, Orig) ->
    case bare(P, Orig) of
        {ok, Set} ->
            case loose(V) of
                Y when Y == "y"; Y == "yes"; Y == "t"; Y == "true" -> {ok, Set};
                N when N == "n"; N == "no"; N == "f"; N == "false" -> {ok, complement(Set)};
                _ -> {error, {bad_property_value, Orig}}
            end;
        Err ->
            Err
    end.

%% Resolution order for a bare name: Perl specials, general category,
%% script extensions, binary property; then retry without an "is"
%% prefix; an "in" prefix means block.
bare(L, Orig) ->
    case special(L) of
        {ok, _} = Ok -> Ok;
        undefined ->
            case first_found([gc, scx, bin], L) of
                {ok, _} = Ok -> Ok;
                undefined ->
                    case L of
                        "is" ++ L1 when L1 =/= [] -> bare(L1, Orig);
                        "in" ++ L1 when L1 =/= [] -> lookup(blk, L1, Orig);
                        _ -> {error, {unknown_property, Orig}}
                    end
            end
    end.

first_found([Kind | Kinds], L) ->
    case lex_unicode_data:canonical(Kind, L) of
        undefined -> first_found(Kinds, L);
        Canon -> {ok, lex_unicode_data:ranges(Kind, Canon)}
    end;
first_found([], _L) ->
    undefined.

lookup(Kind, L, Orig) ->
    case first_found([Kind], L) of
        {ok, _} = Ok -> Ok;
        undefined -> {error, {unknown_property, Orig}}
    end.

special("any") -> {ok, [{0, ?MAX_CP}]};
special("all") -> {ok, [{0, ?MAX_CP}]};
special("unicode") -> {ok, [{0, ?MAX_CP}]};
special("assigned") -> {ok, complement(gc("Cn"))};
special("ascii") -> {ok, [{0, 127}]};
special("l&") -> {ok, gc("LC")};
special("horizspace") -> special("blank");
special("xposix" ++ Name) -> xposix(Name);
special("posix" ++ Name) -> ascii_only(xposix(Name));
special(Name) when Name == "word"; Name == "alnum"; Name == "blank";
                   Name == "xdigit"; Name == "graph"; Name == "print" ->
    xposix(Name);
special(_) -> undefined.

%% Perl's XPosix classes (perlrecharclass): the Unicode versions of the
%% POSIX classes. Restricted to ASCII they are exactly the POSIX "C"
%% locale classes, which is what [:name:] means in bytes mode.
xposix("alpha") -> {ok, bin("Alphabetic")};
xposix("digit") -> {ok, gc("Nd")};
xposix("alnum") -> {ok, union(bin("Alphabetic"), gc("Nd"))};
xposix("upper") -> {ok, bin("Uppercase")};
xposix("lower") -> {ok, bin("Lowercase")};
xposix("space") -> {ok, bin("White_Space")};
xposix("blank") -> {ok, union([{9, 9}], gc("Zs"))};
xposix("cntrl") -> {ok, gc("Cc")};
xposix("punct") -> {ok, union(gc("P"), intersect(gc("S"), [{0, 127}]))};
xposix("graph") ->
    {ok, complement(union_all([bin("White_Space"), gc("Cc"), gc("Cs"), gc("Cn")]))};
xposix("print") ->
    {ok, Graph} = xposix("graph"),
    {ok, Blank} = xposix("blank"),
    {ok, subtract(union(Graph, Blank), gc("Cc"))};
xposix("xdigit") -> {ok, bin("Hex_Digit")};
xposix("word") ->
    %% Perl \w: Alphabetic + Mark + Decimal_Number + Connector_Punctuation
    %%   + Join_Control
    {ok, union_all([bin("Alphabetic"), gc("M"), gc("Nd"), gc("Pc"),
                    bin("Join_Control")])};
xposix(_) -> undefined.

ascii_only({ok, Set}) -> {ok, intersect(Set, [{0, 127}])};
ascii_only(undefined) -> undefined.

%% posix_class(Name, Charset) -> {ok, Set} | {error, Reason}
%%   Name as in [:Name:]: alpha digit alnum upper lower space blank
%%   cntrl punct graph print xdigit word (word is a Perl extension)
%%   Charset = unicode | ascii
posix_class(Name, unicode) ->
    posix_result(xposix(Name), Name);
posix_class(Name, ascii) ->
    posix_result(ascii_only(xposix(Name)), Name).

posix_result({ok, _} = Ok, _Name) -> Ok;
posix_result(undefined, Name) -> {error, {unknown_posix_class, Name}}.

gc(Name) -> lex_unicode_data:ranges(gc, Name).
bin(Name) -> lex_unicode_data:ranges(bin, Name).

%% UAX44-LM3 loose matching (also keeps '&' for "L&")
loose(S) ->
    string:lowercase([ C || C <- S, C =/= $\s, C =/= $_, C =/= $-, C =/= $\t ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code point sets

%% Unicode scalar values: all code points except surrogates. These are
%% exactly the code points that have a well-formed encoding.
scalar_values() ->
    [{0, 16#D7FF}, {16#E000, ?MAX_CP}].

normalize(Rs) ->
    merge(lists:sort([ R || {F, T} = R <- Rs, F =< T ])).

merge([{F1, T1}, {F2, T2} | Rs]) when F2 =< T1 + 1 ->
    merge([{F1, max(T1, T2)} | Rs]);
merge([R | Rs]) ->
    [R | merge(Rs)];
merge([]) ->
    [].

union(A, B) ->
    normalize(A ++ B).

union_all(Sets) ->
    normalize(lists:append(Sets)).

%% complement with respect to 0..10FFFF
complement(Rs) ->
    compl(0, normalize(Rs)).

compl(M, [{F, T} | Rs]) when M < F -> [{M, F - 1} | compl(T + 1, Rs)];
compl(_, [{_, T} | Rs]) -> compl(T + 1, Rs);
compl(M, []) when M =< ?MAX_CP -> [{M, ?MAX_CP}];
compl(_, []) -> [].

intersect([{F1, T1} | R1] = A, [{F2, T2} | R2] = B) ->
    F = max(F1, F2),
    T = min(T1, T2),
    Rest = if
               T1 < T2 -> intersect(R1, B);
               true -> intersect(A, R2)
           end,
    if
        F =< T -> [{F, T} | Rest];
        true -> Rest
    end;
intersect(_, _) ->
    [].

subtract(A, B) ->
    intersect(normalize(A), complement(B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Case-insensitive matching
%%
%% case_closure(Set) adds every code point that has the same simple case
%% folding (CaseFolding.txt, status C and S) as some member of Set; e.g.
%% k -> k K U+212A (KELVIN SIGN), sigma -> sigma, final sigma, SIGMA.
%% Full (multi-character) foldings such as sharp s -> "ss" are not used:
%% a set of single characters can only match single characters.
%%
%% ascii_case_closure(Set) only pairs A-Z with a-z (bytes mode, where
%% the bytes above 127 may be parts of UTF-8 characters).

case_closure(Set0) ->
    Set = normalize(Set0),
    Extra = orbits_meeting(Set, case_index(), []),
    normalize(Set ++ [ {C, C} || O <- Extra, C <- O ]).

%% Walk the sorted [{CodePoint, Orbit}] index alongside the sorted set.
orbits_meeting([{F, T} | Rs] = Set, [{C, O} | Is] = Index, Acc) ->
    if
        C < F -> orbits_meeting(Set, Is, Acc);
        C > T -> orbits_meeting(Rs, Index, Acc);
        true -> orbits_meeting(Set, Is, [O | Acc])
    end;
orbits_meeting(_, _, Acc) ->
    Acc.

case_index() ->
    case persistent_term:get({?MODULE, case_index}, undefined) of
        undefined ->
            Index = lists:sort([ {C, O} || O <- lex_unicode_data:case_orbits(), C <- O ]),
            persistent_term:put({?MODULE, case_index}, Index),
            Index;
        Index ->
            Index
    end.

ascii_case_closure(Set0) ->
    Set = normalize(Set0),
    Upper = intersect(Set, [{$A, $Z}]),
    Lower = intersect(Set, [{$a, $z}]),
    normalize(Set ++ [ {F + 32, T + 32} || {F, T} <- Upper ]
                  ++ [ {F - 32, T - 32} || {F, T} <- Lower ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lowering code point sets to byte sequences
%%
%% byte_seqs(Enc, Set) -> [[{ByteLo, ByteHi}]]
%%
%% Each element is a sequence of byte ranges; it matches every byte
%% string whose i:th byte lies in the i:th range (a cartesian product).
%% The union of all sequences is exactly the set of encodings of the
%% scalar values in Set. Surrogates and code points that the encoding
%% cannot represent are dropped.
%%
%% The central step is split/4, the UTF-8 range splitting of RE2
%% (re2/compile.cc) and of Rust's regex-syntax crate (module utf8),
%% generalized to N digits of Bits bits:
%% a range Lo..Hi of N-digit numbers in base 2^Bits is split into
%% subranges such that each digit position varies independently, i.e.
%% each subrange is a product of digit ranges. UTF-8 uses 6-bit digits
%% (with a lead-byte prefix), UTF-16 code units and UTF-32 use 8-bit
%% digits (bytes), and UTF-16 surrogate pairs use two 10-bit digits.

byte_seqs(latin1, Set) ->
    [ [R] || R <- intersect(normalize(Set), [{0, 255}]) ];
byte_seqs(utf8, Set) ->
    lists:append([ utf8_seqs(F, T) || {F, T} <- scalars(Set) ]);
byte_seqs({utf16, Endian}, Set) ->
    lists:append([ utf16_seqs(Endian, F, T) || {F, T} <- scalars(Set) ]);
byte_seqs({utf32, Endian}, Set) ->
    [ endian(Endian, Seq)
      || {F, T} <- scalars(Set), Seq <- split(F, T, 8, 4) ];
byte_seqs(bytes, _Set) ->
    exit(unicode_class_needs_encoding).

scalars(Set) ->
    intersect(normalize(Set), scalar_values()).

%% UTF-8: split by encoded length, then into products of 6-bit digits
utf8_seqs(F, T) ->
    lists:append(
      [ [ utf8_bytes(N, Seq) || Seq <- split(Lo, Hi, 6, N) ]
        || {N, Min, Max} <- [{1, 0, 16#7F}, {2, 16#80, 16#7FF},
                             {3, 16#800, 16#FFFF}, {4, 16#10000, ?MAX_CP}],
           Lo <- [max(F, Min)], Hi <- [min(T, Max)], Lo =< Hi ]).

utf8_bytes(1, Seq) ->
    Seq;
utf8_bytes(N, [{L, H} | Conts]) ->
    Lead = element(N, {0, 16#C0, 16#E0, 16#F0}),
    [{Lead bor L, Lead bor H} | [ {16#80 bor CL, 16#80 bor CH} || {CL, CH} <- Conts ]].

%% UTF-16: BMP code points are one code unit, the rest a surrogate pair
utf16_seqs(Endian, F, T) ->
    BMP = [ unit_seqs(Endian, Lo, Hi)
            || Lo <- [F], Hi <- [min(T, 16#FFFF)], Lo =< Hi ],
    Supp = [ [ A ++ B || A <- unit_seqs(Endian, 16#D800 + HL, 16#D800 + HH),
                         B <- unit_seqs(Endian, 16#DC00 + LL, 16#DC00 + LH) ]
             || Lo <- [max(F, 16#10000)], Hi <- [T], Lo =< Hi,
                [{HL, HH}, {LL, LH}] <- split(Lo - 16#10000, Hi - 16#10000, 10, 2) ],
    lists:append(BMP) ++ lists:append(Supp).

%% one 16-bit code unit range
unit_seqs(Endian, Lo, Hi) ->
    [ endian(Endian, Seq) || Seq <- split(Lo, Hi, 8, 2) ].

endian(big, Seq) -> Seq;
endian(little, Seq) -> lists:reverse(Seq).

%% split(Lo, Hi, Bits, N) -> [[{DigitLo, DigitHi}]] (most significant
%% digit first). The most significant digit is not masked, so it may be
%% wider than Bits (used for UTF-8 1-byte and UTF-16 10-bit digits).
split(Lo, Hi, Bits, N) ->
    split(Lo, Hi, Bits, N, 1).

split(Lo, Hi, Bits, N, I) when I < N ->
    Shift = Bits * I,
    M = (1 bsl Shift) - 1,
    if
        (Lo bsr Shift) =/= (Hi bsr Shift), (Lo band M) =/= 0 ->
            split(Lo, Lo bor M, Bits, N) ++ split((Lo bor M) + 1, Hi, Bits, N);
        (Lo bsr Shift) =/= (Hi bsr Shift), (Hi band M) =/= M ->
            split(Lo, (Hi band (bnot M)) - 1, Bits, N) ++ split(Hi band (bnot M), Hi, Bits, N);
        true ->
            split(Lo, Hi, Bits, N, I + 1)
    end;
split(Lo, Hi, Bits, N, _I) ->
    [lists:zip(digits(Lo, Bits, N), digits(Hi, Bits, N))].

digits(X, Bits, N) ->
    Mask = (1 bsl Bits) - 1,
    [ case K of
          0 -> X bsr (Bits * (N - 1));
          _ -> (X bsr (Bits * (N - 1 - K))) band Mask
      end || K <- lists:seq(0, N - 1) ].

%% to_regexp(Enc, Set) -> lex internal regexp over bytes
%%   {seq, [R]} | {disj, [R]} | {range, From, To}
%%
%% The byte sequences are merged into a trie on their leading byte
%% ranges so that the NFA does not get one branch per code point range.
to_regexp(Enc, Set) ->
    trie(byte_seqs(Enc, Set)).

trie(Seqs) ->
    case [ alt(Head, Tails) || {Head, Tails} <- group_heads(Seqs) ] of
        [Alt] -> Alt;
        Alts -> {disj, Alts}
    end.

alt({Lo, Hi}, Tails) ->
    case [ T || T <- Tails, T =/= [] ] of
        [] ->
            {range, Lo, Hi};
        NonEmpty when length(NonEmpty) == length(Tails) ->
            {seq, [{range, Lo, Hi}, trie(NonEmpty)]};
        NonEmpty ->
            {seq, [{range, Lo, Hi}, {disj, [{seq, []}, trie(NonEmpty)]}]}
    end.

%% group sequences by first byte range, keeping first-seen order
group_heads(Seqs) ->
    {Order, Map} =
        lists:foldl(
          fun([H | T], {Ord, M}) ->
                  case M of
                      #{H := Ts} -> {Ord, M#{H := [T | Ts]}};
                      _ -> {[H | Ord], M#{H => [T]}}
                  end
          end, {[], #{}}, Seqs),
    [ {H, lists:reverse(maps:get(H, Map))} || H <- lists:reverse(Order) ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decoding and validation of byte lists
%%
%% decode_char(Enc, Bytes) ->
%%     {ok, CodePoint, Length, Rest}
%%   | {invalid, Length}     malformed; Length bytes inspected
%%   | {incomplete, Length}  well-formed so far, but the input ends
%%   | not_a_byte            list element is not an integer 0..255
%%   | eof

decode_char(_Enc, []) ->
    eof;
decode_char(_Enc, [B | _]) when not ?IS_BYTE(B) ->
    not_a_byte;
decode_char(Enc, [B | R]) when Enc == bytes; Enc == latin1 ->
    {ok, B, 1, R};
decode_char(utf8, [B | R]) when B < 16#80 ->
    {ok, B, 1, R};
decode_char(utf8, [B | R]) ->
    utf8_cont(utf8_lead(B), R);
decode_char({utf16, E}, Bytes) ->
    case unit16(E, Bytes) of
        {ok, U, R} when U >= 16#D800, U =< 16#DBFF ->
            case unit16(E, R) of
                {ok, U2, R2} when U2 >= 16#DC00, U2 =< 16#DFFF ->
                    {ok, 16#10000 + ((U - 16#D800) bsl 10) + (U2 - 16#DC00), 4, R2};
                {ok, _, _} -> {invalid, 4};
                {incomplete, N} -> {incomplete, 2 + N};
                not_a_byte -> not_a_byte
            end;
        {ok, U, _R} when U >= 16#DC00, U =< 16#DFFF ->
            {invalid, 2};
        {ok, U, R} ->
            {ok, U, 2, R};
        {incomplete, N} ->
            {incomplete, N};
        not_a_byte ->
            not_a_byte
    end;
decode_char({utf32, E}, Bytes) ->
    case take(4, Bytes) of
        {ok, Bs, R} ->
            <<U:32/integer>> = list_to_binary(endian(E, Bs)),
            if
                U >= 16#D800, U =< 16#DFFF -> {invalid, 4};
                U > ?MAX_CP -> {invalid, 4};
                true -> {ok, U, 4, R}
            end;
        Other ->
            Other
    end.

%% lead byte -> {Value bits, [allowed continuation byte ranges]}
%% (RFC 3629: no overlongs, no surrogates, nothing above 10FFFF)
utf8_lead(B) when B >= 16#C2, B =< 16#DF -> {B band 16#1F, [{16#80, 16#BF}]};
utf8_lead(16#E0) -> {0, [{16#A0, 16#BF}, {16#80, 16#BF}]};
utf8_lead(16#ED) -> {16#D, [{16#80, 16#9F}, {16#80, 16#BF}]};
utf8_lead(B) when B >= 16#E1, B =< 16#EF -> {B band 16#0F, [{16#80, 16#BF}, {16#80, 16#BF}]};
utf8_lead(16#F0) -> {0, [{16#90, 16#BF}, {16#80, 16#BF}, {16#80, 16#BF}]};
utf8_lead(B) when B >= 16#F1, B =< 16#F3 -> {B band 16#07, [{16#80, 16#BF}, {16#80, 16#BF}, {16#80, 16#BF}]};
utf8_lead(16#F4) -> {4, [{16#80, 16#8F}, {16#80, 16#BF}, {16#80, 16#BF}]};
utf8_lead(_) -> invalid.

utf8_cont(invalid, _R) ->
    {invalid, 1};
utf8_cont({V, Conts}, R) ->
    utf8_cont(V, Conts, R, 1).

utf8_cont(V, [], R, Len) ->
    {ok, V, Len, R};
utf8_cont(_V, _Conts, [], Len) ->
    {incomplete, Len};
utf8_cont(_V, _Conts, [C | _], _Len) when not ?IS_BYTE(C) ->
    not_a_byte;
utf8_cont(V, [{Lo, Hi} | Conts], [C | R], Len) when C >= Lo, C =< Hi ->
    utf8_cont((V bsl 6) bor (C band 16#3F), Conts, R, Len + 1);
utf8_cont(_V, _Conts, _R, Len) ->
    {invalid, Len + 1}.

unit16(E, Bytes) ->
    case take(2, Bytes) of
        {ok, Bs, R} ->
            [Hi, Lo] = endian(E, Bs),
            {ok, (Hi bsl 8) bor Lo, R};
        Other ->
            Other
    end.

take(N, Bytes) ->
    take(N, Bytes, []).

take(0, R, Acc) -> {ok, lists:reverse(Acc), R};
take(_N, [], Acc) -> {incomplete, length(Acc)};
take(_N, [B | _], _Acc) when not ?IS_BYTE(B) -> not_a_byte;
take(N, [B | R], Acc) -> take(N - 1, R, [B | Acc]).

%% decode_prefix(Enc, Bytes) -> {CodePoints, UndecodedRest}
%% Decodes as far as the bytes are well-formed.
decode_prefix(Enc, Bytes) ->
    decode_prefix(Enc, Bytes, []).

decode_prefix(Enc, Bytes, Acc) ->
    case decode_char(Enc, Bytes) of
        {ok, C, _Len, R} -> decode_prefix(Enc, R, [C | Acc]);
        _ -> {lists:reverse(Acc), Bytes}
    end.

%% first_error(Enc, Bytes, StopAt) ->
%%     none | {invalid | incomplete | not_a_byte, Offset, BadBytes}
%%
%% Scan characters from the start of Bytes (which must be at a
%% character boundary) and report the first malformed one, provided it
%% starts at byte offset =< StopAt. Only reads a few bytes past StopAt.
first_error(Enc, Bytes, StopAt) ->
    first_error(Enc, Bytes, 0, StopAt).

first_error(_Enc, _Bytes, Off, StopAt) when Off > StopAt ->
    none;
first_error(Enc, Bytes, Off, StopAt) ->
    case decode_char(Enc, Bytes) of
        {ok, _C, Len, R} -> first_error(Enc, R, Off + Len, StopAt);
        eof -> none;
        {Kind, Len} -> {Kind, Off, lists:sublist(Bytes, max(Len, 1))};
        not_a_byte -> {not_a_byte, Off, lists:sublist(Bytes, 1)}
    end.

%% encode(Enc, Chars) -> [byte()]
%% Encode a list of code points (an Erlang string).
encode(bytes, Chars) ->
    binary_to_list(unicode:characters_to_binary(Chars));
encode(latin1, Chars) ->
    case lists:all(fun(C) -> ?IS_BYTE(C) end, Chars) of
        true -> Chars;
        false -> exit({not_latin1, Chars})
    end;
encode(Enc, Chars) ->
    case unicode:characters_to_binary(Chars, unicode, Enc) of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        Err -> exit({cannot_encode, Enc, Err})
    end.
