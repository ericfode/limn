%% JSON Parser/Writer - Pure Prolog DCG
%% =========================================================
%% jso prl | dcg par | pur log
%% *JSON Prolog. DCG parsing. Pure logic.*
%%
%% The monk accepts no impurity. JSON in, terms out. Terms in, JSON out.
%%
%% Usage:
%%   json_parse(Chars, Term)     - parse JSON chars to Prolog term
%%   json_write(Term, Chars)     - write Prolog term as JSON chars
%%
%% Terms:
%%   json([key-value, ...])      - object
%%   [item, ...]                 - array
%%   "string"                    - string (as atom)
%%   123, 123.45                 - number
%%   true, false, null           - literals

:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).

%% ============================================================
%% PARSER
%% ============================================================

%% Main entry point
json_parse(Chars, Term) :-
    phrase(json_value(Term), Chars, []).

%% Value: object | array | string | number | true | false | null
json_value(Term) --> ws, json_value_(Term), ws.

json_value_(json(Pairs)) --> "{", !, ws, json_pairs(Pairs), ws, "}".
json_value_(List) --> "[", !, ws, json_array(List), ws, "]".
json_value_(Str) --> "\"", !, json_string_chars(Cs), "\"", { atom_chars(Str, Cs) }.
json_value_(Num) --> json_number(Num), !.
json_value_(true) --> "true", !.
json_value_(false) --> "false", !.
json_value_(null) --> "null", !.

%% Object pairs
json_pairs([K-V|Rest]) -->
    ws, "\"", json_string_chars(Kc), "\"", ws, ":", ws,
    json_value(V),
    { atom_chars(K, Kc) },
    json_pairs_rest(Rest).
json_pairs([]) --> [].

json_pairs_rest([K-V|Rest]) -->
    ws, ",", ws, "\"", json_string_chars(Kc), "\"", ws, ":", ws,
    json_value(V),
    { atom_chars(K, Kc) },
    json_pairs_rest(Rest).
json_pairs_rest([]) --> [].

%% Array elements
json_array([V|Rest]) -->
    json_value(V),
    json_array_rest(Rest).
json_array([]) --> [].

json_array_rest([V|Rest]) -->
    ws, ",", ws, json_value(V),
    json_array_rest(Rest).
json_array_rest([]) --> [].

%% String (handles escapes)
%% Using char codes: " = 34, \ = 92, n = 110, t = 116, r = 114
%% newline = 10, tab = 9, cr = 13
json_string_chars([]) --> [].
json_string_chars([C|Cs]) --> [B,Q], { char_code(B, 92), char_code(Q, 34), char_code(C, 34) }, !, json_string_chars(Cs).
json_string_chars([C|Cs]) --> [B,B2], { char_code(B, 92), char_code(B2, 92), char_code(C, 92) }, !, json_string_chars(Cs).
json_string_chars([C|Cs]) --> [B,N], { char_code(B, 92), char_code(N, 110), char_code(C, 10) }, !, json_string_chars(Cs).
json_string_chars([C|Cs]) --> [B,T], { char_code(B, 92), char_code(T, 116), char_code(C, 9) }, !, json_string_chars(Cs).
json_string_chars([C|Cs]) --> [B,R], { char_code(B, 92), char_code(R, 114), char_code(C, 13) }, !, json_string_chars(Cs).
json_string_chars([C|Cs]) --> [C], { char_code(C, Code), Code \= 34, Code \= 92 }, json_string_chars(Cs).

%% Number (integer or float)
json_number(Num) -->
    json_int_chars(IntCs),
    { IntCs \= [] },
    ( ".", digit_chars(FracCs), { FracCs \= [] } ->
        { append(IntCs, ['.'|FracCs], AllCs), number_chars(Num, AllCs) }
    ;
        { number_chars(Num, IntCs) }
    ).

json_int_chars(['-'|Cs]) --> "-", !, digit_chars(Cs).
json_int_chars(Cs) --> digit_chars(Cs).

digit_chars([D|Ds]) --> [D], { D @>= '0', D @=< '9' }, digit_chars(Ds).
digit_chars([]) --> [].

%% Whitespace
ws --> [C], { C = ' ' ; C = '\t' ; C = '\n' ; C = '\r' }, !, ws.
ws --> [].

%% ============================================================
%% WRITER
%% ============================================================

%% Main entry point
json_write(Term, Chars) :-
    phrase(json_emit(Term), Chars).

%% Emit value
json_emit(json(Pairs)) --> "{", json_emit_pairs(Pairs), "}".
json_emit([]) --> "[]".
json_emit([H|T]) --> "[", json_emit(H), json_emit_array_rest(T), "]".
json_emit(true) --> "true".
json_emit(false) --> "false".
json_emit(null) --> "null".
json_emit(Str) --> { atom(Str), Str \= true, Str \= false, Str \= null, Str \= [] },
                   "\"", json_emit_string(Str), "\"".
json_emit(Num) --> { number(Num) }, { number_chars(Num, Cs) }, Cs.

%% Emit object pairs
json_emit_pairs([]) --> [].
json_emit_pairs([K-V]) --> "\"", { atom_chars(K, Kc) }, Kc, "\":", json_emit(V).
json_emit_pairs([K-V,P|Ps]) --> "\"", { atom_chars(K, Kc) }, Kc, "\":", json_emit(V), ",", json_emit_pairs([P|Ps]).

%% Emit array rest
json_emit_array_rest([]) --> [].
json_emit_array_rest([H|T]) --> ",", json_emit(H), json_emit_array_rest(T).

%% Emit string with escapes
json_emit_string(Atom) -->
    { atom_chars(Atom, Cs) },
    json_emit_string_chars(Cs).

json_emit_string_chars([]) --> [].
json_emit_string_chars([C|Cs]) -->
    { char_code(C, Code) },
    ( { Code = 34 } -> [B,Q], { char_code(B, 92), char_code(Q, 34) }  % \"
    ; { Code = 92 } -> [B,B2], { char_code(B, 92), char_code(B2, 92) }  % \\
    ; { Code = 10 } -> [B,N], { char_code(B, 92), char_code(N, 110) }  % \n
    ; { Code = 9 } -> [B,T], { char_code(B, 92), char_code(T, 116) }   % \t
    ; { Code = 13 } -> [B,R], { char_code(B, 92), char_code(R, 114) }  % \r
    ; [C]
    ),
    json_emit_string_chars(Cs).

%% ============================================================
%% CONVENIENCE PREDICATES
%% ============================================================

%% Get value from JSON object
json_get(json(Pairs), Key, Value) :-
    member(Key-Value, Pairs).

%% Parse JSON from atom
json_parse_atom(Atom, Term) :-
    atom_chars(Atom, Chars),
    json_parse(Chars, Term).

%% Write JSON to atom
json_write_atom(Term, Atom) :-
    json_write(Term, Chars),
    atom_chars(Atom, Chars).
