%% JSON Library Tests - Property-Based Without Randomness
%% =========================================================
%% tes jso | pro val | exh gen
%% *Testing JSON. Property validation. Exhaustive generation.*
%%
%% The monk proves correctness through exhaustion.
%%
%% Usage: scryer-prolog -g "consult('json_test.pl'), run_tests, halt"

:- use_module(library(lists)).
:- use_module(library(between)).

%% json.pl loaded via wrapper

%% ============================================================
%% TEST FRAMEWORK
%% ============================================================

:- dynamic(test_count/1).
:- dynamic(pass_count/1).
:- dynamic(fail_count/1).

reset_counts :-
    retractall(test_count(_)),
    retractall(pass_count(_)),
    retractall(fail_count(_)),
    assertz(test_count(0)),
    assertz(pass_count(0)),
    assertz(fail_count(0)).

inc_test :-
    retract(test_count(N)), N1 is N + 1, assertz(test_count(N1)).

inc_pass :-
    retract(pass_count(N)), N1 is N + 1, assertz(pass_count(N1)).

inc_fail :-
    retract(fail_count(N)), N1 is N + 1, assertz(fail_count(N1)).

assert_eq(Name, Got, Expected) :-
    inc_test,
    ( Got = Expected ->
        inc_pass
    ;
        inc_fail,
        write('FAIL: '), write(Name), nl,
        write('  Expected: '), write(Expected), nl,
        write('  Got: '), write(Got), nl
    ).

assert_true(Name, Goal) :-
    inc_test,
    ( call(Goal) ->
        inc_pass
    ;
        inc_fail,
        write('FAIL: '), write(Name), nl,
        write('  Goal failed: '), write(Goal), nl
    ).

%% ============================================================
%% UNIT TESTS - PARSING
%% ============================================================

test_parse_literals :-
    write('Testing literal parsing...'), nl,

    json_parse_atom('true', T1), assert_eq('parse true', T1, true),
    json_parse_atom('false', T2), assert_eq('parse false', T2, false),
    json_parse_atom('null', T3), assert_eq('parse null', T3, null).

test_parse_numbers :-
    write('Testing number parsing...'), nl,

    json_parse_atom('0', N1), assert_eq('parse 0', N1, 0),
    json_parse_atom('42', N2), assert_eq('parse 42', N2, 42),
    json_parse_atom('-17', N3), assert_eq('parse -17', N3, -17),
    json_parse_atom('3.14', N4), assert_eq('parse 3.14', N4, 3.14).

test_parse_strings :-
    write('Testing string parsing...'), nl,

    json_parse_atom('"hello"', S1), assert_eq('parse hello', S1, hello),
    json_parse_atom('""', S2), assert_eq('parse empty string', S2, ''),
    json_parse_atom('"with spaces"', S3), assert_eq('parse spaces', S3, 'with spaces').

test_parse_arrays :-
    write('Testing array parsing...'), nl,

    json_parse_atom('[]', A1), assert_eq('parse empty array', A1, []),
    json_parse_atom('[1]', A2), assert_eq('parse [1]', A2, [1]),
    json_parse_atom('[1,2,3]', A3), assert_eq('parse [1,2,3]', A3, [1,2,3]),
    json_parse_atom('[true,false]', A4), assert_eq('parse [true,false]', A4, [true,false]),
    json_parse_atom('["a","b"]', A5), assert_eq('parse ["a","b"]', A5, [a,b]).

test_parse_objects :-
    write('Testing object parsing...'), nl,

    json_parse_atom('{}', O1), assert_eq('parse empty object', O1, json([])),
    json_parse_atom('{"a":1}', O2), assert_eq('parse {"a":1}', O2, json([a-1])),
    json_parse_atom('{"x":true,"y":false}', O3),
    assert_eq('parse {"x":true,"y":false}', O3, json([x-true,y-false])).

test_parse_nested :-
    write('Testing nested parsing...'), nl,

    json_parse_atom('{"a":[1,2]}', N1),
    assert_eq('parse nested array', N1, json([a-[1,2]])),

    json_parse_atom('[{"x":1},{"y":2}]', N2),
    assert_eq('parse array of objects', N2, [json([x-1]),json([y-2])]),

    json_parse_atom('{"outer":{"inner":true}}', N3),
    assert_eq('parse nested objects', N3, json([outer-json([inner-true])])).

test_parse_whitespace :-
    write('Testing whitespace handling...'), nl,

    json_parse_atom('  true  ', W1), assert_eq('parse with spaces', W1, true),
    json_parse_atom('{ "a" : 1 }', W2), assert_eq('parse object with spaces', W2, json([a-1])),
    json_parse_atom('[ 1 , 2 ]', W3), assert_eq('parse array with spaces', W3, [1,2]).

%% ============================================================
%% UNIT TESTS - WRITING
%% ============================================================

test_write_literals :-
    write('Testing literal writing...'), nl,

    json_write_atom(true, L1), assert_eq('write true', L1, 'true'),
    json_write_atom(false, L2), assert_eq('write false', L2, 'false'),
    json_write_atom(null, L3), assert_eq('write null', L3, 'null').

test_write_numbers :-
    write('Testing number writing...'), nl,

    json_write_atom(0, N1), assert_eq('write 0', N1, '0'),
    json_write_atom(42, N2), assert_eq('write 42', N2, '42'),
    json_write_atom(-17, N3), assert_eq('write -17', N3, '-17').

test_write_strings :-
    write('Testing string writing...'), nl,

    json_write_atom(hello, S1), assert_eq('write hello', S1, '"hello"'),
    json_write_atom('', S2), assert_eq('write empty', S2, '""').

test_write_arrays :-
    write('Testing array writing...'), nl,

    json_write_atom([], A1), assert_eq('write []', A1, '[]'),
    json_write_atom([1,2,3], A2), assert_eq('write [1,2,3]', A2, '[1,2,3]').

test_write_objects :-
    write('Testing object writing...'), nl,

    json_write_atom(json([]), O1), assert_eq('write {}', O1, '{}'),
    json_write_atom(json([a-1]), O2), assert_eq('write {"a":1}', O2, '{"a":1}').

%% ============================================================
%% PROPERTY TESTS - ROUNDTRIP
%% ============================================================

%% Property: write then parse gives back original term
prop_roundtrip_write_parse(Term) :-
    json_write_atom(Term, Json),
    json_parse_atom(Json, Parsed),
    Term = Parsed.

%% Generate small JSON terms for exhaustive testing
gen_json_atom(true).
gen_json_atom(false).
gen_json_atom(null).

gen_json_number(N) :- between(0, 10, N).
gen_json_number(N) :- between(1, 5, X), N is -X.

gen_json_string(a).
gen_json_string(b).
gen_json_string(foo).
gen_json_string(bar).
gen_json_string('').

gen_json_simple(X) :- gen_json_atom(X).
gen_json_simple(X) :- gen_json_number(X).
gen_json_simple(X) :- gen_json_string(X).

gen_json_array_small([]).
gen_json_array_small([X]) :- gen_json_simple(X).
gen_json_array_small([X,Y]) :- gen_json_simple(X), gen_json_simple(Y).

gen_json_object_small(json([])).
gen_json_object_small(json([K-V])) :- gen_json_string(K), K \= '', gen_json_simple(V).

gen_json_term(X) :- gen_json_simple(X).
gen_json_term(X) :- gen_json_array_small(X).
gen_json_term(X) :- gen_json_object_small(X).

test_property_roundtrip :-
    write('Testing roundtrip property (write then parse)...'), nl,
    findall(T, gen_json_term(T), Terms),
    length(Terms, N),
    write('  Generated '), write(N), write(' test cases'), nl,
    test_roundtrip_terms(Terms).

test_roundtrip_terms([]).
test_roundtrip_terms([Term|Rest]) :-
    ( prop_roundtrip_write_parse(Term) ->
        inc_test, inc_pass
    ;
        inc_test, inc_fail,
        write('FAIL roundtrip: '), write(Term), nl
    ),
    test_roundtrip_terms(Rest).

%% ============================================================
%% RUN ALL TESTS
%% ============================================================

run_tests :-
    nl, write('▌ JSON LIBRARY TESTS ▐'), nl, nl,
    reset_counts,

    %% Unit tests
    test_parse_literals,
    test_parse_numbers,
    test_parse_strings,
    test_parse_arrays,
    test_parse_objects,
    test_parse_nested,
    test_parse_whitespace,

    test_write_literals,
    test_write_numbers,
    test_write_strings,
    test_write_arrays,
    test_write_objects,

    %% Property tests
    test_property_roundtrip,

    %% Report
    nl,
    test_count(Total),
    pass_count(Passed),
    fail_count(Failed),
    write('▌ RESULTS: '), write(Passed), write(' passed, '),
    write(Failed), write(' failed, '), write(Total), write(' total ▐'), nl,
    ( Failed = 0 ->
        write('✓ ALL TESTS PASSED'), nl
    ;
        write('✗ SOME TESTS FAILED'), nl
    ).
