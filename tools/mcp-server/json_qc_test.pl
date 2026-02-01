%% JSON QuickCheck Tests - Property-Based Testing
%% =========================================================
%% jso qck | rnd prp | exh tes
%% *JSON QuickCheck. Roundtrip properties. Exhaustive testing.*

:- use_module(library(lists)).

%% ============================================================
%% PRNG
%% ============================================================

lcg_next(S0, S1) :-
    S1 is (1103515245 * S0 + 12345) mod 2147483648.

rnd(Min, Max, V, S0, S1) :-
    lcg_next(S0, S1),
    Range is Max - Min + 1,
    V is Min + (S1 mod Range).

%% ============================================================
%% JSON GENERATORS
%% ============================================================

%% Generate random JSON (scalars only for safety)
%% Note: use conditional to work around Scryer indexing quirk
gen_json(S0, S1, V) :-
    rnd(0, 4, C, S0, S2),
    ( C =:= 0 -> gen_choice(0, S2, S1, V)
    ; C =:= 1 -> gen_choice(1, S2, S1, V)
    ; C =:= 2 -> gen_choice(2, S2, S1, V)
    ; C =:= 3 -> gen_choice(3, S2, S1, V)
    ; gen_choice(4, S2, S1, V)
    ).

gen_choice(0, S, S, null).
gen_choice(1, S, S, true).
gen_choice(2, S, S, false).
gen_choice(3, S0, S1, N) :- rnd(-100, 100, N, S0, S1).
gen_choice(4, S0, S1, Str) :- gen_str(S0, S1, Str).

gen_str(S0, S1, Str) :-
    rnd(0, 6, Len, S0, S2),
    gen_chars(Len, S2, S1, Cs),
    atom_chars(Str, Cs).

gen_chars(0, S, S, []) :- !.
gen_chars(N, S0, S2, [C|Cs]) :-
    N > 0,
    rnd(97, 122, Code, S0, S1),
    char_code(C, Code),
    N1 is N - 1,
    gen_chars(N1, S1, S2, Cs).

%% ============================================================
%% PROPERTY TESTS
%% ============================================================

%% Roundtrip: parse(emit(J)) = J
prop_roundtrip(J) :-
    json_write(J, Chars),
    json_parse(Chars, J2),
    J == J2.

%% ============================================================
%% TEST RUNNER
%% ============================================================

run_json_qc_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  JSON QUICKCHECK TESTS'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    %% Random property tests
    write('─── Random Property Tests ───'), nl,
    run_random_tests(500, 42),
    run_random_tests(500, 12345),
    run_random_tests(500, 99999),

    %% Specific value tests
    nl, write('─── Scalar Tests ───'), nl,
    test_val('null', null),
    test_val('true', true),
    test_val('false', false),
    test_val('zero', 0),
    test_val('pos int', 42),
    test_val('neg int', -100),
    test_val('empty str', ''),
    test_val('string', hello),

    nl, write('─── Structure Tests ───'), nl,
    test_val('empty obj', json([])),
    test_val('empty arr', []),
    test_val('simple obj', json([a-1, b-2])),
    test_val('simple arr', [1, 2, 3]),
    test_val('nested obj', json([x-json([y-42])])),
    test_val('nested arr', [[1,2], [3,4]]),
    test_val('mixed', json([nums-[1,2], flag-true])),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

run_random_tests(N, Seed) :-
    write('  '), write(N), write(' tests (seed '), write(Seed), write(')... '),
    run_loop(N, Seed, 0, Passed),
    ( Passed = N ->
        write('✓ all passed'), nl
    ;
        Failed is N - Passed,
        write('✗ '), write(Failed), write(' failed'), nl
    ).

run_loop(0, _, P, P) :- !.
run_loop(N, S0, Acc, Passed) :-
    N > 0,
    gen_json(S0, S1, V),
    ( prop_roundtrip(V) -> Acc1 is Acc + 1 ; Acc1 = Acc ),
    N1 is N - 1,
    run_loop(N1, S1, Acc1, Passed).

test_val(Name, V) :-
    write('  '), write(Name), write('... '),
    ( prop_roundtrip(V) -> write('✓') ; write('✗') ), nl.

%% ============================================================
%% EXTENDED GENERATORS
%% ============================================================

%% Generate JSON with objects
gen_json_obj(S0, S1, json(Pairs)) :-
    rnd(0, 3, Len, S0, S2),
    gen_pairs(Len, S2, S1, Pairs).

gen_pairs(0, S, S, []) :- !.
gen_pairs(N, S0, S3, [K-V|Rest]) :-
    N > 0,
    gen_key(S0, S1, K),
    gen_json(S1, S2, V),
    N1 is N - 1,
    gen_pairs(N1, S2, S3, Rest).

gen_key(S0, S1, K) :-
    rnd(1, 6, Len, S0, S2),
    gen_chars(Len, S2, S1, Cs),
    atom_chars(K, Cs).

%% Generate JSON with arrays
gen_json_arr(S0, S1, Arr) :-
    rnd(0, 5, Len, S0, S2),
    gen_arr_elems(Len, S2, S1, Arr).

gen_arr_elems(0, S, S, []) :- !.
gen_arr_elems(N, S0, S2, [V|Rest]) :-
    N > 0,
    gen_json(S0, S1, V),
    N1 is N - 1,
    gen_arr_elems(N1, S1, S2, Rest).

%% ============================================================
%% EXTENDED TEST RUNNER
%% ============================================================

run_extended_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  JSON EXTENDED TESTS'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    write('─── Object Tests ───'), nl,
    run_obj_tests(200, 42),
    run_obj_tests(200, 12345),

    nl, write('─── Array Tests ───'), nl,
    run_arr_tests(200, 42),
    run_arr_tests(200, 12345),

    nl, write('─── Combined Tests ───'), nl,
    run_random_tests(500, 777),
    run_random_tests(500, 888),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  EXTENDED TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

run_obj_tests(N, Seed) :-
    write('  '), write(N), write(' object tests (seed '), write(Seed), write(')... '),
    run_obj_loop(N, Seed, 0, Passed),
    ( Passed = N ->
        write('✓ all passed'), nl
    ;
        Failed is N - Passed,
        write('✗ '), write(Failed), write(' failed'), nl
    ).

run_obj_loop(0, _, P, P) :- !.
run_obj_loop(N, S0, Acc, Passed) :-
    N > 0,
    gen_json_obj(S0, S1, V),
    ( prop_roundtrip(V) -> Acc1 is Acc + 1 ; Acc1 = Acc ),
    N1 is N - 1,
    run_obj_loop(N1, S1, Acc1, Passed).

run_arr_tests(N, Seed) :-
    write('  '), write(N), write(' array tests (seed '), write(Seed), write(')... '),
    run_arr_loop(N, Seed, 0, Passed),
    ( Passed = N ->
        write('✓ all passed'), nl
    ;
        Failed is N - Passed,
        write('✗ '), write(Failed), write(' failed'), nl
    ).

run_arr_loop(0, _, P, P) :- !.
run_arr_loop(N, S0, Acc, Passed) :-
    N > 0,
    gen_json_arr(S0, S1, V),
    ( prop_roundtrip(V) -> Acc1 is Acc + 1 ; Acc1 = Acc ),
    N1 is N - 1,
    run_arr_loop(N1, S1, Acc1, Passed).

%% ============================================================
%% FULL TEST SUITE
%% ============================================================

run_all_tests :-
    run_json_qc_tests,
    nl,
    run_extended_tests.

%% Entry point
test_json_qc :- run_all_tests.

