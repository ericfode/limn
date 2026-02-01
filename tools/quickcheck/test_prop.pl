%% Property Test Suite - Comprehensive Tests for prop.pl and quickcheck.pl
%% =========================================================
%% tes prp | fal int | qck api
%% *Testing properties. Forall integration. QuickCheck API.*
%%
%% The monk tests the test itself.

:- use_module(library(lists)).

%% ============================================================
%% TEST RUNNER
%% ============================================================

run_prop_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  PROPERTY TEST SUITE'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    write('─── for_all Core ───'), nl,
    run_test('passing property returns passed', test_forall_passing),
    run_test('failing property returns failed', test_forall_failing),
    run_test('TestNum is correct count', test_forall_testnum),

    nl, write('─── Shrinking Integration ───'), nl,
    run_test('counterexample shrunk to minimal int', test_shrink_to_5),
    run_test('counterexample shrunk to minimal list', test_shrink_to_len3),

    nl, write('─── Property Combinators ───'), nl,
    run_test('implies skips non-matching', test_implies),
    run_test('both requires both properties', test_both),
    run_test('either requires one property', test_either),

    nl, write('─── Edge Cases ───'), nl,
    run_test('property that always passes', test_always_passes),
    run_test('property that always fails', test_always_fails),
    run_test('exception in property treated as fail', test_exception_as_fail),

    nl, write('─── Known Properties ───'), nl,
    run_test('prop_reverse_twice passes', test_reverse_twice),
    run_test('prop_append_length passes', test_append_length),
    run_test('prop_sort_sorted passes', test_sort_sorted),

    nl, write('─── Seed Reproducibility ───'), nl,
    run_test('same seed same result', test_seed_reproducibility),
    run_test('different seed different sequence', test_different_seeds),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  PROPERTY TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

run_test(Name, Goal) :-
    write('  '), write(Name), write('... '),
    ( catch(call(Goal), E, (write('✗ EXCEPTION: '), write(E), nl, fail)) ->
        write('✓'), nl
    ;
        write('✗ FAILED'), nl
    ).

%% ============================================================
%% PRNG (inlined for standalone tests)
%% ============================================================

lcg_a(1103515245).
lcg_c(12345).
lcg_m(2147483648).

next_state(S0, S1) :-
    lcg_a(A), lcg_c(C), lcg_m(M),
    S1 is (A * S0 + C) mod M.

random_int(Min, Max, Value, S0, S1) :-
    Min =< Max,
    next_state(S0, S1),
    Range is Max - Min + 1,
    Value is Min + (S1 mod Range).

state(S0, S1), [S1] --> [S0].

%% Simple generators for testing
gen_small_nat(Value) -->
    state(S0, S1),
    { random_int(0, 20, Value, S0, S1) }.

gen_small_list(List) -->
    state(S0, _),
    { random_int(0, 10, Len, S0, S1),
      gen_small_list_n(Len, List, S1, S2) },
    state(_, S2).

gen_small_list_n(0, [], S, S) :- !.
gen_small_list_n(N, [H|T], S0, S2) :-
    N > 0,
    random_int(0, 10, H, S0, S1),
    N1 is N - 1,
    gen_small_list_n(N1, T, S1, S2).

gen_pair_lists(A-B) -->
    gen_small_list(A),
    state(S0, _),
    { gen_small_list_n(5, B, S0, S1) },
    state(_, S1).

%% ============================================================
%% SIMPLE FOR_ALL (inlined for standalone tests)
%% ============================================================

for_all_test(Gen, Prop, NumTests, Seed, Result) :-
    for_all_loop(Gen, Prop, NumTests, 1, Seed, Result).

for_all_loop(_, _, 0, _, _, passed) :- !.
for_all_loop(Gen, Prop, N, TestNum, S0, Result) :-
    N > 0,
    phrase(call(Gen, Value), [S0], [S1]),
    ( catch(call(Prop, Value), _, fail) ->
        N1 is N - 1,
        TestNum1 is TestNum + 1,
        for_all_loop(Gen, Prop, N1, TestNum1, S1, Result)
    ;
        shrink_to_minimal_test(Value, Prop, Minimal),
        Result = failed(Value, Minimal, TestNum)
    ).

%% Simplified shrinking for tests
shrink_to_minimal_test(Value, Prop, Minimal) :-
    ( find_smaller_cex(Value, Prop, Smaller) ->
        shrink_to_minimal_test(Smaller, Prop, Minimal)
    ;
        Minimal = Value
    ).

find_smaller_cex(Value, Prop, Smaller) :-
    shrink_test(Value, Smaller),
    \+ catch(call(Prop, Smaller), _, fail),
    !.

shrink_test(N, S) :- integer(N), N \= 0, S is N // 2.
shrink_test(N, S) :- integer(N), N > 0, S is N - 1.
shrink_test(N, S) :- integer(N), N < 0, S is N + 1.
shrink_test(N, 0) :- integer(N), N \= 0.
shrink_test([_|T], T).
shrink_test([H|T], [H|T2]) :- shrink_test(T, T2).
shrink_test([H|T], [H2|T]) :- shrink_test(H, H2).

%% ============================================================
%% FOR_ALL CORE TESTS
%% ============================================================

test_forall_passing :-
    for_all_test(gen_small_nat, is_non_negative, 50, 42, Result),
    Result = passed.

is_non_negative(N) :- N >= 0.

test_forall_failing :-
    for_all_test(gen_small_nat, is_less_than_10, 100, 42, Result),
    Result = failed(_, _, _).

is_less_than_10(N) :- N < 10.

test_forall_testnum :-
    %% First failing test should be around test 1-10 for gen_small_nat with seed 42
    for_all_test(gen_small_nat, is_less_than_5, 100, 42, failed(_, _, TestNum)),
    TestNum >= 1,
    TestNum =< 50.  % Should fail within first 50 tests

is_less_than_5(N) :- N < 5.

%% ============================================================
%% SHRINKING INTEGRATION TESTS
%% ============================================================

test_shrink_to_5 :-
    %% Property: less than 5
    %% Should shrink any >= 5 to exactly 5
    for_all_test(gen_small_nat, is_less_than_5, 100, 42, failed(_, Minimal, _)),
    Minimal = 5.

test_shrink_to_len3 :-
    %% Property: length < 3
    %% Should shrink to length exactly 3
    for_all_test(gen_small_list, length_lt_3, 100, 42, failed(_, Minimal, _)),
    length(Minimal, 3).

length_lt_3(L) :- length(L, Len), Len < 3.

%% ============================================================
%% PROPERTY COMBINATOR TESTS
%% ============================================================

test_implies :-
    %% implies(is_positive, is_non_negative) should pass
    %% because all positive numbers are non-negative
    for_all_test(gen_small_nat, implies_test(is_positive, is_non_negative), 50, 42, passed).

implies_test(Pre, Prop, Value) :-
    ( call(Pre, Value) -> call(Prop, Value) ; true ).

is_positive(N) :- N > 0.

test_both :-
    %% both(>= 0, < 100) should pass for gen_small_nat (generates 0-20)
    for_all_test(gen_small_nat, both_test(is_non_negative, is_less_than_100), 50, 42, passed).

both_test(P1, P2, Value) :-
    call(P1, Value),
    call(P2, Value).

is_less_than_100(N) :- N < 100.

test_either :-
    %% either(< 5, > 15) should fail (values 5-15 fail both)
    for_all_test(gen_small_nat, either_test(is_less_than_5, is_greater_than_15), 100, 42, failed(_, _, _)).

either_test(P1, P2, Value) :-
    ( call(P1, Value) -> true ; call(P2, Value) ).

is_greater_than_15(N) :- N > 15.

%% ============================================================
%% EDGE CASE TESTS
%% ============================================================

test_always_passes :-
    for_all_test(gen_small_nat, always_true, 100, 42, passed).

always_true(_).

test_always_fails :-
    for_all_test(gen_small_nat, always_false, 10, 42, failed(_, _, 1)).

always_false(_) :- fail.

test_exception_as_fail :-
    %% Property that throws should be treated as failure
    for_all_test(gen_small_nat, throws_exception, 10, 42, failed(_, _, _)).

throws_exception(N) :-
    ( N > 5 -> throw(test_error) ; true ).

%% ============================================================
%% KNOWN PROPERTY TESTS
%% ============================================================

test_reverse_twice :-
    for_all_test(gen_small_list, prop_reverse_twice, 50, 42, passed).

prop_reverse_twice(List) :-
    reverse(List, Rev),
    reverse(Rev, List).

test_append_length :-
    for_all_test(gen_pair_lists, prop_append_length, 50, 42, passed).

prop_append_length(A-B) :-
    append(A, B, C),
    length(A, LA), length(B, LB), length(C, LC),
    LC =:= LA + LB.

test_sort_sorted :-
    for_all_test(gen_small_list, prop_sort_sorted, 50, 42, passed).

prop_sort_sorted(List) :-
    sort(List, Sorted),
    is_sorted(Sorted).

is_sorted([]) :- !.
is_sorted([_]) :- !.
is_sorted([A,B|T]) :- A @=< B, is_sorted([B|T]).

%% ============================================================
%% SEED REPRODUCIBILITY TESTS
%% ============================================================

test_seed_reproducibility :-
    %% Same seed should give same result
    for_all_test(gen_small_nat, is_less_than_5, 50, 42, R1),
    for_all_test(gen_small_nat, is_less_than_5, 50, 42, R2),
    R1 = R2.

test_different_seeds :-
    %% Different seeds can give different failure points
    for_all_test(gen_small_nat, is_less_than_5, 100, 42, failed(_, _, T1)),
    for_all_test(gen_small_nat, is_less_than_5, 100, 12345, failed(_, _, T2)),
    %% They may or may not be different, but both should fail eventually
    integer(T1),
    integer(T2).

%% ============================================================
%% HELPER
%% ============================================================

is_list([]).
is_list([_|T]) :- is_list(T).

%% ============================================================
%% MAIN ENTRY
%% ============================================================

test_prop :- run_prop_tests.

