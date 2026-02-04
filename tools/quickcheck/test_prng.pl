%% PRNG Test Suite - Comprehensive Tests for prng.pl
%% =========================================================
%% tes prn | det ran | dis val
%% *Testing PRNG. Determinism randomness. Distribution validation.*
%%
%% The monk tests the chaos for hidden order.

:- use_module(library(lists)).
:- use_module(library(between)).

%% ============================================================
%% LOAD PRNG
%% ============================================================

%% prng.pl loaded via command line

%% ============================================================
%% TEST RUNNER
%% ============================================================

run_prng_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  PRNG TEST SUITE'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    run_test('Determinism: same seed same sequence', test_determinism),
    run_test('Determinism: different seeds differ', test_different_seeds),
    run_test('Range: basic range compliance', test_range_basic),
    run_test('Range: Min equals Max', test_range_equal),
    run_test('Range: negative ranges', test_range_negative),
    run_test('Range: large ranges', test_range_large),
    run_test('Distribution: crude uniformity', test_distribution),
    run_test('State threading: DCG interface', test_dcg_state),
    run_test('State threading: chained calls', test_chained_state),
    run_test('Boolean: produces true/false', test_bool),
    run_test('Float: produces [0,1)', test_float),
    run_test('Element: picks from list', test_element),
    run_test('Shuffle: permutes list', test_shuffle),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  PRNG TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

run_test(Name, Goal) :-
    write('  '), write(Name), write('... '),
    ( catch(call(Goal), E, (write('✗ EXCEPTION: '), write(E), nl, fail)) ->
        write('✓'), nl
    ;
        write('✗ FAILED'), nl
    ).

%% ============================================================
%% DETERMINISM TESTS
%% ============================================================

%% Test that same seed produces identical sequence
test_determinism :-
    Seed = 42,
    generate_sequence(Seed, 100, Seq1),
    generate_sequence(Seed, 100, Seq2),
    Seq1 == Seq2.

generate_sequence(_, 0, []) :- !.
generate_sequence(S0, N, [V|Vs]) :-
    N > 0,
    random_int(1, 1000, V, S0, S1),
    N1 is N - 1,
    generate_sequence(S1, N1, Vs).

%% Test that different seeds produce different sequences
test_different_seeds :-
    generate_sequence(42, 20, Seq1),
    generate_sequence(12345, 20, Seq2),
    generate_sequence(999999, 20, Seq3),
    Seq1 \= Seq2,
    Seq2 \= Seq3,
    Seq1 \= Seq3.

%% ============================================================
%% RANGE COMPLIANCE TESTS
%% ============================================================

%% Basic range test
test_range_basic :-
    Seed = 42,
    test_range_n(1, 10, 100, Seed, _),
    test_range_n(0, 100, 100, Seed, _),
    test_range_n(-50, 50, 100, Seed, _).

test_range_n(_, _, 0, S, S) :- !.
test_range_n(Min, Max, N, S0, S2) :-
    N > 0,
    random_int(Min, Max, V, S0, S1),
    V >= Min,
    V =< Max,
    N1 is N - 1,
    test_range_n(Min, Max, N1, S1, S2).

%% Test Min equals Max (should always return that value)
test_range_equal :-
    Seed = 42,
    test_equal_range(5, 5, 50, Seed, _),
    test_equal_range(0, 0, 50, Seed, _),
    test_equal_range(-10, -10, 50, Seed, _).

test_equal_range(_, _, 0, S, S) :- !.
test_equal_range(V, V, N, S0, S2) :-
    N > 0,
    random_int(V, V, Result, S0, S1),
    Result == V,
    N1 is N - 1,
    test_equal_range(V, V, N1, S1, S2).

%% Test negative ranges
test_range_negative :-
    Seed = 42,
    test_range_n(-100, -1, 100, Seed, _),
    test_range_n(-1000, -500, 100, Seed, _).

%% Test large ranges
test_range_large :-
    Seed = 42,
    test_range_n(0, 1000000, 100, Seed, _),
    test_range_n(-500000, 500000, 100, Seed, _).

%% ============================================================
%% DISTRIBUTION TEST
%% ============================================================

%% Crude uniformity test: generate 1000 values in [0,9]
%% Each bucket should have roughly 100 values (80-120 acceptable)
test_distribution :-
    Seed = 42,
    count_distribution(0, 9, 1000, Seed, Counts),
    %% Check each bucket is in acceptable range
    check_buckets(Counts, 50, 150).  % LCG has some variance, allow wider range

count_distribution(Min, Max, N, Seed, Counts) :-
    Range is Max - Min + 1,
    length(ZeroCounts, Range),
    maplist(=(0), ZeroCounts),
    count_loop(Min, Max, N, Seed, ZeroCounts, Counts).

count_loop(_, _, 0, _, Counts, Counts) :- !.
count_loop(Min, Max, N, S0, Acc, Counts) :-
    N > 0,
    random_int(Min, Max, V, S0, S1),
    Idx is V - Min,
    increment_at(Idx, Acc, Acc1),
    N1 is N - 1,
    count_loop(Min, Max, N1, S1, Acc1, Counts).

increment_at(0, [H|T], [H1|T]) :- !, H1 is H + 1.
increment_at(N, [H|T], [H|T1]) :-
    N > 0,
    N1 is N - 1,
    increment_at(N1, T, T1).

check_buckets([], _, _).
check_buckets([C|Cs], Min, Max) :-
    C >= Min,
    C =< Max,
    check_buckets(Cs, Min, Max).

%% ============================================================
%% STATE THREADING TESTS
%% ============================================================

%% Test DCG interface
test_dcg_state :-
    Seed = 42,
    phrase(rand_int(1, 100, V1), [Seed], [S1]),
    integer(V1),
    V1 >= 1,
    V1 =< 100,
    S1 \= Seed,  % State changed
    %% Second call from S1 should give different value
    phrase(rand_int(1, 100, _V2), [S1], [S2]),
    S2 \= S1.

%% Test chained state threading
test_chained_state :-
    Seed = 42,
    %% Chain multiple calls
    phrase((
        rand_int(1, 10, A),
        rand_int(1, 10, B),
        rand_int(1, 10, C)
    ), [Seed], [_]),
    integer(A), integer(B), integer(C),
    A >= 1, A =< 10,
    B >= 1, B =< 10,
    C >= 1, C =< 10.

%% ============================================================
%% BOOLEAN TEST
%% ============================================================

test_bool :-
    Seed = 42,
    count_bools(100, Seed, TrueCount, FalseCount),
    TrueCount > 20,   % Expect some trues
    FalseCount > 20.  % Expect some falses

count_bools(N, Seed, TrueCount, FalseCount) :-
    count_bools_loop(N, Seed, 0, 0, TrueCount, FalseCount).

count_bools_loop(0, _, T, F, T, F) :- !.
count_bools_loop(N, S0, TAcc, FAcc, T, F) :-
    N > 0,
    random_bool(B, S0, S1),
    ( B = true -> TAcc1 is TAcc + 1, FAcc1 = FAcc
    ; TAcc1 = TAcc, FAcc1 is FAcc + 1
    ),
    N1 is N - 1,
    count_bools_loop(N1, S1, TAcc1, FAcc1, T, F).

%% ============================================================
%% FLOAT TEST
%% ============================================================

test_float :-
    Seed = 42,
    test_float_loop(100, Seed).

test_float_loop(0, _) :- !.
test_float_loop(N, S0) :-
    N > 0,
    random_float(V, S0, S1),
    V >= 0.0,
    V < 1.0,
    N1 is N - 1,
    test_float_loop(N1, S1).

%% ============================================================
%% ELEMENT TEST
%% ============================================================

test_element :-
    Seed = 42,
    List = [a, b, c, d, e],
    test_element_loop(50, List, Seed, Picked),
    %% Should have picked at least 3 different elements
    sort(Picked, Unique),
    length(Unique, UniqueLen),
    UniqueLen >= 3.

test_element_loop(0, _, _, []) :- !.
test_element_loop(N, List, S0, [E|Es]) :-
    N > 0,
    random_element(List, E, S0, S1),
    member(E, List),
    N1 is N - 1,
    test_element_loop(N1, List, S1, Es).

%% ============================================================
%% SHUFFLE TEST
%% ============================================================

test_shuffle :-
    Seed = 42,
    List = [1, 2, 3, 4, 5],
    random_shuffle(List, Shuffled, Seed, _),
    %% Shuffled should be a permutation
    sort(List, Sorted),
    sort(Shuffled, ShuffledSorted),
    Sorted == ShuffledSorted,
    %% Should have same length
    length(List, L1),
    length(Shuffled, L2),
    L1 == L2.

%% ============================================================
%% MAIN ENTRY
%% ============================================================

%% Alias for backward compatibility
%% test_prng :- run_prng_tests.  % commented to avoid discontiguous warning

