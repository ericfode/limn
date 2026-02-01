%% Shrinking Test Suite - Comprehensive Tests for shrink.pl
%% =========================================================
%% tes shr | min cex | pur log
%% *Testing shrinking. Minimal counterexamples. Pure logic.*
%%
%% The monk tests the path to smallest truth.

:- use_module(library(lists)).

%% ============================================================
%% TEST RUNNER
%% ============================================================

run_shrink_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  SHRINKING TEST SUITE'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    write('─── Integer Shrinking ───'), nl,
    run_test('shrink_int(10) produces 0, 5, 9', test_shrink_int_positive),
    run_test('shrink_int(-10) produces 0, -5, -9', test_shrink_int_negative),
    run_test('shrink_int(0) fails (no smaller)', test_shrink_int_zero),
    run_test('shrink_int(1) produces only 0', test_shrink_int_one),

    nl, write('─── List Shrinking ───'), nl,
    run_test('shrink_list removes elements', test_shrink_list_removal),
    run_test('shrink_list shrinks elements', test_shrink_list_element),
    run_test('shrink_list([]) fails', test_shrink_list_empty),
    run_test('shrink_list half-list works', test_shrink_list_half),

    nl, write('─── Atom Shrinking ───'), nl,
    run_test('shrink_atom(abc) produces bc, c, \'\'', test_shrink_atom),
    run_test('shrink_atom(\'\') fails', test_shrink_atom_empty),

    nl, write('─── Pair Shrinking ───'), nl,
    run_test('shrink pair shrinks value', test_shrink_pair),

    nl, write('─── JSON Shrinking ───'), nl,
    run_test('shrink json shrinks pairs', test_shrink_json),
    run_test('shrink json value propagates', test_shrink_json_value),

    nl, write('─── Shrink-to-Minimal ───'), nl,
    run_test('shrink_to_minimal finds smallest int', test_minimal_int),
    run_test('shrink_to_minimal finds shortest list', test_minimal_list),
    run_test('shrink_to_minimal terminates on irreducible', test_minimal_terminates),

    nl, write('─── Shrink Ordering ───'), nl,
    run_test('shrinks are smaller (integers)', test_shrink_smaller_int),
    run_test('shrinks are smaller (lists)', test_shrink_smaller_list),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  SHRINKING TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

run_test(Name, Goal) :-
    write('  '), write(Name), write('... '),
    ( catch(call(Goal), E, (write('✗ EXCEPTION: '), write(E), nl, fail)) ->
        write('✓'), nl
    ;
        write('✗ FAILED'), nl
    ).

%% ============================================================
%% INTEGER SHRINKING TESTS
%% ============================================================

test_shrink_int_positive :-
    findall(S, shrink(10, S), Shrinks),
    member(0, Shrinks),
    member(5, Shrinks),
    member(9, Shrinks).

test_shrink_int_negative :-
    findall(S, shrink(-10, S), Shrinks),
    member(0, Shrinks),
    member(-5, Shrinks),
    member(-9, Shrinks).

test_shrink_int_zero :-
    \+ shrink(0, _).

test_shrink_int_one :-
    findall(S, shrink(1, S), Shrinks),
    sort(Shrinks, Unique),  % Remove duplicates
    Unique = [0].

%% ============================================================
%% LIST SHRINKING TESTS
%% ============================================================

test_shrink_list_removal :-
    findall(S, shrink([1,2,3], S), Shrinks),
    member([2,3], Shrinks),  % Remove first
    member([1,3], Shrinks),  % Remove middle
    member([1,2], Shrinks).  % Remove last

test_shrink_list_element :-
    findall(S, shrink([10], S), Shrinks),
    member([], Shrinks),     % Remove element
    member([0], Shrinks),    % Shrink element to 0
    member([5], Shrinks),    % Shrink element to 5
    member([9], Shrinks).    % Shrink element to 9

test_shrink_list_empty :-
    \+ shrink([], _).

test_shrink_list_half :-
    findall(S, shrink([1,2,3,4], S), Shrinks),
    member([1,2], Shrinks).  % Half-list shrink

%% ============================================================
%% ATOM SHRINKING TESTS
%% ============================================================

test_shrink_atom :-
    findall(S, shrink(abc, S), Shrinks),
    member(bc, Shrinks),   % Remove first char
    member(ac, Shrinks),   % Remove middle char (via recursive shrinking)
    %% Eventually reaches empty
    shrink_to_minimal(abc, never_true, Min),
    Min = ''.

never_true(_) :- fail.

test_shrink_atom_empty :-
    \+ shrink('', _).

%% ============================================================
%% PAIR SHRINKING TESTS
%% ============================================================

test_shrink_pair :-
    findall(S, shrink(key-10, S), Shrinks),
    member(key-0, Shrinks),
    member(key-5, Shrinks),
    member(key-9, Shrinks).

%% ============================================================
%% JSON SHRINKING TESTS
%% ============================================================

test_shrink_json :-
    findall(S, shrink(json([a-1, b-2]), S), Shrinks),
    %% Should produce versions with fewer pairs
    member(json([b-2]), Shrinks),
    member(json([a-1]), Shrinks).

test_shrink_json_value :-
    findall(S, shrink(json([x-10]), S), Shrinks),
    %% Should shrink the value
    member(json([x-0]), Shrinks),
    member(json([x-5]), Shrinks).

%% ============================================================
%% SHRINK-TO-MINIMAL TESTS
%% ============================================================

test_minimal_int :-
    %% Property: less than 5
    %% Counterexample 100 should shrink to 5
    shrink_to_minimal(100, less_than_5, Minimal),
    Minimal = 5.

less_than_5(N) :- N < 5.

test_minimal_list :-
    %% Property: length < 3
    %% Counterexample [1,2,3,4,5] should shrink to [_,_,_]
    shrink_to_minimal([1,2,3,4,5], length_lt_3, Minimal),
    length(Minimal, 3).

length_lt_3(L) :- length(L, Len), Len < 3.

test_minimal_terminates :-
    %% Should terminate even when no shrink satisfies property
    shrink_to_minimal(0, always_false, Minimal),
    Minimal = 0.

always_false(_) :- fail.

%% ============================================================
%% SHRINK ORDERING TESTS
%% ============================================================

test_shrink_smaller_int :-
    %% All shrinks of a positive int should be smaller or equal to it
    findall(S, shrink(50, S), Shrinks),
    all_satisfy(Shrinks, less_than_or_eq(50)),
    all_satisfy(Shrinks, different_from(50)).

less_than_or_eq(N, S) :- abs(S) =< abs(N).
different_from(N, S) :- S \= N.

all_satisfy([], _).
all_satisfy([H|T], Pred) :- call(Pred, H), all_satisfy(T, Pred).

test_shrink_smaller_list :-
    %% All shrinks should be "smaller" (shorter or with smaller elements)
    findall(S, shrink([10,20,30], S), Shrinks),
    all_satisfy(Shrinks, smaller_list([10,20,30])).

smaller_list(Orig, Shrunk) :-
    length(Orig, LO), length(Shrunk, LS),
    ( LS < LO -> true
    ; LS = LO, lexically_smaller(Shrunk, Orig)
    ).

lexically_smaller([], [_|_]).
lexically_smaller([H1|_], [H2|_]) :- H1 < H2, !.
lexically_smaller([H|T1], [H|T2]) :- lexically_smaller(T1, T2).

%% ============================================================
%% HELPER: is_list
%% ============================================================

is_list([]).
is_list([_|T]) :- is_list(T).

%% ============================================================
%% MAIN ENTRY
%% ============================================================

test_shrink :- run_shrink_tests.

