%% Property Testing Core - for_all and Friends
%% =========================================================
%% prp tes | fal gen | shr min
%% *Property testing. Forall generation. Shrink minimal.*
%%
%% The monk tests ten thousand cases in one breath.

:- use_module(library(lists)).

%% ============================================================
%% CORE FOR_ALL
%% ============================================================

%% for_all(+Gen, +Prop, +NumTests, +Seed, -Result)
%% Run NumTests with generator, report Result
%% Result = passed(Count) | failed(CounterExample, Minimal, TestNum)
for_all(Gen, Prop, NumTests, Seed, Result) :-
    for_all_loop(Gen, Prop, NumTests, 1, Seed, Result).

%% for_all_loop(+Gen, +Prop, +Remaining, +TestNum, +State, -Result)
for_all_loop(_, _, 0, _, _, passed) :- !.
for_all_loop(Gen, Prop, N, TestNum, S0, Result) :-
    N > 0,
    %% Generate a value using DCG
    phrase(call(Gen, Value), [S0], [S1]),
    %% Test the property
    ( call(Prop, Value) ->
        %% Passed - continue
        N1 is N - 1,
        TestNum1 is TestNum + 1,
        for_all_loop(Gen, Prop, N1, TestNum1, S1, Result)
    ;
        %% Failed - shrink to minimal
        shrink_to_minimal(Value, Prop, Minimal),
        Result = failed(Value, Minimal, TestNum)
    ).

%% ============================================================
%% SHRINKING (imported from shrink.pl via wrapper)
%% ============================================================

%% shrink_to_minimal(+Value, +Prop, -Minimal)
%% Repeatedly shrink until no smaller counterexample exists
shrink_to_minimal(Value, Prop, Minimal) :-
    ( find_smaller_counterexample(Value, Prop, Smaller) ->
        shrink_to_minimal(Smaller, Prop, Minimal)
    ;
        Minimal = Value
    ).

%% find_smaller_counterexample(+Value, +Prop, -Smaller)
%% Find a shrink that still fails the property
find_smaller_counterexample(Value, Prop, Smaller) :-
    shrink(Value, Smaller),
    \+ call(Prop, Smaller),
    !.  % Take first one found

%% ============================================================
%% SHRINK DISPATCHER (inlined for standalone use)
%% ============================================================

%% shrink(+Value, -Smaller)
%% Generate smaller versions of a value (on backtracking)
shrink(Value, Smaller) :-
    ( integer(Value) ->
        shrink_int(Value, Smaller)
    ; is_list(Value) ->
        shrink_list(Value, Smaller)
    ; Value = json(Pairs) ->
        shrink_list(Pairs, SmallerPairs),
        Smaller = json(SmallerPairs)
    ; Value = K-V ->
        shrink(V, V2),
        Smaller = K-V2
    ; atom(Value), Value \= true, Value \= false, Value \= null ->
        shrink_atom(Value, Smaller)
    ;
        fail
    ).

%% Integer shrinking
shrink_int(N, 0) :- N \= 0.
shrink_int(N, S) :- N > 0, S is N // 2, S \= N.
shrink_int(N, S) :- N < 0, S is N // 2, S \= N.
shrink_int(N, S) :- N > 0, S is N - 1.
shrink_int(N, S) :- N < 0, S is N + 1.

%% List shrinking
shrink_list([], _) :- !, fail.
shrink_list(List, Smaller) :- select(_, List, Smaller).
shrink_list([H|T], [H2|T]) :- shrink(H, H2).
shrink_list([H|T], [H|T2]) :- shrink_list(T, T2).
shrink_list(List, Smaller) :-
    length(List, Len), Len > 1,
    Half is Len // 2,
    length(Smaller, Half),
    append(Smaller, _, List).

%% Atom shrinking
shrink_atom(A, S) :-
    atom_chars(A, Chars),
    Chars = [_|_],
    shrink_chars(Chars, SmallerChars),
    atom_chars(S, SmallerChars).

shrink_chars([_], []) :- !.
shrink_chars([_|T], T).
shrink_chars([H|T], [H|T2]) :- shrink_chars(T, T2).

is_list([]).
is_list([_|T]) :- is_list(T).

%% ============================================================
%% CONVENIENCE PREDICATES
%% ============================================================

%% for_all_verbose(+Gen, +Prop, +NumTests, +Seed, -Result)
%% Like for_all but prints progress
for_all_verbose(Gen, Prop, NumTests, Seed, Result) :-
    for_all_verbose_loop(Gen, Prop, NumTests, 1, Seed, Result).

for_all_verbose_loop(_, _, 0, _, _, passed) :- !,
    write('.'), nl.
for_all_verbose_loop(Gen, Prop, N, TestNum, S0, Result) :-
    N > 0,
    phrase(call(Gen, Value), [S0], [S1]),
    ( call(Prop, Value) ->
        ( TestNum mod 10 =:= 0 -> write('.') ; true ),
        N1 is N - 1,
        TestNum1 is TestNum + 1,
        for_all_verbose_loop(Gen, Prop, N1, TestNum1, S1, Result)
    ;
        nl,
        shrink_to_minimal(Value, Prop, Minimal),
        Result = failed(Value, Minimal, TestNum)
    ).

%% check(+Gen, +Prop)
%% Quick check with defaults (100 tests, seed 42)
check(Gen, Prop) :-
    check(Gen, Prop, 100).

%% check(+Gen, +Prop, +NumTests)
%% Check with custom test count
check(Gen, Prop, NumTests) :-
    for_all(Gen, Prop, NumTests, 42, Result),
    report_result(Result).

%% report_result(+Result)
report_result(passed) :-
    write('OK, passed all tests.'), nl.
report_result(failed(Original, Minimal, TestNum)) :-
    write('FAILED after '), write(TestNum), write(' tests!'), nl,
    write('  Original:    '), write(Original), nl,
    write('  Minimal:     '), write(Minimal), nl.

%% ============================================================
%% PROPERTY COMBINATORS
%% ============================================================

%% implies(+Precondition, +Property, +Value)
%% Property only tested when precondition holds
implies(Pre, Prop, Value) :-
    ( call(Pre, Value) ->
        call(Prop, Value)
    ;
        true  % Skip values that don't satisfy precondition
    ).

%% collect(+Classifier, +Property, +Value, -Stats)
%% Collect statistics about generated values
%% (Stats accumulated externally)
collect(Classifier, Prop, Value, Class) :-
    call(Classifier, Value, Class),
    call(Prop, Value).

%% conjoin(+Prop1, +Prop2, +Value)
%% Both properties must hold
conjoin(Prop1, Prop2, Value) :-
    call(Prop1, Value),
    call(Prop2, Value).

%% disjoin(+Prop1, +Prop2, +Value)
%% At least one property must hold
disjoin(Prop1, Prop2, Value) :-
    ( call(Prop1, Value) -> true ; call(Prop2, Value) ).

%% ============================================================
%% PRNG DCG (inlined for standalone use)
%% ============================================================

%% LCG parameters (glibc)
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

%% ============================================================
%% TESTS
%% ============================================================

test_prop :-
    write('Testing property core...'), nl,

    %% Test that a passing property passes
    for_all(gen_small_nat, is_non_negative, 20, 42, R1),
    ( R1 = passed ->
        write('  ✓ Passing property detected'), nl
    ;
        write('  ✗ False failure'), nl
    ),

    %% Test that a failing property fails
    for_all(gen_small_nat, is_less_than_5, 100, 42, R2),
    ( R2 = failed(_, Minimal2, _) ->
        write('  ✓ Failing property detected'), nl,
        write('    Minimal counterexample: '), write(Minimal2), nl,
        ( Minimal2 = 5 ->
            write('  ✓ Shrunk to exact boundary'), nl
        ;
            write('  ⚠ Shrinking found: '), write(Minimal2), nl
        )
    ;
        write('  ✗ Should have failed'), nl
    ),

    %% Test list property
    for_all(gen_small_list, list_not_too_long, 50, 42, R3),
    ( R3 = failed(_, Minimal3, _) ->
        write('  ✓ List property failed as expected'), nl,
        write('    Minimal list: '), write(Minimal3), nl
    ;
        write('  ✗ List property should have failed'), nl
    ),

    %% Test implies (conditional property)
    for_all(gen_small_int, implies(is_positive, is_non_negative), 50, 42, R4),
    ( R4 = passed ->
        write('  ✓ Implies combinator works'), nl
    ;
        write('  ✗ Implies combinator broken'), nl
    ),

    write('Property tests complete.'), nl.

%% Test generators (simplified, inlined)
gen_small_nat(Value) -->
    state(S0, S1),
    { random_int(0, 20, Value, S0, S1) }.

gen_small_int(Value) -->
    state(S0, S1),
    { random_int(-10, 10, Value, S0, S1) }.

gen_small_list(List) -->
    gen_small_list_len(Len),
    gen_small_list_elems(Len, List).

gen_small_list_len(Len) -->
    state(S0, S1),
    { random_int(0, 15, Len, S0, S1) }.

gen_small_list_elems(0, []) --> !.
gen_small_list_elems(N, [H|T]) -->
    { N > 0, N1 is N - 1 },
    state(S0, S1),
    { random_int(0, 10, H, S0, S1) },
    gen_small_list_elems(N1, T).

%% DCG state helper
state(S0, S1), [S1] --> [S0].

%% Test properties
is_non_negative(N) :- N >= 0.
is_less_than_5(N) :- N < 5.
is_positive(N) :- N > 0.
list_not_too_long(L) :- length(L, Len), Len < 8.

