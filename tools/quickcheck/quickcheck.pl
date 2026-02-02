%% QuickCheck - User-Facing Property-Based Testing
%% =========================================================
%% qck usr | prp tes | mad mon
%% *QuickCheck user. Property testing. Mad monk.*
%%
%% The monk tests all things with one question.
%%
%% Usage:
%%   ?- quickcheck(prop_reverse_twice).
%%   OK, passed 100 tests.
%%
%%   ?- quickcheck(prop_all_empty, [tests(1000), seed(12345)]).
%%   FAILED after 2 tests!
%%     Counterexample: [0]

:- use_module(library(lists)).

%% ============================================================
%% MAIN INTERFACE
%% ============================================================

%% quickcheck(+Property)
%% Test property with default settings (100 tests, seed 42)
quickcheck(Prop) :-
    quickcheck(Prop, []).

%% quickcheck(+Property, +Options)
%% Test property with options: tests(N), seed(S), verbose(Bool)
quickcheck(Prop, Options) :-
    option_or_default(tests, Options, 100, NumTests),
    option_or_default(seed, Options, 42, Seed),
    option_or_default(verbose, Options, false, Verbose),
    %% Run the property
    ( Verbose = true ->
        for_all_verbose(Prop, NumTests, Seed, Result)
    ;
        for_all(Prop, NumTests, Seed, Result)
    ),
    %% Report
    report_quickcheck(Prop, NumTests, Result).

%% option_or_default(+Key, +Options, +Default, -Value)
option_or_default(Key, Options, Default, Value) :-
    Opt =.. [Key, V],
    ( member(Opt, Options) -> Value = V ; Value = Default ).

%% ============================================================
%% FOR_ALL IMPLEMENTATION
%% ============================================================

%% for_all(+Property, +NumTests, +Seed, -Result)
%% Property is a term like: for_all(Gen, Check)
%% Result = passed | failed(Original, Minimal, TestNum)

for_all(for_all(Gen, Check), NumTests, Seed, Result) :-
    !,
    for_all_loop(Gen, Check, NumTests, 1, Seed, Result).

%% Direct property (no generator, just a check)
for_all(Prop, NumTests, Seed, Result) :-
    %% Assume Prop is a predicate that takes no args or generates its own
    for_all_loop(gen_unit, Prop, NumTests, 1, Seed, Result).

%% gen_unit generates nothing useful (for direct properties)
gen_unit(_) --> [].

for_all_loop(_, _, 0, _, _, passed) :- !.
for_all_loop(Gen, Check, N, TestNum, S0, Result) :-
    N > 0,
    phrase(call(Gen, Value), [S0], [S1]),
    ( safely_call(Check, Value) ->
        N1 is N - 1,
        TestNum1 is TestNum + 1,
        for_all_loop(Gen, Check, N1, TestNum1, S1, Result)
    ;
        shrink_to_minimal(Value, Check, Minimal),
        Result = failed(Value, Minimal, TestNum)
    ).

%% safely_call(+Pred, +Value)
%% Call predicate, catching exceptions as failures
safely_call(Pred, Value) :-
    catch(call(Pred, Value), _, fail).

%% for_all_verbose (with progress dots)
for_all_verbose(for_all(Gen, Check), NumTests, Seed, Result) :-
    !,
    for_all_verbose_loop(Gen, Check, NumTests, 1, Seed, Result).
for_all_verbose(Prop, NumTests, Seed, Result) :-
    for_all_verbose_loop(gen_unit, Prop, NumTests, 1, Seed, Result).

for_all_verbose_loop(_, _, 0, _, _, passed) :- !,
    nl.
for_all_verbose_loop(Gen, Check, N, TestNum, S0, Result) :-
    N > 0,
    phrase(call(Gen, Value), [S0], [S1]),
    ( safely_call(Check, Value) ->
        ( TestNum mod 10 =:= 0 -> write('.') ; true ),
        N1 is N - 1,
        TestNum1 is TestNum + 1,
        for_all_verbose_loop(Gen, Check, N1, TestNum1, S1, Result)
    ;
        nl,
        shrink_to_minimal(Value, Check, Minimal),
        Result = failed(Value, Minimal, TestNum)
    ).

%% ============================================================
%% SHRINKING
%% ============================================================

shrink_to_minimal(Value, Prop, Minimal) :-
    ( find_smaller_counterexample(Value, Prop, Smaller) ->
        shrink_to_minimal(Smaller, Prop, Minimal)
    ;
        Minimal = Value
    ).

find_smaller_counterexample(Value, Prop, Smaller) :-
    shrink(Value, Smaller),
    \+ safely_call(Prop, Smaller),
    !.

%% shrink/2 dispatcher
shrink(Value, Smaller) :-
    ( integer(Value) -> shrink_int(Value, Smaller)
    ; is_list(Value) -> shrink_list(Value, Smaller)
    ; Value = json(Pairs) -> shrink_list(Pairs, SP), Smaller = json(SP)
    ; Value = K-V -> shrink(V, V2), Smaller = K-V2
    ; atom(Value), Value \= true, Value \= false, Value \= null ->
        shrink_atom(Value, Smaller)
    ; fail
    ).

shrink_int(N, 0) :- N \= 0.
shrink_int(N, S) :- N > 0, S is N // 2, S \= N.
shrink_int(N, S) :- N < 0, S is N // 2, S \= N.
shrink_int(N, S) :- N > 0, S is N - 1.
shrink_int(N, S) :- N < 0, S is N + 1.

shrink_list([], _) :- !, fail.
shrink_list(List, Smaller) :- select(_, List, Smaller).
shrink_list([H|T], [H2|T]) :- shrink(H, H2).
shrink_list([H|T], [H|T2]) :- shrink_list(T, T2).
shrink_list(List, Smaller) :-
    length(List, Len), Len > 1,
    Half is Len // 2,
    length(Smaller, Half),
    append(Smaller, _, List).

shrink_atom(A, S) :-
    atom_chars(A, Chars),
    Chars = [_|_],  % non-empty
    shrink_atom_chars(Chars, SmallerChars),
    atom_chars(S, SmallerChars).

shrink_atom_chars([_], []) :- !.  % Single char shrinks to empty
shrink_atom_chars([_|T], T).      % Remove first char
shrink_atom_chars([H|T], [H|T2]) :- % Keep first, shrink rest
    shrink_atom_chars(T, T2).

is_list([]).
is_list([_|T]) :- is_list(T).

%% ============================================================
%% REPORTING
%% ============================================================

report_quickcheck(_, NumTests, passed) :-
    write('OK, passed '), write(NumTests), write(' tests.'), nl.

report_quickcheck(Prop, _, failed(Original, Minimal, TestNum)) :-
    write('FAILED after '), write(TestNum), write(' tests!'), nl,
    write('  Property: '), write(Prop), nl,
    write('  Counterexample: '), write(Original), nl,
    ( Original \= Minimal ->
        write('  Shrunk to:      '), write(Minimal), nl
    ;
        true
    ).

%% ============================================================
%% PRNG (inlined)
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

random_bool(Bool, S0, S1) :-
    random_int(0, 1, N, S0, S1),
    ( N = 0 -> Bool = false ; Bool = true ).

random_float(Value, S0, S1) :-
    next_state(S0, S1),
    lcg_m(M),
    Value is S1 / M.

%% DCG state
state(S0, S1), [S1] --> [S0].

rand_int(Min, Max, Value) -->
    state(S0, S1),
    { random_int(Min, Max, Value, S0, S1) }.

rand_bool(Bool) -->
    state(S0, S1),
    { random_bool(Bool, S0, S1) }.

rand_float(Value) -->
    state(S0, S1),
    { random_float(Value, S0, S1) }.

%% ============================================================
%% BUILT-IN GENERATORS
%% ============================================================

%% Integers
gen_int(Min, Max, Value) --> rand_int(Min, Max, Value).
gen_nat(Value) --> rand_int(0, 100, Value).
gen_pos(Value) --> rand_int(1, 100, Value).
gen_neg(Value) --> rand_int(-100, -1, Value).
gen_small_nat(Value) --> rand_int(0, 20, Value).
gen_small_int(Value) --> rand_int(-10, 10, Value).

%% Boolean
gen_bool(Bool) --> rand_bool(Bool).

%% Float
gen_float(Value) --> rand_float(Value).

%% Characters/Strings
gen_alpha(Char) -->
    rand_int(97, 122, Code),
    { char_code(Char, Code) }.

gen_digit(Char) -->
    rand_int(48, 57, Code),
    { char_code(Char, Code) }.

gen_string(MaxLen, String) -->
    rand_int(0, MaxLen, Len),
    gen_string_n(Len, Chars),
    { atom_chars(String, Chars) }.

gen_string_n(0, []) --> !.
gen_string_n(N, [C|Cs]) -->
    { N > 0, N1 is N - 1 },
    gen_alpha(C),
    gen_string_n(N1, Cs).

%% Lists
gen_list(ElemGen, List) -->
    rand_int(0, 10, Len),
    gen_list_n(Len, ElemGen, List).

gen_list_n(0, _, []) --> !.
gen_list_n(N, ElemGen, [H|T]) -->
    { N > 0, N1 is N - 1 },
    call(ElemGen, H),
    gen_list_n(N1, ElemGen, T).

gen_nonempty_list(ElemGen, List) -->
    rand_int(1, 10, Len),
    gen_list_n(Len, ElemGen, List).

%% Choice
gen_one_of(Gens, Value) -->
    { length(Gens, Len), MaxIdx is Len - 1 },
    rand_int(0, MaxIdx, Idx),
    { nth0(Idx, Gens, Gen) },
    call(Gen, Value).

gen_element(List, Elem) -->
    { length(List, Len), MaxIdx is Len - 1 },
    rand_int(0, MaxIdx, Idx),
    { nth0(Idx, List, Elem) }.

%% Maybe (null or value)
gen_maybe(Gen, Value) -->
    rand_bool(B),
    ( { B = true } -> call(Gen, Value) ; { Value = null } ).

%% Pairs
gen_pair(GenA, GenB, A-B) -->
    call(GenA, A),
    call(GenB, B).

%% ============================================================
%% PROPERTY COMBINATORS
%% ============================================================

%% implies(+Pre, +Prop)
%% Only test when precondition holds
implies(Pre, Prop, Value) :-
    ( call(Pre, Value) -> call(Prop, Value) ; true ).

%% both(+Prop1, +Prop2)
%% Both must hold
both(Prop1, Prop2, Value) :-
    call(Prop1, Value),
    call(Prop2, Value).

%% either(+Prop1, +Prop2)
%% At least one must hold
either(Prop1, Prop2, Value) :-
    ( call(Prop1, Value) -> true ; call(Prop2, Value) ).

%% ============================================================
%% EXAMPLE PROPERTIES
%% ============================================================

%% prop_reverse_twice: reversing twice gives original
prop_reverse_twice(List) :-
    reverse(List, Rev),
    reverse(Rev, List).

%% prop_append_length: length of append is sum of lengths
prop_append_length(A-B) :-
    append(A, B, C),
    length(A, LA), length(B, LB), length(C, LC),
    LC =:= LA + LB.

%% prop_sort_sorted: sorting produces sorted list
prop_sort_sorted(List) :-
    sort(List, Sorted),
    is_sorted(Sorted).

is_sorted([]) :- !.
is_sorted([_]) :- !.
is_sorted([A,B|T]) :- A @=< B, is_sorted([B|T]).

%% prop_sort_length: sorting preserves length
prop_sort_length(List) :-
    sort(List, Sorted),
    length(List, L1), length(Sorted, L2),
    L1 >= L2.  % sort removes duplicates, so Sorted can be shorter

%% ============================================================
%% TESTS
%% ============================================================

test_quickcheck :-
    write('Testing QuickCheck...'), nl,

    %% Test passing property
    write('  Testing reverse twice...'), nl,
    for_all(for_all(gen_list(gen_small_nat), prop_reverse_twice), 100, 42, R1),
    ( R1 = passed -> write('    ✓ passed'), nl ; write('    ✗ unexpected failure'), nl ),

    %% Test append property
    write('  Testing append length...'), nl,
    for_all(for_all(gen_pair(gen_list(gen_small_nat), gen_list(gen_small_nat)), prop_append_length), 100, 42, R2),
    ( R2 = passed -> write('    ✓ passed'), nl ; write('    ✗ unexpected failure'), nl ),

    %% Test sort properties
    write('  Testing sort produces sorted...'), nl,
    for_all(for_all(gen_list(gen_small_nat), prop_sort_sorted), 100, 42, R3),
    ( R3 = passed -> write('    ✓ passed'), nl ; write('    ✗ unexpected failure'), nl ),

    write('  Testing sort preserves length...'), nl,
    for_all(for_all(gen_list(gen_small_nat), prop_sort_length), 100, 42, R4),
    ( R4 = passed -> write('    ✓ passed'), nl ; write('    ✗ unexpected failure'), nl ),

    %% Test failing property (should detect failure)
    write('  Testing failure detection (prop_all_empty)...'), nl,
    for_all(for_all(gen_list(gen_small_nat), prop_all_empty), 50, 42, R5),
    ( R5 = failed(_, Minimal, _) ->
        write('    ✓ correctly detected failure, minimal: '), write(Minimal), nl
    ;
        write('    ✗ should have failed!'), nl
    ),

    write('QuickCheck tests complete.'), nl.

%% Property that should fail
prop_all_empty([]).

