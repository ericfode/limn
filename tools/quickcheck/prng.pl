%% PRNG - Pure Prolog Pseudo-Random Number Generator
%% =========================================================
%% prn pur | lcg alg | sta pas
%% *PRNG pure. LCG algorithm. State passing.*
%%
%% The monk generates chaos from order.
%%
%% Linear Congruential Generator (same parameters as glibc):
%%   X_{n+1} = (a * X_n + c) mod m
%%   a = 1103515245, c = 12345, m = 2^31

:- use_module(library(lists)).
:- use_module(library(between)).

%% ============================================================
%% LCG PARAMETERS
%% ============================================================

%% glibc LCG parameters (proven good distribution)
lcg_a(1103515245).
lcg_c(12345).
lcg_m(2147483648).  % 2^31

%% ============================================================
%% CORE PRNG
%% ============================================================

%% next_state(+State0, -State1)
%% Advance to next random state
next_state(S0, S1) :-
    lcg_a(A), lcg_c(C), lcg_m(M),
    S1 is (A * S0 + C) mod M.

%% random_raw(+State0, -State1, -Raw)
%% Get raw random value (0 to M-1) and new state
random_raw(S0, S1, Raw) :-
    next_state(S0, S1),
    Raw = S1.

%% ============================================================
%% INTEGER GENERATION
%% ============================================================

%% random_int(+Min, +Max, -Value, +State0, -State1)
%% Generate random integer in range [Min, Max] inclusive
random_int(Min, Max, Value, S0, S1) :-
    Min =< Max,
    random_raw(S0, S1, Raw),
    Range is Max - Min + 1,
    Value is Min + (Raw mod Range).

%% random_nat(+Max, -Value, +State0, -State1)
%% Generate random natural number in [0, Max]
random_nat(Max, Value, S0, S1) :-
    random_int(0, Max, Value, S0, S1).

%% ============================================================
%% BOOLEAN GENERATION
%% ============================================================

%% random_bool(-Bool, +State0, -State1)
%% Generate random boolean (true/false)
random_bool(Bool, S0, S1) :-
    random_int(0, 1, N, S0, S1),
    ( N = 0 -> Bool = false ; Bool = true ).

%% ============================================================
%% FLOAT GENERATION
%% ============================================================

%% random_float(-Value, +State0, -State1)
%% Generate random float in [0.0, 1.0)
random_float(Value, S0, S1) :-
    random_raw(S0, S1, Raw),
    lcg_m(M),
    Value is Raw / M.

%% random_float_range(+Min, +Max, -Value, +State0, -State1)
%% Generate random float in [Min, Max)
random_float_range(Min, Max, Value, S0, S1) :-
    random_float(F, S0, S1),
    Value is Min + F * (Max - Min).

%% ============================================================
%% LIST OPERATIONS
%% ============================================================

%% random_element(+List, -Element, +State0, -State1)
%% Pick random element from list
random_element(List, Element, S0, S1) :-
    List = [_|_],  % non-empty
    length(List, Len),
    MaxIdx is Len - 1,
    random_int(0, MaxIdx, Idx, S0, S1),
    nth0(Idx, List, Element).

%% random_shuffle(+List, -Shuffled, +State0, -State1)
%% Fisher-Yates shuffle
random_shuffle([], [], S, S) :- !.
random_shuffle(List, [H|T], S0, S2) :-
    random_element(List, H, S0, S1),
    select(H, List, Rest),
    random_shuffle(Rest, T, S1, S2).

%% ============================================================
%% SEQUENCE GENERATION
%% ============================================================

%% random_ints(+N, +Min, +Max, -List, +State0, -State1)
%% Generate list of N random integers
random_ints(0, _, _, [], S, S) :- !.
random_ints(N, Min, Max, [H|T], S0, S2) :-
    N > 0,
    random_int(Min, Max, H, S0, S1),
    N1 is N - 1,
    random_ints(N1, Min, Max, T, S1, S2).

%% ============================================================
%% SEED HANDLING
%% ============================================================

%% default_seed(-Seed)
%% Provide a default seed
default_seed(42).  % The answer

%% seed_from_list(+List, -Seed)
%% Create seed from list of integers (for reproducibility)
seed_from_list([], 0).
seed_from_list([H|T], Seed) :-
    seed_from_list(T, Rest),
    Seed is (H * 31 + Rest) mod 2147483648.

%% ============================================================
%% DCG INTERFACE (for generator compatibility)
%% ============================================================

%% State is represented as a single integer
%% DCG threads state through: phrase(Goal, [StateIn], [StateOut])

%% state(-S0, +S1)// - get current state, set new state
state(S0, S1), [S1] --> [S0].

%% rand_int(+Min, +Max, -Value)//
rand_int(Min, Max, Value) -->
    state(S0, S1),
    { random_int(Min, Max, Value, S0, S1) }.

%% rand_bool(-Bool)//
rand_bool(Bool) -->
    state(S0, S1),
    { random_bool(Bool, S0, S1) }.

%% rand_float(-Value)//
rand_float(Value) -->
    state(S0, S1),
    { random_float(Value, S0, S1) }.

%% rand_element(+List, -Elem)//
rand_element(List, Elem) -->
    state(S0, S1),
    { random_element(List, Elem, S0, S1) }.

%% ============================================================
%% TESTS
%% ============================================================

test_prng :-
    write('Testing PRNG...'), nl,

    %% Test determinism (same seed = same sequence)
    default_seed(Seed),
    random_int(1, 100, V1, Seed, S1),
    random_int(1, 100, V2, S1, S2),
    random_int(1, 100, V3, S2, _),

    random_int(1, 100, V1b, Seed, S1b),
    random_int(1, 100, V2b, S1b, S2b),
    random_int(1, 100, V3b, S2b, _),

    ( V1 = V1b, V2 = V2b, V3 = V3b ->
        write('  ✓ Determinism: same seed gives same sequence'), nl
    ;
        write('  ✗ FAIL: Non-deterministic!'), nl
    ),

    %% Test range
    random_ints(100, 1, 10, Ints, Seed, _),
    ( all_in_range(Ints, 1, 10) ->
        write('  ✓ Range: all values in [1,10]'), nl
    ;
        write('  ✗ FAIL: Values out of range!'), nl
    ),

    %% Test distribution (crude check)
    count_in_range(Ints, 1, 5, LowCount),
    count_in_range(Ints, 6, 10, HighCount),
    ( LowCount > 30, HighCount > 30 ->
        write('  ✓ Distribution: roughly uniform'), nl
    ;
        write('  ⚠ Distribution may be skewed'), nl
    ),

    %% Test DCG interface
    phrase(rand_int(1, 100, DcgV), [Seed], [_]),
    ( integer(DcgV), DcgV >= 1, DcgV =< 100 ->
        write('  ✓ DCG interface works'), nl
    ;
        write('  ✗ FAIL: DCG interface broken'), nl
    ),

    write('PRNG tests complete.'), nl.

count_in_range([], _, _, 0).
count_in_range([H|T], Min, Max, Count) :-
    count_in_range(T, Min, Max, Rest),
    ( H >= Min, H =< Max ->
        Count is Rest + 1
    ;
        Count = Rest
    ).

all_in_range([], _, _).
all_in_range([H|T], Min, Max) :-
    H >= Min, H =< Max,
    all_in_range(T, Min, Max).
