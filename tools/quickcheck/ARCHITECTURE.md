# Limn QuickCheck - Property-Based Testing for Scryer Prolog

> **tes qck | pur prl | fab gen**
> *Testing QuickCheck. Pure Prolog. Building generators.*

## The Vision

A complete property-based testing framework in pure Prolog. No FFI. No external libraries. No compromise.

## Architecture

```
quickcheck/
├── prng.pl           # Pseudo-random number generator
├── gen.pl            # Value generators (DCG-based)
├── shrink.pl         # Shrinking strategies
├── prop.pl           # Property testing core
├── quickcheck.pl     # Main interface
└── test/             # Self-tests (eating our own dogfood)
```

## Components

### 1. PRNG (prng.pl)

Pure Prolog pseudo-random number generator using Linear Congruential Generator (LCG).

```prolog
%% State-passing style for purity
%% random(Min, Max, Value, StateIn, StateOut)

:- use_module(library(clpz)).  % For large integer arithmetic

%% LCG parameters (same as glibc)
lcg_a(1103515245).
lcg_c(12345).
lcg_m(2147483648).  % 2^31

%% Generate next random state
next_random(State0, State1) :-
    lcg_a(A), lcg_c(C), lcg_m(M),
    State1 is (A * State0 + C) mod M.

%% Random integer in range [Min, Max]
random_int(Min, Max, Value, S0, S1) :-
    next_random(S0, S1),
    Range is Max - Min + 1,
    Value is Min + (S1 mod Range).

%% Initial seed from... we'll use a fixed seed or allow user to provide
seed(12345).
```

### 2. Generators (gen.pl)

DCG-based generators that thread random state.

```prolog
%% Generator type: gen(State0, State1, Value)
%% Represented as DCG over random state

%% Primitive generators
gen_int(Min, Max, Value) -->
    state(S0, S1),
    { random_int(Min, Max, Value, S0, S1) }.

gen_bool(B) -->
    gen_int(0, 1, N),
    { N = 0 -> B = false ; B = true }.

gen_element(List, Elem) -->
    { length(List, Len), Len > 0, MaxIdx is Len - 1 },
    gen_int(0, MaxIdx, Idx),
    { nth0(Idx, List, Elem) }.

%% Compound generators
gen_list(ElemGen, List) -->
    gen_int(0, 10, Len),
    gen_list_n(Len, ElemGen, List).

gen_list_n(0, _, []) --> [].
gen_list_n(N, ElemGen, [H|T]) -->
    { N > 0, N1 is N - 1 },
    call(ElemGen, H),
    gen_list_n(N1, ElemGen, T).

%% JSON generators (domain-specific)
gen_json_value(V) -->
    gen_int(0, 4, Choice),
    gen_json_by_choice(Choice, V).

gen_json_by_choice(0, null) --> [].
gen_json_by_choice(1, true) --> [].
gen_json_by_choice(2, false) --> [].
gen_json_by_choice(3, N) --> gen_int(-100, 100, N).
gen_json_by_choice(4, json(Pairs)) --> gen_json_object(Pairs).
```

### 3. Shrinking (shrink.pl)

When a property fails, find minimal counterexample.

```prolog
%% shrink(Value, SmallerValue)
%% Generates smaller versions of a value

shrink_int(N, S) :-
    N \= 0,
    S is N // 2.
shrink_int(N, S) :-
    N > 0, S is N - 1.
shrink_int(N, S) :-
    N < 0, S is N + 1.

shrink_list([], _) :- fail.
shrink_list([_|T], T).  % Remove head
shrink_list([H|T], [H|T2]) :-
    shrink_list(T, T2).  % Shrink tail
shrink_list([H|T], [H2|T]) :-
    shrink(H, H2).  % Shrink element

%% Generic shrink dispatcher
shrink(N, S) :- integer(N), shrink_int(N, S).
shrink(L, S) :- is_list(L), shrink_list(L, S).
shrink(json(P), json(S)) :- shrink_list(P, S).
```

### 4. Property Testing (prop.pl)

Core property testing logic.

```prolog
%% for_all(Generator, Property, NumTests, Seed, Result)
%% Result = passed | failed(CounterExample)

for_all(Gen, Prop, NumTests, Seed, Result) :-
    for_all_loop(Gen, Prop, NumTests, Seed, Result).

for_all_loop(_, _, 0, _, passed) :- !.
for_all_loop(Gen, Prop, N, S0, Result) :-
    N > 0,
    %% Generate a value
    phrase(Gen(Value), [S0], [S1]),
    %% Test property
    ( call(Prop, Value) ->
        N1 is N - 1,
        for_all_loop(Gen, Prop, N1, S1, Result)
    ;
        %% Property failed - shrink to find minimal counterexample
        shrink_to_minimal(Value, Prop, Minimal),
        Result = failed(Minimal)
    ).

shrink_to_minimal(Value, Prop, Minimal) :-
    ( shrink(Value, Smaller), \+ call(Prop, Smaller) ->
        shrink_to_minimal(Smaller, Prop, Minimal)
    ;
        Minimal = Value
    ).
```

### 5. Main Interface (quickcheck.pl)

User-friendly interface.

```prolog
%% quickcheck(Property) - test with default settings
%% quickcheck(Property, Options) - test with options

quickcheck(Prop) :-
    quickcheck(Prop, [tests(100), seed(12345)]).

quickcheck(Prop, Options) :-
    option(tests(N), Options, 100),
    option(seed(S), Options, 12345),
    %% Extract generator from property
    prop_generator(Prop, Gen),
    prop_check(Prop, Check),
    for_all(Gen, Check, N, S, Result),
    report_result(Prop, Result).

report_result(_, passed) :-
    write('✓ Property passed'), nl.
report_result(Prop, failed(Counter)) :-
    write('✗ Property FAILED'), nl,
    write('  Counterexample: '), write(Counter), nl.
```

## Sub-Tasks (Beads)

1. **limn-prng** - Implement PRNG in pure Prolog
   - LCG algorithm
   - State-passing interface
   - Tests for distribution

2. **limn-gen** - Implement generators
   - Primitives: int, bool, float, atom
   - Compounds: list, tuple, one_of
   - Combinators: map, filter, frequency
   - DCG-based for composability

3. **limn-shrink** - Implement shrinking
   - Shrink integers toward 0
   - Shrink lists by removal
   - Shrink atoms by truncation
   - Generic shrink dispatcher

4. **limn-prop** - Property testing core
   - for_all/5 implementation
   - Shrink-to-minimal loop
   - Statistics tracking

5. **limn-qc-api** - User-facing API
   - quickcheck/1, quickcheck/2
   - Property macros/syntax
   - Result reporting

6. **limn-qc-tests** - Self-tests
   - Test the generators generate valid values
   - Test shrinking finds smaller values
   - Test properties that should pass/fail

## Design Decisions (Mad Monk Style)

1. **Pure state-passing** - No assert/retract for random state
2. **DCG generators** - Elegant, composable, Prolog-native
3. **Minimal dependencies** - Only standard Scryer libraries
4. **Self-hosting** - Framework tests itself
5. **Limn integration** - Include Limn-specific generators

## Timeline

Each sub-task can be parallelized to different polecats:
- prng.pl - Can be built independently
- gen.pl - Needs prng.pl
- shrink.pl - Can be built independently
- prop.pl - Needs gen.pl, shrink.pl
- quickcheck.pl - Needs all above
- tests - Needs all above

## Usage Example

```prolog
%% Property: reversing a list twice gives original
prop_reverse_twice(List) :-
    reverse(List, Rev),
    reverse(Rev, List).

%% Test it
?- quickcheck(for_all(gen_list(gen_int(-100,100)), prop_reverse_twice)).
✓ Property passed (100 tests)

%% Property that fails: all lists are empty (obviously false)
prop_all_empty([]).

?- quickcheck(for_all(gen_list(gen_int(0,10)), prop_all_empty)).
✗ Property FAILED
  Counterexample: [0]
```

---

*The monk builds the tools to prove the tools.*
