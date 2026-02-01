%% Generators - DCG-Based Value Generation
%% =========================================================
%% gen dcg | com val | pur ran
%% *Generators DCG. Composable values. Pure randomness.*
%%
%% The monk generates worlds from seeds.

:- use_module(library(lists)).

%% prng.pl loaded via wrapper

%% ============================================================
%% PRIMITIVE GENERATORS
%% ============================================================

%% gen_int(+Min, +Max, -Value)//
%% Generate random integer in [Min, Max]
gen_int(Min, Max, Value) -->
    rand_int(Min, Max, Value).

%% gen_nat(-Value)//
%% Generate small natural number [0, 100]
gen_nat(Value) -->
    rand_int(0, 100, Value).

%% gen_pos(-Value)//
%% Generate positive integer [1, 100]
gen_pos(Value) -->
    rand_int(1, 100, Value).

%% gen_neg(-Value)//
%% Generate negative integer [-100, -1]
gen_neg(Value) -->
    rand_int(-100, -1, Value).

%% gen_bool(-Bool)//
%% Generate boolean
gen_bool(Bool) -->
    rand_bool(Bool).

%% gen_float(-Value)//
%% Generate float in [0.0, 1.0)
gen_float(Value) -->
    rand_float(Value).

%% gen_float_range(+Min, +Max, -Value)//
%% Generate float in [Min, Max)
gen_float_range(Min, Max, Value) -->
    rand_float(F),
    { Value is Min + F * (Max - Min) }.

%% ============================================================
%% CHARACTER/STRING GENERATORS
%% ============================================================

%% gen_char_range(+Min, +Max, -Char)//
%% Generate character in code range
gen_char_range(Min, Max, Char) -->
    rand_int(Min, Max, Code),
    { char_code(Char, Code) }.

%% gen_alpha(-Char)//
%% Generate lowercase letter a-z
gen_alpha(Char) -->
    gen_char_range(97, 122, Char).  % a-z

%% gen_digit(-Char)//
%% Generate digit 0-9
gen_digit(Char) -->
    gen_char_range(48, 57, Char).  % 0-9

%% gen_alphanum(-Char)//
%% Generate alphanumeric character
gen_alphanum(Char) -->
    rand_int(0, 1, Choice),
    ( { Choice = 0 } -> gen_alpha(Char) ; gen_digit(Char) ).

%% gen_string(+MaxLen, -String)//
%% Generate string (as atom) up to MaxLen chars
gen_string(MaxLen, String) -->
    rand_int(0, MaxLen, Len),
    gen_string_n(Len, Chars),
    { atom_chars(String, Chars) }.

gen_string_n(0, []) --> !.
gen_string_n(N, [C|Cs]) -->
    { N > 0, N1 is N - 1 },
    gen_alpha(C),
    gen_string_n(N1, Cs).

%% ============================================================
%% LIST GENERATORS
%% ============================================================

%% gen_list(+ElemGen, -List)//
%% Generate list with random length [0, 10]
gen_list(ElemGen, List) -->
    rand_int(0, 10, Len),
    gen_list_n(Len, ElemGen, List).

%% gen_list_n(+N, +ElemGen, -List)//
%% Generate list of exactly N elements
gen_list_n(0, _, []) --> !.
gen_list_n(N, ElemGen, [H|T]) -->
    { N > 0, N1 is N - 1 },
    call(ElemGen, H),
    gen_list_n(N1, ElemGen, T).

%% gen_nonempty_list(+ElemGen, -List)//
%% Generate non-empty list [1, 10]
gen_nonempty_list(ElemGen, List) -->
    rand_int(1, 10, Len),
    gen_list_n(Len, ElemGen, List).

%% ============================================================
%% CHOICE GENERATORS
%% ============================================================

%% gen_one_of(+Generators, -Value)//
%% Pick one generator and run it
gen_one_of(Gens, Value) -->
    { length(Gens, Len), MaxIdx is Len - 1 },
    rand_int(0, MaxIdx, Idx),
    { nth0(Idx, Gens, Gen) },
    call(Gen, Value).

%% gen_element(+List, -Elem)//
%% Pick random element from list
gen_element(List, Elem) -->
    rand_element(List, Elem).

%% gen_frequency(+WeightedGens, -Value)//
%% Pick generator by weight: [(Weight, Gen), ...]
gen_frequency(WeightedGens, Value) -->
    { sum_weights(WeightedGens, Total) },
    rand_int(1, Total, R),
    { pick_by_weight(WeightedGens, R, Gen) },
    call(Gen, Value).

sum_weights([], 0).
sum_weights([(W, _)|Rest], Total) :-
    sum_weights(Rest, RestTotal),
    Total is W + RestTotal.

pick_by_weight([(W, Gen)|_], R, Gen) :- R =< W, !.
pick_by_weight([(W, _)|Rest], R, Gen) :-
    R1 is R - W,
    pick_by_weight(Rest, R1, Gen).

%% ============================================================
%% COMBINATORS
%% ============================================================

%% gen_map(+Fun, +Gen, -MappedValue)//
%% Apply function to generated value
gen_map(Fun, Gen, MappedValue) -->
    call(Gen, Value),
    { call(Fun, Value, MappedValue) }.

%% gen_filter(+Pred, +Gen, -Value)//
%% Generate values until one satisfies predicate (max 100 tries)
gen_filter(Pred, Gen, Value) -->
    gen_filter_loop(Pred, Gen, 100, Value).

gen_filter_loop(_, _, 0, _) -->
    { throw(filter_exhausted) }.
gen_filter_loop(Pred, Gen, N, Value) -->
    call(Gen, Candidate),
    ( { call(Pred, Candidate) } ->
        { Value = Candidate }
    ;
        { N1 is N - 1 },
        gen_filter_loop(Pred, Gen, N1, Value)
    ).

%% gen_maybe(+Gen, -MaybeValue)//
%% Generate either null or a value
gen_maybe(Gen, Value) -->
    rand_bool(B),
    ( { B = true } -> call(Gen, Value) ; { Value = null } ).

%% gen_pair(+GenA, +GenB, -Pair)//
%% Generate a pair
gen_pair(GenA, GenB, A-B) -->
    call(GenA, A),
    call(GenB, B).

%% ============================================================
%% RECURSIVE GENERATORS
%% ============================================================

%% gen_tree(+LeafGen, -Tree)//
%% Generate binary tree (with size limit)
gen_tree(LeafGen, Tree) -->
    gen_tree_sized(LeafGen, 5, Tree).

gen_tree_sized(LeafGen, 0, Leaf) --> !,
    call(LeafGen, Leaf).
gen_tree_sized(LeafGen, Depth, Tree) -->
    rand_int(0, 2, Choice),
    { D1 is Depth - 1 },
    ( { Choice = 0 } ->
        call(LeafGen, Tree)
    ;
        gen_tree_sized(LeafGen, D1, Left),
        gen_tree_sized(LeafGen, D1, Right),
        { Tree = node(Left, Right) }
    ).

%% ============================================================
%% JSON GENERATORS
%% ============================================================

%% gen_json(-Value)//
%% Generate JSON value
gen_json(Value) -->
    gen_json_sized(3, Value).

gen_json_sized(0, Value) --> !,
    gen_json_scalar(Value).
gen_json_sized(Depth, Value) -->
    rand_int(0, 4, Choice),
    { D1 is Depth - 1 },
    gen_json_by_choice(Choice, D1, Value).

gen_json_by_choice(0, _, null) --> [].
gen_json_by_choice(1, _, true) --> [].
gen_json_by_choice(2, _, false) --> [].
gen_json_by_choice(3, _, N) --> gen_int(-100, 100, N).
gen_json_by_choice(4, D, json(Pairs)) --> gen_json_object(D, Pairs).

gen_json_scalar(Value) -->
    gen_one_of([
        gen_const(null),
        gen_const(true),
        gen_const(false),
        gen_int(-100, 100)
    ], Value).

gen_const(V, V) --> [].

gen_json_object(Depth, Pairs) -->
    rand_int(0, 3, Len),
    gen_json_pairs(Len, Depth, Pairs).

gen_json_pairs(0, _, []) --> !.
gen_json_pairs(N, Depth, [K-V|Rest]) -->
    { N > 0, N1 is N - 1 },
    gen_string(5, K),
    gen_json_sized(Depth, V),
    gen_json_pairs(N1, Depth, Rest).

%% ============================================================
%% TESTS
%% ============================================================

test_gen :-
    write('Testing generators...'), nl,

    %% Test integer generator
    phrase(gen_int(1, 10, V1), [42], [_]),
    ( integer(V1), V1 >= 1, V1 =< 10 ->
        write('  ✓ gen_int works'), nl
    ;
        write('  ✗ gen_int broken'), nl
    ),

    %% Test list generator
    phrase(gen_list(gen_nat, L), [42], [_]),
    ( is_list(L) ->
        write('  ✓ gen_list works'), nl
    ;
        write('  ✗ gen_list broken'), nl
    ),

    %% Test one_of generator
    phrase(gen_one_of([gen_const(a), gen_const(b), gen_const(c)], V2), [42], [_]),
    ( member(V2, [a, b, c]) ->
        write('  ✓ gen_one_of works'), nl
    ;
        write('  ✗ gen_one_of broken'), nl
    ),

    %% Test string generator
    phrase(gen_string(5, S), [42], [_]),
    ( atom(S) ->
        write('  ✓ gen_string works'), nl
    ;
        write('  ✗ gen_string broken'), nl
    ),

    %% Test JSON generator
    phrase(gen_json(J), [42], [_]),
    ( ground(J) ->
        write('  ✓ gen_json works: '), write(J), nl
    ;
        write('  ✗ gen_json broken'), nl
    ),

    write('Generator tests complete.'), nl.

is_list([]).
is_list([_|T]) :- is_list(T).
