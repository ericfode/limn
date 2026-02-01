%% Shrinking - Find Minimal Counterexamples
%% =========================================================
%% shr min | cex fin | pur log
%% *Shrinking minimal. Counterexample finding. Pure logic.*
%%
%% When a property fails, the monk seeks the smallest truth.

:- use_module(library(lists)).

%% Declare discontiguous predicates
:- discontiguous(shrink/2).

%% is_list/1 - check if argument is a list
is_list([]).
is_list([_|T]) :- is_list(T).

%% ============================================================
%% SHRINK DISPATCHER
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
        fail  % No shrink available
    ).

%% ============================================================
%% INTEGER SHRINKING
%% ============================================================

%% shrink_int(+N, -Smaller)
%% Shrink integer toward 0

shrink_int(N, 0) :-
    N \= 0.

shrink_int(N, S) :-
    N > 0,
    S is N // 2,
    S \= N.

shrink_int(N, S) :-
    N < 0,
    S is N // 2,
    S \= N.

shrink_int(N, S) :-
    N > 0,
    S is N - 1.

shrink_int(N, S) :-
    N < 0,
    S is N + 1.

%% ============================================================
%% LIST SHRINKING
%% ============================================================

%% shrink_list(+List, -Smaller)
%% Shrink list by various strategies

%% Empty list has no shrinks
shrink_list([], _) :- !, fail.

%% Remove one element (try each position)
shrink_list(List, Smaller) :-
    select(_, List, Smaller).

%% Shrink one element (try each position)
shrink_list([H|T], [H2|T]) :-
    shrink(H, H2).

shrink_list([H|T], [H|T2]) :-
    shrink_list(T, T2).

%% Take first half
shrink_list(List, Smaller) :-
    length(List, Len),
    Len > 1,
    Half is Len // 2,
    length(Smaller, Half),
    append(Smaller, _, List).

%% ============================================================
%% ATOM SHRINKING
%% ============================================================

%% shrink_atom(+Atom, -Smaller)
%% Shrink atom by truncation

shrink_atom(A, S) :-
    atom_chars(A, Chars),
    Chars = [_|_],  % non-empty
    shrink_chars(Chars, SmallerChars),
    atom_chars(S, SmallerChars).

shrink_chars([_], []) :- !.  % Single char shrinks to empty
shrink_chars([_|T], T).      % Remove first char
shrink_chars([H|T], [H|T2]) :- % Keep first, shrink rest
    shrink_chars(T, T2).

%% (Pair shrinking handled in main dispatcher)

%% ============================================================
%% SHRINK TO MINIMAL
%% ============================================================

%% shrink_to_minimal(+Value, +Property, -Minimal)
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
%% TESTS
%% ============================================================

test_shrink :-
    write('Testing shrinking...'), nl,

    %% Test integer shrinking
    findall(S, shrink(10, S), IntShrinks),
    ( member(0, IntShrinks), member(5, IntShrinks), member(9, IntShrinks) ->
        write('  ✓ Integer shrinking works'), nl
    ;
        write('  ✗ Integer shrinking broken'), nl
    ),

    %% Test list shrinking
    findall(S, shrink([1,2,3], S), ListShrinks),
    ( member([2,3], ListShrinks), member([1,3], ListShrinks), member([1,2], ListShrinks) ->
        write('  ✓ List shrinking (removal) works'), nl
    ;
        write('  ✗ List shrinking broken'), nl
    ),

    %% Test shrink to minimal
    shrink_to_minimal([1,2,3,4,5], is_short_list, Minimal),
    ( Minimal = [_,_,_] ->  % Should shrink to length 3 (first that fails is_short_list)
        write('  ✓ Shrink to minimal works'), nl
    ;
        write('  ⚠ Shrink to minimal: got '), write(Minimal), nl
    ),

    write('Shrinking tests complete.'), nl.

%% Test property: list has fewer than 3 elements
is_short_list(L) :- length(L, Len), Len < 3.
