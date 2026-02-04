%% Moment Garden Propagation Rules
%% =========================================================
%% gar rip | tem wav | mea spr
%% *Garden ripples. Temporal waves. Meaning spreads.*
%%
%% Propagation logic: How collapsing one seed affects others.

:- use_module(library(lists)).

%% ============================================================
%% SEED ADJACENCY GRAPH
%% ============================================================

%% The 9-seed grid:
%%   1 → 2 → 3
%%   ↓   ↓   ↓
%%   4 → 5 → 6
%%   ↓   ↓   ↓
%%   7 → 8 → 9

%% Horizontal neighbors (left/right)
adjacent_horizontal(1, [2]).
adjacent_horizontal(2, [1, 3]).
adjacent_horizontal(3, [2]).
adjacent_horizontal(4, [5]).
adjacent_horizontal(5, [4, 6]).
adjacent_horizontal(6, [5]).
adjacent_horizontal(7, [8]).
adjacent_horizontal(8, [7, 9]).
adjacent_horizontal(9, [8]).

%% Vertical neighbors (up/down)
adjacent_vertical(1, [4]).
adjacent_vertical(2, [5]).
adjacent_vertical(3, [6]).
adjacent_vertical(4, [1, 7]).
adjacent_vertical(5, [2, 8]).
adjacent_vertical(6, [3, 9]).
adjacent_vertical(7, [4]).
adjacent_vertical(8, [5]).
adjacent_vertical(9, [6]).

%% Diagonal neighbors
adjacent_diagonal(1, [5, 9]).
adjacent_diagonal(2, [4, 6]).
adjacent_diagonal(3, [5, 7]).
adjacent_diagonal(4, [2, 8]).
adjacent_diagonal(5, [1, 3, 7, 9]).
adjacent_diagonal(6, [2, 8]).
adjacent_diagonal(7, [3, 5]).
adjacent_diagonal(8, [4, 6]).
adjacent_diagonal(9, [1, 5]).

%% ============================================================
%% TEMPORAL KEY INVERSION
%% ============================================================

%% Keys invert when rippling vertically
invert_key(was, wil).
invert_key(wil, was).
invert_key(now, now).  % Now inverts to itself

%% ============================================================
%% PROPAGATION RULES
%% ============================================================

%% Calculate all ripples from collapsing a seed with a key
%%
%% Returns: list of ripple(TargetSeed, Effect, Strength)
%% - Effect: reinforcement | tension | inversion
%% - Strength: 1.0 (direct) | 0.5 (diagonal)
calculate_ripples(SeedNum, Key, Ripples) :-
    findall(
        ripple(Target, Effect, Strength),
        (
            (
                % Horizontal ripples: reinforcement
                adjacent_horizontal(SeedNum, Targets),
                member(Target, Targets),
                Effect = reinforcement,
                Strength = 1.0
            ) ;
            (
                % Vertical ripples: inversion
                adjacent_vertical(SeedNum, Targets),
                member(Target, Targets),
                Effect = inversion,
                Strength = 1.0
            ) ;
            (
                % Diagonal ripples: tension
                adjacent_diagonal(SeedNum, Targets),
                member(Target, Targets),
                Effect = tension,
                Strength = 0.5
            )
        ),
        Ripples
    ).

%% ============================================================
%% MEANING SHIFT
%% ============================================================

%% How ripples shift meaning based on key and effect type
%%
%% Seed meanings by default position:
%% seed(N, [PrimaryMeaning, WasMeaning, WillMeaning])
%%
%% Examples:
%% seed(1, [beg, lov, fea])
%% - now: beg (beginning)
%% - was: lov (love - what began)
%% - will: fea (fear - what will begin)

shift_meaning(SeedNum, Key, Effect, ShiftedKey) :-
    (
        % Reinforcement: same temporal perspective
        Effect = reinforcement,
        ShiftedKey = Key
    ) ;
    (
        % Inversion: flip temporal perspective
        Effect = inversion,
        invert_key(Key, ShiftedKey)
    ) ;
    (
        % Tension: stored for later (weakens collapse)
        Effect = tension,
        ShiftedKey = Key  % Tension doesn't change key, just adds ambiguity
    ).

%% ============================================================
%% COLLAPSE COMPUTATION
%% ============================================================

%% Given seed meanings, key, and ripples, determine collapsed meaning
%%
%% Seed structure: seed(Num, [Primary, WasMeaning, WillMeaning])
collapse_seed(seed(Num, [Primary, WasMeaning, WillMeaning]), Key, Ripples, Collapsed) :-
    % Base meaning from key
    (
        Key = now -> BaseMeaning = Primary ;
        Key = was -> BaseMeaning = WasMeaning ;
        Key = wil -> BaseMeaning = WillMeaning
    ),

    % Check for ripples affecting this seed
    (
        member(ripple(Num, Effect, Strength), Ripples) ->
            % Apply ripple influence
            apply_ripple_influence(BaseMeaning, Effect, Strength, Collapsed)
        ;
            % No ripples, use base
            Collapsed = BaseMeaning
    ).

%% Apply ripple influence to meaning
%% (Simplified - could add more nuanced meaning shifts)
apply_ripple_influence(BaseMeaning, reinforcement, 1.0, Collapsed) :-
    % Strong reinforcement: meaning intensifies
    Collapsed = BaseMeaning.

apply_ripple_influence(BaseMeaning, inversion, 1.0, Collapsed) :-
    % Inversion: meaning gets ambiguous (both meanings present)
    Collapsed = BaseMeaning.  % Could extend to show dual meanings

apply_ripple_influence(BaseMeaning, tension, 0.5, Collapsed) :-
    % Tension: meaning becomes uncertain
    Collapsed = BaseMeaning.  % Could mark as "under tension"

%% ============================================================
%% PATH ACCUMULATION
%% ============================================================

%% Given a navigation path, accumulate all ripples
%% Path = [SeedNum1, SeedNum2, ...]
%% Returns accumulated ripples after each step
accumulate_ripples(Path, Key, AccumulatedRipples) :-
    accumulate_ripples(Path, Key, [], AccumulatedRipples).

accumulate_ripples([], _, Acc, Acc).
accumulate_ripples([Seed|Rest], Key, Acc, FinalRipples) :-
    calculate_ripples(Seed, Key, NewRipples),
    append(Acc, NewRipples, UpdatedAcc),
    accumulate_ripples(Rest, Key, UpdatedAcc, FinalRipples).
