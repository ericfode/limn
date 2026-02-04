%% The Moment Garden - State Backend
%% =========================================================
%% gar sta | tim mec | mea ter | prl fac
%% *Garden state. Time mechanic. Meaning territory. Prolog facts.*
%%
%% The monk plants gardens in the logic.

:- module(garden, [
    garden_new/2,
    garden_exists/1,
    garden_join/2,
    garden_select_key/3,
    garden_touch_seed/5,
    garden_reading/2,
    garden_compare/3,
    garden_map/2,
    garden_save/0,
    garden_load/0,
    garden_cleanup/0,
    test_garden/0
]).

:- use_module(library(lists)).
:- use_module(library(format)).

%% forall/2 - Scryer Prolog compatibility
%% Succeeds if for all solutions of Cond, Action succeeds
forall(Cond, Action) :-
    \+ (Cond, \+ Action).

%% ============================================================
%% DYNAMIC FACTS
%% ============================================================

:- dynamic(garden/2).           % garden(ID, CreatedAt)
:- dynamic(reading/6).          % reading(GardenID, ReaderID, Key, Path, Collapses, Timestamp)
:- dynamic(ripple/4).           % ripple(GardenID, SeedIdx, FromMeaning, ToMeaning)
:- dynamic(tension/3).          % tension(GardenID, SeedIdx, PotentialMeanings)

%% ============================================================
%% THE NINE SEEDS (Uncollapsed)
%% ============================================================

seed(1, [beg, lov, fea]).       % Beginning, Love, Fear
seed(2, [mid, hol, bre]).       % Middle, Holding, Breaking
seed(3, ['end', pea, gri]).     % End, Peace, Grief
seed(4, [los, sel, oth]).       % Losing, Self, Other
seed(5, [now, her, gon]).       % Now, Here, Gone
seed(6, [fnd, 'wha was', 'wha wil']).  % Finding, What was, What will be
seed(7, [rem, tru, wis]).       % Remembering, Truth, Wish
seed(8, ['for', giv, tak]).     % Forgetting/Forgiving, Giving, Taking
seed(9, [bec, 'who was', 'who wil']). % Becoming, Who was, Who will be

%% ============================================================
%% ADJACENCY (Grid 3x3, numbered 1-9)
%% ============================================================
%%
%%   1 - 2 - 3
%%   |   |   |
%%   4 - 5 - 6
%%   |   |   |
%%   7 - 8 - 9

%% Horizontal adjacency (left-right ripple)
adjacent_h(1, 2). adjacent_h(2, 1).
adjacent_h(2, 3). adjacent_h(3, 2).
adjacent_h(4, 5). adjacent_h(5, 4).
adjacent_h(5, 6). adjacent_h(6, 5).
adjacent_h(7, 8). adjacent_h(8, 7).
adjacent_h(8, 9). adjacent_h(9, 8).

%% Vertical adjacency (up-down echo)
adjacent_v(1, 4). adjacent_v(4, 1).
adjacent_v(2, 5). adjacent_v(5, 2).
adjacent_v(3, 6). adjacent_v(6, 3).
adjacent_v(4, 7). adjacent_v(7, 4).
adjacent_v(5, 8). adjacent_v(8, 5).
adjacent_v(6, 9). adjacent_v(9, 6).

%% Diagonal adjacency (tension)
diagonal(1, 5). diagonal(5, 1).
diagonal(5, 9). diagonal(9, 5).
diagonal(3, 5). diagonal(5, 3).
diagonal(5, 7). diagonal(7, 5).
diagonal(1, 9). diagonal(9, 1).  % Extended diagonal
diagonal(3, 7). diagonal(7, 3).  % Extended diagonal

%% ============================================================
%% TEMPORAL KEYS
%% ============================================================

temporal_key(was).   % Memory - already occurred
temporal_key(now).   % Presence - happening now
temporal_key(will).  % Anticipation - not yet but coming

%% Opposite orientation for vertical echo
opposite_key(was, will).
opposite_key(will, was).
opposite_key(now, now).  % Now echoes as now (present is timeless)

%% ============================================================
%% GARDEN ID GENERATION
%% ============================================================

%% Simple hash-based ID from timestamp
generate_garden_id(ID) :-
    get_time_seed(Seed),
    hash_to_id(Seed, ID).

%% Get a seed from "time" (using process/random state)
get_time_seed(Seed) :-
    %% Use a simple counter + fixed offset for now
    %% In real implementation, would use actual timestamp
    ( retract(id_counter(N)) -> true ; N = 0 ),
    N1 is N + 1,
    asserta(id_counter(N1)),
    Seed is (N1 * 2654435761) mod 65536.

:- dynamic(id_counter/1).

%% Convert number to 4-char hex-ish ID
hash_to_id(N, ID) :-
    C1 is (N mod 26) + 97,  % a-z
    N1 is N // 26,
    C2 is (N1 mod 26) + 97,
    N2 is N1 // 26,
    C3 is (N2 mod 26) + 97,
    N3 is N2 // 26,
    C4 is (N3 mod 26) + 97,
    atom_codes(ID, [C1, C2, C3, C4]).

%% ============================================================
%% GARDEN OPERATIONS
%% ============================================================

%% Create new garden
garden_new(GardenID, Info) :-
    generate_garden_id(ShortID),
    atom_concat('garden-', ShortID, GardenID),
    get_timestamp(Timestamp),
    assertz(garden(GardenID, Timestamp)),
    %% Build info string
    atom_concat('Garden ', GardenID, P1),
    atom_concat(P1, ' created', Info).

%% Check if garden exists
garden_exists(GardenID) :-
    garden(GardenID, _).

%% Join existing garden (returns reader ID)
garden_join(GardenID, ReaderID) :-
    garden_exists(GardenID),
    generate_reader_id(ReaderID).

generate_reader_id(ReaderID) :-
    get_time_seed(Seed),
    hash_to_id(Seed, ShortID),
    atom_concat('reader-', ShortID, ReaderID).

%% Select temporal key for reading
garden_select_key(GardenID, ReaderID, Key) :-
    temporal_key(Key),
    get_timestamp(Timestamp),
    assertz(reading(GardenID, ReaderID, Key, [], [], Timestamp)).

%% Touch a seed (collapse it)
garden_touch_seed(GardenID, ReaderID, SeedIdx, Collapse, Effects) :-
    integer(SeedIdx),
    SeedIdx >= 1,
    SeedIdx =< 9,
    reading(GardenID, ReaderID, Key, Path, Collapses, _),
    seed(SeedIdx, Meanings),
    collapse_meaning(Key, Path, Meanings, Collapse),
    %% Update reading
    append(Path, [SeedIdx], NewPath),
    append(Collapses, [SeedIdx-Collapse], NewCollapses),
    get_timestamp(NewTimestamp),
    retract(reading(GardenID, ReaderID, Key, Path, Collapses, _)),
    assertz(reading(GardenID, ReaderID, Key, NewPath, NewCollapses, NewTimestamp)),
    %% Calculate propagation effects
    propagate(GardenID, SeedIdx, Key, Collapse, Effects).

%% Collapse meaning based on key and path
collapse_meaning(was, _, Meanings, Collapse) :-
    %% Past-oriented: prefer first meaning or "memory" interpretations
    Meanings = [Collapse|_].
collapse_meaning(now, _, Meanings, Collapse) :-
    %% Present-oriented: prefer middle/present meaning
    length(Meanings, Len),
    Idx is Len // 2,
    nth0(Idx, Meanings, Collapse).
collapse_meaning(will, _, Meanings, Collapse) :-
    %% Future-oriented: prefer last meaning
    last(Meanings, Collapse).

%% Propagate effects to adjacent seeds
propagate(GardenID, SeedIdx, Key, FromMeaning, Effects) :-
    findall(effect(Type, N, Orientation),
        (   ( adjacent_h(SeedIdx, N), Type = horizontal, Orientation = Key )
        ;   ( adjacent_v(SeedIdx, N), opposite_key(Key, OppKey), Type = vertical, Orientation = OppKey )
        ;   ( diagonal(SeedIdx, N), Type = diagonal, Orientation = tension )
        ),
        Effects),
    %% Record ripples for horizontal/vertical
    forall(
        member(effect(Type, N, _), Effects),
        (   Type \= diagonal ->
            assertz(ripple(GardenID, N, FromMeaning, propagated))
        ;   %% Record tension
            seed(N, Meanings),
            assertz(tension(GardenID, N, Meanings))
        )
    ).

%% Get current reading state
garden_reading(GardenID, Reading) :-
    findall(
        reading_info(ReaderID, Key, Path, Collapses),
        reading(GardenID, ReaderID, Key, Path, Collapses, _),
        Readings
    ),
    Reading = readings(GardenID, Readings).

%% Compare readings between two readers
garden_compare(GardenID, Reader1, Comparison) :-
    findall(
        compare(SeedIdx, R1Collapse, R2Collapse),
        (   reading(GardenID, Reader1, _, _, Collapses1, _),
            reading(GardenID, Reader2, _, _, Collapses2, _),
            Reader1 \= Reader2,
            member(SeedIdx-R1Collapse, Collapses1),
            member(SeedIdx-R2Collapse, Collapses2)
        ),
        Comparisons
    ),
    Comparison = divergence(GardenID, Reader1, Comparisons).

%% Get garden map (current state)
garden_map(GardenID, Map) :-
    findall(
        seed_state(Idx, Meanings, State),
        (   seed(Idx, Meanings),
            seed_current_state(GardenID, Idx, State)
        ),
        Seeds
    ),
    Map = garden_map(GardenID, Seeds).

seed_current_state(GardenID, Idx, collapsed(Collapse)) :-
    reading(GardenID, _, _, _, Collapses, _),
    member(Idx-Collapse, Collapses),
    !.
seed_current_state(GardenID, Idx, tension(Meanings)) :-
    tension(GardenID, Idx, Meanings),
    !.
seed_current_state(GardenID, Idx, rippled) :-
    ripple(GardenID, Idx, _, _),
    !.
seed_current_state(_, _, uncollapsed).

%% ============================================================
%% PERSISTENCE
%% ============================================================

%% File path for garden state
garden_file('/tmp/limn-gardens.pl').

%% Save all garden state to disk
garden_save :-
    garden_file(File),
    open(File, write, Stream),
    %% Write gardens
    forall(garden(ID, Created),
        format(Stream, "garden(~q, ~q).~n", [ID, Created])),
    %% Write readings
    forall(reading(GID, RID, Key, Path, Collapses, Ts),
        format(Stream, "reading(~q, ~q, ~q, ~q, ~q, ~q).~n",
               [GID, RID, Key, Path, Collapses, Ts])),
    %% Write ripples
    forall(ripple(GID, Idx, From, To),
        format(Stream, "ripple(~q, ~q, ~q, ~q).~n", [GID, Idx, From, To])),
    %% Write tension
    forall(tension(GID, Idx, Meanings),
        format(Stream, "tension(~q, ~q, ~q).~n", [GID, Idx, Meanings])),
    close(Stream).

%% Load garden state from disk
garden_load :-
    garden_file(File),
    ( file_exists(File) ->
        consult(File)
    ;
        true  % No file yet, start fresh
    ).

%% Check if file exists
file_exists(File) :-
    catch(open(File, read, S), _, fail),
    close(S).

%% Cleanup old gardens (older than 24 hours)
garden_cleanup :-
    get_timestamp(Now),
    Cutoff is Now - 86400,  % 24 hours in seconds
    forall(
        (garden(ID, Created), Created < Cutoff),
        cleanup_garden(ID)
    ).

cleanup_garden(GardenID) :-
    retractall(garden(GardenID, _)),
    retractall(reading(GardenID, _, _, _, _, _)),
    retractall(ripple(GardenID, _, _, _)),
    retractall(tension(GardenID, _, _)).

%% ============================================================
%% UTILITIES
%% ============================================================

%% Simple timestamp (seconds since epoch approximation)
%% In real implementation, would use actual system time
get_timestamp(Ts) :-
    ( retract(ts_counter(N)) -> true ; N = 1738410000 ),  % Base: ~Feb 1, 2025
    N1 is N + 1,
    asserta(ts_counter(N1)),
    Ts = N1.

:- dynamic(ts_counter/1).

%% ============================================================
%% TEST
%% ============================================================

test_garden :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  GARDEN STATE TEST'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    %% Create garden
    garden_new(GID, Info),
    write('Created: '), write(Info), nl,

    %% Join as reader
    garden_join(GID, RID),
    write('Reader: '), write(RID), nl,

    %% Select key
    garden_select_key(GID, RID, now),
    write('Key selected: now'), nl,

    %% Touch seeds
    garden_touch_seed(GID, RID, 5, Collapse1, Effects1),
    write('Seed 5 collapsed: '), write(Collapse1), nl,
    write('Effects: '), write(Effects1), nl,

    garden_touch_seed(GID, RID, 2, Collapse2, Effects2),
    write('Seed 2 collapsed: '), write(Collapse2), nl,
    write('Effects: '), write(Effects2), nl,

    %% Get reading
    garden_reading(GID, Reading),
    write('Reading: '), write(Reading), nl,

    %% Get map
    garden_map(GID, Map),
    write('Map: '), write(Map), nl,

    %% Save
    garden_save,
    write('Saved to disk'), nl,

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  TEST COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.
