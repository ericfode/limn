%% Moment Garden State Tests
%% =========================================================
%% tes gar | sta ver | pur log
%% *Testing garden. State verification. Pure logic.*

:- use_module('state.pl').
:- use_module('propagation.pl').
:- use_module(library(lists)).

%% ============================================================
%% TEST RUNNER
%% ============================================================

run_tests :-
    write('══════════════════════════════════════════'), nl,
    write('  MOMENT GARDEN STATE TESTS'), nl,
    write('══════════════════════════════════════════'), nl, nl,

    write('─── Garden Creation ───'), nl,
    test_create_garden,

    nl, write('─── Reading Operations ───'), nl,
    test_save_reading,
    test_get_readings,

    nl, write('─── Comparison ───'), nl,
    test_compare_readings,

    nl, write('─── Propagation ───'), nl,
    test_calculate_ripples,
    test_adjacency,

    nl, write('══════════════════════════════════════════'), nl,
    write('  ALL TESTS COMPLETE'), nl,
    write('══════════════════════════════════════════'), nl.

%% ============================================================
%% GARDEN CREATION TESTS
%% ============================================================

test_create_garden :-
    TestId = 'test-garden-001',

    % Clean up if exists
    retractall(garden(TestId, _, _, _)),

    % Create new garden
    create_garden(TestId),

    % Verify it exists
    (garden_exists(TestId) ->
        write('  ✓ Garden created and exists'), nl
    ;
        write('  ✗ FAILED: Garden not found'), nl, fail
    ),

    % Verify structure
    garden(TestId, Timestamp, Seeds, _),
    length(Seeds, 9),
    write('  ✓ Garden has 9 seeds'), nl.

%% ============================================================
%% READING OPERATION TESTS
%% ============================================================

test_save_reading :-
    GardenId = 'test-garden-002',
    retractall(garden(GardenId, _, _, _)),
    retractall(reading(GardenId, _, _, _, _, _)),

    create_garden(GardenId),

    % Save a reading
    Path = [5, 4, 1, 2, 3, 6, 9, 8, 7],
    Collapses = [
        collapse(5, "Now manifesting here"),
        collapse(4, "Losing self to find other"),
        collapse(1, "Beginning with love")
    ],
    save_reading(GardenId, 'reader-alice', now, Path, Collapses),

    % Verify saved
    (reading(GardenId, 'reader-alice', now, Path, Collapses, _) ->
        write('  ✓ Reading saved successfully'), nl
    ;
        write('  ✗ FAILED: Reading not found'), nl, fail
    ).

test_get_readings :-
    GardenId = 'test-garden-003',
    retractall(garden(GardenId, _, _, _)),
    retractall(reading(GardenId, _, _, _, _, _)),

    create_garden(GardenId),

    % Add multiple readings
    save_reading(GardenId, 'alice', now, [5, 1], [collapse(5, "now")]),
    save_reading(GardenId, 'bob', was, [1, 5], [collapse(1, "was")]),

    get_readings(GardenId, Readings),
    length(Readings, Count),

    (Count = 2 ->
        write('  ✓ Retrieved 2 readings'), nl
    ;
        format('  ✗ FAILED: Expected 2 readings, got ~w~n', [Count]), fail
    ).

%% ============================================================
%% COMPARISON TESTS
%% ============================================================

test_compare_readings :-
    GardenId = 'test-garden-004',
    retractall(garden(GardenId, _, _, _)),
    retractall(reading(GardenId, _, _, _, _, _)),

    create_garden(GardenId),

    % Two readers, same seeds, different collapses
    Collapses1 = [collapse(1, "beginning"), collapse(2, "holding")],
    Collapses2 = [collapse(1, "beginning"), collapse(2, "breaking")],

    save_reading(GardenId, 'alice', now, [1, 2], Collapses1),
    save_reading(GardenId, 'bob', was, [1, 2], Collapses2),

    get_reader_reading(GardenId, 'alice', R1),
    get_reader_reading(GardenId, 'bob', R2),

    compare_readings(R1, R2, Divergences),
    divergence_score(Divergences, Score),

    format('  ✓ Divergence calculated: ~1f%~n', [Score]).

%% ============================================================
%% PROPAGATION TESTS
%% ============================================================

test_calculate_ripples :-
    % Collapse seed 5 (center) with key 'now'
    calculate_ripples(5, now, Ripples),
    length(Ripples, Count),

    % Seed 5 has: 4 horizontal/vertical + 4 diagonal = 8 neighbors
    (Count = 8 ->
        write('  ✓ Seed 5 generates 8 ripples'), nl
    ;
        format('  ✗ FAILED: Expected 8 ripples, got ~w~n', [Count]), fail
    ),

    % Check ripple types
    include(lambda([ripple(_, E, _)], E = reinforcement), Ripples, Reinforcements),
    include(lambda([ripple(_, E, _)], E = inversion), Ripples, Inversions),
    include(lambda([ripple(_, E, _)], E = tension), Ripples, Tensions),

    length(Reinforcements, RC),
    length(Inversions, IC),
    length(Tensions, TC),

    format('  ✓ Ripple types: ~w reinforcement, ~w inversion, ~w tension~n', [RC, IC, TC]).

test_adjacency :-
    % Test adjacency graphs
    adjacent_horizontal(5, H),
    adjacent_vertical(5, V),
    adjacent_diagonal(5, D),

    length(H, HCount),
    length(V, VCount),
    length(D, DCount),

    (HCount = 2, VCount = 2, DCount = 4 ->
        write('  ✓ Seed 5 adjacency correct (2H, 2V, 4D)'), nl
    ;
        write('  ✗ FAILED: Adjacency counts wrong'), nl, fail
    ).

%% ============================================================
%% LAMBDA HELPER (if not available)
%% ============================================================

%% Simple lambda for filtering
lambda([ripple(_, E, _)], Goal, ripple(_, E, _)) :- call(Goal, E).

%% ============================================================
%% RUN TESTS
%% ============================================================

:- initialization(run_tests).
