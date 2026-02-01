%% Garden State Backend Tests
%% =========================================================

:- use_module(library(lists)).

%% Test helper
run_tests :-
    write('▌ GARDEN STATE TESTS ▐'), nl, nl,
    test_create_garden,
    test_record_reading,
    test_query_ripples,
    test_clear_garden,
    nl,
    write('▌ ALL TESTS PASSED ▐'), nl.

%% Test: Create a garden
test_create_garden :-
    write('Testing garden creation...'),
    create_garden(GardenID),
    ( atom(GardenID) ->
        write(' ✓'), nl
    ;
        write(' ✗ FAILED'), nl, halt(1)
    ).

%% Test: Record a reading
test_record_reading :-
    write('Testing reading record...'),
    create_garden(GID),
    record_reading(GID, 'reader1', 'key1', [1, 2, 3], [[1, 'meaning1'], [2, 'meaning2']]),
    ( reading(GID, 'reader1', 'key1', [1, 2, 3], [[1, 'meaning1'], [2, 'meaning2']], _) ->
        write(' ✓'), nl
    ;
        write(' ✗ FAILED'), nl, halt(1)
    ).

%% Test: Query ripples
test_query_ripples :-
    write('Testing ripple query...'),
    create_garden(GID),
    record_reading(GID, 'reader1', 'key1', [1, 2], [[1, 'meaning1']]),
    query_ripples(GID, Ripples),
    ( Ripples = [_|_] ; Ripples = [] ->
        write(' ✓'), nl
    ;
        write(' ✗ FAILED'), nl, halt(1)
    ).

%% Test: Clear garden
test_clear_garden :-
    write('Testing garden clear...'),
    create_garden(GID),
    clear_garden(GID),
    ( \+ garden(GID, _) ->
        write(' ✓'), nl
    ;
        write(' ✗ FAILED'), nl, halt(1)
    ).
