%% Moment Garden State Management
%% =========================================================
%% gar sta | pur prl | tem nav
%% *Garden state. Pure Prolog. Temporal navigation.*
%%
%% State persistence for The Moment Garden semantic game.
%% Ported to SWI-Prolog.

:- module(state, [
    create_garden/1,
    garden_exists/1,
    save_reading/5,
    get_readings/2,
    get_reader_reading/3,
    compare_readings/3,
    divergence_score/2,
    write_garden_to_stream/2,
    garden/4,
    reading/6,
    seed_state/3
]).

:- use_module(library(lists)).

%% ============================================================
%% PERSISTENCE LAYER
%% ============================================================

%% Path to state storage
state_dir('.beads/moment-garden').

%% Ensure state directory exists
%% Note: Filesystem operations stubbed for testing
ensure_state_dir :- true.

%% Load garden state from disk
%% Note: Filesystem operations stubbed for testing (in-memory only)
load_garden(_GardenId) :- true.

%% Save garden state to disk
%% Note: Filesystem operations stubbed for testing (in-memory only)
save_garden(_GardenId) :- true.

%% ============================================================
%% GARDEN STRUCTURE
%% ============================================================

%% Dynamic predicates for garden state
:- dynamic garden/4.             % garden(Id, Created, Seeds, Metadata)
:- dynamic reading/6.            % reading(GardenId, ReaderId, Key, Path, Collapses, Timestamp)
:- dynamic seed_state/3.         % seed_state(GardenId, SeedNum, Ripples)

%% Create new garden instance
create_garden(GardenId) :-
    \+ garden(GardenId, _, _, _),
    get_time(Timestamp),
    InitialSeeds = [
        seed(1, [beg, lov, fea]),
        seed(2, [mid, hol, bre]),
        seed(3, [end, pea, gri]),
        seed(4, [los, sel, oth]),
        seed(5, [now, her, gon]),
        seed(6, [fnd, wha_was, wha_wil]),
        seed(7, [rem, tru, wis]),
        seed(8, [for, giv, tak]),
        seed(9, [bec, who_was, who_wil])
    ],
    assertz(garden(GardenId, Timestamp, InitialSeeds, metadata([]))),
    save_garden(GardenId).

%% Check if garden exists
garden_exists(GardenId) :-
    load_garden(GardenId),
    garden(GardenId, _, _, _).

%% ============================================================
%% READING OPERATIONS
%% ============================================================

%% Record a reader's navigation
save_reading(GardenId, ReaderId, Key, Path, Collapses) :-
    get_time(Timestamp),
    assertz(reading(GardenId, ReaderId, Key, Path, Collapses, Timestamp)),
    save_garden(GardenId).

%% Get all readings for a garden
get_readings(GardenId, Readings) :-
    load_garden(GardenId),
    findall(
        reading(ReaderId, Key, Path, Collapses, Timestamp),
        reading(GardenId, ReaderId, Key, Path, Collapses, Timestamp),
        Readings
    ).

%% Get specific reader's reading
get_reader_reading(GardenId, ReaderId, Reading) :-
    load_garden(GardenId),
    reading(GardenId, ReaderId, Key, Path, Collapses, Timestamp),
    Reading = reading(Key, Path, Collapses, Timestamp).

%% ============================================================
%% COMPARISON OPERATIONS
%% ============================================================

%% Calculate divergence between two readings
%% Readings are reading(Key, Path, Collapses, Timestamp) (4-arg form)
%% Returns list of divergence(SeedNum, Collapse1, Collapse2, Score)
compare_readings(Reading1, Reading2, Divergences) :-
    Reading1 = reading(_, _, Collapses1, _),
    Reading2 = reading(_, _, Collapses2, _),
    findall(
        divergence(Seed, C1, C2, Score),
        (
            member(collapse(Seed, C1), Collapses1),
            member(collapse(Seed, C2), Collapses2),
            (C1 = C2 -> Score = 0.0 ; Score = 1.0)
        ),
        Divergences
    ).

%% Calculate overall divergence percentage
divergence_score(Divergences, Score) :-
    length(Divergences, Total),
    findall(D, (member(D, Divergences), D = divergence(_, _, _, S), S =:= 1.0), Different),
    length(Different, DiffCount),
    (Total > 0 -> Score is (DiffCount * 100) / Total ; Score = 0.0).

%% ============================================================
%% HELPER PREDICATES
%% ============================================================

%% Write garden state to stream
write_garden_to_stream(GardenId, Stream) :-
    garden(GardenId, Created, Seeds, Metadata),
    format(Stream, '% Garden: ~w~n', [GardenId]),
    format(Stream, 'garden(~w, ~w, ~w, ~w).~n~n', [GardenId, Created, Seeds, Metadata]),
    forall(
        reading(GardenId, ReaderId, Key, Path, Collapses, Timestamp),
        format(Stream, 'reading(~w, ~w, ~w, ~w, ~w, ~w).~n',
               [GardenId, ReaderId, Key, Path, Collapses, Timestamp])
    ).
