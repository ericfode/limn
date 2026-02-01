%% Garden State Backend - Prolog persistence for MCP Server
%% =========================================================
%% gar sta | per prl | tim mul
%% *Garden state. Persistent Prolog. Time-based multiplayer.*
%%
%% Stores garden reading sessions with temporal ripple propagation.
%% State persists to disk for ~24h or until cleared.

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).

%% ============================================================
%% DYNAMIC PREDICATES
%% ============================================================

:- dynamic(garden/2).         % garden(ID, CreatedAt)
:- dynamic(reading/6).        % reading(GardenID, ReaderID, Key, Path, Collapses, Timestamp)
:- dynamic(ripple/4).         % ripple(GardenID, SeedIdx, FromMeaning, ToMeaning)

%% ============================================================
%% PERSISTENCE (Simplified - in-memory for now)
%% ============================================================
%% Note: Disk persistence requires file I/O predicates not available
%% in base scryer-prolog. For now, state persists only during server
%% runtime. To add disk persistence, consult library(files) if available.

%% Get file path for a garden (for future disk persistence)
garden_file(GardenID, Path) :-
    atom_concat('/tmp/limn-garden-', GardenID, T),
    atom_concat(T, '.pl', Path).

%% Load garden state from disk (placeholder)
load_garden(_GardenID) :-
    % For now, just succeed - state is in memory
    true.

%% Save garden state to disk (placeholder)
save_garden(_GardenID) :-
    % For now, just succeed - state is in memory
    true.

%% ============================================================
%% GARDEN ID GENERATION
%% ============================================================

%% Counter for ID generation
:- dynamic(garden_id_counter/1).
garden_id_counter(0).

%% Simple counter-based ID generation
generate_garden_id(ID) :-
    garden_id_counter(N),
    N1 is N + 1,
    retract(garden_id_counter(N)),
    assertz(garden_id_counter(N1)),
    number_chars(N1, NumChars),
    atom_chars(NumAtom, NumChars),
    atom_concat('gdn', NumAtom, ID).


%% ============================================================
%% GARDEN OPERATIONS
%% ============================================================

%% Create a new garden
create_garden(GardenID) :-
    generate_garden_id(GardenID),
    CreatedAt = 0,  % Placeholder timestamp
    assertz(garden(GardenID, CreatedAt)),
    save_garden(GardenID).

%% Record a reading event
record_reading(GardenID, ReaderID, Key, Path, Collapses) :-
    load_garden(GardenID),
    Timestamp = 0,  % Placeholder timestamp
    assertz(reading(GardenID, ReaderID, Key, Path, Collapses, Timestamp)),
    % Calculate ripples based on reading
    calculate_ripples(GardenID, Path, Collapses),
    save_garden(GardenID).

%% Calculate ripples from a reading
%% Ripples propagate based on path topology:
%% - Horizontal: SAME temporal orientation
%% - Vertical: OPPOSITE orientation (inverted)
%% - Diagonal: TENSION (unresolved)
calculate_ripples(_, _, []).
calculate_ripples(GardenID, Path, [[SeedIdx, Meaning]|Rest]) :-
    find_adjacent_seeds(Path, SeedIdx, Adjacent),
    propagate_to_adjacent(GardenID, SeedIdx, Meaning, Adjacent),
    calculate_ripples(GardenID, Path, Rest).

%% Find adjacent seeds in the path topology
%% Path is a list of seed indices
find_adjacent_seeds(Path, SeedIdx, Adjacent) :-
    % Find neighbors in the path
    nth0(Idx, Path, SeedIdx),
    findall(Neighbor,
        ( ( Idx1 is Idx - 1, Idx1 >= 0, nth0(Idx1, Path, Neighbor) )
        ; ( Idx1 is Idx + 1, nth0(Idx1, Path, Neighbor) )
        ),
        Adjacent).

%% Propagate meaning to adjacent seeds
propagate_to_adjacent(_, _, _, []).
propagate_to_adjacent(GardenID, FromIdx, FromMeaning, [ToIdx|Rest]) :-
    % Determine orientation based on topology
    % For now, simplified: just record the ripple
    assertz(ripple(GardenID, FromIdx, FromMeaning, ToIdx)),
    propagate_to_adjacent(GardenID, FromIdx, FromMeaning, Rest).

%% Query ripples for a garden
query_ripples(GardenID, Ripples) :-
    load_garden(GardenID),
    findall([SeedIdx, FromMeaning, ToMeaning],
        ripple(GardenID, SeedIdx, FromMeaning, ToMeaning),
        Ripples).

%% Clear a garden's state
clear_garden(GardenID) :-
    retractall(garden(GardenID, _)),
    retractall(reading(GardenID, _, _, _, _, _)),
    retractall(ripple(GardenID, _, _, _)).
    % Note: File deletion not implemented (requires file I/O library)

