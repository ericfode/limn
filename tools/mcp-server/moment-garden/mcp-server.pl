#!/usr/bin/env swipl
%% Moment Garden MCP Server
%% =========================================================
%% gar mcp | sta per | mul usr
%% *Garden MCP. State persistence. Multiple users.*
%%
%% JSON-RPC server for Moment Garden state operations.
%% Ported to SWI-Prolog.
%%
%% Usage: swipl mcp-server.pl

:- module(mcp_server, [server_loop/0]).

:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module(state).
:- use_module(propagation).

%% ============================================================
%% TOOL DEFINITIONS
%% ============================================================

mcp_tools([
    json([
        name=garden_create,
        description='Create a new Moment Garden instance with unique ID.',
        inputSchema=json([
            type=object,
            properties=json([
                garden_id=json([type=string, description='Unique garden ID (e.g., garden-7f3k)'])
            ]),
            required=[garden_id]
        ])
    ]),
    json([
        name=garden_save_reading,
        description='Save a reader\'s complete garden navigation and collapses.',
        inputSchema=json([
            type=object,
            properties=json([
                garden_id=json([type=string, description='Garden instance ID']),
                reader_id=json([type=string, description='Unique reader ID']),
                temporal_key=json([type=string, description='Temporal key: was, now, or wil', enum=[was, now, wil]]),
                path=json([type=array, description='Ordered list of seed numbers visited', items=json([type=integer, minimum=1, maximum=9])]),
                collapses=json([type=array, description='List of seed collapses', items=json([type=object])])
            ]),
            required=[garden_id, reader_id, temporal_key, path, collapses]
        ])
    ]),
    json([
        name=garden_get_readings,
        description='Get all readings for a garden instance.',
        inputSchema=json([
            type=object,
            properties=json([
                garden_id=json([type=string, description='Garden instance ID'])
            ]),
            required=[garden_id]
        ])
    ]),
    json([
        name=garden_compare,
        description='Compare two readers\' interpretations of same garden.',
        inputSchema=json([
            type=object,
            properties=json([
                garden_id=json([type=string, description='Garden instance ID']),
                reader_id_1=json([type=string, description='First reader ID']),
                reader_id_2=json([type=string, description='Second reader ID'])
            ]),
            required=[garden_id, reader_id_1, reader_id_2]
        ])
    ]),
    json([
        name=garden_calculate_ripples,
        description='Calculate propagation ripples from collapsing a seed.',
        inputSchema=json([
            type=object,
            properties=json([
                seed_num=json([type=integer, description='Seed number 1-9', minimum=1, maximum=9]),
                temporal_key=json([type=string, description='Temporal key: was, now, or wil', enum=[was, now, wil]])
            ]),
            required=[seed_num, temporal_key]
        ])
    ])
]).

%% ============================================================
%% TOOL HANDLERS
%% ============================================================

handle_tool_call(garden_create, json(Args), Result) :-
    member(garden_id=GardenId, Args),
    (
        garden_exists(GardenId) ->
            Result = json([
                content=[json([type=text, text='Garden already exists'])],
                isError= @true
            ])
        ;
            create_garden(GardenId),
            Result = json([
                content=[json([
                    type=text,
                    text='Garden created successfully. Nine seeds await in superposition.'
                ])]
            ])
    ).

handle_tool_call(garden_save_reading, json(Args), Result) :-
    member(garden_id=GardenId, Args),
    member(reader_id=ReaderId, Args),
    member(temporal_key=KeyAtom, Args),
    member(path=PathJson, Args),
    member(collapses=CollapsesJson, Args),

    % Convert JSON arrays to Prolog lists
    json_to_path(PathJson, Path),
    json_to_collapses(CollapsesJson, Collapses),

    % Convert key atom
    parse_temporal_key(KeyAtom, Key),

    % Save the reading
    save_reading(GardenId, ReaderId, Key, Path, Collapses),

    Result = json([
        content=[json([
            type=text,
            text='Reading saved. Your interpretation is preserved.'
        ])]
    ]).

handle_tool_call(garden_get_readings, json(Args), Result) :-
    member(garden_id=GardenId, Args),
    get_readings(GardenId, Readings),
    length(Readings, Count),
    format(atom(Msg), 'Garden ~w has ~w reading(s)', [GardenId, Count]),
    Result = json([
        content=[json([type=text, text=Msg])],
        data=json([readings=Readings, count=Count])
    ]).

handle_tool_call(garden_compare, json(Args), Result) :-
    member(garden_id=GardenId, Args),
    member(reader_id_1=R1, Args),
    member(reader_id_2=R2, Args),

    get_reader_reading(GardenId, R1, Reading1),
    get_reader_reading(GardenId, R2, Reading2),

    compare_readings(Reading1, Reading2, Divergences),
    divergence_score(Divergences, Score),

    format(atom(ScoreMsg), 'Readings diverge by ~1f%', [Score]),
    Result = json([
        content=[json([type=text, text=ScoreMsg])],
        data=json([divergence_score=Score, divergences=Divergences])
    ]).

handle_tool_call(garden_calculate_ripples, json(Args), Result) :-
    member(seed_num=SeedNum, Args),
    member(temporal_key=KeyAtom, Args),

    parse_temporal_key(KeyAtom, Key),

    calculate_ripples(SeedNum, Key, Ripples),
    length(Ripples, Count),

    format(atom(Msg), 'Seed ~w with key ~w generates ~w ripples', [SeedNum, Key, Count]),
    Result = json([
        content=[json([type=text, text=Msg])],
        data=json([seed=SeedNum, key=Key, ripples=Ripples])
    ]).

%% ============================================================
%% JSON CONVERSION HELPERS
%% ============================================================

%% Parse temporal key from string/atom
parse_temporal_key(KeyAtom, Key) :-
    (atom(KeyAtom) -> atom_string(KeyAtom, KeyStr) ; KeyStr = KeyAtom),
    downcase_atom(KeyStr, KeyLower),
    (
        KeyLower = was -> Key = was ;
        KeyLower = now -> Key = now ;
        KeyLower = wil -> Key = wil
    ).

json_to_path(List, Path) :-
    is_list(List),
    findall(N, member(N, List), Path).
json_to_path(json(_), []).  % Fallback for empty

json_to_collapses(List, Collapses) :-
    is_list(List),
    findall(
        collapse(Seed, Meaning),
        (
            member(json(Obj), List),
            member(seed=Seed, Obj),
            member(meaning=Meaning, Obj)
        ),
        Collapses
    ).
json_to_collapses(json(_), []).  % Fallback for empty

%% ============================================================
%% MCP PROTOCOL HANDLER
%% ============================================================

handle_request(json(Request), Response) :-
    member(method=Method, Request),
    member(id=Id, Request),
    (member(params=Params, Request) -> true ; Params = json([])),

    (
        Method = initialize ->
            Response = json([
                jsonrpc='2.0',
                id=Id,
                result=json([
                    protocolVersion='2024-11-05',
                    capabilities=json([tools=json([])]),
                    serverInfo=json([
                        name='moment-garden',
                        version='1.0.0'
                    ])
                ])
            ])
        ;
        Method = 'tools/list' ->
            mcp_tools(Tools),
            Response = json([
                jsonrpc='2.0',
                id=Id,
                result=json([tools=Tools])
            ])
        ;
        Method = 'tools/call' ->
            member(params=json(CallParams), Request),
            member(name=ToolNameStr, CallParams),
            member(arguments=ToolArgs, CallParams),
            atom_string(ToolName, ToolNameStr),
            handle_tool_call(ToolName, ToolArgs, ToolResult),
            Response = json([
                jsonrpc='2.0',
                id=Id,
                result=ToolResult
            ])
        ;
        % Unknown method
            Response = json([
                jsonrpc='2.0',
                id=Id,
                error=json([code= -32601, message='Method not found'])
            ])
    ).

%% ============================================================
%% MAIN SERVER LOOP
%% ============================================================

server_loop :-
    (   at_end_of_stream(current_input)
    ->  true
    ;   catch(
            (   json_read(current_input, Request),
                handle_request(Request, Response),
                json_write(current_output, Response),
                nl,
                flush_output
            ),
            Error,
            (   format(user_error, 'Error: ~w~n', [Error]),
                flush_output(user_error)
            )
        ),
        server_loop
    ).

%% Start server
:- initialization(server_loop, main).
