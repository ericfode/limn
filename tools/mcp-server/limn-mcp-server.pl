%% Limn MCP Server - Pure Prolog
%% =========================================================
%% fab mcp | pur prl | nu nod
%% *Building MCP. Pure Prolog. No Node.*
%%
%% The monk accepts no impurity.
%%
%% Usage: scryer-prolog limn-mcp-server.pl
%%        (reads JSON-RPC from stdin, writes to stdout)

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).

%% Modules loaded via wrapper script

%% ============================================================
%% TOOL DEFINITIONS
%% ============================================================

mcp_tools([
    json([
        name-limn_interpret,
        description-'Interpret a Limn sentence. Words define REGIONS; sentences combine by INTERSECTION.',
        inputSchema-json([
            type-object,
            properties-json([
                sentence-json([type-string, description-'The Limn sentence']),
                key-json([type-string, description-'Optional context key'])
            ]),
            required-[sentence]
        ])
    ]),
    json([
        name-limn_compose,
        description-'Create Limn sentences from English concepts.',
        inputSchema-json([
            type-object,
            properties-json([
                concept-json([type-string, description-'English concept to express'])
            ]),
            required-[concept]
        ])
    ]),
    json([
        name-limn_validate,
        description-'Check if Limn sentence contains valid vocabulary.',
        inputSchema-json([
            type-object,
            properties-json([
                sentence-json([type-string, description-'Sentence to validate'])
            ]),
            required-[sentence]
        ])
    ]),
    json([
        name-limn_teach,
        description-'Get an interactive Limn teaching lesson.',
        inputSchema-json([
            type-object,
            properties-json([
                level-json([type-integer, description-'Lesson 1-3', default-1])
            ])
        ])
    ]),
    json([
        name-limn_poetry,
        description-'Generate Limn poetry on a theme.',
        inputSchema-json([
            type-object,
            properties-json([
                theme-json([type-string, description-'Theme for poetry'])
            ]),
            required-[theme]
        ])
    ]),
    json([
        name-limn_vocabulary,
        description-'Look up Limn words or browse by domain.',
        inputSchema-json([
            type-object,
            properties-json([
                word-json([type-string, description-'Word to look up']),
                domain-json([type-string, description-'Domain to browse'])
            ])
        ])
    ])
]).

%% ============================================================
%% TOOL HANDLERS
%% ============================================================

handle_tool(limn_validate, Args, Result) :-
    json_get(Args, sentence, Sentence),
    validate_sentence(Sentence, Valid, Invalid),
    length(Valid, VC),
    ( Invalid = [] ->
        number_chars(VC, VCChars),
        atom_chars(VCAtom, VCChars),
        atom_concat('Valid Limn. All ', VCAtom, T1),
        atom_concat(T1, ' words recognized.', Text)
    ;
        format(atom(Text), 'Unknown words: ~w', [Invalid])
    ),
    Result = json([content-[json([type-text, text-Text])]]).

handle_tool(limn_interpret, Args, Result) :-
    json_get(Args, sentence, Sentence),
    ( json_get(Args, key, Key) -> true ; Key = '' ),
    format(atom(Text), 'Interpret Limn: "~w"~nLimn words define REGIONS. Sentences combine by INTERSECTION.~nKey: ~w~nGenerate 5 interpretations.', [Sentence, Key]),
    Result = json([content-[json([type-text, text-Text])]]).

handle_tool(limn_compose, Args, Result) :-
    json_get(Args, concept, Concept),
    format(atom(Text), 'Compose Limn for: "~w"~n~nUse 3-letter words. Domains: physical (sol,liq,hot,col), mind (thi,fee,kno,dre), change (tra,mov,gro).~nOperators: nu (not), ve (very), | (scope).~nProvide 2-3 options.', [Concept]),
    Result = json([content-[json([type-text, text-Text])]]).

handle_tool(limn_teach, Args, Result) :-
    ( json_get(Args, level, Level) -> true ; Level = 1 ),
    lesson_text(Level, Text),
    Result = json([content-[json([type-text, text-Text])]]).

handle_tool(limn_poetry, Args, Result) :-
    json_get(Args, theme, Theme),
    format(atom(Text), 'Write Limn poetry on: "~w"~n~nUse 4-6 lines of valid Limn. Annotate each line.', [Theme]),
    Result = json([content-[json([type-text, text-Text])]]).

handle_tool(limn_vocabulary, Args, Result) :-
    ( json_get(Args, word, Word) ->
        ( word(Word) ->
            atom_concat(Word, ' is valid Limn vocabulary.', Text)
        ;
            atom_concat(Word, ' not found.', Text)
        )
    ; json_get(Args, domain, _) ->
        Text = 'Domains: physical, spatial, temporal, change, life, mind, communication, social, values.'
    ;
        Text = 'Use word param to look up, or domain to browse.'
    ),
    Result = json([content-[json([type-text, text-Text])]]).

%% Lesson texts
lesson_text(1, 'Lesson 1: Intersection. Words define REGIONS. hot col = lukewarm, contrast, ambivalence.').
lesson_text(2, 'Lesson 2: Order Independence. sol liq = liq sol = ice, slush, phase boundary.').
lesson_text(3, 'Lesson 3: Operators. nu negates NEXT word. nu sol liq vs sol nu liq.').
lesson_text(_, 'Lessons 1-3 available.').

%% ============================================================
%% JSON-RPC HANDLER
%% ============================================================

handle_request(Request, Response) :-
    json_get(Request, method, Method),
    ( json_get(Request, id, Id) -> true ; Id = null ),
    handle_method(Method, Request, Id, Response).

handle_method(initialize, _, Id, Response) :-
    Response = json([
        jsonrpc-'2.0',
        id-Id,
        result-json([
            protocolVersion-'2024-11-05',
            capabilities-json([tools-json([])]),
            serverInfo-json([name-'limn-mcp', version-'1.0.0'])
        ])
    ]).

handle_method('tools/list', _, Id, Response) :-
    mcp_tools(Tools),
    Response = json([
        jsonrpc-'2.0',
        id-Id,
        result-json([tools-Tools])
    ]).

handle_method('tools/call', Request, Id, Response) :-
    json_get(Request, params, Params),
    json_get(Params, name, ToolName),
    ( json_get(Params, arguments, Args) -> true ; Args = json([]) ),
    ( handle_tool(ToolName, Args, Result) ->
        Response = json([jsonrpc-'2.0', id-Id, result-Result])
    ;
        Response = json([jsonrpc-'2.0', id-Id, error-json([code-(-32603), message-'Tool error'])])
    ).

handle_method('notifications/initialized', _, _, _) :- !.  % No response

handle_method(Method, _, Id, Response) :-
    format(atom(Msg), 'Method not found: ~w', [Method]),
    Response = json([jsonrpc-'2.0', id-Id, error-json([code-(-32601), message-Msg])]).

%% ============================================================
%% MAIN LOOP
%% ============================================================

read_line(Chars) :-
    get_char(C),
    ( C = end_of_file -> Chars = []
    ; C = '\n' -> Chars = []
    ; Chars = [C|Rest], read_line(Rest)
    ).

main_loop :-
    read_line(Line),
    ( Line = [] -> true  % EOF
    ;
        ( json_parse(Line, Request) ->
            ( handle_request(Request, Response) ->
                json_write(Response, OutChars),
                atom_chars(Out, OutChars),
                write(Out), nl, flush_output
            ;
                true  % notification, no response
            )
        ;
            json_write(json([jsonrpc-'2.0', id-null, error-json([code-(-32700), message-'Parse error'])]), ErrChars),
            atom_chars(Err, ErrChars),
            write(Err), nl, flush_output
        ),
        main_loop
    ).

:- initialization(main_loop).
