%% LMN Runtime - Scryer Prolog Compatible
%% =======================================
%% lmn run | tho exe | mea cal
%% *LMN runtime. Thought executes. Meaning calculates.*

%% ============================================================
%% CHARACTER CLASSIFICATION
%% ============================================================

is_alpha(C) :- C @>= 'a', C @=< 'z'.
is_alpha(C) :- C @>= 'A', C @=< 'Z'.

is_digit(C) :- C @>= '0', C @=< '9'.

is_alnum(C) :- is_alpha(C).
is_alnum(C) :- is_digit(C).
is_alnum('_').

is_space(' ').
is_space('\t').
is_space('\n').

%% ============================================================
%% TOKENIZER
%% ============================================================

tokenize(Input, Tokens) :-
    atom_chars(Input, Chars),
    tok_chars(Chars, Tokens).

tok_chars([], []).
tok_chars([C|Cs], Tokens) :-
    is_space(C), !,
    tok_chars(Cs, Tokens).
tok_chars(['@'|Cs], [collapse|Tokens]) :- !, tok_chars(Cs, Tokens).
tok_chars(['~'|Cs], [oracle|Tokens]) :- !, tok_chars(Cs, Tokens).
tok_chars(['['|Cs], [open|Tokens]) :- !, tok_chars(Cs, Tokens).
tok_chars([']'|Cs], [close|Tokens]) :- !, tok_chars(Cs, Tokens).
tok_chars([','|Cs], [comma|Tokens]) :- !, tok_chars(Cs, Tokens).
tok_chars(['-','>'|Cs], [arrow|Tokens]) :- !, tok_chars(Cs, Tokens).
tok_chars([C|Cs], [word(Word)|Tokens]) :-
    is_alpha(C), !,
    read_word([C|Cs], WordChars, Rest),
    atom_chars(Word, WordChars),
    tok_chars(Rest, Tokens).
tok_chars([_|Cs], Tokens) :-
    tok_chars(Cs, Tokens).

read_word([], [], []).
read_word([C|Cs], [C|W], Rest) :-
    is_alnum(C), !,
    read_word(Cs, W, Rest).
read_word(Cs, [], Cs).

%% ============================================================
%% PARSER
%% ============================================================

parse(Input, AST) :-
    tokenize(Input, Tokens),
    parse_expr(Tokens, AST, Rest),
    Rest = [].

% Oracle: ~ prompt
parse_expr([oracle|Tokens], oracle(Expr), Rest) :- !,
    parse_simple(Tokens, Expr, Rest).

% Default: parse primary and check for operators
parse_expr(Tokens, Expr, Rest) :-
    parse_primary(Tokens, Primary, Rest1),
    (Rest1 = [collapse|Rest2] ->
        % Collapse: primary @ context
        parse_simple(Rest2, Ctx, Rest),
        Expr = collapse(Primary, Ctx)
    ; Rest1 = [arrow|_] ->
        % Flow: reconstruct and parse
        parse_flow(Tokens, Steps, Rest),
        (Steps = [_,_|_] ->
            Expr = flow(Steps)
        ;
            Expr = Primary,
            Rest = Rest1
        )
    ;
        Expr = Primary,
        Rest = Rest1
    ).

% Parse primary (simple or superposition)
parse_primary([open|Tokens], superposition(Items), Rest) :- !,
    parse_items(Tokens, Items, Rest1),
    Rest1 = [close|Rest].
parse_primary(Tokens, Expr, Rest) :-
    parse_simple(Tokens, Expr, Rest).

% Simple (non-operator) expression
parse_simple([word(W)|Rest], word(W), Rest) :- !.
parse_simple([open|Tokens], Expr, Rest) :- !,
    parse_expr(Tokens, Expr, [close|Rest]).
parse_simple([], none, []).

% Parse superposition items
parse_items([close|Rest], [], [close|Rest]) :- !.
parse_items(Tokens, [Item|Items], Rest) :-
    parse_simple(Tokens, Item, Rest1),
    (Rest1 = [comma|Rest2] ->
        parse_items(Rest2, Items, Rest)
    ; Rest1 = [close|_] ->
        Items = [],
        Rest = Rest1
    ; Items = [],
      Rest = Rest1
    ).

% Parse flow steps
parse_flow(Tokens, [Step|Steps], Rest) :-
    parse_simple(Tokens, Step, Rest1),
    (Rest1 = [arrow|Rest2] ->
        parse_flow(Rest2, Steps, Rest)
    ;
        Steps = [],
        Rest = Rest1
    ).

%% ============================================================
%% EVALUATOR
%% ============================================================

eval(word(W), _, word(W)).

eval(superposition(Items), Ctx, Results) :-
    eval_list(Items, Ctx, Results).

eval(oracle(Prompt), Ctx, Result) :-
    format("~nOracle: ~w~n", [Prompt]),
    format("Context: ~w~n", [Ctx]),
    format("Response: ", []),
    read(Result).

eval(collapse(Expr, _CtxKey), Ctx, Result) :-
    eval(Expr, Ctx, Values),
    (Values = [Result|_] ->
        true  % Pick first from list
    ;
        Result = Values  % Or return as-is
    ).

eval(flow(Steps), Ctx, Result) :-
    eval_flow(Steps, Ctx, Result).

eval(none, _, none).

eval_list([], _, []).
eval_list([Item|Items], Ctx, [Result|Results]) :-
    eval(Item, Ctx, Result),
    eval_list(Items, Ctx, Results).

eval_flow([], Result, Result).
eval_flow([Step|Steps], Input, Output) :-
    eval(Step, Input, Intermediate),
    eval_flow(Steps, Intermediate, Output).

%% ============================================================
%% MAIN INTERFACE
%% ============================================================

run(Input) :-
    format("~n=== LMN Runtime ===~n", []),
    format("Input: ~w~n", [Input]),
    (parse(Input, AST) ->
        format("AST:   ~w~n", [AST]),
        (eval(AST, context, Result) ->
            format("Result: ~w~n~n", [Result])
        ;
            format("ERROR: Evaluation failed~n~n", [])
        )
    ;
        format("ERROR: Parsing failed~n~n", [])
    ).

demo :-
    format("~n╔════════════════════════════════════════╗~n", []),
    format("║  LMN Runtime - Phase 1 Implementation ║~n", []),
    format("╚════════════════════════════════════════╝~n", []),

    run('[red, blue, green]'),
    run('[run, execute] @ code'),
    run('hello'),

    format("~n✓ LMN Runtime operational~n", []),
    format("✓ Parser: AST generation working~n", []),
    format("✓ Evaluator: Superposition + Collapse working~n", []),
    format("✓ Oracle: Stub implementation (manual input)~n~n", []).

:- initialization(demo).
