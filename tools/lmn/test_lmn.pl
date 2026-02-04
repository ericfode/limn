%% LMN Runtime Tests
%% ==================

%% Load the runtime (copy relevant predicates since we can't use modules/consult)

is_alpha(C) :- C @>= 'a', C @=< 'z'.
is_alpha(C) :- C @>= 'A', C @=< 'Z'.
is_digit(C) :- C @>= '0', C @=< '9'.
is_alnum(C) :- is_alpha(C).
is_alnum(C) :- is_digit(C).
is_alnum('_').
is_space(' ').
is_space('\t').
is_space('\n').

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

parse(Input, AST) :-
    tokenize(Input, Tokens),
    parse_expr(Tokens, AST, Rest),
    Rest = [].

parse_expr([oracle|Tokens], oracle(Expr), Rest) :- !,
    parse_simple(Tokens, Expr, Rest).

parse_expr(Tokens, Expr, Rest) :-
    parse_primary(Tokens, Primary, Rest1),
    (Rest1 = [collapse|Rest2] ->
        parse_simple(Rest2, Ctx, Rest),
        Expr = collapse(Primary, Ctx)
    ; Rest1 = [arrow|_] ->
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

parse_primary([open|Tokens], superposition(Items), Rest) :- !,
    parse_items(Tokens, Items, Rest1),
    Rest1 = [close|Rest].
parse_primary(Tokens, Expr, Rest) :-
    parse_simple(Tokens, Expr, Rest).

parse_simple([word(W)|Rest], word(W), Rest) :- !.
parse_simple([open|Tokens], Expr, Rest) :- !,
    parse_expr(Tokens, Expr, [close|Rest]).
parse_simple([], none, []).

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

parse_flow(Tokens, [Step|Steps], Rest) :-
    parse_simple(Tokens, Step, Rest1),
    (Rest1 = [arrow|Rest2] ->
        parse_flow(Rest2, Steps, Rest)
    ;
        Steps = [],
        Rest = Rest1
    ).

%% ============================================================
%% TESTS
%% ============================================================

test_tokenize_word :-
    tokenize('hello', Tokens),
    Tokens = [word(hello)].

test_tokenize_superposition :-
    tokenize('[red, blue]', Tokens),
    Tokens = [open, word(red), comma, word(blue), close].

test_tokenize_collapse :-
    tokenize('red @ ctx', Tokens),
    Tokens = [word(red), collapse, word(ctx)].

test_parse_word :-
    parse('hello', AST),
    AST = word(hello).

test_parse_superposition :-
    parse('[red, blue, green]', AST),
    AST = superposition([word(red), word(blue), word(green)]).

test_parse_collapse :-
    parse('[red, blue] @ ctx', AST),
    AST = collapse(superposition([word(red), word(blue)]), word(ctx)).

test_parse_flow :-
    parse('a -> b -> c', AST),
    AST = flow([word(a), word(b), word(c)]).

run_test(Name, Goal) :-
    format("  ~w... ", [Name]),
    (call(Goal) ->
        format("✓~n", [])
    ;
        format("✗ FAILED~n", []),
        fail
    ).

run_all_tests :-
    format("~n═══════════════════════════════════════~n", []),
    format("  LMN Runtime Test Suite~n", []),
    format("═══════════════════════════════════════~n~n", []),

    format("Tokenizer Tests:~n", []),
    run_test('tokenize word', test_tokenize_word),
    run_test('tokenize superposition', test_tokenize_superposition),
    run_test('tokenize collapse', test_tokenize_collapse),

    nl,
    format("Parser Tests:~n", []),
    run_test('parse word', test_parse_word),
    run_test('parse superposition', test_parse_superposition),
    run_test('parse collapse', test_parse_collapse),
    run_test('parse flow', test_parse_flow),

    nl,
    format("═══════════════════════════════════════~n", []),
    format("  All tests passed ✓~n", []),
    format("═══════════════════════════════════════~n~n", []).

:- initialization(run_all_tests).
