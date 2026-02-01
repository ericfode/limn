%% Limn Linter - Validate Limn sentences against vocabulary and grammar
%% =====================================================================
%% lin val | voc dol | gra dcg | tri mod
%% *Limn validation. Dolt vocabulary. Grammar DCG. Three modes.*
%%
%% Usage: scryer-prolog -g "consult('limn-lint.pl'), lint('sol aqu tra'), halt"
%%        scryer-prolog -g "consult('limn-lint.pl'), lint_strict('sol aqu tra'), halt"

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).

%% ============================================================
%% CONFIGURATION
%% ============================================================

%% Strictness levels
%% permissive: warn on unknown words, warn on grammar
%% standard:   warn on unknown words, error on grammar (DEFAULT)
%% strict:     error on unknown words, error on grammar

:- dynamic(strictness/1).
strictness(standard).

set_strictness(Level) :-
    member(Level, [permissive, standard, strict]),
    retractall(strictness(_)),
    assertz(strictness(Level)).

%% ============================================================
%% VOCABULARY LOOKUP (from static facts)
%% ============================================================

%% Vocabulary loaded from limn-vocab.pl (consult separately)

%% Check if word exists in vocabulary
word_exists(Word) :-
    atom_chars(Word, Chars),
    length(Chars, Len),
    Len >= 2, Len =< 4,
    word(Word).

%% Check if word is an operator
operator_exists(Op) :- operator(Op, _).

%% Get operator type
operator_type(Op, Type) :- operator(Op, Type).

%% Word is valid (either vocabulary or operator)
valid_word(Word) :- word_exists(Word), !.
valid_word(Word) :- operator_exists(Word).

%% ============================================================
%% TOKENIZER
%% ============================================================

%% Tokenize input string into words
tokenize(Input, Tokens) :-
    atom_chars(Input, Chars),
    phrase(tokens(Tokens), Chars).

tokens([]) --> [].
tokens(Ts) --> white, whites0, !, tokens(Ts).  % consume whitespace, then cut
tokens([T|Ts]) --> word_chars_greedy(Cs), { Cs \= [], atom_chars(T, Cs) }, !, tokens(Ts).
tokens([pipe|Ts]) --> "|", !, tokens(Ts).
tokens(Ts) --> [_], tokens(Ts).  % skip unknown chars (no cut - fallback)

%% Greedily consume all word chars
word_chars_greedy([C|Cs]) --> [C], { is_word_char(C) }, word_chars_greedy(Cs).
word_chars_greedy([]) --> [].

is_word_char(C) :- char_code(C, Code), Code >= 97, Code =< 122.  % a-z

%% At least one whitespace
white --> " ".
white --> "\t".
white --> "\n".

%% Zero or more whitespace
whites0 --> white, whites0.
whites0 --> [].

%% ============================================================
%% GRAMMAR DCG (from grammar-formal.md)
%% ============================================================

%% Operators by type
unary_op(nu). unary_op(ve). unary_op(so). unary_op(te). unary_op(we).
quantifier(al). quantifier(ex). quantifier(on).
reference(yo). reference(an). reference(sa).
comparator(mi). comparator(ma). comparator(eq).

%% Scope expression
scope_expr(scope(Topic, Comment)) -->
    flat_expr(Topic), [pipe], scope_expr(Comment).
scope_expr(Expr) --> flat_expr(Expr).

%% Flat expression (sequence of terms)
flat_expr(flat(Terms)) --> terms(Terms).

terms([T|Ts]) --> term(T), terms(Ts).
terms([T]) --> term(T).
terms([]) --> [].

%% Terms
term(neg(T)) --> [Op], { unary_op(Op) }, term(T).
term(quant(Q, T)) --> [Q], { quantifier(Q) }, term(T).
term(ref(R)) --> [R], { reference(R) }.
term(word(W)) --> [W], { \+ unary_op(W), \+ quantifier(W), \+ reference(W), \+ comparator(W), W \= pipe }.

%% ============================================================
%% VALIDATION
%% ============================================================

%% Validate tokens against vocabulary
validate_vocab([], [], []).
validate_vocab([T|Ts], Valid, Invalid) :-
    ( valid_word(T) ->
        Valid = [T|V1], validate_vocab(Ts, V1, Invalid)
    ; T = pipe ->
        Valid = [T|V1], validate_vocab(Ts, V1, Invalid)
    ;
        Invalid = [T|I1], validate_vocab(Ts, Valid, I1)
    ).

%% Validate grammar
validate_grammar(Tokens, AST) :-
    phrase(scope_expr(AST), Tokens, []).

%% Check for contradictions (semantic conflicts)
check_contradictions(Tokens, Contradictions) :-
    findall(Pair, (
        member(W1, Tokens), member(W2, Tokens),
        W1 @< W2,
        contradiction(W1, W2),
        Pair = W1-W2
    ), Contradictions).

%% Known contradictions
contradiction(hot, col).
contradiction(col, hot).
contradiction(sol, gas).
contradiction(gas, sol).
contradiction(sol, liq).
contradiction(liq, sol).
contradiction(liq, gas).
contradiction(gas, liq).
contradiction(bri, dim).
contradiction(dim, bri).

%% ============================================================
%% MAIN LINTER INTERFACE
%% ============================================================

%% Lint with current strictness
lint(Input) :-
    strictness(Level),
    lint_with_level(Input, Level).

%% Lint permissive
lint_permissive(Input) :- lint_with_level(Input, permissive).

%% Lint standard (default)
lint_standard(Input) :- lint_with_level(Input, standard).

%% Lint strict
lint_strict(Input) :- lint_with_level(Input, strict).

%% Core linting logic
lint_with_level(Input, Level) :-
    nl,
    write('▌ LIMN LINTER ▐'), nl,
    format("Mode: ~w~n", [Level]),
    format("Input: ~w~n", [Input]),
    nl,

    % Tokenize
    tokenize(Input, Tokens),
    format("Tokens: ~w~n", [Tokens]),
    nl,

    % Vocabulary check
    write('◢ VOCABULARY'), nl,
    validate_vocab(Tokens, Valid, Invalid),
    length(Valid, VCount),
    ( Invalid = [] ->
        format("  ✓ All ~w words valid~n", [VCount])
    ;
        format("  ⚠ Unknown words: ~w~n", [Invalid]),
        ( Level = strict ->
            write('  ✗ STRICT MODE: Unknown words not allowed'), nl,
            fail
        ; true )
    ),
    nl,

    % Grammar check
    write('◢ GRAMMAR'), nl,
    ( validate_grammar(Tokens, AST) ->
        write('  ✓ Grammar valid'), nl,
        format("  AST: ~w~n", [AST])
    ;
        write('  ⚠ Grammar invalid'), nl,
        ( Level \= permissive ->
            write('  ✗ Grammar errors not allowed in this mode'), nl,
            fail
        ; true )
    ),
    nl,

    % Contradiction check
    write('◢ SEMANTICS'), nl,
    check_contradictions(Tokens, Contradictions),
    ( Contradictions = [] ->
        write('  ✓ No contradictions')
    ;
        format("  ⚠ Potential contradictions: ~w~n", [Contradictions]),
        write('  (Contradictions allowed - may indicate liminal zone)')
    ),
    nl, nl,

    write('◢ LINT COMPLETE ✓'), nl.

%% ============================================================
%% TEST CASES (from Dr. Solvik)
%% ============================================================

test :-
    nl, write('▌ RUNNING TEST CASES ▐'), nl, nl,

    write('=== VALID (should pass all modes) ==='), nl,
    test_case('sol aqu tra', standard),
    test_case('nu hot', standard),
    test_case('ve bri lux', standard),
    test_case('al hum dre', standard),

    write('=== EDGE CASES (complex but valid) ==='), nl,
    test_case('nu ve so hot', standard),

    write('=== CONTRADICTIONS (flag but allow) ==='), nl,
    test_case('hot col', standard),

    nl, write('◢ ALL TESTS COMPLETE'), nl.

test_case(Input, Level) :-
    format("Testing: ~w ... ", [Input]),
    ( catch(lint_with_level(Input, Level), _, fail) ->
        write('PASS'), nl
    ;
        write('FAIL'), nl
    ).

%% Entry point
main :- test.
