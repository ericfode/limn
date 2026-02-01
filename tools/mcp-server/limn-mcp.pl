%% Limn MCP Server - Prolog Core (Validation Only)
%% =========================================================
%% fab mcp | val cor | min prl
%% *Building MCP. Validation core. Minimal Prolog.*

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).

%% Vocabulary loaded externally via consult

%% ============================================================
%% TOKENIZER (from linter)
%% ============================================================

tokenize(Input, Tokens) :-
    atom_chars(Input, Chars),
    phrase(tokens(Tokens), Chars).

tokens([]) --> [].
tokens(Ts) --> white, !, whites0, tokens(Ts).
tokens([T|Ts]) --> word_chars_greedy(Cs), { Cs \= [], atom_chars(T, Cs) }, !, tokens(Ts).
tokens([pipe|Ts]) --> "|", !, tokens(Ts).
tokens(Ts) --> [_], tokens(Ts).

word_chars_greedy([C|Cs]) --> [C], { is_word_char(C) }, word_chars_greedy(Cs).
word_chars_greedy([]) --> [].

is_word_char(C) :- char_code(C, Code), Code >= 97, Code =< 122.

white --> " ".
white --> "\t".
white --> "\n".

whites0 --> white, whites0.
whites0 --> [].

%% ============================================================
%% VALIDATION
%% ============================================================

validate_sentence(Input, Valid, Invalid) :-
    tokenize(Input, Tokens),
    partition_words(Tokens, Valid, Invalid).

partition_words([], [], []).
partition_words([T|Ts], Valid, Invalid) :-
    ( T = pipe ->
        partition_words(Ts, Valid, Invalid)
    ; word(T) ->
        Valid = [T|V1], partition_words(Ts, V1, Invalid)
    ; operator(T, _) ->
        Valid = [T|V1], partition_words(Ts, V1, Invalid)
    ;
        Invalid = [T|I1], partition_words(Ts, Valid, I1)
    ).

%% ============================================================
%% HANDLERS
%% ============================================================

handle_validate(Sentence) :-
    validate_sentence(Sentence, Valid, Invalid),
    length(Valid, VC), length(Invalid, IC),
    format("VALID_COUNT:~w~n", [VC]),
    format("INVALID_COUNT:~w~n", [IC]),
    format("VALID_WORDS:~w~n", [Valid]),
    format("INVALID_WORDS:~w~n", [Invalid]).

handle_vocab_word(Word) :-
    ( word(Word) ->
        format("FOUND:true~n")
    ;
        format("FOUND:false~n")
    ).
