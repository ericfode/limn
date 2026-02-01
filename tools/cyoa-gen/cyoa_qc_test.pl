%% CYOA QuickCheck Tests - Property-Based Testing for Static Site Generator
%% =========================================================================
%% cyo qck | dcg prp | htm val | pur tes
%% *CYOA QuickCheck. DCG properties. HTML validation. Pure testing.*
%%
%% The monk tests the story-machine.
%% Each test a chapter in the book of truth.

:- use_module(library(lists)).

%% ============================================================
%% PRNG (Pure Pseudo-Random Number Generator)
%% ============================================================

lcg_next(S0, S1) :-
    S1 is (1103515245 * S0 + 12345) mod 2147483648.

rnd(Min, Max, V, S0, S1) :-
    lcg_next(S0, S1),
    Range is Max - Min + 1,
    V is Min + (S1 mod Range).

%% ============================================================
%% GENERATORS
%% ============================================================

%% Generate random lowercase string (safe for IDs)
gen_lowercase_str(S0, S1, Str) :-
    rnd(1, 8, Len, S0, S2),
    gen_lowercase_chars(Len, S2, S1, Cs),
    atom_chars(Str, Cs).

gen_lowercase_chars(0, S, S, []) :- !.
gen_lowercase_chars(N, S0, S2, [C|Cs]) :-
    N > 0,
    rnd(97, 122, Code, S0, S1),  % a-z
    char_code(C, Code),
    N1 is N - 1,
    gen_lowercase_chars(N1, S1, S2, Cs).

%% Generate string with spaces (safe for titles)
gen_title_str(S0, S1, Str) :-
    rnd(1, 12, Len, S0, S2),
    gen_title_chars(Len, S2, S1, Cs),
    atom_chars(Str, Cs).

gen_title_chars(0, S, S, []) :- !.
gen_title_chars(N, S0, S2, [C|Cs]) :-
    N > 0,
    rnd(0, 10, Choice, S0, S1a),
    ( Choice =:= 0 ->
        C = ' ',  % 10% chance of space
        S1 = S1a
    ;
        rnd(97, 122, Code, S1a, S1),
        char_code(C, Code)
    ),
    N1 is N - 1,
    gen_title_chars(N1, S1, S2, Cs).

%% Generate alphanumeric ID (like chapter IDs: 00, 01a, etc)
gen_chapter_id(S0, S1, Id) :-
    rnd(0, 2, Style, S0, S2),
    ( Style =:= 0 ->
        %% Numeric: 00-99
        rnd(0, 99, Num, S2, S1),
        number_codes(Num, Codes),
        ( Num < 10 -> atom_codes(Id, [48|Codes]) ; atom_codes(Id, Codes) )
    ; Style =:= 1 ->
        %% Alphanumeric: 01a, 02b, etc
        rnd(0, 99, Num, S2, S3),
        rnd(97, 99, Suffix, S3, S1),
        number_codes(Num, NumCodes),
        ( Num < 10 ->
            append([48], NumCodes, PaddedCodes)
        ;
            PaddedCodes = NumCodes
        ),
        append(PaddedCodes, [Suffix], IdCodes),
        atom_codes(Id, IdCodes)
    ;
        %% Short word
        gen_lowercase_str(S2, S1, Id)
    ).

%% Generate mixed case string (for upcase testing)
gen_mixed_case_str(S0, S1, Str) :-
    rnd(1, 10, Len, S0, S2),
    gen_mixed_chars(Len, S2, S1, Cs),
    atom_chars(Str, Cs).

gen_mixed_chars(0, S, S, []) :- !.
gen_mixed_chars(N, S0, S2, [C|Cs]) :-
    N > 0,
    rnd(0, 1, Upper, S0, S1a),
    ( Upper =:= 1 ->
        rnd(65, 90, Code, S1a, S1)   % A-Z
    ;
        rnd(97, 122, Code, S1a, S1)  % a-z
    ),
    char_code(C, Code),
    N1 is N - 1,
    gen_mixed_chars(N1, S1, S2, Cs).

%% ============================================================
%% PROPERTIES: UPCASE
%% ============================================================

%% Property: upcase_atom produces all uppercase
prop_upcase_all_upper(Atom) :-
    upcase_atom(Atom, Upper),
    atom_chars(Upper, Chars),
    all_upper_or_nonalpha(Chars).

all_upper_or_nonalpha([]).
all_upper_or_nonalpha([C|Cs]) :-
    char_code(C, Code),
    ( (Code >= 65, Code =< 90) -> true      % A-Z
    ; (Code >= 97, Code =< 122) -> fail     % a-z is wrong!
    ; true                                   % non-alpha ok
    ),
    all_upper_or_nonalpha(Cs).

%% Property: upcase_atom is idempotent
prop_upcase_idempotent(Atom) :-
    upcase_atom(Atom, Upper1),
    upcase_atom(Upper1, Upper2),
    Upper1 == Upper2.

%% Property: upcase preserves length
prop_upcase_preserves_length(Atom) :-
    atom_length(Atom, Len1),
    upcase_atom(Atom, Upper),
    atom_length(Upper, Len2),
    Len1 =:= Len2.

%% ============================================================
%% PROPERTIES: DCG PARSERS
%% ============================================================

%% Property: fm_prop(id-Id) parses correctly (actual working parser)
%% Uses collect_until_quote which does NOT consume the closing quote
prop_fm_id_parses(Id) :-
    atom_chars(Id, IdCs),
    %% Build: id: "xxx" (fm_prop expects this exact format)
    append("id: \"", IdCs, T1),
    append(T1, "\"", Input),
    %% Parse using fm_prop
    phrase(fm_prop(id-ParsedId), Input, _),
    ParsedId == Id.

%% Property: fm_prop(title-Title) parses correctly
prop_fm_title_parses(Title) :-
    atom_chars(Title, TitleCs),
    %% Build: title: "xxx"
    append("title: \"", TitleCs, T1),
    append(T1, "\"", Input),
    %% Parse
    phrase(fm_prop(title-ParsedTitle), Input, _),
    ParsedTitle == Title.

%% Property: collect_until_quote stops BEFORE quote (doesn't consume)
prop_collect_until_quote_stops :-
    Input = "hello\"world",
    phrase(collect_until_quote(Cs), Input, Rest),
    atom_chars(Parsed, Cs),
    Parsed == hello,
    Rest = "\"world".

%% Property: parse_frontmatter extracts id correctly
prop_parse_frontmatter_id(Id) :-
    atom_chars(Id, IdCs),
    %% Build full frontmatter
    append("---\nid: \"", IdCs, T1),
    append(T1, "\"\n---\nBody text", FmChars),
    atom_chars(Content, FmChars),
    parse_frontmatter(Content, ParsedId, _, _, _),
    ParsedId == Id.

%% ============================================================
%% PROPERTIES: HTML GENERATION
%% ============================================================

%% Helper: check if list contains sublist (deterministic)
list_contains(List, Sub) :-
    append(Sub, _, List), !.
list_contains([_|T], Sub) :-
    list_contains(T, Sub).

%% Helper: check if atom contains substring
contains_substring(Atom, Sub) :-
    atom_codes(Atom, AtomCodes),
    atom_codes(Sub, SubCodes),
    list_contains(AtomCodes, SubCodes).

%% Property: html_head contains DOCTYPE
prop_html_head_has_doctype(Title) :-
    html_head(Title, Head),
    contains_substring(Head, '<!DOCTYPE html>').

%% Property: html_head contains title
prop_html_head_contains_title(Title) :-
    html_head(Title, Head),
    atom_concat('<title>', Title, T1),
    contains_substring(Head, T1).

%% Property: html_head is valid (contains </head>)
prop_html_head_closes(Title) :-
    html_head(Title, Head),
    contains_substring(Head, '</head>').

%% Property: html_body has required structure
prop_html_body_structure(Body, IdUp) :-
    html_body(Body, IdUp, Html),
    once(sub_atom(Html, _, _, _, '<body>')),
    once(sub_atom(Html, _, _, _, '</body>')),
    once(sub_atom(Html, _, _, _, '<article>')),
    once(sub_atom(Html, _, _, _, '</article>')).

%% Property: generate_html produces valid HTML
prop_generate_html_valid(Id, Title, Body) :-
    %% Setup chapter
    retractall(chapter(_, _, _, _)),
    assertz(chapter(Id, Title, chapter, Body)),
    %% Generate
    generate_html(Id, HTML),
    %% Validate (use once to avoid choice points)
    once(sub_atom(HTML, _, _, _, '<!DOCTYPE html>')),
    once(sub_atom(HTML, _, _, _, '</html>')),
    %% Cleanup
    retractall(chapter(_, _, _, _)).

%% ============================================================
%% PROPERTIES: GRAPH OPERATIONS
%% ============================================================

%% Property: links_to is symmetric with graph assertion
prop_links_to_asserted(From, To) :-
    retractall(links_to(_, _)),
    assertz(links_to(From, To)),
    links_to(From, To),
    retractall(links_to(_, _)).

%% Property: reachable is transitive
prop_reachable_transitive(A, B, C) :-
    retractall(links_to(_, _)),
    assertz(links_to(A, B)),
    assertz(links_to(B, C)),
    reachable(A, C),
    retractall(links_to(_, _)).

%% Property: path returns valid path
prop_path_valid(From, To) :-
    retractall(links_to(_, _)),
    assertz(links_to(From, To)),
    path(From, To, Path),
    Path = [From, To],
    retractall(links_to(_, _)).

%% ============================================================
%% TEST RUNNER
%% ============================================================

run_cyoa_qc_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  CYOA QUICKCHECK TESTS'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    %% Upcase tests
    write('─── Upcase Properties ───'), nl,
    run_prop('upcase produces uppercase', 50, 42, gen_mixed_case_str, prop_upcase_all_upper),
    run_prop('upcase is idempotent', 50, 12345, gen_mixed_case_str, prop_upcase_idempotent),
    run_prop('upcase preserves length', 50, 99999, gen_mixed_case_str, prop_upcase_preserves_length),

    %% DCG parser tests (testing fm_prop which is the actual working parser)
    nl, write('─── DCG Parser Properties ───'), nl,
    run_prop('fm_prop id parses', 50, 42, gen_lowercase_str, prop_fm_id_parses),
    run_prop('fm_prop title parses', 50, 12345, gen_title_str, prop_fm_title_parses),
    run_unit('collect_until_quote stops before quote', prop_collect_until_quote_stops),
    run_prop('parse_frontmatter extracts id', 30, 42, gen_lowercase_str, prop_parse_frontmatter_id),

    %% HTML generation tests
    nl, write('─── HTML Generation Properties ───'), nl,
    run_prop('html_head has DOCTYPE', 30, 42, gen_title_str, prop_html_head_has_doctype),
    run_prop('html_head contains title', 30, 12345, gen_title_str, prop_html_head_contains_title),
    run_prop('html_head closes properly', 30, 42, gen_title_str, prop_html_head_closes),
    run_prop_pair('html_body has structure', 20, 42, gen_title_str, gen_lowercase_str, prop_html_body_structure),

    %% Graph operation tests
    nl, write('─── Graph Properties ───'), nl,
    run_prop_pair('links_to asserted', 20, 42, gen_chapter_id, gen_chapter_id, prop_links_to_asserted),
    run_prop_triple('reachable is transitive', 15, 12345, gen_chapter_id, gen_chapter_id, gen_chapter_id, prop_reachable_transitive),
    run_prop_pair('path returns valid path', 20, 99999, gen_chapter_id, gen_chapter_id, prop_path_valid),

    %% Specific value tests
    nl, write('─── Unit Tests ───'), nl,
    test_val('upcase empty', prop_upcase_all_upper, ''),
    test_val('upcase lowercase', prop_upcase_all_upper, hello),
    test_val('upcase uppercase', prop_upcase_all_upper, 'HELLO'),
    test_val('upcase mixed', prop_upcase_all_upper, 'HeLLo'),
    test_html_gen('simple chapter', '00', 'Prologue', '<h1>Start</h1>'),
    test_html_gen('unicode title', 'intro', 'The Beginning', '<p>Hello</p>'),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  CYOA QUICKCHECK TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

%% Run property test with single generator
run_prop(Name, N, Seed, Gen, Prop) :-
    write('  '), write(Name), write('... '),
    run_prop_loop(N, Seed, Gen, Prop, 0, Passed),
    ( Passed = N ->
        write('✓ '), write(N), write(' passed'), nl
    ;
        Failed is N - Passed,
        write('✗ '), write(Failed), write('/'), write(N), write(' failed'), nl
    ).

run_prop_loop(0, _, _, _, P, P) :- !.
run_prop_loop(N, S0, Gen, Prop, Acc, Passed) :-
    N > 0,
    call(Gen, S0, S1, Val),
    ( catch(call(Prop, Val), _, fail) ->
        Acc1 is Acc + 1
    ;
        Acc1 = Acc
    ),
    N1 is N - 1,
    run_prop_loop(N1, S1, Gen, Prop, Acc1, Passed).

%% Run property test with two generators
run_prop_pair(Name, N, Seed, Gen1, Gen2, Prop) :-
    write('  '), write(Name), write('... '),
    run_prop_pair_loop(N, Seed, Gen1, Gen2, Prop, 0, Passed),
    ( Passed = N ->
        write('✓ '), write(N), write(' passed'), nl
    ;
        Failed is N - Passed,
        write('✗ '), write(Failed), write('/'), write(N), write(' failed'), nl
    ).

run_prop_pair_loop(0, _, _, _, _, P, P) :- !.
run_prop_pair_loop(N, S0, Gen1, Gen2, Prop, Acc, Passed) :-
    N > 0,
    call(Gen1, S0, S1, Val1),
    call(Gen2, S1, S2, Val2),
    ( catch(call(Prop, Val1, Val2), _, fail) ->
        Acc1 is Acc + 1
    ;
        Acc1 = Acc
    ),
    N1 is N - 1,
    run_prop_pair_loop(N1, S2, Gen1, Gen2, Prop, Acc1, Passed).

%% Run property test with three generators
run_prop_triple(Name, N, Seed, Gen1, Gen2, Gen3, Prop) :-
    write('  '), write(Name), write('... '),
    run_prop_triple_loop(N, Seed, Gen1, Gen2, Gen3, Prop, 0, Passed),
    ( Passed = N ->
        write('✓ '), write(N), write(' passed'), nl
    ;
        Failed is N - Passed,
        write('✗ '), write(Failed), write('/'), write(N), write(' failed'), nl
    ).

run_prop_triple_loop(0, _, _, _, _, _, P, P) :- !.
run_prop_triple_loop(N, S0, Gen1, Gen2, Gen3, Prop, Acc, Passed) :-
    N > 0,
    call(Gen1, S0, S1, Val1),
    call(Gen2, S1, S2, Val2),
    call(Gen3, S2, S3, Val3),
    ( catch(call(Prop, Val1, Val2, Val3), _, fail) ->
        Acc1 is Acc + 1
    ;
        Acc1 = Acc
    ),
    N1 is N - 1,
    run_prop_triple_loop(N1, S3, Gen1, Gen2, Gen3, Prop, Acc1, Passed).

%% Run single unit test
run_unit(Name, Goal) :-
    write('  '), write(Name), write('... '),
    ( catch(call(Goal), _, fail) ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

%% Test specific value
test_val(Name, Prop, Val) :-
    write('  '), write(Name), write('... '),
    ( catch(call(Prop, Val), _, fail) ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

%% Test HTML generation with specific values
test_html_gen(Name, Id, Title, Body) :-
    write('  '), write(Name), write('... '),
    ( catch(prop_generate_html_valid(Id, Title, Body), _, fail) ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

%% ============================================================
%% EXTENDED TESTS - MARKDOWN CONVERSION
%% ============================================================

run_md_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  MARKDOWN CONVERSION TESTS'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    write('─── Header Conversion ───'), nl,
    test_md('h1 header', "# Hello\n", "<h1>Hello</h1>\n"),
    test_md('h2 header', "## World\n", "<h2>World</h2>\n"),
    test_md('h3 header', "### Test\n", "<h3>Test</h3>\n"),

    nl, write('─── Block Elements ───'), nl,
    test_md('horizontal rule', "---\n", "<hr>\n"),
    test_md('list item', "- Item\n", "<li>Item</li>\n"),
    test_md('blockquote', "> Quote\n", "<blockquote>Quote</blockquote>\n"),

    nl, write('─── Inline Elements ───'), nl,
    test_md_contains('link converts', "Click [here](#chapter)\n", ".html"),
    test_md_contains('bold converts', "This is **bold** text\n", "<strong>"),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  MARKDOWN TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

test_md(Name, Input, Expected) :-
    write('  '), write(Name), write('... '),
    atom_chars(InputAtom, Input),
    ( catch(md_to_html(InputAtom, Output), _, fail),
      atom_chars(Output, OutputChars),
      OutputChars = Expected ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

test_md_contains(Name, Input, Substring) :-
    write('  '), write(Name), write('... '),
    atom_chars(InputAtom, Input),
    atom_chars(SubAtom, Substring),
    ( catch(md_to_html(InputAtom, Output), _, fail),
      contains_substring(Output, SubAtom) ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

%% ============================================================
%% FRONTMATTER PARSING TESTS
%% ============================================================

run_frontmatter_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  FRONTMATTER PARSING TESTS'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    write('─── Basic Frontmatter ───'), nl,
    test_fm('parses id', '---\nid: "test"\n---\nBody', test, _, _, _),
    test_fm('parses title', '---\nid: "ch01"\ntitle: "Chapter One"\n---\n', _, 'Chapter One', _, _),
    test_fm('parses type', '---\nid: "end"\ntype: "ending"\n---\n', _, _, ending, _),
    test_fm('parses next list', '---\nid: "ch01"\nnext: ["ch02", "ch03"]\n---\n', _, _, _, ['ch02', 'ch03']),

    nl, write('─── Edge Cases ───'), nl,
    test_fm_invalid('no frontmatter', 'Just body text'),
    test_fm_default('missing title', '---\nid: "x"\n---\n', 'Untitled'),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  FRONTMATTER TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

test_fm(Name, Content, ExpId, ExpTitle, ExpType, ExpNext) :-
    write('  '), write(Name), write('... '),
    ( catch(parse_frontmatter(Content, Id, Title, Type, Next), _, fail),
      ( var(ExpId) ; Id = ExpId ),
      ( var(ExpTitle) ; Title = ExpTitle ),
      ( var(ExpType) ; Type = ExpType ),
      ( var(ExpNext) ; Next = ExpNext ) ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

test_fm_invalid(Name, Content) :-
    write('  '), write(Name), write('... '),
    ( catch(parse_frontmatter(Content, Id, _, _, _), _, fail),
      Id = unknown ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

test_fm_default(Name, Content, ExpTitle) :-
    write('  '), write(Name), write('... '),
    ( catch(parse_frontmatter(Content, _, Title, _, _), _, fail),
      Title = ExpTitle ->
        write('✓'), nl
    ;
        write('✗'), nl
    ).

%% ============================================================
%% FULL TEST SUITE
%% ============================================================

run_all_tests :-
    run_cyoa_qc_tests,
    nl,
    run_md_tests,
    nl,
    run_frontmatter_tests.

%% Entry point
test_cyoa_qc :- run_all_tests.

%% ============================================================
%% STRESS TESTS
%% ============================================================

run_stress_tests :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  STRESS TESTS'), nl,
    write('═══════════════════════════════════════════════════════'), nl, nl,

    write('─── High Volume ───'), nl,
    run_prop('200 upcase tests', 200, 42, gen_mixed_case_str, prop_upcase_all_upper),
    run_prop('100 fm_id tests', 100, 12345, gen_lowercase_str, prop_fm_id_parses),
    run_prop('100 html_head tests', 100, 99999, gen_title_str, prop_html_head_has_doctype),

    nl, write('═══════════════════════════════════════════════════════'), nl,
    write('  STRESS TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════════════════'), nl.
