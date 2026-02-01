%% CYOA Static Site Generator - THE COLLAPSE
%% ==========================================
%% pro log sem | dcg prs | fac grf | htm gen
%% *Prolog semantics. DCG parsing. Fact graph. HTML generation.*
%%
%% Pure Prolog implementation using Scryer Prolog.
%% Unification = key-collapse. Facts = superposed state.

:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).

%% forall/2 - not in Scryer by default
forall(Cond, Action) :-
    \+ (Cond, \+ Action).

%% upcase_atom/2 - convert atom to uppercase
upcase_atom(Atom, Upper) :-
    atom_chars(Atom, Chars),
    maplist(upcase_char, Chars, UpperChars),
    atom_chars(Upper, UpperChars).

upcase_char(C, U) :-
    char_code(C, Code),
    ( Code >= 97, Code =< 122 ->
        UpperCode is Code - 32,
        char_code(U, UpperCode)
    ;
        U = C
    ).

%% ============================================================
%% CHAPTER DATABASE (Dynamic facts)
%% ============================================================

:- dynamic(chapter/4).      % chapter(Id, Title, Type, Body)
:- dynamic(links_to/2).     % links_to(From, To)

%% ============================================================
%% DCG: YAML PROPERTY PARSERS
%% ============================================================

yaml_id(Id) --> "id: \"", string_until_dquote(Cs), "\"",
    { atom_chars(Id, Cs) }.

yaml_title(Title) --> "title: \"", string_until_dquote(Cs), "\"",
    { atom_chars(Title, Cs) }.

%% String until double quote character
string_until_dquote([]) --> "\"", !.  % lookahead
string_until_dquote([]) --> [].
string_until_dquote([C|Cs]) --> [C], { C \= '"' }, string_until_dquote(Cs).

%% Generic: collect until stop char
chars_to([], Stop) --> [Stop].
chars_to([C|Cs], Stop) --> [C], { C \= Stop }, chars_to(Cs, Stop).

%% ============================================================
%% DCG: MARKDOWN LINK PARSER
%% ============================================================

md_link(Title, Anchor) -->
    "[", chars_to(TCs, 93), "]",
    "(#", chars_to(ACs, 41), ")",
    { atom_chars(Title, TCs), atom_chars(Anchor, ACs) }.

%% ============================================================
%% CHAPTER GRAPH - The Prolog Way
%% ============================================================

show_graph :-
    nl, write('◢ CHAPTER GRAPH'), nl,
    forall(
        links_to(From, To),
        ( chapter(To, _, _, _) ->
            format("  ~w → ~w~n", [From, To])
        ;
            format("  ~w ⇢ ~w (future)~n", [From, To])
        )
    ).

%% Transitive closure: all reachable chapters
reachable(Start, End) :- links_to(Start, End).
reachable(Start, End) :- links_to(Start, Mid), reachable(Mid, End).

%% Path finding with backtracking
path(From, To, [From, To]) :- links_to(From, To).
path(From, To, [From|Rest]) :-
    links_to(From, Mid),
    path(Mid, To, Rest).

%% ============================================================
%% HTML GENERATION (atom concatenation)
%% ============================================================

%% Build HTML from parts - with hidden translation support
html_head(Title, Head) :-
    atom_concat('<!DOCTYPE html><html><head><meta charset="UTF-8"><title>', Title, P1),
    atom_concat(P1, ' | THE COLLAPSE</title>', P2),
    atom_concat(P2, '<style>', P3),
    % CSS variables
    atom_concat(P3, ':root{--bg:#0a0a0a;--fg:#c0c0c0;--amber:#d4a017;--amber-dim:#8b6914;--green:#00cc66}', P4),
    % Body
    atom_concat(P4, 'body{background:var(--bg);color:var(--fg);font-family:monospace;max-width:900px;margin:0 auto;padding:2rem;line-height:1.8}', P5),
    % Headers
    atom_concat(P5, 'h1,h2{color:var(--amber);text-transform:uppercase;letter-spacing:2px}', P6),
    % Limn transmission blocks
    atom_concat(P6, '.transmission{background:#0f0f0f;border:1px solid var(--amber-dim);margin:1.5rem 0;position:relative}', P7),
    atom_concat(P7, '.transmission-header{background:var(--amber-dim);color:var(--bg);padding:0.3rem 1rem;font-size:0.8rem;letter-spacing:3px;text-align:center}', P8),
    atom_concat(P8, '.limn-code{padding:1rem;color:var(--amber);font-size:1.1rem}', P9),
    % Hidden translation - revealed on hover
    atom_concat(P9, '.translation{max-height:0;overflow:hidden;opacity:0;transition:all 0.3s;color:var(--fg);font-size:0.9rem;padding:0 1rem;border-top:1px dashed #333}', P10),
    atom_concat(P10, '.transmission:hover .translation{max-height:200px;opacity:0.7;padding:0.5rem 1rem}', P11),
    % Toggle button
    atom_concat(P11, '.reveal-btn{position:absolute;top:0.3rem;right:0.5rem;background:none;border:none;color:var(--amber-dim);cursor:pointer;font-size:0.7rem}', P12),
    % Navigation
    atom_concat(P12, '.nav{margin-top:2rem;border-top:1px solid #404040;padding:1rem}', P13),
    atom_concat(P13, '.nav a{color:var(--amber-dim);text-decoration:none}', P14),
    atom_concat(P14, '.nav a:hover{color:var(--amber)}', P15),
    % Choice blocks
    atom_concat(P15, '.choice{background:#0f0f0f;border-left:3px solid var(--amber);margin:1rem 0;padding:1rem}', P16),
    atom_concat(P16, '.choice a{color:var(--amber);text-decoration:none;font-weight:bold}', P17),
    % Images
    atom_concat(P17, '.chapter-img{max-width:100%;height:auto;margin:1.5rem 0;border:1px solid var(--amber-dim);opacity:0.9}', P18),
    % Visited links (for navigation clarity)
    atom_concat(P18, 'a:visited{color:#666}blockquote a:visited{color:#997a00}', P19),
    atom_concat(P19, '</style></head>', Head).

html_body(Body, IdUp, BodyHtml) :-
    atom_concat('<body><article>', Body, P1),
    atom_concat(P1, '</article><nav class="nav"><a href="index.html">← Start</a></nav>', P2),
    atom_concat(P2, '<div style="position:fixed;bottom:0;left:0;right:0;background:#0a0a0a;border-top:1px solid #8b6914;padding:0.5rem;color:#8b6914">CHAPTER: ', P3),
    atom_concat(P3, IdUp, P4),
    atom_concat(P4, ' | THE COLLAPSE</div></body></html>', BodyHtml).

generate_html(Id, HTML) :-
    chapter(Id, Title, _Type, Body),
    upcase_atom(Id, IdUp),
    html_head(Title, Head),
    html_body(Body, IdUp, BodyPart),
    atom_concat(Head, BodyPart, HTML).

%% ============================================================
%% TEST
%% ============================================================

test :-
    nl, write('▌ PROLOG CYOA GENERATOR TEST ▐'), nl, nl,

    retractall(chapter(_, _, _, _)),
    retractall(links_to(_, _)),

    write('Loading test chapters...'), nl,
    assertz(chapter('00', 'Prologue', chapter, '<h1>THE COLLAPSE</h1><p>Message arrived at 03:47.</p>')),
    assertz(chapter('01a', 'The Rookie', chapter, '<h1>Rookie</h1><p>You recognize Rook.</p>')),
    assertz(chapter('01b', 'The Doubt', chapter, '<h1>Doubt</h1><p>Bishop breaks silence.</p>')),
    assertz(chapter('01c', 'The Trap', chapter, '<h1>Trap</h1><p>Enemy probe.</p>')),
    write('  ✓ 4 chapters'), nl,

    assertz(links_to('00', '01a')),
    assertz(links_to('00', '01b')),
    assertz(links_to('00', '01c')),
    assertz(links_to('01a', '02a')),
    assertz(links_to('01a', '02b')),
    assertz(links_to('01b', '02c')),
    write('  ✓ 6 links'), nl,

    show_graph,

    nl, write('◢ REACHABLE FROM 00'), nl,
    forall(reachable('00', X), (write('  '), write(X), nl)),

    nl, write('◢ PATHS: 00 → 02a'), nl,
    forall(path('00', '02a', P), (write('  '), write(P), nl)),

    nl, write('◢ HTML GENERATION'), nl,
    generate_html('00', HTML),
    sub_atom(HTML, 0, 80, _, Preview),
    write('  Preview: '), write(Preview), write('...'), nl,

    nl, write('◢ TEST COMPLETE ✓'), nl.

main :- test.

%% ============================================================
%% FILE LOADING
%% ============================================================

%% Read file to atom
read_file(Path, Content) :-
    open(Path, read, Stream),
    read_stream_chars(Stream, Chars),
    close(Stream),
    atom_chars(Content, Chars).

read_stream_chars(Stream, []) :-
    at_end_of_stream(Stream), !.
read_stream_chars(Stream, [C|Cs]) :-
    get_char(Stream, C),
    read_stream_chars(Stream, Cs).

%% Parse frontmatter from content
parse_frontmatter(Content, Id, Title, Type, Next) :-
    atom_chars(Content, Chars),
    ( phrase(frontmatter(Props), Chars, _Rest) ->
        get_prop(id, Props, Id, unknown),
        get_prop(title, Props, Title, 'Untitled'),
        get_prop(type, Props, Type, chapter),
        get_prop(next, Props, Next, [])
    ;
        Id = unknown, Title = 'Untitled', Type = chapter, Next = []
    ).

get_prop(Key, Props, Val, _) :- member(Key-Val, Props), !.
get_prop(_, _, Default, Default).

%% Simple frontmatter DCG
frontmatter(Props) -->
    "---", whites, newline,
    fm_props(Props),
    "---".

fm_props([Prop|Props]) -->
    fm_prop(Prop), whites, newline,
    fm_props(Props).
fm_props([]) --> [].

fm_prop(id-Id) -->
    "id:", whites, "\"", collect_until_quote(Cs), "\"",
    { atom_chars(Id, Cs) }.
fm_prop(title-Title) -->
    "title:", whites, "\"", collect_until_quote(Cs), "\"",
    { atom_chars(Title, Cs) }.
fm_prop(type-Type) -->
    "type:", whites, "\"", collect_until_quote(Cs), "\"",
    { atom_chars(Type, Cs) }.
fm_prop(next-Ids) -->
    "next:", whites, yaml_list(Ids).
fm_prop(prev-Ids) -->
    "prev:", whites, yaml_list(Ids).

%% Collect chars until quote, leaving quote unconsumed
collect_until_quote([C|Cs]) --> [C], { C \= '"' }, !, collect_until_quote(Cs).
collect_until_quote([]) --> [].

yaml_list([]) --> "[]".
yaml_list(Ids) --> "[", yaml_items(Ids), "]".

yaml_items([Id|Ids]) -->
    whites, "\"", collect_until_quote(Cs), "\"", whites,
    { atom_chars(Id, Cs) },
    yaml_items_rest(Ids).
yaml_items([]) --> whites.

yaml_items_rest([Id|Ids]) -->
    ",", whites, "\"", collect_until_quote(Cs), "\"", whites,
    { atom_chars(Id, Cs) },
    yaml_items_rest(Ids).
yaml_items_rest([]) --> [].

whites --> " ", whites.
whites --> [].

newline --> "\n".
newline --> "\r\n".

%% Strip YAML frontmatter from content
strip_frontmatter(Content, Body) :-
    atom_chars(Content, Chars),
    ( phrase(skip_frontmatter(BodyChars), Chars) ->
        atom_chars(Body, BodyChars)
    ;
        Body = Content
    ).

skip_frontmatter(Body) -->
    "---", skip_to_newline,
    skip_to_second_dashes,
    rest_chars(Body).

skip_to_newline --> "\n", !.
skip_to_newline --> [_], skip_to_newline.

skip_to_second_dashes --> "\n---", skip_to_newline, !.
skip_to_second_dashes --> [_], skip_to_second_dashes.

rest_chars([C|Cs]) --> [C], rest_chars(Cs).
rest_chars([]) --> [].

%% Simple markdown to HTML conversion
md_to_html(Md, Html) :-
    atom_chars(Md, MdChars),
    phrase(convert_md(HtmlChars), MdChars),
    atom_chars(Html, HtmlChars).

convert_md(Html) -->
    md_line(Line),
    convert_md(Rest),
    { append(Line, Rest, Html) }.
convert_md([]) --> [].

%% Convert markdown lines
md_line(Html) -->
    "# ", chars_to_newline(Title), "\n",
    { append("<h1>", Title, T1), append(T1, "</h1>\n", Html) }.
md_line(Html) -->
    "## ", chars_to_newline(Title), "\n",
    { append("<h2>", Title, T1), append(T1, "</h2>\n", Html) }.
md_line(Html) -->
    "### ", chars_to_newline(Title), "\n",
    { append("<h3>", Title, T1), append(T1, "</h3>\n", Html) }.
%% Limn block with optional hidden translation
%% Format: ```limn\ncode\n```\n*translation*  OR  <!-- translation -->
md_line(Html) -->
    "```limn\n",
    !,  % commit to this rule once we match opening
    chars_to_triple_backtick(Code),  % this consumes the closing ```
    optional_newline_translation(Trans),
    { build_limn_block(Code, Trans, Html) }.
md_line(Html) -->
    "---\n",
    { Html = "<hr>\n" }.
md_line(Html) -->
    "- ", chars_to_newline(Item), "\n",
    { convert_inline_md(Item, ConvItem),
      append("<li>", ConvItem, T1), append(T1, "</li>\n", Html) }.
md_line(Html) -->
    "> ", chars_to_newline(Quote), "\n",
    { convert_inline_md(Quote, ConvQuote),
      append("<blockquote>", ConvQuote, T1), append(T1, "</blockquote>\n", Html) }.
md_line(Html) -->
    "\n",
    { Html = "<br>\n" }.
md_line(Html) -->
    [C],
    { C \= '\n' },
    chars_to_newline(Rest0),
    "\n",
    { convert_inline_md([C|Rest0], Conv),
      append(Conv, "\n", Html) }.
md_line([C]) -->
    [C].

%% Convert inline markdown (links, bold) to HTML
convert_inline_md(In, Out) :-
    phrase(inline_md(OutChars), In),
    append(OutChars, [], Out).

%% DCG for inline markdown conversion
%% Process character by character, detecting patterns

%% Images: ![alt](src)
inline_md(Out) -->
    "![", chars_until_close_bracket(Alt), "](", chars_until_close_paren(Src), ")",
    !,
    { append("<img src=\"", Src, T1),
      append(T1, "\" alt=\"", T2),
      append(T2, Alt, T3),
      append(T3, "\" class=\"chapter-img\">", Img) },
    inline_md(Rest),
    { append(Img, Rest, Out) }.

%% Links: [text](#id) or [text](url)
inline_md(Out) -->
    "[", chars_until_close_bracket(Text), "](#", chars_until_close_paren(Id), ")",
    !,
    { append("<a href=\"", Id, T1),
      append(T1, ".html\">", T2),
      append(T2, Text, T3),
      append(T3, "</a>", Link) },
    inline_md(Rest),
    { append(Link, Rest, Out) }.
inline_md(Out) -->
    "**",
    !,
    chars_until_double_star(Inner),
    { convert_inline_md(Inner, ConvInner),
      append("<strong>", ConvInner, T1),
      append(T1, "</strong>", Bold) },
    inline_md(Rest),
    { append(Bold, Rest, Out) }.
inline_md([C|Rest]) -->
    [C],
    inline_md(Rest).
inline_md([]) --> [].

%% Helper DCGs for inline parsing
chars_until_close_bracket([C|Cs]) --> [C], { not_close_bracket(C) }, chars_until_close_bracket(Cs).
chars_until_close_bracket([]) --> [].

chars_until_close_paren([C|Cs]) --> [C], { not_close_paren(C) }, chars_until_close_paren(Cs).
chars_until_close_paren([]) --> [].

not_close_bracket(C) :- char_code(C, Code), Code =\= 93.  % ]
not_close_paren(C) :- char_code(C, Code), Code =\= 41.    % )

chars_until_double_star([]) --> "**", !.
chars_until_double_star([C|Cs]) --> [C], chars_until_double_star(Cs).

chars_until_single_star([]) --> "*", !.
chars_until_single_star([C|Cs]) --> [C], { not_asterisk(C) }, chars_until_single_star(Cs).

%% Helper for limn blocks with/without newline before translation
optional_newline_translation(Trans) --> "\n", optional_translation(Trans).
optional_newline_translation([]) --> [].

%% Optional translation after limn block
optional_translation(Trans) -->
    "*", chars_until_asterisk(Trans), "*\n", !.
optional_translation(Trans) -->
    "<!-- ", chars_until_close_comment(Trans), " -->\n", !.
optional_translation([]) --> [].

%% Collect chars until * (for translation parsing) - does NOT consume *
chars_until_asterisk([C|Cs]) --> [C], { not_asterisk(C) }, chars_until_asterisk(Cs).
chars_until_asterisk([]) --> [].

not_asterisk(C) :- char_code(C, Code), Code =\= 42.

chars_until_close_comment([]) --> " -->", !.
chars_until_close_comment([C|Cs]) --> [C], chars_until_close_comment(Cs).

%% Build limn block HTML with hidden translation
build_limn_block(Code, [], Html) :-
    append("<div class=\"transmission\"><div class=\"transmission-header\">▌ INTERCEPTED ▐</div><pre class=\"limn-code\"><code>", Code, T1),
    append(T1, "</code></pre></div>\n", Html).
build_limn_block(Code, Trans, Html) :-
    Trans \= [],
    append("<div class=\"transmission\"><div class=\"transmission-header\">▌ INTERCEPTED ▐</div><pre class=\"limn-code\"><code>", Code, T1),
    append(T1, "</code></pre><div class=\"translation\">", T2),
    append(T2, Trans, T3),
    append(T3, "</div></div>\n", Html).

%% Collect chars until newline WITHOUT consuming the newline
chars_to_newline([C|Cs]) --> [C], { C \= '\n' }, !, chars_to_newline(Cs).
chars_to_newline([]) --> [].

chars_to_triple_backtick([]) --> "```", !.
chars_to_triple_backtick([C|Cs]) --> [C], chars_to_triple_backtick(Cs).

%% Load a chapter file and assert facts
load_chapter(Path) :-
    read_file(Path, Content),
    parse_frontmatter(Content, Id, Title, Type, Next),
    ( Id \= unknown ->
        strip_frontmatter(Content, RawBody),
        md_to_html(RawBody, HtmlBody),
        assertz(chapter(Id, Title, Type, HtmlBody)),
        forall(member(N, Next), assertz(links_to(Id, N))),
        format("  ✓ ~w: ~w~n", [Id, Title])
    ;
        format("  ⚠ No frontmatter: ~w~n", [Path])
    ).

%% ============================================================
%% FILE I/O
%% ============================================================

%% Write HTML to file
write_html_file(Dir, Id, HTML) :-
    atom_concat(Dir, '/', P1),
    atom_concat(P1, Id, P2),
    atom_concat(P2, '.html', Filename),
    open(Filename, write, Stream),
    write(Stream, HTML),
    close(Stream),
    format("  ✓ ~w.html~n", [Id]).

%% Generate all chapters to files
generate_all(OutputDir) :-
    nl, write('◢ GENERATING HTML'), nl,
    forall(
        chapter(Id, _, _, _),
        ( generate_html(Id, HTML),
          write_html_file(OutputDir, Id, HTML) )
    ),
    nl, write('◢ COMPLETE'), nl.

%% Generate index.html
generate_index(OutputDir) :-
    atom_concat(OutputDir, '/index.html', IndexPath),
    open(IndexPath, write, S),
    write(S, '<!DOCTYPE html><html><head><meta charset="UTF-8">'),
    write(S, '<meta http-equiv="refresh" content="0; url=00.html">'),
    write(S, '<title>THE COLLAPSE</title>'),
    write(S, '<style>body{background:#0a0a0a;color:#d4a017;font-family:monospace;padding:2rem;text-align:center}</style>'),
    write(S, '</head><body>'),
    write(S, '<h1>THE COLLAPSE</h1>'),
    write(S, '<p>A Limn Spy Thriller</p>'),
    write(S, '<p><a href="00.html" style="color:#d4a017">Enter</a></p>'),
    write(S, '</body></html>'),
    close(S),
    write('  ✓ index.html'), nl.

%% Validate all links point to existing chapters
validate_links :-
    nl, write('◢ VALIDATING LINKS'), nl,
    findall(To, (links_to(_, To), \+ chapter(To, _, _, _)), DeadLinks),
    sort(DeadLinks, UniqueDeadLinks),
    ( UniqueDeadLinks = [] ->
        write('  ✓ All links valid'), nl
    ;
        write('  ✗ DEAD LINKS FOUND:'), nl,
        forall(member(D, UniqueDeadLinks), (write('    → '), write(D), write('.html'), nl)),
        write('  ⚠ These chapters are referenced but not loaded!'), nl,
        fail
    ).

%% Full site generation entry point
generate_site(OutputDir) :-
    nl, write('▌ PROLOG CYOA GENERATOR ▐'), nl, nl,
    show_graph,
    validate_links,
    generate_all(OutputDir),
    generate_index(OutputDir).
