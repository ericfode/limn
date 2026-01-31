#!/usr/bin/env python3
"""
CYOA Static Site Generator - THE COLLAPSE
==========================================
pro log sem | prs yam chp | gen htm | int trn aes
*Prolog semantics. Parse YAML chapters. Generate HTML. Intercepted transmission aesthetic.*

Uses pyDatalog for Limn-like fact/query structure.
"""

import os
import re
import sys
import yaml
from pathlib import Path
from pyDatalog import pyDatalog

# --- Prolog-style fact database ---
pyDatalog.create_terms('chapter, links_to, is_ending, title_of, type_of, X, Y')

# Initialize predicates
+chapter('__init__')
+links_to('__init__', '__init__')
+is_ending('__init__')
+title_of('__init__', '__init__')
+type_of('__init__', 'init')

# --- YAML + Markdown parsing ---

def parse_chapter(content: str) -> dict:
    """Parse chapter with YAML frontmatter."""
    # Extract YAML frontmatter
    frontmatter = {}
    body = content

    if content.startswith('---'):
        parts = content.split('---', 2)
        if len(parts) >= 3:
            try:
                frontmatter = yaml.safe_load(parts[1]) or {}
            except yaml.YAMLError:
                pass
            body = parts[2]

    return {
        'id': frontmatter.get('id', ''),
        'title': frontmatter.get('title', extract_title(body)),
        'type': frontmatter.get('type', 'chapter'),
        'prev': frontmatter.get('prev', []),
        'next': frontmatter.get('next', []),
        'body': body
    }

def extract_title(content: str) -> str:
    """Extract H1 or H2 title from markdown."""
    match = re.search(r'^##?\s+(.+)$', content, re.MULTILINE)
    return match.group(1) if match else "Untitled"

def extract_links(content: str) -> list[tuple[str, str]]:
    """Extract choice links from markdown blockquotes."""
    links = []
    # Pattern: [Title](#id) in choice blockquotes
    anchor_pattern = r'\[([^\]]+)\]\(#([^)]+)\)'
    for match in re.finditer(anchor_pattern, content):
        display = match.group(1).strip()
        anchor_id = match.group(2).strip()
        links.append((anchor_id, display))
    return links

def highlight_limn(code: str) -> str:
    """Limn syntax highlighting - amber/gold intercepted transmission."""
    code = code.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    # Pipes = transmission separators
    code = re.sub(r'\|', '<span class="limn-pipe">│</span>', code)
    # Tone markers = priority flags
    code = re.sub(r'\b(urj|hes|cer|dub|rev|lit|iro)\b',
                  r'<span class="limn-tone">\1</span>', code)
    return code

def markdown_to_html(content: str) -> str:
    """Convert markdown to HTML with Limn highlighting."""
    # Process limn blocks - make them look like intercepted transmissions
    def replace_limn(match):
        code = match.group(1)
        highlighted = highlight_limn(code)
        return f'''<div class="transmission">
<div class="transmission-header">▌ INTERCEPTED TRANSMISSION ▐</div>
<pre class="limn-code"><code>{highlighted}</code></pre>
</div>'''

    content = re.sub(r'```limn\n(.*?)```', replace_limn, content, flags=re.DOTALL)

    # Headers
    content = re.sub(r'^### (.+)$', r'<h3>\1</h3>', content, flags=re.MULTILINE)
    content = re.sub(r'^## (.+)$', r'<h2>\1</h2>', content, flags=re.MULTILINE)
    content = re.sub(r'^# (.+)$', r'<h1>\1</h1>', content, flags=re.MULTILINE)

    # Bold / Italic
    content = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', content)
    content = re.sub(r'\*(.+?)\*', r'<em>\1</em>', content)

    # Horizontal rules
    content = re.sub(r'^---+$', r'<hr>', content, flags=re.MULTILINE)

    # Lists
    content = re.sub(r'^- (.+)$', r'<li>\1</li>', content, flags=re.MULTILINE)

    # Blockquotes (choices) - special styling
    def format_blockquote(match):
        bq_content = match.group(1)
        # Remove > prefixes
        lines = [line.lstrip('> ').rstrip() for line in bq_content.split('\n')]
        inner = '\n'.join(lines)
        return f'<blockquote class="choice-block">{inner}</blockquote>'

    content = re.sub(r'((?:^>.*\n?)+)', format_blockquote, content, flags=re.MULTILINE)

    # Paragraphs
    lines = content.split('\n')
    result = []
    in_para = False
    for line in lines:
        stripped = line.strip()
        if not stripped:
            if in_para:
                result.append('</p>')
                in_para = False
            result.append('')
        elif stripped.startswith('<'):
            if in_para:
                result.append('</p>')
                in_para = False
            result.append(line)
        else:
            if not in_para:
                result.append('<p>')
                in_para = True
            result.append(line)
    if in_para:
        result.append('</p>')

    return '\n'.join(result)

# --- CSS: Intercepted Transmission Aesthetic ---

CSS = '''
:root {
    --bg: #0a0a0a;
    --fg: #c0c0c0;
    --amber: #d4a017;
    --amber-dim: #8b6914;
    --amber-glow: #ffd700;
    --green: #00cc66;
    --red: #cc3333;
    --muted: #404040;
    --code-bg: #0f0f0f;
}

@font-face {
    font-family: 'Terminal';
    src: local('Courier New'), local('Consolas'), local('monospace');
}

* { box-sizing: border-box; }

body {
    background: var(--bg);
    color: var(--fg);
    font-family: 'Courier New', Consolas, monospace;
    max-width: 900px;
    margin: 0 auto;
    padding: 2rem;
    line-height: 1.8;
    font-size: 16px;
}

/* CRT scanline effect - subtle */
body::before {
    content: "";
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    pointer-events: none;
    background: repeating-linear-gradient(
        0deg,
        rgba(0, 0, 0, 0.1),
        rgba(0, 0, 0, 0.1) 1px,
        transparent 1px,
        transparent 2px
    );
    z-index: 1000;
}

h1, h2, h3 {
    color: var(--amber);
    text-transform: uppercase;
    letter-spacing: 2px;
    border-bottom: 1px solid var(--amber-dim);
    padding-bottom: 0.5rem;
    text-shadow: 0 0 10px var(--amber-dim);
}

h1 { font-size: 1.8rem; }
h2 { font-size: 1.4rem; margin-top: 2rem; }
h3 { font-size: 1.1rem; color: var(--green); }

/* Intercepted Transmission blocks */
.transmission {
    background: var(--code-bg);
    border: 1px solid var(--amber-dim);
    margin: 1.5rem 0;
    position: relative;
}

.transmission-header {
    background: var(--amber-dim);
    color: var(--bg);
    padding: 0.3rem 1rem;
    font-size: 0.8rem;
    letter-spacing: 3px;
    text-align: center;
}

.limn-code {
    padding: 1rem 1.5rem;
    margin: 0;
    overflow-x: auto;
    font-size: 1.1rem;
}

.limn-code code {
    font-family: 'Courier New', Consolas, monospace;
    color: var(--amber);
}

.limn-pipe {
    color: var(--amber-glow);
    font-weight: bold;
    text-shadow: 0 0 5px var(--amber);
}

.limn-tone {
    color: var(--green);
    font-style: italic;
    text-shadow: 0 0 5px var(--green);
}

hr {
    border: none;
    border-top: 1px dashed var(--muted);
    margin: 2rem 0;
}

/* Choice blocks - classified document aesthetic */
.choice-block {
    background: linear-gradient(135deg, #0f0f0f 0%, #1a1a1a 100%);
    border-left: 3px solid var(--amber);
    margin: 1rem 0;
    padding: 1rem 1.5rem;
    position: relative;
}

.choice-block::before {
    content: "◢ OPTION";
    position: absolute;
    top: -0.5rem;
    left: 1rem;
    background: var(--bg);
    color: var(--amber-dim);
    font-size: 0.7rem;
    padding: 0 0.5rem;
    letter-spacing: 2px;
}

.choice-block a {
    color: var(--amber);
    text-decoration: none;
    font-weight: bold;
}

.choice-block a:hover {
    color: var(--amber-glow);
    text-shadow: 0 0 10px var(--amber);
}

.choice-block .transmission {
    margin: 0.5rem 0;
    border-color: var(--muted);
}

.choice-block .transmission-header {
    background: var(--muted);
    font-size: 0.7rem;
}

/* Navigation */
.nav {
    margin-top: 3rem;
    padding: 1rem;
    border-top: 1px solid var(--muted);
    font-size: 0.9rem;
    color: var(--muted);
    display: flex;
    justify-content: space-between;
}

.nav a {
    color: var(--amber-dim);
    text-decoration: none;
}

.nav a:hover {
    color: var(--amber);
}

/* Status bar */
.status-bar {
    position: fixed;
    bottom: 0;
    left: 0;
    right: 0;
    background: var(--bg);
    border-top: 1px solid var(--amber-dim);
    padding: 0.5rem 2rem;
    font-size: 0.8rem;
    color: var(--amber-dim);
    display: flex;
    justify-content: space-between;
}

/* Endings */
.ending {
    border: 2px solid;
    padding: 2rem;
    margin: 2rem 0;
    text-align: center;
}
.ending-victory { border-color: var(--green); }
.ending-failure { border-color: var(--red); }
.ending-ambiguous { border-color: var(--amber); }

strong { color: var(--amber); }
em { color: var(--fg); font-style: italic; }

/* Lists */
li {
    list-style: none;
    padding-left: 1.5rem;
    position: relative;
}
li::before {
    content: "▸";
    position: absolute;
    left: 0;
    color: var(--amber-dim);
}
'''

def generate_html(chapter_id: str, data: dict, all_chapters: dict) -> str:
    """Generate complete HTML page for a chapter."""
    html_content = markdown_to_html(data['body'])
    chapter_type = data.get('type', 'chapter')

    # Resolve navigation links
    prev_links = data.get('prev', [])
    next_ids = data.get('next', [])

    # Build prev nav
    prev_html = ''
    if prev_links:
        for pid in prev_links:
            resolved = resolve_id(pid, all_chapters)
            if resolved:
                prev_html += f'<a href="{resolved}.html">← Back</a> '

    # Ending styling
    ending_class = ''
    if chapter_type.startswith('ending'):
        ending_class = f'ending {chapter_type.replace("ending-", "ending-")}'

    status_text = f'CHAPTER: {chapter_id.upper()} | STATUS: ACTIVE | NETWORK: NINTH'
    if 'victory' in chapter_type:
        status_text = f'MISSION: COMPLETE | OUTCOME: SUCCESS | FILE CLOSED'
    elif 'failure' in chapter_type:
        status_text = f'MISSION: FAILED | NETWORK: COMPROMISED | FILE CLOSED'

    return f'''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{data['title']} | THE COLLAPSE</title>
    <style>{CSS}</style>
</head>
<body>
    <article class="{ending_class}">
        {html_content}
    </article>
    <nav class="nav">
        <span>{prev_html or '<a href="index.html">← Start</a>'}</span>
        <span><a href="glossary.html">◈ Glossary</a></span>
    </nav>
    <div class="status-bar">
        <span>{status_text}</span>
        <span>▌THE COLLAPSE▐</span>
    </div>
</body>
</html>
'''

def resolve_id(target_id: str, all_chapters: dict) -> str:
    """Resolve short ID to full chapter filename."""
    if target_id in all_chapters:
        return target_id
    # Try prefix match
    for cid in all_chapters:
        if cid.startswith(target_id) or cid.split('-')[0] == target_id:
            return cid
    return target_id  # Return as-is, might be future chapter

def load_chapters(source_dir: Path) -> dict:
    """Load all chapter markdown files."""
    chapters = {}

    for subdir in ['chapters', 'endings', '.']:
        search_dir = source_dir / subdir if subdir != '.' else source_dir
        if not search_dir.exists():
            continue

        for md_file in search_dir.glob('*.md'):
            if md_file.name in ['SPEC.md', 'README.md']:
                # Still load README/glossary
                if md_file.name == 'README.md':
                    continue

            content = md_file.read_text()
            parsed = parse_chapter(content)

            chapter_id = parsed['id'] or md_file.stem

            # Add to Prolog facts
            +chapter(chapter_id)
            +title_of(chapter_id, parsed['title'])
            +type_of(chapter_id, parsed['type'])

            for next_id in parsed.get('next', []):
                +links_to(chapter_id, next_id)

            if parsed['type'].startswith('ending'):
                +is_ending(chapter_id)

            # Extract inline links too
            inline_links = extract_links(parsed['body'])
            for link_id, display in inline_links:
                +links_to(chapter_id, link_id)

            chapters[chapter_id] = {
                'title': parsed['title'],
                'type': parsed['type'],
                'body': parsed['body'],
                'prev': parsed.get('prev', []),
                'next': parsed.get('next', []),
                'links': inline_links,
                'source': md_file.name
            }

    return chapters

def generate_site(source_dir: Path, output_dir: Path):
    """Generate complete static site."""
    print(f"▌ CYOA GENERATOR ▐")
    print(f"Source: {source_dir}")
    print()

    chapters = load_chapters(source_dir)
    print(f"Loaded {len(chapters)} chapters")

    # Show chapter graph from Prolog facts
    print("\n◢ CHAPTER GRAPH")
    try:
        results = list(links_to(X, Y))
        results = [(r[0], r[1]) for r in results if r[0] != '__init__']
        for src, dst in sorted(results):
            status = '→' if dst in chapters else '⇢'
            print(f"  {src} {status} {dst}")
    except:
        pass

    # Generate output
    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"\n◢ GENERATING")

    for chapter_id, data in chapters.items():
        html = generate_html(chapter_id, data, chapters)
        # Use chapter_id for filename, handle special chars
        filename = chapter_id.replace('/', '-') + '.html'
        (output_dir / filename).write_text(html)
        print(f"  ✓ {filename}")

    # Index redirect
    start_chapter = '00-prologue' if '00-prologue' in chapters else '00'
    if start_chapter not in chapters:
        start_chapter = list(chapters.keys())[0]

    index_html = f'''<!DOCTYPE html>
<html><head>
<meta http-equiv="refresh" content="0; url={start_chapter}.html">
<title>THE COLLAPSE</title>
</head><body style="background:#0a0a0a;color:#d4a017;font-family:monospace;padding:2rem;">
<h1>▌ THE COLLAPSE ▐</h1>
<p>Redirecting to mission briefing...</p>
<p><a href="{start_chapter}.html" style="color:#d4a017;">Enter manually</a></p>
</body></html>
'''
    (output_dir / 'index.html').write_text(index_html)
    print(f"  ✓ index.html → {start_chapter}.html")

    print(f"\n◢ COMPLETE")
    print(f"  Site: {output_dir}")
    print(f"  Open: {output_dir}/index.html")

def main():
    if len(sys.argv) < 2:
        print("Usage: generate.py <source_dir> [output_dir]")
        sys.exit(1)

    source_dir = Path(sys.argv[1])
    output_dir = Path(sys.argv[2]) if len(sys.argv) > 2 else source_dir / 'site'

    if not source_dir.exists():
        print(f"Error: {source_dir} not found")
        sys.exit(1)

    generate_site(source_dir, output_dir)

if __name__ == '__main__':
    main()
