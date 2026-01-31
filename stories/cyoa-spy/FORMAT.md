# CYOA Format Guide

## Two Modes

### Learning Mode (default)
- English narrative explains Limn concepts
- Limn code blocks with context
- Choice descriptions in English
- Best for: First-time readers, teaching Limn

### Immersive Mode
- Pure Limn content
- Minimal or no English
- Choices shown as Limn messages
- Best for: Limn speakers, art experience, replay

## File Conventions

### Chapter files
- `chapters/NN-name.md` - Learning mode
- `chapters/NN-immersive.md` - Immersive mode (optional alternate)

### Generator behavior

**Learning mode:**
- Render markdown as-is
- Code blocks styled distinctly

**Immersive mode:**
- Strip all text outside code blocks
- Strip English inside code blocks (lines without `|`)
- Keep YAML frontmatter for navigation
- Keep choice links

### Stripping rules for immersive

Keep:
```
```limn
car tak | loc unk | tim sho | urj
```
```

Strip:
```
> *Cardinal captured. Location unknown. Time short. Urgent.*
```

Keep:
```
> **→ [roo](#01a)**
>
> ```limn
> key roo tru | app
> ```
```

Strip:
```
> **→ [The Rookie](#01a)**
>
> Apply **[KEY: ROOK-TRUE]**
>
> You recognize Rook's signature...
```

## Limn Typography

### Code blocks
- Monospace font
- Dark background
- Generous line height
- `|` as visual separator (consider dim styling)

### Tone markers
Consider visual treatment:
- `urj` (urgent) - red/bold
- `hes` (hesitant) - italic/dim
- `sin` (sincere) - normal
- `iro` (ironic) - strikethrough or inverted
- `frm` (formal) - uppercase
- `cas` (casual) - lowercase
- `te` (question) - trailing ?
- `we` (imperative) - trailing !

### Flow
Limn should feel like reading poetry or scripture:
- Breath between clusters
- Weight to each word
- Rhythm in the pipes

## Example Transformation

### Learning Mode Input:
```markdown
The message arrived at 03:47 UTC.

```limn
car tak | loc unk | tim sho | urj
```

> *Cardinal captured. Location unknown. Time short. Urgent.*

Your blood goes cold.
```

### Immersive Mode Output:
```markdown
```limn
car tak | loc unk | tim sho | urj
```
```

## Navigation

Both modes use same YAML frontmatter:
```yaml
id: "01a"
prev: ["00"]
next: ["02a", "02b"]
```

Immersive mode chapters can link to learning mode siblings.
Learning mode chapters can offer "Try immersive" option.
