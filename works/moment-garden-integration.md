# The Moment Garden: Integration Guide

> `gar pla | mcp con | sta per`
> *(garden planted | MCP connected | state persists)*

## Architecture Overview

The Moment Garden is a **Claude Code skill** with a **Prolog MCP backend** for state persistence.

### Components

1. **Skill Definition** (`src/claude-skill/garden.skill.json`)
   - Command structure (`/garden new`, `/garden play`, etc.)
   - Game mechanics and flow
   - MCP server configuration

2. **MCP Backend** (`crew/engineer/tools/mcp-server/moment-garden/`)
   - Pure Prolog state persistence
   - 5 MCP tools for garden operations
   - Propagation rule engine
   - Multiplayer reading storage

3. **Skill Bootstrap** (`src/claude-skill/garden-bootstrap.md`)
   - Game rules and seed definitions
   - Response style guide
   - Example collapses and propagation

## Installation

### Step 1: Verify MCP Server

The Moment Garden MCP server requires Scryer Prolog.

```bash
# Test server launches
cd /home/eric/src/limntown/limn
./crew/engineer/tools/mcp-server/moment-garden-mcp

# Should output JSON-RPC initialization (Ctrl+C to exit)
```

### Step 2: Install Skill in Claude Code

**Option A: Manual Configuration**

Add to `~/.claude/settings.json`:

```json
{
  "mcpServers": {
    "moment-garden": {
      "command": "/home/eric/src/limntown/limn/crew/engineer/tools/mcp-server/moment-garden-mcp",
      "args": []
    }
  }
}
```

Then add the skill bootstrap to your Claude Code session:

```bash
# From the limn repo root
claude skill add garden ./crew/author/src/claude-skill/garden.skill.json
```

**Option B: Plugin Installation**

```bash
# Install as Claude Code plugin
cp -r crew/author/.claude/plugins/moment-garden ~/.claude/plugins/

# Add MCP server to settings.json (same as Option A)
```

### Step 3: Verify Installation

In a new Claude Code session:

```
/garden new
```

Claude should:
1. Call the MCP server's `garden_create` tool
2. Display the welcome message with 9-seed grid
3. Prompt you to choose a temporal key (was/now/will)

## MCP Tools Reference

The skill uses these MCP tools (provided by Rex's backend):

### garden_create
```json
{
  "garden_id": "garden-7f3k"
}
```
Creates new garden instance. Returns success message.

### garden_save_reading
```json
{
  "garden_id": "garden-7f3k",
  "reader_id": "alice",
  "temporal_key": "now",
  "path": [5, 4, 1, 2, 3, 6, 9, 8, 7],
  "collapses": [
    {"seed": 5, "meaning": "Now manifesting here"},
    {"seed": 4, "meaning": "Losing self to find other"}
  ]
}
```
Saves complete reading for comparison.

### garden_get_readings
```json
{
  "garden_id": "garden-7f3k"
}
```
Returns all readings for a garden (for multiplayer).

### garden_compare
```json
{
  "garden_id": "garden-7f3k",
  "reader_id_1": "alice",
  "reader_id_2": "bob"
}
```
Compares two readers' interpretations. Returns divergence score and differences.

### garden_calculate_ripples
```json
{
  "seed_num": 5,
  "temporal_key": "now"
}
```
Calculates propagation effects on adjacent seeds.

## Skill Command Flow

### /garden new

1. Skill calls `garden_create` with generated ID
2. Displays ASCII garden grid
3. Presents three keys (was/now/will)
4. Waits for user to select key
5. Begins navigation

### /garden seed N

1. Skill interprets seed N based on:
   - Selected temporal key
   - Previous collapses in path
   - Adjacency to collapsed seeds
2. Calls `garden_calculate_ripples` for propagation
3. Generates poetic interpretation
4. Updates internal state
5. Shows ripple effects on neighbors

### /garden reading

1. Skill displays accumulated path and collapses
2. Calls `garden_save_reading` to persist
3. Generates garden name based on pattern
4. Shows complete reading visualization

### /garden compare

1. Skill calls `garden_get_readings` for current garden
2. Calls `garden_compare` for divergence analysis
3. Displays side-by-side interpretations
4. Shows divergence score

## State Persistence

Gardens are stored in `.beads/moment-garden/<garden-id>.pl`:

```prolog
% Garden instance
garden(
    'garden-7f3k',
    1738502400,  % timestamp
    [seed(1,[beg,lov,fea]), seed(2,[mid,hol,bre]), ...],
    []
).

% Reading record
reading(
    'garden-7f3k',
    'alice',
    now,
    [5,4,1,2,3,6,9,8,7],
    [collapse(5,'Now manifesting here'), collapse(4,'Losing self to find other')],
    1738502450
).
```

## Testing Checklist

- [ ] MCP server launches without errors
- [ ] Skill recognizes `/garden` command
- [ ] `garden_create` successfully creates instance
- [ ] Seed navigation generates interpretations
- [ ] Propagation rules apply correctly (horizontal/vertical/diagonal)
- [ ] `garden_save_reading` persists to `.beads/`
- [ ] Multiple readers can join same garden
- [ ] `garden_compare` shows divergence
- [ ] Garden names are evocative and unique

## Example Session

```
User: /garden new

Claude:
┌───────────────────────────────────────────┐
│  THE MOMENT GARDEN AWAITS                  │
│                                            │
│  Nine seeds rest in temporal superposition.│
│  Your key will collapse them into meaning. │
└───────────────────────────────────────────┘

    ┌───────┐     ┌───────┐     ┌───────┐
    │   1   │────▶│   2   │────▶│   3   │
    │beg|lov│     │mid|hol│     │end|pea│
    │  |fea │     │  |bre │     │  |gri │
    └───┬───┘     └───┬───┘     └───┬───┘
        │             │             │
        ▼             ▼             ▼
    ┌───────┐     ┌───────┐     ┌───────┐
    │   4   │────▶│   5   │────▶│   6   │
    │los|sel│     │now|her│     │fnd|wha│
    │  |oth │     │  |gon │     │was|wil│
    └───┬───┘     └───┬───┘     └───┬───┘
        │             │             │
        ▼             ▼             ▼
    ┌───────┐     ┌───────┐     ┌───────┐
    │   7   │────▶│   8   │────▶│   9   │
    │rem|tru│     │for|giv│     │bec|who│
    │  |wis │     │  |tak │     │was|wil│
    └───────┘     └───────┘     └───────┘

Which key do you hold?

  was — The Key of Memory (observe from the past)
  now — The Key of Presence (observe from this moment)
  wil — The Key of Anticipation (observe from the future)

User: now

Claude:
You hold the Key of Now.

The garden shimmers. Nine seeds await your touch.
All meanings exist simultaneously until you collapse them.

Which seed do you touch first? (1-9)

User: 5

Claude:
SEED 5 COLLAPSES:

┌─────────────────────────────────────────────┐
│  now | her | gon                            │
│                                             │
│  The edge where here becomes gone.          │
│  This moment. Right now. The threshold      │
│  between presence and absence.              │
│                                             │
│  You are here. You are already going.       │
│  Both true. Both now.                       │
└─────────────────────────────────────────────┘

RIPPLE EFFECTS (horizontal reinforcement):
  Seed 4 → los|sel|oth shifts toward simultaneity
  Seed 6 → fnd|wha was|wha wil shifts toward finding in now

VERTICAL ECHO (inverted):
  Seed 2 → mid|hol|bre inverts to "was" orientation
  Seed 8 → for|giv|tak inverts to "wil" orientation

TENSION with: Seeds 1, 3, 7, 9 (diagonal)

Which seed next?
```

## Troubleshooting

### MCP Server Won't Start

```bash
# Check Scryer Prolog installation
scryer-prolog --version

# Verify state directory exists
mkdir -p .beads/moment-garden

# Check for syntax errors
scryer-prolog crew/engineer/tools/mcp-server/moment-garden/test_state.pl
```

### Skill Not Recognized

```bash
# Verify Claude Code sees the skill
claude skill list

# Check MCP server in settings
cat ~/.claude/settings.json | grep moment-garden
```

### Tool Calls Fail

```bash
# Check MCP server logs (stderr)
# Server should log each tool call and response

# Verify garden ID format
# Should be: garden-XXXX (4 alphanumeric chars)
```

## Next Steps

1. **Polish Skill Responses**
   Add more evocative language for seed collapses and ripples.

2. **Garden Naming Algorithm**
   Generate poetic names based on collapse patterns.

3. **Multiplayer UI**
   Enhance `/garden compare` with visual divergence display.

4. **Export Readings**
   Allow users to save readings as artifacts.

5. **Temporal Meditation Mode**
   Special mode where all three keys are applied to same garden.

---

```limn
mcp flo | sta gro | gar liv | mea mad | tim cha you
```

> *MCP flows. State grows. Garden lives. Meaning made. Time changes you.*

— Yuki & Rex
2026-02-02
