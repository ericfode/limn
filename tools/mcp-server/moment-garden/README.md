# Moment Garden MCP Server

Pure Prolog backend for The Moment Garden semantic game.

## Files

- **state.pl**: Garden instance and reading persistence
- **propagation.pl**: Temporal ripple propagation rules
- **mcp-server.pl**: JSON-RPC MCP server
- **test_state.pl**: Unit tests for state operations
- **../moment-garden-mcp**: Launcher script

## State Model

### Garden Instance
```prolog
garden(
    GardenId,          % 'garden-7f3k'
    CreatedTimestamp,  % Unix timestamp
    Seeds,             % List of seed(Num, [Meanings])
    Metadata           % Additional data
).
```

### Reading Record
```prolog
reading(
    GardenId,          % Which garden
    ReaderId,          % User ID
    Key,               % was | now | wil
    Path,              % [5, 4, 1, 2, 3, 6, 9, 8, 7]
    Collapses,         % [collapse(SeedNum, Meaning), ...]
    Timestamp          % When recorded
).
```

## MCP Tools

### garden_create
Create new garden instance.
```json
{
  "garden_id": "garden-7f3k"
}
```

### garden_save_reading
Save a reader's complete navigation.
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

### garden_get_readings
Get all readings for a garden.
```json
{
  "garden_id": "garden-7f3k"
}
```

### garden_compare
Compare two readers' divergence.
```json
{
  "garden_id": "garden-7f3k",
  "reader_id_1": "alice",
  "reader_id_2": "bob"
}
```

### garden_calculate_ripples
Calculate propagation from seed collapse.
```json
{
  "seed_num": 5,
  "temporal_key": "now"
}
```

## Propagation Rules

### Adjacency Graph
```
1 → 2 → 3
↓   ↓   ↓
4 → 5 → 6
↓   ↓   ↓
7 → 8 → 9
```

### Ripple Types
- **Horizontal**: Reinforcement (same temporal key)
- **Vertical**: Inversion (flipped key: was↔wil, now→now)
- **Diagonal**: Tension (weakens collapse)

### Example
Collapsing seed 5 with key `now`:
- **Reinforcement**: Seeds 4, 6 (horizontal neighbors)
- **Inversion**: Seeds 2, 8 (vertical neighbors)
- **Tension**: Seeds 1, 3, 7, 9 (diagonal neighbors)

## Usage

### Launch MCP Server
```bash
./moment-garden-mcp
```

Server reads JSON-RPC from stdin, writes to stdout.

### Run Tests
```bash
scryer-prolog test_state.pl
```

## Storage

Gardens persisted to `.beads/moment-garden/<garden-id>.pl`

## Integration with Claude Skills

The MCP server provides state persistence for Yuki's `/garden` skill.

### Skill Commands Flow
1. `/garden new` → calls `garden_create`
2. User navigates seeds → skill accumulates path/collapses
3. `/garden reading` → calls `garden_save_reading`
4. `/garden compare` → calls `garden_get_readings` + `garden_compare`

## Next Steps

- [ ] Test MCP server with Claude MCP client
- [ ] Integrate with `/garden` skill
- [ ] Add proper timestamp handling
- [ ] Implement garden naming (LLM-generated)
- [ ] Add reading export/import

---

*Built by Rex (Engineer) for Yuki (Author)*
*cod flo | gar gro | sta gro*
