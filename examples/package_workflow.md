# Limn Package Workflow Example

This document walks through a complete package workflow using the IPFS-based package manager.

## 1. Create a New Package

```bash
# Initialize the package
limn pak init my-math-lib
cd my-math-lib
```

This creates:
```
my-math-lib/
├── pak.limn
├── src/
│   └── main.limn
├── test/
│   └── test_main.limn
└── .gitignore
```

## 2. Edit the Package

Edit `pak.limn` to set metadata:

```limn
whe pak_nom
whe pak_ver_maj
whe pak_ver_min
whe pak_ver_pat
whe pak_aut
whe pak_des
whe pak_ent

---
pak_nom sa "my-math-lib"
pak_ver_maj sa 1
pak_ver_min sa 0
pak_ver_pat sa 0
pak_aut sa "your-name"
pak_des sa "My mathematical utilities"
pak_ent sa "src/main.limn"
```

Edit `src/main.limn` with your constraints:

```limn
# Quadratic formula: ax² + bx + c = 0
# Solutions: x = (-b ± √(b²-4ac)) / 2a

whe quad_a
whe quad_b
whe quad_c
whe quad_discriminant
whe quad_x1
whe quad_x2

# discriminant = b² - 4ac
whe quad_b_sq
whe quad_4ac

quad_b exp quad_b sa quad_b_sq
quad_a exp quad_c exp 4 sa quad_4ac
quad_b_sq cut quad_4ac sa quad_discriminant

# x1 = (-b + √discriminant) / 2a
# x2 = (-b - √discriminant) / 2a
# (sqrt would need host primitive support)

---
# key: Example - solve x² - 5x + 6 = 0
quad_a sa 1
quad_b sa -5
quad_c sa 6
# discriminant = 25 - 24 = 1
# x1 = (5 + 1) / 2 = 3
# x2 = (5 - 1) / 2 = 2
```

## 3. Validate the Package

```bash
limn pak check
# Output: ✓ Package structure valid
```

## 4. Test Locally

```bash
limn run src/main.limn
# Shows computed results

limn run test/test_main.limn
# Runs tests
```

## 5. Publish to IPFS

First, start your IPFS daemon:
```bash
ipfs daemon
```

Then publish:
```bash
limn pak pub
# Output:
# Built: my-math-lib@1.0.0
# Publishing to IPFS...
#
# ✓ Published: my-math-lib@1.0.0
#   CID: bafybeiabc123xyz...
#
# To use this package:
#   use cid bafybeiabc123xyz...
```

## 6. Sign the Package (Optional)

```bash
# Generate a signing key (first time only)
limn pak key gen my-key

# Sign and publish
limn pak pub --sign my-key
```

## 7. Pin for Availability (Optional)

```bash
# Configure a pinning service
limn pak pin service add pinata pinata https://api.pinata.cloud <api-key> <secret>

# Pin during publish
limn pak pub --pin pinata

# Or pin later
limn pak pin bafybeiabc123xyz --name "my-math-lib@1.0.0"
```

## 8. Use the Package

In another project:

```limn
# Import by CID
use cid bafybeiabc123xyz

# Use the exported constraints
whe my_result
quad_a sa 1
quad_b sa -5
quad_c sa 6

---
# key
```

Or add as a dependency:

```bash
limn pak add my-math-lib --cid bafybeiabc123xyz
limn pak install
```

## 9. Update the Package

1. Make changes to your code
2. Bump version in `pak.limn`:
   ```limn
   pak_ver_min sa 1  # 1.0.0 -> 1.1.0
   ```
3. Publish new version:
   ```bash
   limn pak pub
   # New CID is generated
   ```

## 10. Registry (For Discoverability)

If you maintain a registry:

```bash
# Initialize registry
limn pak registry init

# Add your package
limn pak registry add my-math-lib 1.0.0 bafybeiabc123xyz --author "your-name"

# Publish registry
limn pak registry publish --key limn-registry
```

Users can then:
```limn
use nom "my-math-lib"
```

---

## Summary of Commands

| Action | Command |
|--------|---------|
| Create package | `limn pak init <name>` |
| Validate | `limn pak check` |
| Build | `limn pak build` |
| Publish | `limn pak pub [--sign <key>] [--pin <service>]` |
| Install deps | `limn pak install` |
| Add dep | `limn pak add <pkg>` |
| Search | `limn pak search <query>` |
| Info | `limn pak info <pkg>` |
| Sign | `limn pak sign . <key>` |
| Verify | `limn pak verify .` |
| Pin | `limn pak pin <cid>` |
