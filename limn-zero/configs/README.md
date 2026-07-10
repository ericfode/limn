# Configurations

Configuration files are immutable experimental inputs.

Conventions:

- `debug-*.yaml`: reduced smoke runs; never part of primary aggregates.
- `E000-<variant>-v1.yaml`: frozen primary configuration for one variant.
- Shared values should be expanded into each frozen config or referenced through a content-hashed
  base file.
- Every result records the full configuration SHA-256.

`limn-jepa-0.example.yaml` is illustrative. Freeze exact values only after environment and continuous
baseline smoke tests establish feasible budgets.
