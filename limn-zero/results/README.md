# Results

Commit small, machine-readable summaries here. Do not commit model weights, checkpoints, generated
datasets, or unbounded logs.

Recommended layout:

```text
results/
  E000/
    runs/
      E000-emergent_graph-s23-a81f29c4.json
    aggregate.json
    report.md
```

Every run summary must validate against `schema.json`. Large artifact references use hashes and an
explicit storage location.
