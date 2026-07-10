# Reproducibility

## Immutable Inputs

Every primary run is determined by:

- Git commit,
- configuration file and SHA-256,
- dataset manifest and SHA-256,
- experiment ID,
- variant,
- seed,
- dependency lock or environment manifest,
- hardware description.

Changing any of these creates a distinct run ID.

## Run IDs

Use:

```text
<experiment>-<variant>-s<seed>-<config-hash-prefix>
```

Example:

```text
E000-emergent_graph-s23-a81f29c4
```

## Configuration Rules

- Configurations contain all experiment-affecting values.
- Source code must not hide experimental constants.
- A config that produced a run is immutable.
- Derived configs use a new filename and hash.
- Debug configs are stored separately and marked non-primary.

## Dataset Manifests

Manifests record:

- generator version,
- generator parameters,
- seed,
- split assignment,
- example counts,
- factor and combination coverage,
- serialized data hashes.

Train, validation, and every held-out split must be checked for template and semantic leakage.

## Results

Small summaries conform to `results/schema.json` and are committed. Large logs, checkpoints, and
generated datasets are stored externally under content-addressed names. The summary records their
hashes and locations.

Never hand-edit a metric emitted by a run. Corrections are new derived artifacts with provenance.

## Aggregation

An aggregate report must list:

- included and excluded run IDs,
- exclusion reasons,
- per-seed values,
- mean and dispersion,
- paired comparisons where seeds are shared,
- confidence intervals,
- failures and missing artifacts.

Do not collapse failed runs into silence. Report success rate separately from conditional metrics.

## Environment Capture

Record at minimum:

- operating system,
- Python version,
- dependency versions,
- accelerator model,
- accelerator count,
- driver and runtime versions,
- precision mode,
- deterministic-kernel settings.

## Determinism

Set and record seeds for Python, NumPy, the ML framework, data generation, and sampler workers. When
an operation is nondeterministic, document it and quantify rerun variability rather than claiming
bitwise reproducibility.
