# Ninety-Day Roadmap

The schedule is ordered by dependency, not ambition. Do not advance a phase whose entry gate has not
passed.

## Phase 0: Contract and Worlds — Weeks 1-3

Deliverables:

- frozen `E000` numeric configuration,
- deterministic object-world generator,
- deterministic temporal-causal generator,
- hashed interpolation and held-out manifests,
- exact environment-factor evaluator,
- leakage tests,
- result writer conforming to `results/schema.json`.

Exit gate: identical manifests and metrics reproduce on two clean environments.

## Phase 1: Continuous Ceiling — Weeks 4-5

Deliverables:

- context and EMA target encoders,
- action-conditioned predictor,
- semantic multi-target masking,
- continuous JEPA training run for every primary seed,
- in-distribution and held-out ceiling report.

Exit gate: the continuous model learns both domains and beats non-predictive controls.

## Phase 2: Discrete Bottlenecks — Weeks 6-9

Deliverables:

- flat quantized bottleneck,
- fixed graph bottleneck,
- emergent graph bottleneck,
- matched-bit accounting,
- no-bypass tests,
- collapse diagnostics,
- all primary variants across all seeds.

Exit gate: Gate A is satisfied and Gate B can be evaluated.

## Phase 3: Causality and Transmission — Weeks 10-12

Deliverables:

- intervention suite,
- trace corruption and rescue,
- fresh-receiver training,
- iterated transmission pilot,
- preregistered aggregate report,
- explicit full-go, conditional-go, pivot, or stop verdict.

Exit gate: `docs/GO-NO-GO.md` is evaluated without changing its thresholds.

## Deferred Until Full Go

- cross-modal perception,
- natural-language adapters,
- human-readable token naming,
- physical robotics,
- runtime or compiler integration,
- large-scale model training,
- public claims about machine-native language.
