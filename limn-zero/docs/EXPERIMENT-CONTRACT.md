# Experiment Contract: E000 / LIMN-JEPA-0

Status: draft contract. Freeze exact numeric configuration before primary training.

## Primary Question

At a matched resource and channel budget, does an emergent graph-structured bottleneck outperform a
flat quantized bottleneck on held-out compositional prediction while remaining close to the
continuous JEPA ceiling?

## Experimental Domains

### Object World

Deterministic scenes composed from independently sampled factors:

- identity,
- shape,
- color,
- size,
- position,
- containment,
- velocity, and
- pairwise spatial relations.

Training and evaluation generators must expose exact state and transition truth.

### Temporal-Causal Graphs

Deterministic event graphs supporting:

- temporal precedence,
- interventions,
- counterfactual branches,
- delayed effects,
- hidden causes, and
- goal-conditioned action selection.

## Primary Variants

1. `continuous_jepa`: unrestricted continuous bottleneck ceiling.
2. `flat_quantized`: matched-bit flat discrete bottleneck.
3. `fixed_graph`: human-designed factorized graph bottleneck.
4. `emergent_graph`: learned concept and relation codebooks with the same graph budget.

An autoregressive sequence model may be reported as a secondary baseline, but it cannot replace any
primary variant.

## Matched Conditions

Except where a difference defines the variant, hold constant:

- environment episodes and split manifests,
- context and target encoders,
- predictor capacity,
- optimizer family and training-step budget,
- batch construction,
- target and masking schedule,
- parameter budget within a declared tolerance,
- persistent channel capacity in bits,
- evaluation compute, and
- checkpoint selection rule.

Report every unavoidable mismatch.

## Splits

The manifest must define non-overlapping splits for:

- interpolation,
- unseen factor combinations,
- unseen relation combinations,
- increased reasoning depth,
- held-out environment templates,
- counterfactual interventions, and
- fresh-receiver transmission.

Splits are generated once, hashed, and shared by every variant.

## Seeds

Primary seeds:

```text
11, 23, 37, 53, 71
```

Debug seeds and reduced runs must be labeled `debug` and excluded from primary aggregates.

## Primary Metrics

- target-embedding prediction error,
- exact environment-factor accuracy,
- held-out compositional accuracy,
- task or planning success,
- successful predictions per transmitted bit,
- performance gap from continuous ceiling,
- fresh-receiver sample efficiency.

## Causal Intervention Suite

For each discrete system:

1. Remove the full bottleneck state.
2. Shuffle states between matched examples.
3. Change one node while preserving graph syntax.
4. Change one relation while preserving graph syntax.
5. Corrupt a valid state, then inject the correct state as a rescue.
6. Swap semantically matched graph fragments between domains.

Predicted behavioral changes must be computed from environment truth before model inference.

## Collapse and Shortcut Audits

- code and relation utilization,
- mutual information with individual environment factors,
- nearest-state collisions,
- sender-specific receiver leakage,
- target-information leakage,
- continuous bypass detection,
- memorized-template detection.

These are diagnostics, not substitutes for the primary metrics.

## Required Artifacts

Each run produces:

- immutable configuration,
- configuration hash,
- Git commit,
- dataset-manifest hash,
- environment and dependency manifest,
- seed and variant,
- periodic training metrics,
- final evaluation summary,
- causal intervention summary,
- checkpoint and log artifact hashes,
- explicit run status and failure reason.

## Contract Changes

After the first primary run starts, this file is frozen for `E000`. Corrections or design changes
create `E001` or a clearly versioned successor. Results from different contracts are never pooled as
if they were one preregistered experiment.
