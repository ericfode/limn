# Agent Instructions

## Mission

Limn Zero is a proof-or-kill research program for one question:

> Can a sparse, discrete, graph-structured predictive bottleneck preserve useful world-model
> performance while improving compositional generalization, cross-agent transmission, and causal
> inspectability?

The goal is not to defend the historical Limn design. The goal is to discover whether a language-like
predictive workspace is useful and, if so, what structure it should have.

## Controlling Documents

Read these before changing code or experiments, in this order:

1. `docs/THESIS.md`
2. `docs/EXPERIMENT-CONTRACT.md`
3. `docs/GO-NO-GO.md`
4. `docs/ARCHITECTURE.md`
5. `docs/REPRODUCIBILITY.md`
6. `docs/ROADMAP.md`

If code, configuration, or prose conflicts with the experiment contract, the contract wins. Do not
silently revise a contract after seeing results. Create a versioned successor instead.

## Non-Negotiable Research Rules

1. **No English in the core experiment.** English-capable adapters may exist outside the measured
   reasoning path, but the core model, environments, targets, and bottleneck must not depend on them.
2. **No pretrained language model in the core.** Core experimental models begin from random
   initialization. A teacher is permitted only in a separately labeled distillation experiment.
3. **The bottleneck must be real.** No encoder-to-predictor skip connection, cached continuous state,
   hidden recurrent state, retrieval side channel, or unreported auxiliary input may bypass it.
4. **Reset between reasoning cycles.** Persistent state crossing a cycle boundary must be the declared
   bottleneck representation. Continuous activations and KV caches are discarded.
5. **Match the controls.** Comparisons use the same environments, examples, parameter budget,
   training steps, optimization budget, message-bit budget, and evaluation splits.
6. **Use multiple seeds.** A primary result requires all preregistered seeds. A single run is a debug
   result, never evidence.
7. **Prefer exact outcomes.** Use executable environment truth and deterministic metrics. Do not use an
   LLM judge for a primary metric.
8. **Treat negative results as deliverables.** Never hide, overwrite, or cosmetically reinterpret a
   failed run.
9. **Do not anthropomorphize.** Report observable mechanisms and causal tests, not claims that a model
   literally thinks, understands, believes, or experiences.
10. **Keep scope narrow.** No HVM runtime, package manager, social launch, general chatbot, UI, or
    embodiment expansion until the current go/no-go gate passes.

## Architecture Boundary

The strong experimental path is:

```text
observation -> context encoder -> declared bottleneck -> predictor -> target embedding
```

The target embedding comes from a stop-gradient or EMA target encoder. At rollout boundaries, the
predicted representation is requantized through the bottleneck before reuse.

A Limn renderer after an unrestricted latent model is a captioner, not a Limn-mediated reasoner. Keep
that distinction explicit in names, diagrams, and claims.

## Repository Boundaries

- `src/limn_zero/environments/`: deterministic procedural worlds and dataset manifests.
- `src/limn_zero/models/`: encoders, target encoders, predictors, and shared model components.
- `src/limn_zero/bottlenecks/`: continuous, flat discrete, fixed-graph, and emergent-graph channels.
- `src/limn_zero/protocols/`: serialization, graph constraints, interventions, and receiver interfaces.
- `src/limn_zero/training/`: training loops, checkpointing, and run orchestration.
- `baselines/`: thin entrypoints or documentation for matched comparison systems.
- `evals/`: evaluation implementations. Evaluation code must not import training-only shortcuts.
- `configs/`: immutable run configurations. Never edit a config after it has produced a result.
- `results/`: small machine-readable summaries and manifests. Large artifacts live outside Git.
- `tests/`: deterministic unit, property, leakage, and no-bypass tests.

## Experimental Workflow

Before a run:

1. Assign an experiment ID such as `E000`.
2. Freeze the config and compute its SHA-256 hash.
3. Record the Git commit, dataset-manifest hash, seed, hardware, and software environment.
4. Run leakage and no-bypass checks.
5. Confirm that every primary variant has the same declared budget.

After a run:

1. Write a result matching `results/schema.json`.
2. Preserve raw logs and checkpoints as content-addressed external artifacts.
3. Record failures with `status: failed` and the original error.
4. Aggregate only after every preregistered seed is present or explicitly marked failed.
5. Update conclusions only from committed result summaries.

## Quality Gates

Run before committing:

```bash
python scripts/validate_scaffold.py
python -m pytest
ruff check .
ruff format --check .
mypy src
```

For experiment-affecting changes, also run the smallest deterministic smoke configuration and verify
that its result validates against `results/schema.json`.

## Work Tracking

Use `bd` when the new repository has been initialized with Beads:

```bash
bd onboard
bd ready
bd show <id>
bd update <id> --status in_progress
bd close <id>
bd sync
```

If `.beads/` does not exist yet, follow `NEW-REPO-CHECKLIST.md`. Do not initialize Beads inside a
parent or source repository by accident.

## Landing the Plane

Work is complete only when the branch is clean and pushed:

1. File issues for remaining work.
2. Run the relevant quality gates.
3. Close or update active issues.
4. Inspect `git diff` and commit only intentional files.
5. Run `git pull --rebase`.
6. Run `bd sync` when Beads is initialized.
7. Run `git push`.
8. Verify `git status` reports the branch is up to date with its upstream.
9. Provide a handoff containing experiment IDs, result paths, failed checks, and the next safe step.

Never claim success from syntax validity alone. The decisive evidence is matched performance plus
causal dependence on the declared bottleneck.
