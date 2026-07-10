# Limn Zero

**Grounded predictive semantic bottlenecks, tested before they are named.**

Status: pre-implementation research scaffold. No empirical claims have been established.

## The Question

Can a sparse, discrete, graph-structured predictive workspace approach the performance of a
continuous world model while providing better:

- compositional generalization,
- transmission to unfamiliar agents,
- channel efficiency, and
- causal inspectability?

Limn Zero does not begin with a fixed language. It begins with a bottleneck and asks what reusable
semantic structure emerges under pressure to predict, reconstruct relevant factors, plan, and teach.

## Why a New Repository?

The historical Limn repository explores a designed constructed language, interpreters, model
fine-tuning, runtimes, creative work, and communication experiments. Limn Zero asks a narrower and
more falsifiable question. Keeping it separate prevents inherited vocabulary and architecture choices
from becoming unexamined assumptions.

Historical work may be cited as prior evidence. Code moves here only when a current experiment
requires it and the dependency is documented.

## Strong Architecture

```text
observation x_t
    -> context encoder E_theta
    -> bottleneck Q
    -> state g_t
    -> action-conditioned predictor P_phi
    -> predicted target embedding z_hat_(t+1)

future observation x_(t+1)
    -> EMA target encoder E_xi
    -> target embedding z_(t+1)
```

Training minimizes distance between the predicted and target embeddings. During recurrence,
`z_hat_(t+1)` is passed through `Q` again and all other continuous state is discarded.

The primary comparison is between:

1. an unrestricted continuous JEPA ceiling,
2. a flat quantized bottleneck,
3. a fixed human-designed graph bottleneck, and
4. an emergent graph bottleneck.

## First Experiment

`LIMN-JEPA-0` uses two deterministic procedural domains:

- compositional object worlds,
- temporal-causal intervention graphs.

Every variant receives matched data, compute, parameter count, message-bit budget, and evaluation
splits. Primary results require five preregistered seeds and causal intervention tests.

See:

- [Thesis](docs/THESIS.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Experiment contract](docs/EXPERIMENT-CONTRACT.md)
- [Go/no-go gates](docs/GO-NO-GO.md)
- [Reproducibility rules](docs/REPRODUCIBILITY.md)
- [Roadmap](docs/ROADMAP.md)

## Non-Goals

Until the first gate passes, this project is not building:

- a general chatbot,
- a natural-language interface,
- an HVM runtime,
- a package ecosystem,
- a consciousness architecture,
- a production robotics stack, or
- a public launch campaign.

## Quick Start After Copying

Follow [NEW-REPO-CHECKLIST.md](NEW-REPO-CHECKLIST.md), then:

```bash
python -m venv .venv
python -m pip install -e ".[dev]"
python scripts/validate_scaffold.py
python -m pytest
```

The repository intentionally contains no training implementation yet. The first implementation must
satisfy the experiment contract rather than precede it.

## Origin

Limn Zero was separated from the historical
[Limn research repository](https://github.com/ericfode/limn) so the learned-language hypothesis can
be tested independently from the designed-language artifact.

## License

MIT. See `LICENSE`.
