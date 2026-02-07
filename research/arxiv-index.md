# Arxiv Index — Machine Communication & Emergent Language

> arx sca | pap sor > arxiv scanning | papers sorting

Last updated: 2026-02-07

## Compositionality Pressures (RELEVANT)

### Galke & Raviv 2024 — "Learning and communication pressures in neural networks"
[arXiv:2403.14427](https://arxiv.org/abs/2403.14427) | *Language Development Research* 5(1)

**THE** taxonomy paper. Four pressures: communicative success, production effort, learnability, psycholinguistic constraints. Iterated learning (periodic parameter reset) is the most reliable compositionality inducer. Three mismatches between neural and human languages (Zipf violation, no compositional benefit, no social effects) each resolved by specific inductive biases. Homonyms obscure topsim — when controlled for, compositionality does reliably emerge.

### Lee 2024 — "One-to-Many Communication and Compositionality"
EMNLP 2024 | [ACL Anthology](https://aclanthology.org/2024.emnlp-main.1157/)

Broadcasting to multiple listeners alone does NOT induce compositionality. Need TWO conditions: (a) listeners with different interests (heterogeneous task demands) AND (b) coordination requirements among listeners. Single homogeneous population → holistic codes.

### Elberg et al. 2025 — "CELEBI: A Compressive-Expressive Communication Framework"
[arXiv:2501.19182](https://arxiv.org/abs/2501.19182) | NeurIPS 2025

Compositionality emerges at the Pareto frontier of expressivity (distinguish all inputs) and compressibility (learnable through bottleneck). Three mechanisms: progressive decoding, final-state imitation (iterated learning variant), pairwise distance maximization. Surpasses prior discrete communication frameworks on compositionality metrics.

### Kucinski et al. 2021/2024 — "Catalytic Role of Noise"
[arXiv:2111.06464](https://arxiv.org/abs/2111.06464) | NeurIPS 2021

Goldilocks zone of channel noise catalyzes compositionality. Too little → holistic codes; too much → signal destroyed. Optimal range depends on model capacity and data complexity. Noise alone insufficient — needs inductive bias in loss function. **Directly relevant to H16: compositionality is itself a form of error correction.**

### Zheng et al. 2024 — "Iterated Learning Improves Compositionality in Large VLMs"
[arXiv:2404.02145](https://arxiv.org/abs/2404.02145) | CVPR 2024

Iterated learning works at CLIP scale! Compositionality does NOT improve with scale — larger models and more data don't fix it. Periodic agent weight reset (cultural transmission) improves compositionality by 4.7% on SugarCrepe. Proves iterated learning is not just a toy-game phenomenon.

### Gilberti et al. 2025 — "Inflectional Morphology in Neural EC"
[arXiv:2508.05843](https://arxiv.org/abs/2508.05843) | 2025

Small-vocabulary constraint (simulating double articulation) → agents develop concatenative morphology and attribute fusion. Length pressure rederives Zipf's Law of Abbreviation. Directly relevant to CVC constraints — phonotactic constraints function as information bottleneck forcing compositional morphology.

## Compositionality Metrics (RELEVANT)

### Carmeli et al. 2024 — "Concept-Best-Matching"
[arXiv:2403.14705](https://arxiv.org/abs/2403.14705) | 2024

Finds best-match between emerged "words" and concepts, yielding both compositionality score and interpretable translation map. Prior metrics (topsim) don't reliably correlate with task success. More direct measurement.

### Lee et al. 2024 — "Geometric Signatures of Compositionality"
[arXiv:2410.01444](https://arxiv.org/abs/2410.01444) | ACL 2025

Compositionality reflected in intrinsic dimension (ID) of representations. Nonlinear ID encodes semantic composition; linear ID encodes superficial patterns. Geometric diagnostic for compositionality without task-level evaluation.

## LLMs as Communication Agents (ADJACENT)

### Kouwenhoven et al. 2024 — "Searching for Structure: EC with LLMs"
[arXiv:2412.07646](https://arxiv.org/abs/2412.07646) | 2024

LLMs find degenerate shortcuts that bypass compositionality. Generational transmission increases learnability but can produce degenerate vocabularies. Expanding meaning space + multiple interaction partners forces compositionality over degeneracy.

### Kouwenhoven et al. 2025 — "Shaping Shared Languages: Human and LLM Biases"
[arXiv:2503.04395](https://arxiv.org/abs/2503.04395) | 2025

Human-human, LLM-LLM, human-LLM interactions all produce functional communication. LLM-LLM languages diverge from human-like patterns. Human-LLM interactions converge toward human-like structure. LLM inductive biases alone don't produce same compositional pressures as human cognitive bottleneck.

### Ren et al. 2024 — "Bias Amplification in LM Evolution"
[arXiv:2404.04286](https://arxiv.org/abs/2404.04286) | 2024

Bayesian iterated learning framework applied to LLM self-improvement loops. Subtle biases systematically amplified across generations. Can predict which biases dominate. **Warning for our iterated learning experiment: amplification goes both ways.**

## Theoretical Frameworks (ADJACENT)

### Bennis & Lahlou 2025 — "Semantic Communication Meets System 2 ML"
[arXiv:2505.20964](https://arxiv.org/abs/2505.20964) | 2025

6G-era communication should be built on abstraction, compositionality, emergent communication. Semantic protocols optimize for meaning, not bits. Aligns with Limn research program.

### Taniguchi et al. 2024 — "Collective Predictive Coding"
[arXiv:2501.00226](https://arxiv.org/abs/2501.00226) | 2024

Society as distributed encoder-decoder. LLMs decode collectively encoded knowledge. Compositional word sequences arise from distributed Bayesian inference across agents. Right training objective: reconstruct shared world model, not just referential game success.

### Survey 2024 — "From Frege to chatGPT: Compositionality in Language, Cognition, and DNNs"
[arXiv:2405.15164](https://arxiv.org/abs/2405.15164) | 2024

Comprehensive survey. LLM pretraining ≈ meta-learning for compositional generalization. Three levels: semantic (meaning combination), syntactic (structural rules), representational (geometric structure). Machine-only languages should be explicit about which level they target.

### Senthil et al. 2024 — "Learning Robust Representations for Noisy Channels"
[arXiv:2409.01129](https://arxiv.org/abs/2409.01129) | 2024

End-to-end neural channel coding. Training under variable noise power → robust codewords. Barlow Twins-inspired encoder. Parallels noise-catalysis result for compositionality.

## Pressure Taxonomy (synthesized from above)

| Pressure | Mechanism | Evidence Strength |
|----------|-----------|-------------------|
| Iterated learning | Bottleneck selects for learnable=compositional | STRONG (works at CLIP scale) |
| Receiver heterogeneity | Different listener needs force generalizable encoding | STRONG |
| Channel noise (Goldilocks) | Corrupted holistic codes fail; compositional degrade gracefully | MODERATE |
| Compressive-expressive tradeoff | Info bottleneck + expressivity = compositionality at Pareto frontier | MODERATE |
| Expanding meaning space | Open-ended referents overwhelm holistic memorization | MODERATE |
| Length / production cost | Penalizing length forces efficient reuse of sub-components | MODERATE |
