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

### Carmeli, Meir & Belinkov 2025 — "CtD: Composition through Decomposition"
[OpenReview](https://openreview.net/forum?id=KlalQu2423) | ICLR 2025

Two-step method: agents first *decompose* images into basic concepts via codebook in multi-target coordination games, then *compose* concepts to describe novel images. Zero-shot generalization without additional training. Key: multi-target games are crucial for concept decomposition — a concrete mechanism for inducing compositionality via task structure.

### Freeborn 2025 — "Compositional Understanding in Signaling Games"
[arXiv:2507.15706](https://arxiv.org/abs/2507.15706) | 2025

**Critical finding:** Even when senders produce compositional messages, standard receivers fail to interpret them compositionally. Novel receiver architectures (minimalist + generalist) where genuine compositional understanding evolves and persists. **Directly relevant to our receiver heterogeneity experiment — the receiver side is the bottleneck.**

### Sevestre & Dupoux 2025 — "Frequency & Compositionality in Emergent Communication"
[ACL Anthology](https://aclanthology.org/2025.emnlp-main.1387/) | EMNLP 2025

Tests whether high-frequency words resist compositional structure (as in natural language). Finding: *limited data exposure* — not frequency itself — drives compositionality. Dissociates frequency from sample count. **Supports iterated learning mechanism: the bottleneck works because of limited examples, not frequency effects.**

### Ben Zion et al. 2024 — "Semantics and Spatiality of Emergent Communication"
[arXiv:2411.10173](https://arxiv.org/abs/2411.10173) | NeurIPS 2024

Introduces *semantic consistency* — messages must maintain consistent meanings across instances. Proves formally that semantically inconsistent protocols can be optimal for discrimination but NOT for reconstruction. Reconstruction objectives encourage *spatial meaningfulness* (distance-preserving). **Training objective matters more than architecture: reconstruction > discrimination for structured communication.**

### Piriyajitakonkij et al. 2025 — "From Grunts to Lexicons"
[arXiv:2505.12872](https://arxiv.org/abs/2505.12872) | 2025

Deep RL in partially-observable foraging worlds → agents spontaneously develop communication exhibiting arbitrariness, displacement, cultural transmission, and compositionality. Group size, interpersonal dynamics, and temporal dependencies modulate which features emerge. Ecological pressures shape compositionality.

### Lee et al. 2026 — "Image, Word and Thought: Iterated Learning at Scale"
[arXiv:2601.02911](https://arxiv.org/abs/2601.02911) | 2026

Extends iterated learning to complex meaning spaces (128 seven-segment display glyphs) via semi-supervised autoencoder. Languages emerge that are simultaneously expressive, compositional, and stable. **Demonstrates iterated learning scales to non-trivial meaning spaces — validates our experiment design.**

## Compositionality Metrics (RELEVANT)

### Carmeli et al. 2024 — "Concept-Best-Matching"
[arXiv:2403.14705](https://arxiv.org/abs/2403.14705) | 2024

Finds best-match between emerged "words" and concepts, yielding both compositionality score and interpretable translation map. Prior metrics (topsim) don't reliably correlate with task success. More direct measurement.

### Lee et al. 2024 — "Geometric Signatures of Compositionality"
[arXiv:2410.01444](https://arxiv.org/abs/2410.01444) | ACL 2025

Compositionality reflected in intrinsic dimension (ID) of representations. Nonlinear ID encodes semantic composition; linear ID encodes superficial patterns. Geometric diagnostic for compositionality without task-level evaluation.

### Boldt & Mortensen 2025 — "Searching for the Most Human-like Emergent Language"
[arXiv:2510.03467](https://arxiv.org/abs/2510.03467) | 2025

Hyperparameter optimization with XferBench (transfer learning benchmark) to find EC languages resembling human language. Entropy is strongly predictive of transfer learning performance. Maps EC configuration space to human-language similarity — practical guide for calibrating experiments.

### Levy et al. 2025 — "Unsupervised Translation of Emergent Communication"
[arXiv:2502.07552](https://arxiv.org/abs/2502.07552) | AAAI 2025

First unsupervised NMT applied to EC protocols. Task complexity (*semantic diversity*) enhances translatability; constrained variation produces pragmatic but harder-to-interpret communication. Translatability as indirect compositionality measure.

### Zhang 2024 — "A Combinatorial Approach to Neural Emergent Communication"
[arXiv:2410.18806](https://arxiv.org/abs/2410.18806) | COLING 2025

**Methodological warning:** Identifies a *sampling pitfall* — standard Lewis game datasets are too easy, successful communication typically requires only 1-2 symbols. Introduces SolveMinSym algorithm for calibrated symbolic complexity. Higher complexity → more effective symbols. **Must verify our 4-attr × 8-val setup actually requires all 4 message positions.**

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

### Elmoznino et al. 2025 — "A Complexity-Based Theory of Compositionality"
[arXiv:2410.14817](https://arxiv.org/abs/2410.14817) | ICML 2025

Formal mathematical definition grounded in algorithmic information theory: representation is compositional if (1) expressive, (2) re-describable as discrete symbolic sequences with recombinable parts, (3) mapping has low Kolmogorov complexity. Principled ceiling against which topsim and related measures could be compared.

### Tucker et al. 2025 — "Human-Like EC via Utility, Informativeness, Complexity"
[Open Mind (MIT Press)](https://pmc.ncbi.nlm.nih.gov/articles/PMC11984795/) | 2025

VQ-VIB (Vector-Quantized Variational Information Bottleneck) unifies task-specific utility with info-theoretic efficiency. Informativeness pressure accelerates learning; complexity pressure yields human-like systems. IB framework directly applicable to compositionality pressures.

### Peters et al. 2025 — "Emergent Language: A Survey and Taxonomy"
[Springer](https://link.springer.com/article/10.1007/s10458-025-09691-y) | Autonomous Agents and MAS

Comprehensive survey focused on EC *metrics and quantification*. Systematic taxonomy addressing field fragmentation. Useful as reference map.

### Hill et al. 2025 — "Communicating Plans, Not Percepts"
[arXiv:2508.02912](https://arxiv.org/abs/2508.02912) | 2025

World-model-based communication (compressing planned futures into messages) outperforms emergent communication in multi-agent coordination. **Negative result for pure EC:** when agents have predictive world models, structured protocols beat emergent ones. Relevant to machine-only language design.

## Pressure Taxonomy (synthesized from above, 32 papers)

| Pressure | Mechanism | Evidence Strength |
|----------|-----------|-------------------|
| Iterated learning | Bottleneck selects for learnable=compositional | STRONG (CLIP scale, 128-glyph scale, our experiment) |
| Receiver heterogeneity | Different listener needs force generalizable encoding | STRONG |
| Multi-target decomposition | Multiple referents force concept codebook | STRONG (Carmeli 2025, ICLR) |
| Channel noise (Goldilocks) | Corrupted holistic codes fail; compositional degrade gracefully | MODERATE |
| Compressive-expressive tradeoff | Info bottleneck + expressivity = compositionality at Pareto frontier | MODERATE |
| Expanding meaning space | Open-ended referents overwhelm holistic memorization | MODERATE |
| Length / production cost | Penalizing length forces efficient reuse of sub-components | MODERATE |
| Limited data exposure | Fewer examples → compositional generalization (not frequency) | MODERATE (Sevestre 2025) |
| Reconstruction objective | Reconstruction > discrimination for semantic consistency | MODERATE (Ben Zion 2024) |

**Negative results:**
| Non-pressure | Why it fails | Source |
|-------------|-------------|--------|
| Scale alone | Larger models + more data don't fix compositionality | Zheng 2024 |
| Grokking / regularization | Weight decay compresses holistic codes, doesn't replace them | Our H17 |
| Pure EC vs structured protocols | World models beat EC when agents have planning capacity | Hill 2025 |
| Standard receiver architecture | Receivers fail to interpret compositional messages | Freeborn 2025 |
