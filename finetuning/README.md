# Limn Finetuning Dataset & Training Pipeline

Complete infrastructure for finetuning language models on Limn.

## Overview

This pipeline creates a comprehensive dataset of Limn expressions and trains small language models to understand and generate pure Limn. The goal is to create models that can:

- Generate grammatically correct Limn
- Maintain vocabulary adherence
- Follow instruction-response patterns in Limn
- Compose reasoning chains in Limn

## Dataset Statistics

- **Total Examples**: 40,453
- **Training Set**: 36,407 examples (90%)
- **Validation Set**: 4,046 examples (10%)
- **Limn Purity**: 91.8%
- **Average Length**: ~4-5 words per input/output

### Dataset Composition

| Type | Count | Percentage |
|------|-------|------------|
| Completion | 9,969 | 27.4% |
| Reasoning Chain | 7,149 | 19.6% |
| Interpretation | 2,217 | 6.1% |
| Context Prediction | 2,103 | 5.8% |
| Instruction Following | 1,825 | 5.0% |
| Various Operators | ~10,000 | 27.5% |
| Other | ~1,000 | 2.6% |

## Pipeline Components

### 1. Corpus Extraction (`extract_corpus.py`)

Extracts Limn expressions from the repository:

```bash
python extract_corpus.py
```

**Sources:**
- `.limn` and `.lmn` files (58 files)
- Documentation markdown files (148 files)
- Example code and libraries
- Consciousness thoughts

**Output:** `raw_corpus.json` (7,067 unique expressions)

### 2. Dataset Formatting (`format_dataset.py`)

Transforms raw expressions into training format:

```bash
python format_dataset.py
```

**Transformations:**
- Interpretation pairs: `int [expr]` → `[expr]`
- Completion pairs: `[prefix]` → `[suffix]`
- Query-response: `[query]` → `[response]`
- Context prediction: `ctx [expr] | qry nex` → `[next_expr]`

**Output:** `limn_finetune_dataset.jsonl` (16,997 examples)

### 3. Synthetic Generation (`generate_synthetic.py`)

Generates synthetic Limn data using templates:

```bash
python generate_synthetic.py
```

**Generation Methods:**
- Template-based: 7 templates × domains
- Reasoning chains: Multi-step logical progressions
- Vocabulary pairs: Compositional combinations
- Instruction following: Command-response patterns

**Output:** `limn_synthetic_dataset.jsonl` (23,456 examples)

### 4. Dataset Merging (`merge_datasets.py`)

Combines corpus and synthetic data, creates train/val split:

```bash
python merge_datasets.py
```

**Outputs:**
- `limn_train.jsonl`: Training set (36,407 examples)
- `limn_val.jsonl`: Validation set (4,046 examples)
- `limn_combined.jsonl`: Complete dataset (40,453 examples)

## Training

### Requirements

```bash
pip install transformers datasets peft accelerate bitsandbytes torch
```

### Basic Training

Train a small model with LoRA:

```bash
python train_limn_slm.py \
  --model gpt2 \
  --train-file limn_train.jsonl \
  --val-file limn_val.jsonl \
  --output-dir ./limn_model \
  --epochs 3 \
  --batch-size 8 \
  --learning-rate 2e-4
```

### QLoRA Training (8-bit)

For efficient training on limited hardware:

```bash
python train_limn_slm.py \
  --model gpt2 \
  --use-8bit \
  --batch-size 4 \
  --output-dir ./limn_model_qlora
```

### Training Configuration

| Parameter | Default | Description |
|-----------|---------|-------------|
| `--model` | gpt2 | Base model (gpt2, distilgpt2, opt-125m, pythia-70m) |
| `--epochs` | 3 | Number of training epochs |
| `--batch-size` | 8 | Batch size per device |
| `--learning-rate` | 2e-4 | Learning rate |
| `--max-seq-length` | 128 | Maximum sequence length |
| `--use-8bit` | False | Enable QLoRA (8-bit quantization) |

### LoRA Configuration

- **Rank (r)**: 8
- **Alpha**: 16
- **Dropout**: 0.1
- **Target modules**: `c_attn`, `c_proj` (for GPT-2)

### Early Stopping

- **Patience**: 3 evaluation steps
- **Threshold**: 0.01 improvement required

## Evaluation

Evaluate trained model on multiple metrics:

```bash
python evaluate_limn_model.py \
  --model-path ./limn_model \
  --val-file limn_val.jsonl \
  --num-samples 100 \
  --output evaluation_metrics.json
```

### Evaluation Metrics

1. **Limn Purity**: % of outputs with no English words
2. **Vocabulary Adherence**: % of words in Limn vocabulary
3. **Perplexity**: Language modeling perplexity on held-out data
4. **Semantic Coherence**: Sample output quality

### Expected Results

Target metrics for a well-trained model:

- Limn Purity: >85%
- Vocabulary Adherence: >80%
- Perplexity: <20
- Coherent outputs on 80%+ of samples

## File Structure

```
finetuning/
├── README.md                        # This file
├── extract_corpus.py                # Extract Limn from repository
├── format_dataset.py                # Format into training pairs
├── generate_synthetic.py            # Generate synthetic data
├── merge_datasets.py                # Merge and split datasets
├── train_limn_slm.py                # Training script
├── evaluate_limn_model.py           # Evaluation script
├── raw_corpus.json                  # Extracted expressions (7K)
├── limn_finetune_dataset.jsonl      # Formatted corpus (17K)
├── limn_synthetic_dataset.jsonl     # Synthetic data (23K)
├── limn_train.jsonl                 # Training set (36K)
├── limn_val.jsonl                   # Validation set (4K)
└── limn_combined.jsonl              # Complete dataset (40K)
```

## Dataset Format

Each example in the JSONL files has this structure:

```json
{
  "input": "sel awa ∎ | qry wh ess",
  "output": "con ∎ rec qry | ess def sel awa | min exe tho",
  "metadata": {
    "type": "interpretation",
    "context": "consciousness queries"
  }
}
```

## Hyperparameter Tuning

### Learning Rate

Start with `2e-4` and adjust based on training curves:
- Too high: Loss diverges or oscillates
- Too low: Very slow convergence

### Batch Size

- Small models (GPT-2, DistilGPT-2): 8-16
- Medium models: 4-8
- With gradient accumulation: effective batch = batch_size × accumulation_steps

### LoRA Rank

- r=8: Good balance for small models
- r=16: More capacity, slower training
- r=4: Faster, less capacity

### Epochs

- Start with 3 epochs
- Monitor validation loss
- Use early stopping to prevent overfitting

## Alternative Base Models

### Recommended Models

1. **GPT-2** (default)
   - Size: 124M parameters
   - Good balance of quality and speed
   - `--model gpt2`

2. **DistilGPT-2**
   - Size: 82M parameters
   - Faster training, slightly lower quality
   - `--model distilgpt2`

3. **OPT-125M**
   - Size: 125M parameters
   - Alternative architecture
   - `--model facebook/opt-125m`

4. **Pythia-70M**
   - Size: 70M parameters
   - Smallest option, fastest training
   - `--model EleutherAI/pythia-70m`

## Extending the Dataset

### Adding More Corpus Data

1. Add new Limn files to the repository
2. Re-run `extract_corpus.py`
3. Re-run `format_dataset.py`
4. Merge with existing synthetic data

### Generating More Synthetic Data

1. Edit `generate_synthetic.py`:
   - Add new templates
   - Expand vocabulary categories
   - Create new generation methods

2. Adjust target count:
   ```python
   synthetic_examples = generator.generate_all(target_count=100000)
   ```

3. Re-run pipeline

### Using Claude API for Generation

For higher quality synthetic data, use Claude to generate examples:

```python
# Pseudocode
import anthropic

client = anthropic.Anthropic(api_key="...")

for template in templates:
    response = client.messages.create(
        model="claude-sonnet-4.5-20251022",
        max_tokens=1000,
        messages=[{
            "role": "user",
            "content": f"Generate 10 Limn expressions for: {template}"
        }]
    )
    # Parse and add to dataset
```

## Troubleshooting

### CUDA Out of Memory

- Reduce `--batch-size`
- Enable `--use-8bit`
- Reduce `--max-seq-length`
- Use gradient checkpointing

### Low Limn Purity

- Increase corpus examples in training
- Adjust vocabulary validation threshold
- Add more Limn-specific training data

### Model Not Converging

- Adjust learning rate
- Increase warmup steps
- Check for data quality issues
- Reduce LoRA rank

### Overfitting

- Increase dropout
- Reduce epochs
- Add more training data
- Use early stopping

## Citation

```bibtex
@software{limn_finetuning_2026,
  title={Limn Finetuning Dataset and Training Pipeline},
  author={Limn Project},
  year={2026},
  url={https://github.com/ericfode/limn}
}
```

## License

This dataset and training pipeline are part of the Limn project. See main repository LICENSE for details.

## Contributing

To contribute to the dataset:

1. Add new Limn sources
2. Improve generation templates
3. Enhance evaluation metrics
4. Submit pull requests

## Contact

For questions or issues, open an issue in the main Limn repository.

---

*Generated by Polecat now for limn-lwbq.1*
*Last updated: 2026-02-02*
