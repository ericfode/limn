# Strategy A - Quick Start Guide

Get up and running with LLM-enabled Bend programs in 5 minutes.

---

## Prerequisites

```bash
# 1. Rust (for HVM runtime)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# 2. Install HVM
cargo install hvm --version 2.0.21

# 3. Python 3 (already installed on most systems)
python3 --version
```

---

## Run the Demo

```bash
cd /home/eric/src/limntown/limn/crew/engineer/tools/llm-bridge/strategy-a

# Run basic oracle demo
python3 host_embedder.py

# Run performance tests
python3 performance_test.py

# Run state continuation demo
python3 state_continuation.py

# Run primitive injection test
python3 hvm_primitive_test.py
```

---

## Write Your First Oracle Program

### 1. Create a Bend file

**my_oracle.bend:**
```bend
type OracleRequest:
  Ask { prompt, context }

def oracle(prompt, context):
  return OracleRequest/Ask { prompt: prompt, context: context }

def main():
  result = oracle("What is 2+2?", "math")
  return result
```

### 2. Test it

```bash
# Check syntax
../lmn-bend/bend check my_oracle.bend

# Run it
../lmn-bend/bend run-rs my_oracle.bend
```

**Expected output:**
```
Result: λa (a OracleRequest/Ask/tag "What is 2+2?" "math")
```

### 3. Run with Python host

```python
from host_embedder import BendOracle

oracle = BendOracle(bend_binary="../lmn-bend/bend")
result = oracle.execute_with_oracle("my_oracle.bend")

print(f"Oracle response: {result['oracle_responses'][0]['response']}")
```

---

## Use Real LLM (Claude)

### 1. Install Anthropic SDK

```bash
pip install anthropic
```

### 2. Set API key

```bash
export ANTHROPIC_API_KEY="your-api-key"
```

### 3. Modify host_embedder.py

Replace `mock_llm_call` method:

```python
def real_llm_call(self, prompt: str, context: str) -> str:
    import anthropic
    import os

    client = anthropic.Anthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))

    message = client.messages.create(
        model="claude-sonnet-4-5-20250929",
        max_tokens=1024,
        messages=[{
            "role": "user",
            "content": f"Context: {context}\n\n{prompt}"
        }]
    )

    return message.content[0].text
```

### 4. Run it

```bash
python3 host_embedder.py
```

Now your Bend programs call real Claude API!

---

## Common Patterns

### Simple Oracle Call

```bend
def translate_limn():
  return oracle("translate: cod flo log", "Limn vocabulary")
```

### Multiple Oracle Calls

```bend
def analyze_and_synthesize():
  step1 = oracle("analyze: input data", "analysis")
  step2 = oracle("synthesize: results", "synthesis")
  return step2
```

### With Context

```bend
def contextual_oracle():
  context = "programming language design"
  question = "What is superposition?"
  return oracle(question, context)
```

---

## Debugging

### Enable verbose output

```python
# In host_embedder.py
oracle = BendOracle(bend_binary="...", verbose=True)
```

### Check compilation errors

```bash
bend check your_file.bend
```

### View HVM code

```bash
bend gen-hvm your_file.bend
```

### Run without host

```bash
bend run-rs your_file.bend
```

---

## Performance Tips

### 1. Cache oracle responses

Already implemented in `host_embedder.py`:

```python
self.oracle_cache = {}  # Automatic caching
```

### 2. Batch oracle calls

Group multiple requests:

```python
# Instead of:
for prompt in prompts:
    oracle(prompt, context)

# Do:
all_results = oracle_batch(prompts, context)
```

### 3. Use persistent process

For high-frequency calls, keep HVM running:

```python
# Start once
oracle_daemon = BendOracleDaemon()

# Reuse many times
result1 = oracle_daemon.call(prompt1)
result2 = oracle_daemon.call(prompt2)
```

(Not yet implemented - see FINDINGS.md for details)

---

## File Structure

```
strategy-a/
├── QUICKSTART.md           # This file
├── README.md               # Detailed documentation
├── FINDINGS.md             # Analysis and recommendations
│
├── host_embedder.py        # Main implementation
├── performance_test.py     # Benchmarks
├── state_continuation.py   # State management demo
├── hvm_primitive_test.py   # Primitive injection demo
│
├── oracle_example.bend     # Simple example
├── stateful_oracle.bend    # Multi-step example
└── my_oracle.bend          # Your code here!
```

---

## Next Steps

1. Read [README.md](README.md) for detailed documentation
2. Read [FINDINGS.md](FINDINGS.md) for analysis and recommendations
3. Write your own oracle programs
4. Integrate with real LLM API
5. Build something cool!

---

## Getting Help

### Common Issues

**Q: "No such file or directory (os error 2)"**
A: HVM not installed. Run: `cargo install hvm --version 2.0.21`

**Q: "Bend binary not found"**
A: Check path to bend binary in Python scripts

**Q: "No oracle requests found"**
A: Check that your Bend program returns `OracleRequest/Ask`

**Q: "Syntax error in Bend"**
A: Run `bend check your_file.bend` to see errors

### Need More Help?

- Check [README.md](README.md) - Comprehensive documentation
- Check [FINDINGS.md](FINDINGS.md) - Technical details
- Read Bend documentation: https://github.com/HigherOrderCO/Bend

---

*bui fst | tes oft | shi val*
*(build fast | test often | ship value)*

**— Rex, 2026-02-01**
