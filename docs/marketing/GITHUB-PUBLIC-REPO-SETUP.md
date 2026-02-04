# GitHub Public Repository Setup

**Target Repo:** github.com/ericfode/limn-lang (or similar)
**Status:** Ready for creation
**Prepared:** 2026-02-01

---

## Repository Name Options

1. **limn-lang** (Recommended) - Clear, concise, matches convention
2. **limn** - Simple, but may conflict with tools/projects
3. **limn-language** - Explicit but verbose
4. **limn-conlang** - Clarifies it's a constructed language

**Recommendation:** `limn-lang`

---

## Repository Description

**Short:**
"Limn: A constructed language with constraint-based semantics and key-collapsible ambiguity. Empirically validated 52% compositionality advantage."

**Long (for README):**
See draft README below.

---

## Public Repository Structure

```
limn-lang/
├── README.md                  # Main introduction
├── LICENSE                    # Choose license (MIT recommended)
├── CONTRIBUTING.md            # How to contribute (Prolog-only policy)
├── .gitignore                # Standard ignores
├── docs/
│   ├── spec/
│   │   ├── bootstrap-v3-natural.md
│   │   ├── grammar-formal.md
│   │   ├── vocabulary-v3-natural.md (or reference to DoltHub)
│   │   └── LIMN-PL-SPECIFICATION.md
│   ├── theory/
│   │   ├── liminal-semantics.md
│   │   ├── key-mechanism.md
│   │   └── constraint-intersection.md
│   ├── guides/
│   │   ├── VOCAB-MANAGEMENT.md
│   │   └── GETTING-STARTED.md
│   └── examples/
│       ├── hello-world.limn
│       ├── temperature-converter.limn
│       └── semantic-examples.md
├── experiments/
│   ├── INDEX.md
│   ├── 005-FINAL-REPORT.md (compositionality study)
│   └── zero-bootstrap-validation/
├── tools/
│   ├── linter/
│   │   └── limn-lint.pl
│   └── vocab/
│       └── vocab.sh
├── examples/                  # More example programs
└── tests/                     # Test suites
```

---

## Draft README.md for Public Repo

```markdown
# Limn

**A constructed language with constraint-based semantics**

[![GitHub stars](https://img.shields.io/github/stars/ericfode/limn-lang.svg)](https://github.com/ericfode/limn-lang/stargazers)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Vocabulary: 938 words](https://img.shields.io/badge/vocabulary-938%20words-blue)](https://www.dolthub.com/repositories/ericfode/limn)

## What is Limn?

Limn is a constructed language where **words define constraints** in semantic space, and **meaning emerges from their intersection**. It's designed for efficient LLM-human communication with empirically validated properties.

### Example

```limn
sol liq tra
→ solid liquid transformation
→ water freezing/melting
```

```limn
hot col bet
→ hot cold between
→ lukewarm
```

```limn
joy sad lim | nu sta | alw cha
→ joy sadness liminal. not static. always changing.
→ bittersweet—emotions in flux
```

## Key Properties

- **🎯 52% More Compositional** than English (empirically validated)
- **📚 Zero-Bootstrap Learnable** (77-85% comprehension without training)
- **🔑 Key-Collapsible Ambiguity** (precise with context, ambiguous without)
- **🧮 3-Letter Vocabulary** (938 words, 26 semantic domains)
- **💻 Programming Language Extension** (Limn-PL for declarative computation)

## Quick Start

### Learn Limn in 10 Minutes

Start with the [Bootstrap v3-Natural](docs/spec/bootstrap-v3-natural.md) - designed for zero-training learning.

### Example Sentences

```limn
# Physical World
fir hot bri      → fire hot bright
sol aqu tra      → solid water transformation (ice ↔ water)
liq flo fas      → liquid flows fast

# Emotions & Abstract
lov los gri      → love loss grief
fea cou bat      → fear courage battle
tru fal lim      → true false liminal (uncertain)

# With Operators
nu hot           → not hot (cold)
ve bri           → very bright
al hum dre       → all humans dream
ex sol aqu       → some solid water (ice exists)

# Compositional Patterns
beg mid end | cyc | alw
→ beginning middle end. cycle. always.
→ eternal cycle
```

## Empirical Validation

### Experiment 005: Compositional Semantics

Tested whether Limn phrases are more compositional than English using embedding similarity.

**Results:**
- **Limn:** 0.88 mean similarity (embed(A B) ≈ embed(A) + embed(B))
- **English:** 0.58 mean similarity
- **Advantage:** 52% more compositional
- **Statistical significance:** p = 0.0059, Cohen's d = 2.06
- **Wins:** 10/11 direct comparisons

[📊 Full Report](experiments/005-FINAL-REPORT.md)

### Zero-Bootstrap Validation

Can humans learn Limn without training?

**Results:**
- **77-85% comprehension** from bootstrap document alone
- **Natural extensions principle** validated
- **First-syllable extraction** enables confident guessing
- **Latin/Greek roots** universally recognizable

[📋 Validation Report](experiments/zero-bootstrap-validation-log.md)

## Documentation

### Language Specification
- [📖 Bootstrap v3-Natural](docs/spec/bootstrap-v3-natural.md) - Learn Limn (zero-training)
- [📝 Formal Grammar](docs/spec/grammar-formal.md) - Complete grammar specification
- [📚 Vocabulary](https://www.dolthub.com/repositories/ericfode/limn) - 938 words (DoltHub database)
- [💻 Limn-PL Specification](docs/spec/LIMN-PL-SPECIFICATION.md) - Programming language extension

### Theory
- [🔗 Liminal Semantics](docs/theory/liminal-semantics.md) - How contradictions resolve
- [🔑 Key Mechanism](docs/theory/key-mechanism.md) - How keys collapse ambiguity
- [📐 Constraint Intersection](docs/theory/constraint-intersection.md) - Mathematical foundation

### Guides
- [🎓 Getting Started](docs/guides/GETTING-STARTED.md) - New learner guide
- [📋 Vocabulary Management](docs/guides/VOCAB-MANAGEMENT.md) - How to add words
- [🔧 Contributing](CONTRIBUTING.md) - How to contribute (Prolog-only)

### Experiments
- [🧪 Experiment Index](experiments/INDEX.md) - All 49 experiments cataloged
- [📊 Experiment 005](experiments/005-FINAL-REPORT.md) - Compositionality study

## Implementation

**Limn uses Prolog exclusively** (engineer-approved).

All interpreters, tools, and language infrastructure use Prolog to leverage its natural fit for constraint-based semantics.

### Tools

```bash
# Vocabulary management
./scripts/vocab.sh stats           # 938 words, 26 domains
./scripts/vocab.sh search fire     # Search words
./scripts/vocab.sh check xyz       # Check availability

# Linting (Prolog)
swipl -s tools/linter/limn-lint.pl -g "lint_file('example.limn')"
```

## Vocabulary Database

**938 words | 26 domains | 16 operators**

Stored in Dolt database with version control:

```bash
# Clone vocabulary
dolt clone ericfode/limn
cd limn
dolt sql -q "SELECT word, meaning FROM words WHERE domain_id = 1"
```

**Browse online:** [DoltHub Repository](https://www.dolthub.com/repositories/ericfode/limn)

## Examples

### Temperature Converter (Limn-PL)

```limn
pro tem:
  var cel fah
  cns
    cel tim 9 div 5 equ fah min 32
```

Run bidirectionally:
```limn
run tem whe cel = 100  → fah = 212
run tem whe fah = 32   → cel = 0
```

### More Examples

See [examples/](examples/) directory for:
- Semantic examples
- Programming patterns
- Translation challenges
- Poetry and creative writing

## Contributing

We welcome contributions! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for:
- **Prolog-only policy** (all code must be in Prolog)
- Vocabulary addition guidelines
- Experiment proposals
- Documentation improvements

**Areas needing help:**
- Prolog interpreter development
- Translation examples
- Empirical validation experiments
- Tool development

## Community

- **GitHub:** [ericfode/limn-lang](https://github.com/ericfode/limn-lang)
- **Reddit:** [r/limn](https://reddit.com/r/limn) (coming soon)
- **DoltHub:** [Vocabulary Database](https://www.dolthub.com/repositories/ericfode/limn)
- **Issues:** [GitHub Issues](https://github.com/ericfode/limn-lang/issues)

## License

MIT License - see [LICENSE](LICENSE) file.

## Citation

If you use Limn in research, please cite:

```bibtex
@misc{limn2026,
  title={Limn: A Constructed Language with Constraint-Based Semantics},
  author={Fode, Eric},
  year={2026},
  url={https://github.com/ericfode/limn-lang}
}
```

## Roadmap

- [x] Core vocabulary (938 words, 26 domains)
- [x] Formal grammar specification
- [x] Zero-bootstrap validation (77-85% success)
- [x] Compositionality validation (52% advantage)
- [ ] Prolog interpreter (in progress)
- [ ] Limn-PL implementation
- [ ] Discord community
- [ ] Wikipedia article
- [ ] Academic publication

---

*amb mys | mea eme | gro con*

*[ambiguous mystery. meaning emerges. growth continues.]*
```

---

## Repository Settings

### Visibility
- **Public** ✓

### Features to Enable
- [x] Issues
- [x] Wiki (for community documentation)
- [x] Discussions (for Q&A, translation challenges)
- [x] Projects (for roadmap tracking)
- [ ] Packages (not needed initially)

### Topics/Tags
Add these topics for discoverability:
- `constructed-language`
- `conlang`
- `language-design`
- `prolog`
- `nlp`
- `linguistics`
- `semantic-web`
- `constraint-programming`
- `logic-programming`

### Protection Rules
- Require pull request reviews for main branch
- No force pushing to main
- Require status checks to pass

---

## Initial Setup Commands

```bash
# Create new repo on GitHub (via web interface)
# Then locally:

cd /path/to/limn/crew/student
git remote add public git@github.com:ericfode/limn-lang.git

# Push selective content (don't push crew-specific files)
git push public master

# Or create new repo with curated content:
mkdir limn-lang-public
cd limn-lang-public
git init

# Copy curated content
cp -r ../limn/crew/student/docs .
cp -r ../limn/crew/student/experiments .
cp -r ../limn/crew/student/examples .
cp -r ../limn/crew/student/tools .

# Create public README (using draft above)
# Create LICENSE (MIT recommended)
# Create CONTRIBUTING.md (Prolog-only policy)
# Create .gitignore

git add .
git commit -m "Initial public release of Limn language"
git remote add origin git@github.com:ericfode/limn-lang.git
git push -u origin main
```

---

## License Recommendation

**MIT License** - Permissive, widely used, encourages adoption

```
MIT License

Copyright (c) 2026 Eric Fode

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

---

## Human Execution Checklist

### Pre-Creation
- [ ] Decide repository name (recommend: limn-lang)
- [ ] Prepare curated content (exclude crew-specific files)
- [ ] Review documentation for public consumption
- [ ] Create LICENSE file (MIT recommended)
- [ ] Create public README (draft provided above)

### Creation
- [ ] Create repository on GitHub
- [ ] Set visibility to Public
- [ ] Initialize with README, LICENSE
- [ ] Add topics/tags for discoverability
- [ ] Enable Issues, Wiki, Discussions

### Initial Push
- [ ] Push curated content to public repo
- [ ] Verify all links work
- [ ] Add branch protection rules
- [ ] Create initial GitHub Issues for roadmap

### Post-Creation
- [ ] Announce on r/conlangs
- [ ] Add to Wikipedia external links
- [ ] Link from DoltHub vocabulary repo
- [ ] Share on social media

---

**Content prepared:** ✓ README draft, structure, license
**Setup guide:** ✓ Commands and checklist ready
**Documentation:** ✓ Curated content list

Ready for human execution (limn-hzy6).

---

*Prepared by Kira (Archivist)*
