-- Limn Bootstrap Schema: Turing Completeness Test for Languages
--
-- This schema tracks Limn's ability to define itself - the ultimate test
-- of whether a language is complete enough to be self-referential.
--
-- Version controlled in Dolt to track evolution of self-definition capability.

-- =============================================================================
-- LIMN DEFINITIONS: Words defined IN Limn
-- =============================================================================

CREATE TABLE IF NOT EXISTS limn_definitions (
  -- Primary identification
  word VARCHAR(10) PRIMARY KEY,

  -- Core definition (in Limn!)
  definition_limn TEXT NOT NULL,

  -- English translation (for reference, not canonical)
  definition_english TEXT,

  -- Bootstrap hierarchy
  bootstrap_layer INT NOT NULL,
  -- 1 = Primitives (bei, hav, act, cha, sta, thi)
  -- 2 = Basic properties (big, sma, mor, les, one, man)
  -- 3 = Physical world (aqu, pyr, sol, liq, hot, col)
  -- 4 = Life & biology (liv, die, gro, bod, see, hea)
  -- 5 = Mind & cognition (min, kno, und, rem, lea)
  -- 6 = Social & human (fri, foe, fam, hel, hur)
  -- 7 = Philosophy (kar, dha, eud, aret, tao)

  -- Definitional status
  is_axiom BOOLEAN DEFAULT FALSE,
  -- TRUE if concept is irreducible (cannot be defined from simpler terms)

  axiom_reason TEXT,
  -- Why this must be an axiom (e.g., "circular with kno/und", "irreducible value")

  -- Circular dependencies
  circular_with JSON,
  -- Array of words in circular dependency chain
  -- Example: ["kno", "und"] for mutual definition

  -- Dependencies
  depends_on JSON,
  -- Array of words used in this definition
  -- Enables: "show dependency graph", "what breaks if we remove X?"

  unlocks_count INT DEFAULT 0,
  -- How many other words can be defined once this exists
  -- Metric: strategic importance of word

  -- Examples in Limn
  examples_limn JSON,
  -- Array of example sentences using this word
  -- Shows usage, not just definition

  -- Metadata
  definable BOOLEAN DEFAULT TRUE,
  -- FALSE if gaps prevent definition (missing vocabulary, collision)

  gap_blocker VARCHAR(20),
  -- Reference to vocabulary_gaps.gap_id if blocked

  -- Versioning
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  updated_by VARCHAR(50),
  definition_version INT DEFAULT 1,

  -- Validation
  semantic_coherence FLOAT,
  -- Embedding similarity between Limn definition and English concept
  -- Computed by Limn-native embedder

  CHECK (bootstrap_layer >= 1 AND bootstrap_layer <= 7),
  CHECK (semantic_coherence IS NULL OR (semantic_coherence >= 0 AND semantic_coherence <= 1))
);

-- Indexes for common queries
CREATE INDEX idx_bootstrap_layer ON limn_definitions(bootstrap_layer);
CREATE INDEX idx_is_axiom ON limn_definitions(is_axiom);
CREATE INDEX idx_definable ON limn_definitions(definable);
CREATE INDEX idx_coherence ON limn_definitions(semantic_coherence);

-- =============================================================================
-- VOCABULARY GAPS: What prevents complete bootstrap
-- =============================================================================

CREATE TABLE IF NOT EXISTS vocabulary_gaps (
  -- Primary identification
  gap_id VARCHAR(20) PRIMARY KEY,

  -- Gap classification
  gap_type ENUM(
    'collision',      -- Word has conflicting meanings (thi = thing/think)
    'circular',       -- Circular definition chain (kno ← und ← kno)
    'missing',        -- Required word doesn't exist (abl, cho, suf)
    'ambiguous',      -- One word, multiple meanings (telo = end/purpose)
    'irreducible'     -- Cannot be defined from primitives (axiom candidate)
  ) NOT NULL,

  -- Detailed description
  title TEXT NOT NULL,
  description TEXT NOT NULL,

  -- Impact analysis
  severity ENUM('critical', 'high', 'medium', 'low') NOT NULL,

  blocked_words JSON,
  -- Array of words that cannot be defined due to this gap
  -- Metric: scope of impact

  blocked_count INT GENERATED ALWAYS AS (JSON_LENGTH(blocked_words)) STORED,
  -- Auto-computed count for sorting by impact

  -- Domain impact
  domains_blocked JSON,
  -- Array of conceptual domains affected
  -- Example: ["cognitive", "social", "modal"]

  -- Solutions
  proposed_solutions JSON,
  -- Array of proposed fixes
  -- Example: [{"type": "add_word", "word": "cog", "meaning": "think"}]

  accepted_solution TEXT,
  -- Which solution was implemented (if resolved)

  -- Examples
  example_failed_definition TEXT,
  -- Limn definition attempt that fails

  example_reason TEXT,
  -- Why it fails

  -- Tracking
  bead_id VARCHAR(20),
  -- Link to Gas Town bead tracking this issue

  discovered_date DATE NOT NULL,
  resolved_date DATE,

  status ENUM('open', 'in_progress', 'resolved', 'wont_fix') DEFAULT 'open',

  -- Versioning
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

  CHECK (resolved_date IS NULL OR resolved_date >= discovered_date)
);

-- Indexes
CREATE INDEX idx_gap_type ON vocabulary_gaps(gap_type);
CREATE INDEX idx_severity ON vocabulary_gaps(severity);
CREATE INDEX idx_status ON vocabulary_gaps(status);
CREATE INDEX idx_blocked_count ON vocabulary_gaps(blocked_count DESC);

-- =============================================================================
-- BOOTSTRAP AXIOMS: Undefined primitives that unlock definitions
-- =============================================================================

CREATE TABLE IF NOT EXISTS bootstrap_axioms (
  -- Primary identification
  axiom_word VARCHAR(10) PRIMARY KEY,

  -- Justification
  axiom_type ENUM(
    'cognitive',      -- Mental primitives (kno, fee, thi)
    'volitional',     -- Will/desire (wan, nee)
    'value',          -- Ethical/truth values (goo, bad, tru, fal)
    'modal',          -- Possibility/necessity (can, mus)
    'social',         -- Interpersonal (per, we)
    'existential'     -- Being/existence (bei, exi)
  ) NOT NULL,

  reason TEXT NOT NULL,
  -- Why this concept is irreducible to simpler terms

  -- Alternative approaches considered
  attempted_definitions JSON,
  -- Array of failed definition attempts
  -- Shows we tried to define it, couldn't

  circular_dependencies JSON,
  -- What circular chains this axiom breaks
  -- Example: Declaring "kno" as axiom breaks [kno ← und ← kno]

  -- Impact
  unlocks JSON,
  -- Array of words that can be defined if this is accepted as axiom

  unlocks_count INT GENERATED ALWAYS AS (JSON_LENGTH(unlocks)) STORED,
  -- Auto-computed for sorting by strategic value

  -- Related philosophical traditions
  similar_concepts JSON,
  -- Array of {tradition, concept, note}
  -- Example: [{"tradition": "Descartes", "concept": "cogito", "note": "I think therefore I am"}]

  -- Decision tracking
  accepted BOOLEAN DEFAULT FALSE,
  accepted_date DATE,
  accepted_by VARCHAR(50),
  rationale TEXT,
  -- Why we chose to accept this as axiom vs. trying to define it

  -- Versioning
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- Indexes
CREATE INDEX idx_axiom_type ON bootstrap_axioms(axiom_type);
CREATE INDEX idx_accepted ON bootstrap_axioms(accepted);
CREATE INDEX idx_unlocks_count ON bootstrap_axioms(unlocks_count DESC);

-- =============================================================================
-- DEFINITION EXAMPLES: Usage examples in Limn
-- =============================================================================

CREATE TABLE IF NOT EXISTS definition_examples (
  example_id INT AUTO_INCREMENT PRIMARY KEY,

  word VARCHAR(10) NOT NULL,

  -- Example sentence in Limn
  sentence_limn TEXT NOT NULL,

  -- English translation
  sentence_english TEXT NOT NULL,

  -- Context
  domain VARCHAR(50),
  -- philosophical, physical, social, cognitive, etc.

  tradition VARCHAR(50),
  -- Buddhist, Aristotelian, Daoist, Stoic, etc.

  -- Embedding similarity
  limn_to_english_similarity FLOAT,
  -- Computed by Limn-native embedder

  -- Source
  source_experiment VARCHAR(50),
  -- Which experiment generated this example

  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  FOREIGN KEY (word) REFERENCES limn_definitions(word) ON DELETE CASCADE,

  CHECK (limn_to_english_similarity IS NULL OR
         (limn_to_english_similarity >= 0 AND limn_to_english_similarity <= 1))
);

CREATE INDEX idx_word ON definition_examples(word);
CREATE INDEX idx_domain ON definition_examples(domain);
CREATE INDEX idx_similarity ON definition_examples(limn_to_english_similarity DESC);

-- =============================================================================
-- BOOTSTRAP METRICS: Track completeness over time
-- =============================================================================

CREATE TABLE IF NOT EXISTS bootstrap_metrics (
  metric_date DATE PRIMARY KEY,

  -- Definition counts
  total_vocabulary INT NOT NULL,
  -- Total Limn words in vocabulary

  defined_in_limn INT NOT NULL,
  -- How many have Limn definitions

  definable_count INT NOT NULL,
  -- How many could theoretically be defined (no gaps blocking)

  axiom_count INT NOT NULL,
  -- How many accepted axioms

  -- Completeness metrics
  bootstrap_completeness FLOAT GENERATED ALWAYS AS
    (defined_in_limn / NULLIF(total_vocabulary, 0)) STORED,
  -- Percentage of vocabulary with Limn definitions

  -- Gap metrics
  open_gaps INT NOT NULL,
  critical_gaps INT NOT NULL,
  blocked_word_count INT NOT NULL,
  -- Total words blocked by all gaps

  -- Layer coverage
  layer1_coverage FLOAT,
  layer2_coverage FLOAT,
  layer3_coverage FLOAT,
  layer4_coverage FLOAT,
  layer5_coverage FLOAT,
  layer6_coverage FLOAT,
  layer7_coverage FLOAT,
  -- Percentage of each layer defined

  -- Quality metrics
  mean_semantic_coherence FLOAT,
  -- Average embedding similarity of Limn definitions

  circular_definition_count INT,
  -- How many circular dependencies exist

  -- Notes
  notes TEXT,

  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  CHECK (bootstrap_completeness >= 0 AND bootstrap_completeness <= 1)
);

CREATE INDEX idx_completeness ON bootstrap_metrics(bootstrap_completeness DESC);
CREATE INDEX idx_metric_date ON bootstrap_metrics(metric_date DESC);

-- =============================================================================
-- VIEWS: Useful queries
-- =============================================================================

-- Words blocked by gaps
CREATE VIEW words_blocked_by_gaps AS
SELECT
  g.gap_id,
  g.gap_type,
  g.severity,
  g.blocked_count,
  JSON_UNQUOTE(JSON_EXTRACT(g.blocked_words, CONCAT('$[', idx, ']'))) AS blocked_word
FROM vocabulary_gaps g
CROSS JOIN (
  SELECT 0 AS idx UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4
  UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9
) AS indices
WHERE idx < JSON_LENGTH(g.blocked_words)
  AND g.status = 'open';

-- Bootstrap layers progress
CREATE VIEW bootstrap_layer_progress AS
SELECT
  bootstrap_layer,
  COUNT(*) AS total_words,
  SUM(CASE WHEN definition_limn IS NOT NULL THEN 1 ELSE 0 END) AS defined_words,
  SUM(CASE WHEN definable = FALSE THEN 1 ELSE 0 END) AS blocked_words,
  SUM(CASE WHEN is_axiom = TRUE THEN 1 ELSE 0 END) AS axiom_words,
  AVG(semantic_coherence) AS avg_coherence
FROM limn_definitions
GROUP BY bootstrap_layer
ORDER BY bootstrap_layer;

-- Most strategic words (unlock the most others)
CREATE VIEW strategic_words AS
SELECT
  word,
  unlocks_count,
  bootstrap_layer,
  is_axiom,
  definable,
  gap_blocker
FROM limn_definitions
WHERE unlocks_count > 5
ORDER BY unlocks_count DESC;

-- Circular dependency chains
CREATE VIEW circular_chains AS
SELECT
  word,
  definition_limn,
  circular_with,
  JSON_LENGTH(circular_with) AS chain_length
FROM limn_definitions
WHERE circular_with IS NOT NULL
  AND JSON_LENGTH(circular_with) > 0
ORDER BY chain_length DESC;

-- =============================================================================
-- COMMENTS
-- =============================================================================

COMMENT ON TABLE limn_definitions IS
'Limn words defined IN Limn - the core of the self-reference test. Each definition proves Limn can explain its own vocabulary.';

COMMENT ON TABLE vocabulary_gaps IS
'Gaps that prevent complete bootstrap. These are not bugs - they reveal fundamental linguistic boundaries and design decisions.';

COMMENT ON TABLE bootstrap_axioms IS
'Irreducible primitives that cannot be defined from simpler terms. Accepting axioms is a strategic decision to break circular chains.';

COMMENT ON TABLE bootstrap_metrics IS
'Historical tracking of bootstrap completeness. Shows evolution toward self-sufficiency.';
