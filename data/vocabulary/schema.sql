-- Limn Vocabulary Database Schema
-- Version: 1.0
-- Date: 2026-01-31

-- Domains (semantic categories)
CREATE TABLE domains (
    id INTEGER PRIMARY KEY,
    name VARCHAR(50) NOT NULL,
    description TEXT
);

-- Core vocabulary
CREATE TABLE words (
    word VARCHAR(4) PRIMARY KEY,
    source VARCHAR(50) NOT NULL,
    meaning VARCHAR(100) NOT NULL,
    examples TEXT,
    domain_id INTEGER REFERENCES domains(id),
    added_date DATE DEFAULT (CURRENT_DATE),
    notes TEXT
);

-- Operators (grammar words with special semantics)
CREATE TABLE operators (
    word VARCHAR(4) PRIMARY KEY,
    op_type VARCHAR(20) NOT NULL,  -- unary, quantifier, reference, comparator, scope, sequence
    precedence FLOAT,
    associativity VARCHAR(10),  -- left, right, none
    description TEXT
);

-- Etymology tracking
CREATE TABLE etymology (
    word VARCHAR(4) PRIMARY KEY REFERENCES words(word),
    source_lang VARCHAR(20),
    source_word VARCHAR(50),
    derivation VARCHAR(20),  -- truncation, whole-word, root
    notes TEXT
);

-- Collision log (historical record of resolved collisions)
CREATE TABLE collision_log (
    id INTEGER PRIMARY KEY,
    word VARCHAR(4) NOT NULL,
    meaning1 VARCHAR(100) NOT NULL,
    meaning2 VARCHAR(100) NOT NULL,
    resolution TEXT NOT NULL,
    resolved_date DATE DEFAULT (CURRENT_DATE),
    resolved_by VARCHAR(50)
);

-- Insert domains
INSERT INTO domains (id, name, description) VALUES
(1, 'Physical World', 'Matter states, elements, materials, properties, actions'),
(2, 'Space & Position', 'Directions, positions, regions, scales'),
(3, 'Time & Change', 'Time points, durations, change patterns'),
(4, 'Living Things', 'Life, body parts, organisms, biological processes'),
(5, 'Mind & Cognition', 'Mental states, senses, emotions, cognition'),
(6, 'Communication', 'Speech acts, language, expression'),
(7, 'Social', 'Relationships, groups, social actions'),
(8, 'Abstract', 'Quantities, logic, values, meta-concepts'),
(9, 'Operators', 'Grammar operators and function words'),
(10, 'Metalinguistic', 'Words about language and meaning'),
(11, 'Agent/AI', 'Vocabulary for AI and agent concepts');
