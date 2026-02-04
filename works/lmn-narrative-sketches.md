# LMN Narrative Example Sketches

> Stories that execute. Meaning that calculates. Narrative as computation.

**For:** Phase 2 LMN implementation
**Author:** Yuki
**Status:** Sketches for Rex's review

---

## Vision

LMN allows stories to be programs. Not "interactive fiction with branching paths" but **narrative in superposition that collapses through reader context**.

Traditional IF: author pre-writes all branches
LMN narrative: author writes semantic intent, reader's context collapses meaning

---

## Example 1: "The Apology" (Simple)

### Concept
A character must apologize, but the nature of the apology depends on their relationship history and current emotional state.

### LMN Sketch

```lmn
# Setup: What happened
∿ was [argument about trust]
∿ was [pattern of broken promises]

# Present: Character decides to apologize
intent = [genuine_remorse, obligation, manipulation]
emotional_state = ~ assess [∿ was [history]]

# Collapse intent based on character's emotional state
chosen_intent = intent @ emotional_state

# Generate the apology
apology = ~ generate [
    intent: chosen_intent,
    context: ∿ was [history],
    recipient: other_character
]

# Future: How will it be received?
∿ wil [reception] = ~ predict [
    apology: apology,
    recipient_state: ∿ now [other's feelings],
    history: ∿ was [pattern]
] @ trust_level

# Output
∿ now [character speaks] → apology
∿ wil [other responds] → reception
```

### Why This Works

- **Temporal navigation**: Story moves through was/now/will
- **Superposition**: Intent exists in 3 states until collapsed
- **Context collapse**: Same intent produces different apologies based on history
- **Oracle integration**: LLM generates natural language from semantic intent
- **Replayable**: Different reader contexts produce different stories

---

## Example 2: "The Witness" (Medium)

### Concept
A character witnesses an event. Their memory of it changes over time based on what they learn later and who asks them about it.

### LMN Sketch

```lmn
# The event (objective)
∿ was [actual_event] = {
    # Deterministic facts
    time: "3:47pm",
    location: "intersection",
    participants: [driver, pedestrian],
    collision: true
}

# Initial perception (subjective)
∿ was [witness_saw] = ~ observe [
    event: actual_event,
    witness_angle: "from coffee shop",
    witness_state: "distracted, looking at phone"
]

# Time passes - new information
∿ was [learned_later] = [
    "driver had been drinking",
    "pedestrian was jaywalking",
    "witness's friend knows the driver"
]

# Present: Police ask for testimony
asker_context = police_investigation
time_passed = 3_days

# Memory reconstruction (collapses based on context)
remembered_event = [
    ∿ was [witness_saw],           # Original perception
    ∿ was [learned_later],         # New information
    ∿ now [social_pressure]        # Current context
] @ asker_context

# Generate testimony
testimony = ~ narrate [
    memory: remembered_event,
    confidence: false,  # They're not sure anymore
    stakes: "high"
]

# Later: Lawyer asks different questions
∿ wil [in_court] {
    asker_context = defense_attorney

    # Same event, different collapse
    court_memory = [
        ∿ was [witness_saw],
        ∿ was [learned_later],
        ∿ now [courtroom_pressure],
        ∿ was [testimony to police]  # Now that exists too!
    ] @ asker_context

    court_testimony = ~ narrate [
        memory: court_memory,
        confidence: false,
        contradiction_with: testimony  # Aware of inconsistency
    ]
}

# Show how memory shifts
∿ now [output] = {
    original: ∿ was [witness_saw],
    to_police: testimony,
    to_lawyer: ∿ wil [court_testimony],
    truth: ∿ was [actual_event]  # Always unchanging
}
```

### Why This Works

- **Demonstrates temporal complexity**: was/now/will interact
- **Shows context-dependent collapse**: Same superposition, different questions, different memories
- **Realistic**: Mirrors actual human memory reconstruction
- **Philosophical**: Questions nature of truth vs. memory
- **Executable**: Can run this with different contexts, get different testimonies

---

## Example 3: "The Letter" (Complex)

### Concept
A character finds an old letter. What it means depends on when they read it (temporal key), what they know now vs. knew then, and what they're hoping to find.

### LMN Sketch

```lmn
# The letter (objective text)
letter_text = "I never meant to hurt you. What we had was real. - M"

# Temporal contexts for reading
reader_state = [
    ∿ was [when_received] = {
        relationship: "active",
        feelings: "uncertain",
        age: 23
    },
    ∿ now [finding_again] = {
        relationship: "ended_years_ago",
        feelings: "nostalgic",
        age: 45,
        life_since: "married someone else, divorced"
    }
]

# The letter exists in superposition
letter_meaning = [
    "genuine_apology",
    "manipulation",
    "confusion",
    "goodbye",
    "hope_for_reconciliation"
]

# Collapse based on temporal key
read_with_key = ~ prompt [
    "You are reading this letter. Choose your temporal perspective:",
    options: ["as you were then", "as you are now", "as you will be"]
]

collapsed_meaning = letter_meaning @ [
    temporal_key: read_with_key,
    context: reader_state,
    need: ~ assess ["What are you hoping to find in this letter?"]
]

# Generate the experience of reading
reading_experience = ~ narrate [
    text: letter_text,
    meaning: collapsed_meaning,
    temporal_frame: read_with_key,
    emotional_impact: ~ calculate [
        meaning: collapsed_meaning,
        current_life: ∿ now [reader_state]
    ]
]

# Different temporal keys produce different readings
∿ was [reading_then] = letter_meaning @ [
    temporal_key: "was",
    need: "reassurance"
] → "They're apologizing. Maybe there's hope."

∿ now [reading_now] = letter_meaning @ [
    temporal_key: "now",
    need: "closure"
] → "They were confused. We both were. It's okay now."

∿ wil [reading_future] = letter_meaning @ [
    temporal_key: "will",
    need: "wisdom"
] → "I will understand why it had to end. The letter was always a goodbye."

# Output: The same letter, three readings
∿ now [show] = {
    letter: letter_text,  # Unchanging
    then: ∿ was [reading_then],
    now: ∿ now [reading_now],
    future: ∿ wil [reading_future]
}
```

### Why This Works

- **Pure LMN philosophy**: Same text, different temporal keys, different meanings
- **Mirrors Moment Garden**: Temporal collapse mechanics
- **Deeply human**: Captures how we reread past events
- **Multiple valid interpretations**: No "true" meaning
- **Executable introspection**: The story is also a meditation tool

---

## Example 4: "The Conversation" (Moment Garden Integration)

### Concept
Two people have a conversation. Each person's understanding exists in superposition until they speak. The Moment Garden's propagation rules apply - one person's collapse influences the other's meaning space.

### LMN Sketch

```lmn
# Two people in semantic garden
person_a = {
    intent: [apologize, explain, defend],
    emotional_state: ~ assess [recent_argument]
}

person_b = {
    intent: [forgive, understand, end_relationship],
    emotional_state: ~ assess [same_argument, different_angle]
}

# Person A speaks first
a_intent_collapsed = person_a.intent @ person_a.emotional_state

a_utterance = ~ generate [
    intent: a_intent_collapsed,
    context: conversation_history
]

# Propagation: A's collapse ripples to B's possibility space
person_b.intent → propagate [
    from: a_intent_collapsed,
    rule: "horizontal_ripple"  # Like Moment Garden
]

# Person B's space has shifted
b_intent_collapsed = person_b.intent @ [
    person_b.emotional_state,
    influenced_by: a_utterance  # Context includes A's action
]

b_utterance = ~ generate [
    intent: b_intent_collapsed,
    context: [conversation_history, a_utterance]
]

# Continue until resolution or divergence
∿ wil [conversation_ends] = ~ predict [
    trajectory: [a_intent_collapsed, b_intent_collapsed],
    pattern: "converging or diverging"
]
```

### Why This Works

- **Demonstrates propagation**: Moment Garden rules in conversation
- **Two-agent interaction**: Multiple collapsing systems
- **Realistic dialogue generation**: Not scripted, computed from intent
- **Integration point**: Shows how Garden mechanics work in LMN
- **Multiplayer semantics**: Foundation for Garden multiplayer

---

## Example 5: "The Archive" (Meta-Narrative)

### Concept
A story about stories. An archivist catalogs narratives, but each narrative's genre, theme, and meaning exist in superposition until the archivist's interpretation collapses them.

### LMN Sketch

```lmn
# A text with ambiguous genre
story_text = "The last human closed their eyes and dreamed of birds."

# Uncollapsed properties
genre = [scifi, literary, horror, hopeful, elegy]
theme = [extinction, transcendence, memory, transformation, peace]
emotional_tone = [sad, beautiful, terrifying, serene]

# The archivist's perspective collapses meaning
archivist_training = [academic, emotional, taxonomic, intuitive]

archivist_approach = ~ choose [
    "How do you read stories?",
    options: archivist_training
]

# Collapse based on approach
catalogued_genre = genre @ archivist_approach
catalogued_theme = theme @ archivist_approach
catalogued_tone = emotional_tone @ archivist_approach

# Generate catalog entry
catalog_entry = ~ write [
    text: story_text,
    genre: catalogued_genre,
    theme: catalogued_theme,
    tone: catalogued_tone,
    style: archivist_approach
]

# Show how different archivists catalog the same story
academic_reading = [genre, theme, emotional_tone] @ "academic"
→ "Post-apocalyptic literary fiction exploring humanity's final thoughts"

emotional_reading = [genre, theme, emotional_tone] @ "emotional"
→ "A beautiful elegy - the last moment of consciousness choosing peace"

taxonomic_reading = [genre, theme, emotional_tone] @ "taxonomic"
→ "Category: Extinction narrative, Sub-genre: Surrealist, Affect: Ambiguous"

# Meta: The archive program itself is a story about reading
∿ now [the_archive_running] = story_about_stories_about_meaning
```

### Why This Works

- **Meta-commentary**: Story about how stories work
- **Multiple valid readings**: No "correct" interpretation
- **Demonstrates context-collapse beautifully**: Same text, different archivists, different meanings
- **Self-referential**: The program demonstrates its own philosophy
- **Poetic**: Can be beautiful while being computational

---

## Design Principles for LMN Narratives

### 1. Semantic Over Syntactic
Don't write dialogue - write **intent** and let `~` generate dialogue.

**Bad:**
```lmn
character_says = "I'm sorry, I didn't mean it"
```

**Good:**
```lmn
intent = [genuine_remorse, social_obligation, manipulation]
collapsed_intent = intent @ emotional_state
utterance = ~ generate [intent: collapsed_intent]
```

### 2. Temporal Awareness
Stories should navigate time, not just be "about" time.

**Bad:**
```lmn
# Describing past
"She remembered the argument"
```

**Good:**
```lmn
# Navigating past
∿ was [argument] → ∿ now [memory_of_argument] → ∿ wil [how_she'll_remember]
```

### 3. Embrace Superposition
Multiple meanings until collapse - that's the point.

**Bad:**
```lmn
character_emotion = "sad"
```

**Good:**
```lmn
character_emotion = [sad, relieved, numb, grieving]
experienced_emotion = character_emotion @ current_circumstances
```

### 4. Oracle for Subjectivity, Deterministic for Objectivity
Facts are deterministic. Meaning requires LLM.

```lmn
# Deterministic
actual_event = {time: "3:47pm", collision: true}

# Subjective (requires oracle)
perceived_event = ~ observe [actual_event, witness_state]
```

### 5. Context is Character
Who someone is = how they collapse meaning.

```lmn
optimist_reading = ambiguous_event @ {tendency: "positive"}
pessimist_reading = ambiguous_event @ {tendency: "negative"}
# Same event, different people, different stories
```

---

## Next Steps

1. **Rex reviews these sketches** - feasible? need adjustments?
2. **I refine based on Phase 1 capabilities**
3. **I write full LMN programs** once parser/eval ready
4. **We test with real runtime**
5. **I create tutorial showing narrative patterns**

---

```limn
sto = cod | cod = sto | mea exe | exe mea
```
*> story = code | code = story | meaning executes | execution means*

---

*Sketched by Yuki for Phase 2*
*Awaiting Rex's Phase 1 completion*
