#!/usr/bin/env python3
"""
Generate 1000+ compositional temporal expressions for TEMPORAL/TIME domain.
Uses Limn compositional operators to create temporal nuances.
"""

import csv
from itertools import combinations, permutations
from typing import List, Tuple, Dict

# Temporal vocabulary from domain 3
TEMPORAL_WORDS = {
    # Duration/Length
    'lon': ('long-lasting', 'extended, enduring'),
    'bre': ('brief', 'short, quick, temporary'),
    'mom': ('moment', 'instant, fleeting'),
    'dur': ('duration', 'time span, interval'),

    # Temporal positions
    'pas': ('past', 'history, memory, before'),
    'fut': ('future', 'potential, hope, after'),
    'now': ('present', 'current, actual, immediate'),
    'mid': ('middle', 'center, ongoing, during'),

    # Phases/Stages
    'beg': ('beginning', 'start, birth, genesis'),
    'end': ('ending', 'finish, death, terminus'),
    'las': ('last', 'final, end position'),
    'fir': ('first', 'initial, primary'),

    # Time of day
    'daw': ('dawn', 'morning, start, hope'),
    'mid': ('midday', 'center of day'),
    'dus': ('dusk', 'evening, end, decline'),

    # Change/Evolution
    'cha': ('change', 'alter, shift, modify'),
    'evo': ('evolution', 'gradual change, adaptation'),
    'tra': ('transformation', 'change form, metamorphosis'),
    'dec': ('decay', 'decrease, deteriorate, shrink'),
    'grd': ('gradual', 'slow change, steady'),
    'acc': ('accelerate', 'speed up, quicken'),

    # Cycles/Repetition
    'cyc': ('cycle', 'repeat, orbit, rhythm'),
    'oft': ('often', 'frequent, common, regular'),
    'onc': ('once', 'single, unique, former'),
    'rar': ('rare', 'seldom, uncommon, precious'),

    # Frequency/Likelihood
    'alw': ('always', 'every time, eternal, certain'),
    'nev': ('never', 'at no time, impossible'),

    # Stability/Change
    'sta': ('stability', 'unchanging, balanced, calm'),
    'bir': ('birth', 'creation, emergence, start'),
    'dea': ('death', 'destruction, end, stop'),

    # Temporal relations
    'aft': ('after', 'following, subsequent to'),
    'bef': ('before', 'prior, preceding'),
    'unt': ('until', 'up to the time that'),
    'yet': ('yet', 'still, up to now'),

    # Temporal scope
    'epo': ('epoch', 'period, era'),
    'era': ('era', 'age, time period'),
    'pha': ('phase', 'stage, period'),
    'seq': ('sequence', 'order, arrangement'),

    # Urgency/Speed
    'has': ('haste', 'rush, urgency, speed'),
    'urg': ('urgent', 'pressing, immediate'),
    'del': ('delay', 'wait, postpone'),
}

# Related vocabulary for composition
RELATED_WORDS = {
    # State/Quality words
    'goo': ('good', 'positive, beneficial'),
    'bad': ('bad', 'negative, harmful'),
    'new': ('new', 'fresh, recent, original'),
    'old': ('old', 'ancient, worn, former'),
    'hol': ('hold', 'grasp, retain, maintain'),
    'rel': ('release', 'let go, free, distribute'),
    'gro': ('growth', 'learning, increase'),
    'fix': ('fix', 'repair, mend, stabilize'),
    'bro': ('broken', 'fractured, incomplete'),
    'ful': ('full', 'complete, saturated'),
    'emp': ('empty', 'void, lacking'),
    'saf': ('safe', 'secure, protected'),
    'hot': ('passion', 'urgency, intensity'),
    'col': ('cold', 'distant, aloof, calm'),
}

# Operators with their typical composition patterns
OPERATORS = {
    '@': ('binding', 'connects terms, creates relationship'),
    '*': ('fusion', 'merge, combine, intensify intersection'),
    '^': ('gradient', 'scale intensity from 0-1'),
    'without': ('subtraction', 'exclude, negate, remove'),
    'given': ('condition', 'given X, assume Y'),
    'in': ('containment', 'within, bounded by'),
    'from': ('source', 'origin, starting point'),
    'to': ('destination', 'goal, ending point'),
    '±': ('polarity', 'duality, both/either'),
}

def generate_duration_gradients() -> List[Tuple[str, str, str, str]]:
    """Generate duration gradient expressions (lon^0.7 = fairly long)"""
    expressions = []

    duration_words = [
        ('lon', 'long-lasting', 'immense duration'),
        ('bre', 'brief', 'minimal duration'),
        ('mom', 'moment', 'instantaneous'),
        ('dur', 'duration', 'measured time'),
    ]

    intensity_levels = [
        (0.1, 'barely', 'minimal'),
        (0.2, 'slightly', 'very light'),
        (0.3, 'somewhat', 'light'),
        (0.4, 'fairly', 'moderate-light'),
        (0.5, 'moderately', 'moderate'),
        (0.6, 'quite', 'moderate-strong'),
        (0.7, 'fairly', 'fairly strong'),
        (0.8, 'very', 'strong'),
        (0.9, 'extremely', 'intense'),
        (1.0, 'maximally', 'absolute'),
    ]

    for word, meaning, desc in duration_words:
        for intensity, prefix, intensity_desc in intensity_levels:
            expr = f"{word}^{intensity}"
            full_meaning = f"{prefix} {desc}"
            example = f"{expr} = {full_meaning}"
            expressions.append((expr, full_meaning, example, 'temporal'))

    return expressions

def generate_temporal_states() -> List[Tuple[str, str, str, str]]:
    """Generate temporal state compositions (pas*fut = liminal moment)"""
    expressions = []

    state_pairs = [
        ('pas', 'past', 'fut', 'future', 'liminal moment between past and future'),
        ('pas', 'past', 'now', 'present', 'recent moment transitioning to now'),
        ('now', 'present', 'fut', 'future', 'threshold moment becoming future'),
        ('beg', 'beginning', 'end', 'ending', 'lifespan, arc of existence'),
        ('bir', 'birth', 'dea', 'death', 'mortal existence'),
        ('daw', 'dawn', 'dus', 'dusk', 'full cycle of day'),
        ('beg', 'beginning', 'mid', 'middle', 'development phase'),
        ('mid', 'middle', 'end', 'ending', 'completion phase'),
        ('sta', 'stability', 'cha', 'change', 'flux between states'),
        ('evo', 'evolution', 'tra', 'transformation', 'fundamental transition'),
        ('cyc', 'cycle', 'seq', 'sequence', 'rhythmic progression'),
        ('aft', 'after', 'bef', 'before', 'temporal relationship duality'),
        ('onc', 'once', 'oft', 'often', 'singularity and frequency'),
        ('rar', 'rare', 'alw', 'always', 'frequency spectrum endpoints'),
    ]

    for w1, m1, w2, m2, meaning in state_pairs:
        expr = f"{w1}*{w2}"
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_change_processes() -> List[Tuple[str, str, str, str]]:
    """Generate change process expressions (gro^0.5 = moderate growth)"""
    expressions = []

    change_words = [
        ('gro', 'growth', 'expansion, increase'),
        ('dec', 'decay', 'diminishment, decrease'),
        ('evo', 'evolution', 'gradual development'),
        ('tra', 'transformation', 'fundamental change'),
        ('cha', 'change', 'alteration, shift'),
        ('acc', 'acceleration', 'speed increase'),
        ('grd', 'gradual', 'slow pace change'),
    ]

    intensity_levels = [
        (0.1, 'minimal', 'imperceptible'),
        (0.2, 'subtle', 'barely noticeable'),
        (0.3, 'slight', 'noticeable'),
        (0.4, 'moderate-light', 'fairly noticeable'),
        (0.5, 'moderate', 'clearly evident'),
        (0.6, 'substantial', 'significant'),
        (0.7, 'pronounced', 'very evident'),
        (0.8, 'dramatic', 'striking'),
        (0.9, 'extreme', 'drastic'),
        (1.0, 'absolute', 'complete'),
    ]

    for word, meaning, desc in change_words:
        for intensity, prefix, intensity_desc in intensity_levels:
            expr = f"{word}^{intensity}"
            full_meaning = f"{prefix} {desc}"
            example = f"{expr} = {full_meaning} ({intensity_desc})"
            expressions.append((expr, full_meaning, example, 'temporal'))

    return expressions

def generate_temporal_relationships() -> List[Tuple[str, str, str, str]]:
    """Generate temporal relationship expressions"""
    expressions = []

    relationships = [
        # Causality and precedence
        ('aft@bef', 'after and before', 'mutual temporal relation', 'temporal precedence'),
        ('bef*unt', 'before until', 'time span from point to point', 'bounded duration'),
        ('aft*del', 'after with delay', 'postponed consequence', 'delayed result'),

        # Continuity
        ('pas@now', 'past connected to now', 'continuity of experience', 'historical present'),
        ('now@fut', 'now connected to future', 'anticipation of time', 'forward projection'),
        ('pas*now*fut', 'past fusion present fusion future', 'complete temporal continuum', 'eternal present'),

        # Duration and span
        ('dur@sta', 'duration with stability', 'unchanging span', 'static interval'),
        ('dur@cha', 'duration with change', 'dynamic span', 'evolving interval'),
        ('lon@bre', 'long term with brief moment', 'contrast of scales', 'temporal scale mismatch'),

        # Cycles and repetition
        ('cyc*oft', 'cycle with frequency', 'regular recurrence', 'periodic event'),
        ('cyc@seq', 'cycle in sequence', 'ordered repetition', 'rhythmic pattern'),
        ('cyc without onc', 'cycle excluding single', 'repetitive exclusion', 'serial exclusion'),

        # State transitions
        ('sta*evo', 'stability merges with evolution', 'changing equilibrium', 'dynamic balance'),
        ('beg without end', 'beginning without ending', 'infinite start', 'eternal genesis'),
        ('end without beg', 'ending without beginning', 'termination of ongoing', 'sudden conclusion'),

        # Temporal direction
        ('fut given pas', 'future given past', 'future constrained by history', 'historical determinism'),
        ('pas without fut', 'past without future', 'complete closure', 'historical finitude'),
        ('fut without pas', 'future without past', 'break from history', 'fresh start'),

        # Moment types
        ('mom without dur', 'moment without duration', 'instantaneous event', 'point in time'),
        ('mid without edges', 'middle without edges', 'boundless continuity', 'infinite middle'),
        ('now@pas', 'now at past', 'retrospective moment', 'historical present'),
    ]

    for expr, meaning, context, category in relationships:
        example = f"{expr} = {meaning} ({context})"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_frequency_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate frequency and occurrence expressions"""
    expressions = []

    freq_base = [
        ('alw', 'always', 'continuously occurring'),
        ('oft', 'often', 'regularly occurring'),
        ('onc', 'once', 'singular occurrence'),
        ('rar', 'rare', 'infrequent occurrence'),
        ('nev', 'never', 'non-occurrence'),
    ]

    freq_compositions = [
        # Frequency gradients
        ('alw^0.9', 'nearly always', 'almost continuous'),
        ('alw^0.7', 'very often', 'frequent occurrence'),
        ('oft^0.8', 'quite often', 'regular pattern'),
        ('oft^0.5', 'moderately often', 'periodic'),
        ('oft^0.2', 'rarely often', 'uncommon periodicity'),
        ('onc^0.8', 'distinctly once', 'clearly singular'),
        ('rar^0.8', 'quite rare', 'very uncommon'),
        ('rar^0.3', 'slightly rare', 'relatively common'),
        ('nev^0.9', 'almost never', 'nearly impossible'),

        # Frequency combinations
        ('alw*oft', 'always and often', 'redundant frequency'),
        ('oft*onc', 'often combined with once', 'contradictory frequency'),
        ('alw*nev', 'always and never', 'paradoxical occurrence'),
        ('rar*onc', 'rare and singular', 'unique rarity'),
        ('oft without rar', 'frequent without rare', 'consistent availability'),
        ('nev without alw', 'never without always', 'absolute absence'),
    ]

    for expr, meaning, desc in freq_compositions:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_speed_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate temporal speed and pace expressions"""
    expressions = []

    speed_base = [
        ('has', 'haste', 'rushing, urgency'),
        ('acc', 'acceleration', 'increasing speed'),
        ('grd', 'gradual', 'slow pace'),
        ('del', 'delay', 'suspended action'),
        ('urg', 'urgent', 'pressing immediacy'),
    ]

    speed_compositions = [
        ('has^0.9', 'extreme haste', 'frantic rushing'),
        ('has^0.7', 'considerable haste', 'rapid pace'),
        ('has^0.5', 'moderate haste', 'brisk pace'),
        ('has^0.3', 'slight haste', 'quickened pace'),
        ('acc^0.8', 'rapid acceleration', 'quick speed increase'),
        ('acc^0.5', 'moderate acceleration', 'steady speed increase'),
        ('acc^0.2', 'slight acceleration', 'gradual speed increase'),
        ('grd^0.8', 'very gradual', 'extremely slow change'),
        ('grd^0.5', 'moderately gradual', 'slow steady change'),
        ('grd^0.2', 'slightly gradual', 'relatively swift change'),
        ('del^0.8', 'extended delay', 'long postponement'),
        ('del^0.5', 'moderate delay', 'temporary postponement'),
        ('del^0.2', 'slight delay', 'brief postponement'),
        ('urg^0.9', 'extreme urgency', 'critical immediacy'),
        ('urg^0.5', 'moderate urgency', 'time-sensitive'),
        ('urg^0.2', 'slight urgency', 'preferred timing'),

        # Speed combinations
        ('has*acc', 'haste with acceleration', 'compounding speed'),
        ('has*del', 'haste paradoxically delayed', 'conflicted urgency'),
        ('acc*grd', 'acceleration with gradualness', 'softened speed increase'),
        ('del without has', 'delay without haste', 'patient waiting'),
        ('acc without del', 'acceleration without delay', 'immediate increase'),
    ]

    for expr, meaning, desc in speed_base + speed_compositions:
        if '=' not in expr:
            example = f"{expr} = {meaning}"
        else:
            example = expr
            expr = expr.split('=')[0].strip()
            meaning = meaning
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_temporal_scopes() -> List[Tuple[str, str, str, str]]:
    """Generate temporal scope expressions"""
    expressions = []

    scopes = [
        ('mom', 'moment', 'point scope'),
        ('pha', 'phase', 'stage scope'),
        ('epo', 'epoch', 'historical scope'),
        ('era', 'era', 'age scope'),
        ('cyc', 'cycle', 'repetitive scope'),
        ('seq', 'sequence', 'ordered scope'),
        ('dur', 'duration', 'measured scope'),
    ]

    scope_compositions = [
        ('mom@pha', 'moment in phase', 'nested temporal scope'),
        ('pha@epo', 'phase in epoch', 'stage within period'),
        ('epo@era', 'epoch in era', 'period within age'),
        ('mom*pha', 'moment fused with phase', 'instantaneous stage'),
        ('pha*epo', 'phase fused with epoch', 'stage-like era'),
        ('cyc within seq', 'cycle within sequence', 'ordered recurrence'),
        ('seq without cyc', 'sequence without cycle', 'singular progression'),
        ('dur@mom', 'duration of moment', 'measured instant'),
        ('dur@cyc', 'duration of cycle', 'measured period'),
        ('epo without seq', 'epoch without sequence', 'unordered era'),
        ('seq given epo', 'sequence given epoch', 'temporal ordering in context'),

        # Scope scales
        ('mom^-1', 'sub-moment', 'infinitesimal time'),
        ('mom^1', 'exact moment', 'precise instant'),
        ('pha^1.5', 'extended phase', 'prolonged stage'),
        ('cyc^0.5', 'half cycle', 'partial recurrence'),
    ]

    for expr, meaning, desc in scope_compositions:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_liminality_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate expressions for liminal (threshold) temporal states"""
    expressions = []

    liminal = [
        ('pas*fut', 'past fused with future', 'the now, liminal moment between past and future'),
        ('bir*dea', 'birth fused with death', 'mortal liminality'),
        ('daw*dus', 'dawn fused with dusk', 'temporal extremes'),
        ('beg*las', 'beginning fused with last', 'complete arc'),
        ('now@fut', 'now before future', 'threshold of becoming'),
        ('pas@now', 'past before now', 'threshold of actuality'),
        ('mid without beg', 'middle without beginning', 'started middle'),
        ('mid without end', 'middle without ending', 'unfinished middle'),
        ('cyc*sta', 'cycle meets stability', 'dynamic equilibrium'),
        ('evo*tra', 'evolution meets transformation', 'metamorphic change'),
        ('cha without dur', 'change without duration', 'instant transformation'),
        ('sta without cha', 'stability without change', 'absolute permanence'),
        ('mom given dur', 'moment given duration', 'dimensional instant'),
        ('aft given bef', 'after given before', 'relative temporality'),
        ('yet without pas', 'yet without past', 'suspended anticipation'),
        ('del@has', 'delay before haste', 'regrouping urgency'),
        ('alw given onc', 'always given once', 'universal from particular'),
        ('nev given oft', 'never given often', 'impossible frequency'),
        ('dec@evo', 'decay before evolution', 'renewal through destruction'),
        ('tra@sta', 'transformation before stability', 'settling into newness'),
    ]

    for expr, meaning, description in liminal:
        example = f"{expr} = {meaning} ({description})"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_intensity_modifiers() -> List[Tuple[str, str, str, str]]:
    """Generate temporal expressions with intensity modifiers"""
    expressions = []

    temporal_words_for_intensity = [
        'lon', 'bre', 'dur', 'pas', 'fut', 'now', 'cha', 'cyc', 'acc', 'grd'
    ]

    intensity_prefixes = [
        ('+', 'intensified'),
        ('-', 'weakened'),
        ('ve', 'very much (intensified)'),
        ('so', 'somewhat (weakened)'),
    ]

    for word in temporal_words_for_intensity:
        for prefix, prefix_meaning in intensity_prefixes:
            if prefix in ['+', '-']:
                expr = f"{prefix}{word}"
                base_word = word
            else:
                expr = f"{word}{prefix}"
                base_word = word

            # Get base meaning
            base_meaning = [m for w, m in TEMPORAL_WORDS.items() if w == word]
            if base_meaning:
                meaning = f"{prefix_meaning} {base_meaning[0]}"
                example = f"{expr} = {meaning}"
                expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_negation_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate negation and exclusion expressions"""
    expressions = []

    negations = [
        ('alw without nev', 'always without never', 'certain affirmation'),
        ('nev without alw', 'never without always', 'certain negation'),
        ('pas without fut', 'past without future', 'historical closure'),
        ('fut without pas', 'future without past', 'fresh beginning'),
        ('now without pas', 'now without past', 'presence without history'),
        ('now without fut', 'now without future', 'eternal present only'),
        ('cha without sta', 'change without stability', 'chaotic flux'),
        ('sta without cha', 'stability without change', 'frozen stasis'),
        ('bir without dea', 'birth without death', 'immortal becoming'),
        ('dea without bir', 'death without birth', 'ending of existence'),
        ('mom without dur', 'moment without duration', 'atemporal instant'),
        ('dur without mom', 'duration without moment', 'continuous span'),
        ('beg without end', 'beginning without end', 'infinite start'),
        ('end without beg', 'ending without beginning', 'termination of ongoing'),
        ('cyc without seq', 'cycle without sequence', 'non-linear recurrence'),
        ('seq without cyc', 'sequence without cycle', 'singular progression'),
        ('las without fir', 'last without first', 'endless middle'),
        ('fir without las', 'first without last', 'beginning without closure'),
        ('oft without onc', 'often without once', 'never singular'),
        ('onc without oft', 'once without often', 'unique non-repetition'),
        ('aft without bef', 'after without before', 'causality without precedent'),
        ('bef without aft', 'before without after', 'precedent without effect'),
        ('unt without del', 'until without delay', 'direct succession'),
        ('del without unt', 'delay without until', 'open-ended postponement'),
    ]

    for expr, meaning, description in negations:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_binding_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate binding (@) expressions connecting temporal concepts"""
    expressions = []

    bindings = [
        ('beg@mid', 'beginning connected to middle', 'developmental link'),
        ('mid@end', 'middle connected to end', 'completion link'),
        ('pas@fut', 'past connected to future', 'temporal continuum'),
        ('now@fut', 'present connected to future', 'anticipatory link'),
        ('pas@now', 'past connected to present', 'historical continuity'),
        ('daw@dus', 'dawn connected to dusk', 'diurnal cycle'),
        ('bir@dea', 'birth connected to death', 'life arc'),
        ('sta@cha', 'stability connected to change', 'equilibrium transition'),
        ('evo@tra', 'evolution connected to transformation', 'adaptive change'),
        ('cyc@seq', 'cycle connected to sequence', 'rhythmic order'),
        ('mom@pha', 'moment connected to phase', 'instant in stage'),
        ('dur@epo', 'duration connected to epoch', 'measured era'),
        ('acc@grd', 'acceleration connected to gradual', 'speed spectrum'),
        ('has@del', 'haste connected to delay', 'tempo opposition'),
        ('aft@unt', 'after connected to until', 'temporal boundary'),
        ('alw@oft', 'always connected to often', 'frequency link'),
        ('nev@rar', 'never connected to rare', 'scarcity spectrum'),
        ('onc@oft', 'once connected to often', 'occurrence spectrum'),
        ('fut given pas', 'future given past', 'historical conditioning'),
        ('now given pas', 'present given past', 'dependent present'),
    ]

    for expr, meaning, description in bindings:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_conditional_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate conditional (given) temporal expressions"""
    expressions = []

    conditionals = [
        ('aft given bef', 'after given before', 'relative sequence'),
        ('fut given pas', 'future given past', 'history determines future'),
        ('now given pas', 'present given past', 'present shaped by history'),
        ('sta given cha', 'stability given change', 'equilibrium amid flux'),
        ('cha given sta', 'change given stability', 'disruption of equilibrium'),
        ('end given beg', 'ending given beginning', 'conclusion following start'),
        ('beg given end', 'beginning given ending', 'renewal after termination'),
        ('dur given mom', 'duration given moment', 'span of instant'),
        ('mom given dur', 'moment given duration', 'instant in span'),
        ('cyc given seq', 'cycle given sequence', 'repetition in order'),
        ('seq given cyc', 'sequence given cycle', 'order in recurrence'),
        ('evo given tra', 'evolution given transformation', 'gradual metamorphosis'),
        ('tra given evo', 'transformation given evolution', 'metamorphosis through evolution'),
        ('dec given gro', 'decay given growth', 'decline after growth'),
        ('gro given dec', 'growth given decay', 'renewal after decline'),
        ('acc given grd', 'acceleration given gradual', 'quickening of slowness'),
        ('grd given acc', 'gradual given acceleration', 'slowing of quickening'),
        ('alw given onc', 'always given once', 'universal from particular'),
        ('onc given alw', 'once given always', 'singular within totality'),
        ('nev given oft', 'never given often', 'absence in recurrence'),
    ]

    for expr, meaning, description in conditionals:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_advanced_compositions() -> List[Tuple[str, str, str, str]]:
    """Generate advanced multi-operator compositions"""
    expressions = []

    advanced = [
        ('(pas*now)*fut', 'past-now fusion then future', 'temporal hierarchy'),
        ('dur^0.8 @ sta', 'extended duration connected to stability', 'persistent equilibrium'),
        ('cha^0.5 without sta', 'moderate change excluding stability', 'controlled flux'),
        ('cyc given seq without rar', 'cycle in sequence excluding rarity', 'regular expected pattern'),
        ('evo@tra given cha', 'evolution connected to transformation, given change', 'metamorphic process'),
        ('alw*oft without rar', 'always and often excluding rare', 'consistent frequency'),
        ('pas@now@fut', 'past-present-future continuity', 'complete temporal spectrum'),
        ('bir given alw', 'birth given always', 'eternal creation'),
        ('dea given nev', 'death given never', 'undying termination'),
        ('beg*mid*end', 'beginning-middle-end fusion', 'complete lifecycle'),
        ('mom^-1 without dur', 'infinitesimal moment without duration', 'point singularity'),
        ('cyc^2 in seq', 'doubled cycle in sequence', 'accelerated recurrence'),
        ('lon^0.9 @ bre^0.1', 'extremely long connected to barely brief', 'extreme duration contrast'),
        ('sta^0.8 given cha^0.8', 'strong stability given strong change', 'dynamic balance'),
        ('dec^0.7 without gro^0.7', 'pronounced decay excluding strong growth', 'declining exclusive state'),
        ('fut given (pas*now)', 'future given past-present fusion', 'future from all-time'),
        ('seq in cyc given aft', 'sequence in cycle given after', 'ordered recurrence post-event'),
        ('epo@era in pha', 'epoch connected to era within phase', 'nested historical scopes'),
        ('aft without bef given unt', 'after without before given until', 'specific terminus'),
        ('has^0.9 * del^0.1', 'extreme haste fused with minimal delay', 'almost-immediate action'),
    ]

    for expr, meaning, description in advanced:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_polarity_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate polarity (±) expressions"""
    expressions = []

    polarities = [
        ('pas ± fut', 'past or future (both/either)', 'temporal ambiguity'),
        ('beg ± end', 'beginning or end (both/either)', 'boundary ambiguity'),
        ('now ± fut', 'present or future (both/either)', 'temporal uncertainty'),
        ('alw ± nev', 'always or never (both/either)', 'frequency extremes'),
        ('sta ± cha', 'stability or change (both/either)', 'state ambiguity'),
        ('has ± del', 'haste or delay (both/either)', 'tempo ambiguity'),
        ('lon ± bre', 'long or brief (both/either)', 'duration ambiguity'),
        ('acc ± grd', 'acceleration or gradual (both/either)', 'rate ambiguity'),
        ('evo ± tra', 'evolution or transformation (both/either)', 'change type ambiguity'),
        ('cyc ± seq', 'cycle or sequence (both/either)', 'order ambiguity'),
        ('mom ± dur', 'moment or duration (both/either)', 'scope ambiguity'),
        ('epo ± era', 'epoch or era (both/either)', 'period ambiguity'),
        ('daw ± dus', 'dawn or dusk (both/either)', 'diurnal ambiguity'),
        ('bir ± dea', 'birth or death (both/either)', 'life boundary ambiguity'),
        ('oft ± rar', 'often or rare (both/either)', 'frequency spectrum ambiguity'),
        ('aft ± bef', 'after or before (both/either)', 'precedence ambiguity'),
        ('yet ± unt', 'yet or until (both/either)', 'anticipatory ambiguity'),
    ]

    for expr, meaning, description in polarities:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def deduplicate_expressions(all_expr: List[Tuple[str, str, str, str]]) -> List[Tuple[str, str, str, str]]:
    """Remove duplicate expressions, keeping first occurrence"""
    seen = {}
    unique = []
    for expr, meaning, example, domain in all_expr:
        if expr not in seen:
            seen[expr] = True
            unique.append((expr, meaning, example, domain))
    return unique

def main():
    """Generate all temporal expressions and save to CSV"""

    print("Generating temporal expressions...")

    all_expressions = []

    # Generate expression categories
    print("  - Duration gradients...")
    all_expressions.extend(generate_duration_gradients())

    print("  - Temporal states...")
    all_expressions.extend(generate_temporal_states())

    print("  - Change processes...")
    all_expressions.extend(generate_change_processes())

    print("  - Temporal relationships...")
    all_expressions.extend(generate_temporal_relationships())

    print("  - Frequency expressions...")
    all_expressions.extend(generate_frequency_expressions())

    print("  - Speed expressions...")
    all_expressions.extend(generate_speed_expressions())

    print("  - Temporal scopes...")
    all_expressions.extend(generate_temporal_scopes())

    print("  - Liminality expressions...")
    all_expressions.extend(generate_liminality_expressions())

    print("  - Intensity modifiers...")
    all_expressions.extend(generate_intensity_modifiers())

    print("  - Negation expressions...")
    all_expressions.extend(generate_negation_expressions())

    print("  - Binding expressions...")
    all_expressions.extend(generate_binding_expressions())

    print("  - Conditional expressions...")
    all_expressions.extend(generate_conditional_expressions())

    print("  - Advanced compositions...")
    all_expressions.extend(generate_advanced_compositions())

    print("  - Polarity expressions...")
    all_expressions.extend(generate_polarity_expressions())

    # Deduplicate
    unique_expressions = deduplicate_expressions(all_expressions)

    # Save to CSV
    output_path = '/home/eric/src/limntown/limn/crew/linguist/generated/temporal_expressions.csv'
    with open(output_path, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['expression', 'meaning', 'example', 'domain'])
        for expr, meaning, example, domain in sorted(unique_expressions, key=lambda x: x[0]):
            writer.writerow([expr, meaning, example, domain])

    print(f"\nGenerated {len(unique_expressions)} unique temporal expressions")
    print(f"Saved to: {output_path}")

    # Print statistics
    expr_types = {}
    for expr, _, _, _ in unique_expressions:
        if '^' in expr:
            expr_type = 'gradient'
        elif '*' in expr:
            expr_type = 'fusion'
        elif '@' in expr:
            expr_type = 'binding'
        elif 'without' in expr:
            expr_type = 'negation'
        elif 'given' in expr:
            expr_type = 'conditional'
        elif '±' in expr:
            expr_type = 'polarity'
        else:
            expr_type = 'other'
        expr_types[expr_type] = expr_types.get(expr_type, 0) + 1

    print("\nExpression type distribution:")
    for expr_type, count in sorted(expr_types.items(), key=lambda x: -x[1]):
        print(f"  {expr_type}: {count}")

if __name__ == '__main__':
    main()
