#!/usr/bin/env python3
"""
Generate 1000+ compositional temporal expressions using advanced combinations.
"""

import csv
from typing import List, Tuple, Set

# Extended temporal vocabulary
TEMPORAL_BASE = {
    # Duration/Length (10 words)
    'lon': 'long-lasting', 'bre': 'brief', 'mom': 'moment', 'dur': 'duration',
    'end': 'ending', 'beg': 'beginning', 'mid': 'middle', 'las': 'last',
    'fir': 'first', 'epo': 'epoch',

    # Temporal positions (10 words)
    'pas': 'past', 'fut': 'future', 'now': 'present', 'daw': 'dawn',
    'dus': 'dusk', 'aft': 'after', 'bef': 'before', 'unt': 'until',
    'yet': 'yet', 'era': 'era',

    # Change/Evolution (10 words)
    'cha': 'change', 'evo': 'evolution', 'tra': 'transformation', 'dec': 'decay',
    'grd': 'gradual', 'acc': 'accelerate', 'bir': 'birth', 'dea': 'death',
    'sta': 'stability', 'gro': 'growth',

    # Cycles/Patterns (7 words)
    'cyc': 'cycle', 'oft': 'often', 'onc': 'once', 'rar': 'rare',
    'alw': 'always', 'nev': 'never', 'seq': 'sequence',

    # Urgency/Speed (5 words)
    'has': 'haste', 'del': 'delay', 'urg': 'urgent', 'pha': 'phase',
    'slow': 'slow',
}

# Additional quality words for composition
QUALITY_WORDS = {
    'new': 'new', 'old': 'old', 'goo': 'good', 'bad': 'bad',
    'hot': 'hot', 'col': 'cold', 'ful': 'full', 'emp': 'empty',
    'hol': 'hold', 'rel': 'release', 'saf': 'safe', 'fix': 'fix',
}

ALL_WORDS = {**TEMPORAL_BASE, **QUALITY_WORDS}

def generate_gradient_variants() -> List[Tuple[str, str, str, str]]:
    """Generate extensive gradient (^) expressions with all intensities"""
    expressions = []

    # All intensity levels
    intensities = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    intensity_names = {
        0.1: 'barely', 0.2: 'slightly', 0.3: 'somewhat', 0.4: 'fairly light',
        0.5: 'moderately', 0.6: 'quite', 0.7: 'fairly', 0.8: 'very',
        0.9: 'extremely', 1.0: 'absolutely',
    }

    for word, base_meaning in TEMPORAL_BASE.items():
        for intensity in intensities:
            expr = f"{word}^{intensity}"
            prefix = intensity_names[intensity]
            meaning = f"{prefix} {base_meaning}"
            example = f"{expr} = {meaning}"
            expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_pairwise_fusions() -> List[Tuple[str, str, str, str]]:
    """Generate fusion (*) expressions for all word pairs"""
    expressions = []

    words_list = list(TEMPORAL_BASE.keys())

    # Generate key pairwise combinations (not all, to avoid explosion)
    important_pairs = [
        ('pas', 'fut'), ('pas', 'now'), ('now', 'fut'),
        ('beg', 'end'), ('beg', 'mid'), ('mid', 'end'),
        ('bir', 'dea'), ('daw', 'dus'),
        ('sta', 'cha'), ('evo', 'tra'),
        ('cyc', 'seq'), ('cyc', 'oft'),
        ('lon', 'bre'), ('lon', 'mom'),
        ('acc', 'grd'), ('acc', 'has'),
        ('gro', 'dec'), ('gro', 'evo'),
        ('alw', 'nev'), ('alw', 'oft'),
        ('onc', 'oft'), ('rar', 'oft'),
        ('del', 'has'), ('del', 'urg'),
        ('aft', 'bef'), ('aft', 'unt'),
        ('yet', 'unt'), ('pha', 'seq'),
    ]

    for w1, w2 in important_pairs:
        if w1 in TEMPORAL_BASE and w2 in TEMPORAL_BASE:
            m1, m2 = TEMPORAL_BASE[w1], TEMPORAL_BASE[w2]
            expr = f"{w1}*{w2}"
            meaning = f"{m1} fused with {m2}"
            example = f"{expr} = {meaning}"
            expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_binding_links() -> List[Tuple[str, str, str, str]]:
    """Generate binding (@) expressions"""
    expressions = []

    bindings = [
        ('pas@now', 'past to present'),
        ('now@fut', 'present to future'),
        ('pas@fut', 'past to future'),
        ('beg@mid', 'beginning to middle'),
        ('mid@end', 'middle to end'),
        ('beg@end', 'beginning to end'),
        ('bir@dea', 'birth to death'),
        ('daw@dus', 'dawn to dusk'),
        ('sta@cha', 'stability to change'),
        ('evo@tra', 'evolution to transformation'),
        ('cyc@seq', 'cycle to sequence'),
        ('acc@grd', 'acceleration to gradual'),
        ('has@del', 'haste to delay'),
        ('alw@oft', 'always to often'),
        ('onc@rar', 'once to rare'),
        ('nev@rar', 'never to rare'),
        ('epo@era', 'epoch to era'),
        ('pha@pha', 'phase through phase'),
        ('lon@bre', 'long through brief'),
        ('mom@dur', 'moment through duration'),
    ]

    for expr, meaning in bindings:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_negation_pairs() -> List[Tuple[str, str, str, str]]:
    """Generate negation (without) expressions for key contrasts"""
    expressions = []

    negations = [
        ('alw without nev', 'certain affirmation'),
        ('nev without alw', 'certain negation'),
        ('pas without fut', 'historical closure'),
        ('fut without pas', 'fresh beginning'),
        ('cha without sta', 'pure flux'),
        ('sta without cha', 'absolute permanence'),
        ('bir without dea', 'immortal becoming'),
        ('dea without bir', 'termination of existence'),
        ('cyc without seq', 'non-linear recurrence'),
        ('seq without cyc', 'singular progression'),
        ('beg without end', 'infinite start'),
        ('end without beg', 'ending without genesis'),
        ('lon without bre', 'extended without brevity'),
        ('bre without lon', 'brief without extension'),
        ('oft without rar', 'guaranteed frequency'),
        ('rar without oft', 'perpetual singularity'),
        ('has without del', 'unimpeded haste'),
        ('del without has', 'patient waiting'),
        ('acc without grd', 'sharp acceleration'),
        ('grd without acc', 'persistent slowness'),
        ('now without pas', 'presence without history'),
        ('now without fut', 'eternal now only'),
        ('fir without las', 'endless beginning'),
        ('las without fir', 'endless middle'),
        ('onc without oft', 'unique non-repetition'),
        ('aft without bef', 'consequence without cause'),
        ('bef without aft', 'cause without effect'),
        ('evo without tra', 'gradual without transformation'),
        ('tra without evo', 'transformation without evolution'),
        ('unt without yet', 'limit without anticipation'),
        ('yet without unt', 'anticipation without limit'),
    ]

    for expr, meaning in negations:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_conditional_chains() -> List[Tuple[str, str, str, str]]:
    """Generate conditional (given) expressions"""
    expressions = []

    conditionals = [
        ('fut given pas', 'future shaped by past'),
        ('now given pas', 'present derives from past'),
        ('pas given fut', 'past leads to future'),
        ('aft given bef', 'after follows before'),
        ('bef given aft', 'before precedes after'),
        ('end given beg', 'ending follows beginning'),
        ('beg given end', 'beginning renews after end'),
        ('sta given cha', 'equilibrium from flux'),
        ('cha given sta', 'disruption of stability'),
        ('dec given gro', 'decay after growth'),
        ('gro given dec', 'growth after decay'),
        ('tra given evo', 'transformation through evolution'),
        ('evo given tra', 'evolution through transformation'),
        ('cyc given seq', 'cycle within order'),
        ('seq given cyc', 'order within cycle'),
        ('alw given onc', 'universal from singular'),
        ('onc given alw', 'singular within totality'),
        ('nev given oft', 'absence within frequency'),
        ('oft given rar', 'frequency from rarity'),
        ('rar given oft', 'rarity from frequency'),
        ('acc given grd', 'acceleration of slowness'),
        ('grd given acc', 'gradual deceleration'),
        ('has given del', 'haste after delay'),
        ('del given has', 'delay after haste'),
        ('bir given dea', 'rebirth after death'),
        ('dea given bir', 'mortality from birth'),
        ('mom given dur', 'instant within span'),
        ('dur given mom', 'duration of moments'),
        ('era given epo', 'era from epoch'),
    ]

    for expr, meaning in conditionals:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_polarity_spreads() -> List[Tuple[str, str, str, str]]:
    """Generate polarity (±) expressions"""
    expressions = []

    polarities = [
        ('pas ± fut', 'past or future'),
        ('pas ± now ± fut', 'past or present or future'),
        ('beg ± end', 'beginning or end'),
        ('alw ± nev', 'always or never'),
        ('sta ± cha', 'stable or changing'),
        ('bir ± dea', 'birth or death'),
        ('lon ± bre', 'long or brief'),
        ('has ± del', 'haste or delay'),
        ('acc ± grd', 'acceleration or gradual'),
        ('cyc ± seq', 'cycle or sequence'),
        ('oft ± rar', 'frequent or rare'),
        ('onc ± oft', 'once or often'),
        ('evo ± tra', 'evolution or transformation'),
        ('gro ± dec', 'growth or decay'),
        ('aft ± bef', 'after or before'),
        ('yet ± unt', 'yet or until'),
        ('daw ± dus', 'dawn or dusk'),
        ('epo ± era', 'epoch or era'),
        ('now ± fut', 'now or future'),
        ('mid ± fir ± las', 'middle or first or last'),
    ]

    for expr, meaning in polarities:
        example = f"{expr} = {meaning} (duality)"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_intensity_modifiers() -> List[Tuple[str, str, str, str]]:
    """Generate expressions with intensity markers"""
    expressions = []

    modifiers = [
        ('ve', 'very much intensified'),
        ('so', 'somewhat weakened'),
    ]

    for word, base_meaning in list(TEMPORAL_BASE.items())[:25]:
        for prefix, prefix_meaning in modifiers:
            expr_format = f"{word}{prefix}"
            meaning = f"{prefix_meaning} {base_meaning}"
            example = f"{expr_format} = {meaning}"
            expressions.append((expr_format, meaning, example, 'temporal'))

    return expressions

def generate_nested_compositions() -> List[Tuple[str, str, str, str]]:
    """Generate nested and multi-operator expressions"""
    expressions = []

    nested = [
        ('(pas*now)*fut', 'temporal chain past-present-future'),
        ('(beg*mid)*end', 'full lifecycle phases'),
        ('(bir*sta)*dea', 'life through stability to death'),
        ('(alw*oft)*onc', 'frequency spectrum from always to once'),
        ('(daw*mid)*dus', 'full diurnal cycle'),
        ('(acc*grd)*sta', 'motion to stability'),
        ('(cha*evo)*tra', 'change through evolution to transformation'),
        ('(cyc*seq)*pha', 'ordered cyclic phases'),
        ('(lon*mom)*bre', 'duration spectrum'),
        ('(pas@now)*fut', 'connected continuity to future'),
        ('pas*(now@fut)', 'past to connected future'),
        ('(gro^0.8)*(dec^0.2)', 'strong growth fused with weak decay'),
        ('(evo^0.9)*(tra^0.1)', 'pronounced evolution with minimal transformation'),
        ('((pas*now)*fut) given cha', 'temporal chain given change'),
        ('(cyc without seq) given oft', 'non-linear cycle given frequency'),
        ('(sta given cha) without bir', 'stability from change excluding birth'),
        ('(lon^0.9) @ (bre^0.1)', 'extreme long connected to barely brief'),
        ('(fut given pas) without nev', 'future from past excluding impossibility'),
        ('(beg@end) given sta', 'lifecycle connected given stability'),
        ('(alw*oft) without rar', 'frequent certainty excluding rarity'),
    ]

    for expr, meaning in nested:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_spectrum_expressions() -> List[Tuple[str, str, str, str]]:
    """Generate spectrum mapping expressions"""
    expressions = []

    spectra = [
        ('pas^1.0 to fut^1.0', 'complete temporal spectrum'),
        ('alw^1.0 to nev^1.0', 'certainty spectrum'),
        ('lon^1.0 to bre^1.0', 'duration spectrum'),
        ('acc^1.0 to grd^1.0', 'rate of change spectrum'),
        ('beg^1.0 to end^1.0', 'lifecycle spectrum'),
        ('has^1.0 to del^1.0', 'tempo spectrum'),
        ('sta^1.0 to cha^1.0', 'changeability spectrum'),
        ('oft^1.0 to rar^1.0', 'frequency spectrum'),
        ('cyc^1.0 to seq^1.0', 'order spectrum'),
        ('evo^1.0 to tra^1.0', 'transformation spectrum'),
    ]

    for expr, meaning in spectra:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_inverse_relationships() -> List[Tuple[str, str, str, str]]:
    """Generate inverse relationship expressions"""
    expressions = []

    inverses = [
        ('pas^-1', 'inverse past (future-facing)'),
        ('fut^-1', 'inverse future (past-facing)'),
        ('cha^-1', 'inverse change (preservation)'),
        ('gro^-1', 'inverse growth (reduction)'),
        ('acc^-1', 'inverse acceleration (deceleration)'),
        ('dea^-1', 'inverse death (vitality)'),
        ('dec^-1', 'inverse decay (restoration)'),
        ('has^-1', 'inverse haste (stillness)'),
        ('del^-1', 'inverse delay (immediacy)'),
        ('cyc^-1', 'inverse cycle (linearity)'),
        ('sta^-1', 'inverse stability (fluidity)'),
        ('evo^-1', 'inverse evolution (devolution)'),
        ('tra^-1', 'inverse transformation (preservation of form)'),
        ('lon^-1', 'inverse long (short)'),
        ('bre^-1', 'inverse brief (extended)'),
    ]

    for expr, meaning in inverses:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_combination_chains() -> List[Tuple[str, str, str, str]]:
    """Generate chained combination expressions"""
    expressions = []

    chains = []

    # Create chains by combining operators
    temporal_triplets = [
        ('pas', 'now', 'fut'),
        ('beg', 'mid', 'end'),
        ('daw', 'mid', 'dus'),
        ('bir', 'sta', 'dea'),
        ('cyc', 'seq', 'pha'),
        ('gro', 'sta', 'dec'),
        ('acc', 'sta', 'grd'),
    ]

    for w1, w2, w3 in temporal_triplets:
        m1, m2, m3 = TEMPORAL_BASE[w1], TEMPORAL_BASE[w2], TEMPORAL_BASE[w3]

        # Fusion chains
        expr1 = f"{w1}*{w2}*{w3}"
        meaning1 = f"{m1} through {m2} through {m3}"
        chains.append((expr1, meaning1))

        # Binding chains
        expr2 = f"{w1}@{w2}@{w3}"
        meaning2 = f"{m1} to {m2} to {m3}"
        chains.append((expr2, meaning2))

        # Mixed chains
        expr3 = f"{w1}*{w2}@{w3}"
        meaning3 = f"{m1} fused {m2} connected {m3}"
        chains.append((expr3, meaning3))

    for expr, meaning in chains:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_domain_bridges() -> List[Tuple[str, str, str, str]]:
    """Generate expressions bridging temporal and quality domains"""
    expressions = []

    bridges = [
        ('pas*goo', 'good past (positive history)'),
        ('fut*bad', 'bad future (negative prospect)'),
        ('now*hot', 'passionate present'),
        ('now*col', 'cold present (detached)'),
        ('lon*ful', 'full long (completely extended)'),
        ('bre*emp', 'empty brief (hollow moment)'),
        ('cha*new', 'new change (fresh transformation)'),
        ('sta*old', 'old stability (ancient permanence)'),
        ('gro*goo', 'good growth (positive development)'),
        ('dec*bad', 'bad decay (negative degeneration)'),
        ('bir*new', 'new birth (fresh beginning)'),
        ('dea*old', 'old death (ancient ending)'),
        ('acc*hot', 'passionate acceleration'),
        ('grd*col', 'cold gradualness'),
        ('has*hot', 'passionate haste'),
        ('del*col', 'cold delay'),
        ('cyc*hol', 'held cycle (sustained recurrence)'),
        ('seq*rel', 'released sequence (free progression)'),
        ('evo*new', 'new evolution (emerging adaptation)'),
        ('tra*goo', 'good transformation (positive change)'),
    ]

    for expr, meaning in bridges:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def deduplicate(all_expr: List[Tuple[str, str, str, str]]) -> List[Tuple[str, str, str, str]]:
    """Remove duplicates keeping first occurrence"""
    seen: Set[str] = set()
    unique = []
    for expr, meaning, example, domain in all_expr:
        if expr not in seen:
            seen.add(expr)
            unique.append((expr, meaning, example, domain))
    return unique

def main():
    """Generate and save extended temporal expressions"""

    print("Generating extended temporal expressions...")

    all_expressions = []

    print("  - Gradient variants...")
    all_expressions.extend(generate_gradient_variants())

    print("  - Pairwise fusions...")
    all_expressions.extend(generate_pairwise_fusions())

    print("  - Binding links...")
    all_expressions.extend(generate_binding_links())

    print("  - Negation pairs...")
    all_expressions.extend(generate_negation_pairs())

    print("  - Conditional chains...")
    all_expressions.extend(generate_conditional_chains())

    print("  - Polarity spreads...")
    all_expressions.extend(generate_polarity_spreads())

    print("  - Intensity modifiers...")
    all_expressions.extend(generate_intensity_modifiers())

    print("  - Nested compositions...")
    all_expressions.extend(generate_nested_compositions())

    print("  - Spectrum expressions...")
    all_expressions.extend(generate_spectrum_expressions())

    print("  - Inverse relationships...")
    all_expressions.extend(generate_inverse_relationships())

    print("  - Combination chains...")
    all_expressions.extend(generate_combination_chains())

    print("  - Domain bridges...")
    all_expressions.extend(generate_domain_bridges())

    # Deduplicate
    unique_expressions = deduplicate(all_expressions)

    # Save to CSV
    output_path = '/home/eric/src/limntown/limn/crew/linguist/generated/temporal_expressions.csv'
    with open(output_path, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['expression', 'meaning', 'example', 'domain'])
        for expr, meaning, example, domain in sorted(unique_expressions, key=lambda x: x[0]):
            writer.writerow([expr, meaning, example, domain])

    print(f"\nGenerated {len(unique_expressions)} unique temporal expressions")
    print(f"Saved to: {output_path}")

    # Statistics
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
        elif 'to' in expr:
            expr_type = 'spectrum'
        elif '^-' in expr:
            expr_type = 'inverse'
        else:
            expr_type = 'other'
        expr_types[expr_type] = expr_types.get(expr_type, 0) + 1

    print("\nExpression type distribution:")
    for expr_type, count in sorted(expr_types.items(), key=lambda x: -x[1]):
        print(f"  {expr_type}: {count}")

if __name__ == '__main__':
    main()
