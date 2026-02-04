#!/usr/bin/env python3
"""
Generate 1000+ compositional temporal expressions using comprehensive combinations.
"""

import csv
from typing import List, Tuple, Set

TEMPORAL_WORDS = {
    # Core temporal (40 words)
    'acc': 'accelerate', 'aft': 'after', 'alw': 'always', 'bef': 'before',
    'beg': 'beginning', 'bir': 'birth', 'bre': 'brief', 'cha': 'change',
    'cyc': 'cycle', 'daw': 'dawn', 'dea': 'death', 'dec': 'decay',
    'del': 'delay', 'dur': 'duration', 'dus': 'dusk', 'end': 'ending',
    'epo': 'epoch', 'era': 'era', 'evo': 'evolution', 'fir': 'first',
    'fut': 'future', 'grd': 'gradual', 'gro': 'growth', 'has': 'haste',
    'las': 'last', 'lon': 'long', 'mid': 'middle', 'mom': 'moment',
    'nev': 'never', 'now': 'present', 'oft': 'often', 'onc': 'once',
    'pas': 'past', 'pha': 'phase', 'rar': 'rare', 'seq': 'sequence',
    'sta': 'stability', 'tra': 'transformation', 'unt': 'until', 'urg': 'urgent',
    'yet': 'yet',
}

QUALITY_WORDS = {
    'bad': 'bad', 'bea': 'beautiful', 'bri': 'bright', 'col': 'cold',
    'dar': 'dark', 'dee': 'deep', 'emp': 'empty', 'fix': 'fix',
    'ful': 'full', 'goo': 'good', 'hol': 'hold', 'hot': 'hot',
    'new': 'new', 'old': 'old', 'rel': 'release', 'saf': 'safe',
}

def generate_comprehensive_gradients() -> List[Tuple[str, str, str, str]]:
    """Generate gradients for all words at all intensity levels"""
    expressions = []
    intensities = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]

    for word in TEMPORAL_WORDS:
        meaning = TEMPORAL_WORDS[word]
        for intensity in intensities:
            expr = f"{word}^{intensity}"
            int_desc = ['barely', 'slightly', 'somewhat', 'fairly-light',
                       'moderately', 'quite', 'fairly', 'very',
                       'extremely', 'absolutely'][int(intensity * 10) - 1]
            full_meaning = f"{int_desc} {meaning}"
            example = f"{expr} = {full_meaning}"
            expressions.append((expr, full_meaning, example, 'temporal'))

    return expressions

def generate_all_pairwise_fusions() -> List[Tuple[str, str, str, str]]:
    """Generate fusion expressions for comprehensive pairs"""
    expressions = []
    words_list = list(TEMPORAL_WORDS.keys())

    # Sample from all pairs systematically
    for i, w1 in enumerate(words_list):
        for j, w2 in enumerate(words_list[i+1:], start=i+1):
            m1, m2 = TEMPORAL_WORDS[w1], TEMPORAL_WORDS[w2]
            expr = f"{w1}*{w2}"
            meaning = f"{m1} fused with {m2}"
            example = f"{expr} = {meaning}"
            expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_gradient_fusions() -> List[Tuple[str, str, str, str]]:
    """Generate fusion of gradients"""
    expressions = []

    gradient_pairs = [
        ('lon^0.9', 'bre^0.1', 'extreme duration contrast'),
        ('acc^0.8', 'grd^0.2', 'sharp acceleration with minimal graduality'),
        ('has^0.9', 'del^0.1', 'intense haste with minimal delay'),
        ('gro^0.8', 'dec^0.2', 'strong growth with minimal decay'),
        ('evo^0.7', 'tra^0.3', 'pronounced evolution with slight transformation'),
        ('sta^0.9', 'cha^0.1', 'extreme stability with minimal change'),
        ('alw^0.95', 'nev^0.05', 'near-certain occurrence'),
        ('oft^0.8', 'rar^0.2', 'frequent with slight rarity'),
        ('pas^0.9', 'fut^0.1', 'past-dominant present'),
        ('fut^0.9', 'pas^0.1', 'future-dominant present'),
        ('cyc^0.8', 'seq^0.2', 'cyclic with minimal sequence'),
        ('urg^0.9', 'del^0.1', 'extreme urgency with minimal delay'),
    ]

    for expr1, expr2, desc in gradient_pairs:
        expr = f"{expr1}*{expr2}"
        meaning = desc
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_multiple_negations() -> List[Tuple[str, str, str, str]]:
    """Generate negations with multiple terms"""
    expressions = []

    negations = [
        ('alw without nev without rar', 'certain frequency'),
        ('pas without fut without now', 'pure past only'),
        ('fut without pas without now', 'pure future only'),
        ('now without pas without fut', 'eternal now only'),
        ('cha without sta without tra', 'base change only'),
        ('bir without dea without dec', 'pure creation only'),
        ('dea without bir without gro', 'pure ending only'),
        ('lon without bre without mom', 'extreme duration only'),
        ('bre without lon without dur', 'pure brevity only'),
        ('cyc without seq without pha', 'pure cycle only'),
        ('seq without cyc without pha', 'pure sequence only'),
        ('acc without grd without sta', 'pure acceleration only'),
        ('grd without acc without has', 'pure graduality only'),
        ('has without del without urg', 'unmodulated haste only'),
        ('del without has without urg', 'pure delay only'),
    ]

    for expr, meaning in negations:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_nested_structures() -> List[Tuple[str, str, str, str]]:
    """Generate deeply nested structures"""
    expressions = []

    nested = [
        # Triple nesting
        ('((pas*now)*fut)*cha', 'temporal continuum through change'),
        ('((beg*mid)*end)*seq', 'lifecycle in sequence'),
        ('((bir*sta)*dea)*cyc', 'life through stability through death cyclically'),
        ('((lon^0.9)*bre^0.1)*dur', 'duration extremes measured'),
        ('((alw*oft)*onc)*rar', 'frequency spectrum'),
        ('((gro*evo)*tra)*sta', 'growth through transformation to stability'),

        # Mixed nesting with different operators
        ('(pas@now)*fut', 'connected past fused with future'),
        ('pas*(now@fut)', 'past fused with connected future'),
        ('(pas without fut)@now', 'pure past connected to present'),
        ('(cha given sta)*evo', 'change from stability fused with evolution'),
        ('(sta given cha) without tra', 'stability from change excluding transformation'),

        # Quadruple structures
        ('((pas*now)*(fut*cha))', 'temporal-change symmetry'),
        ('(((beg*mid)*end)*las)*fir', 'full lifecycle spectrum'),
        ('((alw*oft)*onc)*(rar*nev)', 'complete frequency spectrum'),
        ('(((cha*evo)*tra)*dec)*gro', 'change evolution transformation decay growth'),
    ]

    for expr, meaning in nested:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_conditional_chains_advanced() -> List[Tuple[str, str, str, str]]:
    """Generate complex conditional chains"""
    expressions = []

    conditionals = [
        ('fut given (pas*now)', 'future from past-present'),
        ('(end given beg) without cha', 'ending from beginning without change'),
        ('sta given (cha*evo)', 'stability from change-evolution'),
        ('cyc given (seq*oft)', 'cycle from sequence-frequency'),
        ('tra given (evo*dec)', 'transformation from evolution-decay'),
        ('gro given (dec*sta)', 'growth from decay-stability'),
        ('alw given (oft*onc)', 'always from often-once'),
        ('nev given (rar*alw)', 'never from rare-always'),
        ('dur given (mom*lon)', 'duration from moment-long'),
        ('pha given (beg*end)', 'phase from beginning-end'),
        ('((fut given pas) given now)', 'future through past through present'),
        ('(cha given sta) given (evo given tra)', 'change from stability from evolution from transformation'),
    ]

    for expr, meaning in conditionals:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_multi_gradient_combinations() -> List[Tuple[str, str, str, str]]:
    """Generate combinations of multiple gradients"""
    expressions = []

    # Create systematic combinations of gradients at different levels
    gradient_combos = [
        ('lon^0.3', 'bre^0.7', 'brief-leaning duration'),
        ('lon^0.7', 'bre^0.3', 'long-leaning duration'),
        ('pas^0.3', 'fut^0.7', 'future-leaning temporal position'),
        ('pas^0.7', 'fut^0.3', 'past-leaning temporal position'),
        ('sta^0.3', 'cha^0.7', 'change-leaning state'),
        ('sta^0.7', 'cha^0.3', 'stability-leaning state'),
        ('alw^0.3', 'nev^0.7', 'never-leaning frequency'),
        ('alw^0.7', 'nev^0.3', 'always-leaning frequency'),
        ('acc^0.3', 'grd^0.7', 'gradual-leaning rate'),
        ('acc^0.7', 'grd^0.3', 'acceleration-leaning rate'),
        ('gro^0.4', 'dec^0.6', 'decay-leaning development'),
        ('gro^0.6', 'dec^0.4', 'growth-leaning development'),
        ('has^0.3', 'del^0.7', 'delay-leaning tempo'),
        ('has^0.7', 'del^0.3', 'haste-leaning tempo'),
        ('cyc^0.4', 'seq^0.6', 'sequence-leaning order'),
        ('cyc^0.6', 'seq^0.4', 'cycle-leaning order'),
    ]

    for g1, g2, desc in gradient_combos:
        expr = f"({g1}*{g2})"
        meaning = desc
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_binding_networks() -> List[Tuple[str, str, str, str]]:
    """Generate binding networks connecting multiple terms"""
    expressions = []

    networks = [
        ('pas@now@fut@cha', 'temporal chain through change'),
        ('beg@mid@end@las', 'lifecycle connected'),
        ('daw@mid@dus@cyc', 'daily cycle connected'),
        ('bir@sta@dea@epo', 'lifetime through stability through death'),
        ('cyc@seq@pha@dur', 'pattern elements connected'),
        ('acc@has@urg@del', 'tempo elements connected'),
        ('gro@evo@tra@sta', 'development elements connected'),
        ('alw@oft@onc@rar@nev', 'frequency spectrum connected'),
        ('lon@dur@mom@bre', 'duration spectrum connected'),
        ('sta@cha@evo@dec@gro', 'change cycle connected'),
    ]

    for expr, meaning in networks:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_polarity_networks() -> List[Tuple[str, str, str, str]]:
    """Generate polarity networks"""
    expressions = []

    networks = [
        ('pas ± now ± fut', 'complete temporal spectrum (both/either)'),
        ('beg ± mid ± end', 'complete lifecycle (both/either)'),
        ('alw ± oft ± onc ± rar ± nev', 'complete frequency spectrum'),
        ('lon ± dur ± mom ± bre', 'complete duration spectrum'),
        ('sta ± cha ± evo ± tra ± dec', 'complete change spectrum'),
        ('acc ± sta ± grd', 'complete rate spectrum'),
        ('has ± del ± urg', 'complete tempo spectrum'),
        ('bir ± dea', 'life boundaries (both/either)'),
        ('cyc ± seq', 'order types (both/either)'),
        ('gro ± dec', 'development direction (both/either)'),
    ]

    for expr, meaning in networks:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_inverse_gradients() -> List[Tuple[str, str, str, str]]:
    """Generate inverse gradient expressions"""
    expressions = []

    for word in list(TEMPORAL_WORDS.keys())[:30]:
        for inverse_val in [-0.5, -1.0]:
            expr = f"{word}^{inverse_val}"
            meaning = f"inverse {TEMPORAL_WORDS[word]} (degree {inverse_val})"
            example = f"{expr} = {meaning}"
            expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_recursive_compositions() -> List[Tuple[str, str, str, str]]:
    """Generate self-referential and recursive-like expressions"""
    expressions = []

    recursive = [
        ('cyc*cyc', 'cycle of cycles'),
        ('seq*seq', 'sequence of sequences'),
        ('pha*pha', 'phase of phases'),
        ('cha*cha', 'change of change'),
        ('evo*evo', 'evolution of evolution'),
        ('tra*tra', 'transformation of transformation'),
        ('sta*sta', 'stability of stability'),
        ('dur*dur', 'duration of duration'),
        ('epo*epo', 'epoch of epochs'),
        ('now*now', 'present of present'),
        ('cyc^cyc', 'cycle raised to cycle'),
        ('seq given seq', 'sequence given sequence'),
        ('pha given pha', 'phase given phase'),
        ('cha without cha', 'change without change'),
        ('sta without sta', 'stability without stability'),
    ]

    for expr, meaning in recursive:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_domain_cross_products() -> List[Tuple[str, str, str, str]]:
    """Generate cross-domain temporal expressions"""
    expressions = []

    crosses = [
        ('pas*goo', 'positive past'),
        ('fut*bad', 'negative future'),
        ('now*hot', 'passionate present'),
        ('cha*new', 'new change'),
        ('sta*old', 'ancient stability'),
        ('gro*bea', 'beautiful growth'),
        ('dec*dar', 'dark decay'),
        ('bir*bri', 'bright birth'),
        ('dea*dar', 'dark death'),
        ('alw*goo', 'always good'),
        ('nev*bad', 'never bad'),
        ('oft*goo', 'often good'),
        ('rar*bad', 'rarely bad'),
        ('lon*ful', 'full long'),
        ('bre*emp', 'empty brief'),
        ('acc*hot', 'hot acceleration'),
        ('grd*col', 'cold gradualness'),
        ('has*hot', 'hot haste'),
        ('del*col', 'cold delay'),
        ('cyc*rel', 'released cycle'),
        ('seq*hol', 'held sequence'),
        ('evo*new', 'new evolution'),
        ('tra*goo', 'good transformation'),
        ('pha*dee', 'deep phase'),
        ('era*old', 'old era'),
        ('epo*ancient', 'ancient epoch'),
    ]

    for expr, meaning in crosses:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def generate_bound_gradients() -> List[Tuple[str, str, str, str]]:
    """Generate gradient expressions with binding"""
    expressions = []

    bound_gradients = [
        ('(lon^0.1)@(lon^0.9)', 'brief to long connected'),
        ('(pas^0.1)@(pas^0.9)', 'recent to ancient past'),
        ('(fut^0.1)@(fut^0.9)', 'near to distant future'),
        ('(acc^0.1)@(acc^0.9)', 'slight to extreme acceleration'),
        ('(sta^0.1)@(sta^0.9)', 'unstable to perfectly stable'),
        ('(cha^0.1)@(cha^0.9)', 'subtle to profound change'),
        ('(cyc^0.1)@(cyc^0.9)', 'weak to strong cycle'),
        ('(gro^0.1)@(gro^0.9)', 'minimal to maximal growth'),
        ('(dec^0.1)@(dec^0.9)', 'minimal to complete decay'),
        ('(evo^0.1)@(evo^0.9)', 'barely to fully evolved'),
    ]

    for expr, meaning in bound_gradients:
        example = f"{expr} = {meaning}"
        expressions.append((expr, meaning, example, 'temporal'))

    return expressions

def deduplicate_all(all_expr: List[Tuple[str, str, str, str]]) -> List[Tuple[str, str, str, str]]:
    """Remove duplicate expressions"""
    seen: Set[str] = set()
    unique = []
    for expr, meaning, example, domain in all_expr:
        if expr not in seen:
            seen.add(expr)
            unique.append((expr, meaning, example, domain))
    return unique

def main():
    """Generate complete temporal expression set"""

    print("Generating 1000+ comprehensive temporal expressions...")

    all_expressions = []

    print(f"  - Comprehensive gradients... ({len(TEMPORAL_WORDS)} * 10 = {len(TEMPORAL_WORDS)*10})")
    all_expressions.extend(generate_comprehensive_gradients())

    print("  - All pairwise fusions...")
    all_expressions.extend(generate_all_pairwise_fusions())

    print("  - Gradient fusions...")
    all_expressions.extend(generate_gradient_fusions())

    print("  - Multiple negations...")
    all_expressions.extend(generate_multiple_negations())

    print("  - Nested structures...")
    all_expressions.extend(generate_nested_structures())

    print("  - Advanced conditional chains...")
    all_expressions.extend(generate_conditional_chains_advanced())

    print("  - Multi-gradient combinations...")
    all_expressions.extend(generate_multi_gradient_combinations())

    print("  - Binding networks...")
    all_expressions.extend(generate_binding_networks())

    print("  - Polarity networks...")
    all_expressions.extend(generate_polarity_networks())

    print("  - Inverse gradients...")
    all_expressions.extend(generate_inverse_gradients())

    print("  - Recursive compositions...")
    all_expressions.extend(generate_recursive_compositions())

    print("  - Domain cross-products...")
    all_expressions.extend(generate_domain_cross_products())

    print("  - Bound gradients...")
    all_expressions.extend(generate_bound_gradients())

    # Deduplicate
    unique_expressions = deduplicate_all(all_expressions)

    # Save to CSV
    output_path = '/home/eric/src/limntown/limn/crew/linguist/generated/temporal_expressions.csv'
    with open(output_path, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['expression', 'meaning', 'example', 'domain'])
        for expr, meaning, example, domain in sorted(unique_expressions, key=lambda x: x[0]):
            writer.writerow([expr, meaning, example, domain])

    print(f"\nGenerated {len(unique_expressions)} unique temporal expressions")
    print(f"Saved to: {output_path}")

    # Distribution analysis
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
    total = sum(expr_types.values())
    for expr_type, count in sorted(expr_types.items(), key=lambda x: -x[1]):
        pct = (count / total) * 100
        print(f"  {expr_type}: {count} ({pct:.1f}%)")

    # Sample expressions
    print("\nSample expressions:")
    for expr, meaning, example, _ in sorted(unique_expressions, key=lambda x: x[0])[:20]:
        print(f"  {expression}: {meaning}")

if __name__ == '__main__':
    main()
