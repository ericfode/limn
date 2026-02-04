#!/usr/bin/env python3
"""
Gradient Expression Generator for Limn Language
Generates A^n expressions using the ^ (intensity/gradient) operator
Covers 1000+ combinations across emotional, physical, cognitive, and abstract domains
"""

import csv
import json
from typing import List, Tuple, Dict
from collections import defaultdict

# Intensity values and their semantic meanings
INTENSITY_LEVELS = {
    0.1: "minimal",
    0.2: "slight",
    0.3: "subtle",
    0.5: "moderate",
    0.7: "strong",
    0.8: "intense",
    0.9: "extreme",
    1.0: "maximum"
}

# Domain categories for semantic organization
GRADABLE_WORDS = {
    "physical_intensity": {
        "hot": ("temperature increase", ["barely warm", "slightly warm", "moderately warm", "warm", "hot", "very hot", "scorching", "searing"]),
        "col": ("temperature decrease", ["barely cold", "slightly cool", "moderately cool", "cool", "cold", "very cold", "freezing", "absolute zero"]),
        "lit": ("weight lightness", ["heavy-ish", "fairly light", "light", "lighter", "very light", "feather-light", "ultra-light", "weightless"]),
        "hev": ("weight heaviness", ["light-ish", "fairly heavy", "heavy", "heavier", "very heavy", "ultra-heavy", "crushing", "immovable"]),
        "bri": ("brightness", ["dim", "faint", "dull", "moderate-light", "bright", "very bright", "brilliant", "blinding"]),
        "dim": ("darkness", ["pale", "faint", "dim", "darker", "dark", "very dark", "pitch-black", "absolute-dark"]),
        "smo": ("smoothness", ["rough", "somewhat smooth", "fairly smooth", "smooth", "smoother", "very smooth", "polished", "mirror-smooth"]),
        "rou": ("roughness", ["smooth", "somewhat rough", "fairly rough", "rough", "rougher", "very rough", "coarse", "abrasive"]),
        "har": ("hardness", ["soft", "somewhat hard", "fairly hard", "hard", "harder", "very hard", "diamond-hard", "indestructible"]),
        "sof": ("softness", ["stiff", "somewhat soft", "fairly soft", "soft", "softer", "very soft", "pillowy", "liquid-soft"]),
        "wet": ("wetness", ["dry", "damp", "moist", "wet", "wetter", "very wet", "dripping", "soaked"]),
        "dry": ("dryness", ["wet", "moist", "damp", "dry", "drier", "very dry", "arid", "parched"]),
        "mag": ("size/magnitude", ["tiny", "small", "smallish", "medium", "large", "very large", "huge", "enormous"]),
        "min": ("minuteness", ["large", "medium-small", "small", "smaller", "tiny", "very tiny", "microscopic", "subatomic"]),
        "lon": ("length", ["short", "fairly long", "moderately long", "long", "longer", "very long", "extensive", "infinite"]),
        "bre": ("brevity", ["long", "fairly short", "moderately short", "short", "shorter", "very short", "instantaneous", "zero-duration"]),
        "wid": ("width", ["narrow", "fairly wide", "moderately wide", "wide", "wider", "very wide", "vast", "boundless"]),
        "nar": ("narrowness", ["wide", "fairly narrow", "moderately narrow", "narrow", "narrower", "very narrow", "thread-thin", "invisible"]),
        "den": ("density", ["sparse", "somewhat dense", "moderately dense", "dense", "denser", "very dense", "packed", "neutron-star-dense"]),
        "spa": ("sparsity", ["dense", "somewhat sparse", "moderately sparse", "sparse", "sparser", "very sparse", "empty", "void"]),
    },
    "emotional": {
        "joy": ("happiness intensity", ["slight happy", "somewhat happy", "fairly happy", "happy", "very happy", "delighted", "ecstatic", "transcendent bliss"]),
        "sad": ("sadness intensity", ["slight sad", "somewhat sad", "fairly sad", "sad", "very sad", "sorrowful", "anguished", "utterly devastated"]),
        "ang": ("anger intensity", ["mildly annoyed", "somewhat annoyed", "fairly angry", "angry", "very angry", "furious", "enraged", "apocalyptic rage"]),
        "lov": ("love intensity", ["slight affection", "somewhat loving", "fairly loving", "loving", "very loving", "devoted", "passionate", "all-consuming love"]),
        "hat": ("hate intensity", ["mild dislike", "somewhat disliking", "fairly hostile", "hostile", "very hostile", "hateful", "contemptuous", "absolute hatred"]),
        "hop": ("hope intensity", ["slight hope", "somewhat hopeful", "fairly hopeful", "hopeful", "very hopeful", "confident", "assured", "absolute faith"]),
        "des": ("despair intensity", ["slight doubt", "somewhat doubtful", "fairly despairing", "despairing", "very despairing", "hopeless", "utterly lost", "absolute despair"]),
        "fea": ("fear intensity", ["slight worried", "somewhat anxious", "fairly scared", "scared", "very scared", "terrified", "petrified", "absolute terror"]),
        "tru": ("trust intensity", ["slight trust", "somewhat trusting", "fairly trusting", "trusting", "very trusting", "confident", "absolute faith", "blind trust"]),
        "sus": ("suspicion intensity", ["slight doubt", "somewhat suspicious", "fairly suspicious", "suspicious", "very suspicious", "distrustful", "paranoid", "absolute suspicion"]),
        "cal": ("calmness", ["slightly agitated", "somewhat calm", "fairly calm", "calm", "very calm", "peaceful", "serene", "perfect peace"]),
        "anx": ("anxiety", ["slight worry", "somewhat anxious", "fairly anxious", "anxious", "very anxious", "nervous", "panicked", "absolute panic"]),
        "exc": ("excitement", ["slight interest", "somewhat excited", "fairly excited", "excited", "very excited", "thrilled", "ecstatic", "uncontrollable excitement"]),
        "pri": ("pride", ["slight proud", "somewhat proud", "fairly proud", "proud", "very proud", "haughty", "arrogant", "unbridled pride"]),
        "sha": ("shame", ["slight embarrassed", "somewhat ashamed", "fairly ashamed", "ashamed", "very ashamed", "humiliated", "mortified", "complete mortification"]),
        "gui": ("guilt", ["slight guilt", "somewhat guilty", "fairly guilty", "guilty", "very guilty", "remorseful", "tortured", "consuming guilt"]),
        "gra": ("gratitude", ["slight grateful", "somewhat grateful", "fairly grateful", "grateful", "very grateful", "deeply thankful", "profoundly grateful", "absolute gratitude"]),
        "env": ("envy", ["slight envy", "somewhat envious", "fairly envious", "envious", "very envious", "covetous", "green with envy", "consuming envy"]),
        "jea": ("jealousy", ["slight protective", "somewhat protective", "fairly jealous", "jealous", "very jealous", "intensely protective", "obsessive", "consuming jealousy"]),
        "dis": ("disgust", ["slight repulsed", "somewhat disgusted", "fairly disgusted", "disgusted", "very disgusted", "revolted", "absolutely repulsed", "utter disgust"]),
        "awe": ("awe", ["slight amazed", "somewhat amazed", "fairly awed", "awed", "very awed", "profoundly amazed", "stunned", "absolute awe"]),
        "emp": ("empathy", ["slight understanding", "somewhat empathetic", "fairly empathetic", "empathetic", "very empathetic", "deeply empathetic", "profound connection", "complete empathy"]),
    },
    "cognitive": {
        "thi": ("thinking intensity", ["barely conscious", "slight thought", "somewhat thoughtful", "thoughtful", "very thoughtful", "intensely focused", "deep contemplation", "total absorption"]),
        "und": ("understanding", ["confused", "partially clear", "somewhat clear", "clear", "very clear", "well understood", "deeply understood", "complete mastery"]),
        "kno": ("knowing/certainty", ["uncertain", "slightly sure", "somewhat sure", "fairly sure", "sure", "very sure", "certain", "absolute certainty"]),
        "bel": ("believing", ["slight belief", "somewhat believing", "fairly believing", "believing", "very believing", "strong faith", "absolute conviction", "unshakeable belief"]),
        "dou": ("doubting", ["slight doubt", "somewhat doubtful", "fairly doubtful", "doubtful", "very doubtful", "deeply skeptical", "complete skepticism", "absolute disbelief"]),
        "rem": ("remembering", ["faint recall", "slight memory", "somewhat remembered", "remembered", "well remembered", "vivid memory", "crystal clear recall", "perfect recall"]),
        "obl": ("forgetting", ["slight lapse", "somewhat forgotten", "partially forgotten", "forgotten", "well forgotten", "completely forgotten", "utterly erased", "absolute amnesia"]),
        "ima": ("imagining", ["slight fantasy", "somewhat imaginative", "fairly imaginative", "imaginative", "very imaginative", "highly creative", "wild imagination", "boundless fantasy"]),
        "cur": ("curiosity", ["slight interest", "somewhat curious", "fairly curious", "curious", "very curious", "intensely curious", "burning curiosity", "insatiable curiosity"]),
        "att": ("attention", ["minimal focus", "slight attention", "moderate attention", "attentive", "very attentive", "highly focused", "laser-focused", "total immersion"]),
        "ign": ("ignoring", ["slight distraction", "somewhat ignoring", "fairly ignoring", "ignoring", "very ignoring", "completely ignoring", "total dismissal", "absolute oblivion"]),
        "lea": ("learning", ["slight grasp", "basic understanding", "good understanding", "learning well", "learning very well", "mastering", "complete mastery", "enlightened"]),
    },
    "states": {
        "awa": ("wakefulness", ["slightly drowsy", "somewhat awake", "fairly alert", "alert", "very alert", "wide awake", "hyperaware", "absolute wakefulness"]),
        "sle": ("sleepiness", ["fully awake", "somewhat tired", "fairly tired", "tired", "very tired", "exhausted", "completely exhausted", "comatose"]),
        "hun": ("hunger", ["barely hungry", "slightly hungry", "somewhat hungry", "hungry", "very hungry", "ravenous", "starving", "absolute starvation"]),
        "str": ("strength", ["barely strong", "slightly strong", "somewhat strong", "strong", "very strong", "extremely strong", "superhuman", "ultimate strength"]),
        "wea": ("weakness", ["barely weak", "slightly weak", "somewhat weak", "weak", "very weak", "extremely weak", "feeble", "complete incapacity"]),
        "hea": ("healthiness", ["somewhat ill", "mostly healthy", "fairly healthy", "healthy", "very healthy", "extremely healthy", "robust", "perfect health"]),
        "sic": ("sickness", ["minor illness", "somewhat sick", "fairly sick", "sick", "very sick", "critically ill", "terminal", "absolute infirmity"]),
        "you": ("youth/newness", ["aging", "somewhat young", "fairly young", "young", "very young", "newborn", "just created", "absolute newness"]),
        "old": ("age/oldness", ["relatively new", "somewhat old", "fairly old", "old", "very old", "ancient", "primordial", "eternal ancientness"]),
        "awa": ("awareness", ["barely conscious", "slightly aware", "somewhat aware", "aware", "very aware", "highly aware", "enlightened", "omniscient"]),
        "dre": ("dreaming", ["almost awake", "slight dream-state", "fairly dreamy", "dreamy", "very dreamy", "deep dreaming", "immersed in dream", "dream-world merger"]),
        "bor": ("boredom", ["slight interest", "somewhat interested", "fairly interested", "interested", "neutral interest", "slightly bored", "bored", "terminally bored"]),
    },
    "action_intensity": {
        "mov": ("motion intensity", ["barely moving", "slight motion", "slow motion", "moving", "fast motion", "very fast", "rapid", "instantaneous"]),
        "ris": ("rising intensity", ["barely rising", "slightly rising", "moderately rising", "rising", "rapidly rising", "sharply rising", "soaring", "infinite ascension"]),
        "fal": ("falling intensity", ["barely falling", "slightly falling", "moderately falling", "falling", "rapidly falling", "sharply falling", "plummeting", "infinite descent"]),
        "gro": ("growth", ["minimal growth", "slight growth", "moderate growth", "growing", "rapid growth", "explosive growth", "exponential growth", "infinite expansion"]),
        "dec": ("decay", ["minimal decay", "slight decay", "moderate decay", "decaying", "rapid decay", "complete decay", "absolute dissolution", "total destruction"]),
        "exp": ("expansion", ["minimal expansion", "slight expansion", "moderate expansion", "expanding", "rapid expansion", "explosive expansion", "rapid-fire", "instantaneous spread"]),
        "con": ("contraction", ["minimal contraction", "slight contraction", "moderate contraction", "contracting", "rapid contraction", "sudden contraction", "implosion", "point-singularity"]),
        "rot": ("rotation", ["barely rotating", "slight rotation", "moderate rotation", "rotating", "fast rotation", "very fast rotation", "spinning", "absolute spin"]),
    },
    "abstract": {
        "big": ("magnitude/proportion", ["tiny", "small", "moderate", "large", "huge", "massive", "colossal", "infinite"]),
        "mor": ("increase", ["slight increase", "modest increase", "moderate increase", "increase", "substantial increase", "rapid increase", "dramatic increase", "unbounded increase"]),
        "les": ("decrease", ["slight decrease", "modest decrease", "moderate decrease", "decrease", "substantial decrease", "rapid decrease", "dramatic decrease", "zero"]),
        "use": ("utility/usefulness", ["hardly useful", "slightly useful", "somewhat useful", "useful", "very useful", "extremely useful", "essential", "absolutely indispensable"]),
        "eas": ("ease", ["difficult", "somewhat difficult", "fairly difficult", "somewhat easy", "easy", "very easy", "trivial", "effortless"]),
        "val": ("value/worth", ["worthless", "low value", "moderate value", "valuable", "highly valuable", "extremely valuable", "priceless", "infinitely valuable"]),
        "rea": ("reality/actuality", ["nonexistent", "imaginary", "questionable", "possibly real", "probably real", "actually real", "definitely real", "absolutely real"]),
        "fak": ("fakeness/artificiality", ["absolutely real", "mostly real", "partially real", "partially fake", "mostly fake", "completely fake", "pure illusion", "absolute falseness"]),
    }
}

def generate_gradient_expressions() -> List[Dict[str, str]]:
    """Generate all gradient expressions across domains."""
    expressions = []

    for domain, words_dict in GRADABLE_WORDS.items():
        for word, (description, gradient_labels) in words_dict.items():
            # For each word, generate expressions for all intensity levels
            for intensity, intensity_name in INTENSITY_LEVELS.items():
                idx = int(intensity * 10) - 1  # Convert 0.1-1.0 to 0-9 index
                if 0 <= idx < len(gradient_labels):
                    meaning = gradient_labels[idx]
                else:
                    meaning = gradient_labels[-1]  # Fallback to max

                expression = f"{word}^{intensity}"
                example = f"{word}^{intensity} = {meaning}"

                expressions.append({
                    "expression": expression,
                    "meaning": meaning,
                    "example": example,
                    "domain": domain.replace("_", " ").title(),
                    "base_word": word,
                    "intensity_level": intensity_name,
                    "intensity_value": intensity,
                    "semantic_group": description
                })

    # Add composite expressions (A^n OP B^m combinations)
    # These create 2-word compounds with coordinated intensities
    composite_pairs = [
        ("hot", "bri", "thermal brightness"),
        ("col", "dim", "cold darkness"),
        ("joy", "hop", "optimistic happiness"),
        ("sad", "des", "hopeless sadness"),
        ("lov", "tru", "trusting love"),
        ("ang", "hat", "righteous anger"),
        ("str", "will", "determined strength"),  # will as placeholder
        ("lit", "mov", "light movement"),
        ("mag", "str", "powerful size"),
        ("gro", "thi", "thoughtful growth"),
        ("exp", "joy", "joyful expansion"),
        ("dec", "sad", "sad decline"),
        ("ris", "hop", "hopeful rising"),
        ("fal", "des", "despairing fall"),
        ("eas", "use", "usefully easy"),
        ("val", "rea", "real value"),
    ]

    for word1, word2, description in composite_pairs:
        for intensity1, intensity1_name in list(INTENSITY_LEVELS.items())[:4]:  # 0.1-0.5
            for intensity2, intensity2_name in list(INTENSITY_LEVELS.items())[4:]:  # 0.7-1.0
                expression = f"{word1}^{intensity1}+{word2}^{intensity2}"
                meaning = f"{intensity1_name} {word1} with {intensity2_name} {word2}"
                example = f"{expression} = {meaning}"

                expressions.append({
                    "expression": expression,
                    "meaning": meaning,
                    "example": example,
                    "domain": "composite".title(),
                    "base_word": f"{word1}|{word2}",
                    "intensity_level": f"{intensity1_name}+{intensity2_name}",
                    "intensity_value": (intensity1 + intensity2) / 2,
                    "semantic_group": description
                })

    # Add negation patterns (nu word^n)
    negation_words = [
        "hot", "col", "joy", "sad", "tru", "use", "rea", "goo", "hea",
        "str", "awa", "kno", "lov", "hop", "eas", "awa"
    ]

    for word in negation_words:
        for intensity, intensity_name in list(INTENSITY_LEVELS.items())[2:7]:  # 0.3-0.9
            expression = f"nu {word}^{intensity}"
            meaning = f"negated/not {intensity_name} {word}"
            example = f"{expression} = {meaning}"

            expressions.append({
                "expression": expression,
                "meaning": meaning,
                "example": example,
                "domain": "negation".title(),
                "base_word": word,
                "intensity_level": intensity_name,
                "intensity_value": intensity,
                "semantic_group": "negation operator"
            })

    # Add modifier patterns (ve/so for narrowing/broadening)
    modifier_words = [
        "mag", "min", "lon", "bre", "wid", "nar", "joy", "sad",
        "hot", "col", "str", "wea", "gro", "dec"
    ]

    for word in modifier_words:
        for intensity, intensity_name in INTENSITY_LEVELS.items():
            # Narrowing modifier (ve)
            expression_ve = f"ve {word}^{intensity}"
            meaning_ve = f"prototypical/narrow {intensity_name} {word}"
            example_ve = f"{expression_ve} = {meaning_ve}"

            expressions.append({
                "expression": expression_ve,
                "meaning": meaning_ve,
                "example": example_ve,
                "domain": "modifiers".title(),
                "base_word": word,
                "intensity_level": intensity_name,
                "intensity_value": intensity,
                "semantic_group": "narrowing (ve) operator"
            })

            # Broadening modifier (so)
            expression_so = f"so {word}^{intensity}"
            meaning_so = f"peripheral/broad {intensity_name} {word}"
            example_so = f"{expression_so} = {meaning_so}"

            expressions.append({
                "expression": expression_so,
                "meaning": meaning_so,
                "example": example_so,
                "domain": "modifiers".title(),
                "base_word": word,
                "intensity_level": intensity_name,
                "intensity_value": intensity,
                "semantic_group": "broadening (so) operator"
            })

    return expressions


def main():
    """Generate and save gradient expressions to CSV."""
    print("Generating gradient expressions...")
    expressions = generate_gradient_expressions()

    print(f"Generated {len(expressions)} expressions")

    # Save to CSV
    output_path = "/home/eric/src/limntown/limn/crew/linguist/generated/gradient_expressions.csv"

    with open(output_path, 'w', newline='', encoding='utf-8') as f:
        fieldnames = [
            "expression",
            "meaning",
            "example",
            "domain",
            "base_word",
            "intensity_level",
            "intensity_value",
            "semantic_group"
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(expressions)

    print(f"\nSaved {len(expressions)} expressions to {output_path}")

    # Print summary statistics
    domains = defaultdict(int)
    for expr in expressions:
        domains[expr['domain']] += 1

    print("\nExpression counts by domain:")
    for domain in sorted(domains.keys()):
        print(f"  {domain}: {domains[domain]}")

    # Print sample expressions
    print("\nSample expressions:")
    for expr in expressions[::len(expressions)//10][:10]:
        print(f"  {expr['expression']:20} = {expr['meaning']}")


if __name__ == "__main__":
    main()
