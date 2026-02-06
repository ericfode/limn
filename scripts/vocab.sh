#!/bin/bash
# Limn Vocabulary Helper
# Usage: vocab.sh <command> [args]
#
# val alw | wor che | lmn tru
# (validate always | words checked | limn true)

VOCAB_DIR="$(dirname "$0")/../data/vocabulary"

cd "$VOCAB_DIR" || exit 1

# ============================================================
# WORD LOOKUP (shared helper)
# ============================================================

lookup_word() {
    local word="$1"
    dolt sql -q "SELECT word, meaning FROM words WHERE word = '$word'" -r csv | tail -n +2
}

lookup_word_full() {
    local word="$1"
    dolt sql -q "SELECT w.word, w.meaning, d.name as domain FROM words w LEFT JOIN domains d ON w.domain_id = d.id WHERE w.word = '$word'"
}

lookup_word_domain() {
    local word="$1"
    dolt sql -q "SELECT d.name FROM words w LEFT JOIN domains d ON w.domain_id = d.id WHERE w.word = '$word'" -r csv | tail -n +2
}

# ============================================================
# OPERATOR SEMANTICS
# ============================================================

# v4 compositional operators (from bootstrap spec)
# These are the single-character operators that compose words
describe_operator() {
    local op="$1"
    case "$op" in
        @)  echo "projection (extract B-aspect of A)" ;;
        '*') echo "interference (emergent blend of A and B)" ;;
        '^') echo "gradient (intensity 0.0-1.0)" ;;
        '\') echo "subtraction (A without B-component)" ;;
        :)  echo "conditional (A given context B)" ;;
        ±)  echo "superposition (quantum both A and B)" ;;
        *)  echo "unknown operator" ;;
    esac
}

compose_meaning() {
    local a_meaning="$1"
    local op="$2"
    local b_meaning="$3"
    case "$op" in
        @)  echo "${b_meaning}-aspect of ${a_meaning}" ;;
        '*') echo "${a_meaning} blended with ${b_meaning}" ;;
        '\') echo "${a_meaning} without ${b_meaning}" ;;
        :)  echo "${a_meaning} given ${b_meaning}" ;;
        ±)  echo "both ${a_meaning} and ${b_meaning}" ;;
        *)  echo "${a_meaning} ${op} ${b_meaning}" ;;
    esac
}

# ============================================================
# EXPRESSION PARSER
# ============================================================

# Parse and validate a single token (word or composed expression)
# Returns: 0 if valid, 1 if invalid
# Output: validation line to stdout
parse_token() {
    local token="$1"
    local errors=0

    # Skip empty tokens
    [ -z "$token" ] && return 0

    # Skip pure numbers (gradient values like 0.7)
    if echo "$token" | grep -qE '^[0-9.]+$'; then return 0; fi

    # Check for gradient operator: word^N.N
    if echo "$token" | grep -qE '\^[0-9]'; then
        local word=$(echo "$token" | sed 's/\^.*//')
        local intensity=$(echo "$token" | sed 's/.*\^//')
        local result=$(lookup_word "$word")
        if [ -n "$result" ]; then
            local meaning=$(echo "$result" | cut -d',' -f2-)
            echo "  ✓ ${token} = ${meaning} at ${intensity} intensity (gradient)"
            return 0
        else
            echo "  ✗ ${token} — '${word}' NOT IN VOCABULARY"
            return 1
        fi
    fi

    # Check for binary compositional operators: A@B, A*B, A\B, A:B, A±B
    # Order matters: check in precedence order (@ before * before \ before : before ±)
    local op=""
    local left=""
    local right=""

    if [[ "$token" == *@* ]]; then
        op="@"; left="${token%%@*}"; right="${token#*@}"
    elif [[ "$token" == *\** ]]; then
        op="*"; left="${token%%\**}"; right="${token#*\*}"
    elif [[ "$token" == *\\* ]]; then
        op="\\"; left="${token%%\\*}"; right="${token#*\\}"
    elif [[ "$token" == *:* ]]; then
        op=":"; left="${token%%:*}"; right="${token#*:}"
    elif [[ "$token" == *±* ]]; then
        op="±"; left="${token%%±*}"; right="${token#*±}"
    fi

    if [ -n "$op" ] && [ -n "$left" ] && [ -n "$right" ]; then
        # Composed expression
        local left_result=$(lookup_word "$left")
        local right_result=$(lookup_word "$right")
        local left_ok=1
        local right_ok=1
        local left_meaning=""
        local right_meaning=""

        if [ -n "$left_result" ]; then
            left_meaning=$(echo "$left_result" | cut -d',' -f2-)
            left_ok=0
        fi
        if [ -n "$right_result" ]; then
            right_meaning=$(echo "$right_result" | cut -d',' -f2-)
            right_ok=0
        fi

        if [ $left_ok -eq 0 ] && [ $right_ok -eq 0 ]; then
            local composed=$(compose_meaning "$left_meaning" "$op" "$right_meaning")
            local op_desc=$(describe_operator "$op")
            echo "  ✓ ${token} = ${composed} (${op_desc})"
            return 0
        else
            [ $left_ok -ne 0 ] && echo "  ✗ ${token} — '${left}' NOT IN VOCABULARY"
            [ $right_ok -ne 0 ] && echo "  ✗ ${token} — '${right}' NOT IN VOCABULARY"
            return 1
        fi
    fi

    # Plain word lookup
    local result=$(lookup_word "$token")
    if [ -n "$result" ]; then
        local meaning=$(echo "$result" | cut -d',' -f2-)
        echo "  ✓ ${token} = ${meaning}"
        return 0
    fi

    # Check if it's a known DB operator (nu, ve, al, etc.)
    local op_result=$(dolt sql -q "SELECT word, description FROM operators WHERE word = '$token'" -r csv | tail -n +2)
    if [ -n "$op_result" ]; then
        local op_desc=$(echo "$op_result" | cut -d',' -f2-)
        echo "  ✓ ${token} = ${op_desc} (operator)"
        return 0
    fi

    echo "  ✗ ${token} — NOT IN VOCABULARY"
    return 1
}

# ============================================================
# COMMANDS
# ============================================================

case "$1" in
    search)
        # Search words by pattern (includes domain)
        dolt sql -q "SELECT w.word, w.meaning, d.name as domain, w.examples FROM words w LEFT JOIN domains d ON w.domain_id = d.id WHERE w.word LIKE '%$2%' OR w.meaning LIKE '%$2%' OR w.examples LIKE '%$2%' ORDER BY w.word"
        ;;

    domain)
        # List words in a domain
        dolt sql -q "SELECT w.word, w.meaning, d.name as domain FROM words w JOIN domains d ON w.domain_id = d.id WHERE d.id = $2 OR d.name LIKE '%$2%' ORDER BY w.word"
        ;;

    check)
        # Check if a single word exists in the DB
        result=$(lookup_word "$2")
        if [ -n "$result" ]; then
            lookup_word_full "$2"
            exit 0
        else
            echo "✗ '$2' not in vocabulary"
            exit 1
        fi
        ;;

    batch-check|batch)
        # Check multiple words at once — table with word, status, meaning, domain
        shift
        if [ $# -eq 0 ]; then
            echo "Usage: vocab.sh batch word1 word2 word3 ..."
            exit 1
        fi
        printf "  %-8s %-5s %-30s %s\n" "WORD" "OK?" "MEANING" "DOMAIN"
        printf "  %-8s %-5s %-30s %s\n" "────────" "─────" "──────────────────────────────" "──────────"
        errors=0
        for word in "$@"; do
            result=$(lookup_word "$word")
            if [ -n "$result" ]; then
                meaning=$(echo "$result" | cut -d',' -f2-)
                domain=$(lookup_word_domain "$word")
                printf "  %-8s %-5s %-30s %s\n" "$word" "yes" "$meaning" "$domain"
            else
                printf "  %-8s %-5s %-30s %s\n" "$word" "NO" "—" "—"
                errors=$((errors + 1))
            fi
        done
        echo ""
        if [ $errors -gt 0 ]; then
            echo "$errors unknown word(s). Run: vocab.sh search <word> to find alternatives."
            exit 1
        else
            echo "All $# word(s) valid."
            exit 0
        fi
        ;;

    validate)
        # Validate a Limn sentence/phrase with full operator parsing
        # Usage: vocab.sh validate "lov@fea | kno^0.7 | und*dou"
        shift
        input="$*"
        if [ -z "$input" ]; then
            echo "Usage: vocab.sh validate \"lov dee | fea gon | hop sma\""
            echo "       vocab.sh validate \"lov@fea | kno^0.7 | und*dou\""
            exit 1
        fi

        # Split on pipes and whitespace into tokens
        # Preserve operator-joined tokens (lov@fea stays together)
        tokens=$(echo "$input" | sed 's/|/ /g' | sed 's/→/ /g' | sed 's/[>]/ /g' | tr -s ' ')

        errors=0
        total=0

        for token in $tokens; do
            # Skip empty
            [ -z "$token" ] && continue
            # Skip pure punctuation
            if echo "$token" | grep -qE '^[|>→(){}]+$'; then continue; fi
            # Skip numbers
            if echo "$token" | grep -qE '^[0-9.]+$'; then continue; fi

            total=$((total + 1))
            if ! parse_token "$token"; then
                errors=$((errors + 1))
            fi
        done

        echo ""
        if [ $errors -gt 0 ]; then
            echo "INVALID: $errors/$total token(s) have errors."
            echo "Run: vocab.sh search <word> to find alternatives."
            exit 1
        else
            echo "VALID: $total/$total tokens confirmed."
            exit 0
        fi
        ;;

    reduce)
        # Reduce a compositional expression to its meaning
        # Usage: vocab.sh reduce "lov@fea"
        shift
        input="$*"
        if [ -z "$input" ]; then
            echo "Usage: vocab.sh reduce \"lov@fea\""
            exit 1
        fi
        parse_token "$input"
        ;;

    gotchas)
        # Common false friends and traps
        echo "=== Limn False Friends ==="
        echo "Words that LOOK like English abbreviations but mean something different."
        echo "Always validate with: vocab.sh check <word>"
        echo ""
        echo "  Intuition     You'd guess    Actual word    Actual meaning"
        echo "  ─────────     ──────────     ──────────     ──────────────"
        echo "  \"list\"         lis            lis            listen"
        echo "  \"describe\"     des            des            desire"
        echo "  \"improve\"      imp            imp            implode"
        echo "  \"bug\" (sw)     bug            bug            insect (use err)"
        echo "  \"read\"         rea            rea            real (use red)"
        echo "  \"result\"       res            res            rest (use ans/resp)"
        echo "  \"where\"        whe            whe            wheel"
        echo "  \"learn\"        lea            lea            leader (use gro)"
        echo "  \"language\"     lan            lan            land/arrive"
        echo "  \"danger\"       dan            dan            dance (use ris/thr)"
        echo "  \"certainty\"    cer            cer            ceremony (use sur)"
        echo ""
        echo "=== Words That Do NOT Exist ==="
        echo "Common guesses that are NOT in the vocabulary at all."
        echo ""
        echo "  Guess    Expected meaning    Reality"
        echo "  ─────    ────────────────    ───────"
        echo "  fer      fear                DOES NOT EXIST (use fea)"
        echo "  dbt      doubt               DOES NOT EXIST (use dou)"
        echo "  trs      trust               DOES NOT EXIST (use tru)"
        echo ""
        echo "=== SQL Gotcha ==="
        echo "  The backslash operator (\\) needs CHAR(92) in SQL WHERE clauses."
        echo "  Wrong:  WHERE op = '\\\\'"
        echo "  Right:  WHERE op = CHAR(92)"
        echo ""
        echo "Rule: Limn words are NOT English abbreviations. They are semantic"
        echo "atoms from Latin, Greek, and phonaesthetic roots. Always check."
        ;;

    stats)
        echo "=== Limn Vocabulary Statistics ==="
        dolt sql -q "SELECT 'Total words' as metric, COUNT(*) as value FROM words
                     UNION ALL
                     SELECT 'Operators', COUNT(*) FROM operators
                     UNION ALL
                     SELECT 'Collisions resolved', COUNT(*) FROM collision_log"
        echo ""
        echo "=== Words by Domain ==="
        dolt sql -q "SELECT d.name, COUNT(w.word) as count FROM domains d LEFT JOIN words w ON d.id = w.domain_id GROUP BY d.id, d.name ORDER BY d.id"
        ;;

    operators)
        echo "=== v4 Compositional Operators ==="
        echo "  @   projection    A@B = B-aspect of A         lov@fea = fear-aspect of love"
        echo "  *   interference  A*B = emergent blend         und*dou = understanding ⊗ doubt"
        echo "  ^   gradient      A^N = intensity (0.0-1.0)    kno^0.7 = knowing at 70%"
        echo "  \\   subtraction   A\\B = A without B            lov\\fea = love without fear"
        echo "  :   conditional   A:B = A given context B      lov:tru = love given trust"
        echo "  ±   superposition A±B = quantum both/and       was±now = past and present"
        echo ""
        echo "  Precedence: ^ > @ > * > \\ > : > ±"
        echo ""
        echo "=== DB Operators ==="
        dolt sql -q "SELECT word, op_type, precedence, description FROM operators ORDER BY precedence DESC, op_type"
        ;;

    collisions)
        dolt sql -q "SELECT word, meaning1, meaning2, resolution, resolved_by FROM collision_log ORDER BY id"
        ;;

    add)
        if [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ] || [ -z "$5" ]; then
            echo "Usage: vocab.sh add <word> <source> <meaning> <domain_id> [examples]"
            exit 1
        fi
        existing=$(dolt sql -q "SELECT word FROM words WHERE word = '$2'" -r csv | tail -n +2)
        if [ -n "$existing" ]; then
            echo "Cannot add: '$2' already exists"
            dolt sql -q "SELECT word, meaning, domain_id FROM words WHERE word = '$2'"
            exit 1
        fi
        dolt sql -q "INSERT INTO words (word, source, meaning, domain_id, examples) VALUES ('$2', '$3', '$4', $5, '$6')"
        echo "✓ Added: $2"
        dolt add . && dolt commit -m "Add word: $2"
        ;;

    sql)
        shift
        dolt sql -q "$*"
        ;;

    *)
        echo "Limn Vocabulary Helper"
        echo ""
        echo "Usage: vocab.sh <command> [args]"
        echo ""
        echo "Commands:"
        echo "  validate \"phrase\"     Parse and validate Limn (words + operators)"
        echo "  reduce \"lov@fea\"     Reduce a single composed expression"
        echo "  check <word>          Check if a word exists (meaning + domain)"
        echo "  batch w1 w2 ...       Check multiple words at once (table format)"
        echo "  search <term>         Search words by pattern (includes domain)"
        echo "  gotchas               Show common false-friend traps"
        echo "  operators             Show all operators (v4 + DB)"
        echo "  domain <id|name>      List words in a domain"
        echo "  stats                 Show vocabulary statistics"
        echo "  collisions            Show collision history"
        echo "  add <word> <src> <meaning> <domain_id> [examples]"
        echo "  sql <query>           Run arbitrary SQL query"
        echo ""
        echo "Before writing Limn, always:"
        echo "  vocab.sh validate \"lov dee | fea@hop | kno^0.7\""
        ;;
esac
