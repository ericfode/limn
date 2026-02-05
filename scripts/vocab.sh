#!/bin/bash
# Limn Vocabulary Query Helper
# Usage: vocab.sh <command> [args]
#
# val alw | wor che | lmn tru
# (validate always | words checked | limn true)

VOCAB_DIR="$(dirname "$0")/../data/vocabulary"

cd "$VOCAB_DIR" || exit 1

case "$1" in
    search)
        # Search words by pattern (includes domain)
        dolt sql -q "SELECT w.word, w.meaning, d.name as domain, w.examples FROM words w LEFT JOIN domains d ON w.domain_id = d.id WHERE w.word LIKE '%$2%' OR w.meaning LIKE '%$2%' OR w.examples LIKE '%$2%' ORDER BY w.word"
        ;;

    domain)
        # List words in a domain
        dolt sql -q "SELECT w.word, w.meaning FROM words w JOIN domains d ON w.domain_id = d.id WHERE d.id = $2 OR d.name LIKE '%$2%' ORDER BY w.word"
        ;;

    check)
        # Check if a single word exists in the DB
        result=$(dolt sql -q "SELECT word, meaning FROM words WHERE word = '$2'" -r csv | tail -n +2)
        if [ -n "$result" ]; then
            dolt sql -q "SELECT w.word, w.meaning, d.name as domain FROM words w LEFT JOIN domains d ON w.domain_id = d.id WHERE w.word = '$2'"
            exit 0
        else
            echo "✗ '$2' not in vocabulary"
            exit 1
        fi
        ;;

    batch-check)
        # Check multiple words at once
        # Usage: vocab.sh batch-check word1 word2 word3 ...
        shift
        if [ $# -eq 0 ]; then
            echo "Usage: vocab.sh batch-check word1 word2 word3 ..."
            exit 1
        fi
        errors=0
        for word in "$@"; do
            result=$(dolt sql -q "SELECT word, meaning FROM words WHERE word = '$word'" -r csv | tail -n +2)
            if [ -n "$result" ]; then
                meaning=$(echo "$result" | cut -d',' -f2-)
                echo "  ✓ $word = $meaning"
            else
                echo "  ✗ $word — NOT IN VOCABULARY"
                errors=$((errors + 1))
            fi
        done
        if [ $errors -gt 0 ]; then
            echo ""
            echo "$errors unknown word(s). Run: vocab.sh search <word> to find alternatives."
            exit 1
        else
            echo ""
            echo "All words valid."
            exit 0
        fi
        ;;

    validate)
        # Validate a Limn sentence/phrase
        # Usage: vocab.sh validate "lov dee | fea gon | hop sma"
        # Strips operators (@ * ^ \ : ±) and pipe separators, checks each word
        shift
        input="$*"
        if [ -z "$input" ]; then
            echo "Usage: vocab.sh validate \"lov dee | fea gon | hop sma\""
            exit 1
        fi

        # Strip Limn operators and punctuation, extract bare words
        cleaned=$(echo "$input" | sed 's/[|@*^\\:±><→(){}]/ /g' | sed 's/\^[0-9.]*//g' | tr -s ' ')
        errors=0
        total=0
        bad_words=""

        for word in $cleaned; do
            # Skip numbers, empty strings, and operator fragments
            if echo "$word" | grep -qE '^[0-9.]+$'; then continue; fi
            if [ -z "$word" ]; then continue; fi
            # Skip common structural words not in DB
            if echo "$word" | grep -qE '^(a|the|of|in|to|and|or|is|if|at|by)$'; then continue; fi

            total=$((total + 1))
            result=$(dolt sql -q "SELECT word, meaning FROM words WHERE word = '$word'" -r csv | tail -n +2)
            if [ -n "$result" ]; then
                meaning=$(echo "$result" | cut -d',' -f2-)
                echo "  ✓ $word = $meaning"
            else
                echo "  ✗ $word — NOT IN VOCABULARY"
                bad_words="$bad_words $word"
                errors=$((errors + 1))
            fi
        done

        echo ""
        if [ $errors -gt 0 ]; then
            echo "INVALID: $errors/$total words not found:$bad_words"
            echo "Run: vocab.sh search <word> to find alternatives."
            exit 1
        else
            echo "VALID: $total/$total words confirmed."
            exit 0
        fi
        ;;

    gotchas)
        # Common false friends — words that look like English abbreviations but aren't
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
        echo "Rule: Limn words are NOT English abbreviations. They are semantic"
        echo "atoms from Latin, Greek, and phonaesthetic roots. Always check."
        ;;

    stats)
        # Show vocabulary statistics
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
        # List all operators
        dolt sql -q "SELECT word, op_type, precedence, description FROM operators ORDER BY precedence, op_type"
        ;;

    collisions)
        # Show collision history
        dolt sql -q "SELECT word, meaning1, meaning2, resolution, resolved_by FROM collision_log ORDER BY id"
        ;;

    add)
        # Add a new word (with collision check)
        if [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ] || [ -z "$5" ]; then
            echo "Usage: vocab.sh add <word> <source> <meaning> <domain_id> [examples]"
            exit 1
        fi
        # Check for collision first
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
        # Run arbitrary SQL
        shift
        dolt sql -q "$*"
        ;;

    *)
        echo "Limn Vocabulary Helper"
        echo ""
        echo "Usage: vocab.sh <command> [args]"
        echo ""
        echo "Commands:"
        echo "  check <word>          Check if a word exists (shows meaning + domain)"
        echo "  batch-check w1 w2 ... Check multiple words at once"
        echo "  validate \"phrase\"     Validate a full Limn sentence"
        echo "  search <term>         Search words by pattern (includes domain)"
        echo "  gotchas               Show common false-friend traps"
        echo "  domain <id|name>      List words in a domain"
        echo "  stats                 Show vocabulary statistics"
        echo "  operators             List all operators"
        echo "  collisions            Show collision history"
        echo "  add <word> <src> <meaning> <domain_id> [examples]"
        echo "  sql <query>           Run arbitrary SQL query"
        echo ""
        echo "Before writing Limn, always:"
        echo "  vocab.sh validate \"lov dee | fea gon | hop sma\""
        ;;
esac
