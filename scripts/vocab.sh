#!/bin/bash
# Limn Vocabulary Query Helper
# Usage: vocab.sh <command> [args]

VOCAB_DIR="$(dirname "$0")/../data/vocabulary"

cd "$VOCAB_DIR" || exit 1

case "$1" in
    search)
        # Search words by pattern
        dolt sql -q "SELECT word, meaning, examples FROM words WHERE word LIKE '%$2%' OR meaning LIKE '%$2%' OR examples LIKE '%$2%'"
        ;;

    domain)
        # List words in a domain
        dolt sql -q "SELECT w.word, w.meaning FROM words w JOIN domains d ON w.domain_id = d.id WHERE d.id = $2 OR d.name LIKE '%$2%' ORDER BY w.word"
        ;;

    check)
        # Check if word exists (collision prevention)
        result=$(dolt sql -q "SELECT word, meaning FROM words WHERE word = '$2'" -r csv | tail -n +2)
        if [ -n "$result" ]; then
            echo "⚠️  COLLISION: '$2' already exists"
            dolt sql -q "SELECT word, source, meaning, domain_id FROM words WHERE word = '$2'"
            exit 1
        else
            echo "✓ '$2' is available"
            exit 0
        fi
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
        "$0" check "$2" > /dev/null 2>&1
        if [ $? -eq 1 ]; then
            echo "Cannot add: word already exists"
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
        echo "Limn Vocabulary Query Helper"
        echo ""
        echo "Usage: vocab.sh <command> [args]"
        echo ""
        echo "Commands:"
        echo "  search <term>     Search words by pattern"
        echo "  domain <id|name>  List words in a domain"
        echo "  check <word>      Check if word exists (collision check)"
        echo "  stats             Show vocabulary statistics"
        echo "  operators         List all operators"
        echo "  collisions        Show collision history"
        echo "  add <word> <source> <meaning> <domain_id> [examples]"
        echo "                    Add a new word (with collision check)"
        echo "  sql <query>       Run arbitrary SQL query"
        ;;
esac
