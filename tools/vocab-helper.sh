#!/bin/bash
# Vocabulary Helper for HGttG Translation
# Helper script for checking and adding words to Limn vocabulary

set -e

VOCAB_DB="."  # Dolt database is in the translator workspace root
LINTER_DIR="tools/linter"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    cat <<EOF
Usage: $0 <command> [args]

Commands:
    check <word>              Check if word exists in vocabulary
    search <meaning>          Search for words by meaning
    add <word> <source> <meaning>  Add new word to vocabulary
    suggest <english>         Suggest abbreviated form for English word
    regenerate               Regenerate limn-vocab.pl from database
    stats                    Show vocabulary statistics

Examples:
    $0 check tow
    $0 search "towel"
    $0 add tow towel "cloth for drying"
    $0 suggest "towel"
    $0 regenerate
EOF
    exit 1
}

# Check if word exists
check_word() {
    local word="$1"
    if [[ -z "$word" ]]; then
        echo -e "${RED}Error: word required${NC}"
        exit 1
    fi

    echo "Checking word: $word"
    result=$(dolt sql -q "SELECT word, source, meaning FROM words WHERE word='$word'" -r csv)

    if [[ $(echo "$result" | wc -l) -gt 1 ]]; then
        echo -e "${GREEN}✓ Word exists:${NC}"
        echo "$result" | tail -n +2
    else
        echo -e "${YELLOW}✗ Word not found${NC}"
        # Check collision log
        collision=$(dolt sql -q "SELECT * FROM collision_log WHERE word='$word'" -r csv)
        if [[ $(echo "$collision" | wc -l) -gt 1 ]]; then
            echo -e "${YELLOW}⚠ Collision logged for this word:${NC}"
            echo "$collision" | tail -n +2
        fi
    fi
}

# Search by meaning
search_meaning() {
    local meaning="$1"
    if [[ -z "$meaning" ]]; then
        echo -e "${RED}Error: meaning required${NC}"
        exit 1
    fi

    echo "Searching for: $meaning"
    result=$(dolt sql -q "SELECT word, source, meaning FROM words WHERE meaning LIKE '%$meaning%' OR source LIKE '%$meaning%'" -r csv)

    if [[ $(echo "$result" | wc -l) -gt 1 ]]; then
        echo -e "${GREEN}Found matches:${NC}"
        echo "$result"
    else
        echo -e "${YELLOW}No matches found${NC}"
    fi
}

# Add new word
add_word() {
    local word="$1"
    local source="$2"
    local meaning="$3"

    if [[ -z "$word" ]] || [[ -z "$source" ]] || [[ -z "$meaning" ]]; then
        echo -e "${RED}Error: word, source, and meaning required${NC}"
        usage
    fi

    # Validate word length (2-4 characters)
    if [[ ${#word} -lt 2 ]] || [[ ${#word} -gt 4 ]]; then
        echo -e "${RED}Error: word must be 2-4 characters${NC}"
        exit 1
    fi

    # Check if word already exists
    existing=$(dolt sql -q "SELECT word FROM words WHERE word='$word'" -r csv | tail -n +2)
    if [[ -n "$existing" ]]; then
        echo -e "${RED}Error: word '$word' already exists${NC}"
        check_word "$word"
        exit 1
    fi

    echo "Adding word: $word ($source) = $meaning"
    dolt sql -q "INSERT INTO words (word, source, meaning) VALUES ('$word', '$source', '$meaning')"

    echo -e "${GREEN}✓ Word added successfully${NC}"
    echo "Remember to:"
    echo "  1. Commit the change: dolt add . && dolt commit -m 'Add word: $word'"
    echo "  2. Regenerate vocab file: $0 regenerate"
}

# Suggest abbreviated form
suggest_word() {
    local english="$1"
    if [[ -z "$english" ]]; then
        echo -e "${RED}Error: English word required${NC}"
        exit 1
    fi

    echo "Suggesting abbreviations for: $english"

    # Generate suggestions
    # First 3 letters
    local abbr3="${english:0:3}"
    # First 4 letters
    local abbr4="${english:0:4}"
    # First 2 letters
    local abbr2="${english:0:2}"

    echo -e "\nSuggestions (check each for collisions):"
    echo "  3-letter: $abbr3"
    echo "  4-letter: $abbr4"
    echo "  2-letter: $abbr2"

    echo -e "\nChecking collisions:"
    for abbr in "$abbr2" "$abbr3" "$abbr4"; do
        result=$(dolt sql -q "SELECT word, source, meaning FROM words WHERE word='$abbr'" -r csv | tail -n +2)
        if [[ -n "$result" ]]; then
            echo -e "  ${RED}✗ $abbr: COLLISION${NC}"
            echo "    $result"
        else
            echo -e "  ${GREEN}✓ $abbr: available${NC}"
        fi
    done
}

# Regenerate limn-vocab.pl
regenerate_vocab() {
    echo "Regenerating limn-vocab.pl from Dolt database..."
    cd "$LINTER_DIR"
    bash regenerate-vocab.sh
    echo -e "${GREEN}✓ Vocabulary file regenerated${NC}"
}

# Show statistics
show_stats() {
    echo "Vocabulary Statistics:"
    dolt sql -q "SELECT COUNT(*) as total_words FROM words"
    dolt sql -q "SELECT COUNT(*) as total_operators FROM operators"
    echo ""
    echo "Recent additions:"
    dolt sql -q "SELECT word, source, meaning, added_date FROM words ORDER BY added_date DESC LIMIT 10"
}

# Main command dispatcher
case "${1:-}" in
    check)
        check_word "$2"
        ;;
    search)
        search_meaning "$2"
        ;;
    add)
        add_word "$2" "$3" "$4"
        ;;
    suggest)
        suggest_word "$2"
        ;;
    regenerate)
        regenerate_vocab
        ;;
    stats)
        show_stats
        ;;
    *)
        usage
        ;;
esac
