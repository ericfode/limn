#!/bin/bash
# Regenerate limn-vocab.pl from Dolt database
# Run this when vocabulary changes in the Dolt database

DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$DIR/../../data/vocabulary"

echo "Regenerating vocabulary from Dolt..."

{
  echo "%% Limn Vocabulary - Auto-generated from Dolt database"
  echo "%% Run regenerate-vocab.sh to update"
  echo ""

  # Count words and operators
  WORD_COUNT=$(dolt sql -q "SELECT COUNT(*) FROM words" -r csv | tail -n +2)
  OP_COUNT=$(dolt sql -q "SELECT COUNT(*) FROM operators" -r csv | tail -n +2)
  echo "%% $WORD_COUNT words, $OP_COUNT operators"
  echo ""

  echo "%% Word facts: word(Word)."
  dolt sql -q "SELECT word FROM words ORDER BY word" -r csv | tail -n +2 | while read w; do
    echo "word('$w')."
  done

  echo ""
  echo "%% Operator facts: operator(Op, Type)."
  dolt sql -q "SELECT word, op_type FROM operators ORDER BY word" -r csv | tail -n +2 | while IFS=, read op type; do
    echo "operator('$op', '$type')."
  done
} > ../../tools/linter/limn-vocab.pl

echo "Done. Generated $(wc -l < ../../tools/linter/limn-vocab.pl) lines."
