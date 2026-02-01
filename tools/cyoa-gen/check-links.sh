#!/bin/bash
# Check for dead links in generated CYOA site
# Usage: ./check-links.sh /path/to/site

SITE_DIR="${1:-.}"
ERRORS=0

echo "◢ CHECKING LINKS in $SITE_DIR"

for html in "$SITE_DIR"/*.html; do
    [ -f "$html" ] || continue

    # Extract all href values pointing to .html files
    grep -oP 'href="[^"#]+\.html"' "$html" 2>/dev/null | \
    sed 's/href="//;s/"$//' | \
    while read -r link; do
        target="$SITE_DIR/$link"
        if [ ! -f "$target" ]; then
            echo "  ✗ $(basename "$html") → $link (NOT FOUND)"
            ERRORS=$((ERRORS + 1))
        fi
    done
done

if [ $ERRORS -eq 0 ]; then
    echo "  ✓ All links valid"
    exit 0
else
    echo "  ⚠ Found $ERRORS dead links"
    exit 1
fi
