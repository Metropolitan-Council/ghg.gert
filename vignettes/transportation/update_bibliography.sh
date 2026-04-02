#!/bin/bash
# Update transportation_elasticities.bib from the CPRG bibliography
# Run this script whenever the CPRG bibliography is updated or new citations are added to the .qmd

# Path to CPRG bibliography
CPRG_BIB="../../../ghg-cprg/metcouncil-cprg-ghg.bib"

# Source Quarto document
QMD_FILE="transportation_elasticities_table.qmd"

# Output file
OUTPUT_BIB="transportation_elasticities.bib"

# Check if .qmd file exists
if [ ! -f "$QMD_FILE" ]; then
    echo "⚠ Error: $QMD_FILE not found"
    exit 1
fi

echo "Scanning $QMD_FILE for citation keys..."

# Extract all citation keys from the .qmd file
# Matches both @citationKey and [@citationKey] patterns
# Extracts unique citation keys, sorted alphabetically
CITATION_KEYS=$(grep -oE '@[a-zA-Z0-9_]+' "$QMD_FILE" | sed 's/@//' | sort -u)

if [ -z "$CITATION_KEYS" ]; then
    echo "⚠ No citation keys found in $QMD_FILE"
    exit 1
fi

KEY_COUNT=$(echo "$CITATION_KEYS" | wc -l)
echo "Found $KEY_COUNT unique citation keys"
echo "Extracting from CPRG bibliography..."

# Clear output file
> "$OUTPUT_BIB"

# Track counts
EXTRACTED_COUNT=0
MISSING_KEYS=""

# Extract each citation
while IFS= read -r key; do
    ENTRY=$(awk -v k="$key" '/^@[a-zA-Z]+\{'"$key"',/,/^}$/ {print; if (/^}$/) print ""}' "$CPRG_BIB")
    
    if [ -n "$ENTRY" ]; then
        echo "$ENTRY" >> "$OUTPUT_BIB"
        ((EXTRACTED_COUNT++))
    else
        MISSING_KEYS="${MISSING_KEYS}  - ${key}\n"
    fi
done <<< "$CITATION_KEYS"

# Report results
LINE_COUNT=$(wc -l < "$OUTPUT_BIB")

echo ""
echo "✓ Successfully extracted $EXTRACTED_COUNT of $KEY_COUNT citations"
echo "✓ Output: $OUTPUT_BIB ($LINE_COUNT lines)"

if [ -n "$MISSING_KEYS" ]; then
    MISSING_COUNT=$((KEY_COUNT - EXTRACTED_COUNT))
    echo ""
    echo "⚠ Warning: $MISSING_COUNT citation(s) not found in CPRG bibliography:"
    echo -e "$MISSING_KEYS"
fi

echo ""
echo "Preview of first entry:"
head -5 "$OUTPUT_BIB"
