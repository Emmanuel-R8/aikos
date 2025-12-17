#!/usr/bin/env bash
# Compare C and Zig emulator execution logs line by line
# Identifies first difference and shows context

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

C_LOG="c_emulator_execution_log.txt"
ZIG_LOG="zig_emulator_execution_log.txt"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=== Execution Log Comparison ==="
echo "Date: $(date '+%Y-%m-%d %H:%M')"
echo ""

# Check if both logs exist
if [ ! -f "$C_LOG" ]; then
    echo -e "${RED}ERROR: C emulator log not found: $C_LOG${NC}"
    exit 1
fi

if [ ! -f "$ZIG_LOG" ]; then
    echo -e "${RED}ERROR: Zig emulator log not found: $ZIG_LOG${NC}"
    exit 1
fi

C_LINES=$(wc -l < "$C_LOG")
ZIG_LINES=$(wc -l < "$ZIG_LOG")

echo "C emulator log: $C_LINES lines"
echo "Zig emulator log: $ZIG_LINES lines"
echo ""

# Compare line by line
echo "Comparing logs line by line..."
echo ""

FIRST_DIFF=0
MAX_COMPARE=${1:-100}  # Compare first N lines (default 100)

line_num=0
while IFS= read -r c_line && IFS= read -r zig_line <&3; do
    line_num=$((line_num + 1))
    
    if [ "$line_num" -gt "$MAX_COMPARE" ]; then
        break
    fi
    
    if [ "$c_line" != "$zig_line" ]; then
        if [ "$FIRST_DIFF" -eq 0 ]; then
            FIRST_DIFF=$line_num
            echo -e "${RED}First difference at line $line_num${NC}"
            echo ""
            echo -e "${BLUE}C emulator (line $line_num):${NC}"
            echo "$c_line"
            echo ""
            echo -e "${BLUE}Zig emulator (line $line_num):${NC}"
            echo "$zig_line"
            echo ""
            
            # Show character-by-character comparison for first 200 chars
            echo -e "${YELLOW}Character-by-character comparison (first 200 chars):${NC}"
            c_chars=$(echo -n "$c_line" | head -c 200 | od -An -tx1 | tr -d ' \n')
            zig_chars=$(echo -n "$zig_line" | head -c 200 | od -An -tx1 | tr -d ' \n')
            
            # Find first differing byte
            min_len=$(( ${#c_chars} < ${#zig_chars} ? ${#c_chars} : ${#zig_chars} ))
            for ((i=0; i<min_len; i+=2)); do
                c_byte="${c_chars:$i:2}"
                zig_byte="${zig_chars:$i:2}"
                if [ "$c_byte" != "$zig_byte" ]; then
                    byte_pos=$((i/2))
                    echo "  First difference at byte position $byte_pos:"
                    echo "    C:   0x$c_byte"
                    echo "    Zig: 0x$zig_byte"
                    echo "    Context: '${c_line:$byte_pos:20}' vs '${zig_line:$byte_pos:20}'"
                    break
                fi
            done
            echo ""
        fi
    fi
done < "$C_LOG" 3< "$ZIG_LOG"

if [ "$FIRST_DIFF" -eq 0 ]; then
    echo -e "${GREEN}✓ Logs match for first $line_num lines${NC}"
else
    echo ""
    echo -e "${YELLOW}Showing context around first difference:${NC}"
    echo ""
    
    # Show 3 lines before and after
    start_line=$((FIRST_DIFF - 3))
    end_line=$((FIRST_DIFF + 3))
    
    if [ "$start_line" -lt 1 ]; then
        start_line=1
    fi
    
    echo -e "${BLUE}C emulator (lines $start_line-$end_line):${NC}"
    sed -n "${start_line},${end_line}p" "$C_LOG" | nl -v "$start_line" -w 4 -s ': '
    echo ""
    echo -e "${BLUE}Zig emulator (lines $start_line-$end_line):${NC}"
    sed -n "${start_line},${end_line}p" "$ZIG_LOG" | nl -v "$start_line" -w 4 -s ': '
fi

echo ""
echo "=== Summary ==="
if [ "$FIRST_DIFF" -eq 0 ]; then
    echo -e "${GREEN}✓ Logs are identical for first $line_num lines${NC}"
else
    echo -e "${RED}✗ First difference found at line $FIRST_DIFF${NC}"
    echo "  Compare lines around $FIRST_DIFF to identify the issue"
fi
