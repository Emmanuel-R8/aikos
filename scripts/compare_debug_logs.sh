#!/usr/bin/env bash
# Script to compare debug logs from C and Zig emulators
# Usage: ./scripts/compare_debug_logs.sh [c_log] [zig_log]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

C_LOG="${1:-c_emulator_execution_log.txt}"
ZIG_LOG="${2:-zig_emulator_execution_log.txt}"

echo "=== Comparing Debug Logs ==="
echo "C Emulator log: $C_LOG"
echo "Zig Emulator log: $ZIG_LOG"
echo ""

if [ ! -f "$C_LOG" ]; then
    echo "ERROR: C emulator log not found: $C_LOG"
    exit 1
fi

if [ ! -f "$ZIG_LOG" ]; then
    echo "ERROR: Zig emulator log not found: $ZIG_LOG"
    exit 1
fi

C_LINES=$(wc -l < "$C_LOG")
ZIG_LINES=$(wc -l < "$ZIG_LOG")

echo "C log: $C_LINES lines"
echo "Zig log: $ZIG_LINES lines"
echo ""

echo "=== First 5 Lines Comparison ==="
echo ""
echo "--- C Emulator (first 5 lines) ---"
head -5 "$C_LOG"
echo ""
echo "--- Zig Emulator (first 5 lines) ---"
head -5 "$ZIG_LOG"
echo ""

echo "=== Field-by-Field Comparison (first line) ==="
C_FIRST=$(head -1 "$C_LOG")
ZIG_FIRST=$(head -1 "$ZIG_LOG")

echo ""
echo "C:   $C_FIRST"
echo "Zig: $ZIG_FIRST"
echo ""

# Extract key fields for comparison
echo "=== Key Field Extraction ==="
echo ""
echo "PC Address:"
echo "  C:   $(echo "$C_FIRST" | grep -oP 'PC: \K0x[0-9a-f]+')"
echo "  Zig: $(echo "$ZIG_FIRST" | grep -oP 'PC: \K0x[0-9a-f]+')"
echo ""
echo "FuncObj Offset:"
echo "  C:   $(echo "$C_FIRST" | grep -oP 'FuncObj\+\K[^ ]+')"
echo "  Zig: $(echo "$ZIG_FIRST" | grep -oP 'FuncObj\+\K[^ ]+')"
echo ""
echo "Instruction Bytes:"
echo "  C:   $(echo "$C_FIRST" | grep -oP 'PC:.*?\K[0-9a-f]{16}(?=\s)')"
echo "  Zig: $(echo "$ZIG_FIRST" | grep -oP 'PC:.*?\K[0-9a-f]{16}(?=\s)')"
echo ""
echo "Opcode Name:"
echo "  C:   $(echo "$C_FIRST" | grep -oP '\s\K[A-Z_]+(?=\s+Stack)')"
echo "  Zig: $(echo "$ZIG_FIRST" | grep -oP '\s\K[A-Z_]+(?=\s+Stack)')"
echo ""
echo "Stack Depth:"
echo "  C:   $(echo "$C_FIRST" | grep -oP 'Stack: D:\s*\K[0-9]+')"
echo "  Zig: $(echo "$ZIG_FIRST" | grep -oP 'Stack: D:\s*\K[0-9]+')"
echo ""
echo "Frame Header (FH):"
echo "  C:   $(echo "$C_FIRST" | grep -oP 'FH:\K0x[0-9a-f]+')"
echo "  Zig: $(echo "$ZIG_FIRST" | grep -oP 'FH:\K0x[0-9a-f]+')"
echo ""

echo "=== Summary ==="
if [ "$C_FIRST" = "$ZIG_FIRST" ]; then
    echo "✓ First lines match exactly!"
else
    echo "✗ First lines differ - see field-by-field comparison above"
fi
