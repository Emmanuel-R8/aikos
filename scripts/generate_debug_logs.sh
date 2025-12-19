#!/usr/bin/env bash
# Script to generate debug logs from both C and Zig emulators
# Usage: ./scripts/generate_debug_logs.sh [sysout_file]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

SYSOUT_FILE="${1:-medley/internal/loadups/starter.sysout}"

echo "=== Generating Debug Logs for Comparison ==="
echo "Sysout file: $SYSOUT_FILE"
echo ""

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
elif [ -f "$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
else
    echo "ERROR: C emulator (ldesdl) not found"
    exit 1
fi

echo "=== Running C Emulator ==="
echo "Command: $C_EMULATOR $SYSOUT_FILE"
cd "$REPO_ROOT"
timeout 5 "$C_EMULATOR" "$SYSOUT_FILE" > /dev/null 2>&1 || true

if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
    C_LINES=$(wc -l < "$REPO_ROOT/c_emulator_execution_log.txt")
    echo "✓ C emulator log created: $C_LINES lines"
else
    echo "✗ C emulator log not created"
fi

echo ""
echo "=== Running Zig Emulator ==="
echo "Command: cd zaiko && zig build run -- $SYSOUT_FILE"
cd "$REPO_ROOT/zaiko"
timeout 5 zig build run -- "../../../$SYSOUT_FILE" > /dev/null 2>&1 || true

# Check for log in both possible locations
if [ -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt" ]; then
    cp "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt")
    echo "✓ Zig emulator log created: $ZIG_LINES lines"
elif [ -f "$REPO_ROOT/zig_emulator_execution_log.txt" ]; then
    ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt")
    echo "✓ Zig emulator log found: $ZIG_LINES lines"
else
    echo "✗ Zig emulator log not created"
fi

echo ""
echo "=== Summary ==="
if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ] && [ -f "$REPO_ROOT/zig_emulator_execution_log.txt" ]; then
    echo "Both logs generated successfully!"
    echo ""
    echo "First 3 lines comparison:"
    echo "--- C Emulator ---"
    head -3 "$REPO_ROOT/c_emulator_execution_log.txt"
    echo ""
    echo "--- Zig Emulator ---"
    head -3 "$REPO_ROOT/zig_emulator_execution_log.txt"
else
    echo "One or both logs are missing"
fi
