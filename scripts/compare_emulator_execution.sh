#!/usr/bin/env bash
# Script to run both C and Zig emulators for 1000 steps and compare execution logs
# Usage: ./scripts/compare_emulator_execution.sh [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

SYSOUT_FILE="${1:-medley/internal/loadups/starter.sysout}"
MAX_STEPS=1000
SYSOUT_PATH="$SYSOUT_FILE"

# Use an absolute sysout path so Zig runs correctly from any cwd.
if [[ "$SYSOUT_PATH" != /* ]]; then
    SYSOUT_PATH="$REPO_ROOT/$SYSOUT_PATH"
fi

echo "=== Comparing C and Zig Emulator Execution (${MAX_STEPS} steps) ==="
echo "Sysout file: $SYSOUT_FILE"
echo "Sysout path: $SYSOUT_PATH"
echo ""

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zaiko/zig_emulator_execution_log.txt"

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/build-cmake/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build-cmake/ldesdl"
elif [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
else
    echo "ERROR: C emulator (ldesdl) not found"
    echo "  Searched:"
    echo "    - $REPO_ROOT/maiko/build-cmake/ldesdl"
    echo "    - $REPO_ROOT/maiko/linux.x86_64/ldesdl"
    exit 1
fi

echo "Found C emulator: $C_EMULATOR"

# Check if Zig emulator can be built/run
ZIG_DIR="$REPO_ROOT/zaiko"
if [ ! -d "$ZIG_DIR" ]; then
    echo "ERROR: Zig emulator directory not found: $ZIG_DIR"
    exit 1
fi

echo ""
echo "=== Fast iteration cap ==="
export EMULATOR_MAX_STEPS="${EMULATOR_MAX_STEPS:-$MAX_STEPS}"
echo "Using EMULATOR_MAX_STEPS=${EMULATOR_MAX_STEPS} (unset or 0 => run to completion)"

# Run C emulator
echo ""
echo "=== Running C Emulator ==="
echo "Command: $C_EMULATOR $SYSOUT_FILE"
cd "$REPO_ROOT"
timeout --kill-after 11 10 "$C_EMULATOR" "$SYSOUT_FILE" > /dev/null 2>&1 || true

if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
    C_LINES=$(wc -l < "$REPO_ROOT/c_emulator_execution_log.txt")
    echo "✓ C emulator log created: $C_LINES lines"
    if [ "$C_LINES" -gt "$MAX_STEPS" ]; then
        echo "  WARNING: Log has more than ${MAX_STEPS} lines, truncating..."
        head -n "$MAX_STEPS" "$REPO_ROOT/c_emulator_execution_log.txt" > "$REPO_ROOT/c_emulator_execution_log_truncated.txt"
        mv "$REPO_ROOT/c_emulator_execution_log_truncated.txt" "$REPO_ROOT/c_emulator_execution_log.txt"
        C_LINES=$MAX_STEPS
    fi
else
    echo "✗ C emulator log not created"
    exit 1
fi

# Run Zig emulator
echo ""
echo "=== Running Zig Emulator ==="
echo "Command: cd zaiko && zig build run -- $SYSOUT_FILE"
cd "$ZIG_DIR"
timeout --kill-after 11 10 zig build run -- "$SYSOUT_PATH" > /dev/null 2>&1 || true

# Check for log in both possible locations
if [ -f "$ZIG_DIR/zig_emulator_execution_log.txt" ]; then
    cp "$ZIG_DIR/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt")
    echo "✓ Zig emulator log created: $ZIG_LINES lines"
    if [ "$ZIG_LINES" -gt "$MAX_STEPS" ]; then
        echo "  WARNING: Log has more than ${MAX_STEPS} lines, truncating..."
        head -n "$MAX_STEPS" "$REPO_ROOT/zig_emulator_execution_log.txt" > "$REPO_ROOT/zig_emulator_execution_log_truncated.txt"
        mv "$REPO_ROOT/zig_emulator_execution_log_truncated.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
        ZIG_LINES=$MAX_STEPS
    fi
elif [ -f "$ZIG_DIR/zaiko/zig_emulator_execution_log.txt" ]; then
    cp "$ZIG_DIR/zaiko/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt")
    echo "✓ Zig emulator log created (nested): $ZIG_LINES lines"
    if [ "$ZIG_LINES" -gt "$MAX_STEPS" ]; then
        echo "  WARNING: Log has more than ${MAX_STEPS} lines, truncating..."
        head -n "$MAX_STEPS" "$REPO_ROOT/zig_emulator_execution_log.txt" > "$REPO_ROOT/zig_emulator_execution_log_truncated.txt"
        mv "$REPO_ROOT/zig_emulator_execution_log_truncated.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
        ZIG_LINES=$MAX_STEPS
    fi
elif [ -f "$REPO_ROOT/zig_emulator_execution_log.txt" ]; then
    ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt")
    echo "✓ Zig emulator log found: $ZIG_LINES lines"
    if [ "$ZIG_LINES" -gt "$MAX_STEPS" ]; then
        echo "  WARNING: Log has more than ${MAX_STEPS} lines, truncating..."
        head -n "$MAX_STEPS" "$REPO_ROOT/zig_emulator_execution_log.txt" > "$REPO_ROOT/zig_emulator_execution_log_truncated.txt"
        mv "$REPO_ROOT/zig_emulator_execution_log_truncated.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
        ZIG_LINES=$MAX_STEPS
    fi
else
    echo "✗ Zig emulator log not created"
    exit 1
fi

# Compare logs
echo ""
echo "=== Comparing Execution Logs ==="
echo "C emulator log: $C_LINES lines"
echo "Zig emulator log: $ZIG_LINES lines"
echo ""

if [ "$C_LINES" -ne "$ZIG_LINES" ]; then
    echo "⚠️  WARNING: Logs have different line counts!"
    echo "  C:   $C_LINES lines"
    echo "  Zig: $ZIG_LINES lines"
    echo ""
fi

# Use diff to compare
if diff -q "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt" > /dev/null 2>&1; then
    echo "✅ SUCCESS: Logs are IDENTICAL!"
    echo ""
    echo "First 5 lines of both logs:"
    echo "--- C Emulator ---"
    head -5 "$REPO_ROOT/c_emulator_execution_log.txt"
    echo ""
    echo "--- Zig Emulator ---"
    head -5 "$REPO_ROOT/zig_emulator_execution_log.txt"
else
    echo "❌ DIFFERENCES FOUND: Logs are NOT identical"
    echo ""
    echo "First difference (showing 10 lines of context):"
    diff -u "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt" | head -20
    echo ""
    echo "Full diff saved to: $REPO_ROOT/emulator_execution_diff.txt"
    diff -u "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt" > "$REPO_ROOT/emulator_execution_diff.txt" || true

    # Count differences
    DIFF_COUNT=$(diff "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt" | grep -c "^[<>]" || echo "0")
    echo "Total differences: $DIFF_COUNT lines"
fi

echo ""
echo "=== Summary ==="
echo "C emulator log:   $REPO_ROOT/c_emulator_execution_log.txt ($C_LINES lines)"
echo "Zig emulator log: $REPO_ROOT/zig_emulator_execution_log.txt ($ZIG_LINES lines)"
if [ -f "$REPO_ROOT/emulator_execution_diff.txt" ]; then
    echo "Diff file:         $REPO_ROOT/emulator_execution_diff.txt"
fi
