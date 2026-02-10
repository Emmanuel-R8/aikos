#!/usr/bin/env bash
# Compare all four emulator implementations (C, Zig, TypeScript, Lisp)
# Runs all emulators with same sysout and step count, collects traces, compares systematically
# Usage: ./scripts/compare_all_implementations.sh [max_steps] [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

MAX_STEPS="${1:-100}"
SYSOUT_FILE="${2:-medley/internal/loadups/starter.sysout}"

# Use absolute sysout path
if [[ "$SYSOUT_FILE" != /* ]]; then
    SYSOUT_PATH="$REPO_ROOT/$SYSOUT_FILE"
else
    SYSOUT_PATH="$SYSOUT_FILE"
fi

echo "=== COMPARING ALL IMPLEMENTATIONS ==="
echo "Max steps: $MAX_STEPS"
echo "Sysout: $SYSOUT_FILE"
echo "Sysout path: $SYSOUT_PATH"
echo ""

# Export for emulators
export EMULATOR_MAX_STEPS="$MAX_STEPS"

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/typescript_emulator_execution_log.txt"
rm -f "$REPO_ROOT/lisp_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/laiko/lisp_emulator_execution_log.txt"

# Results tracking
declare -A TRACE_FILES
declare -A TRACE_STATUS

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/build-cmake/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build-cmake/ldesdl"
elif [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
elif [ -f "$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
fi

# Run C emulator (reference)
echo "--- Running C emulator (reference) ---"
if [ -n "$C_EMULATOR" ]; then
    cd "$REPO_ROOT"
    timeout --kill-after 11 10 "$C_EMULATOR" "$SYSOUT_PATH" > /dev/null 2>&1 || true
    if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
        C_LINES=$(wc -l < "$REPO_ROOT/c_emulator_execution_log.txt" 2>/dev/null || echo "0")
        TRACE_FILES["C"]="$REPO_ROOT/c_emulator_execution_log.txt"
        TRACE_STATUS["C"]="OK ($C_LINES lines)"
        echo "✓ C emulator: $C_LINES lines"
    else
        TRACE_STATUS["C"]="FAILED (no trace file)"
        echo "✗ C emulator: No trace file generated"
    fi
else
    TRACE_STATUS["C"]="SKIPPED (not found)"
    echo "✗ C emulator: Not found"
fi

# Run Zig emulator
echo "--- Running Zig emulator ---"
ZIG_DIR="$REPO_ROOT/zaiko"
if [ -d "$ZIG_DIR" ] && command -v zig >/dev/null 2>&1; then
    cd "$ZIG_DIR"
    ZIG_GLOBAL_CACHE_DIR=/tmp/zig-cache zig build run -- "$SYSOUT_PATH" > /dev/null 2>&1 || true
    # Check for trace file in multiple locations
    if [ -f "$REPO_ROOT/zig_emulator_execution_log.txt" ]; then
        ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt" 2>/dev/null || echo "0")
        TRACE_FILES["Zig"]="$REPO_ROOT/zig_emulator_execution_log.txt"
        TRACE_STATUS["Zig"]="OK ($ZIG_LINES lines)"
        echo "✓ Zig emulator: $ZIG_LINES lines"
    elif [ -f "$ZIG_DIR/zig_emulator_execution_log.txt" ]; then
        cp "$ZIG_DIR/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
        ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt" 2>/dev/null || echo "0")
        TRACE_FILES["Zig"]="$REPO_ROOT/zig_emulator_execution_log.txt"
        TRACE_STATUS["Zig"]="OK ($ZIG_LINES lines)"
        echo "✓ Zig emulator: $ZIG_LINES lines"
    else
        TRACE_STATUS["Zig"]="FAILED (no trace file)"
        echo "✗ Zig emulator: No trace file generated"
    fi
else
    TRACE_STATUS["Zig"]="SKIPPED (not found or zig not available)"
    echo "✗ Zig emulator: Not found or zig not available"
fi

# Run TypeScript emulator
echo "--- Running TypeScript emulator ---"
TS_DIR="$REPO_ROOT/taiko"
if [ -d "$TS_DIR" ] && [ -f "$TS_DIR/src/vm/trace.ts" ]; then
    # TypeScript emulator may need Node.js/browser environment
    # For now, check if trace module exists
    TRACE_STATUS["TypeScript"]="SKIPPED (requires Node.js/browser environment)"
    echo "✗ TypeScript emulator: Requires Node.js/browser environment (not automated yet)"
else
    TRACE_STATUS["TypeScript"]="SKIPPED (not found)"
    echo "✗ TypeScript emulator: Not found"
fi

# Run Lisp emulator
echo "--- Running Lisp emulator ---"
LISP_DIR="$REPO_ROOT/laiko"
if [ -d "$LISP_DIR" ] && command -v sbcl >/dev/null 2>&1; then
    cd "$LISP_DIR"
    bash run.sh "$SYSOUT_PATH" > /dev/null 2>&1 || true
    # Check for trace file in multiple locations
    if [ -f "$REPO_ROOT/lisp_emulator_execution_log.txt" ]; then
        LISP_LINES=$(wc -l < "$REPO_ROOT/lisp_emulator_execution_log.txt" 2>/dev/null || echo "0")
        TRACE_FILES["Lisp"]="$REPO_ROOT/lisp_emulator_execution_log.txt"
        TRACE_STATUS["Lisp"]="OK ($LISP_LINES lines)"
        echo "✓ Lisp emulator: $LISP_LINES lines"
    elif [ -f "$LISP_DIR/lisp_emulator_execution_log.txt" ]; then
        cp "$LISP_DIR/lisp_emulator_execution_log.txt" "$REPO_ROOT/lisp_emulator_execution_log.txt"
        LISP_LINES=$(wc -l < "$REPO_ROOT/lisp_emulator_execution_log.txt" 2>/dev/null || echo "0")
        TRACE_FILES["Lisp"]="$REPO_ROOT/lisp_emulator_execution_log.txt"
        TRACE_STATUS["Lisp"]="OK ($LISP_LINES lines)"
        echo "✓ Lisp emulator: $LISP_LINES lines"
    else
        TRACE_STATUS["Lisp"]="FAILED (no trace file)"
        echo "✗ Lisp emulator: No trace file generated"
    fi
else
    TRACE_STATUS["Lisp"]="SKIPPED (not found or sbcl not available)"
    echo "✗ Lisp emulator: Not found or sbcl not available"
fi

echo ""
echo "=== TRACE GENERATION SUMMARY ==="
for impl in C Zig TypeScript Lisp; do
    echo "$impl: ${TRACE_STATUS[$impl]}"
done

echo ""
echo "=== COMPARISON ==="
cd "$REPO_ROOT"

# Compare each implementation against C reference
if [ -n "${TRACE_FILES[C]:-}" ] && [ -f "${TRACE_FILES[C]}" ]; then
    for impl in Zig TypeScript Lisp; do
        if [ -n "${TRACE_FILES[$impl]:-}" ] && [ -f "${TRACE_FILES[$impl]}" ]; then
            echo ""
            echo "--- Comparing C vs $impl ---"
            if command -v python3 >/dev/null 2>&1; then
                python3 "$SCRIPT_DIR/compare_unified_traces.py" \
                    "${TRACE_FILES[C]}" \
                    "${TRACE_FILES[$impl]}" \
                    --max-lines "$MAX_STEPS" 2>&1 | head -20 || true
            else
                echo "Python3 not available for comparison"
            fi
        fi
    done
else
    echo "C reference trace not available for comparison"
fi

echo ""
echo "=== COMPARISON COMPLETE ==="
