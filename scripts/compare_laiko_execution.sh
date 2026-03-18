#!/usr/bin/env bash
# Script to run C and Laiko emulators for N steps and compare execution logs
# Usage: ./scripts/compare_laiko_execution.sh [max_steps] [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Defaults
MAX_STEPS="${1:-20}"
SYSOUT_FILE="${2:-medley/internal/loadups/starter.sysout}"
SYSOUT_PATH="$REPO_ROOT/$SYSOUT_FILE"

echo "=== Comparing Laiko and C Emulator Execution (${MAX_STEPS} steps) ==="
echo "Sysout: $SYSOUT_PATH"

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt"
rm -f "$REPO_ROOT/lisp_emulator_execution_log.txt"
rm -f "$REPO_ROOT/laiko_unified_trace.txt"
rm -f "$REPO_ROOT/c_emulator_unified_trace.txt"

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/bin/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/bin/ldesdl"
elif [ -f "$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
elif [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
else
    echo "ERROR: C emulator (ldesdl) not found. Build it first."
    exit 1
fi
echo "C Emulator: $C_EMULATOR"

# Set max steps
export EMULATOR_MAX_STEPS="$MAX_STEPS"

# Run C emulator
echo ""
echo "=== Running C Emulator ==="
# Note: C emulator typically creates log in current directory as c_emulator_execution_log.txt
# We pipe output to /dev/null but allow stderr
timeout --signal=KILL 10 "$C_EMULATOR" "$SYSOUT_PATH" >/dev/null 2>&1 || true

if [ -f "c_emulator_execution_log.txt" ]; then
    mv "c_emulator_execution_log.txt" "c_emulator_unified_trace.txt"
    C_LINES=$(wc -l <"c_emulator_unified_trace.txt")
    echo "✓ C trace generated: $C_LINES lines"
else
    echo "✗ C trace not found!"
    exit 1
fi

# Run Laiko
echo ""
echo "=== Running Laiko Emulator ==="
LAIKO_RUN="$REPO_ROOT/laiko/run.sh"
# Laiko typically writes to lisp_emulator_execution_log.txt in CWD
timeout --signal=KILL 30 "$LAIKO_RUN" "$SYSOUT_PATH" >/dev/null 2>&1 || true

if [ -f "lisp_emulator_execution_log.txt" ]; then
    mv "lisp_emulator_execution_log.txt" "laiko_unified_trace.txt"
    LAIKO_LINES=$(wc -l <"laiko_unified_trace.txt")
    echo "✓ Laiko trace generated: $LAIKO_LINES lines"
elif [ -f "laiko/lisp_emulator_execution_log.txt" ]; then
    mv "laiko/lisp_emulator_execution_log.txt" "laiko_unified_trace.txt"
    LAIKO_LINES=$(wc -l <"laiko_unified_trace.txt")
    echo "✓ Laiko trace found in subdir: $LAIKO_LINES lines"
else
    echo "✗ Laiko trace not found!"
    exit 1
fi

# Compare
echo ""
echo "=== Comparison ==="
if diff -q "c_emulator_unified_trace.txt" "laiko_unified_trace.txt" >/dev/null 2>&1; then
    echo "✅ SUCCESS: Traces are IDENTICAL!"
else
    echo "❌ DIFFERENCE DETECTED"
    echo "First 10 differences:"
    diff -u "c_emulator_unified_trace.txt" "laiko_unified_trace.txt" | head -n 20
    
    echo ""
    echo "Running analysis script..."
    if [ -f "scripts/analyze_execution_divergence.py" ]; then
        python3 scripts/analyze_execution_divergence.py \
            "c_emulator_unified_trace.txt" \
            "laiko_unified_trace.txt"
    fi
fi
