#!/usr/bin/env bash
# Automated Parity Check Script for Laiko (Common Lisp)
# Runs C and Laiko emulators, compares traces, and reports parity status
# Usage: ./scripts/check_parity_laiko.sh --max-steps N [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Parse named arguments
MAX_STEPS=100
SYSOUT_FILE="medley/internal/loadups/starter.sysout"

while [[ $# -gt 0 ]]; do
    case $1 in
        --max-steps)
            MAX_STEPS="$2"
            shift 2
            ;;
        --sysout)
            SYSOUT_FILE="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 --max-steps N [--sysout PATH]"
            exit 1
            ;;
    esac
done

# Use absolute sysout path
if [[ "$SYSOUT_FILE" != /* ]]; then
    SYSOUT_PATH="$REPO_ROOT/$SYSOUT_FILE"
else
    SYSOUT_PATH="$SYSOUT_FILE"
fi

echo "=== AUTOMATED PARITY CHECK (Laiko) ==="
echo "Max steps: $MAX_STEPS"
echo "Sysout: $SYSOUT_FILE"
echo ""

# Export for emulators
export EMULATOR_MAX_STEPS="$MAX_STEPS"

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/lisp_emulator_execution_log.txt"

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
elif [ -f "$REPO_ROOT/maiko/build-cmake/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build-cmake/ldesdl"
elif [ -f "$REPO_ROOT/maiko/bin/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/bin/ldesdl"
elif [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
else
    echo "ERROR: C emulator (ldesdl) not found"
    exit 1
fi

# Run C emulator
echo "Running C emulator..."
cd "$REPO_ROOT"
timeout --kill-after 11 10 "$C_EMULATOR" "$SYSOUT_FILE" > /dev/null 2>&1 || true

if [ ! -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
    echo "ERROR: C emulator log not created"
    exit 1
fi

C_LINES=$(wc -l < "$REPO_ROOT/c_emulator_execution_log.txt")
echo "✓ C emulator: $C_LINES lines"

# Run Laiko emulator
echo "Running Laiko emulator..."
LAIKO_DIR="$REPO_ROOT/laiko"
cd "$REPO_ROOT"  # Run from repo root so trace file goes to correct location
timeout --kill-after 31 30 bash "$LAIKO_DIR/run.sh" "$SYSOUT_PATH" > /dev/null 2>&1 || true

# Find Laiko log (may be in laiko dir or repo root)
LAIKO_LOG=""
if [ -f "$REPO_ROOT/lisp_emulator_execution_log.txt" ]; then
    LAIKO_LOG="$REPO_ROOT/lisp_emulator_execution_log.txt"
elif [ -f "$LAIKO_DIR/lisp_emulator_execution_log.txt" ]; then
    cp "$LAIKO_DIR/lisp_emulator_execution_log.txt" "$REPO_ROOT/lisp_emulator_execution_log.txt"
    LAIKO_LOG="$REPO_ROOT/lisp_emulator_execution_log.txt"
else
    echo "ERROR: Laiko emulator log not created"
    echo "Checked: $REPO_ROOT/lisp_emulator_execution_log.txt"
    echo "Checked: $LAIKO_DIR/lisp_emulator_execution_log.txt"
    exit 1
fi

LAIKO_LINES=$(wc -l < "$LAIKO_LOG")
echo "✓ Laiko emulator: $LAIKO_LINES lines"
echo ""

# Compare traces
echo "=== COMPARISON RESULTS ==="
cd "$REPO_ROOT"

# Use Python comparison script
if command -v python3 &> /dev/null; then
    python3 "$SCRIPT_DIR/compare_unified_traces.py" \
        "$REPO_ROOT/c_emulator_execution_log.txt" \
        "$REPO_ROOT/lisp_emulator_execution_log.txt" \
        --max-lines "$MAX_STEPS" > /tmp/parity_comparison_laiko.txt 2>&1 || true

    cat /tmp/parity_comparison_laiko.txt

    # Check for differences
    if grep -q "No differences found" /tmp/parity_comparison_laiko.txt; then
        echo ""
        echo "✅ PARITY: PASSED"
        exit 0
    else
        echo ""
        echo "❌ PARITY: FAILED"

        # Run divergence analysis
        echo ""
        echo "=== DIVERGENCE ANALYSIS ==="
        awk -f "$SCRIPT_DIR/analyze_trace_divergence.awk" \
            "$REPO_ROOT/c_emulator_execution_log.txt" \
            "$REPO_ROOT/lisp_emulator_execution_log.txt" || true

        exit 1
    fi
else
    echo "ERROR: python3 not found"
    exit 1
fi
