#!/usr/bin/env bash
# Automated Parity Check Script
# Runs both emulators, compares traces, and reports parity status
# Usage: ./scripts/check_parity.sh [max_steps] [sysout_file]

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

echo "=== AUTOMATED PARITY CHECK ==="
echo "Max steps: $MAX_STEPS"
echo "Sysout: $SYSOUT_FILE"
echo ""

# Export for emulators
export EMULATOR_MAX_STEPS="$MAX_STEPS"

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/build-cmake/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build-cmake/ldesdl"
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

# Run Zig emulator
echo "Running Zig emulator..."
ZIG_DIR="$REPO_ROOT/zaiko"
cd "$ZIG_DIR"
export ZIG_GLOBAL_CACHE_DIR="$ZIG_DIR/.zig-cache"
timeout --kill-after 31 30 zig build run -- --max-steps "$MAX_STEPS" "$SYSOUT_PATH" > /dev/null 2>&1 || true

# Find Zig log
ZIG_LOG=""
if [ -f "$ZIG_DIR/zig_emulator_execution_log.txt" ]; then
    cp "$ZIG_DIR/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    ZIG_LOG="$REPO_ROOT/zig_emulator_execution_log.txt"
elif [ -f "$ZIG_DIR/zig-out/bin/zig_emulator_execution_log.txt" ]; then
    cp "$ZIG_DIR/zig-out/bin/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    ZIG_LOG="$REPO_ROOT/zig_emulator_execution_log.txt"
else
    echo "ERROR: Zig emulator log not created"
    exit 1
fi

ZIG_LINES=$(wc -l < "$ZIG_LOG")
echo "✓ Zig emulator: $ZIG_LINES lines"
echo ""

# Compare traces
echo "=== COMPARISON RESULTS ==="
cd "$REPO_ROOT"

# Use Python comparison script
if command -v python3 &> /dev/null; then
    python3 "$SCRIPT_DIR/compare_unified_traces.py" \
        "$REPO_ROOT/c_emulator_execution_log.txt" \
        "$REPO_ROOT/zig_emulator_execution_log.txt" \
        --max-lines "$MAX_STEPS" > /tmp/parity_comparison.txt 2>&1 || true
    
    cat /tmp/parity_comparison.txt
    
    # Check for differences
    if grep -q "No differences found" /tmp/parity_comparison.txt; then
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
            "$REPO_ROOT/zig_emulator_execution_log.txt" || true
        
        exit 1
    fi
else
    echo "ERROR: python3 not found"
    exit 1
fi
