#!/usr/bin/env bash
# Autonomous parity test runner
# Runs comparisons with different step counts and logs results
# Usage: ./scripts/run_parity_suite.sh [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

SYSOUT_FILE="${1:-medley/internal/loadups/starter.sysout}"

# Create reports directory
REPORTS_DIR="$REPO_ROOT/reports/parity"
mkdir -p "$REPORTS_DIR"

# Timestamp for this run
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
RUN_DIR="$REPORTS_DIR/run_$TIMESTAMP"
mkdir -p "$RUN_DIR"

echo "=== PARITY TEST SUITE ==="
echo "Timestamp: $TIMESTAMP"
echo "Reports directory: $RUN_DIR"
echo "Sysout: $SYSOUT_FILE"
echo ""

# Step counts to test
STEP_COUNTS=(10 100 1000 10000)

# Summary file
SUMMARY_FILE="$RUN_DIR/summary.txt"
echo "Parity Test Suite Results - $TIMESTAMP" > "$SUMMARY_FILE"
echo "========================================" >> "$SUMMARY_FILE"
echo "" >> "$SUMMARY_FILE"

for STEPS in "${STEP_COUNTS[@]}"; do
    echo "--- Testing with $STEPS steps ---"
    echo "Testing $STEPS steps..." >> "$SUMMARY_FILE"

    # Run comparison
    LOG_FILE="$RUN_DIR/steps_${STEPS}.log"
    JSON_FILE="$RUN_DIR/steps_${STEPS}.json"
    TYPST_FILE="$RUN_DIR/steps_${STEPS}.typ"

    if bash "$SCRIPT_DIR/compare_all_implementations.sh" "$STEPS" "$SYSOUT_FILE" > "$LOG_FILE" 2>&1; then
        echo "  ✓ Comparison completed"

        # Generate JSON and Typst reports if trace files exist
        if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
            python3 "$SCRIPT_DIR/compare_multi_implementation.py" \
                --c-trace "$REPO_ROOT/c_emulator_execution_log.txt" \
                --zig-trace "$REPO_ROOT/zig_emulator_execution_log.txt" \
                --lisp-trace "$REPO_ROOT/lisp_emulator_execution_log.txt" \
                --max-lines "$STEPS" \
                --json > "$JSON_FILE" 2>&1 || true

            python3 "$SCRIPT_DIR/compare_multi_implementation.py" \
                --c-trace "$REPO_ROOT/c_emulator_execution_log.txt" \
                --zig-trace "$REPO_ROOT/zig_emulator_execution_log.txt" \
                --lisp-trace "$REPO_ROOT/lisp_emulator_execution_log.txt" \
                --max-lines "$STEPS" \
                --typst > "$TYPST_FILE" 2>&1 || true

            # Extract summary from JSON
            if [ -f "$JSON_FILE" ] && command -v python3 >/dev/null 2>&1; then
                python3 -c "
import json
try:
    with open('$JSON_FILE') as f:
        data = json.load(f)
    summary = data.get('summary', {})
    print(f\"  Matching: {summary.get('matching', 0)}/{summary.get('total_implementations', 0)}\")
    print(f\"  Diverging: {summary.get('diverging', 0)}/{summary.get('total_implementations', 0)}\")
    print(f\"  Missing: {summary.get('missing_or_empty', 0)}/{summary.get('total_implementations', 0)}\")
except:
    pass
" >> "$SUMMARY_FILE" 2>&1 || true
            fi
        fi
    else
        echo "  ✗ Comparison failed (check $LOG_FILE)"
        echo "  FAILED" >> "$SUMMARY_FILE"
    fi

    echo "" >> "$SUMMARY_FILE"
done

echo ""
echo "=== SUMMARY ==="
cat "$SUMMARY_FILE"
echo ""
echo "Full results in: $RUN_DIR"
