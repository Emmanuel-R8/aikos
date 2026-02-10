#!/usr/bin/env bash
# Autonomous continuous parity check
# Runs parity checks at regular intervals without user intervention
# Usage: ./scripts/continuous_parity_check.sh [interval_seconds] [max_iterations]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

INTERVAL="${1:-300}"  # Default: 5 minutes
MAX_ITERATIONS="${2:-0}"  # 0 = unlimited
SYSOUT_FILE="${3:-medley/internal/loadups/starter.sysout}"
STEPS="${4:-100}"  # Default step count

# Create logs directory
LOGS_DIR="$REPO_ROOT/reports/parity/continuous"
mkdir -p "$LOGS_DIR"

# State file to track last run
STATE_FILE="$LOGS_DIR/state.json"

echo "=== CONTINUOUS PARITY CHECK ==="
echo "Interval: ${INTERVAL} seconds"
echo "Max iterations: ${MAX_ITERATIONS:-unlimited}"
echo "Sysout: $SYSOUT_FILE"
echo "Steps: $STEPS"
echo "Logs: $LOGS_DIR"
echo ""
echo "Press Ctrl+C to stop"
echo ""

ITERATION=0
LAST_DIVERGENCES=""

while true; do
    ITERATION=$((ITERATION + 1))

    if [ "$MAX_ITERATIONS" -gt 0 ] && [ "$ITERATION" -gt "$MAX_ITERATIONS" ]; then
        echo "Reached max iterations ($MAX_ITERATIONS)"
        break
    fi

    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    LOG_FILE="$LOGS_DIR/run_${TIMESTAMP}.log"
    JSON_FILE="$LOGS_DIR/run_${TIMESTAMP}.json"

    echo "[$ITERATION] $(date): Running parity check..."

    # Run comparison
    if bash "$SCRIPT_DIR/compare_all_implementations.sh" "$STEPS" "$SYSOUT_FILE" > "$LOG_FILE" 2>&1; then
        # Generate JSON report if trace files exist
        if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
            python3 "$SCRIPT_DIR/compare_multi_implementation.py" \
                --c-trace "$REPO_ROOT/c_emulator_execution_log.txt" \
                --zig-trace "$REPO_ROOT/zig_emulator_execution_log.txt" \
                --lisp-trace "$REPO_ROOT/lisp_emulator_execution_log.txt" \
                --max-lines "$STEPS" \
                --json > "$JSON_FILE" 2>&1 || true

            # Check for new divergences
            if [ -f "$JSON_FILE" ] && command -v python3 >/dev/null 2>&1; then
                CURRENT_DIVERGENCES=$(python3 -c "
import json
try:
    with open('$JSON_FILE') as f:
        data = json.load(f)
    divs = []
    for impl, comp in data.get('comparisons', {}).items():
        if comp.get('status') == 'diverges':
            div = comp.get('first_divergence', {})
            divs.append(f\"{impl}:step{div.get('step', 'N/A')}\")
    print('|'.join(divs))
except:
    pass
" 2>/dev/null || echo "")

                if [ "$CURRENT_DIVERGENCES" != "$LAST_DIVERGENCES" ]; then
                    echo "  ⚠️ Divergence status changed!"
                    echo "  Previous: ${LAST_DIVERGENCES:-none}"
                    echo "  Current: ${CURRENT_DIVERGENCES:-none}"
                    LAST_DIVERGENCES="$CURRENT_DIVERGENCES"
                else
                    echo "  ✓ No changes detected"
                fi
            fi
        fi
    else
        echo "  ✗ Comparison failed (check $LOG_FILE)"
    fi

    echo "  Next check in ${INTERVAL} seconds..."
    sleep "$INTERVAL"
done

echo ""
echo "Continuous parity check stopped"
