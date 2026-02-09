#!/usr/bin/env bash
# Interactive Divergence Fix Workflow
# Helps identify and fix divergences systematically
# Usage: ./scripts/fix_divergence.sh [max_steps] [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

MAX_STEPS="${1:-15}"
SYSOUT_FILE="${2:-medley/internal/loadups/starter.sysout}"

echo "=== INTERACTIVE DIVERGENCE FIX WORKFLOW ==="
echo "Max steps: $MAX_STEPS"
echo "Sysout: $SYSOUT_FILE"
echo ""

# Step 1: Run parity check
echo "Step 1: Running parity check..."
if "$SCRIPT_DIR/check_parity.sh" "$MAX_STEPS" "$SYSOUT_FILE"; then
    echo ""
    echo "✅ Parity achieved! No divergences found."
    exit 0
fi

echo ""
echo "Step 2: Analyzing first divergence..."

# Extract first divergence from analysis
FIRST_DIV_LINE=$(awk -f "$SCRIPT_DIR/analyze_trace_divergence.awk" \
    "$REPO_ROOT/c_emulator_execution_log.txt" \
    "$REPO_ROOT/zig_emulator_execution_log.txt" 2>/dev/null | \
    grep "^Line:" | head -1 | awk '{print $2}')

FIRST_DIV_FIELD=$(awk -f "$SCRIPT_DIR/analyze_trace_divergence.awk" \
    "$REPO_ROOT/c_emulator_execution_log.txt" \
    "$REPO_ROOT/zig_emulator_execution_log.txt" 2>/dev/null | \
    grep "^Field:" | head -1 | awk '{print $2}')

if [ -z "$FIRST_DIV_LINE" ] || [ "$FIRST_DIV_LINE" = "-1" ]; then
    echo "Could not determine first divergence line"
    exit 1
fi

echo "First divergence at line: $FIRST_DIV_LINE"
echo "First divergence field: $FIRST_DIV_FIELD"
echo ""

# Step 3: Show context around divergence
echo "Step 3: Showing context around divergence..."
echo ""
echo "--- C Trace (lines $((FIRST_DIV_LINE - 2))-$((FIRST_DIV_LINE + 2))) ---"
sed -n "$((FIRST_DIV_LINE - 1)),$((FIRST_DIV_LINE + 3))p" "$REPO_ROOT/c_emulator_execution_log.txt" 2>/dev/null || true
echo ""
echo "--- Zig Trace (lines $((FIRST_DIV_LINE - 2))-$((FIRST_DIV_LINE + 2))) ---"
sed -n "$((FIRST_DIV_LINE - 1)),$((FIRST_DIV_LINE + 3))p" "$REPO_ROOT/zig_emulator_execution_log.txt" 2>/dev/null || true
echo ""

# Step 4: Extract specific field values
echo "Step 4: Field values at divergence..."
C_LINE=$(sed -n "${FIRST_DIV_LINE}p" "$REPO_ROOT/c_emulator_execution_log.txt" 2>/dev/null)
ZIG_LINE=$(sed -n "${FIRST_DIV_LINE}p" "$REPO_ROOT/zig_emulator_execution_log.txt" 2>/dev/null)

if [ -n "$C_LINE" ] && [ -n "$ZIG_LINE" ]; then
    # Parse the specific field
    FIELD_NUM=$(echo "$FIRST_DIV_FIELD" | cut -d'.' -f1)
    SUB_FIELD=$(echo "$FIRST_DIV_FIELD" | cut -d'.' -f2-)
    
    case "$FIELD_NUM" in
        "REGISTERS")
            FIELD_INDEX=6
            ;;
        "FLAGS")
            FIELD_INDEX=7
            ;;
        "SP_FP")
            FIELD_INDEX=8
            ;;
        "STACK")
            FIELD_INDEX=9
            ;;
        "MEMORY")
            FIELD_INDEX=10
            ;;
        "MAPPING")
            FIELD_INDEX=11
            ;;
        *)
            FIELD_INDEX=0
            ;;
    esac
    
    if [ "$FIELD_INDEX" -gt 0 ]; then
        C_FIELD=$(echo "$C_LINE" | awk -F'|' "{print \$$FIELD_INDEX}")
        ZIG_FIELD=$(echo "$ZIG_LINE" | awk -F'|' "{print \$$FIELD_INDEX}")
        
        echo "C value:   $C_FIELD"
        echo "Zig value: $ZIG_FIELD"
        echo ""
        
        # Extract sub-field if applicable
        if [ -n "$SUB_FIELD" ] && [ "$SUB_FIELD" != "$FIELD_NUM" ]; then
            C_SUB=$(echo "$C_FIELD" | awk -F',' "{for(i=1;i<=NF;i++){if(\$i ~ /^$SUB_FIELD:/){print \$i}}}")
            ZIG_SUB=$(echo "$ZIG_FIELD" | awk -F',' "{for(i=1;i<=NF;i++){if(\$i ~ /^$SUB_FIELD:/){print \$i}}}")
            echo "Sub-field $SUB_FIELD:"
            echo "  C:   $C_SUB"
            echo "  Zig: $ZIG_SUB"
            echo ""
        fi
    fi
fi

# Step 5: Suggest relevant files
echo "Step 5: Suggested files to investigate..."
case "$FIRST_DIV_FIELD" in
    "STACK.TOS"|"STACK.N1"|"STACK.N2")
        echo "  - zaiko/src/vm/stack.zig (stack operations)"
        echo "  - zaiko/src/vm/dispatch/dispatch_loop.zig (TOS synchronization)"
        echo "  - maiko/src/xc.c (C reference)"
        ;;
    "SP_FP.SP"|"SP_FP.FP")
        echo "  - zaiko/src/vm/vm_initialization.zig (initialization)"
        echo "  - zaiko/src/vm/stack.zig (stack pointer management)"
        echo "  - maiko/src/main.c (C reference)"
        ;;
    "REGISTERS."*)
        echo "  - zaiko/src/vm/execution_trace.zig (register logging)"
        echo "  - maiko/src/execution_trace.c (C reference)"
        ;;
    "MEMORY."*)
        echo "  - zaiko/src/data/sysout.zig (memory loading)"
        echo "  - zaiko/src/utils/memory_access.zig (memory access)"
        echo "  - maiko/src/main.c (C reference)"
        ;;
    *)
        echo "  - Check trace logging in zaiko/src/vm/execution_trace.zig"
        echo "  - Compare with maiko/src/execution_trace.c"
        ;;
esac

echo ""
echo "=== NEXT STEPS ==="
echo "1. Review the divergence context above"
echo "2. Check the suggested files"
echo "3. Fix the issue in Zig emulator"
echo "4. Re-run: ./scripts/check_parity.sh $MAX_STEPS $SYSOUT_FILE"
echo "5. Repeat until parity is achieved"
echo ""
