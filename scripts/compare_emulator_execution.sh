#!/usr/bin/env bash
# Script to run Zig emulator (and optionally C emulator) for N steps and compare execution logs
# Usage: ./scripts/compare_emulator_execution.sh [--with-c] [sysout_file]
#   --with-c: Also run C emulator and compare logs (default: Zig only)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Parse arguments
RUN_C_EMULATOR=false
SYSOUT_FILE=""
for arg in "$@"; do
    case "$arg" in
        --with-c|--compare)
            RUN_C_EMULATOR=true
            ;;
        -*)
            echo "Unknown option: $arg"
            echo "Usage: $0 [--with-c] [sysout_file]"
            exit 1
            ;;
        *)
            SYSOUT_FILE="$arg"
            ;;
    esac
done

SYSOUT_FILE="${SYSOUT_FILE:-medley/internal/loadups/starter.sysout}"
MAX_STEPS=1000
SYSOUT_PATH="$SYSOUT_FILE"

# Use an absolute sysout path so Zig runs correctly from any cwd.
if [[ "$SYSOUT_PATH" != /* ]]; then
    SYSOUT_PATH="$REPO_ROOT/$SYSOUT_PATH"
fi

if [ "$RUN_C_EMULATOR" = true ]; then
    echo "=== Running C and Zig Emulator Execution (${MAX_STEPS} steps) ==="
else
    echo "=== Running Zig Emulator Execution (${MAX_STEPS} steps) ==="
fi
echo "Sysout file: $SYSOUT_FILE"
echo "Sysout path: $SYSOUT_PATH"
echo ""

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zaiko/zig_emulator_execution_log.txt"

# Find C emulator (only if --with-c)
C_EMULATOR=""
if [ "$RUN_C_EMULATOR" = true ]; then
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
fi

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

# Run C emulator (only if --with-c)
C_LINES=0
if [ "$RUN_C_EMULATOR" = true ]; then
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
fi

# Run Zig emulator (headless when no display; use cache in workspace)
echo ""
echo "=== Running Zig Emulator ==="
echo "Command: cd zaiko && ZIG_GLOBAL_CACHE_DIR=zaiko/.zig-cache zig build run -- --max-steps $EMULATOR_MAX_STEPS $SYSOUT_PATH"
cd "$ZIG_DIR"
# Force Zig cache; pass step cap via --max-steps so Zig stops at same line count as C
export ZIG_GLOBAL_CACHE_DIR="$ZIG_DIR/.zig-cache"
# When step cap is set, allow longer run (display/event polling can slow steps)
ZIG_TIMEOUT=30
if [ -n "${EMULATOR_MAX_STEPS}" ] && [ "${EMULATOR_MAX_STEPS}" -gt 0 ] 2>/dev/null; then
  ZIG_TIMEOUT=90
fi
timeout --kill-after $((ZIG_TIMEOUT + 1)) "$ZIG_TIMEOUT" zig build run -- --max-steps "$EMULATOR_MAX_STEPS" "$SYSOUT_PATH" > /dev/null 2>&1 || true

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
elif [ -f "$ZIG_DIR/zig-out/bin/zig_emulator_execution_log.txt" ]; then
    cp "$ZIG_DIR/zig-out/bin/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    ZIG_LINES=$(wc -l < "$REPO_ROOT/zig_emulator_execution_log.txt")
    echo "✓ Zig emulator log created (zig-out/bin): $ZIG_LINES lines"
    if [ "$ZIG_LINES" -gt "$MAX_STEPS" ]; then
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

# Compare logs (only if C emulator was run)
if [ "$RUN_C_EMULATOR" = true ]; then
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
fi

echo ""
echo "=== Summary ==="
if [ "$RUN_C_EMULATOR" = true ]; then
    echo "C emulator log:   $REPO_ROOT/c_emulator_execution_log.txt ($C_LINES lines)"
fi
echo "Zig emulator log: $REPO_ROOT/zig_emulator_execution_log.txt ($ZIG_LINES lines)"
if [ "$RUN_C_EMULATOR" = true ] && [ -f "$REPO_ROOT/emulator_execution_diff.txt" ]; then
    echo "Diff file:         $REPO_ROOT/emulator_execution_diff.txt"
fi
