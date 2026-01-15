#!/usr/bin/env bash
# Script to generate debug logs from both C and Zig emulators
# Usage:
#   ./scripts/generate_debug_logs.sh [sysout_file]
#   ./scripts/generate_debug_logs.sh --staged   # starter.sysout then full.sysout

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

arg1="${1:-}"

run_one() {
    local sysout_file="$1"
    local tag="$2"
    local sysout_path="$sysout_file"

    # Use an absolute sysout path so Zig runs correctly from any cwd.
    if [[ "$sysout_path" != /* ]]; then
        sysout_path="$REPO_ROOT/$sysout_path"
    fi

    echo "=== Generating Debug Logs for Comparison ==="
    echo "Sysout file: $sysout_file"
    echo "Sysout path: $sysout_path"
    echo "Tag: $tag"
    echo ""

    # Clean old logs (canonical names)
    rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"

    # Find C emulator (linux default paths)
    local c_emulator=""
    if [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
        c_emulator="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
    elif [ -f "$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl" ]; then
        c_emulator="$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
    else
        echo "ERROR: C emulator (ldesdl) not found" >&2
        return 1
    fi

    echo "=== Running C Emulator ==="
    echo "Command: $c_emulator $sysout_file"
    cd "$REPO_ROOT"
    timeout 5 "$c_emulator" "$sysout_file" > /dev/null 2>&1 || true

    if [ -f "$REPO_ROOT/c_emulator_execution_log.txt" ]; then
        local c_out="$REPO_ROOT/c_emulator_execution_log.${tag}.txt"
        cp "$REPO_ROOT/c_emulator_execution_log.txt" "$c_out"
        local c_lines
        c_lines=$(wc -l < "$c_out")
        echo "✓ C emulator log created: $c_lines lines ($c_out)"
    else
        echo "✗ C emulator log not created" >&2
    fi

    echo ""
    echo "=== Running Zig Emulator ==="
    echo "Command: cd zaiko && zig build run -- $sysout_path"
    cd "$REPO_ROOT/zaiko"
    timeout 5 zig build run -- "$sysout_path" > /dev/null 2>&1 || true

    # Normalize Zig log location and persist a tagged copy
    if [ -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt" ]; then
        cp "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    elif [ -f "$REPO_ROOT/zaiko/zaiko/zig_emulator_execution_log.txt" ]; then
        # Legacy/alternate output location (nested project dir)
        cp "$REPO_ROOT/zaiko/zaiko/zig_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
    fi

    if [ -f "$REPO_ROOT/zig_emulator_execution_log.txt" ]; then
        local z_out="$REPO_ROOT/zig_emulator_execution_log.${tag}.txt"
        cp "$REPO_ROOT/zig_emulator_execution_log.txt" "$z_out"
        local z_lines
        z_lines=$(wc -l < "$z_out")
        echo "✓ Zig emulator log created: $z_lines lines ($z_out)"
    else
        echo "✗ Zig emulator log not created" >&2
    fi

    echo ""
    echo "=== Summary ($tag) ==="
    if [ -f "$REPO_ROOT/c_emulator_execution_log.${tag}.txt" ] && [ -f "$REPO_ROOT/zig_emulator_execution_log.${tag}.txt" ]; then
        echo "Both logs generated successfully!"
        echo ""
        echo "First 3 lines comparison:"
        echo "--- C Emulator ---"
        head -3 "$REPO_ROOT/c_emulator_execution_log.${tag}.txt"
        echo ""
        echo "--- Zig Emulator ---"
        head -3 "$REPO_ROOT/zig_emulator_execution_log.${tag}.txt"
    else
        echo "One or both logs are missing for tag: $tag" >&2
    fi
}

if [ "$arg1" = "--staged" ] || [ "$arg1" = "--stage" ]; then
    run_one "medley/internal/loadups/starter.sysout" "starter"
    echo ""
    echo "------------------------------------------------------------"
    echo ""
    run_one "medley/loadups/full.sysout" "full"
    exit 0
fi

SYSOUT_FILE="${arg1:-medley/internal/loadups/starter.sysout}"
tag="$(basename "$SYSOUT_FILE")"
tag="${tag%.sysout}"

run_one "$SYSOUT_FILE" "$tag"
