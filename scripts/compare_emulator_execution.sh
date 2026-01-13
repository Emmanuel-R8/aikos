#!/usr/bin/env bash
# Script to run both C and Zig emulators for 1000 steps and compare execution logs
# Usage: ./scripts/compare_emulator_execution.sh [sysout_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

SYSOUT_FILE="${1:-medley/internal/loadups/starter.sysout}"
MAX_STEPS=1000

echo "=== Comparing C and Zig Emulator Execution (${MAX_STEPS} steps) ==="
echo "Sysout file: $SYSOUT_FILE"
echo ""

# Clean old logs
rm -f "$REPO_ROOT/c_emulator_execution_log.txt" "$REPO_ROOT/zig_emulator_execution_log.txt"
rm -f "$REPO_ROOT/zaiko/zig_emulator_execution_log.txt"

# Find C emulator
C_EMULATOR=""
if [ -f "$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
elif [ -f "$REPO_ROOT/maiko/linux.x86_64/ldesdl" ]; then
    C_EMULATOR="$REPO_ROOT/maiko/linux.x86_64/ldesdl"
else
    echo "ERROR: C emulator (ldesdl) not found"
    echo "  Searched:"
    echo "    - $REPO_ROOT/maiko/build/c/linux.x86_64/ldesdl"
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

# Backup files that will be modified
BACKUP_DIR="$REPO_ROOT/.emulator_comparison_backup"
mkdir -p "$BACKUP_DIR"

# Function to restore backups
restore_backups() {
    if [ -d "$BACKUP_DIR" ]; then
        echo "Restoring original files..."
        if [ -f "$BACKUP_DIR/dispatch.zig" ]; then
            cp "$BACKUP_DIR/dispatch.zig" "$ZIG_DIR/src/vm/dispatch.zig"
        fi
        if [ -f "$BACKUP_DIR/execution_trace.zig" ]; then
            cp "$BACKUP_DIR/execution_trace.zig" "$ZIG_DIR/src/vm/execution_trace.zig"
        fi
        if [ -f "$BACKUP_DIR/xc.c" ]; then
            cp "$BACKUP_DIR/xc.c" "$REPO_ROOT/maiko/src/xc.c"
        fi
        rm -rf "$BACKUP_DIR"
    fi
}

# Trap to restore on exit
trap restore_backups EXIT

# Modify Zig emulator to stop after MAX_STEPS
echo ""
echo "=== Modifying Zig emulator to stop after ${MAX_STEPS} steps ==="
if [ -f "$ZIG_DIR/src/vm/dispatch.zig" ]; then
    cp "$ZIG_DIR/src/vm/dispatch.zig" "$BACKUP_DIR/dispatch.zig"
    # Replace MAX_INSTRUCTIONS with MAX_STEPS using Python for better reliability
    python3 <<EOF
import re
with open("$ZIG_DIR/src/vm/dispatch.zig", "r") as f:
    content = f.read()
content = re.sub(r'const MAX_INSTRUCTIONS: u64 = \d+', f'const MAX_INSTRUCTIONS: u64 = ${MAX_STEPS}', content)
with open("$ZIG_DIR/src/vm/dispatch.zig", "w") as f:
    f.write(content)
EOF
    echo "✓ Modified dispatch.zig: MAX_INSTRUCTIONS = ${MAX_STEPS}"
else
    echo "ERROR: dispatch.zig not found"
    exit 1
fi

# Modify execution_trace to stop logging after MAX_STEPS
if [ -f "$ZIG_DIR/src/vm/execution_trace.zig" ]; then
    cp "$ZIG_DIR/src/vm/execution_trace.zig" "$BACKUP_DIR/execution_trace.zig"
    # Add check to stop logging after MAX_STEPS using Python
    python3 <<EOF
with open("$ZIG_DIR/src/vm/execution_trace.zig", "r") as f:
    lines = f.readlines()

# Find the line with "self.instruction_count += 1" and add check before it
new_lines = []
for i, line in enumerate(lines):
    if "self.instruction_count += 1" in line and i > 0:
        # Check if check already exists
        if "if (self.instruction_count >= ${MAX_STEPS})" not in lines[i-1]:
            new_lines.append(f"        // Stop logging after {MAX_STEPS} steps\n")
            new_lines.append(f"        if (self.instruction_count >= {MAX_STEPS}) {{\n")
            new_lines.append("            return;\n")
            new_lines.append("        }\n")
    new_lines.append(line)

with open("$ZIG_DIR/src/vm/execution_trace.zig", "w") as f:
    f.writelines(new_lines)
EOF
    echo "✓ Modified execution_trace.zig: Stop logging after ${MAX_STEPS} steps"
else
    echo "ERROR: execution_trace.zig not found"
    exit 1
fi

# Modify C emulator to stop after MAX_STEPS
echo ""
echo "=== Modifying C emulator to stop after ${MAX_STEPS} steps ==="
if [ -f "$REPO_ROOT/maiko/src/xc.c" ]; then
    cp "$REPO_ROOT/maiko/src/xc.c" "$BACKUP_DIR/xc.c"
    # Add check before debug_instruction_count++ using Python
    python3 <<EOF
with open("$REPO_ROOT/maiko/src/xc.c", "r") as f:
    lines = f.readlines()

# Find the line with "debug_instruction_count++" and add check before it
new_lines = []
for i, line in enumerate(lines):
    if "debug_instruction_count++" in line and i > 0:
        # Check if check already exists
        if "if (debug_instruction_count >= ${MAX_STEPS})" not in lines[i-1]:
            new_lines.append(f"  if (debug_instruction_count >= {MAX_STEPS}) {{\n")
            new_lines.append(f"    break; /* Stop after {MAX_STEPS} steps */\n")
            new_lines.append("  }\n")
    new_lines.append(line)

with open("$REPO_ROOT/maiko/src/xc.c", "w") as f:
    f.writelines(new_lines)
EOF
    echo "✓ Modified xc.c: Stop after ${MAX_STEPS} steps"

    # Rebuild C emulator
    echo "Rebuilding C emulator..."
    cd "$REPO_ROOT/maiko"
    if [ -d "build-cmake" ]; then
        make -C build-cmake ldesdl >/dev/null 2>&1 || {
            echo "ERROR: Failed to rebuild C emulator"
            exit 1
        }
        # Update C_EMULATOR path
        if [ -f "build-cmake/ldesdl" ]; then
            C_EMULATOR="$REPO_ROOT/maiko/build-cmake/ldesdl"
        fi
    else
        echo "ERROR: build-cmake directory not found"
        exit 1
    fi
    cd "$REPO_ROOT"
else
    echo "WARNING: xc.c not found, C emulator may not stop at ${MAX_STEPS} steps"
fi

# Run C emulator
echo ""
echo "=== Running C Emulator ==="
echo "Command: $C_EMULATOR $SYSOUT_FILE"
cd "$REPO_ROOT"
timeout 10 "$C_EMULATOR" "$SYSOUT_FILE" > /dev/null 2>&1 || true

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
timeout 10 zig build run -- "../../../$SYSOUT_FILE" > /dev/null 2>&1 || true

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
