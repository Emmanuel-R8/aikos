# Execution Parity Between Emulators Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Achieve step-by-step execution parity between C and Zig Maiko emulators by resolving FPtoVP mapping differences and fixing top-level return handling.

**Architecture:** Focus on systematic debugging of memory management differences, starting with FPtoVP table verification, then opcode validation using unified traces. Use C implementation as reference, apply critical value analysis for byte/DLword conversions.

**Tech Stack:** Zig 0.15.2+, C reference in maiko/src/, unified trace format, comparison scripts in scripts/

---

## Prerequisites

- Read AGENTS.md for project guidelines
- Read reports/CURRENT_STATUS.md for current issues
- Understand FPtoVP table: maps file pages to virtual pages for BIGVM sysout format
- Know critical constants: BYTESPER_PAGE=512, STK_OFFSET=0x00010000
- Have access to starter.sysout in medley/internal/loadups/

---

### Task 1: Add FPtoVP Table Logging to Zig Emulator

**Files:**

- Modify: `zaiko/src/data/sysout.zig:473-495` (FPtoVP loading logic)
- Test: `scripts/compare_emulator_execution.sh` (for trace generation)

**Step 1: Add comprehensive FPtoVP logging**

Add debug prints in the FPtoVP loading loop to log each file page -> virtual page mapping:

```zig
// In zaiko/src/data/sysout.zig around line 485
std.debug.print("FPtoVP[{}] = file_page {} -> virtual_page {}\n", .{i, file_page, virtual_page});
```

**Step 2: Run Zig emulator with logging**

Run: `cd zaiko && zig build run -- ../medley/internal/loadups/starter.sysout`

Expected: Console output showing FPtoVP mappings for first 10-20 entries

**Step 3: Capture output for comparison**

Redirect output: `zig build run -- ../medley/internal/loadups/starter.sysout > zig_fptovp_log.txt 2>&1`

**Step 4: Commit logging changes**

```bash
git add zaiko/src/data/sysout.zig
git commit -m "feat: add FPtoVP table logging to Zig emulator for comparison"
```

---

### Task 2: Add FPtoVP Table Logging to C Emulator

**Files:**

- Modify: `maiko/src/ldsout.c:315-355` (FPtoVP loading section)
- Test: Build and run C emulator (if possible)

**Step 1: Add FPtoVP logging in C code**

Add printf statements in the FPtoVP loading loop:

```c
// In maiko/src/ldsout.c around line 359
for(i = 0; i < sysout_size * 2; i += 2) {
    unsigned short entry = word_swap_page(fptovp + i);
    printf("FPtoVP[%d] = %u\n", i/2, entry);
    // existing code...
}
```

**Step 2: Build C emulator**

Run: `cd maiko && make` (or appropriate build command)

Expected: Successful compilation

**Step 3: Run C emulator with logging**

Run: `./maiko ../medley/internal/loadups/starter.sysout > c_fptovp_log.txt 2>&1`

Expected: Console output with FPtoVP mappings

**Step 4: Commit changes**

```bash
git add maiko/src/ldsout.c
git commit -m "feat: add FPtoVP table logging to C emulator for comparison"
```

---

### Task 3: Compare FPtoVP Tables

**Files:**

- Create: `scripts/compare_fptovp_tables.py` (comparison script)
- Input: `c_fptovp_log.txt`, `zig_fptovp_log.txt`

**Step 1: Write comparison script**

```python
#!/usr/bin/env python3
import re

def parse_log(filename):
    mappings = {}
    with open(filename) as f:
        for line in f:
            match = re.search(r'FPtoVP\[(\d+)\] = (\d+)', line)
            if match:
                idx, value = int(match.group(1)), int(match.group(2))
                mappings[idx] = value
    return mappings

c_mappings = parse_log('c_fptovp_log.txt')
zig_mappings = parse_log('zig_fptovp_log.txt')

print("First 20 FPtoVP mappings:")
for i in range(min(20, len(c_mappings))):
    c_val = c_mappings.get(i, 'MISSING')
    z_val = zig_mappings.get(i, 'MISSING')
    status = "MATCH" if c_val == z_val else "DIFFER"
    print(f"[{i}] C:{c_val} Z:{z_val} {status}")
```

**Step 2: Run comparison**

Run: `python3 scripts/compare_fptovp_tables.py`

Expected: Output showing matching/differing entries

**Step 3: Identify divergence point**

Look for first index where C and Zig differ

**Step 4: Commit script**

```bash
git add scripts/compare_fptovp_tables.py
git commit -m "feat: add FPtoVP table comparison script"
```

---

### Task 4: Debug FPtoVP Byte-Swapping Logic

**Files:**

- Modify: `zaiko/src/utils/endianness.zig:225-227` (swap boundary calculation)
- Reference: `maiko/src/ldsout.c:323` (C boundary: (sysout_size / 4) + 1)

**Step 1: Verify swap boundary calculation**

Check that Zig's `calculateFPtoVPSwapBoundary` matches C's `(sysout_size / 4) + 1`

**Step 2: Add debug prints for boundary**

```zig
const boundary = endianness_utils.calculateFPtoVPSwapBoundary(sysout_size_halfpages);
std.debug.print("Swap boundary: {}\n", .{boundary});
```

**Step 3: Test with known sysout**

Run on starter.sysout, verify boundary matches C

**Step 4: Fix if needed and commit**

```bash
git add zaiko/src/utils/endianness.zig
git commit -m "fix: correct FPtoVP swap boundary calculation"
```

---

### Task 5: Fix Top-Level Return Handling

**Files:**

- Modify: `zaiko/src/vm/function.zig:86-89` (handleRETURN function)
- Reference: `maiko/src/tosret.c:112-125` (C OPRETURN continues execution)

**Step 1: Analyze current Zig behavior**

Current: `returnFromFunction` returns from dispatch loop

Expected: Continue dispatch loop after top-level return

**Step 2: Modify handleRETURN**

Change to advance PC by instruction length and continue:

```zig
// Instead of return returnFromFunction(...)
const result = returnFromFunction(...);
if (is_top_level_return) {
    // Advance PC and continue
    pc += instruction_length;
    continue;
}
return result;
```

**Step 3: Test execution continues**

Run with step limit, verify >16 instructions execute

**Step 4: Commit fix**

```bash
git add zaiko/src/vm/function.zig
git commit -m "fix: continue execution after top-level returns in Zig emulator"
```

---

### Task 6: Verify SIC Opcode with Identical Context

**Files:**

- Test: `zaiko/src/vm/opcodes/data_ops.zig:76-95` (SIC implementation)
- Reference: Memory at PC=0x60f136 should read 0x3e

**Step 1: Ensure FPtoVP mappings match**

Prerequisite: Previous tasks complete

**Step 2: Run SIC instruction**

Execute to PC=0x60f136, verify operand read as 0x3e

**Step 3: Check stack result**

Verify stack pushed 0xfffe0000

**Step 4: Update unified trace**

Add SIC to trace comparison

**Step 5: Commit verification**

```bash
git add zaiko/src/vm/execution_trace.zig
git commit -m "feat: add SIC opcode to unified trace verification"
```

---

### Task 7: Establish Unified Trace Comparison Workflow

**Files:**

- Use: `scripts/compare_emulator_execution.sh` (main script)
- Use: `scripts/compare_unified_traces.awk` (fast comparison)

**Step 1: Generate traces**

Run: `EMULATOR_MAX_STEPS=100 scripts/compare_emulator_execution.sh`

**Step 2: Compare traces**

Run: `scripts/compare_unified_traces.awk c_emulator_unified_trace.txt zig_emulator_unified_trace.txt`

**Step 3: Identify next divergence**

Find first differing line after SIC

**Step 4: Document workflow**

Update docs with trace comparison process

**Step 5: Commit documentation**

```bash
git add documentation/implementations/zig-implementation.typ
git commit -m "docs: document unified trace comparison workflow"
```

---

### Task 8: Complete Remaining SDL2 Tasks

**Files:**

- Test: SDL2 test cases T092-T096
- Polish: Tasks T103-T108

**Step 1: Implement SDL2 test cases**

Add tests for display, keyboard, mouse integration

**Step 2: Run tests**

Verify SDL2 functionality works

**Step 3: Complete polish tasks**

Address compilation warnings, memory leaks

**Step 4: Update task status**

Mark tasks complete in specs/005-zig-completion/tasks.md

**Step 5: Commit completion**

```bash
git add specs/005-zig-completion/tasks.md
git commit -m "feat: complete SDL2 test cases and polish tasks"
```

---

### Task 9: Update Documentation

**Files:**

- Update: `documentation/specifications/` (emulator-independent findings)
- Update: `documentation/implementations/zig-implementation.typ` (Zig-specific details)

**Step 1: Document FPtoVP findings**

Add section on byte-swapping boundary and mapping logic

**Step 2: Document return handling fix**

Explain top-level return behavior difference

**Step 3: Update per critical_memory.typ**

Use date command for timestamps: `date '+%Y-%m-%d %H:%M'`

**Step 4: Commit documentation**

```bash
git add documentation/specifications/ documentation/implementations/
git commit -m "docs: update documentation with FPtoVP and return handling findings"
```

---

## Execution Options

Plan complete and saved to `docs/plans/2026-01-26-execution-parity-between-emulators.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
