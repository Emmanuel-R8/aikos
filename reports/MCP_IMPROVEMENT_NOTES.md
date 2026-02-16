# MCP Improvement Notes

**Date**: 2026-02-15
**Session**: Laiko Emulator Development

## Summary of Difficulties Encountered

### 1. Byte Ordering / XOR Addressing Confusion

**Problem**: Went back and forth multiple times on whether bytecode extraction should use XOR addressing. This wasted significant debugging time.

**Root Cause**: 
- No single source of truth for memory access patterns
- XOR addressing applies differently to different data types (bytes vs words vs LispPTRs)
- Pages were 32-bit word-swapped during load, but the exact implications weren't clear

**Solution**: 
- Created `memory-access.lisp` with tested primitives
- Need unit tests for each memory access function
- Document the exact XOR pattern for each access type

### 2. MCP Underutilization

**Problem**: Manual testing via `./laiko/run.sh` instead of interactive debugging.

**What I Should Have Done**:
1. Start Swank server at session start
2. Use `swank_eval` for interactive testing
3. Use `swank_backtrace` for debugging errors
4. Hot-patch code changes without restart
5. Inspect VM state interactively

**MCP Deficiencies Observed**:
- Swank connection was attempted but failed silently
- No clear guidance on how to use Swank for Common Lisp debugging
- Would benefit from a `swank_ensure_running` function that starts Swank if not already running

### 3. No Systematic Trace Comparison

**Problem**: Comparing C/Zig/Laiko traces manually with grep instead of automated comparison.

**Solution**: Create trace comparison utility:
```lisp
(defun compare-traces (c-trace zig-trace laiko-trace)
  ;; Extract PC/TOS/SP at each step
  ;; Highlight divergences
  ;; Report first point of divergence
  )
```

### 4. Zig vs C Verification

**Problem**: Relied too heavily on Zig emulator traces without verifying against C.

**Correction**: Always double-check Zig behavior against C emulator:
```bash
# Run both emulators with same parameters
env EMULATOR_MAX_STEPS=N ./maiko/build/c/linux.x86_64/lde sysout
env EMULATOR_MAX_STEPS=N ./zaiko/zig-out/bin/zaiko sysout
# Compare outputs
```

### 5. Stack Architecture Confusion

**Problem**: VM had both legacy stack array and virtual memory offsets, causing confusion about which to use.

**Root Cause**: Incomplete refactoring when switching to virtual memory stack.

**Solution**: 
- Remove legacy stack fields entirely
- Use only `stack-ptr-offset` (byte offset into virtual memory)
- Use `top-of-stack` as cached value (per C behavior)

## Recommended MCP Improvements

### 1. Swank Integration Helpers

```markdown
## swank_ensure_running(port=4005)
Starts SBCL with Swank if not already running. Returns connection info.

## swank_load_system(system-name)
Loads an ASDF system in the connected Swank session.

## swank_inspect(variable-path)
Inspects a variable in the connected Swank session, returning a structured representation.

## swank_trace_function(function-name)
Traces a function in the Swank session.

## swank_break_on_error()
Sets Swank to break on errors for interactive debugging.
```

### 2. Trace Comparison Tools

```markdown
## compare_emulator_traces(c-trace, zig-trace, laiko-trace)
Compares execution traces from all three emulators.
Returns first divergence point with context.

## extract_trace_state(trace-file, step-number)
Extracts PC, TOS, SP at a specific step from a trace file.
```

### 3. Memory Access Testing

```markdown
## test_memory_access(vm, address, expected-value)
Tests that reading from VM at address returns expected value.
Useful for verifying XOR addressing is correct.
```

### 4. Interactive VM Debugging

```markdown
## vm_state_summary(vm)
Returns a human-readable summary of VM state.

## vm_stack_dump(vm, count=10)
Dumps top N stack entries.

## vm_trace_step(vm)
Executes one instruction and returns state change.
```

## Utility Functions to Create

### 1. Unified Memory Access Layer

```lisp
;; In memory-access.lisp

(defun vm-read-byte (vm offset) ...)   ; XOR addressing built-in
(defun vm-read-word (vm offset) ...)   ; XOR addressing built-in
(defun vm-read-lispptr (vm offset) ...); XOR addressing built-in
(defun vm-write-byte (vm offset val) ...)
(defun vm-write-word (vm offset val) ...)
(defun vm-write-lispptr (vm offset val) ...)
```

### 2. Stack Operations

```lisp
(defun vm-stack-push (vm value) ...)
(defun vm-stack-pop (vm) ...)
(defun vm-stack-tos (vm) ...)
(defun vm-stack-set-tos (vm value) ...)
```

### 3. Trace Utilities

```lisp
(defun parse-trace-line (line) ...)
(defun compare-trace-states (c-state zig-state laiko-state) ...)
(defun find-first-divergence (c-trace zig-trace laiko-trace) ...)
```

## Lessons Learned

1. **Test memory access early**: Create unit tests for XOR addressing before relying on it
2. **Use MCP interactively**: Don't rely on print debugging
3. **Verify C first**: Always check C emulator behavior before trusting Zig
4. **Document decisions**: Write down why XOR addressing is needed (32-bit word swap during page load)
5. **Single source of truth**: One memory access module, not scattered functions

## Action Items for Future Sessions

1. [ ] Add `swank_ensure_running` to MCP
2. [ ] Create trace comparison utility
3. [ ] Add unit tests for memory access functions
4. [ ] Remove legacy stack fields from VM structure
5. [ ] Always verify Zig against C before acting on trace data

---

## Additional Notes (2026-02-15 23:30)

### 6. Package System Causing Silent Failures

**Problem**: Many SBCL tests failed with stack traces instead of useful error messages. The root cause was often missing exports or undefined functions, but errors were silently swallowed.

**Root Cause**: 
- `DEFPACKAGE` for `MAIKO-LISP-TESTS` was being evaluated repeatedly
- Missing exports caused "unbound variable" errors that were hard to diagnose
- The MCP's SBCL calls used `--disable-debugger` which hid useful info

**Solution**:
- Use `swank_eval` to get proper error messages
- Add exports to package before using them
- Create a "check exports" utility function

### 7. Value Cell Address Verification Issue

**Problem**: GVAR reads value=0 but Zig shows TOS=0x0E after GVAR push.

**Debugging Steps**:
1. Verify value cell byte offset calculation
2. Check if page containing value cell is loaded
3. Verify XOR addressing for value cell read
4. Compare raw bytes at value cell location with C/Zig

**Key Learning**: The value cell address calculation:
```
VALS_OFFSET_DLWORDS = 0xC0000
byte_offset = (0xC0000 + (atom_index << 1)) * 2
```

For atom 0x20A:
- laddr_dlwords = 0xC0000 + 0x414 = 0xC0414
- byte_offset = 0x180828 (page 3076, offset 0x28)

### 8. Zig vs C Verification Critical Example

**Problem**: Zig trace showed opcode 0x0A (FN2) at PC 0x60F136, but trace also showed opcode 0x12 (UNBIND).

**Critical Discovery**: Looking at the Zig output:
```
DEBUG dispatch: PC=0x60f136, RAW bytes (no XOR): 0x0a 0x02...
DEBUG dispatch: PC=0x60f136, XOR bytes: 0x12 0xc9...
DEBUG: Decoded opcode 0xa (FN2) at PC=0x60f136
```

Zig decodes from **RAW bytes** for the opcode, but the trace log shows **XOR bytes**. This means:
- Opcode decoding uses XOR addressing (0x0A)
- Trace display shows XOR bytes (0x12)
- The trace display is **misleading**!

**Lesson**: Always check the actual decoded opcode, not just the trace display.

### 9. Recommended Debugging Workflow

```markdown
## When debugging memory/execution issues:

1. **Check C first**: Run C emulator, capture trace
2. **Check Zig second**: Run Zig, compare with C
3. **Check Laiko**: Compare with both

## For memory access issues:

1. Calculate expected address
2. Check if page is loaded (VP from FPtoVP)
3. Check raw bytes at that address
4. Verify XOR addressing is applied correctly

## For stack issues:

1. Verify stack pointer initialization from FX->nextblock
2. Check stack grows DOWN (decreasing addresses)
3. Verify TOS caching matches C behavior
```

### 10. MCP Session Management Issues

**Problem**: Started Swank server but couldn't connect from MCP.

**Root Cause**:
- No `swank_connect` implementation in MCP
- Server was running but no way to send commands

**Workaround Used**:
- Created separate SBCL processes with `--eval` for each test
- Very slow iteration cycle (reloading all files each time)

**Ideal MCP Functions**:
```lisp
;; Start persistent SBCL session
(swank_start :port 4006)  ; Returns connection handle

;; Evaluate in running session
(swank_eval "(+ 1 2)")   ; Uses existing connection

;; Load file in session
(swank_load "path/to/file.lisp")

;; Inspect variable
(swank_inspect '*vm*)

;; Get backtrace on error
(swank_backtrace)
```

