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
