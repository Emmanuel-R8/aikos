# Next Tasks: Get Running Zig VM

**Date**: 2025-01-27  
**Status**: Proposal for immediate next steps  
**Goal**: Get a functional, running Zig VM that can execute Medley Interlisp

## Current Status

- ✅ **Compilation**: Fixed (unused error capture in sysout.zig)
- ✅ **Phases 1-5**: Marked complete (94/108 tasks, 87.0%)
- ✅ **Build**: Successfully compiles (`zig build` passes)
- ⏳ **Runtime**: Needs verification and testing

## Critical Path to Running VM

### Immediate Priority Tasks

#### 1. **Fix Compilation Error** ✅ COMPLETE
- **Task**: Fix unused error capture in `sysout.zig:270`
- **Status**: ✅ Fixed - changed `catch |err|` to `catch` (error not used)
- **Result**: Build now succeeds

#### 2. **Verify Basic Execution** 🔴 HIGH PRIORITY
- **Task**: Test that VM can load sysout and enter dispatch loop
- **Action**: Run `./zig-out/bin/maiko-zig medley/internal/loadups/starter.sysout`
- **Expected**: VM loads sysout, initializes, enters dispatch loop
- **If fails**: Fix runtime errors preventing execution
- **Files to check**: `main.zig`, `vm/dispatch.zig`, `vm/init.zig`

#### 3. **Fix Runtime Errors** 🔴 HIGH PRIORITY (if needed)
- **Task**: Identify and fix any runtime errors during execution
- **Common issues**:
  - Missing opcode handlers (should be complete per tasks.md)
  - Memory access errors (address translation issues)
  - Stack initialization problems
  - SDL2 initialization failures (if display required)
- **Debug approach**: Add debug output, compare with C emulator behavior

#### 4. **Verify Dispatch Loop Execution** 🟡 MEDIUM PRIORITY
- **Task**: Ensure dispatch loop executes instructions correctly
- **Action**: Run with debug output, verify instructions are being executed
- **Check**: 
  - PC (program counter) advances correctly
  - Stack operations work
  - Basic opcodes execute without errors
- **Files**: `vm/dispatch/dispatch.zig`, `vm/dispatch/execution.zig`

#### 5. **Test with Minimal Sysout** 🟡 MEDIUM PRIORITY
- **Task**: Test with `starter.sysout` (smallest sysout file)
- **Goal**: Verify basic execution path works
- **Success criteria**: 
  - Sysout loads without errors
  - VM initializes correctly
  - Dispatch loop runs (even if it doesn't complete startup)

## Secondary Tasks (After VM Runs)

### Test Cases (T092-T096)
- **T092**: SDL2 window creation and display test
- **T093**: BitBLT rendering operations test
- **T094**: Keyboard event translation test
- **T095**: Mouse event translation test
- **T096**: Integration test for interactive session

**Note**: These can be done after basic VM execution is verified.

### Polish Tasks (T103-T108)
- **T103**: Performance optimization for sysout loading
- **T104**: Performance optimization for bytecode execution
- **T105**: Quickstart validation ✅ (already done)
- **T106**: Error messages ✅ (already done)
- **T107**: Test verification ✅ (already done)
- **T108**: C emulator comparison (partially done, needs execution comparison)

**Note**: Performance optimization can be deferred until VM runs correctly.

## Recommended Execution Order

### Phase A: Get VM Running (Immediate)
1. ✅ Fix compilation error (DONE)
2. 🔴 Test basic execution with `starter.sysout`
3. 🔴 Fix any runtime errors that prevent execution
4. 🔴 Verify dispatch loop works
5. 🟡 Test with minimal sysout file

### Phase B: Verify Functionality (After Phase A)
6. 🟡 Test with `lisp.sysout` (full Medley startup)
7. 🟡 Verify essential opcodes work correctly
8. 🟡 Check memory management (GC operations)
9. 🟡 Test SDL2 display integration (if needed for startup)

### Phase C: Complete Testing (After Phase B)
10. Implement SDL2 test cases (T092-T096)
11. Complete C emulator comparison (T108 remaining items)
12. Performance optimization (T103-T104)

## Testing Commands

```bash
# Build
cd maiko/alternatives/zig
zig build

# Test basic execution
./zig-out/bin/maiko-zig ../../medley/internal/loadups/starter.sysout

# Test with full Medley (if basic works)
./zig-out/bin/maiko-zig ../../medley/internal/loadups/lisp.sysout

# Run tests
zig build test

# Compare with C emulator (if available)
../../linux.x86_64/ldesdl ../../medley/internal/loadups/starter.sysout
```

## Success Criteria

### Minimum Viable Running VM
- ✅ Compiles without errors
- ⏳ Loads sysout file successfully
- ⏳ Initializes VM state correctly
- ⏳ Enters dispatch loop
- ⏳ Executes at least one instruction without crashing

### Full Running VM
- ⏳ Executes multiple instructions correctly
- ⏳ Handles basic opcodes (arithmetic, stack, function calls)
- ⏳ Can start Medley (reach Lisp prompt or initialization)
- ⏳ Memory management works (no leaks)
- ⏳ Display integration works (if required)

## Known Issues to Watch For

1. **SDL2 Initialization**: May fail if SDL2 not available (but shouldn't block basic execution)
2. **Missing Opcodes**: All essential opcodes should be implemented, but may have bugs
3. **Memory Access**: Address translation issues could cause crashes
4. **Stack Management**: Stack initialization or bounds checking issues
5. **Event Polling**: SDL2 event polling may block if display not available

## Next Steps

1. **Immediate**: Test execution with `starter.sysout`
2. **If errors**: Debug and fix runtime issues
3. **If works**: Test with `lisp.sysout` for full startup
4. **Then**: Implement remaining test cases and polish tasks

---

**Last Updated**: 2025-01-27  
**Status**: Ready to test execution
