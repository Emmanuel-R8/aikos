# Laiko Common Lisp Emulator Parity Improvement Plan

**Date**: 2026-02-15 20:34
**Status**: In Progress
**Goal**: Achieve execution parity between Laiko (Common Lisp) and C/Zig emulators

---

## Core Principle

**One instruction at a time, validated against C and Zig traces.**

Parity can ONLY be achieved by comparing detailed traces/logs from each emulator.

---

## Phase 1: Trace Infrastructure Setup

### 1.1 Instrument Laiko for Trace Generation
- Verify `laiko/src/vm/trace.lisp` produces unified trace format
- Ensure same columns as C/Zig: `LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|...`
- Add FX dump to first trace line

### 1.2 Generate Reference Traces
Run all three emulators with `EMULATOR_MAX_STEPS=20`:
- C: `c_emulator_unified_trace.txt`
- Zig: `zig_emulator_unified_trace.txt`
- Laiko: `lisp_emulator_unified_trace.txt`

### 1.3 Compare Initial State
Extract from each trace:
- Initial PC value
- Initial SP value
- FX contents (fnheader, pc, nextblock)
- First 5 instructions executed

---

## Phase 2: MCP Development Environment

### 2.1 Start Persistent SBCL Session
```bash
sbcl --load quicklisp/setup.lisp \
     --eval "(ql:quickload :swank)" \
     --eval "(swank:create-server :port 4005 :dont-close t)"
```

### 2.2 Connect and Load via MCP
```
swank_connect(host="127.0.0.1", port=4005)
swank_eval("(load \"laiko/src/package.lisp\")")
swank_eval("(load \"laiko/src/utils/types.lisp\")")
... load all files ...
swank_eval("(maiko-lisp.data:load-sysout \"starter.sysout\")")
```

### 2.3 Inspect State via MCP
```
swank_eval("(list (maiko-lisp.data:ifpage-currentfxp ifpage) ...)")
swank_backtrace()  ; on errors
```

---

## Phase 3: Instruction-by-Instruction Parity

For each divergence found:

1. **Identify**: Which instruction differs?
2. **Trace C**: What does C do at this PC?
3. **Trace Zig**: What does Zig do? (comments may explain logic)
4. **Inspect Laiko**: Use MCP to examine VM state
5. **Fix**: Hot-reload corrected code via MCP
6. **Validate**: Re-run and compare
7. **Proceed**: Only when match

---

## Phase 4: Key Investigation - FX Page Mapping

### 4.1 The Core Question
`currentfxp = 0x2E72` → byte offset `0x15CE4` → virtual page 174

**Where does VP 174 come from in the sysout file?**

### 4.2 Investigation via Traces
1. C trace: Show which FP maps to VP 174
2. Zig trace: Same mapping?
3. Laiko: Dump FPtoVP[fp] for all fp where `GETFPTOVP == 174`

### 4.3 Expected Resolution
- Page should be loaded from sysout at file page N
- FPtoVP[N] should give VP 174
- If VP 174 is nil in vmem, page wasn't loaded

---

## Files to Study (Zig Reference)

| Priority | File | What to Extract |
|----------|------|-----------------|
| HIGH | `zaiko/src/data/sysout.zig` | Page loading loop, FPtoVP usage |
| HIGH | `zaiko/src/vm/vm_initialization.zig` | FX reading, PC init |
| HIGH | `zaiko/src/data/fptovp.zig` | GETFPTOVP, GETPAGEOK macros |
| MEDIUM | `zaiko/src/vm/frame.zig` | Frame Extension structure |
| MEDIUM | `zaiko/src/memory/manager.zig` | Address translation |

---

## Progress Log

### 2026-02-15 21:30
- **Completed: Virtual memory stack operations**
  - Added vm-stack-base-offset, vm-stack-ptr-offset, vm-stack-end-offset
  - Added vm-top-of-stack (cached TOS per C behavior)
  - Added vm-push, vm-pop, vm-tos, vm-set-tos
  - Added vm-read-lispptr, vm-write-lispptr with XOR addressing
- **Fixed FN opcode handlers**
  - Updated FN0-FN4 to use vm-pop/vm-push
- **Current execution progress**
  - 4 instructions execute successfully:
    1. NOP (0x00)
    2. NOP (0x00)
    3. GVAR (0x60) - reads atom index
    4. FN2 (0x0A) - needs atom/defcell lookup
- **Blocking issue**: FN opcodes need DEFCELL/ATOMCELL implementation
  - Need to read atom definition from atom table
  - Need function header lookup
  - See zaiko/src/data/defcell.zig for reference

### 2026-02-15 20:34 (earlier)
- Fixed 32-bit word swap (not 16-bit pair swap)
- Fixed stack offset calculation (STK_OFFSET * 2)
- Fixed XOR addressing for bytecode (GETBYTE = base ^ 3)
- PC = 0x60F130, first opcode 0xBF (POP) ✅

---

## Success Criteria

After implementation:
- [ ] Laiko trace matches C trace for first 20 instructions
- [ ] PC progression identical
- [ ] SP/FP values identical
- [ ] Opcode execution results match
- [ ] No cascade errors from instruction 1
