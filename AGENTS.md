# AI Agent Guidelines for Interlisp Project

**Date**: 2025-12-10 12:45
**Purpose**: Guidelines and best practices for AI agents working on this project

## Project Overview

This repository contains the **Interlisp** project, which includes:

- **Maiko - C Implementation in @/maiko_untouched**: Virtual machine emulator for Medley Interlisp bytecode. Reference implementation in C (fully functional) with additional commenting. NEVER MODIFY APART FROM COMMENTING.
- **Maiko - C Implementation in @/maiko**: Virtual machine emulator for Medley Interlisp bytecode. Reference implementation in C (fully functional) with minor refactoring and tracing statements
- **Zig Implementation**: Alternative implementation of Maiko in Zig (in progress)
- **Common Lisp Implementation**: Alternative implementation of Maiko in Common Lisp (targetting Sbcl) (in progress)

## Critical Documentation

### Must-Read Files

1. Read ./WORK_STATE.md, then ./STEP_COMPARISON_STATUS.md

1. When **compressing or summarizing context**, follow **§7**: Phase 1 (aggressive compression of tool/command/trace/linter outputs only) is mandatory and first; Phase 2 (re-read AGENTS.md, then compress the remainder) only if further compression is needed.

1. **`documentation/core/critical_memory.typ`** - **CRITICAL**: Rules for documentation updates
   - All documentation improvements MUST be emulator-independent in `specifications/`
   - Language-specific details go in `implementations/`
   - **ALWAYS** write documentation using the Typst document format
   - **ALWAYS** update both before committing
   - **ALWAYS** use the command `date` to date entries (when necessary )i as YYYY-mm-dd HH:MM

1. **`documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`** - **CRITICAL**: Essential debugging techniques and practices

1. **`documentation/README.md`** - Overview of documentation structure
1. **`specs/004-emulator-runner/plan.md`** - Implementation plan for emulator runner
1. **`specs/004-emulator-runner/tasks.md`** - Task list for emulator runner
1. **`specs/005-zig-completion/plan.md`** - Implementation plan for Zig emulator
1. **`specs/005-zig-completion/tasks.md`** - Detailed task list (94/108 complete, 87.0%)

## Project Structure

```
Interlisp/
├── documentation/             # Knowledge base (emulator-independent specs + implementations)
│   ├── specifications/        # Emulator-independent specifications
│   ├── implementations/       # Language-specific implementation notes
│   └── core/
│       └── critical_memory.typ # ⚠️ MUST READ AND ABIDE BY - Documentation rules
├── maiko/                     # Maiko VM source code
│   ├── src/                   # C implementation (reference)
│   └── alternatives/zig/      # Zig implementation (in progress)
│   └── alternatives/lisp/     # Common Lisp (sbcl) implementation (in progress - will be developed after Zig)
├── specs/                     # Feature specifications
│   └── 005-zig-completion/   # Zig emulator completion spec
└── medley/                    # Medley Interlisp system
```

## Current Status (2026-01-20)

### Zig Emulator Completion: 89.2% (96/108 tasks)

**Completed**:

- ✅ Phase 1: Sysout Loading (22/22 tasks)
- ✅ Phase 2: Basic Execution (12/12 tasks)
- ✅ Phase 3: Essential Opcodes (25/25 tasks)
- ✅ Phase 4: GC Operations (15/15 tasks)
- ✅ Phase 5: SDL2 Display Integration (22/22 tasks - implementation complete)
- ✅ Phase 6: Advanced Opcodes (2/2 critical: UNBIND fully implemented with parity debugging)

**Remaining**:

- ⏳ SDL2 Test Cases (5 tasks: T092-T096)
- ⏳ Polish Tasks (5 tasks: T103-T108)
- ⏳ TOPOFSTACK Source Investigation (UNBIND temporary fix needs proper implementation)

**Recent Achievements**:

- ✅ **UNBIND Opcode**: Complete implementation with systematic debugging
- ✅ **Parity Debugging**: Achieved instruction-by-instruction parity through 5 operations
- ✅ **Critical Bug Fix**: Signed vs unsigned comparison in UNBIND marker detection
- ✅ **Debugging Methodology**: Established comprehensive technique hierarchy

**Known Issues**:

- ⚠️ Minor compilation fixes needed (SDL2 type mismatches, optional unwrapping)
- ⚠️ Test cases for SDL2 not yet implemented
- ⚠️ UNBIND TOPOFSTACK restoration uses temporary hardcoded value (needs proper source investigation)

## Working Guidelines

### 1. Code Organization

- **File Size Limit**: Keep files under 500 lines (user preference)
- **Modular Structure**: Split large files into logical modules
- **Naming**: Follow existing conventions (see `zaiko/src/`)

### 1.1 Submodules (CRITICAL)

This repository includes git submodules (notably `maiko/` and `medley/`).

- **Default**: Treat submodules as **read-only** unless the user explicitly asks to modify them.
- **Committing**:
  - **Do not commit inside submodules** unless the user explicitly requests it.
  - Superproject commits may update submodule pointers _only if explicitly intended_.
- **Staging**: Paths inside submodules cannot be staged from the superproject (e.g. `git add maiko/...` will fail). Enter the submodule repo if changes are required.

### 1.2 Generated Artifacts (never commit)

Avoid committing machine/local outputs that create noisy diffs:

- **Execution logs / traces**: `*_execution_log*.txt`, `c_*_trace*.txt`, `zig_*_trace*.txt`
- **Python caches**: `__pycache__/`, `*.pyc`
- **Build outputs**: Zig `zig-out/`, `**/.zig-cache/`, C/CMake build directories
- **Binary documents**: generated PDFs (commit the Typst source instead)

### 1.3 File System Access Restrictions (CRITICAL)

**SECURITY REQUIREMENT**: Agents must not access any files or folders outside the project directory containing this AGENTS.md file.

- **Prohibited**: Accessing `/tmp`, `/var`, `/home`, or any system directories
- **Prohibited**: Creating files outside the project workspace
- **Allowed**: Only files and folders within the Interlisp project directory
- **Reason**: Security, isolation, and reproducibility requirements

**Violation will result in immediate termination of the session.**

### 2. Documentation Updates

**CRITICAL**: Before ANY git commit, follow `documentation/core/critical_memory.typ`:

1. ✅ Review discoveries and learnings
2. ✅ Update general documentation (`specifications/`) with emulator-independent findings
3. ✅ Update language-specific documentation (`implementations/`) with Zig-specific details
4. ✅ Make comprehensive commit with documentation updates

**Example Commit Format**:

```
Update documentation: [Brief description]

CRITICAL: Document [general findings] and [language-specific findings]

General Documentation Updates:
- Updated [file]: [general finding 1]
- Updated [file]: [general finding 2]

Language-Specific Documentation Updates:
- Updated [file]: [language-specific detail 1]
- Updated [file]: [language-specific detail 2]
```

### 3. C Implementation as Reference

**CRITICAL RULE**: The C implementation (`maiko/src/`) is the ultimate reference.

- If comments/documentation conflict with C code, **trust the C code**
- If documentation is incorrect, **update the documentation**
- When debugging, use the C emulator with debug statements to understand behavior

### 4. Systematic Testing and Validation

#### Core Testing Strategy
- **Fast Smoke Tests**: Use quick test runner by default (user preference)
- **Comprehensive Tests**: Keep separate for thorough validation
- **Test Location**: Tests should be next to scripts in `tests/` folders

#### Parity Testing Methodology
- **Cross-Reference C Traces**: Compare execution traces instruction-by-instruction
- **Step-by-Step Validation**: Run emulators with `EMULATOR_MAX_STEPS=N` for controlled testing
- **Canonical Comparison Script**: Use `scripts/compare_emulator_execution.sh` for systematic comparison
- **Unified Trace Format**: Ensure consistent logging format across implementations

#### Regression Testing
- **Commit-Hook Validation**: Run parity tests before commits to catch regressions
- **Incremental Testing**: Test after each opcode implementation
- **Boundary Testing**: Validate edge cases and error conditions

#### Debugging Integration
- **Systematic Debugging**: Follow `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ` hierarchy
- **Trace Analysis**: Use comparison tools for divergence identification
- **Performance Profiling**: Monitor execution speed during testing

### 4.4 Systematic Debugging Workflow

**CRITICAL**: For any implementation divergence or bug, follow this systematic process:

1. **Cross-reference C traces** - Establish baseline with verified C emulator output
2. **Instrument execution** - Add targeted debug prints following technique hierarchy
3. **Validate incrementally** - Test after each code change, not at the end
4. **Document findings** - Update both specifications and implementation docs
5. **Regression test** - Ensure no existing functionality breaks

**Key Principle**: Never implement blindly - always validate against C reference first.

**Performance Considerations**:
- VM emulators require careful optimization - profile memory access patterns
- Stack operations are performance-critical - minimize redundant calculations
- Memory translation overhead should be amortized across operations
- Consider instruction caching for frequently executed opcodes

### 4.2 Unified Trace Format

**Purpose**: Enable rapid divergence identification between C and Zig emulators

**Format**: Single-line, pipe-delimited columns:

```
LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
```

**Key Benefits**:

- **Rapid comparison** with awk/Python scripts
- **Comprehensive context** in single line
- **Consistent format** across both emulators
- **Memory issue triage** with dedicated fields

**Comparison Tools**:

- `scripts/compare_unified_traces.awk` - Fast awk-based comparison
- `scripts/compare_unified_traces.py` - Detailed Python analysis

### 4.3 Centralized Memory Management

**Problem Solved**: Scattered memory logic causing recurring address translation, endianness, and paging issues

**Solution**: Centralized memory management module (`zaiko/src/memory/manager.zig`)

**Components**:

- **AddressManager**: LispPTR ↔ byte conversions, virtual page calculations
- **FPtoVPManager**: File page ↔ virtual page mapping, page OK flags
- **EndiannessManager**: Byte-swapping logic, XOR addressing
- **MemoryAccessManager**: Safe memory reads, bounds checking

**Integration**: Both C and Zig emulators use centralized functions for consistency

### 4.1 Parity Workflow (C vs Zig) – Canonical

For repeatable execution-trace parity work:

- **Canonical script**: `scripts/compare_emulator_execution.sh`
  - Supports the shared runtime cap knob **`EMULATOR_MAX_STEPS`** (unset/0 → run to completion).
- **Unified trace format**: Single-line column-formatted traces for rapid comparison
  - **C trace**: `c_emulator_unified_trace.txt`
  - **Zig trace**: `zig_emulator_unified_trace.txt`
  - **Comparison scripts**: `scripts/compare_unified_traces.awk`, `scripts/compare_unified_traces.py`
- **Divergence analysis**: `scripts/analyze_execution_divergence.py` (supports LCP skip + `--start-line` resume).
- **Path robustness**: Prefer **absolute sysout paths** (scripts should not depend on cwd).

### 5. Build System

- **Zig**: Uses Zig build system (`build.zig`)
- **C**: Uses traditional Makefile/CMake
- **Nix**: Project uses Nix for managing dependencies (see `shell.nix`)

### 6. Code Style

- **Indentation**: 2 spaces (R conventions)
- **Python Version**: 3.12
- **Zig Version**: 0.15.2+

### 7. Context Compression and Summarization

**Purpose**: When context must be reduced, these rules preserve decision‑critical information. Compression is done in two phases: a mandatory **Phase 1** (aggressive compression of tool/command/trace/linter outputs only), then **Phase 2** (only if needed: re-read AGENTS.md and compress the remainder).

**When to use**: Before or after compressing a conversation; when producing a "session summary" or "context pack"; when an agent is asked to "summarize" or "reduce context".

---

#### 7.1 Phase 1 — Preliminary (mandatory, always first)

Aggressively compress the following **without** compressing the rest of the context (conversation, file contents, documentation/spec excerpts, etc.).

| Category | Treatment | Excerpt? |
|----------|-----------|----------|
| **Tool outputs** (e.g. `run_terminal_cmd`, `grep`, `list_dir`) | **Option B**: Keep a tiny excerpt (first 2 + last 2 lines, or the critical/failing line) plus a 1–2 line summary; drop the rest. | Yes |
| **Unix command outputs** (terminal stdout/stderr) | **Option B**: Same — first 2 + last 2 lines, or the error/failure lines if that is the critical part, plus 1–2 line summary. | Yes |
| **Tracing/debugging** (execution traces, `std.debug.print`/`printf` dumps, unified trace blocks) | **Option B**: Same; for execution traces, the first-divergence or error line is the critical line. | Yes |
| **Linter/compiler output** | **Do not keep.** Replace with at most a one-line summary (e.g. "zig build: 3 errors", "Linter: 2 issues in X"); drop all raw lines. | No |

Do **not** compress the remainder in Phase 1.

---

#### 7.2 Assess

After Phase 1, check whether the context still needs further compression. If it does **not**, stop. If it does, apply **Phase 2**.

---

#### 7.3 Phase 2 — Further compression (only if needed)

1. **Re-read AGENTS.md in full** — Perform a full read of this file as if starting a new task (all Must-Read, Working Guidelines, etc. apply afresh).
2. **Compress the remainder** — Apply **§7.4** to the **remainder** only. The remainder is: conversation, file contents, documentation/spec excerpts, and the Phase‑1 summary lines; it excludes raw tool outputs, command outputs, tracing/debugging, and linter/compiler output (already handled in Phase 1).
3. **Final context** — Append the compressed remainder to the full AGENTS.md. Order: **[AGENTS.md in full]** then **[compressed remainder]**.

---

#### 7.4 Rules for compressing the remainder

These apply **only in Phase 2** and **only to the remainder**.

**7.4.1 Retain with highest priority** (always keep, or a one‑line pointer)

- **AGENTS.md**: At least a minimal stub: Critical Documentation, Common Debugging Gotchas, Important Paths, submodule policy, and a pointer to §7. If the full file does not fit, retain those plus §7.
- **Session / work state**: `WORK_STATE.md`, `STEP_COMPARISON_STATUS.md` (or the current "session" file). Current blocker, first divergence, next action.
- **Current blocker or first divergence**: 1–3 lines (e.g. "Zig SP/FP init wrong at `zaiko/src/vm/vm_initialization.zig`; C SP=0x02e88 FP=0x307864, Zig differs").
- **Critical pitfalls**: Full "Common Debugging Gotchas" or: PC=byte not DLword; FPtoVP=512‑byte pages; byte swap vs XOR; CSTKPTRL/TOPOFSTACK re-read from memory after restore; VM/stack init (SP/FP from IFPAGE). See `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ` Part III.
- **Key paths**: `zaiko/src/`, `maiko/src/`, `scripts/compare_emulator_execution.sh`, `documentation/specifications/`, `documentation/implementations/`, `specs/004-emulator-runner/`, `specs/005-zig-completion/`.

**7.4.2 Retain in abbreviated form**

- **Specs** (`specs/004-emulator-runner/tasks.md`, `specs/005-zig-completion/tasks.md`): Phase name, checkpoint status, **next 3–5 open tasks**; drop full body of completed tasks.
- **Documentation**: Section titles and "see `path` for X"; drop long code examples and duplicate explanations.
- **CRITICAL_DEBUGGING_TECHNIQUE.typ**: Part titles and one‑line pitfall list; drop full code blocks (replace with "see CRITICAL_DEBUGGING_TECHNIQUE.typ §II.1, §III.2").

**7.4.3 Can drop or drastically shorten**

- **Full code blocks**: Replace with "see `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ` §I.2 (Integrity checks)", etc.
- **Completed phases and old session logs**: Keep only latest status and next open phase.
- **Redundant explanations**: One canonical version and pointers.

**7.4.4 Compressed-session template**

When producing a summary or context pack, structure the compressed remainder as:

```
## Compressed context — [date or session]

### Blockers / current divergence
- [1–3 lines]

### Next steps
- [3–5 concrete items: file, task ID, or command]

### Key paths
- [5–10 paths]

### Pitfalls to recall
- [3–5 one‑liners: PC bytes, FPtoVP 512B, CSTKPTRL/TOPOFSTACK, init SP/FP, byte swap vs XOR]

### Where to resume
- [File, phase, or script]
```

**7.4.5 If AGENTS.md is itself compressed**

In the normal Phase 2 flow, AGENTS.md is re-read in full and not compressed. If in an edge case AGENTS.md must also be compressed, **retain at minimum**:

- Critical Documentation (Must-Read, pointer to specs), submodule policy, C as reference, documentation rules.
- **Common Debugging Gotchas** (full bullets or the 5 one‑liners in 7.4.1).
- **Important Paths** and **Important Constants**.
- **§7 Context Compression and Summarization** (this section, or a pointer to it).

## Common Tasks

### Adding New Opcodes

1. **Cross-reference C implementation** - Study `maiko/src/` for algorithm and semantics
2. **Add opcode definition** to `zaiko/src/vm/dispatch/opcode.zig`
3. **Implement handler** in appropriate module in `zaiko/src/vm/opcodes/`
4. **Add to dispatch switch** in `zaiko/src/vm/dispatch/execution.zig`
5. **Systematic testing** - Use parity debugging techniques from `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`
6. **Update documentation** in `documentation/specifications/instruction-set/opcodes.typ`
7. **Add regression tests** if applicable

### Systematic Debugging Workflow

**CRITICAL**: Follow `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ` for all debugging tasks:

1. **Cross-reference C traces** - Establish baseline with verified C emulator output
2. **Step-by-instruction validation** - Run emulators with `EMULATOR_MAX_STEPS=N`
3. **Debug instrumentation** - Add targeted prints following technique hierarchy
4. **Memory integrity verification** - Check for corruption at critical addresses
5. **Algorithm reverse engineering** - Analyze C code for correct implementation
6. **Hypothesis testing** - Formulate and validate specific bug theories
7. **Documentation updates** - Record findings in specifications and implementations

### Complex Multi-Step Debugging

For issues requiring systematic investigation:

1. **Isolate the problem** - Reproduce with minimal test case
2. **Establish baseline** - Verify C emulator behavior with traces
3. **Apply debugging hierarchy** - Start with cross-referencing, progress through techniques
4. **Document incrementally** - Update findings as they're discovered
5. **Validate fixes** - Ensure no regressions in existing functionality
6. **Commit comprehensively** - Include documentation updates in commits

### File Splitting

When a file exceeds 500 lines:

1. Identify logical groupings
2. Create new modules in appropriate subdirectory
3. Update re-export file to import and re-export from new modules
4. Ensure all imports are updated
5. Test compilation

## Key Concepts

### Memory Management

- **LispPTR**: 32-bit virtual address (DLword offset from Lisp_world, multiply by 2 for bytes)
- **DLword**: 16-bit unsigned integer
- **Virtual Memory**: Complete Lisp address space, allocated based on `process_size`
- **FPtoVP Table**: Maps file page numbers to virtual page numbers
- **Stack**: Part of virtual memory, grows DOWN

### VM Execution

- **PC (Program Counter)**: Byte offset in virtual memory
- **Stack Frame**: 10 DLwords (20 bytes), contains return address, saved registers, local variables
- **Function Header**: Contains `startpc` (byte offset), `na` (arg count), `pv` (param var count)
- **Dispatch Loop**: Fetches, decodes, and executes bytecode instructions

### Common Debugging Gotchas (read before parity fixes)

- **PC units**: traces may show both `PC` (bytes) and `PC/2` (DLword address). When indexing `virtual_memory`, use **byte PC**.
- **FPtoVP units**: FPtoVP "virtual page" values correspond to **512-byte pages** (matches trace `[vpage:...]`), not DLword pages.
- **Byte swap vs XOR addressing**:
  - Sysout pages are typically **32-bit byte-swapped** on load on little-endian hosts.
  - Instruction decode may apply **XOR (`addr ^ 3`)** for BYTESWAP byte access; however, trace logging often prints **raw bytes at PC** (no XOR) to match C.
- **CSTKPTRL/TOPOFSTACK synchronization (CRITICAL)**:
  - The C code's `StackPtrRestore()` macro restores `CSTKPTRL` from `CurrentStackPTR` before each opcode
  - **TOPOFSTACK must be read from memory** (`*(CSTKPTRL - 1)`) after restoring CSTKPTRL, NOT from a cached value
  - This is because operations like GVAR push values (changing TOPOFSTACK), and UNBIND walks CSTKPTRL into the binding stack
  - Without syncing TOPOFSTACK from memory, operations after UNBIND see stale cached values
  - **Fix**: Call `readTopOfStackFromMemory()` after `initCSTKPTRLFromCurrentStackPTR()` in the dispatch loop

### SDL2 Integration

- **Display Region**: DLword array where each bit represents a pixel
- **BitBLT**: Converts bit array to pixels (foreground/background colors)
- **Event Translation**: SDL keycodes → Lisp keycodes via keymap (74 entries)
- **Coordinate Translation**: Window coordinates → Display coordinates (divide by pixel_scale)

## Resources

### Documentation

- **Main README**: `documentation/README.md`
- **Critical Debugging Techniques**: `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`
- **Index**: `documentation/reference/index.typ`
- **Architecture**: `documentation/components/vm-core.typ`
- **Glossary**: `documentation/reference/glossary.typ`

### Specifications

- **Zig Completion Spec**: `specs/005-zig-completion/spec.md`
- **Implementation Plan**: `specs/005-zig-completion/plan.md`
- **Tasks**: `specs/005-zig-completion/tasks.md`
- **Current State**: `specs/005-zig-completion/current-state-analysis.md`

### Implementation Notes

- **Zig Implementation**: `documentation/implementations/zig-implementation.typ`
- **C Implementation**: Reference in `maiko/src/`

## Quick Reference

### Important Paths

- Zig source: `zaiko/src/`
- Zig tests: `zaiko/tests/`
- C reference: `maiko/src/`
- Documentation: `documentation/`
- Specs: `specs/005-zig-completion/`

### Important Constants

- `IFPAGE_KEYVAL`: `0x15e3` (CRITICAL: Must match C implementation)
- `IFPAGE_ADDRESS`: `512` bytes from start of sysout file
- `BYTESPER_PAGE`: `512` bytes (256 DLwords)
- `STK_OFFSET`: `0x00010000` (DLword offset for stack area)
- `FRAMESIZE`: `10` DLwords (20 bytes)

### Build Commands

```bash
# Build Zig emulator
cd zaiko
zig build

# Run Zig emulator
zig build run -- medley/internal/loadups/starter.sysout

# Run tests
zig build test

# Build C emulator (if needed)
cd ../../..
medley/scripts/build/build-c-emulator.sh
```

## Troubleshooting Common Issues

### Memory Corruption
- **Symptom**: Execution diverges from C traces unexpectedly
- **Cause**: Incorrect memory access, bounds violations, or type mismatches
- **Solution**: Use memory integrity verification techniques from `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`

### Stack Corruption
- **Symptom**: Invalid stack pointers or TOPOFSTACK values
- **Cause**: Incorrect stack manipulation, missing synchronization
- **Solution**: Validate stack state after each operation, check CSTKPTRL/TOPOFSTACK sync

### Type Mismatches
- **Symptom**: Compilation errors or runtime crashes
- **Cause**: LispPTR vs native pointer confusion, signed vs unsigned issues
- **Solution**: Always cross-reference C code for correct types and casting

### Performance Issues
- **Symptom**: Execution too slow for practical use
- **Cause**: Inefficient algorithms, excessive memory allocations
- **Solution**: Profile with `EMULATOR_MAX_STEPS`, optimize hot paths

### Submodule Issues
- **Symptom**: Changes not committed or merged properly
- **Cause**: Incorrect submodule workflow
- **Solution**: Enter submodule directory for commits, update pointers from parent

## Best Practices

1. **Always check C implementation first** when implementing new features
2. **Update documentation** before committing (follow `documentation/core/critical_memory.typ`)
3. **Keep files under 500 lines** for better maintainability
4. **Test incrementally** - don't wait until everything is done
5. **Document findings** in `documentation` for future reference
6. **Use descriptive commit messages** with task IDs when applicable
7. **Follow systematic debugging** from `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`
8. **Validate against C traces** before considering implementation complete
9. **Handle edge cases** identified during C code analysis
10. **Profile performance** during development, not just at the end

## Contact & Support

- **Project Documentation**: See `documentation/README.md`
- **Implementation Status**: See `specs/005-zig-completion/current-state-analysis.md`
- **Task Tracking**: See `specs/005-zig-completion/tasks.md`

---

**Last Updated**: 2026-01-20
**Status**: Zig emulator 89.2% complete, systematic debugging methodology established, parity testing framework active
