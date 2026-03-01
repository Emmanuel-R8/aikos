# AI Agent Guidelines for Interlisp Project

**Date**: 2026-02-20
**Purpose**: Guidelines and best practices for AI agents working on this project

## Project Overview

This repository contains the **Interlisp** project, which includes:

- **Maiko - C Implementation in @/maiko_untouched**: Historical baseline with sparse comments. Reference for original implementation details only.
- **Maiko - C Implementation in @/maiko**: Production-ready C implementation with comprehensive documentation. Primary reference for development.
- **Zaiko - Zig Implementation (@/zaiko)**: Alternative implementation of Maiko in Zig (incomplete, ~60-70% coverage)
- **Laiko - Common Lisp Implementation (@/laiko)**: Alternative implementation of Maiko in Common Lisp targeting SBCL (in progress)
- **Taiko - TypeScript Implementation (@/taiko)**: Browser-based implementation using WebGL (in progress, targeting full parity with C)

## Critical Documentation

### Must-Read Files

1. Read ./reports/WORK_STATE.md, then ./reports/STEP_COMPARISON_STATUS.md

1. When **compressing or summarizing context**, follow **§7**: Phase 1 (aggressive compression of tool/command/trace/linter outputs only) is mandatory and first; Phase 2 (re-read AGENTS.md, then compress the remainder) only if further compression is needed.

1. **`documentation/core/critical-memory.typ`** - **CRITICAL**: Rules for documentation updates
   - All documentation improvements MUST be emulator-independent in `documentation/specifications/`
   - Language-specific details go in `documentation/implementations/`
   - **ALWAYS** write documentation using the Typst document format
   - **ALWAYS** update both before committing
   - **ALWAYS** use the command `date` to date entries (when necessary )i as YYYY-mm-dd HH:MM

1. **`documentation/core/critical-debugging-technique.typ`** - **CRITICAL**: Essential debugging techniques and practices

1. **`documentation/README.md`** - Overview of documentation structure

## Project Structure

```
Interlisp/
├── documentation/             # Comprehensive but may overstate Zig completion
│   ├── specifications/        # Technical specs (verify against actual state)
│   ├── implementations/       # Implementation-specific notes
│   └── core/
│       └── critical-memory.typ # ⚠️ MUST READ AND ABIDE BY - Documentation rules
├── maiko/                     # C Implementation (PRODUCTION-READY)
│   ├── src/                   # Enhanced with superior documentation
│   └── inc/                   # Production headers
├── maiko_untouched/           # Historical baseline (sparse comments)
├── zaiko/                     # Zig Implementation (INCOMPLETE)
│   ├── src/                   # 60-70% complete, many placeholders
│   └── tests/                 # Basic test structure
├── laiko/                     # Common Lisp Implementation (IN PROGRESS)
│   ├── src/                   # VM core, memory, I/O in development
│   └── tests/                 # Test structure
├── taiko/                     # TypeScript Browser Implementation (IN PROGRESS)
│   ├── src/                   # WebGL-based emulator
│   ├── web/                   # Browser UI
│   └── tests/                 # Test structure
├── reports/                   # Reports and in markdown format
│   ├── CURRENT_STATUS.md      # Current status of the project
│   ├── WORK_STATE.md          # Work state of the project
│   ├── STEP_COMPARISON_STATUS.md # Step-wise comparison status of the project
│   ├── NEXT_STEPS.md          # Next steps for the project
│   ├── COMPARISON_REPORT.md   # Comparison report of the project
│   └── COMMIT_MESSAGE.md      # Commit message for the project
├── specs/                     # Task tracking (89.2% - inaccurate)
└── medley/                    # Medley Interlisp system
```

## Current Status

**Last Updated**: 2026-02-27

For detailed implementation status of all Maiko emulator implementations, see [`reports/IMPLEMENTATION_STATUS.md`](reports/IMPLEMENTATION_STATUS.md).

**Summary**:

- **C (Maiko)**: Production-ready (94.5% opcode coverage)
- **Zig (Zaiko)**: Incomplete (~60-70% actual coverage, 245 TODO markers)
- **Common Lisp (Laiko)**: In progress (early stage)
- **TypeScript (Taiko)**: In progress (early stage)

**Note**: Previous documentation overstated Zig completion by 20-30%. See consolidated status for accurate details.

## Introspection Module

### C Implementation: Fully Functional

- **Location**: `maiko/src/introspect/`
- **Status**: Fully functional and production-ready
- **Database**: `maiko/maiko_introspect.db` (SQLite)

**Populated Tables**:

- `sessions` - Emulator run sessions
- `events` - Instruction execution events with PC, opcode, TOS
- `build_config` - Build configuration (BIGVM, BIGATOMS, etc.)
- `runtime_config` - Runtime parameters
- `memory_snapshots` - Memory state snapshots
- `memory_writes` - Memory write operations
- `vals_pages` - VALS page tracking
- `gvar_executions` - GVAR opcode execution details (atom_index, value_read)
- `atom_cell_writes` - Atom cell write operations

**Key Documentation**: `documentation/specifications/gvar-introspection-timing.typ`

**Critical Timing Convention**: TOS values in the events table are recorded **AFTER the PREVIOUS instruction**, **BEFORE the current instruction** executes. This means event N shows the TOS value that resulted from instruction N-1, not from instruction N.

**Solved Mystery**: The "0x0E atom 522" issue was a misinterpretation of this timing convention. GVAR for atom 522 correctly pushed 0x140000; the 0x0E was a stale TOS from a previous POP operation.

## Implementation Reality Assessment

### C Codebase: Production Reference

**Comment State Analysis**:

- **maiko/**: Contains superior comprehensive documentation with structured headers, confidence levels, and algorithm explanations
- **maiko_untouched/**: Sparse historical baseline for reference only
- **Recommendation**: Use maiko/ as primary development reference

### Zig Codebase: Development Reality

**Completion Gap**:

- **Task Tracking**: Claims 89.2% completion
- **Actual Implementation**: 60-70% coverage with major gaps
- **Evidence**: 245 TODO/FIXME markers indicate significant immaturity vs 31 in C
- **Critical Gaps**: Floating point operations completely stubbed, graphics operations incomplete, I/O systems missing substantial functionality

**Quality Indicators**:

- Placeholder implementations return without action
- Minimal error handling in incomplete modules
- Insufficient test coverage for production use

## Parity Development Guidance

### Priority Areas for Zig Parity

**Critical Missing Functionality**:

1. **Floating Point Operations**: All opcodes currently stubbed
2. **Graphics Pipeline**: Essential BitBLT and drawing operations missing
3. **I/O Subsystems**: File system, device handling, network operations incomplete
4. **Placeholder Elimination**: Replace non-functional implementations

**Quality Requirements for Parity**:

1. **Comprehensive Testing**: Build test coverage matching C implementation
2. **Error Handling**: Implement robust error management across all modules
3. **Performance**: Achieve comparable execution speed to C reference
4. **Documentation**: Add algorithm explanations for completed implementations

## Working Guidelines

### 1. Code Organization

- **File Size Limit**: Keep files under 500 lines (user preference)
- **Modular Structure**: Split large files into logical modules
- **Naming**: Follow existing conventions (see `zaiko/src/`)

### 1.1 Submodules (CRITICAL)

This repository includes git submodules (notably `maiko_untouched/`, and `medley/`).

- **Default**: Treat submodules as **read-only** unless the user explicitly asks to modify them.
- **Committing**:
  - **Allowed**: Commit inside the `maiko/` directory when changes are made there (e.g. C code comment improvements, trace format updates). Enter the `maiko/` directory to stage and commit.
  - **Prohibited**: Do **not** commit inside `maiko_untouched/`; treat it as read-only historical baseline.
  - Superproject commits may update submodule pointers _only if explicitly intended_.
- **Staging**: Paths inside submodules cannot be staged from the superproject (e.g. `git add maiko_untouched/...` from repo root only updates the submodule pointer). Enter the submodule repo to commit changes there.

### 1.2 Generated Artifacts (never commit)

Avoid committing machine/local outputs that create noisy diffs:

- **Execution logs / traces**: `*_execution_log*.txt`, `c_*_trace*.txt`, `zig_*_trace*.txt`
- **Python caches**: `__pycache__/`, `*.pyc`
- **Build outputs**: Zig `zig-out/`, `**/.zig-cache/`, C/CMake build directories
- **Binary documents**: generated PDFs (commit the Typst source instead)

### 1.3 Markdown Document Storage (CRITICAL)

**REQUIREMENT**: All new Markdown documents (`.md` files) containing intermediary reports or temporary information MUST be stored in the `reports/` directory or its subdirectories.

- **Location**: `reports/` or `reports/[subdirectory]/` (e.g., `reports/parity/`)
- **Examples**:
  - ✅ `reports/MANUAL_VALIDATION.md`
  - ✅ `reports/parity/MANUAL_VALIDATION.md`
  - ✅ `reports/WORK_STATE.md`
  - ❌ `specs/001-multi-impl-parity/MANUAL_VALIDATION.md` (WRONG)
  - ❌ `scripts/MANUAL_VALIDATION.md` (WRONG)
- **Rationale**: Maintains consistent project organization; all markdown reports belong in `reports/` alongside existing status documents
- **Exception**: Specification artifacts in `specs/` directories (e.g., `specs/001-multi-impl-parity/spec.md`, `plan.md`, `tasks.md`) are allowed as they are part of the specification workflow, not reports

**When creating new markdown documents**:

1. Determine if it's a report/documentation → place in `reports/` or `reports/[subdirectory]/`
2. Determine if it's a specification artifact → place in `specs/[feature]/`
3. Determine if it's long term documentation that will be used to generate a PDF → place in `documentation/` in Typst format
4. If unsure, default to `reports/`

### 1.4 File System Access Restrictions (CRITICAL)

**SECURITY REQUIREMENT**: Agents must not access any files or folders outside the project directory containing this AGENTS.md file.

- **Prohibited**: Accessing `/tmp`, `/var`, `/home`, or any system directories
- **Prohibited**: Creating files outside the project workspace
- **Allowed**: Only files and folders within the Interlisp project directory
- **Reason**: Security, isolation, and reproducibility requirements

**Violation will result in immediate termination of the session.**

### 1.5 Command line restrictions (CRITICAL)

**REQUIREMENT**: Agents must not use the command `rm` or `rm -f` or `rm -rf`. Alternatives **must** be sought such as using `find` to locate files with specific file names or extensions. You must always use specific file names or extensions to locate files to delete. **NEVER** use wildcards or patterns to delete files. Always make sure that the files backed up before deletion (preferably with Git). Acceptable exceptions are `.fasl` files.

### 2. Documentation Updates

**CRITICAL**: Before ANY git commit, follow `documentation/core/critical-memory.typ`:

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

- **Use maiko/ as primary reference** - Contains superior documentation to maiko_untouched/
- **Leverage comprehensive headers** - Use confidence levels and testing guidance
- **Preserve documentation quality** - Maintain structured explanations
- **Cross-reference maiko_untouched/** - Only for historical context

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

- **Systematic Debugging**: Follow `documentation/core/critical-debugging-technique.typ` hierarchy
- **Trace Analysis**: Use comparison tools for divergence identification
- **Performance Profiling**: Monitor execution speed during testing

### 4.1 Parity Testing and Debugging

For comprehensive parity testing methodology, workflows, and tools, see [`reports/PARITY_TESTING_GUIDE.md`](reports/PARITY_TESTING_GUIDE.md).

**Key Points**:

- **Canonical Script**: `scripts/compare_emulator_execution.sh` with `EMULATOR_MAX_STEPS` runtime cap
- **Unified Trace Format**: Single-line pipe-delimited format for rapid divergence identification
- **Comparison Tools**: `scripts/compare_unified_traces.awk` (fast), `scripts/compare_unified_traces.py` (detailed)
- **Divergence Analysis**: `scripts/analyze_execution_divergence.py` with LCP skip and resume support
- **Centralized Memory Management**: `zaiko/src/memory/manager.zig` for consistent address translation
- **Systematic Debugging**: Cross-reference C traces, validate incrementally, document findings

**Multi-Implementation Support**:

- C (Maiko): Production-ready reference
- Zig (Zaiko): Incomplete (~60-70% actual coverage)
- Common Lisp (Laiko): In progress
- TypeScript (Taiko): In progress

See [`reports/PARITY_TESTING_GUIDE.md`](reports/PARITY_TESTING_GUIDE.md) for complete documentation including detailed workflow procedures, all testing tools, and best practices.

### 5. Build System

- **Zig**: Uses Zig build system (`build.zig`)
- **C**: Uses traditional Makefile/CMake. Executable built with `./medley/scripts/build/build-c-emulator.sh --display-backend sdl --build-system cmake --force`
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

| Category                                                                                         | Treatment                                                                                                                           | Excerpt? |
| ------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------- | -------- |
| **Tool outputs** (e.g. `run_terminal_cmd`, `grep`, `list_dir`)                                   | **Option B**: Keep a tiny excerpt (first 2 + last 2 lines, or the critical/failing line) plus a 1–2 line summary; drop the rest.    | Yes      |
| **Unix command outputs** (terminal stdout/stderr)                                                | **Option B**: Same — first 2 + last 2 lines, or the error/failure lines if that is the critical part, plus 1–2 line summary.        | Yes      |
| **Tracing/debugging** (execution traces, `std.debug.print`/`printf` dumps, unified trace blocks) | **Option B**: Same; for execution traces, the first-divergence or error line is the critical line.                                  | Yes      |
| **Linter/compiler output**                                                                       | **Do not keep.** Replace with at most a one-line summary (e.g. "zig build: 3 errors", "Linter: 2 issues in X"); drop all raw lines. | No       |

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
- **Session / work state**: `reports/WORK_STATE.md`, `STEP_COMPARISON_STATUS.md` (or the current "session" file). Current blocker, first divergence, next action.
- **Current blocker or first divergence**: 1–3 lines (e.g. "Zig SP/FP init wrong at `zaiko/src/vm/vm_initialization.zig`; C SP=0x02e88 FP=0x307864, Zig differs").
- **Critical pitfalls**: Full "Common Debugging Gotchas" or: PC=byte not DLword; FPtoVP=512‑byte pages; byte swap vs XOR; CSTKPTRL/TOPOFSTACK re-read from memory after restore; VM/stack init (SP/FP from IFPAGE). See `documentation/core/critical-debugging-technique.typ` Part III.
- **Key paths**: `zaiko/src/`, `maiko/src/`, `scripts/compare_emulator_execution.sh`, `documentation/specifications/`, `documentation/implementations/`

**7.4.2 Retain in abbreviated form**

- **Specs**: Phase name, checkpoint status, **next 3–5 open tasks**; drop full body of completed tasks.
- **Documentation**: Section titles and "see `path` for X"; drop long code examples and duplicate explanations.
- **Critical Debugging Technique**: Part titles and one‑line pitfall list; drop full code blocks (replace with "see `documentation/core/critical-debugging-technique.typ` §II.1, §III.2").

**7.4.3 Can drop or drastically shorten**

- **Full code blocks**: Replace with "see `documentation/core/critical-debugging-technique.typ` §I.2 (Integrity checks)", etc.
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

## Documentation Accuracy Warning

For comprehensive task tracking, implementation status, and completion criteria for all emulator implementations, see [`reports/TASK_TRACKING.md`](reports/TASK_TRACKING.md).

### Known Discrepancies

1. **Task Tracking Inaccuracy**: Zig completion shown as 89.2% vs actual 60-70%
2. **Placeholder Not Accounted**: Task completion doesn't reflect numerous TODO implementations
3. **Quality vs Quantity**: Completed tasks may have non-functional implementations
4. **Testing Insufficient**: Completion tracking doesn't assess test coverage

### Verification Requirements

1. **Always inspect source code** before trusting documentation claims
2. **Check for TODO/FIXME markers** as incompleteness indicators
3. **Verify functionality** rather than relying on completion percentages
4. **Use C implementation** as reference for Zig development priorities

## Common Tasks

### Adding New Opcodes

1. **Cross-reference C implementation** - Study `maiko/src/` for algorithm and semantics
2. **Add opcode definition** to `zaiko/src/vm/dispatch/opcode.zig`
3. **Implement handler** in appropriate module in `zaiko/src/vm/opcodes/`
4. **Add to dispatch switch** in `zaiko/src/vm/dispatch/execution.zig`
5. **Systematic testing** - Use parity debugging techniques from `documentation/core/critical-debugging-technique.typ`
6. **Update documentation** in `documentation/specifications/instruction-set/opcodes.typ`
7. **Add regression tests** if applicable

### Systematic Debugging Workflow

**CRITICAL**: Follow `documentation/core/critical-debugging-technique.typ` for all debugging tasks:

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

For comprehensive memory management documentation, see [`documentation/components/memory-management.typ`](documentation/components/memory-management.typ). This document covers:

- Virtual memory architecture and address translation (LispPTR, DLword, FPtoVP table)
- Garbage collection algorithm (reference counting, scan/reclaim phases)
- Storage allocation (cons cells, arrays, memory regions)
- Memory access functions and performance considerations

### VM Execution

- **PC (Program Counter)**: Byte offset in virtual memory
- **Stack Frame**: 10 DLwords (20 bytes), contains return address, saved registers, local variables
- **Function Header**: Contains `startpc` (byte offset), `na` (arg count), `pv` (param var count)
- **Dispatch Loop**: Fetches, decodes, and executes bytecode instructions

### Common Debugging Gotchas

For comprehensive debugging techniques and systematic debugging workflows, see [`documentation/core/critical-debugging-technique.typ`](documentation/core/critical-debugging-technique.typ). This document covers:

- Cross-referencing C traces for baseline establishment
- Step-by-instruction validation with EMULATOR_MAX_STEPS
- Debug instrumentation hierarchy
- Memory integrity verification techniques
- Algorithm reverse engineering from C code
- Project-specific pitfalls (PC units, FPtoVP units, byte swap vs XOR, CSTKPTRL/TOPOFSTACK synchronization, introspection TOS timing)

### SDL2 Integration

- **Display Region**: DLword array where each bit represents a pixel
- **BitBLT**: Converts bit array to pixels (foreground/background colors)
- **Event Translation**: SDL keycodes → Lisp keycodes via keymap (74 entries)
- **Coordinate Translation**: Window coordinates → Display coordinates (divide by pixel_scale)

## Resources

### Documentation

- **Main README**: `documentation/README.md`
- **Critical Debugging Techniques**: `documentation/core/critical-debugging-technique.typ`
- **Index**: `documentation/reference/index.typ`
- **Architecture**: `documentation/components/vm-core.typ`
- **Glossary**: `documentation/reference/glossary.typ`

### Specifications

- **Zig Completion Spec**: `specs/spec.md`
- **Implementation Plan**: `specs/plan.md`
- **Tasks**: `specs/tasks.md`
- **Current State**: `specs/current-state-analysis.md`

### Implementation Notes

- **Zig Implementation**: `documentation/implementations/zig-implementation.typ`
- **C Implementation**: Reference in `maiko/src/`

## Quick Reference

### Important Paths

- Zig source: `zaiko/src/`
- Zig tests: `zaiko/tests/`
- C reference: `maiko/src/`
- C introspection: `maiko/src/introspect/`
- Introspection database: `maiko/maiko_introspect.db`
- Documentation: `documentation/`
- Specs: `specs/`

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
- **Solution**: Use memory integrity verification techniques from `documentation/core/critical-debugging-technique.typ`

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

### For C Implementation (Production Reference)

1. **Use maiko/ as primary reference** - Contains superior documentation
2. **Leverage comprehensive headers** - Use confidence levels and testing guidance
3. **Preserve documentation quality** - Maintain structured explanations
4. **Cross-reference maiko_untouched/** - Only for historical context

### For Zig Development (Parity Focus)

1. **Address TODO markers systematically** - 245 markers indicate critical gaps
2. **Replace placeholder implementations** - Focus on non-functional stubs
3. **Implement missing functional areas** - Priority: floating point, graphics, I/O
4. **Test comprehensively** - Build coverage as implementations are completed
5. **Document algorithms** - Add explanations during implementation, not after

### For Documentation Accuracy

1. **Verify completion claims** - Zig is 60-70% complete, not 89.2%
2. **Cross-reference implementations** - Use C as reference for Zig development
3. **Check for placeholder code** - TODO markers indicate incomplete implementations
4. **Maintain reality checks** - Regular assessment vs documented assumptions

### General Practices

1. **Always check C implementation first** when implementing new features
2. **Update documentation** before committing (follow `documentation/core/critical-memory.typ`)
3. **Keep files under 500 lines** for better maintainability
4. **Test incrementally** - don't wait until everything is done
5. **Document findings** in `documentation` for future reference
6. **Use descriptive commit messages** with task IDs when applicable
7. **Follow systematic debugging** from `documentation/core/critical-debugging-technique.typ`
8. **Validate against C traces** before considering implementation complete
9. **Handle edge cases** identified during C code analysis
10. **Profile performance** during development, not just at the end

## Contact & Support

- **Project Documentation**: See `documentation/README.md`
- **Implementation Status**: See `specs/current-state-analysis.md`
- **Task Tracking**: See `specs/tasks.md`

---

**Last Updated**: 2026-02-20
**Status**: C implementation production-ready with comprehensive documentation and fully functional introspection module; Zig implementation incomplete (60-70% actual coverage); Common Lisp and TypeScript implementations in progress
