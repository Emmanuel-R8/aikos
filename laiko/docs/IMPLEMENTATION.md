# Laiko Implementation Notes

**Date**: 2026-02-06
**Implementation**: Common Lisp (SBCL)

## Sysout Loading Implementation

### IFPAGE Structure

The IFPAGE (Interface Page) structure was corrected to match the C implementation in `maiko/inc/ifpage.h`:

**Critical Fixes**:

1. `IFPAGE_KEYVAL` changed from `#x12345678` to `#x15e3` (16-bit value)
2. IFPAGE reading offset changed from byte 0 to byte 512 (`+ifpage-address+`)
3. IFPAGE struct fields corrected to use `DLword` (16-bit) for most fields

**Source Reference**: `maiko/src/ldsout.c:187` (IFPAGE_ADDRESS), `maiko/inc/ifpage.h:15` (IFPAGE_KEYVAL)

### Byte Swapping

Sysout files are stored in big-endian format. On little-endian hosts (x86/x64), byte-swapping is required:

```lisp
(defun little-endian-p ()
  "Detect host endianness at runtime."
  ...)
```

**Source Reference**: `maiko/src/ldsout.c:156` (BYTESWAP conditional)

### FPtoVP Table

The FPtoVP (File Page to Virtual Page) table maps file page numbers to virtual page numbers:

- Table located at offset: `(fptovpstart - 1) * BYTESPER_PAGE + 2`
- Each entry is a 16-bit DLword
- `GETPAGEOK(entry)` checks if page is present (high byte != 0xFFFF)
- `GETFPTOVP(entry)` returns virtual page number

**Source Reference**: `maiko/src/ldsout.c:804-907`

## Virtual Memory Layout

Virtual memory is organized as an array of 512-byte pages:

- `BYTESPER_PAGE = #x200` bytes (#x100 DLwords)
- Pages stored in big-endian, byte-swapped on little-endian hosts
- Page access via `read-vm-word(address)` helper function

## Code Changes Summary

### Files Modified

| File                   | Changes                                     |
| ---------------------- | ------------------------------------------- |
| `src/data/sysout.lisp` | Complete rewrite for correct sysout loading |
| `src/utils/types.lisp` | Added `little-endian-p` function            |
| `src/package.lisp`     | Updated exports for new functions           |
| `src/vm/stack.lisp`    | Added `fptovp` slot to VM struct            |
| `src/main.lisp`        | Updated to use new sysout loading           |
| `laiko.asd`            | Fixed ASDF system definition                |

### Key Functions Added

| Function            | Purpose                              |
| ------------------- | ------------------------------------ |
| `little-endian-p`   | Detect host endianness               |
| `read-dlword`       | Read 16-bit word (with byte-swap)    |
| `read-lisp-ptr`     | Read 32-bit pointer (with byte-swap) |
| `read-ifpage`       | Parse IFPAGE structure               |
| `byte-swap-page`    | Byte-swap #x200-byte page            |
| `read-fptovp-table` | Load FPtoVP mapping                  |
| `load-sysout`       | Complete sysout loading              |

## Entry Point, Trace & Parity

Laiko emits the same unified pipe-delimited trace format as the C and Zig emulators for parity testing, and the `main.lisp` entry point now mirrors the C/Zig runners more closely.

### CLI and entry point

- `main` inspects `argv` and dispatches:
  - `-info` / `-INFO` → print implementation status and opcode count.
  - `-help` / `--help` → print usage and exit.
  - otherwise: first argument is treated as the sysout path and `run-emulator` is called with the remaining arguments.
- `run-emulator` orchestrates:
  1. **Argument parsing** (`parse-run-args`): extracts `-trace <file>` and `-max-steps <N>` from the CLI.
  2. **Sysout loading** (`load-sysout`): returns `ifpage`, `fptovp`, and `virtual-memory`, logging process size and stack base.
  3. **VM creation** (`create-and-initialize-vm`):
     - Creates a VM with stack/PVAR sizes.
     - Attaches `virtual-memory`, `fptovp`, interrupt state, storage, and GC.
     - Ensures Valspace pages are allocated in virtual memory (VP 3072–3583) when not present in the sysout.
     - Reads the initial FX from virtual memory and derives:
       - Initial PC: `FuncObj = fnheader * 2`, `PC = FuncObj + FX->pc`.
       - Stack pointer (`CurrentStackPTR`) and cached top-of-stack.
  4. **Step limit configuration** (`configure-step-limit`): combines CLI `-max-steps` and `EMULATOR_MAX_STEPS` (the larger wins) and writes the result into `*max-trace-steps*`.
  5. **Tracing** (`configure-tracing`): opens a trace file either from `-trace` or, when only `EMULATOR_MAX_STEPS` is set, auto-enables tracing to `lisp_emulator_execution_log.txt`.
  6. **Dispatch** (`run-dispatch-loop`): initializes opcode maps/handlers, extracts bytecode from virtual memory at the initial PC, logs basic diagnostics, and calls `dispatch`.
  7. **Cleanup**: closes the trace file (if any) and exits with status 0.

This layout keeps the execution pipeline close to the C/Zig implementations, which is critical for cross-emulator parity debugging.

**Invocation notes**:

- `run.sh` lives under `laiko/` and runs SBCL with `laiko:main` as the entry point. When calling it from the repository root you should pass sysout paths relative to `laiko/`, e.g.:

  ```bash
  cd /path/to/Interlisp
  EMULATOR_MAX_STEPS=32 ./laiko/run.sh ../medley/internal/loadups/starter.sysout
  ```

- The parity script `scripts/compare_emulator_execution.sh --with-laiko` sets `EMULATOR_MAX_STEPS` and calls this same entry point, so keeping these conventions consistent is important for reliable instruction-by-instruction comparison.

### Trace configuration and comparison

- _Environment_: `EMULATOR_MAX_STEPS=N` limits execution to N instructions and, when no explicit `-trace` file is given, triggers auto-tracing for parity runs.
- _CLI_: `-max-steps <N>` and `-trace <file>`; `-max-steps` is combined with `EMULATOR_MAX_STEPS` so that the effective limit is `max(N, EMULATOR_MAX_STEPS)`.
- _Default trace file_: When tracing is auto-enabled (e.g., `EMULATOR_MAX_STEPS` set but no `-trace` flag), output is written to `lisp_emulator_execution_log.txt` in the current working directory (typically the repo root when using the comparison script).
- _Comparison script_: From repo root, run `EMULATOR_MAX_STEPS=N ./scripts/compare_emulator_execution.sh [sysout]`; use `--with-laiko` to include Laiko in the run and comparison (the script will invoke C, Zig, and Laiko with consistent limits and trace formats).

## Known Issues

1. **Bytecode extraction**: Still uses placeholder array in `main.lisp`
2. **FPtoVP offset calculation**: Simplified to non-BIGVM case only
3. **Function header parsing**: Not yet implemented

## Next Steps

1. Complete bytecode extraction from sysout virtual memory
2. ~~Implement unified trace format (Phase 2)~~ — Done; see Trace & Parity above
3. Complete remaining opcode handlers (Phase 3)
4. Integrate SDL3 display backend (Phase 4)
