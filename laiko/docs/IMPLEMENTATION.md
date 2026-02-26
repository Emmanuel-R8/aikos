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

- `BYTESPER_PAGE = 512` bytes (256 DLwords)
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
| `maiko-lisp.asd`       | Fixed ASDF system definition                |

### Key Functions Added

| Function            | Purpose                              |
| ------------------- | ------------------------------------ |
| `little-endian-p`   | Detect host endianness               |
| `read-dlword`       | Read 16-bit word (with byte-swap)    |
| `read-lisp-ptr`     | Read 32-bit pointer (with byte-swap) |
| `read-ifpage`       | Parse IFPAGE structure               |
| `byte-swap-page`    | Byte-swap 512-byte page              |
| `read-fptovp-table` | Load FPtoVP mapping                  |
| `load-sysout`       | Complete sysout loading              |

## Trace & Parity

Laiko emits the same unified pipe-delimited trace format as the C and Zig emulators for parity testing.

- *Environment*: `EMULATOR_MAX_STEPS=N` limits execution to N instructions and auto-enables tracing when no explicit trace file is given.
- *CLI*: `-max-steps <N>` and `-trace <file>`; `-max-steps` overrides `EMULATOR_MAX_STEPS` when both are set.
- *Default trace file*: When tracing is auto-enabled (e.g. `EMULATOR_MAX_STEPS` set), output is written to `lisp_emulator_execution_log.txt` in the current working directory (run from repo root when using the comparison script).
- *Comparison script*: From repo root, run `EMULATOR_MAX_STEPS=N ./scripts/compare_emulator_execution.sh [sysout]`; use `--with-laiko` to include Laiko in the run and comparison (see script for C/Zig/Laiko options).

## Known Issues

1. **Bytecode extraction**: Still uses placeholder array in `main.lisp`
2. **FPtoVP offset calculation**: Simplified to non-BIGVM case only
3. **Function header parsing**: Not yet implemented

## Next Steps

1. Complete bytecode extraction from sysout virtual memory
2. ~~Implement unified trace format (Phase 2)~~ — Done; see Trace & Parity above
3. Complete remaining opcode handlers (Phase 3)
4. Integrate SDL3 display backend (Phase 4)
