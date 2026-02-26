= Trace and Logging Formats for Emulator Comparison

*Date*: 2026-01-21
*Status*: Specification (merged from unified-trace, unified-logging, execution-trace)
*Purpose*: Define canonical and alternate trace formats for C/Zig parity debugging and divergence identification

== Overview

Execution traces enable comparison between emulator implementations and systematic parity debugging. This specification defines:

1. *Canonical: Unified pipe-delimited format* — for rapid divergence identification with awk/text tools
2. *Verbose debugging format* — for deep investigation (hex/octal and *2,/2 values)
3. *Legacy column-based format* — see `execution-trace.typ` for column layout and CRITICAL stack/frame notes

*Implementation*: C: `maiko/src/xc.c`; Zig: `zaiko/src/vm/execution_trace.zig`; Laiko: `laiko/src/vm/trace.lisp`.

== Canonical: Unified Pipe-Delimited Format

Single-line, pipe-delimited format for rapid comparison. *Preferred for parity workflows.*

=== File Locations

- *C Emulator*: `c_emulator_unified_trace.txt`
- *Zig Emulator*: `zig_emulator_unified_trace.txt`
- *Laiko (Common Lisp) Emulator*: `lisp_emulator_execution_log.txt` (written to current working directory when tracing is enabled)

*Invocation (all emulators)*: Set `EMULATOR_MAX_STEPS=N` in the environment to limit execution to N instructions and enable trace output. Laiko also supports `-trace <file>` and `-max-steps <N>` on the command line; `-max-steps` overrides the environment when both are set.

=== Line Format

#codeblock(lang: "text", [
LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
])

=== Field Specifications

- *LINE#*: `%6d` — Sequential instruction counter
- *PC*: `0x%06x` — Byte offset from Lisp_world
- *INSTRUCTION*: `%-16s` — Mnemonic (padded to 16 chars)
- *OPCODE*: `0x%02x` — Byte opcode value
- *OPERANDS*: `%-20s` — Decoded operands (padded to 20 chars)
- *REGISTERS*: `r1:0x%04x,r2:0x%04x,r3:0x%02x` — Comma-separated sub-fields for Awk parsing
- *FLAGS*: `Z:%d,N:%d,C:0` — Comma-separated sub-fields (Z=zero, N=negative, C=carry)
- *SP_FP*: `SP:0x%06x,FP:0x%06x` — Comma-separated sub-fields
- *STACK_SUMMARY*: `TOS:0x%08x,N1:0x%08x,N2:0x%08x` — Comma-separated sub-fields
- *MEMORY_CONTEXT*: `@mem:?,vpage:%u,off:0x%03x` — Comma-separated sub-fields (brackets removed)
- *FP_VP_FO_VA*: `FP:0,VP:%u,FO:0x0,VA:0x%06x` — Comma-separated sub-fields
- *BS_MEM*: `BS:RAW,MEM:????????` — Comma-separated sub-fields (byte-swap status, 4 bytes at PC)
- *NOTES*: `%-30s` — e.g. `PC_MISMATCH_CHECK`, `MEM_ZEROS` (padded to 30 chars)

=== Awk Parsing

Parse by *pipe-separated fields* (`awk -F'|'`), then extract sub-fields using comma separator:

```awk
# Extract register r1 value
awk -F'|' '{split($6, regs, ","); split(regs[1], r1, ":"); print r1[2]}'

# Extract TOS value
awk -F'|' '{split($9, stack, ","); split(stack[1], tos, ":"); print tos[2]}'

# Extract SP value
awk -F'|' '{split($8, spfp, ","); split(spfp[1], sp, ":"); print sp[2]}'
```

=== Comparison

Parse by *pipe-separated fields*, not column offsets. Use `awk -F'|'` or equivalent to compare PC, INSTRUCTION, STACK_SUMMARY, etc. Sub-fields within each major field are comma-separated for easy extraction.

== Verbose Debugging Format

Used for deep investigation. Each line includes hex, octal, and *2 and /2 variants to spot DLword/byte and off-by-one-bit errors.

=== File Locations

- *C*: `c_emulator_execution_log.txt`
- *Zig*: `zig_emulator_execution_log.txt`
- *Laiko*: `lisp_emulator_execution_log.txt`

=== Purpose of *2 and /2

- DLword vs byte confusion (DLword = byte / 2)
- Off-by-one-bit in address calculations
- Frame offset and stack depth errors

=== Line Content (summary)

PC (hex/octal, *2, /2), Lisp+ offset, FuncObj+ bytes, [MEM: 8 bytes], INSTR, Stack (D, P, TOS, N[4]) in hex/octal and *2,/2, Frame (FX, FH, PC, NB, FO) in hex/octal and *2,/2.

*Implementation*: C: `maiko/src/xc.c`; Zig: `zaiko/src/vm/execution_trace.zig`.

== Legacy Column-Based Format

Fixed-width column layout; lines can exceed 462 characters. *Parse by stable tokens/fields, not column offsets.*

*Full column layout, CRITICAL stack depth formula, and CRITICAL frame field memory layout*: see `execution-trace.typ` in this directory.

== Related Documentation

- Execution Model — VM execution flow
- Stack Management — Stack structure
- Function Calls — Frame structure
- Critical Debugging Technique — `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`
- Memory — `specifications/memory/address-translation.typ`, `components/memory-management.typ`
