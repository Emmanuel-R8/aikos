= C Emulator Enhanced Tracing Proposal

*Navigation*: Zig Implementation Status | Implementations README | Main README

*Date*: 2025-12-23 16:43
*Status*: Proposal - Enhanced tracing for memory loading debugging

== Overview

This document proposes additional tracing in the C emulator to complete our understanding of memory loading, address calculations, and execution flow. The enhanced tracing will help systematically debug memory loading issues in the Zig emulator.

== Current Execution Log Format

*File*: `c_emulator_execution_log.txt`
*Location*: `maiko/src/xc.c` (lines 536-807)

*Current Format*:
```
%5d PC: 0x%06x (Lisp_world+0x%06x, FuncObj+%5d bytes) %16s %-40s Stack: D:%5d P:%5d TOS:0x%016lx N:[...] Frame: FX:%5d FH:0x%06x PC:%5d NB:%5d FO:+%5d
```

*Current Fields*:
- Instruction count
- PC (byte offset from Lisp_world)
- FuncObj offset (bytes)
- Instruction bytes (8 bytes hex)
- Instruction name + parameters
- Stack depth, pointer, TOS, next 4 values
- Frame: FX offset, FH (fnheader), PC, NB, FO

*Current Debug Additions* (lines 591-620):
- Virtual page and offset: `[vpage:%u off:0x%03x]` (first 10 instructions)
- Memory address: `@mem:%p` (first instruction)
- Zero detection: `[MEM_ZEROS]` (first 10 instructions)
- PC mismatch check: `[PC_MISMATCH_CHECK vpage:%u]` (for PC 0x307898)

== Proposed Enhancements

=== Enhancement 1: Memory Loading Trace Log

*Purpose*: Trace file page → virtual page mapping and byte-swapping

*File*: `c_emulator_memory_loading_trace.txt`
*Location*: `maiko/src/ldsout.c` (in `sysout_loader` function)

*Format*:
```
=== Memory Loading Trace ===
File: <sysout_filename>
File size: <size> bytes (<num_pages> pages)

=== FPtoVP Table Loading ===
FPtoVP offset: 0x%lx (%lu bytes)
FPtoVP entries: %u
Byte-swap boundary: %u (first %u entries swapped)

=== Page Loading ===
File page %u -> Virtual page %u:
  File offset: 0x%lx (%lu bytes)
  Virtual address: 0x%lx (%lu bytes)
  GETPAGEOK: 0x%04x
  Raw bytes (first 16): <hex dump>
  Swapped bytes (first 16): <hex dump>
  Bytes at PC offset 0x%x: <hex dump>

=== Critical Address Verification ===
PC: 0x%06x
  Virtual page: %u (0x%x)
  Offset in page: %u (0x%x)
  File page mapping: %u -> %u
  File offset: 0x%lx
  Virtual address: 0x%lx
  Memory content at PC: <hex dump>
```

*Implementation*:
- Add `FILE *memory_trace_log` in `sysout_loader`
- Log FPtoVP table loading details
- Log each page load with file/virtual mapping
- Log byte-swapping details
- Log critical addresses (PC 0x307898, frame pages, etc.)

=== Enhancement 2: Enhanced Execution Log Format

*Purpose*: Add memory loading diagnostics to execution log

*File*: `c_emulator_execution_log.txt` (enhanced)
*Location*: `maiko/src/xc.c` (modify existing log format)

*New Fields* (append to existing format):
```
[FP:%u VP:%u FO:0x%lx VA:0x%lx] [BS:%s] [MEM:0x%02x...]
```

Where:
- `FP:%u` = File page number (if known)
- `VP:%u` = Virtual page number
- `FO:0x%lx` = File offset (bytes)
- `VA:0x%lx` = Virtual address (bytes)
- `BS:%s` = Byte-swap status (`SWAPPED`/`RAW`)
- `MEM:0x%02x...` = Memory content at PC (8 bytes hex)

*Conditional Logging*:
- Always log for first 100 instructions
- Always log for PC 0x307898 (known mismatch location)
- Log when memory content changes unexpectedly
- Log when file page mapping changes

*Implementation*:
- Add file page lookup function: `getFilePageForVirtualPage(vpage)`
- Add file offset calculation: `file_page * BYTESPER_PAGE`
- Add byte-swap status tracking
- Modify log format to include new fields

=== Enhancement 3: Address Calculation Trace

*Purpose*: Trace all address calculations step-by-step

*File*: `c_emulator_address_trace.txt`
*Location*: `maiko/src/xc.c` (in execution loop)

*Format*:
```
=== Address Calculation Trace (Instruction %d) ===
PC: 0x%06x

Step 1: PC Analysis
  PC (dec): %u
  PC / 2: %u (0x%x)
  PC * 2: %u (0x%x)

Step 2: Virtual Page Calculation
  Virtual page: %u = PC / %u
  Virtual page / 2: %u
  Virtual page * 2: %u
  Offset in page: %u = PC %% %u
  Offset / 2: %u
  Offset * 2: %u

Step 3: File Page Lookup
  FPtoVP[%u] = %u (virtual page)
  GETPAGEOK[%u] = 0x%04x
  File page: %u
  File page / 2: %u
  File page * 2: %u

Step 4: File Offset Calculation
  File offset: 0x%lx = file_page * %u
  File offset / 2: 0x%lx
  File offset * 2: 0x%lx

Step 5: Virtual Address Calculation
  Virtual address: 0x%lx = virtual_page * %u
  Virtual address / 2: 0x%lx
  Virtual address * 2: 0x%lx

Step 6: Memory Content Verification
  Memory at PC: <hex dump>
  Memory at virtual address: <hex dump>
  Match: %s
```

*Conditional Logging*:
- Always log for first 10 instructions
- Always log for PC 0x307898
- Log when address calculation changes unexpectedly

=== Enhancement 4: FPtoVP Table Lookup Trace

*Purpose*: Trace FPtoVP table lookups and mappings

*File*: `c_emulator_fptovp_trace.txt`
*Location*: `maiko/src/xc.c` (add lookup function)

*Format*:
```
=== FPtoVP Lookup Trace ===
Virtual page: %u
Searching FPtoVP table (%u entries)...

Entry %u: FPtoVP=%u GETPAGEOK=0x%04x
  Match: %s
  File page: %u
  File offset: 0x%lx

Result: File page %u -> Virtual page %u
```

*Implementation*:
- Add `traceFPtoVPLookup(vpage)` function
- Search FPtoVP table for matching virtual page
- Log each entry checked
- Log final mapping result

=== Enhancement 5: Byte-Swapping Trace

*Purpose*: Trace byte-swapping operations

*File*: `c_emulator_byteswap_trace.txt`
*Location*: `maiko/src/ldsout.c` (in byte-swap functions)

*Format*:
```
=== Byte-Swap Trace ===
Operation: <FPtoVP table | Page load>
Location: 0x%p
Size: %u bytes (%u words)

Before swap:
  Bytes: <hex dump>
  As words: <hex dump>

After swap:
  Bytes: <hex dump>
  As words: <hex dump>

Swap pattern: <32-bit longword | 16-bit word>
Boundary: %u (first %u entries swapped)
```

*Implementation*:
- Add tracing to `word_swap_page` function
- Log before/after bytes
- Log swap pattern and boundary
- Log for FPtoVP table and page loads

=== Enhancement 6: Memory Access Trace

*Purpose*: Trace memory reads/writes with address translation

*File*: `c_emulator_memory_access_trace.txt`
*Location*: `maiko/src/xc.c` (around memory access macros)

*Format*:
```
=== Memory Access Trace ===
Access type: <READ | WRITE>
Address: 0x%p (LispPTR: 0x%06x)
Address / 2: 0x%p
Address * 2: 0x%p

Address Translation:
  Virtual page: %u
  Offset in page: %u
  File page: %u
  File offset: 0x%lx

Memory Content:
  Raw bytes: <hex dump>
  XOR-addressed bytes: <hex dump>
  Value read: 0x%x

XOR Addressing:
  Base: 0x%p
  XOR mask: 0x%x
  XOR result: 0x%p
```

*Conditional Logging*:
- Log for PC reads (first 100 instructions)
- Log for stack accesses (first 100)
- Log for frame accesses
- Log when address translation changes

== Implementation Plan

=== Phase 1: Memory Loading Trace

*Tasks*:
1. Add `memory_trace_log` file handle in `sysout_loader`
2. Log FPtoVP table loading details
3. Log each page load with mapping
4. Log byte-swapping for each page
5. Log critical addresses (PC 0x307898)

*Files*:
- `maiko/src/ldsout.c` (modify `sysout_loader`)

=== Phase 2: Enhanced Execution Log

*Tasks*:
1. Add file page lookup function
2. Add file offset calculation
3. Modify execution log format
4. Add conditional logging logic

*Files*:
- `maiko/src/xc.c` (modify execution log)
- `maiko/src/ldsout.c` (add lookup function)

=== Phase 3: Address Calculation Trace

*Tasks*:
1. Add address trace log file
2. Implement step-by-step address calculation
3. Add critical debugging technique application
4. Add conditional logging

*Files*:
- `maiko/src/xc.c` (add address trace)

=== Phase 4: FPtoVP Lookup Trace

*Tasks*:
1. Add FPtoVP lookup trace log
2. Implement lookup function with tracing
3. Log search process
4. Log final mapping

*Files*:
- `maiko/src/xc.c` (add lookup trace)

=== Phase 5: Byte-Swapping Trace

*Tasks*:
1. Add byte-swap trace log
2. Modify `word_swap_page` to trace
3. Log before/after bytes
4. Log swap pattern

*Files*:
- `maiko/src/ldsout.c` (modify byte-swap functions)

=== Phase 6: Memory Access Trace

*Tasks*:
1. Add memory access trace log
2. Trace address translation
3. Trace XOR addressing
4. Log memory content

*Files*:
- `maiko/src/xc.c` (add memory access trace)

== Log File Management

*Proposed Files*:
1. `c_emulator_execution_log.txt` (enhanced existing)
2. `c_emulator_memory_loading_trace.txt` (new)
3. `c_emulator_address_trace.txt` (new)
4. `c_emulator_fptovp_trace.txt` (new)
5. `c_emulator_byteswap_trace.txt` (new)
6. `c_emulator_memory_access_trace.txt` (new)

*File Size Management*:
- Use conditional logging to limit file sizes
- Rotate logs if they exceed size limits
- Provide command-line flags to enable/disable tracing

== Command-Line Flags

*Proposed Flags*:
- `--trace-memory-loading` - Enable memory loading trace
- `--trace-address-calculation` - Enable address calculation trace
- `--trace-fptovp` - Enable FPtoVP lookup trace
- `--trace-byteswap` - Enable byte-swapping trace
- `--trace-memory-access` - Enable memory access trace
- `--trace-all` - Enable all tracing
- `--trace-instructions=N` - Trace first N instructions (default: 100)

== Benefits

1. *Complete Understanding*: See exactly how C emulator loads memory
2. *Systematic Debugging*: Apply critical debugging technique at each step
3. *Address Verification*: Verify all address calculations
4. *Byte-Swap Verification*: Verify byte-swapping matches expectations
5. *Memory Mapping Verification*: Verify file page → virtual page mapping
6. *Cross-Reference Validation*: Multiple independent verification methods

== Success Criteria

- [ ] Memory loading trace shows file page → virtual page mapping
- [ ] Enhanced execution log shows file page and virtual address for each instruction
- [ ] Address calculation trace shows step-by-step calculations with critical debugging
- [ ] FPtoVP lookup trace shows search process and final mapping
- [ ] Byte-swapping trace shows before/after bytes for each swap
- [ ] Memory access trace shows address translation and XOR addressing
- [ ] All traces can be compared with Zig emulator traces

== Next Steps

1. *IMMEDIATE*: Implement memory loading trace (Phase 1)
2. *IMMEDIATE*: Enhance execution log format (Phase 2)
3. *IMMEDIATE*: Add address calculation trace (Phase 3)
4. Run C emulator with enhanced tracing
5. Compare C traces with Zig emulator
6. Identify discrepancies systematically
7. Fix Zig emulator to match C traces

== Related Documentation

- Memory Debugging Systematic - Systematic debugging approach
- Execution Debugging - Current discrepancies
- Critical Debugging Technique - Value analysis method
- Sysout Byte Swapping - Byte-endianness specification
