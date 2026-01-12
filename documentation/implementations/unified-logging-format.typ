= Unified Execution Logging Format

*Date*: 2026-01-12 19:55
*Status*: ✅ Implemented
*Purpose*: Document the unified logging format for C and Zig emulator execution comparison

== Overview

A unified execution logging format has been implemented in both C and Zig emulators to enable precise comparison of execution traces. The format includes hexadecimal, octal, and bit-shifted values for key fields to identify off-by-one-bit errors.

== Log Format Specification

=== File Location

- C emulator: `c_emulator_execution_log.txt`
- Zig emulator: `zig_emulator_execution_log.txt`

=== Line Format

Each line represents one instruction execution and contains:

```
%5d PC:0x%06lx/0%011lo(*2:0x%06lx/0%011lo /2:0x%06lx/0%011lo) Lisp+0x%06lx/0%011lo FuncObj+%5td [MEM:...] INSTR Stack: D:%5d/0%011o(*2:%5d /2:%5d) P:0x%05x/0%011o(*2:0x%05x /2:0x%05x) TOS:0x%016lx/0%022lo(*2:0x%016lx /2:0x%016lx) N:[...] Frame: FX:%5d/0%011o(*2:%5d /2:%5d) FH:0x%06x/0%011o(*2:0x%06x /2:0x%06x) PC:%5d/0%011o(*2:%5d /2:%5d) NB:%5d/0%011o(*2:%5d /2:%5d) FO:+%5d/0%011o(*2:%5d /2:%5d)
```

=== Field Descriptions

==== PC Information (Columns 7-68)

- `PC:0x%06lx/0%011lo`: PC byte offset in hex and octal
- `(*2:0x%06lx/0%011lo)`: PC * 2 (left-shifted by 1 bit)
- `(/2:0x%06lx/0%011lo)`: PC / 2 (right-shifted by 1 bit)
- `Lisp+0x%06lx/0%011lo`: PC offset from Lisp_world
- `FuncObj+%5td`: PC offset from FuncObj in bytes

==== Memory Bytes (Columns 69-88)

- `[MEM:...]`: 8 bytes of instruction memory in hex

==== Instruction Name (Columns 89-128)

- Instruction mnemonic and parameters (40 characters, left-aligned)

==== Stack Information (Columns 129-298)

- `D:%5d/0%011o`: Stack depth in decimal and octal
- `(*2:%5d /2:%5d)`: Stack depth * 2 and / 2
- `P:0x%05x/0%011o`: Stack pointer offset in hex and octal
- `(*2:0x%05x /2:0x%05x)`: Stack pointer * 2 and / 2
- `TOS:0x%016lx/0%022lo`: Top of stack value in hex and octal
- `(*2:0x%016lx /2:0x%016lx)`: TOS * 2 and / 2
- `N:[...]`: Next 4 stack values in hex and octal

==== Frame Information (Columns 299-461)

- `FX:%5d/0%011o`: CurrentFX offset in decimal and octal
- `(*2:%5d /2:%5d)`: FX offset * 2 and / 2
- `FH:0x%06x/0%011o`: Frame fnheader in hex and octal
- `(*2:0x%06x /2:0x%06x)`: FH * 2 and / 2
- `PC:%5d/0%011o`: Frame PC (byte offset from FuncObj)
- `(*2:%5d /2:%5d)`: Frame PC * 2 and / 2
- `NB:%5d/0%011o`: Frame nextblock in decimal and octal
- `(*2:%5d /2:%5d)`: NB * 2 and / 2
- `FO:+%5d/0%011o`: FuncObj offset from Lisp_world
- `(*2:%5d /2:%5d)`: FO * 2 and / 2

== Implementation Details

=== C Emulator

*Location*: `maiko/src/xc.c:722-1159`

- Logs exactly 1000 instructions
- Uses `fprintf` with unified format strings
- All values shown in hex, octal, and bit-shifted formats

=== Zig Emulator

*Location*: `zaiko/src/vm/execution_trace.zig:200-500`

- Logs up to 1000 instructions (stops on crash)
- Uses `std.fmt.bufPrint` with unified format strings
- Buffer size increased to 1024 bytes to accommodate verbose format
- All values shown in hex, octal, and bit-shifted formats

== Bit-Shifted Values Purpose

The `*2` and `/2` values (left- and right-shifted by 1 bit) help identify:
1. Off-by-one-bit errors in address calculations
2. DLword vs byte offset confusion (DLword = byte / 2)
3. Incorrect bit manipulation in PC advancement
4. Frame offset calculation errors

== Comparison Workflow

1. Generate logs from both emulators:
   ```bash
   # C emulator
   ./maiko/linux.x86_64/ldesdl medley/internal/loadups/starter.sysout
   
   # Zig emulator
   ./zaiko/zig-out/bin/zaiko medley/internal/loadups/starter.sysout
   ```

2. Compare logs line by line:
   ```bash
   diff -u c_emulator_execution_log.txt zig_emulator_execution_log.txt
   ```

3. Identify first divergence point
4. Analyze bit-shifted values to find root cause

== Example Log Line

```
    1 PC:0x60f130/000030170460(*2:0xc1e260/000060361140 /2:0x307898/000014074230) Lisp+0x60f130/000030170460 FuncObj+  104 000060bfc9120a02    [vpage:12408 off:0x130] @mem:0x7fd5e860f130 [FP:65535 VP:12408 FO:0x0 VA:0x60f000] [MEM:000060bfc9120a02]POP                                     Stack: D: 5956/000000013504(*2:11912 /2: 2978) P:0x02e88/000000027210(*2:0x05d10 /2:0x01744) TOS:0x0000000000000000/00000000000000000000000(*2:0x0000000000000000 /2:0x0000000000000000) N:[0x0000000e/000000000016 0xfffe0002/037777400002 0xfffe0000/037777400000 0x006de388/000033361610]Frame: FX:11890/000000027162(*2:23780 /2: 5945) FH:0x307864/000014074144(*2:0x60f0c8/000030170310 /2:0x183c32/000006036062) PC:  104/000000000150(*2:  208 /2:   52) NB:11914/000000027212(*2:23828 /2: 5957) FO:+6353096/000030170310(*2:12706192 /2:3176548)
```

== Related Documentation

- C Emulator PC Advancement Fix - Bug fix that enabled logging
- Zig GVAR BIGATOMS Implementation - Opcode implementation matching
- Execution Comparison Analysis - Comparison results
