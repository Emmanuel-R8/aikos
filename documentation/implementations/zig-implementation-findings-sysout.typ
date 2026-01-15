= Zig Implementation Findings - Sysout Loading

*Navigation*: Zig Implementation Findings | Zig Implementation Status | Implementations README

Sysout loading related findings and implementations.

== Zig-Specific Implementation Notes

=== Zig Implementation Details

*IFPAGE Structure*:
- *Zig Location*: `zaiko/src/utils/types.zig:24-95`
- *C Reference*: `maiko/inc/ifpage.h` (non-BIGVM, non-BYTESWAP version used as base)
- *Status*: ✅ Complete with ~100 fields matching C implementation exactly

*FPtoVP Table Loading*:
- *Zig Implementation*: `zaiko/src/data/sysout.zig:loadFPtoVPTable`
- *C Reference*: `maiko/src/ldsout.c:197-250`
- *Status*: ✅ Implemented with BIGVM format support (32-bit entries)

*Page Loading*:
- *Zig Implementation*: `zaiko/src/data/sysout.zig:loadMemoryPages`
- *C Reference*: `maiko/src/ldsout.c:250-350`
- *Status*: ✅ Implemented with sparse page handling and byte-swapping

=== Key Discovery: FPtoVP `virtual_page` is in 512-byte pages

*Last Updated*: 2026-01-15 17:25

When debugging execution parity, the trace may show both:
- `PC` in bytes (e.g. `PC:0x60f130`)
- `PC/2` in DLwords (e.g. `/2:0x307898`)

The sysout FPtoVP mapping uses the byte-addressed virtual page index:

- `vpage = PC_bytes / 512`
- `off  = PC_bytes % 512`

For `starter.sysout`, the first instruction page is:
- `PC_bytes=0x60f130` → `vpage=12408`, `off=0x130`
- `file_page=3003` maps to `vpage=12408` and contains bytes `bf 60 00 00 02 0a 12 c9` at offset `0x130` (before byte-swap), which become `00 00 60 bf c9 12 0a 02` in memory after 32-bit swapping.

*Version Constants*:
- *Zig Implementation*: `zaiko/src/data/sysout.zig:18-19`
- *Constants*: `LVERSION = 21000`, `MINBVERSION = 21001`

=== Zig-Specific Issues Fixed

*IFPAGE_KEYVAL Correction*:
- *Issue*: Initially used `0x12345678` instead of correct `0x15e3`
- *Fix*: Updated in `zaiko/src/data/sysout.zig:14` and `zaiko/src/utils/types.zig:95`
- *Impact*: This was a critical blocker preventing sysout validation from working

*Opcode Conflicts Discovered*:
- Several opcodes in Zig implementation don't exist in C implementation
- *Resolution*: Commented out in dispatch switch statements
- *Details*: See `zig-opcode-findings.md` for Zig-specific conflicts

== Related Documentation

- Zig Implementation Findings - Complete findings index
- VM Execution Findings - VM execution findings
- Opcode Implementation Findings - Opcode implementation findings
