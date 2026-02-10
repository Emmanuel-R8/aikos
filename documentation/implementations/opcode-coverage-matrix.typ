= Opcode Coverage Matrix

*Date*: 2026-02-09
*Status*: Active - Continuously Updated
*Purpose*: Track opcode implementation status across all emulator implementations

== Overview

This matrix tracks which opcodes are implemented, tested, and verified in each emulator implementation. Status codes:

- ✅ *Implemented*: Opcode handler exists and is functional
- ⚠️ *Diverges*: Implemented but produces different results than C reference
- ❌ *Missing*: Not yet implemented
- 🔍 *Untested*: Implemented but not yet verified against C reference
- 📝 *Stub*: Placeholder implementation exists

== Implementation Status Legend

| Status | Meaning |
|--------|---------|
| ✅ | Implemented and verified |
| ⚠️ | Implemented but diverges |
| ❌ | Not implemented |
| 🔍 | Implemented, untested |
| 📝 | Stub/placeholder |

== Opcode Coverage Table

// This table will be populated by automated analysis tools
// Format: Opcode Name | Opcode Hex | C | Zig | TypeScript | Lisp

#table(
  columns: 6,
  [Opcode Name], [Hex], [C], [Zig], [TypeScript], [Lisp],
  [RECLAIMCELL], [0xbf], [✅], [✅], [🔍], [❌],
  [POP], [0x60], [✅], [✅], [🔍], [❌],
  [GVAR], [0x12], [✅], [✅], [🔍], [❌],
  [FN2], [0x12], [✅], [✅], [🔍], [❌],
  // More entries will be added by automated tools
)

== Coverage Statistics

*Total Opcodes*: TBD (will be populated by analysis)

=== By Implementation

- *C (Reference)*: TBD opcodes implemented
- *Zig*: TBD opcodes implemented, TBD verified, TBD diverge
- *TypeScript*: TBD opcodes implemented, TBD verified
- *Lisp*: TBD opcodes implemented, TBD verified

=== By Category

- *Stack Operations*: TBD
- *Arithmetic*: TBD
- *Memory Operations*: TBD
- *Control Flow*: TBD
- *List Operations*: TBD
- *Type Checking*: TBD
- *Graphics*: TBD
- *I/O*: TBD
- *Floating Point*: TBD

== Related Documentation

- Opcode Specifications: `documentation/specifications/instruction-set/opcodes.typ`
- Parity Divergences: `documentation/implementations/parity-divergences.typ`
- Implementation Status: Individual implementation Typst files

== Notes

This matrix is automatically updated by parity comparison tools. Manual entries are temporary until automated analysis completes.

*Last Updated*: 2026-02-09
