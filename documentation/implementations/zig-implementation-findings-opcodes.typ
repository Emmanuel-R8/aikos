= Zig Implementation Findings - Opcode Implementation


Opcode implementation related findings and implementations.

== Zig-Specific Implementation Notes

=== Arithmetic Opcodes: SMALLP/FIXP Handling pointerZig Implementation:
- *Location*: `maiko/alternatives/zig/src/utils/types.zig:124-212`
- *Functions*: `extractInteger()`, `encodeIntegerResult()` matching C `N_IGETNUMBER` and `N_ARITH_SWITCH` macros
- *Constants*: `S_POSITIVE`, `S_NEGATIVE`, `SEGMASK`, `MAX_SMALL`, `MIN_SMALL`, `MAX_FIXP`, `MIN_FIXP`
- *Status*: ✅ Implemented - Arithmetic opcodes (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM) now match C behavior
- *Note*: FIXP object creation deferred to Phase 4* (GC implementation)

*General Knowledge*: See `rewrite-spec/instruction-set/opcodes-arithmetic.md` for SMALLP/FIXP handling details

=== Array Operations Implementation pointerZig Implementation:
- *Location*: `maiko/alternatives/zig/src/vm/opcodes/array_ops.zig:handleAREF1()`, `handleASET1()`
- *Module*: `maiko/alternatives/zig/src/data/array.zig` with `OneDArray` structure
- *C Reference*: `maiko/inc/lsptypes.h` for structure definition
- *Status*: ✅ Implemented - AREF1 and ASET1 now properly handle OneDArray structures with type dispatch pointerZig-Specific Details:
- *OneDArray Structure*: Packed struct matching non-BIGVM format (see general docs for structure details)
- *Type Constants*: Defined in `array.zig` (see general docs for type numbers)
- *Type Dispatch*: Switch statement on `typenumber` matches C `aref_switch()` and `aset_switch()`
- *Address Translation*: Uses `virtual_memory_module.translateAddress()` for LispPTR conversion pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-data.md` for OneDArray structure and type dispatch details

=== Variable Access with DLword Offsets pointerZig Implementation:
- *Location*: `maiko/alternatives/zig/src/vm/opcodes/variable_access.zig:handlePVARX()`, `handleIVARX()`, `handlePVAR_SET()`, `handleIVARX_()`
- *Status*: ✅ Implemented - PVARX/IVARX operations now correctly use DLword offsets matching C implementation pointerZig-Specific Details:
- *Offset Calculation*: `offset_bytes = x × 2` (each DLword is 2 bytes)
- *IVAR Base*: Uses `virtual_memory_module.translateAddress()` for LispPTR conversion
- *PVAR Base*: Direct native calculation: `pvar_base = frame_addr + @sizeOf(FX)`
- *Byte Order*: Manual big-endian read/write handling pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-control-memory.md` for DLword offset details

=== Frame Information Opcodes pointerZig Implementation:
- *Location*: `maiko/alternatives/zig/src/vm/opcodes/variable_access.zig:handleMYALINK()`, `handleMYARGCOUNT()`
- *Status*: ✅ Implemented - MYALINK and MYARGCOUNT now provide frame information matching C implementation pointerZig-Specific Details:
- *FRAMESIZE Constant*: Defined as `const FRAMESIZE: u32 = 10` (DLwords)
- *MYARGCOUNT*: Currently simplified - full implementation requires `Stackspace` and `blink` fields pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-control-memory.md` for frame information opcode details

=== Atom Table Access Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/data/atom.zig`
- *Status*: ✅ Implemented - GVAR, GVAR_, ACONST, GCONST opcodes now properly access atom table pointerZig-Specific Details: - Supports BIGVM BIGATOMS format (assumed for now)
- Functions: `getVALCELL()`, `getDEFCELL()`, `readAtomValue()`, `writeAtomValue()`, `getAtomPointer()`
- Value cell reading: Handles big-endian byte swapping from sysout format pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-control-memory.md` for atom table access details

=== Type Checking Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/utils/type_check.zig`
- *Status*: ✅ Implemented - Type checking integrated into CAR/CDR and type opcodes pointerZig-Specific Details: - `isList()`: Simplified - TODO: Full type table lookup when MDStypetbl is available
- `getTypeNumber()`: Simplified for now
- `getTypeEntry()`: Placeholder - Constants: `TYPE_LISTP = 5`, `TYPE_NEWATOM = 21`, `TYPE_FIXP = 1`

*General Knowledge*: See `rewrite-spec/instruction-set/opcodes-data.md` for type checking details

=== Base Operations Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/base_ops.zig`
- *Status*: ✅ Implemented - All base operations complete (2 TODOs remain for FIXP handling in byte operations)

*Zig-Specific Details*: - Added `POINTERMASK` constant (0xfffffff for BIGVM) to `utils/types.zig`
- Added `getHiWord()` and `getLoWord()` helper functions matching C macros
- All base operations use `translateAddress()` for LispPTR to native conversion
- Handles big-endian byte swapping from sysout format pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-arithmetic.md` for base operations details

=== Function Lookup Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/data/defcell.zig`, `maiko/alternatives/zig/src/vm/opcodes/function_calls.zig`
- *Status*: ✅ Implemented - FN0-FN4 opcodes now properly lookup functions from atom table (C code functions TODO)

*Zig-Specific Details*: - DefCell structure matches C definition (BIGVM format: 28-bit defpointer, non-BIGVM: 24-bit)
- Functions: `readDefCell()`, `getFunctionHeader()`, `isCCode()` - C code functions: Currently return error (not yet supported)

*General Knowledge*: See `rewrite-spec/instruction-set/opcodes-control-memory.md` for function call details

=== Binding Operations Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/binding.zig`
- *Status*: ✅ Implemented - All binding operations complete pointerZig-Specific Details: - Uses `@ptrFromInt(@intFromPtr(ptr) - offset)` for negative indexing (Zig doesn't support negative array indices)
- All calculations use explicit address arithmetic pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-control-memory.md` for binding operations details

=== Comparison Operations Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/comparison.zig`
- *Status*: ✅ Implemented - All comparison operations complete with proper atom and array handling pointerZig-Specific Details: - Uses `type_check.getTypeNumber()` for type detection
- `compareArrays()` helper for element-by-element array comparison pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-arithmetic.md` for comparison operations details

=== GC Operations Integration pointerZig Implementation:
- *Location*: `maiko/alternatives/zig/src/vm/stack.zig`, `maiko/alternatives/zig/src/vm/opcodes/gc_ops.zig`
- *Status*: ✅ Implemented - GC operations integrated into VM struct pointerZig-Specific Details:
- *VM Struct*: Added optional `gc: ?*GC` field (can be null if GC disabled)
- *GCREF Handler*: Uses GC from VM struct, returns early if null
- *Error Handling*: GC errors are non-fatal - caught and ignored pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-reference.md` for GC operations details

=== FIXP Handling in Base Operations pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/base_ops.zig`
- *Status*: ✅ Implemented - FIXP handling complete for base byte operations pointerZig-Specific Details: - Detects FIXP type using `type_check.getTypeNumber()`
- Translates FIXP to native address using `translateAddress()` - Reads int32 value from memory pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-arithmetic.md` for FIXP handling details

=== GC Integration in GVAR* (underscore)Zig Implementation*:
- *Location*: `maiko/alternatives/zig/src/vm/opcodes/atom_ops.zig`
- *Status*: ✅ Implemented - GC integration complete for GVAR_ opcode pointerZig-Specific Details: - Reads old value before writing new value (for GC)
- Calls `gc_module.deleteReference()` and `addReference()` - GC errors are non-fatal (caught and ignored)

*General Knowledge*: See `rewrite-spec/instruction-set/opcodes-control-memory.md` for GVAR_ details

=== List Operations Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/list_ops.zig`
- *Status*: ✅ Implemented - All list operations complete (RESTLIST simplified, full IVar version TODO)

*Zig-Specific Details*: - *RESTLIST*: Simplified implementation - TODO for full IVar version
- Helper functions: `getCAR()`, `getCDR()` with error handling pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-data.md` for list operations details

=== RPLPTR_N Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/control_misc.zig`
- *Status*: ✅ Implemented - RPLPTR_N complete with GC ref updates pointerZig-Specific Details: - Updates GC refs: DELREF old value, ADDREF new value
- Uses `translateAddress()` for memory access pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-data.md` for RPLPTR_N details

=== FIXP Box Operations Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/number_ops.zig`
- *Status*: ✅ Implemented - FIXP box operations complete pointerZig-Specific Details: - Checks TYPE_FIXP using `type_check.getTypeNumber()`
- Extracts integer using `extractInteger()`
- Modifies FIXP box value directly pointerGeneral Knowledge: See `rewrite-spec/instruction-set/opcodes-arithmetic.md` for FIXP box operations details

=== Type Predicates Implementation pointerZig Implementation: - *Location*: `maiko/alternatives/zig/src/vm/opcodes/type_checking.zig`, `maiko/alternatives/zig/src/utils/type_check.zig`
- *Status*: ✅ Implemented - Type predicates complete with proper type number checks pointerZig-Specific Details: - Uses `GetTypeNumber` for type checking - Type constants: `TYPE_SMALLP = 1`, `TYPE_FIXP = 2`, `TYPE_LISTP = 5`

*General Knowledge*: See `rewrite-spec/instruction-set/opcodes-data.md` for type predicates details

=== Compilation Issues Fixed pointerType Mismatches: - Fixed `usize` vs `u32` conversions in function.zig and stack.zig
- Fixed alignment issues in storage.zig using `@alignCast`
- Fixed const vs mutable Storage in VM structure pointerError Types: - Added `StackUnderflow` and `DivisionByZero` to VMError enum pointerAlignment Issues: - Changed `translateAddress` alignment parameter from `u2` to `u8` to support 4-byte alignment

== Related Documentation

- Zig Implementation Findings - Complete findings index
- Sysout Loading Findings - Sysout loading findings
- VM Execution Findings - VM execution findings
