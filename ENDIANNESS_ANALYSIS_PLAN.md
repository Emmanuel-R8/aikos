# Exhaustive Endianness Analysis Plan

**Date**: 2025-01-27 14:30
**Status**: PLANNING PHASE

## Objective

Exhaustively trace the C code to understand:
1. Endianness handling for 16-bit, 32-bit, and 64-bit values
2. How data is read/written
3. How addresses are read
4. How relative address calculations are performed
5. For EACH INSTRUCTION, the impact (if any) of endianness

## Strategy

### Phase 1: Foundation Analysis (Systematic Macro Tracing)

**Goal**: Understand all memory access primitives before analyzing instructions

**Approach**:
1. Trace all memory access macros (`GETBYTE`, `GETWORD`, `GETLONGWORD`, etc.)
2. Document both BYTESWAP and non-BYTESWAP versions
3. Understand XOR addressing pattern
4. Map macro expansion chains (e.g., `Get_BYTE_PCMAC0` → `Get_BYTE` → `GETBYTE`)

**Files to Analyze**:
- `maiko/inc/lsptypes.h` - Core memory access macros
- `maiko/inc/lispemul.h` - Get_BYTE, Get_DLword, Get_Pointer definitions
- `maiko/inc/inlineC.h` - PCMAC access macros
- `maiko/inc/adr68k.h` - Address translation functions

**Output Format**:
```markdown
### Macro: GETBYTE
- **Location**: `maiko/inc/lsptypes.h:370` (non-BYTESWAP), `:565` (BYTESWAP)
- **Non-BYTESWAP**: `(*(base))`
- **BYTESWAP**: `(*(unsigned char *)(3 ^ (UNSIGNED)(base)))`
- **Purpose**: Read 8-bit value
- **XOR Pattern**: `base ^ 3` in BYTESWAP mode
- **Used By**: [list of instructions/macros that use this]
```

### Phase 2: Address Translation Analysis

**Goal**: Understand how LispPTR values are converted to/from native pointers

**Approach**:
1. Trace all address translation functions
2. Document DLword offset vs byte offset handling
3. Identify any inconsistencies or special cases
4. Verify actual behavior vs documentation

**Files to Analyze**:
- `maiko/inc/adr68k.h` - All translation functions
- `maiko/src/xc.c` - Usage patterns (converting native → LispPTR and vice versa)

**Output Format**:
```markdown
### Function: NativeAligned4FromLAddr
- **Location**: `maiko/inc/adr68k.h:53-68`
- **Signature**: `LispPTR *NativeAligned4FromLAddr(LispPTR LAddr)`
- **Current Implementation**: `(void *)((char *)Lisp_world + LAddr)` (byte addressing - TEST CODE)
- **Documented Behavior**: `(void *)(Lisp_world + LAddr)` (DLword arithmetic)
- **Assumption**: LispPTR is DLword offset, so byte offset = LispPTR * 2
- **Usage Patterns**: [list of call sites]
- **Endianness Impact**: None (pointer arithmetic, not data access)
```

### Phase 3: Relative Address Calculation Analysis

**Goal**: Understand how jump/branch offsets are read and applied

**Approach**:
1. Trace all jump instruction implementations
2. Document how offsets are read (byte, signed byte, DLword)
3. Document how offsets are applied to PC
4. Verify XOR addressing usage for offset reading

**Files to Analyze**:
- `maiko/inc/inlineC.h` - JUMPMACRO, FJUMPMACRO, TJUMPMACRO definitions
- `maiko/src/xc.c` - JUMPX, JUMPXX, FJumpx, TJumpx implementations

**Output Format**:
```markdown
### Instruction: JUMPX (opcode 0x0260)
- **Location**: `maiko/src/xc.c:1317`
- **Implementation**: `PCMACL += Get_SBYTE_PCMAC1;`
- **Offset Reading**: 
  - Uses `Get_SBYTE_PCMAC1` → `Get_BYTE(PCMAC + 1)`
  - In BYTESWAP: Reads from `(PCMAC + 1) ^ 3`
- **Offset Application**: Adds signed byte to PC (byte offset)
- **Endianness Impact**: XOR addressing used for reading offset
- **Size**: 1-byte signed offset (-128 to +127 bytes)
```

### Phase 4: Instruction-by-Instruction Analysis

**Goal**: For each instruction, document endianness impact

**Approach**:
1. Group instructions by operand type (no operands, 8-bit, 16-bit, 24/32-bit, memory access)
2. For each instruction:
   - Document opcode and name
   - Trace operand reading (if any)
   - Trace memory access (if any)
   - Document endianness impact
   - Note any special cases

**Files to Analyze**:
- `maiko/src/xc.c` - Main dispatch loop and instruction implementations
- `maiko/inc/inlineC.h` - Instruction macro definitions
- Instruction-specific files (e.g., `maiko/inc/tosfns.h`, `maiko/inc/arithopsdefs.h`)

**Output Format**:
```markdown
## Instruction Category: No Operands

### POP (opcode 0x00)
- **Location**: `maiko/src/xc.c:XXX` or `maiko/inc/inlineC.h:XXX`
- **Operands**: None
- **Memory Access**: Stack pop (native pointer, no byte-swapping)
- **Endianness Impact**: None
- **Notes**: Operates on register/stack pointer, no memory reads of multi-byte data

---

## Instruction Category: 8-bit Operands

### TYPEP (opcode 0x05)
- **Location**: `maiko/src/xc.c:833`
- **Implementation**: `TYPEP(Get_BYTE_PCMAC1);`
- **Operand Reading**:
  - Uses `Get_BYTE_PCMAC1` → `Get_BYTE(PCMAC + 1)`
  - In BYTESWAP: Reads from `(PCMAC + 1) ^ 3` (XOR addressing)
- **Memory Access**: None (operand is immediate value)
- **Endianness Impact**: XOR addressing used for reading operand byte
- **Notes**: Single-byte operand, no multi-byte construction needed

---

## Instruction Category: Memory Access via LispPTR

### GETBASE_N (opcode 0xXX)
- **Location**: `maiko/inc/inlineC.h:246-251`
- **Implementation**: [macro expansion]
- **Operand Reading**: Reads offset N (compile-time constant)
- **Memory Access**:
  1. Converts LispPTR to native: `NativeAligned2FromLAddr((POINTERMASK & TOPOFSTACK) + N)`
  2. Reads word: `GETWORD((DLword *)native_ptr)`
  3. In BYTESWAP: `GETWORD()` uses `base ^ 2` (XOR addressing)
- **Endianness Impact**: 
  - Address translation: DLword arithmetic (no endianness)
  - Data reading: XOR addressing for word access
- **Notes**: Two-stage process: address translation then data access
```

### Phase 5: Special Cases and Edge Cases

**Goal**: Document any special handling or edge cases

**Approach**:
1. Frame structure access (different field order in BYTESWAP)
2. Stack operations
3. Array access (AREF1, ASET1)
4. Bit manipulation (GETBITS, PUTBITS)
5. Function header access
6. Any other complex memory access patterns

**Output Format**:
```markdown
### Special Case: Frame Structure Access

**Location**: `maiko/inc/stack.h` - frameex1 structure definitions
- **Non-BYTESWAP**: Field order: [list]
- **BYTESWAP**: Field order: [list] (different!)
- **Access Pattern**: [how fields are accessed]
- **Endianness Impact**: Fields are byte-swapped during page loading, then accessed via native pointers
- **Critical Note**: Frame bytes are swapped during page load, so runtime access uses native byte order
```

### Phase 6: Verification and Cross-Reference

**Goal**: Verify findings and cross-reference with documentation

**Approach**:
1. Compare findings with ENDIANNESS.md
2. Compare findings with sysout-byte-swapping.md
3. Identify discrepancies
4. Verify against actual execution logs (if available)
5. Document any contradictions or unresolved questions

## Note-Taking Format

### File Structure

```
ENDIANNESS_FINDINGS.md
├── Executive Summary
├── Phase 1: Foundation Analysis
│   ├── Memory Access Macros
│   │   ├── Byte Access (8-bit)
│   │   ├── Word Access (16-bit)
│   │   ├── Longword Access (32-bit)
│   │   └── Pointer Access (24/32-bit)
│   └── Macro Expansion Chains
├── Phase 2: Address Translation
│   ├── LispPTR to Native Pointer
│   ├── Native Pointer to LispPTR
│   └── Address Calculation Patterns
├── Phase 3: Relative Address Calculations
│   ├── Jump Instructions
│   ├── Conditional Jump Instructions
│   └── Offset Reading Patterns
├── Phase 4: Instruction-by-Instruction Analysis
│   ├── No Operands
│   ├── 8-bit Operands
│   ├── 16-bit Operands
│   ├── 24/32-bit Operands
│   ├── Memory Access via LispPTR
│   └── Complex Instructions
├── Phase 5: Special Cases
│   ├── Frame Structure Access
│   ├── Stack Operations
│   ├── Array Access
│   └── Bit Manipulation
├── Phase 6: Verification
│   ├── Comparison with Documentation
│   ├── Discrepancies Found
│   └── Unresolved Questions
└── Implementation Checklist for Zig
```

### Entry Template

For each finding, use this template:

```markdown
### [Category]: [Name]

**Location**: `file:line` (and other relevant locations)
**Opcode**: `0xXX` (if instruction)
**Purpose**: [brief description]

**Non-BYTESWAP Implementation**:
```c
[code snippet]
```

**BYTESWAP Implementation**:
```c
[code snippet]
```

**Endianness Impact**:
- [ ] No impact
- [ ] XOR addressing used: `base ^ N`
- [ ] Manual value construction from bytes
- [ ] Byte-swapping during access
- [ ] Other: [describe]

**Key Details**:
- [Important observation 1]
- [Important observation 2]

**Used By**:
- [Instruction/macro that uses this]
- [Another usage]

**Verification**:
- [ ] Traced through macro expansion
- [ ] Verified against C execution log
- [ ] Cross-referenced with documentation
- [ ] Tested with actual sysout file

**Notes**:
[Any additional observations, contradictions, or questions]
```

## Execution Strategy

### Step 1: Set Up Tracking

1. Create `ENDIANNESS_FINDINGS.md` with structure
2. Create checklist of all instructions (from opcodes.h or xc.c)
3. Set up progress tracking (which instructions analyzed, which pending)

### Step 2: Phase 1 - Foundation

1. Read `maiko/inc/lsptypes.h` completely
2. Document all memory access macros
3. Read `maiko/inc/lispemul.h` completely
4. Document Get_BYTE, Get_DLword, Get_Pointer
5. Read `maiko/inc/inlineC.h` completely
6. Document PCMAC access macros
7. Build macro expansion chain diagram

### Step 3: Phase 2 - Address Translation

1. Read `maiko/inc/adr68k.h` completely
2. Document all translation functions
3. Search `maiko/src/xc.c` for usage patterns
4. Document conversion patterns (native → LispPTR and vice versa)
5. Identify any test code or modifications

### Step 4: Phase 3 - Relative Addresses

1. Search for all jump instructions in `maiko/src/xc.c`
2. Document JUMP, FJUMP, TJUMP, JUMPX, JUMPXX, etc.
3. Trace offset reading mechanism
4. Document offset application

### Step 5: Phase 4 - Instructions

1. Get complete list of instructions from `maiko/src/xc.c` switch statement
2. Group by operand type
3. For each instruction:
   - Find implementation location
   - Trace operand reading
   - Trace memory access (if any)
   - Document endianness impact
   - Mark as complete in checklist

### Step 6: Phase 5 - Special Cases

1. Frame structure access
2. Stack operations
3. Array access
4. Bit manipulation
5. Function headers
6. Any other complex patterns

### Step 7: Phase 6 - Verification

1. Compare with ENDIANNESS.md
2. Compare with sysout-byte-swapping.md
3. Compare with address-translation.md
4. Document discrepancies
5. Create implementation checklist for Zig

## Quality Checks

After each phase:

1. **Completeness**: All relevant files read?
2. **Accuracy**: All code snippets verified?
3. **Consistency**: Findings consistent with each other?
4. **Documentation**: All findings properly documented?
5. **Traceability**: Can trace from instruction to macro to implementation?

Before finalizing:

1. **Coverage**: All instructions analyzed?
2. **Verification**: Cross-referenced with documentation?
3. **Clarity**: Findings clear and actionable?
4. **Implementation Ready**: Zig implementation checklist complete?

## Tools and Commands

**For Tracing**:
- `grep -r "pattern" maiko/inc maiko/src` - Find all occurrences
- `grep -A 5 -B 5 "pattern" file` - Context around matches
- Read files systematically

**For Verification**:
- Compare with existing documentation
- Check execution logs (if available)
- Verify macro expansions manually

## Time Estimate

- Phase 1 (Foundation): 2-3 hours
- Phase 2 (Address Translation): 1 hour
- Phase 3 (Relative Addresses): 1 hour
- Phase 4 (Instructions): 4-6 hours (100+ instructions)
- Phase 5 (Special Cases): 2 hours
- Phase 6 (Verification): 1-2 hours

**Total**: 11-15 hours

## Deliverables

1. **ENDIANNESS_FINDINGS.md** - Complete findings document
2. **Instruction Checklist** - All instructions with endianness impact noted
3. **Implementation Checklist** - Specific items for Zig implementation
4. **Discrepancy Report** - Any contradictions with existing documentation

## Next Steps

1. Review this plan with user
2. Get approval to proceed
3. Execute phases systematically
4. Update findings document incrementally
5. Review with user after each major phase
