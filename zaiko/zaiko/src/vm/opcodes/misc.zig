const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");
const cons = @import("../../data/cons.zig");
const array = @import("../../data/array.zig");
const virtual_memory_module = @import("../../memory/virtual.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

/// GCREF: Garbage collection reference counting
/// Per C implementation: maiko/src/gc.c:OP_gcref
/// alpha operand: ADDREF (0), DELREF (1), or STKREF (2)
/// TopOfStack is the slot address to reference count
/// TODO: Implement full GC hash table lookup (Phase 4)
pub fn handleGCREF(vm: *VM, alpha: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    _ = alpha; // TODO: Use alpha to determine ADDREF/DELREF/STKREF operation

    // For now, just leave TopOfStack alone (stub implementation)
    // Full implementation will call GCLOOKUPV with TopOfStack and alpha
    // and replace TopOfStack with 0 if refcnt != 0
    _ = stack_module.getTopOfStack(vm);
}
// ============================================================================
// String/Character Opcodes
// ============================================================================

/// CHARCODE: Get character code from character object
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops character object from stack, pushes character code
/// Character objects are typically represented as fixnums with character code
pub fn handleCHARCODE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop character object from stack
    const char_obj = try stack_module.popStack(vm);

    // Extract character code from character object
    // In Lisp, characters are typically represented as fixnums
    // Character code is typically in the low 8 bits (or 16 bits for wide chars)
    // For now, treat as 8-bit character code
    const char_code: LispPTR = char_obj & 0xFF;

    // Push character code
    try stack_module.pushStack(vm, char_code);
}

/// CHARN: Create character object from code
/// Per rewrite documentation instruction-set/opcodes.md
/// Pops character code from stack, pushes character object
/// Character code is typically a small integer (0-255 for 8-bit, 0-65535 for 16-bit)
pub fn handleCHARN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Pop character code from stack
    const char_code = try stack_module.popStack(vm);

    // Create character object from code
    // In Lisp, characters are typically represented as fixnums
    // Character code is stored in the low bits
    // For now, treat as 8-bit character code (mask to ensure valid range)
    const char_obj: LispPTR = char_code & 0xFF;

    // Push character object
    try stack_module.pushStack(vm, char_obj);
}
/// SLRETURN: Stack-relative return
/// Per rewrite documentation instruction-set/opcodes.md
/// Returns from function using stack-relative addressing
pub fn handleSLRETURN(vm: *VM) errors.VMError!void {
    // SLRETURN requires:
    // 1. Stack-relative return address
    // 2. Restore previous frame
    // 3. Return to caller

    // TODO: Proper implementation needs:
    // 1. Get return address from stack-relative location
    // 2. Restore previous frame
    // 3. Set PC to return address

    // Placeholder: similar to RETURN but uses stack-relative addressing
    // Will be properly implemented with frame management
    _ = vm;
}
/// MAKENUMBER: Create number object
/// Per rewrite documentation instruction-set/opcodes.md
/// Creates a number object from value on stack
/// For small integers, encodes as fixnum (odd address)
pub fn handleMAKENUMBER(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // Get value from stack
    const value = stack_module.getTopOfStack(vm);

    // Check if value is already a fixnum (odd address)
    if ((value & 1) != 0) {
        // Already a fixnum, no conversion needed
        return;
    }

    // Check if value is NIL (0)
    if (value == 0) {
        // NIL is not a number, keep as-is
        return;
    }

    // Try to encode as fixnum if it's a small integer
    // Fixnums are encoded as (value << 1) | 1
    // Small integers typically fit in 15 bits (signed: -16384 to 16383)
    const value_signed = @as(i32, @bitCast(@as(u32, value)));

    // Check if value fits in fixnum range
    // Fixnum range: typically -16384 to 16383 (15 bits signed)
    if (value_signed >= -16384 and value_signed <= 16383) {
        // Encode as fixnum: (value << 1) | 1
        const fixnum_value = (@as(u32, @bitCast(@as(i32, value_signed))) << 1) | 1;
        stack_module.setTopOfStack(vm, @as(LispPTR, fixnum_value));
        return;
    }

    // For larger integers or floats, would need bignum or float object creation
    // TODO: Implement bignum and float object creation
    // For now, keep value as-is (may be a pointer to number object)
}
/// RPLPTR_N: Replace pointer N
/// Per rewrite documentation instruction-set/opcodes.md
/// Replaces pointer at offset N
pub fn handleRPLPTR_N(vm: *VM, offset: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // RPLPTR_N requires:
    // 1. Pointer value on stack
    // 2. Target address on stack
    // 3. Replace pointer at offset

    // TODO: Proper implementation needs:
    // 1. Pop new pointer value
    // 2. Pop target address
    // 3. Replace pointer at target + offset

    // Placeholder: pop values but don't modify memory
    const new_ptr = try stack_module.popStack(vm);
    const target = try stack_module.popStack(vm);
    _ = new_ptr;
    _ = target;
    _ = offset;
}

/// ASSOC: Association list lookup
/// Per rewrite documentation instruction-set/opcodes.md
/// Looks up key in association list
pub fn handleASSOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // ASSOC requires:
    // 1. Key on stack
    // 2. Association list on stack
    // 3. Look up key-value pair
    // 4. Push value or NIL

    // TODO: Proper implementation needs:
    // 1. Pop key and association list
    // 2. Traverse association list (list of (key . value) pairs)
    // 3. Compare keys using EQ
    // 4. Push value if found, NIL if not

    // Placeholder: return NIL
    const key = try stack_module.popStack(vm);
    const alist = try stack_module.popStack(vm);
    _ = key;
    _ = alist;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GVAR_: Set global variable
/// Per rewrite documentation instruction-set/opcodes.md
/// Sets global variable value
pub fn handleGVAR_(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // GVAR_ requires:
    // 1. Value on stack
    // 2. Atom index
    // 3. Set global variable value

    // TODO: Proper implementation needs:
    // 1. Get value from stack
    // 2. Look up atom in atom table
    // 3. Set DEFCELL value

    // Placeholder: pop value but don't set variable
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = atom_index;
}

/// CMLASSOC: Case-insensitive association list lookup
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCMLASSOC(vm: *VM) errors.VMError!void {
    // Similar to ASSOC but case-insensitive
    // Placeholder: same as ASSOC
    try handleASSOC(vm);
}

/// FMEMB: Fast member test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFMEMB(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // FMEMB requires:
    // 1. Item on stack
    // 2. List on stack
    // 3. Test membership

    // TODO: Proper implementation
    // Placeholder: return NIL
    const item = try stack_module.popStack(vm);
    const list = try stack_module.popStack(vm);
    _ = item;
    _ = list;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// CMLMEMBER: Case-insensitive member test
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCMLMEMBER(vm: *VM) errors.VMError!void {
    // Similar to FMEMB but case-insensitive
    // Placeholder: same as FMEMB
    try handleFMEMB(vm);
}

/// FINDKEY: Find key in association list
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFINDKEY(vm: *VM, key_index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // FINDKEY requires:
    // 1. Association list on stack
    // 2. Key index
    // 3. Find key-value pair

    // TODO: Proper implementation
    // Placeholder: return NIL
    const alist = try stack_module.popStack(vm);
    _ = alist;
    _ = key_index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// CREATECELL: Create cons cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCREATECELL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // CREATECELL requires:
    // 1. CDR value on stack
    // 2. CAR value on stack
    // 3. Allocate cons cell
    // 4. Push cons cell pointer

    // TODO: Proper implementation needs:
    // 1. Pop CDR and CAR values
    // 2. Allocate cons cell from heap
    // 3. Set CAR and CDR
    // 4. Push cons cell pointer

    // Placeholder: similar to CONS but creates new cell
    // For now, just pop values
    const cdr = try stack_module.popStack(vm);
    const car = try stack_module.popStack(vm);
    _ = cdr;
    _ = car;
    try stack_module.pushStack(vm, 0); // Return NIL for now
}

/// BIN: Binary input
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBIN(vm: *VM) errors.VMError!void {
    // BIN requires:
    // 1. Stream on stack
    // 2. Read binary data
    // 3. Push value

    // TODO: Proper implementation needs I/O subsystem
    // Placeholder: return NIL
    _ = vm;
}

/// BOUT: Binary output
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBOUT(vm: *VM) errors.VMError!void {
    // BOUT requires:
    // 1. Value on stack
    // 2. Stream on stack
    // 3. Write binary data

    // TODO: Proper implementation needs I/O subsystem
    // Placeholder: pop values
    const stack_module = @import("../stack.zig");
    const value = try stack_module.popStack(vm);
    const stream = try stack_module.popStack(vm);
    _ = value;
    _ = stream;
}

/// RESTLIST: Rest of list
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleRESTLIST(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // RESTLIST requires:
    // 1. List on stack
    // 2. Skip count elements
    // 3. Push rest of list

    // TODO: Proper implementation needs:
    // 1. Pop list
    // 2. Traverse list count times using CDR
    // 3. Push remaining list

    // Placeholder: return list as-is
    const list = stack_module.getTopOfStack(vm);
    _ = list;
    _ = count;
}

/// MISCN: Miscellaneous N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISCN(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    // MISCN is a catch-all for miscellaneous operations
    // TODO: Proper implementation based on arguments
    // Placeholder: do nothing
    _ = vm;
    _ = arg1;
    _ = arg2;
}

/// RPLCONS: Replace cons
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleRPLCONS(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // RPLCONS requires:
    // 1. New CDR on stack
    // 2. New CAR on stack
    // 3. Cons cell pointer on stack
    // 4. Replace CAR and CDR

    // TODO: Proper implementation needs:
    // 1. Pop new CDR, new CAR, and cons cell pointer
    // 2. Modify cons cell in memory
    // 3. Push cons cell pointer

    // Placeholder: pop values
    const new_cdr = try stack_module.popStack(vm);
    const new_car = try stack_module.popStack(vm);
    const cell_ptr = try stack_module.popStack(vm);
    _ = new_cdr;
    _ = new_car;
    _ = cell_ptr;
    try stack_module.pushStack(vm, 0); // Return NIL for now
}

/// LISTGET: Get from list
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleLISTGET(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // LISTGET requires:
    // 1. Index on stack
    // 2. List on stack
    // 3. Get element at index
    // 4. Push element

    // TODO: Proper implementation needs:
    // 1. Pop index and list
    // 2. Traverse list index times using CDR
    // 3. Get CAR
    // 4. Push element

    // Placeholder: return NIL
    const index = try stack_module.popStack(vm);
    const list = try stack_module.popStack(vm);
    _ = index;
    _ = list;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// EVAL: Evaluate expression
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleEVAL(vm: *VM) errors.VMError!void {
    // EVAL requires:
    // 1. Expression on stack
    // 2. Evaluate expression
    // 3. Push result

    // TODO: Proper implementation needs:
    // 1. Pop expression
    // 2. Call evaluator
    // 3. Push result

    // Placeholder: return expression as-is
    _ = vm;
}

/// ENVCALL: Environment call
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleENVCALL(vm: *VM) errors.VMError!void {
    // ENVCALL requires:
    // 1. Function and arguments on stack
    // 2. Call in environment context
    // 3. Push result

    // TODO: Proper implementation needs:
    // 1. Get function and arguments
    // 2. Set up environment
    // 3. Call function
    // 4. Push result

    // Placeholder: similar to CALL but with environment
    _ = vm;
}

/// ATOMCELL_N: Atom cell N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleATOMCELL_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation needs atom table access
    _ = index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GETBASEBYTE: Get base byte
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBASEBYTE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// INSTANCEP: Instance predicate
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleINSTANCEP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
    stack_module.setTopOfStack(vm, 0); // Return NIL
}

/// BLT: BitBLT operation
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBLT(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// MISC10: Miscellaneous 10
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC10(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// PUTBASEBYTE: Put base byte
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBASEBYTE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
}

/// GETBASE_N: Get base N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    _ = index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GETBASEPTR_N: Get base pointer N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBASEPTR_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    _ = index;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// GETBITS_N_FD: Get bits N FD
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleGETBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    _ = arg1;
    _ = arg2;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// CMLEQUAL: Case-insensitive member equal
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCMLEQUAL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    _ = a;
    _ = b;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// PUTBASE_N: Put base N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBASE_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// PUTBASEPTR_N: Put base pointer N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBASEPTR_N(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// PUTBITS_N_FD: Put bits N FD
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handlePUTBITS_N_FD(vm: *VM, arg1: u8, arg2: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = arg1;
    _ = arg2;
}

/// ADDBASE: Add base
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleADDBASE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result = a + b;
    try stack_module.pushStack(vm, result);
}

/// VAG2: Vector add get 2
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleVAG2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// HILOC: High location
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleHILOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// LOLOC: Low location
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleLOLOC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// IPLUS_N: Integer plus N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIPLUS_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation - add count values
    var sum: LispPTR = 0;
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        const value = try stack_module.popStack(vm);
        sum += value;
    }
    try stack_module.pushStack(vm, sum);
}

/// IDIFFERENCE_N: Integer difference N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleIDIFFERENCE_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    if (count == 0) {
        try stack_module.pushStack(vm, 0);
        return;
    }
    var result = try stack_module.popStack(vm);
    var i: u8 = 1;
    while (i < count) : (i += 1) {
        const value = try stack_module.popStack(vm);
        result -= value;
    }
    try stack_module.pushStack(vm, result);
}

/// BASE_LESSTHAN: Base less than
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBASE_LESSTHAN(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result: LispPTR = if (a < b) 1 else 0;
    try stack_module.pushStack(vm, result);
}

/// UBFLOAT2: Unbox float 2
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUBFLOAT2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper floating-point implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
}

/// UBFLOAT1: Unbox float 1
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUBFLOAT1(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper floating-point implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
}

/// BOXIPLUS: Box integer plus
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBOXIPLUS(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result = a + b;
    try stack_module.pushStack(vm, result);
}

/// BOXIDIFFERENCE: Box integer difference
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBOXIDIFFERENCE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result = a - b;
    try stack_module.pushStack(vm, result);
}

/// FLOATBLT: Float BitBLT
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFLOATBLT(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// FFTSTEP: FFT step
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleFFTSTEP(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// MISC3: Miscellaneous 3
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC3(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// MISC4: Miscellaneous 4
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC4(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// UPCTRACE: Up counter trace
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUPCTRACE(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// CL_EQUAL: Case-insensitive equal
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCL_EQUAL(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper case-insensitive comparison
    const b = try stack_module.popStack(vm);
    const a = try stack_module.popStack(vm);
    const result: LispPTR = if (a == b) 1 else 0; // Simplified
    try stack_module.pushStack(vm, result);
}


/// SIC: Set instance cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSIC(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // SIC requires:
    // 1. Value on stack
    // 2. Set instance cell at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// SNIC: Set named instance cell
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSNIC(vm: *VM, index: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // SNIC requires:
    // 1. Value on stack
    // 2. Set named instance cell at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// SICX: Set instance cell X
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSICX(vm: *VM, index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // SICX requires:
    // 1. Value on stack
    // 2. Set instance cell at index

    // TODO: Proper implementation
    // Placeholder: pop value
    const value = try stack_module.popStack(vm);
    _ = value;
    _ = index;
}

/// ELT: Element
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleELT(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// NTHCHC: Nth character
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNTHCHC(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// SETA: Set array element
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSETA(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = try stack_module.popStack(vm);
    const index = try stack_module.popStack(vm);
    const array_ptr = try stack_module.popStack(vm);
    _ = value;
    _ = index;
    _ = array_ptr;
}

/// RPLCHARCODE: Replace character code
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleRPLCHARCODE(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const new_code = try stack_module.popStack(vm);
    const char_obj = try stack_module.popStack(vm);
    _ = new_code;
    _ = char_obj;
    try stack_module.pushStack(vm, 0); // Return NIL
}

/// TYPECHECK: Type check
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleTYPECHECK(vm: *VM, type_code: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
    _ = type_code;
    stack_module.setTopOfStack(vm, 0); // Return NIL
}

/// BUSBLT: Bus BitBLT
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleBUSBLT(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// MISC8: Miscellaneous 8
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC8(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
}

/// UBFLOAT3: Unbox float 3
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleUBFLOAT3(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper floating-point implementation
    _ = arg;
    const value = stack_module.getTopOfStack(vm);
    _ = value;
}

/// TYPEMASK_N: Type mask N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleTYPEMASK_N(vm: *VM, mask: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    const value = stack_module.getTopOfStack(vm);
    _ = value;
    _ = mask;
    stack_module.setTopOfStack(vm, 0); // Return NIL
}

/// MISC7: Miscellaneous 7
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleMISC7(vm: *VM, arg: u8) errors.VMError!void {
    // TODO: Proper implementation
    _ = vm;
    _ = arg;
}

/// DRAWLINE: Draw line
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleDRAWLINE(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs graphics subsystem
    _ = vm;
}

/// STORE_N: Store N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleSTORE_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        _ = try stack_module.popStack(vm);
    }
}

/// COPY_N: Copy N
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleCOPY_N(vm: *VM, count: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // TODO: Proper implementation
    // Placeholder: duplicate top value count times
    const value = stack_module.getTopOfStack(vm);
    var i: u8 = 0;
    while (i < count) : (i += 1) {
        try stack_module.pushStack(vm, value);
    }
}

/// RAID: RAID operation
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleRAID(vm: *VM) errors.VMError!void {
    // TODO: Proper implementation needs I/O subsystem
    _ = vm;
}

/// JUMPXX: Extended jump XX
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleJUMPXX(vm: *VM) errors.VMError!void {
    // Similar to JUMPX but with different semantics
    // Placeholder: same as JUMPX
    _ = vm;
}

/// NFJUMPX: Not false jump extended
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNFJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // NFJUMPX jumps if TOS is not false (not NIL)
    // Similar to TJUMPX but inverted logic
    const tos = stack_module.getTopOfStack(vm);
    _ = tos;
    _ = offset;
}

/// NTJUMPX: Not true jump extended
/// Per rewrite documentation instruction-set/opcodes.md
pub fn handleNTJUMPX(vm: *VM, offset: i16) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    // NTJUMPX jumps if TOS is not true (is NIL)
    // Similar to FJUMPX but inverted logic
    const tos = stack_module.getTopOfStack(vm);
    _ = tos;
    _ = offset;
}


/// GCONST: Global constant
/// Per rewrite documentation instruction-set/opcodes.md
/// Pushes global constant atom by atom index
pub fn handleGCONST(vm: *VM, atom_index: u16) errors.VMError!void {
    const stack_module = @import("../stack.zig");

    // GCONST requires:
    // 1. Atom table lookup (from atom_index)
    // 2. Get global constant atom object
    // 3. Push atom on stack

    // TODO: Proper implementation needs:
    // 1. Atom table access (needs atom table structure)
    // 2. Global constant atom object creation/retrieval
    // For now, push atom_index as placeholder (will be properly implemented with atom tables)

    // Placeholder: push atom_index as atom pointer (will be properly implemented)
    try stack_module.pushStack(vm, @as(LispPTR, atom_index));
}