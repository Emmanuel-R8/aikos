const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");

const VM = stack.VM;

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