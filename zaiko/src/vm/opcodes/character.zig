const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

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
