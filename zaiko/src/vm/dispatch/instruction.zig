// Re-export file for instruction-related types and functions
// Split from monolithic instruction.zig (742 lines) into smaller modules

const opcode_module = @import("opcode.zig");
const instruction_struct_module = @import("instruction_struct.zig");
const decode_module = @import("decode.zig");
const length_module = @import("length.zig");

// Re-export all public types and functions
pub const Opcode = opcode_module.Opcode;
pub const Instruction = instruction_struct_module.Instruction;
pub const decodeOpcode = decode_module.decodeOpcode;
pub const decodeInstructionFromMemory = decode_module.decodeInstructionFromMemory;
pub const decodeInstruction = decode_module.decodeInstruction;
pub const fetchInstruction = decode_module.fetchInstruction;
pub const fetchInstructionByte = decode_module.fetchInstructionByte;
pub const getInstructionLength = length_module.getInstructionLength;
