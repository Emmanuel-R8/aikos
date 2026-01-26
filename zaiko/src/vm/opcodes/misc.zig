// Re-export all miscellaneous opcode handlers from modular files
const gc_ops = @import("gc_ops.zig");
const character = @import("character.zig");
const list_ops = @import("list_ops.zig");
const base_ops = @import("base_ops.zig");
const io_ops = @import("io_ops.zig");
const instance_ops = @import("instance_ops.zig");
const graphics_ops = @import("graphics_ops.zig");
const number_ops = @import("number_ops.zig");
const float_ops = @import("float_ops.zig");
const control_flow = @import("control_flow.zig");
const control_misc = @import("control_misc.zig");
const element_ops = @import("element_ops.zig");
const type_misc = @import("type_misc.zig");
const stack = @import("../stack.zig");
const misc_ops = @import("misc_ops.zig");
const atom_ops = @import("atom_ops.zig");
const type_check = @import("../../utils/type_check.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;

// Re-export all handlers with original names
// Arithmetic
// ...
// Jump opcodes (128-143)
pub const handleJUMP0 = control_flow.handleJUMP0;
pub const handleJUMP1 = control_flow.handleJUMP1;
pub const handleJUMP2 = control_flow.handleJUMP2;
pub const handleJUMP3 = control_flow.handleJUMP3;
pub const handleJUMP4 = control_flow.handleJUMP4;
pub const handleJUMP5 = control_flow.handleJUMP5;
pub const handleJUMP6 = control_flow.handleJUMP6;
pub const handleJUMP7 = control_flow.handleJUMP7;
pub const handleJUMP8 = control_flow.handleJUMP8;
pub const handleJUMP9 = control_flow.handleJUMP9;
pub const handleJUMP10 = control_flow.handleJUMP10;
pub const handleJUMP11 = control_flow.handleJUMP11;
pub const handleJUMP12 = control_flow.handleJUMP12;
pub const handleJUMP13 = control_flow.handleJUMP13;
pub const handleJUMP14 = control_flow.handleJUMP14;
pub const handleJUMP15 = control_flow.handleJUMP15;
pub const handleJUMPX = control_flow.handleJUMPX;
pub const handleFJUMPX = control_flow.handleFJUMPX;
pub const handleTJUMPX = control_flow.handleTJUMPX;
pub const handleJUMPXX = control_flow.handleJUMPXX;
pub const handleNFJUMPX = control_flow.handleNFJUMPX;
pub const handleNTJUMPX = control_flow.handleNTJUMPX;

// Missing handlers
pub const handleSLRETURN = control_misc.handleSLRETURN;
pub const handleMAKENUMBER = number_ops.handleMAKENUMBER;
pub const handleRPLPTR_N = control_misc.handleRPLPTR_N;
pub const handleSIC = instance_ops.handleSIC;
pub const handleSICX = instance_ops.handleSICX;
pub const handleGVAR_ = atom_ops.handleGVAR_;
pub const handleCMLASSOC = list_ops.handleCMLASSOC;
pub const handleATOMCELL_N = atom_ops.handleATOMCELL_N;
pub const handleSNIC = instance_ops.handleSNIC;
pub const handleGCREF = gc_ops.handleGCREF;
pub const handleFMEMB = list_ops.handleFMEMB;
pub const handleGETBASEBYTE = base_ops.handleGETBASEBYTE;
pub const handleGCONST = atom_ops.handleGCONST;
pub const handleASSOC = list_ops.handleASSOC;
pub const handleCMLMEMBER = list_ops.handleCMLMEMBER;
pub const handleINSTANCEP = instance_ops.handleINSTANCEP;
pub const handleFINDKEY = list_ops.handleFINDKEY;
pub const handleBLT = graphics_ops.handleBLT;
