// Re-export all miscellaneous opcode handlers from modular files
// This file serves as a re-export hub for all misc opcodes

const gc_ops = @import("gc_ops.zig");
const character = @import("character.zig");
const list_ops = @import("list_ops.zig");
const base_ops = @import("base_ops.zig");
const io_ops = @import("io_ops.zig");
const atom_ops = @import("atom_ops.zig");
const instance_ops = @import("instance_ops.zig");
const graphics_ops = @import("graphics_ops.zig");
const number_ops = @import("number_ops.zig");
const float_ops = @import("float_ops.zig");
const control_misc = @import("control_misc.zig");
const element_ops = @import("element_ops.zig");
const type_misc = @import("type_misc.zig");
const misc_ops = @import("misc_ops.zig");

// Re-export all handlers
pub const handleGCREF = gc_ops.handleGCREF;

pub const handleCHARCODE = character.handleCHARCODE;
pub const handleCHARN = character.handleCHARN;

pub const handleASSOC = list_ops.handleASSOC;
pub const handleCMLASSOC = list_ops.handleCMLASSOC;
pub const handleFMEMB = list_ops.handleFMEMB;
pub const handleCMLMEMBER = list_ops.handleCMLMEMBER;
pub const handleFINDKEY = list_ops.handleFINDKEY;
pub const handleRESTLIST = list_ops.handleRESTLIST;
pub const handleRPLCONS = list_ops.handleRPLCONS;
pub const handleLISTGET = list_ops.handleLISTGET;

pub const handleGETBASEBYTE = base_ops.handleGETBASEBYTE;
pub const handlePUTBASEBYTE = base_ops.handlePUTBASEBYTE;
pub const handleGETBASE_N = base_ops.handleGETBASE_N;
pub const handleGETBASEPTR_N = base_ops.handleGETBASEPTR_N;
pub const handleGETBITS_N_FD = base_ops.handleGETBITS_N_FD;
pub const handlePUTBASE_N = base_ops.handlePUTBASE_N;
pub const handlePUTBASEPTR_N = base_ops.handlePUTBASEPTR_N;
pub const handlePUTBITS_N_FD = base_ops.handlePUTBITS_N_FD;
pub const handleADDBASE = base_ops.handleADDBASE;
pub const handleHILOC = base_ops.handleHILOC;
pub const handleLOLOC = base_ops.handleLOLOC;
pub const handleBASE_LESSTHAN = base_ops.handleBASE_LESSTHAN;

pub const handleBIN = io_ops.handleBIN;
pub const handleBOUT = io_ops.handleBOUT;
pub const handleRAID = io_ops.handleRAID;

pub const handleGVAR_ = atom_ops.handleGVAR_;
pub const handleATOMCELL_N = atom_ops.handleATOMCELL_N;
pub const handleGCONST = atom_ops.handleGCONST;

pub const handleINSTANCEP = instance_ops.handleINSTANCEP;
pub const handleSIC = instance_ops.handleSIC;
pub const handleSNIC = instance_ops.handleSNIC;
pub const handleSICX = instance_ops.handleSICX;

pub const handleBLT = graphics_ops.handleBLT;
pub const handleBUSBLT = graphics_ops.handleBUSBLT;
pub const handleFLOATBLT = graphics_ops.handleFLOATBLT;
pub const handleDRAWLINE = graphics_ops.handleDRAWLINE;

pub const handleMAKENUMBER = number_ops.handleMAKENUMBER;
pub const handleIPLUS_N = number_ops.handleIPLUS_N;
pub const handleIDIFFERENCE_N = number_ops.handleIDIFFERENCE_N;
pub const handleBOXIPLUS = number_ops.handleBOXIPLUS;
pub const handleBOXIDIFFERENCE = number_ops.handleBOXIDIFFERENCE;

pub const handleUBFLOAT1 = float_ops.handleUBFLOAT1;
pub const handleUBFLOAT2 = float_ops.handleUBFLOAT2;
pub const handleUBFLOAT3 = float_ops.handleUBFLOAT3;

pub const handleSLRETURN = control_misc.handleSLRETURN;
pub const handleRPLPTR_N = control_misc.handleRPLPTR_N;
pub const handleEVAL = control_misc.handleEVAL;
pub const handleENVCALL = control_misc.handleENVCALL;
pub const handleJUMPXX = control_misc.handleJUMPXX;
pub const handleNFJUMPX = control_misc.handleNFJUMPX;
pub const handleNTJUMPX = control_misc.handleNTJUMPX;

pub const handleCREATECELL = element_ops.handleCREATECELL;
pub const handleELT = element_ops.handleELT;
pub const handleNTHCHC = element_ops.handleNTHCHC;
pub const handleSETA = element_ops.handleSETA;
pub const handleRPLCHARCODE = element_ops.handleRPLCHARCODE;

pub const handleTYPECHECK = type_misc.handleTYPECHECK;
pub const handleTYPEMASK_N = type_misc.handleTYPEMASK_N;

pub const handleMISCN = misc_ops.handleMISCN;
pub const handleMISC3 = misc_ops.handleMISC3;
pub const handleMISC4 = misc_ops.handleMISC4;
pub const handleMISC7 = misc_ops.handleMISC7;
pub const handleMISC8 = misc_ops.handleMISC8;
pub const handleMISC10 = misc_ops.handleMISC10;
pub const handleUPCTRACE = misc_ops.handleUPCTRACE;
pub const handleCL_EQUAL = misc_ops.handleCL_EQUAL;
pub const handleCMLEQUAL = misc_ops.handleCMLEQUAL;
pub const handleVAG2 = misc_ops.handleVAG2;
pub const handleFFTSTEP = misc_ops.handleFFTSTEP;
pub const handleSTORE_N = misc_ops.handleSTORE_N;
pub const handleCOPY_N = misc_ops.handleCOPY_N;