// Re-export all opcode handlers from modular files
// This file serves as the main entry point for opcode handlers

// Import all opcode modules
const arithmetic = @import("opcodes/arithmetic.zig");
const bitwise = @import("opcodes/bitwise.zig");
const stack_ops = @import("opcodes/stack_ops.zig");
const function_calls = @import("opcodes/function_calls.zig");
const binding = @import("opcodes/binding.zig");
const control_flow = @import("opcodes/control_flow.zig");
const data_ops = @import("opcodes/data_ops.zig");
const array_ops = @import("opcodes/array_ops.zig");
const comparison = @import("opcodes/comparison.zig");
const type_checking = @import("opcodes/type_checking.zig");
const variable_access = @import("opcodes/variable_access.zig");
const floating_point = @import("opcodes/floating_point.zig");
const misc = @import("opcodes/misc.zig");

// Re-export all handlers with original names
// Arithmetic
pub const handleIPLUS2 = arithmetic.handleIPLUS2;
pub const handleIDIFFERENCE = arithmetic.handleIDIFFERENCE;
pub const handleITIMES2 = arithmetic.handleITIMES2;
pub const handleIQUO = arithmetic.handleIQUO;
pub const handleIREM = arithmetic.handleIREM;
pub const handlePLUS2 = arithmetic.handlePLUS2;
pub const handleDIFFERENCE = arithmetic.handleDIFFERENCE;
pub const handleTIMES2 = arithmetic.handleTIMES2;
pub const handleQUOTIENT = arithmetic.handleQUOTIENT;
pub const handleIPLUS_N = arithmetic.handleIPLUS_N;
pub const handleIDIFFERENCE_N = arithmetic.handleIDIFFERENCE_N;

// Bitwise
pub const handleLOGOR2 = bitwise.handleLOGOR2;
pub const handleLOGAND2 = bitwise.handleLOGAND2;
pub const handleLOGXOR2 = bitwise.handleLOGXOR2;
pub const handleLSH = bitwise.handleLSH;
pub const handleLLSH1 = bitwise.handleLLSH1;
pub const handleLLSH8 = bitwise.handleLLSH8;
pub const handleLRSH1 = bitwise.handleLRSH1;
pub const handleLRSH8 = bitwise.handleLRSH8;

// Stack operations
pub const handlePUSH = stack_ops.handlePUSH;
pub const handlePOP = stack_ops.handlePOP;
pub const handlePOP_N = stack_ops.handlePOP_N;
pub const handleSWAP = stack_ops.handleSWAP;
pub const handleNOP = stack_ops.handleNOP;

// Function calls
pub const handleFN0 = function_calls.handleFN0;
pub const handleFN1 = function_calls.handleFN1;
pub const handleFN2 = function_calls.handleFN2;
pub const handleFN3 = function_calls.handleFN3;
pub const handleFN4 = function_calls.handleFN4;
pub const handleRETURN = function_calls.handleRETURN;

// Binding
pub const handleBIND = binding.handleBIND;
pub const handleUNBIND = binding.handleUNBIND;
pub const handleDUNBIND = binding.handleDUNBIND;

// Control flow
pub const handleJUMP = control_flow.handleJUMP;
pub const handleFJUMP = control_flow.handleFJUMP;
pub const handleTJUMP = control_flow.handleTJUMP;
pub const handleJUMPX = control_flow.handleJUMPX;
pub const handleFJUMPX = control_flow.handleFJUMPX;
pub const handleTJUMPX = control_flow.handleTJUMPX;
pub const handleJUMPXX = control_flow.handleJUMPXX;
pub const handleNFJUMPX = control_flow.handleNFJUMPX;
pub const handleNTJUMPX = control_flow.handleNTJUMPX;

// Data operations
pub const handleCAR = data_ops.handleCAR;
pub const handleCDR = data_ops.handleCDR;
pub const handleCONS = data_ops.handleCONS;
pub const handleRPLACA = data_ops.handleRPLACA;
pub const handleRPLACD = data_ops.handleRPLACD;

// Array operations
pub const handleGETAEL1 = array_ops.handleGETAEL1;
pub const handleGETAEL2 = array_ops.handleGETAEL2;
pub const handleSETAEL1 = array_ops.handleSETAEL1;
pub const handleSETAEL2 = array_ops.handleSETAEL2;
pub const handleAREF1 = array_ops.handleAREF1;
pub const handleAREF2 = array_ops.handleAREF2;
pub const handleASET1 = array_ops.handleASET1;
pub const handleASET2 = array_ops.handleASET2;

// Comparison
pub const handleEQ = comparison.handleEQ;
pub const handleEQL = comparison.handleEQL;
pub const handleLESSP = comparison.handleLESSP;
pub const handleGREATERP = comparison.handleGREATERP;
pub const handleIGREATERP = comparison.handleIGREATERP;
pub const handleEQUAL = comparison.handleEQUAL;

// Type checking
pub const handleNTYPX = type_checking.handleNTYPX;
pub const handleTYPEP = type_checking.handleTYPEP;
pub const handleDTEST = type_checking.handleDTEST;
pub const handleUNWIND = type_checking.handleUNWIND;
pub const handleFIXP = type_checking.handleFIXP;
pub const handleSMALLP = type_checking.handleSMALLP;
pub const handleLISTP = type_checking.handleLISTP;

// Variable access
pub const handleIVAR = variable_access.handleIVAR;
pub const handlePVAR = variable_access.handlePVAR;
pub const handleFVAR = variable_access.handleFVAR;
pub const handleGVAR = variable_access.handleGVAR;
pub const handleACONST = variable_access.handleACONST;
pub const handleSTKSCAN = variable_access.handleSTKSCAN;
pub const handlePVAR_SET = variable_access.handlePVAR_SET;
pub const handlePVARSETPOP = variable_access.handlePVARSETPOP;
pub const handleARG0 = variable_access.handleARG0;
pub const handlePVARX = variable_access.handlePVARX;
pub const handleIVARX = variable_access.handleIVARX;
pub const handleIVARX_ = variable_access.handleIVARX_;
pub const handleFVARX_ = variable_access.handleFVARX_;
pub const handleCOPY = variable_access.handleCOPY;
pub const handleMYARGCOUNT = variable_access.handleMYARGCOUNT;
pub const handleMYALINK = variable_access.handleMYALINK;
pub const handleAPPLYFN = variable_access.handleAPPLYFN;
pub const handleCHECKAPPLY = variable_access.handleCHECKAPPLY;

// Floating point
pub const handleFPLUS2 = floating_point.handleFPLUS2;
pub const handleFDIFFERENCE = floating_point.handleFDIFFERENCE;
pub const handleFTIMES2 = floating_point.handleFTIMES2;
pub const handleFQUOTIENT = floating_point.handleFQUOTIENT;
pub const handleFGREATERP = floating_point.handleFGREATERP;

// Miscellaneous
pub const handleGCREF = misc.handleGCREF;
pub const handleCHARCODE = misc.handleCHARCODE;
pub const handleCHARN = misc.handleCHARN;
pub const handleSLRETURN = misc.handleSLRETURN;
pub const handleMAKENUMBER = misc.handleMAKENUMBER;
pub const handleRPLPTR_N = misc.handleRPLPTR_N;
pub const handleASSOC = misc.handleASSOC;
pub const handleGVAR_ = misc.handleGVAR_;
pub const handleCMLASSOC = misc.handleCMLASSOC;
pub const handleFMEMB = misc.handleFMEMB;
pub const handleCMLMEMBER = misc.handleCMLMEMBER;
pub const handleFINDKEY = misc.handleFINDKEY;
pub const handleCREATECELL = misc.handleCREATECELL;
pub const handleBIN = misc.handleBIN;
pub const handleBOUT = misc.handleBOUT;
pub const handleRESTLIST = misc.handleRESTLIST;
pub const handleMISCN = misc.handleMISCN;
pub const handleRPLCONS = misc.handleRPLCONS;
pub const handleLISTGET = misc.handleLISTGET;
pub const handleEVAL = misc.handleEVAL;
pub const handleENVCALL = misc.handleENVCALL;
pub const handleATOMCELL_N = misc.handleATOMCELL_N;
pub const handleGETBASEBYTE = misc.handleGETBASEBYTE;
pub const handleINSTANCEP = misc.handleINSTANCEP;
pub const handleBLT = misc.handleBLT;
pub const handleMISC10 = misc.handleMISC10;
pub const handlePUTBASEBYTE = misc.handlePUTBASEBYTE;
pub const handleGETBASE_N = misc.handleGETBASE_N;
pub const handleGETBASEPTR_N = misc.handleGETBASEPTR_N;
pub const handleGETBITS_N_FD = misc.handleGETBITS_N_FD;
pub const handleCMLEQUAL = misc.handleCMLEQUAL;
pub const handlePUTBASE_N = misc.handlePUTBASE_N;
pub const handlePUTBASEPTR_N = misc.handlePUTBASEPTR_N;
pub const handlePUTBITS_N_FD = misc.handlePUTBITS_N_FD;
pub const handleADDBASE = misc.handleADDBASE;
pub const handleVAG2 = misc.handleVAG2;
pub const handleHILOC = misc.handleHILOC;
pub const handleLOLOC = misc.handleLOLOC;
pub const handleBASE_LESSTHAN = misc.handleBASE_LESSTHAN;
pub const handleUBFLOAT2 = misc.handleUBFLOAT2;
pub const handleUBFLOAT1 = misc.handleUBFLOAT1;
pub const handleUBFLOAT3 = misc.handleUBFLOAT3;
pub const handleBOXIPLUS = misc.handleBOXIPLUS;
pub const handleBOXIDIFFERENCE = misc.handleBOXIDIFFERENCE;
pub const handleFLOATBLT = misc.handleFLOATBLT;
pub const handleFFTSTEP = misc.handleFFTSTEP;
pub const handleMISC3 = misc.handleMISC3;
pub const handleMISC4 = misc.handleMISC4;
pub const handleUPCTRACE = misc.handleUPCTRACE;
pub const handleCL_EQUAL = misc.handleCL_EQUAL;
pub const handleSIC = misc.handleSIC;
pub const handleSNIC = misc.handleSNIC;
pub const handleSICX = misc.handleSICX;
pub const handleELT = misc.handleELT;
pub const handleNTHCHC = misc.handleNTHCHC;
pub const handleSETA = misc.handleSETA;
pub const handleRPLCHARCODE = misc.handleRPLCHARCODE;
pub const handleTYPECHECK = misc.handleTYPECHECK;
pub const handleBUSBLT = misc.handleBUSBLT;
pub const handleMISC8 = misc.handleMISC8;
pub const handleTYPEMASK_N = misc.handleTYPEMASK_N;
pub const handleMISC7 = misc.handleMISC7;
pub const handleDRAWLINE = misc.handleDRAWLINE;
pub const handleSTORE_N = misc.handleSTORE_N;
pub const handleCOPY_N = misc.handleCOPY_N;
pub const handleRAID = misc.handleRAID;
pub const handleGCONST = misc.handleGCONST;
