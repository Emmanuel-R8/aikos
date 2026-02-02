# File Split Plan - opcodes.zig and dispatch.zig

**Date**: 2025-01-27
**Purpose**: Detailed plan for splitting large files into manageable modules

## Overview

### Target Files
1. `zaiko/src/vm/opcodes.zig` (2,820 lines) → 13 modules
2. `zaiko/src/vm/dispatch.zig` (1,150 lines) → 3 modules

### Goal
- Each file < 500 lines (user preference)
- Logical grouping by functionality
- Maintain compilation and functionality

## opcodes.zig Split Plan

### Module Structure

```
zaiko/src/vm/opcodes/
├── arithmetic.zig      (~200 lines) - Integer and general arithmetic
├── bitwise.zig         (~130 lines) - Bitwise operations
├── stack_ops.zig        (~65 lines) - Stack manipulation
├── function_calls.zig   (~85 lines) - Function calls and returns
├── binding.zig          (~65 lines) - Binding operations
├── control_flow.zig      (~55 lines) - Jump operations
├── data_ops.zig        (~270 lines) - CAR, CDR, CONS, RPLACA, RPLACD
├── array_ops.zig       (~135 lines) - Array operations
├── comparison.zig      (~150 lines) - Comparison operations
├── type_checking.zig   (~150 lines) - Type checking
├── variable_access.zig (~210 lines) - Variable access
├── floating_point.zig   (~80 lines) - Floating point operations
└── misc.zig            (~800 lines) - Miscellaneous operations
```

### New opcodes.zig (Main File)

```zig
// Re-exports all opcode handlers
pub const arithmetic = @import("opcodes/arithmetic.zig");
pub const bitwise = @import("opcodes/bitwise.zig");
pub const stack_ops = @import("opcodes/stack_ops.zig");
pub const function_calls = @import("opcodes/function_calls.zig");
pub const binding = @import("opcodes/binding.zig");
pub const control_flow = @import("opcodes/control_flow.zig");
pub const data_ops = @import("opcodes/data_ops.zig");
pub const array_ops = @import("opcodes/array_ops.zig");
pub const comparison = @import("opcodes/comparison.zig");
pub const type_checking = @import("opcodes/type_checking.zig");
pub const variable_access = @import("opcodes/variable_access.zig");
pub const floating_point = @import("opcodes/floating_point.zig");
pub const misc = @import("opcodes/misc.zig");

// Re-export all handlers with original names
pub const handleIPLUS2 = arithmetic.handleIPLUS2;
pub const handleIDIFFERENCE = arithmetic.handleIDIFFERENCE;
// ... (all other handlers)
```

### Line Range Mapping

**arithmetic.zig** (lines 28-175, 176-253, 2292-2322):
- handleIPLUS2 (28-58)
- handleIDIFFERENCE (60-87)
- handleITIMES2 (89-116)
- handleIQUO (118-145)
- handleIREM (147-174)
- handlePLUS2 (180-197)
- handleDIFFERENCE (199-214)
- handleTIMES2 (216-231)
- handleQUOTIENT (233-253)
- handleIPLUS_N (2292-2304)
- handleIDIFFERENCE_N (2306-2322)

**bitwise.zig** (lines 255-381):
- handleLOGOR2 (259-272)
- handleLOGAND2 (274-287)
- handleLOGXOR2 (289-302)
- handleLSH (304-333)
- handleLLSH1 (335-345)
- handleLLSH8 (347-357)
- handleLRSH1 (359-369)
- handleLRSH8 (371-381)

**stack_ops.zig** (lines 383-444):
- handlePUSH (388-394)
- handlePOP (396-401)
- handlePOP_N (403-421)
- handleSWAP (423-436)
- handleNOP (438-444)

**function_calls.zig** (lines 446-527):
- handleFN0 (458-460)
- handleFN1 (462-464)
- handleFN2 (466-468)
- handleFN3 (470-472)
- handleFN4 (474-476)
- handleFN (478-511) - internal
- handleRETURN (513-527)

**binding.zig** (lines 529-593):
- handleBIND (533-559)
- handleUNBIND (561-576)
- handleDUNBIND (578-593)

**control_flow.zig** (lines 595-654):
- handleJUMP (598-602)
- handleFJUMP (608-616)
- handleTJUMP (618-626)
- handleJUMPX (628-634)
- handleFJUMPX (636-644)
- handleTJUMPX (646-654)
- handleJUMPXX (2726-2732)
- handleNFJUMPX (2734-2743)
- handleNTJUMPX (2745-2754)

**data_ops.zig** (lines 656-922):
- handleCAR (660-715)
- handleCDR (717-793)
- handleCONS (795-841)
- handleRPLACA (843-888)
- handleRPLACD (890-922)

**array_ops.zig** (lines 924-1057):
- handleGETAEL1 (928-959)
- handleGETAEL2 (961-991)
- handleSETAEL1 (993-1024)
- handleSETAEL2 (1026-1057)
- handleAREF2 (2756-2776)
- handleASET2 (2778-2800)

**comparison.zig** (lines 1059-1205, 1706-1794):
- handleEQ (1063-1074)
- handleEQL (1076-1096)
- eqlDeep (1098-1160) - helper
- handleLESSP (1162-1175)
- handleGREATERP (1177-1190)
- handleIGREATERP (1192-1205)
- isNil, isFixnum, isConsCell (1709-1722) - helpers
- equalRecursive (1724-1776) - helper
- handleEQUAL (1778-1794)

**type_checking.zig** (lines 1207-1352):
- handleNTYPX (1211-1223)
- handleTYPEP (1225-1245)
- handleDTEST (1247-1268)
- handleUNWIND (1270-1286)
- getValueType (1288-1317) - helper
- handleFIXP (1319-1328)
- handleSMALLP (1330-1341)
- handleLISTP (1343-1352)

**variable_access.zig** (lines 1398-1605, 2421-2543):
- handleIVAR (1402-1416)
- handlePVAR (1418-1433)
- handleFVAR (1435-1497)
- handleGVAR (1499-1520)
- handleACONST (1522-1540)
- handleSTKSCAN (1581-1605)
- handlePVAR_SET (2421-2439)
- handleARG0 (2441-2455)
- handleIVARX_ (2457-2472)
- handleFVARX_ (2474-2489)
- handleCOPY (2491-2509)
- handleMYARGCOUNT (2511-2526)
- handleMYALINK (2528-2543)

**floating_point.zig** (lines 1607-1685):
- handleFPLUS2 (1607-1630)
- handleFDIFFERENCE (1632-1643)
- handleFTIMES2 (1645-1656)
- handleFQUOTIENT (1658-1672)
- handleFGREATERP (1674-1685)

**misc.zig** (lines 13-26, 1354-1396, 1690-1704, 1796-1835, 1837-2820):
- handleGCREF (13-26)
- handleCHARCODE (1358-1376)
- handleCHARN (1378-1396)
- handleSLRETURN (1687-1704)
- handleMAKENUMBER (1796-1835)
- handleRPLPTR_N (1837-1859)
- handleASSOC (1861-1885)
- handleGVAR_ (1887-1907)
- handleCMLASSOC (1909-1915)
- handleFMEMB (1917-1934)
- handleCMLMEMBER (1936-1942)
- handleFINDKEY (1944-1960)
- handleCREATECELL (1962-1986)
- handleBIN (1988-1999)
- handleBOUT (2001-2016)
- handleRESTLIST (2018-2037)
- handleMISCN (2039-2048)
- handleRPLCONS (2050-2074)
- handleLISTGET (2076-2099)
- handleEVAL (2101-2116)
- handleENVCALL (2118-2134)
- handleATOMCELL_N (2136-2143)
- handleGETBASEBYTE (2145-2151)
- handleINSTANCEP (2153-2161)
- handleBLT (2163-2168)
- handleMISC10 (2170-2175)
- handlePUTBASEBYTE (2177-2184)
- handleGETBASE_N (2186-2193)
- handleGETBASEPTR_N (2195-2202)
- handleGETBITS_N_FD (2204-2212)
- handleCMLEQUAL (2214-2224)
- handlePUTBASE_N (2226-2234)
- handlePUTBASEPTR_N (2236-2244)
- handlePUTBITS_N_FD (2246-2255)
- handleADDBASE (2257-2266)
- handleVAG2 (2268-2274)
- handleHILOC (2276-2282)
- handleLOLOC (2284-2290)
- handleBASE_LESSTHAN (2324-2333)
- handleUBFLOAT2 (2335-2342)
- handleUBFLOAT1 (2344-2351)
- handleUBFLOAT3 (2659-2667)
- handleBOXIPLUS (2353-2362)
- handleBOXIDIFFERENCE (2364-2373)
- handleFLOATBLT (2375-2380)
- handleFFTSTEP (2382-2387)
- handleMISC3 (2389-2394)
- handleMISC4 (2396-2401)
- handleUPCTRACE (2403-2408)
- handleCL_EQUAL (2410-2419)
- handleSIC (2545-2559)
- handleSNIC (2561-2575)
- handleSICX (2577-2591)
- handleELT (2593-2599)
- handleNTHCHC (2601-2607)
- handleSETA (2609-2620)
- handleRPLCHARCODE (2622-2632)
- handleTYPECHECK (2634-2643)
- handleBUSBLT (2645-2650)
- handleMISC8 (2652-2657)
- handleTYPEMASK_N (2669-2678)
- handleMISC7 (2680-2686)
- handleDRAWLINE (2688-2693)
- handleSTORE_N (2695-2704)
- handleCOPY_N (2706-2717)
- handleRAID (2719-2724)
- handleGCONST (2802-2820)

## dispatch.zig Split Plan

### Module Structure

```
zaiko/src/vm/dispatch/
├── instruction.zig  (~400 lines) - Instruction struct and decoding
└── execution.zig    (~700 lines) - executeOpcodeWithOperands switch
```

### Line Range Mapping

**instruction.zig** (lines 13-421):
- Instruction struct (13-40)
- fetchInstructionByte (327-339)
- decodeInstructionFromMemory (341-384)
- fetchInstruction (386-393)
- decodeInstruction (395-421)
- decodeOpcode (423-437)
- getInstructionLength (996-1099)

**execution.zig** (lines 439-994):
- dispatch (439-528)
- executeInstruction (530-535)
- handleFJUMPWithOffset (537-550)
- handleTJUMPWithOffset (552-565)
- executeOpcodeWithOperands (567-994)
- initializeVMState (1101-1150)

**dispatch.zig** (main file, ~50 lines):
- Re-exports from instruction and execution
- Opcode enum (if needed separately)

## Execution Steps

1. Create `opcodes/` directory
2. Extract each module from opcodes.zig
3. Create new opcodes.zig with re-exports
4. Update dispatch.zig imports
5. Create `dispatch/` directory
6. Extract instruction.zig and execution.zig
7. Update dispatch.zig to re-export
8. Test compilation
9. Fix any import errors
10. Verify functionality

## Common Imports for All Modules

Each opcode module needs:
```zig
const errors = @import("../../utils/errors.zig");
const stack = @import("../stack.zig");
const types = @import("../../utils/types.zig");

const VM = stack.VM;
const LispPTR = types.LispPTR;
const DLword = types.DLword;
```

Some modules also need:
- `cons = @import("../../data/cons.zig")` (data_ops, comparison)
- `array = @import("../../data/array.zig")` (array_ops)
- `virtual_memory_module = @import("../../memory/virtual.zig")` (data_ops, array_ops)

## Testing Strategy

1. After each module creation, verify it compiles
2. After all modules created, verify full compilation
3. Run existing tests to ensure no regressions
4. Check that dispatch.zig can still import all handlers
