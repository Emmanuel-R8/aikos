#import "../../prelude.typ": codeblock

= Execution Model Specification

*Navigation*: README | Stack Management | Function Calls | Interrupt Handling

Complete specification of the VM execution model, including the dispatch loop algorithm, instruction fetch/decode/execute cycle, and program counter management.

== Overview

The execution model defines how the VM executes bytecode instructions. The core is the dispatch loop that continuously fetches, decodes, and executes instructions until the program terminates or an error occurs.

== Dispatch Loop Algorithm

=== High-Level Algorithm

#codeblock(lang: "mermaid", [
graph TD
    Start[Start VM] --> Init[Initialize Stack]
    Init --> Enter[Enter Dispatch Loop]
    Enter --> Fetch[Fetch Instruction]
    Fetch --> Decode[Decode Opcode]
    Decode --> Execute[Execute Handler]
    Execute --> Update[Update PC]
    Update --> CheckIRQ{Check Interrupts}
    CheckIRQ -->|Pending| HandleIRQ[Handle Interrupt]
    CheckIRQ -->|None| Fetch
    HandleIRQ --> Fetch
    Fetch -->|Error| Error[Error Handler]
    Error -->|Recover| Fetch
    Error -->|Fatal| Exit[Exit VM]
])

=== Pseudocode Implementation

#codeblock(lang: "pseudocode", [
function dispatch():
    // Initialize execution state
    PC = FunctionObject.code_start
    CurrentFrame = InitialFrame
    TopOfStack = InitialValue

    // CRITICAL: Stack must be initialized with at least one value (NIL = 0)
    // before entering dispatch loop. This ensures conditional jumps (FJUMP/TJUMP)
    // have a value to pop from the stack.
    // C: start_lisp() -> TopOfStack = 0;
    TopOfStack = 0  // Initialize with NIL

    // Main dispatch loop
    while not ErrorExit:
        // Fetch instruction
        opcode_byte = ReadByte(PC)

        // Decode instruction
        instruction_length = opcode_length_table[opcode_byte]
        operands = ReadBytes(PC + 1, instruction_length - 1)

        // Check interrupts before execution
        if CheckInterrupts():
            HandleInterrupts()
            continue

        // Execute opcode handler
        ExecuteOpcode(opcode_byte, operands)

        // Update program counter
        PC = PC + instruction_length

        // Check interrupts after execution
        if CheckInterrupts():
            HandleInterrupts()
])

== Instruction Fetch

=== Fetch Algorithm

#codeblock(lang: "pseudocode", [
function FetchInstruction():
    // Read opcode byte
    opcode = ReadByte(PC)

    // Validate opcode
    if opcode not in valid_opcodes:
        HandleInvalidOpcode(opcode)
        return

    // Determine instruction length
    length = opcode_length_table[opcode]

    // Read operands
    operands = []
    for i = 1 to length - 1:
        operands.append(ReadByte(PC + i))

    return Instruction(opcode, operands, length)
])

=== Program Counter Management

The program counter (PC) tracks the current instruction:

- *Initial Value*: Set to function code start
- *Update*: Advanced by instruction length after execution
- *Format*: Offset from function object base address
- *Storage*: Stored in stack frame for return

=== PC Initialization from Sysout

When loading a sysout file, the PC must be initialized from the saved VM state:

#codeblock(lang: "pseudocode", [
function InitializePCFromSysout(ifpage, virtual_memory):
    // Get current frame pointer from IFPAGE
    currentfxp = ifpage.currentfxp

    // Read frame structure (FX) from virtual memory
    // CRITICAL: Frame fields are stored in big-endian format in sysout
    // Must byte-swap when reading on little-endian machines
    frame = ReadFrame(virtual_memory, currentfxp)

    // Get function header address from frame
    fnheader_addr = frame.fnheader

    // CRITICAL: PC initialization uses FastRetCALL logic
    // C: FastRetCALL macro calculates PC = FuncObj + CURRENTFX->pc
    // Where FuncObj comes from FX_FNHEADER (translated fnheader_addr)
    // And CURRENTFX->pc is the pc field from the frame (byte offset from FuncObj)
    // This is DIFFERENT from using function header's startpc field!
    
    if fnheader_addr != 0:
        // Read function header
        // CRITICAL: Function header fields are also big-endian
        fnheader = ReadFunctionHeader(virtual_memory, fnheader_addr)
        
        // Translate fnheader_addr (LispPTR) to byte offset
        fnheader_offset = TranslateLispPTRToOffset(fnheader_addr)
        FuncObj = virtual_memory + fnheader_offset
        
        // Get PC offset from frame (CURRENTFX->pc)
        frame_pc = frame.pc  // Byte offset from FuncObj
        
        // Calculate PC: FuncObj + CURRENTFX->pc
        PC = FuncObj + frame_pc
        
        // Validate PC points to valid bytecode
        if ReadByte(PC) is invalid_opcode:
            // Fallback: use function header startpc
            PC = fnheader_offset + fnheader.startpc
    else:
        // Frame is uninitialized (fnheader=0)
        // This can happen with sparse pages in sysout
        // Use fallback entry point or initialize frame first
        PC = FindEntryPoint()  // Fallback mechanism

    return PC
])

*CRITICAL*: Frame and function header fields must be byte-swapped when reading from sysout files on little-endian machines. The sysout format stores all multi-byte values in big-endian format.

*C Reference*: `maiko/src/main.c:797-807` - `start_lisp()` reads current frame and initializes PC

== Instruction Decode

=== Decode Algorithm

#codeblock(lang: "pseudocode", [
function DecodeInstruction(opcode_byte, operand_bytes):
    // Get opcode metadata
    opcode_info = opcode_table[opcode_byte]

    // Decode operands based on opcode type
    decoded_operands = []

    switch opcode_info.operand_format:
        case NO_OPERANDS:
            // No operands to decode
            break

        case SINGLE_BYTE:
            decoded_operands.append(operand_bytes[0])
            break

        case ATOM_INDEX:
            if BIGATOMS:
                atom_index = (operand_bytes[0] << 16) |
                            (operand_bytes[1] << 8) |
                            operand_bytes[2]
            else:
                atom_index = (operand_bytes[0] << 8) | operand_bytes[1]
            decoded_operands.append(atom_index)
            break

        case SIGNED_OFFSET:
            offset = sign_extend(operand_bytes[0])
            decoded_operands.append(offset)
            break

        case MULTI_BYTE:
            // Variable-length decoding
            decoded_operands = DecodeVariableOperands(opcode_info, operand_bytes)
            break

    return DecodedInstruction(opcode_byte, decoded_operands)
])

== Instruction Execute

=== Execution Framework

#codeblock(lang: "pseudocode", [
function ExecuteOpcode(opcode, operands):
    // Lookup opcode handler
    handler = opcode_handler_table[opcode]

    // Prepare execution context
    execution_context = {
        PC: current_PC,
        Stack: current_stack,
        Frame: current_frame,
        Function: current_function
    }

    // Execute handler
    try:
        result = handler(operands, execution_context)

        // Update execution state
        UpdateExecutionState(result)

    except Error as e:
        HandleExecutionError(e, opcode, operands)
])

=== Handler Execution

Each opcode handler:

1. Receives decoded operands
2. Accesses stack/memory as needed
3. Performs operation
4. Updates stack/memory
5. Returns execution result

== Dispatch Mechanisms

=== Mechanism 1: Computed Goto (OPDISP)

*When Available*: GCC compiler with computed goto support

*Implementation*:

#codeblock(lang: "pseudocode", [#raw("function dispatch_computed_goto():\n    // Jump table with labels\n    static label_table[256] = {\n        &&case_000, &&case_001, &&case_002, ...\n    }\n\n    opcode = ReadByte(PC)\n    goto *label_table[opcode]\n\ncase_001:\n    ExecuteCAR()\n    goto next_instruction\n\ncase_002:\n    ExecuteCDR()\n    goto next_instruction\n\n// ... cases for all opcodes ...\n\nnext_instruction:\n    PC = PC + instruction_length\n    goto dispatch_computed_goto")])

*Advantages*:

- Fastest dispatch method
- Minimal overhead per instruction
- Direct jump to handler

=== Mechanism 2: Switch Statement

*When Used*: Compilers without computed goto support

*Implementation*:

#codeblock(lang: "pseudocode", [
function dispatch_switch():
    while not ErrorExit:
        opcode = ReadByte(PC)

        switch opcode:
            case 0x01:
                ExecuteCAR()
                break
            case 0x02:
                ExecuteCDR()
                break
            // ... cases for all opcodes ...
            default:
                HandleInvalidOpcode(opcode)

        PC = PC + instruction_length
        CheckInterrupts()
])

*Advantages*:

- Portable across compilers
- Standard C construct
- Slightly slower but acceptable

== Interrupt Handling

=== Interrupt Check Points

Interrupts are checked:

1. *Before instruction execution*: After fetch/decode
2. *After instruction execution*: Before next fetch
3. *During long operations*: Periodically in loops

=== Interrupt Check Algorithm

#codeblock(lang: "pseudocode", [
function CheckInterrupts():
    interrupt_state = GetInterruptState()

    if interrupt_state.waitinginterrupt:
        if interrupt_state.LogFileIO:
            HandleLogFileIO()
        if interrupt_state.ETHERInterrupt:
            HandleEthernetInterrupt()
        if interrupt_state.IOInterrupt:
            HandleIOInterrupt()
        if interrupt_state.storagefull:
            HandleStorageFull()
        if interrupt_state.stackoverflow:
            HandleStackOverflow()

        return true

    return false
])

== Execution State Management

=== State Structure

#codeblock(lang: "pseudocode", [#raw("struct ExecutionState:\n    PC: ByteCode*              // Program counter\n    CurrentFrame: Frame*       // Current stack frame\n    FunctionObject: Function*  // Current function\n    TopOfStack: LispPTR        // Top of stack value\n    StackPointer: DLword*      // Current stack pointer\n    EndOfStack: DLword*        // End of stack\n    ErrorExit: boolean         // Error exit flag")])

=== State Transitions

#codeblock(lang: "mermaid", [#raw("stateDiagram-v2\n    [*] --> Initialized: VM Start\n    Initialized --> Running: Enter Dispatch\n    Running --> Running: Execute Instruction\n    Running --> Interrupted: Interrupt Pending\n    Interrupted --> Running: Handle Interrupt\n    Running --> Error: Execution Error\n    Error --> Running: Error Recovery\n    Error --> Terminated: Fatal Error\n    Running --> Terminated: Normal Exit\n    Terminated --> [*]")])

== Performance Optimizations

=== PC Caching

*CRITICAL*: The `pccache` variable must be initialized at the start of the dispatch loop before any use of `PCMAC` (which is `pccache - 1`). This is essential because `PCMAC` is used immediately for instruction fetching and logging.

#codeblock(lang: "pseudocode", [
// Cache PC in register/local variable
// CRITICAL: Initialize pccache from PC at start of dispatch loop
// PCMAC = pccache - 1, so pccache = PC + 1
pccache = PC + 1

while not ErrorExit:
    // PCMAC = pccache - 1 points to current instruction
    opcode = ReadByte(PCMAC)  // or ReadByte(pccache - 1)
    // ... execute ...
    // After instruction execution, update pccache
    pccache = pccache + instruction_length
    // Continue loop (pccache now points one byte ahead again)
    
PC = pccache - 1  // Update global PC periodically (if needed)
])

*Implementation Note*: In the C implementation, `pccache` is a local variable in `dispatch()` that must be initialized at the `nextopcode:` label. The original code had a bug where `pccache` was uninitialized when `PCMAC` was first used, causing undefined behavior. The fix is to add `pccache = PC + 1;` at the very start of the `nextopcode:` label, before any code that uses `PCMAC`.

=== Stack Pointer Caching

#codeblock(lang: "pseudocode", [
// Cache stack pointer
cspcache = CurrentStackPTR
// Use cached pointer for stack operations
// Update global pointer periodically
])

=== Instruction Prefetch

Some implementations may prefetch next instruction:

- Read next opcode while executing current
- Reduces fetch latency
- Must handle PC updates correctly

== Error Handling

=== Error Detection

Errors detected during execution:

- *Type Errors*: Wrong type for operation
- *Memory Errors*: Invalid address
- *Arithmetic Errors*: Overflow, division by zero
- *Stack Errors*: Stack overflow/underflow

=== Error Recovery

#codeblock(lang: "pseudocode", [
function HandleExecutionError(error, opcode, operands):
    // Save error context
    error_context = {
        opcode: opcode,
        operands: operands,
        PC: PC,
        Frame: CurrentFrame
    }

    // Attempt recovery
    if CanRecover(error):
        RecoverFromError(error, error_context)
    else:
        ErrorExit = true
        UnwindStack()
        ReportError(error, error_context)
])

=== Unknown Opcode Handling

When encountering an unknown opcode (not in the opcode table), the VM should:

1. *Log the opcode*: Record the opcode byte and PC for debugging
2. *Check for UFN*: Unknown opcodes may be UFNs (Undefined Function Names) that require lookup
3. *Continue or halt*: Depending on implementation, either continue execution (skipping the opcode) or halt with an error

#codeblock(lang: "pseudocode", [
function HandleUnknownOpcode(opcode_byte, PC):
    // Log for debugging
    Log("Unknown opcode 0x%02X at PC=0x%X", opcode_byte, PC)

    // Check if this might be a UFN (Undefined Function Name)
    // UFNs are opcodes that map to Lisp functions via UFN table
    if IsUFN(opcode_byte):
        return HandleUFN(opcode_byte, PC)

    // For development: continue execution to identify missing opcodes
    // For production: halt with error
    if development_mode:
        PC = PC + 1  // Skip opcode byte
        continue
    else:
        ErrorExit = true
        ReportError("Unknown opcode", opcode_byte, PC)
])

*C Reference*: `maiko/src/xc.c:249-258` - `op_ufn` handler for unknown opcodes that are UFNs

== Related Documentation

- Instruction Set - Opcode specifications
- Stack Management - Stack frame handling
- Function Calls - Function invocation
- Interrupt Handling - Interrupt processing
