= Function Call Mechanism Specification

*Navigation*: README | Execution Model | Stack Management

Complete specification of function call and return mechanisms, including frame setup, argument passing, and return value handling.

== Overview

Function calls in the VM involve:

1. Saving current execution state
2. Allocating new stack frame
3. Setting up arguments
4. Transferring control to called function
5. Returning with result value

== Function Call Opcodes

=== FN0-FN4 (Fixed Argument Count)

*Opcode*: FN0 (0x08), FN1 (0x09), FN2 (0x0A), FN3 (0x0B), FN4 (0x0C)

*Instruction Format*:

- *Length*: 3 bytes for non-BIGATOMS (FN_OPCODE_SIZE = 3)
  - Byte 0: Opcode (0x08-0x0C)
  - Bytes 1-2: Atom index (DLword, 2 bytes) - `Get_AtomNo_PCMAC1` in C
- *Length*: 4-5 bytes for BIGATOMS (FN_OPCODE_SIZE = 4-5)
  - Byte 0: Opcode
  - Bytes 1-3/4: Atom index (extended size)

*Argument Count*: Fixed by opcode (FN0=0, FN1=1, FN2=2, FN3=3, FN4=4)

*Call Algorithm*:

#codeblock(lang: "pseudocode", [
function ExecuteFN(arg_count, atom_index):
    // Get atom index from instruction operand (bytes 1-2 for non-BIGATOMS)
    // C: Get_AtomNo_PCMAC1 - reads DLword from PC+1

    // Lookup function definition from atom table
    // C: defcell = GetDEFCELL68k(atom_index)
    // See data-structures/atom-table.typ for detailed calculation
    // For BIGVM+BIGATOMS: GetDEFCELLlitatom(index) = AtomSpace + (5 * index + 2) * sizeof(LispPTR)
    defcell = GetDEFCELL(atom_index)
    
    // Check if C code function (ccodep flag)
    // C: defcell_word = *(int *)fn_defcell; ccodep = (defcell_word >> 31) & 1;
    // DefCell first LispPTR: bit 31 = ccodep, bit 30 = fastp, bits 29-28 = argtype, bits 27-0 = defpointer
    if defcell.ccodep != 0:
        // C code function - handle via C function dispatch
        HandleCCodeFunction(defcell)
    else:
        // Lisp function - get function header from defpointer
        // C: LOCFNCELL = (struct fnhead *)NativeAligned4FromLAddr((defcell->defpointer & POINTERMASK))
        // defpointer is 28-bit value in low bits of first LispPTR
        fnheader_ptr = defcell.defpointer & POINTERMASK
        
        // CRITICAL: Check if defpointer is valid (not 0/NIL)
        // C: op_fn_common checks GetTypeNumber(defcell->defpointer) == TYPE_COMPILED_CLOSURE
        // If defpointer is 0 or not a compiled closure, uses ATOM_INTERPRETER
        // If defpointer is 0, should trigger UFN (Undefined Function Name) lookup
        if fnheader_ptr == 0:
            // Atom has no function definition - trigger UFN lookup
            // C: goto op_ufn; - looks up function in UFN table
            TriggerUFNLookup(atom_index)
            return
        
        function_obj = ReadFunctionHeader(fnheader_ptr)

    // Validate function object
    if not IsFunction(function_obj):
        ERROR_EXIT(function_obj)

    // Call function with fixed argument count
    CallFunction(function_obj, arg_count)
])

=== FNX (Variable Argument Count)

*Opcode*: FNX (0x0D)

*Call Algorithm*:

#codeblock(lang: "pseudocode", [
function ExecuteFNX():
    // Decode atom index and argument count
    atom_index = DecodeAtomIndex(operands)
    arg_count = DecodeArgCount(operands)

    // Get function from atom
    function_obj = GetFunctionFromAtom(atom_index)

    // Call function with variable arguments
    CallFunction(function_obj, arg_count)
])

== Function Call Process

=== Step 1: Save Current State

#codeblock(lang: "pseudocode", [
function SaveCurrentState():
    current_frame = GetCurrentFrame()
    current_frame.pc = PC - FunctionObject.code_start
    PushCStack(TopOfStack)  // Save TOS
])

=== Step 2: Get Function Object

#codeblock(lang: "pseudocode", [
function GetFunctionObject(atom_index):
    // Read DefCell from atom's definition cell
    // C: defcell = GetDEFCELL68k(atom_index)
    defcell = GetDEFCELL(atom_index)
    
    // DefCell structure contains:
    // - ccodep: 1 bit (C code flag: 1 = C function, 0 = Lisp function)
    // - fastp: 1 bit (fast function flag)
    // - argtype: 2 bits (argument type)
    // - defpointer: 28 bits (BIGVM) or 24 bits (non-BIGVM) - pointer to function header
    
    // Check if C code function
    if defcell.ccodep != 0:
        // C code function - handle via C function dispatch
        return HandleCCodeFunction(defcell)
    else:
        // Lisp function - get function header from defpointer
        // C: LOCFNCELL = (struct fnhead *)NativeAligned4FromLAddr((defcell->defpointer & POINTERMASK))
        fnheader_ptr = defcell.defpointer & POINTERMASK
        function_obj = ReadFunctionHeader(fnheader_ptr)
        return function_obj
])

=== Step 3: Check Stack Space

#codeblock(lang: "pseudocode", [
function CheckStackSpace(function_obj, arg_count):
    required_space = function_obj.stkmin + STK_SAFE
    if CurrentStackPTR - required_space < EndSTKP:
        if CurrentStackPTR < GuardStackAddr:
            HandleStackOverflow()
        else:
            ExtendStack()
])

=== Step 4: Allocate Frame

#codeblock(lang: "pseudocode", [
function AllocateFrame(function_obj):
    // Calculate frame size
    frame_size = FRAMESIZE + function_obj.local_count * 2

    // Allocate frame
    frame_ptr = CurrentStackPTR - frame_size
    CurrentStackPTR = frame_ptr

    // Initialize frame
    frame = GetFrame(frame_ptr)
    frame.flags = FX_MARK
    frame.fnheader = function_obj.address
    frame.alink = LAddrFromNative(PreviousFrame)
    frame.nextblock = CalculateNextBlock(arg_count)

    return frame_ptr
])

=== Step 5: Set Up Arguments

#codeblock(lang: "pseudocode", [
function SetupArguments(function_obj, arg_count):
    // Handle spread arguments
    if function_obj.na >= 0:
        // Spread type function
        rest = arg_count - function_obj.na
        while rest < 0:
            PushStack(NIL_PTR)  // Fill missing args
            rest = rest + 1
        CurrentStackPTR = CurrentStackPTR - (rest * 2)

    // Set up PVar area
    pvar_count = function_obj.pv + 1
    while pvar_count > 0:
        *CurrentStackPTR = 0x0ffff0000  // Initialize PVar
        CurrentStackPTR = CurrentStackPTR + DLWORDSPER_CELL
        pvar_count = pvar_count - 1
])

=== Step 6: Set Up Binding Frame

#codeblock(lang: "pseudocode", [
function SetupBindingFrame():
    CurrentStackPTR = CurrentStackPTR + 2
    *CurrentStackPTR = BF_MARK
    *(CurrentStackPTR + 1) = CurrentFrame.nextblock
    CurrentStackPTR = CurrentStackPTR + 2
])

=== Step 7: Transfer Control

#codeblock(lang: "pseudocode", [
function TransferControl(function_obj):
    // Set new function object
    FunctionObject = function_obj

    // Set program counter
    PC = function_obj.code_start + function_obj.startpc

    // Set IVar pointer
    IVar = NativeAligned2FromStackOffset(CurrentFrame.nextblock)

    // Set PVar pointer
    PVar = CurrentStackPTR + FRAMESIZE

    // Continue dispatch loop
    ContinueDispatch()
])

== Return Mechanism

=== RETURN Opcode

*Opcode*: RETURN (0x10)

*Return Algorithm*:

#codeblock(lang: "pseudocode", [
function ExecuteRETURN():
    // Get return value
    return_value = TopOfStack

    // Get previous frame
    previous_frame = GetPreviousFrame(CurrentFrame)

    // Restore execution state
    RestoreExecutionState(previous_frame)

    // Set return value
    TopOfStack = return_value

    // Continue dispatch loop
    ContinueDispatch()
])

=== State Restoration

#codeblock(lang: "pseudocode", [
function RestoreExecutionState(frame):
    // Restore frame
    CurrentFrame = frame

    // Restore function object
    FunctionObject = GetFunctionFromFrame(frame)

    // Restore program counter
    PC = FunctionObject.code_start + frame.pc

    // Restore stack pointer
    CurrentStackPTR = CalculateStackPointer(frame)

    // Restore IVar and PVar
    IVar = NativeAligned2FromStackOffset(frame.nextblock)
    PVar = CurrentStackPTR + FRAMESIZE
])

== Function Object Structure

=== Function Header

#codeblock(lang: "pseudocode", [
struct FunctionHeader:
    stkmin: DLword        // Minimum stack space
    na: short            // Number of arguments (negative if spread)
    pv: short            // Parameter variable count
    startpc: DLword      // Code start offset
    nil4: 1 bit          // Reserved
    byteswapped: 1 bit   // Code byte-swapped flag
    argtype: 2 bits      // Argument type
    framename: 24-28 bits // Frame name atom index
    ntsize: DLword       // Name table size
    nlocals: 8 bits     // Local variable count
    fvaroffset: 8 bits   // Free variable offset
    // Name table follows
])

== Argument Passing

=== Argument Types

*Fixed Arguments*:

- Arguments passed on stack
- Count matches function definition
- Accessed via PVar

*Spread Arguments*:

- Variable number of arguments
- Extra arguments in list
- Accessed via PVar and list operations

=== Argument Setup

#codeblock(lang: "pseudocode", [
function SetupArguments(function_obj, arg_count):
    // Arguments already on stack (TOS is last arg)
    // Adjust for spread functions
    if function_obj.na >= 0:
        // Fill missing arguments with NIL
        while arg_count < function_obj.na:
            PushStack(NIL_PTR)
            arg_count = arg_count + 1
        // Remove extra arguments
        if arg_count > function_obj.na:
            CurrentStackPTR = CurrentStackPTR + ((arg_count - function_obj.na) * 2)
])

== Return Value Handling

=== Return Value on Stack

#codeblock(lang: "pseudocode", [
function HandleReturnValue(return_value):
    // Return value is on top of stack
    // Previous frame's TOS is restored
    TopOfStack = return_value
])

=== Multiple Return Values

Some functions may return multiple values:

- Primary value on TOS
- Additional values in frame
- Accessed via special operations

== Error Handling

=== Invalid Function

#codeblock(lang: "pseudocode", [
if not IsFunction(function_obj):
    ERROR_EXIT(function_obj)
    // Triggers error handler
])

=== Stack Overflow

#codeblock(lang: "pseudocode", [
if StackOverflowDetected():
    HandleStackOverflow()
    // May extend stack or signal error
])

== Related Documentation

- Stack Management - Frame structure and management
- Execution Model - How calls integrate with dispatch
- Instruction Set - Function call opcodes
