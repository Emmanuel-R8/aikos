= Function Header Specification

*Navigation*: README | Cons Cells | Arrays

Complete specification of function header format, including function metadata, code layout, and name tables.

== Overview

Function headers contain metadata about Lisp functions, including argument counts, local variables, code location, and name table information.

== Function Header Structure (FNHEAD)

#codeblock(lang: "pseudocode", [
struct FunctionHeader:
    stkmin: DLword         // Minimum stack space required
    na: short             // Number of arguments (negative if spread)
    pv: short             // Parameter variable count
    startpc: DLword       // Code start offset in BYTES from function header start
                          // CRITICAL: Despite being a DLword type, startpc is a BYTE offset!
                          // C: PC = (ByteCode *)FuncObj + FuncObj->startpc;
                          // The comment in maiko/inc/stack.h saying "DLword offset from stkmin" is INCORRECT.
                          // Per maiko/src/intcall.c:106, maiko/src/bbtsub.c:1730, maiko/src/loopsops.c:428
    nil4: 1 bit           // Reserved
    byteswapped: 1 bit    // Code byte-swapped flag
    argtype: 2 bits       // Argument type
    framename: 24-28 bits // Frame name atom index
    ntsize: DLword        // Name table size
    nlocals: 8 bits       // Local variable count
    fvaroffset: 8 bits    // Free variable offset (from name table)
    // Name table follows header
])

*Size*: Variable (depends on name table)
*Alignment*: 4-byte aligned

== Function Header Fields

=== Stack Management

*stkmin*: Minimum stack space required by function

- Used for stack overflow checking
- Includes frame size and local variables

*na*: Number of arguments

- *Positive*: Fixed number of arguments
- *Negative*: Spread function (variable arguments)
- Used for argument validation

*pv*: Parameter variable count

- Number of parameter variables
- Used for PVar area allocation

=== Code Location

*startpc*: Code start offset

- *CRITICAL*: This is a BYTE offset from the function header, not a DLword offset!
- C code: `PC = (ByteCode *)FuncObj + FuncObj->startpc;` adds `startpc` BYTES to `FuncObj`
- The comment in `maiko/inc/stack.h:63` saying "DLword offset from stkmin" is INCORRECT
- Per actual C implementation: `maiko/src/bbtsub.c:1730`, `maiko/src/loopsops.c:428`, `maiko/src/ufn.c:194`
- Used to locate function bytecode start

*byteswapped*: Code byte-swap flag

- Indicates if code needs byte-swapping
- Used for cross-platform compatibility

=== Name Table

*framename*: Frame name atom index

- Atom index for function name
- Used for debugging and error messages

*ntsize*: Name table size

- Size of name table in words
- Name table follows function header

*nlocals*: Local variable count

- Number of local variables
- Used for IVar allocation

*fvaroffset*: Free variable offset

- Offset from name table start to free variables
- Used for FVar access

== Name Table Structure

Name table follows function header:

#codeblock(lang: "pseudocode", [
struct NameTable:
    // Variable name entries
    entries: array[ntsize] of NameEntry

struct NameEntry:
    atom_index: LispPTR   // Atom index for variable name
    // ... variable metadata ...
])

== Function Code Layout

#codeblock(lang: "pseudocode", [
function GetFunctionCode(function_header):
    // Calculate code address
    code_base = function_header + function_header.stkmin
    code_start = code_base + function_header.startpc

    return code_start
])

== Function Invocation

=== Function Call Setup

#codeblock(lang: "pseudocode", [
function SetupFunctionCall(function_header, arg_count):
    // Validate argument count
    if function_header.na >= 0:
        if arg_count != function_header.na:
            Error("Argument count mismatch")
    else:
        // Spread function
        if arg_count < abs(function_header.na):
            Error("Too few arguments")

    // Allocate frame
    frame = AllocateStackFrame(function_header)

    // Set up PVar area
    pvar_count = function_header.pv + 1
    AllocatePVarArea(frame, pvar_count)

    // Set up IVar area
    ivar_count = function_header.nlocals
    AllocateIVarArea(frame, ivar_count)

    // Set up name table reference
    if function_header.ntsize > 0:
        name_table = GetNameTable(function_header)
        frame.nametable = LispAddressOf(name_table)
        frame.validnametable = true
])

== Variable Access

=== IVar (Local Variables)

#codeblock(lang: "pseudocode", [
function GetIVar(function_header, index):
    frame = GetCurrentFrame()
    ivar_base = NativeAligned2FromStackOffset(frame.nextblock)
    return ivar_base[index]
])

=== PVar (Parameter Variables)

#codeblock(lang: "pseudocode", [
function GetPVar(function_header, index):
    frame = GetCurrentFrame()
    pvar_base = frame + FRAMESIZE
    return pvar_base[index]
])

=== FVar (Free Variables)

#codeblock(lang: "pseudocode", [
function GetFVar(function_header, index):
    frame = GetCurrentFrame()
    name_table = GetNameTable(function_header)
    fvar_base = name_table + function_header.fvaroffset
    return fvar_base[index]
])

== Function Types

=== Fixed Argument Functions

Functions with fixed argument count:

- *na >= 0*: Exact argument count required
- Arguments passed on stack
- Accessed via PVar

=== Spread Functions

Functions with variable arguments:

- *na < 0*: Minimum argument count
- Extra arguments in list
- Accessed via PVar and list operations

== Related Documentation

- VM Core - Function call mechanism
- Stack Management - Frame structure
- Memory Management - Function allocation
