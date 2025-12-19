= Stack Management Specification

*Navigation*: README | Execution Model | Function Calls

Complete specification of stack frame structure, stack operations, and frame management.

== Overview

The VM uses a stack-based execution model where each function call creates a stack frame (FX - Frame eXtended). The stack manages function activation, local variables, parameters, and return addresses.

== Stack Frame Addressing

*CRITICAL*: Frame pointers (`currentfxp`, `stackbase`, `endofstack`) in IFPAGE are `DLword` StackOffsets, NOT LispPTR values!

- `currentfxp`: DLword offset from Stackspace base
- `stackbase`: DLword offset from Stackspace base
- `endofstack`: DLword offset from Stackspace base

C: `NativeAligned2FromStackOffset(DLword StackOffset) = Stackspace + StackOffset`
- Since `Stackspace` is `DLword*`, pointer arithmetic adds `StackOffset` DLwords = `StackOffset * 2` bytes
- Per `maiko/inc/adr68k.h:72-75` and `maiko/src/main.c:797`

== Stack Frame Structure

=== Frame Layout

#codeblock(lang: "mermaid", [
graph TD
    Frame[Stack Frame FX] --> Flags[Flags Word]
    Frame --> Alink[Activation Link]
    Frame --> FnHeader[Function Header Ptr]
    Frame --> NextBlock[Next Block Ptr]
    Frame --> PC[Program Counter]
    Frame --> NameTable[Name Table Ptr]
    Frame --> Blink[Binding Link]
    Frame --> Clink[Closure Link]
    Frame --> Locals[Local Variables]
])

=== Frame Structure (FX)

*CRITICAL*: The actual memory layout of frame fields may differ from the C struct definition due to compiler-specific bitfield packing and struct layout. Implementations MUST verify the actual byte offsets by examining memory contents, not just relying on struct definitions.

*Non-BIGVM Frame Layout (actual memory bytes)*:

#codeblock(lang: "pseudocode", [
struct FrameEx:
    // Offset 0-1: flags + usecount (packed into one DLword)
    flags: 3 bits          // Frame flags
    fast: 1 bit            // Fast call flag
    nil2: 1 bit            // Reserved
    incall: 1 bit          // In-call flag
    validnametable: 1 bit  // Name table valid flag
    nopush: 1 bit          // No push flag
    usecount: 8 bits       // Use count for GC
    
    // Offset 2-3: alink
    alink: DLword          // Activation link (previous frame)
    
    // Offset 4-5: hi1fnheader_hi2fnheader (NOT lofnheader!)
    // CRITICAL: Field positions are swapped compared to struct definition
    hi1fnheader: 8 bits    // Function header pointer (Hi1 addr, low byte)
    hi2fnheader: 8 bits    // Function header pointer (Hi2 addr, high byte)
    
    // Offset 6-7: lofnheader (NOT hi1fnheader_hi2fnheader!)
    // CRITICAL: Field positions are swapped compared to struct definition
    lofnheader: DLword     // Function header pointer (Low addr, 16 bits)
    
    // Offset 8-9: nextblock
    nextblock: DLword      // Next stack block offset
    
    // Offset 10-11: pc
    pc: DLword             // Program counter offset (byte offset from FuncObj)
    
    // Offset 12-15: nametable (non-BIGVM: lonametable + hi1nametable_hi2nametable)
    nametable: LispPTR     // Name table pointer
    
    // Offset 16-17: blink
    blink: DLword          // Binding link
    
    // Offset 18-19: clink
    clink: DLword          // Closure link
    
    // ... local variables follow ...
])

*FX_FNHEADER Calculation*:

#codeblock(lang: "pseudocode", [
function CalculateFX_FNHEADER(frame):
    // CRITICAL: Frame fields are stored in native little-endian format after page byte-swapping
    // Read hi1fnheader_hi2fnheader from offset 4-5 (swapped position)
    hi1fnheader_hi2fnheader = ReadDLword(frame, offset=4)  // Native little-endian
    
    // Read lofnheader from offset 6-7 (swapped position)
    lofnheader = ReadDLword(frame, offset=6)  // Native little-endian
    
    // hi2fnheader is in the LOW byte (bits 0-7) of hi1fnheader_hi2fnheader
    hi2fnheader = (hi1fnheader_hi2fnheader & 0xFF)  // Low byte
    
    // Combine: FX_FNHEADER = (hi2fnheader << 16) | lofnheader
    // FX_FNHEADER is a LispPTR, which is a DLword offset from Lisp_world
    return (hi2fnheader << 16) | lofnheader
])

*Address Translation from FX_FNHEADER*:

*CRITICAL DISCREPANCY (2025-12-12 16:54)*: Based on actual execution logs, FX_FNHEADER appears to be treated as a *byte offset* in FastRetCALL, not a DLword offset. See Address Translation Investigation for details.

#codeblock(lang: "pseudocode", [
function TranslateFX_FNHEADERToFuncObj(fx_fnheader):
    // OBSERVED BEHAVIOR (from execution logs):
    // PC = 0x307898 = FX_FNHEADER (0x307864) + 0x34 (52 bytes = 104/2)
    // This suggests FX_FNHEADER is a byte offset, not DLword offset!
    return Lisp_world + fx_fnheader  // Byte offset (NOT multiplied by 2)
])

*Documented Behavior* (may not match actual):
#codeblock(lang: "pseudocode", [
    // C: NativeAligned4FromLAddr(FX_FNHEADER) = (void *)(Lisp_world + FX_FNHEADER)
    // Since Lisp_world is DLword*, adding FX_FNHEADER adds FX_FNHEADER DLwords
    // Byte offset = FX_FNHEADER * 2
    return Lisp_world + (fx_fnheader * 2)  // In bytes
])

*PC Calculation*:
#codeblock(lang: "pseudocode", [
function CalculatePCFromFrame(funcobj, frame_pc):
    // OBSERVED: PC = FuncObj + (CURRENTFX->pc / 2)
    // This suggests CURRENTFX->pc is stored as DLword offset, not byte offset
    return funcobj + (frame_pc / 2)  // Divide by 2 to convert DLword to bytes
])

*Verification Needed*: Run C emulator with debug statements to confirm actual behavior vs. documentation.

*C Reference*: `maiko/inc/stack.h:81-110` defines the struct, but actual memory layout may differ. Verified against `starter.sysout` frame at offset `0x25ce4` (virtual page 302).

=== Frame Markers

*FX_MARK (0xC000)*:

- Marks start of frame
- Used for frame identification

*BF_MARK (0x8000)*:

- Marks binding frame
- Used for variable binding tracking

*STK_FSB_WORD (0xA000)*:

- Marks free stack block
- Used for stack space management

== Stack Initialization

=== Stack Area Location

*CRITICAL*: The stack area is part of virtual memory (`Lisp_world`), NOT a separate allocation!

- `Stackspace = NativeAligned2FromLAddr(STK_OFFSET) = Lisp_world + STK_OFFSET`
- `STK_OFFSET = 0x00010000` (DLword offset from Lisp_world base)
- Stackspace byte offset = `STK_OFFSET * 2 = 0x20000` bytes
- The stack area already contains data from the sysout file (thousands of DLwords)
- Stack operations must use the virtual memory's stack area directly

*C Reference*: `maiko/src/initsout.c:222` - `Stackspace = (DLword *)NativeAligned2FromLAddr(STK_OFFSET);`

=== CurrentStackPTR Initialization

*CRITICAL*: `CurrentStackPTR` is initialized from the frame's `nextblock` field, not from a separate stack pointer.

#codeblock(lang: "pseudocode", [
function InitializeStackPointer(frame, Stackspace):
    // C: start_lisp() -> next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock)
    // C: CurrentStackPTR = next68k - 2
    next68k = Stackspace + frame.nextblock  // nextblock is DLword offset from Stackspace
    CurrentStackPTR = next68k - 2  // Move back 2 DLwords (stack grows down)

    // Stack depth = (CurrentStackPTR - Stackspace) / 2 (in DLwords)
    stack_depth = (CurrentStackPTR - Stackspace) / 2
    // Typically thousands of DLwords (e.g., 5956 DLwords in starter.sysout)
])

*C Reference*: `maiko/src/main.c:795-801` - `next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock); CurrentStackPTR = next68k - 2;`

=== Initial Stack State

*CRITICAL*: The stack area from the sysout already contains data. `TopOfStack` is just a cached variable, not the actual stack pointer.

- The stack area has pre-existing data (typically thousands of DLwords)
- `TopOfStack = 0` in `start_lisp()` is just resetting a cached variable
- The actual stack pointer (`CurrentStackPTR`) points to existing stack data
- Stack depth is calculated as `(CurrentStackPTR - Stackspace) / 2` DLwords

*CRITICAL*: `TopOfStack` must be implemented as a cached value, NOT read from memory initially. Reading from stack memory initially would return garbage data (e.g., 0xaaaaaaaa patterns) from uninitialized sysout memory. The C code sets `TopOfStack = 0` as a cached variable, and updates it only when stack operations occur.

*C Reference*: `maiko/src/main.c:790` - `TopOfStack = 0;` (cached variable, not stack pointer initialization)

== Stack Operations

=== Push Stack

*CRITICAL*: Stack stores LispPTR values as 32-bit (2 DLwords). The stack pointer is a `DLword*` array, but values are stored as full LispPTR (4 bytes).

*CRITICAL*: Stack grows DOWN. `Stackspace` is the BASE (lowest address), `CurrentStackPTR` is the current top (higher address when stack has data). Pushing moves `CurrentStackPTR` DOWN (toward lower addresses).

*CRITICAL*: Stack memory from sysout stores DLwords in BIG-ENDIAN format. When writing to stack memory, values must be stored in big-endian format to maintain compatibility with sysout format.

#codeblock(lang: "pseudocode", [
function PushStack(value: LispPTR):
    // Stack grows down, move pointer down by 4 bytes (2 DLwords)
    CurrentStackPTR = CurrentStackPTR - 2  // Move DOWN 2 DLwords

    // Store LispPTR as 2 DLwords in BIG-ENDIAN format
    // CRITICAL: Stack memory stores DLwords as [high_byte, low_byte]
    low_word = value & 0xFFFF        // Low 16 bits
    high_word = (value >> 16) & 0xFFFF  // High 16 bits
    
    // Write big-endian: [high_byte, low_byte] for each DLword
    CurrentStackPTR[0] = (low_word >> 8) | ((low_word & 0xFF) << 8)   // Big-endian low word
    CurrentStackPTR[1] = (high_word >> 8) | ((high_word & 0xFF) << 8)  // Big-endian high word

    TopOfStack = value  // Update cached value

    // Check stack overflow (CurrentStackPTR must not go below EndSTKP)
    if CurrentStackPTR < EndSTKP:
        HandleStackOverflow()
])

*C Implementation Reference*: `maiko/inc/lispemul.h:PushStack(x)` decrements `CurrentStackPTR` by 2 DLwords and stores LispPTR value.

*Stack Layout*:
- `Stackspace` (BASE): Lowest address, where stack starts
- `CurrentStackPTR`: Current top, higher address when stack has data
- Stack depth = `(CurrentStackPTR - Stackspace) / 2` DLwords
- Stack grows DOWN: pushing moves `CurrentStackPTR` DOWN (toward lower addresses)

=== Pop Stack

*CRITICAL*: Stack stores LispPTR values as 32-bit (2 DLwords). Reading requires reconstructing the 32-bit value from 2 DLwords.

*CRITICAL*: Stack grows DOWN. Popping moves `CurrentStackPTR` UP (toward higher addresses). Stack is empty when `CurrentStackPTR <= Stackspace`.

*CRITICAL*: Stack memory from sysout stores DLwords in BIG-ENDIAN format. When reading from stack memory, values must be byte-swapped from big-endian to native format.

#codeblock(lang: "pseudocode", [
function PopStack():
    // Check for stack underflow: CurrentStackPTR must be > Stackspace (stack has data)
    if CurrentStackPTR <= Stackspace:
        return StackUnderflow

    // Read LispPTR as 2 DLwords in BIG-ENDIAN format
    // CRITICAL: Stack memory stores DLwords as [high_byte, low_byte]
    // Must byte-swap when reading on little-endian machines
    low_word_be = CurrentStackPTR[0]   // Read as big-endian DLword
    high_word_be = CurrentStackPTR[1]  // Read as big-endian DLword
    
    // Byte-swap from big-endian to native format
    low_word = ((low_word_be & 0xFF) << 8) | ((low_word_be >> 8) & 0xFF)
    high_word = ((high_word_be & 0xFF) << 8) | ((high_word_be >> 8) & 0xFF)
    
    value = (high_word << 16) | low_word  // Reconstruct 32-bit value

    CurrentStackPTR = CurrentStackPTR + 2  // Move UP 2 DLwords (stack grows down)
    
    // Update cached TopOfStack value
    if CurrentStackPTR <= Stackspace:
        TopOfStack = 0  // Stack empty - TopOfStack = NIL
    else:
        TopOfStack = ReadTopOfStackFromMemory()  // Read new top (with byte-swapping)
    
    return value
])

*C Implementation Reference*: `maiko/inc/tos1defs.h:POP_TOS_1` increments `CSTKPTRL` (LispPTR*) and reads LispPTR value.

*Stack Underflow Check*:
- Stack is empty when `CurrentStackPTR <= Stackspace`
- Stack has data when `CurrentStackPTR > Stackspace`
- Stack depth = `(CurrentStackPTR - Stackspace) / 2` DLwords

=== Stack Frame Allocation

#codeblock(lang: "pseudocode", [
function AllocateStackFrame(function_obj):
    // Calculate frame size
    frame_size = FRAMESIZE + function_obj.local_count * 2

    // Check available space
    if CurrentStackPTR - frame_size < EndSTKP:
        ExtendStack()

    // Allocate frame
    frame_ptr = CurrentStackPTR - frame_size
    CurrentStackPTR = frame_ptr

    // Initialize frame
    frame = GetFrame(frame_ptr)
    frame.flags = FX_MARK
    frame.fnheader = function_obj.address
    frame.pc = 0
    frame.alink = LAddrFromNative(PreviousFrame)

    return frame_ptr
])

== Frame Management

=== Activation Links

Activation links chain frames together:

#codeblock(lang: "pseudocode", [
function SetActivationLink(new_frame, previous_frame):
    new_frame.alink = LAddrFromNative(previous_frame)
])

=== Frame Traversal

#codeblock(lang: "pseudocode", [
function GetPreviousFrame(current_frame):
    if current_frame.alink == 0:
        return null
    return NativeAligned4FromLAddr(current_frame.alink)
])

=== Current Frame Access

#codeblock(lang: "pseudocode", [
function GetCurrentFrame():
    return NativeAligned4FromStackOffset(CurrentFrameOffset)
])

== Variable Access

=== IVar (Local Variables)

#codeblock(lang: "pseudocode", [
function GetIVar(index):
    frame = GetCurrentFrame()
    ivar_base = NativeAligned2FromStackOffset(frame.nextblock)
    return ivar_base[index]
])

=== PVar (Parameter Variables)

#codeblock(lang: "pseudocode", [
function GetPVar(index):
    frame = GetCurrentFrame()
    pvar_base = frame + FRAMESIZE
    return pvar_base[index]
])

=== FVar (Free Variables)

#codeblock(lang: "pseudocode", [
function GetFVar(index):
    frame = GetCurrentFrame()
    fvar_offset = frame.fnheader.fvaroffset
    nametable = GetNameTable(frame)
    fvar_base = nametable + fvar_offset
    return fvar_base[index]
])

== Stack Extension

=== Extend Stack Algorithm

#codeblock(lang: "pseudocode", [
function ExtendStack():
    // Check if extension needed
    if CurrentStackPTR < EndSTKP:
        return  // No extension needed

    // Allocate new stack page
    new_page = AllocateStackPage()

    // Initialize free stack block
    free_block = GetFreeStackBlock(new_page)
    free_block.marker = STK_FSB_WORD
    free_block.size = DLWORDSPER_PAGE - 2

    // Update end of stack
    EndSTKP = new_page + DLWORDSPER_PAGE

    // Set up guard block
    guard_block = GetGuardBlock(EndSTKP)
    guard_block.marker = STK_GUARD_WORD
])

== Stack Overflow Handling

=== Overflow Detection

*CRITICAL*: Stack overflow checks must include a safety margin (`STK_SAFE = 32` words) to prevent stack exhaustion during operations.

#codeblock(lang: "pseudocode", [
const STK_SAFE = 32  // Safety margin in words (matches C: maiko/inc/stack.h:38)

function CheckStackOverflow(required_space):
    // Add safety margin to required space
    safe_required_space = required_space + (STK_SAFE * sizeof(DLword))

    if CurrentStackPTR - safe_required_space < EndSTKP:
        if CurrentStackPTR < GuardStackAddr:
            SetInterruptFlag(STACKOVERFLOW)
            return true
        else:
            ExtendStack()  // Try to extend stack
            return false
    return false
])

*C Reference*: `maiko/inc/stack.h:STK_SAFE`, `maiko/src/llstk.c:do_stackoverflow()`

=== Overflow Recovery

#codeblock(lang: "pseudocode", [
function HandleStackOverflow():
    // Set interrupt flag
    interrupt_state.stackoverflow = true
    interrupt_state.waitinginterrupt = true

    // Trigger interrupt handler
    TriggerInterrupt(STACKOVERFLOW)
])

== Free Stack Block Management

=== Free Block Structure

#codeblock(lang: "pseudocode", [
struct FreeStackBlock:
    marker: DLword      // STK_FSB_WORD
    size: DLword        // Size in words
    // ... free space ...
])

=== Merge Free Blocks

#codeblock(lang: "pseudocode", [
function MergeFreeBlocks(block_ptr):
    while GetNextBlock(block_ptr).marker == STK_FSB_WORD:
        next_block = GetNextBlock(block_ptr)
        block_ptr.size += next_block.size
        block_ptr = next_block
    return block_ptr
])

== Frame Cleanup

=== Frame Deallocation

#codeblock(lang: "pseudocode", [
function DeallocateFrame(frame_ptr):
    frame = GetFrame(frame_ptr)

    // Mark as free stack block
    free_block = GetFreeStackBlock(frame_ptr)
    free_block.marker = STK_FSB_WORD
    free_block.size = CalculateFrameSize(frame)

    // Merge with adjacent free blocks
    MergeFreeBlocks(free_block)
])

== Related Documentation

- Execution Model - How stack is used in execution
- Function Calls - Frame creation during calls
- Memory Management - Stack memory allocation
