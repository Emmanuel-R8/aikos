= Interrupt Handling Specification

*Navigation*: README | Execution Model | Function Calls

Complete specification of interrupt handling, including interrupt types, check points, and interrupt processing.

== Overview

Interrupts are checked between instruction execution and allow the VM to handle asynchronous events like I/O, timers, and system events without blocking execution.

== Interrupt Types

=== I/O Interrupts

- *Keyboard*: Key press/release events
- *Mouse*: Mouse movement/button events
- *Network*: Ethernet/socket events
- *File I/O*: File system events

=== Timer Interrupts

- *Periodic*: Regular timer ticks
- *Scheduled*: Time-based events

=== System Interrupts

- *Stack Overflow*: Stack space exhausted
- *Storage Full*: Memory exhausted
- *GC*: Garbage collection requests

== Interrupt State Structure

#codeblock(lang: "pseudocode", [
struct InterruptState:
    LogFileIO: 1 bit          // Log file I/O interrupt
    ETHERInterrupt: 1 bit     // Ethernet interrupt
    IOInterrupt: 1 bit         // General I/O interrupt
    gcdisabled: 1 bit          // GC disabled flag
    vmemfull: 1 bit            // Virtual memory full
    stackoverflow: 1 bit       // Stack overflow
    storagefull: 1 bit         // Storage full
    waitinginterrupt: 1 bit    // Interrupt pending
    intcharcode: DLword        // Interrupt character code
])

== Interrupt Check Points

=== Check Before Instruction

#codeblock(lang: "pseudocode", [
function CheckInterruptsBeforeExecution():
    if interrupt_state.waitinginterrupt:
        HandlePendingInterrupts()
        return true
    return false
])

=== Check After Instruction

#codeblock(lang: "pseudocode", [
function CheckInterruptsAfterExecution():
    // Check stack overflow
    if CheckStackOverflow():
        HandleStackOverflow()
        return true

    // Check I/O events
    if CheckIOEvents():
        HandleIOInterrupts()
        return true

    // Check timer
    if CheckTimer():
        HandleTimerInterrupt()
        return true

    return false
])

== Interrupt Processing

=== Interrupt Processing Algorithm

#codeblock(lang: "mermaid", [
sequenceDiagram
    participant Dispatch as Dispatch Loop
    participant Check as Interrupt Check
    participant Process as Interrupt Processor
    participant Handler as Interrupt Handler

    Dispatch->>Check: Check Interrupts
    Check->>Process: Interrupt Pending
    Process->>Handler: Call Handler
    Handler->>Dispatch: Return to Dispatch
])

=== Process Interrupts

#codeblock(lang: "pseudocode", [
function ProcessInterrupts():
    interrupt_state = GetInterruptState()

    // Process I/O interrupts
    if interrupt_state.IOInterrupt:
        ProcessIOInterrupts()

    if interrupt_state.ETHERInterrupt:
        ProcessEthernetInterrupts()

    if interrupt_state.LogFileIO:
        ProcessLogFileIO()

    // Process system interrupts
    if interrupt_state.stackoverflow:
        ProcessStackOverflow()

    if interrupt_state.storagefull:
        ProcessStorageFull()

    // Process timer interrupts
    if CheckPeriodicInterrupt():
        ProcessPeriodicInterrupt()

    // Clear interrupt flags
    ClearInterruptFlags()
])

== I/O Interrupt Handling

=== Keyboard Interrupts

#codeblock(lang: "pseudocode", [
function ProcessKeyboardInterrupts():
    while HasKeyboardEvents():
        event = GetKeyboardEvent()
        TranslateKeyEvent(event)
        QueueKeyEvent(event)

    SetInterruptFlag(IOInterrupt)
])

=== Mouse Interrupts

#codeblock(lang: "pseudocode", [
function ProcessMouseInterrupts():
    while HasMouseEvents():
        event = GetMouseEvent()
        TranslateMouseEvent(event)
        QueueMouseEvent(event)

    SetInterruptFlag(IOInterrupt)
])

=== Network Interrupts

#codeblock(lang: "pseudocode", [
function ProcessNetworkInterrupts():
    while HasNetworkEvents():
        event = GetNetworkEvent()
        ProcessNetworkPacket(event)

    SetInterruptFlag(ETHERInterrupt)
])

== Timer Interrupt Handling

=== Periodic Timer

#codeblock(lang: "pseudocode", [
function ProcessPeriodicInterrupt():
    // Check if periodic interrupt enabled
    if PERIODIC_INTERRUPT != NIL:
        period_count = period_count - 1
        if period_count <= 0:
            CauseInterruptCall(PERIODIC_INTERRUPT_FRAME)
            ResetPeriodCount()
])

=== Timer Update

#codeblock(lang: "pseudocode", [
function UpdateTimer():
    // Update system timer
    current_time = GetSystemTime()
    timer_delta = current_time - last_timer_time
    last_timer_time = current_time

    // Check for timer-based events
    CheckTimerEvents()
])

== Stack Overflow Handling

=== Overflow Detection

#codeblock(lang: "pseudocode", [
function CheckStackOverflow():
    if CurrentStackPTR < Irq_Stk_Check:
        if Irq_Stk_End > 0 and Irq_Stk_Check > 0:
            return true
    return false
])

=== Overflow Processing

#codeblock(lang: "pseudocode", [
function ProcessStackOverflow():
    // Save TOS
    PushCStack(TopOfStack)

    // Attempt stack extension
    if ExtendStack():
        // Stack extended successfully
        PopStack()
        ResetOverflowFlags()
    else:
        // Stack extension failed
        SetInterruptFlag(stackoverflow)
        CauseInterruptCall(STACKOVERFLOW_FRAME)
        // May trigger hard reset
])

== Interrupt Call Mechanism

=== Cause Interrupt Call

#codeblock(lang: "pseudocode", [
function CauseInterruptCall(frame_index):
    // Get interrupt frame
    interrupt_frame = GetInterruptFrame(frame_index)

    // Save current state
    SaveCurrentState()

    // Set up interrupt frame
    SetCurrentFrame(interrupt_frame)

    // Transfer control to interrupt handler
    PC = interrupt_frame.code_start
    ContinueDispatch()
])

=== Interrupt Frame Types

- *KEYBOARD_FRAME*: Keyboard interrupt handler
- *MOUSE_FRAME*: Mouse interrupt handler
- *NETWORK_FRAME*: Network interrupt handler
- *TIMER_FRAME*: Timer interrupt handler
- *STACKOVERFLOW_FRAME*: Stack overflow handler
- *STORAGEFULL_FRAME*: Storage full handler

== Interrupt Disabling

=== Disable Interrupts

#codeblock(lang: "pseudocode", [
function DisableInterrupts():
    interrupt_state.gcdisabled = true
    // Interrupts checked but not processed
])

=== Enable Interrupts

#codeblock(lang: "pseudocode", [
function EnableInterrupts():
    interrupt_state.gcdisabled = false
    // Interrupts processed normally
])

== Async Interrupt Emulation

=== Timer Emulation

#codeblock(lang: "pseudocode", [
function EmulateTimerInterrupts():
    instruction_count = instruction_count - 1
    if instruction_count <= 0:
        SetInterruptFlag(IOInterrupt)
        instruction_count = INSTRUCTIONS_PER_INTERRUPT
])

=== Async Event Emulation

#codeblock(lang: "pseudocode", [
function EmulateAsyncInterrupts():
    if IO_Signalled:
        IO_Signalled = false
        ProcessIOEvents()
        SetInterruptFlag(IOInterrupt)
])

== Related Documentation

- Execution Model - Where interrupts are checked
- I/O Systems - I/O interrupt sources
- Stack Management - Stack overflow handling
