= Keyboard Protocol Specification

*Navigation*: README | Mouse Protocol | File System | Network Protocol

Complete specification of keyboard event translation and handling protocol.

== Overview

The keyboard protocol translates platform-specific keycodes to Lisp keycodes and queues events for processing by the VM.

== Keycode Translation

=== Translation Algorithm

#codeblock(lang: "pseudocode", [
function TranslateKeycode(os_keycode, modifiers):
    // Look up base keycode
    base_keycode = KeycodeMap[os_keycode]

    if base_keycode == null:
        // Unknown keycode, use raw value
        base_keycode = os_keycode

    // Apply modifiers
    lisp_keycode = base_keycode

    if modifiers.SHIFT:
        lisp_keycode = ApplyShiftModifier(lisp_keycode)

    if modifiers.CONTROL:
        lisp_keycode = ApplyControlModifier(lisp_keycode)

    if modifiers.META:
        lisp_keycode = ApplyMetaModifier(lisp_keycode)

    return lisp_keycode
])

=== Keycode Map

Lisp uses its own keycode space:

- *ASCII (0x00-0x7F)*: Direct mapping for printable characters
- *Control Characters (0x00-0x1F)*: Control+character combinations
- *Special Keys (0x80-0xFF)*: Function keys, arrows, etc.

=== Modifier Encoding

Modifiers can be encoded in two ways:

1. *Separate flags*: Modifiers as bitmask
2. *Encoded in keycode*: High bits encode modifiers

== Keyboard Event Structure

=== Event Format

#codeblock(lang: "pseudocode", [
struct KeyboardEvent:
    type: KEY_PRESS | KEY_RELEASE
    keycode: uint            // Lisp keycode (after translation)
    modifiers: bitmask       // Modifier flags
    timestamp: uint          // Event timestamp
    os_keycode: uint         // Original OS keycode (for debugging)
])

=== Event Queue

#codeblock(lang: "pseudocode", [
struct KeyEventQueue:
    events: array[KeyboardEvent]
    head: int
    tail: int
    size: int
    buffering: boolean       // Buffering enabled flag
])

== Event Processing

=== Process Keyboard Event

#codeblock(lang: "pseudocode", [
function ProcessKeyboardEvent(os_event):
    // Translate keycode
    lisp_keycode = TranslateKeycode(os_event.keycode, os_event.modifiers)

    // Create Lisp event
    lisp_event = CreateKeyboardEvent(
        type: os_event.type,
        keycode: lisp_keycode,
        modifiers: os_event.modifiers,
        timestamp: GetTimestamp()
    )

    // Queue event
    if KeyEventQueue.buffering:
        EnqueueKeyEvent(lisp_event)
    else:
        ProcessKeyEventImmediately(lisp_event)

    // Set interrupt flag
    SetInterruptFlag(IOInterrupt)
    KBDEventFlg = true
])

=== Key Event Buffering

#codeblock(lang: "pseudocode", [
function EnableKeyBuffering():
    KeyEventQueue.buffering = true

function DisableKeyBuffering():
    KeyEventQueue.buffering = false
    // Process all buffered events
    while not QueueEmpty():
        event = DequeueKeyEvent()
        ProcessKeyEventImmediately(event)
])

== Special Key Handling

=== Function Keys

Function keys (F1-F12) map to special keycodes:

- *F1*: 0x80
- *F2*: 0x81
- *...*
- *F12*: 0x8B

=== Arrow Keys

Arrow keys map to special keycodes:

- *Up*: 0x8C
- *Down*: 0x8D
- *Left*: 0x8E
- *Right*: 0x8F

=== Control Keys

Control key combinations:

- *Control-A through Control-Z*: 0x01-0x1A
- *Control-[*: 0x1B (ESC)
- *Control-\\*: 0x1C
- *Control-]*: 0x1D
- *Control-^*: 0x1E
- *Control-_*: 0x1F

== Platform-Specific Translation

=== X11 Keycode Translation

#codeblock(lang: "pseudocode", [
function X11TranslateKeycode(x_keycode, x_keysym, modifiers):
    // Use keysym for translation
    lisp_keycode = XKeysymToLispKeycode(x_keysym)

    // Apply modifiers
    if modifiers.ShiftMask:
        lisp_keycode = ApplyShift(lisp_keycode)
    if modifiers.ControlMask:
        lisp_keycode = ApplyControl(lisp_keycode)
    if modifiers.Mod1Mask:  // Meta/Alt
        lisp_keycode = ApplyMeta(lisp_keycode)

    return lisp_keycode
])

=== SDL Keycode Translation

#codeblock(lang: "pseudocode", [
function SDLTranslateKeycode(sdl_keycode, sdl_scancode, modifiers):
    // Map SDL keycode to Lisp keycode
    lisp_keycode = SDLKeycodeMap[sdl_keycode]

    // Apply modifiers
    if modifiers & KMOD_SHIFT:
        lisp_keycode = ApplyShift(lisp_keycode)
    if modifiers & KMOD_CTRL:
        lisp_keycode = ApplyControl(lisp_keycode)
    if modifiers & KMOD_ALT:
        lisp_keycode = ApplyMeta(lisp_keycode)

    return lisp_keycode
])

== Interrupt Integration

=== Keyboard Interrupt

#codeblock(lang: "pseudocode", [
function HandleKeyboardInterrupt():
    // Process pending keyboard events
    while HasKeyboardEvents():
        event = GetKeyboardEvent()
        ProcessKeyboardEvent(event)

    // Trigger interrupt handler
    SetInterruptFlag(IOInterrupt)
    TriggerInterruptCall(KEYBOARD_FRAME)
])

== Related Documentation

- Event Protocols - General event handling
- Mouse Protocol - Mouse event handling
- VM Core - Interrupt processing
