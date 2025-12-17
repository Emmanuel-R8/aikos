= Graphics Operations Specification


Complete specification of graphics operations, including BitBLT, line drawing, and rendering semantics.

== Overview

Graphics operations manipulate the display region memory and update the screen. The primary operation is BitBLT (Bit-Block Transfer), which copies rectangular regions with various operations.

== BitBLT Operation

=== BitBLT Overview

BitBLT copies a rectangular region from source to destination with a specified operation:

[`function BitBLT(source_base, dest_base, source_x, dest_x, width, height,`]
[`                source_bpl, dest_bpl, backward_flag, source_type, operation`]:
    // Copy rectangular region with operation
    // source_base: Base address of source bitmap
    // dest_base: Base address of destination bitmap
    // source_x, dest_x: X coordinates (in bits for monochrome)
    // width, height: Region dimensions
    // source_bpl, dest_bpl: Bytes per line
    // backward_flag: Copy backward (for overlapping regions)
    // source_type: Source type (INPUT, TEXTURE, etc.)
    // operation: Operation (COPY, XOR, AND, OR, etc.))

=== BitBLT Parameters pointerPILOTBBT Structure: [`struct PILOTBBT:`]
[`    pbtwidth: int           // Width in pixels/bits`]
[`    pbtheight: int          // Height in pixels/bits`]
[`    pbtsourcehi: uint       // Source high address bits`]
[`    pbtsourcelo: uint       // Source low address bits`]
[`    pbtdesthi: uint         // Destination high address bits`]
[`    pbtdestlo: uint         // Destination low address bits`]
[`    pbtsourcebit: int       // Source X bit offset`]
[`    pbtdestbit: int         // Destination X bit offset`]
[`    pbtsourcebpl: int       // Source bytes per line`]
[`    pbtdestbpl: int         // Destination bytes per line`]
[`    pbtsourcetype: int      // Source type code`]
[`    pbtoperation: int       // Operation code`]
[`    pbtbackward: int        // Backward copy flag`]
[`    pbtusegray: int         // Use gray pattern flag`]

=== BitBLT Algorithm

[`function ExecuteBitBLT(pilot_bbt`]:
    // Extract parameters
    width = pilot_bbt.pbtwidth
    height = pilot_bbt.pbtheight
    source_x = pilot_bbt.pbtsourcebit
    dest_x = pilot_bbt.pbtdestbit

    // Calculate addresses
    source_base = VAG2(pilot_bbt.pbtsourcehi, pilot_bbt.pbtsourcelo)
    dest_base = VAG2(pilot_bbt.pbtdesthi, pilot_bbt.pbtdestlo)

    // Lock screen
    LockScreen()

    // Check if cursor needs hiding
    if CursorInRegion(dest_base, dest_x, width, height):
        HideCursor()

    // Perform BitBLT
    if pilot_bbt.pbtbackward:
        BitBLTBackward(source_base, dest_base, source_x, dest_x,
                       width, height, source_bpl, dest_bpl, operation)
    else:
        BitBLTForward(source_base, dest_base, source_x, dest_x,
                      width, height, source_bpl, dest_bpl, operation)

    // Flush display if destination is display region
    if IsDisplayRegion(dest_base):
        FlushDisplayRegion(dest_x, dest_y, width, height)

    // Show cursor if hidden
    if CursorWasHidden():
        ShowCursor()

    UnlockScreen())

== Graphics Operations

=== Operation Types pointerCOPY (REPLACE):

[`function OperationCOPY(source, destination`]:
    destination = source)

*XOR*:

[`function OperationXOR(source, destination`]:
    destination = source XOR destination)

*AND*:

[`function OperationAND(source, destination`]:
    destination = source AND destination)

*OR*:

[`function OperationOR(source, destination`]:
    destination = source OR destination)

*NOT*:

[`function OperationNOT(source, destination`]:
    destination = NOT source)

=== Source Types pointerINPUT: Source is input bitmap pointerTEXTURE: Source is texture pattern pointerMERGE: Source is merge pattern pointerGRAY: Source uses gray pattern

== Line Drawing

=== Line Drawing Algorithm

[`function DrawLine(x1, y1, x2, y2, operation`]:
    // Bresenham's line algorithm
    dx = abs(x2 - x1)
    dy = abs(y2 - y1)
    sx = sign(x2 - x1)
    sy = sign(y2 - y1)
    err = dx - dy

    x = x1
    y = y1

    while true:
        SetPixel(x, y, operation)
        if x == x2 and y == y2:
            break

        e2 = 2 * err
        if e2 >* -dy:
            err -= dy
            x += sx
        if e2 < dx:
            err += dx
            y += sy)

== Character Rendering

=== Character BitBLT

[`function RenderCharacter(char_code, x, y`]:
    // Get character bitmap
    char_bitmap = GetCharacterBitmap(char_code)

    // Calculate source and destination
    source_base = char_bitmap.base
    dest_base = DisplayRegion68k
    source_x = 0
    dest_x = x
    width = char_bitmap.width
    height = char_bitmap.height

    // Perform BitBLT
    BitBLT(source_base, dest_base, source_x, dest_x, width, height,
           char_bitmap.bpl, DisplayBytesPerLine, false, INPUT, COPY)

    // Flush display
    FlushDisplayRegion(x, y, width, height))

== Display Flushing

=== Flush Display Region

[`function FlushDisplayRegion(x, y, width, height`]:
    // Check if region is in display
    if not InDisplayRegion(x, y, width, height):
        return

    // Get display backend
    backend = GetDisplayBackend()

    // Flush region to screen
    backend.FlushRegion(x, y, width, height))

=== Platform-Specific Flushing pointerX11: [`function X11FlushRegion(x, y, width, height`]:
    // Copy from display region to XImage
    XPutImage(display, window, gc, ximage, x, y, x, y, width, height)
    XFlush(display))

*SDL*:

[`function SDLFlushRegion(x, y, width, height`]:
    // Update texture from display region
    SDL_UpdateTexture(texture, rect, pixels, pitch)
    SDL_RenderCopy(renderer, texture, NULL, NULL)
    SDL_RenderPresent(renderer))

== Screen Locking

=== Lock/Unlock Screen

[`function LockScreen(`]:
    ScreenLocked = true
    // Prevent concurrent access to display

function UnlockScreen():
    ScreenLocked = false
    // Allow display access)

*Purpose*: Prevent race conditions during graphics operations

== Cursor Management

=== Hide/Show Cursor

[`function HideCursor(`]:
    if CursorVisible:
        SaveCursorArea()
        HideCursorOnScreen()
        CursorVisible = false

function ShowCursor():
    if not CursorVisible:
        RestoreCursorArea()
        ShowCursorOnScreen()
        CursorVisible = true)

*Purpose*: Hide cursor during BitBLT to prevent flicker

== Related Documentation

- Interface Abstraction - Display interface contract
- Event Protocols - Input event handling - Platform Abstraction - Platform-specific details
