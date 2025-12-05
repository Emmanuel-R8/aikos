const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");
const sdl_backend = @import("sdl_backend.zig");

const DLword = types.DLword;

/// Graphics operation types
pub const GraphicsOperation = enum {
    COPY,
    XOR,
    AND,
    OR,
    NOT,
};

/// Render region (BitBLT)
/// Per contracts/display-interface.zig
pub fn renderRegion(
    display: *sdl_backend.DisplayInterface,
    source_x: u32,
    source_y: u32,
    width: u32,
    height: u32,
    dest_x: u32,
    dest_y: u32,
    operation: GraphicsOperation,
) errors.DisplayError!void {
    _ = display;
    _ = source_x;
    _ = source_y;
    _ = width;
    _ = height;
    _ = dest_x;
    _ = dest_y;
    _ = operation;
    // TODO: Implement BitBLT operation
    // 1. Calculate source and destination addresses
    // 2. Perform pixel operation based on operation type
    // 3. Update display region buffer
}

/// Flush display region
/// Per contracts/display-interface.zig
pub fn flushDisplayRegion(
    display: *sdl_backend.DisplayInterface,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
) void {
    _ = display;
    _ = x;
    _ = y;
    _ = width;
    _ = height;
    // TODO: Copy display region buffer to SDL texture
    // TODO: Update SDL renderer
}

/// Update display
/// Per contracts/display-interface.zig
pub fn updateDisplay(display: *sdl_backend.DisplayInterface) void {
    _ = display;
    // TODO: Present SDL renderer
}