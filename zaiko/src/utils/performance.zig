// Performance measurement utilities
// T103-T104: Performance optimization support

const std = @import("std");

/// Performance timer for measuring operation duration
pub const PerformanceTimer = struct {
    start_time: i128,
    
    pub fn start() PerformanceTimer {
        return PerformanceTimer{
            .start_time = std.time.nanoTimestamp(),
        };
    }
    
    pub fn elapsed(self: *const PerformanceTimer) f64 {
        const end_time = std.time.nanoTimestamp();
        const elapsed_ns = @as(f64, @floatFromInt(end_time - self.start_time));
        return elapsed_ns / 1_000_000_000.0; // Convert to seconds
    }
    
    pub fn elapsedMs(self: *const PerformanceTimer) f64 {
        const end_time = std.time.nanoTimestamp();
        const elapsed_ns = @as(f64, @floatFromInt(end_time - self.start_time));
        return elapsed_ns / 1_000_000.0; // Convert to milliseconds
    }
};

/// Measure and report performance of an operation
pub fn measureOperation(comptime name: []const u8, operation: anytype) @TypeOf(operation) {
    var timer = PerformanceTimer.start();
    const result = operation();
    const elapsed = timer.elapsed();
    std.debug.print("Performance: {s} took {d:.3} seconds\n", .{ name, elapsed });
    return result;
}

/// Check if debug output should be enabled
/// Set to false for production builds to improve performance
pub const DEBUG_ENABLED = @import("builtin").mode == .Debug;
