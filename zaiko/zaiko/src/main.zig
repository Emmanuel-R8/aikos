const std = @import("std");
const types = @import("utils/types.zig");
const errors = @import("utils/errors.zig");
const stack = @import("vm/stack.zig");
const dispatch = @import("vm/dispatch.zig");
const sysout = @import("data/sysout.zig");
const storage = @import("memory/storage.zig");
const gc_module = @import("memory/gc.zig");

/// Command-line options structure
const Options = struct {
    sysout_path: ?[]const u8 = null,
    screen_width: ?u32 = null,
    screen_height: ?u32 = null,
    pixel_scale: ?u32 = null,
    foreground_color: ?[]const u8 = null,
    background_color: ?[]const u8 = null,
    window_title: ?[]const u8 = null,
    timer_interval: ?u32 = null,
    memory_size_mb: ?u32 = null,
    show_info: bool = false,
    show_help: bool = false,
    no_fork: bool = false,
    init_mode: bool = false,
};

const help_string =
    \\either setenv LDESRCESYSOUT or do:
    \\medley [<sysout-name>] [<options>]
    \\-info                    Print general info about the system
    \\-help                    Print this message
    \\-sysout <file>           Specify sysout file
    \\-pixelscale <n>          The amount of pixels to show for one Medley screen pixel.
    \\-fg/-foreground <color>  Screen foreground color, default Black.  X color name or #RRBBGG hex
    \\-bg/-background <color> Screen background color, default White.  X color name or #RRBBGG hex
    \\-sc[reen] <w>x<h>       The Medley screen geometry
    \\-t/-title <title>       The window title
    \\-timer <interval>       Timer interval (undocumented)
    \\-m <size>               Virtual memory size in Mega Bytes (undocumented)
    \\-NF                      Don't fork (for debugging)
    \\-INIT                    Init sysout, no packaged (undocumented)
    \\
;

fn printInfo() void {
    std.debug.print("Emulator for Medley Interlisp (Zig implementation)\n", .{});
    std.debug.print("Version: 0.1.0\n", .{});
    std.debug.print("Built with Zig 0.15.1\n", .{});
}

fn parseCommandLine(args: []const []const u8) !Options {
    var options = Options{};
    var i: usize = 1; // Skip program name

    // Check for -info or -help first
    while (i < args.len) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-info") or std.mem.eql(u8, arg, "-INFO")) {
            options.show_info = true;
            return options;
        }
        if (std.mem.eql(u8, arg, "-help") or std.mem.eql(u8, arg, "-HELP") or
            std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "--HELP"))
        {
            options.show_help = true;
            return options;
        }
        i += 1;
    }

    // Reset and parse all options
    i = 1;
    while (i < args.len) {
        const arg = args[i];

        // Skip -info and -help (already handled)
        if (std.mem.eql(u8, arg, "-info") or std.mem.eql(u8, arg, "-INFO") or
            std.mem.eql(u8, arg, "-help") or std.mem.eql(u8, arg, "-HELP"))
        {
            i += 1;
            continue;
        }

        // -sysout <file>
        if (std.mem.eql(u8, arg, "-sysout")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -sysout\n", .{});
                return error.InvalidArgument;
            }
            options.sysout_path = args[i];
            i += 1;
            continue;
        }

        // -sc or -screen <width>x<height>
        if (std.mem.eql(u8, arg, "-sc") or std.mem.eql(u8, arg, "-SC") or
            std.mem.eql(u8, arg, "-screen") or std.mem.eql(u8, arg, "-SCREEN"))
        {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -sc\n", .{});
                return error.InvalidArgument;
            }
            const geometry = args[i];
            // Find 'x' separator
            if (std.mem.indexOfScalar(u8, geometry, 'x')) |x_pos| {
                const width_str = geometry[0..x_pos];
                const height_str = geometry[x_pos + 1..];
                const width = std.fmt.parseInt(u32, width_str, 10) catch {
                    std.debug.print("Could not parse width in -sc argument {s}\n", .{geometry});
                    return error.InvalidArgument;
                };
                const height = std.fmt.parseInt(u32, height_str, 10) catch {
                    std.debug.print("Could not parse height in -sc argument {s}\n", .{geometry});
                    return error.InvalidArgument;
                };
                options.screen_width = width;
                options.screen_height = height;
            } else {
                std.debug.print("Could not parse -sc argument {s} (expected WIDTHxHEIGHT)\n", .{geometry});
                return error.InvalidArgument;
            }
            i += 1;
            continue;
        }

        // -pixelscale <n>
        if (std.mem.eql(u8, arg, "-pixelscale") or std.mem.eql(u8, arg, "-PIXELSCALE")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -pixelscale\n", .{});
                return error.InvalidArgument;
            }
            const scale = try std.fmt.parseInt(u32, args[i], 10);
            options.pixel_scale = scale;
            i += 1;
            continue;
        }

        // -fg or -foreground <color>
        if (std.mem.eql(u8, arg, "-fg") or std.mem.eql(u8, arg, "-FG") or
            std.mem.eql(u8, arg, "-foreground") or std.mem.eql(u8, arg, "-FOREGROUND"))
        {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -fg/-foreground\n", .{});
                return error.InvalidArgument;
            }
            options.foreground_color = args[i];
            i += 1;
            continue;
        }

        // -bg or -background <color>
        if (std.mem.eql(u8, arg, "-bg") or std.mem.eql(u8, arg, "-BG") or
            std.mem.eql(u8, arg, "-background") or std.mem.eql(u8, arg, "-BACKGROUND"))
        {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -bg/-background\n", .{});
                return error.InvalidArgument;
            }
            options.background_color = args[i];
            i += 1;
            continue;
        }

        // -t or -title <title>
        if (std.mem.eql(u8, arg, "-t") or std.mem.eql(u8, arg, "-T") or
            std.mem.eql(u8, arg, "-title") or std.mem.eql(u8, arg, "-TITLE"))
        {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -t/-title\n", .{});
                return error.InvalidArgument;
            }
            options.window_title = args[i];
            i += 1;
            continue;
        }

        // -timer <interval>
        if (std.mem.eql(u8, arg, "-timer")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -timer\n", .{});
                return error.InvalidArgument;
            }
            const interval = try std.fmt.parseInt(u32, args[i], 10);
            if (interval == 0) {
                std.debug.print("Bad value for -timer (integer > 0)\n", .{});
                return error.InvalidArgument;
            }
            options.timer_interval = interval;
            i += 1;
            continue;
        }

        // -m <size>
        if (std.mem.eql(u8, arg, "-m")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after -m\n", .{});
                return error.InvalidArgument;
            }
            const size = try std.fmt.parseInt(u32, args[i], 10);
            if (size == 0) {
                std.debug.print("Bad value for -m (integer > 0)\n", .{});
                return error.InvalidArgument;
            }
            options.memory_size_mb = size;
            i += 1;
            continue;
        }

        // -NF (no fork)
        if (std.mem.eql(u8, arg, "-NF")) {
            options.no_fork = true;
            i += 1;
            continue;
        }

        // -INIT
        if (std.mem.eql(u8, arg, "-INIT")) {
            options.init_mode = true;
            i += 1;
            continue;
        }

        // If argument doesn't start with '-', treat as sysout file name
        if (arg[0] != '-') {
            if (options.sysout_path == null) {
                options.sysout_path = arg;
            } else {
                std.debug.print("Warning: Multiple sysout files specified, ignoring {s}\n", .{arg});
            }
            i += 1;
            continue;
        }

        // Unknown option
        std.debug.print("Unknown option: {s}\n", .{arg});
        std.debug.print("Use -help for usage information\n", .{});
        return error.InvalidArgument;
    }

    return options;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const options = parseCommandLine(args) catch |err| {
        std.debug.print("Error parsing command line: {}\n", .{err});
        return;
    };

    // Handle -info
    if (options.show_info) {
        printInfo();
        return;
    }

    // Handle -help
    if (options.show_help) {
        std.debug.print("{s}", .{help_string});
        return;
    }

    // Determine sysout path
    const sysout_path = options.sysout_path orelse blk: {
        // Check environment variable
        if (std.process.getEnvVarOwned(allocator, "LDESRCESYSOUT")) |env_path| {
            break :blk env_path;
        } else |_| {
            std.debug.print("No sysout file specified. Use -sysout <file> or set LDESRCESYSOUT environment variable.\n", .{});
            std.debug.print("Use -help for usage information.\n", .{});
            return;
        }
    };

    std.debug.print("Loading sysout file: {s}\n", .{sysout_path});

    // Print parsed options (for debugging)
    if (options.screen_width) |w| {
        std.debug.print("Screen geometry: {}x{}\n", .{ w, options.screen_height.? });
    }
    if (options.pixel_scale) |scale| {
        std.debug.print("Pixel scale: {}\n", .{scale});
    }
    if (options.window_title) |title| {
        std.debug.print("Window title: {s}\n", .{title});
    }
    if (options.timer_interval) |interval| {
        std.debug.print("Timer interval: {}\n", .{interval});
    }
    if (options.memory_size_mb) |size| {
        std.debug.print("Memory size: {} MB\n", .{size});
    }

    // Initialize VM
    const stack_size = 1024 * 1024; // 1MB stack
    var vm = try stack.VM.init(allocator, stack_size);
    defer vm.deinit();

    // Initialize memory management
    const heap_size = if (options.memory_size_mb) |mb| mb * 1024 * 1024 else 10 * 1024 * 1024; // Default 10MB
    var mem_storage = try storage.Storage.init(allocator, heap_size, 100);
    defer mem_storage.deinit();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    // Load sysout file
    var sysout_result = sysout.loadSysout(allocator, sysout_path) catch |err| {
        std.debug.print("Failed to load sysout file: {}\n", .{err});
        return;
    };
    defer sysout_result.deinit();

    std.debug.print("Sysout loaded: version={}, keyval=0x{x}\n", .{ sysout_result.ifpage.lversion, sysout_result.ifpage.key });

    // Initialize VM state from IFPAGE
    dispatch.initializeVMState(&vm, &sysout_result.ifpage, sysout_result.virtual_memory, sysout_result.fptovp.entries) catch |err| {
        std.debug.print("Failed to initialize VM state: {}\n", .{err});
        return;
    };

    std.debug.print("VM state initialized from IFPAGE\n", .{});
    std.debug.print("  stackbase=0x{x}, endofstack=0x{x}, currentfxp=0x{x}\n", .{
        sysout_result.ifpage.stackbase,
        sysout_result.ifpage.endofstack,
        sysout_result.ifpage.currentfxp,
    });

    // TODO: Initialize SDL display with options (screen_width, screen_height, pixel_scale, colors, title)
    // TODO: Initialize timer with timer_interval if specified

    std.debug.print("VM initialized, entering dispatch loop\n", .{});

    // Enter dispatch loop
    // Dispatch loop now reads bytecode from virtual_memory using PC
    // PC is initialized from current frame (if available) or starts at 0
    dispatch.dispatch(&vm) catch |err| {
        std.debug.print("Dispatch loop error: {}\n", .{err});
        return;
    };

    std.debug.print("VM execution complete\n", .{});
}