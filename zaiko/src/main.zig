const std = @import("std");
const types = @import("utils/types.zig");
const errors = @import("utils/errors.zig");
const stack = @import("vm/stack.zig");
const dispatch = @import("vm/dispatch.zig");
const instrument = @import("vm/instrument.zig");
const sysout = @import("data/sysout.zig");
const storage = @import("memory/storage.zig");
const gc_module = @import("memory/gc.zig");
const sdl_backend = @import("display/sdl_backend.zig");
const events = @import("display/events.zig");
const keyboard = @import("io/keyboard.zig");
const mouse = @import("io/mouse.zig");

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
    max_steps: ?u64 = null, // Step cap for parity comparison (overrides EMULATOR_MAX_STEPS)
    trace: bool = false, // VM instrumentation (step trace, state dumps; also ZAIKO_TRACE=1)
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
                const height_str = geometry[x_pos + 1 ..];
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

        // --max-steps <n> (parity comparison step cap; overrides EMULATOR_MAX_STEPS)
        if (std.mem.eql(u8, arg, "--max-steps")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Missing argument after --max-steps\n", .{});
                return error.InvalidArgument;
            }
            options.max_steps = try std.fmt.parseInt(u64, args[i], 10);
            i += 1;
            continue;
        }

        // --trace (VM instrumentation; also ZAIKO_TRACE=1)
        if (std.mem.eql(u8, arg, "--trace") or std.mem.eql(u8, arg, "-trace")) {
            options.trace = true;
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

/// Main entry point for Medley Interlisp Zig emulator
///
/// EXECUTION SEQUENCE OVERVIEW:
/// 1. Parse command-line arguments
/// 2. Load sysout file (contains frozen Lisp image)
/// 3. Initialize VM state from IFPAGE (Interface Page)
/// 4. Initialize SDL2 display system
/// 5. Initialize memory management (storage, GC)
/// 6. Initialize system state (I/O, timers, etc.)
/// 7. Enter main dispatch loop (execute bytecode + handle events)
///
/// COMPONENT INTERACTIONS:
/// - main.zig: Orchestrates initialization and main loop
/// - sysout: Loads frozen Lisp image from disk
/// - vm/stack.zig: Virtual machine state and stack management
/// - vm/dispatch.zig: Bytecode execution and VM control
/// - memory/storage.zig: Dynamic memory allocation
/// - memory/gc.zig: Garbage collection
/// - display/sdl_backend.zig: SDL2 graphics and windowing
/// - display/events.zig: Input event handling
/// - io/keyboard.zig & io/mouse.zig: Input device management
///
/// MEMORY LAYOUT:
/// - Virtual memory: Loaded from sysout file
/// - Stack: Separate allocation within virtual memory
/// - Heap: Managed by storage/GC system
/// - Display buffer: SDL texture for graphics
///
/// EVENT LOOP:
/// - Poll SDL events (keyboard, mouse, window)
/// - Execute VM instructions (dispatch loop)
/// - Update display when needed
/// - Handle errors gracefully
pub fn main() !void {
    // Memory allocator for the entire application
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

    // PHASE 1: DETERMINE SYSOUT FILE
    // Sysout file contains the frozen Lisp image with pre-compiled code and data
    // This is the "operating system image" that gets loaded into memory
    const sysout_path = options.sysout_path orelse blk: {
        // Check environment variable (matches C emulator behavior)
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

    // PHASE 2: LOAD SYSOUT FILE
    // Load the frozen Lisp image from disk into memory
    // This includes: virtual memory, IFPAGE (interface page), FPtoVP table
    // CRITICAL: Must load before VM initialization to know memory layout
    var sysout_result = sysout.loadSysout(allocator, sysout_path) catch |err| {
        std.debug.print("Failed to load sysout file '{s}': {}\n", .{ sysout_path, err });
        std.debug.print("Please ensure the sysout file exists and is a valid Medley sysout file.\n", .{});
        return;
    };
    defer sysout_result.deinit();

    // PHASE 3: INITIALIZE VIRTUAL MACHINE
    // Create VM instance with stack management
    // Stack is separate from sysout virtual memory but points into it
    const stack_size = 1024 * 1024; // 1MB stack
    var vm = try stack.VM.init(allocator, stack_size);
    defer vm.deinit();

    // PHASE 4: INITIALIZE MEMORY MANAGEMENT
    // Set up dynamic memory allocation and garbage collection
    // Storage manages the heap in the DS (Dynamic Storage) region
    // GC handles reference counting and memory reclamation
    const heap_size = if (options.memory_size_mb) |mb| mb * 1024 * 1024 else 10 * 1024 * 1024; // Default 10MB
    // CRITICAL: Use DS_OFFSET as the lisp_base for storage (matches C implementation)
    // Storage is allocated in the DS (Dynamic Storage) region of the Lisp address space
    const memory_layout = @import("memory/layout.zig");
    var mem_storage = try storage.Storage.init(allocator, heap_size, 100, memory_layout.MemoryOffsets.DS_OFFSET);
    defer mem_storage.deinit();

    var gc = try gc_module.GC.init(allocator, 1024);
    defer gc.deinit();

    std.debug.print("Sysout loaded: version={}, keyval=0x{x}\n", .{ sysout_result.ifpage.lversion, sysout_result.ifpage.key });

    // PHASE 5: INITIALIZE SYSTEM STATE
    // Set up low-level system structures before VM execution
    // Equivalent to C: build_lisp_map(), init_ifpage(), init_iopage(), init_miscstats(), init_storage()
    // Prepares virtual memory layout and system tables
    const init_module = @import("vm/init.zig");
    const sysout_size = sysout_result.ifpage.process_size;
    init_module.initializeSystem(
        sysout_result.virtual_memory,
        &sysout_result.ifpage,
        sysout_size,
    ) catch |err| {
        std.debug.print("Failed to initialize system state: {}\n", .{err});
        return;
    };
    std.debug.print("System state initialized\n", .{});

    // PHASE 7: INITIALIZE VM STATE FROM IFPAGE
    // Set up VM registers, stack pointers, and program counter from IFPAGE data
    // This is equivalent to C's start_lisp() function
    // CRITICAL: Establishes the initial execution state for the Lisp system
    dispatch.initializeVMState(&vm, &sysout_result.ifpage, sysout_result.virtual_memory, &sysout_result.fptovp) catch |err| {
        std.debug.print("Failed to initialize VM state: {}\n", .{err});
        return;
    };

    // Set storage and GC so CONS and other allocation opcodes can run (avoids MemoryAccessFailed at ~40 steps)
    vm.storage = &mem_storage;
    vm.gc = &gc;

    std.debug.print("VM state initialized from IFPAGE\n", .{});
    std.debug.print("  stackbase=0x{x}, endofstack=0x{x}, currentfxp=0x{x}\n", .{
        sysout_result.ifpage.stackbase,
        sysout_result.ifpage.endofstack,
        sysout_result.ifpage.currentfxp,
    });

    // PHASE 6: INITIALIZE DISPLAY (optional for headless / parity comparison)
    // When display init fails (e.g. no video device), run headless: VM only, no events.
    const screen_width = options.screen_width orelse 1024;
    const screen_height = options.screen_height orelse 768;
    const pixel_scale = options.pixel_scale orelse 1;
    const window_title = options.window_title orelse "Medley Interlisp (Zig)";

    const display_opt: ?*sdl_backend.DisplayInterface = blk: {
        break :blk sdl_backend.initDisplay(
            window_title,
            screen_width,
            screen_height,
            pixel_scale,
            allocator,
        ) catch |err| {
            std.debug.print("SDL2 display unavailable ({}), running headless for trace/parity\n", .{err});
            break :blk null;
        };
    };
    const display = display_opt;
    defer if (display) |d| sdl_backend.destroyDisplay(d, allocator);

    var key_queue: ?keyboard.KeyEventQueue = null;
    var mouse_state: ?mouse.MouseState = null;
    if (display) |_| {
        key_queue = try keyboard.KeyEventQueue.init(allocator, 256);
        defer key_queue.?.deinit(allocator);
        mouse_state = .{ .x = 0, .y = 0, .buttons = 0 };
        std.debug.print("SDL2 display initialized: {}x{} (scale: {})\n", .{ screen_width, screen_height, pixel_scale });
    }

    std.debug.print("VM initialized, entering dispatch loop\n", .{});

    // Step cap for parity: prefer --max-steps, fallback to EMULATOR_MAX_STEPS (script may pass env but not args)
    var effective_max_steps: ?u64 = options.max_steps;
    if (effective_max_steps == null) {
        if (std.process.getEnvVarOwned(allocator, "EMULATOR_MAX_STEPS")) |env_val| {
            defer allocator.free(env_val);
            const trimmed = std.mem.trim(u8, env_val, " \t\r\n");
            if (trimmed.len > 0) {
                if (std.fmt.parseInt(u64, trimmed, 10)) |n| {
                    if (n > 0) effective_max_steps = n;
                } else |_| {}
            }
        } else |_| {}
    }

    // VM trace: --trace or ZAIKO_TRACE=1 (skip "0" and empty)
    var trace_enabled = options.trace;
    if (!trace_enabled) {
        if (std.process.getEnvVarOwned(allocator, "ZAIKO_TRACE")) |env_val| {
            defer allocator.free(env_val);
            const trimmed = std.mem.trim(u8, env_val, " \t\r\n");
            trace_enabled = trimmed.len > 0 and !std.mem.eql(u8, trimmed, "0");
        } else |_| {}
    }
    instrument.setTraceEnabled(trace_enabled);

    // PHASE 7: MAIN DISPATCH LOOP
    // Core execution loop that alternates between:
    // 1. Polling SDL events (keyboard, mouse, window events)
    // 2. Executing VM instructions (bytecode interpretation)
    // 3. Updating display when needed
    // This is the heart of the emulator - equivalent to C's main event loop
    var quit_requested = false;
    var instruction_count: u64 = 0;
    const max_instructions: u64 = 1000000; // Safety limit to prevent infinite loops

    while (!quit_requested) {
        // Safety check: prevent infinite loops
        instruction_count += 1;
        if (instruction_count > max_instructions) {
            std.debug.print("WARNING: Reached maximum instruction count ({}), stopping execution\n", .{max_instructions});
            break;
        }

        // Poll SDL2 events when display available (skip when headless)
        if (display) |d| {
            const should_quit = events.pollEvents(d, &key_queue.?, &mouse_state.?, allocator) catch |err| blk: {
                std.debug.print("Event polling error: {}\n", .{err});
                break :blk false;
            };
            if (should_quit) {
                quit_requested = true;
                break;
            }
        }

        // Execute VM instructions
        // ERROR HANDLING STRATEGY:
        // - Critical VM errors (stack/pointer corruption): Stop execution immediately
        //   Rationale: Memory corruption could cause crashes or data loss
        //   Implication: Emulator exits safely rather than continuing with invalid state
        // - InvalidOpcode: Log and continue
        //   Rationale: Allows debugging missing opcode implementations
        //   Implication: Execution continues to identify all missing opcodes
        // - Other errors: Log and continue (for debugging incomplete implementations)
        dispatch.dispatch(&vm, effective_max_steps) catch |err| {
            // Plan: identify which error caused dispatch to return (for parity debugging)
            if (effective_max_steps != null) {
                std.debug.print("DISPATCH_RETURNED_ERROR: error={s} (step cap was {}, instruction_count={})\n", .{ @errorName(err), effective_max_steps.?, instruction_count });
            }
            switch (err) {
                // CRITICAL ERRORS: Stop execution immediately
                // These indicate severe VM state corruption that cannot be safely continued
                // Note: StackUnderflow is now handled as non-fatal in dispatch_loop for parity testing
                errors.VMError.StackOverflow, errors.VMError.InvalidAddress, errors.VMError.MemoryAccessFailed, errors.VMError.InvalidStackPointer, errors.VMError.InvalidFramePointer => {
                    std.debug.print("CRITICAL VM ERROR: {}\n", .{err});
                    std.debug.print("Stopping execution due to critical error\n", .{});
                    std.debug.print("Executed {} instructions before error\n", .{instruction_count});
                    quit_requested = true;
                    break;
                },
                // NON-CRITICAL: Invalid opcode - log and continue
                // Rationale: Allows systematic discovery of missing opcode implementations
                // Implication: Execution continues to find all unimplemented opcodes
                errors.VMError.InvalidOpcode => {
                    std.debug.print("VM dispatch error: {}\n", .{err});
                    // Continue execution to see what happens
                },
                // OTHER ERRORS: Log and continue (for debugging)
                // Rationale: During development, continue execution to identify all issues
                // Implication: May lead to cascading errors but helps complete debugging
                else => {
                    std.debug.print("VM dispatch error: {}\n", .{err});
                    // For other errors, continue execution (for debugging)
                },
            }
            // When step cap is set and we've hit it (vm.stop_requested), quit to prevent re-calling dispatch.
            if (effective_max_steps != null and vm.stop_requested) {
                quit_requested = true;
                break;
            }
        };

        // Plan hardening: when step cap is set, do not re-call dispatch after it returns (error or stop).
        // This ensures at most N trace lines when EMULATOR_MAX_STEPS=N, instead of thousands of mini-runs.
        if (effective_max_steps != null) {
            quit_requested = true;
            break;
        }

        // If dispatch requested a stop (e.g. EMULATOR_MAX_STEPS reached), exit main loop.
        if (vm.stop_requested) {
            quit_requested = true;
            break;
        }

        // Small delay to prevent CPU spinning
        std.Thread.sleep(1000); // 1 microsecond
    }

    std.debug.print("VM execution complete (executed {} instructions)\n", .{instruction_count});
}
