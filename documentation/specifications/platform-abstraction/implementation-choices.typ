= Implementation Choices Specification

*Navigation*: README | Required Behaviors

Complete specification of implementation choices that MAY differ across platforms while maintaining
compatibility.

== Overview

Implementation choices are aspects where different implementations may use different approaches, as
long as the required behaviors match. These allow platform-specific optimizations and adaptations.

== VM Core Implementation Choices

=== Dispatch Mechanism

*MAY DIFFER*:

- *Computed Goto*: GCC computed goto (fastest)
- *Switch Statement*: Standard C switch (portable)
- *Function Table*: Array of function pointers
- *Other*: Any mechanism that executes opcodes correctly

*Constraint*: Must execute opcodes identically regardless of dispatch method.

=== Stack Allocation

*MAY DIFFER*:

- *Allocation Strategy*: Pre-allocate vs grow-on-demand
- *Memory Management*: Native malloc vs custom allocator
- *Stack Extension*: When and how to extend stack

*Constraint*: Stack frame layout and behavior must match.

=== Instruction Caching

*MAY DIFFER*:

- *PC Caching*: Cache PC in register vs always read from memory
- *Instruction Prefetch*: Prefetch next instruction
- *No Caching*: Always fetch from memory

*Constraint*: Execution semantics must match.

== Display Implementation Choices

=== Graphics Library

*MAY DIFFER*:

- *SDL*: Simple DirectMedia Layer (current recommended)
- *X11*: X Window System (legacy, still available)
- *DirectX*: Windows DirectX
- *Metal*: macOS Metal
- *Vulkan*: Vulkan API
- *Other*: Any graphics library

*Deprecated/Removed*:
- *VESA*: VESA VBE graphics (removed 2026-02-19 - obsolete DOS-era display)
- *VGA*: VGA graphics (removed 2026-02-19 - obsolete DOS-era display)

*Constraint*: Must provide required display operations and match graphics semantics.

=== Window Management

*MAY DIFFER*:

- *Window Decoration*: Title bar style, borders
- *Window Manager*: Integration with window manager
- *Multi-Window*: Single vs multiple windows

*Constraint*: Content area and coordinate system must match.

=== Event Delivery

*MAY DIFFER*:

- *Polling*: Poll for events periodically
- *Callbacks*: Event-driven callbacks
- *Signals*: Signal-based event notification

*Constraint*: Events must be available and correctly translated.

=== Cursor Appearance

*MAY DIFFER*:

- *Visual Appearance*: Cursor bitmap appearance
- *Size*: Cursor size (as long as hotspot correct)
- *Animation*: Animated vs static cursor

*Constraint*: Cursor hotspot position must be correct.

== I/O Implementation Choices

=== File System APIs

*MAY DIFFER*:

- *POSIX*: Standard POSIX file operations
- *Windows API*: Windows file operations
- *Platform-Specific*: Native file system APIs

*Constraint*: Pathname translation and file I/O semantics must match.

=== Case Sensitivity

*MAY DIFFER*:

- *Case-Sensitive*: Preserve case exactly (Unix)
- *Case-Insensitive*: Case-insensitive matching (Windows)
- *Case-Preserving*: Preserve case but match case-insensitively (macOS)

*Constraint*: Must handle Lisp pathname case correctly per platform conventions.

=== Network Backend

*MAY DIFFER*:

- *Nethub*: TCP-based emulation (current recommended)
- *DLPI*: Data Link Provider Interface (Solaris)
- *Raw Sockets*: Raw socket access
- *Other*: Platform-specific network APIs

*Deprecated/Removed*:
- *NIT*: Network Interface Tap (obsolete, removed from active support)

*Constraint*: Network packet format and protocol semantics must match.

== Memory Management Implementation Choices

=== GC Implementation

*MAY DIFFER*:

- *Hash Table Implementation*: Different hash table structures (as long as semantics match)
- *Overflow Handling*: Different overflow table implementations
- *Scanning Strategy*: Different GC scanning order

*Constraint*: Reference counting algorithm and reclamation behavior must match.

=== Memory Allocation

*MAY DIFFER*:

- *Allocator*: Different memory allocators
- *Page Management*: Different page allocation strategies
- *Fragmentation Handling*: Different fragmentation strategies

*Constraint*: Memory layout and allocation behavior must match.

== Performance Optimizations

=== Optimization Techniques

*MAY DIFFER*:

- *Inlining*: Function inlining
- *Loop Unrolling*: Instruction loop unrolling
- *Register Allocation*: Different register usage
- *Cache Management*: Different caching strategies

*Constraint*: Must not change execution semantics.

=== Profiling and Debugging

*MAY DIFFER*:

- *Debugging Tools*: Platform-specific debuggers
- *Profiling*: Different profiling mechanisms
- *Tracing*: Different tracing implementations

*Constraint*: Must not affect normal execution.

== Platform-Specific Adaptations

=== Operating System Differences

*MAY DIFFER*:

- *System Calls*: Different OS system call interfaces
- *Signal Handling*: Different signal mechanisms
- *Process Management*: Different process APIs

*Constraint*: VM behavior must match regardless of OS.

=== Architecture Differences

*MAY DIFFER*:

- *Byte Order*: Handle endianness differences
- *Word Size*: Adapt to different word sizes (while maintaining 16-bit words internally)
- *Alignment*: Handle different alignment requirements

*Constraint*: Data formats and execution must match.

== Related Documentation

- Required Behaviors - What must match
- Platform Abstraction - Overview
- Validation - Compatibility testing
