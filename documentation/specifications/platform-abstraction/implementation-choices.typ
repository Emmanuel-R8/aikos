= Implementation Choices Specification


Complete specification of implementation choices that MAY differ across platforms while maintaining compatibility.

== Overview

Implementation choices are aspects where different implementations may use different approaches, as long as the required behaviors match. These allow platform-specific optimizations and adaptations.

== VM Core Implementation Choices

=== Dispatch Mechanism pointerMAY DIFFER:
- *Computed Goto*: GCC computed goto (fastest)
- *Switch Statement*: Standard C switch (portable)
- *Function Table*: Array of function pointers
- *Other*: Any mechanism that executes opcodes correctly pointerConstraint: Must execute opcodes identically regardless of dispatch method.

=== Stack Allocation pointerMAY DIFFER:
- *Allocation Strategy*: Pre-allocate vs grow-on-demand
- *Memory Management*: Native malloc vs custom allocator
- *Stack Extension*: When and how to extend stack pointerConstraint: Stack frame layout and behavior must match.

=== Instruction Caching pointerMAY DIFFER:
- *PC Caching*: Cache PC in register vs always read from memory
- *Instruction Prefetch*: Prefetch next instruction
- *No Caching*: Always fetch from memory pointerConstraint: Execution semantics must match.

== Display Implementation Choices

=== Graphics Library pointerMAY DIFFER:
- *X11*: X Window System
- *SDL*: Simple DirectMedia Layer
- *DirectX*: Windows DirectX
- *Metal*: macOS Metal
- *Vulkan*: Vulkan API
- *Other*: Any graphics library pointerConstraint: Must provide required display operations and match graphics semantics.

=== Window Management pointerMAY DIFFER:
- *Window Decoration*: Title bar style, borders
- *Window Manager*: Integration with window manager
- *Multi-Window*: Single vs multiple windows pointerConstraint: Content area and coordinate system must match.

=== Event Delivery pointerMAY DIFFER:
- *Polling*: Poll for events periodically
- *Callbacks*: Event-driven callbacks
- *Signals*: Signal-based event notification pointerConstraint: Events must be available and correctly translated.

=== Cursor Appearance pointerMAY DIFFER:
- *Visual Appearance*: Cursor bitmap appearance
- *Size*: Cursor size (as long as hotspot correct)
- *Animation*: Animated vs static cursor pointerConstraint: Cursor hotspot position must be correct.

== I/O Implementation Choices

=== File System APIs pointerMAY DIFFER:
- *POSIX*: Standard POSIX file operations
- *Windows API*: Windows file operations
- *Platform-Specific*: Native file system APIs pointerConstraint: Pathname translation and file I/O semantics must match.

=== Case Sensitivity pointerMAY DIFFER:
- *Case-Sensitive*: Preserve case exactly (Unix)
- *Case-Insensitive*: Case-insensitive matching (Windows)
- *Case-Preserving*: Preserve case but match case-insensitively (macOS)

*Constraint*: Must handle Lisp pathname case correctly per platform conventions.

=== Network Backend pointerMAY DIFFER:
- *DLPI*: Data Link Provider Interface (Solaris)
- *NIT*: Network Interface Tap (older Unix)
- *Nethub*: TCP-based emulation
- *Raw Sockets*: Raw socket access
- *Other*: Platform-specific network APIs pointerConstraint: Network packet format and protocol semantics must match.

== Memory Management Implementation Choices

=== GC Implementation pointerMAY DIFFER:
- *Hash Table Implementation*: Different hash table structures (as long as semantics match)
- *Overflow Handling*: Different overflow table implementations
- *Scanning Strategy*: Different GC scanning order pointerConstraint: Reference counting algorithm and reclamation behavior must match.

=== Memory Allocation pointerMAY DIFFER:
- *Allocator*: Different memory allocators
- *Page Management*: Different page allocation strategies
- *Fragmentation Handling*: Different fragmentation strategies pointerConstraint: Memory layout and allocation behavior must match.

== Performance Optimizations

=== Optimization Techniques pointerMAY DIFFER:
- *Inlining*: Function inlining
- *Loop Unrolling*: Instruction loop unrolling
- *Register Allocation*: Different register usage
- *Cache Management*: Different caching strategies pointerConstraint: Must not change execution semantics.

=== Profiling and Debugging pointerMAY DIFFER:
- *Debugging Tools*: Platform-specific debuggers
- *Profiling*: Different profiling mechanisms
- *Tracing*: Different tracing implementations pointerConstraint: Must not affect normal execution.

== Platform-Specific Adaptations

=== Operating System Differences pointerMAY DIFFER:
- *System Calls*: Different OS system call interfaces
- *Signal Handling*: Different signal mechanisms
- *Process Management*: Different process APIs pointerConstraint: VM behavior must match regardless of OS.

=== Architecture Differences pointerMAY DIFFER:
- *Byte Order*: Handle endianness differences
- *Word Size*: Adapt to different word sizes (while maintaining 16-bit words internally)
- *Alignment*: Handle different alignment requirements pointerConstraint: Data formats and execution must match.

== Related Documentation

- Required Behaviors - What must match
- Platform Abstraction - Overview
- Validation - Compatibility testing
