= Common Lisp Display Implementation

*Navigation*: Spec | C Implementation | Zig Implementation

This document describes the Common Lisp implementation of the Display subsystem.

== Overview

The Common Lisp display implementation targets SDL3 via CFFI or cl-sdl3.

== Key Components

=== SDL Backend

- SDL3 integration for graphics output
- BitBLT operations in `src/display/graphics.lisp`
- Event handling in `src/display/events.lisp`

=== Graphics Operations

- BitBLT rendering
- Coordinate translation

== Key Implementation Decisions

- *SDL3*: Target display backend
- *CFFI*: Foreign function interface for SDL

== Status

- ⏳ Implementation planned
- SDL3 targeting

See `implementations/lisp-implementation.typ` for details.