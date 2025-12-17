= Sysout Files


== Overview

Sysout files are binary files containing a complete Lisp system state that Maiko can load and execute. They serve as the starting point for Medley sessions, providing a pre-initialized Lisp environment with code, data, and execution state.

== Sysout File Types

=== lisp.sysout

Minimal Interlisp and Common Lisp environment.

*Contents*: - Basic Interlisp implementation
- Basic Common Lisp implementation
- Minimal functionality pointerUse Cases: - Stripped-down Medley sessions
- Testing minimal functionality - Base for custom loadups pointerLocation: `MEDLEYDIR/loadups/lisp.sysout`

*Creation*: Created in loadup Stage 3 (Lisp)

*See*: Loadup Workflow Component for loadup process details

=== full.sysout

Complete Interlisp and Common Lisp environment with development tools.

*Contents*: - Everything in lisp.sysout
- Complete Interlisp implementation
- Complete Common Lisp implementation
- Standard development tools (TEdit, etc.)
- Modernizations and updates pointerUse Cases: - Primary sysout for running Medley sessions
- Development work - Standard Medley functionality pointerLocation: `MEDLEYDIR/loadups/full.sysout`

*Creation*: Created in loadup Stage 4 (Full)

*See*: Loadup Workflow Component for loadup process details

=== apps.sysout

Full sysout plus Medley applications.

*Contents*: - Everything in full.sysout
- Medley applications:
  - Notecards
  - Rooms (window/desktop manager)
  - CLOS (Common Lisp Object System)
  - Buttons
- Pre-installed links to key Medley documentation pointerUse Cases: - Running Medley with applications
- Using Notecards, Rooms, CLOS - Complete Medley experience pointerLocation: `MEDLEYDIR/loadups/apps.sysout`

*Creation*: Created in loadup Stage 5 (Apps)

*See*: Loadup Workflow Component for loadup process details

== Sysout File Format

Sysout files are binary files containing:
- *Lisp Heap State*: Complete memory image of Lisp heap
- *Code*: Compiled Lisp code (bytecode)
- *Data*: Lisp data structures, symbols, atoms
- *Execution State*: Stack frames, program counters (if applicable)
- *System State*: System variables, configuration pointerFormat Specification: Binary format compatible with Maiko virtual memory format.

*See*: Interface - File Formats for detailed format specification pointerRelated Maiko Documentation: - `../../rewrite-spec/data-structures/sysout-format.md` - Complete sysout format specification
- `../../components/memory-management.md` - Memory management and sysout loading
- `../../architecture.md` - Maiko system architecture

== Loading Process

=== Script Resolution

Medley scripts resolve sysout files in this order:

1. *Explicit sysout argument*: Use specified file path
2. *`-f, --full` flag*: `MEDLEYDIR/loadups/full.sysout`
3. *`-l, --lisp` flag*: `MEDLEYDIR/loadups/lisp.sysout`
4. *`-a, --apps` flag*: `MEDLEYDIR/loadups/apps.sysout`
5. *No flag/argument*: Check for vmem file (session continuation) -Source Code Reference: medley/scripts/medley/medley_args.sh - sysout resolution logic

=== Maiko Loading

Maiko loads sysout files during initialization:

1. *File Opening*: Maiko opens sysout file
2. *Memory Mapping*: Maiko maps sysout contents to virtual memory
3. *Initialization*: Maiko initializes Lisp system from sysout state
4. *Execution*: Maiko begins executing Lisp system pointerSee: Interface - Protocols for startup sequence details pointerRelated Maiko Documentation: See Maiko memory management documentation for sysout loading implementation.

== Relationship to Lisp System State

Sysout files represent a snapshot of Lisp system state:
- *Code State*: All loaded Lisp code
- *Data State*: All Lisp data structures
- *Symbol State*: Atom table and symbol bindings
- *System State*: System variables and configuration

When Maiko loads a sysout file, it restores this complete state, allowing Medley to continue from that point.

== Session Continuation

Sysout files are the starting point, but session state is saved in vmem files:

1. *Startup*: Load sysout file (initial state)
2. *Execution*: Run Medley session
3. *Exit*: Save session state to vmem file
4. *Next Startup*: Load vmem file (if present) to continue session, otherwise load sysout file pointerSee: Virtual Memory Files Component for vmem file details

== Creating Sysout Files

Sysout files are created by the loadup process:

=== Loadup Stages

1. *Init*: Create `init.dlinit` (internal, not copied to loadups)
2. *Mid*: Create `init-mid.sysout` (internal, not copied to loadups)
3. *Lisp*: Create `lisp.sysout` (copied to loadups)
4. *Full*: Create `full.sysout` (copied to loadups)
5. *Apps*: Create `apps.sysout` (copied to loadups) -See: Loadup Workflow Component for complete loadup process

=== Loadup Scripts

- `loadup-all.sh`: Orchestrates stages 1-4 and 6 (optionally 5)
- `loadup-init.sh`: Stage 1
- `loadup-mid-from-init.sh`: Stage 2
- `loadup-lisp-from-mid.sh`: Stage 3
- `loadup-full-from-lisp.sh`: Stage 4 - `loadup-apps-from-full.sh`: Stage 5

*Source Code Reference*: medley/scripts/loadups/ - loadup scripts

== File Locations

=== Standard Locations
- *lisp.sysout*: `MEDLEYDIR/loadups/lisp.sysout`
- *full.sysout*: `MEDLEYDIR/loadups/full.sysout`
- *apps.sysout*: `MEDLEYDIR/loadups/apps.sysout`

=== Custom Sysout Files

Sysout files can be created with custom names and stored in any location. Specify the path explicitly as a command-line argument:

[`medley /path/to/custom.sysout`]

== Usage Examples

=== Starting from full.sysout

[`medley -f`]
[`# or`]
[`medley --full`]

=== Starting from lisp.sysout

[`medley -l`]
[`# or`]
[`medley --lisp`]

=== Starting from apps.sysout

[`medley -a`]
[`# or`]
[`medley --apps`]

=== Starting from custom sysout

[`medley /path/to/custom.sysout`]

=== Session continuation (no sysout specified)

[`medley`]
[`# Loads vmem file if present, otherwise defaults to full.sysout`]

== Related Documentation
- *Architecture*: Architecture Overview - System architecture
- *Scripts*: Scripts Component - Script system and argument parsing
- *Virtual Memory Files*: Virtual Memory Files Component - Vmem files and session persistence
- *Loadup Workflow*: Loadup Workflow Component - Loadup process and sysout creation
- *Interface - File Formats*: File Formats - File format specifications
- *Interface - Protocols*: Protocols - Startup sequence and loading protocols
