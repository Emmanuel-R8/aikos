= File Format Specifications

*Navigation*: Medley README | Medley Index | Interface Overview

== Overview

This document provides complete specifications for all file formats used in Medley-Maiko communication. These formats enable sysout loading, session persistence, configuration, and initialization.

== Sysout File Format

=== Overview

Sysout files are binary files containing a complete Lisp system state. They are created by the loadup process and loaded by Maiko to initialize Medley sessions.

*See*: Sysout Files Component for sysout file usage and purpose

*Related Maiko Documentation*:

- `../rewrite-spec/data-structures/sysout-format.md` - Complete sysout format specification
- `../components/memory-management.md` - Memory management and sysout loading
- `../architecture.md` - Maiko system architecture

=== File Structure

Sysout files are organized as:

- *Page-based*: File organized into 256-byte pages
- *Sparse*: Not all pages present (FPtoVP table indicates which)
- *Mapped*: FPtoVP table maps file pages to virtual pages

=== File Layout

#codeblock(lang: "text", [
Sysout File
├── Interface Page (IFPAGE)
│   ├── Validation key (IFPAGE_KEYVAL)
│   ├── Lisp version
│   ├── Bytecode version
│   ├── Process size
│   ├── Number of active pages
│   ├── FPtoVP table offset
│   └── VM state (stack, registers, etc.)
├── FPtoVP Table
│   └── Maps file pages to virtual pages
└── Memory Pages
    └── Actual page data mapped by FPtoVP
])

=== Interface Page (IFPAGE)

Located at fixed address: `IFPAGE_ADDRESS`

*Key Fields*:

- `key`: Validation key (must be `IFPAGE_KEYVAL`)
- `lversion`: Lisp version
- `minbversion`: Minimum bytecode version
- `process_size`: Process size in MB
- `nactivepages`: Number of active pages
- `fptovpstart`: FPtoVP table start offset
- `stackbase`, `endofstack`, `currentfxp`: Stack state
- Other VM state fields

*Validation*: Maiko validates IFPAGE key and version compatibility before loading.

*Related Maiko Documentation*: `../rewrite-spec/data-structures/sysout-format.md#interface-page-ifpage` - IFPAGE structure details

=== FPtoVP Table

Maps file page numbers to virtual page numbers.

*Structure*:

- Array of virtual page numbers
- Special value `0177777` (0xFFFF): Page not present in file
- Other values: Virtual page number

*Location*: Offset `ifpage.fptovpstart`, size `nactivepages` entries

*Related Maiko Documentation*: `../rewrite-spec/data-structures/sysout-format.md#fptovp-table` - FPtoVP table details

=== Memory Regions

Sysout files contain these memory regions:

- *Stack Space*: Stack frames and data
- *Atom Space*: Symbol table
- *Heap Space (MDS)*: Cons cells, arrays, code
- *Interface Page*: VM state

*Related Maiko Documentation*: `../rewrite-spec/data-structures/sysout-format.md#memory-regions-in-sysout` - Memory region details

=== Byte Order

- *Byte order*: Little-endian
- *Word order*: 16-bit words in little-endian
- *Byte swapping*: Maiko handles byte swapping if host byte order differs from file byte order

*Related Maiko Documentation*: `../rewrite-spec/data-structures/sysout-format.md#byte-swapping` - Byte swapping details

=== Version Compatibility

Sysout files include version information:

- *Lisp version*: Must be compatible with Maiko
- *Bytecode version*: Must be supported by Maiko

*Related Maiko Documentation*: `../rewrite-spec/data-structures/sysout-format.md#version-compatibility` - Version checking details

== Vmem File Format

=== Overview

Vmem files are binary files storing persistent session state. They enable session continuation across Medley restarts.

*See*: Virtual Memory Files Component for vmem file usage and purpose

=== File Structure

Vmem files contain:

- *Virtual Memory Image*: Complete Lisp heap state
- *Memory Layout*: Virtual memory page mappings
- *System State*: System variables and configuration
- *Session State*: Session-specific state

=== Format Characteristics

- *Binary format*: Platform-specific binary format
- *Platform-specific*: Vmem files are not portable across platforms
- *Writeable*: Must be writeable by user running Medley

=== File Location

Default location: `LOGINDIR/vmem/lisp_{run-id}.virtualmem` or `LOGINDIR/vmem/lisp.virtualmem` (if run ID is "default")

*See*: Virtual Memory Files Component for location resolution

=== Lifecycle

1. *Creation*: Created when Medley exits (if session state changed)
2. *Loading*: Loaded on next startup (if present and no sysout specified)
3. *Update*: Updated on each Medley exit

*Related Maiko Documentation*:

- `../components/memory-management.md` - Virtual memory management
- `../rewrite-spec/memory/virtual-memory.md` - Virtual memory specification

== Config File Format

=== Overview

Config files are text files containing default command-line arguments for Medley.

*See*: Configuration Files Component for config file usage and purpose

=== File Format

*Text Format*: Plain text file, one argument per line

*Line Format*:

- *Single token*: Flag without value (e.g., `-f`, `--full`, `-ns`)
- *Two tokens*: Flag with value (e.g., `-g 1024x768`, `--geometry 1024x768`, `-i myid`)

*Value Quoting*: Values can be quoted with double quotes if they contain spaces:

#codeblock(lang: "text", [
-t "My Medley Window"
])

Quotes are stripped during parsing.

=== Example Config File

#codeblock(lang: "text", [
-f
-g 1024x768
-i work
-t "Medley Work Session"
-m 512
])

=== File Locations

1. *User config*: `~/.medley_config` (user home directory)
2. *Medley config*: `MEDLEYDIR/.medley_config` (Medley installation directory)
3. *Custom*: Specified with `-c FILE, --config FILE` flag

*See*: Configuration Files Component for location details

=== Parsing

Config files are processed in reverse order (last line first):

1. Config file is read line by line
2. Lines are reversed
3. Arguments are added to argument array in reverse order
4. Command-line arguments are then processed (override config)

*Source Code Reference*: medley/scripts/medley/medley_configfile.sh - config file parsing

== Greet File Format

=== Overview

Greet files are Lisp source files executed during Medley startup.

*See*: Greet Files Component for greet file usage and purpose

=== File Format

*Text Format*: Plain text file containing Lisp source code

*Format*:

- *File Format*: Plain text, Lisp source code
- *Encoding*: Platform-specific (typically UTF-8 or platform default)
- *Line Endings*: Platform-specific (LF on Unix, CRLF on Windows)

=== Example Greet File

#codeblock(lang: "lisp", [
(SETQ INTERLISPMODE T)
(LOAD 'MYINIT)
])

=== File Locations

- *Default*: `MEDLEYDIR/greetfiles/MEDLEYDIR-INIT` (or `APPS-INIT` for apps sysout)
- *Custom*: Specified with `-r FILE, --greet FILE` flag
- *User-specific*: `LOGINDIR/INIT.LISP` (if present)

*See*: Greet Files Component for location details

=== Execution

Greet files are executed:

- *Before*: Main Lisp system initialization
- *Context*: Early Lisp environment
- *Purpose*: Initialize environment before main system starts

*See*: Greet Files Component for execution details

== REM.CM File Format

=== Overview

REM.CM files are Lisp source files executed after greet files, typically used for loadup operations.

*See*: Greet Files Component for REM.CM file details

=== File Format

*Text Format*: Plain text file containing Lisp source code (same as greet files)

*Format*: Same as greet file format

=== File Location

- *Specification*: `-cm FILE, --rem.cm FILE` flag (must be absolute path)
- *Environment Variable*: `LDEREMCM` environment variable
- *No Default*: No default REM.CM file

=== Execution

REM.CM files are executed:

- *After*: Greet files
- *Before*: Main Lisp system
- *Purpose*: Loadup operations and maintenance tasks

== File Format Summary

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | File Type | Format | Purpose | Platform-Specific |
// |-----------|--------|---------|-------------------|
// | **Sysout** | Binary | Complete Lisp system state | No (byte order handled) |
// | **Vmem** | Binary | Session persistence | Yes |
// | **Config** | Text | Default arguments | No |
// | **Greet** | Text (Lisp) | Startup initialization | No |
// | **REM.CM** | Text (Lisp) | Loadup operations | No |
// 

== Related Documentation

- *Sysout Files*: Sysout Files Component - Sysout file usage
- *Virtual Memory Files*: Virtual Memory Files Component - Vmem file usage
- *Configuration Files*: Configuration Files Component - Config file usage
- *Greet Files*: Greet Files Component - Greet file usage
- *Maiko Sysout Format*: `../rewrite-spec/data-structures/sysout-format.md` - Complete sysout format specification
- *Maiko Virtual Memory*: See Maiko documentation for vmem file structure
