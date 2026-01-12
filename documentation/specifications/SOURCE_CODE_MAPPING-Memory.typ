= Source Code Mapping - Memory Management

*Navigation*: Source Code Mapping | Main README

Mapping of Memory Management source code files to documentation sections.

== Memory Management

=== Garbage Collection

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/gchtfind.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: `htfind()`, `rec_htfind()`, `enter_big_reference_count()` | | |
// | `maiko/src/gcmain3.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: `gcmapscan()`, `gcscanstack()`, GC phases | | |
// | `maiko/src/gcscan.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Stack scanning for GC | | |
// | `maiko/src/gcr.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: GC reclamation | | |
// | `maiko/src/gcrcell.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Cell reclamation | | |
// | `maiko/src/gcarray.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Array reclamation | | |
// | `maiko/src/gccode.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Code reclamation | | |
// | `maiko/src/gcfinal.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Final GC cleanup | | |
// | `maiko/src/gcoflow.c` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Overflow handling | | |
// | `maiko/inc/gcdata.h` | `documentation/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Macros**: `ADDREF`, `DELREF`, `STKREF`, `GCLOOKUP` | | |
// 

=== Virtual Memory & Address Translation

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/storage.c` | `documentation/rewrite-spec/memory/virtual-memory.md` | ✅ Complete |
// | | `documentation/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: `checkfor_storagefull()`, `newpage()`, storage management | | |
// | `maiko/inc/address.h` | `documentation/rewrite-spec/memory/address-translation.md` | ✅ Complete |
// | | **Macros**: `HILOC`, `LOLOC`, `POINTER_PAGE`, `ADDBASE`, `VAG2` | | |
// | `maiko/src/adr68k.c` (if exists) | `documentation/rewrite-spec/memory/address-translation.md` | ✅ Complete |
// | | **Functions**: Address translation functions | | |
// 

=== Memory Allocation

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/conspage.c` | `documentation/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
// | | `documentation/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: `cons()`, `N_OP_cons()`, `init_conspage()` | | |
// | `maiko/src/allocmds.c` | `documentation/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: MDS allocation | | |
// | `maiko/src/mkcell.c` | `documentation/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: Cell creation | | |
// | `maiko/inc/cell.h` | `documentation/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
// | | **Structures**: `ConsCell`, `conspage`, CDR coding macros | | |
// 

=== Sysout File Handling

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/ldsout.c` | `documentation/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: `sysout_loader()` - Load sysout file | | |
// | `maiko/src/setsout.c` | `documentation/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: Save sysout file | | |
// | `maiko/src/tstsout.c` | `documentation/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: Test sysout integrity | | |
// | `maiko/src/initsout.c` | `documentation/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: Initialize sysout | | |
// | `maiko/inc/ifpage.h` | `documentation/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Structures**: `IFPAGE` - Interface page structure | | |
// 

=== Data Structures

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/inc/array.h` | `documentation/rewrite-spec/data-structures/arrays.md` | ✅ Complete |
// | | **Structures**: `OneDArray`, `LispArray`, `arrayheader` | | |
// | `maiko/inc/lsptypes.h` | `documentation/rewrite-spec/data-structures/` (various) | ✅ Complete |
// | | **Types**: Lisp data types | | |
// | `maiko/src/rplcons.c` | `documentation/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
// | | **Functions**: Cons cell manipulation | | |
// 

== Related Documentation

- Source Code Mapping - Complete mapping index
- Memory Management - Memory management specifications
