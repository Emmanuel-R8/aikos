= Source Code Mapping - Memory Management

*Navigation*: Source Code Mapping | Main README

Mapping of Memory Management source code files to documentation sections.

== Memory Management

=== Garbage Collection

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/gchtfind.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: `htfind()`, `rec_htfind()`, `enter_big_reference_count()` | | |
// | `maiko/src/gcmain3.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: `gcmapscan()`, `gcscanstack()`, GC phases | | |
// | `maiko/src/gcscan.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Stack scanning for GC | | |
// | `maiko/src/gcr.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: GC reclamation | | |
// | `maiko/src/gcrcell.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Cell reclamation | | |
// | `maiko/src/gcarray.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Array reclamation | | |
// | `maiko/src/gccode.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Code reclamation | | |
// | `maiko/src/gcfinal.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Final GC cleanup | | |
// | `maiko/src/gcoflow.c` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Functions**: Overflow handling | | |
// | `maiko/inc/gcdata.h` | `.ai_assistant_db/rewrite-spec/memory/garbage-collection.md` | ✅ Complete |
// | | **Macros**: `ADDREF`, `DELREF`, `STKREF`, `GCLOOKUP` | | |
// 

=== Virtual Memory & Address Translation

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/storage.c` | `.ai_assistant_db/rewrite-spec/memory/virtual-memory.md` | ✅ Complete |
// | | `.ai_assistant_db/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: `checkfor_storagefull()`, `newpage()`, storage management | | |
// | `maiko/inc/address.h` | `.ai_assistant_db/rewrite-spec/memory/address-translation.md` | ✅ Complete |
// | | **Macros**: `HILOC`, `LOLOC`, `POINTER_PAGE`, `ADDBASE`, `VAG2` | | |
// | `maiko/src/adr68k.c` (if exists) | `.ai_assistant_db/rewrite-spec/memory/address-translation.md` | ✅ Complete |
// | | **Functions**: Address translation functions | | |
// 

=== Memory Allocation

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/conspage.c` | `.ai_assistant_db/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
// | | `.ai_assistant_db/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: `cons()`, `N_OP_cons()`, `init_conspage()` | | |
// | `maiko/src/allocmds.c` | `.ai_assistant_db/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: MDS allocation | | |
// | `maiko/src/mkcell.c` | `.ai_assistant_db/rewrite-spec/memory/memory-layout.md` | ✅ Complete |
// | | **Functions**: Cell creation | | |
// | `maiko/inc/cell.h` | `.ai_assistant_db/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
// | | **Structures**: `ConsCell`, `conspage`, CDR coding macros | | |
// 

=== Sysout File Handling

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/src/ldsout.c` | `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: `sysout_loader()` - Load sysout file | | |
// | `maiko/src/setsout.c` | `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: Save sysout file | | |
// | `maiko/src/tstsout.c` | `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: Test sysout integrity | | |
// | `maiko/src/initsout.c` | `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Functions**: Initialize sysout | | |
// | `maiko/inc/ifpage.h` | `.ai_assistant_db/rewrite-spec/data-structures/sysout-format.md` | ✅ Complete |
// | | **Structures**: `IFPAGE` - Interface page structure | | |
// 

=== Data Structures

// TODO: Convert table to Typst table syntax
// Original markdown table:
// | Source File | Documentation Section | Coverage Status |
// |-------------|----------------------|-----------------|
// | `maiko/inc/array.h` | `.ai_assistant_db/rewrite-spec/data-structures/arrays.md` | ✅ Complete |
// | | **Structures**: `OneDArray`, `LispArray`, `arrayheader` | | |
// | `maiko/inc/lsptypes.h` | `.ai_assistant_db/rewrite-spec/data-structures/` (various) | ✅ Complete |
// | | **Types**: Lisp data types | | |
// | `maiko/src/rplcons.c` | `.ai_assistant_db/rewrite-spec/data-structures/cons-cells.md` | ✅ Complete |
// | | **Functions**: Cons cell manipulation | | |
// 

== Related Documentation

- Source Code Mapping - Complete mapping index
- Memory Management - Memory management specifications
