 = Source Code Mapping - Memory Management
 
 *Navigation*: Source Code Mapping | Main README
 
 Mapping of Memory Management source code files to documentation sections.
 
 == Memory Management
 
 === Garbage Collection
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/gchtfind.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: `htfind()`, `rec_htfind()`, `enter_big_reference_count()` | | |
 // | `maiko/src/gcmain3.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: `gcmapscan()`, `gcscanstack()`, GC phases | | |
 // | `maiko/src/gcscan.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: Stack scanning for GC | | |
 // | `maiko/src/gcr.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: GC reclamation | | |
 // | `maiko/src/gcrcell.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: Cell reclamation | | |
 // | `maiko/src/gcarray.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: Array reclamation | | |
 // | `maiko/src/gccode.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: Code reclamation | | |
 // | `maiko/src/gcfinal.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: Final GC cleanup | | |
 // | `maiko/src/gcoflow.c` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Functions**: Overflow handling | | |
 // | `maiko/inc/gcdata.h` | `documentation/specifications/memory/garbage-collection.typ` | ✅ Complete |
 // | | **Macros**: `ADDREF`, `DELREF`, `STKREF`, `GCLOOKUP` | | |
 // 
 
 === Virtual Memory & Address Translation
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/storage.c` | `documentation/specifications/memory/virtual-memory.typ` | ✅ Complete |
 // | | `documentation/specifications/memory/memory-layout.typ` | ✅ Complete |
 // | | **Functions**: `checkfor_storagefull()`, `newpage()`, storage management | | |
 // | `maiko/inc/address.h` | `documentation/specifications/memory/address-translation.typ` | ✅ Complete |
 // | | **Macros**: `HILOC`, `LOLOC`, `POINTER_PAGE`, `ADDBASE`, `VAG2` | | |
 // | `maiko/src/adr68k.c` (if exists) | `documentation/specifications/memory/address-translation.typ` | ✅ Complete |
 // | | **Functions**: Address translation functions | | |
 // 
 
 === Memory Allocation
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/conspage.c` | `documentation/specifications/data-structures/cons-cells.typ` | ✅ Complete |
 // | | `documentation/specifications/memory/memory-layout.typ` | ✅ Complete |
 // | | **Functions**: `cons()`, `N_OP_cons()`, `init_conspage()` | | |
 // | `maiko/src/allocmds.c` | `documentation/specifications/memory/memory-layout.typ` | ✅ Complete |
 // | | **Functions**: MDS allocation | | |
 // | `maiko/src/mkcell.c` | `documentation/specifications/memory/memory-layout.typ` | ✅ Complete |
 // | | **Functions**: Cell creation | | |
 // | `maiko/inc/cell.h` | `documentation/specifications/data-structures/cons-cells.typ` | ✅ Complete |
 // | | **Structures**: `ConsCell`, `conspage`, CDR coding macros | | |
 // 
 
 === Sysout File Handling
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/src/ldsout.c` | `documentation/specifications/data-structures/sysout-format.typ` | ✅ Complete |
 // | | **Functions**: `sysout_loader()` - Load sysout file | | |
 // | `maiko/src/setsout.c` | `documentation/specifications/data-structures/sysout-format.typ` | ✅ Complete |
 // | | **Functions**: Save sysout file | | |
 // | `maiko/src/tstsout.c` | `documentation/specifications/data-structures/sysout-format.typ` | ✅ Complete |
 // | | **Functions**: Test sysout integrity | | |
 // | `maiko/src/initsout.c` | `documentation/specifications/data-structures/sysout-format.typ` | ✅ Complete |
 // | | **Functions**: Initialize sysout | | |
 // | `maiko/inc/ifpage.h` | `documentation/specifications/data-structures/sysout-format.typ` | ✅ Complete |
 // | | **Structures**: `IFPAGE` - Interface page structure | | |
 // 
 
 === Data Structures
 
 // TODO: Convert table to Typst table syntax
 // Original markdown table:
 // | Source File | Documentation Section | Coverage Status |
 // |-------------|----------------------|-----------------|
 // | `maiko/inc/array.h` | `documentation/specifications/data-structures/arrays.typ` | ✅ Complete |
 // | | **Structures**: `OneDArray`, `LispArray`, `arrayheader` | | |
 // | `maiko/inc/lsptypes.h` | `documentation/specifications/data-structures/` (various) | ✅ Complete |
 // | | **Types**: Lisp data types | | |
 // | `maiko/src/rplcons.c` | `documentation/specifications/data-structures/cons-cells.typ` | ✅ Complete |
 // | | **Functions**: Cons cell manipulation | | |
 // 
 
 == Related Documentation
 
 - Source Code Mapping - Complete mapping index
 - Memory Management - Memory management specifications
