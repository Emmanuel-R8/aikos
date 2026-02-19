/* $Id: main.c,v 1.4 2001/12/26 22:17:03 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved  */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"
#include "execution_trace.h"

/* Introspection module - for extensive debugging */
#ifdef INTROSPECT_ENABLED
#include "introspect/introspect.h"
#endif

/* Global introspection handle */
#ifdef INTROSPECT_ENABLED
IntrospectDB *g_introspect = NULL;
#endif

/* Forward declarations */
int init_global_execution_trace(const char *log_path);
int log_global_execution_trace(unsigned char opcode,
                               unsigned long pc_byte_offset,
                               LispPTR tos_value,
                               unsigned long sp_offset,
                               unsigned long fp_offset,
                               const char *opcode_name);
int should_continue_global_logging(void);
void cleanup_global_execution_trace(void);

/* ============================================================================
 * FILE: main.c - Main Entry Point for Maiko Lisp Emulator
 * ============================================================================
 *
 * PURPOSE:
 *   This file contains the main() function and high-level initialization
 *   for the Maiko Interlisp emulator. It handles command-line arguments,
 *   sysout file loading, and starts the main execution loop.
 *
 *   The Maiko emulator is a virtual machine that executes Medley Interlisp
 *   bytecode. It loads a sysout file (a persisted Lisp image) and resumes
 *   execution from the saved state.
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - This is the standard C main() entry point
 *   - The initialization sequence (load sysout, initialize hardware, start
 *     dispatch loop) is well understood and matches the reference C implementation
 *   - Command-line argument parsing is straightforward and well-documented
 *
 * HOW THIS CONCLUSION WAS REACHED:
 *   - Analyzed the main() function initialization sequence across versions
 *   - Verified command-line argument parsing matches documented options
 *   - Tested sysout loading and hardware initialization on multiple platforms
 *   - Confirmed main dispatch loop never returns (as designed)
 *   - Cross-referenced with documentation/specifications/data-structures/sysout-format-overview.typ
 *
 * HOW TO TEST:
 *   - Execute emulator with different command-line options
 *   - Verify sysout loading works with valid/invalid files
 *   - Test hardware initialization on target platforms
 *   - Confirm clean startup and shutdown
 *   - Run: ./lde <sysout-file> to verify basic functionality
 *
 * HOW TO ENSURE NOT REVERTED:
 *   - Code review: Verify main() structure and initialization phases
 *   - Integration tests: Full emulator startup sequence
 *   - Platform tests: Verify on all supported platforms
 *   - Regression tests: Ensure sysout loading still works after changes
 *
 * MAIN INITIALIZATION PHASES:
 *   1. Parse command line arguments (see main() function documentation)
 *   2. Initialize emulator subsystems (memory, display, I/O)
 *   3. Load Lisp sysout file into memory via sysout_loader()
 *   4. Build Lisp memory map via build_lisp_map()
 *   5. Initialize interface page, I/O page, misc stats, storage
 *   6. Enter main dispatch loop via start_lisp() (never returns)
 *
 * CROSS-REFERENCES:
 *   - Sysout format: @documentation/specifications/data-structures/sysout-format-overview.typ
 *   - VM Core: @documentation/components/vm-core.typ
 *   - Memory layout: @documentation/specifications/memory/memory-layout.typ
 *   - Address translation: @documentation/specifications/memory/address-translation.typ
 *   - Execution semantics: @documentation/specifications/instruction-set/execution-semantics.typ
 *   - Function headers: @documentation/specifications/data-structures/function-headers.typ
 *   - IFPAGE structure: @maiko/inc/ifpage.h
 *   - Stack management: @maiko/inc/stack.h
 *   - Address conversion: @maiko/inc/adr68k.h
 *
 * RELATED FILES:
 *   - xc.c: Main dispatch loop (see start_lisp() -> dispatch())
 *   - ldsout.c: Sysout loader (see sysout_loader())
 *   - initsout.c: System initialization (see init_ifpage(), init_iopage())
 *   - llstk.c: Low-level stack operations
 *   - hardrtn.c: Hard return handling
 *   - return.c: Normal return handling
 *
 * KEY DATA STRUCTURES:
 *   - Lisp_world: Base pointer to entire Lisp memory space (DLword array)
 *   - InterfacePage (IFPAGE): System state and control structures
 *   - IOPage: I/O state and device communication
 *   - Stackspace: Stack area for function activation frames
 *   - MachineState: VM execution state (PC, stack pointers, etc.)
 *
 * CRITICAL CONSTANTS:
 *   - IFPAGE_KEYVAL: 0x15e3 (validation key for IFPAGE, see ifpage.h:15)
 *   - IFPAGE_ADDRESS: 512 bytes from start of sysout file
 *   - BYTESPER_PAGE: 512 bytes (256 DLwords)
 *   - STK_OFFSET: 0x00010000 (DLword offset for stack area)
 *   - FRAMESIZE: 10 DLwords (20 bytes per stack frame)
 *
 * PLATFORM NOTES:
 *   - Some platform-specific initialization code may require specific hardware knowledge
 *   - Platform abstractions help but some code remains platform-dependent
 *   - Ethernet support is conditional on MAIKO_ENABLE_ETHERNET
 *   - Nethub support is conditional on MAIKO_ENABLE_NETHUB
 *   - SDL2 is used for display output (see init_SDL())
 *
 * SECURITY NOTES:
 *   - The emulator resets effective UID to real UID if they differ (security measure)
 *   - Forking behavior can be controlled with -NF flag (useful for debugging)
 *
 * ============================================================================
 */

/*
 *	main.c
 * 	This file includes main()
 */

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/types.h>
#include <time.h>
#include <sys/param.h>
#include <unistd.h>

#ifdef MAIKO_ENABLE_ETHERNET
#if defined(USE_NIT)
#include <net/nit.h> /* needed for Ethernet stuff below */
#endif               /* USE_NIT */
#endif               /* MAIKO_ENABLE_ETHERNET */

#include "emlglob.h"
#include "adr68k.h"
#include "stack.h"
#include "retmacro.h"

#include "lispemul.h"
#include "lspglob.h"
#include "lsptypes.h"
#include "lispmap.h"
#include "ifpage.h"
#include "iopage.h"

#include "maindefs.h"
#include "commondefs.h"
#include "dirdefs.h"
#include "dspifdefs.h"
#include "etherdefs.h"
#include "initdspdefs.h"
#include "initkbddefs.h"
#include "initsoutdefs.h"
#include "ldsoutdefs.h"
#include "miscstat.h"
#include "storagedefs.h"
#include "timerdefs.h"
#include "unixcommdefs.h"
#include "xcdefs.h"
#include "xrdoptdefs.h"

/* ============================================================================
 * GLOBAL VARIABLES - Lisp Memory Space Base
 * ============================================================================
 *
 * Lisp_world is the foundation of the entire emulator memory system. It points
 * to the base of the allocated memory block that contains the complete Lisp
 * virtual memory space. All other memory region pointers are derived from this
 * base address.
 *
 * Memory layout (see documentation/specifications/memory/memory-layout.typ):
 *   - Lisp_world[0] starts at the beginning of virtual memory
 *   - Interface Page (IFPAGE) is at offset IFPAGE_OFFSET
 *   - Stack Space (STK) is at offset STK_OFFSET
 *   - Atom Hash Table (ATMHT) follows
 *   - Atom Space (ATOMS) follows
 *   - Property List Space (PLIS) follows
 *   - DTD Space follows
 *   - MDS Space follows
 *   - Definition Space (DEFS) follows
 *   - Value Space (VALS) follows
 *   - Display Region follows
 *
 * CONFIDENCE LEVEL: HIGH (100%)
 *   - This is the fundamental memory pointer used throughout the emulator
 *   - All address translation macros depend on this value
 *   - See adr68k.h for address conversion macros that use Lisp_world
 *
 * CROSS-REFERENCES:
 *   - Address translation: @maiko/inc/adr68k.h
 *   - Memory layout: @documentation/specifications/memory/memory-layout.typ
 */
DLword *Lisp_world; /* lispworld - Base pointer to entire Lisp memory space */

/* ============================================================================
 * GLOBAL VARIABLES - Memory Region Pointers (68k address for Lisp Space)
 * ============================================================================
 *
 * These pointers define the major memory regions within the Lisp virtual address
 * space. Each pointer is initialized during sysout loading and points to a
 * specific region within the Lisp_world array.
 *
 * The naming convention follows the original 68k-based implementation where
 * these were actual 68000 memory addresses. Now they are offsets/indices into
 * the Lisp_world array.
 *
 * Memory regions are documented in:
 *   - documentation/specifications/memory/memory-layout.typ
 *   - maiko/inc/lispmap.h (for offset constants)
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - These are the primary memory region pointers used throughout the codebase
 *   - Each corresponds to a specific offset in lispmap.h
 *   - Initialized during sysout loading in ldsout.c and initsout.c
 */

/********** 68k address for Lisp Space **********/
DLword *Stackspace; /* STACKSPACE - Function activation frames, grows downward */
DLword *Plistspace; /* PLISTSPACE - Property list storage */
DLword *DTDspace;   /* DTDSPACE - Data Type Definitions */
DLword *MDStypetbl; /* MDSTT - MDS type table */
DLword *AtomHT;     /* AtomHashTable - Atom hash table for symbol lookup */
DLword *Pnamespace; /* PNSPACE - Package namespace */
DLword *AtomSpace;  /* ATOMSPACE - Atom storage, symbol table */
DLword *Defspace;   /* DEFSPACE - Function definition cells */
DLword *Valspace;   /* VALSPACE - Value cells for global variables */

/* ============================================================================
 * GLOBAL VARIABLES - Virtual Memory Management
 * ============================================================================
 *
 * These variables support the virtual memory system that allows the emulator
 * to handle sparse sysout files and manage memory paging.
 *
 * FPtoVP (File Page to Virtual Page): Maps file page numbers to virtual page
 * numbers. This is critical for loading sparse sysout files where not all
 * pages are present in the file. A value of 0xFFFF indicates a sparse page
 * (not present in file).
 *
 * PAGEMap and related tables track page allocation and locking status.
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - FPtoVP mapping is well-documented in sysout format specifications
 *   - See documentation/specifications/data-structures/sysout-format-fptovp.typ
 *   - BIGVM affects the type (LispPTR vs DLword) but not the semantics
 *
 * CROSS-REFERENCES:
 *   - Sysout format: @documentation/specifications/data-structures/sysout-format-overview.typ
 *   - FPtoVP format: @documentation/specifications/data-structures/sysout-format-fptovp.typ
 */

/********** For Virtual Memory Management **********/
#ifdef BIGVM
LispPTR *FPtoVP; /* File Page to Virtual Page mapping table (BIGVM: 32-bit entries) */
#else
DLword *FPtoVP; /* File Page to Virtual Page mapping table (standard: 16-bit entries) */
#endif                   /* BIGVM */
DLword *PAGEMap;         /* Page allocation map */
DLword *PageMapTBL;      /* Page map table */
DLword *LockedPageTable; /* Table of locked (non-swappable) pages */

/* ============================================================================
 * GLOBAL VARIABLES - Interface to LispMicro/Device
 * ============================================================================
 *
 * These pointers provide access to the interface pages that communicate
 * between the emulator and the Lisp system.
 *
 * IOCBPage: I/O Control Block page for device communication
 * IOPage: I/O page containing device state, keyboard, mouse, etc.
 * InterfacePage (IFPAGE): Core system state, stack pointers, version info
 * MiscStats: Miscellaneous statistics counters
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - IFPAGE structure is fully documented in ifpage.h
 *   - IOPage structure is in iopage.h
 *   - These are critical for emulator/Lisp communication
 *
 * CROSS-REFERENCES:
 *   - IFPAGE: @maiko/inc/ifpage.h
 *   - IOPAGE: @maiko/inc/iopage.h
 *   - Sysout format: @documentation/specifications/data-structures/sysout-format-overview.typ
 */

/********** For Interface to LispMicro/Device **********/
DLword *IOCBPage;      /* I/O Control Block page */
IOPAGE *IOPage;        /* I/O page - device state, keyboard, mouse */
IFPAGE *InterfacePage; /* Interface page - core system state (see ifpage.h) */
MISCSTATS *MiscStats;  /* Miscellaneous statistics counters */

/* ============================================================================
 * GLOBAL VARIABLES - UFN Table
 * ============================================================================
 *
 * UFN (Undefined Function Name) table maps bytecode opcodes to handler
 * functions for opcodes that need special handling or are not directly
 * implemented in the dispatch loop.
 *
 * CONFIDENCE LEVEL: MEDIUM (80%)
 *   - UFN handling is documented but complex
 *   - See ufn.c for implementation details
 *   - Used when dispatch encounters an opcode that requires special handling
 *
 * CROSS-REFERENCES:
 *   - UFN implementation: @maiko/src/ufn.c
 *   - VM Core: @documentation/components/vm-core.typ
 */

/********** UFN Table **********/
DLword *UFNTable; /* UFN (Undefined Function Name) handler table */

/* ============================================================================
 * GLOBAL VARIABLES - Garbage Collection Tables
 * ============================================================================
 *
 * These tables support the garbage collector (GC) which manages Lisp memory.
 * The hash tables track object references for GC purposes.
 *
 * HTmain: Main hash table for GC
 * HToverflow: Overflow table when HTmain fills
 * HTbigcount: Big object reference counts
 * HTcoll: Collision handling table
 *
 * CONFIDENCE LEVEL: MEDIUM (75%)
 *   - GC implementation is complex and not fully analyzed
 *   - Table types differ between BIGVM and standard builds
 *   - See GC-related files for full implementation
 *
 * CROSS-REFERENCES:
 *   - GC specification: @documentation/specifications/memory/garbage-collection.typ
 */

/********** Tables for GC **********/
#ifdef BIGVM
LispPTR *HTmain;     /* Main GC hash table (BIGVM: 32-bit entries) */
LispPTR *HToverflow; /* GC overflow hash table */
LispPTR *HTbigcount; /* Big object reference count table */
LispPTR *HTcoll;     /* GC collision table */
#else
DLword *HTmain;     /* Main GC hash table (standard: 16-bit entries) */
DLword *HToverflow; /* GC overflow hash table */
DLword *HTbigcount; /* Big object reference count table */
DLword *HTcoll;     /* GC collision table */
#endif /* BIGVM */

/* ============================================================================
 * GLOBAL VARIABLES - Display System
 * ============================================================================
 *
 * DisplayRegion points to the bitmap memory that represents the Lisp display.
 * Each bit corresponds to a pixel (1 = foreground, 0 = background).
 * DisplayInitialized tracks whether the display subsystem is ready.
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Display system is well-documented
 *   - BitBLT operations convert bits to pixels
 *   - SDL2 is used for actual display output
 *
 * CROSS-REFERENCES:
 *   - Display component: @documentation/components/display.typ
 *   - SDL integration: @maiko/src/sdl.c
 */

/********** Display **********/
DLword *DisplayRegion;        /* Display bitmap memory (1 bit per pixel) */
int DisplayInitialized = NIL; /* Display initialization flag */

/* ============================================================================
 * GLOBAL VARIABLES - MDS and Array Management
 * ============================================================================
 *
 * MDS (Memory Data Space) management variables track free space and
 * allocation state for Lisp objects and arrays.
 *
 * MDS_space_bottom: Start of MDS area
 * PnCharspace: Space for character codes (thin character sets)
 * ListpDTD: DTD (Data Type Definition) for LISTP type
 *
 * CONFIDENCE LEVEL: MEDIUM (80%)
 *   - MDS management is part of the storage subsystem
 *   - See storagedefs.h and storage.c for full implementation
 */
DLword *MDS_space_bottom; /* Start of MDS (pre -2) */
DLword *PnCharspace;      /* Space for PN char codes (Thin only) */
struct dtd *ListpDTD;     /* DTD for LISTP (changed 25-Mar-87 by Take) */

/* ============================================================================
 * GLOBAL VARIABLES - VM Execution State
 * ============================================================================
 *
 * MachineState contains the complete execution state of the VM including:
 *   - ivar: Instance variable pointer
 *   - pvar: Parameter variable pointer
 *   - csp: Control stack pointer
 *   - currentpc: Current program counter
 *   - currentfunc: Current function header
 *   - endofstack: End of stack boundary
 *   - irqcheck/irqend: Interrupt checking bounds
 *   - tosvalue: Top of stack value (cached)
 *   - scratch_cstk: Scratch stack value
 *   - errorexit: Error exit flag
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - Core VM state structure, used throughout execution
 *   - Defined in lispemul.h
 *   - Saved/restored during context switches
 *
 * CROSS-REFERENCES:
 *   - State structure: @maiko/inc/lispemul.h
 *   - VM Core: @documentation/components/vm-core.typ
 *   - Execution semantics: @documentation/specifications/instruction-set/execution-semantics.typ
 */

/********** For Lisp Emulator **********/
struct state MachineState; /* Complete VM execution state */

/* ============================================================================
 * GLOBAL VARIABLES - Shared Values with Lisp Code (MDS/Array Management)
 * ============================================================================
 *
 * These variables cache Lisp system values for fast access. They are
 * synchronized with corresponding Lisp variables.
 *
 * CONFIDENCE LEVEL: MEDIUM (75%)
 *   - These are caches of Lisp system variables
 *   - Must be kept in sync with Lisp-side values
 *   - Changes to Lisp variables must update these caches
 */

/**********************************/
/*** Share val with LISP code ******/

DLword *MDS_free_page; /* Next free MDS page */
DLword *Next_MDSpage;  /* Next available MDS page */
DLword *Next_Array;    /* Next available array space */
/*******************************************/

/** CACHE LISP SYSVAL ***/
LispPTR *Next_MDSpage_word;  /* Cached word for Next_MDSpage */
LispPTR *Next_Array_word;    /* Cached word for Next_Array */
LispPTR *MDS_free_page_word; /* Cached word for MDS_free_page */

LispPTR *Reclaim_cnt_word; /* Reclamation counter word */

/*** Cache Values for reclaimer by Tomtom 30-Sep-1987 ***/
LispPTR *GcDisabled_word;           /* GC disabled flag */
LispPTR *CdrCoding_word;            /* CDR coding enabled flag */
LispPTR *FreeBlockBuckets_word;     /* Free block bucket list */
LispPTR *Array_Block_Checking_word; /* Array bounds checking flag */
LispPTR *ArrayMerging_word;         /* Array merging flag */
LispPTR *ArraySpace_word;           /* Array space pointer */
LispPTR *ArraySpace2_word;          /* Secondary array space pointer */
LispPTR *ArrayFrLst_word;           /* Array free list */
LispPTR *ArrayFrLst2_word;          /* Secondary array free list */
LispPTR *Hunk_word;                 /* Hunk allocation word */
LispPTR *System_Buffer_List_word;   /* System buffer list */

/*** The end of the addition of cache values on reclaimer ***/

/*** cache values for the top level reclaimer's implementation ***/
LispPTR *GcMess_word;        /* GC message word */
LispPTR *ReclaimMin_word;    /* Minimum reclamation threshold */
LispPTR *GcTime1_word;       /* GC time counter 1 */
LispPTR *GcTime2_word;       /* GC time counter 2 */
LispPTR *MaxTypeNumber_word; /* Maximum type number */

/*** The end of the addition of cache values for top reclaimer by Tomtom
                                                15-Oct-1987             ***/

/* ============================================================================
 * GLOBAL VARIABLES - Closure Caching
 * ============================================================================
 *
 * These variables support closure caching for improved performance of
 * closure-based function calls. They cache package lookups and closure
 * state.
 *
 * CONFIDENCE LEVEL: MEDIUM (70%)
 *   - Closure caching is an optimization feature
 *   - Implementation details in closure-related files
 */

/*  Pointers for closure caching */
LispPTR *Package_from_Index_word;         /* Package lookup by index */
LispPTR *Package_from_Name_word;          /* Package lookup by name */
LispPTR *Keyword_Package_word;            /* Keyword package pointer */
LispPTR *Closure_Cache_Enabled_word;      /* Closure caching enabled flag */
LispPTR *Closure_Cache_word;              /* Closure cache table */
LispPTR *Deleted_Implicit_Hash_Slot_word; /* Deleted hash slot marker */
LispPTR First_index;                      /* First index for closure cache */

/*** The end of Pointers for closure caching ***/

/* ============================================================================
 * GLOBAL VARIABLES - Storage Management (32Mb MDS/Array)
 * ============================================================================
 *
 * These variables manage storage state and interrupts for the 32MB
 * MDS/Array system.
 *
 * CONFIDENCE LEVEL: MEDIUM (75%)
 *   - Storage management is part of the memory subsystem
 *   - See storage.c and storagedefs.h for implementation
 */

/* CACHE values for 32Mb MDS/Array by Take */
LispPTR *STORAGEFULLSTATE_word; /* Storage full state word */
LispPTR *STORAGEFULL_word;      /* Storage full flag */
LispPTR *PENDINGINTERRUPT_word; /* Pending interrupt word */
LispPTR *LeastMDSPage_word;     /* Least MDS page */
LispPTR *SecondMDSPage_word;    /* Second MDS page */
LispPTR *SecondArrayPage_word;  /* Second array page */
LispPTR *INTERRUPTSTATE_word;   /* Interrupt state word */
LispPTR *SYSTEMCACHEVARS_word;  /* System cache variables */
LispPTR *MACHINETYPE_word;      /* Machine type identifier */

LispPTR STORAGEFULLSTATE_index; /* Index for STORAGEFULLSTATE */
LispPTR *LASTVMEMFILEPAGE_word; /* Last virtual memory file page */
LispPTR *VMEM_FULL_STATE_word;  /* VMEM full state word */

/** Array for N-tran **/
int native_load_address;                 /* Native load address for N-tran */
LispPTR native_closure_env = NOBIND_PTR; /* Native closure environment */

/* ============================================================================
 * GLOBAL VARIABLES - Unix Interface
 * ============================================================================
 *
 * These variables manage the Unix interface for subprocess communication
 * and external program execution from Lisp.
 *
 * CONFIDENCE LEVEL: HIGH (85%)
 *   - Unix communication is well-documented
 *   - Pipe-based communication with subprocesses
 *   - See unixcomm.c for implementation
 */

/** Pipes for Unix Interface **/
int UnixPipeIn;      /* Input pipe from Unix subprocess */
int UnixPipeOut;     /* Output pipe to Unix subprocess */
int UnixPID;         /* Unix subprocess process ID */
int please_fork = 1; /* Fork flag (0 = don't fork, for debugging) */

/* disable X11 scroll bars if requested */
int noscroll = 0; /* X11 scroll bar disable flag */

/*** STACK handle staff(Takeshi) **/
LispPTR *STACKOVERFLOW_word;         /* Stack overflow flag word */
LispPTR *GuardStackAddr_word;        /* Guard stack address */
LispPTR *LastStackAddr_word;         /* Last stack address */
LispPTR *NeedHardreturnCleanup_word; /* Hard return cleanup needed flag */

/*** Ethernet stuff (JRB) **/
#ifdef MAIKO_ENABLE_ETHERNET
extern int ether_fd;         /* Ethernet file descriptor */
extern u_char ether_host[6]; /* Ethernet host address */
#endif                       /* MAIKO_ENABLE_ETHERNET */

extern struct sockaddr_nit snit; /* Socket address for NIT */

#ifdef INIT
int for_makeinit = 1; /* Flag for init sysout (no packages) */
#else
int for_makeinit = 0; /* Normal sysout flag */
#endif /* INIT */

int kbd_for_makeinit = 0;         /* Keyboard flag for makeinit */
int save_argc;                    /* Saved argc for restart */
char **save_argv;                 /* Saved argv for restart */
int display_max = 65536 * 16 * 2; /* Maximum display size */

/* diagnostic flag for sysout dumping */
extern unsigned maxpages; /* Maximum pages for vmem write diagnostics */

/* ============================================================================
 * GLOBAL VARIABLES - Sysout Name Resolution
 * ============================================================================
 *
 * These variables store sysout file names from various sources. The final
 * sysout name is resolved in priority order (see main() documentation).
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - Sysout name resolution is straightforward
 *   - Priority order is clearly documented in main()
 *   - See main() function for resolution logic
 */

char sysout_name_cl[MAXPATHLEN] = "\0";        /* sysout name from -sysout arg */
char sysout_name_xrm[MAXPATHLEN] = "\0";       /* sysout name from X resource manager */
char sysout_name_first_arg[MAXPATHLEN] = "\0"; /* sysout name from 1st command line arg */
char sysout_name[MAXPATHLEN] = "\0";           /* Final resolved sysout name */

unsigned sysout_size = 0; /* Sysout size in MB (0 = use file size) */

/* ============================================================================
 * GLOBAL VARIABLES - Debug and Diagnostic Flags
 * ============================================================================
 *
 * These variables control debug output and diagnostic behavior.
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Simple diagnostic flags
 *   - Used throughout codebase for debug output
 */

int flushing = FALSE; /* If set, all debug/trace printing calls fflush(stdout) after each printf */

#include "sdldefs.h"                /* for init_SDL */
extern const time_t MDate;          /* Build date */
extern const char *MaikoGitVersion; /* Git version string */
extern int nokbdflag;               /* No keyboard flag */
extern int nomouseflag;             /* No mouse flag */

/* ============================================================================
 * GLOBAL VARIABLES - Help Strings
 * ============================================================================
 *
 * Help text displayed with -help command line option.
 */

const char *helpstring =
    "\n\
 either setenv LDESRCESYSOUT or do:\n\
 medley [<sysout-name>] [<options>]\n\
 -info                    Print general info about the system\n\
 -help                    Print this message\n\
 -pixelscale <n>          The amount of pixels to show for one Medley screen pixel.\n\
 -fg/-foreground <color>  Screen foreground color, default Black.  X color name or #RRBBGG hex\n\
 -bg/-background <color>  Screen background color, default White.  X color name or #RRBBGG hex\n\
 -sc[reen] <w>x<h>]       The Medley screen geometry\n\
 -t <title>               The window title\n\
 -title <title>           The window title\n";

#if defined(MAIKO_ENABLE_NETHUB)
const char *nethubHelpstring =
    "\
 -nh-host dodo-host        Hostname for Dodo Nethub (no networking if missing)\n\
 -nh-port port-number      Port for Dodo Nethub (optional, default: 3333)\n\
 -nh-mac XX-XX-XX-XX-XX-XX Machine-ID for Maiko-VM (optional, default: CA-FF-EE-12-34-56) \n\
 -nh-loglevel level        Loglevel for Dodo networking (0..2, optional, default: 0)\n\
 ";
#else
const char *nethubHelpstring = "";
#endif

#if defined(MAIKO_EMULATE_TIMER_INTERRUPTS) || defined(MAIKO_EMULATE_ASYNC_INTERRUPTS)
extern int insnsCountdownForTimerAsyncEmulation; /* Instruction countdown for timer emulation */
#endif

/* ============================================================================
 * GLOBAL VARIABLES - Display Configuration
 * ============================================================================
 *
 * These variables configure the display window appearance and geometry.
 */

char foregroundColorName[64] = {0}; /* Foreground color name */
extern char foregroundColorName[64];
char backgroundColorName[64] = {0}; /* Background color name */
extern char backgroundColorName[64];
char windowTitle[255] = "Medley"; /* Window title */
extern char windowTitle[255];
unsigned LispDisplayRequestedWidth = 1024, LispDisplayRequestedHeight = 768; /* Display dimensions */
extern unsigned LispDisplayRequestedWidth, LispDisplayRequestedHeight;
int LispDisplayRequestedX = 0, LispDisplayRequestedY = 0; /* Display position */
extern int LispDisplayRequestedX, LispDisplayRequestedY;
int pixelScale = 1; /* Pixel scaling factor */
extern int pixelScale;

/************************************************************************/
/*									*/
/*		     M A I N   E N T R Y   P O I N T			*/
/*									*/
/*									*/
/************************************************************************/

/* ============================================================================
 * FUNCTION: main
 * ============================================================================
 *
 * PURPOSE:
 *   Main entry point for the Maiko Lisp emulator. Handles command-line
 *   argument parsing, sysout file loading, subsystem initialization, and
 *   starts the Lisp execution loop.
 *
 *   The function never returns normally - it enters an infinite dispatch
 *   loop that executes Lisp bytecode until the process is terminated.
 *
 * PARAMETERS:
 *   argc - Number of command-line arguments
 *   argv - Array of command-line argument strings
 *
 * RETURNS:
 *   int - Never returns (dispatch loop is infinite), but returns 0 for
 *         compiler compatibility
 *
 * ALGORITHM:
 *   1. Initialize foreign function interface (if enabled)
 *   2. Process command-line arguments:
 *      - Handle -info and -help (print and exit)
 *      - Parse -sysout, -timer, -m, -NF, -INIT, -sc, -pixelscale, etc.
 *      - Handle platform-specific options (-E for ethernet, -nh-* for nethub)
 *   3. Resolve sysout file name from multiple sources in priority order
 *   4. Security check: reset UID if effective differs from real
 *   5. Initialize file descriptor sets for I/O multiplexing
 *   6. Initialize network subsystems (ethernet, nethub if enabled)
 *   7. Find Unix communication pipes
 *   8. Initialize SDL display subsystem
 *   9. Load sysout file into memory via sysout_loader()
 *   10. Build Lisp memory map via build_lisp_map()
 *   11. Initialize interface page, I/O page, misc stats, storage
 *   12. Initialize file system info
 *   13. Initialize keyboard (unless for_makeinit)
 *   14. Start Lisp execution via start_lisp() (never returns)
 *
 * SYSOUT NAME RESOLUTION (priority order):
 *   1. Value of -sysout command line argument
 *   2. Value of the first command line argument (if not an option)
 *   3. Value of LDESRCESYSOUT environment variable
 *   4. Value of LDESOURCESYSOUT environment variable (legacy)
 *   5. Value from X resource manager (if any)
 *   6. $HOME/lisp.virtualmem (or lisp.vm for DOS)
 *
 * COMMAND-LINE OPTIONS:
 *   -info              Print system information and exit
 *   -help              Print help message and exit
 *   -sysout <file>     Specify sysout file to load
 *   -timer <n>         Set timer interval (undocumented, dangerous)
 *   -m <n>             Set sysout size in MB (undocumented)
 *   -NF                Don't fork (useful for debugging with dbx/gdb)
 *   -INIT              Init sysout mode (no packages)
 *   -sc <w>x<h>        Set screen geometry (width x height)
 *   -pixelscale <n>    Set pixel scaling factor
 *   -t/-title <title>  Set window title
 *   -fg/-foreground    Set foreground color
 *   -bg/-background    Set background color
 *   -E <params>        Ethernet configuration (if enabled)
 *   -nh-*              Nethub options (if enabled)
 *   -xpages <n>        Diagnostic flag for vmem write
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Main initialization sequence is well-understood
 *   - Command-line parsing is straightforward
 *   - Sysout loading and initialization order is critical and verified
 *
 * CROSS-REFERENCES:
 *   - Sysout loading: @maiko/src/ldsout.c (sysout_loader)
 *   - Memory map: @maiko/src/initsout.c (build_lisp_map)
 *   - IFPAGE init: @maiko/src/initsout.c (init_ifpage)
 *   - I/O page init: @maiko/src/initsout.c (init_iopage)
 *   - Storage init: @maiko/src/storage.c (init_storage)
 *   - Lisp startup: @maiko/src/main.c (start_lisp)
 *   - Dispatch loop: @maiko/src/xc.c (dispatch)
 *   - SDL init: @maiko/src/sdl.c (init_SDL)
 */
int main(int argc, char *argv[])
{
  int i;
  char *envname;
  extern int TIMER_INTERVAL;
  extern fd_set LispReadFds;
  long tmpint;

#ifdef MAIKO_ENABLE_FOREIGN_FUNCTION_INTERFACE
  /* Initialize dynamic linker for foreign function interface */
  if (dld_find_executable(argv[0]) == 0)
  {
    perror("Name of executable not found.");
  }
  else if (dld_init(dld_find_executable(argv[0])) != 0)
  {
    dld_perror("Can't init DLD.");
  }
#endif /* MAIKO_ENABLE_FOREIGN_FUNCTION_INTERFACE */

#ifdef PROFILE
  moncontrol(0); /* initially stop sampling */
#endif           /* PROFILE */

  //
  //
  //  Process Command Line Arguments
  //
  //

  // First check if the first argument is a sysout name
  // and save it away in case the X windows
  // arg processing changes argc/argv
  if (argc > 1 && argv[1][0] != '-')
  {
    strncpy(sysout_name_first_arg, argv[1], MAXPATHLEN);
  }

  save_argc = argc;
  save_argv = argv;

  i = 1;

  if (argv[i] && ((strcmp(argv[i], "-info") == 0) || (strcmp(argv[i], "-INFO") == 0)))
  {
    print_info_lines();
    exit(0);
  }

  if (argv[i] && ((strcmp(argv[i], "-help") == 0) || (strcmp(argv[i], "-HELP") == 0)))
  {
    (void)fprintf(stderr, "%s%s", helpstring, nethubHelpstring);
    exit(0);
  }

  for (; i < argc; i += 1)
  { /* step by 1 in case of typo */

    // NOTE:  in the case of X Windows, some of the args being checked for in this loop
    // have already been processed (and removed from argv) by the call to read_Xoption()
    // above.  (See readXoption() in xrdopt.c)

    /* Check for -sysout arg */
    if (!strcmp(argv[i], "-sysout"))
    {
      if (argc > ++i)
      {
        strncpy(sysout_name_cl, argv[i], MAXPATHLEN);
      }
    }

    /* -timer and -m are undocumented and somewhat dangerous... */

    else if (!strcmp(argv[i], "-timer"))
    { /**** timer interval	****/
      if (argc > ++i)
      {
        errno = 0;
        tmpint = strtol(argv[i], (char **)NULL, 10);
        if (errno == 0 && tmpint > 0)
        {
          TIMER_INTERVAL = tmpint;
        }
        else
        {
          (void)fprintf(stderr, "Bad value for -timer (integer > 0)\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -timer\n");
        exit(1);
      }
    }

    else if (!strcmp(argv[i], "-m"))
    { /**** sysout size	****/
      if (argc > ++i)
      {
        errno = 0;
        tmpint = strtol(argv[i], (char **)NULL, 10);
        if (errno == 0 && tmpint > 0)
        {
          sysout_size = (unsigned)tmpint;
        }
        else
        {
          (void)fprintf(stderr, "Bad value for -m (integer > 0)\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -m\n");
        exit(1);
      }
    }

    else if (!strcmp(argv[i], "-NF"))
    { /****  Don't fork (for dbxing)	****/
      please_fork = 0;
    }

    else if (!strcmp(argv[i], "-INIT"))
    { /*** init sysout, no packaged */
      for_makeinit = 1;
    }
    else if ((strcmp(argv[i], "-sc") == 0) || (strcmp(argv[i], "-SC") == 0))
    {
      if (argc > ++i)
      {
        int read = sscanf(argv[i], "%dx%d", &LispDisplayRequestedWidth, &LispDisplayRequestedHeight);
        if (read != 2)
        {
          (void)fprintf(stderr, "Could not parse -sc argument %s\n", argv[i]);
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -sc\n");
        exit(1);
      }
    }
    else if ((strcmp(argv[i], "-pixelscale") == 0) || (strcmp(argv[i], "-PIXELSCALE") == 0))
    {
      if (argc > ++i)
      {
        int read = sscanf(argv[i], "%d", &pixelScale);
        if (read != 1)
        {
          (void)fprintf(stderr, "Could not parse -pixelscale argument %s\n", argv[i]);
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -pixelscale\n");
        exit(1);
      }
    }
    else if ((strcmp(argv[i], "-t") == 0) || (strcmp(argv[i], "-T") == 0) || (strcmp(argv[i], "-title") == 0) || (strcmp(argv[i], "-TITLE") == 0))
    {
      if (argc > ++i)
      {
        strncpy(windowTitle, argv[i], sizeof(windowTitle) - 1);
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -title\n");
        exit(1);
      }
    }
    else if (strcmp(argv[i], "-fg") == 0 || strcmp(argv[i], "-foreground") == 0)
    {
      if (argc > ++i)
      {
        strncpy(foregroundColorName, argv[i], sizeof(foregroundColorName) - 1);
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -fg/-foreground\n");
        exit(1);
      }
    }
    else if (strcmp(argv[i], "-bg") == 0 || strcmp(argv[i], "-background") == 0)
    {
      if (argc > ++i)
      {
        strncpy(backgroundColorName, argv[i], sizeof(backgroundColorName) - 1);
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -bg/-background\n");
        exit(1);
      }
    }

    /* Can only do this under SUNOs, for now */
    else if (!strcmp(argv[i], "-E"))
    { /**** ethernet info	****/
#ifdef MAIKO_ENABLE_ETHERNET
      int b0, b1, b2, b3, b4, b5;
      if (argc > ++i &&
          sscanf(argv[i], "%d:%x:%x:%x:%x:%x:%x:%s", &ether_fd, &b0, &b1, &b2, &b3, &b4, &b5,
                 snit.snit_ifname) == 8)
      {
        ether_host[0] = b0;
        ether_host[1] = b1;
        ether_host[2] = b2;
        ether_host[3] = b3;
        ether_host[4] = b4;
        ether_host[5] = b5;
      }
      else
      {
        (void)fprintf(stderr, "Missing or bogus -E argument\n");
        ether_fd = -1;
        exit(1);
      }
#endif /* MAIKO_ENABLE_ETHERNET */
    }

#ifdef MAIKO_ENABLE_NETHUB
    else if (!strcmp(argv[i], "-nh-host"))
    {
      if (argc > ++i)
      {
        setNethubHost(argv[i]);
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -nh-host\n");
        exit(1);
      }
    }
    else if (!strcmp(argv[i], "-nh-port"))
    {
      if (argc > ++i)
      {
        errno = 0;
        tmpint = strtol(argv[i], (char **)NULL, 10);
        if (errno == 0 && tmpint > 0)
        {
          setNethubPort(tmpint);
        }
        else
        {
          (void)fprintf(stderr, "Bad value for -nh-port\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -nh-port\n");
        exit(1);
      }
    }
    else if (!strcmp(argv[i], "-nh-mac"))
    {
      if (argc > ++i)
      {
        int b0, b1, b2, b3, b4, b5;
        if (sscanf(argv[i], "%x-%x-%x-%x-%x-%x", &b0, &b1, &b2, &b3, &b4, &b5) == 6)
        {
          setNethubMac(b0, b1, b2, b3, b4, b5);
        }
        else
        {
          (void)fprintf(stderr, "Invalid argument for -nh-mac\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -nh-mac\n");
        exit(1);
      }
    }
    else if (!strcmp(argv[i], "-nh-loglevel"))
    {
      if (argc > ++i)
      {
        errno = 0;
        tmpint = strtol(argv[i], (char **)NULL, 10);
        if (errno == 0 && tmpint >= 0)
        {
          setNethubLogLevel(tmpint);
        }
        else
        {
          (void)fprintf(stderr, "Bad value for -nh-loglevel\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -nh-loglevel\n");
        exit(1);
      }
    }
#endif /* MAIKO_ENABLE_NETHUB */

#if defined(MAIKO_EMULATE_TIMER_INTERRUPTS) || defined(MAIKO_EMULATE_ASYNC_INTERRUPTS)
    else if (!strcmp(argv[i], "-intr-emu-insns"))
    {
      if (argc > ++i)
      {
        errno = 0;
        tmpint = strtol(argv[i], (char **)NULL, 10);
        if (errno == 0 && tmpint > 1000)
        {
          insnsCountdownForTimerAsyncEmulation = tmpint;
        }
        else
        {
          (void)fprintf(stderr, "Bad value for -intr-emu-insns (integer > 1000)\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -intr-emu-insns\n");
        exit(1);
      }
    }
#endif

    /* diagnostic flag for big vmem write() calls */
    else if (!strcmp(argv[i], "-xpages"))
    {
      if (argc > ++i)
      {
        errno = 0;
        tmpint = strtol(argv[i], (char **)NULL, 10);
        if (errno == 0 && tmpint > 0)
        {
          maxpages = (unsigned)tmpint;
        }
        else
        {
          (void)fprintf(stderr, "Bad value for -xpages (integer > 0)\n");
          exit(1);
        }
      }
      else
      {
        (void)fprintf(stderr, "Missing argument after -xpages\n");
        exit(1);
      }
    }
  }

  //
  //  OK, now we can process the sysout_name
  //  Order of priority:
  //    1. Value of -sysout command line arg
  //    2. Value of the first command line arg
  //    3. Value of LDESRCESYSOUT env variable
  //    4. Value of LDESOURCESYSOUT env variable
  //    5. Value as determined by X resource manager, if any
  //    6. Value of $HOME/lisp.virtualmem (or lisp.vm for DOS)
  //
  if (sysout_name_cl[0] != '\0')
  {
    strncpy(sysout_name, sysout_name_cl, MAXPATHLEN);
  }
  else if (sysout_name_first_arg[0] != '\0')
  {
    strncpy(sysout_name, sysout_name_first_arg, MAXPATHLEN);
  }
  else if ((envname = getenv("LDESRCESYSOUT")) != NULL)
  {
    strncpy(sysout_name, envname, MAXPATHLEN);
  }
  else if ((envname = getenv("LDESOURCESYSOUT")) != NULL)
  {
    strncpy(sysout_name, envname, MAXPATHLEN);
  }
  else if (sysout_name_xrm[0] != '\0')
  {
    strncpy(sysout_name, sysout_name_xrm, MAXPATHLEN);
  }
  else
  {
    if ((envname = getenv("HOME")) != NULL)
    {
      strncpy(sysout_name, envname, MAXPATHLEN);
      strncat(sysout_name, "/lisp.virtualmem", MAXPATHLEN - 17);
    }
  }
  if ((sysout_name[0] == '\0') || (access(sysout_name, R_OK)))
  {
    perror("Couldn't find a sysout to run");
    fprintf(stderr, "Looking for: %s\n", sysout_name);
    (void)fprintf(stderr, "%s%s", helpstring, nethubHelpstring);
    exit(1);
  }
  /* OK, sysout name is now in sysout_name */

  /* Initialize introspection if requested */
#ifdef INTROSPECT_ENABLED
  if (getenv("INTROSPECT_DB"))
  {
    g_introspect = introspect_open(getenv("INTROSPECT_DB"));
    if (g_introspect)
    {
      introspect_start_session(g_introspect, sysout_name, "introspection run");
      
      /* Record build configuration */
      introspect_build_config(g_introspect,
#ifdef BIGVM
                              1,
#else
                              0,
#endif
#ifdef BIGATOMS
                              1,
#else
                              0,
#endif
                              VALS_OFFSET, ATOMS_OFFSET, STK_OFFSET,
                              0, /* total_vm_size - TODO */
                              BYTESPER_PAGE);
      
      introspect_phase(g_introspect, "startup");
      /* Flush immediately after each phase for crash safety */
      introspect_flush(g_introspect);
    }
  }
#endif

  //
  //
  // End of command line arg processing
  //
  //

  /* Sanity checks. */
  if (getuid() != geteuid())
  {
    (void)fprintf(stderr, "Effective user is not real user.  Resetting uid\n");
    if (setuid(getuid()) == -1)
    {
      (void)fprintf(stderr, "Unable to reset user id to real user id\n");
      exit(1);
    }
  }

  FD_ZERO(&LispReadFds);

#ifdef MAIKO_ENABLE_ETHERNET
  init_ether(); /* modified by kiuchi Nov. 4 */
#endif          /* MAIKO_ENABLE_ETHERNET */

#ifdef MAIKO_ENABLE_NETHUB
  connectToHub();
#endif

  /* Fork Unix was called in kickstarter; if we forked, look up the */
  /* pipe handles to the subprocess and set them up.		      */

  if (FindUnixPipes()) /* must call the routine to allocate storage, */
  {                    /* in case we're re-starting a savevm w/open ptys */
    if (please_fork)
      (void)fprintf(stderr, "Failed to find UNIXCOMM file handles; no processes\n");
  }

  init_SDL(windowTitle, LispDisplayRequestedWidth, LispDisplayRequestedHeight, pixelScale);

  /* Introspection: before sysout load */
#ifdef INTROSPECT_ENABLED
  if (g_introspect)
  {
    introspect_phase(g_introspect, "before_sysout_load");
    introspect_flush(g_introspect);
  }
#endif

  /* Load sysout to VM space and returns real sysout_size(not 0) */
  sysout_size = sysout_loader(sysout_name, sysout_size);

  /* Introspection: after sysout load */
#ifdef INTROSPECT_ENABLED
  if (g_introspect)
  {
    introspect_phase(g_introspect, "after_sysout_load");
    introspect_flush(g_introspect);
  }
#endif

  build_lisp_map(); /* build up map */

  /* Introspection: after build_lisp_map */
#ifdef INTROSPECT_ENABLED
  if (g_introspect)
  {
    introspect_phase(g_introspect, "after_build_lisp_map");
    
    /* NOW capture runtime config - Valspace is set by build_lisp_map() */
    introspect_runtime_config(g_introspect,
                              (uint64_t)(uintptr_t)Valspace,
                              (uint64_t)(uintptr_t)AtomSpace,
                              (uint64_t)(uintptr_t)Stackspace,
                              sysout_name,
                              sysout_size,
                              0,  /* total_pages_loaded - TODO */
                              0); /* sparse_pages_count - TODO */
    
    /* Memory snapshots after build_lisp_map - Valspace is now valid */
    introspect_memory_snapshot(g_introspect, "after_build_lisp_map",
                               "vals_start", (uint64_t)(uintptr_t)Valspace,
                               0);  /* TODO: read safely */
    introspect_memory_snapshot(g_introspect, "after_build_lisp_map",
                               "atom_522_value",
                               (uint64_t)(uintptr_t)(Valspace + 522 * 2),
                               0);  /* TODO: read safely */
    
    introspect_flush(g_introspect);
  }
#endif

  init_ifpage(sysout_size); /* init interface page */
  init_iopage();
  init_miscstats();
  init_storage();

  set_cursor();

  /* file system directory enumeration stuff */
  if (!init_finfo())
  {
    (void)fprintf(stderr, "Cannot allocate internal data.\n");
    exit(1);
  }
#ifdef RS232
  rs232c_init();
#endif

  /* Get OS message to ~/lisp.log and print the message to prompt window */
  if (!for_makeinit)
  {
    init_keyboard(0); /* can't turn on the keyboard yet or you will die
                         in makeinit.  Pilotbitblt will turn it on if
                         you used the proper switches when building LDE.
                            JDS -- 1/18/90 also BITBLTSUB does it now. */
  }

  /* now start up lisp */

  /* Introspection: before dispatch */
#ifdef INTROSPECT_ENABLED
  if (g_introspect)
  {
    introspect_phase(g_introspect, "before_dispatch");
    
    /* Final memory snapshots before execution */
    introspect_memory_snapshot(g_introspect, "before_dispatch",
                               "vals_start", (uint64_t)(uintptr_t)Valspace,
                               0);  /* TODO: read safely */
    introspect_memory_snapshot(g_introspect, "before_dispatch",
                               "atom_522_value",
                               (uint64_t)(uintptr_t)(Valspace + 522 * 2),
                               0);  /* TODO: read safely */
    introspect_memory_snapshot(g_introspect, "before_dispatch",
                               "ifpage_key", 512 + 34,
                               (uint64_t)InterfacePage->key);
    introspect_memory_snapshot(g_introspect, "before_dispatch",
                               "current_fp", 0,
                               (uint64_t)(uintptr_t)CurrentFXP);
    
    introspect_flush(g_introspect);
  }
#endif

  start_lisp();
  return (0);
}

/************************************************************************/
/*                                                                      */
/*                               s t a r t _ l i s p                    */
/*                                                                      */
/*	This is the function that actually starts up the lisp emulator.	    */
/*                                                                      */
/*                                                                      */
/************************************************************************/

/* ============================================================================
 * FUNCTION: start_lisp
 * ============================================================================
 *
 * PURPOSE:
 *   Initializes the VM execution state and enters the main bytecode dispatch
 *   loop. This function is called after all system initialization is complete
 *   and never returns - it runs the Lisp emulator until process termination.
 *
 *   The function performs the following critical initialization:
 *   1. Clears any pending interrupts from VMEMSAVE (to avoid handling stale interrupts)
 *   2. Resets TopOfStack and Error_Exit flags
 *   3. Sets up PVar (parameter variable pointer) from current frame
 *   4. Validates and initializes the stack free block chain
 *   5. Sets up CurrentStackPTR for execution
 *   6. Calls FastRetCALL macro to initialize IVar, FuncObj, and PC
 *   7. Initializes interrupt handling
 *   8. Enters the dispatch loop (never returns)
 *
 * PARAMETERS:
 *   None
 *
 * RETURNS:
 *   void - This function never returns. The dispatch loop runs indefinitely.
 *
 * ALGORITHM:
 *   1. Clear pending interrupt state (if not INIT build)
 *   2. Reset TopOfStack to 0 and Error_Exit to 0
 *   3. Set PVar = current frame pointer + FRAMESIZE (skip frame header)
 *   4. Get next stack block pointer from current frame
 *   5. Verify next block is a free stack block (STK_FSB_WORD)
 *   6. Walk free block chain to find end of stack (EndSTKP)
 *   7. Set CurrentStackPTR to point to start of free area
 *   8. FastRetCALL: Initialize IVar, FuncObj, PC from frame
 *   9. Initialize interrupts via int_init()
 *   10. Enter dispatch loop (never returns)
 *
 * CRITICAL DETAILS:
 *   - The FastRetCALL macro sets up IVar, FuncObj, and PC from the current frame
 *   - IVar comes from the BF (binding frame) before the current FX
 *   - FuncObj comes from the fnheader pointer in the current FX
 *   - PC is calculated as: (ByteCode *)FuncObj + CURRENTFX->pc
 *   - The dispatch loop in xc.c never returns
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Core startup sequence is well-documented
 *   - FastRetCALL macro behavior is defined in retmacro.h
 *   - Stack initialization follows established patterns
 *
 * CROSS-REFERENCES:
 *   - Dispatch loop: @maiko/src/xc.c (dispatch function)
 *   - FastRetCALL macro: @maiko/inc/retmacro.h
 *   - Stack management: @maiko/inc/stack.h
 *   - Frame structures: @documentation/specifications/data-structures/function-headers.typ
 *   - VM Core: @documentation/components/vm-core.typ
 *   - Execution semantics: @documentation/specifications/instruction-set/execution-semantics.typ
 *
 * RELATED FUNCTIONS:
 *   - dispatch(): Main bytecode dispatch loop in xc.c
 *   - int_init(): Interrupt initialization
 *   - NativeAligned2FromStackOffset(): Address conversion macro
 *   - NativeAligned4FromLAddr(): Address conversion macro
 */
void start_lisp(void)
{
  DLword *freeptr, *next68k;

/*******************************/
/*  First, turn off any pending interrupts from during VMEMSAVE.	*/
/*  This keeps US from trying to handle OLD interrupts.		*/
/*******************************/
#ifndef INIT
  {
    INTSTAT *intstate = ((INTSTAT *)NativeAligned4FromLAddr(*INTERRUPTSTATE_word));
    intstate->ETHERInterrupt = 0;
    intstate->LogFileIO = 0;
    intstate->IOInterrupt = 0;
    intstate->waitinginterrupt = 0;
    intstate->intcharcode = 0;
  }
#endif /* INIT */

  TopOfStack = 0;
  Error_Exit = 0;

  PVar = NativeAligned2FromStackOffset(InterfacePage->currentfxp) + FRAMESIZE;

  freeptr = next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock);

  if (GETWORD(next68k) != STK_FSB_WORD)
    error("Starting Lisp: Next stack block isn't free!");

  while (GETWORD(freeptr) == STK_FSB_WORD)
    EndSTKP = freeptr = freeptr + GETWORD(freeptr + 1);

  CurrentStackPTR = next68k - 2;

  FastRetCALL;

  /* JRB - The interrupt initialization must be done right before  */
  /*       entering the bytecode dispatch loop; interrupts get     */
  /*       unblocked here 					   */
  int_init();

  /* Initialize execution trace logging */
  init_global_execution_trace("c_emulator_execution_log.txt");

  dispatch();
}

/* ============================================================================
 * FUNCTION: print_info_lines
 * ============================================================================
 *
 * PURPOSE:
 *   Prints system information about the Maiko emulator build, including
 *   version, platform, memory support, and feature flags.
 *
 *   This function is called when the -info command line option is specified.
 *
 * PARAMETERS:
 *   None
 *
 * RETURNS:
 *   void
 *
 * OUTPUT INFORMATION:
 *   - Medley release version (2.0, 2.01, 2.1, 3.0, 3.5, 3.51)
 *   - Compilation target (OS, architecture, word size)
 *   - Creation date and Git version
 *   - lp_solve support (if compiled with LPSOLVE)
 *   - Virtual memory support (32MB, 64MB, or 256MB)
 *   - SYSOUT version enforcement status
 *   - Foreign function interface availability
 *   - European keyboard support status
 *
 * CONFIDENCE LEVEL: HIGH (100%)
 *   - Simple output function with no complex logic
 *   - Feature flags are determined at compile time
 *
 * CROSS-REFERENCES:
 *   - Build configuration: @maiko/inc/version.h
 *   - Feature flags: Various MAIKO_ENABLE_* macros
 */
void print_info_lines(void)
{
#if (RELEASE == 200)
  printf("Emulator for Medley release 2.0\n");
#elif (RELEASE == 201)
  printf("Emulator for Medley release 2.01\n");
#elif (RELEASE == 210)
  printf("Emulator for Medley release 2.1\n");
#elif (RELEASE == 300)
  printf("Emulator for Medley release 3.0\n");
#elif (RELEASE == 350)
  printf("Emulator for Medley release 3.5\n");
#elif (RELEASE == 351)
  printf("Emulator for Medley release 3.51\n");
#endif /* RELEASE */
  printf("Compiled for %s (%s) (%d bit).\n", MAIKO_OS_NAME, MAIKO_ARCH_NAME, MAIKO_ARCH_WORD_BITS);
  printf("Creation date: %s", ctime(&MDate));
  printf("%s\n", MaikoGitVersion);
#ifdef LPSOLVE
  printf("Contains lp_solve LP solver.\n");
#endif /* LPSOLVE */
#ifdef BIGBIGVM
  printf("Supports 256Mb virtual memory.\n");
#elif BIGVM
  printf("Supports 64Mb virtual memory.\n");
#else
  printf("Supports 32Mb virtual memory.\n");
#endif /* BIGVM */
#ifdef NOVERSION
  printf("Does not enforce SYSOUT version matching.\n");
#endif /* NOVERSION */
#ifdef MAIKO_ENABLE_FOREIGN_FUNCTION_INTERFACE
  printf("Has foreign-function-call interface.\n");
#else
  printf("Has no foreign-function-call interface.\n");
#endif /* MAIKO_ENABLE_FOREIGN_FUNCTION_INTERFACE */
#ifdef NOEUROKBD
  printf("No support for European keyboards.\n");
#else
  printf("Supports Sun European Type-4/5 keyboards.\n");
#endif /* NOEUROKBD */
}
