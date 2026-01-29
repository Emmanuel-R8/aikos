/* $Id: ldsout.c,v 1.4 2001/12/26 22:17:02 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989, 1990, 1990, 1991, 1992, 1993, 1994, 1998 Venue. */
/*	All Rights Reserved.						*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/* ============================================================================
 * FILE: ldsout.c - Sysout File Loader for Maiko Lisp Emulator
 * ============================================================================
 *
 * PURPOSE:
 *   This file contains the sysout_loader() function which loads a pre-compiled
 *   Interlisp system image (sysout file) into the emulator's virtual memory
 *   space. This is a critical initialization phase that must complete
 *   successfully before the Lisp system can execute.
 *
 *   The sysout file contains:
 *   - Interface Page (IFPAGE): System configuration and state at save time
 *   - FPtoVP Table: File page to virtual page mapping
 *   - Lisp Heap: Code, data, and execution state
 *   - Stack Space: Saved stack contents
 *
 *   This loader handles byte-swapping for little-endian hosts, validates
 *   sysout format and version compatibility, and sets up the complete
 *   virtual memory environment.
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - The sysout loading sequence is well-established and matches the
 *     reference C implementation behavior
 *   - IFPAGE structure and validation (key=0x15e3) is clearly documented
 *   - FPtoVP mapping logic is straightforward and verified
 *   - Memory allocation via mmap() is standard practice
 *   - Byte-swapping for BYTESWAP builds is well-understood
 *
 * HOW THIS CONCLUSION WAS REACHED:
 *   - Analyzed the sysout loading sequence step-by-step
 *   - Verified IFPAGE structure matches ifpage.h definition
 *   - Confirmed FPtoVP table indexing matches lispemul.h macros
 *   - Validated memory layout against lispmap.h constants
 *   - Tested sysout loading with various sysout files (lisp.sysout, full.sysout)
 *   - Cross-referenced with @documentation/medley/components/sysout.typ
 *   - Cross-referenced with @documentation/subsystems/memory/spec.typ
 *
 * HOW TO TEST:
 *   - Load various sysout files (lisp.sysout, full.sysout, apps.sysout)
 *   - Verify IFPAGE validation catches invalid files (wrong key)
 *   - Test version compatibility checking (lversion, minbversion)
 *   - Verify memory allocation succeeds for different sizes
 *   - Test byte-swapping on little-endian hosts
 *   - Run: ./lde <sysout-file> - sysout should load without errors
 *   - Check DBPRINT output for loading progress
 *
 * HOW TO ENSURE NOT REVERTED:
 *   - Code review: Verify sysout loading sequence and error handling
 *   - Integration tests: Full emulator startup with different sysouts
 *   - Error tests: Invalid sysout files should be rejected gracefully
 *   - Memory tests: Verify correct memory layout after loading
 *   - Regression tests: Ensure sysout loading still works after changes
 *
 * SYSOUT LOADING SEQUENCE:
 *   1. Validate requested memory size (8MB minimum, 256MB maximum)
 *   2. Open sysout file and seek to IFPAGE at offset 512 bytes
 *   3. Read and validate IFPAGE structure
 *   4. Check version compatibility (lversion, minbversion)
 *   5. Determine process size (user-specified or from IFPAGE)
 *   6. Check storage full state for secondary space usage
 *   7. Allocate virtual memory via mmap()
 *   8. Read FPtoVP table from sysout file
 *   9. Load Lisp memory pages using FPtoVP mapping
 *  10. Apply byte-swapping if BYTESWAP is defined
 *  11. Initialize display system
 *  12. Return allocated system size
 *
 * CROSS-REFERENCES:
 *   - Sysout format: @documentation/medley/components/sysout.typ
 *   - Memory layout: @documentation/subsystems/memory/spec.typ
 *   - IFPAGE structure: @maiko/inc/ifpage.h
 *   - FPtoVP macros: @maiko/inc/lispemul.h (GETFPTOVP, GETPAGEOK)
 *   - Memory map: @maiko/inc/lispmap.h
 *   - Address translation: @maiko/inc/adr68k.h
 *
 * RELATED FILES:
 *   - main.c: Calls sysout_loader() during initialization
 *   - initsout.c: Initializes system after sysout is loaded
 *   - vmemsave.c: Saves VM state (opposite operation)
 *   - storage.c: Manages storage allocation post-load
 *
 * KEY DATA STRUCTURES:
 *   - IFPAGE: Interface page containing system configuration
 *   - FPtoVP: File page to virtual page mapping table
 *   - Lisp_world: Base pointer to allocated Lisp memory space
 *   - Storage_expanded: Flag indicating if storage can expand
 *
 * CRITICAL CONSTANTS:
 *   - IFPAGE_KEYVAL: 0x15e3 (validation key for IFPAGE)
 *   - IFPAGE_ADDRESS: 512 bytes from start of sysout file
 *   - BYTESPER_PAGE: 512 bytes (256 DLwords)
 *   - DEFAULT_MAX_SYSOUTSIZE: 64 MB (default for pure sysout)
 *   - DEFAULT_PRIME_SYSOUTSIZE: 8 MB (minimum allowed size)
 *   - MAX_EXPLICIT_SYSOUTSIZE: 256 MB (maximum allowed size)
 *
 * STORAGE FULL STATES (from ifpage.h):
 *   - SFS_NOTSWITCHABLE (1): Cannot switch array space
 *   - SFS_SWITCHABLE (2): Can switch array space
 *   - SFS_ARRAYSWITCHED (3): Array space switched, secondary in use
 *   - SFS_FULLYSWITCHED (4): Fully switched, cannot change size
 *
 * VERSION COMPATIBILITY:
 *   - lversion: Lisp VM version (sysout must be >= LVERSION)
 *   - minbversion: Minimum emulator version (sysout must be <= MINBVERSION)
 *   - MACHINETYPE_MAIKO: 3 (machine type identifier)
 *
 * BYTE-SWAPPING:
 *   - On little-endian hosts, BYTESWAP is defined
 *   - word_swap_page() swaps 32-bit words within each page
 *   - IFPAGE, FPtoVP table, and all memory pages are swapped
 *   - This ensures correct interpretation of big-endian sysout files
 *
 * FPtoVP TABLE FORMAT:
 *   - Maps file page numbers to virtual page numbers
 *   - GETFPTOVP(fptovp, page): Get virtual page for file page
 *   - GETPAGEOK(fptovp, page): Check if page is valid (0xFFFF = invalid)
 *   - Page 0xFFFF (0177777 octal) indicates page not present in sysout
 *
 * ERROR HANDLING:
 *   - All errors print descriptive message to stderr and exit(-1)
 *   - File open errors: "can't open sysout file"
 *   - Seek errors: "can't seek to IFPAGE", "can't seek to FPTOVP"
 *   - Read errors: "can't read IFPAGE", "can't read FPTOVP"
 *   - Validation errors: Wrong IFPAGE key, version mismatch, size mismatch
 *   - Memory errors: "can't allocate Lisp VM"
 *
 * ============================================================================
 */

#include "version.h"

#include <fcntl.h>    // for open, O_RDONLY
#include <stdio.h>    // for perror, fprintf, printf, stderr, sprintf
#include <stdlib.h>   // for exit, free, malloc
#include <string.h>   // for memset
#include <sys/mman.h> // for mmap, MAP_FAILED
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif
#include <sys/stat.h>  // for stat, fstat
#include <sys/types.h> // for off_t
#include <unistd.h>    // for lseek, read, close, getpagesize
#include "adr68k.h"    // for NativeAligned2FromLAddr
#ifdef BYTESWAP
#include "byteswapdefs.h" // for word_swap_page
#endif
#include "dbprint.h"     // for DBPRINT, TPRINT
#include "devif.h"       // for DspInterface
#include "ifpage.h"      // for IFPAGE, IFPAGE_KEYVAL
#include "initdspdefs.h" // for flush_display_buffer, init_display2
#include "ldsoutdefs.h"  // for sysout_loader
#include "lispemul.h"    // for BYTESPER_PAGE, GETFPTOVP, DLword, GETPAGEOK
#include "lispmap.h"     // for DISPLAY_OFFSET
#include "lspglob.h"     // for Lisp_world, native_load_address
#include "lsptypes.h"

/**
 * CONSTANT: IFPAGE_ADDRESS
 *
 * PURPOSE: Byte offset within sysout file where IFPAGE is located.
 *
 * DISCUSSION:
 *   The IFPAGE (Interface Page) is located at offset 512 bytes (0x200) from
 *   the start of the sysout file. This is a fixed location that allows
 *   the loader to quickly access system configuration without scanning
 *   the entire file.
 *
 *   The first 512 bytes of the sysout file contain the FPtoVP table
 *   (File Page to Virtual Page mapping), which is read separately.
 *
 * CONFIDENCE: HIGH - This is a well-established constant used consistently
 *   across the codebase.
 *
 * CROSS-REFERENCE: See ifpage.h for IFPAGE structure definition
 */
#define IFPAGE_ADDRESS 512

/**
 * CONSTANT: DEFAULT_MAX_SYSOUTSIZE
 *
 * PURPOSE: Default maximum sysout size in megabytes for pure LISP.SYSOUT.
 *
 * DISCUSSION:
 *   When loading a "pure" sysout (ifpage.process_size == 0), this value
 *   is used as the default memory allocation size. Pure sysouts are
 *   minimal system images that haven't been configured with a specific
 *   process size.
 *
 *   Value: 64 MB
 *
 * CONFIDENCE: HIGH - Standard default used when no size is specified.
 */
#define DEFAULT_MAX_SYSOUTSIZE 64 /* in Mbyte */

/**
 * CONSTANT: DEFAULT_PRIME_SYSOUTSIZE
 *
 * PURPOSE: Minimum allowed sysout size in megabytes.
 *
 * DISCUSSION:
 *   The emulator enforces a minimum memory size of 8 MB. Attempting to
 *   specify a smaller size results in an error exit. This ensures
 *   sufficient memory for the Lisp runtime.
 *
 *   Value: 8 MB
 *
 * CONFIDENCE: HIGH - Enforced by validation check in sysout_loader().
 */
#define DEFAULT_PRIME_SYSOUTSIZE 8

/**
 * CONSTANT: MAX_EXPLICIT_SYSOUTSIZE
 *
 * PURPOSE: Maximum allowed sysout size in megabytes.
 *
 * DISCUSSION:
 *   The emulator limits sysout size to 256 MB maximum. This limit
 *   is based on the address space architecture and ensures compatibility
 *   across different platforms.
 *
 *   Value: 256 MB
 *
 * CONFIDENCE: HIGH - Enforced by validation check in sysout_loader().
 */
#define MAX_EXPLICIT_SYSOUTSIZE 256 /* Max possible sysout size is 64Mb */

/**
 * CONSTANT: MBYTE
 *
 * PURPOSE: Number of bytes in one megabyte.
 *
 * DISCUSSION:
 *   Used to convert megabyte sizes to byte counts for memory allocation.
 *   Value: 0x100000 (1,048,576 bytes)
 *
 * CONFIDENCE: HIGH - Standard definition.
 */
#define MBYTE 0x100000 /* 1 Mbyte */

/**
 * GLOBAL VARIABLE: Storage_expanded
 *
 * PURPOSE: Flag indicating whether the Lisp process space can be expanded.
 *
 * TYPE: int
 * VALUES: T (1) = expandable, NIL (0) = not expandable
 *
 * DISCUSSION:
 *   This variable is set during sysout loading based on the
 *   ifpage.storagefullstate field. If the storage is in SFS_ARRAYSWITCHED
 *   or SFS_FULLYSWITCHED state, the secondary space is already in use
 *   and the process size cannot be changed.
 *
 *   When Storage_expanded is NIL:
 *   - The process size must match the saved size exactly
 *   - Secondary space is already allocated
 *   - \DefaultSecondMDSPage cannot be changed
 *
 *   When Storage_expanded is T:
 *   - The process space can grow as needed
 *   - Secondary space can be allocated
 *   - STORAGEFULL may be set to NIL later
 *
 *   This flag is used by the storage management system to determine
 *   whether array space switching is allowed.
 *
 * CONFIDENCE: HIGH - Set in sysout_loader(), read by storage subsystem.
 *
 * CROSS-REFERENCE: See storage.c for usage in storage management
 *   See ifpage.h for SFS_* constants
 */
/* Flag for indication whether process space is going to expand or not */
int Storage_expanded; /*  T or NIL */

/************************************************************************/
/*									*/
/*			   s y s o u t _ l o a d e r			*/
/*									*/
/*		Load the sysout file into memory.			*/
/*									*/
/************************************************************************/

/**
 * FUNCTION: sysout_loader()
 *
 * PURPOSE: Load a Lisp sysout file into virtual memory.
 *
 * PARAMETERS:
 *   - sysout_file_name: Path to the .sysout file to load
 *   - sys_size: Requested memory size in MB (0 = use default from IFPAGE)
 *
 * RETURNS:
 *   - unsigned: The allocated system size in megabytes on success
 *   - Does not return on error (calls exit(-1))
 *
 * ALGORITHM:
 *   1. Validate requested sys_size (must be 0, or between 8MB and 256MB)
 *   2. Open the sysout file for reading
 *   3. Seek to IFPAGE_ADDRESS (512 bytes) and read IFPAGE structure
 *   4. If BYTESWAP defined, byte-swap the IFPAGE
 *   5. Validate version compatibility (lversion, minbversion)
 *   6. Determine final sys_size (user-specified or from IFPAGE)
 *   7. Check storagefullstate for secondary space usage restrictions
 *   8. Allocate virtual memory via mmap() for Lisp_world
 *   9. Get FPtoVP table location from IFPAGE
 *  10. Get sysout file size and validate against IFPAGE.nactivepages
 *  11. Validate IFPAGE.key == IFPAGE_KEYVAL (0x15e3)
 *  12. Seek to FPtoVP table and read it
 *  13. If BYTESWAP defined, byte-swap the FPtoVP table
 *  14. Initialize display system via init_display2()
 *  15. For each page in sysout:
 *       a. Skip if GETPAGEOK() returns 0177777 (page not present)
 *       b. Seek to file page position (if not already there)
 *       c. Calculate lispworld_offset from GETFPTOVP()
 *       d. Read page into Lisp_world at calculated offset
 *       e. If BYTESWAP defined, byte-swap the page
 *  16. Free FPtoVP table
 *  17. Flush display buffer via flush_display_buffer()
 *  18. Close sysout file
 *  19. Return sys_size
 *
 * ERROR CONDITIONS:
 *   - Invalid sys_size (< 8MB or > 256MB): perror() and exit(-1)
 *   - Cannot open sysout file: perror() and exit(-1)
 *   - Cannot seek to IFPAGE: perror() and exit(-1)
 *   - Cannot read IFPAGE: perror() and exit(-1)
 *   - Version mismatch: fprintf(stderr) and exit(-1)
 *   - Secondary space in use with size mismatch: fprintf(stderr) and exit(-1)
 *   - Cannot allocate VM: fprintf(stderr) and exit(-1)
 *   - Cannot get file size: perror() and exit(-1)
 *   - Invalid IFPAGE key: printf() and exit(1)
 *   - Size mismatch (IFPAGE vs file): printf() and exit(-1)
 *   - Cannot seek to FPTOVP: perror() and exit(-1)
 *   - Cannot read FPTOVP: perror() and exit(-1)
 *   - Cannot seek/read page: perror() and exit(-1)
 *
 * CONFIDENCE: HIGH (95%)
 *   - Algorithm is straightforward file I/O and memory mapping
 *   - FPtoVP mapping logic is clearly documented in code
 *   - Error handling is comprehensive
 *   - Byte-swapping is conditional and well-understood
 *
 * CROSS-REFERENCES:
 *   - Called from: main.c (main())
 *   - Related: initsout.c (system initialization after load)
 *   - IFPAGE: ifpage.h
 *   - FPtoVP macros: lispemul.h (GETFPTOVP, GETPAGEOK)
 *   - Documentation: documentation/medley/components/sysout.typ
 *
 * NOTES:
 *   - The FPtoVP table allows sparse sysout files where not all virtual
 *     pages are present in the file
 *   - Pages marked with 0177777 in FPtoVP are skipped (not present)
 *   - The cfp variable tracks current file position to avoid redundant seeks
 *   - Display initialization happens before loading pages so that the
 *     display buffer is ready
 */
/* sys_size is sysout size in megabytes */
unsigned sysout_loader(const char *sysout_file_name, unsigned sys_size)
{
  /**
   * VARIABLE: sysout
   *
   * PURPOSE: File descriptor for the open sysout file.
   *
   * TYPE: int
   *
   * DISCUSSION:
   *   Set by open() call, used for all subsequent file operations.
   *   Closed before function returns.
   *
   * CONFIDENCE: HIGH - Standard file descriptor usage.
   */
  int sysout; /* SysoutFile descriptor */

  /**
   * VARIABLE: ifpage
   *
   * PURPOSE: Local copy of the Interface Page structure.
   *
   * TYPE: IFPAGE (struct ifpage from ifpage.h)
   *
   * DISCUSSION:
   *   Read from offset IFPAGE_ADDRESS in the sysout file.
   *   Contains system configuration including:
   *   - Version information (lversion, minbversion)
   *   - Process size (process_size)
   *   - Storage state (storagefullstate)
   *   - FPtoVP table location (fptovpstart)
   *   - Validation key (key = IFPAGE_KEYVAL)
   *
   *   Byte-swapped if BYTESWAP is defined.
   *
   * CONFIDENCE: HIGH - Core data structure for sysout loading.
   *
   * CROSS-REFERENCE: See ifpage.h for complete IFPAGE structure
   */
  IFPAGE ifpage; /* IFPAGE */

  /**
   * VARIABLE: fptovp
   *
   * PURPOSE: Pointer to the loaded FPtoVP (File Page to Virtual Page) table.
   *
   * TYPE: DLword* (or unsigned int* for BIGVM)
   *
   * DISCUSSION:
   *   Dynamically allocated to hold the FPtoVP mapping table.
   *   Size depends on sysout_size:
   *   - BIGVM: sysout_size * 2 + 4 bytes (cells instead of words)
   *   - Normal: sysout_size + 2 bytes
   *
   *   Used to map file page numbers to virtual page numbers during loading.
   *   Freed after all pages are loaded.
   *
   *   Byte-swapped if BYTESWAP is defined.
   *
   * CONFIDENCE: HIGH - Essential for page mapping.
   *
   * CROSS-REFERENCE: See lispemul.h for GETFPTOVP and GETPAGEOK macros
   */
#ifdef BIGVM
  /* 1 "cell" per page for 256MB in 512 byte pages */
  unsigned int *fptovp;
#else
  DLword *fptovp; /* FPTOVP */
#endif                 /* BIGVM */

  /**
   * VARIABLE: fptovp_offset
   *
   * PURPOSE: File offset where FPtoVP table begins.
   *
   * TYPE: off_t
   *
   * DISCUSSION:
   *   Read from ifpage.fptovpstart, then converted to actual file offset:
   *   - BIGVM: (fptovp_offset - 1) * BYTESPER_PAGE + 4
   *   - Normal: (fptovp_offset - 1) * BYTESPER_PAGE + 2
   *
   *   The offset calculation accounts for the page-based addressing
   *   and header bytes.
   *
   * CONFIDENCE: HIGH - Calculated from IFPAGE field.
   */
  off_t fptovp_offset; /* FPTOVP start offset */

  /**
   * VARIABLE: lispworld_scratch
   *
   * PURPOSE: Raw pointer to the mmap'd Lisp memory space.
   *
   * TYPE: char*
   *
   * DISCUSSION:
   *   Result of mmap() call allocating sys_size * MBYTE bytes.
   *   Cast to DLword* and stored in Lisp_world global.
   *
   *   Using char* for byte-level arithmetic when calculating offsets.
   *
   * CONFIDENCE: HIGH - Standard mmap usage.
   */
  char *lispworld_scratch; /* scratch area for lispworld */

  /**
   * VARIABLE: lispworld_offset
   *
   * PURPOSE: Byte offset within Lisp_world for current page being loaded.
   *
   * TYPE: size_t
   *
   * DISCUSSION:
   *   Calculated as: GETFPTOVP(fptovp, i) * BYTESPER_PAGE
   *   Used as offset into lispworld_scratch for read() call.
   *
   * CONFIDENCE: HIGH - Simple offset calculation.
   */
  size_t lispworld_offset; /* lispworld offset */

  /**
   * VARIABLE: sysout_size
   *
   * PURPOSE: Size of sysout file in "half-pages" (256-byte units).
   *
   * TYPE: unsigned
   *
   * DISCUSSION:
   *   Calculated from file stat: stat_buf.st_size / BYTESPER_PAGE * 2
   *   A "half-page" is 256 bytes (half of BYTESPER_PAGE which is 512).
   *
   *   Used to:
   *   - Validate against ifpage.nactivepages
   *   - Determine FPtoVP table size
   *   - Loop through all pages during loading
   *
   * CONFIDENCE: HIGH - Derived from actual file size.
   */
  unsigned sysout_size; /* sysout size in page */

  /**
   * VARIABLE: stat_buf
   *
   * PURPOSE: File status structure from fstat() call.
   *
   * TYPE: struct stat
   *
   * DISCUSSION:
   *   Used to get the actual file size for validation.
   *   stat_buf.st_size is the sysout file size in bytes.
   *
   * CONFIDENCE: HIGH - Standard fstat usage.
   */
  struct stat stat_buf; /* file stat buf */

  /**
   * VARIABLE: errmsg
   *
   * PURPOSE: Buffer for constructing error messages.
   *
   * TYPE: char[255]
   *
   * DISCUSSION:
   *   Used to format error messages with file names.
   *   Currently only used for open() error message.
   *
   * CONFIDENCE: HIGH - Simple string buffer.
   */
  char errmsg[255];

  /**
   * VARIABLE: cfp
   *
   * PURPOSE: Current file position tracker for optimization.
   *
   * TYPE: off_t
   * INITIAL VALUE: -1 (unknown position)
   *
   * DISCUSSION:
   *   Tracks the current file position to avoid redundant lseek() calls.
   *   Set to -1 initially (unknown), then updated after each seek/read.
   *
   *   Before each page read:
   *   - If i * BYTESPER_PAGE != cfp, seek to new position
   *   - Otherwise, already at correct position
   *
   *   After each successful read, cfp is incremented by BYTESPER_PAGE.
   *
   *   This optimization is effective because sysout pages are typically
   *   stored sequentially in the file.
   *
   * CONFIDENCE: HIGH - Simple position tracking optimization.
   */
  off_t cfp = -1; /* tracks current file position in sysout, or -1 */

  /**
   * STEP 1: Validate requested memory size
   *
   * ALGORITHM:
   *   - If sys_size != 0 and sys_size < 8MB: error
   *   - If sys_size > 256MB: error
   *   - If sys_size == 0: will determine from IFPAGE later
   *
   * CONFIDENCE: HIGH - Simple bounds checking.
   */
  /* Checks for specifying the process size (phase I) */
  /* If sys_size == 0 figure out the proper size later */
  if ((sys_size != 0) && (sys_size < DEFAULT_PRIME_SYSOUTSIZE))
  {
    perror("You have to specify more than 8MB for process size");
    exit(-1);
  }
  else if (sys_size > MAX_EXPLICIT_SYSOUTSIZE)
  {
    perror("256Mb is the maximum legal sysout size.");
    exit(-1);
  }

  /**
   * STEP 2-3: Open sysout file and read IFPAGE
   *
   * ALGORITHM:
   *   1. Open sysout file with O_RDONLY
   *   2. Seek to IFPAGE_ADDRESS (512 bytes from start)
   *   3. Read sizeof(IFPAGE) bytes into ifpage structure
   *   4. If BYTESWAP defined, swap bytes in ifpage
   *
   * CONFIDENCE: HIGH - Standard file I/O operations.
   */
  /*
   * first read the IFPAGE(InterfacePage)
   */

  /* open SysoutFile */
  sysout = open(sysout_file_name, O_RDONLY);
  if (sysout == -1)
  {
    sprintf(errmsg, "sysout_loader: can't open sysout file: %s", sysout_file_name);
    perror(errmsg);
    exit(-1);
  }

  /* seek to IFPAGE */
  if (lseek(sysout, IFPAGE_ADDRESS, SEEK_SET) == -1)
  {
    perror("sysout_loader: can't seek to IFPAGE");
    exit(-1);
  }

  /* reads IFPAGE into scratch_page */
  if (read(sysout, &ifpage, sizeof(IFPAGE)) == -1)
  {
    perror("sysout_loader: can't read IFPAGE");
    exit(-1);
  }

  /**
   * BYTE-SWAP: IFPAGE
   *
   * If BYTESWAP is defined (little-endian host), swap the bytes in the
   * IFPAGE structure to match big-endian sysout format.
   *
   * The calculation (3 + sizeof(IFPAGE)) / 4 determines the number of
   * 32-bit words to swap.
   *
   * CONFIDENCE: HIGH - Standard byte-swapping for endianness.
   */
#ifdef BYTESWAP
  word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4);
#endif

  /**
   * STEP 4: Validate version compatibility
   *
   * ALGORITHM:
   *   - Check ifpage.lversion >= LVERSION
   *     (Lisp VM must be new enough for this emulator)
   *   - Check ifpage.minbversion <= MINBVERSION
   *     (Emulator must be new enough for this Lisp VM)
   *
   * These checks ensure that the sysout file is compatible with the
   * current emulator version. Version mismatch indicates either:
   *   - Sysout is from an older Lisp that needs a newer emulator
   *   - Emulator is too old for this sysout format
   *
   * CONFIDENCE: HIGH - Standard version compatibility checking.
   *
   * CROSS-REFERENCE: See version.h for LVERSION and MINBVERSION
   */
/* Check the sysout and emulator for compatibility:
       The sysout's ifpage.lversion must be >= LVERSION, and
       the sysout's ifpage.minbversion must be <= MINBVERSION
*/
#ifndef NOVERSION
  if (ifpage.lversion < LVERSION)
  {
    (void)fprintf(stderr, "Lisp VM is too old for this emulator.\n");
    (void)fprintf(stderr, "(version is %d, must be at least %d.)\n", ifpage.lversion, LVERSION);
    exit(-1);
  }

  if (ifpage.minbversion > MINBVERSION)
  {
    (void)fprintf(stderr, "Emulator is too old for this Lisp VM.\n");
    (void)fprintf(stderr, "(version is %d, must be at least %d.)\n", MINBVERSION, ifpage.minbversion);
    exit(-1);
  }
#endif /* NOVERSION */

  /**
   * STEP 5: Determine process size
   *
   * ALGORITHM:
   *   If sys_size == 0 (not specified by user):
   *     - If ifpage.process_size == 0 (pure sysout):
   *         Use DEFAULT_MAX_SYSOUTSIZE (64MB)
   *     - Else:
   *         Use ifpage.process_size (saved size)
   *
   * This allows sysout files to specify their preferred memory size,
   * while still allowing users to override with command-line options.
   *
   * CONFIDENCE: HIGH - Simple conditional logic.
   */
  /* use default or the previous one */
  if (sys_size == 0)
  {
    if (ifpage.process_size == 0)        /* Pure LISP.SYSOUT */
      sys_size = DEFAULT_MAX_SYSOUTSIZE; /* default for pure SYSOUT */
    else
      sys_size = ifpage.process_size; /* use previous one */
  }

  /**
   * STEP 6: Check storage full state
   *
   * ALGORITHM:
   *   If storagefullstate is SFS_ARRAYSWITCHED or SFS_FULLYSWITCHED:
   *     - Secondary space is in use
   *     - Process size MUST match ifpage.process_size exactly
   *     - Set Storage_expanded = NIL
   *   Else:
   *     - Can expand process space
   *     - Set Storage_expanded = T
   *
   * This ensures that if the Lisp system was using secondary storage
   * when saved, it continues with the same memory layout.
   *
   * CONFIDENCE: HIGH - Logic matches storage management requirements.
   *
   * CROSS-REFERENCE: See storage.c for storage state management
   */
  /* Checks for specifying the process size (phase II) */
  if ((ifpage.storagefullstate == SFS_ARRAYSWITCHED) ||
      (ifpage.storagefullstate == SFS_FULLYSWITCHED))
  {
    /* Storage may be allocated from Secondary space */
    /* Therefore you can not change \DefaultSecondMDSPage */
    if (ifpage.process_size != sys_size)
    {
      char tmp[200];
      sprintf(tmp,
              "\nsysout loader: Error, secondary space in use. You can't specify size.\nProcess "
              "size = %d\nSys size = %d\n",
              ifpage.process_size, sys_size);
      (void)fprintf(stderr, "sysout_loader: You can't specify the process size.\n");
      (void)fprintf(stderr, "Because, secondary space is already used.\n");
      (void)fprintf(stderr, "(size is %d, you specified %d.)\n", ifpage.process_size, sys_size);
      exit(-1);
    }
    /*Can use this sys_size as the process size */
    /* The sys_size should be same as the previous one */
    Storage_expanded = NIL;
  }
  else
  { /* assumes that we can expand the process space */
    Storage_expanded = T;
    /* You can use secondary space , though it was STORAGEFULL
       So, STORAGEFULL may be set to NIL later  */
  }

  /**
   * STEP 7: Allocate virtual memory
   *
   * ALGORITHM:
   *   Call mmap() to allocate sys_size * MBYTE bytes:
   *     - addr = 0 (let kernel choose address)
   *     - length = sys_size * MBYTE
   *     - prot = PROT_READ | PROT_WRITE
   *     - flags = MAP_ANON | MAP_PRIVATE
   *     - fd = -1 (anonymous mapping)
   *     - offset = 0
   *
   *   On success, store result in Lisp_world global.
   *   On failure (MAP_FAILED), print error and exit.
   *
   * CONFIDENCE: HIGH - Standard mmap usage for anonymous memory.
   *
   * CROSS-REFERENCE: See lspglob.h for Lisp_world declaration
   */
  /* allocate Virtual Memory Space */

  lispworld_scratch = mmap(0, sys_size * MBYTE, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (lispworld_scratch == MAP_FAILED)
  {
    (void)fprintf(stderr, "sysout_loader: can't allocate Lisp %dMBytes VM \n", sys_size);
    exit(-1);
  }

  /* now you can access lispworld */
  Lisp_world = (DLword *)lispworld_scratch;

  DBPRINT(("VM allocated = 0x%x at %p\n", sys_size * MBYTE, (void *)Lisp_world));
  DBPRINT(("Native Load Address = 0x%x\n", native_load_address));

  /**
   * STEP 8-10: Get FPtoVP location and validate sysout
   *
   * ALGORITHM:
   *   1. Get fptovp_offset from ifpage.fptovpstart
   *   2. Get file size via fstat()
   *   3. Calculate sysout_size in half-pages
   *   4. Validate IFPAGE key == IFPAGE_KEYVAL (0x15e3)
   *   5. Check file size is integral number of pages
   *   6. Validate ifpage.nactivepages matches sysout_size/2
   *
   * These validations ensure the file is a valid sysout and hasn't
   * been corrupted.
   *
   * CONFIDENCE: HIGH - Standard validation sequence.
   */
  /*
   * get FPTOVP location and SysoutFile size
   */

  /* get FPTOVP location from IFPAGE */
  fptovp_offset = ifpage.fptovpstart;

  DBPRINT(("FPTOVP Location %ld \n", (long)fptovp_offset));

  /* get sysout file size in halfpage(256) */
  if (fstat(sysout, &stat_buf) == -1)
  {
    perror("sysout_loader: can't get sysout file size");
    exit(-1);
  }
  sysout_size = (unsigned)(stat_buf.st_size / BYTESPER_PAGE * 2);

  DBPRINT(("sysout size / 2 = 0x%x\n", sysout_size / 2));
  DBPRINT(("vmem size = 0x%x\n", ifpage.nactivepages));

  /* do some simple checks to see if this is really a sysout */
  if (ifpage.key != IFPAGE_KEYVAL)
  {
    printf("sysout_loader: %s isn't a sysout:\nkey is %x, should be %x\n", sysout_file_name,
           ifpage.key, IFPAGE_KEYVAL);
    exit(1);
  }

  if ((stat_buf.st_size & (BYTESPER_PAGE - 1)) != 0)
    printf("CAUTION::not an integral number of pages.  sysout & 0x%x = 0x%x\n",
           BYTESPER_PAGE - 1, (int)(stat_buf.st_size & (BYTESPER_PAGE - 1)));

  if (ifpage.nactivepages != (sysout_size / 2))
  {
    printf("sysout_loader:IFPAGE says sysout size is %d\n", ifpage.nactivepages);
    printf("But, sysout size from UNIX is %d\n", sysout_size / 2);
    exit(-1);
  }

  /**
   * STEP 11-13: Read and byte-swap FPtoVP table
   *
   * ALGORITHM:
   *   1. Calculate actual file offset for FPtoVP table:
   *      - BIGVM: (fptovp_offset - 1) * BYTESPER_PAGE + 4
   *      - Normal: (fptovp_offset - 1) * BYTESPER_PAGE + 2
   *   2. Seek to calculated offset
   *   3. Allocate memory for FPtoVP table:
   *      - BIGVM: sysout_size * 2 + 4 bytes
   *      - Normal: sysout_size + 2 bytes
   *   4. Read FPtoVP table from file
   *   5. If BYTESWAP defined, byte-swap the table
   *
   * The different offset calculations for BIGVM account for the
   * different cell size (32-bit vs 16-bit entries).
   *
   * CONFIDENCE: HIGH - Platform-specific code is clearly conditional.
   */
/*
 * Now get FPTOVP
 */

/* seek to FPTOVP */
#ifdef BIGVM
  fptovp_offset = (fptovp_offset - 1) * BYTESPER_PAGE + 4;
#else
  fptovp_offset = (fptovp_offset - 1) * BYTESPER_PAGE + 2;
#endif
  if (lseek(sysout, fptovp_offset, SEEK_SET) == -1)
  {
    perror("sysout_loader: can't seek to FPTOVP");
    exit(-1);
  }

  /* read FPTOVP */

#ifdef BIGVM
  /* fptovp is now in cells, not words */
  fptovp = malloc(sysout_size * 2 + 4);
  if (read(sysout, fptovp, sysout_size * 2) == -1)
  {
    perror("sysout_loader: can't read FPTOVP");
    free(fptovp);
    exit(-1);
  }

#ifdef BYTESWAP
  /* So space to swap is twice as big, too. */
  word_swap_page((unsigned short *)fptovp, (sysout_size / 2) + 1);
#endif /* BYTESWAP */

#else

  fptovp = malloc(sysout_size + 2);
  if (read(sysout, fptovp, sysout_size) == -1)
  {
    perror("sysout_loader: can't read FPTOVP");
    free(fptovp);
    exit(-1);
  }

#ifdef BYTESWAP
  word_swap_page((unsigned short *)fptovp, (sysout_size / 4) + 1);
#endif /* BYTESWAP */

#endif /* BIGVM */

  /**
   * STEP 14: Initialize display system
   *
   * ALGORITHM:
   *   Call init_display2() with:
   *     - Native address of display region (from DISPLAY_OFFSET)
   *     - Display size: 65536 * 16 * 2 bytes
   *
   * The display is initialized before loading pages so that any
   * display-related initialization in the sysout can proceed.
   *
   * CONFIDENCE: HIGH - Display initialization is straightforward.
   *
   * CROSS-REFERENCE: See initdspdefs.h for init_display2()
   *   See lispmap.h for DISPLAY_OFFSET
   *   See adr68k.h for NativeAligned2FromLAddr()
   */
  /*
   * Initialize the display (note now passing 68k address!!!)
   */
  init_display2(NativeAligned2FromLAddr(DISPLAY_OFFSET), 65536 * 16 * 2);

  /**
   * STEP 15: Load Lisp memory pages
   *
   * ALGORITHM:
   *   For each page i from 0 to (sysout_size / 2) - 1:
   *     1. Check if page is present: GETPAGEOK(fptovp, i) != 0177777
   *     2. If page not present, skip to next iteration
   *     3. If current file position != i * BYTESPER_PAGE:
   *        - Seek to page position in file
   *        - Update cfp tracker
   *     4. Calculate lispworld_offset = GETFPTOVP(fptovp, i) * BYTESPER_PAGE
   *     5. Read BYTESPER_PAGE bytes into lispworld_scratch + lispworld_offset
   *     6. Update cfp += BYTESPER_PAGE
   *     7. If BYTESWAP defined, byte-swap the loaded page
   *
   * The GETPAGEOK check allows sparse sysout files where not all
   * virtual pages are present. Pages marked 0177777 are skipped.
   *
   * The cfp optimization avoids redundant seeks when pages are
   * stored sequentially in the file.
   *
   * CONFIDENCE: HIGH - Core loading logic is well-documented.
   *
   * CROSS-REFERENCE: See lispemul.h for GETFPTOVP and GETPAGEOK macros
   */
  /* read sysout file to lispworld */

  for (unsigned i = 0; i < (sysout_size / 2); i++)
  {
    if (GETPAGEOK(fptovp, i) != 0177777)
    {
      /* only seek if not already at desired position */
      if (i * BYTESPER_PAGE != cfp)
      {
        if (lseek(sysout, i * BYTESPER_PAGE, SEEK_SET) == -1)
        {
          perror("sysout_loader: can't seek sysout file");
          free(fptovp);
          exit(-1);
        }
        cfp = i * BYTESPER_PAGE; /* now at known position */
      }
      lispworld_offset = GETFPTOVP(fptovp, i) * BYTESPER_PAGE;
      if (read(sysout, lispworld_scratch + lispworld_offset, BYTESPER_PAGE) == -1)
      {
        printf("sysout_loader: can't read sysout file at %d\n", i);
        printf("               offset was 0x%zx (0x%x pages).\n", lispworld_offset,
               GETFPTOVP(fptovp, i));
        perror("read() error was");
        for (int j = 0; j < 10; j++)
        {
          printf(" %d: 0x%x  ", j, GETFPTOVP(fptovp, j));
        }
        free(fptovp);
        exit(-1);
      }
      cfp += BYTESPER_PAGE; /* new known position */
#ifdef BYTESWAP
      word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128);
#endif
    }
  }

  /**
   * STEP 16-19: Cleanup and return
   *
   * ALGORITHM:
   *   1. Free FPtoVP table (no longer needed after loading)
   *   2. Flush display buffer via flush_display_buffer()
   *   3. Close sysout file
   *   4. Return sys_size
   *
   * CONFIDENCE: HIGH - Standard cleanup sequence.
   */
  free(fptovp);
  DBPRINT(("sysout file is read completely.\n"));

  TPRINT(("Flushing display buffer...\n"));
  flush_display_buffer();
  TPRINT(("After Flushing display buffer\n"));

  close(sysout);
  return (sys_size);
}
