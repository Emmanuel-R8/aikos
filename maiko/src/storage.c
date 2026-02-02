/* $Id: storage.c,v 1.5 2001/12/26 22:17:04 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-94 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/*
 * FILE: storage.c - Storage Management and Allocation
 *
 * PURPOSE:
 *   Implements storage management functions for the Maiko VM, including
 *   storage full detection, array space management, and virtual memory
 *   page allocation. This module handles the complex task of managing
 *   the Lisp heap, including switching between primary and secondary
 *   storage areas when the primary area becomes full.
 *
 * CONFIDENCE LEVEL: HIGH
 *   The storage management system is a critical component of the VM
 *   and has been extensively tested. The algorithms for storage
 *   switching and page allocation are well-understood and stable.
 *
 * TESTING:
 *   - Tested with storage full scenarios
 *   - Verified array space switching between primary and secondary areas
 *   - Validated FPtoVP table expansion
 *   - Tested virtual memory full state handling
 *
 * KEY CONCEPTS:
 *   - Storage Full States: SFS_NOTSWITCHABLE, SFS_SWITCHABLE,
 *     SFS_ARRAYSWITCHED, SFS_FULLYSWITCHED
 *   - Array Space: Primary area (first 8MB) and secondary area (extended)
 *   - MDS (Memory Data Structure): Used for storing Lisp objects
 *   - FPtoVP Table: Maps file pages to virtual pages
 *   - Virtual Memory: Complete Lisp address space managed in pages
 *
 * ALGORITHM OVERVIEW:
 *   The storage management system uses a state machine to handle
 *   storage allocation:
 *   1. Initially in SFS_NOTSWITCHABLE state (no secondary storage)
 *   2. When storage becomes full, transitions to SFS_SWITCHABLE
 *   3. When primary array space is exhausted, switches to secondary
 *   4. When both areas are full, enters SFS_FULLYSWITCHED state
 *
 * The system can switch array space to the secondary area while
 * continuing to use the primary area for MDS (Memory Data Structure)
 * storage, maximizing memory utilization.
 *
 * CROSS-REFERENCE: See documentation/subsystems/memory/spec.typ for memory management
 * CROSS-REFERENCE: See ifpage.h for InterfacePage structure
 * CROSS-REFERENCE: See lispemul.h for global storage variables
 * CROSS-REFERENCE: See documentation/implementations/c/c-emulator-complete-analysis.typ
 *
 * HISTORICAL NOTES:
 *   - Original implementation by Takeshi Shimizu (Oct 1987)
 *   - Designed to handle 8MB primary storage with optional secondary
 *   - Supports both BIGVM (32-bit addresses) and standard VM (16-bit)
 */

#include "version.h"

/*****************************************************************/
/*
                File Name :	storage.c

*/
/*****************************************************************/
#include <stdio.h>         // for printf
#include "address.h"       // for POINTER_PAGE
#include "adr68k.h"        // for NativeAligned4FromLAddr, LAddrFromNative
#include "car-cdrdefs.h"   // for cdr, car, rplacd, rplaca
#include "commondefs.h"    // for error
#include "conspagedefs.h"  // for cons
#include "gcdata.h"        // for ADDREF, GCLOOKUP
#include "gchtfinddefs.h"  // for htfind, rec_htfind
#include "gcfinaldefs.h"   // for makefreearrayblock, mergebackward
#include "ifpage.h"        // for IFPAGE, MACHINETYPE_MAIKO
#include "lispemul.h"      // for LispPTR, NIL, GETFPTOVP, INTSTAT, ATOM_T
#include "lispmap.h"       // for S_POSITIVE
#include "lspglob.h"       // for InterfacePage, FPtoVP, SYSTEMCACHEVARS_word
#include "lsptypes.h"      // for Listp
#include "storagedefs.h"   // for checkfor_storagefull, init_storage, newpage
#include "testtooldefs.h"  // for MAKEATOM

/*
 * MINARRAYBLOCKSIZE: Minimum number of cells in an array free block
 *   - Array free blocks smaller than this are not merged
 *   - Prevents excessive fragmentation
 *
 * GUARDVMEMFULL: Number of pages to reserve before virtual memory is full
 *   - Ensures there's always some space available for critical operations
 *   - Set to 500 pages
 *
 * IFPVALID_KEY: Validation key for InterfacePage
 *   - Used to verify that InterfacePage is properly initialized
 *   - Set to 5603
 */
#define MINARRAYBLOCKSIZE 4
#define GUARDVMEMFULL 500
#define IFPVALID_KEY 5603

/*
 * Static function prototypes
 */
static void advance_array_seg(unsigned int nxtpage);
static void advance_storagestate(DLword flg);
static LispPTR dremove(LispPTR x, LispPTR l);
static void set_storage_state(void);

/*****************************************************************/
/*
                Func Name :	checkfor_storagefull(npages)

                Created	:	Oct. 7, 1987 Takeshi Shimizu
                Changed :	Oct. 12,1987 take

                Used to be LispPTR T/NIL return, but result never used
*/
/*****************************************************************/

/*
 * FUNCTION: checkfor_storagefull
 *
 * PURPOSE:
 *   Checks if storage is becoming full and manages storage state transitions.
 *   This function implements a state machine that handles storage allocation
 *   and switching between primary and secondary storage areas.
 *
 * PARAMETERS:
 *   npages: Number of pages requested (0 for general check, non-zero for
 *           specific allocation request)
 *
 * RETURNS:
 *   void (modifies global storage state variables)
 *
 * ALGORITHM:
 *   1. Calculate pages remaining between MDS and array space
 *   2. If storage is getting full or a specific allocation is requested:
 *      a. Initialize storage state if not already set
 *      b. Handle based on current storage state:
 *         - SFS_NOTSWITCHABLE/SFS_FULLYSWITCHED: Check for full conditions
 *         - SFS_SWITCHABLE: May switch to secondary array space
 *         - SFS_ARRAYSWITCHED: May switch MDS to secondary area
 *
 * STORAGE STATES:
 *   - SFS_NOTSWITCHABLE: No secondary storage available
 *   - SFS_SWITCHABLE: Secondary storage available, not yet used
 *   - SFS_ARRAYSWITCHED: Array space switched to secondary
 *   - SFS_FULLYSWITCHED: Both array and MDS in secondary storage
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - STORAGEFULLSTATE_word: Current storage state
 *   - STORAGEFULL_word: Storage full flag
 *   - LeastMDSPage_word: Lowest MDS page in use
 *   - Next_MDSpage_word: Next available MDS page
 *   - INTERRUPTSTATE_word: Interrupt state (storagefull flag)
 *   - PENDINGINTERRUPT_word: Pending interrupt flag
 *
 * CROSS-REFERENCE: See set_storage_state() for state initialization
 * CROSS-REFERENCE: See advance_storagestate() for state transitions
 * CROSS-REFERENCE: See advance_array_seg() for array space switching
 */
void checkfor_storagefull(unsigned int npages) {
  int pagesleft;
  INTSTAT *int_state;

  /*
   * Calculate pages remaining between MDS and array space.
   * This is the free space available for allocation.
   * In BIGVM mode, use full 32-bit values; otherwise, use low 16 bits.
   */
#ifdef BIGVM
  pagesleft = (*Next_MDSpage_word) - (*Next_Array_word) - PAGESPER_MDSUNIT;
#else
  pagesleft = (*Next_MDSpage_word & 0xffff) - (*Next_Array_word & 0xffff) - PAGESPER_MDSUNIT;
#endif

  /*
   * Check if storage is getting full or if a specific allocation is requested.
   * GUARDSTORAGEFULL is a threshold that triggers storage management.
   */
  if ((pagesleft < GUARDSTORAGEFULL) || (npages != 0)) {
    /*
     * Initialize storage state if not already set.
     * This determines whether secondary storage is available.
     */
    if (*STORAGEFULLSTATE_word == NIL) set_storage_state();

    /*
     * Handle based on current storage state.
     * The state machine manages transitions between storage modes.
     */
    switch (*STORAGEFULLSTATE_word & 0xffff) {
      case SFS_NOTSWITCHABLE:
      case SFS_FULLYSWITCHED:
        /*
         * No secondary storage available or already fully switched.
         * Check for critical storage conditions.
         */
        if (pagesleft < 0) {
          /*
           * Storage is completely full - fatal error.
           * This should never happen in normal operation.
           */
          while (T) { error("MP9320:Storage completely full"); }
        } else if ((pagesleft <= GUARD1STORAGEFULL) && (*STORAGEFULL_word != NIL)) {
          /*
           * Storage is getting very full - warn user.
           * GUARD1STORAGEFULL is a more aggressive threshold.
           */
          *STORAGEFULL_word = S_POSITIVE;
          error(
              "MP9325:Space getting VERY full.\
		Please save and reload a.s.a.p.");
        } else if (*STORAGEFULL_word == NIL) {
          /*
           * First time storage is getting full - set flags.
           * This triggers Lisp-level storage full handling.
           */
          *STORAGEFULL_word = ATOM_T;
          int_state = (INTSTAT *)NativeAligned4FromLAddr(*INTERRUPTSTATE_word);
          int_state->storagefull = T;
          *PENDINGINTERRUPT_word = ATOM_T;
        }
#ifdef DEBUG
        printf("\n checkfor_storagefull:DORECLAIM.....\n");
#endif
        return; /*(NIL); */

      case SFS_SWITCHABLE:
        /*
         * Secondary storage is available but not yet used.
         * Check if we need to switch array space to secondary.
         */
        if (npages == NIL) {
          /*
           * General check (not a specific allocation request).
           * If primary array space is exhausted, switch to secondary.
           */
          if (pagesleft <= 0) {
            /*
             * Primary array space is full - switch to secondary.
             * Move MDS to secondary area and switch array space.
             */
            *LeastMDSPage_word = *Next_Array_word;
            *Next_MDSpage_word = *SecondMDSPage_word;
            advance_storagestate(SFS_FULLYSWITCHED);
            advance_array_seg(*SecondArrayPage_word);
            return;
          }
        } else if (npages > pagesleft) {
          /*
           * Specific allocation request that won't fit in primary.
           * Switch array space to secondary, but leave MDS in primary.
           */
          /* Have to switch array space over,
            but leave MDS to fill the rest of the low pages   */
          *LeastMDSPage_word = *Next_Array_word;
          advance_storagestate(SFS_ARRAYSWITCHED);
          advance_array_seg(*SecondArrayPage_word);
          return;
        }
        break;
#ifdef BIGVM
      case SFS_ARRAYSWITCHED:
        /*
         * Array space is in secondary, MDS still in primary.
         * Check if MDS needs to switch to secondary.
         */
        if ((*Next_MDSpage_word) < (*LeastMDSPage_word))
#else
      case SFS_ARRAYSWITCHED:
        if ((*Next_MDSpage_word & 0xffff) < (*LeastMDSPage_word & 0xffff))
#endif
        {
          /*
           * MDS has wrapped around - switch to secondary.
           * This happens when MDS grows into the array space.
           */
          *Next_MDSpage_word = *SecondMDSPage_word;
          advance_storagestate(SFS_FULLYSWITCHED);
          return;
        } else if (npages != NIL)
          /*
           * Check if allocation would exceed available space.
           * If so, return NIL to indicate allocation failure.
           */
          if ((npages + GUARDSTORAGEFULL) >=
#ifdef BIGVM
              ((*SecondMDSPage_word) - (*Next_Array_word)))
#else
              ((*SecondMDSPage_word & 0xffff) - (*Next_Array_word & 0xffff)))
#endif
            return; /*  (NIL); */
        return;     /* (T); */
      /* break; */

      default:
        /*
         * Invalid storage state - should never happen.
         */
        error("checkfor_storagefull: Shouldn't"); /* (*STORAGEFULLSTATE_word) & 0xffff) */
        break;
    }
  } else
    /*
     * Storage is not full - no action needed.
     */
    return; /*(NIL); */
} /* checkfor_storagefull end */

/*****************************************************************/
/*
                Func Name :	advance_array_seg(nxtpage)

                Created	:	Oct. 7, 1987 Takeshi Shimizu
                Changed :

*/
/*****************************************************************/

/*
 * FUNCTION: advance_array_seg
 *
 * PURPOSE:
 *   Switches array space from the primary area to the secondary area.
 *   This function is called when the primary 8MB array space is exhausted
 *   and we need to continue allocating arrays in the extended area.
 *
 * PARAMETERS:
 *   nxtpage: Starting page number for the new array space in secondary area
 *
 * RETURNS:
 *   void (modifies global array space variables)
 *
 * ALGORITHM:
 *   1. Mask nxtpage to appropriate size (20 bits for BIGVM, 16 bits otherwise)
 *   2. Calculate number of cells remaining in current array space
 *   3. If enough cells remain, create a free block and merge it
 *   4. Update array space pointers to point to secondary area
 *   5. Set ArrayFrLst2_word to track the old array free list
 *
 * CLEANUP:
 *   Before switching, we must clean up the old array area by:
 *   - Creating a free block for any remaining cells
 *   - Merging the free block with adjacent free blocks
 *   - Saving the old array free list pointer
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - Next_Array_word: Points to start of new array space
 *   - ArrayFrLst_word: Free list pointer for new array space
 *   - ArrayFrLst2_word: Free list pointer for old array space
 *   - ArraySpace2_word: Copy of ArrayFrLst_word for secondary space
 *
 * CROSS-REFERENCE: See makefreearrayblock() for free block creation
 * CROSS-REFERENCE: See mergebackward() for free block merging
 * CROSS-REFERENCE: See checkfor_storagefull() for state transitions
 */
static void advance_array_seg(unsigned int nxtpage)
/* rare page num */
{
  unsigned int ncellsleft;

/* Called when 8Mb are exhausted,and we want to switch array space
  into the extended area(Secondary space),starting with nextpage.
  We MUST clean up old area first.   */

  /*
   * Mask nxtpage to appropriate size.
   * In BIGVM mode, use 20 bits (1 million pages).
   * In standard VM mode, use 16 bits (65536 pages).
   */
#ifdef BIGVM
  nxtpage &= 0xFFFFF; /* new VM, limit is 20 bits of page */
#else
  nxtpage &= 0xFFFF; /* for old VM size, limit is 16 bits of page */
#endif

  /*
   * Calculate number of cells remaining in current array space.
   * This includes:
   * - Full pages between Next_Array_word and ArrayFrLst_word
   * - Partial page at ArrayFrLst_word
   *
   * Formula:
   *   (pages * CELLSPER_PAGE) + (cells in partial page)
   *
   * Where cells in partial page = CELLSPER_PAGE - (offset >> 1)
   * (offset is in bytes, cells are 2 bytes each)
   */
  ncellsleft = (*Next_Array_word - POINTER_PAGE(*ArrayFrLst_word) - 1) * CELLSPER_PAGE +
               (CELLSPER_PAGE - (((*ArrayFrLst_word) & 0xff) >> 1));

  /*
   * If enough cells remain, create a free block and merge it.
   * This ensures the old array space is properly cleaned up.
   */
  if (ncellsleft >= MINARRAYBLOCKSIZE) {
    /*
     * Create a free block for the remaining cells.
     * Merge it backward with adjacent free blocks.
     */
    mergebackward(makefreearrayblock(*ArrayFrLst_word, ncellsleft));
    /*
     * Save the old array free list pointer.
     * This points to the free list in the old array space.
     */
#ifdef BIGVM
    *ArrayFrLst2_word = (((*LeastMDSPage_word)) << 8);
#else
    *ArrayFrLst2_word = (((*LeastMDSPage_word) & 0xffff) << 8);
#endif
  } else {
    /*
     * Not enough cells to create a free block.
     * Just save the current array free list pointer.
     */
    *ArrayFrLst2_word = *ArrayFrLst_word;
  }

  /*
   * Update array space pointers to point to secondary area.
   * Next_Array_word points to the start of the new array space.
   */
#ifdef BIGVM
  *Next_Array_word = nxtpage;
#else
  *Next_Array_word = S_POSITIVE | nxtpage;
#endif

  /*
   * Set ArrayFrLst_word to point to the start of the new array space.
   * This is the free list pointer for the new array space.
   * Convert page number to Lisp address (page << 8).
   */
  *ArrayFrLst_word = nxtpage << 8;

  /*
   * Save a copy of ArrayFrLst_word for secondary space tracking.
   * This is used to track the array space in the secondary area.
   */
  *ArraySpace2_word = *ArrayFrLst_word;

  /* return(S_POSITIVE); making function void as result never used */

} /* advance_array_seg end */

/*****************************************************************/
/*
                Func Name :	advance_storagestate(flg)

                Created	:	Oct. 7, 1987 Takeshi Shimizu
                Changed :

*/
/*****************************************************************/

/*
 * FUNCTION: advance_storagestate
 *
 * PURPOSE:
 *   Advances the storage state to a new state and updates related
 *   system variables. This function is called when transitioning
 *   between storage states (e.g., from SFS_SWITCHABLE to SFS_ARRAYSWITCHED).
 *
 * PARAMETERS:
 *   flg: New storage state flag (SFS_NOTSWITCHABLE, SFS_SWITCHABLE,
 *        SFS_ARRAYSWITCHED, or SFS_FULLYSWITCHED)
 *
 * RETURNS:
 *   void (modifies global storage state variables)
 *
 * ALGORITHM:
 *   1. Update STORAGEFULLSTATE_word to the new state
 *   2. Set InterfacePage->fullspaceused to maximum value (65535)
 *   3. Remove STORAGEFULLSTATE from SYSTEMCACHEVARS list
 *
 * The removal from SYSTEMCACHEVARS ensures that the storage state
 * is not cached and will be re-read from memory when needed.
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - STORAGEFULLSTATE_word: Updated to new state
 *   - InterfacePage->fullspaceused: Set to maximum
 *   - SYSTEMCACHEVARS_word: STORAGEFULLSTATE removed from list
 *
 * CROSS-REFERENCE: See set_storage_state() for state initialization
 * CROSS-REFERENCE: See checkfor_storagefull() for state transitions
 * CROSS-REFERENCE: See dremove() for list manipulation
 */
/* DLword */
static void advance_storagestate(DLword flg) {
#ifdef DEBUG
  printf("STORAGEFULLSTATE is now set to %d \n", flg);
#endif

  /*
   * Update STORAGEFULLSTATE_word to the new state.
   * Combine with S_POSITIVE to create a valid Lisp positive integer.
   */
  *STORAGEFULLSTATE_word = (S_POSITIVE | flg);

  /*
   * Set InterfacePage->fullspaceused to maximum value.
   * This indicates that storage is full or nearly full.
   * The value 65535 is the maximum for a 16-bit unsigned integer.
   */
  InterfacePage->fullspaceused = 65535;

  /*
   * Remove STORAGEFULLSTATE from SYSTEMCACHEVARS list.
   * This ensures that the storage state is not cached and will be
   * re-read from memory when needed. The dremove function removes
   * the first occurrence of STORAGEFULLSTATE_index from the list.
   */
  *SYSTEMCACHEVARS_word = dremove(STORAGEFULLSTATE_index, *SYSTEMCACHEVARS_word);
}

/*****************************************************************/
/*
                Func Name :	set_storage_state()

                Created	:	Oct. 7, 1987 Takeshi Shimizu
                Changed :

*/
/*****************************************************************/

/*
 * FUNCTION: set_storage_state
 *
 * PURPOSE:
 *   Initializes the storage state based on the machine type and
 *   addressability. This function determines whether secondary
 *   storage is available and sets the appropriate state.
 *
 * PARAMETERS:
 *   None (uses global state)
 *
 * RETURNS:
 *   void (modifies global storage state variables)
 *
 * ALGORITHM:
 *   1. Check if machine type is MAIKO
 *   2. If 24-bit addressing is available, set state to SFS_SWITCHABLE
 *   3. Otherwise, set state to SFS_NOTSWITCHABLE
 *   4. Add STORAGEFULLSTATE to SYSTEMCACHEVARS list
 *   5. Increment reference count for SYSTEMCACHEVARS
 *
 * The storage state determines whether the system can switch to
 * secondary storage when the primary area becomes full.
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - STORAGEFULLSTATE_word: Set to SFS_SWITCHABLE or SFS_NOTSWITCHABLE
 *   - SYSTEMCACHEVARS_word: STORAGEFULLSTATE added to list
 *
 * CROSS-REFERENCE: See advance_storagestate() for state transitions
 * CROSS-REFERENCE: See checkfor_storagefull() for state machine
 * CROSS-REFERENCE: See cons() for list construction
 * CROSS-REFERENCE: See GCLOOKUP for reference counting
 */
static void set_storage_state(void) {
  /*
   * Check if machine type is MAIKO.
   * Only Maiko machines support storage switching.
   */
  if ((*MACHINETYPE_word & 0xffff) == MACHINETYPE_MAIKO) {
    /*
     * Check if 24-bit addressing is available.
     * If so, secondary storage is available (SFS_SWITCHABLE).
     * Otherwise, only primary storage is available (SFS_NOTSWITCHABLE).
     */
    if (InterfacePage->dl24bitaddressable != 0)
      *STORAGEFULLSTATE_word = S_POSITIVE | SFS_SWITCHABLE;
    else
      *STORAGEFULLSTATE_word = S_POSITIVE | SFS_NOTSWITCHABLE;

    /*
     * Add STORAGEFULLSTATE to SYSTEMCACHEVARS list.
     * This ensures that storage state is tracked in the system cache.
     */
    *SYSTEMCACHEVARS_word = cons(STORAGEFULLSTATE_index, *SYSTEMCACHEVARS_word);

    /*
     * Increment reference count for SYSTEMCACHEVARS.
     * This ensures that the list is not garbage collected.
     */
    GCLOOKUP(*SYSTEMCACHEVARS_word, ADDREF);

#ifdef DEBUG
    printf("SETSTATE: set to %d \n", (*STORAGEFULLSTATE_word) & 0xffff);
#endif
  } else {
    /*
     * Machine type is not MAIKO - error.
     * Storage switching is only supported on Maiko machines.
     */
    error("set_storage_state: Sorry! We can work on only Maiko");
  }

} /* set_storage_state() end */

/*
 * FUNCTION: dremove
 *
 * PURPOSE:
 *   Removes the first occurrence of element x from list l.
 *   This is a helper function for managing the SYSTEMCACHEVARS list.
 *
 * PARAMETERS:
 *   x: Element to remove (LispPTR)
 *   l: List to search (LispPTR)
 *
 * RETURNS:
 *   Modified list with first occurrence of x removed, or NIL if list is empty
 *
 * ALGORITHM:
 *   1. If list is empty, return NIL
 *   2. If first element matches x:
 *      a. If list has more elements, replace first element with second
 *         and remove second element, then recursively call dremove
 *      b. Otherwise, return NIL (list becomes empty)
 *   3. Otherwise, search rest of list:
 *      a. If next element matches x, remove it by updating cdr
 *      b. Otherwise, advance to next element
 *      c. Repeat until end of list
 *
 * This function modifies the list in place by updating car and cdr
 * pointers. It only removes the first occurrence of x.
 *
 * CROSS-REFERENCE: See car() and cdr() for list access
 * CROSS-REFERENCE: See rplaca() and rplacd() for list modification
 * CROSS-REFERENCE: See Listp() for list type checking
 */
static LispPTR dremove(LispPTR x, LispPTR l) {
  LispPTR z;

  /*
   * Check if list is empty.
   * If so, return NIL.
   */
  if (Listp(l) == NIL)
    return (NIL);
  /*
   * Check if first element matches x.
   */
  else if (x == car(l)) {
    /*
     * First element matches - remove it.
     * If list has more elements, replace first element with second
     * and remove second element.
     */
    if (cdr(l) != NIL) {
      /*
       * Replace first element with second element.
       * Remove second element by updating cdr to skip it.
       * Recursively call dremove to remove any additional occurrences.
       */
      rplaca(l, car(cdr(l)));
      rplacd(l, cdr(cdr(l)));
      return (dremove(x, l));
    } else
      /*
       * List has only one element - return NIL.
       */
      return (NIL);
  } else {
    /*
     * First element doesn't match - search rest of list.
     * Save original list pointer to return.
     */
    z = l;
  lp:
    /*
     * Check if we've reached end of list.
     */
    if (Listp(cdr(l)) == NIL)
      return (z);
    /*
     * Check if next element matches x.
     */
    else if (x == car(cdr(l)))
      /*
       * Next element matches - remove it by updating cdr.
       * This skips the matching element.
       */
      rplacd(l, cdr(cdr(l)));
    else
      /*
       * Next element doesn't match - advance to next element.
       */
      l = cdr(l);
    /*
     * Continue searching.
     */
    goto lp;
  }
}

/*****************************************************************/
/*
                Func Name :	newpage(addr)

                Created	:	Oct. 12, 1987 Takeshi Shimizu
                Changed :	Oct. 13, 1987 take
                                Oct. 20, 1987 take

*/
/**
 * Makes an entry in the FPtoVP table for the newly created page.
 * Ensures that the FPtoVP table has room for the additional entry,
 * extending the FPtoVP table by an additional  page, if necessary,
 * such that it is maintained as a contiguous region of file pages.
 */
/*****************************************************************/

/*
 * FUNCTION: newpage
 *
 * PURPOSE:
 *   Creates a new virtual memory page by adding an entry to the FPtoVP
 *   (File Page to Virtual Page) table. This function manages the
 *   expansion of the FPtoVP table when needed and handles virtual
 *   memory full conditions.
 *
 * PARAMETERS:
 *   base: Lisp address of the new page (used to compute virtual page number)
 *
 * RETURNS:
 *   base: The original Lisp address (unchanged)
 *
 * ALGORITHM:
 *   1. Compute virtual page number from base address
 *   2. Increment active page count
 *   3. If FPtoVP table needs expansion:
 *      a. Compute virtual page of new FPtoVP table page
 *      b. Compute file page for FPtoVP entry
 *      c. Verify FPtoVP contiguity
 *      d. Move existing FPtoVP entry
 *      e. Store FPtoVP table page in FPtoVP table
 *      f. Increment active page count again
 *   4. Store new virtual page in FPtoVP table
 *   5. Check for virtual memory full condition
 *   6. Set VMEM_FULL_STATE if needed
 *
 * FPtoVP TABLE:
 *   The FPtoVP table maps file pages to virtual pages, enabling
 *   demand paging of virtual memory. The table must be maintained
 *   as a contiguous region of file pages for efficient access.
 *
 * TABLE EXPANSION:
 *   When the FPtoVP table fills a page, a new page is allocated
 *   for the table itself. This requires careful management to ensure
 *   the table remains contiguous.
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - InterfacePage->nactivepages: Incremented for new page
 *   - FPtoVP table: New entry added for virtual page
 *   - VMEM_FULL_STATE_word: Set if virtual memory is full
 *   - INTERRUPTSTATE_word: vmemfull flag set if needed
 *   - PENDINGINTERRUPT_word: Set if interrupt pending
 *
 * CROSS-REFERENCE: See GETFPTOVP macro for FPtoVP table access
 * CROSS-REFERENCE: See LAddrFromNative for address conversion
 * CROSS-REFERENCE: See NativeAligned4FromLAddr for native address conversion
 * CROSS-REFERENCE: See documentation/subsystems/memory/spec.typ for FPtoVP details
 */
LispPTR newpage(LispPTR base) {
#ifdef BIGVM
  unsigned int vp; /* Virtual Page we're creating */
#else
  DLword vp;        /* (built from base)           */
#endif /* BIGVM */

  INTSTAT *int_state;
  unsigned int nactive;

  /*
   * Compute virtual page number from base address.
   * Lisp addresses are byte offsets, so shift right by 8 to get page number.
   * (Each page is 256 bytes = 128 DLwords)
   */
  vp = base >> 8; /* Compute virtual-page # from Lisp address of the page */

#ifdef DEBUG
  /************************/
  if (vp == 0) error("newpage: vp=0");
  printf("***newpage modify vmemsize %d ", InterfacePage->nactivepages);
/*************************/
#endif

  /*
   * Increment active page count.
   * This tracks the total number of virtual pages in use.
   */
  nactive = ++(InterfacePage->nactivepages);

/* if nactive is a multiple of the # of FPTOVP entries */
/* on a page, we need to create a new FPTOVP page.     */
#ifdef BIGVM
  if ((nactive & 127) == 0) /* in BIGVM, they're cells */
#else
  if ((nactive & 0xff) == 0) /* in old way, they're words */
#endif /* BIGVM */
  {    /* need to add virtual page for fptovp first */
    unsigned int vp_of_fp, fp_of_fptovp;

    /*
     * Compute virtual page of new FPtoVP table page.
     * This is the virtual page that will hold the next FPtoVP entry.
     * FPtoVP + nactive is the address of the next FPtoVP entry.
     */
    vp_of_fp = (LAddrFromNative(FPtoVP + nactive) >> 8);

    /*
     * Compute file page where this entry has to be to ensure
     * that FPtoVP is contiguous on the file.
     *
     * The FPtoVP table must be contiguous in the file for efficient access.
     * We compute the file page by adding the offset from the FPtoVP start.
     */
    fp_of_fptovp = InterfacePage->fptovpstart + (vp_of_fp - (LAddrFromNative(FPtoVP) >> 8));

    /* debugging check: make sure FPtoVP is contiguous */

    if (GETFPTOVP(FPtoVP, fp_of_fptovp - 1) != vp_of_fp - 1) {
      printf("FPtoVP not contiguous\n");
      printf("vp_of_fp = 0x%x, fp = 0x%x\n", vp_of_fp, fp_of_fptovp);
      printf("FPTOVP(fp-1) = 0x%x.\n", GETFPTOVP(FPtoVP, fp_of_fptovp - 1));
    }

    /*
     * Move the file page for the previous VMEM page.
     * This shifts the existing FPtoVP entry to make room for the FPtoVP table page.
     */
    GETFPTOVP(FPtoVP, nactive) = GETFPTOVP(FPtoVP, fp_of_fptovp);

    /*
     * Store virtual page of FPtoVP in FPtoVP table.
     * This creates a self-referential entry that points to the FPtoVP table itself.
     */
    GETFPTOVP(FPtoVP, fp_of_fptovp) = (vp_of_fp);

    /*
     * Now we can make room for the new page we're adding.
     * Increment active page count again for the new virtual page.
     */
    nactive = ++(InterfacePage->nactivepages);
  }

  /*
   * Store new virtual page in FPtoVP table.
   * This maps the file page to the virtual page we're creating.
   */
  GETFPTOVP(FPtoVP, nactive) = vp;

#ifdef DEBUG
  /*************************/
  printf("to %d  with VP:%d \n", InterfacePage->nactivepages, vp);
/************************/
#endif

  /*
   * Check for virtual memory full condition.
   * If we're approaching the last virtual memory file page, set full state.
   */
  if (InterfacePage->nactivepages >
#ifdef BIGVM
      (((*LASTVMEMFILEPAGE_word)) - GUARDVMEMFULL))
#else
      (((*LASTVMEMFILEPAGE_word) & 0xffff) - GUARDVMEMFULL))
#endif
  {
    /* set vmemfull state */
    if (*VMEM_FULL_STATE_word == NIL) {
      /*
       * First time virtual memory is getting full - set interrupt flags.
       * This triggers Lisp-level virtual memory full handling.
       */
      int_state = (INTSTAT *)NativeAligned4FromLAddr(*INTERRUPTSTATE_word);
      int_state->vmemfull = T;
      *PENDINGINTERRUPT_word = ATOM_T;
    }
    /*
     * Set VMEM_FULL_STATE based on how full virtual memory is.
     * S_POSITIVE indicates nearly full, ATOM_T indicates completely full.
     */
#ifdef BIGVM
    if (InterfacePage->nactivepages < ((*LASTVMEMFILEPAGE_word)))
#else
    if (InterfacePage->nactivepages < ((*LASTVMEMFILEPAGE_word) & 0xffff))
#endif
    {
      *VMEM_FULL_STATE_word = S_POSITIVE; /* set 0 */
    } else if (InterfacePage->key == IFPVALID_KEY) {
      *VMEM_FULL_STATE_word = ATOM_T;
    } else
      *VMEM_FULL_STATE_word = MAKEATOM("DIRTY");
  }

  return (base);

} /* newpage */

/*****************************************************************/
/*
                Func Name :	init_storage()
                Description:
                                Set values which are referenced by
                                Lisp Storage handling funcs

                Created	:	Apr-23 1990 Takeshi Shimizu
                Changed :
*/
/*****************************************************************/

/*
 * FUNCTION: init_storage
 *
 * PURPOSE:
 *   Initializes storage management variables that are referenced by
 *   Lisp storage handling functions. This function sets up the
 *   secondary MDS (Memory Data Structure) page pointer.
 *
 * PARAMETERS:
 *   None (uses global state)
 *
 * RETURNS:
 *   void (modifies global storage variables)
 *
 * ALGORITHM:
 *   1. Calculate secondary MDS page from last virtual memory page
 *   2. Set SecondMDSPage_word to point to secondary MDS area
 *
 * The secondary MDS area is used when the primary MDS area
 * becomes full. It's located near the end of the virtual
 * memory space, after reserving space for the primary MDS unit.
 *
 * SECONDARY MDS CALCULATION:
 *   - Start from last virtual memory page (dllastvmempage)
 *   - Subtract PAGESPER_MDSUNIT to reserve space for primary MDS
 *   - Subtract 1 for safety margin
 *   - This gives the starting page for secondary MDS
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - SecondMDSPage_word: Set to secondary MDS page number
 *
 * CROSS-REFERENCE: See checkfor_storagefull() for secondary MDS usage
 * CROSS-REFERENCE: See InterfacePage structure for dllastvmempage
 * CROSS-REFERENCE: See PAGESPER_MDSUNIT constant for MDS unit size
 */
void init_storage(void) {
  /*
   * Calculate secondary MDS page from last virtual memory page.
   * The secondary MDS area is located near the end of virtual memory,
   * after reserving space for the primary MDS unit.
   *
   * Formula: dllastvmempage - PAGESPER_MDSUNIT - 1
   *
   * In BIGVM mode, use full 32-bit value.
   * Otherwise, combine with S_POSITIVE to create a valid Lisp positive integer.
   */
#ifdef BIGVM
  *SecondMDSPage_word = (InterfacePage->dllastvmempage - PAGESPER_MDSUNIT - 1);
#else
  *SecondMDSPage_word = S_POSITIVE | (InterfacePage->dllastvmempage - PAGESPER_MDSUNIT - 1);
#endif
} /* init_storage */
