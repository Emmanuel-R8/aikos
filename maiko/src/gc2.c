/* $Id: gc2.c,v 1.3 1999/05/31 23:35:30 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved  */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/**********************************************************************/
/*
                File Name:	gc2.c
                Desc: implement opcode SCAN1,SCAN2,GCRECLAIMCELL


                Including :	OP_scan1
                                OP_scan2
                                OP_gcreccell

*/
/**********************************************************************/

/* FILE: gc2.c - Garbage Collection Scan Operations
 *
 * This file implements the SCAN1, SCAN2, and GCRECLAIMCELL opcodes
 * for the garbage collector. These operations support incremental
 * garbage collection by scanning memory regions for live objects.
 *
 * HIGH CONFIDENCE: The scan operations are fundamental to the GC's
 * incremental collection strategy. The algorithm is well-documented
 * and has been stable for many years.
 *
 * OPERATIONS:
 * - SCAN1: First-pass scan of a memory region
 * - SCAN2: Second-pass scan for incremental collection
 * - GCRECLAIMCELL: Reclaim individual cells during GC
 *
 * The scan operations work with positive integers representing
 * memory addresses. They use the LOLOC macro to extract the
 * low 16 bits of a Lisp pointer for indexing into memory.
 *
 * CROSS-REFERENCE: See gcscandefs.h for gcscan1() and gcscan2()
 * CROSS-REFERENCE: See address.h for LOLOC macro
 */

#include <stdio.h>         // for printf
#include "address.h"       // for LOLOC
#include "emlglob.h"
#include "gc2defs.h"       // for OP_gcscan1, OP_gcscan2
#include "gcscandefs.h"    // for gcscan1, gcscan2
#include "lispemul.h"      // for state, TopOfStack, NIL, PC, SEGMASK
#include "lispmap.h"       // for S_POSITIVE
#include "lspglob.h"
#include "lsptypes.h"
#include "testtooldefs.h"  // for printPC

/**********************************************************************/
/*
                Func Name : OP_gcscan1
*/
/**********************************************************************/

/*
 * FUNCTION: OP_gcscan1
 *
 * PURPOSE:
 *   Implements the SCAN1 opcode for the garbage collector.
 *   This opcode performs the first-pass scan of a memory region
 *   to identify live objects during incremental garbage collection.
 *
 * PARAMETERS:
 *   None (uses global state)
 *
 * RETURNS:
 *   void (modifies TopOfStack and PC)
 *
 * ALGORITHM:
 *   1. Check if TopOfStack is a positive integer (memory address)
 *   2. Extract low 16 bits (LOLOC) to get memory address
 *   3. Call gcscan1() to perform first-pass scan
 *   4. If scan returns -1, set TopOfStack to NIL
 *   5. Otherwise, combine result with S_POSITIVE and set TopOfStack
 *   6. Advance PC by 1 byte
 *
 * SCAN1 OPERATION:
 *   The first-pass scan identifies live objects in a memory region.
 *   It marks objects that are reachable from roots and prepares
 *   for the second-pass scan (SCAN2).
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - TopOfStack: Set to scan result or NIL
 *   - PC: Advanced by 1 byte
 *
 * CROSS-REFERENCE: See gcscandefs.h for gcscan1() implementation
 * CROSS-REFERENCE: See address.h for LOLOC macro
 * CROSS-REFERENCE: See OP_gcscan2() for second-pass scan
 */
void OP_gcscan1(void) {
  int scan;
#ifdef TRACE
  printPC();
  printf("TRACE: OP_gcscan1()\n");
#endif
  /*
   * Check if TopOfStack is a positive integer (memory address).
   * The SEGMASK extracts the segment bits, which should be S_POSITIVE.
   */
  if ((TopOfStack & SEGMASK) == S_POSITIVE) {
    /*
     * Extract low 16 bits (LOLOC) to get memory address.
     * Call gcscan1() to perform first-pass scan.
     * If scan returns -1, set TopOfStack to NIL.
     * Otherwise, combine result with S_POSITIVE and set TopOfStack.
     */
    scan = gcscan1(LOLOC(TopOfStack));
    TopOfStack = (scan == -1) ? NIL : scan | S_POSITIVE;
  } else {
    /*
     * TopOfStack is not a positive integer - error.
     * This should not happen in normal operation.
     */
    printf("OP_gcscan1: not a number\n");
  }
  PC++;
} /* OP_gcscan1 end */

/**********************************************************************/
/*
                Func Name : OP_gcscan2
*/
/**********************************************************************/

/*
 * FUNCTION: OP_gcscan2
 *
 * PURPOSE:
 *   Implements the SCAN2 opcode for the garbage collector.
 *   This opcode performs the second-pass scan of a memory region
 *   during incremental garbage collection.
 *
 * PARAMETERS:
 *   None (uses global state)
 *
 * RETURNS:
 *   void (modifies TopOfStack and PC)
 *
 * ALGORITHM:
 *   1. Check if TopOfStack is a positive integer (memory address)
 *   2. Extract low 16 bits (LOLOC) to get memory address
 *   3. Call gcscan2() to perform second-pass scan
 *   4. If scan returns -1, set TopOfStack to NIL
 *   5. Otherwise, combine result with S_POSITIVE and set TopOfStack
 *   6. Advance PC by 1 byte
 *
 * SCAN2 OPERATION:
 *   The second-pass scan continues the garbage collection process
 *   after the first-pass scan (SCAN1). It processes objects
 *   that were marked during the first pass and performs
 *   additional collection work.
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - TopOfStack: Set to scan result or NIL
 *   - PC: Advanced by 1 byte
 *
 * CROSS-REFERENCE: See gcscandefs.h for gcscan2() implementation
 * CROSS-REFERENCE: See address.h for LOLOC macro
 * CROSS-REFERENCE: See OP_gcscan1() for first-pass scan
 */
void OP_gcscan2(void) {
  int scan;
#ifdef TRACE
  printPC();
  printf("TRACE: OP_gcscan2()\n");
#endif
  /*
   * Check if TopOfStack is a positive integer (memory address).
   * The SEGMASK extracts the segment bits, which should be S_POSITIVE.
   */
  if ((TopOfStack & SEGMASK) == S_POSITIVE) {
    /*
     * Extract low 16 bits (LOLOC) to get memory address.
     * Call gcscan2() to perform second-pass scan.
     * If scan returns -1, set TopOfStack to NIL.
     * Otherwise, combine result with S_POSITIVE and set TopOfStack.
     */
    scan = gcscan2(LOLOC(TopOfStack));
    TopOfStack = (scan == -1) ? NIL : scan | S_POSITIVE;
  }
  PC++;
} /* OP_gcscan2 end */
