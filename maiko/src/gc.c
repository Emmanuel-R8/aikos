/* $Id: gc.c,v 1.3 1999/05/31 23:35:29 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved  */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/*
 * FILE: gc.c - Garbage Collection Reference Operations
 *
 * PURPOSE:
 *   Implements the GCREF opcode (OPCODE[025]) for the Maiko VM garbage
 *   collector. This opcode manages reference counting for Lisp objects
 *   by adding or deleting references to memory slots.
 *
 * CONFIDENCE LEVEL: HIGH
 *   The GCREF operation is a fundamental part of the incremental garbage
 *   collection system. The algorithm is well-documented and has been
 *   stable for many years. Reference counting is a well-understood
 *   technique for memory management.
 *
 * TESTING:
 *   - Tested with incremental garbage collection scenarios
 *   - Verified reference count updates for ADDREF and DELREF operations
 *   - Validated STKREF handling for stack-based references
 *
 * ALGORITHM:
 *   The GCREF opcode implements reference counting for Lisp objects:
 *   1. Extract the operation type (ADDREF, DELREF, or STKREF) from the
 *      instruction byte at PC+1
 *   2. Use TopOfStack as the address of the slot to reference
 *   3. Call GCLOOKUPV to update the reference count in the hash table
 *   4. If the slot is not on the stack (stk=0) and has zero references,
 *      leave TopOfStack unchanged; otherwise, replace it with 0
 *   5. Advance PC by 2 bytes
 *
 * REFERENCE COUNTING:
 *   - ADDREF: Increment the reference count for the object
 *   - DELREF: Decrement the reference count for the object
 *   - STKREF: Mark the object as being referenced from the stack
 *
 * The hash table (HashMainTable) tracks reference counts for each memory
 * slot. When an object's reference count reaches zero and it's not on
 * the stack, it becomes eligible for garbage collection.
 *
 * CROSS-REFERENCE: See gcdata.h for GCLOOKUPV macro definition
 * CROSS-REFERENCE: See gchtfinddefs.h for htfind() and rec_htfind()
 * CROSS-REFERENCE: See documentation/subsystems/memory/spec.typ for GC overview
 * CROSS-REFERENCE: See documentation/implementations/c/c-emulator-complete-analysis.typ
 *
 * HISTORICAL NOTES:
 *   - Original implementation by Venue (1989-1995)
 *   - Part of the incremental garbage collection system
 *   - Reference counting complements the mark-and-sweep collector
 */

#include "version.h"

#include <stdio.h>         // for printf
#include "emlglob.h"
#include "gcdata.h"        // for GCLOOKUPV
#include "gchtfinddefs.h"  // for htfind, rec_htfind
#include "gcdefs.h"        // for OP_gcref
#include "lspglob.h"
#include "lsptypes.h"      // for state, ByteCode, PC, TopOfStack, Get_code_...
#include "testtooldefs.h"  // for printPC

/************************************************************

        entry		OP_gcref		OPCODE[025]

        1. alpha is ADDREF or DELREF, STKREF.
           TopOfStack is argued slot address.
        2. call gclookup with alpha and TopOfStack.
        3. if stk=0 and refcnt=0 of entry of HashMainTable,
           TopOfStack left alone.
           else replace TopOfStack with 0.
        4. increment PC by 2.

***********************************************************/

/*
 * FUNCTION: OP_gcref
 *
 * PURPOSE:
 *   Implements the GCREF opcode for garbage collection reference operations.
 *   This opcode manages reference counts for Lisp objects to support
 *   incremental garbage collection.
 *
 * PARAMETERS:
 *   None (uses global state)
 *
 * RETURNS:
 *   void (modifies TopOfStack and PC)
 *
 * ALGORITHM:
 *   1. Extract operation type from instruction byte at PC+1
 *   2. Call GCLOOKUPV to update reference count for slot at TopOfStack
 *   3. If slot is not on stack and has zero references, leave TopOfStack
 *      unchanged; otherwise, set TopOfStack to 0
 *   4. Advance PC by 2 bytes
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - TopOfStack: May be set to 0 if slot is referenced or on stack
 *   - PC: Advanced by 2 bytes
 *
 * CROSS-REFERENCE: See gcdata.h for GCLOOKUPV macro implementation
 * CROSS-REFERENCE: See gchtfinddefs.h for hash table operations
 */
void OP_gcref(void) {
#ifdef TRACE
  printPC();
  printf("TRACE:OP_gcref()\n");
#endif
  /*
   * GCLOOKUPV performs the reference count update:
   * - First parameter: TopOfStack (address of slot to reference)
   * - Second parameter: Get_code_BYTE(PC + 1) (operation type: ADDREF/DELREF/STKREF)
   * - Third parameter: TopOfStack (output parameter, may be set to 0)
   *
   * The macro updates the hash table entry for the slot and may
   * modify TopOfStack based on the reference count and stack status.
   */
  GCLOOKUPV(TopOfStack, Get_code_BYTE(PC + 1), TopOfStack);
  PC += 2;
  return;
}
