/* $Id: findkey.c,v 1.3 1999/05/31 23:35:28 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/* FILE: findkey.c - FINDKEY Opcode Implementation
 *
 * This file implements the FINDKEY opcode, which searches for a specific
 * key in the argument list of the current function. It handles both fast
 * and slow case scenarios for stack frame traversal.
 *
 * HIGH CONFIDENCE: The implementation directly follows the stack frame
 * conventions and uses standard pointer arithmetic for searching.
 *
 * PURPOSE:
 * - Implements N_OP_findkey() - the FINDKEY opcode for Medley
 * - Searches for a key in the current function's argument list
 * - Handles both fast case (regular stack frames) and slow case (extended frames)
 * - Returns position of key or NIL if not found
 *
 * OPERATION:
 * - FINDKEY takes a key (TOS) and byte offset from IVAR
 * - Determines the search range based on frame type (regular or extended)
 * - Scans IVAR for matching key
 * - Returns positive offset if found, NIL otherwise
 *
 * CROSS-REFERENCE: Stack frame structure in stack.h
 * CROSS-REFERENCE: FX register definitions in lspglob.h
 * CROSS-REFERENCE: Instruction dispatch in lpmain.c
 * CROSS-REFERENCE: Opcode definitions in opcode.h
 */

#include "version.h"

#include <stdio.h>
#include "lispemul.h"
#include "lispmap.h"
#include "emlglob.h"
#include "stack.h"
#include "lspglob.h"
#include "adr68k.h"
#include "testtooldefs.h"
#include "findkeydefs.h"

/***********************************************************************/
/*
                File Name :	findkey.c

                Desc	:

                                Date :		Mar. 29 88
                                Edited by :	Bob Krivacic
                Including :	N_OP_findkey


*/
/**********************************************************************/

LispPTR N_OP_findkey(LispPTR tos, int byte) {
  LispPTR *ptr;
  DLword *find_end;
  DLword arg_nth;

#ifdef TRACE
  printPC();
  printf("TRACE : N_OP_findkey \n");
#endif

  if (CURRENTFX->alink & 1) { /* slow case */
    find_end = NativeAligned2FromStackOffset(CURRENTFX->blink - 4);
  } else { /*  Fast cae */
    find_end = ((DLword *)CURRENTFX) - 2 - 4;
  }

  arg_nth = byte + 1;

  for (ptr = (LispPTR *)(IVar + ((byte * 2) - 2)); (UNSIGNED)find_end >= (UNSIGNED)ptr;
       ptr += 2, arg_nth += 2) {
    if (*ptr == tos) { /* KEY founded */
      return (S_POSITIVE | arg_nth);
    }
  } /* for end */

  /* No matched */

  return (NIL_PTR);

} /* end N_OP_findkey() */
