/* $Id: asmbbt.c,v 1.3 1999/05/31 23:35:23 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*			File:	asmbbt.c				*/
/*									*/
/*	Dummy C-function "bitblt", used to compile the bitblt code	*/
/*	for hand-optimization.						*/
/*									*/
/*									*/
/*									*/
/*									*/
/************************************************************************/

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/* FILE: asmbbt.c - BITBLT Compilation Helper
 *
 * This file provides a dummy C function version of the BITBLT operation,
 * used as a template for generating hand-optimized assembly language
 * implementations. The actual BITBLT implementation is provided in
 * architecture-specific assembly files.
 *
 * HIGH CONFIDENCE: This is a simple compilation helper with no
 * runtime functionality, used only during build/optimization.
 *
 * PURPOSE:
 * - Provides a C language template for BITBLT
 * - Used to generate assembly intermediate for hand-optimization
 * - Enables compilation checking before assembly optimization
 *
 * DUMMY FUNCTION:
 * - bitblt(): Prototype function matching assembly implementation
 * - No actual implementation - replaced by assembly code
 *
 * ARCHITECTURE-SPECIFIC IMPLEMENTATIONS:
 * - bbt68k.s: MC68020 implementation
 * - bbtSPARC.s: SPARC implementation
 * - xbbt.c: X11 implementation
 *
 * CROSS-REFERENCE: BITBLT structure in bb.h
 * CROSS-REFERENCE: BITBLT operations in bitblt.c and blt.c
 * CROSS-REFERENCE: Assembly implementations in bbt68k.s and bbtSPARC.s
 */

#include "version.h"

#include "lispemul.h"
#include "bb.h"

void bitblt(DLword *srcbase, DLword *dstbase, int sx, int dx, int w, int h, int srcbpl, int dstbpl,
            int backwardflg, int src_comp, int op, int gray, int num_gray, int curr_gray_line) {
  new_bitblt_code;
}
