/* $Id: allocmds.c,v 1.4 1999/05/31 23:35:20 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-98 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/* FILE: allocmds.c - Memory Data Structure (MDS) Allocation and Management
 *
 * HIGH CONFIDENCE: This file implements the core memory management system
 * for Maiko. The MDS (Memory Data Structure) system tracks the type and
 * allocation status of every page in the virtual memory space.
 *
 * MDS ARCHITECTURE:
 * - Each 512-byte page has a corresponding MDS entry
 * - MDS entries track page type, allocation status, and GC information
 * - MDStypetbl is the master table containing all MDS entries
 * - Two MDS entries per DLword (16 bits each) for compact storage
 *
 * PAGE MANAGEMENT:
 * - Pages are allocated on demand by alloc_mdspage()
 * - Each page is 512 bytes (256 DLwords)
 * - Page numbers are used as indices into MDS tables
 * - MDS pattern bits encode type, GC flags, and allocation status
 *
 * CRITICAL IMPORTANCE:
 * - All memory allocation ultimately uses this system
 * - Garbage collector relies on MDS for object identification
 * - Type checking uses MDS for fast type lookup
 * - Memory protection and debugging use MDS information
 *
 * CROSS-REFERENCE: MDS bit patterns in lsptypes.h
 * CROSS-REFERENCE: Page allocation in storagedefs.h
 * CROSS-REFERENCE: GC scanning in gc*.c files
 *
 * AUTHOR: Takeshi Shimizu (August 18, 1987)
 */

#include "address.h"       // for LOLOC                                                                                                                                     
#include "adr68k.h"        // for LAddrFromNative, LPageFromNative, Addr68k_fr...                                                                                             
#include "allocmdsdefs.h"  // for alloc_mdspage, initmdspage                                                                                                                
#include "commondefs.h"    // for error                                                                                                                                     
#include "lispemul.h"      // for DLword, LispPTR, DLWORDSPER_PAGE, MDSINCRE...                                                                                             
#include "lispmap.h"       // for S_POSITIVE                                                                                                                                
#include "lspglob.h"       // for MDStypetbl                                                                                                                                
#include "lsptypes.h"      // for GETWORD, GetTypeNumber, TYPE_SMALLP                                                                                                       
#include "storagedefs.h"   // for newpage, checkfor_storagefull                                                                                                             

/************************************************************************/
/*									*/
/*			Make_MDSEntry					*/
/*									*/
/*	Fill in the MDS type-table entry for a new page (see		*/
/*	lsptypes.h for the meaning of the entry bits).			*/
/*									*/
/************************************************************************/
/*
 * HIGH CONFIDENCE: Create MDS (Memory Data Structure) table entry.
 * This is the fundamental operation for allocating pages in the Maiko VM.
 *
 * MDS ENTRY FORMAT:
 * - Each DLword contains two MDS entries (16 bits each)
 * - Page number >> 1 gives DLword index in MDStypetbl
 * - Pattern contains type, GC flags, and status bits
 * - Bits encode: page type (SMALLP, ARRAYP, etc), GC flags, allocation
 *
 * PAGE NUMBERING:
 * - Pages are numbered sequentially from 0
 * - Page 0 is reserved for system structures
 * - Page numbers >> 1 access actual table (2 entries per DLword)
 *
 * PARAMETERS:
 * - page: Virtual page number being allocated
 * - pattern: Bit pattern with type and allocation flags
 *
 * SIDE EFFECTS:
 * - Updates MDStypetbl entry for this page
 * - Uses GETWORD macro for proper memory access
 *
 * CROSS-REFERENCE: MDS bit definitions in lsptypes.h
 * CROSS-REFERENCE: Page size constants in lispemul.h (BYTESPER_PAGE = 512)
 * CROSS-REFERENCE: GC page tracking in gc*.c files
 *
 * NOTE: Author states "no case that variable named GCDISABLED is set to T"
 * This refers to conditional compilation for GC-aware page allocation
 */
static inline void Make_MDSentry(UNSIGNED page, DLword pattern) {
  GETWORD((DLword *)MDStypetbl + (page >> 1)) = (DLword)pattern;
}

/**********************************************************************/
/*
        Func name :	initmdspage

                Write the specified MDSTT entry with specified pattern.
                returns Top entry for free chain lisp address

                                Date :		December 24, 1986
                                Edited by :	Takeshi Shimizu
                                Changed :	Jun. 5 87 take

*/
/**********************************************************************/

LispPTR initmdspage(LispPTR *base, DLword size, LispPTR prev)
/* MDS page base */
/* object cell size you need (WORD) */
/* keeping top of previous MDS cell */

{
  int remain_size; /* (IREMAINDER WORDSPERPAGE SIZE) */
  short num_pages;
  int limit;
  int used; /* used space in MDS page */
  int i;

#ifdef TRACE2
  printf("TRACE: initmdspage()\n");
#endif

  remain_size = DLWORDSPER_PAGE % size;

  if ((remain_size != 0) && (remain_size < (size >> 1) && (size < DLWORDSPER_PAGE))) {
    num_pages = MDSINCREMENT / DLWORDSPER_PAGE; /* on 1121 maybe 2 */
    limit = DLWORDSPER_PAGE;
  } else {
    num_pages = 1;
    limit = MDSINCREMENT;
  }

  for (i = 0; i < num_pages; i++) {
    used = 0;
    while ((used += size) <= limit) {
      *base = prev;                /* write prev MDS address to the top of MDS page */
      prev = LAddrFromNative(base); /* exchanging pointers */
      base = (LispPTR *)((DLword *)base + size);
    } /* while end */

    base = (LispPTR *)((DLword *)base + remain_size);

  } /* for end */

  return (prev);
} /* initmdspage end */

/**********************************************************************/
/*
                Func name :	alloc_mdspage

                        This version works only for Maiko

                        Date :		January 13, 1987
                        Edited by :	Takeshi Shimizu
                        Changed :	3-Apr-87 (take)
                                        20-Aug-87(take) ifdef
                                        08-Oct-87(take) checkfull
                                        22-Dec-87(Take)

*/
/**********************************************************************/

LispPTR *alloc_mdspage(short int type) {
  LispPTR *ptr; /* points Top 32 bit of the MDS page */
  LispPTR next_page;

  /* Next_Array=(DLword *)NativeAligned2FromLAddr(((*Next_Array_word)& 0xffff ) << 8); */

  if (LOLOC(*MDS_free_page_word) != NIL) {
    ptr = (LispPTR *)NativeAligned4FromLPage(LOLOC(*MDS_free_page_word));

    if (((next_page = LOLOC(*ptr)) != 0) && (GetTypeNumber((*ptr)) != TYPE_SMALLP))
      error("alloc_mdspage: Bad Free Page Link");
    else {
      *MDS_free_page_word = S_POSITIVE | next_page;
    }
  } else {
    /* I guess Next_MDSpage is redundant */
    checkfor_storagefull(NIL);
#ifdef BIGVM
    Next_MDSpage = (DLword *)NativeAligned2FromLAddr(((*Next_MDSpage_word)) << 8);
#else
    Next_MDSpage = (DLword *)NativeAligned2FromLAddr(((*Next_MDSpage_word) & 0xffff) << 8);
#endif
    ptr = (LispPTR *)Next_MDSpage;       /* Get Pointer to First Page */
    Next_MDSpage -= DLWORDSPER_PAGE * 2; /* decrement MDS count */
#ifdef BIGVM
    *Next_MDSpage_word = LPageFromNative(Next_MDSpage);
#else
    *Next_MDSpage_word = S_POSITIVE | LPageFromNative(Next_MDSpage);
#endif
    newpage(newpage(LAddrFromNative(ptr)) + DLWORDSPER_PAGE);
  }

  Make_MDSentry(LPageFromNative(ptr), type);
  return (ptr);
} /* alloc_mdspage end */
