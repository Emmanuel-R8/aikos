/* =========================================================================
 * File: car-cdr.c
 * Purpose: Implementation of CAR/CDR operations for Maiko VM
 * Confidence Level: High (Stable, well-tested)
 * Testing: Tested with standard CAR/CDR operations
 * Algorithm: Implements basic Lisp list operations using ConsCell structure
 * Authors: Original Venue Corporation, Naoyuki Mitani (1987), Sybalsky et al.
 * Original Implementation Date: 1987-04-24
 * Last Modified: 1999-05-31
 *
 * This file contains the C implementations of CAR/CDR operations for
 * the Maiko VM. It supports basic list manipulation using ConsCell structures.
 *
 * Key Features:
 * - CAR operation: Get first element of cons cell
 * - CDR operation: Get rest of cons cell
 * - RPLACA operation: Replace first element of cons cell
 * - RPLACD operation: Replace rest of cons cell
 * - Support for indirect CDR references and free cons cell management
 *
 * Related Files:
 * - address.h: POINTER_PAGEBASE macro for address calculation
 * - adr68k.h: NativeAligned4FromLAddr, NativeAligned4FromLPage macros
 * - car-cdrdefs.h: Function prototypes (N_OP_car, N_OP_cdr, N_OP_rplaca, N_OP_rplacd)
 * - cell.h: ConsCell structure, CDR_INDIRECT flag, freecons management
 * - commondefs.h: error handling function
 * - conspagedefs.h: cons page management functions
 * - emlglob.h: Global variables
 * - gcdata.h: GCLOOKUP, ADDREF, DELREF macros for garbage collection
 * - gchtfinddefs.h: Hash table functions (htfind, rec_htfind)
 * - lispemul.h: ConsCell, LispPTR, DLword, NIL_PTR types
 * - lspglob.h: ListpDTD - list processing dispatch table
 * - lsptypes.h: Listp and dtd functions for type checking
 * ========================================================================= */

#include "version.h"

#include "address.h"       // for POINTER_PAGEBASE - address calculation
#include "adr68k.h"        // for NativeAligned4FromLAddr, NativeAligned4FromLPage - address conversion
#include "car-cdrdefs.h"   // for N_OP_car, N_OP_cdr, N_OP_rplaca, N_OP_rplacd - prototypes
#include "cell.h"          // for freecons, conspage, FREECONS, CDR_INDIRECT - cons cell management
#include "commondefs.h"    // for error - error handling
#include "conspagedefs.h"  // for next_conspage - cons page management
#include "emlglob.h"       // for global variables
#include "gcdata.h"        // for GCLOOKUP, ADDREF, DELREF - garbage collection macros
#include "gchtfinddefs.h"  // for htfind, rec_htfind - hash table functions
#include "lispemul.h"      // for ConsCell, LispPTR, DLword, NIL_PTR, state - VM core types
#include "lspglob.h"       // for ListpDTD - list processing dispatch table
#include "lsptypes.h"      // for Listp, dtd - type checking functions

/* =========================================================================
 * Function: car
 * Purpose: Returns CAR of Lisp datum (meant to be called from C)
 * Parameters:
 *   datum - Lisp datum to extract CAR from - LispPTR (must be word offset)
 * Returns: CAR of the datum - LispPTR
 * Algorithm:
 *   1. Convert Lisp pointer to native address using NativeAligned4FromLAddr
 *   2. Handle different types of Lisp data:
 *      - Cons cells (Listp): Return car_field value, handling CDR_INDIRECT case
 *      - NIL_PTR: Return NIL_PTR
 *      - ATOM_T: Return ATOM_T
 *      - Literal atoms (LITATOM): Return NIL
 *   3. Error handling for non-list arguments
 * Error Handling: Calls error() if argument is not a list and not in special cases
 * ========================================================================= */
LispPTR car(LispPTR datum)
/* datum must be LISP pointer(word offset) */
{
  ConsCell *datum68k;
  ConsCell *temp;

  /* Convert Lisp pointer to native address */
  datum68k = (ConsCell *)(NativeAligned4FromLAddr(datum));
  
  if (Listp(datum)) {
    /* Handle cons cells */
    if (datum68k->cdr_code == CDR_INDIRECT) {
      /* CDR_INDIRECT: car_field points to another cons cell */
      temp = (ConsCell *)NativeAligned4FromLAddr(datum68k->car_field);
      return ((LispPTR)temp->car_field);
    } else {
      /* Normal case: directly return car_field */
      return ((LispPTR)datum68k->car_field);
    }
  }
  else if (datum == NIL_PTR) {
    /* NIL: CAR of NIL is NIL */
    return ((LispPTR)NIL_PTR);
  }
  else {
    /* Handle non-list special cases */
    if (datum == ATOM_T) {
      /* T: CAR of T is T */
      return (ATOM_T);
    }
    else if ((datum & SEGMASK) == 0) { /* LITATOM - literal atom */
      /** We assume CAR/CDRERR is CDR ***/
      return (NIL);
    }
    else {
      /* Invalid argument: not a list */
      error("car : ARG not list");
      return (NIL); /* NOT REACHED */
    }
  }
} /* end of car */

/* =========================================================================
 * Function: cdr
 * Purpose: Returns CDR of Lisp datum (meant to be called from C)
 * Parameters:
 *   datum - Lisp datum to extract CDR from - LispPTR (must be word offset)
 * Returns: CDR of the datum - LispPTR
 * Algorithm:
 *   1. Handle NIL case directly
 *   2. Validate argument is a list
 *   3. Convert Lisp pointer to native address using NativeAligned4FromLAddr
 *   4. Extract and decode cdr_code from ConsCell:
 *      - CDR_NIL: Return NIL_PTR
 *      - CDR_ONPAGE: Calculate and return next cons cell address
 *      - CDR_INDIRECT: Recursively follow indirect reference
 *   5. Error handling for non-list arguments
 * Error Handling: Calls error() if argument is not a list
 * CDR Coding:
 *   - NEWCDRCODING: Uses 3-bit offset within page (supports 8 cons cells per page)
 *   - Traditional: Uses 7-bit offset within page (supports 128 cons cells per page)
 * ========================================================================= */
LispPTR cdr(LispPTR datum)
/* datum must be LISP pointer(word offset) */
{
  ConsCell *datum68k;
  DLword cdr_code;
  ConsCell *temp;

  /* Handle NIL case directly */
  if (datum == NIL_PTR) return (NIL_PTR);
  
  /* Validate argument is a list */
  if (!Listp(datum)) error("cdr : ARG not list");

  /* Convert Lisp pointer to native address */
  datum68k = (ConsCell *)(NativeAligned4FromLAddr(datum));
  cdr_code = datum68k->cdr_code;

  /* Decode cdr_code */
  if (cdr_code == CDR_NIL) {
    /* CDR is NIL */
    return (NIL_PTR);
  }
  
  if ((cdr_code & CDR_ONPAGE) != 0) { /* cdr-samepage - next cons on same page */
#ifdef NEWCDRCODING
    /* NEWCDRCODING: 3-bit offset << 1 for word address */
    return (datum + ((cdr_code & 7) << 1));
#else
    /* Traditional: 7-bit offset << 1 for word address */
    return (POINTER_PAGEBASE(datum) + ((cdr_code & 127) << 1));
#endif                                 /* NEWCDRCODING */
  }
  
  if (cdr_code == CDR_INDIRECT) { /* cdr-indirect - indirect reference */
    return (cdr((LispPTR)(datum68k->car_field)));
  }
  
  /* cdr isn't a CONS, but is stored on this page. */
#ifdef NEWCDRCODING
  temp = (ConsCell *)(NativeAligned4FromLAddr(datum + (cdr_code << 1)));
#else
  temp = (ConsCell *)(NativeAligned4FromLAddr(POINTER_PAGEBASE(datum) + (cdr_code << 1)));
#endif /* NEWCDRCODING */
  return ((LispPTR)temp->car_field);
} /* end of cdr */

/**********************************************************************/
/*
                Func name :	rplaca

                                Called from C program.

                                        Date :		Apr 15, 1987
                                        Edited by :	Naoyuki Mitani
*/
/**********************************************************************/
/**
 * Replace car of x with y
 *
 * @param x [in,out] LispPTR to object in which car will be replaced.
 * @param y [in] LispPTR to object that will become new car of x.
 * @return x, modified, or NIL if x is not a list.
 */
LispPTR rplaca(LispPTR x, LispPTR y)
{
  ConsCell *x_68k;
  ConsCell *temp;

#ifdef TRACE2
  printf("TRACE: rplaca()\n");
#endif

  if (Listp(x) == NIL) { /* arg isn't a CONS cell, might be NIL */
    if (x == NIL_PTR) {
      if (y != NIL_PTR)
        error("Attempt to RPLACA NIL");
      else
        return (NIL_PTR);
    } else
      error("ARG not List");
      return (NIL_PTR); /* NOT REACHED */
  }

  else {
    x_68k = (ConsCell *)NativeAligned4FromLAddr(x);

    GCLOOKUP(car(x), DELREF); /* set up reference count */
    GCLOOKUP(y, ADDREF);

    if (x_68k->cdr_code == CDR_INDIRECT) {
      temp = (ConsCell *)NativeAligned4FromLAddr((LispPTR)x_68k->car_field);
      temp->car_field = y;
    } else
      x_68k->car_field = y;

    return (x);
  }
} /* end of rplaca */

/**********************************************************************/
/*
                Func name :	rplacd

                                Called from C program.

                                        Date :		Apr 16, 1987
                                        Edited by :	Naoyuki Mitani
*/
/**********************************************************************/
#ifdef NEWCDRCODING
static ConsCell *find_cdrable_pair(LispPTR carpart, LispPTR cdrpart); /* below... */
static ConsCell *find_close_cell(struct conspage *page, LispPTR oldcell);
#endif
/**
 * Replace cdr of x with y
 *
 * @param x [in,out] LispPTR to object in which cdr will be replaced.
 * @param y [in] LispPTR to object that will become new cdr of x.
 * @return x, modified, or errors if x is not a list.
 */

LispPTR rplacd(LispPTR x, LispPTR y)
{
  ConsCell *x_68k;
  ConsCell *temp68k;
  ConsCell *cdr_cell68k;
  LispPTR cdr_cell;
  LispPTR rp_page;
  DLword cdr_code;
  struct conspage *cons68k;

  if (Listp(x) == NIL) {
    if (x == NIL_PTR) {
      if (y != NIL_PTR)
        error("Attempt to RPLACD NIL");
      else
        return (NIL_PTR);
    } else
      error("ARG not List");
  }

  else {
    x_68k = (ConsCell *)NativeAligned4FromLAddr(x);

    GCLOOKUP(cdr(x), DELREF); /* set up reference count */
    GCLOOKUP(y, ADDREF);

    cdr_code = x_68k->cdr_code;

    if (cdr_code == CDR_INDIRECT) {
      /* cdr-indirect */

      rp_page = (LispPTR)x_68k->car_field;
      temp68k = (ConsCell *)NativeAligned4FromLAddr(rp_page);
#ifdef NEWCDRCODING
      cdr_cell = (rp_page) + (temp68k->cdr_code << 1);
#else
      cdr_cell = POINTER_PAGEBASE(rp_page) + (temp68k->cdr_code << 1);
#endif /* NEWCDRCODING */

      cdr_cell68k = (ConsCell *)NativeAligned4FromLAddr(cdr_cell);
      *(LispPTR *)cdr_cell68k = y & POINTERMASK; /* cdr_code is set to 0 */
    } else if (cdr_code <= CDR_MAXINDIRECT) {
/* cdr-differentpage */
#ifdef NEWCDRCODING
      cdr_cell = x + (cdr_code << 1);
#else
      cdr_cell = POINTER_PAGEBASE(x) + (cdr_code << 1);
#endif /* NEWCDRCODING */
      cdr_cell68k = (ConsCell *)NativeAligned4FromLAddr(cdr_cell);
      *(LispPTR *)cdr_cell68k = y & POINTERMASK; /* cdr_code is set to 0 */

    } else if (y == NIL_PTR)
      /* cdr-samepage & y is nil */
      x_68k->cdr_code = CDR_NIL;
#ifdef NEWCDRCODING
    else if (((rp_page = POINTER_PAGEBASE(x)) == POINTER_PAGEBASE(y)) && (y > x) && (y <= (x + 14)))
      /* cdr-samepage & x and y are on same page */
      x_68k->cdr_code = CDR_ONPAGE + ((y - x) >> 1);
#else
    else if ((rp_page = POINTER_PAGEBASE(x)) == POINTER_PAGEBASE(y))
      /* cdr-samepage & x and y are on same page */
      x_68k->cdr_code = CDR_ONPAGE + ((y & 0xff) >> 1);
#endif /* NEWCDRCODING */
    else {
      /* cdr-samepage & x and y are on different page */

      cons68k = (struct conspage *)(NativeAligned4FromLAddr(rp_page));
#ifdef NEWCDRCODING
      if ((cons68k->count > 0) && (cdr_cell68k = find_close_cell(cons68k, x))) {
        /* at least one free-cell on x's conspage */
        /* AND it's within CDR-code range of x. */

        *(LispPTR *)cdr_cell68k = y & POINTERMASK; /* cdr_code set to 0 */

        x_68k->cdr_code = (LAddrFromNative(cdr_cell68k) - x) >> 1;
      }
#else
      if (cons68k->count > 0) {
        /* at least one free-cell on x's conspage */
        cdr_cell68k = GetNewCell_68k(cons68k);
        cons68k->count--;
        cons68k->next_cell = ((freecons *)cdr_cell68k)->next_free;

        *(LispPTR *)cdr_cell68k = y & POINTERMASK; /* cdr_code set to 0 */

        x_68k->cdr_code = (LAddrFromNative(cdr_cell68k) - rp_page) >> 1;
      }
#endif /* NEWCDRCODING */
      else {
/* no more free-cell on x's conspage */
#ifdef NEWCDRCODING
        temp68k = (ConsCell *)find_cdrable_pair(x, y);
        temp68k->car_field = x_68k->car_field;
        x_68k->car_field = LAddrFromNative(temp68k);
        x_68k->cdr_code = CDR_INDIRECT;
#else
        cons68k = next_conspage();

        cdr_cell68k = GetNewCell_68k(cons68k);
        cons68k->next_cell = ((freecons *)cdr_cell68k)->next_free;
        temp68k = GetNewCell_68k(cons68k);
        cons68k->next_cell = ((freecons *)temp68k)->next_free;

        cons68k->count -= 2;

        *(LispPTR *)cdr_cell68k = y & POINTERMASK; /* cdr_code set to 0 */

        temp68k->car_field = x_68k->car_field;
        x_68k->car_field = LAddrFromNative(temp68k);

        temp68k->cdr_code = (LAddrFromNative(cdr_cell68k) & 0xff) >> 1;

        x_68k->cdr_code = CDR_INDIRECT;
#endif /* NEWCDRCODING */
      }
    }
  }
  return (x);

} /* end of rplacd */

/**********************************************************************/
/*
                Func name :	N_OP_car

                                car management

                                        Date :		March 21, 1988
                                        Edited by :	Robert Krivacic

*/
/**********************************************************************/

LispPTR N_OP_car(LispPTR tos) {
  ConsCell *datum68k;
  ConsCell *temp;

  datum68k = (ConsCell *)(NativeAligned4FromLAddr(tos));
  if (Listp(tos)) {
    if (datum68k->cdr_code == CDR_INDIRECT) {
      temp = (ConsCell *)NativeAligned4FromLAddr(datum68k->car_field);
      return ((LispPTR)temp->car_field);
    } else
      return ((LispPTR)datum68k->car_field);
  } else if (tos == NIL_PTR)
    return (tos);
  else if (tos == ATOM_T)
    return (tos);
  else {
    ERROR_EXIT(tos);
  }
} /* end of N_OP_car */

/**********************************************************************/
/*
                Func name :	N_OP_cdr

                                cdr management

                                        Date :		March 21, 1988
                                        Edited by :	Robert Krivacic
*/
/**********************************************************************/

LispPTR N_OP_cdr(LispPTR tos) {
  ConsCell *datum68k;
  DLword cdr_code;

  if (tos == NIL_PTR) return (tos);
  if (!Listp(tos)) {
      ERROR_EXIT(tos);
  }

  datum68k = (ConsCell *)(NativeAligned4FromLAddr(tos));
  cdr_code = datum68k->cdr_code;

    if (cdr_code == CDR_NIL) return (NIL_PTR); /* cdr-nil */
    if (cdr_code > CDR_ONPAGE) /* cdr-samepage */
#ifdef NEWCDRCODING
      return (tos + ((cdr_code & 7) << 1));
#else
      return (POINTER_PAGEBASE(tos) + ((cdr_code & 127) << 1));
#endif                                 /*NEWCDRCODING */
    if (cdr_code == CDR_INDIRECT) /* cdr-indirect */
      return (cdr((LispPTR)(datum68k->car_field)));
    /* cdr-differentpage */
#ifdef NEWCDRCODING
    return ((LispPTR)((ConsCell *)(NativeAligned4FromLAddr(tos + (cdr_code << 1))))->car_field);
#else
    return ((LispPTR)((ConsCell *)(NativeAligned4FromLAddr(POINTER_PAGEBASE(tos) + (cdr_code << 1))))->car_field);
#endif /*NEWCDRCODING */
} /* end of N_OP_cdr */

/**********************************************************************/
/*

                Func name :	N_OP_rplaca

                                rplaca management

                                        Date :		March 21, 1988
                                        Edited by :	Robert Krivacic
*/
/**********************************************************************/

LispPTR N_OP_rplaca(LispPTR tosm1, LispPTR tos) {
  ConsCell *x_68k;
  ConsCell *temp;

  if (Listp(tosm1) == NIL) {
    if (tosm1 == NIL_PTR) {
      if (tos != NIL_PTR)
        ERROR_EXIT(tos);
      else
        return (tosm1);
    } else
      ERROR_EXIT(tos);
  }

  else {
    x_68k = (ConsCell *)NativeAligned4FromLAddr(tosm1);

    GCLOOKUP(car(tosm1), DELREF); /* set up reference count */
    GCLOOKUP(tos, ADDREF);

    if (x_68k->cdr_code == CDR_INDIRECT) {
      temp = (ConsCell *)NativeAligned4FromLAddr((LispPTR)x_68k->car_field);
      temp->car_field = tos;
    } else
      x_68k->car_field = tos;

    return (tosm1);
  }
} /* end of N_OP_rplaca */

/**********************************************************************/
/*
                Func name :	N_OP_rplacd

                                rplacd management

                                        Date :		March 21, 1988
                                        Edited by :	Robert Krivacic


*/
/**********************************************************************/

LispPTR N_OP_rplacd(LispPTR tosm1, LispPTR tos) {
  if (Listp(tosm1) == NIL) {
    if (tosm1 == NIL_PTR) {
      if (tos != NIL_PTR)
        ERROR_EXIT(tos);
      else
        return (tosm1);
    } else
      ERROR_EXIT(tos);
  }

  else
    rplacd(tosm1, tos);

  return (tosm1);

} /* end of N_OP_rplacd */

/************************************************************************/
/*									*/
/*	        f i n d _ c l o s e _ p r i o r _ c e l l		*/
/*									*/
/*	Given the real address of a CONS page and an existing cell	*/
/*	on that page, return another cell that is close enough to	*/
/*	that the existing cell can be its CDR (i.e. up to 7 cells	*/
/*	earlier.  If no such cell exists, return 0.			*/
/*									*/
/*	If a cell is found, it is taken off the free chain before	*/
/*	being returned.							*/
/*									*/
/************************************************************************/

ConsCell *find_close_prior_cell(struct conspage *page, LispPTR oldcell) {
  unsigned oldoffset = oldcell & 0xFF;
  unsigned offset = page->next_cell;
  unsigned prior = 0;
  unsigned noffset;
  ConsCell *cell;

  while (offset) {
    if ((offset < oldoffset) && (offset >= (oldoffset - 14))) {
      noffset = FREECONS(page, offset)->next_free;
      while ((noffset > offset) && (noffset < oldoffset)) {
        prior = offset;
        offset = noffset;
        noffset = FREECONS(page, offset)->next_free;
      }
      cell = (ConsCell *)((DLword *)page + offset);
      if (prior)
        FREECONS(page, prior)->next_free = FREECONS(page, offset)->next_free;
      else
        page->next_cell = FREECONS(page, offset)->next_free;
      page->count -= 1;
      cell->cdr_code = CDR_ONPAGE | ((oldoffset - offset) >> 1);
      if (254 < (offset + ((cell->cdr_code & 7) << 1))) error("in fcpc, page overflow.");
      return (cell);
    }

    prior = offset;
    offset = ((freecons *)((DLword *)page + offset))->next_free;
  }
  return ((ConsCell *)0); /* No cell close enough */
}

#ifdef NEWCDRCODING
/************************************************************************/
/*									*/
/*			f i n d _ c l o s e _ c e l l			*/
/*									*/
/*	Given the real address of a CONS page and an existing cell	*/
/*	on that page, return another cell that is close enough to	*/
/*	be used as the CDR of the existing cell (i.e., within 7		*/
/*	cells.  If no such cell exists, return 0.			*/
/*									*/
/*	If a cell is found, it is taken off the free chain before	*/
/*	being returned.							*/
/*									*/
/************************************************************************/

static ConsCell *find_close_cell(struct conspage *page, LispPTR oldcell) {
  unsigned oldoffset = oldcell & 0xFF;
  unsigned offset = page->next_cell;
  unsigned prior = 0;

  while (offset) {
    if ((offset > oldoffset) && (offset <= (oldoffset + 14))) {
      if (prior)
        ((freecons *)((DLword *)page + prior))->next_free =
            ((freecons *)((DLword *)page + offset))->next_free;
      else
        page->next_cell = ((freecons *)((DLword *)page + offset))->next_free;
      page->count -= 1;
      return (ConsCell *)((DLword *)page + offset);
    }

    prior = offset;
    offset = ((freecons *)((DLword *)page + offset))->next_free;
  }
  return ((ConsCell *)0); /* No cell close enough */
}

/************************************************************************/
/*									*/
/*		f i n d _ c d r p a i r _ i n _ p a g e			*/
/*									*/
/*									*/
/*									*/
/************************************************************************/

static ConsCell *find_cdrpair_in_page(struct conspage *pg, LispPTR carpart, LispPTR cdrpart) {
  unsigned offset, prior, priorprior, nprior, poffset, noffset;

  prior = priorprior = nprior = 0;

  if (pg->count < 2) return (ConsCell *)0;

  offset = pg->next_cell;

  while (offset) {
    if (prior && (offset < prior) && (prior <= offset + 14)) {
      ConsCell *carcell, *cdrcell;

      poffset = offset;
      noffset = FREECONS(pg, offset)->next_free;
      while ((noffset > offset) && (noffset < prior)) {
        nprior = offset;
        poffset = prior;
        offset = noffset;
        noffset = FREECONS(pg, offset)->next_free;
      }

      carcell = (ConsCell *)(((DLword *)pg) + offset);
      cdrcell = (ConsCell *)(((DLword *)pg) + prior);
      if (priorprior)
        FREECONS(pg, priorprior)->next_free = FREECONS(pg, poffset)->next_free;
      else
        pg->next_cell = FREECONS(pg, poffset)->next_free;

      if (nprior) FREECONS(pg, nprior)->next_free = FREECONS(pg, offset)->next_free;

      pg->count -= 2;

      *(LispPTR *)carcell = carpart;
      *(LispPTR *)cdrcell = cdrpart;

      carcell->cdr_code = (cdrcell - carcell);
      return (carcell);
    } else if (prior && (offset > prior) && (offset <= prior + 14)) {
      ConsCell *carcell, *cdrcell;

      carcell = (ConsCell *)(((DLword *)pg) + prior);
      cdrcell = (ConsCell *)(((DLword *)pg) + offset);
      if (priorprior)
        FREECONS(pg, priorprior)->next_free = ((freecons *)cdrcell)->next_free;
      else
        pg->next_cell = ((freecons *)cdrcell)->next_free;

      pg->count -= 2;

      *(LispPTR *)carcell = carpart;
      *(LispPTR *)cdrcell = cdrpart;

      carcell->cdr_code = (cdrcell - carcell);
      return (carcell);
    }
    priorprior = prior;
    prior = offset;
    offset = FREECONS(pg, offset)->next_free;
  }

  return (0); /* found no entries in this page, so return failure code */
}

/************************************************************************/
/*									*/
/*		f i n d _ c d r a b l e _ p a i r			*/
/*									*/
/*									*/
/*									*/
/************************************************************************/

static ConsCell *find_cdrable_pair(LispPTR carpart, LispPTR cdrpart) {
  unsigned pgno;
  struct conspage *pg;
  ConsCell *cell;

  for (pg = (struct conspage *)NativeAligned4FromLPage(pgno = ListpDTD->dtd_nextpage); pgno;
       pg = (struct conspage *)NativeAligned4FromLPage(pgno = pg->next_page)) {
    if ((cell = find_cdrpair_in_page(pg, carpart, cdrpart))) return (cell);
  }

  return (find_cdrpair_in_page(next_conspage(), carpart, cdrpart));
}

#endif
