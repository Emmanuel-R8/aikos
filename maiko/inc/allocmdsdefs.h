/* FILE: allocmdsdefs.h - MDS (Memory Data Structure) Page Allocation
 *
 * This file defines the interface for MDS (Memory Data Structure) page
 * allocation functions. MDS pages are used for managing various data
 * structures in Lisp memory, including arrays and other composite objects.
 *
 * HIGH CONFIDENCE: These function prototypes are simple and well-documented,
 * providing the basic interface for MDS page management.
 *
 * FUNCTIONS:
 * - initmdspage(): Initialize an MDS page with specified size and previous page
 * - alloc_mdspage(): Allocate an MDS page of the specified type
 *
 * CROSS-REFERENCE: MDS page structure in storage.c
 * CROSS-REFERENCE: Memory management in allocmds.c
 * CROSS-REFERENCE: Lisp type definitions in lsptypes.h
 */

#ifndef ALLOCMDSDEFS_H
#define ALLOCMDSDEFS_H 1
#include "lispemul.h" /* for LispPTR, DLword */
LispPTR initmdspage(LispPTR *base, DLword size, LispPTR prev);
LispPTR *alloc_mdspage(short int type);
#endif
