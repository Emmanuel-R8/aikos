#ifndef XKEYMAP_H
#define XKEYMAP_H 1

/* $Id: XKeymap.h,v 1.2 1999/01/03 02:05:48 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-92 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/************************************************************************/
/*									*/
/*	Generic X-keyboard map for Medley.  This table is used at	*/
/*	start-up time to create the table that maps X keycodes to	*/
/*	Medley key numbers.						*/
/*									*/
/*	This is done by asking X for the keycodes that correspond	*/
/*	to given KEYSYMs (X's machine-independent coding scheme),	*/
/*	and building the table.  This has one problem:  The mapping	*/
/*	for non-ASCII characters isn't standard among keyboards.  To	*/
/*	get as reasonable a map as possible, the table below contains	*/
/*	possibly several mappings for each Medley key number.   Since	*/
/*	not every keyboard has every key (e.g., Alt vs Meta), there	*/
/*	may also be several mappings for a single KEYSYM.		*/
/*									*/
/*	Here's how it works:  Each entry is tried in turn.  If the	*/
/*	Medley key number we'd be assigning is already assigned a	*/
/*	mapping, skip this entry.  If not, assign this mapping, and	*/
/*	set the "this-KEYSYM-used" flag.  If this is a new KEYSYM,	*/
/*	reset the flag before trying anything.  If the "used" flag	*/
/*	is set, skip until we find a new KEYSYM.			*/
/*									*/
/*	Constraints:							*/
/*		Put the better key-number assignment earlier.		*/
/*		Put the better KEYSYM assignment earlier.		*/
/*		All entries for a single KEYSYM -must- be adjacent	*/
/*		Final entry in the map has key number -1.		*/
/*									*/
/************************************************************************/
/*									*/
/*		      C H A N G E   H I S T O R Y			*/
/*									*/
/*	23 SEP 91 JDS:	Rearrange CUT, BS, and DEL keys to assure that	*/
/*			we get a BS key first, then a CUT key, then,	*/
/*			if there's a key left, a DEL (BW) key.		*/
/*									*/
/*	26 MAY 92 JDS:	Rearrange keys for PC kbd layout.		*/
/*									*/
/*									*/
/*									*/
/*									*/
/*									*/
/*									*/
/************************************************************************/

#endif
