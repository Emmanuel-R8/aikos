/* $Id: kbdif.c,v 1.3 1999/05/31 23:35:35 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989, 1990, 1990, 1991, 1992, 1993, 1994, 1995 Venue.	*/
/*	    All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/* FILE: kbdif.c - Keyboard Interface Abstraction Layer
 *
 * HIGH CONFIDENCE: This file implements the keyboard interface abstraction
 * layer for Maiko. It provides a generic interface that can be implemented
 * by different keyboard backends (X11, SDL, etc.).
 *
 * KEYBOARD INTERFACE ARCHITECTURE:
 * - KbdInterface: Abstract interface for keyboard operations
 * - curkbd: Current keyboard interface instance
 * - currentkbd: Global pointer to active keyboard interface
 *
 * GENERIC KEYBOARD FUNCTIONS:
 * - make_kbd_instance(): Creates keyboard interface instance (empty implementation)
 *
 * INTERFACE STRUCTURE:
 * The KbdInterface structure (defined in devif.h) typically includes:
 * - Keyboard state management
 * - Key event handling
 * - Keyboard layout configuration
 * - Modifier key tracking
 *
 * PLATFORM-SPECIFIC IMPLEMENTATIONS:
 * - X11 backend: Implemented in xinit.c and xlspwin.c
 * - SDL backend: Implemented in sdl.c
 * - Other backends can be added by implementing KbdInterface
 *
 * CROSS-REFERENCE: Keyboard operations in kbdsubrs.c and keyevent.c
 * CROSS-REFERENCE: Keyboard initialization in initkbd.c
 * CROSS-REFERENCE: Keyboard interface definition in devif.h
 * CROSS-REFERENCE: Display interface in dspif.c
 */

#include "version.h"

#include "lispemul.h"
#include "dbprint.h"
#include "devif.h"
#include "dspifdefs.h"

KbdInterfaceRec curkbd;
KbdInterface currentkbd = &curkbd;

void make_kbd_instance(KbdInterface kbd) {}
