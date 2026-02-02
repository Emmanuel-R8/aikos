/* FILE: mouseif.c - Mouse Interface Abstraction Layer
 *
 * HIGH CONFIDENCE: This file implements the mouse interface abstraction
 * layer for Maiko. It provides a generic interface that can be implemented
 * by different mouse backends (X11, SDL, DOS, etc.).
 *
 * MOUSE INTERFACE ARCHITECTURE:
 * - MouseInterface: Abstract interface for mouse operations
 * - curmouse: Current mouse interface instance
 * - currentmouse: Global pointer to active mouse interface
 *
 * GENERIC MOUSE FUNCTIONS:
 * - make_mouse_instance(): Creates mouse interface instance (empty implementation)
 *
 * INTERFACE STRUCTURE:
 * The MouseInterface structure (defined in devif.h) typically includes:
 * - Mouse state management
 * - Mouse event handling
 * - Coordinate tracking (x, y)
 * - Button state management
 * - Mouse motion and button click detection
 *
 * PLATFORM-SPECIFIC IMPLEMENTATIONS:
 * - X11 backend: Implemented in xinit.c and xlspwin.c
 * - SDL backend: Implemented in sdl.c
 * - DOS backend: Implemented in dosmouse.c
 * - Other backends can be added by implementing MouseInterface
 *
 * CROSS-REFERENCE: Mouse operations in mouseif.c and dosmouse.c
 * CROSS-REFERENCE: Mouse initialization in initkbd.c
 * CROSS-REFERENCE: Mouse interface definition in devif.h
 * CROSS-REFERENCE: Display interface in dspif.c
 */

/* $Id: mouseif.c,v 1.2 1999/01/03 02:07:26 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989, 1990, 1990, 1991, 1992, 1993, 1994, 1995 Venue.	*/
/*	    All Rights Reserved.					*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

#include "devif.h" // for MouseInterface, MouseInterfaceRec

MouseInterfaceRec curmouse;
MouseInterface currentmouse = &curmouse;

void make_mouse_instance(MouseInterface mouse) {}
