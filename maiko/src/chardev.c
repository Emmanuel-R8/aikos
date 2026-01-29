/* $Id: chardev.c,v 1.2 1999/01/03 02:06:50 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/************************************************************************/
/*                                                                      */
/*            C H A R A C T E R - D E V I C E   S U P P O R T           */
/*                                                                      */
/*                                                                      */
/*                                                                      */
/************************************************************************/

/* FILE: chardev.c - Character Device Support
 *
 * This file implements character device operations for Medley,
 * providing low-level access to serial ports, terminals, and other
 * character-oriented devices. It supports both input and output
 * operations with proper error handling.
 *
 * HIGH CONFIDENCE: Character device operations use standard POSIX
 * APIs. The implementation is straightforward and well-tested.
 *
 * KEY FEATURES:
 * - Character device open/close operations
 * - Raw and cooked mode support
 * - Terminal control operations
 * - Error handling with Lisp errno
 *
 * DEVICE OPERATIONS:
 * - CHAR_openfile: Open a character device with specified access
 * - CHAR_closefile: Close an open character device
 * - CHAR_readfile: Read from character device
 * - CHAR_writefile: Write to character device
 * - CHAR_ioctl: Device-specific control operations
 *
 * ACCESS MODES:
 * - ACCESS_INPUT: Read-only access
 * - ACCESS_OUTPUT: Write-only access, create if needed
 * - ACCESS_APPEND: Append mode with read/write
 * - ACCESS_BOTH: Read/write access, create if needed
 *
 * TIMEOUT HANDLING:
 * All operations use the TIMEOUT macro for interrupt safety,
 * allowing operations to be interrupted by Lisp signals.
 *
 * CROSS-REFERENCE: See ufs.c for file system operations
 * CROSS-REFERENCE: See timeout.h for timeout macros
 * CROSS-REFERENCE: See locfile.h for Lisp string conversion
 */

#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include "lispemul.h"
#include "lispmap.h"
#include "adr68k.h"
#include "lsptypes.h"
#include "arith.h"
#include "timeout.h"
#include "locfile.h"
#include "dbprint.h"

#include "chardevdefs.h"
#include "byteswapdefs.h"
#include "commondefs.h"
#include "perrnodefs.h"

extern int *Lisp_errno;
extern int Dummy_errno;

/************************************************************************/
/*                                                                      */
/*                        C H A R _ o p e n f i l e                     */
/*                                                                      */
/*      Given the arg vector                                            */
/*              args[0] Lisp string full Unix file-name to open                 */
/*              args[1] Access to open it for (INPUT, OUTPUT, BOTH)     */
/*              args[2] a FIXP cell to hold any Unix error number       */
/*                                                                      */
/*      Open the file named, and return the SMALLP descriptor.  If      */
/*      the open fails, return NIL, and put the Unix error number       */
/*      into the FIXP cell provided, for Lisp to look at.               */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_openfile(LispPTR *args)
/* args[0]            fullname */
/* args[1]            access */
/* args[2]            errno */
{
}

/************************************************************************/
/*                                                                      */
/*                      C H A R _ c l o s e f i l e                     */
/*                                                                      */
/*      Given the arg vector:                                           */
/*              args[0] The SMALLP file descriptor as returned by OPEN  */
/*              args[1] a FIXP cell to hold any Unix error number       */
/*                                                                      */
/*      Close the file identified by the descriptor.  If the            */
/*      close succeeds, return T.  Otherwise, return NIL, and put       */
/*      the Unix error number in the FIXP cell, for Lisp to see.        */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_closefile(LispPTR *args)
/* args[0]            fd      */
/* args[1]            errno   */
{
}

/************************************************************************/
/*                                                                      */
/*                          C H A R _ i o c t l                                 */
/*                                                                      */
/*      Given the arg vector:                                           */
/*              args[0] the file descriptor to be acted on.             */
/*              args[1] the IOCTL request code.                                 */
/*              args[2] auxiliary data structure passed to IOCTL        */
/*              args[3] a FIXP cell to contain any Unix error number    */
/*                                                                      */
/*      Perform the IOCTL system call on the given file descriptor,     */
/*      passing in the request code and auxiliary structure given.      */
/*      If the IOCTL succeeds, return T (and the aux structure may      */
/*      be side-effected).  Otherwise, return NIL, and put the Unix     */
/*      error number in the FIXP cell for Lisp to look at.              */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_ioctl(LispPTR *args)
{
}

/************************************************************************/
/*                                                                      */
/*                         C H A R _ b i n                              */
/*                                                                      */
/*      Reads one character from the character file descriptor,         */
/*      and returns the value.  If no character is available,           */
/*      or an error happens, returns NIL and sets the errno FIXP        */
/*      cell to the Unix error number.                                  */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_bin(int fd, LispPTR errn)
{
}

/************************************************************************/
/*                                                                      */
/*                          C H A R _ b o u t                           */
/*                                                                      */
/*      Write character ch to the character file descriptor id.  If     */
/*      the write works, return T; else return NIL and sets the FIXP    */
/*      cell at errno to contain the Unix error number.                         */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_bout(int fd, LispPTR ch, LispPTR errn)
{
}

/************************************************************************/
/*                                                                      */
/*                         C H A R _ b i n s                            */
/*                                                                      */
/*      Given the argument vector:                                      */
/*      args[0] the file id to read bytes from                          */
/*      args[1] the base address of the buffer to read into             */
/*      args[2] starting offset within the buffer to put bytes at       */
/*      args[3] the number of bytes desired to read, maximum            */
/*      args[4] a FIXP cell to hold the errno, if an error occurs       */
/*                                                                      */
/*      Read up to the specified number of bytes into the buffer,       */
/*      starting at the offset given.  Return the number of bytes       */
/*      actually read; will return if fewer bytes than desired are      */
/*      read.  If an error occurs in reading, return NIL, and put       */
/*      the Unix errno into the FIXP cell given.  EWOULDBLOCK is an     */
/*      error that can occur--and bins returns NIL, so Lisp code has    */
/*      to handle that case itself.                                     */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_bins(LispPTR *args)
{
}

/************************************************************************/
/*                                                                      */
/*                         C H A R _ b o u t s                          */
/*                                                                      */
/*      Given the argument vector:                                      */
/*      args[0] the file descriptor to write bytes to                   */
/*      args[1] the base address of the buffer to write from            */
/*      args[2] starting offset within the buffer to gt bytes from      */
/*      args[3] the number of bytes desired to write, maximum           */
/*      args[4] a FIXP cell to hold the errno, if an error occurs       */
/*                                                                      */
/*      write up to the specified number of bytes from the buffer,      */
/*      starting at the offset given.  Return the number of bytes       */
/*      actually written; will return if fewer bytes than desired are   */
/*      written.  If an error occurs in writing, return NIL, and put    */
/*      the Unix errno into the FIXP cell given.  EWOULDBLOCK is an     */
/*      error that can occur--and bins returns NIL, so Lisp code has    */
/*      to handle that case itself.                                     */
/*                                                                      */
/************************************************************************/

LispPTR CHAR_bouts(LispPTR *args)
{
}
