/* =========================================================================
 * File: common.c
 * Purpose: Common utility functions and error handling for Maiko VM
 * Confidence Level: High (Stable, well-tested utility functions)
 * Testing: Tested with standard error handling and utility operations
 * Algorithm: Provides core utility functions used throughout the VM
 * Authors: Original Venue Corporation, Sybalsky et al.
 * Original Implementation Date: 1989-1995
 * Last Modified: 1999-01-03
 *
 * This file contains common utility functions used throughout the Maiko
 * VM, including error handling, debugging support, and system interface
 * functions. These are fundamental utility functions that are used
 * by many other modules in the system.
 *
 * Key Features:
 * - Error handling with URAID debugger integration
 * - Warning message handling
 * - Debugging support functions
 * - System interface for keyboard and display management
 * - URAID debugger integration
 *
 * Related Files:
 * - commondefs.h: Function prototypes for common utilities
 * - dbprint.h: Debug printing macros
 * - emlglob.h: Global variables
 * - kprintdefs.h: Kernel print functions
 * - lispemul.h: LispPTR and DLword types
 * - lspglob.h: Global variables and Lisp system integration
 * - uraiddefs.h: URAID debugger integration functions
 * - uraidextdefs.h: URAID debugger extended definitions
 * ========================================================================= */

#include "version.h"

#include <fcntl.h>      // for fcntl, F_GETFL, O_RDONLY, O_RDWR - file control
#include <setjmp.h>     // for setjmp, jmp_buf - non-local jumps
#include <stdio.h>      // for fflush, fprintf, printf, getchar, stderr - I/O functions
#include <stdlib.h>     // for exit - process termination
#include <string.h>     // for memset - memory operations
#include <sys/select.h> // for fd_set - file descriptor sets

#include "commondefs.h"   // for error, stab, warn - common utility prototypes
#include "dbprint.h"      // for DBPRINT - debug printing
#include "emlglob.h"      // for global variables
#include "kprintdefs.h"   // for print - kernel print function
#include "lispemul.h"     // for NIL, DLword, LispPTR - core VM types
#include "lspglob.h"      // for Lisp system integration and global variables
#include "uraiddefs.h"    // for device_after_raid, device_before_raid - URAID integration
#include "uraidextdefs.h" // for URMAXFXNUM, URaid_inputstring, URaid_FXarray - URAID data structures

/* =========================================================================
 * Function: stab
 * Purpose: Debugging stub function for breakpoint/tracing
 * Algorithm: Prints debugging message using DBPRINT
 * Notes: This is a simple debugging aid that can be called from anywhere
 * in the codebase to indicate program execution point
 * ========================================================================= */
void stab(void) { DBPRINT(("Now in stab\n")); }

/* =========================================================================
 * External Variables
 *
 * These variables are declared in other modules but used in this file.
 * They provide access to system resources for keyboard handling, display,
 * and low-level debugging.
 * ========================================================================= */
extern fd_set LispReadFds;              /* File descriptors for Lisp read operations */
extern int LispKbdFd;                   /* File descriptor for Lisp keyboard */
extern struct screen LispScreen;        /* Screen information structure */
extern int displaywidth, displayheight; /* Display dimensions */
extern DLword *DisplayRegion68k;        /* Display buffer pointer */
extern int FrameBufferFd;               /* Frame buffer file descriptor */
extern jmp_buf BT_jumpbuf;              /* Jump buffer for break handling */
extern jmp_buf SD_jumpbuf;              /* Jump buffer for stack debugging */
extern int BT_temp;                     /* Holds the continue-character the user typed */

/* Currently don't care about Ether re-initialization */
/* This is Medley-only functionality */

/* =========================================================================
 * Variable: Uraid_mess
 * Purpose: Message to display when entering URAID debugger
 * Type: LispPTR
 * Notes: Initialized to NIL, may be set by URAID operations
 * ========================================================================= */
LispPTR Uraid_mess = NIL;

/* =========================================================================
 * Function: error
 * Purpose: Last-ditch error handling function that enters URAID debugger
 * Parameters:
 *   cp - Error message to display
 * Returns: 0 if successfully returns from URAID
 * Algorithm:
 *   1. Attempts to enter URAID debugger
 *   2. Prints error message to stderr
 *   3. Initializes URAID state
 *   4. Enters URAID command loop
 *   5. Handles return from URAID or failure to return to Lisp
 * Error Handling:
 *   - If can't enter URAID, prints message and exits
 *   - If can't return to Lisp after URAID, asks user if they want to exit to UNIX
 * Side Effects: Enters URAID debugger which can affect VM state
 * ========================================================================= */
int error(const char *cp)
{
  char *ptr;

  /* Attempt to prepare for URAID debugger */
  if (device_before_raid() < 0)
  {
    (void)fprintf(stderr, "Can't Enter URAID.\n");
    exit(-1); /* Fatal error */
  }

  /* Set error message for URAID */
  URaid_errmess = cp;

  /* Print error message to standard error */
  (void)fprintf(stderr, "\n*Error* %s\n", cp);
  fflush(stdin);
  (void)fprintf(stderr, "Enter the URaid\n");

  /* Print URAID message if available */
  print(Uraid_mess);
  putchar('\n');

  /* Ensure all output is flushed */
  fflush(stdout);
  fflush(stderr);

  /* Initialize URAID state */
  URaid_currentFX = URMAXFXNUM + 1;
  memset(URaid_FXarray, 0, URMAXFXNUM * 4);

uraidloop:
  /* Set up jump buffers for exception handling */
  if (setjmp(BT_jumpbuf) == 1)
    goto uraidloop;
  if (setjmp(SD_jumpbuf) == 1)
    goto uraidloop;

  /* URAID command loop */
  for (;;)
  {
    uraid_commclear();
    BT_temp = 0; /* So we get the "more" option on screen-full */

    /* Read command from user with bounds checking */
    printf("\n< ");
    for (ptr = URaid_inputstring; ptr < URaid_inputstring + URMAXCOMM - 1; ptr++)
    {
      int c = getchar();
      if (c == '\n' || c == EOF)
      {
        *ptr = '\0';
        break;
      }
      *ptr = (char)c;
    }
    /* Ensure null termination even if buffer is full */
    URaid_inputstring[URMAXCOMM - 1] = '\0';

    /* Parse command */
    URaid_argnum = sscanf(URaid_inputstring, "%1s%s%s", URaid_comm, URaid_arg1, URaid_arg2);

    /* Execute URAID command */
    if (uraid_commands() == NIL)
      break;

    /* Ensure output is flushed */
    fflush(stdout);
    fflush(stderr);
  }

  /* Prepare to return to Lisp */
  /**TopOfStack = NIL; if error is called from subr TOS will be set NIL**/

  /* Attempt to return to Lisp */
  if (device_after_raid() < 0)
  {
    /* Can't return to Lisp, ask user if they want to exit */
    printf("Can't return to Lisp. Return to UNIX?");
    {
      int c;
      c = getchar();
      if ((c == 'Y') || (c == 'y'))
        exit(-1);
    }
    fflush(stdin);
    goto uraidloop; /* Return to URAID loop */
  }

  return (0);
}

/* =========================================================================
 * Function: warn
 * Purpose: Warning message handler
 * Parameters:
 *   s - Warning message to display
 * Algorithm: Prints warning message to standard output without stopping execution
 * Notes: This is a non-fatal error handler that allows execution to continue
 * after displaying a warning. Use when the situation is recoverable.
 * ========================================================================= */
void warn(const char *s)
{
  printf("\nWARN: %s \n", s);
}
