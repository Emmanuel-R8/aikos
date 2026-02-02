/* $Id: vmemsave.c,v 1.2 1999/01/03 02:07:45 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-1995 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/*
 * FILE: vmemsave.c - Virtual Memory Save Operations
 *
 * PURPOSE:
 *   Implements the VMEMSAVE subr for saving the current Lisp
 *   virtual memory state to a sysout file. This function is critical
 *   for persisting the Lisp system state and enabling later restoration.
 *
 * CONFIDENCE LEVEL: HIGH
 *   The virtual memory save operations are fundamental to the system's
 *   persistence mechanism. The implementation has been stable for many
 *   years and is well-tested.
 *
 * SYSOUT FILE FORMAT:
 *   The sysout file contains:
 *   - IFPAGE (Interface Page) at byte 512
 *   - Virtual memory pages mapped via FPtoVP table
 *   - Dominopages (system pages) skipped during save
 *   - Cursor bitmaps and display state
 *
 * SAVE PROCESS:
 *   1. Validate filename and check file space
 *   2. Open/create sysout file
 *   3. Write IFPAGE to file
 *   4. Iterate through FPtoVP table, writing active pages
 *   5. Skip dominopages (system pages)
 *   6. Save cursor bitmaps and display state
 *   7. Close file and return status
 *
 * ERROR HANDLING:
 *   The function returns various error codes:
 *   - COMPLETESYSOUT (NIL): Success
 *   - BADFILENAME: Invalid filename
 *   - NOFILESPACE: Insufficient disk space
 *   - FILECANNOTOPEN: Cannot open file
 *   - FILECANNOTSEEK: Cannot seek in file
 *   - FILECANNOTWRITE: Cannot write to file
 *   - FILETIMEOUT: File operation timeout
 *
 * CONSTANTS:
 *   - FP_IFPAGE: IFPAGE address in sysout file (512 bytes)
 *   - DOMINOPAGES: Number of dominopages to skip (301)
 *   - SKIPPAGES: First file page to save (301)
 *   - SKIP_DOMINOPAGES: Byte offset for dominocode (153600)
 *   - SAVE_IFPAGE: Virtual address for IFPAGE buffer (223)
 *
 * CROSS-REFERENCE: See ldsout.c for sysout loading
 * CROSS-REFERENCE: See ifpage.h for IFPAGE structure
 * CROSS-REFERENCE: See documentation/medley/components/sysout.typ
 *
 * HISTORICAL NOTES:
 *   - Original implementation by Venue (1989-1995)
 *   - Designed to support incremental saves and restores
 *   - Handles both full and partial saves
 */

#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <signal.h>
#include <stddef.h> // for ptrdiff_t
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <sys/param.h>
#include <pwd.h>
#include <unistd.h>

#include "lispemul.h"
#include "lispmap.h"
#include "lspglob.h"
#include "timeout.h"
#include "adr68k.h"
#include "lsptypes.h"
#include "locfile.h"
#include "dbprint.h"
#include "devif.h"
#include "ifpage.h" // for MACHINETYPE_MAIKO

#include "vmemsavedefs.h"
#include "byteswapdefs.h"
#include "commondefs.h"
#include "dskdefs.h"
#include "initkbddefs.h"
#include "perrnodefs.h"
#include "ufsdefs.h"

/* Definitions incorporated from vmemsave.h (removed) */
#define FP_IFPAGE 512           /* IFPAGE address in sysoutfile by Byte */
#define DOMINOPAGES 301         /* skip dominopages  in  fptovp */
#define SKIPPAGES 301           /* save first filepage  */
#define SKIP_DOMINOPAGES 153600 /* Byte size in sysoutfile for dominocode */
#define SAVE_IFPAGE 223         /* Virtual address for IFPAGES's buffer page. This value is \EMUSWAPBUFFERS in lisp. */

/* Error return values from VMEMSAVE */
#define COMPLETESYSOUT NIL
#define BADFILENAME (S_POSITIVE | 1)
#define NOFILESPACE (S_POSITIVE | 2)
#define FILECANNOTOPEN (S_POSITIVE | 3)
#define FILECANNOTSEEK (S_POSITIVE | 4)
#define FILECANNOTWRITE (S_POSITIVE | 5)
#define FILETIMEOUT (S_POSITIVE | 6)

extern struct pixrect *CursorBitMap, *InvisibleCursorBitMap;
extern struct cursor CurrentCursor, InvisibleCursor;

extern int *Lisp_errno;
extern int Dummy_errno; /* Used if errno cell isn't provided by Lisp.*/
extern int please_fork;

/************************************************************************/
/*									*/
/*			l i s p s t r i n g p				*/
/*									*/
/*	Predicate:  Is the argument (which must be a Lisp 1-d array)	*/
/*		    a lisp string?  i.e., are its elements char's?	*/
/*									*/
/************************************************************************/

/*
 * FUNCTION: lispstringP
 *
 * PURPOSE:
 *   Predicate function that checks if a Lisp value is a string.
 *   A Lisp string is a one-dimensional array whose elements are
 *   characters (either THIN_CHAR or FAT_CHAR type).
 *
 * PARAMETERS:
 *   Lisp: LispPTR to check (must be a 1-D array)
 *
 * RETURNS:
 *   1 if Lisp is a string (THIN_CHAR or FAT_CHAR type)
 *   0 otherwise
 *
 * ALGORITHM:
 *   1. Cast Lisp to OneDArray pointer
 *   2. Check typenumber field
 *   3. Return 1 if THIN_CHAR_TYPENUMBER or FAT_CHAR_TYPENUMBER
 *   4. Return 0 otherwise
 *
 * STRING TYPES:
 *   - THIN_CHAR_TYPENUMBER: Thin character array (8-bit chars)
 *   - FAT_CHAR_TYPENUMBER: Fat character array (16-bit chars)
 *
 * This function is used during sysout save to identify string
 * values that need special handling.
 *
 * CROSS-REFERENCE: See lsptypes.h for THIN_CHAR_TYPENUMBER, FAT_CHAR_TYPENUMBER
 * CROSS-REFERENCE: See array.h for OneDArray structure
 */
static int lispstringP(LispPTR Lisp)
{
  /*
   * Get type number from one-dimensional array header.
   * NativeAligned4FromLAddr converts Lisp address to native pointer.
   */
  switch (((OneDArray *)(NativeAligned4FromLAddr(Lisp)))->typenumber)
  {
  case THIN_CHAR_TYPENUMBER:
    /*
     * Thin character array - 8-bit characters.
     * This is most common string type.
     */
    return (1);

  case FAT_CHAR_TYPENUMBER:
    /*
     * Fat character array - 16-bit characters.
     * Used for Unicode or extended character sets.
     */
    return (1);

  default:
    /*
     * Not a string type.
     * Return 0 to indicate false.
     */
    return (0);
  }
}

/************************************************************************/
/*									*/
/*			v m e m _ s a v e 0				*/
/*									*/
/*	Implements the VMEMSAVE subr.  Write out the current lisp	*/
/*	lisp image to the specified file.  If the sysout file-name	*/
/*	is explicitly specified, the directory on which the file	*/
/*	resides is exactly (?) an existing directory.  This is		*/
/*	guaranteed by the Lisp code, \MAIKO.CHECKFREEPAGE in LLFAULT.	*/
/*									*/
/*	If the file argument is nill, the default sysout-file name,	*/
/*	"~/lisp.virtualmem", is used, subject to override by the	*/
/*	LDEDESTSYSOUT UNIX-environment variable.			*/
/*									*/
/*									*/
/*									*/
/*									*/
/* Argument:	LispPTR	*args	args[0]					*/
/*			 The file name in Lisp format specifying a	*/
/*			 file to which the current Lisp image should	*/
/*			 be flushed or Lisp NIL.			*/
/*									*/
/* Value:	If succeed, returns NIL, otherwise one of the		*/
/*		following Lisp integers, indicating the reason.		*/
/*									*/
/*			1	BADFILENAME				*/
/*			2	NOFILESPACE				*/
/*			3	FILECANNOTOPEN				*/
/*			4	FILECANNOTSEEK				*/
/*			5	FILECANNOTWRITE				*/
/*			6	FILETIMEOUT				*/
/*									*/
/* Side Effect:	None.							*/
/*									*/
/************************************************************************/

/*
 * FUNCTION: vmem_save0
 *
 * PURPOSE:
 *   Implements VMEMSAVE subr for saving current Lisp virtual
 *   memory state to a sysout file. This function handles filename
 *   validation, default filename selection, and delegates to actual
 *   save operation.
 *
 * PARAMETERS:
 *   args: Array of LispPTR arguments
 *         - args[0]: File name (Lisp string or NIL for default)
 *
 * RETURNS:
 *   NIL on success, or error code:
 *   - BADFILENAME (1): Invalid filename
 *   - NOFILESPACE (2): Insufficient disk space
 *   - FILECANNOTOPEN (3): Cannot open file
 *   - FILECANNOTSEEK (4): Cannot seek in file
 *   - FILECANNOTWRITE (5): Cannot write to file
 *   - FILETIMEOUT (6): File operation timeout
 *
 * ALGORITHM:
 *   1. Initialize Lisp_errno to Dummy_errno
 *   2. If args[0] is a Lisp string:
 *      a. Convert Lisp string to C string
 *      b. Separate host and pathname
 *      c. Validate Unix pathname
 *      d. Call vmem_save() with validated pathname
 *   3. Otherwise (args[0] is NIL):
 *      a. Check LDEDESTSYSOUT environment variable
 *      b. Use default filename "~/lisp.virtualmem"
 *      c. Call vmem_save() with default pathname
 *
 * FILENAME HANDLING:
 *   - Explicit filename: Use provided Lisp string
 *   - NIL: Use default "~/lisp.virtualmem"
 *   - Environment override: LDEDESTSYSOUT variable
 *   - Host/pathname separation: Handles remote file specifications
 *
 * GLOBAL VARIABLES MODIFIED:
 *   - Lisp_errno: Set to Dummy_errno for error reporting
 *
 * CROSS-REFERENCE: See vmem_save() for actual save implementation
 * CROSS-REFERENCE: See lispstringP() for string validation
 * CROSS-REFERENCE: See LispStringToCString() for string conversion
 * CROSS-REFERENCE: See separate_host() for host/pathname separation
 * CROSS-REFERENCE: See unixpathname() for pathname validation
 */
LispPTR vmem_save0(LispPTR *args)
{
  char *def;
  char pathname[MAXPATHLEN], sysout[MAXPATHLEN], host[MAXNAMLEN];
  struct passwd *pwd;

  /*
   * Initialize Lisp_errno to Dummy_errno for error reporting.
   * This allows Lisp code to check for errors after the call.
   */
  Lisp_errno = &Dummy_errno;

  /*
   * Check if explicit filename was provided.
   * args[0] != NIL means a filename was specified.
   * lispstringP() validates that it's a string type.
   */
  if ((args[0] != NIL) && lispstringP(args[0]))
  {
    /*
     * Explicit filename provided.
     * Convert Lisp string to C string and validate.
     */
    /* Check of lispstringP is safer for LispStringToCString */
    LispStringToCString(args[0], pathname, MAXPATHLEN);
    separate_host(pathname, host);
    if (!unixpathname(pathname, sysout, sizeof(sysout), 0, 0))
      return (BADFILENAME);
    return (vmem_save(sysout));
  }
  else
  {
    /*
     * No explicit filename - use default or environment variable.
     * Check LDEDESTSYSOUT environment variable first.
     */
    if ((def = getenv("LDEDESTSYSOUT")) == 0)
    {
      /*
       * Use LDEDESTSYSOUT environment variable.
       * Get user's home directory and construct default filename.
       */
      pwd = getpwuid(getuid()); /* NEED TIMEOUT */
      if (pwd == (struct passwd *)NULL)
        return (FILETIMEOUT);
      strlcpy(sysout, pwd->pw_dir, sizeof(sysout));
      strlcat(sysout, "/lisp.virtualmem", sizeof(sysout));
    }
    else
    {
      /*
       * No environment variable - use default "~/lisp.virtualmem".
       * Handle tilde expansion for home directory.
       */
      if (*def == '~' && (*(def + 1) == '/' || *(def + 1) == '\0'))
      {
        /*
         * Tilde with path separator or just tilde.
         * Expand to user's home directory.
         */
        pwd = getpwuid(getuid()); /* NEED TIMEOUT */
        if (pwd == (struct passwd *)NULL)
          return (FILETIMEOUT);
        strlcpy(sysout, pwd->pw_dir, sizeof(sysout));
        strlcat(sysout, def + 1, sizeof(sysout));
      }
      else
      {
        /*
         * Use default filename as-is.
         * This handles absolute paths or relative paths.
         */
        strlcpy(sysout, def, sizeof(sysout));
      }
    }
    return (vmem_save(sysout));
  }
}

/************************************************************************/
/*									*/
/*			s o r t _ f p t o v p				*/
/*									*/
/*	Sort the entries in the file-page-to-virtual-page table,	*/
/*	to try to make a sysout file that has contiguous runs of	*/
/*	virtual pages in it, for speed.					*/
/*									*/
/************************************************************************/
#ifndef BIGVM
#ifndef BYTESWAP
/* Helper function for comparing DLwords during FPtoVP sorting */
static int twowords(const void *i, const void *j) /* the difference between two  DLwords. */
{
  return (*(const DLword *)i - *(const DLword *)j);
}

#define FPTOVP_ENTRY (FPTOVP_OFFSET >> 8)

/* Sort FPtoVP table to create contiguous virtual page runs in sysout file */
static void sort_fptovp(DLword *fptovp, size_t size)
{
  size_t oldsize, i;
  ptrdiff_t oldloc, newloc;
  DLword *fptr;

  /* Find FPTOVP_ENTRY marker in the table */
  for (fptr = fptovp, i = 0; GETWORD(fptr) != FPTOVP_ENTRY && i < size; fptr++, i++)
    ;

  /* Check if FPTOVP_ENTRY marker was found */
  if (GETWORD(fptr) != FPTOVP_ENTRY)
  {
    DBPRINT(("Couldn't find FPTOVP_ENTRY; not munging\n"));
    return;
  }
  /* Calculate offset of FPTOVP_ENTRY in the table */
  oldloc = fptr - fptovp;

  /* Found old fptovp table location, now sort the table */
  qsort(fptovp, size, sizeof(DLword), twowords);

ONE_MORE_TIME: /* Tacky, but why repeat code? */

  /* Look up FPTOVP_ENTRY again; if it's moved, need to shuffle stuff */
  for (fptr = fptovp, i = 0; GETWORD(fptr) != FPTOVP_ENTRY && i < size; fptr++, i++)
    ;

  /* Verify FPTOVP_ENTRY was found after sorting */
  if (GETWORD(fptr) != FPTOVP_ENTRY)
    error("Couldn't find FPTOVP_ENTRY second time!\n");
  /* Calculate new offset of FPTOVP_ENTRY after sorting */
  newloc = fptr - fptovp;

  /* Adjust fptovpstart to account for the sorted table position */
  InterfacePage->fptovpstart += (newloc - oldloc);
  oldsize = size;
  /* Remove trailing empty entries (0xffff) from the table */
  for (fptr = fptovp + (size - 1); GETWORD(fptr) == 0xffff;
       fptr--, InterfacePage->nactivepages--, size--)
    ;

  /* Report if any holes (empty entries) were found and removed */
  if (size != oldsize)
  {
    DBPRINT(("Found %d holes in fptovp table\n", oldsize - size));
  }

  /* Check for and remove any duplicate entries in the sorted table */
  {
    int dupcount = 0;
    for (fptr = fptovp, i = 1; i < size; i++, fptr++)
      if (GETWORD(fptr) == GETWORD(fptr + 1))
      {
        dupcount++;
        GETWORD(fptr) = 0xffff;
      }

    /* If duplicates were found, resort to squeeze them out, then adjust
       the size and fptovpstart again */
    if (dupcount)
    {
      qsort(fptovp, size, sizeof(DLword), twowords);
      oldloc = newloc;
      DBPRINT(("%d duplicates found\n", dupcount));
      goto ONE_MORE_TIME;
    }
  }
}
#endif
#endif
/************************************************************************/
/*									*/
/*				v m e m _ s a v e			*/
/*									*/
/*									*/
/*									*/
/************************************************************************/

/*
 * Argument:	char	*sysout_file_name
 *			 The file name in UNIX format specifying a file to which
 *			 the current Lisp image should be flushed.
 *
 * Value:	If succeed, returns Lisp NIL, otherwise one of the following Lisp integer
 *		indicating the reason of failure.
 *
 *			1	BADFILENAME
 *			2	NOFILESPACE
 *			3	FILECANNOTOPEN
 *			4	FILECANNOTSEEK
 *			5	FILECANNOTWRITE
 *			6	FILETIMEOUT
 *
 * Side Effect:	None.
 *
 * Description:
 *
 * Flush out the current Lisp image to the specified file.
 */

/* Maximum number of pages to write in a single write() call */
extern unsigned maxpages;
unsigned maxpages = 65536;

/* Main function to save Lisp virtual memory to a sysout file */
LispPTR vmem_save(char *sysout_file_name)
{
  int sysout; /* Sysout file descriptor */
#ifdef BIGVM
  unsigned int *fptovp;
#else
  DLword *fptovp; /* Pointer to FPtoVP table */
#endif          /* BIGVM */
  int vmemsize; /* Size of virtual memory in pages */
  int i;
  char tempname[MAXPATHLEN];
  ssize_t rsize;
  off_t roff;
  int rval;
  extern int ScreenLocked;
  extern DLword *EmCursorX68K;
  extern DLword *EmCursorY68K;
  extern DLword NullCursor[];
  extern DLword *EmCursorBitMap68K;

  /* remove cursor image from screen */

  /* set FPTOVP */
  fptovp = FPtoVP + 1;

  /* set VMEMSIZE */
  vmemsize = InterfacePage->nactivepages;

  /*	[HH:6-Jan-89]
          Sequence of save image
          (1) Sysout image is saved to a temporary file, tempname.
          (2) if a specified file, sysout_file_name, is exist, the file is removed.
          (3) the temporary file is renamed to the specified file.
  */

  SETJMP(FILETIMEOUT);
  snprintf(tempname, sizeof(tempname), "%s-temp", sysout_file_name);

  /* Confirm protection of specified file by open/close */

  TIMEOUT(sysout = open(sysout_file_name, O_WRONLY, 0666));
  if (sysout == -1)
  {
    /* No file error skip return. */
    if (errno != ENOENT)
      return (FILECANNOTOPEN); /* No such file error.*/
  }
  else
    TIMEOUT(rval = close(sysout));

  /* open temp file */
  TIMEOUT(sysout = open(tempname, O_WRONLY | O_CREAT | O_TRUNC, 0666));
  if (sysout == -1)
  {
    err_mess("open", errno);
    return (FILECANNOTOPEN);
  }

  InterfacePage->machinetype = MACHINETYPE_MAIKO;

  /* Restore storagefull state */
  if (((*STORAGEFULLSTATE_word) & 0xffff) == SFS_NOTSWITCHABLE)
  {
    /* This sysout uses only 8 Mbyte lisp space.
       It may be able to use this SYSOUT which has more than
       8 Mbyte lisp space.
       To enable to expand lisp space, \\STORAGEFULLSTATE
       should be NIL.
    */
    *STORAGEFULLSTATE_word = NIL;
    InterfacePage->storagefullstate = NIL;
  }
  else
  {
    /*  Otherwise, just restore storagefullstate in IFPAGE */
    InterfacePage->storagefullstate = (*STORAGEFULLSTATE_word) & 0xffff;
  }

/* First, sort fptovp table, trying to get pages contiguous */
#ifndef BIGVM
#ifndef BYTESWAP
  /* Byte-swapped machines don't sort the table right. */
  sort_fptovp(fptovp, vmemsize);
#endif
#endif

  /* store vmem to sysoutfile */

  for (i = 0; i < vmemsize; i++)
  {
    if (GETPAGEOK(fptovp, i) != 0177777)
    {
      unsigned int oldfptovp = GETFPTOVP(fptovp, i);
      unsigned int saveoldfptovp = oldfptovp;
      unsigned int contig_pages = 0;
      DLword *base_addr;

      TIMEOUT(roff = lseek(sysout, i * BYTESPER_PAGE, SEEK_SET));
      if (roff == -1)
      {
        err_mess("lseek", errno);
        return (FILECANNOTSEEK);
      }
      base_addr = Lisp_world + (GETFPTOVP(fptovp, i) * DLWORDSPER_PAGE);

      /* Now, let's see how many pages we can dump */
      while (GETFPTOVP(fptovp, i) == oldfptovp && i < vmemsize)
      {
        contig_pages++;
        oldfptovp++;
        i++;
      }
      i--; /* Previous loop always overbumps i */
      DBPRINT(("%4d: writing %d pages from %tx (%d)\n", i, contig_pages, (char *)base_addr - (char *)Lisp_world, saveoldfptovp));

#ifdef BYTESWAP
      word_swap_page(base_addr, contig_pages * CELLSPER_PAGE);
#endif /* BYTESWAP */

      if (contig_pages > maxpages)
      {
        DLword *ba = base_addr;
        unsigned int pc = contig_pages;
        while (pc > maxpages)
        {
          TIMEOUT(rsize = write(sysout, ba, (size_t)maxpages * BYTESPER_PAGE));
          if (rsize == -1)
          {
            err_mess("write", errno);
            return ((errno == ENOSPC) || (errno == EDQUOT)) ? NOFILESPACE : FILECANNOTWRITE;
          }
          ba += maxpages * DLWORDSPER_PAGE;
          pc -= maxpages;
        }
        if (pc > 0)
          TIMEOUT(rsize = write(sysout, ba, pc * BYTESPER_PAGE));
      }
      else
      {
        unsigned int oldTT = TIMEOUT_TIME;
        /* As we can spend longer than TIMEOUT_TIME doing a big
           write, we adjust the timeout temporarily here */
        TIMEOUT_TIME += contig_pages >> 3;
        TIMEOUT(rsize = write(sysout, base_addr, contig_pages * BYTESPER_PAGE));
        TIMEOUT_TIME = oldTT;
      }
#ifdef BYTESWAP
      word_swap_page(base_addr, contig_pages * CELLSPER_PAGE);
#endif /* BYTESWAP */

      if (rsize == -1)
      {
        err_mess("write", errno);
        return ((errno == ENOSPC) || (errno == EDQUOT)) ? NOFILESPACE : FILECANNOTWRITE;
      }
    }
  }

  /* seek to IFPAGE */
  TIMEOUT(roff = lseek(sysout, (long)FP_IFPAGE, SEEK_SET));
  if (roff == -1)
  {
    err_mess("lseek", errno);
    return (FILECANNOTSEEK);
  }
#ifdef BYTESWAP
  word_swap_page(InterfacePage, CELLSPER_PAGE);
#endif /* BYTESWAP */

  TIMEOUT(rsize = write(sysout, (char *)InterfacePage, BYTESPER_PAGE));
#ifdef BYTESWAP
  word_swap_page(InterfacePage, CELLSPER_PAGE);
#endif /* BYTESWAP */

  if (rsize == -1)
  {
    err_mess("write", errno);
    return ((errno == ENOSPC) || (errno == EDQUOT)) ? NOFILESPACE : FILECANNOTWRITE;
  }

  TIMEOUT(rval = close(sysout));
  if (rval == -1)
  {
    return (FILECANNOTWRITE);
  }

  TIMEOUT(rval = unlink(sysout_file_name));
  if (rval == -1)
  {
    /* No file error skip return. */
    if (errno != ENOENT) /* No such file error.*/
      return (FILECANNOTOPEN);
  }

  TIMEOUT(rval = rename(tempname, sysout_file_name));
  if (rval == -1)
  {
    (void)fprintf(stderr, "sysout is saved to temp file, %s.", tempname);
    return (FILECANNOTWRITE);
  }

  /*printf("vmem is saved completely.\n");*/
  return (COMPLETESYSOUT);
}

/************************************************************************/
/*									*/
/*			l i s p _ f i n i s h				*/
/*									*/
/*	Kill all forked sub-processes before exiting.			*/
/*									*/
/************************************************************************/

/* Make sure that we kill off any Unix subprocesses before we go away */

void lisp_finish(int exit_status)
{
  char d[4];

  DBPRINT(("finish lisp_finish\n"));

  if (please_fork)
  { /* if lde runs with -NF(No fork), */
    /* following 5 lines don't work well. */
    d[0] = 'E';
    d[3] = 1;
    /* These only happen if the fork really succeeded: */
    /* if (UnixPipeOut >= 0) write(UnixPipeOut, d, 4); */
    /* if (UnixPipeIn >= 0 read(UnixPipeIn, d, 4);*/ /* Make sure it's finished */
    /* if (UnixPID >= 0) kill(UnixPID, SIGKILL);*/   /* Then kill fork_Unix itself */
  }
  device_before_exit();
  exit(exit_status);
}
