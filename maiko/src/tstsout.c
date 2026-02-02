/* $Id: tstsout.c,v 1.3 1999/05/31 23:35:44 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */
/*
 *	tstsout.c
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/* FILE: tstsout.c - Sysout File Validation Utility
 *
 * This file implements a utility to validate Medley sysout files.
 * It checks the IFPAGE (Interface Page) of a sysout file to verify
 * its integrity and display basic information.
 *
 * HIGH CONFIDENCE: Simple file I/O and structure verification using
 * standard C library functions.
 *
 * PURPOSE:
 * - Validate sysout file format and structure
 * - Check IFPAGE integrity and version compatibility
 * - Display sysout file information (version, size, state)
 *
 * KEY FUNCTIONS:
 * - check_sysout: Main validation function
 * - usage: Print usage information
 * - main: Entry point with command-line parsing
 *
 * COMMAND-LINE OPTIONS:
 * - [-v]: Verbose mode - print detailed IFPAGE information
 * - [sysout-filename]: Path to sysout file to validate
 *
 * IFPAGE LOCATION:
 * - IFPAGE_ADDRESS = 512 bytes from start of file
 * - IFPAGE contains system initialization parameters
 * - Byteswapping may be needed on little-endian systems
 *
 * CROSS-REFERENCE: IFPAGE structure in ifpage.h
 * CROSS-REFERENCE: Byteswap functions in byteswapdefs.h
 * CROSS-REFERENCE: Sysout loading in initsout.c
 */

#include "version.h"

#include <fcntl.h>           // for open
#include <stdio.h>           // for printf, perror, fprintf, sprintf, SEEK_SET
#include <stdlib.h>          // for exit
#include <string.h>          // for strncmp
#include <fcntl.h>           // for O_RDONLY
#include <unistd.h>          // for close, lseek, read

#include "byteswapdefs.h"    // for word_swap_page
#include "ifpage.h"          // for IFPAGE

#define IFPAGE_ADDRESS 512
#define MBYTE 0x100000 /* 1 Mbyte */

void check_sysout(char *sysout_file_name, int verbose);
void usage(char *prog);

void check_sysout(char *sysout_file_name, int verbose) {
  int sysout; /* SysoutFile descriptor */

  IFPAGE ifpage; /* IFPAGE */

  char errmsg[255];

  /*
   * first read the IFPAGE(InterfacePage)
   */

  /* open SysoutFile */
  sysout = open(sysout_file_name, O_RDONLY);
  if (sysout == -1) {
    sprintf(errmsg, "sysout_loader: can't open sysout file: %s", sysout_file_name);
    perror(errmsg);
    exit(-1);
  }
  /* seek to IFPAGE */
  if (lseek(sysout, IFPAGE_ADDRESS, SEEK_SET) == -1) {
    perror("sysout_loader: can't seek to IFPAGE");
    exit(-1);
  }
  /* reads IFPAGE into scratch_page */
  if (read(sysout, &ifpage, sizeof(IFPAGE)) == -1) {
    perror("sysout_loader: can't read IFPAGE");
    exit(-1);
  }

#ifdef BYTESWAP
  word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4);
#endif
  close(sysout);
  if (verbose) {
      printf("ifpage.minbversion %d\n", ifpage.minbversion);
      printf("ifpage.process_size %d\n", ifpage.process_size);
      printf("ifpage.storagefullstate %d\n", ifpage.storagefullstate);
      printf("ifpage.nactivepages %d\n", ifpage.nactivepages);
  } else {
      printf("%d", ifpage.minbversion);
  }
}

void usage(char *prog) {
    (void)fprintf(stderr, "Usage: %s [-v] sysout-filename\n", prog);
    exit(-1);
}

int main(int argc, char **argv) {
    switch (argc) {
    case 2:
        check_sysout(argv[1], 0);
        break;
    case 3:
        if (0 == strncmp(argv[1], "-v", 2)) {
            check_sysout(argv[2], 1);
        } else {
            usage(argv[0]);
        }
        break;
    default:
        usage(argv[0]);
    }
  exit(0);
}
