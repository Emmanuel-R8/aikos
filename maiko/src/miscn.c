/* $Id: miscn.c,v 1.3 1999/05/31 23:35:39 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/***********************************************************/
/*							*/
/*	FILE: miscn.c - Miscellaneous Opcode Implementation	*/
/*							*/
/*	Implements various miscellaneous opcodes that don't fit into	*/
/*	other categories. Includes stack manipulation, string operations,*/
/*	debugging tools, and system interface functions.		*/
/*							*/
/***********************************************************/

#include "arith.h"      // for N_GETNUMBER
#include "commondefs.h" // for error
#include "emlglob.h"
#include "lispemul.h"     // for LispPTR, state, CurrentStackPTR, TopOfStack
#include "loopsopsdefs.h" // for LCFetchMethod, LCFetchMethodOrHelp, LCFind...
#include "lspglob.h"
#include "lsptypes.h"
#include "miscndefs.h"   // for OP_miscn
#include "mvsdefs.h"     // for values, values_list
#include "subrs.h"       // for miscn_LCFetchMethod, miscn_LCFetchMethodOr...
#include "sxhashdefs.h"  // for STRING_EQUAL_HASHBITS, STRING_HASHBITS
#include "usrsubrdefs.h" // for UserSubr

/***********************************************************/
/*							*/
/*	FUNCTION: OP_miscn - Miscellaneous Opcode Dispatcher	*/
/*							*/
/*	Handles various miscellaneous operations including:		*/
/*	- Stack manipulation and argument gathering		*/
/*	- String hashing operations				*/
/*	- VALUES and VALUES-LIST handling			*/
/*	- Loop control fetch operations			*/
/*	- System interface functions (RS232, debugging)	*/
/*							*/
/*	INTERFACE: Global Machine State			*/
/*	RETURNS:   0 = continue, C code succeeded		*/
/*		   1 = must UFN, C code failed		*/
/*							*/
/*	HIGH CONFIDENCE: This function provides unified interface	*/
/*	for miscellaneous operations that would otherwise need	*/
/*	separate opcode implementations.			*/
/*							*/
/***********************************************************/

int OP_miscn(int misc_index, int arg_count)
{
  LispPTR *stk;             /* Stack pointer for argument collection */
  int result;               /* Operation result return value */
  static LispPTR args[255]; /* Local array for collected arguments */

  /* STACK ARGUMENT COLLECTION: Pop arguments from stack into vector
   *
   * This section reverses stack order to collect arguments in correct
   * calling convention order. The stack grows downward, so we need
   * to careful with pointer arithmetic.
   *
   * STACK MANIPULATION STRATEGY:
   * 1. Position stk pointer just above CurrentStackPTR
   * 2. Store current TopOfStack as first element (args[0])
   * 3. Pop remaining args from stack into reverse order
   *
   * HIGH CONFIDENCE: Standard argument gathering for variadic functions.
   * CRITICAL: Must maintain stack consistency throughout operation.
   *
   * CROSS-REFERENCE: Similar pattern in other opcode implementations
   * in xc.c and various opcode dispatch files.
   */
  args[0] = NIL_PTR;                              /* Initialize first element to NIL */
  stk = ((LispPTR *)(void *)CurrentStackPTR) + 1; /* Position above current stack */

  {
    int arg_num = arg_count;
    if (arg_num > 0)
    {
      *stk++ = (LispPTR)TopOfStack; /* Store current TOS as first arg */
      /* Pop remaining args from stack into args[] in reverse order */
      while (arg_num > 0)
        args[--arg_num] = *--stk;
    }
  }

  /* SELECT MISCELLANEOUS OPERATION: Dispatch based on misc_index
   *
   * Each case implements a specific miscellaneous operation.
   * The operation may access arguments through the args[] array
   * and must return appropriate result values.
   *
   * OPCODE CATEGORIES:
   * ==================
   * - USER_SUBR: Call user-defined subroutine by index
   * - SXHASH/STRING*: String hashing and comparison operations
   */

  switch (misc_index)
  {
  case miscn_USER_SUBR:
  {
    int user_subr;
    N_GETNUMBER(args[0], user_subr, do_ufn);
    if ((result = UserSubr(user_subr, arg_count - 1, &args[1])) < 0)
      goto do_ufn;
  }
  break;
  case miscn_SXHASH:
    result = SX_hash(args[0]);
    break;

  case miscn_STRING_EQUAL_HASHBITS:
    result = STRING_EQUAL_HASHBITS(args[0]);
    break;

  case miscn_STRINGHASHBITS:
    result = STRING_HASHBITS(args[0]);
    break;

  case miscn_VALUES:
    if (arg_count > 255)
    {
      error("miscn: arg_count too big! continue punts");
      goto do_ufn;
    }
    result = values(arg_count, args);
    break;

  case miscn_VALUES_LIST:
    /*** debugging: should be impossible, but ADB found this once -FS *****/
    if (arg_count > 255)
    {
      error("miscn: arg_count too big! continue punts");
      goto do_ufn;
    }
    result = values_list(arg_count, args);
    break;

  case miscn_LCFetchMethod:
    result = LCFetchMethod(args[0], args[1]);
    if (result < 0)
      goto lc_ufn;
    break;

  case miscn_LCFetchMethodOrHelp:
    result = LCFetchMethodOrHelp(args[0], args[1]);
    if (result < 0)
      goto lc_ufn;
    break;

  case miscn_LCFindVarIndex:
    result = LCFindVarIndex(args[0], args[1]);
    if (result < 0)
      goto lc_ufn;
    break;

  case miscn_LCGetIVValue:
    result = LCGetIVValue(args[0], args[1]);
    if (result < 0)
      goto lc_ufn;
    break;

  case miscn_LCPutIVValue:
    result = LCPutIVValue(args[0], args[1], args[2]);
    if (result < 0)
      goto lc_ufn;
    break;

  case /* miscn_CALL_C*/ 014:
    /* result = call_c_fn(args); */
    result = 0;
    break;

  default:
    goto do_ufn;

  } /* switch end */

  /* Setup Global Machine State for a Normal Return */

  PC += 3;
  CurrentStackPTR = (DLword *)(stk - 1);
  TopOfStack = (LispPTR)result;
  return (0);

  /* A UFN request, so return 1 & don't change the Machine State */

do_ufn:
  return (1);
lc_ufn:
  if (result == -2)
  {
    return (0); /* have built new stack frame */
  }
  else
  {
    goto do_ufn;
  }

} /* OP_miscn */
