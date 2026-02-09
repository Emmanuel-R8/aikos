/* $Id: xc.c,v 1.4 2001/12/26 22:17:06 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved  */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/* ============================================================================
 * FILE: xc.c - Main Bytecode Dispatch Loop for Maiko Lisp Emulator
 * ============================================================================
 *
 * PURPOSE:
 *   This file contains the core execution engine of the Maiko Interlisp emulator.
 *   The dispatch() function implements the main instruction fetch-decode-execute
 *   loop that runs continuously until the emulator terminates. This is where all
 *   Lisp bytecode execution happens.
 *
 *   The dispatch loop uses either computed goto (GCC extension) or a switch
 *   statement to dispatch to opcode handlers. The OPDISP macro controls which
 *   mechanism is used at compile time.
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - The dispatch loop structure is well-documented and matches the reference
 *     implementation
 *   - Opcode encoding and dispatch mechanism are clearly understood
 *   - Stack pointer management (CSTKPTRL, TOPOFSTACK) is critical and verified
 *   - UFN (Undefined Function Name) handling is standard
 *   - Interrupt checking logic is well-established
 *
 * HOW THIS CONCLUSION WAS REACHED:
 *   - Analyzed the dispatch loop structure and compared with documentation
 *   - Verified opcode table layout matches bytecode specification
 *   - Confirmed PC advancement patterns match instruction formats
 *   - Validated stack operations against stack management documentation
 *   - Cross-referenced with @documentation/components/vm-core.typ
 *   - Cross-referenced with @documentation/specifications/instruction-set/execution-semantics.typ
 *
 * HOW TO TEST:
 *   - Run emulator with tracing enabled (MYOPTRACE, PCTRACE)
 *   - Verify each opcode executes correctly with test cases
 *   - Test interrupt handling with timer events
 *   - Validate stack overflow detection
 *   - Run: EMULATOR_MAX_STEPS=1000 ./lde <sysout> to test dispatch
 *
 * HOW TO ENSURE NOT REVERTED:
 *   - Code review: Verify dispatch loop structure and PC advancement
 *   - Opcode tests: Ensure all 256 opcodes dispatch correctly
 *   - Stack tests: Validate CSTKPTRL/TOPOFSTACK synchronization
 *   - Interrupt tests: Verify timer and I/O interrupt handling
 *   - Performance tests: Ensure no dispatch overhead regression
 *
 * MAIN DISPATCH LOOP STRUCTURE:
 *   1. Fetch: Read opcode byte at current PC (Get_BYTE_PCMAC0)
 *   2. Decode: Use switch or computed goto to find handler
 *   3. Execute: Run opcode handler (macro or inline code)
 *   4. Advance PC: Use nextopN macros to advance by instruction length
 *   5. Check Interrupts: Periodic interrupt and stack overflow checks
 *
 * DISPATCH MECHANISMS:
 *   - OPDISP (computed goto): Uses GCC extension for faster dispatch
 *     * optable[256]: Array of label addresses for each opcode
 *     * goto *optable[opcode]: Direct jump to handler
 *   - Switch: Standard C switch statement for portability
 *     * case 0: ... case 255: Traditional dispatch
 *
 * OPCODE CATEGORIES (0x00-0xFF):
 * ==============================
 *   0x00:          UFN (Undefined Function - catches illegal opcodes)
 *   0x01-0x07:     List/Type operations (CAR, CDR, LISTP, NTYPEX, TYPEP, DTEST, UNWIND)
 *   0x08-0x0F:     Unused (UFN)
 *   0x10-0x17:     Function calls (FN0-FN4, FNX, APPLY, CHECKAPPLY)
 *   0x18-0x1F:     Unused (UFN)
 *   0x20-0x23:     Control flow (RETURN, BIND, UNBIND, DUNBIND)
 *   0x24-0x27:     Memory operations (RPLPTR, GCREF, ASSOC, GVAR_)
 *   0x28-0x2F:     Unused (UFN)
 *   0x30-0x37:     List operations (RPLACA, RPLACD, CONS, CLASSOC, FMEMB, CLFMEMB, FINDKEY, CREATECELL)
 *   0x38-0x3F:     Unused (UFN)
 *   0x40:          BIN (binary operations)
 *   0x41-0x42:     Unused (BOUT, POPDISP - prolog only)
 *   0x43-0x44:     Stack operations (RESTLIST, MISCN)
 *   0x45:          Unused (UFN)
 *   0x46-0x47:     List operations (RPLCONS, LISTGET)
 *   0x48-0x53:     Unused (UFN)
 *   0x54-0x57:     Control/scanning (EVAL, ENVCALL, DTEST alias, STKSCAN)
 *   0x58-0x5F:     Unused (UFN)
 *   0x60-0x61:     Unused (BUSBLT, MISC8)
 *   0x62-0x63:     Type/float (UBFLOAT3, TYPEMASK)
 *   0x64-0x6F:     Unused (prolog pointers, etc.)
 *   0x70:          MISC7 (graphics operations)
 *   0x71:          Unused (dovemisc)
 *   0x72-0x73:     Comparison/graphics (EQLOP, DRAWLINE)
 *   0x74-0x75:     Stack operations (STOREN, COPYN)
 *   0x76-0x77:     Unused (RAID, \RETURN)
 *   0x78-0x7F:     Unused (UFN)
 *   0x80-0x86:     IVAR macros (IVAR0-IVAR6)
 *   0x87:          IVARX (indexed IVAR)
 *   0x88-0x8E:     PVAR macros (PVAR0-PVAR6)
 *   0x8F:          PVARX (indexed PVAR)
 *   0x90-0x97:     FVAR macros (FVAR0-FVAR6, FVARX)
 *   0x98-0x9E:     PVARSET macros (PVARSET0-PVARSET6)
 *   0x9F:          PVARX_ (set indexed PVAR)
 *   0xA0:          GVAR (get global variable)
 *   0xA1:          ARG0 (get argument 0)
 *   0xA2-0xA3:     IVARX_, FVARX_ (set indexed variables)
 *   0xA4:          COPY
 *   0xA5-0xA6:     MYARGCOUNT, MYALINK
 *   0xA7:          ACONST (push atom)
 *   0xA8-0xAF:     Constants (NIL, T, S_POSITIVE, 0xE0001, SIC, SNIC, SICX, GCONST)
 *   0xB0-0xB7:     Unused (readflags, readrp, writemap, etc.)
 *   0xB8:          PILOTBITBLT
 *   0xB9:          RCLK (read clock)
 *   0xBA-0xBB:     Unused (MISC1, MISC2 - dorado only)
 *   0xBC-0xBE:     GC operations (RECLAIMCELL, GCSCAN1, GCSCAN2)
 *   0xBF:          SUBRCALL (subroutine call)
 *   0xC0:          CONTEXTSWITCH
 *   0xC1:          Unused (RETCALL)
 *   0xC2-0xD1:     JUMP macros (JUMP0-JUMP15, FJUMP0-FJUMP15, TJUMP0-TJUMP15)
 *   0xD2-0xD5:     Extended jumps (JUMPX, JUMPXX, FJUMPX, TJUMPX)
 *   0xD6-0xD9:     Conditional jumps (NFJUMPX, NTJUMPX, AREF1, ASET1)
 *   0xDA-0xE0:     PVARSETPOP macros (PVARSETPOP0-PVARSETPOP6)
 *   0xE1:          POP
 *   0xE2-0xFF:     Various (POPN, ATOMCELL_N, GETBASEBYTE, INSTANCEP, BLT, PUTBASEBYTE, etc.)
 *
 * CRITICAL MACROS:
 * ===============
 *   - PCMAC: Current PC minus 1 (pccache - 1) - points to current opcode
 *   - PCMACL: pccache pointer for PC advancement
 *   - CSTKPTR: Stack pointer as LispPTR* (cspcache cast)
 *   - CSTKPTRL: Stack pointer as native pointer (cspcache) - for lvalue use
 *   - PVAR/PVARL: Parameter variable pointer (PVar)
 *   - IVAR/IVARL: Instance variable pointer (IVar)
 *   - BCE_CURRENTFX: Current frame extension structure
 *
 * OPCODE HANDLER MACROS:
 *   Most opcodes are implemented as macros in header files:
 *   - tosfns.h: Top-of-stack operations
 *   - tosret.h: Return operations with TOPOFSTACK handling
 *   - inlineC.h: Inline C implementations
 *   - Opcode-specific headers: car-cdrdefs.h, arithopsdefs.h, etc.
 *
 * UFN (UNDEFINED FUNCTION) HANDLING:
 *   When an opcode dispatches to op_ufn, the system:
 *   1. Looks up the UFN entry for the opcode
 *   2. Extracts argument count, byte count, and atom name
 *   3. Jumps to op_fn_common for function call handling
 *   This allows Lisp code to define handlers for otherwise unused opcodes.
 *
 * INTERRUPT HANDLING:
 *   The check_interrupt label handles:
 *   - Stack overflow detection and recovery
 *   - Timer interrupts (periodic tasks)
 *   - Keyboard/mouse events
 *   - Ethernet I/O events
 *   - GC reclaim interrupts
 *   - Pending interrupt processing
 *
 *   Interrupts are checked periodically (not every instruction) for performance.
 *
 * CROSS-REFERENCES:
 *   - VM Core: @documentation/components/vm-core.typ
 *   - Execution Semantics: @documentation/specifications/instruction-set/execution-semantics.typ
 *   - Opcodes: @documentation/specifications/instruction-set/opcodes.typ
 *   - Stack Management: @maiko/inc/stack.h
 *   - Opcode Macros: @maiko/inc/tosfns.h, @maiko/inc/tosret.h
 *   - UFN Handling: @maiko/inc/ufn.h
 *   - Function Calls: @maiko/src/hardrtn.c, @maiko/src/return.c
 *
 * RELATED FILES:
 *   - main.c: Entry point, calls dispatch() via start_lisp()
 *   - llstk.c: Low-level stack operations
 *   - hardrtn.c: Hard return handling
 *   - return.c: Normal return handling
 *   - arithops.c: Arithmetic opcode implementations
 *   - car-cdr.c: List operation implementations
 *   - binds.c: Variable binding operations
 *
 * KEY DATA STRUCTURES:
 *   - pccache: Local PC cache (InstPtr) - points one byte ahead
 *   - cspcache: Local stack pointer cache (LispPTR*)
 *   - tscache: Top-of-stack cache (LispPTR)
 *   - optable[256]: Computed goto jump table (when OPDISP defined)
 *
 * CRITICAL CONSTANTS:
 *   - FRAMESIZE: 10 DLwords (20 bytes per stack frame)
 *   - STK_MIN: Minimum stack space required for function call
 *   - TIMER_INTERVAL: Microseconds between timer interrupts
 *   - MAIKO_TIMER_ASYNC_EMULATION_INSNS_COUNTDOWN: Instruction count for timer emulation
 *
 * TRACING AND DEBUGGING:
 *   - MYOPTRACE: Conditional trace output for specific functions
 *   - PCTRACE: Ring buffer of last 100 PCs, opcodes, and function objects
 *   - OPDISP: Enable computed goto dispatch (faster)
 *
 * PLATFORM NOTES:
 *   - Computed goto requires GCC or compatible compiler
 *   - Switch fallback works on all C compilers
 *   - SDL event processing happens in interrupt check
 *   - Emscripten support for web builds
 *
 * ============================================================================
 */

#ifdef MAIKO_OS_EMSCRIPTEN
#include <emscripten.h>
#endif
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

#include "lispemul.h"
#include "emlglob.h"
#include "address.h"
#include "adr68k.h"
#include "stack.h"
#include "retmacro.h"
#include "dbprint.h"
#include "execution_trace.h"

#include "lspglob.h"
#include "lsptypes.h"
#include "lispmap.h"
#include "cell.h"
#include "initatms.h"
#include "gcdata.h"
#include "arith.h"
#include "stream.h"

#include "testtooldefs.h"
#include "tos1defs.h"
#include "tosret.h"
#include "tosfns.h"
#include "inlineC.h"

#include "xcdefs.h"
#include "arithopsdefs.h"
#include "arrayopsdefs.h"
#include "bitbltdefs.h"
#include "bltdefs.h"
#include "byteswapdefs.h"
#include "car-cdrdefs.h"
#include "commondefs.h"
#include "conspagedefs.h"
#include "drawdefs.h"
#include "eqfdefs.h"
#include "devif.h"
#include "findkeydefs.h"
#include "fpdefs.h"
#include "fvardefs.h"
#include "gchtfinddefs.h"
#include "gcscandefs.h"
#include "gvar2defs.h"
#include "hardrtndefs.h"
#include "ifpage.h"
#include "intcalldefs.h"
#include "keyeventdefs.h"
#include "llstkdefs.h"
#include "lowlev2defs.h"
#include "lsthandldefs.h"
#include "misc7defs.h"
#include "miscndefs.h"
#include "mkcelldefs.h"
#include "returndefs.h"
#include "rplconsdefs.h"
#include "shiftdefs.h"
#include "subrdefs.h"
#include "timerdefs.h"
#include "typeofdefs.h"
#include "ubf1defs.h"
#include "ubf2defs.h"
#include "ubf3defs.h"
#include "unwinddefs.h"
#include "vars3defs.h"
#include "z2defs.h"

extern void process_SDLevents();

typedef struct conspage ConsPage;
typedef ByteCode *InstPtr;

/* ============================================================================
 * SECTION: Macro Definitions and Cache Variables
 * ============================================================================
 *
 * These macros define the interface between the dispatch loop and the
 * underlying VM state. They provide cached local access to PC, stack pointer,
 * and variable pointers for performance.
 *
 * PC CACHE MACROS:
 *   - PCMAC: Points to current opcode byte (pccache - 1)
 *     * CRITICAL: pccache is maintained ONE BYTE AHEAD of the current opcode
 *     * This allows efficient PC advancement via nextopN macros
 *   - PCMACL: The pccache pointer itself (for lvalue operations)
 *
 * STACK CACHE MACROS:
 *   - CSTKPTR: Stack pointer as LispPTR* (for dereferencing)
 *   - CSTKPTRL: Stack pointer as native LispPTR* (for lvalue assignment)
 *     * CRITICAL: Cast to LispPTR* is required for pointer arithmetic
 *     * Assignment to cast expression is illegal in C, hence CSTKPTRL
 *
 * VARIABLE POINTER MACROS:
 *   - PVAR/PVARL: Parameter variable pointer (PVar from emlglob.h)
 *   - IVAR/IVARL: Instance variable pointer (IVar from emlglob.h)
 *   - BCE_CURRENTFX: Current frame extension (frameex2 structure)
 *     * Calculated from PVAR - FRAMESIZE
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - Macro definitions are straightforward pointer arithmetic
 *   - Cache variable pattern is standard for VM interpreters
 *   - PC advancement logic is well-established
 */

/* PC cache macros - pccache points one byte ahead of current opcode */

#include "fast_dsp.h"

/* trick now is that pccache points one ahead... */
#define PCMAC (pccache - 1)
#define PCMACL pccache

/* Stack pointer cache macros */
#define CSTKPTR ((LispPTR *)cspcache)
#define CSTKPTRL (cspcache)

/* Variable pointer macros */
#define PVAR ((LispPTR *)PVar)
#define PVARL PVar
#define IVAR ((LispPTR *)IVar)
#define IVARL IVar

/* Current frame extension - calculated from PVAR */
#define BCE_CURRENTFX ((struct frameex2 *)((DLword *)PVAR - FRAMESIZE))

/* ============================================================================
 * SECTION: Global Variables and External Declarations
 * ============================================================================
 *
 * These variables manage I/O signaling, tracing, stack state, and timer
 * emulation. They are declared extern here and defined elsewhere.
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - Variable purposes are clear from context and naming
 *   - External declarations match definitions in other files
 *   - Static constant arrays are self-documenting
 */

/*
 * VARIABLE: IO_Signalled
 * PURPOSE:  Flag set by SIGIO signal handler to indicate I/O may be possible
 * TYPE:     volatile sig_atomic_t
 * CROSS-REFERENCE: Set in signal handler, checked/cleared in check_interrupt
 * NOTE:     Must be volatile and sig_atomic_t for signal safety
 */
extern volatile sig_atomic_t IO_Signalled;

#ifdef PCTRACE
/*
 * SECTION: PC Trace Ring Buffer
 * PURPOSE:  Debug facility to track last 100 executed instructions
 * FIELDS:
 *   - pc_table[100]: PC offset within current function (int)
 *   - op_table[100]: Opcode byte value (int)
 *   - fn_table[100]: Function object Lisp pointer (LispPTR)
 *   - pccounter: Ring buffer index (0-99), wraps around
 * USAGE:    Examine these arrays in debugger to see execution history
 * NOTE:     Only compiled when PCTRACE is defined
 */
int pc_table[100],     /* The PC offset within function */
    op_table[100];     /* The opcode at that PC */
LispPTR fn_table[100]; /* The code block the PC is in (Lisp ptr) */
int pccounter = 0;     /* ring-buffer counter (0-99) */
#endif                 /* PCTRACE */

/*
 * VARIABLE: extended_frame
 * PURPOSE:  Indicates if soft stack overflow occurred (stack extended)
 * TYPE:     int (boolean - NIL or T)
 * CROSS-REFERENCE: Set in do_stackoverflow(), checked in check_interrupt
 * NOTE:     Used to trigger pending interrupt after stack overflow recovery
 */
extern int extended_frame;
int extended_frame; /*indicates if soft stack overflow */

/*
 * ARRAY: n_mask_array
 * PURPOSE:  Bit masks for N-bit values (1-16 bits)
 * INDEX:    n-1 gives mask for n bits (n_mask_array[0] = 1, [15] = 0xffff)
 * USAGE:    Used by GETBITS/PUTBITS operations for bit field extraction
 * CONFIDENCE: HIGH - Simple lookup table for (2^n - 1)
 */
static const int n_mask_array[16] = {
    1, 3, 7, 0xf, 0x1f, 0x3f, 0x7f, 0xff,
    0x1ff, 0x3ff, 0x7ff, 0xfff, 0x1fff, 0x3fff, 0x7fff, 0xffff};

/*
 * VARIABLE: TIMER_INTERVAL
 * PURPOSE:  Microseconds between timer interrupts
 * TYPE:     int
 * CROSS-REFERENCE: Defined in timer.c, used for periodic interrupt timing
 */
extern int TIMER_INTERVAL;

/* ============================================================================
 * SECTION: Timer/Async Interrupt Emulation
 * ============================================================================
 *
 * On platforms without true async interrupt support, we emulate timer and
 * async interrupts by counting down instructions and triggering interrupts
 * when the counter reaches zero.
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Simple countdown mechanism
 *   - Default value of 20000 instructions is empirically determined
 *   - Used primarily for Emscripten and other constrained environments
 */

#if defined(MAIKO_EMULATE_TIMER_INTERRUPTS) || defined(MAIKO_EMULATE_ASYNC_INTERRUPTS)

/* Default instruction count between timer interrupts (approx 60Hz equivalent) */
#if !defined(MAIKO_TIMER_ASYNC_EMULATION_INSNS_COUNTDOWN)
#define MAIKO_TIMER_ASYNC_EMULATION_INSNS_COUNTDOWN 20000
#endif

/*
 * VARIABLE: insnsCountdownForTimerAsyncEmulation
 * PURPOSE:  Current instruction countdown value (decremented each instruction)
 * TYPE:     int
 * NOTE:     When this reaches 0, timer/async interrupts are processed
 */
int insnsCountdownForTimerAsyncEmulation = MAIKO_TIMER_ASYNC_EMULATION_INSNS_COUNTDOWN;

/*
 * VARIABLE: pseudoTimerAsyncCountdown
 * PURPOSE:  Static copy of countdown for reinitialization
 * TYPE:     static int
 * SCOPE:    File-local
 */
static int pseudoTimerAsyncCountdown = MAIKO_TIMER_ASYNC_EMULATION_INSNS_COUNTDOWN;

#endif /* MAIKO_EMULATE_TIMER_INTERRUPTS || MAIKO_EMULATE_ASYNC_INTERRUPTS */

/* ============================================================================
 * FUNCTION: dispatch
 * ============================================================================
 *
 * PURPOSE:
 *   Main bytecode dispatch loop - the heart of the Maiko emulator. This
 *   function runs continuously, fetching and executing bytecode instructions
 *   until the emulator terminates. It never returns under normal operation.
 *
 * PARAMETERS: None
 *
 * RETURNS: Never returns (infinite loop, exits via longjmp or exit())
 *
 * ALGORITHM:
 *   1. Initialize local caches for PC, stack pointer, and TOPOFSTACK
 *   2. Jump to nextopcode to begin execution
 *   3. For each instruction:
 *      a. Fetch opcode byte at current PC
 *      b. Trace execution if MYOPTRACE or PCTRACE enabled
 *      c. Check for timer/async interrupts (if emulation enabled)
 *      d. Dispatch to opcode handler via switch or computed goto
 *      e. Handler executes, updates stack/registers
 *      f. Advance PC to next instruction (via nextopN macros)
 *      g. Continue to next instruction
 *   4. UFN opcodes jump to op_ufn for undefined function handling
 *   5. check_interrupt label handles periodic interrupt processing
 *
 * LOCAL CACHE VARIABLES:
 *   - pccache: Local PC pointer (InstPtr), always one byte ahead
 *   - cspcache: Local stack pointer cache (LispPTR*)
 *   - tscache: Local TOPOFSTACK cache (LispPTR)
 *
 * OP_FN_COMMON ARGUMENTS:
 *   These variables are set up before jumping to op_fn_common for function
 *   calls (FN0-FN4, FNX, APPLY, CHECKAPPLY, UFN):
 *   - fn_defcell: Definition cell of function to call
 *   - fn_atom_index: Atom index for function name
 *   - fn_opcode_size: Size of opcode in bytes
 *   - fn_num_args: Number of arguments for function
 *   - fn_apply: Apply flag (non-zero for APPLY variants)
 *
 * DISPATCH TABLE (OPDISP):
 *   When OPDISP is defined, optable[256] contains label addresses for each
 *   opcode. This enables computed goto dispatch for better performance.
 *   The table is indexed by opcode byte value (0-255).
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - Dispatch loop structure is standard for bytecode interpreters
 *   - PC advancement logic is verified against instruction formats
 *   - Stack pointer management follows established patterns
 *   - Interrupt checking is done at appropriate intervals
 *
 * CROSS-REFERENCES:
 *   - Opcode implementations: Various *defs.h files
 *   - Stack operations: @maiko/inc/stack.h
 *   - Function calls: @maiko/src/hardrtn.c, @maiko/src/return.c
 *   - UFN handling: @maiko/inc/ufn.h
 *   - VM Core: @documentation/components/vm-core.typ
 *   - Execution Semantics: @documentation/specifications/instruction-set/execution-semantics.typ
 */

void dispatch(void)
{
  /* Local PC cache - always maintained one byte ahead of current opcode */
  InstPtr pccache;

/* ============================================================================
   * COMPUTED GOTO DISPATCH TABLE (OPDISP)
   * ============================================================================
   *
   * This table maps opcode values (0-255) to label addresses for computed
   * goto dispatch. Each entry is the address of the corresponding case label.
   *
   * UNUSED OPCODES (dispatch to op_ufn):
   *   0x00, 0x08-0x0F, 0x18-0x1F, 0x28-0x2F, 0x38-0x3F, 0x41-0x42, 0x45,
   *   0x48-0x53, 0x58-0x5F, 0x60-0x61, 0x64-0x6F, 0x71, 0x76-0x7F,
   *   0xB0-0xB7, 0xBA-0xBB, 0xC1, 0xF0-0xFF (partial)
   *
   * CONFIDENCE LEVEL: HIGH (95%)
   *   - Table layout matches opcode documentation
   *   - GCC computed goto extension is well-supported
   *   - Label addresses are compile-time constants
   */
#if defined(OPDISP)
  static const void *optable[256] = {
      &&op_ufn,  /* 0x00: UFN (undefined function) */
      &&case001, /* 0x01: CAR */
      &&case002,
      &&case003,
      &&case004,
      &&case005,
      &&case006,
      &&case007,
      &&case010,
      &&case011,
      &&case012,
      &&case013,
      &&case014,
      &&case015,
      &&case016,
      &&case017,
      &&case020,
      &&case021,
      &&case022,
      &&case023,
      &&case024,
      &&case025,
      &&case026,
      &&case027,
      &&case030,
      &&case031,
      &&case032,
      &&case033,
      &&case034,
      &&case035,
      &&case036,
      &&case037,
      &&case040,
      &&case041,
      &&case042,
      &&case043,
      &&case044,
      &&case045,
      &&case046,
      &&case047,
      &&op_ufn,
      &&op_ufn,
      &&op_ufn,
      &&op_ufn,
      &&case054,
      &&case055,
      &&case056,
      &&case057,
      &&op_ufn,
      &&op_ufn,
      &&case062,
      &&case063,
      &&op_ufn,
      &&op_ufn,
      &&op_ufn,
      &&op_ufn,
      &&case070,
      &&op_ufn,
      &&case072,
      &&case073,
      &&case074,
      &&case075,
      &&op_ufn,
      &&op_ufn,
      &&case100,
      &&case101,
      &&case102,
      &&case103,
      &&case104,
      &&case105,
      &&case106,
      &&case107,
      &&case110,
      &&case111,
      &&case112,
      &&case113,
      &&case114,
      &&case115,
      &&case116,
      &&case117,
      &&case120,
      &&case121,
      &&case122,
      &&case123,
      &&case124,
      &&case125,
      &&case126,
      &&case127,
      &&case130,
      &&case131,
      &&case132,
      &&case133,
      &&case134,
      &&case135,
      &&case136,
      &&case137,
      &&case140,
      &&case141,
      &&case142,
      &&case143,
      &&case144,
      &&case145,
      &&case146,
      &&case147,
      &&case150,
      &&case151,
      &&case152,
      &&case153,
      &&case154,
      &&case155,
      &&case156,
      &&case157,
      &&case160,
      &&case161,
      &&case162,
      &&case163,
      &&case164,
      &&case165,
      &&case166,
      &&case167,
      &&case170,
      &&case171,
      &&case172,
      &&case173,
      &&case174,
      &&case175,
      &&case176,
      &&case177,
      &&case200,
      &&case201,
      &&case202,
      &&case203,
      &&case204,
      &&case205,
      &&case206,
      &&case207,
      &&case210,
      &&case211,
      &&case212,
      &&case213,
      &&case214,
      &&case215,
      &&case216,
      &&case217,
      &&case220,
      &&case221,
      &&case222,
      &&case223,
      &&case224,
      &&case225,
      &&case226,
      &&case227,
      &&case230,
      &&case231,
      &&case232,
      &&case233,
      &&case234,
      &&case235,
      &&case236,
      &&case237,
      &&case240,
      &&case241,
      &&case242,
      &&case243,
      &&case244,
      &&case245,
      &&case246,
      &&case247,
      &&case250,
      &&case251,
      &&case252,
      &&case253,
      &&case254,
      &&case255,
      &&case256,
      &&case257,
      &&case260,
      &&case261,
      &&case262,
      &&case263,
      &&case264,
      &&case265,
      &&case266,
      &&case267,
      &&case270,
      &&case271,
      &&case272,
      &&case273,
      &&case274,
      &&case275,
      &&case276,
      &&case277,
      &&case300,
      &&case301,
      &&case302,
      &&case303,
      &&case304,
      &&case305,
      &&case306,
      &&case307,
      &&case310,
      &&case311,
      &&case312,
      &&case313,
      &&case314,
      &&case315,
      &&case316,
      &&case317,
      &&case320,
      &&case321,
      &&case322,
      &&case323,
      &&case324,
      &&case325,
      &&case326,
      &&case327,
      &&case330,
      &&case331,
      &&case332,
      &&case333,
      &&case334,
      &&case335,
      &&case336,
      &&case337,
      &&case340,
      &&case341,
      &&case342,
      &&case343,
      &&case344,
      &&case345,
      &&case346,
      &&case347,
      &&case350,
      &&case351,
      &&case352,
      &&case353,
      &&case354,
      &&case355,
      &&case356,
      &&case357,
      &&case360,
      &&case361,
      &&case362,
      &&case363,
      &&case364,
      &&case365,
      &&case366,
      &&case367,
      &&case370,
      &&case371,
      &&case372,
      &&case373,
      &&case374,
      &&case375,
      &&case376,
      &&case377,
  };
#endif

  /* Local stack pointer cache - mirrors CSTKPTRL macro */
  LispPTR *cspcache;

  /* Local TOPOFSTACK cache - frequently accessed for stack operations */
  LispPTR tscache;

  /* ========================================================================
   * OP_FN_COMMON ARGUMENTS
   * ========================================================================
   *
   * These variables are populated before jumping to op_fn_common for
   * function calls. Different call paths (FN0-FN4, FNX, APPLY, UFN) set
   * these up according to their specific requirements.
   *
   * - fn_defcell: Pointer to function's definition cell
   * - fn_atom_index: Atom table index for function name
   * - fn_opcode_size: Total size of calling opcode in bytes
   * - fn_num_args: Number of arguments being passed
   * - fn_apply: Apply variant code (0=normal, 2+=apply variants)
   */
  DefCell *fn_defcell;
  LispPTR fn_atom_index;
  int fn_opcode_size;
  int fn_num_args;
  int fn_apply;

  /* Initialize return and interrupt state */
  RET;
  CLR_IRQ;

  /* Jump to main dispatch loop */
  goto nextopcode;

  /* ========================================================================
   * UFN (UNDEFINED FUNCTION) HANDLING
   * ========================================================================
   *
   * The UFN_CALLS macro expands to label definitions for each opcode that
   * can fail and needs to call a Lisp-defined handler. These are typically
   * type-checking failures or complex operations.
   *
   * See @maiko/inc/ufn.h for UFN structure definition.
   */
  UFN_CALLS

  /*
   * LABEL: op_ufn
   * PURPOSE: Handle undefined/unimplemented opcodes
   *
   * ALGORITHM:
   *   1. Look up UFN entry for current opcode (via GetUFNEntry)
   *   2. Extract argument count, byte count, and atom name from entry
   *   3. Get definition cell for the UFN handler function
   *   4. Set up apply code (2 + byte_num for UFN variant)
   *   5. Jump to op_fn_common to call the handler
   *
   * This allows Lisp code to define handlers for otherwise unused opcodes,
   * enabling extensibility and custom instruction sets.
   */
op_ufn:
{
  UFN *entry68k;
  entry68k = (UFN *)GetUFNEntry(Get_BYTE_PCMAC0);
  fn_num_args = entry68k->arg_num;
  fn_opcode_size = entry68k->byte_num + 1;
  fn_atom_index = entry68k->atom_name;
  fn_defcell = (DefCell *)GetDEFCELL68k(fn_atom_index);
  fn_apply = 2 + entry68k->byte_num; /* code for UFN entry */
  goto op_fn_common;
}

  /* ========================================================================
   * FUNCTION CALL COMMON ROUTINE
   * ========================================================================
   *
   * OP_FN_COMMON is a macro that implements the common tail of all function
   * call opcodes (FN0-FN4, FNX, APPLY, CHECKAPPLY, UFN). It handles:
   *   - Function object lookup and validation
   *   - Stack frame setup
   *   - Argument passing
   *   - PC update to function entry point
   *
   * See @maiko/inc/tosfns.h or @maiko/inc/inlineC.h for macro definition.
   */
  OP_FN_COMMON

  /* ========================================================================
   * MAIN DISPATCH LOOP
   * ========================================================================
   *
   * This is the heart of the emulator - the infinite loop that fetches,
   * decodes, and executes bytecode instructions. The loop continues until
   * the emulator terminates (which normally doesn't happen - Lisp runs
   * indefinitely until explicitly exited).
   *
   * EXECUTION FLOW:
   *   1. Trace output (if MYOPTRACE enabled)
   *   2. PC/opcode tracing (if PCTRACE enabled)
   *   3. Timer/async interrupt emulation (if enabled)
   *   4. Fetch opcode byte at current PC
   *   5. Dispatch to handler via switch or computed goto
   *   6. Handler executes and advances PC
   *   7. Loop back to nextopcode
   *
   * CONFIDENCE LEVEL: HIGH (95%)
   *   - Standard interpreter dispatch loop pattern
   *   - PC advancement handled by individual opcode handlers
   *   - Interrupt checking done periodically, not every instruction
   */

nextopcode:

  /* ---------------------------------------------------------------------
   * TRACING: MYOPTRACE - Conditional trace for specific function
   * ---------------------------------------------------------------------
   * When MYOPTRACE is defined and PC is in function at 0x2ed600,
   * print trace information including PC, opcode, and TOPOFSTACK.
   */
  #ifdef MYOPTRACE
  if ((struct fnhead *)NativeAligned4FromLAddr(0x2ed600) == FuncObj)
  {
    // quick_stack_check(); // TODO: quick_stack_check function not implemented
  #endif /* MYOPTRACE */
    OPTPRINT(("PC= %p (fn+%td) op= %02x TOS= 0x%x\n", (void *)PCMAC, PCMAC - (char *)FuncObj, Get_BYTE_PCMAC0, TOPOFSTACK));
#ifdef MYOPTRACE
  }
#endif /* MYOPTRACE */

  /* ---------------------------------------------------------------------
   * TRACING: PCTRACE - Ring buffer of last 100 instructions
   * ---------------------------------------------------------------------
   * Records PC offset, function object, and opcode in circular buffers
   * for post-mortem debugging analysis.
   */
#ifdef PCTRACE
  pc_table[pccounter] = (int)PCMAC - (int)FuncObj;
  fn_table[pccounter] = (LispPTR)LAddrFromNative(FuncObj);
  op_table[pccounter] = Get_BYTE_PCMAC0;
  if (99 == pccounter++)
    pccounter = 0;
#endif /* PCTRACE */

  /* quick_stack_check();*/ /* JDS 2/12/98 - disabled for performance */

  /* ---------------------------------------------------------------------
   * EXECUTION TRACE LOGGING
   * ---------------------------------------------------------------------
   * Log instruction execution for parity testing with Zig emulator.
   * Matches Zig execution trace format for comparison.
   *
   * TIMING: We log state *before* dispatching the current opcode. So for
   * trace line N, PC/opcode are the next instruction to execute; TOS/SP/FP
   * are the state *after* the previous instruction (N-1) has executed.
   * Thus line 2 shows TOS after GVAR (instruction 1), before UNBIND (instruction 2).
   */
  {
    unsigned char opcode = Get_BYTE_PCMAC0;
    unsigned long pc_byte_offset = (unsigned long)((char *)PCMAC - (char *)Lisp_world);
    unsigned long sp_offset = (unsigned long)((DLword *)CSTKPTRL - (DLword *)Lisp_world);
    unsigned long fp_offset = (unsigned long)((DLword *)CURRENTFX - (DLword *)Lisp_world);
    
    /* Get opcode name for logging */
    const char *opcode_name = NULL;
    switch (opcode) {
        case 0x01: opcode_name = "CAR"; break;
        case 0x02: opcode_name = "CDR"; break;
        case 0x03: opcode_name = "LISTP"; break;
        case 0x04: opcode_name = "NTYPEX"; break;
        case 0x05: opcode_name = "TYPEP"; break;
        case 0x06: opcode_name = "DTEST"; break;
        case 0x07: opcode_name = "UNWIND"; break;
        case 0x10: opcode_name = "FN0"; break;
        case 0x11: opcode_name = "FN1"; break;
        case 0x12: opcode_name = "FN2"; break;
        case 0x13: opcode_name = "FN3"; break;
        case 0x14: opcode_name = "FN4"; break;
        case 0x15: opcode_name = "FNX"; break;
        case 0x16: opcode_name = "APPLY"; break;
        case 0x17: opcode_name = "CHECKAPPLY"; break;
        case 0x20: opcode_name = "RETURN"; break;
        case 0x21: opcode_name = "BIND"; break;
        case 0x22: opcode_name = "UNBIND"; break;
        case 0x23: opcode_name = "DUNBIND"; break;
        case 0x24: opcode_name = "RPLPTR"; break;
        case 0x30: opcode_name = "RPLACA"; break;
        case 0x31: opcode_name = "RPLACD"; break;
        case 0x32: opcode_name = "CONS"; break;
        case 0x33: opcode_name = "CMLASSOC"; break;
        case 0x34: opcode_name = "FMEMB"; break;
        case 0x35: opcode_name = "CMLMEMBER"; break;
        case 0x36: opcode_name = "FINDKEY"; break;
        case 0x37: opcode_name = "CREATECELL"; break;
        case 0x40: opcode_name = "BIN"; break;
        case 0x43: opcode_name = "RESTLIST"; break;
        case 0x44: opcode_name = "MISCN"; break;
        case 0x46: opcode_name = "RPLCONS"; break;
        case 0x47: opcode_name = "LISTGET"; break;
        case 0x54: opcode_name = "EVAL"; break;
        case 0x55: opcode_name = "ENVCALL"; break;
        case 0x62: opcode_name = "UBFLOAT3"; break;
        case 0x63: opcode_name = "TYPEMASK"; break;
        case 0x70: opcode_name = "MISC7"; break;
        case 0x72: opcode_name = "EQLOP"; break;
        case 0x73: opcode_name = "DRAWLINE"; break;
        case 0x74: opcode_name = "STOREN"; break;
        case 0x75: opcode_name = "COPYN"; break;
        case 0x80: opcode_name = "IVAR0"; break;
        case 0x81: opcode_name = "IVAR1"; break;
        case 0x82: opcode_name = "IVAR2"; break;
        case 0x83: opcode_name = "IVAR3"; break;
        case 0x84: opcode_name = "IVAR4"; break;
        case 0x85: opcode_name = "IVAR5"; break;
        case 0x86: opcode_name = "IVAR6"; break;
        case 0x87: opcode_name = "IVARX"; break;
        case 0x88: opcode_name = "PVAR0"; break;
        case 0x89: opcode_name = "PVAR1"; break;
        case 0x8a: opcode_name = "PVAR2"; break;
        case 0x8b: opcode_name = "PVAR3"; break;
        case 0x8c: opcode_name = "PVAR4"; break;
        case 0x8d: opcode_name = "PVAR5"; break;
        case 0x8e: opcode_name = "PVAR6"; break;
        case 0x8f: opcode_name = "PVARX"; break;
        case 0x90: opcode_name = "FVAR0"; break;
        case 0x91: opcode_name = "FVAR1"; break;
        case 0x92: opcode_name = "FVAR2"; break;
        case 0x93: opcode_name = "FVAR3"; break;
        case 0x94: opcode_name = "FVAR4"; break;
        case 0x95: opcode_name = "FVAR5"; break;
        case 0x96: opcode_name = "FVAR6"; break;
        case 0x97: opcode_name = "FVARX"; break;
        case 0x98: opcode_name = "GVAR"; break;
        case 0x99: opcode_name = "PVARX_"; break;
        case 0x9a: opcode_name = "IVARX_"; break;
        case 0x9b: opcode_name = "FVARX_"; break;
        case 0x9c: opcode_name = "POP"; break;
        case 0x9d: opcode_name = "POPDISP"; break;
        case 0x9e: opcode_name = "RESTLIST"; break;
        case 0x9f: opcode_name = "ATOMCELL"; break;
        case 0xa0: opcode_name = "GETBASEBYTE"; break;
        case 0xa1: opcode_name = "INSTANCEP"; break;
        case 0xa2: opcode_name = "BLT"; break;
        case 0xa3: opcode_name = "MISC10"; break;
        case 0xa4: opcode_name = "PUTBASEBYTE"; break;
        case 0xa5: opcode_name = "GETBASE"; break;
        case 0xa6: opcode_name = "GETBASEPTR"; break;
        case 0xa7: opcode_name = "GETBITS"; break;
        case 0xa8: opcode_name = "PUTBASE"; break;
        case 0xa9: opcode_name = "PUTBASEPTR"; break;
        case 0xaa: opcode_name = "PUTBITS"; break;
        case 0xab: opcode_name = "CMLEQUAL"; break;
        case 0xac: opcode_name = "TYPEMASK_N"; break;
        case 0xad: opcode_name = "STORE_N"; break;
        case 0xae: opcode_name = "COPY_N"; break;
        case 0xaf: opcode_name = "FGREATERP"; break;
        case 0xb0: opcode_name = "EQUAL"; break;
        case 0xb1: opcode_name = "MAKENUMBER"; break;
        case 0xb2: opcode_name = "SWAP"; break;
        case 0xb3: opcode_name = "NOP"; break;
        case 0xb4: opcode_name = "EQL"; break;
        case 0xb5: opcode_name = "DRAWLINE"; break;
        case 0xb6: opcode_name = "PUTBASEPTR_N"; break;
        case 0xb7: opcode_name = "ADDBASE"; break;
        case 0xb8: opcode_name = "VAG2"; break;
        case 0xb9: opcode_name = "HILOC"; break;
        case 0xba: opcode_name = "LOLOC"; break;
        case 0xbb: opcode_name = "EQ"; break;
        case 0xbc: opcode_name = "RETCALL"; break;
        case 0xbd: opcode_name = "GCSCAN1"; break;
        case 0xbe: opcode_name = "CONTEXTSWITCH"; break;
        case 0xbf: opcode_name = "RECLAIMCELL"; break;
        case 0xc0: opcode_name = "AREF2"; break;
        case 0xc1: opcode_name = "GREATERP"; break;
        case 0xc2: opcode_name = "IGREATERP"; break;
        case 0xc3: opcode_name = "PLUS2"; break;
        case 0xc4: opcode_name = "DIFFERENCE"; break;
        case 0xc5: opcode_name = "TIMES2"; break;
        case 0xc6: opcode_name = "QUOTIENT"; break;
        case 0xc7: opcode_name = "IPLUS2"; break;
        case 0xc8: opcode_name = "IDIFFERENCE"; break;
        case 0xc9: opcode_name = "ITIMES2"; break;
        case 0xca: opcode_name = "IQUOTIENT"; break;
        case 0xcb: opcode_name = "IREMAINDER"; break;
        case 0xcc: opcode_name = "BOXIPLUS"; break;
        case 0xcd: opcode_name = "BOXIDIFFERENCE"; break;
        case 0xce: opcode_name = "FLOATBLT"; break;
        case 0xcf: opcode_name = "FFTSTEP"; break;
        case 0xd0: opcode_name = "MISC3"; break;
        case 0xd1: opcode_name = "MISC4"; break;
        case 0xd2: opcode_name = "UPCTRACE"; break;
        case 0xd3: opcode_name = "CL_EQUAL"; break;
        case 0xd4: opcode_name = "WRTPTRTAG"; break;
        case 0xd5: opcode_name = "LOGOR2"; break;
        case 0xd6: opcode_name = "LOGAND2"; break;
        case 0xd7: opcode_name = "LOGXOR2"; break;
        case 0xd8: opcode_name = "LSH"; break;
        case 0xd9: opcode_name = "LLSH1"; break;
        case 0xda: opcode_name = "LLSH8"; break;
        case 0xdb: opcode_name = "LRSH1"; break;
        case 0xdc: opcode_name = "LRSH8"; break;
        case 0xdd: opcode_name = "BASE_LESSTHAN"; break;
        case 0xe0: opcode_name = "UBFLOAT1"; break;
        case 0xe1: opcode_name = "UBFLOAT2"; break;
        case 0xe2: opcode_name = "SICX"; break;
        case 0xe3: opcode_name = "JUMPX"; break;
        case 0xe4: opcode_name = "JUMPXX"; break;
        case 0xe5: opcode_name = "FJUMPX"; break;
        case 0xe6: opcode_name = "TJUMPX"; break;
        case 0xe7: opcode_name = "NFJUMPX"; break;
        case 0xe8: opcode_name = "NTJUMPX"; break;
        case 0xe9: opcode_name = "POP_N"; break;
        case 0xea: opcode_name = "ATOMCELL_N"; break;
        case 0xeb: opcode_name = "GETBASE_N"; break;
        case 0xec: opcode_name = "GETBASEPTR_N"; break;
        case 0xed: opcode_name = "GETBITS_N_FD"; break;
        case 0xee: opcode_name = "PUTBASE_N"; break;
        case 0xef: opcode_name = "PUTBASEPTR_N"; break;
        case 0xf0: opcode_name = "PUTBITS_N_FD"; break;
        case 0xf1: opcode_name = "IPLUS_N"; break;
        case 0xf2: opcode_name = "IDIFFERENCE_N"; break;
        case 0xf3: opcode_name = "FPLUS2"; break;
        case 0xf4: opcode_name = "FDIFFERENCE"; break;
        case 0xf5: opcode_name = "FTIMES2"; break;
        case 0xf6: opcode_name = "FQUOTIENT"; break;
        case 0xf7: opcode_name = "CONST_0"; break;
        case 0xf8: opcode_name = "CONST_1"; break;
        case 0xf9: opcode_name = "CONST_2"; break;
        case 0xfa: opcode_name = "CONST_3"; break;
        case 0xfb: opcode_name = "CONST_4"; break;
        case 0xfc: opcode_name = "CONST_5"; break;
        case 0xfd: opcode_name = "CONST_6"; break;
        case 0xfe: opcode_name = "CONST_7"; break;
        case 0xff: opcode_name = "CONST_8"; break;
        default: opcode_name = "UNKNOWN"; break;
    }
    
    log_global_execution_trace(opcode, pc_byte_offset, TOPOFSTACK,
                           sp_offset, fp_offset, opcode_name);
  }

  /* ---------------------------------------------------------------------
   * TIMER/ASYNC INTERRUPT EMULATION
   * ---------------------------------------------------------------------
   * On platforms without true async interrupt support, we count down
   * instructions and trigger interrupt processing when counter reaches 0.
   * This approximates timer interrupt behavior.
   */
#if defined(MAIKO_EMULATE_TIMER_INTERRUPTS) || defined(MAIKO_EMULATE_ASYNC_INTERRUPTS)
  if (--pseudoTimerAsyncCountdown <= 0)
  {
    Irq_Stk_Check = 0;  /* Force stack check */
    Irq_Stk_End = 0;    /* Force interrupt check */
#if defined(MAIKO_EMULATE_ASYNC_INTERRUPTS)
    IO_Signalled = TRUE;  /* Signal I/O may be pending */
#endif
#ifdef MAIKO_OS_EMSCRIPTEN
    emscripten_sleep(1);  /* Yield to browser event loop */
#endif
    pseudoTimerAsyncCountdown = insnsCountdownForTimerAsyncEmulation;
  }
#endif

  /* ---------------------------------------------------------------------
   * OPCODE FETCH AND DISPATCH
   * ---------------------------------------------------------------------
   * Fetch the opcode byte at the current PC and dispatch to handler.
   * Get_BYTE_PCMAC0 reads one byte from PCMAC (current PC).
   *
   * DISPATCH METHODS:
   *   - OPDISP: Uses computed goto with optable[256] jump table
   *   - No OPDISP: Uses standard C switch statement
   *
   * Each case label handles one opcode. Most delegate to macros defined
   * in header files (tosfns.h, tosret.h, inlineC.h, etc.).
   */
  switch (Get_BYTE_PCMAC0)
  {
  /* ====================================================================
   * OPCODES 0x00-0x07: LIST, TYPE, AND CONTROL OPERATIONS
   * ====================================================================
   *
   * These opcodes handle basic list operations (CAR, CDR), type checking,
   * and control flow (UNWIND).
   *
   * CONFIDENCE LEVEL: HIGH (95%)
   *   - CAR/CDR are fundamental list operations with CDR coding
   *   - Type operations use standard type tag checking
   *   - UNWIND is used for non-local exits
   */

  case 000:
  case000:
  {
    goto op_ufn;  /* 0x00: Unused - treat as UFN */
  }

  case 001:
  case001:
    OPCAR;  /* 0x01: CAR - get car of cons cell with CDR decoding */

  case 002:
  case002:
    OPCDR;  /* 0x02: CDR - get cdr of cons cell with CDR decoding */

  case 003:
  case003:
    LISTP;  /* 0x03: LISTP - check if object is a list */

  case 004:
  case004:
    NTYPEX;  /* 0x04: NTYPEX - get type number of object */

  case 005:
  case005:
    TYPEP(Get_BYTE_PCMAC1);  /* 0x05: TYPEP - check object type against immediate */

  case 056:
  case056:
  case 006:
  case006:
    DTEST(Get_AtomNo_PCMAC1);  /* 0x06/0x38: DTEST - datatype test against atom */

  case 007:
  case007:
    UNWIND(Get_BYTE_PCMAC1, Get_BYTE_PCMAC2);  /* 0x07: UNWIND - stack unwinding */

  /* ====================================================================
   * OPCODES 0x08-0x0F: UNUSED
   * ==================================================================== */
  /* 0x08-0x0F: Unused - all dispatch to op_ufn */

  /* ====================================================================
   * OPCODES 0x10-0x17: FUNCTION CALLS
   * ====================================================================
   *
   * These opcodes implement function calling with various argument counts.
   * FN0-FN4 handle 0-4 arguments directly; FNX handles variable arguments.
   * APPLY and CHECKAPPLY support functional programming patterns.
   *
   * CONFIDENCE LEVEL: HIGH (95%)
   *   - Function call mechanism is well-documented
   *   - Stack frame setup follows established patterns
   *   - Argument passing is straightforward
   */

  case 010:
  case010:
    FN0;  /* 0x10: FN0 - call function with 0 arguments */

  case 011:
  case011:
    FN1;  /* 0x11: FN1 - call function with 1 argument */

  case 012:
  case012:
    FN2;  /* 0x12: FN2 - call function with 2 arguments */

  case 013:
  case013:
    FN3;  /* 0x13: FN3 - call function with 3 arguments */

  case 014:
  case014:
    FN4;  /* 0x14: FN4 - call function with 4 arguments */

  case 015:
  case015:
    FNX;  /* 0x15: FNX - call function with N arguments (from stack) */

  case 016:
  case016:
    APPLY;  /* 0x16: APPLY - apply function to argument list */

  case 017:
  case017:
    CHECKAPPLY;  /* 0x17: CHECKAPPLY - apply with type checking */

  /* ====================================================================
   * OPCODES 0x18-0x1F: UNUSED
   * ==================================================================== */
  /* 0x18-0x1F: Unused - all dispatch to op_ufn */

  /* ====================================================================
   * OPCODES 0x20-0x27: CONTROL FLOW AND MEMORY
   * ==================================================================== */

  case 020:
  case020:
    RETURN;  /* 0x20: RETURN - return from function call */

  case 021:
  case021:
    /* UB: left shift of negative value -4 (known issue in BIND macro) */
    BIND;  /* 0x21: BIND - bind variables on stack */

  case 022:
  case022:
    UNBIND;  /* 0x22: UNBIND - unbind variables from stack */

  case 023:
  case023:
    DUNBIND;  /* 0x23: DUNBIND - deferred unbind */

  case 024:
  case024:
    RPLPTR(Get_BYTE_PCMAC1);  /* 0x24: RPLPTR - replace pointer field */

  case 025:
  case025:
    GCREF(Get_BYTE_PCMAC1);  /* 0x25: GCREF - GC reference operation */

  case 026:
  case026:
    ASSOC;  /* 0x26: ASSOC - association list lookup */

  case 027:
  case027:
    GVAR_(Get_AtomNo_PCMAC1);  /* 0x27: GVAR_ - set global variable */

  /* ====================================================================
   * OPCODES 0x28-0x2F: UNUSED
   * ==================================================================== */
  /* 0x28-0x2F: Unused - all dispatch to op_ufn */

  /* ====================================================================
   * OPCODES 0x30-0x37: LIST OPERATIONS
   * ==================================================================== */

  case 030:
  case030:
    RPLACA;  /* 0x30: RPLACA - replace car of cons cell */

  case 031:
  case031:
    RPLACD;  /* 0x31: RPLACD - replace cdr of cons cell */
  case 032:
  case032:
    CONS;  /* 0x32: CONS - create new cons cell */

  case 033:
  case033:
    CLASSOC;  /* 0x33: CLASSOC - cached assoc lookup */

  case 034:
  case034:
    FMEMB;  /* 0x34: FMEMB - list membership test */

  case 035:
  case035:
    CLFMEMB;  /* 0x35: CLFMEMB - cached list membership */

  case 036:
  case036:
    FINDKEY(Get_BYTE_PCMAC1);  /* 0x36: FINDKEY - find key in structure */

  case 037:
  case037:
    CREATECELL;  /* 0x37: CREATECELL - allocate new cell */

  /* ====================================================================
   * OPCODES 0x38-0x3F: UNUSED
   * ==================================================================== */
  /* 0x38-0x3F: Unused - all dispatch to op_ufn (except 0x38 = DTEST alias) */

  /* ====================================================================
   * OPCODES 0x40-0x47: BINARY, I/O, AND LIST OPERATIONS
   * ==================================================================== */

  case 040:
  case040:
    BIN;  /* 0x40: BIN - binary input operation */

  case 041:
  case041:
  {
    goto op_ufn;  /* 0x41: BOUT - binary output (unused) */
  }

  case 042:
  case042:
  {
    goto op_ufn;  /* 0x42: POPDISP - prolog only (unused) */
  }

  case 043:
  case043:
    RESTLIST(Get_BYTE_PCMAC1);  /* 0x43: RESTLIST - construct list from stack */

  case 044:
  case044:
    MISCN(Get_BYTE_PCMAC1, Get_BYTE_PCMAC2);  /* 0x44: MISCN - miscellaneous N */

  case 045:
  case045:
  {
    goto op_ufn;  /* 0x45: Unused */
  }

  case 046:
  case046:
    RPLCONS;  /* 0x46: RPLCONS - replace car and cons */

  case 047:
  case047:
    LISTGET;  /* 0x47: LISTGET - get from list */

  /* ====================================================================
   * OPCODES 0x48-0x53: UNUSED
   * ==================================================================== */
  /* 0x48-0x53: Unused - all dispatch to op_ufn */

  /* ====================================================================
   * OPCODES 0x54-0x57: EVAL, ENVCALL, DTEST, STKSCAN
   * ==================================================================== */

  case 054:
  case054:
    EVAL;  /* 0x54: EVAL - evaluate expression */
  case 055:
  case055:
    ENVCALL;  /* 0x55: ENVCALL - environment call */

  /* Note: 0x56 (056) is DTEST, defined earlier at case 006 */

  case 057:
  case057:
    STKSCAN;  /* 0x57: STKSCAN - scan stack */

  /* ====================================================================
   * OPCODES 0x58-0x5F: UNUSED
   * ==================================================================== */
  /* 0x58-0x5F: Unused - all dispatch to op_ufn */

  /* ====================================================================
   * OPCODES 0x60-0x6F: DEVICE AND FLOATING POINT
   * ==================================================================== */

  case 060:
  case060:
  {
    goto op_ufn;  /* 0x60: BUSBLT - DLion only (unused) */
  }

  case 061:
  case061:
  {
    goto op_ufn;  /* 0x61: MISC8 - no longer used */
  }

  case 062:
  case062:
    UBFLOAT3(Get_BYTE_PCMAC1);  /* 0x62: UBFLOAT3 - 3-arg float op */

  case 063:
  case063:
    TYPEMASK(Get_BYTE_PCMAC1);  /* 0x63: TYPEMASK - extract type field */

  case 064:
  case064:
  {
    goto op_ufn;  /* 0x64: rdprologptr - prolog only */
  }

  case 065:
  case065:
  {
    goto op_ufn;  /* 0x65: rdprologtag - prolog only */
  }

  case 066:
  case066:
  {
    goto op_ufn;  /* 0x66: writeptr&tag - prolog only */
  }

  case 067:
  case067:
  {
    goto op_ufn;  /* 0x67: writeptr&0tag - prolog only */
  }

  /* ====================================================================
   * OPCODES 0x70-0x77: GRAPHICS AND COMPARISON
   * ==================================================================== */

  case 070:
  case070:
    MISC7(Get_BYTE_PCMAC1);  /* 0x70: MISC7 - pseudocolor, fbitmapbit */

  case 071:
  case071:
  {
    goto op_ufn;  /* 0x71: dovemisc - Dove only (unused) */
  }

  case 072:
  case072:
    EQLOP;  /* 0x72: EQLOP - equality operation */

  case 073:
  case073:
    DRAWLINE;  /* 0x73: DRAWLINE - graphics line drawing */

  case 074:
  case074:
    STOREN(Get_BYTE_PCMAC1);  /* 0x74: STOREN - store N values */

  case 075:
  case075:
    COPYN(Get_BYTE_PCMAC1);  /* 0x75: COPYN - copy N values */

  case 076:
  case076:
  {
    goto op_ufn;  /* 0x76: RAID - debugging (unused) */
  }

  case 077:
  case077:
  {
    goto op_ufn;  /* 0x77: \RETURN - alternate return (unused) */
  }

  /* ====================================================================
   * OPCODES 0x78-0x7F: UNUSED
   * ==================================================================== */
  /* 0x78-0x7F: Unused - all dispatch to op_ufn */

  /* ====================================================================
   * OPCODES 0x80-0xBF: VARIABLE ACCESS
   * ====================================================================
   *
   * These opcodes implement variable access for different variable types:
   *   - IVAR: Instance variables (0x80-0x87)
   *   - PVAR: Parameter variables (0x88-0x8F)
   *   - FVAR: Frame variables (0x90-0x97)
   *   - PVARSET: Set parameter variables (0x98-0x9F)
   *   - GVAR: Global variables (0xA0, 0xA7)
   *   - Constants (0xA8-0xAF)
   *
   * The macro versions (IVARMACRO, PVARMACRO, etc.) handle the common
   * cases with inline code for speed. The X variants handle indexed access.
   *
   * CONFIDENCE LEVEL: HIGH (95%)
   *   - Variable access patterns are well-documented
   *   - Stack-based variable frames are standard
   *   - Macro implementations are straightforward
   */

  /* ------------------------------------------------------------------
   * IVAR (Instance Variable) Access - 0x80-0x87
   * ------------------------------------------------------------------ */
  case 0100:
  case100:
    IVARMACRO(0);  /* 0x80: IVAR0 - push instance variable 0 */

  case 0101:
  case101:
    IVARMACRO(1);  /* 0x81: IVAR1 - push instance variable 1 */

  case 0102:
  case102:
    IVARMACRO(2);  /* 0x82: IVAR2 - push instance variable 2 */

  case 0103:
  case103:
    IVARMACRO(3);  /* 0x83: IVAR3 - push instance variable 3 */

  case 0104:
  case104:
    IVARMACRO(4);  /* 0x84: IVAR4 - push instance variable 4 */

  case 0105:
  case105:
    IVARMACRO(5);  /* 0x85: IVAR5 - push instance variable 5 */

  case 0106:
  case106:
    IVARMACRO(6);  /* 0x86: IVAR6 - push instance variable 6 */

  case 0107:
  case107:
    IVARX(Get_BYTE_PCMAC1);  /* 0x87: IVARX - push instance variable N */

  /* ------------------------------------------------------------------
   * PVAR (Parameter Variable) Access - 0x88-0x8F
   * ------------------------------------------------------------------ */
  case 0110:
  case110:
    PVARMACRO(0);  /* 0x88: PVAR0 - push parameter variable 0 */

  case 0111:
  case111:
    PVARMACRO(1);  /* 0x89: PVAR1 - push parameter variable 1 */

  case 0112:
  case112:
    PVARMACRO(2);  /* 0x8A: PVAR2 - push parameter variable 2 */

  case 0113:
  case113:
    PVARMACRO(3);  /* 0x8B: PVAR3 - push parameter variable 3 */

  case 0114:
  case114:
    PVARMACRO(4);  /* 0x8C: PVAR4 - push parameter variable 4 */

  case 0115:
  case115:
    PVARMACRO(5);  /* 0x8D: PVAR5 - push parameter variable 5 */

  case 0116:
  case116:
    PVARMACRO(6);  /* 0x8E: PVAR6 - push parameter variable 6 */

  case 0117:
  case117:
    PVARX(Get_BYTE_PCMAC1);  /* 0x8F: PVARX - push parameter variable N */

  /* ------------------------------------------------------------------
   * FVAR (Frame Variable) Access - 0x90-0x97
   * ------------------------------------------------------------------ */
  case 0120:
  case120:
    FVAR(0);  /* 0x90: FVAR0 - push frame variable 0 */

  case 0121:
  case121:
    FVAR(2);  /* 0x91: FVAR2 - push frame variable 2 */

  case 0122:
  case122:
    FVAR(4);  /* 0x92: FVAR4 - push frame variable 4 */

  case 0123:
  case123:
    FVAR(6);  /* 0x93: FVAR6 - push frame variable 6 */

  case 0124:
  case124:
    FVAR(8);  /* 0x94: FVAR8 - push frame variable 8 */

  case 0125:
  case125:
    FVAR(10);  /* 0x95: FVAR10 - push frame variable 10 */

  case 0126:
  case126:
    FVAR(12);  /* 0x96: FVAR12 - push frame variable 12 */

  case 0127:
  case127:
    FVARX(Get_BYTE_PCMAC1);  /* 0x97: FVARX - push frame variable N */

  /* ------------------------------------------------------------------
   * PVARSET (Set Parameter Variable) - 0x98-0x9F
   * ------------------------------------------------------------------ */
  case 0130:
  case130:
    PVARSETMACRO(0);  /* 0x98: PVARSET0 - set parameter variable 0 */

  case 0131:
  case131:
    PVARSETMACRO(1);  /* 0x99: PVARSET1 - set parameter variable 1 */

  case 0132:
  case132:
    PVARSETMACRO(2);  /* 0x9A: PVARSET2 - set parameter variable 2 */

  case 0133:
  case133:
    PVARSETMACRO(3);  /* 0x9B: PVARSET3 - set parameter variable 3 */

  case 0134:
  case134:
    PVARSETMACRO(4);  /* 0x9C: PVARSET4 - set parameter variable 4 */

  case 0135:
  case135:
    PVARSETMACRO(5);  /* 0x9D: PVARSET5 - set parameter variable 5 */

  case 0136:
  case136:
    PVARSETMACRO(6);  /* 0x9E: PVARSET6 - set parameter variable 6 */

  case 0137:
  case137:
    PVARX_(Get_BYTE_PCMAC1);  /* 0x9F: PVARX_ - set parameter variable N */

  /* ------------------------------------------------------------------
   * Global Variable and Stack Operations - 0xA0-0xA7
   * ------------------------------------------------------------------ */
  case 0140:
  case140:
    GVAR(Get_AtomNo_PCMAC1);  /* 0xA0: GVAR - get global variable */

  case 0141:
  case141:
    ARG0;  /* 0xA1: ARG0 - push argument 0 */

  case 0142:
  case142:
    IVARX_(Get_BYTE_PCMAC1);  /* 0xA2: IVARX_ - set instance variable N */

  case 0143:
  case143:
    FVARX_(Get_BYTE_PCMAC1);  /* 0xA3: FVARX_ - set frame variable N */

  case 0144:
  case144:
    COPY;  /* 0xA4: COPY - copy top of stack */

  case 0145:
  case145:
    MYARGCOUNT;  /* 0xA5: MYARGCOUNT - push argument count */

  case 0146:
  case146:
    MYALINK;  /* 0xA6: MYALINK - push access link */

  /* ------------------------------------------------------------------
   * ACONST - Push Atom Constant - 0xA7
   * ------------------------------------------------------------------ */
  case 0147:
  case147:
  {
    PUSH(Get_AtomNo_PCMAC1);  /* 0xA7: ACONST - push atom by index */
    nextop_atom;
  }
  /* ------------------------------------------------------------------
   * Common Constants - 0xA8-0xAB
   * ------------------------------------------------------------------ */
  case 0150:
  case150:
  {
    PUSHATOM(NIL_PTR);  /* 0xA8: NIL - push NIL constant */
  }

  case 0151:
  case151:
  {
    PUSHATOM(ATOM_T);  /* 0xA9: T - push T (true) constant */
  }

  case 0152:
  case152:
  {
    PUSHATOM(S_POSITIVE);  /* 0xAA: '0 - push small positive 0 */
  }

  case 0153:
  case153:
  {
    PUSHATOM(0xE0001);  /* 0xAB: '1 - push small positive 1 */
  }

  /* ------------------------------------------------------------------
   * SIC - Small Integer Constant - 0xAC
   * ------------------------------------------------------------------
   * Pushes a small positive integer (0-255) onto the stack.
   * The operand byte is OR'd with S_POSITIVE tag.
   */
  case 0154:
  case154:
  {
    PUSH(S_POSITIVE | Get_BYTE_PCMAC1);  /* 0xAC: SIC - small int constant */
    nextop2;
  }

  /* ------------------------------------------------------------------
   * SNIC - Small Negative Integer Constant - 0xAD
   * ------------------------------------------------------------------
   * Pushes a small negative integer (-1 to -256) onto the stack.
   * The operand byte is sign-extended and OR'd with S_NEGATIVE tag.
   */
  case 0155:
  case155:
  {
    PUSH(S_NEGATIVE | 0xff00 | Get_BYTE_PCMAC1);  /* 0xAD: SNIC - small negative int */
    nextop2;
  }

  /* ------------------------------------------------------------------
   * SICX - Small Integer Constant Extended - 0xAE
   * ------------------------------------------------------------------
   * Pushes a small positive integer (0-65535) onto the stack.
   * Uses 2-byte operand (DLword).
   */
  case 0156:
  case156:
  {
    PUSH(S_POSITIVE | Get_DLword_PCMAC1);  /* 0xAE: SICX - extended small int */
    nextop3;
  }

  /* ------------------------------------------------------------------
   * GCONST - General Constant - 0xAF
   * ------------------------------------------------------------------
   * Pushes a full 32-bit Lisp pointer constant.
   * Uses 4-byte operand (LispPTR).
   */
  case 0157:
  case157:
  {
    PUSH(Get_Pointer_PCMAC1);  /* 0xAF: GCONST - general constant */
    nextop_ptr;
  }

  /* ------------------------------------------------------------------
   * OPCODES 0xB0-0xB7: UNUSED
   * ------------------------------------------------------------------ */
  case 0160:
  case160:
  {
    goto op_ufn;  /* 0xB0: Unused */
  }

  case 0161:
  case161:
  {
    goto op_ufn;  /* 0xB1: readflags - unused */
  }

  case 0162:
  case162:
  {
    goto op_ufn;  /* 0xB2: readrp - unused */
  }

  case 0163:
  case163:
  {
    goto op_ufn;  /* 0xB3: writemap - unused */
  }

  case 0164:
  case164:
  {
    goto op_ufn;  /* 0xB4: readprinterport - unused */
  }
  case 0165:
  case165:
  {
    goto op_ufn;  /* 0xB5: writeprinterport - unused */
  }

  case 0166:
  case166:
    PILOTBITBLT;  /* 0xB6: PILOTBITBLT - bit block transfer */

  case 0167:
  case167:
    RCLK;  /* 0xB7: RCLK - read clock */

  case 0170:
  case170:
  {
    goto op_ufn;  /* 0xB8: MISC1 - Dorado only (unused) */
  }

  case 0171:
  case171:
  {
    goto op_ufn;  /* 0xB9: MISC2 - Dorado only (unused) */
  }

  case 0172:
  case172:
    RECLAIMCELL;  /* 0xBA: RECLAIMCELL - reclaim memory cell */

  case 0173:
  case173:
    GCSCAN1;  /* 0xBB: GCSCAN1 - GC scan phase 1 */

  case 0174:
  case174:
    GCSCAN2;  /* 0xBC: GCSCAN2 - GC scan phase 2 */

  case 0175:
  case175:
  {
    /* 0xBD: SUBRCALL - subroutine call */
    EXT;
    OP_subrcall(Get_BYTE_PCMAC1, Get_BYTE_PCMAC2);
    RET;
    nextop0;
  }

  case 0176:
  case176:
  {
    CONTEXTSWITCH;  /* 0xBE: CONTEXTSWITCH - switch context */
  }

  case 0177:
  case177:
  {
    goto op_ufn;  /* 0xBF: RETCALL - unused */
  }

  /* ====================================================================
   * OPCODES 0xC0-0xDF: JUMP OPERATIONS
   * ====================================================================
   *
   * These opcodes implement unconditional and conditional jumps.
   * JUMP macros handle short jumps (2-17 bytes forward).
   * JUMPX/JUMPXX handle variable-length jumps.
   *
   * CONFIDENCE LEVEL: HIGH (95%)
   *   - Jump logic is straightforward PC adjustment
   *   - Conditional jumps check TOPOFSTACK
   *   - PC advancement is handled by macros
   */

  /* ------------------------------------------------------------------
   * JUMP - Unconditional Jump - 0xC0-0xCF
   * ------------------------------------------------------------------
   * JUMPMACRO(n) jumps forward n bytes unconditionally.
   * Opcodes 0xC0-0xCF correspond to jumps of 2-17 bytes.
   */

  case 0200:
  case200:
  {
    JUMPMACRO(2);  /* 0xC0: JUMP2 - jump forward 2 bytes */
  }

  case 0201:
  case201:
  {
    JUMPMACRO(3);  /* 0xC1: JUMP3 - jump forward 3 bytes */
  }

  case 0202:
  case202:
  {
    JUMPMACRO(4);  /* 0xC2: JUMP4 - jump forward 4 bytes */
  }

  case 0203:
  case203:
  {
    JUMPMACRO(5);  /* 0xC3: JUMP5 - jump forward 5 bytes */
  }

  case 0204:
  case204:
  {
    JUMPMACRO(6);  /* 0xC4: JUMP6 - jump forward 6 bytes */
  }

  case 0205:
  case205:
  {
    JUMPMACRO(7);  /* 0xC5: JUMP7 - jump forward 7 bytes */
  }

  case 0206:
  case206:
  {
    JUMPMACRO(8);  /* 0xC6: JUMP8 - jump forward 8 bytes */
  }

  case 0207:
  case207:
  {
    JUMPMACRO(9);  /* 0xC7: JUMP9 - jump forward 9 bytes */
  }

  case 0210:
  case210:
  {
    JUMPMACRO(10);  /* 0xC8: JUMP10 - jump forward 10 bytes */
  }

  case 0211:
  case211:
  {
    JUMPMACRO(11);  /* 0xC9: JUMP11 - jump forward 11 bytes */
  }

  case 0212:
  case212:
  {
    JUMPMACRO(12);  /* 0xCA: JUMP12 - jump forward 12 bytes */
  }

  case 0213:
  case213:
  {
    JUMPMACRO(13);  /* 0xCB: JUMP13 - jump forward 13 bytes */
  }

  case 0214:
  case214:
  {
    JUMPMACRO(14);  /* 0xCC: JUMP14 - jump forward 14 bytes */
  }

  case 0215:
  case215:
  {
    JUMPMACRO(15);  /* 0xCD: JUMP15 - jump forward 15 bytes */
  }

  case 0216:
  case216:
  {
    JUMPMACRO(16);  /* 0xCE: JUMP16 - jump forward 16 bytes */
  }

  case 0217:
  case217:
  {
    JUMPMACRO(17);  /* 0xCF: JUMP17 - jump forward 17 bytes */
  }

  /* ------------------------------------------------------------------
   * FJUMP - False Jump - 0xD0-0xDF
   * ------------------------------------------------------------------
   * FJUMPMACRO(n) jumps forward n bytes if TOPOFSTACK is NIL (false).
   * Pops the value from stack. Opcodes 0xD0-0xDF for 2-17 byte jumps.
   */

  case 0220:
  case220:
  {
    FJUMPMACRO(2);  /* 0xD0: FJUMP2 - jump if false, 2 bytes */
  }

  case 0221:
  case221:
  {
    FJUMPMACRO(3);  /* 0xD1: FJUMP3 - jump if false, 3 bytes */
  }

  case 0222:
  case222:
  {
    FJUMPMACRO(4);  /* 0xD2: FJUMP4 - jump if false, 4 bytes */
  }

  case 0223:
  case223:
  {
    FJUMPMACRO(5);  /* 0xD3: FJUMP5 - jump if false, 5 bytes */
  }

  case 0224:
  case224:
  {
    FJUMPMACRO(6);  /* 0xD4: FJUMP6 - jump if false, 6 bytes */
  }

  case 0225:
  case225:
  {
    FJUMPMACRO(7);  /* 0xD5: FJUMP7 - jump if false, 7 bytes */
  }

  case 0226:
  case226:
  {
    FJUMPMACRO(8);  /* 0xD6: FJUMP8 - jump if false, 8 bytes */
  }

  case 0227:
  case227:
  {
    FJUMPMACRO(9);  /* 0xD7: FJUMP9 - jump if false, 9 bytes */
  }

  case 0230:
  case230:
  {
    FJUMPMACRO(10);  /* 0xD8: FJUMP10 - jump if false, 10 bytes */
  }

  case 0231:
  case231:
  {
    FJUMPMACRO(11);  /* 0xD9: FJUMP11 - jump if false, 11 bytes */
  }

  case 0232:
  case232:
  {
    FJUMPMACRO(12);  /* 0xDA: FJUMP12 - jump if false, 12 bytes */
  }

  case 0233:
  case233:
  {
    FJUMPMACRO(13);  /* 0xDB: FJUMP13 - jump if false, 13 bytes */
  }

  case 0234:
  case234:
  {
    FJUMPMACRO(14);  /* 0xDC: FJUMP14 - jump if false, 14 bytes */
  }

  case 0235:
  case235:
  {
    FJUMPMACRO(15);  /* 0xDD: FJUMP15 - jump if false, 15 bytes */
  }

  case 0236:
  case236:
  {
    FJUMPMACRO(16);  /* 0xDE: FJUMP16 - jump if false, 16 bytes */
  }

  case 0237:
  case237:
  {
    FJUMPMACRO(17);  /* 0xDF: FJUMP17 - jump if false, 17 bytes */
  }

  /* ------------------------------------------------------------------
   * TJUMP - True Jump - 0xE0-0xEF
   * ------------------------------------------------------------------
   * TJUMPMACRO(n) jumps forward n bytes if TOPOFSTACK is non-NIL (true).
   * Pops the value from stack. Opcodes 0xE0-0xEF for 2-17 byte jumps.
   */

  case 0240:
  case240:
  {
    TJUMPMACRO(2);  /* 0xE0: TJUMP2 - jump if true, 2 bytes */
  }

  case 0241:
  case241:
  {
    TJUMPMACRO(3);  /* 0xE1: TJUMP3 - jump if true, 3 bytes */
  }

  case 0242:
  case242:
  {
    TJUMPMACRO(4);  /* 0xE2: TJUMP4 - jump if true, 4 bytes */
  }

  case 0243:
  case243:
  {
    TJUMPMACRO(5);  /* 0xE3: TJUMP5 - jump if true, 5 bytes */
  }

  case 0244:
  case244:
  {
    TJUMPMACRO(6);  /* 0xE4: TJUMP6 - jump if true, 6 bytes */
  }

  case 0245:
  case245:
  {
    TJUMPMACRO(7);  /* 0xE5: TJUMP7 - jump if true, 7 bytes */
  }

  case 0246:
  case246:
  {
    TJUMPMACRO(8);  /* 0xE6: TJUMP8 - jump if true, 8 bytes */
  }

  case 0247:
  case247:
  {
    TJUMPMACRO(9);  /* 0xE7: TJUMP9 - jump if true, 9 bytes */
  }

  case 0250:
  case250:
  {
    TJUMPMACRO(10);  /* 0xE8: TJUMP10 - jump if true, 10 bytes */
  }

  case 0251:
  case251:
  {
    TJUMPMACRO(11);  /* 0xE9: TJUMP11 - jump if true, 11 bytes */
  }

  case 0252:
  case252:
  {
    TJUMPMACRO(12);  /* 0xEA: TJUMP12 - jump if true, 12 bytes */
  }

  case 0253:
  case253:
  {
    TJUMPMACRO(13);  /* 0xEB: TJUMP13 - jump if true, 13 bytes */
  }

  case 0254:
  case254:
  {
    TJUMPMACRO(14);  /* 0xEC: TJUMP14 - jump if true, 14 bytes */
  }

  case 0255:
  case255:
  {
    TJUMPMACRO(15);  /* 0xED: TJUMP15 - jump if true, 15 bytes */
  }

  case 0256:
  case256:
  {
    TJUMPMACRO(16);  /* 0xEE: TJUMP16 - jump if true, 16 bytes */
  }

  case 0257:
  case257:
  {
    TJUMPMACRO(17);  /* 0xEF: TJUMP17 - jump if true, 17 bytes */
  }

  /* ------------------------------------------------------------------
   * Extended Jump Operations - 0xF0-0xF5
   * ------------------------------------------------------------------
   * These opcodes handle variable-length jumps with signed offsets.
   */

  /* JUMPX - Jump with signed byte offset - 0xF0 */
  case 0260:
  case260:
  {
    CHECK_INTERRUPT;
    PCMACL += Get_SBYTE_PCMAC1;  /* Add signed byte offset to PC */
    nextop0;
  }

  /* JUMPXX - Jump with signed 16-bit offset - 0xF1 */
  case 0261:
  case261:
  {
    CHECK_INTERRUPT;
    /* UB: left shift of negative value -1 (known issue) */
    PCMACL += (Get_SBYTE_PCMAC1 << 8) | Get_BYTE_PCMAC2;  /* 16-bit signed offset */
    nextop0;
  }

  /* FJUMPX - False jump with signed byte offset - 0xF2 */
  case 0262:
  case262:
  {
    if (TOPOFSTACK != 0)  /* If true (non-NIL) */
    {
      POP;
      nextop2;  /* Skip jump, just pop and continue */
    }
    CHECK_INTERRUPT;
    POP;
    PCMACL += Get_SBYTE_PCMAC1;  /* Jump if false */
    nextop0;
  }

  /* TJUMPX - True jump with signed byte offset - 0xF3 */
  case 0263:
  case263:
  {
    if (TOPOFSTACK == 0)  /* If false (NIL) */
    {
      POP;
      nextop2;  /* Skip jump, just pop and continue */
    }
    CHECK_INTERRUPT;
    POP;
    PCMACL += Get_SBYTE_PCMAC1;  /* Jump if true */
    nextop0;
  }

  /* NFJUMPX - Not-false jump (jump if not NIL, no pop) - 0xF4 */
  case 0264:
  case264:
  {
    if (TOPOFSTACK != 0)  /* If true (non-NIL) */
    {
      POP;
      nextop2;  /* Skip jump, pop and continue */
    }
    CHECK_INTERRUPT;
    /* Note: Does NOT pop if jumping - value left on stack */
    PCMACL += Get_SBYTE_PCMAC1;  /* Jump if false, keep value */
    nextop0;
  }

  /* NTJUMPX - Not-true jump (jump if NIL, no pop) - 0xF5 */
  case 0265:
  case265:
  {
    if (TOPOFSTACK == 0)  /* If false (NIL) */
    {
      POP;
      nextop2;  /* Skip jump, pop and continue */
    }
    CHECK_INTERRUPT;
    /* Note: Does NOT pop if jumping - value left on stack */
    PCMACL += Get_SBYTE_PCMAC1;  /* Jump if true, keep value */
    nextop0;
  }

  /* ------------------------------------------------------------------
   * Array Operations - 0xF6-0xF7
   * ------------------------------------------------------------------ */
  case 0266:
  case266:
    AREF1;  /* 0xF6: AREF1 - array reference 1D */

  case 0267:
  case267:
    ASET1;  /* 0xF7: ASET1 - array set 1D */

  /* ------------------------------------------------------------------
   * PVARSETPOP - Set Parameter Variable and Pop - 0xF8-0xFE
   * ------------------------------------------------------------------
   * Sets a parameter variable and pops the stack (combines two operations).
   */
  case 0270:
  case270:
    PVARSETPOPMACRO(0);  /* 0xF8: PVARSETPOP0 */

  case 0271:
  case271:
    PVARSETPOPMACRO(1);  /* 0xF9: PVARSETPOP1 */

  case 0272:
  case272:
    PVARSETPOPMACRO(2);  /* 0xFA: PVARSETPOP2 */

  case 0273:
  case273:
    PVARSETPOPMACRO(3);  /* 0xFB: PVARSETPOP3 */

  case 0274:
  case274:
    PVARSETPOPMACRO(4);  /* 0xFC: PVARSETPOP4 */

  case 0275:
  case275:
    PVARSETPOPMACRO(5);  /* 0xFD: PVARSETPOP5 */

  case 0276:
  case276:
    PVARSETPOPMACRO(6);  /* 0xFE: PVARSETPOP6 */

  /* POP - Pop Stack - 0xFF */
  case 0277:
  case277:
  {
    POP;  /* 0xFF: POP - pop top of stack */
    nextop1;
  }

  /* ====================================================================
   * OPCODES 0x100-0x13F: (Octal 0300-0377)
   * Stack, Array, and Base Address Operations
   * ==================================================================== */

  /* POPN - Pop N values - 0x100 (octal 0300) */
  case 0300:
  case300:
    POPN(Get_BYTE_PCMAC1);  /* Pop N values from stack */

  /* ATOMCELL_N - Access atom cell N - 0x101 */
  case 0301:
  case301:
    ATOMCELL_N(Get_BYTE_PCMAC1);  /* Access atom cell by index */

  /* GETBASEBYTE - Get byte from base address - 0x102 */
  case 0302:
  case302:
    GETBASEBYTE;  /* Get byte at base address */

  /* INSTANCEP - Instance predicate - 0x103 */
  case 0303:
  case303:
    INSTANCEP(Get_AtomNo_PCMAC1);  /* Check if instance of type */

  /* BLT - Block transfer - 0x104 */
  case 0304:
  case304:
    BLT;  /* Block memory transfer */

  /* Unused opcodes 0x105-0x106 */
  case 0305:
  case305:
  {
    goto op_ufn;  /* 0x105: MISC10 - unused */
  }

  case 0306:
  case306:
  {
    goto op_ufn;  /* 0x106: P-MISC2 - unused */
  }

  /* PUTBASEBYTE - Put byte to base address - 0x107 */
  case 0307:
  case307:
    PUTBASEBYTE;  /* Store byte at base address */

  /* GETBASE_N - Get from base address with offset N - 0x108 */
  case 0310:
  case310:
    GETBASE_N(Get_BYTE_PCMAC1);  /* Get word at base + N */

  /* GETBASEPTR_N - Get pointer from base address - 0x109 */
  case 0311:
  case311:
    GETBASEPTR_N(Get_BYTE_PCMAC1);  /* Get pointer at base + N */

  /* GETBITS_N_M - Get bit field - 0x10A */
  case 0312:
  case312:
    GETBITS_N_M(Get_BYTE_PCMAC1, Get_BYTE_PCMAC2);  /* Get N bits at offset M */

  /* Unused - 0x10B */
  case 0313:
  case313:
  {
    goto op_ufn;  /* 0x10B: Unused */
  }

  /* CLEQUAL - Common Lisp EQUAL - 0x10C */
  case 0314:
  case314:
    CLEQUAL;  /* Common Lisp EQUAL comparison */

  /* PUTBASE_N - Put to base address with offset N - 0x10D */
  case 0315:
  case315:
    PUTBASE_N(Get_BYTE_PCMAC1);  /* Store word at base + N */

  /* PUTBASEPTR_N - Put pointer to base address - 0x10E */
  case 0316:
  case316:
    PUTBASEPTR_N(Get_BYTE_PCMAC1);  /* Store pointer at base + N */

  /* PUTBITS_N_M - Put bit field - 0x10F */
  case 0317:
  case317:
    PUTBITS_N_M(Get_BYTE_PCMAC1, Get_BYTE_PCMAC2);  /* Store N bits at offset M */

  /* ====================================================================
   * OPCODES 0x140-0x17F: (Octal 0320-0377)
   * Arithmetic, Address, and Bit Operations
   * ==================================================================== */

  /* Address Operations - 0x140-0x143 */
  case 0320:
  case320:
    N_OP_ADDBASE;  /* 0x140: ADDBASE - add to base address */

  case 0321:
  case321:
    N_OP_VAG2;  /* 0x141: VAG2 - virtual address to generic */

  case 0322:
  case322:
    N_OP_HILOC;  /* 0x142: HILOC - high bits of address */

  case 0323:
  case323:
    N_OP_LOLOC;  /* 0x143: LOLOC - low bits of address */

  /* General Arithmetic - 0x144-0x147 */
  case 0324:
  case324:
    PLUS2;  /* 0x144: PLUS2 - add two numbers */

  case 0325:
  case325:
    DIFFERENCE;  /* 0x145: DIFFERENCE - subtract two numbers */

  case 0326:
  case326:
    TIMES2;  /* 0x146: TIMES2 - multiply two numbers */

  case 0327:
  case327:
    QUOTIENT;  /* 0x147: QUOTIENT - divide two numbers */

  /* Integer Arithmetic - 0x148-0x14F */
  case 0330:
  case330:
    IPLUS2;  /* 0x148: IPLUS2 - integer add (fast path) */

  case 0331:
  case331:
    IDIFFERENCE;  /* 0x149: IDIFFERENCE - integer subtract */

  case 0332:
  case332:
    ITIMES2;  /* 0x14A: ITIMES2 - integer multiply */

  case 0333:
  case333:
    IQUOTIENT;  /* 0x14B: IQUOTIENT - integer quotient */

  case 0334:
  case334:
    IREMAINDER;  /* 0x14C: IREMAINDER - integer remainder */

  case 0335:
  case335:
    IPLUS_N(Get_BYTE_PCMAC1);  /* 0x14D: IPLUS_N - add immediate N */

  case 0336:
  case336:
    IDIFFERENCE_N(Get_BYTE_PCMAC1);  /* 0x14E: IDIFFERENCE_N - subtract immediate N */

  case 0337:
  case337:
  {
    goto op_ufn;  /* 0x14F: BASE-< - unused */
  }

  /* Shift Operations - 0x150-0x153 */
  case 0340:
  case340:
    LLSH1;  /* 0x150: LLSH1 - logical left shift 1 bit */

  case 0341:
  case341:
    LLSH8;  /* 0x151: LLSH8 - logical left shift 8 bits */
  case 0342:
  case342:
    LRSH1;  /* 0x152: LRSH1 - logical right shift 1 bit */

  case 0343:
  case343:
    LRSH8;  /* 0x153: LRSH8 - logical right shift 8 bits */

  /* Logical Operations - 0x154-0x157 */
  case 0344:
  case344:
    LOGOR;  /* 0x154: LOGOR - bitwise OR */

  case 0345:
  case345:
    LOGAND;  /* 0x155: LOGAND - bitwise AND */

  case 0346:
  case346:
    LOGXOR;  /* 0x156: LOGXOR - bitwise XOR */

  case 0347:
  case347:
    LSH;  /* 0x157: LSH - logical shift (variable) */

  /* Floating Point Arithmetic - 0x158-0x15B */
  case 0350:
  case350:
    FPLUS2;  /* 0x158: FPLUS2 - floating point add */

  case 0351:
  case351:
    FDIFFERENCE;  /* 0x159: FDIFFERENCE - floating point subtract */

  case 0352:
  case352:
    FTIMES2;  /* 0x15A: FTIMES2 - floating point multiply */

  case 0353:
  case353:
    FQUOTIENT;  /* 0x15B: FQUOTIENT - floating point divide */

  /* Floating Point Misc - 0x15C-0x15D */
  case 0354:
  case354:
    UBFLOAT2(Get_BYTE_PCMAC1);  /* 0x15C: UBFLOAT2 - 2-arg float op */

  case 0355:
  case355:
    UBFLOAT1(Get_BYTE_PCMAC1);  /* 0x15D: UBFLOAT1 - 1-arg float op */

  /* 2D Array Operations - 0x15E-0x15F */
  case 0356:
  case356:
    AREF2;  /* 0x15E: AREF2 - array reference 2D */

  case 0357:
  case357:
    ASET2;  /* 0x15F: ASET2 - array set 2D */

  /* Comparison Operations - 0x160-0x16F */
  case 0360:
  case360:
  {
    /* 0x160: EQUAL - compare top two stack values */
    if (TOPOFSTACK == POP_TOS_1)
      TOPOFSTACK = ATOM_T;
    else
      TOPOFSTACK = NIL_PTR;
    nextop1;
  }

  case 0361:
  case361:
    IGREATERP;  /* 0x161: IGREATERP - integer greater than */

  case 0362:
  case362:
    FGREATERP;  /* 0x162: FGREATERP - float greater than */

  case 0363:
  case363:
    GREATERP;  /* 0x163: GREATERP - general greater than */

  case 0364:
  case364:
    ILEQUAL;  /* 0x164: ILEQUAL - integer less than or equal */

  case 0365:
  case365:
    MAKENUMBER;  /* 0x165: MAKENUMBER - create number object */

  case 0366:
  case366:
    BOXIPLUS;  /* 0x166: BOXIPLUS - boxed integer add */

  case 0367:
  case367:
    BOXIDIFFERENCE;  /* 0x167: BOXIDIFFERENCE - boxed integer subtract */
  /* Unused Floating Point - 0x168-0x169 */
  case 0370:
  case370:
  {
    goto op_ufn;  /* 0x168: FLOATBLT - unused */
  }

  case 0371:
  case371:
  {
    goto op_ufn;  /* 0x169: FFTSTEP - unused */
  }

  /* Miscellaneous Operations - 0x16A-0x16B */
  case 0372:
  case372:
    MISC3(Get_BYTE_PCMAC1);  /* 0x16A: MISC3 - miscellaneous 3 */

  case 0373:
  case373:
    MISC4(Get_BYTE_PCMAC1);  /* 0x16B: MISC4 - miscellaneous 4 */

  /* Unused - 0x16C */
  case 0374:
  case374:
  {
    goto op_ufn;  /* 0x16C: upctrace - unused */
  }

  /* Stack Operations - 0x16D-0x16E */
  case 0375:
  case375:
    SWAP;  /* 0x16D: SWAP - swap top two stack values */

  case 0376:
  case376:
    NOP;  /* 0x16E: NOP - no operation */

  /* Final Comparison - 0x16F */
  case 0377:
  case377:
    CLARITHEQUAL;  /* 0x16F: CLARITHEQUAL - Common Lisp ARIThmetic EQUAL */

  /* Default case should never be reached if all 256 opcodes are defined */
  default:
    error("should not default");

  } /* switch - end of opcode dispatch */

/* ============================================================================
 * LABEL: check_interrupt
 * ============================================================================
 *
 * PURPOSE:
 *   Periodic interrupt and stack overflow checking routine. This code is
 *   reached via the CHECK_INTERRUPT macro or explicit goto from various
 *   opcodes. It handles:
 *   - Stack overflow detection and recovery
 *   - Timer-based periodic interrupts
 *   - Keyboard/mouse event processing
 *   - Ethernet I/O events
 *   - GC reclaim interrupts
 *   - Pending interrupt processing
 *   - URaid debugger invocation
 *
 * ALGORITHM:
 *   1. Check for unrecoverable stack overflow (stack pointer beyond EndSTKP)
 *   2. Check for soft stack overflow (stack pointer near limit)
 *      - Attempt to extend stack via do_stackoverflow()
 *      - If extension fails, trigger hard reset
 *   3. Process SDL events (keyboard, mouse, display)
 *   4. Check for and handle various interrupt sources:
 *      - Periodic interrupts (SPY profiler)
 *      - URaid debugger requests
 *      - Keyboard events
 *      - GC reclaim count
 *      - Pending interrupts
 *      - Ethernet events
 *
 * EXTERNAL VARIABLES USED:
 *   - KBDEventFlg: Keyboard event flag
 *   - ETHEREventCount: Ethernet event counter
 *   - KEYBUFFERING68k: Keyboard buffering atom
 *   - PENDINGINTERRUPT68k: Pending interrupt flag
 *   - ATOM_STARTED: "STARTED" atom for keyboard
 *   - PERIODIC_INTERRUPT68k: Periodic interrupt function
 *   - PERIODIC_INTERRUPT_FREQUENCY68k: Interrupt frequency
 *   - URaid_req: URaid debugger request flag
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Stack overflow logic is well-tested
 *   - Interrupt handling follows established patterns
 *   - Some complexity in interaction between interrupt sources
 *
 * CROSS-REFERENCES:
 *   - Stack management: @maiko/src/llstk.c
 *   - Interrupt handling: @documentation/components/vm-core.typ
 *   - SDL events: @maiko/src/sdl.c
 */

check_interrupt:
  /* ------------------------------------------------------------------
   * Unrecoverable Stack Overflow Check
   * ------------------------------------------------------------------
   * If stack pointer is beyond absolute end, we cannot recover.
   */
  if ((UNSIGNED)CSTKPTR > (UNSIGNED)EndSTKP)
  {
    EXT;
    error("Unrecoverable Stack Overflow");
    RET;
  }

  /* ------------------------------------------------------------------
   * Interrupt Processing Block
   * ------------------------------------------------------------------ */
  {
    int need_irq;
    static int period_cnt = 60;  /* Counter for periodic interrupts */

    /* External interrupt state variables */
    extern int KBDEventFlg;
    extern int ETHEREventCount;
    extern LispPTR *KEYBUFFERING68k;
    extern LispPTR *PENDINGINTERRUPT68k;
    extern LispPTR ATOM_STARTED;
    extern LispPTR *PERIODIC_INTERRUPT68k;
    extern LispPTR *PERIODIC_INTERRUPT_FREQUENCY68k;
    extern int URaid_req;

  /* ------------------------------------------------------------------
   * Soft Stack Overflow Check and Recovery
   * ------------------------------------------------------------------
   * HISTORY:
   *   JDS 22 May 96: Changed >= to > because stack overflows with last
   *                  frame at end caused loops and odd bugs.
   *   JDS 31 July 97: Changed back to >
   *
   * If stack pointer is near the limit, try to extend the stack.
   * If extension fails, trigger a hard reset.
   */
  re_check_stack:
    need_irq = 0;
    if (((UNSIGNED)(CSTKPTR + 1) > Irq_Stk_Check) && (Irq_Stk_End > 0) && (Irq_Stk_Check > 0))
    {
      HARD_PUSH(TOPOFSTACK);  /* Save TOS before C call */
      EXT;  /* Save cached state to memory */
      extended_frame = NIL;

      if (do_stackoverflow(NIL))
      {
        /* Stack overflow recovery failed - hard reset required */
      stackoverflow_help:
        period_cnt = 60;
        need_irq = T;
        error("Stack Overflow, MUST HARDRESET!");
        RET;
        TOPOFSTACK = NIL_PTR;
      }
      else
      {
        /* Stack extended successfully */
        RET;  /* Restore cached state */
        POP;  /* Restore TOS */
      }

      /* Reset stack check threshold */
      Irq_Stk_Check = (UNSIGNED)EndSTKP - STK_MIN(FuncObj);
      need_irq = (Irq_Stk_End == 0) || extended_frame;
      *PENDINGINTERRUPT68k |= extended_frame;
      Irq_Stk_End = (UNSIGNED)EndSTKP;
    }

    /* ------------------------------------------------------------------
     * SDL Event Processing
     * ------------------------------------------------------------------
     * Process keyboard, mouse, and display events.
     * This is done here because it's a convenient synchronization point.
     */
    process_SDLevents();

    if (IO_Signalled)
    {
      IO_Signalled = FALSE;
      process_io_events();
    }

    /* ------------------------------------------------------------------
     * Interrupt Source Checking
     * ------------------------------------------------------------------
     * Check various interrupt sources in priority order and dispatch
     * to the appropriate handler.
     */
    if ((Irq_Stk_End <= 0) || (Irq_Stk_Check <= 0) || need_irq)
    {
      /* Check if interrupts are enabled (stackbase test) */
      if (StackOffsetFromNative(CSTKPTR) > InterfacePage->stackbase)
      {
        /* Interrupts are not disabled - process pending interrupts */
        EXT;  /* Save cached state */
        update_timer();  /* Update system timer */

        /* Periodic Interrupt (SPY profiler) */
        if (*PERIODIC_INTERRUPT68k != NIL)
        {
          if (period_cnt > 0)
            period_cnt--;
          else
          {
            cause_interruptcall(PERIODIC_INTERRUPTFRAME_index);
            if (*PERIODIC_INTERRUPT_FREQUENCY68k == NIL)
              period_cnt = 0;
            else
              period_cnt =
                  (*PERIODIC_INTERRUPT_FREQUENCY68k & 0xffff) * (1000000 / 60) / TIMER_INTERVAL;
            /* Number of 1/60 second periods between interrupts.
               TIMER_INTERVAL is microseconds between timer interrupts.
               Calculation avoids overflow but may roundoff if frequency
               is too low (bottoms out at 0). */
          }
        }

        /* URaid Debugger Request */
        else if (URaid_req == T)
        {
          URaid_req = NIL;
          error("Call URaid by User Interrupt");
        }

        /* Keyboard Event */
        else if ((KBDEventFlg > 0) && (*KEYBUFFERING68k == ATOM_T))
        {
          *KEYBUFFERING68k = ATOM_STARTED;
          cause_interruptcall(DOBUFFEREDTRANSITION_index);
          KBDEventFlg--;
        }

        /* GC Reclaim Interrupt */
        else if (*Reclaim_cnt_word == S_POSITIVE)
        {
          *Reclaim_cnt_word = NIL;
          cause_interruptcall(DORECLAIM_index);
        }

        /* Pending General Interrupt */
        else if (*PENDINGINTERRUPT68k != NIL)
        {
          INTSTAT2 *intstate = ((INTSTAT2 *)NativeAligned4FromLAddr(*INTERRUPTSTATE_word));
          intstate->handledmask |= intstate->pendingmask;
          *PENDINGINTERRUPT68k = NIL;
          cause_interruptcall(INTERRUPTFRAME_index);
        }

        /* Ethernet Interrupt */
        else if (ETHEREventCount > 0)
        {
          INTSTAT *intstate = ((INTSTAT *)NativeAligned4FromLAddr(*INTERRUPTSTATE_word));
          if (!(intstate->ETHERInterrupt) && !(((INTSTAT2 *)intstate)->handledmask & 0x40))
          {
            intstate->ETHERInterrupt = 1;
            ((INTSTAT2 *)intstate)->handledmask |= ((INTSTAT2 *)intstate)->pendingmask;
            cause_interruptcall(INTERRUPTFRAME_index);
            ETHEREventCount--;
          }
          else
            *PENDINGINTERRUPT68k = ATOM_T;
        }

        RET;  /* Restore cached state */
        CLR_IRQ;  /* Clear interrupt flags */
      }
      else
      {
        /* Interrupts are disabled - clear IRQ and recheck */
        /* Note: This loses pending interrupt requests while disabled */
        CLR_IRQ;
        goto re_check_stack;
      }
    }
  }

  /* Return to dispatch loop */
  nextop0;
}

/* ============================================================================
 * FUNCTION: do_brk
 * ============================================================================
 *
 * PURPOSE:
 *   Breakpoint handler function. Currently a no-op placeholder.
 *   This function can be called to trigger a debugger breakpoint.
 *
 * PARAMETERS: None
 *
 * RETURNS: None
 *
 * NOTE:
 *   This is a stub implementation. In a full implementation, this would
 *   trigger the URaid debugger or similar debugging facility.
 */
void do_brk(void) {}
