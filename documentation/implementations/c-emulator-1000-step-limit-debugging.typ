= C Emulator 1000-Step Limit Debugging

*Date*: 2026-01-12 15:30
*Status*: Critical Mystery - Code path executes but check doesn't work

== Overview

Working to get the C emulator to stop execution at exactly 1000 steps for comparison with Zig emulator execution traces.

== Critical Discovery

*Finding*: The increment at line 556 (`global_debug_instruction_count++`) IS working (log shows counts 1-38726), and the log print at line 697 (`fprintf(debug_log, "%5d ", ...)`) IS working (log file exists with 38,726 lines). However, ALL test code added between these two lines is NOT executing:
- Unconditional file write at lines 560-567: NOT creating file
- Unconditional stderr write at lines 558-563: NOT appearing in stderr
- Check at line 572: NOT triggering

*Mystery*: How can the increment and log print work if the code between them doesn't execute?

*Possible Explanations*:
1. Compiler optimization removing the test code (but `volatile` should prevent this)
2. Runtime issue preventing file/stderr operations from working
3. Different code path executing increment and log print (but they're sequential in source)
4. Code is executing but operations are failing silently

*Next Steps*:
1. Verify code is actually in compiled binary using `objdump` or `gdb`
2. Check if there are compiler flags preventing optimization
3. Try using `__attribute__((noinline))` or similar to prevent optimization
4. Check if file operations are failing due to permissions or other runtime issues

== Progress Log

[Previous attempts documented above - see file history]

== Current Code State

*File*: `maiko/src/xc.c`
*Function*: `dispatch()` - main dispatch loop
*Label*: `nextopcode:` at line 550

*Code Flow*:
1. Line 556: `global_debug_instruction_count++` - ✅ WORKING (log shows counts)
2. Lines 560-567: Unconditional file write test - ❌ NOT WORKING
3. Line 572: Check `if (global_debug_instruction_count > 1000)` - ❌ NOT WORKING
4. Line 697: `fprintf(debug_log, "%5d ", ...)` - ✅ WORKING (log file exists)

*Critical Question*: Why do steps 1 and 4 work, but steps 2 and 3 don't?
