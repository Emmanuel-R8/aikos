= Reference Behaviors

*Navigation*: README | Compatibility Criteria

Reference test cases for validating emulator rewrite implementations. These test cases verify that implementations match Maiko behavior.

== Overview

Reference behaviors provide concrete test cases that implementations must pass to ensure compatibility. Each test case specifies input, expected output, and validation criteria.

== Opcode Test Cases

=== Test Case: CAR Operation

*Input*: Cons cell containing `(A . B)`

*Expected Output*: `A`

*Validation*:

#codeblock(lang: "pseudocode", [
cons_cell = CreateConsCell(A, B)
result = CAR(cons_cell)
assert result == A
])

=== Test Case: CDR Operation

*Input*: Cons cell containing `(A . B)`

*Expected Output*: `B`

*Validation*:

#codeblock(lang: "pseudocode", [
cons_cell = CreateConsCell(A, B)
result = CDR(cons_cell)
assert result == B
])

=== Test Case: CONS Operation

*Input*: Values `A` and `B`

*Expected Output*: Cons cell `(A . B)`

*Validation*:

#codeblock(lang: "pseudocode", [
result = CONS(A, B)
assert CAR(result) == A
assert CDR(result) == B
])

=== Test Case: Arithmetic Operations

*Input*: `IPLUS2(5, 3)`

*Expected Output*: `8`

*Validation*:

#codeblock(lang: "pseudocode", [
PushStack(5)
PushStack(3)
ExecuteOpcode(IPLUS2)
result = PopStack()
assert result == 8
])

=== Test Case: Function Call

*Input*: Function with 2 arguments

*Expected Output*: Function executes correctly

*Validation*:

#codeblock(lang: "pseudocode", [
function = GetFunction("add")
PushStack(5)
PushStack(3)
CallFunction(function, 2)
result = PopStack()
assert result == 8
])

== Memory Management Test Cases

=== Test Case: Cons Cell Allocation

*Input*: Request to allocate cons cell

*Expected Output*: Valid cons cell address

*Validation*:

#codeblock(lang: "pseudocode", [
cell = AllocateConsCell()
assert cell != null
assert IsValidAddress(cell)
assert GetConsCell(cell).car_field == NIL
assert GetConsCell(cell).cdr_code == CDR_NIL
])

=== Test Case: Reference Counting

*Input*: Object with multiple references

*Expected Output*: Reference count matches references

*Validation*:

#codeblock(lang: "pseudocode", [
object = AllocateObject()
ADDREF(object)  // refcount = 1
ADDREF(object)  // refcount = 2
DELREF(object)  // refcount = 1
assert GetReferenceCount(object) == 1
])

=== Test Case: Garbage Collection

*Input*: Object with zero references

*Expected Output*: Object reclaimed

*Validation*:

#codeblock(lang: "pseudocode", [
object = AllocateObject()
ADDREF(object)
DELREF(object)  // refcount = 0
RunGC()
assert IsReclaimed(object)
])

== Display Test Cases

=== Test Case: BitBLT Copy

*Input*: Source and destination regions

*Expected Output*: Destination matches source

*Validation*:

#codeblock(lang: "pseudocode", [
source = CreateBitmap(10, 10)
FillBitmap(source, pattern)
dest = CreateBitmap(10, 10)
BitBLT(source, dest, 0, 0, 10, 10, COPY)
assert BitmapsEqual(source, dest)
])

=== Test Case: Keycode Translation

*Input*: OS keycode for 'A' key

*Expected Output*: Lisp keycode for 'A'

*Validation*:

#codeblock(lang: "pseudocode", [
os_keycode = GetOSKeycode('A')
lisp_keycode = TranslateKeycode(os_keycode, no_modifiers)
assert lisp_keycode == 0x41  // ASCII 'A'
])

== File System Test Cases

=== Test Case: Pathname Translation

*Input*: Lisp pathname `"DSK:>file>test.lisp"`

*Expected Output*: Platform pathname (e.g., `"/file/test.lisp"`)

*Validation*:

#codeblock(lang: "pseudocode", [
lisp_path = "DSK:>file>test.lisp"
platform_path = LispToPlatformPathname(lisp_path)
assert platform_path == "/file/test.lisp"  // Unix
// or "C:\file\test.lisp" on DOS
])

=== Test Case: File Operations

*Input*: Create, write, read file

*Expected Output*: File operations succeed

*Validation*:

#codeblock(lang: "pseudocode", [
file = OpenFile("test.lisp", WRITE)
WriteFile(file, "test data", 9)
CloseFile(file)

file = OpenFile("test.lisp", READ)
data = ReadFile(file, 9)
assert data == "test data"
CloseFile(file)
])

== Sysout Compatibility Test Cases

=== Test Case: Load Sysout

*Input*: Valid sysout file

*Expected Output*: Sysout loads successfully

*Validation*:

#codeblock(lang: "pseudocode", [
sysout = LoadSysoutFile("test.sysout")
assert sysout != null
assert ValidateSysout(sysout)
assert CanExecuteBytecode(sysout)
])

=== Test Case: Execute Sysout Program

*Input*: Loaded sysout file

*Expected Output*: Program executes correctly

*Validation*:

#codeblock(lang: "pseudocode", [
sysout = LoadSysoutFile("test.sysout")
StartVM(sysout)
ExecuteProgram()
assert ProgramCompletes()
assert ResultsMatchExpected()
])

== Integration Test Cases

=== Test Case: Complete Lisp Program

*Input*: Lisp program bytecode

*Expected Output*: Program executes and produces correct results

*Validation*:

#codeblock(lang: "pseudocode", [
program = LoadLispProgram("test.lisp")
ExecuteProgram(program)
results = GetProgramResults()
assert results == ExpectedResults()
])

=== Test Case: Interactive Session

*Input*: User input events

*Expected Output*: Correct responses to input

*Validation*:

#codeblock(lang: "pseudocode", [
SendKeyEvent('A')
assert DisplayShows('A')
SendMouseEvent(click, x, y)
assert CorrectResponseToClick()
])

== Performance Test Cases

=== Test Case: Opcode Execution Speed

*Input*: Execute opcode 1,000,000 times

*Expected Output*: Acceptable performance

*Validation*:

#codeblock(lang: "pseudocode", [
start_time = GetTime()
for i = 1 to 1000000:
    ExecuteOpcode(CAR, test_cell)
end_time = GetTime()
execution_time = end_time - start_time
assert execution_time < MAX_EXECUTION_TIME
])

== Related Documentation

- Compatibility Criteria - What must match
- Instruction Set - Opcode specifications
- Memory Management - GC and memory specifications
