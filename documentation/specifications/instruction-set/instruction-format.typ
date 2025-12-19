= Instruction Format Specification

*Navigation*: README | Opcodes | Execution Semantics

Complete specification of how bytecode instructions are encoded and decoded.

== Instruction Structure

=== Basic Format

#codeblock(lang: "text", [
[Opcode Byte] [Operand Bytes...]
])

- *Opcode Byte*: Single byte (0-255) identifying the instruction
- *Operand Bytes*: Zero or more bytes containing operands

=== Instruction Length

Instruction length varies by opcode:

- *1 byte*: Opcode only (e.g., NOP, NIL, T)
- *2 bytes*: Opcode + 1 operand (e.g., TYPEP, BIND)
- *3 bytes*: Opcode + 2 operands (e.g., UNWIND, some atom references)
- *4 bytes*: Opcode + 3 operands (for BIGATOMS, atom references)
- *Variable length*: Some opcodes have variable-length operands (up to 9 bytes)

== Operand Encoding

=== Single Byte Operands

Many opcodes use a single byte operand:

- *Type codes*: TYPEP uses byte for type code
- *Counts*: BIND uses byte for binding count
- *Offsets*: JUMP uses byte for jump offset

=== Multi-Byte Operands

Some operands span multiple bytes:

- *Atom indices*:
  - 2 bytes for standard atoms (BIGATOMS undefined)
  - 3 bytes for BIGATOMS
- *Addresses*:
  - 2-3 bytes depending on address space size
- *Numbers*:
  - Variable encoding depending on value size

=== Atom Reference Encoding

Atom references use different encoding based on BIGATOMS:

- *Without BIGATOMS*: 2-byte atom index
- *With BIGATOMS*: 3-byte atom index

== Opcode Length Table

The VM maintains a table mapping opcode values to instruction lengths:

#codeblock(lang: "pseudocode", [
opcode_length_table[256] = [
  0, 0, 0, 0, 0, 1, 2, 2,  // 0-7
  2, 2, 2, 2, 2, 3, 0, 0,  // 8-15
  // ... (varies by BIGATOMS setting)
]
])

*Length values*:

- `0`: Unused opcode
- `1`: Opcode only
- `2`: Opcode + 1 byte operand
- `3`: Opcode + 2 byte operands
- `4`: Opcode + 3 byte operands (BIGATOMS)
- `9`: Variable-length opcode

== Instruction Decoding Algorithm

#codeblock(lang: "pseudocode", [
function decode_instruction(pc):
    opcode = read_byte(pc)
    length = opcode_length_table[opcode]

    if length == 0:
        error("Unused opcode")

    operands = []
    for i = 1 to length - 1:
        operands.append(read_byte(pc + i))

    return Instruction(opcode, operands, length)
])

== Program Counter Updates

After instruction execution:

#codeblock(lang: "pseudocode", [
pc = pc + instruction_length
])

The program counter advances by the instruction length, positioning it at the next instruction.

== Endianness

*CRITICAL*: Instruction operands in sysout files are stored in *big-endian byte order*. When loading on little-endian hosts, byte-swapping is required.

- *Byte order in sysout files*: Big-endian for multi-byte operands
- *Byte order in memory (after loading)*: Native host format (little-endian on x86_64, big-endian on big-endian hosts)
- *Byte swapping*: Required when loading on little-endian hosts (handled by `word_swap_page()` during page loading)
- *Address encoding*: LispPTR values are opaque 32-bit offsets, never byte-swapped (pointer arithmetic only)

== Special Cases

=== UFN (Undefined Function Name)

Some opcode values trigger UFN handling:

- Opcode value indicates UFN
- Additional bytes specify function name and arguments
- UFN table lookup resolves to actual function

=== Multi-Byte Opcodes

Some opcodes span multiple bytes:

- First byte identifies opcode category
- Subsequent bytes specify operation details
- Total length determined by opcode type

== Examples

=== Example 1: Simple Opcode (NIL)

#codeblock(lang: "text", [
Byte 0: 0x68 (opc_NIL)
Length: 1 byte
No operands
])

=== Example 2: Opcode with Operand (TYPEP)

#codeblock(lang: "text", [
Byte 0: 0x05 (opc_TYPEP)
Byte 1: 0x03 (type code)
Length: 2 bytes
])

=== Example 3: Multi-Byte Opcode (UNWIND)

#codeblock(lang: "text", [
Byte 0: 0x07 (opc_UNWIND)
Byte 1: 0x10 (first operand)
Byte 2: 0x20 (second operand)
Length: 3 bytes
])

== Related Documentation

- Opcodes Reference - Complete opcode list with operand formats
- Execution Semantics - How instructions execute
- VM Core Execution Model - Dispatch loop implementation
