= Memory Layout Specification


Complete specification of memory regions, their organization, and memory layout.

== Overview

Maiko memory is organized into distinct regions, each serving a specific purpose. Memory regions are mapped to virtual addresses and managed independently.

== Memory Regions

=== Memory Layout Diagram

#figure(
  caption: [Diagram],
  [Diagram: See original documentation for visual representation.],
)

  )
)

== Region Specifications

=== Interface Page (IFPAGE)

*Offset*: IFPAGE_OFFSET pointerSize: 1 page (256 bytes)
*Purpose*: VM state and control structures pointerContents: - Execution state
- Stack pointers
- GC state
- Interrupt state - Storage state

=== Stack Space (STK)

*Offset*: STK_OFFSET pointerSize: Variable (grows as needed)
*Purpose*: Function activation frames pointerOrganization: - Stack frames (FX)
- Binding frames (BF)
- Free stack blocks (FSB) - Guard blocks pointerGrowth: Grows downward (toward lower addresses)

=== Atom Space (ATOMS)

*Offset*: ATOMS_OFFSET pointerSize: Variable pointerPurpose: Symbol table pointerOrganization: - Atom structures
- Print names
- Value cells
- Definition cells - Property lists

=== Atom Hash Table (ATMHT)

*Offset*: ATMHT_OFFSET pointerSize: Fixed pointerPurpose: Atom lookup table pointerOrganization: - Hash table entries
- Collision chains - Atom indices

=== Property List Space (PLIS)

*Offset*: PLIS_OFFSET pointerSize: Variable pointerPurpose: Property lists pointerOrganization: - Property list cells - Property values

=== DTD Space (DTD)

*Offset*: DTD_OFFSET pointerSize: Variable pointerPurpose: Data type descriptors pointerOrganization: - DTD structures - Type information

=== MDS Space (Memory Data Structure)

*Offset*: MDS_OFFSET pointerSize: Variable (grows as needed)
*Purpose*: Heap objects pointerOrganization: - Cons cells
- Arrays
- Code blocks - Other heap objects pointerGrowth: Grows upward (toward higher addresses)

=== Definition Space (DEFS)

*Offset*: DEFS_OFFSET pointerSize: Variable pointerPurpose: Function definitions pointerOrganization: - Definition cells
- Function headers - Code blocks

=== Value Space (VALS)

*Offset*: VALS_OFFSET pointerSize: Variable pointerPurpose: Global values pointerOrganization: - Value cells - Global bindings

=== Display Region (DISPLAY)

*Offset*: DISPLAY_OFFSET pointerSize: Variable pointerPurpose: Display buffer pointerOrganization: - Display memory
- Bitmap data
- Graphics buffers

== GC Hash Tables

=== HTmain pointerOffset: HTMAIN_OFFSET pointerSize: Fixed pointerPurpose: Main GC hash table pointerOrganization: - Hash entries
- Reference counts
- Collision flags

=== HTcoll pointerOffset: HTCOLL_OFFSET pointerSize: Variable pointerPurpose: GC collision table pointerOrganization: - Collision entries
- Linked chains

=== HTbigcount pointerOffset: HTBIG_OFFSET pointerSize: Variable pointerPurpose: Overflow reference counts pointerOrganization: - Overflow entries
- Large counts

=== HToverflow pointerOffset: HTOVERFLOW_OFFSET pointerSize: Variable pointerPurpose: Additional overflow

== Memory Allocation

=== Cons Cell Allocation

[`function AllocateConsCell(`]:
    // Find free cons cell
    cons_page = FindFreeConsPage()
    cell = GetFreeCell(cons_page)

    // Initialize cell
    cell.car_field = NIL
    cell.cdr_code = CDR_NIL

    return LispAddressOf(cell))

=== Array Allocation

[`function AllocateArray(size, type`]:
    // Calculate required space
    array_size = CalculateArraySize(size, type)

    // Find free array block
    array_block = FindFreeArrayBlock(array_size)

    // Initialize array header
    array_header = GetArrayHeader(array_block)
    array_header.size = size
    array_header.type = type

    return LispAddressOf(array_block))

=== Code Allocation

[`function AllocateCodeBlock(size`]:
    // Allocate code block
    code_block = AllocateMDSBlock(size)

    // Initialize code header
    code_header = GetCodeHeader(code_block)
    code_header.size = size

    return LispAddressOf(code_block))

== Memory Organization

=== Page-Based Organization

Memory is organized into 256-byte pages:
- *Page Number*: High bits of address
- *Page Offset*: Low 8 bits
- *Page Base*: Address with offset cleared

=== Segment Organization

Address space divided into segments:
- *Segment Number*: High bits (8-12 bits)
- *Segment Base*: Address with page/offset cleared
- *Segment Size*: Multiple pages

== Storage Management

=== Primary Space

Initial memory allocation:
- *Base*: MDS_OFFSET
- *Limit*: Next_MDSpage_word
- *Purpose*: Primary heap allocation

=== Secondary Space

Extended memory when primary exhausted:
- *Base*: SecondMDSPage_word
- *Limit*: Process size limit
- *Purpose*: Extended heap allocation

=== Storage States
- *SFS_NOTSWITCHABLE*: Cannot use secondary space
- *SFS_SWITCHABLE*: Can switch to secondary space
- *SFS_ARRAYSWITCHED*: Array space switched
- *SFS_FULLYSWITCHED*: Fully switched

== Related Documentation

- Virtual Memory - Virtual memory system
- Garbage Collection - Memory reclamation
- Data Structures - Object layouts
