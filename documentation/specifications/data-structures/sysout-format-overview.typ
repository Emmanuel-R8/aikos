= Sysout File Format Specification


Complete specification of the sysout file format, including file structure, page organization, and loading procedures.

== Overview

Sysout files are persistent snapshots of the Lisp VM state. They contain all memory pages, VM state, and metadata needed to restore a complete Lisp environment.

== File Structure

=== File Layout

#figure(
  caption: [Sysout File Layout],
  [Diagram: See original documentation for visual representation.],
)
    
  )
)

=== File Organization

- *Page-based*: File organized into 512-byte pages (`BYTESPER_PAGE = 512`)
- *Sparse*: Not all pages present (FPtoVP indicates which, 0xFFFF = sparse marker)
- *Mapped*: FPtoVP table maps file pages to virtual pages
- *IFPAGE Location*: Fixed at offset 512 bytes (`IFPAGE_ADDRESS = 512`)

== Interface Page (IFPAGE)

=== IFPAGE Structure

Located at fixed address: `IFPAGE_ADDRESS` (512 bytes from start of file)

The IFPAGE structure contains ~100 fields that define the complete VM state, memory layout, and system configuration. It must match the C structure definition in `maiko/inc/ifpage.h` exactly for byte-for-byte compatibility.

*Complete Structure* (non-BIGVM, non-BYTESWAP version):

[`struct IFPAGE:`]
[`    // Frame pointers (DLword - 16-bit stack offsets from Stackspace`]
    currentfxp: DLword      // Current frame (DLword offset from Stackspace)
    resetfxp: DLword        // Reset frame subovfxp: DLword        // Stack overflow frame kbdfxp: DLword          // Keyboard interrupt frame hardreturnfxp: DLword   // Hard return frame gcfxp: DLword           // GC frame faultfxp: DLword        // Fault frame endofstack: DLword      // End of stack (DLword offset from Stackspace)

    // Version information (DLword)
    lversion: DLword        // Lisp version
    minrversion: DLword     // Minimum runtime version
    minbversion: DLword     // Minimum bytecode version
    rversion: DLword        // Runtime version
    bversion: DLword        // Bytecode version
    machinetype: DLword     // Machine type (MACHINETYPE_MAIKO = 3)
    miscfxp: DLword         // Miscellaneous frame // Validation and identification (DLword)
    key: DLword             // Validation key (IFPAGE_KEYVAL = 0x15e3) - CRITICAL
                            // Defined in maiko/inc/ifpage.h:15
                            // Must match exactly for sysout validation to pass
    serialnumber: DLword    // Serial number
    emulatorspace: DLword   // Emulator space identifier
    screenwidth: DLword     // Screen width in pixels
    nxtpmaddr: DLword       // Next PM address

    // Page management (DLword)
    nactivepages: DLword    // Number of active pages
    ndirtypages: DLword     // Number of dirty pages
    filepnpmp0: DLword      // File page number PM 0
    filepnpmt0: DLword      // File page number PM table 0
    teleraidfxp: DLword     // Teleray RAID frame filler1: DLword         // Reserved filler
    filler2: DLword         // Reserved filler
    filler3: DLword         // Reserved filler

    // User information (DLword)
    usernameaddr: DLword    // Username address (LispPTR)
    userpswdaddr: DLword    // User password address (LispPTR)
    stackbase: DLword       // Stack base (DLword offset from Stackspace)

    // Fault handling (DLword)
    faulthi: DLword         // Fault high word
    faultlo: DLword         // Fault low word
    devconfig: DLword       // Device configuration (was realpagetable)

    // Real page table (DLword)
    rptsize: DLword         // Real page table size
    rpoffset: DLword        // Real page table offset
    wasrptlast: DLword      // Was real page table last
    embufvp: DLword         // Emulator buffer virtual page

    // Network host addresses (DLword)
    nshost0: DLword         // Network host address 0
    nshost1: DLword         // Network host address 1
    nshost2: DLword         // Network host address 2

    // Memory zones (DLword)
    mdszone: DLword         // MDS zone identifier
    mdszonelength: DLword   // MDS zone length
    emubuffers: DLword      // Emulator buffers identifier
    emubuflength: DLword    // Emulator buffer length
    process_size: DLword    // Process size in MB (was lastnumchars)
    storagefullstate: DLword // Storage full state (was sysdisk)
    isfmap: DLword          // ISF map identifier

    // Miscapply stack (LispPTR - 32-bit pointers, not ref counted)
    miscstackfn: LispPTR    // Miscapply stack function miscstackarg1: LispPTR  // Miscapply stack argument 1
    miscstackarg2: LispPTR  // Miscapply stack argument 2
    miscstackresult: LispPTR // Miscapply stack result

    // Page management continued (DLword)
    nrealpages: DLword      // Number of real pages
    lastlockedfilepage: DLword // Last locked file page
    lastdominofilepage: DLword // Last domino file page
    fptovpstart: DLword     // FPtoVP table start page number (1-based)
    fakemousebits: DLword   // Fake mouse bits
    dl24bitaddressable: DLword // 24-bit addressable flag
    realpagetableptr: LispPTR // Real page table dllastvmempage: DLword  // Last VM emulator page
    fullspaceused: DLword   // Full space used flag
    fakekbdad4: DLword      // Fake keyboard address 4
    fakekbdad5: DLword      // Fake keyboard address 5)

*Field Types*:
- `DLword`: 16-bit unsigned integer (2 bytes)
- `LispPTR`: 32-bit unsigned integer (4 bytes) - virtual address in Lisp address space pointerByte Layout: - All fields are packed (no padding between fields)
- Total structure size: ~200 bytes (varies by BIGVM/BYTESWAP configuration)
- Fields are stored in big-endian format in sysout files
- Must be byte-swapped when reading on little-endian machines

*Critical Fields*:
- `key`: Must be `0x15e3` for valid sysout files
- `fptovpstart`: 1-based page number where FPtoVP table starts (calculate offset: `(fptovpstart - 1) * BYTESPER_PAGE`)
- `currentfxp`: DLword offset from Stackspace (not a LispPTR!) - multiply by 2 for byte offset
- `stackbase`, `endofstack`: DLword offsets from Stackspace
- `process_size`: Process size in MB (0 = use default 64MB)

*CRITICAL*: The IFPAGE validation key `IFPAGE_KEYVAL` is `0x15e3` (not `0x12345678`). This value is defined in `maiko/inc/ifpage.h:15`. Any implementation must use this exact value for sysout validation to work correctly.

=== IFPAGE Validation

[`function ValidateSysout(file`]:
    // Read IFPAGE
    ifpage = ReadIFPAGE(file)

    // Check key
    if ifpage.key != IFPAGE_KEYVAL:
        Error("Invalid sysout file")

    // Check version compatibility
    if ifpage.lversion < LVERSION:
        Error("Sysout version too old")

    if ifpage.minbversion > MINBVERSION:
        Error("Sysout version too new")

    return true)

