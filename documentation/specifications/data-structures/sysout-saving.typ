= Sysout Saving Procedures

*Navigation*: Sysout Format | Data Structures README | Main README

Complete specification of procedures for saving VM state to sysout files.

== Save Procedure

#codeblock(lang: "pseudocode", [
function SaveSysoutFile(filename):
    // Create file
    file = CreateFile(filename)

    // Write IFPAGE
    WriteIFPAGE(file, InterfacePage)

    // Build FPtoVP table
    fptovp = BuildFPtoVPTable()

    // Write FPtoVP table
    WriteFPtoVP(file, fptovp)

    // Write memory pages
    for virtual_page in active_pages:
        file_page = GetFilePageForVirtualPage(virtual_page)
        page_data = ReadMemoryPage(virtual_page)
        WritePage(file, file_page, page_data)

    CloseFile(file)
])

== Related Documentation

- Sysout Format - Complete sysout file format specification
- Memory Layout - Memory organization
