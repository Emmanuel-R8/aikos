= File System Interface Specification


Complete specification of file I/O operations and pathname translation between Lisp and platform-specific formats.

== Overview

The file system interface provides file operations abstracting platform-specific file systems. It handles pathname translation between Lisp pathname format and platform-specific formats (Unix, DOS, etc.).

== Pathname Translation

=== Lisp Pathname Format

Lisp uses a generic pathname format:

[`[host:][device:][directory>]name[.extension][;version]`]

*Components*:
- *host*: Host name (DSK, UNIX, etc.)
- *device*: Device name (drive letter on DOS)
- *directory*: Directory path (separated by `>`)
- *name*: File name
- *extension*: File extension (preceded by `.`)
- *version*: Version number (preceded by `;`)

=== Platform Pathname Format pointerUnix: [`[/]directory/.../name[.extension]`]

*DOS*: [`[drive:][\directory\...\name[.extension]]`]

=== Translation Algorithm pointerLisp to Platform: [`function LispToPlatformPathname(lisp_pathname, versionp, genp`]:
    // Extract components
    host = ExtractHost(lisp_pathname)
    device = ExtractDevice(lisp_pathname)
    directory = ExtractDirectory(lisp_pathname)
    name = ExtractName(lisp_pathname)
    extension = ExtractExtension(lisp_pathname)
    version = ExtractVersion(lisp_pathname)

    // Handle root directory
    if lisp_pathname == "<":
        return "/"

    // Handle relative paths
    if StartsWith(".", lisp_pathname):
        return HandleRelativePath(lisp_pathname)

    // Convert directory separators
    platform_directory = ConvertDirectorySeparators(directory, ">", "/")

    // Handle version (if versionp)
    if versionp:
        platform_version = ConvertVersion(version)
    else:
        platform_version = null

    // Build platform pathname
    platform_path = BuildPlatformPath(device, platform_directory, name, extension, platform_version)

    return platform_path)

*Platform to Lisp*: [`function PlatformToLispPathname(platform_pathname, dirp, versionp`]:
    // Handle root directory
    if platform_pathname == "/":
        return "<"

    // Extract components
    device = ExtractDevice(platform_pathname)
    directory = ExtractDirectory(platform_pathname)
    name = ExtractName(platform_pathname)
    extension = ExtractExtension(platform_pathname)

    // Convert directory separators
    lisp_directory = ConvertDirectorySeparators(directory, "/", ">")

    // Quote special characters
    lisp_name = QuoteSpecialCharacters(name)
    lisp_extension = QuoteSpecialCharacters(extension)

    // Build Lisp pathname
    lisp_path = BuildLispPath(device, lisp_directory, lisp_name, lisp_extension)

    return lisp_path)

== File Operations

=== Open File

[`function OpenFile(lisp_pathname, mode, recognition, error_ptr`]:
    // Translate pathname
    platform_path = LispToPlatformPathname(lisp_pathname, true, false)

    // Determine open flags
    flags = DetermineOpenFlags(mode)

    // Handle recognition
    switch recognition:
        case RECOG_OLD:
            // File must exist
            if not FileExists(platform_path):
                SetError(error_ptr, ENOENT)
                return NIL
        case RECOG_NEW:
            // File must not exist
            if FileExists(platform_path):
                SetError(error_ptr, EEXIST)
                return NIL
        case RECOG_OLD_NEW:
            // File may or may not exist
            pass

    // Open file
    file_descriptor = Open(platform_path, flags)

    if file_descriptor < 0:
        SetError(error_ptr, errno)
        return NIL

    return CreateFileHandle(file_descriptor))

=== Read File

[`function ReadFile(file_handle, buffer, length, error_ptr`]:
    file_descriptor = GetFileDescriptor(file_handle)

    bytes_read = Read(file_descriptor, buffer, length)

    if bytes_read < 0:
        SetError(error_ptr, errno)
        return NIL

    return bytes_read)

=== Write File

[`function WriteFile(file_handle, buffer, length, error_ptr`]:
    file_descriptor = GetFileDescriptor(file_handle)

    bytes_written = Write(file_descriptor, buffer, length)

    if bytes_written < 0:
        SetError(error_ptr, errno)
        return NIL

    return bytes_written)

=== Close File

[`function CloseFile(file_handle`]:
    file_descriptor = GetFileDescriptor(file_handle)
    Close(file_descriptor)
    return T)

== Directory Operations

=== List Directory

[`function ListDirectory(lisp_directory, pattern, error_ptr`]:
    // Translate directory pathname
    platform_directory = LispToPlatformPathname(lisp_directory, false, true)

    // Open directory
    dir_handle = OpenDirectory(platform_directory)

    if dir_handle == null:
        SetError(error_ptr, errno)
        return NIL

    // Read directory entries
    entries = []
    while entry = ReadDirectory(dir_handle):
        // Translate entry name to Lisp format
        lisp_entry = PlatformToLispPathname(entry.name, true, false)

        // Match pattern if specified
        if pattern == null or MatchPattern(lisp_entry, pattern):
            entries.append(lisp_entry)

    CloseDirectory(dir_handle)

    return CreateList(entries))

=== Create Directory

[`function CreateDirectory(lisp_directory, error_ptr`]:
    platform_directory = LispToPlatformPathname(lisp_directory, false, true)

    result = MakeDirectory(platform_directory, permissions)

    if result < 0:
        SetError(error_ptr, errno)
        return NIL

    return T)

=== Delete File

[`function DeleteFile(lisp_pathname, error_ptr`]:
    platform_path = LispToPlatformPathname(lisp_pathname, true, false)

    result = Unlink(platform_path)

    if result < 0:
        SetError(error_ptr, errno)
        return NIL

    return T)

== File Attributes

=== Get File Info

[`function GetFileInfo(lisp_pathname, attribute, buffer, error_ptr`]:
    platform_path = LispToPlatformPathname(lisp_pathname, true, false)

    stat_info = Stat(platform_path)

    if stat_info == null:
        SetError(error_ptr, errno)
        return NIL

    switch attribute:
        case LENGTH:
            value = stat_info.st_size
        case WDATE:
            value = ConvertTimeToLisp(stat_info.st_mtime)
        case RDATE:
            value = ConvertTimeToLisp(stat_info.st_atime)
        case AUTHOR:
            value = GetFileOwner(stat_info)
        case PROTECTION:
            value = ConvertPermissions(stat_info.st_mode)

    StoreInBuffer(buffer, value)
    return T)

== Special Characters

=== Quoting Rules

Special characters in Lisp pathnames must be quoted:
- `>`: Directory separator (quoted as `'>`)
- `;`: Version separator (quoted as `';`)
- `'`: Quote character (quoted as `''`)
- `.`: Extension separator (quoted as `'.` when in extension)

=== Quoting Algorithm

[`function QuoteSpecialCharacters(string, in_extension`]:
    result = ""
    for char in string:
        if char in special_chars:
            if in_extension and char == '.':
                result += "'.'"
            else:
                result += "'" + char
        else:
            result += char
    return result)

== Version Handling

=== Version Translation pointerLisp Version Format: `;version` (semicolon followed by number)

*Unix Version Format*: Version handled via:

- Hard links (multiple versions as separate files)
- Version numbers in filenames
- No native version support pointerDOS Version Format: Version numbers in filenames

[`function ConvertVersion(lisp_version`]:
    if platform == UNIX:
        // Unix doesn't support versions natively
        return null
    else if platform == DOS:
        // DOS version in filename
        return lisp_version)

== Related Documentation

- Network Protocol - Network file access
- Platform Abstraction - Platform-specific differences
