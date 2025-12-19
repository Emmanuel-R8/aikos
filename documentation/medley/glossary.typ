= Medley Glossary

*Navigation*: README | Index | Architecture | Components | Interface | Platform

Terminology and concepts used throughout Medley documentation.

*Related Documentation*:

- Components Documentation - Component-specific terms
- Interface Documentation - Interface-related terms
- Platform Documentation - Platform-specific terms

== Core Concepts

=== MEDLEYDIR

Top-level directory of the Medley installation. Contains the Medley script that is invoked after all symbolic links are resolved. In standard global installation, this is `/usr/local/interlisp/medley`, but Medley can be installed in multiple places.

*See also*: Environment Variables, Directory Structure

=== LOGINDIR

User-specific Medley directory where Medley stores user-specific files including vmem files, INIT files, and user configuration. Defaults to `MEDLEYDIR/logindir` but can be overridden with `-x` flag.

*See also*: Environment Variables, Virtual Memory Files

=== Sysout

System output file. A binary file containing a complete Lisp system state that Maiko can load and execute. Standard sysout files include `lisp.sysout`, `full.sysout`, and `apps.sysout`.

*See also*: Sysout Files, File Formats

=== Vmem (Virtual Memory File)

Virtual memory file. A file storing the persistent state of a Medley session, allowing continuation across restarts. Default location is `LOGINDIR/lisp.virtualmem` or `LOGINDIR/{run-id}.virtualmem`.

*See also*: Virtual Memory Files, File Formats

=== Run ID

Identifier for a Medley session instance. Used to distinguish multiple Medley instances and to name vmem files. Default is "default" but can be specified with `-id` flag.

*See also*: Command-Line Arguments, Virtual Memory Files

== File Types

=== Sysout File

Binary file containing a complete Lisp system state. Created by the loadup process and loaded by Maiko to start a Medley session. Standard types:

- *lisp.sysout*: Minimal Interlisp and Common Lisp environment
- *full.sysout*: Complete Interlisp and Common Lisp with development tools
- *apps.sysout*: Full sysout plus Medley applications (Notecards, Rooms, CLOS)

*See also*: Sysout Files, Loadup Workflow

=== Vmem File

Virtual memory file storing session state. Created when Medley exits and loaded on next startup to continue the session. Format is binary and platform-specific.

*See also*: Virtual Memory Files, File Formats

=== Config File

Text file containing default command-line arguments for Medley. Processed before command-line arguments, allowing defaults to be overridden. Default location is `MEDLEYDIR/.medley_config` or `~/.medley_config`.

*See also*: Configuration Files, File Formats

=== Greet File

Lisp file executed during Medley startup to initialize the environment. Greet files are executed in order before the main Lisp system starts. Default greet file is `MEDLEYDIR/greetfiles/INIT.LISP`.

*See also*: Greet Files, File Formats

=== REM.CM File

Lisp file executed after greet files, typically used for loadup operations. Can be specified with `-cm` flag or `LDEREMCM` environment variable.

*See also*: Greet Files, Command-Line Arguments

== Script Terms

=== Medley Script

Main entry point script that parses arguments, sets up environment, and invokes Maiko. Platform-specific variants:

- *medley_run.sh*: Linux/macOS shell script
- *medley.command*: macOS application bundle script
- *medley.ps1*: Windows PowerShell script

*See also*: Scripts, Platform Documentation

=== Argument Parsing

Process by which Medley scripts parse command-line arguments, apply config file defaults, and transform arguments for Maiko.

*See also*: Scripts, Command-Line Interface

=== Maiko Invocation

Process by which Medley scripts invoke the Maiko emulator with transformed arguments and environment variables.

*See also*: Scripts, Command-Line Interface

== Interface Terms

=== Command-Line Arguments

Arguments passed from Medley scripts to Maiko emulator. Medley flags are transformed into Maiko flags during argument parsing.

*See also*: Command-Line Interface

=== Environment Variables

Variables set by Medley scripts and used by Maiko for communication. Key variables include MEDLEYDIR, LOGINDIR, LDESOURCESYSOUT, LDEINIT, LDEREMCM, and LDEDESTSYSOUT.

*See also*: Environment Variables

=== File Format Specification

Complete specification of binary and text file formats used for Medley-Maiko communication, including sysout, vmem, config, and greet file formats.

*See also*: File Formats

=== Runtime Protocol

Communication patterns and protocols used during Medley execution, including script invocation, error handling, exit codes, and Maiko startup sequence.

*See also*: Protocols

== Directory Terms

=== MEDLEYDIR Structure

Standard Medley installation directory structure:

- *scripts/*: Medley scripts
- *sources/*: Lisp source code
- *library/*: Supported packages
- *lispusers/*: User-contributed packages
- *loadups/*: Sysout files and build artifacts
- *greetfiles/*: Greet files
- *docs/*: Documentation files

*See also*: Directory Structure

=== LOGINDIR Structure

User-specific directory structure:

- *lisp.virtualmem*: Default vmem file
- *{run-id}.virtualmem*: Run-specific vmem files
- *INIT.LISP*: User-specific greet file
- *.medley_config*: User-specific config file

*See also*: Directory Structure

== Loadup Terms

=== Loadup

Process of creating sysout files from Lisp source code. The loadup procedure runs in sequential stages to build progressively more complete sysout files.

*See also*: Loadup Workflow

=== Loadup Stages

Sequential stages of the loadup process:

1. *Init*: Create init.dlinit sysout
2. *Mid*: Create init-mid.sysout
3. *Lisp*: Create lisp.sysout
4. *Full*: Create full.sysout
5. *Apps*: Create apps.sysout

*See also*: Loadup Workflow

== Platform Terms

=== Platform Detection

Process by which Medley scripts detect the host platform (Linux, macOS, Windows, WSL) and select appropriate script variants and behaviors.

*See also*: Platform Documentation

=== Display Backend

Graphics output subsystem used by Maiko. Options include X11, SDL, and VNC (for WSL).

*See also*: Platform Documentation, Maiko Display Documentation

== Related Maiko Terms

For Maiko-specific terminology, see the Maiko Glossary.

Key Maiko terms relevant to Medley:

- *Maiko*: The emulator that executes Lisp bytecode
- *LispPTR*: Virtual address pointer in Lisp address space
- *ByteCode*: Single byte representing a Lisp bytecode instruction
- *Dispatch Loop*: Main execution loop in Maiko

*See also*: Maiko Documentation

