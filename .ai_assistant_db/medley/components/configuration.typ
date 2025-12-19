= Configuration Files

*Navigation*: Medley README | Medley Index | Architecture

== Overview

Configuration files provide default command-line arguments for Medley. They allow users to set preferred defaults without specifying them on every command line. Config files are processed before command-line arguments, allowing command-line arguments to override config file defaults.

== Config File Format

=== Text Format

Config files are plain text files with one argument per line:

#codeblock(lang: "text", [
flag value
--flag value
flag-with-value
])

=== Line Format

Each line contains one or two space-separated tokens:

- *Single token*: Flag without value (e.g., `-f`, `--full`, `-ns`)
- *Two tokens*: Flag with value (e.g., `-g 1024x768`, `--geometry 1024x768`, `-i myid`)

=== Value Quoting

Values can be quoted with double quotes if they contain spaces:

#codeblock(lang: "text", [
-t "My Medley Window"
--title "My Medley Window"
])

Quotes are stripped during parsing.

=== Example Config File

#codeblock(lang: "text", [
-f
-g 1024x768
-i work
-t "Medley Work Session"
-m 512
])

*Source Code Reference*:

- medley/scripts/medley/medley_configfile.sh - Config file parsing logic (lines 17-75)
- medley/scripts/medley/medley_args.sh - Argument processing with config file support

== Config File Locations

=== Default Locations

Config files are searched in this order:

1. *User config*: `~/.medley_config` (user home directory)
2. *Medley config*: `MEDLEYDIR/.medley_config` (Medley installation directory)

*Source Code Reference*: medley/scripts/medley/medley_configfile.sh - config file resolution

=== Custom Location

Config file location can be specified with `-c, --config` flag:

#codeblock(lang: "bash", [
medley -c /path/to/config/file
])

=== Suppressing Config File

Config file usage can be suppressed with `-c -, --config -`:

#codeblock(lang: "bash", [
medley -c -
# Ignore config files, use only command-line arguments
])

== Precedence Rules

=== Processing Order

Arguments are processed in this order:

1. *Config file*: Read default arguments from config file (if present)
2. *Command-line*: Process command-line arguments (override config)

=== Override Behavior

Command-line arguments override config file arguments:

- *Config file*: `-f` (use full.sysout)
- *Command-line*: `-l` (use lisp.sysout)
- *Result*: `-l` wins, lisp.sysout is used

=== Last-Wins Rule

For flags that can be specified multiple times, the last one wins:

- *Config file*: `-g 800x600`
- *Command-line*: `-g 1024x768`
- *Result*: `1024x768` is used

== Parsing Logic

=== Reverse Order Processing

Config files are processed in reverse order (last line first):

1. Config file is read line by line
2. Lines are reversed
3. Arguments are added to argument array in reverse order
4. Command-line arguments are then processed

This ensures that earlier lines in the config file can be overridden by later lines, and command-line arguments override everything.

*Source Code Reference*: medley/scripts/medley/medley_configfile.sh - reverse order processing

=== Argument Separation

Config file arguments are separated from command-line arguments with a marker:

- `--start_cl_args` marker separates config file args from command-line args
- Scripts track which stage arguments come from (`args_stage` variable)

*Source Code Reference*: medley/scripts/medley/medley_args.sh - argument stage tracking

== Supported Flags

All Medley command-line flags can be used in config files:

=== Sysout Selection

- `-f, --full`: Use full.sysout
- `-l, --lisp`: Use lisp.sysout
- `-a, --apps`: Use apps.sysout

=== Display

- `-g WxH, --geometry WxH`: Window geometry
- `-sc WxH, --screensize WxH`: Screen size
- `-d :N, --display :N`: X display
- `-ps N, --pixelscale N`: Pixel scale
- `-bw N, --borderwidth N`: Border width
- `-ns, --noscroll`: Disable scroll bars

=== Memory

- `-m N, --mem N`: Virtual memory size
- `-p FILE, --vmem FILE`: Vmem file path

=== Session

- `-i ID, --id ID`: Run ID
- `-x DIR, --logindir DIR`: LOGINDIR
- `-t STRING, --title STRING`: Window title

=== Greet Files

- `-r FILE, --greet FILE`: Greet file
- `-r -, --greet -`: No greet file

=== Other

- `-cm FILE, --rem.cm FILE`: REM.CM file
- `-nh-host HOST, --nethub-host HOST`: Nethub host
- `-nh-port PORT, --nethub-port PORT`: Nethub port
- `-nh-mac MAC, --nethub-mac MAC`: Nethub MAC
- `-nh-debug, --nethub-debug`: Nethub debug

*See*: Scripts Component for complete flag list

== Usage Examples

=== Basic Config File

Create `~/.medley_config`:

#codeblock(lang: "text", [
-f
-g 1024x768
-i work
])

Then run:

#codeblock(lang: "bash", [
medley
# Uses full.sysout, 1024x768 geometry, run ID "work"
])

=== Override Config File

#codeblock(lang: "bash", [
medley -l
# Overrides -f from config, uses lisp.sysout instead
])

=== Suppress Config File

#codeblock(lang: "bash", [
medley -c -
# Ignores config file, uses only command-line arguments
])

=== Custom Config File

#codeblock(lang: "bash", [
medley -c /path/to/custom/config
# Uses specified config file
])

== Edge Cases

=== Missing Config File

If specified config file doesn't exist:

- Script reports error: "Error: specified config file not found."
- Script exits with code 52

*Source Code Reference*: medley/scripts/medley/medley_configfile.sh - error handling

=== Empty Config File

Empty config files are valid and simply provide no defaults.

=== Invalid Lines

Invalid lines in config files are ignored (no error reported). Only the first two tokens on each line are used.

== Platform Considerations

=== Path Handling

Config file paths follow platform conventions:

- *Linux/macOS*: Standard Unix paths
- *Windows/Cygwin*: Windows/Cygwin path conventions
- *WSL*: WSL path conventions

=== File Permissions

Config files should be readable by the user running Medley. No special permissions are required.

== Related Documentation

- *Architecture*: Architecture Overview - System architecture
- *Scripts*: Scripts Component - Script system and argument parsing
- *Directory Structure*: Directory Structure Component - Config file locations
- *Interface - File Formats*: File Formats - Config file format specification
