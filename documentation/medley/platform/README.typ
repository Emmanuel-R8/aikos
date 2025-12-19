= Platform-Specific Documentation

*Navigation*: Medley README | Medley Index | Architecture

== Overview

This directory contains platform-specific documentation for Medley. Medley supports multiple platforms, each with unique behaviors, script variations, and interface differences.

== Supported Platforms

- *Linux*: Standard Linux systems
- *macOS*: macOS systems (Darwin)
- *Windows*: Windows/Cygwin systems
- *WSL*: Windows Subsystem for Linux (WSL1 and WSL2)

== Platform Detection

Medley scripts detect the platform using system information:

#codeblock(lang: "bash", [
if [ "$(uname)" = "Darwin" ]
then
  darwin=true
  platform=darwin
elif [ "$(uname -s | head --bytes 6)" = "CYGWIN" ]
then
  cygwin=true
  platform=cgwin
elif [ -e "/proc/version" ] && grep --ignore-case --quiet Microsoft /proc/version
then
  platform=wsl
  wsl=true
  # Detect WSL1 vs WSL2
else
  linux=true
  platform=linux
fi
])

*Source Code Reference*: medley/scripts/medley/medley_main.sh - platform detection

== Platform Differences

=== Script Variants

- *Linux/macOS*: `medley_run.sh` (shell script)
- *macOS*: `medley.command` (macOS application bundle script)
- *Windows*: `medley.ps1` (PowerShell script)

=== Display Backends

- *Linux*: X11 or SDL
- *macOS*: X11 or SDL
- *Windows/Cygwin*: SDL (X11 not available)
- *WSL*: X11 or VNC (VNC recommended for better scaling)

=== Path Handling

- *Linux/macOS*: Standard Unix paths
- *Windows/Cygwin*: Windows/Cygwin path conventions
- *WSL*: WSL path conventions

=== File System

- *Linux/macOS*: Standard Unix file system
- *Windows/Cygwin*: Medley file system vs. host Windows file system
- *WSL*: WSL file system with Windows integration

== Platform-Specific Features

=== Linux

- Standard Unix behavior
- X11 or SDL display backend
- Standard path conventions

*See*: Linux Platform Documentation

=== macOS

- macOS-specific script (`medley.command`)
- X11 or SDL display backend
- macOS path handling

*See*: macOS Platform Documentation

=== Windows/Cygwin

- PowerShell script (`medley.ps1`)
- May use Docker for execution
- SDL display backend (X11 not available)
- Windows/Cygwin path conventions

*See*: Windows Platform Documentation

=== WSL

- VNC support for better display scaling
- Automation mode for short sessions
- WSL1 vs WSL2 differences
- Windows integration

*See*: WSL Platform Documentation

== Related Documentation

- *Scripts Component*: Scripts Component - Script system and platform variations
- *Interface Documentation*: Interface Documentation - Interface mechanisms
- *Architecture*: Architecture Overview - System architecture

