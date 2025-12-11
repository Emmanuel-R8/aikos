# Maiko Emulator Development Environment for NixOS
#
# This shell.nix provides all packages needed to build:
# - C emulator (with X11 and SDL2 support)
# - Zig emulator (with SDL2 support)
# - Lisp emulator (with SDL3 support, preferred)
#
# Note: If SDL3 or Zig are not available in your nixpkgs channel,
# you may need to use nixpkgs-unstable:
{pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {}}:
# {pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  # Use stdenv to get proper C compilation environment
  # This ensures standard C library headers (ctype.h, errno.h, etc.) are available
  stdenv = pkgs.stdenv;

  # Native build inputs (tools needed during build)
  nativeBuildInputs = with pkgs; [
    pkg-config # For library detection and linking
    cmake # CMake build system (for C emulator)
    gnumake # Make build system (for C emulator)
    # stdenv provides CC (C compiler) with proper includes
  ];

  # Build inputs (libraries and runtime dependencies)
  buildInputs = with pkgs; [
    # C standard library headers (needed for ctype.h, errno.h, etc.)
    glibc.dev # C standard library development headers

    # C compiler (both for compatibility)
    clang # Preferred C compiler
    gcc # Alternative C compiler (useful for compatibility testing)

    # Display libraries - include all for maximum compatibility
    SDL2 # SDL2 display backend (for C and Zig emulators)
    sdl3 # SDL3 display backend (for Lisp emulator, preferred)

    # Language compilers
    zig # Zig compiler (for Zig emulator)
    sbcl # Steel Bank Common Lisp (for Lisp emulator)

    # IDEs
    bun # For amp
    cursor-cli
    gemini-cli

    code-cursor
    vscode
  ];

  # Set up environment variables for pkg-config and C compilation
  shellHook = ''
    # Ensure C standard library headers are available
    export NIX_CFLAGS_COMPILE="-isystem ${pkgs.glibc.dev}/include $NIX_CFLAGS_COMPILE"

    echo "Maiko emulator development environment"
    echo "====================================="
    echo "C compiler: $(which clang || which gcc || echo 'Not found')"
    echo "Zig: $(which zig || echo 'Not found')"
    echo "SBCL: $(which sbcl || echo 'Not found')"
    echo ""
    echo "Display libraries:"
    pkg-config --exists x11 && echo "  ✓ X11 found" || echo "  ✗ X11 not found"
    pkg-config --exists sdl2 && echo "  ✓ SDL2 found" || echo "  ✗ SDL2 not found"
    pkg-config --exists sdl3 && echo "  ✓ SDL3 found" || echo "  ✗ SDL3 not found (may need to build from source)"
    echo ""
    echo "Build systems:"
    command -v cmake >/dev/null && echo "  ✓ CMake found" || echo "  ✗ CMake not found"
    command -v make >/dev/null && echo "  ✓ Make found" || echo "  ✗ Make not found"
    echo ""
    echo "C library headers:"
    [ -f "${pkgs.glibc.dev}/include/ctype.h" ] && echo "  ✓ glibc headers found" || echo "  ✗ glibc headers not found"
    echo ""
    echo "Ready to build emulators!"
    echo "  C emulator:   ./medley/scripts/build/build-emulator.sh --emulator c"
    echo "  Zig emulator: ./medley/scripts/build/build-emulator.sh --emulator zig"
    echo "  Lisp emulator: ./medley/scripts/build/build-emulator.sh --emulator lisp"
  '';
}
