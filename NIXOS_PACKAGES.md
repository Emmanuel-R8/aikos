# NixOS Packages Required for Building Maiko Emulators

This document lists all packages needed under NixOS to compile the C, Zig, and Lisp implementations of the Maiko emulator.

## Core Build Tools (Required for All Emulators)

These packages are needed for the build system itself:

```nix
# In your configuration.nix or shell.nix
buildInputs = [
  # Build tools
  gnumake          # Make build system
  cmake            # CMake build system (alternative to Make)
  pkg-config       # For detecting libraries via pkg-config
];
```

## C Emulator Prerequisites

The C emulator can be built with either CMake or Make, and supports X11 or SDL2 display backends.

### Required Packages

```nix
buildInputs = [
  # C compiler (choose one or both)
  clang            # Preferred C compiler
  # OR
  gcc              # Alternative C compiler

  # Build system (choose one or both)
  gnumake          # Make build system
  cmake            # CMake build system

  # Display backend (choose one or both)
  xorg.libX11      # X11 display backend
  xorg.libX11.dev  # X11 development headers

  # OR/AND
  SDL2             # SDL2 display backend
  SDL2.dev         # SDL2 development headers

  # Utility
  pkg-config       # For library detection
];
```

### Minimal C Emulator Build (X11 backend with Make)

```nix
buildInputs = [
  clang
  gnumake
  xorg.libX11
  xorg.libX11.dev
  pkg-config
];
```

### Minimal C Emulator Build (SDL2 backend with CMake)

```nix
buildInputs = [
  clang
  cmake
  SDL2
  SDL2.dev
  pkg-config
];
```

## Zig Emulator Prerequisites

The Zig emulator requires the Zig compiler and SDL2 libraries.

### Required Packages

```nix
buildInputs = [
  zig              # Zig compiler (0.11+)
  SDL2             # SDL2 library
  SDL2.dev         # SDL2 development headers
  pkg-config       # For library detection
];
```

**Note**: Zig may not be available in nixpkgs stable. You may need to:
- Use nixpkgs-unstable
- Or install Zig manually from https://ziglang.org/download/
- Or use a Nix flake/overlay for Zig

### Example with Nix Flake

If Zig is not in your nixpkgs, you can add it via flake:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    zig-overlay.url = "github:mitchellh/zig-overlay";
  };

  outputs = { self, nixpkgs, zig-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ zig-overlay.overlays.default ];
      };
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.zigpkgs.master.latest
          pkgs.SDL2
          pkgs.SDL2.dev
          pkgs.pkg-config
        ];
      };
    };
}
```

## Lisp Emulator Prerequisites

The Lisp emulator requires SBCL (Steel Bank Common Lisp) and SDL3 libraries.

### Required Packages

```nix
buildInputs = [
  sbcl             # Steel Bank Common Lisp compiler
  # SDL3 may need special handling - see below
  pkg-config
];
```

**Note**: SDL3 may not be available in nixpkgs. You may need to:
- Build SDL3 from source
- Use a Nix flake/overlay for SDL3
- Or wait for SDL3 to be added to nixpkgs

### SDL3 Handling

SDL3 is relatively new and may not be in nixpkgs. Options:

1. **Build from source** (recommended for now):
   ```bash
   # Follow SDL3 build instructions
   # https://github.com/libsdl-org/SDL
   ```

2. **Use a Nix overlay/flake** if available

3. **Temporary workaround**: The Lisp emulator may work without SDL3 for VM core testing, but display functionality will be unavailable.

## Complete Package List (All Emulators)

If you want to build all three emulators, here's the complete list:

```nix
buildInputs = [
  # C compiler
  clang
  gcc              # Optional, but useful for compatibility testing

  # Build systems
  gnumake
  cmake

  # Display libraries
  xorg.libX11
  xorg.libX11.dev
  SDL2
  SDL2.dev
  # SDL3 - see note above

  # Language compilers
  zig              # May need special handling
  sbcl

  # Utilities
  pkg-config
];
```

## Example shell.nix

Here's a complete `shell.nix` file for development:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # C emulator
    clang
    gnumake
    cmake
    xorg.libX11
    xorg.libX11.dev
    SDL2
    SDL2.dev

    # Zig emulator
    zig              # May need to use nixpkgs-unstable or overlay

    # Lisp emulator
    sbcl
    # SDL3 - handle separately

    # Utilities
    pkg-config
  ];

  shellHook = ''
    echo "Maiko emulator development environment"
    echo "C compiler: $(which clang || which gcc)"
    echo "Zig: $(which zig || echo 'Not found - may need overlay')"
    echo "SBCL: $(which sbcl || echo 'Not found')"
  '';
}
```

## Example configuration.nix (System-wide)

If you want to install these system-wide:

```nix
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # C emulator
    clang
    gnumake
    cmake
    xorg.libX11
    xorg.libX11.dev
    SDL2
    SDL2.dev

    # Zig emulator
    zig              # May need nixpkgs-unstable

    # Lisp emulator
    sbcl

    # Utilities
    pkg-config
  ];
}
```

## Platform-Specific Notes

### NixOS on x86_64 Linux

All packages should be available in nixpkgs, except possibly:
- Zig (may need unstable or overlay)
- SDL3 (may need to build from source)

### NixOS on ARM64 (aarch64)

Same as x86_64, but verify:
- Zig has ARM64 builds
- SBCL has ARM64 support

## Verification

After installing packages, verify they're available:

```bash
# Check C compiler
clang --version || gcc --version

# Check build tools
make --version
cmake --version

# Check Zig
zig version  # Should show 0.11+ or later

# Check SBCL
sbcl --version

# Check libraries
pkg-config --exists x11 && echo "X11 found"
pkg-config --exists sdl2 && echo "SDL2 found"
pkg-config --exists sdl3 && echo "SDL3 found" || echo "SDL3 not found (may need to build from source)"
```

## Troubleshooting

### Zig Not Found

If `zig` is not available in your nixpkgs:

1. **Use nixpkgs-unstable**:
   ```nix
   # In your configuration
   nixpkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
   ```

2. **Use a Zig overlay** (see flake example above)

3. **Install manually** from https://ziglang.org/download/

### SDL3 Not Found

SDL3 is new and may not be in nixpkgs. Options:

1. Build from source following SDL3 instructions
2. Wait for SDL3 to be added to nixpkgs
3. Use the Lisp emulator without display (VM core only)

### pkg-config Issues

If `pkg-config` can't find libraries, you may need to set:

```bash
export PKG_CONFIG_PATH="${pkgs.SDL2.dev}/lib/pkgconfig:${pkgs.xorg.libX11.dev}/lib/pkgconfig"
```

In a `shell.nix`, this can be handled automatically:

```nix
shellHook = ''
  export PKG_CONFIG_PATH="${pkgs.SDL2.dev}/lib/pkgconfig:${pkgs.xorg.libX11.dev}/lib/pkgconfig"
'';
```

## Quick Start

For a quick start with just the C emulator (most common use case):

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    clang
    gnumake
    xorg.libX11
    xorg.libX11.dev
    pkg-config
  ];
}
```

Then run:
```bash
nix-shell
cd medley/scripts/build
./build-emulator.sh --emulator c
```
