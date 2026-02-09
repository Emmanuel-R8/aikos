{
  description = "Maiko Emulator Development Environment";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    llms.url = "github:numtide/llm-agents.nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nixpkgs-stable,
      llms,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        llmsPkgs = llms.packages.${pkgs.stdenv.hostPlatform.system};
      in
      {
        devShells.default = pkgs.mkShell {
          # Use stdenv to get proper C compilation environment
          # This ensures standard C library headers (ctype.h, errno.h, etc.) are available
          stdenv = pkgs.stdenv;

          # Native build inputs (tools needed during build)
          nativeBuildInputs = with pkgs; [
          ];

          # Build inputs (libraries and runtime dependencies)
          buildInputs =
            (with pkgs; [
              # Basic utilities
              ripgrep
              hexdump
              jq
              xxd

              # C standard library headers (needed for ctype.h, errno.h, etc.)
              glibc.dev # C standard library development headers

              # C compiler (both for compatibility)
              clang # Preferred C compiler
              gcc # Alternative C compiler (useful for compatibility testing)

              pkg-config # For library detection and linking
              cmake # CMake build system (for C emulator)
              gnumake # Make build system (for C emulator)
              # stdenv provides CC (C compiler) with proper includes

              # Display libraries - include all for maximum compatibility
              # SDL2: Required for C and Zig emulators
              SDL2 # SDL2 display backend (for C and Zig emulators)
              SDL2.dev # SDL2 development headers and pkg-config files

              # SDL3: Required for Lisp emulator (preferred)
              sdl3 # SDL3 display backend (for Lisp emulator, preferred)

              # X11: Required for C emulator X11 backend (alternative to SDL2)
              xorg.libX11 # X11 library (for C emulator X11 backend)
              xorg.libX11.dev # X11 development headers
              xorg.libXext # X11 extensions
              xorg.libXext.dev # X11 extensions development headers

              # Language compilers
              zig # Zig compiler (for Zig emulator)
              sbcl # Steel Bank Common Lisp (for Lisp emulator)

              # IDEs
              bun # For amp
              code-cursor
              vscode
            ])
            ++ (with llmsPkgs; [
              cursor-agent
              amp
              kilocode-cli
              opencode
              openskills
              openspec
              spec-kit
            ]);

          # Set up environment variables for pkg-config and C compilation
          shellHook = ''
            # Ensure C standard library headers are available
            export NIX_CFLAGS_COMPILE="-isystem ${pkgs.glibc.dev}/include $NIX_CFLAGS_COMPILE"

            # Set up library paths for runtime linking
            # Nix automatically sets up library paths, but we ensure SDL2/X11 are available
            export LD_LIBRARY_PATH="${
              pkgs.lib.makeLibraryPath [
                pkgs.SDL2
                pkgs.sdl3
                pkgs.xorg.libX11
                pkgs.xorg.libXext
              ]
            }:$LD_LIBRARY_PATH"

            # Set up PKG_CONFIG_PATH for pkg-config to find SDL2/X11
            export PKG_CONFIG_PATH="${
              pkgs.lib.makeSearchPath "lib/pkgconfig" [
                pkgs.SDL2
                pkgs.sdl3
                pkgs.xorg.libX11
                pkgs.xorg.libXext
              ]
            }:$PKG_CONFIG_PATH"

            echo "Maiko emulator development environment"
            echo "====================================="
            echo "C compiler: $(which clang || which gcc || echo 'Not found')"
            echo "Zig: $(which zig || echo 'Not found')"
            echo "SBCL: $(which sbcl || echo 'Not found')"
            echo ""
            echo "Display libraries:"
            pkg-config --exists x11 && echo "  ✓ X11 found ($(pkg-config --modversion x11))" || echo "  ✗ X11 not found"
            pkg-config --exists sdl2 && echo "  ✓ SDL2 found ($(pkg-config --modversion sdl2))" || echo "  ✗ SDL2 not found"
            pkg-config --exists sdl3 && echo "  ✓ SDL3 found ($(pkg-config --modversion sdl3))" || echo "  ✗ SDL3 not found (may need to build from source)"
            echo ""
            echo "Runtime library paths:"
            echo "  LD_LIBRARY_PATH includes SDL2, SDL3, X11 libraries"
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
            echo ""
            echo "Note: Emulators built in this shell will have access to SDL2/SDL3/X11 libraries"
          '';
        };
      }
    );
}
