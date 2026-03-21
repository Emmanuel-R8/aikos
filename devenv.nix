{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

let
  pkgs-stable = import inputs.nixpkgs-stable {
    system = pkgs.stdenv.system;
    config.allowUnfree = true;
  };
  llmsPkgs = inputs.llms.packages.${pkgs.stdenv.system};
in
{
  # https://devenv.sh/basics/
  env.GREET = "Interlisp Emulator Development Environment";

  # https://devenv.sh/packages/
  packages =
    (with pkgs; [
      # Basic utilities
      ripgrep
      hexdump
      jq
      xxd

      # C compiler
      clang
      gcc
      glibc.dev

      # Build systems
      pkg-config
      cmake
      gnumake

      # Libraries
      sqlite
      sqlit-tui
      sqlite.dev
      sqlite.out
      SDL2
      SDL2.dev
      sdl3
      openssl

      # Languages
      zig
      sbcl
      ecl
      rlwrap
      bun
      vscode
    ])
    ++ (with llmsPkgs; [
      copilot-cli
      cursor-agent
      amp
      kilocode-cli
      opencode
      openspec
      spec-kit
    ]);

  # https://devenv.sh/scripts/
  enterShell = ''
    echo $GREET
    echo "====================================="
    echo "C compiler: $(which clang || which gcc || echo 'Not found')"
    echo "Zig: $(which zig || echo 'Not found')"
    echo "SBCL: $(which sbcl || echo 'Not found')"
    echo ""
    echo "Display libraries:"
    pkg-config --exists sdl2 && echo "  ✓ SDL2 found ($(pkg-config --modversion sdl2))" || echo "  ✗ SDL2 not found"
    pkg-config --exists sdl3 && echo "  ✓ SDL3 found ($(pkg-config --modversion sdl3))" || echo "  ✗ SDL3 not found"
    echo ""
    echo "Ready to build emulators!"
  '';

  # https://devenv.sh/languages/
  languages.c.enable = true;
  languages.zig.enable = true;
  languages.nix.enable = true;

  # Environment variables
  env.NIX_CFLAGS_COMPILE = "-isystem ${pkgs.glibc.dev}/include";

  # Set up library paths for runtime linking
  env.LD_LIBRARY_PATH = "${pkgs.openssl.out}/lib:${
    pkgs.lib.makeLibraryPath (
      with pkgs;
      [
        openssl
        SDL2
        sdl3
      ]
    )
  }";

  # Set up PKG_CONFIG_PATH for pkg-config
  env.PKG_CONFIG_PATH = "${pkgs.lib.makeSearchPath "lib/pkgconfig" (
    with pkgs;
    [
      SDL2
      sdl3
    ]
  )}";
}
