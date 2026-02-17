# Maiko

Maiko is the implementation of the Medley Interlisp virtual machine for a
byte-coded Lisp instruction set, and some low-level functions for
connecting Lisp to a display (via SDL), the local filesystem,
and a network subsystem.

For an overview, see [Medley Interlisp Introduction](https://interlisp.org/medley/using/docs/medley/).

See [the Medley repository](https://github.com/Interlisp/medley) for
* [Issues](https://github.com/Interlisp/medley/issues) (note that maiko issues are there too)
* [Discussions](https://github.com/Interlisp/medley/discussions) (Q&A, announcements, etc)
* [Medley's README](https://github.com/Interlisp/medley/blob/master/README.md)

Bug reports, feature requests, fixes and improvements, support for additional platforms and hardware are all welcome.

## Development Platforms

Development has been primarily on macOS, FreeBSD, and Linux.
Processor architectures i386, x86\_64, arm64, and arm7l.


## Building Maiko

### Building with CMake

Building requires a C compiler (`clang` or `gcc`), CMake, and SDL2 or SDL3.

```sh
# Install dependencies (Debian/Ubuntu)
sudo apt update
sudo apt install clang cmake libsdl2-dev

# Or for SDL3
sudo apt install clang cmake libsdl3-dev
```

```sh
cd maiko
cmake .
make
```

This produces the `ldesdl` executable.

CMake options:
* `MAIKO_SDL_VERSION`: [2] or 3 - selects SDL version
* `MAIKO_NETWORK_TYPE`: [NONE] or NETHUB - network subsystem access
* `MAIKO_RELEASE`: [351] - see `maiko/inc/version.h`

Optional: SQLite3 for introspection debugging support. If available, introspection is enabled automatically.

### Building For macOS

Building/running on macOS requires SDL2 or SDL3 library from https://libsdl.org

For SDL3 on macOS, the build applies an RPATH fix for the xcframework.
