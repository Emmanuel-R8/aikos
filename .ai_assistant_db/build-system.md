# Build System Documentation

**Navigation**: [README](README.md) | [Index](INDEX.md) | [Architecture](architecture.md) | [Components](components/) | [API](api/) | [Glossary](glossary.md)

Maiko supports two build systems: CMake (modern) and Make (traditional).

**Related Documentation**:

- [Architecture Overview](architecture.md) - Platform abstraction details
- [Platform-Specific Considerations](#platform-specific-considerations) - OS and architecture support

## CMake Build System

### Configuration

The CMake build is configured via `maiko/CMakeLists.txt`:

```bash
cmake -DMAIKO_RELEASE=351 \
      -DMAIKO_DISPLAY_X11=ON \
      -DMAIKO_DISPLAY_SDL=OFF \
      -DMAIKO_NETWORK_TYPE=NONE \
      <source_directory>
```

### Build Options

#### Release Version (`MAIKO_RELEASE`)

- **Values**: 115, 200, 201, 210, 300, 350, 351
- **Default**: 351
- **Purpose**: Sets feature flags and version compatibility

#### Display Subsystem (`MAIKO_DISPLAY_X11`, `MAIKO_DISPLAY_SDL`)

- **X11**: `MAIKO_DISPLAY_X11=ON` (default)
- **SDL**: `MAIKO_DISPLAY_SDL=2` or `MAIKO_DISPLAY_SDL=3`
- **Purpose**: Select display backend

#### Network Type (`MAIKO_NETWORK_TYPE`)

- **Values**: NONE, SUN_DLPI, SUN_NIT, NETHUB
- **Default**: NONE
- **Purpose**: Select network subsystem

### Build Process

1. **Configure**: `cmake <options> <source_dir>`
2. **Build**: `cmake --build .` or `make`
3. **Output**: Binaries in build directory

### Output Files

- `lde` - Lisp Development Environment (init version)
- `ldex` - Lisp Development Environment (execution version)

## Make Build System

### Configuration

The Make build uses platform-specific makefile fragments:

```bash
cd maiko/bin
./makeright x    # X11 display
./makeright sdl  # SDL display
```

### Platform Detection

The build system automatically detects:

- **OS**: linux, darwin, freebsd, sunos, windows
- **Architecture**: x86_64, i386, arm64, arm7l, sparc

### Makefile Structure

- **Main Makefile**: `maiko/bin/makeright`
- **Platform Fragments**: `makefile-<os>.<arch>-x`
- **Build Scripts**: Platform-specific build scripts

### Build Process

1. **Detect Platform**: Automatic OS/arch detection
2. **Select Makefile**: Choose platform-specific fragment
3. **Build**: Execute make with selected configuration
4. **Output**: Binaries in `../<os>.<arch>/`

## Platform-Specific Considerations

### Linux

- Requires X11 libraries (`libx11-dev`) or SDL2
- Supports x86_64, i386, arm64, arm7l

### macOS

- Requires XQuartz (X11) or SDL2
- Supports x86_64, arm64
- Uses `darwin` as OS name

### FreeBSD

- Similar to Linux
- Supports x86_64, i386

### Solaris

- Requires SunOS-specific network libraries
- Supports SPARC, x86_64

### Windows

- Limited support
- May require MinGW or Cygwin

## Compiler Options

### Default Compiler

- **Preferred**: `clang`
- **Alternative**: `gcc`
- **Override**: Edit platform makefile fragment

### Compiler Flags

- `-fno-strict-aliasing` - Required for correct operation
- `-DRELEASE=<version>` - Release version
- `-DXWINDOW` - X11 display
- `-DSDL=2` or `-DSDL=3` - SDL display
- Platform-specific flags

## Feature Flags

### Memory Model

- `BIGVM` - Large virtual memory (releases 210+)
- `BIGBIGVM` - Very large VM (releases 350+)
- `NEWCDRCODING` - New CDR coding scheme

### Display

- `XWINDOW` - X11 display
- `SDL=2` or `SDL=3` - SDL display version

### Network

- `MAIKO_ENABLE_ETHERNET` - Ethernet support
- `USE_DLPI` - DLPI network
- `USE_NIT` - NIT network
- `MAIKO_ENABLE_NETHUB` - NETHUB support

### Other Features

- `BIGATOMS` - Large atom indices (releases 200+)
- `OPDISP` - Computed goto dispatch (GCC)
- `PROFILE` - Profiling support
- `DEBUG` - Debug builds

## Version Compatibility

### Version Numbers

Defined in `maiko/inc/version.h`:

- **LVERSION**: Minimum Lisp version
- **MINBVERSION**: Emulator version

### Release Features

#### Release 115

- Small atoms
- Basic VM

#### Release 200

- Big atoms
- Enhanced features

#### Release 201

- European keyboard support

#### Release 210

- Big VM support
- New CDR coding

#### Release 300

- Big VM release

#### Release 350

- 256MB VM support

#### Release 351

- 256MB VM with cursor fix

## Build Dependencies

### Required

- C compiler (clang or gcc)
- Make or CMake
- X11 libraries (for X11) or SDL2/SDL3 (for SDL)

### Optional

- Math library (`libm`)
- Network libraries (for networking)

## Troubleshooting

### Common Issues

1. **Platform Detection Fails**
   - Manually edit makefile fragment
   - Set platform variables explicitly

2. **Missing Libraries**
   - Install development packages
   - Check library paths

3. **Version Mismatch**
   - Ensure RELEASE matches sysout version
   - Check version.h compatibility

4. **Display Issues**
   - Verify X11/SDL installation
   - Check display permissions

## Related Documentation

- [Architecture](architecture.md) - System architecture
  - [Platform Abstraction](architecture.md#platform-abstraction) - How platform differences are handled
- [Components](components/) - Component details
  - [VM Core](components/vm-core.md) - Execution engine
  - [Memory Management](components/memory-management.md) - Memory system
  - [Display](components/display.md) - Display subsystems
  - [I/O Systems](components/io.md) - I/O systems
- [Glossary](glossary.md) - Build-related terms
  - [Build Terms](glossary.md#build-terms) - Build system terminology
  - [Version Terms](glossary.md#version-terms) - Version compatibility
