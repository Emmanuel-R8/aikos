CMAKE_MINIMUM_REQUIRED(VERSION 3.15)
PROJECT(maiko C)

SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)
SET(CMAKE_C_STANDARD 99)

IF(CMAKE_C_COMPILER_ID MATCHES "Clang" OR CMAKE_COMPILER_ID MATCHES "GNU")
  IF(UNIX AND CMAKE_GENERATOR STREQUAL "Ninja")
    SET(CMAKE_C_FLAGS "-fdiagnostics-color=always ${CMAKE_C_FLAGS}")
  ENDIF()
  SET(CMAKE_C_FLAGS "-fno-strict-aliasing ${CMAKE_C_FLAGS}")
ENDIF()

INCLUDE(CheckLibraryExists)
CHECK_LIBRARY_EXISTS(m fmod "" NEED_LIB_M)
IF(NEED_LIB_M)
  SET(MAIKO_LIBRARIES m)
ENDIF()

SET(MAIKO_RELEASE 351 CACHE STRING "Release version to build")
MESSAGE("-- Configured for release ${MAIKO_RELEASE}")
SET(MAIKO_DEFINITIONS "-DRELEASE=${MAIKO_RELEASE}")

# SDL display (required)
SET(MAIKO_SDL_VERSION 2 CACHE STRING "SDL version: 2 or 3")
SET_PROPERTY(CACHE MAIKO_SDL_VERSION PROPERTY STRINGS 2 3)

IF(MAIKO_SDL_VERSION STREQUAL "3")
  FIND_PACKAGE(SDL3 REQUIRED CONFIG COMPONENTS SDL3)
  SET(MAIKO_SDL_DEFINITIONS "-DSDL=3")
  SET(MAIKO_SDL_LIBRARIES SDL3::SDL3)
  MESSAGE("-- Configured for SDL3 display")
ELSE()
  FIND_PACKAGE(SDL2 REQUIRED CONFIG COMPONENTS SDL2)
  SET(MAIKO_SDL_DEFINITIONS "-DSDL=2")
  SET(MAIKO_SDL_LIBRARIES SDL2::SDL2)
  MESSAGE("-- Configured for SDL2 display")
ENDIF()

# Networking support (optional)
SET(MAIKO_NETWORK_TYPE NONE CACHE STRING "Type of networking: NONE or NETHUB")
SET_PROPERTY(CACHE MAIKO_NETWORK_TYPE PROPERTY STRINGS NONE NETHUB)

IF(MAIKO_NETWORK_TYPE STREQUAL "NETHUB")
  LIST(APPEND MAIKO_DEFINITIONS "-DMAIKO_ENABLE_NETHUB")
  MESSAGE("-- Configured for NETHUB network support")
ELSEIF(NOT MAIKO_NETWORK_TYPE STREQUAL "NONE")
  MESSAGE(WARNING "Invalid MAIKO_NETWORK_TYPE, must be NONE or NETHUB")
ENDIF()

# Introspection module (optional, requires SQLite3)
add_subdirectory(src/introspect)

IF(APPLE)
  SET(CMAKE_C_ARCHIVE_CREATE "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
  SET(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
ENDIF()

SET(MAIKO_SRCS
  src/allocmds.c
  src/arithops.c
  src/arrayops.c
  src/asmbbt.c
  src/bbtsub.c
  src/bin.c
  src/binds.c
  src/bitblt.c
  src/blt.c
  src/byteswap.c
  src/car-cdr.c
  src/chardev.c
  src/common.c
  src/conspage.c
  src/dbgtool.c
  src/dir.c
  src/dlpi.c
  src/draw.c
  src/dsk.c
  src/dspif.c
  src/dspsubrs.c
  src/eqf.c
  src/ether_common.c
  src/ether_nethub.c
  src/execution_trace.c
  src/findkey.c
  src/foreign.c
  src/fp.c
  src/fvar.c
  src/gc2.c
  src/gcarray.c
  src/gc.c
  src/gccode.c
  src/gcfinal.c
  src/gchtfind.c
  src/gcmain3.c
  src/gcoflow.c
  src/gcr.c
  src/gcrcell.c
  src/gcscan.c
  src/gvar2.c
  src/hardrtn.c
  src/inet.c
  src/initdsp.c
  src/initkbd.c
  src/initsout.c
  src/intcall.c
  src/kbdsubrs.c
  src/keyevent.c
  src/kprint.c
  src/ldsout.c
  src/lineblt8.c
  src/lisp2c.c
  src/llcolor.c
  src/llstk.c
  src/loopsops.c
  src/lowlev1.c
  src/lowlev2.c
  src/lsthandl.c
  src/misc7.c
  src/miscn.c
  src/mkatom.c
  src/mkcell.c
  src/mouseif.c
  src/mvs.c
  src/osmsg.c
  src/perrno.c
  src/return.c
  src/rpc.c
  src/rplcons.c
  src/shift.c
  src/storage.c
  src/subr0374.c
  src/subr.c
  src/sxhash.c
  src/testtool.c
  src/timer.c
  src/tty.c
  src/typeof.c
  src/ubf1.c
  src/ubf2.c
  src/ubf3.c
  src/ufn.c
  src/ufs.c
  src/unixcomm.c
  src/unwind.c
  src/uraid.c
  src/usrsubr.c
  src/uutils.c
  src/vars3.c
  src/vmemsave.c
  src/xc.c
  src/z2.c
  src/sdl.c
)

# Utility: mkvdate (needed for vdate.c generation)
# Get git version for build info
FIND_PACKAGE(Git QUIET)
IF(GIT_FOUND)
  EXECUTE_PROCESS(
    COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
    OUTPUT_VARIABLE GIT_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )
  EXECUTE_PROCESS(
    COMMAND ${GIT_EXECUTABLE} status --porcelain
    OUTPUT_VARIABLE GIT_STATUS
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  )
  IF(GIT_STATUS)
    SET(GIT_VERSION "${GIT_VERSION}-dirty")
  ENDIF()
ELSE()
  SET(GIT_VERSION "unknown")
ENDIF()

ADD_EXECUTABLE(mkvdate src/mkvdate.c)
TARGET_COMPILE_DEFINITIONS(mkvdate PRIVATE ${MAIKO_DEFINITIONS} "MAIKO_GIT_VERSION=\"${GIT_VERSION}\"")
TARGET_INCLUDE_DIRECTORIES(mkvdate PRIVATE inc)

ADD_CUSTOM_COMMAND(OUTPUT vdate.c
  COMMAND mkvdate > vdate.c
  DEPENDS mkvdate
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# Main emulator executable
ADD_EXECUTABLE(ldesdl
  src/main.c
  vdate.c
  ${MAIKO_SRCS}
)
TARGET_COMPILE_DEFINITIONS(ldesdl PRIVATE ${MAIKO_DEFINITIONS} ${MAIKO_SDL_DEFINITIONS})
if(MAIKO_INTROSPECT_ENABLED)
  TARGET_COMPILE_DEFINITIONS(ldesdl PRIVATE INTROSPECT_ENABLED)
endif()
TARGET_INCLUDE_DIRECTORIES(ldesdl PRIVATE inc ${CMAKE_SOURCE_DIR}/src)

IF(APPLE)
  IF(MAIKO_SDL_VERSION STREQUAL "3")
    MESSAGE("-- Applying fixup for macOS RPATH for SDL3.xcframework")
    SET_PROPERTY(TARGET ldesdl APPEND PROPERTY BUILD_RPATH "/Library/Frameworks/SDL3.xcframework/macos-arm64_x86_64")
  ENDIF()
ENDIF()

TARGET_LINK_LIBRARIES(ldesdl ${MAIKO_LIBRARIES} ${MAIKO_SDL_LIBRARIES} $<$<TARGET_EXISTS:maiko_introspect>:maiko_introspect>)

# Other utility executables
ADD_EXECUTABLE(setsout src/setsout.c src/byteswap.c)
TARGET_COMPILE_DEFINITIONS(setsout PRIVATE ${MAIKO_DEFINITIONS})
TARGET_INCLUDE_DIRECTORIES(setsout PRIVATE inc)

ADD_EXECUTABLE(tstsout src/tstsout.c src/byteswap.c)
TARGET_COMPILE_DEFINITIONS(tstsout PRIVATE ${MAIKO_DEFINITIONS})
TARGET_INCLUDE_DIRECTORIES(tstsout PRIVATE inc)
