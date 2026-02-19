# CMakeLists.txt for Maiko Introspection Module
#
# This builds the introspection library that links into the main emulator.
# Disabled by default - enable with -DMAIKO_INTROSPECT_ENABLED=ON

cmake_minimum_required(VERSION 3.15)

# Option to enable introspection (disabled by default)
option(MAIKO_INTROSPECT_ENABLED "Enable introspection module" OFF)

if(MAIKO_INTROSPECT_ENABLED)
    # Find SQLite3 (required for introspection)
    find_package(SQLite3 QUIET)

    if(SQLite3_FOUND)
    message(STATUS "SQLite3 found: Introspection module enabled")

    # Create introspection library
    add_library(maiko_introspect STATIC
        introspect.c
        introspect_db.c
    )

    # Include directories
    target_include_directories(maiko_introspect
        PUBLIC 
            ${CMAKE_CURRENT_SOURCE_DIR}
    )

    # Link libraries
    target_link_libraries(maiko_introspect
        PUBLIC
            SQLite::SQLite3
    )

    # Compiler features - match main project C standard
    target_compile_features(maiko_introspect PUBLIC c_std_99)

    # Warnings
    if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
        target_compile_options(maiko_introspect PRIVATE -Wall -Wextra)
    endif()

    # Export target for parent project
    set(MAIKO_INTROSPECT_FOUND TRUE PARENT_SCOPE)
    set(MAIKO_INTROSPECT_ENABLED TRUE PARENT_SCOPE)

else()
    message(STATUS "SQLite3 not found: Introspection module disabled")
    set(MAIKO_INTROSPECT_FOUND FALSE PARENT_SCOPE)
    set(MAIKO_INTROSPECT_ENABLED FALSE PARENT_SCOPE)
endif()

else()
    message(STATUS "Introspection module disabled (set -DMAIKO_INTROSPECT_ENABLED=ON to enable)")
    set(MAIKO_INTROSPECT_FOUND FALSE PARENT_SCOPE)
    set(MAIKO_INTROSPECT_ENABLED FALSE PARENT_SCOPE)
endif()
