# CMakeLists.txt for Maiko Introspection Module
#
# This builds the introspection library that links into the main emulator.

cmake_minimum_required(VERSION 3.14)

# Find SQLite3
find_package(SQLite3 REQUIRED)

# Create introspection library
add_library(maiko_introspect STATIC
    introspect.c
    introspect_db.c
)

# Include directories
target_include_directories(maiko_introspect
    PUBLIC 
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${SQLite3_INCLUDE_DIRS}
)

# Link libraries
target_link_libraries(maiko_introspect
    PUBLIC
        ${SQLite3_LIBRARIES}
)

# Compiler features
target_compile_features(maiko_introspect PUBLIC c_std_11)

# Warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(maiko_introspect PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Export target for parent project
set(MAIKO_INTROSPECT_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE)
set(MAIKO_INTROSPECT_LIBRARIES maiko_introspect PARENT_SCOPE)

# Install rules
install(TARGETS maiko_introspect
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

install(FILES 
    introspect.h
    DESTINATION include/maiko/introspect
)

install(FILES 
    schema.sql
    DESTINATION share/maiko/introspect
)
