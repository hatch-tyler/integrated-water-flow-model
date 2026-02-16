#***********************************************************************
#  IWFM CMake - Zlib Dependency Module
#
#  Provides zlib as a shared dependency for HDF5 and heclib.
#  Must be included BEFORE IWFMFetchHDF5.cmake and IWFMFetchHeclib.cmake.
#***********************************************************************

include_guard(GLOBAL)
include(FetchContent)

# Option to use system-installed zlib instead of building from source
option(IWFM_USE_SYSTEM_ZLIB "Use system-installed zlib instead of FetchContent" OFF)

if(IWFM_USE_SYSTEM_ZLIB)
    message(STATUS "Looking for system-installed zlib...")
    find_package(ZLIB REQUIRED)

    # Create static alias if needed for consistency
    if(NOT TARGET ZLIB::ZLIBSTATIC)
        add_library(ZLIB::ZLIBSTATIC ALIAS ZLIB::ZLIB)
    endif()

    message(STATUS "System zlib found: ${ZLIB_VERSION_STRING}")
    return()
endif()

# =============================================================================
# FetchContent: zlib v1.3.1
# =============================================================================
message(STATUS "Configuring zlib v1.3.1 via FetchContent...")

FetchContent_Declare(zlib
    GIT_REPOSITORY https://github.com/madler/zlib.git
    GIT_TAG        v1.3.1
    GIT_SHALLOW    TRUE
)

# Configure zlib build options
set(ZLIB_BUILD_EXAMPLES OFF CACHE BOOL "Disable zlib examples" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build static libraries" FORCE)

# Suppress zlib install targets to avoid conflicts
set(SKIP_INSTALL_ALL ON CACHE BOOL "Skip zlib install" FORCE)

FetchContent_MakeAvailable(zlib)

# =============================================================================
# Create Standardized Aliases
# =============================================================================
# zlib's CMake creates 'zlibstatic' for static builds
if(TARGET zlibstatic)
    # Create ZLIB:: namespace targets for compatibility
    add_library(ZLIB::ZLIBSTATIC ALIAS zlibstatic)
    add_library(ZLIB::ZLIB ALIAS zlibstatic)
    message(STATUS "Created zlib aliases: ZLIB::ZLIBSTATIC, ZLIB::ZLIB -> zlibstatic")
elseif(TARGET zlib)
    # Fallback if only 'zlib' target exists
    add_library(ZLIB::ZLIBSTATIC ALIAS zlib)
    add_library(ZLIB::ZLIB ALIAS zlib)
    message(STATUS "Created zlib aliases: ZLIB::ZLIBSTATIC, ZLIB::ZLIB -> zlib")
else()
    message(FATAL_ERROR "zlib target not found after FetchContent_MakeAvailable")
endif()

# Store include directories for downstream consumers (separate PATH variables)
set(ZLIB_INCLUDE_DIR_SOURCE "${zlib_SOURCE_DIR}" CACHE PATH "Zlib source include directory" FORCE)
set(ZLIB_INCLUDE_DIR_BINARY "${zlib_BINARY_DIR}" CACHE PATH "Zlib binary include directory (zconf.h)" FORCE)

# Set MSVC runtime library for zlib to match IWFM
if(MSVC AND TARGET zlibstatic)
    set_property(TARGET zlibstatic PROPERTY
        MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

# Enable PIC for zlib on Unix (required for shared library builds)
if(UNIX AND TARGET zlibstatic)
    set_property(TARGET zlibstatic PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

message(STATUS "zlib configured successfully")
message(STATUS "  Source: ${zlib_SOURCE_DIR}")
message(STATUS "  Binary: ${zlib_BINARY_DIR}")
