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

    # Prefer static library to avoid runtime DLL dependency.
    # CMake's FindZLIB picks zlib.lib (DLL import lib) over zlibstatic.lib
    # when both are present, which causes STATUS_DLL_NOT_FOUND at runtime.
    if(ZLIB_ROOT)
        find_library(_ZLIB_STATIC_LIB NAMES zlibstatic PATHS "${ZLIB_ROOT}/lib" NO_DEFAULT_PATH)
    else()
        find_library(_ZLIB_STATIC_LIB NAMES zlibstatic)
    endif()

    if(_ZLIB_STATIC_LIB)
        message(STATUS "Found static zlib: ${_ZLIB_STATIC_LIB}")
        # Find headers
        if(ZLIB_ROOT)
            find_path(_ZLIB_INCLUDE_DIR NAMES zlib.h PATHS "${ZLIB_ROOT}/include" NO_DEFAULT_PATH)
        else()
            find_path(_ZLIB_INCLUDE_DIR NAMES zlib.h)
        endif()
        if(NOT _ZLIB_INCLUDE_DIR)
            message(FATAL_ERROR "zlib headers not found")
        endif()

        add_library(ZLIB::ZLIB STATIC IMPORTED)
        set_target_properties(ZLIB::ZLIB PROPERTIES
            IMPORTED_LOCATION "${_ZLIB_STATIC_LIB}"
            INTERFACE_INCLUDE_DIRECTORIES "${_ZLIB_INCLUDE_DIR}"
        )
        add_library(ZLIB::ZLIBSTATIC ALIAS ZLIB::ZLIB)
    else()
        # Fallback to CMake's FindZLIB
        find_package(ZLIB REQUIRED)
        if(NOT TARGET ZLIB::ZLIBSTATIC)
            add_library(ZLIB::ZLIBSTATIC ALIAS ZLIB::ZLIB)
        endif()
    endif()

    message(STATUS "System zlib configured (static)")
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
