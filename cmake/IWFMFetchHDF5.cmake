#***********************************************************************
#  IWFM CMake - HDF5 Dependency Module
#
#  Provides HDF5 with Fortran bindings for IWFM.
#  Requires: IWFMFetchZlib.cmake must be included first.
#
#  Build Methods:
#  - System HDF5: IWFM_USE_SYSTEM_HDF5=ON
#  - Bundled pre-built (Windows): IWFM_HDF5_USE_BUNDLED=ON
#  - ExternalProject (default): Downloads and builds HDF5 separately
#***********************************************************************

include_guard(GLOBAL)

# Verify zlib is available
if(NOT TARGET ZLIB::ZLIBSTATIC)
    message(FATAL_ERROR "IWFMFetchZlib.cmake must be included before IWFMFetchHDF5.cmake")
endif()

# =============================================================================
# Configuration Options
# =============================================================================
option(IWFM_USE_SYSTEM_HDF5 "Use system-installed HDF5" OFF)
option(IWFM_HDF5_USE_BUNDLED "Use bundled pre-built HDF5 (Windows fallback)" OFF)
set(IWFM_HDF5_VERSION "1.14.3" CACHE STRING "HDF5 version to fetch")

# =============================================================================
# Option 1: System HDF5
# =============================================================================
if(IWFM_USE_SYSTEM_HDF5)
    message(STATUS "Looking for system-installed HDF5...")
    find_package(HDF5 REQUIRED COMPONENTS Fortran)

    if(NOT TARGET HDF5::Fortran)
        add_library(HDF5::Fortran INTERFACE IMPORTED GLOBAL)
        set_target_properties(HDF5::Fortran PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${HDF5_Fortran_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${HDF5_Fortran_LIBRARIES};ZLIB::ZLIBSTATIC"
        )
    endif()

    set(HDF5_FORTRAN_MODULE_DIR "${HDF5_Fortran_INCLUDE_DIRS}" CACHE PATH "HDF5 Fortran module directory" FORCE)
    message(STATUS "System HDF5 found: ${HDF5_VERSION}")
    return()
endif()

# =============================================================================
# Option 2: Bundled Pre-built HDF5 (Windows fallback)
# =============================================================================
if(IWFM_HDF5_USE_BUNDLED AND WIN32)
    message(STATUS "Using bundled pre-built HDF5 libraries...")

    set(HDF5_BUNDLED_DIR "${CMAKE_SOURCE_DIR}/SourceCode/IWFM-kernel/HDF5")

    # Determine platform
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        set(HDF5_PLATFORM "x64")
    else()
        set(HDF5_PLATFORM "win32")
    endif()

    set(HDF5_RELEASE_DIR "${HDF5_BUNDLED_DIR}/${HDF5_PLATFORM}/Release")
    set(HDF5_DEBUG_DIR "${HDF5_BUNDLED_DIR}/${HDF5_PLATFORM}/Debug")

    # Check if bundled libraries exist
    if(NOT EXISTS "${HDF5_RELEASE_DIR}/libhdf5_fortran.lib")
        message(FATAL_ERROR "Bundled HDF5 libraries not found at ${HDF5_RELEASE_DIR}. "
                "Set IWFM_HDF5_USE_BUNDLED=OFF to build from source.")
    endif()

    # Create imported targets for HDF5 Fortran
    add_library(hdf5_fortran-static STATIC IMPORTED GLOBAL)
    set_target_properties(hdf5_fortran-static PROPERTIES
        IMPORTED_LOCATION "${HDF5_RELEASE_DIR}/libhdf5_fortran.lib"
        IMPORTED_LOCATION_DEBUG "${HDF5_DEBUG_DIR}/libhdf5_fortran_D.lib"
        IMPORTED_LOCATION_RELEASE "${HDF5_RELEASE_DIR}/libhdf5_fortran.lib"
    )

    add_library(hdf5_f90cstub-static STATIC IMPORTED GLOBAL)
    set_target_properties(hdf5_f90cstub-static PROPERTIES
        IMPORTED_LOCATION "${HDF5_RELEASE_DIR}/libhdf5_f90cstub.lib"
        IMPORTED_LOCATION_DEBUG "${HDF5_DEBUG_DIR}/libhdf5_f90cstub_D.lib"
        IMPORTED_LOCATION_RELEASE "${HDF5_RELEASE_DIR}/libhdf5_f90cstub.lib"
    )

    add_library(hdf5-static STATIC IMPORTED GLOBAL)
    set_target_properties(hdf5-static PROPERTIES
        IMPORTED_LOCATION "${HDF5_RELEASE_DIR}/libhdf5.lib"
        IMPORTED_LOCATION_DEBUG "${HDF5_DEBUG_DIR}/libhdf5_D.lib"
        IMPORTED_LOCATION_RELEASE "${HDF5_RELEASE_DIR}/libhdf5.lib"
    )

    # Create HDF5::Fortran interface bundling all components
    add_library(HDF5::Fortran INTERFACE IMPORTED GLOBAL)
    set_target_properties(HDF5::Fortran PROPERTIES
        INTERFACE_LINK_LIBRARIES "hdf5_fortran-static;hdf5_f90cstub-static;hdf5-static;ZLIB::ZLIBSTATIC"
    )

    set(HDF5_FORTRAN_MODULE_DIR "${HDF5_RELEASE_DIR}" CACHE PATH "HDF5 Fortran module directory" FORCE)
    set(HDF5_FORTRAN_MODULE_DIR_DEBUG "${HDF5_DEBUG_DIR}" CACHE PATH "HDF5 Fortran module directory (Debug)" FORCE)

    message(STATUS "Bundled HDF5 configured:")
    message(STATUS "  Platform: ${HDF5_PLATFORM}")
    message(STATUS "  Release libs: ${HDF5_RELEASE_DIR}")
    message(STATUS "  Debug libs: ${HDF5_DEBUG_DIR}")
    return()
endif()

# =============================================================================
# Option 3: Build HDF5 from Source via ExternalProject
# =============================================================================
# ExternalProject builds HDF5 as a separate CMake project.
# For static linking on Linux, we apply HDF5 libraries directly to executables
# using iwfm_link_hdf5() function to ensure proper linker flag handling.
# =============================================================================
include(ExternalProject)

message(STATUS "Configuring HDF5 ${IWFM_HDF5_VERSION} via ExternalProject...")

# =============================================================================
# HDF5 Install Location and Library Paths
# =============================================================================
set(HDF5_PREFIX "${CMAKE_BINARY_DIR}/hdf5-install")

# Platform-specific library names and paths
if(WIN32)
    set(HDF5_LIB_PREFIX "lib")
    set(HDF5_LIB_SUFFIX ".lib")
    set(HDF5_LIB_DIR "lib")
else()
    set(HDF5_LIB_PREFIX "lib")
    set(HDF5_LIB_SUFFIX ".a")
    set(HDF5_LIB_DIR "lib")
endif()

# Define library file paths - export as CACHE for use in other CMakeLists
set(IWFM_HDF5_FORTRAN_LIB "${HDF5_PREFIX}/${HDF5_LIB_DIR}/${HDF5_LIB_PREFIX}hdf5_fortran${HDF5_LIB_SUFFIX}" CACHE FILEPATH "HDF5 Fortran library" FORCE)
set(IWFM_HDF5_F90CSTUB_LIB "${HDF5_PREFIX}/${HDF5_LIB_DIR}/${HDF5_LIB_PREFIX}hdf5_f90cstub${HDF5_LIB_SUFFIX}" CACHE FILEPATH "HDF5 F90 C stub library" FORCE)
set(IWFM_HDF5_C_LIB "${HDF5_PREFIX}/${HDF5_LIB_DIR}/${HDF5_LIB_PREFIX}hdf5${HDF5_LIB_SUFFIX}" CACHE FILEPATH "HDF5 C library" FORCE)

set(HDF5_LIBRARIES
    "${IWFM_HDF5_FORTRAN_LIB}"
    "${IWFM_HDF5_F90CSTUB_LIB}"
    "${IWFM_HDF5_C_LIB}"
)

# Include directories
set(HDF5_INCLUDE_DIRS
    "${HDF5_PREFIX}/include"
    "${HDF5_PREFIX}/include/static"
)

# Fortran module directory (where .mod files are installed)
set(HDF5_FORTRAN_MODULE_DIR "${HDF5_PREFIX}/include/static" CACHE PATH "HDF5 Fortran module directory" FORCE)

# =============================================================================
# CMake Arguments for HDF5 Build
# =============================================================================
# CRITICAL: HDF5 Fortran flags must match IWFM's flags to ensure consistent
# symbol mangling. Intel Fortran's -standard-semantics flag affects module
# procedure name mangling (MP vs mp). Without this, IWFM generates symbols
# like h5lib_MP_h5open_f_ but HDF5 generates h5lib_mp_h5open_f_.
# Also need -fPIC on Unix for shared library (IWFM_C_DLL) support.
# =============================================================================
if(IWFM_USING_INTEL)
    if(UNIX)
        set(HDF5_FORTRAN_FLAGS "-standard-semantics -fPIC")
        set(HDF5_C_FLAGS "-fPIC")
    else()
        set(HDF5_FORTRAN_FLAGS "-standard-semantics")
        set(HDF5_C_FLAGS "")
    endif()
else()
    if(UNIX)
        set(HDF5_FORTRAN_FLAGS "-fPIC")
        set(HDF5_C_FLAGS "-fPIC")
    else()
        set(HDF5_FORTRAN_FLAGS "")
        set(HDF5_C_FLAGS "")
    endif()
endif()

set(hdf5_cmake_args
    # Install location - set BOTH CMAKE_INSTALL_PREFIX and HDF5's internal variable
    # HDF5 uses HDF5_INSTALL_PREFIX which defaults to CMAKE_INSTALL_PREFIX
    -DCMAKE_INSTALL_PREFIX:PATH=${HDF5_PREFIX}
    -DHDF5_INSTALL_PREFIX:PATH=${HDF5_PREFIX}

    # Build configuration
    -DCMAKE_BUILD_TYPE:STRING=Release

    # Compilers - MUST match IWFM's compilers for Fortran module compatibility
    -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
    -DCMAKE_Fortran_COMPILER:FILEPATH=${CMAKE_Fortran_COMPILER}

    # Fortran flags - MUST match IWFM's flags for symbol name mangling
    -DCMAKE_Fortran_FLAGS:STRING=${HDF5_FORTRAN_FLAGS}

    # C flags - need -fPIC on Unix for shared library support
    -DCMAKE_C_FLAGS:STRING=${HDF5_C_FLAGS}

    # Static libraries only
    -DBUILD_SHARED_LIBS:BOOL=OFF
    -DBUILD_STATIC_LIBS:BOOL=ON

    # Enable Fortran bindings (required for IWFM)
    -DHDF5_BUILD_FORTRAN:BOOL=ON

    # Disable unnecessary components
    -DHDF5_BUILD_CPP_LIB:BOOL=OFF
    -DHDF5_BUILD_EXAMPLES:BOOL=OFF
    -DHDF5_BUILD_TOOLS:BOOL=OFF
    -DHDF5_BUILD_HL_LIB:BOOL=OFF
    -DHDF5_BUILD_HL_FORTRAN:BOOL=OFF
    -DBUILD_TESTING:BOOL=OFF

    # Compression support - let HDF5 build its own internal zlib
    # This avoids complex cross-project zlib sharing issues
    -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON
    -DZLIB_USE_EXTERNAL:BOOL=OFF
    -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF

    # Disable parallel HDF5 (MPI)
    -DHDF5_ENABLE_PARALLEL:BOOL=OFF

    # Suppress warnings and header generation
    -DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=ON
    -DHDF5_GENERATE_HEADERS:BOOL=OFF

    # Disable CPack packaging to avoid install path issues
    -DHDF5_NO_PACKAGES:BOOL=ON
)

# Windows: Use static CRT to match IWFM's /MT runtime
# This avoids linker errors like "unresolved external symbol __imp_fdopen"
if(WIN32)
    list(APPEND hdf5_cmake_args
        -DCMAKE_MSVC_RUNTIME_LIBRARY:STRING=MultiThreaded
    )
endif()

# Windows with Intel icx: __float128 is not supported
# Use CMAKE_CACHE_ARGS to force these values before HDF5's CMake runs its detection
# Also force the install prefix via cache to override any HDF5 defaults
set(hdf5_cache_args
    -DCMAKE_INSTALL_PREFIX:PATH=${HDF5_PREFIX}
    -DHDF5_INSTALL_PREFIX:PATH=${HDF5_PREFIX}
)
if(WIN32 AND CMAKE_C_COMPILER_ID MATCHES "IntelLLVM")
    list(APPEND hdf5_cache_args
        -DH5_HAVE___FLOAT128:INTERNAL=0
        -DH5_SIZEOF___FLOAT128:INTERNAL=0
        -DH5_PAC_C_MAX_REAL_PRECISION:INTERNAL=15
    )
endif()

# =============================================================================
# HDF5 Download URL
# =============================================================================
# Use GitHub archive URL (always available for any tag)
string(REPLACE "." "_" HDF5_VERSION_UNDERSCORES "${IWFM_HDF5_VERSION}")
set(HDF5_URL "https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-${HDF5_VERSION_UNDERSCORES}.tar.gz")

# =============================================================================
# HDF5 Patch Command for Windows with Intel icx
# =============================================================================
# Intel icx on Windows doesn't support __float128 type. HDF5's configure check
# passes but actual compilation fails. We patch all affected source files to
# replace sizeof(__float128) with sizeof(long double).
# =============================================================================
set(HDF5_PATCH_COMMAND "")
if(WIN32 AND CMAKE_C_COMPILER_ID MATCHES "IntelLLVM")
    # Create a CMake script to patch the files
    set(HDF5_PATCH_SCRIPT "${CMAKE_BINARY_DIR}/patch_hdf5_float128.cmake")
    file(WRITE ${HDF5_PATCH_SCRIPT} "
# Patch HDF5 source files to disable __float128 on Windows with Intel icx
# Files known to use __float128: H5match_types.c, H5_f.c

set(FILES_TO_PATCH
    \"\${SOURCE_DIR}/fortran/src/H5match_types.c\"
    \"\${SOURCE_DIR}/fortran/src/H5_f.c\"
)

foreach(FILE_PATH \${FILES_TO_PATCH})
    if(EXISTS \"\${FILE_PATH}\")
        file(READ \"\${FILE_PATH}\" CONTENT)
        # Replace sizeof(__float128) with sizeof(long double)
        string(REPLACE
            \"sizeof(__float128)\"
            \"sizeof(long double) /* __float128 disabled */\"
            CONTENT \"\${CONTENT}\")
        # Also guard any #if H5_HAVE___FLOAT128 blocks
        string(REPLACE
            \"#if H5_HAVE___FLOAT128\"
            \"#if 0 /* H5_HAVE___FLOAT128 disabled on Windows */\"
            CONTENT \"\${CONTENT}\")
        file(WRITE \"\${FILE_PATH}\" \"\${CONTENT}\")
        message(STATUS \"Patched \${FILE_PATH} to disable __float128\")
    endif()
endforeach()
")
    set(HDF5_PATCH_COMMAND ${CMAKE_COMMAND} -DSOURCE_DIR=<SOURCE_DIR> -P ${HDF5_PATCH_SCRIPT})
endif()

# =============================================================================
# ExternalProject_Add
# =============================================================================
ExternalProject_Add(HDF5_External
    URL ${HDF5_URL}
    PATCH_COMMAND ${HDF5_PATCH_COMMAND}
    CMAKE_ARGS ${hdf5_cmake_args}
    CMAKE_CACHE_ARGS ${hdf5_cache_args}
    BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
    CONFIGURE_HANDLED_BY_BUILD ON
    USES_TERMINAL_DOWNLOAD ON
    USES_TERMINAL_CONFIGURE ON
    USES_TERMINAL_BUILD ON
    USES_TERMINAL_INSTALL ON
)

# =============================================================================
# Create include directories (required for INTERFACE_INCLUDE_DIRECTORIES)
# =============================================================================
file(MAKE_DIRECTORY "${HDF5_PREFIX}/include")
file(MAKE_DIRECTORY "${HDF5_PREFIX}/include/static")

# =============================================================================
# Create HDF5::HDF5 interface target with includes and libraries
# =============================================================================
# With OBJECT library approach for iwfm_kernel, the linker sees all object files
# directly, so HDF5 symbol references are active when processing HDF5 libraries.
# This should resolve the previous link order issues on Linux.
# =============================================================================
add_library(HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
set_target_properties(HDF5::HDF5 PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
    INTERFACE_LINK_LIBRARIES "${IWFM_HDF5_FORTRAN_LIB};${IWFM_HDF5_F90CSTUB_LIB};${IWFM_HDF5_C_LIB}"
)
add_dependencies(HDF5::HDF5 HDF5_External)

# Unix system libraries needed by HDF5
if(UNIX)
    set_property(TARGET HDF5::HDF5 APPEND PROPERTY
        INTERFACE_LINK_LIBRARIES "m;dl"
    )
endif()

# Create alias for compatibility with code expecting HDF5::Fortran
add_library(HDF5_Fortran_Interface INTERFACE)
target_link_libraries(HDF5_Fortran_Interface INTERFACE HDF5::HDF5)
target_include_directories(HDF5_Fortran_Interface INTERFACE ${HDF5_INCLUDE_DIRS})
add_library(HDF5::Fortran ALIAS HDF5_Fortran_Interface)

# =============================================================================
# Function: Apply HDF5 linking directly to a target (DEPRECATED)
# =============================================================================
# NOTE: With the OBJECT library approach for iwfm_kernel, this function is
# no longer needed. HDF5 libraries are now linked through the iwfm_dependencies
# interface chain. The OBJECT library ensures all object files are linked
# directly into executables, so HDF5 symbol references are active when the
# linker processes HDF5 static libraries.
#
# This function is kept as a fallback in case the OBJECT library approach
# doesn't work in some environments. To use it, call iwfm_link_hdf5(target)
# for each executable on Unix platforms.
# =============================================================================
function(iwfm_link_hdf5 target)
    if(NOT TARGET HDF5_External)
        message(WARNING "iwfm_link_hdf5: HDF5_External not found, skipping ${target}")
        return()
    endif()

    # Ensure HDF5 is built before the target
    add_dependencies(${target} HDF5_External)

    if(UNIX AND NOT APPLE)
        # Linux: Use --whole-archive to force inclusion of all HDF5 Fortran symbols
        target_link_libraries(${target} PRIVATE
            "-Wl,--whole-archive"
            "${IWFM_HDF5_FORTRAN_LIB}"
            "${IWFM_HDF5_F90CSTUB_LIB}"
            "-Wl,--no-whole-archive"
            "${IWFM_HDF5_C_LIB}"
            m
            dl
        )
        message(STATUS "Applied HDF5 static linking to: ${target}")

    elseif(APPLE)
        # macOS: Use -force_load for each Fortran library
        target_link_libraries(${target} PRIVATE
            "-Wl,-force_load,${IWFM_HDF5_FORTRAN_LIB}"
            "-Wl,-force_load,${IWFM_HDF5_F90CSTUB_LIB}"
            "${IWFM_HDF5_C_LIB}"
            m
        )
        message(STATUS "Applied HDF5 static linking to: ${target}")

    else()
        # Windows: Handled via HDF5::HDF5 interface target
        message(STATUS "HDF5 linking for ${target} handled via interface target")
    endif()
endfunction()

message(STATUS "HDF5 ${IWFM_HDF5_VERSION} configured via ExternalProject")
message(STATUS "  Download URL: ${HDF5_URL}")
message(STATUS "  Install prefix: ${HDF5_PREFIX}")
message(STATUS "  Fortran library: ${IWFM_HDF5_FORTRAN_LIB}")
message(STATUS "  Module directory: ${HDF5_FORTRAN_MODULE_DIR}")
