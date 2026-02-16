#***********************************************************************
#  IWFM CMake - heclib (HEC-DSS 7) Dependency Module
#
#  Provides the HEC-DSS 7 C library for time series data I/O.
#  Requires: IWFMFetchZlib.cmake must be included first.
#
#  Key Insight: We bypass heclib's CMakeLists.txt to avoid duplicate
#  zlib fetching and ensure we use the shared zlib instance.
#***********************************************************************

include_guard(GLOBAL)
include(FetchContent)

# Verify zlib is available
if(NOT TARGET ZLIB::ZLIBSTATIC)
    message(FATAL_ERROR "IWFMFetchZlib.cmake must be included before IWFMFetchHeclib.cmake")
endif()

# =============================================================================
# Configuration Options
# =============================================================================
option(IWFM_USE_SYSTEM_HECLIB "Use pre-built heclib from source tree" OFF)

# =============================================================================
# Option 1: Use Pre-built heclib (Windows bundled libraries)
# =============================================================================
if(IWFM_USE_SYSTEM_HECLIB)
    message(STATUS "Using pre-built heclib from source tree...")

    set(HECLIB_DIR "${CMAKE_SOURCE_DIR}/SourceCode/IWFM-kernel/heclib")

    add_library(heclib STATIC IMPORTED GLOBAL)

    if(WIN32)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            # x64 platform
            if(EXISTS "${HECLIB_DIR}/heclib_x64.lib")
                set_target_properties(heclib PROPERTIES
                    IMPORTED_LOCATION "${HECLIB_DIR}/heclib_x64.lib"
                    IMPORTED_LOCATION_DEBUG "${HECLIB_DIR}/heclib_x64_D.lib"
                    IMPORTED_LOCATION_RELEASE "${HECLIB_DIR}/heclib_x64.lib"
                )
            else()
                message(FATAL_ERROR "Pre-built heclib not found at ${HECLIB_DIR}/heclib_x64.lib")
            endif()
        else()
            # Win32 platform
            if(EXISTS "${HECLIB_DIR}/heclib.lib")
                set_target_properties(heclib PROPERTIES
                    IMPORTED_LOCATION "${HECLIB_DIR}/heclib.lib"
                    IMPORTED_LOCATION_DEBUG "${HECLIB_DIR}/heclib_D.lib"
                    IMPORTED_LOCATION_RELEASE "${HECLIB_DIR}/heclib.lib"
                )
            else()
                message(FATAL_ERROR "Pre-built heclib not found at ${HECLIB_DIR}/heclib.lib")
            endif()
        endif()

        message(STATUS "Using pre-built heclib from: ${HECLIB_DIR}")
    else()
        message(FATAL_ERROR "Pre-built heclib not available for this platform. "
                "Set IWFM_USE_SYSTEM_HECLIB=OFF to build from source.")
    endif()

    # Link zlib to the imported heclib
    set_target_properties(heclib PROPERTIES
        INTERFACE_LINK_LIBRARIES "ZLIB::ZLIBSTATIC"
    )

    add_library(HECLIB::HECLIB ALIAS heclib)
    set(HECLIB_INCLUDE_DIR "" CACHE PATH "HECLIB headers" FORCE)

    return()
endif()

# =============================================================================
# Option 2: Build heclib from Source via FetchContent
# =============================================================================
message(STATUS "Configuring heclib (HEC-DSS 7) via FetchContent...")

FetchContent_Declare(heclib
    GIT_REPOSITORY https://github.com/HydrologicEngineeringCenter/hec-dss.git
    GIT_TAG        main
    GIT_SHALLOW    TRUE
)

# We intentionally use FetchContent_Populate instead of FetchContent_MakeAvailable
# because we need to bypass heclib's CMakeLists.txt to avoid zlib conflicts and
# use our own build configuration. Suppress the deprecation warning.
FetchContent_GetProperties(heclib)
if(NOT heclib_POPULATED)
    # Suppress CMP0169 warning for direct FetchContent_Populate call
    # This is intentional - we need to build heclib manually
    if(POLICY CMP0169)
        cmake_policy(PUSH)
        cmake_policy(SET CMP0169 OLD)
    endif()
    FetchContent_Populate(heclib)
    if(POLICY CMP0169)
        cmake_policy(POP)
    endif()

    # ==========================================================================
    # Build heclib manually (bypass their CMake to avoid zlib conflict)
    # ==========================================================================
    message(STATUS "Building heclib from source (bypassing upstream CMake)...")

    set(HECLIB_SRC_DIR "${heclib_SOURCE_DIR}/heclib/heclib_c/src")

    # Check if source directory exists
    if(NOT EXISTS "${HECLIB_SRC_DIR}")
        # Try alternative paths
        if(EXISTS "${heclib_SOURCE_DIR}/heclib/src")
            set(HECLIB_SRC_DIR "${heclib_SOURCE_DIR}/heclib/src")
        elseif(EXISTS "${heclib_SOURCE_DIR}/src")
            set(HECLIB_SRC_DIR "${heclib_SOURCE_DIR}/src")
        else()
            message(FATAL_ERROR "Cannot find heclib source directory. Tried:\n"
                    "  ${heclib_SOURCE_DIR}/heclib/heclib_c/src\n"
                    "  ${heclib_SOURCE_DIR}/heclib/src\n"
                    "  ${heclib_SOURCE_DIR}/src")
        endif()
    endif()

    # Collect all C source files
    file(GLOB_RECURSE HECLIB_SOURCES
        "${HECLIB_SRC_DIR}/*.c"
    )

    if(NOT HECLIB_SOURCES)
        message(FATAL_ERROR "No heclib source files found in ${HECLIB_SRC_DIR}")
    endif()

    # Exclude problematic files that use K&R style C (incompatible with modern compilers)
    # sortfiles.c has conflicting function types due to K&R style definitions
    list(FILTER HECLIB_SOURCES EXCLUDE REGEX "sortfiles\\.c$")

    list(LENGTH HECLIB_SOURCES HECLIB_SOURCE_COUNT)
    message(STATUS "Found ${HECLIB_SOURCE_COUNT} heclib source files")

    # Create static library
    add_library(heclib STATIC ${HECLIB_SOURCES})

    # Find headers directory
    set(HECLIB_HEADERS_DIR "${HECLIB_SRC_DIR}/headers")
    if(NOT EXISTS "${HECLIB_HEADERS_DIR}")
        # Try alternative paths
        if(EXISTS "${heclib_SOURCE_DIR}/heclib/heclib_c/src/headers")
            set(HECLIB_HEADERS_DIR "${heclib_SOURCE_DIR}/heclib/heclib_c/src/headers")
        elseif(EXISTS "${heclib_SOURCE_DIR}/heclib/headers")
            set(HECLIB_HEADERS_DIR "${heclib_SOURCE_DIR}/heclib/headers")
        elseif(EXISTS "${heclib_SOURCE_DIR}/headers")
            set(HECLIB_HEADERS_DIR "${heclib_SOURCE_DIR}/headers")
        else()
            # Fall back to source directory itself
            set(HECLIB_HEADERS_DIR "${HECLIB_SRC_DIR}")
        endif()
    endif()

    target_include_directories(heclib PUBLIC
        "${HECLIB_HEADERS_DIR}"
        "${HECLIB_SRC_DIR}"
        "${ZLIB_INCLUDE_DIR_SOURCE}"
        "${ZLIB_INCLUDE_DIR_BINARY}"
    )

    # Link against our shared zlib
    target_link_libraries(heclib PUBLIC ZLIB::ZLIBSTATIC)

    # ==========================================================================
    # Platform-specific compile options
    # ==========================================================================
    if(MSVC OR (WIN32 AND CMAKE_C_COMPILER_ID MATCHES "IntelLLVM"))
        # MSVC or Intel icx on Windows (MSVC-like command line)
        target_compile_definitions(heclib PRIVATE
            _CRT_SECURE_NO_WARNINGS
            _CRT_NONSTDC_NO_WARNINGS
            WIN32
            _WINDOWS
        )

        if(MSVC)
            target_compile_options(heclib PRIVATE
                /W0       # Suppress all warnings (heclib has many issues)
                /wd4996   # Disable deprecated function warnings
                /wd4244   # Disable conversion warnings
                /wd4267   # Disable size_t to int warnings
            )
        else()
            # Intel icx on Windows - uses clang under the hood
            # Need to allow K&R style function definitions and suppress warnings
            target_compile_options(heclib PRIVATE
                /W0       # Suppress all warnings
                # Use clang flags via /clang: prefix for C standard issues
                "SHELL:/clang:-Wno-error"
                "SHELL:/clang:-Wno-incompatible-function-pointer-types"
                "SHELL:/clang:-Wno-deprecated-non-prototype"
                "SHELL:/clang:-Wno-incompatible-pointer-types"
            )
        endif()

        # Use static CRT to match IWFM
        set_property(TARGET heclib PROPERTY
            MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

    elseif(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang|IntelLLVM|Intel")
        # GCC, Clang, or Intel compilers on Unix
        target_compile_options(heclib PRIVATE
            -fPIC
            -w  # Suppress warnings (heclib has many)
        )

        if(UNIX AND NOT APPLE)
            target_compile_definitions(heclib PRIVATE
                _POSIX_C_SOURCE=200809L
                _GNU_SOURCE
            )
        endif()
    endif()

    # Platform-specific system libraries
    if(WIN32)
        target_link_libraries(heclib PUBLIC ws2_32 shlwapi)
    elseif(UNIX AND NOT APPLE)
        target_link_libraries(heclib PUBLIC m dl pthread)
    elseif(APPLE)
        target_link_libraries(heclib PUBLIC m)
    endif()

    set(HECLIB_INCLUDE_DIR "${HECLIB_HEADERS_DIR}" CACHE PATH "HECLIB headers" FORCE)

    message(STATUS "heclib configured successfully")
    message(STATUS "  Source: ${HECLIB_SRC_DIR}")
    message(STATUS "  Headers: ${HECLIB_HEADERS_DIR}")
endif()

# Create alias target
if(TARGET heclib)
    add_library(HECLIB::HECLIB ALIAS heclib)
    message(STATUS "Created HECLIB::HECLIB alias")
else()
    message(FATAL_ERROR "heclib target not created")
endif()
