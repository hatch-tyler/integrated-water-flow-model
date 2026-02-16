#***********************************************************************
#  IWFM CMake - Dependency Management Orchestrator
#
#  This module orchestrates external dependencies in the correct order:
#  1. zlib (for heclib - HDF5 builds its own internal zlib)
#  2. HDF5 (with Fortran bindings via ExternalProject)
#  3. heclib (HEC-DSS 7 C library)
#
#  The modular design ensures:
#  - Proper dependency ordering
#  - Static linking on all platforms
#  - HDF5 built separately via ExternalProject to avoid link order issues
#***********************************************************************

include_guard(GLOBAL)

message(STATUS "")
message(STATUS "=== IWFM Dependency Configuration ===")

# =============================================================================
# Global Static Build Settings
# =============================================================================
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build static libraries" FORCE)

# =============================================================================
# Include Dependencies in Order
# =============================================================================

# 1. Zlib (for heclib compression)
include(IWFMFetchZlib)

# 2. HDF5 (with Fortran bindings via ExternalProject)
# Note: HDF5 builds its own internal zlib to avoid cross-project linking issues
include(IWFMFetchHDF5)

# 3. heclib (HEC-DSS 7, uses our zlib)
include(IWFMFetchHeclib)

# =============================================================================
# Create Unified Dependency Interface
# =============================================================================
# This interface library bundles all dependencies for easy linking
add_library(iwfm_dependencies INTERFACE)
add_library(IWFM::Dependencies ALIAS iwfm_dependencies)

# Link HDF5
# HDF5_Fortran_Interface or HDF5::HDF5 depending on build method
if(TARGET HDF5_Fortran_Interface)
    target_link_libraries(iwfm_dependencies INTERFACE HDF5_Fortran_Interface)
    message(STATUS "  Linked: HDF5::Fortran (via HDF5_Fortran_Interface)")
elseif(TARGET HDF5::HDF5)
    target_link_libraries(iwfm_dependencies INTERFACE HDF5::HDF5)
    message(STATUS "  Linked: HDF5::HDF5")
elseif(TARGET HDF5::Fortran)
    target_link_libraries(iwfm_dependencies INTERFACE HDF5::Fortran)
    message(STATUS "  Linked: HDF5::Fortran")
endif()

# Link heclib
if(TARGET HECLIB::HECLIB)
    target_link_libraries(iwfm_dependencies INTERFACE HECLIB::HECLIB)
    message(STATUS "  Linked: HECLIB::HECLIB")
endif()

# =============================================================================
# Build Order Dependencies for ExternalProject
# =============================================================================
# When using ExternalProject, HDF5 is built at build time (not configure time).
# We need to ensure HDF5_External completes before IWFM targets compile.
if(TARGET HDF5_External)
    add_library(iwfm_hdf5_modules INTERFACE)
    add_dependencies(iwfm_hdf5_modules HDF5_External)
    target_link_libraries(iwfm_dependencies INTERFACE iwfm_hdf5_modules)
    message(STATUS "  Build dependency: HDF5_External (ExternalProject)")
elseif(TARGET hdf5_fortran-static)
    # FetchContent path (legacy, for bundled or system HDF5)
    add_library(iwfm_hdf5_modules INTERFACE)
    add_dependencies(iwfm_hdf5_modules hdf5_fortran-static)
    target_link_libraries(iwfm_dependencies INTERFACE iwfm_hdf5_modules)
    message(STATUS "  Build dependency: hdf5_fortran-static")
endif()

# =============================================================================
# Include Directories
# =============================================================================
# HDF5 Fortran modules (.mod files)
if(DEFINED HDF5_FORTRAN_MODULE_DIR)
    target_include_directories(iwfm_dependencies INTERFACE ${HDF5_FORTRAN_MODULE_DIR})
    message(STATUS "  HDF5 modules: ${HDF5_FORTRAN_MODULE_DIR}")
endif()

# heclib headers
if(DEFINED HECLIB_INCLUDE_DIR AND HECLIB_INCLUDE_DIR)
    target_include_directories(iwfm_dependencies INTERFACE ${HECLIB_INCLUDE_DIR})
    message(STATUS "  heclib headers: ${HECLIB_INCLUDE_DIR}")
endif()

# =============================================================================
# Platform-Specific System Libraries
# =============================================================================
if(WIN32)
    # Windows system libraries for networking and shell
    target_link_libraries(iwfm_dependencies INTERFACE
        ws2_32      # Windows Sockets
        shlwapi     # Shell API
    )
    message(STATUS "  System libs: ws2_32 shlwapi")

elseif(UNIX AND NOT APPLE)
    # Linux system libraries
    target_link_libraries(iwfm_dependencies INTERFACE
        m           # Math library
        dl          # Dynamic linking
        pthread     # POSIX threads
    )
    message(STATUS "  System libs: m dl pthread")

elseif(APPLE)
    # macOS system libraries
    # Note: macOS does not support fully static executables
    target_link_libraries(iwfm_dependencies INTERFACE
        m           # Math library
    )
    message(STATUS "  System libs: m")
    message(STATUS "  Note: macOS does not support fully static linking")
endif()

# =============================================================================
# Summary
# =============================================================================
message(STATUS "")
message(STATUS "IWFM Dependencies configured:")
message(STATUS "  ZLIB::ZLIBSTATIC    -> ${IWFM_USE_SYSTEM_ZLIB}")
message(STATUS "  HDF5::Fortran       -> ${IWFM_USE_SYSTEM_HDF5}")
message(STATUS "  HECLIB::HECLIB      -> ${IWFM_USE_SYSTEM_HECLIB}")
message(STATUS "  Static build        -> ON")
message(STATUS "===========================================")
message(STATUS "")
