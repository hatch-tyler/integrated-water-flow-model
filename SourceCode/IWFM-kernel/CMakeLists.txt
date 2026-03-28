#***********************************************************************
#  IWFM CMake - Kernel Library Configuration
#
#  This file builds the IWFM kernel static library containing all
#  core packages shared by all IWFM targets.
#
#  IMPORTANT: Source file order matters for Fortran module dependencies.
#  Files are listed in dependency order - utilities first, then base
#  classes, then derived classes.
#***********************************************************************

# =============================================================================
# Load shared source file lists from IWFMSourceFiles.cmake
# =============================================================================
if(NOT DEFINED IWFM_KERNEL_DIR)
    set(IWFM_KERNEL_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
endif()
include(IWFMSourceFiles)

# =============================================================================
# Create the IWFM Kernel Library
# =============================================================================
# On Unix: Use OBJECT library to avoid Linux linker symbol resolution issues
# with HDF5 Fortran static libraries.
#
# On Windows: Use STATIC library. The /WHOLEARCHIVE linker flag (applied in
# IWFMCompilerFlags.cmake) forces the linker to include ALL object files from
# the archive, matching the behavior of direct .obj linking in VS .vfproj.
# Without /WHOLEARCHIVE, the MSVC linker uses selective inclusion which can
# exclude code needed for Fortran polymorphic dispatch.
# =============================================================================
if(WIN32)
    add_library(iwfm_kernel STATIC ${IWFM_ALL_KERNEL_SOURCES})
else()
    add_library(iwfm_kernel OBJECT ${IWFM_ALL_KERNEL_SOURCES})
endif()

# Apply compiler flags
target_link_libraries(iwfm_kernel
    PUBLIC
        iwfm_compiler_flags
        iwfm_dependencies
)

# Enable OpenMP for the kernel library ONLY when parallel or DLL targets need it.
# Without this guard, ALL executables linking iwfm_kernel inherit /Qopenmp,
# causing 30-50% runtime overhead on non-parallel targets (Simulation, Budget, etc.)
# from OMP thread management, barrier synchronization, and thread-safe allocations.
#
# When IWFM_BUILD_PARALLEL or IWFM_BUILD_COARRAY is ON, the kernel must compile
# !$OMP directives as parallel regions; otherwise they are treated as comments.
# The DLL also needs OpenMP because external callers may use parallel features.
# Note: In library-based builds, if BOTH Simulation and Simulation_Parallel are
# built, they share this kernel library. OpenMP will be compiled into the kernel
# for both. For maximum non-parallel performance, use the monolithic build
# (-DIWFM_MONOLITHIC_BUILD=ON) which gives each target its own compilation.
if(IWFM_BUILD_PARALLEL OR IWFM_BUILD_COARRAY OR IWFM_BUILD_DLL)
    iwfm_add_openmp_flags(iwfm_kernel)
endif()

# Set Fortran module output directory
iwfm_set_module_directory(iwfm_kernel)

# Workaround: Intel ifx 2025.3 ICE on Class_StrmNodeBudget.f90 in Debug mode
# with /check:bounds. Disable bounds checking for this single file.
if(IWFM_USING_INTEL AND WIN32)
    set_source_files_properties(
        ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmNodeBudget.f90
        PROPERTIES COMPILE_OPTIONS "$<$<CONFIG:Debug>:/check:nobound>"
    )
elseif(IWFM_USING_INTEL)
    set_source_files_properties(
        ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmNodeBudget.f90
        PROPERTIES COMPILE_OPTIONS "$<$<CONFIG:Debug>:-check nobound>"
    )
endif()

# Alias for consistent naming
add_library(IWFM::Kernel ALIAS iwfm_kernel)

# =============================================================================
# Installation
# =============================================================================
# STATIC libraries (Windows) can be installed; OBJECT libraries (Unix) cannot.
if(WIN32)
    install(TARGETS iwfm_kernel
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
endif()
