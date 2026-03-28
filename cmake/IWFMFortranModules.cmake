#***********************************************************************
#  IWFM CMake - Fortran Module Dependency Management
#
#  Utilities for handling Fortran module compilation order
#  and inter-target dependencies
#***********************************************************************

# =============================================================================
# Modern CMake Fortran Module Scanning
# =============================================================================
# CMake 3.20+ with Ninja supports automatic Fortran module dependency scanning.
# NOTE: CMAKE_Fortran_PREPROCESS is set to OFF in root CMakeLists.txt to prevent
# Ninja's two-step preprocess-then-compile pipeline, which causes ifx to generate
# ~34% slower code. The /fpp flag in compile options lets ifx handle preprocessing
# inline (single-step), matching the VS build behavior.

# CMake 3.28+ has improved Fortran module scanning
if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.28")
    cmake_policy(SET CMP0155 NEW)  # Fortran module scan policy
endif()

# =============================================================================
# Function: Add Module Dependency Between Targets
# =============================================================================
# Use this when one target depends on modules from another target
function(iwfm_add_module_dependency dependent_target dependency_target)
    add_dependencies(${dependent_target} ${dependency_target})

    # Also ensure the module directory is in the include path
    get_target_property(MOD_DIR ${dependency_target} Fortran_MODULE_DIRECTORY)
    if(MOD_DIR)
        target_include_directories(${dependent_target} PRIVATE ${MOD_DIR})
    endif()
endfunction()

# =============================================================================
# Function: Set Fortran Module Directory for a Target
# =============================================================================
function(iwfm_set_module_directory target)
    # In monolithic builds, each target compiles all sources independently.
    # Give each target its own module directory to avoid .mod file conflicts.
    if(IWFM_MONOLITHIC_BUILD)
        set(_mod_dir "${CMAKE_BINARY_DIR}/modules/${target}")
    else()
        set(_mod_dir "${CMAKE_Fortran_MODULE_DIRECTORY}")
    endif()
    set_target_properties(${target} PROPERTIES
        Fortran_MODULE_DIRECTORY ${_mod_dir}
    )
    target_include_directories(${target} PUBLIC
        $<BUILD_INTERFACE:${_mod_dir}>
        $<INSTALL_INTERFACE:include/iwfm>
    )
endfunction()

# =============================================================================
# Macro: Collect Fortran Sources in Order
# =============================================================================
# This macro helps maintain compilation order for Fortran sources
# by grouping them according to their module dependencies

macro(iwfm_collect_sources VAR_NAME)
    set(${VAR_NAME} ${ARGN})
endmacro()

# =============================================================================
# Note on Fortran Module Dependencies
# =============================================================================
# For Fortran projects, the order of source files in add_library/add_executable
# can matter because modules must be compiled before files that USE them.
#
# Modern CMake (3.20+) with Ninja generator handles this automatically through
# dependency scanning. For other generators (Make, VS), source order in the
# CMakeLists.txt should reflect the dependency order.
#
# The IWFM-kernel sources are ordered in CMakeLists.txt to ensure:
# 1. Utility modules compile first (GeneralUtilities, MessageLogger, etc.)
# 2. Base classes compile before derived classes
# 3. Package modules compile after their dependencies
#
# If you encounter "module not found" errors, check:
# 1. Source file order in CMakeLists.txt
# 2. Target dependencies (use iwfm_add_module_dependency)
# 3. Include directories for module files
