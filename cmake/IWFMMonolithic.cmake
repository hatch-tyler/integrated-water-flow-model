#***********************************************************************
#  IWFM CMake - Monolithic Build Support
#
#  Provides iwfm_add_monolithic_executable() and iwfm_add_monolithic_dll()
#  which compile ALL Fortran source files directly into each target,
#  bypassing intermediate static libraries. This matches the vfproj
#  build behavior and eliminates the .lib extraction overhead.
#
#  Enabled via: cmake -DIWFM_MONOLITHIC_BUILD=ON
#***********************************************************************

include_guard(GLOBAL)

# =============================================================================
# Function: Add a monolithic executable target
# =============================================================================
# Usage:
#   iwfm_add_monolithic_executable(Simulation_Parallel
#       SCOPE FULL
#       SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/Simulation/Iwfm_f2.f90
#       OPENMP
#   )
#
# Arguments:
#   TARGET_NAME  - Name of the executable target
#   SCOPE        - FULL (kernel+version+wsa_ann+model) or KERNEL (kernel+version only)
#   SOURCES      - Additional source files specific to this executable
#   OPENMP       - Flag to enable OpenMP
#   COARRAY      - Flag to enable Coarray Fortran
#
function(iwfm_add_monolithic_executable TARGET_NAME)
    cmake_parse_arguments(MONO "OPENMP;COARRAY" "SCOPE" "SOURCES" ${ARGN})

    # Select base source set based on scope
    if(MONO_SCOPE STREQUAL "FULL")
        set(_base_sources ${IWFM_ALL_COMMON_SOURCES})
    elseif(MONO_SCOPE STREQUAL "KERNEL")
        set(_base_sources ${IWFM_KERNEL_PLUS_VERSION_SOURCES})
    else()
        message(FATAL_ERROR "iwfm_add_monolithic_executable: SCOPE must be FULL or KERNEL, got '${MONO_SCOPE}'")
    endif()

    # Combine base sources with executable-specific sources
    set(_all_sources ${_base_sources} ${MONO_SOURCES})

    # Create the executable with ALL sources compiled directly
    add_executable(${TARGET_NAME} ${_all_sources})

    # Apply compiler flags and dependencies via interface libraries (no static libs)
    target_link_libraries(${TARGET_NAME} PRIVATE iwfm_compiler_flags iwfm_dependencies)

    # Set module directory for .mod file output
    iwfm_set_module_directory(${TARGET_NAME})

    # Conditionally apply OpenMP or Coarray
    if(MONO_OPENMP)
        iwfm_add_openmp_flags(${TARGET_NAME})
    endif()
    if(MONO_COARRAY)
        iwfm_add_coarray_flags(${TARGET_NAME})
    endif()

    # Note: OpenMP is NOT unconditionally applied to all monolithic targets.
    # The .vfproj build only enables OpenMP for Simulation_Parallel and DLL.
    # Adding /Qopenmp to plain Simulation/Budget/etc. causes 30-50% runtime
    # overhead from OMP thread management, barrier synchronization, and
    # thread-safe allocations — even though no parallel regions are active.

    # Apply the ifx ICE workaround for Class_StrmNodeBudget.f90
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

    # Link HDF5 on Unix
    if(UNIX)
        iwfm_link_hdf5(${TARGET_NAME})
    endif()

endfunction()

# =============================================================================
# Function: Add a monolithic shared library (DLL) target
# =============================================================================
# Usage:
#   iwfm_add_monolithic_dll(IWFM_C_DLL
#       SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/IWFM_DLL/IWFM_Model_Exports_C.f90 ...
#   )
#
function(iwfm_add_monolithic_dll TARGET_NAME)
    cmake_parse_arguments(MONO "" "" "SOURCES" ${ARGN})

    # DLL always uses FULL scope
    set(_all_sources ${IWFM_ALL_COMMON_SOURCES} ${MONO_SOURCES})

    # Create the shared library with ALL sources compiled directly
    add_library(${TARGET_NAME} SHARED ${_all_sources})

    # Apply compiler flags and dependencies via interface libraries
    target_link_libraries(${TARGET_NAME} PRIVATE iwfm_compiler_flags iwfm_dependencies)

    # Set module directory
    iwfm_set_module_directory(${TARGET_NAME})

    # Link C++ solver if enabled
    # Enable OpenMP for kernel sources
    iwfm_add_openmp_flags(${TARGET_NAME})

    # Apply DLL-specific flags (heap-arrays)
    iwfm_add_dll_flags(${TARGET_NAME})

    # Apply the ifx ICE workaround
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

    # Link HDF5 on Unix
    if(UNIX)
        iwfm_link_hdf5(${TARGET_NAME})
    endif()

endfunction()
