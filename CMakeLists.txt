#***********************************************************************
#  Integrated Water Flow Model (IWFM)
#  Copyright (C) 2005-2025
#  State of California, Department of Water Resources
#
#  CMake Build System for IWFM
#***********************************************************************

cmake_minimum_required(VERSION 3.20)

# Project definition with Fortran and C enabled
project(IWFM
    VERSION 2025.0.1747
    DESCRIPTION "Integrated Water Flow Model"
    LANGUAGES Fortran C
)

# CMake policies for modern behavior
cmake_policy(SET CMP0074 NEW)  # Use <Package>_ROOT variables
if(POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)  # URL download timestamp (CMake 3.24+)
endif()
cmake_policy(SET CMP0091 NEW)  # MSVC runtime library flags

# Use static runtime library for all targets (avoids DLL dependencies)
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

# Build type default
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo")
endif()

message(STATUS "IWFM Version: ${PROJECT_VERSION}")
message(STATUS "Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran Compiler: ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")

# =============================================================================
# Build Options
# =============================================================================
option(IWFM_BUILD_SIMULATION "Build main Simulation executable" ON)
option(IWFM_BUILD_PREPROCESSOR "Build PreProcessor executable" ON)
option(IWFM_BUILD_BUDGET "Build Budget post-processor" ON)
option(IWFM_BUILD_ZBUDGET "Build ZBudget post-processor" ON)
option(IWFM_BUILD_DLL "Build IWFM C-interface DLL/shared library" ON)
option(IWFM_BUILD_PARALLEL "Build OpenMP parallel simulation" OFF)
option(IWFM_BUILD_COARRAY "Build Coarray Fortran multi-model simulation (Intel only)" OFF)
option(IWFM_USE_SYSTEM_HDF5 "Use system-installed HDF5 instead of FetchContent" OFF)
option(IWFM_USE_SYSTEM_HECLIB "Use system-installed heclib instead of FetchContent" OFF)

# =============================================================================
# Determine if IWFM core targets need dependencies (HDF5, heclib, zlib)
# =============================================================================
# Only fetch heavy dependencies if building targets that actually need them
if(IWFM_BUILD_SIMULATION OR IWFM_BUILD_PREPROCESSOR OR IWFM_BUILD_BUDGET OR
   IWFM_BUILD_ZBUDGET OR IWFM_BUILD_DLL OR IWFM_BUILD_PARALLEL OR IWFM_BUILD_COARRAY)
    set(IWFM_NEED_DEPENDENCIES TRUE)
else()
    set(IWFM_NEED_DEPENDENCIES FALSE)
endif()

# =============================================================================
# Include Custom CMake Modules
# =============================================================================
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(IWFMCompilerFlags)
if(IWFM_NEED_DEPENDENCIES)
    include(IWFMFetchDependencies)
endif()
include(IWFMFortranModules)

# =============================================================================
# Global Fortran Module Output Directory
# =============================================================================
# This is critical for Fortran - all .mod files go to one place
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})

# =============================================================================
# Output Directories
# =============================================================================
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/Bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/Bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# For multi-config generators (Visual Studio, Xcode)
foreach(CONFIG_TYPE ${CMAKE_CONFIGURATION_TYPES})
    string(TOUPPER ${CONFIG_TYPE} CONFIG_TYPE_UPPER)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CONFIG_TYPE_UPPER} ${CMAKE_SOURCE_DIR}/Bin)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CONFIG_TYPE_UPPER} ${CMAKE_SOURCE_DIR}/Bin)
endforeach()

# =============================================================================
# Add Source Directories
# =============================================================================
add_subdirectory(SourceCode)

# =============================================================================
# Testing Support (only when building IWFM executables)
# =============================================================================
option(IWFM_BUILD_TESTS "Enable testing with sample model" ON)

if(IWFM_BUILD_TESTS AND IWFM_NEED_DEPENDENCIES)
    enable_testing()
    include(CTest)

    # Define the sample model directory
    set(IWFM_SAMPLE_MODEL_DIR "${CMAKE_SOURCE_DIR}/../samplemodel" CACHE PATH "Path to IWFM sample model")

    if(EXISTS "${IWFM_SAMPLE_MODEL_DIR}")
        message(STATUS "Sample model directory: ${IWFM_SAMPLE_MODEL_DIR}")

        # Executable directory
        set(IWFM_BIN_DIR "${CMAKE_SOURCE_DIR}/Bin")

        # Test 1: PreProcessor
        if(IWFM_BUILD_PREPROCESSOR)
            add_test(
                NAME IWFM_PreProcessor_Test
                COMMAND "${IWFM_BIN_DIR}/PreProcessor_x64.exe" PreProcessor_MAIN.IN
                WORKING_DIRECTORY "${IWFM_SAMPLE_MODEL_DIR}/PreProcessor"
            )
            set_tests_properties(IWFM_PreProcessor_Test PROPERTIES
                TIMEOUT 300
                LABELS "integration"
            )
        endif()

        # Test 2: Simulation (depends on PreProcessor output)
        if(IWFM_BUILD_SIMULATION)
            add_test(
                NAME IWFM_Simulation_Test
                COMMAND "${IWFM_BIN_DIR}/Simulation_x64.exe" Simulation_MAIN.IN
                WORKING_DIRECTORY "${IWFM_SAMPLE_MODEL_DIR}/Simulation"
            )
            set_tests_properties(IWFM_Simulation_Test PROPERTIES
                TIMEOUT 600
                LABELS "integration"
                DEPENDS IWFM_PreProcessor_Test
            )
        endif()

        # Test 3: Budget (depends on Simulation output)
        if(IWFM_BUILD_BUDGET)
            add_test(
                NAME IWFM_Budget_Test
                COMMAND "${IWFM_BIN_DIR}/Budget_x64.exe" Budget.in
                WORKING_DIRECTORY "${IWFM_SAMPLE_MODEL_DIR}/Budget"
            )
            set_tests_properties(IWFM_Budget_Test PROPERTIES
                TIMEOUT 60
                LABELS "integration"
                DEPENDS IWFM_Simulation_Test
            )
        endif()

        # Test 4: ZBudget (depends on Simulation output)
        if(IWFM_BUILD_ZBUDGET)
            add_test(
                NAME IWFM_ZBudget_Test
                COMMAND "${IWFM_BIN_DIR}/ZBudget_x64.exe" ZBudget.in
                WORKING_DIRECTORY "${IWFM_SAMPLE_MODEL_DIR}/ZBudget"
            )
            set_tests_properties(IWFM_ZBudget_Test PROPERTIES
                TIMEOUT 60
                LABELS "integration"
                DEPENDS IWFM_Simulation_Test
            )
        endif()

        message(STATUS "Integration tests configured for sample model")
    else()
        message(WARNING "Sample model directory not found: ${IWFM_SAMPLE_MODEL_DIR}")
        message(STATUS "Set IWFM_SAMPLE_MODEL_DIR to enable integration tests")
    endif()
endif()

# =============================================================================
# Installation Rules
# =============================================================================
include(GNUInstallDirs)

# Install Fortran module files
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/iwfm
    FILES_MATCHING PATTERN "*.mod"
)

# =============================================================================
# CPack Packaging Configuration
# =============================================================================

# Package metadata
set(CPACK_PACKAGE_NAME "IWFM")
set(CPACK_PACKAGE_VENDOR "California Department of Water Resources")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Integrated Water Flow Model - Hydrological Simulation")
set(CPACK_PACKAGE_DESCRIPTION "IWFM (Integrated Water Flow Model) is a hydrological simulation software for modeling groundwater, surface water, and land use interactions.")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://water.ca.gov/Library/Modeling-and-Analysis/Modeling-Platforms/Integrated-Water-Flow-Model")
set(CPACK_PACKAGE_CONTACT "IWFMtechsupport@water.ca.gov")
# License file (optional - check if it exists)
if(EXISTS "${CMAKE_SOURCE_DIR}/../LICENSE")
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/../LICENSE")
elseif(EXISTS "${CMAKE_SOURCE_DIR}/../LICENSE.txt")
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/../LICENSE.txt")
endif()

# Package file naming
set(CPACK_PACKAGE_FILE_NAME "IWFM-${PROJECT_VERSION}-${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "IWFM-${PROJECT_VERSION}-source")

# Generator selection based on platform
if(WIN32)
    set(CPACK_GENERATOR "ZIP")
    set(CPACK_SOURCE_GENERATOR "ZIP")
else()
    set(CPACK_GENERATOR "TGZ")
    set(CPACK_SOURCE_GENERATOR "TGZ")
endif()

# Components for binary package
set(CPACK_COMPONENTS_ALL executables libraries headers documentation)

set(CPACK_COMPONENT_EXECUTABLES_DISPLAY_NAME "IWFM Executables")
set(CPACK_COMPONENT_EXECUTABLES_DESCRIPTION "IWFM simulation and utility executables")
set(CPACK_COMPONENT_EXECUTABLES_REQUIRED ON)

set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "IWFM Libraries")
set(CPACK_COMPONENT_LIBRARIES_DESCRIPTION "IWFM C-interface shared library")

set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "Fortran Modules")
set(CPACK_COMPONENT_HEADERS_DESCRIPTION "Fortran module files for development")

set(CPACK_COMPONENT_DOCUMENTATION_DISPLAY_NAME "Documentation")
set(CPACK_COMPONENT_DOCUMENTATION_DESCRIPTION "User guides and technical documentation")

# Source package configuration - exclude build artifacts and large files
set(CPACK_SOURCE_IGNORE_FILES
    # Build directories (comprehensive patterns)
    "/build$"
    "/build/"
    "/build-.*/"
    "/cmake-build-.*/"
    "_deps/"
    "hdf5-install/"
    "HDF5_External-prefix/"
    # IDE files
    "/\\\\.vs/"
    "/\\\\.vscode/"
    "/\\\\.idea/"
    "\\\\.sln$"
    "\\\\.vcxproj"
    # Git files
    "/\\\\.git/"
    "\\\\.gitignore$"
    # Output files - executables and libraries
    "/Bin/.*\\\\.exe$"
    "/Bin/.*\\\\.dll$"
    "/Bin/.*\\\\.pdb$"
    "/Bin/.*\\\\.so$"
    "/Bin/.*\\\\.lib$"
    "/Bin/Budget$"
    "/Bin/ZBudget$"
    "/Bin/Simulation$"
    "/Bin/PreProcessor$"
    "/Bin/libiwfm"
    # Build artifacts
    "\\\\.obj$"
    "\\\\.o$"
    "\\\\.mod$"
    "\\\\.a$"
    "\\\\.ninja"
    "CMakeCache\\\\.txt$"
    "CMakeFiles/"
    "Makefile$"
    # Temp files
    "~$"
    "\\\\.swp$"
    "\\\\.bak$"
    "\\\\.tmp$"
    # Python cache
    "__pycache__"
    "\\\\.pyc$"
    # Log/output files
    "Messages\\\\.out$"
    "\\\\.log$"
    # Large documentation (users can download separately)
    "\\\\.pdf$"
    # Test model copies
    "samplemodel_unix_test/"
    # Windows reserved device names (nul, con, aux, prn, com#, lpt#)
    "/nul$"
    "/nul/"
)

# Install executables to bin/
install(PROGRAMS
    ${CMAKE_SOURCE_DIR}/Bin/Simulation_x64.exe
    ${CMAKE_SOURCE_DIR}/Bin/PreProcessor_x64.exe
    ${CMAKE_SOURCE_DIR}/Bin/Budget_x64.exe
    ${CMAKE_SOURCE_DIR}/Bin/ZBudget_x64.exe
    DESTINATION bin
    COMPONENT executables
    OPTIONAL
)

# Install Linux executables (different names)
install(PROGRAMS
    ${CMAKE_SOURCE_DIR}/Bin/Simulation
    ${CMAKE_SOURCE_DIR}/Bin/PreProcessor
    ${CMAKE_SOURCE_DIR}/Bin/Budget
    ${CMAKE_SOURCE_DIR}/Bin/ZBudget
    DESTINATION bin
    COMPONENT executables
    OPTIONAL
)

# Install shared library
install(FILES
    ${CMAKE_SOURCE_DIR}/Bin/IWFM_C_x64.dll
    ${CMAKE_SOURCE_DIR}/Bin/libiwfm_c.so
    DESTINATION lib
    COMPONENT libraries
    OPTIONAL
)

# Install documentation (only files that exist)
install(FILES
    ${CMAKE_SOURCE_DIR}/BUILD.md
    ${CMAKE_SOURCE_DIR}/CLAUDE.md
    DESTINATION doc
    COMPONENT documentation
    OPTIONAL
)

# Install PDF documentation if available
file(GLOB IWFM_PDF_DOCS "${CMAKE_SOURCE_DIR}/../*.pdf")
if(IWFM_PDF_DOCS)
    install(FILES ${IWFM_PDF_DOCS}
        DESTINATION doc
        COMPONENT documentation
        OPTIONAL
    )
endif()

# Include CPack module (must be last)
include(CPack)

# =============================================================================
# Summary
# =============================================================================
message(STATUS "")
message(STATUS "IWFM CMake Configuration Summary:")
message(STATUS "  Build Simulation:       ${IWFM_BUILD_SIMULATION}")
message(STATUS "  Build PreProcessor:     ${IWFM_BUILD_PREPROCESSOR}")
message(STATUS "  Build Budget:           ${IWFM_BUILD_BUDGET}")
message(STATUS "  Build ZBudget:          ${IWFM_BUILD_ZBUDGET}")
message(STATUS "  Build DLL:              ${IWFM_BUILD_DLL}")
message(STATUS "  Build Parallel (OpenMP):${IWFM_BUILD_PARALLEL}")
message(STATUS "  Build Coarray:          ${IWFM_BUILD_COARRAY}")
message(STATUS "  Use System HDF5:        ${IWFM_USE_SYSTEM_HDF5}")
message(STATUS "  Use System HECLIB:      ${IWFM_USE_SYSTEM_HECLIB}")
message(STATUS "  Fetch Dependencies:     ${IWFM_NEED_DEPENDENCIES}")
message(STATUS "  Build Tests:            ${IWFM_BUILD_TESTS}")
if(IWFM_BUILD_TESTS AND EXISTS "${IWFM_SAMPLE_MODEL_DIR}")
    message(STATUS "  Sample Model:           ${IWFM_SAMPLE_MODEL_DIR}")
endif()
if(NOT IWFM_NEED_DEPENDENCIES)
    message(STATUS "")
    message(STATUS "  Note: Dependencies (HDF5, heclib, zlib) skipped - not needed for selected targets")
endif()
message(STATUS "")
if(IWFM_NEED_DEPENDENCIES)
    message(STATUS "To run tests after building:")
    message(STATUS "  cd build && ctest -V")
    message(STATUS "")
endif()
