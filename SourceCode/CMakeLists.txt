#***********************************************************************
#  IWFM CMake - Source Code Configuration
#
#  This file configures the build for all IWFM source components
#***********************************************************************

# =============================================================================
# IWFM Core Libraries (only built when dependencies are available)
# =============================================================================
if(IWFM_NEED_DEPENDENCIES)

    # =========================================================================
    # IWFM Kernel Library (core shared code)
    # =========================================================================
    add_subdirectory(IWFM-kernel)

    # =========================================================================
    # IWFM Version Module
    # =========================================================================
    add_library(iwfm_version STATIC
        IWFM_Version/IWFM_Version.f90
    )
    target_link_libraries(iwfm_version PUBLIC iwfm_kernel)
    iwfm_set_module_directory(iwfm_version)

    # =========================================================================
    # WSA_ANN Module
    # =========================================================================
    add_library(iwfm_wsa_ann STATIC
        WSA_ANN/WSA_ANN.f90
    )
    target_link_libraries(iwfm_wsa_ann PUBLIC iwfm_kernel)
    iwfm_set_module_directory(iwfm_wsa_ann)

    # =========================================================================
    # Package_Model Library
    # =========================================================================
    add_library(iwfm_model STATIC
        Package_Model/Class_Model_ForInquiry.f90
        Package_Model/Package_Model.f90
    )
    target_link_libraries(iwfm_model
        PUBLIC
            iwfm_kernel
            iwfm_version
            iwfm_wsa_ann
    )
    iwfm_set_module_directory(iwfm_model)
    add_library(IWFM::Model ALIAS iwfm_model)

endif() # IWFM_NEED_DEPENDENCIES

# =============================================================================
# Simulation Executable
# =============================================================================
if(IWFM_BUILD_SIMULATION)
    add_executable(Simulation
        Simulation/Iwfm_f2.f90
    )
    target_link_libraries(Simulation PRIVATE iwfm_model)
    iwfm_set_output_name(Simulation "Simulation")
    if(UNIX)
        iwfm_link_hdf5(Simulation)
    endif()

    install(TARGETS Simulation RUNTIME DESTINATION bin)
endif()

# =============================================================================
# Simulation_Parallel Executable (OpenMP)
# =============================================================================
if(IWFM_BUILD_PARALLEL)
    add_executable(Simulation_Parallel
        Simulation/Iwfm_f2.f90
    )
    target_link_libraries(Simulation_Parallel PRIVATE iwfm_model)
    iwfm_add_openmp_flags(Simulation_Parallel)
    iwfm_set_output_name(Simulation_Parallel "Simulation_PLL")
    if(UNIX)
        iwfm_link_hdf5(Simulation_Parallel)
    endif()

    install(TARGETS Simulation_Parallel RUNTIME DESTINATION bin)
endif()

# =============================================================================
# Simulation_MM Executable (Coarray - Intel only)
# =============================================================================
if(IWFM_BUILD_COARRAY)
    add_executable(Simulation_MM
        Simulation_MM/Coarray_Debugging_Mod.f90
        Simulation_MM/IWFM_f2_MM.f90
    )
    target_link_libraries(Simulation_MM PRIVATE iwfm_model)
    iwfm_add_coarray_flags(Simulation_MM)
    iwfm_set_output_name(Simulation_MM "Simulation_MM")
    if(UNIX)
        iwfm_link_hdf5(Simulation_MM)
    endif()

    install(TARGETS Simulation_MM RUNTIME DESTINATION bin)
endif()

# =============================================================================
# PreProcessor Executable
# =============================================================================
if(IWFM_BUILD_PREPROCESSOR)
    add_executable(PreProcessor
        PreProcessor/Iwfm_f1.f90
    )
    target_link_libraries(PreProcessor PRIVATE iwfm_model)
    iwfm_set_output_name(PreProcessor "PreProcessor")
    if(UNIX)
        iwfm_link_hdf5(PreProcessor)
    endif()

    install(TARGETS PreProcessor RUNTIME DESTINATION bin)
endif()

# =============================================================================
# Budget Executable
# =============================================================================
if(IWFM_BUILD_BUDGET)
    add_executable(Budget
        Budget/Budget.f90
        Budget/BudgetControls.f90
    )
    target_link_libraries(Budget PRIVATE iwfm_kernel iwfm_version)
    iwfm_set_output_name(Budget "Budget")
    if(UNIX)
        iwfm_link_hdf5(Budget)
    endif()

    install(TARGETS Budget RUNTIME DESTINATION bin)
endif()

# =============================================================================
# ZBudget Executable
# =============================================================================
if(IWFM_BUILD_ZBUDGET)
    add_executable(ZBudget
        ZBudget/ZBudget_Main.f90
        ZBudget/ZBudgetControls.f90
    )
    target_link_libraries(ZBudget PRIVATE iwfm_kernel iwfm_version)
    iwfm_set_output_name(ZBudget "ZBudget")
    if(UNIX)
        iwfm_link_hdf5(ZBudget)
    endif()

    install(TARGETS ZBudget RUNTIME DESTINATION bin)
endif()

# =============================================================================
# IWFM2OBS Executable (Hydrograph to PEST SMP converter with multi-layer target)
# =============================================================================
if(IWFM_BUILD_IWFM2OBS)
    add_executable(IWFM2OBS
        IWFM2OBS/Class_SMP2SMP.f90
        IWFM2OBS/Class_PESTOutput.f90
        IWFM2OBS/Class_HeadDifference.f90
        IWFM2OBS/Class_MultiLayerTarget.f90
        IWFM2OBS/Class_IWFM2OBS.f90
        IWFM2OBS/IWFM2OBS_Main.f90
    )
    target_link_libraries(IWFM2OBS PRIVATE iwfm_kernel iwfm_version)
    iwfm_set_output_name(IWFM2OBS "IWFM2OBS")
    if(UNIX)
        iwfm_link_hdf5(IWFM2OBS)
    endif()

    install(TARGETS IWFM2OBS RUNTIME DESTINATION bin)
endif()

# =============================================================================
# CalcTypeHyd Executable (Cluster type hydrograph computation)
# =============================================================================
if(IWFM_BUILD_CALCTYPHYD)
    add_executable(CalcTypeHyd
        CalcTypeHyd/Class_CalcTypeHyd.f90
        CalcTypeHyd/CalcTypeHyd_Main.f90
    )
    target_link_libraries(CalcTypeHyd PRIVATE iwfm_kernel iwfm_version)
    iwfm_set_output_name(CalcTypeHyd "CalcTypeHyd")
    if(UNIX)
        iwfm_link_hdf5(CalcTypeHyd)
    endif()

    install(TARGETS CalcTypeHyd RUNTIME DESTINATION bin)
endif()

# =============================================================================
# IWFM C DLL / Shared Library
# =============================================================================
if(IWFM_BUILD_DLL)
    add_library(IWFM_C_DLL SHARED
        IWFM_DLL/IWFM_Model_Exports_C.f90
        IWFM_DLL/IWFM_Budget_Exports_C.f90
        IWFM_DLL/IWFM_ZBudget_Exports_C.f90
        IWFM_DLL/IWFM_Misc_Exports_C.f90
    )
    target_link_libraries(IWFM_C_DLL PRIVATE iwfm_model)
    iwfm_add_dll_flags(IWFM_C_DLL)
    if(UNIX)
        iwfm_link_hdf5(IWFM_C_DLL)
    endif()

    if(WIN32)
        set_target_properties(IWFM_C_DLL PROPERTIES
            OUTPUT_NAME "IWFM_C_x64$<$<CONFIG:Debug>:_D>"
            WINDOWS_EXPORT_ALL_SYMBOLS ON
        )
    else()
        set_target_properties(IWFM_C_DLL PROPERTIES
            OUTPUT_NAME "iwfm_c$<$<CONFIG:Debug>:_d>"
        )
    endif()

    install(TARGETS IWFM_C_DLL
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
    )
endif()

# =============================================================================
# DSS Read Diagnostic Test
# =============================================================================
if(IWFM_BUILD_TESTS AND IWFM_NEED_DEPENDENCIES)
    add_executable(test_dss_read
        tests/test_dss_read.f90
    )
    target_link_libraries(test_dss_read PRIVATE iwfm_kernel)
    iwfm_set_module_directory(test_dss_read)
    set_target_properties(test_dss_read PROPERTIES
        OUTPUT_NAME "test_dss_read"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/Bin"
    )
endif()
