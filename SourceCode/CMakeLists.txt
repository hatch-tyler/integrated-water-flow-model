#***********************************************************************
#  IWFM CMake - Source Code Configuration
#
#  This file configures the build for all IWFM source components
#***********************************************************************

# Path variables for IWFMSourceFiles.cmake
set(IWFM_KERNEL_DIR "${CMAKE_CURRENT_SOURCE_DIR}/IWFM-kernel")
set(IWFM_SOURCECODE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

# =============================================================================
# Monolithic Build Mode — all .f90 compiled directly into each executable
# =============================================================================
if(IWFM_MONOLITHIC_BUILD AND IWFM_NEED_DEPENDENCIES)

    # Re-include source lists now that IWFM_KERNEL_DIR and IWFM_SOURCECODE_DIR are set
    include(IWFMSourceFiles)

    # -----------------------------------------------------------------
    # Simulation
    # -----------------------------------------------------------------
    if(IWFM_BUILD_SIMULATION)
        iwfm_add_monolithic_executable(Simulation
            SCOPE FULL
            SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/Simulation/Iwfm_f2.f90
        )
        iwfm_set_output_name(Simulation "Simulation")
        install(TARGETS Simulation RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # Simulation_Parallel (OpenMP)
    # -----------------------------------------------------------------
    if(IWFM_BUILD_PARALLEL)
        iwfm_add_monolithic_executable(Simulation_Parallel
            SCOPE FULL
            SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/Simulation/Iwfm_f2.f90
            OPENMP
        )
        iwfm_set_output_name(Simulation_Parallel "Simulation_PLL")
        install(TARGETS Simulation_Parallel RUNTIME DESTINATION bin)

        # Copy OpenMP runtime DLL next to the executable (Windows)
        if(WIN32 AND IWFM_USING_INTEL)
            get_filename_component(_ifx_dir "${CMAKE_Fortran_COMPILER}" DIRECTORY)
            find_file(IOMP5_DLL_MONO libiomp5md.dll
                PATHS "${_ifx_dir}" "${_ifx_dir}/../bin" "${_ifx_dir}/../../bin"
                NO_DEFAULT_PATH)
            if(IOMP5_DLL_MONO)
                add_custom_command(TARGET Simulation_Parallel POST_BUILD
                    COMMAND ${CMAKE_COMMAND} -E copy_if_different
                        "${IOMP5_DLL_MONO}" "$<TARGET_FILE_DIR:Simulation_Parallel>"
                    COMMENT "Copying libiomp5md.dll to output directory"
                )
            endif()
        endif()
    endif()

    # -----------------------------------------------------------------
    # Simulation_MM (Coarray)
    # -----------------------------------------------------------------
    if(IWFM_BUILD_COARRAY)
        iwfm_add_monolithic_executable(Simulation_MM
            SCOPE FULL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/Simulation_MM/Coarray_Debugging_Mod.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/Simulation_MM/IWFM_f2_MM.f90
            COARRAY
        )
        iwfm_set_output_name(Simulation_MM "Simulation_MM")
        install(TARGETS Simulation_MM RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # PreProcessor
    # -----------------------------------------------------------------
    if(IWFM_BUILD_PREPROCESSOR)
        iwfm_add_monolithic_executable(PreProcessor
            SCOPE FULL
            SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/PreProcessor/Iwfm_f1.f90
        )
        iwfm_set_output_name(PreProcessor "PreProcessor")
        install(TARGETS PreProcessor RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # Budget
    # -----------------------------------------------------------------
    if(IWFM_BUILD_BUDGET)
        iwfm_add_monolithic_executable(Budget
            SCOPE KERNEL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/Budget/Budget.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/Budget/BudgetControls.f90
        )
        iwfm_set_output_name(Budget "Budget")
        install(TARGETS Budget RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # ZBudget
    # -----------------------------------------------------------------
    if(IWFM_BUILD_ZBUDGET)
        iwfm_add_monolithic_executable(ZBudget
            SCOPE KERNEL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/ZBudget/ZBudget_Main.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/ZBudget/ZBudgetControls.f90
        )
        iwfm_set_output_name(ZBudget "ZBudget")
        install(TARGETS ZBudget RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # IWFM2OBS
    # -----------------------------------------------------------------
    if(IWFM_BUILD_IWFM2OBS)
        iwfm_add_monolithic_executable(IWFM2OBS
            SCOPE KERNEL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/Class_SMP2SMP.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/Class_PESTOutput.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/Class_HeadDifference.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/Class_HydrographReader.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/Class_MultiLayerTarget.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/Class_IWFM2OBS.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM2OBS/IWFM2OBS_Main.f90
        )
        iwfm_set_output_name(IWFM2OBS "IWFM2OBS")
        install(TARGETS IWFM2OBS RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # CalcTypeHyd
    # -----------------------------------------------------------------
    if(IWFM_BUILD_CALCTYPHYD)
        iwfm_add_monolithic_executable(CalcTypeHyd
            SCOPE KERNEL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/CalcTypeHyd/Class_CalcTypeHyd.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/CalcTypeHyd/CalcTypeHyd_Main.f90
        )
        iwfm_set_output_name(CalcTypeHyd "CalcTypeHyd")
        install(TARGETS CalcTypeHyd RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # ResultsExtract
    # -----------------------------------------------------------------
    if(IWFM_BUILD_RESULTSEXTRACT)
        iwfm_add_monolithic_executable(ResultsExtract
            SCOPE KERNEL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/ResultsExtract/Class_ResultsExtract.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/ResultsExtract/ResultsExtract_Main.f90
        )
        iwfm_set_output_name(ResultsExtract "ResultsExtract")
        install(TARGETS ResultsExtract RUNTIME DESTINATION bin)
    endif()

    # -----------------------------------------------------------------
    # IWFM C DLL / Shared Library
    # -----------------------------------------------------------------
    if(IWFM_BUILD_DLL)
        iwfm_add_monolithic_dll(IWFM_C_DLL
            SOURCES
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM_DLL/IWFM_Model_Exports_C.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM_DLL/IWFM_Budget_Exports_C.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM_DLL/IWFM_ZBudget_Exports_C.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/IWFM_DLL/IWFM_Misc_Exports_C.f90
        )

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

    # -----------------------------------------------------------------
    # DSS Read Diagnostic Test (monolithic)
    # -----------------------------------------------------------------
    if(IWFM_BUILD_TESTS)
        iwfm_add_monolithic_executable(test_dss_read
            SCOPE KERNEL
            SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_dss_read.f90
        )
        set_target_properties(test_dss_read PROPERTIES
            OUTPUT_NAME "test_dss_read"
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/Bin"
        )
    endif()

else() # Original library-based build — entirely unchanged

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
    target_link_libraries(Simulation PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_model>")
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
    target_link_libraries(Simulation_Parallel PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_model>")
    iwfm_add_openmp_flags(Simulation_Parallel)
    iwfm_set_output_name(Simulation_Parallel "Simulation_PLL")
    if(UNIX)
        iwfm_link_hdf5(Simulation_Parallel)
    endif()

    install(TARGETS Simulation_Parallel RUNTIME DESTINATION bin)

    # Copy OpenMP runtime DLL next to the executable (Windows)
    if(WIN32 AND IWFM_USING_INTEL)
        get_filename_component(_ifx_dir "${CMAKE_Fortran_COMPILER}" DIRECTORY)
        find_file(IOMP5_DLL libiomp5md.dll
            PATHS "${_ifx_dir}" "${_ifx_dir}/../bin" "${_ifx_dir}/../../bin"
            NO_DEFAULT_PATH)
        if(IOMP5_DLL)
            add_custom_command(TARGET Simulation_Parallel POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${IOMP5_DLL}" "$<TARGET_FILE_DIR:Simulation_Parallel>"
                COMMENT "Copying libiomp5md.dll to output directory"
            )
        else()
            message(WARNING "libiomp5md.dll not found near compiler — parallel exe may fail at runtime")
        endif()
    endif()
endif()

# =============================================================================
# Simulation_MM Executable (Coarray - Intel only)
# =============================================================================
if(IWFM_BUILD_COARRAY)
    add_executable(Simulation_MM
        Simulation_MM/Coarray_Debugging_Mod.f90
        Simulation_MM/IWFM_f2_MM.f90
    )
    target_link_libraries(Simulation_MM PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_model>")
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
    target_link_libraries(PreProcessor PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_model>")
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
    target_link_libraries(Budget PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_kernel>" iwfm_version)
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
    target_link_libraries(ZBudget PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_kernel>" iwfm_version)
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
        IWFM2OBS/Class_HydrographReader.f90
        IWFM2OBS/Class_MultiLayerTarget.f90
        IWFM2OBS/Class_IWFM2OBS.f90
        IWFM2OBS/IWFM2OBS_Main.f90
    )
    target_link_libraries(IWFM2OBS PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_kernel>" iwfm_version)
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
    target_link_libraries(CalcTypeHyd PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_kernel>" iwfm_version)
    iwfm_set_output_name(CalcTypeHyd "CalcTypeHyd")
    if(UNIX)
        iwfm_link_hdf5(CalcTypeHyd)
    endif()

    install(TARGETS CalcTypeHyd RUNTIME DESTINATION bin)
endif()

# =============================================================================
# ResultsExtract Executable (Generalized hydrograph extractor: HEAD/SUBSIDENCE)
# =============================================================================
if(IWFM_BUILD_RESULTSEXTRACT)
    add_executable(ResultsExtract
        ResultsExtract/Class_ResultsExtract.f90
        ResultsExtract/ResultsExtract_Main.f90
    )
    target_link_libraries(ResultsExtract PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_kernel>" iwfm_version)
    iwfm_set_output_name(ResultsExtract "ResultsExtract")
    if(UNIX)
        iwfm_link_hdf5(ResultsExtract)
    endif()

    install(TARGETS ResultsExtract RUNTIME DESTINATION bin)
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
    target_link_libraries(IWFM_C_DLL PRIVATE "$<LINK_LIBRARY:WHOLE_ARCHIVE,iwfm_model>")
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

endif() # IWFM_MONOLITHIC_BUILD
