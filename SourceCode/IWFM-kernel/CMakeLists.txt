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
# Heclib Interface (Fortran-C bindings for HEC-DSS 7)
# =============================================================================
set(HECLIB_INTERFACE_SOURCES
    heclib_interface/heclib_c_binding.f90
    heclib_interface/heclib_compat.f90
)

# =============================================================================
# IWFM_Util - Utilities (must compile first)
# =============================================================================
set(IWFM_UTIL_SOURCES
    # Utilities - base modules
    IWFM_Util/Utilities/Class_Version.f90
    IWFM_Util/Utilities/GeneralUtilities.f90
    IWFM_Util/Utilities/GenericLinkedList.f90
    IWFM_Util/Utilities/Class_BinaryTree.f90
    IWFM_Util/Utilities/MessageLogger.f90
    IWFM_Util/Utilities/ProgramTimer.f90
    IWFM_Util/Utilities/TimeSeriesUtilities.f90
    # IOInterface - file I/O abstraction
    IWFM_Util/IOInterface/Class_BaseFileType.f90
    IWFM_Util/IOInterface/Class_AsciiFileType.f90
    IWFM_Util/IOInterface/Class_FortBinaryFileType.f90
    IWFM_Util/IOInterface/Class_HDF5FileType.f90
    IWFM_Util/IOInterface/Class_DSSFileType.f90
    IWFM_Util/IOInterface/IOInterface_Local.f90
    IWFM_Util/IOInterface/IOInterface.f90
    IWFM_Util/IOInterface/TSDFileHandler.f90
)

# =============================================================================
# IWFM_Kernel_Version
# =============================================================================
set(KERNEL_VERSION_SOURCES
    IWFM_Kernel_Version/IWFM_Kernel_Version.f90
)

# =============================================================================
# Package_Discretization - Grid and stratigraphy
# =============================================================================
set(PACKAGE_DISCRETIZATION_SOURCES
    Package_Discretization/Class_Grid.f90
    Package_Discretization/ParametricGrid.f90
    Package_Discretization/Class_AppFace.f90
    Package_Discretization/Class_AppGrid.f90
    Package_Discretization/Class_Stratigraphy.f90
    Package_Discretization/Package_Discretization.f90
)

# =============================================================================
# Package_Matrix - Linear algebra solvers
# =============================================================================
set(PACKAGE_MATRIX_SOURCES
    Package_Matrix/Lubksb.f90
    Package_Matrix/Ludcmp.f90
    Package_Matrix/pgmres.f90
    Package_Matrix/Package_Matrix.f90
)

# =============================================================================
# Package_Miscellaneous
# =============================================================================
set(PACKAGE_MISC_SOURCES
    Package_Miscellaneous/AbstractFunction.f90
    Package_Miscellaneous/Class_PairedData.f90
    Package_Miscellaneous/Class_SolverData.f90
    Package_Miscellaneous/Class_BaseHydrograph.f90
    Package_Miscellaneous/Class_TecplotOutput.f90
    Package_Miscellaneous/Opening_screen_WINDOWS.f90
    Package_Miscellaneous/Package_Misc.f90
)

# =============================================================================
# Package_Budget
# =============================================================================
set(PACKAGE_BUDGET_SOURCES
    Package_Budget/Budget_Parameters.f90
    Package_Budget/Class_BudgetInputFile.f90
    Package_Budget/Class_Budget.f90
    Package_Budget/Package_Budget.f90
)

# =============================================================================
# Package_ZBudget
# =============================================================================
set(PACKAGE_ZBUDGET_SOURCES
    Package_ZBudget/ZBudget_Parameters.f90
    Package_ZBudget/ZBudget_Util.f90
    Package_ZBudget/Class_ZBudgetHeader.f90
    Package_ZBudget/Class_SystemData.f90
    Package_ZBudget/Class_ZoneList.f90
    Package_ZBudget/Class_ZBudget.f90
    Package_ZBudget/Package_ZBudget.f90
)

# =============================================================================
# Package_PrecipitationET
# =============================================================================
set(PACKAGE_PRECIPET_SOURCES
    Package_PrecipitationET/Class_AtmosphericData.f90
    Package_PrecipitationET/Package_PrecipitationET.f90
)

# =============================================================================
# Package_Supply
# =============================================================================
set(PACKAGE_SUPPLY_SOURCES
    Package_Supply/Class_IrigFracFile.f90
    Package_Supply/SupplyAdjustment.f90
    Package_Supply/Package_Supply.f90
)

# =============================================================================
# Package_UnsatZone
# =============================================================================
set(PACKAGE_UNSATZONE_SOURCES
    Package_UnsatZone/Class_Soil.f90
    Package_UnsatZone/RainfallRunoff.f90
    Package_UnsatZone/UnsatZoneOps.f90
    Package_UnsatZone/Package_UnsatZone.f90
)

# =============================================================================
# Package_RootZone (with version subdirectories)
# =============================================================================
set(PACKAGE_ROOTZONE_SOURCES
    # Base classes
    Package_RootZone/Class_GenericLandUse.f90
    Package_RootZone/Class_GenericMoistureData.f90
    Package_RootZone/Class_LandUseDataFile.f90
    Package_RootZone/Class_BaseRootZone.f90
    Package_RootZone/Util_Package_RootZone.f90
    # Version 4.0
    Package_RootZone/VERSION_4.0/Class_RootDepthFracDataFile.f90
    Package_RootZone/VERSION_4.0/Class_NativeRiparianLandUse_v40.f90
    Package_RootZone/VERSION_4.0/Class_NonPondedAgLandUse_v40.f90
    Package_RootZone/VERSION_4.0/Class_PondedAgLandUse_v40.f90
    Package_RootZone/VERSION_4.0/Class_UrbanLandUse_v40.f90
    Package_RootZone/VERSION_4.0/Util_RootZone_v40.f90
    Package_RootZone/VERSION_4.0/RootZone_v40.f90
    # Version 4.01
    Package_RootZone/VERSION_4.01/RootZone_v401.f90
    # Version 4.1
    Package_RootZone/VERSION_4.1/Class_GenericLandUse_v41.f90
    Package_RootZone/VERSION_4.1/Class_NativeRiparianLandUse_v41.f90
    Package_RootZone/VERSION_4.1/Class_NonPondedAgLandUse_v41.f90
    Package_RootZone/VERSION_4.1/Class_PondedAgLandUse_v41.f90
    Package_RootZone/VERSION_4.1/Class_UrbanLandUse_v41.f90
    Package_RootZone/VERSION_4.1/Class_RVETFromStrm.f90
    Package_RootZone/VERSION_4.1/Util_RootZone_v41.f90
    Package_RootZone/VERSION_4.1/RootZone_v41.f90
    # Version 4.11
    Package_RootZone/VERSION_4.11/RootZone_v411.f90
    # Version 4.12
    Package_RootZone/VERSION_4.12/RootZone_v412.f90
    # Version 4.13
    Package_RootZone/VERSION_4.13/RootZone_v413.f90
    # Version 5.0
    Package_RootZone/VERSION_5.0/Class_AgLandUse_v50.f90
    Package_RootZone/VERSION_5.0/Class_NativeRiparianLandUse_v50.f90
    Package_RootZone/VERSION_5.0/Class_UrbanLandUse_v50.f90
    Package_RootZone/VERSION_5.0/RootZone_v50.f90
    # Package module
    Package_RootZone/Package_RootZone.f90
)

# =============================================================================
# Package_AppGW - Groundwater
# =============================================================================
set(PACKAGE_APPGW_SOURCES
    # Boundary Conditions
    Package_AppGW/BC/Class_TSBCDataFile.f90
    Package_AppGW/BC/Class_LayerBC.f90
    Package_AppGW/BC/Class_AppBC.f90
    # Pumping
    Package_AppGW/Package_AppPumping/Class_Well.f90
    Package_AppGW/Package_AppPumping/Class_PumpsAtElem.f90
    Package_AppGW/Package_AppPumping/Class_ElementPumping.f90
    Package_AppGW/Package_AppPumping/Class_Pumping.f90
    Package_AppGW/Package_AppPumping/Package_AppPumping.f90
    # Subsidence
    Package_AppGW/Package_AppSubsidence/Class_BaseAppSubsidence.f90
    Package_AppGW/Package_AppSubsidence/VERSION_4.0/Class_AppSubsidence_v40.f90
    Package_AppGW/Package_AppSubsidence/VERSION_5.0/Class_AppSubsidence_v50.f90
    Package_AppGW/Package_AppSubsidence/Package_AppSubsidence.f90
    # Tile Drain
    Package_AppGW/Package_AppTileDrain/AppTileDrain_Parameters.f90
    Package_AppGW/Package_AppTileDrain/TileDrainHydrograph.f90
    Package_AppGW/Package_AppTileDrain/Class_BaseTileDrain.f90
    Package_AppGW/Package_AppTileDrain/Package_AppTileDrain.f90
    # Main GW classes
    Package_AppGW/Class_GWState.f90
    Package_AppGW/GWHydrograph.f90
    Package_AppGW/VerticalFlow.f90
    Package_AppGW/Class_AppGW.f90
    Package_AppGW/Package_AppGW.f90
)

# =============================================================================
# Package_AppStream - Streams/Rivers
# =============================================================================
set(PACKAGE_APPSTREAM_SOURCES
    # Base classes
    Package_AppStream/Class_StrmState.f90
    Package_AppStream/Class_StrmNode.f90
    Package_AppStream/Class_StrmReach.f90
    Package_AppStream/Class_Diversion.f90
    Package_AppStream/Class_Bypass.f90
    Package_AppStream/Class_AppDiverBypass.f90
    Package_AppStream/Class_ElemToRecvLoss.f90
    Package_AppStream/Class_LossDestination.f90
    Package_AppStream/Class_StrmEvap.f90
    Package_AppStream/Class_StrmInflow.f90
    Package_AppStream/Class_StrmNodeBudget.f90
    Package_AppStream/StrmHydrograph.f90
    Package_AppStream/Class_BaseAppStream.f90
    # Version 4.0
    Package_AppStream/VERSION_4.0/Class_AppStream_v40.f90
    # Version 4.1
    Package_AppStream/VERSION_4.1/Class_StrmNode_v41.f90
    Package_AppStream/VERSION_4.1/Class_AppStream_v41.f90
    # Version 4.2
    Package_AppStream/VERSION_4.2/Class_AppStream_v42.f90
    # Version 4.21
    Package_AppStream/VERSION_4.21/Class_AppStream_v421.f90
    # Version 4.2 WSA
    Package_AppStream/VERSION_4.2_WSA/Class_AppStream_v42_WSA.f90
    # Version 5.0
    Package_AppStream/VERSION_5.0/Class_StrmNode_v50.f90
    Package_AppStream/VERSION_5.0/Class_AppStream_v50.f90
    # Package module
    Package_AppStream/Package_AppStream.f90
)

# =============================================================================
# Package_AppLake - Lakes
# =============================================================================
set(PACKAGE_APPLAKE_SOURCES
    Package_AppLake/Class_Lake.f90
    Package_AppLake/Class_BaseAppLake.f90
    Package_AppLake/VERSION_4.0/Class_MaxLakeElevFile.f90
    Package_AppLake/VERSION_4.0/Class_AppLake_v40.f90
    Package_AppLake/VERSION_5.0/Class_AppLake_v50.f90
    Package_AppLake/Package_AppLake.f90
)

# =============================================================================
# Package_ComponentConnectors
# =============================================================================
set(PACKAGE_CONNECTORS_SOURCES
    # Stream-GW Connector
    Package_ComponentConnectors/StrmGWConnector/Class_BaseStrmGWConnector.f90
    Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v40.f90
    Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v41.f90
    Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v42.f90
    Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v421.f90
    Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v50.f90
    Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector.f90
    # Other connectors
    Package_ComponentConnectors/LakeGWConnector.f90
    Package_ComponentConnectors/StrmLakeConnector.f90
    Package_ComponentConnectors/SupplyDestinationConnector.f90
    Package_ComponentConnectors/Package_ComponentConnectors.f90
)

# =============================================================================
# Package_AppSmallWatershed
# =============================================================================
set(PACKAGE_SMALLWATERSHED_SOURCES
    Package_AppSmallWatershed/Class_BaseAppSmallWatershed.f90
    Package_AppSmallWatershed/Version_4.0/Class_AppSmallWatershed_v40.f90
    Package_AppSmallWatershed/Version_4.1/Class_AppSmallWatershed_v41.f90
    Package_AppSmallWatershed/Package_AppSmallWatershed.f90
)

# =============================================================================
# Package_AppUnsatZone
# =============================================================================
set(PACKAGE_APPUNSATZONE_SOURCES
    Package_AppUnsatZone/Package_AppUnsatZone.f90
)

# =============================================================================
# Package_GWZBudget
# =============================================================================
set(PACKAGE_GWZBUDGET_SOURCES
    Package_GWZBudget/Class_GWZBudget.f90
    Package_GWZBudget/Package_GWZBudget.f90
)

# =============================================================================
# Create the IWFM Kernel Library
# =============================================================================
# On Unix: Use OBJECT library to avoid Linux linker symbol resolution issues
# with HDF5 Fortran static libraries. The OBJECT library compiles all sources
# to .o files which are then linked directly into executables.
#
# On Windows: Use STATIC library because Visual Studio's parallel build system
# doesn't properly handle Fortran module dependencies with OBJECT libraries,
# leading to race conditions where dependent projects start before .mod files
# are generated.
# =============================================================================
if(WIN32)
    add_library(iwfm_kernel STATIC
        ${HECLIB_INTERFACE_SOURCES}
        ${IWFM_UTIL_SOURCES}
        ${KERNEL_VERSION_SOURCES}
        ${PACKAGE_DISCRETIZATION_SOURCES}
        ${PACKAGE_MATRIX_SOURCES}
        ${PACKAGE_MISC_SOURCES}
        ${PACKAGE_BUDGET_SOURCES}
        ${PACKAGE_ZBUDGET_SOURCES}
        ${PACKAGE_PRECIPET_SOURCES}
        ${PACKAGE_SUPPLY_SOURCES}
        ${PACKAGE_UNSATZONE_SOURCES}
        ${PACKAGE_ROOTZONE_SOURCES}
        ${PACKAGE_APPGW_SOURCES}
        ${PACKAGE_APPSTREAM_SOURCES}
        ${PACKAGE_APPLAKE_SOURCES}
        ${PACKAGE_CONNECTORS_SOURCES}
        ${PACKAGE_SMALLWATERSHED_SOURCES}
        ${PACKAGE_APPUNSATZONE_SOURCES}
        ${PACKAGE_GWZBUDGET_SOURCES}
    )
else()
    add_library(iwfm_kernel OBJECT
    ${HECLIB_INTERFACE_SOURCES}
    ${IWFM_UTIL_SOURCES}
    ${KERNEL_VERSION_SOURCES}
    ${PACKAGE_DISCRETIZATION_SOURCES}
    ${PACKAGE_MATRIX_SOURCES}
    ${PACKAGE_MISC_SOURCES}
    ${PACKAGE_BUDGET_SOURCES}
    ${PACKAGE_ZBUDGET_SOURCES}
    ${PACKAGE_PRECIPET_SOURCES}
    ${PACKAGE_SUPPLY_SOURCES}
    ${PACKAGE_UNSATZONE_SOURCES}
    ${PACKAGE_ROOTZONE_SOURCES}
    ${PACKAGE_APPGW_SOURCES}
    ${PACKAGE_APPSTREAM_SOURCES}
    ${PACKAGE_APPLAKE_SOURCES}
    ${PACKAGE_CONNECTORS_SOURCES}
    ${PACKAGE_SMALLWATERSHED_SOURCES}
    ${PACKAGE_APPUNSATZONE_SOURCES}
    ${PACKAGE_GWZBUDGET_SOURCES}
    )
endif()

# Apply compiler flags
target_link_libraries(iwfm_kernel
    PUBLIC
        iwfm_compiler_flags
        iwfm_dependencies
)

# Set Fortran module output directory
iwfm_set_module_directory(iwfm_kernel)

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
