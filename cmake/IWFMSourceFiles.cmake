#***********************************************************************
#  IWFM CMake - Centralized Source File Lists
#
#  Single source of truth for all IWFM Fortran source files.
#  Used by both the library-based build and the monolithic build.
#
#  REQUIRES: IWFM_KERNEL_DIR must be set before including this file.
#  OPTIONAL: IWFM_SOURCECODE_DIR for non-kernel sources (version, wsa_ann, model).
#***********************************************************************

include_guard(GLOBAL)

if(NOT DEFINED IWFM_KERNEL_DIR)
    message(FATAL_ERROR "IWFM_KERNEL_DIR must be set before including IWFMSourceFiles.cmake")
endif()

# =============================================================================
# Heclib Interface (Fortran-C bindings for HEC-DSS 7)
# =============================================================================
set(IWFM_HECLIB_INTERFACE_SOURCES
    ${IWFM_KERNEL_DIR}/heclib_interface/heclib_c_binding.f90
    ${IWFM_KERNEL_DIR}/heclib_interface/heclib_compat.f90
)

# =============================================================================
# IWFM_Util - Utilities (must compile first)
# =============================================================================
set(IWFM_UTIL_SOURCES
    # Utilities - base modules
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/Class_Version.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/GeneralUtilities.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/GenericLinkedList.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/Class_BinaryTree.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/MessageLogger.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/ProgramTimer.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/Utilities/TimeSeriesUtilities.f90
    # IOInterface - file I/O abstraction
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/Class_BaseFileType.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/Class_AsciiFileType.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/Class_FortBinaryFileType.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/Class_HDF5FileType.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/Class_DSSFileType.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/IOInterface_Local.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/IOInterface.f90
    ${IWFM_KERNEL_DIR}/IWFM_Util/IOInterface/TSDFileHandler.f90
)

# =============================================================================
# IWFM_Kernel_Version
# =============================================================================
set(IWFM_KERNEL_VERSION_SOURCES
    ${IWFM_KERNEL_DIR}/IWFM_Kernel_Version/IWFM_Kernel_Version.f90
)

# =============================================================================
# Package_Discretization - Grid and stratigraphy
# =============================================================================
set(IWFM_PACKAGE_DISCRETIZATION_SOURCES
    ${IWFM_KERNEL_DIR}/Package_Discretization/Class_Grid.f90
    ${IWFM_KERNEL_DIR}/Package_Discretization/ParametricGrid.f90
    ${IWFM_KERNEL_DIR}/Package_Discretization/Class_AppFace.f90
    ${IWFM_KERNEL_DIR}/Package_Discretization/Class_AppGrid.f90
    ${IWFM_KERNEL_DIR}/Package_Discretization/Class_Stratigraphy.f90
    ${IWFM_KERNEL_DIR}/Package_Discretization/Package_Discretization.f90
)

# =============================================================================
# Package_Matrix - Linear algebra solvers
# =============================================================================
set(IWFM_PACKAGE_MATRIX_SOURCES
    ${IWFM_KERNEL_DIR}/Package_Matrix/Lubksb.f90
    ${IWFM_KERNEL_DIR}/Package_Matrix/Ludcmp.f90
    ${IWFM_KERNEL_DIR}/Package_Matrix/pgmres.f90
    ${IWFM_KERNEL_DIR}/Package_Matrix/Package_Matrix.f90
)

# =============================================================================
# Package_Miscellaneous
# =============================================================================
set(IWFM_PACKAGE_MISC_SOURCES
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/AbstractFunction.f90
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/Class_PairedData.f90
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/Class_SolverData.f90
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/Class_BaseHydrograph.f90
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/Class_TecplotOutput.f90
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/Opening_screen_WINDOWS.f90
    ${IWFM_KERNEL_DIR}/Package_Miscellaneous/Package_Misc.f90
)

# =============================================================================
# Package_Budget
# =============================================================================
set(IWFM_PACKAGE_BUDGET_SOURCES
    ${IWFM_KERNEL_DIR}/Package_Budget/Budget_Parameters.f90
    ${IWFM_KERNEL_DIR}/Package_Budget/Class_BudgetInputFile.f90
    ${IWFM_KERNEL_DIR}/Package_Budget/Class_Budget.f90
    ${IWFM_KERNEL_DIR}/Package_Budget/Package_Budget.f90
)

# =============================================================================
# Package_ZBudget
# =============================================================================
set(IWFM_PACKAGE_ZBUDGET_SOURCES
    ${IWFM_KERNEL_DIR}/Package_ZBudget/ZBudget_Parameters.f90
    ${IWFM_KERNEL_DIR}/Package_ZBudget/ZBudget_Util.f90
    ${IWFM_KERNEL_DIR}/Package_ZBudget/Class_ZBudgetHeader.f90
    ${IWFM_KERNEL_DIR}/Package_ZBudget/Class_SystemData.f90
    ${IWFM_KERNEL_DIR}/Package_ZBudget/Class_ZoneList.f90
    ${IWFM_KERNEL_DIR}/Package_ZBudget/Class_ZBudget.f90
    ${IWFM_KERNEL_DIR}/Package_ZBudget/Package_ZBudget.f90
)

# =============================================================================
# Package_PrecipitationET
# =============================================================================
set(IWFM_PACKAGE_PRECIPET_SOURCES
    ${IWFM_KERNEL_DIR}/Package_PrecipitationET/Class_AtmosphericData.f90
    ${IWFM_KERNEL_DIR}/Package_PrecipitationET/Package_PrecipitationET.f90
)

# =============================================================================
# Package_Supply
# =============================================================================
set(IWFM_PACKAGE_SUPPLY_SOURCES
    ${IWFM_KERNEL_DIR}/Package_Supply/Class_IrigFracFile.f90
    ${IWFM_KERNEL_DIR}/Package_Supply/SupplyAdjustment.f90
    ${IWFM_KERNEL_DIR}/Package_Supply/Package_Supply.f90
)

# =============================================================================
# Package_UnsatZone
# =============================================================================
set(IWFM_PACKAGE_UNSATZONE_SOURCES
    ${IWFM_KERNEL_DIR}/Package_UnsatZone/Class_Soil.f90
    ${IWFM_KERNEL_DIR}/Package_UnsatZone/RainfallRunoff.f90
    ${IWFM_KERNEL_DIR}/Package_UnsatZone/UnsatZoneOps.f90
    ${IWFM_KERNEL_DIR}/Package_UnsatZone/Package_UnsatZone.f90
)

# =============================================================================
# Package_RootZone (with version subdirectories)
# =============================================================================
set(IWFM_PACKAGE_ROOTZONE_SOURCES
    # Base classes
    ${IWFM_KERNEL_DIR}/Package_RootZone/Class_GenericLandUse.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/Class_GenericMoistureData.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/Class_LandUseDataFile.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/Class_BaseRootZone.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/Util_Package_RootZone.f90
    # Version 4.0
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/Class_RootDepthFracDataFile.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/Class_NativeRiparianLandUse_v40.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/Class_NonPondedAgLandUse_v40.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/Class_PondedAgLandUse_v40.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/Class_UrbanLandUse_v40.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/Util_RootZone_v40.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.0/RootZone_v40.f90
    # Version 4.01
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.01/RootZone_v401.f90
    # Version 4.1
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Class_GenericLandUse_v41.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Class_NativeRiparianLandUse_v41.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Class_NonPondedAgLandUse_v41.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Class_PondedAgLandUse_v41.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Class_UrbanLandUse_v41.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Class_RVETFromStrm.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/Util_RootZone_v41.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.1/RootZone_v41.f90
    # Version 4.11
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.11/RootZone_v411.f90
    # Version 4.12
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.12/RootZone_v412.f90
    # Version 4.13
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_4.13/RootZone_v413.f90
    # Version 5.0
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_5.0/Class_AgLandUse_v50.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_5.0/Class_NativeRiparianLandUse_v50.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_5.0/Class_UrbanLandUse_v50.f90
    ${IWFM_KERNEL_DIR}/Package_RootZone/VERSION_5.0/RootZone_v50.f90
    # Package module
    ${IWFM_KERNEL_DIR}/Package_RootZone/Package_RootZone.f90
)

# =============================================================================
# Package_AppGW - Groundwater
# =============================================================================
set(IWFM_PACKAGE_APPGW_SOURCES
    # Boundary Conditions
    ${IWFM_KERNEL_DIR}/Package_AppGW/BC/Class_TSBCDataFile.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/BC/Class_LayerBC.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/BC/Class_AppBC.f90
    # Pumping
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppPumping/Class_Well.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppPumping/Class_PumpsAtElem.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppPumping/Class_ElementPumping.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppPumping/Class_Pumping.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppPumping/Package_AppPumping.f90
    # Subsidence
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppSubsidence/Class_BaseAppSubsidence.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppSubsidence/VERSION_4.0/Class_AppSubsidence_v40.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppSubsidence/VERSION_4.1/Class_AppSubsidence_v41.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppSubsidence/VERSION_5.0/Class_AppSubsidence_v50.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppSubsidence/VERSION_5.1/Class_AppSubsidence_v51.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppSubsidence/Package_AppSubsidence.f90
    # Tile Drain
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppTileDrain/AppTileDrain_Parameters.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppTileDrain/TileDrainHydrograph.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppTileDrain/Class_BaseTileDrain.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppTileDrain/Package_AppTileDrain.f90
    # Main GW classes
    ${IWFM_KERNEL_DIR}/Package_AppGW/Class_GWState.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/GWHydrograph.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/VerticalFlow.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Class_AppGW.f90
    ${IWFM_KERNEL_DIR}/Package_AppGW/Package_AppGW.f90
)

# =============================================================================
# Package_AppStream - Streams/Rivers
# =============================================================================
set(IWFM_PACKAGE_APPSTREAM_SOURCES
    # Base classes
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmState.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmNode.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmReach.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_Diversion.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_Bypass.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_AppDiverBypass.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_ElemToRecvLoss.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_LossDestination.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmEvap.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmInflow.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_StrmNodeBudget.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/StrmHydrograph.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/Class_BaseAppStream.f90
    # Version 4.0
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_4.0/Class_AppStream_v40.f90
    # Version 4.1
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_4.1/Class_StrmNode_v41.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_4.1/Class_AppStream_v41.f90
    # Version 4.2
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_4.2/Class_AppStream_v42.f90
    # Version 4.21
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_4.21/Class_AppStream_v421.f90
    # Version 4.2 WSA
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_4.2_WSA/Class_AppStream_v42_WSA.f90
    # Version 5.0
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_5.0/Class_StrmNode_v50.f90
    ${IWFM_KERNEL_DIR}/Package_AppStream/VERSION_5.0/Class_AppStream_v50.f90
    # Package module
    ${IWFM_KERNEL_DIR}/Package_AppStream/Package_AppStream.f90
)

# =============================================================================
# Package_AppLake - Lakes
# =============================================================================
set(IWFM_PACKAGE_APPLAKE_SOURCES
    ${IWFM_KERNEL_DIR}/Package_AppLake/Class_Lake.f90
    ${IWFM_KERNEL_DIR}/Package_AppLake/Class_BaseAppLake.f90
    ${IWFM_KERNEL_DIR}/Package_AppLake/VERSION_4.0/Class_MaxLakeElevFile.f90
    ${IWFM_KERNEL_DIR}/Package_AppLake/VERSION_4.0/Class_AppLake_v40.f90
    ${IWFM_KERNEL_DIR}/Package_AppLake/VERSION_5.0/Class_AppLake_v50.f90
    ${IWFM_KERNEL_DIR}/Package_AppLake/Package_AppLake.f90
)

# =============================================================================
# Package_ComponentConnectors
# =============================================================================
set(IWFM_PACKAGE_CONNECTORS_SOURCES
    # Stream-GW Connector
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_BaseStrmGWConnector.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v40.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v41.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v42.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v421.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v50.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector.f90
    # Other connectors
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/LakeGWConnector.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/StrmLakeConnector.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/SupplyDestinationConnector.f90
    ${IWFM_KERNEL_DIR}/Package_ComponentConnectors/Package_ComponentConnectors.f90
)

# =============================================================================
# Package_AppSmallWatershed
# =============================================================================
set(IWFM_PACKAGE_SMALLWATERSHED_SOURCES
    ${IWFM_KERNEL_DIR}/Package_AppSmallWatershed/Class_BaseAppSmallWatershed.f90
    ${IWFM_KERNEL_DIR}/Package_AppSmallWatershed/Version_4.0/Class_AppSmallWatershed_v40.f90
    ${IWFM_KERNEL_DIR}/Package_AppSmallWatershed/Version_4.1/Class_AppSmallWatershed_v41.f90
    ${IWFM_KERNEL_DIR}/Package_AppSmallWatershed/Package_AppSmallWatershed.f90
)

# =============================================================================
# Package_AppUnsatZone
# =============================================================================
set(IWFM_PACKAGE_APPUNSATZONE_SOURCES
    ${IWFM_KERNEL_DIR}/Package_AppUnsatZone/Package_AppUnsatZone.f90
)

# =============================================================================
# Package_GWZBudget
# =============================================================================
set(IWFM_PACKAGE_GWZBUDGET_SOURCES
    ${IWFM_KERNEL_DIR}/Package_GWZBudget/Class_GWZBudget.f90
    ${IWFM_KERNEL_DIR}/Package_GWZBudget/Package_GWZBudget.f90
)

# =============================================================================
# Combined Lists
# =============================================================================

# All kernel sources (~158 files)
set(IWFM_ALL_KERNEL_SOURCES
    ${IWFM_HECLIB_INTERFACE_SOURCES}
    ${IWFM_UTIL_SOURCES}
    ${IWFM_KERNEL_VERSION_SOURCES}
    ${IWFM_PACKAGE_DISCRETIZATION_SOURCES}
    ${IWFM_PACKAGE_MATRIX_SOURCES}
    ${IWFM_PACKAGE_MISC_SOURCES}
    ${IWFM_PACKAGE_BUDGET_SOURCES}
    ${IWFM_PACKAGE_ZBUDGET_SOURCES}
    ${IWFM_PACKAGE_PRECIPET_SOURCES}
    ${IWFM_PACKAGE_SUPPLY_SOURCES}
    ${IWFM_PACKAGE_UNSATZONE_SOURCES}
    ${IWFM_PACKAGE_ROOTZONE_SOURCES}
    ${IWFM_PACKAGE_APPGW_SOURCES}
    ${IWFM_PACKAGE_APPSTREAM_SOURCES}
    ${IWFM_PACKAGE_APPLAKE_SOURCES}
    ${IWFM_PACKAGE_CONNECTORS_SOURCES}
    ${IWFM_PACKAGE_SMALLWATERSHED_SOURCES}
    ${IWFM_PACKAGE_APPUNSATZONE_SOURCES}
    ${IWFM_PACKAGE_GWZBUDGET_SOURCES}
)

# Kernel + Version sources (~159 files, for Budget/ZBudget/IWFM2OBS etc.)
if(DEFINED IWFM_SOURCECODE_DIR)
    set(IWFM_KERNEL_PLUS_VERSION_SOURCES
        ${IWFM_ALL_KERNEL_SOURCES}
        ${IWFM_SOURCECODE_DIR}/IWFM_Version/IWFM_Version.f90
    )

    # All common sources (~162 files, for Simulation/PreProcessor/DLL)
    set(IWFM_ALL_COMMON_SOURCES
        ${IWFM_KERNEL_PLUS_VERSION_SOURCES}
        ${IWFM_SOURCECODE_DIR}/WSA_ANN/WSA_ANN.f90
        ${IWFM_SOURCECODE_DIR}/Package_Model/Class_Model_ForInquiry.f90
        ${IWFM_SOURCECODE_DIR}/Package_Model/Package_Model.f90
    )
endif()
