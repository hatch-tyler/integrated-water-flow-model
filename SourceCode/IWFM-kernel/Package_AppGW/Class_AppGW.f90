!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2025  
!  State of California, Department of Water Resources 
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov 
!***********************************************************************
MODULE Class_AppGW
  !$ USE OMP_LIB
  USE IWFM_Kernel_Version         , ONLY: IWFMKernelVersion
  USE GenericLinkedList           , ONLY: GenericLinkedListType
  USE GeneralUtilities            , ONLY: IntToText                                          , &
                                          StripTextUntilCharacter                            , &
                                          ArrangeText                                        , &
                                          UpperCase                                          , &
                                          CleanSpecialCharacters                             , &
                                          EstablishAbsolutePathFileName                      , &
                                          ConvertID_To_Index                                 , &
                                          LocateInList                                       , &
                                          FEXP, &
                                   f_cInlineCommentChar
  USE TimeSeriesUtilities         , ONLY: TimeStepType                                       , &
                                          NPeriods                                           , &
                                          IsTimeIntervalValid                                , &
                                          TimeIntervalConversion                             , &
                                          IncrementTimeStamp                                 , &
                                          CTimeStep_To_RTimeStep                             , &
                                          GetJulianDatesBetweenTimeStampsWithTimeIncrement   , &
                                          TimeStampToJulian                                  , &
                                          f_iTimeStampLength                                 , &
                                          OPERATOR(.TULE.)                                   
  USE IOInterface                 , ONLY: GenericFileType                                    , &
                                          DoesFileExist                                      , &
                                          f_iTXT                                                
  USE MessageLogger               , ONLY: LogMessage                                         , &
                                          SetLastMessage                                     , &
                                          EchoProgress                                       , &
                                          IsLogFileDefined                                   , &
                                          MessageArray                                       , &
                                          f_iFILE                                            , &
                                          f_iMessage                                         , &
                                          f_iFatal                                           , &
                                          f_iWarn                                            , &
                                          f_iInfo                                              
  USE Package_Budget              , ONLY: BudgetType                                         , &
                                          BudgetHeaderType                                   , &
                                          f_iColumnHeaderLen                                 , &
                                          f_cVolumeUnitMarker                                , &
                                          f_cAreaUnitMarker                                  , &
                                          f_cAreaMarker                                      , &
                                          f_cLocationNameMarker                              , &
                                          f_iVR                                              , &
                                          f_iVLB                                             , &
                                          f_iVLE                                             , &
                                          f_iPER_CUM                                            
  USE Package_ComponentConnectors , ONLY: SupplyType                                         , &
                                          SupplyToDestinationType                            , &
                                          SupplyDestinationConnectorType                     , &
                                          StrmGWConnectorType                                , &
                                          LakeGWConnectorType                                     
  USE Package_Misc                , ONLY: FlowDestinationType                                , &
                                          f_iGWComp                                          , &
                                          f_rSmoothMaxP                                      , &
                                          f_rSmoothStepP                                     , &
                                          f_iAllLocationIDsListed                            , &
                                          f_iLocationType_Subregion                          , &
                                          f_iLocationType_Node                               , &
                                          f_iLocationType_GWHeadObs                          , &
                                          f_iLocationType_SubsidenceObs                      , &
                                          f_iLocationType_TileDrainObs                       , &
                                          f_iDataUnitType_Length                             , &
                                          f_iDataUnitType_Volume                             , &
                                          f_iFlowDest_Outside                                , &
                                          f_iFlowDest_StrmNode                               , &
                                          f_iFlowDest_Lake     
  USE Package_Discretization      , ONLY: AppGridType                                        , &
                                          StratigraphyType                                   , &
                                          GetValuesFromParametricGrid                        
  USE Class_GWState               , ONLY: GWStateType                                        
  USE GWHydrograph                , ONLY: GWHydrographType                                   , &
                                          f_iTecPlot_PrintGWHeads
  USE Class_LayerBC               , ONLY: f_iSpFlowBCID                                      , &
                                          f_iSpHeadBCID                                      , &
                                          f_iGHBCID                                          , &
                                          f_iConstrainedGHBCID                                     
  USE Class_AppBC                 , ONLY: AppBCType                                                           
  USE Package_AppSubsidence       , ONLY: AppSubsidenceType                                  , &
                                          f_cDescription_SubsHyd
  USE Package_AppTileDrain        , ONLY: AppTileDrainType                                   , &
                                          f_iTileDrain                                       , &
                                          f_iSubIrig                                         , &
                                          f_cDescription_TDHyd
  USE Package_AppPumping          , ONLY: AppPumpingType                                     , &
                                          f_iPump_Well                                       , &
                                          f_iPump_ElemPump                                     
  USE VerticalFlow                , ONLY: VerticalFlowOutputType                             , &
                                          VerticalFlowOutput_New                             , &
                                          VerticalFlowOutput_Kill                            , &
                                          VerticalFlowOutput_PrintResults                    , &
                                          VerticalFlow_ComputeAtNodesLayer                   , &
                                          VerticalFlow_ComputeDerivativesAtNodesLayer        , &
                                          VerticalFlow_ComputeElementsUpwardDownward_AtLayer 
  USE Package_Matrix              , ONLY: MatrixType                                         , &
                                          ConnectivityListType
  IMPLICIT NONE
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: AppGWType                         , &
            f_iSpFlowBCID                     , &
            f_iSpHeadBCID                     , &
            f_iGHBCID                         , &
            f_iConstrainedGHBCID              , &
            f_iTileDrain                      , &
            f_iSubIrig                        , &
            f_iBudgetType_GW                  , &
            f_cDescription_GWHyd_AtNodeLayer  , &
            f_iPump_Well                      , &
            f_iPump_ElemPump                  
  
  
  ! -------------------------------------------------------------
  ! --- GW NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GWNodeType
      REAL(8),ALLOCATABLE :: Kh(:,:)              !Aquifer horizontal hydraulic conductivity at each (node,layer)
      REAL(8),ALLOCATABLE :: Kv(:,:)              !Aquifer vertical hyadraulic conductivity at each (node,layer)
      REAL(8),ALLOCATABLE :: AquitardKv(:,:)      !Aquitard vertical hydraulic conductivity at each (node,layer)  
      REAL(8),ALLOCATABLE :: LeakageV(:,:)        !Vertical leakage between the aquifer and the aquifer above at each (node,layer)
      REAL(8),ALLOCATABLE :: Ss(:,:)              !Aquifer specific storage at each (node,layer)
      REAL(8),ALLOCATABLE :: Sy(:,:)              !Aquifer specific yield at each (node,layer)
  CONTAINS
      PROCEDURE,PASS :: Kill => GWNode_Kill
  END TYPE GWNodeType
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION GW DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppGWType
      PRIVATE
      CHARACTER(LEN=6)              :: VarTimeUnit                  = ''       !Time unit for aquifer variables
      TYPE(GWNodeType)              :: Nodes                                   !Groundwater data at each (node,layer) combination
      TYPE(GWStateType)             :: State                                   !Data type that stores the state of the groundwater
      REAL(8),ALLOCATABLE           :: ElemTransmissivity(:,:)                 !Element transmissivity at each (element,layer) combination computed using AppGW%State%Head
      REAL(8),ALLOCATABLE           :: RegionalStorage(:)                      !Subregional gw storage at the current time step (computed only when gw budget output is required)
      REAL(8),ALLOCATABLE           :: RegionalStorage_P(:)                    !Subregional gw storage at the previous N-R iteration for each (node), counting all the nodes for all the layers
      REAL(8),ALLOCATABLE           :: rGWReturnFlow(:)                        !GW return flow at each (node) where head above ground surface is turned into surface flow; this computed for top active layer at each node only
      LOGICAL,ALLOCATABLE           :: lComputeGWReturnFlowAtNode(:)           !Flag to check if gw return flow will be computed at a (node) based on if node is at a stream, lake or certain b.c.s
      INTEGER,ALLOCATABLE           :: iGWReturnFlowDestType(:)                !Destination type for GW return flow at each (node) 
      INTEGER,ALLOCATABLE           :: iGWReturnFlowDest(:)                    !Destination for GW return flow at each (node) 
      LOGICAL                       :: lSimulateGWReturnFlow        = .FALSE.  !Flag to to turn on or off simulation of groundwater return flow
      TYPE(GWHydrographType)        :: GWHyd                                   !Groundwater hydrograph output related data
      REAL(8)                       :: FactHead                     = 1.0      !Conversion factor for output groundwater heads
      CHARACTER(LEN=10)             :: UnitHead                     = ''       !Unit of output head values
      REAL(8)                       :: FactFlow                     = 1.0      !Conversion factor for output groundwater flows
      CHARACTER(LEN=10)             :: UnitFlow                     = ''       !Unit of output groundwater flow values
      REAL(8)                       :: FactVelocity                 = 1.0      !Conversion factor for output groundwater flow velocities
      CHARACTER(LEN=10)             :: UnitVelocity                 = ''       !Unit of output groundwater flow velocity values
      TYPE(AppBCType)               :: AppBC                                   !Boundary conditions database
      LOGICAL                       :: lAppBC_Defined               = .FALSE.  !Flag to check any boundary conditions other than non-flow b.c. is defined
      TYPE(AppTileDrainType)        :: AppTileDrain                            !Tile drain/subsurface irrigation component
      LOGICAL                       :: lTileDrain_Defined           = .FALSE.  !Flag to check if tile drains/subsurface irrigation are simulated
      TYPE(AppPumpingType)          :: AppPumping                              !Pumping component
      LOGICAL                       :: lPumping_Defined             = .FALSE.  !Flag to check if pumping/recharge component is used
      TYPE(AppSubsidenceType)       :: AppSubsidence                           !Subsidence component
      LOGICAL                       :: lSubsidence_Defined          = .FALSE.  !Flag to check if subsidence is modeled
      TYPE(VerticalFlowOutputType)  :: VerticalFlowOutput                      !Output file for the vetical flows at subregions
      TYPE(BudgetType)              :: GWBudFile                               !Groundwater budget file
      LOGICAL                       :: lGWBudFile_Defined           = .FALSE.  !Flag to check if the GW budget file is being printed out
      CHARACTER(LEN=1000)           :: cZBudRawFileName             = ''       !Filename for the Z-Budget output; used only to be transferred to Z-Budget class
      TYPE(GenericFileType)         :: FinalHeadsFile                          !Optional file to store final groundwater heads
      LOGICAL                       :: lFinalHeadsFile_Defined      = .FALSE.  !Flag to check if final heads output file is defined
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_NColumns 
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenAppGW
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetBudget_CumGWStorChange_GivenAppGW
      PROCEDURE,NOPASS :: GetBudget_CumGWStorChange_GivenFile
      PROCEDURE,NOPASS :: GetAquiferParameters_FromFile 
      PROCEDURE,PASS   :: GetAquiferKh 
      PROCEDURE,NOPASS :: GetAquiferKh_FromFile
      PROCEDURE,PASS   :: GetAquiferKv 
      PROCEDURE,NOPASS :: GetAquiferKv_FromFile
      PROCEDURE,PASS   :: GetAquitardKv 
      PROCEDURE,NOPASS :: GetAquitardKv_FromFile
      PROCEDURE,PASS   :: GetAquiferSy
      PROCEDURE,NOPASS :: GetAquiferSy_FromFile
      PROCEDURE,PASS   :: GetAquiferSs
      PROCEDURE,NOPASS :: GetAquiferSs_FromFile
      PROCEDURE,PASS   :: GetGWReturnFlows
      PROCEDURE,PASS   :: GetGWReturnFlowsIntoLakes
      PROCEDURE,PASS   :: GetGWReturnFlowsIntoStrmNodes
      PROCEDURE,PASS   :: GetGWReturnFlowsIntoStrmNodesAndLakes
      PROCEDURE,NOPASS :: GetGWNParametricGrids
      PROCEDURE,NOPASS :: GetGWNParametricNodes
      PROCEDURE,NOPASS :: GetGWNParametricElements
      PROCEDURE,NOPASS :: GetGWParametricNodeXY
      PROCEDURE,NOPASS :: GetGWParametricElementConfigData
      PROCEDURE,NOPASS :: GetGWParametricAquiferParameters
      PROCEDURE,PASS   :: GetNames
      PROCEDURE,PASS   :: GetHydrographTypeList
      PROCEDURE,PASS   :: GetNHydrographs
      PROCEDURE,PASS   :: GetHydrographIDs
      PROCEDURE,PASS   :: GetHydrographCoordinates
      PROCEDURE,PASS   :: GetHydrograph_GivenAppGW
      PROCEDURE,NOPASS :: GetHydrograph_GivenFile
      PROCEDURE,PASS   :: GetHeads_All
      PROCEDURE,PASS   :: GetHead_AtOneNodeLayer
      PROCEDURE,PASS   :: GetGWHeads_ForALayer_GivenAppGW
      PROCEDURE,NOPASS :: GetGWHeads_ForALayer_GivenFile 
      PROCEDURE,NOPASS :: GetGWHeadsIC
      PROCEDURE,PASS   :: GetNodalStorages
      PROCEDURE,PASS   :: GetElementDepthToGW              
      PROCEDURE,PASS   :: GetHorizontalFlow  
      PROCEDURE,PASS   :: GetRotation
      PROCEDURE,PASS   :: GetVerticalFlowAtNodesLayer 
      PROCEDURE,PASS   :: GetVerticalElementUpwardDownwardFlow_AtLayer
      PROCEDURE,PASS   :: GetChangeInStorageAtLayer
      PROCEDURE,PASS   :: GetElementStorageAtLayer
      PROCEDURE,PASS   :: GetZBudgetRawFileName             
      PROCEDURE,PASS   :: GetElementSy  
      PROCEDURE,PASS   :: GetSubsidence_All
      PROCEDURE,PASS   :: GetSubsidenceAtLayer
      PROCEDURE,PASS   :: GetSubsidenceInterbedThick_All
      PROCEDURE,PASS   :: GetNDrain                         
      PROCEDURE,PASS   :: GetNSubIrig                       
      PROCEDURE,PASS   :: GetTileDrainIDs                      
      PROCEDURE,PASS   :: GetTileDrainNodesLayers                 
      PROCEDURE,PASS   :: GetTileDrainFlows                 
      PROCEDURE,PASS   :: GetTileDrainFlowsToStreams        
      PROCEDURE,PASS   :: GetNWells                         
      PROCEDURE,PASS   :: GetNElemPumps
      PROCEDURE,PASS   :: GetElemPumpIDs
      PROCEDURE,PASS   :: GetWellIDs
      PROCEDURE,PASS   :: GetWellCoordinates
      PROCEDURE,PASS   :: GetWellPerfTopBottom
      PROCEDURE,PASS   :: GetPumpDestination
      PROCEDURE,PASS   :: GetNodalPumpActual 
      PROCEDURE,PASS   :: GetNodalPumpRequired
      PROCEDURE,PASS   :: GetElementPumpActual      !Total pumping (element pumping + well pumping) at all elements
      PROCEDURE,PASS   :: GetPumpActual             !Pumping for a given type (element or well) at all elements or wells        
      PROCEDURE,PASS   :: GetActualPumpingAtElementLayerNode      
      PROCEDURE,PASS   :: GetPumpElement
      PROCEDURE,PASS   :: GetPumpPurpose
      PROCEDURE,PASS   :: GetLayerPumpFactors               
      PROCEDURE,PASS   :: GetSupply 
      PROCEDURE,PASS   :: GetSupplySpecs
      PROCEDURE,PASS   :: GetSupplyAdjustData               
      PROCEDURE,PASS   :: GetiColAdjust                     
      PROCEDURE,PASS   :: GetNNodesWithBCType               
      PROCEDURE,PASS   :: GetNodesWithBCType                
      PROCEDURE,PASS   :: GetBoundaryFlowAtElementNodeLayer
      PROCEDURE,PASS   :: GetBoundaryFlowAtNodeLayer
      PROCEDURE,PASS   :: GetBoundaryFlowAtNodeLayer_All
      PROCEDURE,PASS   :: GetSubregionAgPumpingAverageDepthToGW
      PROCEDURE,PASS   :: GetZoneAgPumpingAverageDepthToGW
      PROCEDURE,PASS   :: SetHeads
      PROCEDURE,PASS   :: SetBCNodes
      PROCEDURE,PASS   :: SetBC
      PROCEDURE,PASS   :: SetIrigFracsRead                  
      PROCEDURE,PASS   :: SetSupplySpecs
      PROCEDURE,PASS   :: SetPumpRequired
      PROCEDURE,PASS   :: SetVelocities
      PROCEDURE,PASS   :: IsGWBudgetGenerated  
      PROCEDURE,PASS   :: IsCellVelocityOutputDefined
      PROCEDURE,PASS   :: IsVelocityTecplotDefined
      PROCEDURE,PASS   :: IsFaceFlowOutputDefined
      PROCEDURE,PASS   :: IsSubsidenceDefined               
      PROCEDURE,PASS   :: IsPumpingDefined                  
      PROCEDURE,PASS   :: IsPumpingToModelDomain 
      PROCEDURE,PASS   :: IsBoundaryFlowNode
      PROCEDURE,NOPASS :: RegisterWithMatrix
      PROCEDURE,PASS   :: Simulate
      PROCEDURE,PASS   :: ComputeRHS
      PROCEDURE,PASS   :: ConvertTimeUnit                   
      PROCEDURE,PASS   :: ReadTSData 
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: UpdateHeads                       
      PROCEDURE,PASS   :: UpdateStorage                     
      PROCEDURE,PASS   :: ResetHeads                        
      PROCEDURE,PASS   :: AdvanceState                      
      PROCEDURE,PASS   :: UpdatePumpDistFactors             
      PROCEDURE,PASS   :: ResetIrigFracs                    
      PROCEDURE,PASS   :: CheckSupplyDestinationConnection 
      PROCEDURE,PASS   :: ResetActualPumping
      PROCEDURE,PASS   :: RestorePumpingToReadValues
      PROCEDURE,PASS   :: TransferOutputToHDF
      PROCEDURE,PASS   :: RemoveBC
      GENERIC          :: GetGWHeads_ForALayer              => GetGWHeads_ForALayer_GivenFile        , &
                                                               GetGWHeads_ForALayer_GivenAppGW
      GENERIC          :: GetBudget_MonthlyFlows            => GetBudget_MonthlyFlows_GivenFile      , &
                                                               GetBudget_MonthlyFlows_GivenAppGW     
      GENERIC          :: GetBudget_CumGWStorChange         => GetBudget_CumGWStorChange_GivenFile   , &
                                                               GetBudget_CumGWStorChange_GivenAppGW  
      GENERIC          :: GetHydrograph                     => GetHydrograph_GivenFile               , &
                                                               GetHydrograph_GivenAppGW
  END TYPE AppGWType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iBudgetType_GW                 = f_iGWComp*1000 + 1
  CHARACTER(LEN=18),PARAMETER :: f_cDescription_GWBudget          = 'Groundwater budget'
  CHARACTER(LEN=22),PARAMETER :: f_cDescription_GWHyd_AtWell      = 'Groundwater hydrograph at well'
  CHARACTER(LEN=40),PARAMETER :: f_cDescription_GWHyd_AtNodeLayer = 'Groundwater hydrograph at node and layer'
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNGWBudColumns = 17
  CHARACTER(LEN=25),PARAMETER :: f_cBudgetColumnTitles(f_iNGWBudColumns) = ['Percolation'                , &
                                                                            'Beginning Storage (+)'      , &
                                                                            'Ending Storage (-)'         , &
                                                                            'Deep Percolation (+)'       , &
                                                                            'Gain from Stream (+)'       , &
                                                                            'Recharge (+)'               , &
                                                                            'Gain from Lake (+)'         , &
                                                                            'Boundary Inflow (+)'        , &
                                                                            'Subsidence (+)'             , &
                                                                            'Subsurface Irrigation (+)'  , &
                                                                            'Tile Drain Outflow (-)'     , &
                                                                            'Pumping (-)'                , &
                                                                            'GW Return Flow (-)'         , &
                                                                            'Outflow to Root Zone (-)'   , &
                                                                            'Net Subsurface Inflow (+)'  , &
                                                                            'Discrepancy (=)'            , &
                                                                            'Cumulative Subsidence'      ]
  
  
  ! -------------------------------------------------------------
  ! --- ELEVATION ABOVE BOTTOM OF AQUIFER AT WHICH HORIZONTAL FLOW WILL START SCALING DOWN
  ! -------------------------------------------------------------
  REAL(8),PARAMETER :: f_rScaleElevation = 1d0
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 13
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppGW::'


  
CONTAINS
    
    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INSTANTIATE GW COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE New(AppGW,lIsForInquiry,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,StrmConnectivity,iStrmNodeIDs,StrmGWConnector,iLakeIDs,LakeGWConnector,TimeStep,NTIME,iStat,GWHeadICFile,SubsICFile,lPrintParametersOverwrite) 
    CLASS(AppGWType),INTENT(OUT)         :: AppGW
    LOGICAL,INTENT(IN)                   :: lIsForInquiry
    CHARACTER(LEN=*),INTENT(IN)          :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    COMPLEX,INTENT(IN)                   :: StrmConnectivity(:)
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    INTEGER,INTENT(IN)                   :: NTIME,iStrmNodeIDs(:),iLakeIDs(:)
    INTEGER,INTENT(OUT)                  :: iStat
    TYPE(GenericFileType),OPTIONAL       :: GWHeadICFile,SubsICFile
    LOGICAL,OPTIONAL,INTENT(IN)          :: lPrintParametersOverwrite
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // "New"
    TYPE(GenericFileType)       :: AppGWParamFile
    TYPE(BudgetHeaderType)      :: BudHeader
    CHARACTER                   :: cALine*3000,cErrorMsg*300,cAllHeadOutFileName*1000,cHeadTecplotFileName*1200, &
                                   cVelTecplotFileName*1200,cBCFileName*1200,cOverwriteFileName*1200,            &
                                   cCellVelocityFileName*1200,cSubsidenceFileName*1200
    INTEGER                     :: NNodes,NElements,NLayers,NRegions,iGWNodeIDs(AppGrid%NNodes),iTecPlotFlag,    &
                                   iPrintParameters,iLineNumberToRewindTo,iErrorCode1,iErrorCode2,               &
                                   iGWReturnFlowDataFlag
    REAL(8)                     :: Head(AppGrid%NNodes,Stratigraphy%NLayers),rFactor 
    LOGICAL                     :: lTecPlotFlag_Defined,lGWReturnData_Provided
    CHARACTER,ALLOCATABLE       :: cCountLines(:)*50
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    INTEGER,PARAMETER           :: f_iYesPrintAquiferParameters = 1 , &
                                   f_iNotPrintAquiferParameters = 0 
    
    !Initialize
    iStat      = 0
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating groundwater component')
    
    !Initialize
    NNodes    = AppGrid%GetNNodes()
    NElements = AppGrid%GetNElements()
    NLayers   = Stratigraphy%GetNLayers()
    NRegions  = AppGrid%NSubregions
        
    !Allocate memory and initialize variables
    ALLOCATE (AppGW%rGWReturnFlow(NNodes)                 , &
              AppGW%iGWReturnFlowDestType(NNodes)         , &
              AppGW%iGWReturnFlowDest(NNodes)             , &
              AppGW%lComputeGWReturnFlowAtNode(NNodes)    , &
              AppGW%Nodes%Kh(NNodes,NLayers)              , &
              AppGW%Nodes%Kv(NNodes,NLayers)              , &
              AppGW%Nodes%AquitardKv(NNodes,NLayers)      , &
              AppGW%Nodes%LeakageV(NNodes,NLayers)        , &
              AppGW%Nodes%Ss(NNodes,NLayers)              , &
              AppGW%Nodes%Sy(NNodes,NLayers)              , &
              AppGW%ElemTransmissivity(NElements,NLayers) , &
              STAT = iErrorCode1                          , &
              ERRMSG = cErrorMsg                          )
    IF (iErrorCode1 .NE. 0) THEN
        MessageArray(1) = 'Error in allocating memory for the groundwater component.'
        MessageArray(2) = cErrorMsg
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    AppGW%Nodes%Kh           = 0.0
    AppGW%Nodes%Kv           = 0.0
    AppGW%Nodes%AquitardKv   = 0.0
    AppGW%Nodes%LeakageV     = 0.0
    AppGW%Nodes%Ss           = 0.0
    AppGW%Nodes%Sy           = 0.0
    AppGW%ElemTransmissivity = 0.0
       
    !Instantiate State data
    CALL AppGW%State%New(NNodes,NLayers,iStat)  
    IF (iStat .NE. 0) RETURN

    !Open file
    CALL AppGWParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='groundwater data main input',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read away the first version number line to avoid any errors
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !BACKWARD COMPATIBILITY: Check if there are 19 or 20 lines of entry to see if flag (dictating gw heads or 
    !                        depth-to-groundwater will be printed to TecPlot output) is provided
    CALL AppGWParamFile%ReadData(cCountLines,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (SIZE(cCountLines) .EQ. 19) THEN
        lTecPlotFlag_Defined = .FALSE.
    ELSE
        lTecPlotFlag_Defined = .TRUE.
    END IF
    CALL AppGWParamFile%RewindFile()
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    
    !Initialize related files
    !-------------------------
    
    !Boundary conditions data filename
    CALL AppGWParamFile%ReadData(cBCFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cBCFileName = StripTextUntilCharacter(cBCFileName,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cBCFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cBCFileName)),cWorkingDirectory,cAbsPathFileName)
    cBCFileName = cAbsPathFileName
    
    !Tile drains/subsurface irrigation
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AppGW%AppTileDrain%New(lIsForInquiry,cAbsPathFileName,cWorkingDirectory,iStrmNodeIDs,TimeStep,AppGrid,Stratigraphy,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AppGW%AppTileDrain%GetNDrain() .GT. 0   .OR.   AppGW%AppTileDrain%GetNSubIrig() .GT. 0)  &
        AppGW%lTileDrain_Defined = .TRUE.
    
    !Pumping
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AppGW%AppPumping%New(lIsForInquiry,cAbsPathFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AppGW%AppPumping%GetNWells() .GT. 0   .OR.   AppGW%AppPumping%GetNElemPumps() .GT. 0)   &
        AppGW%lPumping_Defined = .TRUE.
    
    !Subsidence filename
    CALL AppGWParamFile%ReadData(cSubsidenceFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cSubsidenceFileName = StripTextUntilCharacter(cSubsidenceFileName,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cSubsidenceFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cSubsidenceFileName)),cWorkingDirectory,cAbsPathFileName)
    cSubsidenceFileName = cAbsPathFileName

    !Parameter over-write file name
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    cOverwriteFileName = cAbsPathFileName

    !Output units and conversion factors
    CALL AppGWParamFile%ReadData(AppGW%FactHead,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(cALine) ; cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)
    AppGW%UnitHead = ADJUSTL(TRIM(cALine))
    CALL AppGWParamFile%ReadData(AppGW%FactFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(cALine) ; cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)
    AppGW%UnitFlow = ADJUSTL(TRIM(cALine))
    CALL AppGWParamFile%ReadData(AppGW%FactVelocity,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(cALine) ; cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)
    AppGW%UnitVelocity = ADJUSTL(TRIM(cALine))
    
    !Output file for velocities at cell centroids
    CALL AppGWParamFile%ReadData(cCellVelocityFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN   
    CALL CleanSpecialCharacters(cCellVelocityFileName)
    cCellVelocityFileName = ADJUSTL(StripTextUntilCharacter(cCellVelocityFileName,f_cInlineCommentChar))
    CALL EstablishAbsolutePathFileName(TRIM(cCellVelocityFileName),cWorkingDirectory,cAbsPathFileName)
    cCellVelocityFileName = cAbsPathFileName

    !Vertical flow output file
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cALine) 
    cALine = ADJUSTL(StripTextUntilCharacter(cALine,f_cInlineCommentChar))
    CALL EstablishAbsolutePathFileName(TRIM(cALine),cWorkingDirectory,cAbsPathFileName)
    CALL VerticalFlowOutput_New(lIsForInquiry,TimeStep,NLayers,NRegions,AppGW%UnitFlow,cAbsPathFileName,AppGW%VerticalFlowOutput,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Output file for heads at all nodes and layers
    CALL AppGWParamFile%ReadData(cAllHeadOutFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cAllHeadOutFileName) 
    cAllHeadOutFileName = ADJUSTL(StripTextUntilCharacter(cAllHeadOutFileName,f_cInlineCommentChar))
    CALL EstablishAbsolutePathFileName(TRIM(cAllHeadOutFileName),cWorkingDirectory,cAbsPathFileName)
    cAllHeadOutFileName = cAbsPathFileName
    
    !Head Tecplot output file
    CALL AppGWParamFile%ReadData(cHeadTecplotFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cHeadTecplotFileName) 
    cHeadTecplotFileName = ADJUSTL(StripTextUntilCharacter(cHeadTecplotFileName,f_cInlineCommentChar))
    CALL EstablishAbsolutePathFileName(TRIM(cHeadTecplotFileName),cWorkingDirectory,cAbsPathFileName)
    cHeadTecplotFileName = cAbsPathFileName
    
    !Velocity Tecplot output file
    CALL AppGWParamFile%ReadData(cVelTecplotFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(cVelTecplotFileName) 
    cVelTecplotFileName = ADJUSTL(StripTextUntilCharacter(cVelTecplotFileName,f_cInlineCommentChar))
    CALL EstablishAbsolutePathFileName(TRIM(cVelTecplotFileName),cWorkingDirectory,cAbsPathFileName)
    cVelTecplotFileName = cAbsPathFileName
    
    !Groundwater budget output
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        IF (lIsForInquiry) THEN
            CALL AppGW%GWBudFile%New(cAbsPathFileName,iStat)
            IF (iStat .EQ. -1) RETURN
        ELSE
            BudHeader = PrepareGWBudgetHeader(NTIME,TimeStep,AppGrid)
            CALL AppGW%GWBudFile%New(cAbsPathFileName,BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
            !Allocate memory for subregional storage values
            ALLOCATE (AppGW%RegionalStorage(NRegions+1) , AppGW%RegionalStorage_P(NRegions+1) , STAT=iErrorCode1 , ERRMSG=cErrorMsg)
            IF (iErrorCode1 .NE. 0) THEN 
                MessageArray(1) = 'Error in allocating memory for the regional storage values for groundwater budget.'
                MessageArray(2) = cErrorMsg
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        AppGW%lGWBudFile_Defined = .TRUE.
    END IF
    
    !Z-Budget binary output filename
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    AppGW%cZBudRawFileName = cAbsPathFileName
    
    !Final results output file
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        IF (lIsForInquiry) THEN
            CALL AppGW%FinalHeadsFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,Descriptor='final groundwater heads output',iStat=iStat)
        ELSE
            CALL AppGW%FinalHeadsFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,Descriptor='final groundwater heads output',iStat=iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
        IF (AppGW%FinalHeadsFile%iGetFileType() .NE. f_iTXT) THEN
            CALL SetLastMessage('End-of-simulation groundwater heads output file must be a text file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        AppGW%lFinalHeadsFile_Defined = .TRUE.
    END IF
    
    !TecPlot flag, if provided, to print gw heads or depth-to-groundwater
    IF (lTecPlotFlag_Defined) THEN
        CALL AppGWParamFile%ReadData(iTecPlotFlag,iStat)  
        IF (iStat .EQ. -1) RETURN
    ELSE
        iTecPlotFlag = f_iTecPlot_PrintGWHeads
    END IF
    
    !Debug option
    CALL AppGWParamFile%ReadData(iPrintParameters,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (PRESENT(lPrintParametersOverwrite)) THEN
        IF (lPrintParametersOverwrite) THEN
            iPrintParameters = f_iYesPrintAquiferParameters
        ELSE
            iPrintParameters = f_iNotPrintAquiferParameters
        END IF
    END IF
    
    !Groundwater hydrographs
    CALL AppGW%GWHyd%New(lIsForInquiry,AppGrid,Stratigraphy,cWorkingDirectory,iGWNodeIDs,iTecPlotFlag,AppGW%FactHead,AppGW%UnitHead,AppGW%UnitFlow,AppGW%UnitVelocity,TRIM(cAllHeadOutFileName),TRIM(cCellVelocityFileName),TRIM(cHeadTecplotFileName),TRIM(cVelTecplotFileName),TimeStep,NTIME,AppGWParamFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Aquifer parameters
    CALL ReadAquiferParameters(NLayers,AppGrid,TimeStep,AppGWParamFile,AppGW%VarTimeUnit,AppGW%Nodes,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !BACKWARD COMPATIBILITY: Check if data for gw return flow is provided by trying to read both gw return flow and initial conditions
    iLineNumberToRewindTo = AppGWParamFile%GetLineNumber()
    !Try reading gw return flow data (assume it is gw return flow data)
    CALL AppGWParamFile%ReadData(iGWReturnFlowDataFlag,iErrorCode1)       
    IF (iGWReturnFlowDataFlag .EQ. 1) CALL AppGWParamFile%ReadData(cCountLines,iErrorCode2)  
    !Now, try to read initial conditions
    CALL AppGWParamFile%ReadData(rFactor,iErrorCode1)  
    CALL AppGWParamFile%ReadData(cCountLines,iErrorCode2)
    IF (iErrorCode1+iErrorCode2 .NE. 0) THEN
        lGWReturnData_Provided      = .FALSE.
        AppGW%lSimulateGWReturnFlow = .FALSE.
    ELSE
        lGWReturnData_Provided      = .TRUE.
    END IF
    CALL AppGWParamFile%GotoLine(iLineNumberToRewindTo,iStat)  ;  IF (iStat .NE. 0) RETURN
   
    !Read GW return flow data, if provided
    IF (lGWReturnData_Provided) THEN
        CALL ReadGWReturnFlowDestinations(iStrmNodeIDs,iLakeIDs,iGWNodeIDs,AppGWParamFile,AppGW%lSimulateGWReturnFlow,AppGW%iGWReturnFlowDestType,AppGW%iGWReturnFlowDest,iStat)
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Initial conditions
    IF (PRESENT(GWHeadICFile)) THEN
        CALL ReadInitialHeads(GWHeadICFile,NNodes,iGWNodeIDs,Stratigraphy,Head,iStat)
    ELSE
        CALL ReadInitialHeads(AppGWParamFile,NNodes,iGWNodeIDs,Stratigraphy,Head,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    AppGW%State%Head = Head
    
    !Instantiate the boundary conditions data and overwrite the initial conditions if necessary
    CALL AppGW%AppBC%New(lIsForInquiry,ADJUSTL(cBCFileName),cWorkingDirectory,AppGrid,Stratigraphy,iGWNodeIDs,AppGW%UnitFlow,TimeStep,AppGW%State%Head,iStat)
    IF (iStat .EQ. -1) RETURN
    AppGW%lAppBC_Defined = AppGW%AppBC%IsDefined()

    !Assign previous head as current head
    AppGW%State%Head_P = AppGW%State%Head
    
    !Instantiate subsidence; this has to be done after AppGW initial conditions are processed
    CALL AppGW%AppSubsidence%New(lIsForInquiry,cSubsidenceFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat,SubsICFile)
    IF (iStat .EQ. -1) RETURN
    AppGW%lSubsidence_Defined = AppGW%AppSubsidence%IsDefined()

    !Aquifer overwrite parameters
    IF (cOverwriteFileName .NE. '') THEN
        CALL OverwriteParameters(cOverwriteFileName,AppGrid,AppGW%VarTimeUnit,TimeStep%TrackTime,AppGW%lSubsidence_Defined,AppGW%Nodes,AppGW%AppSubsidence,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Print final aquifer parameters, if desired
    IF (iPrintParameters .EQ. f_iYesPrintAquiferParameters) THEN
        IF (IsLogFileDefined()) THEN
            CALL PrintAquiferParameters(iGWNodeIDs,AppGW%Nodes)
            IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%PrintParameters(iGWNodeIDs,AppGrid%AppNode%Area)
        END IF
    END IF
    
    !Print initial conditions for hydrographs including head at all nodes and Tecplot output
    IF (.NOT. lIsForInquiry) CALL AppGW%GWHyd%PrintInitialValues(AppGrid,Stratigraphy,AppGW%State%Head,AppGW%FactHead,AppGW%FactVelocity,StrmConnectivity,TimeStep)
    
    !Process aquifer parameters for use in simulation
    CALL ProcessAquiferParameters(AppGrid,Stratigraphy,AppGW%lSubsidence_Defined,AppGW%AppSubsidence,AppGW%Nodes,AppGW%State,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compute initial subregional groundwater storages
    CALL ComputeRegionalStorage(AppGrid,Stratigraphy,AppGW)
    AppGW%RegionalStorage_P = AppGW%RegionalStorage
    
    !Compile nodes where gw return flow will be computed
    CALL CompileNodesWithGWReturnFlow(Stratigraphy,AppGW%AppBC,StrmGWConnector,LakeGWConnector,AppGW%rGWReturnFlow,AppGW%lComputeGWReturnFlowAtNode)
    
    !Close GW main file
    CALL AppGWParamFile%Kill()
    
  END SUBROUTINE New
  
  
  
   
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL APPGW OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppGW)
    CLASS(AppGWType) :: AppGW
    
    !Local variables
    INTEGER         :: iErrorCode
    TYPE(AppGWType) :: Dummy
    
    !Deallocate allocatable arrays
    DEALLOCATE (AppGW%ElemTransmissivity         , &
                AppGW%RegionalStorage            , &
                AppGW%RegionalStorage_P          , &
                AppGW%rGWReturnFlow              , &
                AppGW%lComputeGWReturnFlowAtNode , &
                STAT=iErrorCode                  )
    
    !Kill gw node data
    CALL AppGW%Nodes%Kill()
    
    !Kill gw state related data
    CALL AppGW%State%Kill()
    
    !Kill groundwater hydrographs related data
    CALL AppGW%GWHyd%Kill()
    
    !Kill boundary conditions related data
    IF (AppGW%lAppBC_Defined) CALL AppGW%AppBC%Kill()
    
    !Kill tile drains related data
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%Kill()
    
    !Kill pumping related data
    IF (AppGW%lPumping_Defined) CALL AppGW%AppPumping%Kill()
    
    !Kill subsidence related data
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%Kill()
    
    !Kill vertical flow output data
    CALL VerticalFlowOutput_Kill(AppGW%VerticalFlowOutput)
    
    !Close groundwater budget output
    IF (AppGW%lGWBudFile_Defined) CALL AppGW%GWBudFile%Kill()
    
    !Close final heads output file
    IF (AppGW%lFinalHeadsFile_Defined) CALL AppGW%FinalHeadsFile%Kill()
    
    !Set the object attributes to their default values
    SELECT TYPE (AppGW)
        TYPE IS (AppGWType)
            AppGW = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL GWNode OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GWNode_Kill(GWNodes)
    CLASS(GWNodeType) :: GWNodes
    
    !Local variables
    INTEGER :: iErrorCode
    
    !Deallocate allocatable arrays
    DEALLOCATE (GWNodes%Kh         , &
                GWNodes%Kv         , &
                GWNodes%AquitardKv , &
                GWNodes%LeakageV   , &
                GWNodes%Ss         , &
                GWNodes%Sy         , &
                STAT=iErrorCode    )
    
  END SUBROUTINE GWNode_Kill
  
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF A BOUNDARY NODE FLOW NODE
  ! --- Note: It is assumed that the node is already on the model boundary 
  ! -------------------------------------------------------------
  PURE FUNCTION IsBoundaryFlowNode(AppGW,iNode,iLayer) RESULT(lFlowNode)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iNode,iLayer
    LOGICAL                     :: lFlowNode
    
    lFlowNode = AppGW%AppBC%IsBoundaryFlowNode(iNode,iLayer)
    
  END FUNCTION IsBoundaryFlowNode
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF SUBSIDENCE IS SIMULATED
  ! -------------------------------------------------------------
  PURE FUNCTION IsSubsidenceDefined(AppGW) RESULT(IsDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: IsDefined
    
    IsDefined = AppGW%lSubsidence_Defined
    
  END FUNCTION IsSubsidenceDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF GW BUDGET OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsGWBudgetGenerated(AppGW) RESULT(lGenerated)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lGenerated
    
    lGenerated = AppGW%lGWBudFile_Defined
    
  END FUNCTION IsGWBudgetGenerated
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF GW VELOCITIES TECPLOT OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsVelocityTecplotDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%GWHyd%IsVelocityTecplotDefined()
    
  END FUNCTION IsVelocityTecplotDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF CELL VELOCITY OUTPUT FILE IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsCellVelocityOutputDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%GWHyd%IsCellVelocityOutputDefined()
    
  END FUNCTION IsCellVelocityOutputDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF FACE FLOW OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsFaceFlowOutputDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%GWHyd%IsFaceFlowOutputDefined()
    
  END FUNCTION IsFaceFlowOutputDefined
  
  
  ! -------------------------------------------------------------
  ! --- GET THE FLAG TO CHECK IF PUMPING IS DEFINED 
  ! -------------------------------------------------------------
  PURE FUNCTION IsPumpingDefined(AppGW) RESULT(lDefined)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDefined
    
    lDefined = AppGW%lPumping_Defined
    
  END FUNCTION IsPumpingDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ANY OF THE PUMPING GOES TO MODEL DOMAIN
  ! -------------------------------------------------------------
  PURE FUNCTION IsPumpingToModelDomain(AppGW) RESULT(lDest)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL                     :: lDest
    
    IF (AppGW%lPumping_Defined) THEN
        lDest = AppGW%AppPumping%IsDestinationToModelDomain()
    ELSE
        lDest = .FALSE.
    END IF
    
  END FUNCTION IsPumpingToModelDomain
    
    


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET GW RETURN FLOWS AT EACH NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetGWReturnFlows(AppGW,rGWReturnFlows)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rGWReturnFlows(:)
    
    rGWReturnFlows = AppGW%rGWReturnFlow
    
  END SUBROUTINE GetGWReturnFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET GW RETURN FLOWS INTO EACH STREAM NODE AND LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetGWReturnFlowsIntoStrmNodesAndLakes(AppGW,rGWReturnFlowsIntoStrmNodes,rGWReturnFlowsIntoLakes)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rGWReturnFlowsIntoStrmNodes(:),rGWReturnFlowsIntoLakes(:)
    
    !Local variables
    INTEGER :: indxNode,iDest
    
    !Initialize
    rGWReturnFlowsIntoStrmNodes = 0.0
    rGWReturnFlowsIntoLakes     = 0.0
    
    IF (.NOT. AppGW%lSimulateGWReturnFlow) RETURN
    
    DO indxNode=1,SIZE(AppGW%iGWReturnFlowDestType)
        SELECT CASE (AppGW%iGWReturnFlowDestType(indxNode))
            CASE (f_iFlowDest_StrmNode)
                iDest                              = AppGW%iGWReturnFlowDest(indxNode)
                rGWReturnFlowsIntoStrmNodes(iDest) = rGWReturnFlowsIntoStrmNodes(iDest) + AppGW%rGWReturnFlow(indxNode)
                
            CASE (f_iFlowDest_Lake)
                iDest                          = AppGW%iGWReturnFlowDest(indxNode)
                rGWReturnFlowsIntoLakes(iDest) = rGWReturnFlowsIntoLakes(iDest) + AppGW%rGWReturnFlow(indxNode)
        END SELECT
    END DO
    
  END SUBROUTINE GetGWReturnFlowsIntoStrmNodesAndLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET GW RETURN FLOWS INTO EACH STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE GetGWReturnFlowsIntoStrmNodes(AppGW,rGWReturnFlowsIntoStrmNodes)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rGWReturnFlowsIntoStrmNodes(:)
    
    !Local variables
    INTEGER :: indxNode,iDest
    
    !Initialize
    rGWReturnFlowsIntoStrmNodes = 0.0
    
    IF (.NOT. AppGW%lSimulateGWReturnFlow) RETURN
    
    DO indxNode=1,SIZE(AppGW%iGWReturnFlowDestType)
        IF (AppGW%iGWReturnFlowDestType(indxNode) .EQ. f_iFlowDest_StrmNode) THEN
            iDest                              = AppGW%iGWReturnFlowDest(indxNode)
            rGWReturnFlowsIntoStrmNodes(iDest) = rGWReturnFlowsIntoStrmNodes(iDest) + AppGW%rGWReturnFlow(indxNode)
                
        END IF
    END DO
    
  END SUBROUTINE GetGWReturnFlowsIntoStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET GW RETURN FLOWS INTO EACH LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetGWReturnFlowsIntoLakes(AppGW,rGWReturnFlowsIntoLakes)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rGWReturnFlowsIntoLakes(:)
    
    !Local variables
    INTEGER :: indxNode,iDest
    
    !Initialize
    rGWReturnFlowsIntoLakes = 0.0
    
    IF (.NOT. AppGW%lSimulateGWReturnFlow) RETURN
    
    DO indxNode=1,SIZE(AppGW%iGWReturnFlowDestType)
        IF (AppGW%iGWReturnFlowDestType(indxNode) .EQ. f_iFlowDest_Lake) THEN
            iDest                          = AppGW%iGWReturnFlowDest(indxNode)
            rGWReturnFlowsIntoLakes(iDest) = rGWReturnFlowsIntoLakes(iDest) + AppGW%rGWReturnFlow(indxNode)
        END IF
    END DO
    
  END SUBROUTINE GetGWReturnFlowsIntoLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET GW NUMBER OF PARAMETRIC GRIDS
  ! -------------------------------------------------------------
  SUBROUTINE GetGWNParametricGrids(cGWMainFileName,iNParamGrids,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cGWMainFileName
    INTEGER,INTENT(OUT)         :: iNParamGrids,iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx
    CHARACTER             :: ALine*10
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Number of parametric grids
    CALL vGWMainFile%ReadData(iNParamGrids,iStat) 

    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWNParametricGrids
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF PARAMETRIC NODES FOR A PARAMETRIC GRID
  ! -------------------------------------------------------------
  SUBROUTINE GetGWNParametricNodes(cGWMainFileName,iParamGridID,iNParamGridNodes,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cGWMainFileName
    INTEGER,INTENT(IN)          :: iParamGridID
    INTEGER,INTENT(OUT)         :: iNParamGridNodes,iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx,iNParamGrids,iDummy
    CHARACTER             :: ALine*10
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Number of parametric grids
    CALL vGWMainFile%ReadData(iNParamGrids,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iNParamGrids .EQ. 0) THEN
        iNParamGridNodes = 0
        CALL vGWMainFile%Kill()
        RETURN
    END IF
    
    !Skip data blocks
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iParamGridID-1
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
        IF (iDummy .GT. 0) THEN
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  
            IF (iStat .NE. 0) RETURN
        END IF
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    END DO
    
    !Read the number of parametric grid nodes
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNParamGridNodes,iStat)
    
    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWNParametricNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF PARAMETRIC ELEMENTS FOR A PARAMETRIC GRID
  ! -------------------------------------------------------------
  SUBROUTINE GetGWNParametricElements(cGWMainFileName,iParamGridID,iNParamGridElements,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cGWMainFileName
    INTEGER,INTENT(IN)          :: iParamGridID
    INTEGER,INTENT(OUT)         :: iNParamGridElements,iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx,iNParamGrids,iDummy
    CHARACTER             :: ALine*10
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Number of parametric grids
    CALL vGWMainFile%ReadData(iNParamGrids,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iNParamGrids .EQ. 0) THEN
        iNParamGridElements = 0
        CALL vGWMainFile%Kill()
        RETURN
    END IF
    
    !Skip data blocks
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iParamGridID-1
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
        IF (iDummy .GT. 0) THEN
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  
            IF (iStat .NE. 0) RETURN
        END IF
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    END DO
    
    !Read the number of parametric grid nodes
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNParamGridElements,iStat)
    
    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWNParametricElements
  
  
  ! -------------------------------------------------------------
  ! --- GET COORDINATES OF PARAMETRIC NODES FOR A PARAMETRIC GRID
  ! -------------------------------------------------------------
  SUBROUTINE GetGWParametricNodeXY(cGWMainFileName,iParamGridID,iNLayers,rX,rY,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cGWMainFileName
    INTEGER,INTENT(IN)          :: iParamGridID,iNLayers
    REAL(8),INTENT(OUT)         :: rX(:),rY(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx,iNParamGrids,iNNodes,iNElems,indxLayer
    REAL(8)               :: rFactor,rDummyArray8(8)
    CHARACTER             :: ALine*10
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Number of parametric grids
    CALL vGWMainFile%ReadData(iNParamGrids,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iNParamGrids .EQ. 0) THEN
        rX = 0.0
        rY = 0.0
        CALL vGWMainFile%Kill()
        RETURN
    END IF
    
    !Conversion factor for coordinates
    CALL vGWMainFile%ReadData(rFactor,iStat)  ;  IF (iStat .NE. 0) RETURN

    !Skip data blocks
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iParamGridID-1
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iNNodes,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iNElems,iStat)  ;  IF (iStat .NE. 0) RETURN
        IF (iNElems .GT. 0) THEN
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  
            IF (iStat .NE. 0) RETURN
        END IF
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip the unnecessary data for the parametric grid in question
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNNodes,iStat)   ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNElems,iStat)   ;  IF (iStat .NE. 0) RETURN
    IF (iNElems .GT. 0) THEN
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Read the coordinates
    DO indx=1,SIZE(rX)
        CALL vGWMainFile%ReadData(rDummyArray8,iStat)  ;  IF (iStat .NE. 0) RETURN
        rX(indx) = rDummyArray8(2) * rFactor
        rY(indx) = rDummyArray8(3) * rFactor
        DO indxLayer=2,iNLayers
            CALL vGWMainFile%ReadData(ALine,iStat)
            IF (iStat .NE. 0) RETURN
        END DO
    END DO
    
    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWParametricNodeXY
  
  
  ! -------------------------------------------------------------
  ! --- GET VERTICES OF A PARAMETRIC ELEMENT FOR A PARAMETRIC GRID
  ! -------------------------------------------------------------
  SUBROUTINE GetGWParametricElementConfigData(cGWMainFileName,iParamGridID,iParamElemID,iVertices,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cGWMainFileName
    INTEGER,INTENT(IN)          :: iParamGridID,iParamElemID
    INTEGER,INTENT(OUT)         :: iVertices(4),iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx,iNParamGrids,iNNodes,iNElems,iDummyArray5(5)
    CHARACTER             :: ALine*10
    
    !Initialize
    iVertices = 0
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Number of parametric grids
    CALL vGWMainFile%ReadData(iNParamGrids,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iNParamGrids .EQ. 0) THEN
        CALL vGWMainFile%Kill()
        RETURN
    END IF
    
    !Skip data blocks
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iParamGridID-1
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iNNodes,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iNElems,iStat)  ;  IF (iStat .NE. 0) RETURN
        IF (iNElems .GT. 0) THEN
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  
            IF (iStat .NE. 0) RETURN
        END IF
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip the unnecessary data for the parametric grid in question
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNNodes,iStat)   ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNElems,iStat)   ;  IF (iStat .NE. 0) RETURN
    IF (iNElems .EQ. 0) THEN
        CALL vGWMainFile%Kill()
        RETURN
    END IF
    
    !Read the elememnt config data
    DO indx=1,iNElems
        CALL vGWMainFile%ReadData(iDummyArray5,iStat)  ;  IF (iStat .NE. 0) RETURN
        IF (iDummyArray5(1) .EQ. iParamElemID) THEN
            iVertices = iDummyArray5(2:)
            EXIT
        END IF
    END DO
    
    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWParametricElementConfigData
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER PARAMETERS DEFINED BY A SPECIFIED PARAMETRIC GRID
  ! -------------------------------------------------------------
  SUBROUTINE GetGWParametricAquiferParameters(cGWMainFileName,iParamGridID,rKh,rAquiferKv,rAquitardKv,rSy,rSs,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cGWMainFileName
    INTEGER,INTENT(IN)          :: iParamGridID
    REAL(8),INTENT(OUT)         :: rKh(:,:),rAquiferKv(:,:),rAquitardKv(:,:),rSy(:,:),rSs(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx,indxLayer,iNParamGrids,iNNodes,iNElems,iNLayers
    REAL(8)               :: rFactors(6),rDummyArray8(8),rDummyArray5(5)
    CHARACTER             :: ALine*10
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Number of parametric grids
    CALL vGWMainFile%ReadData(iNParamGrids,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iNParamGrids .EQ. 0) THEN
        rKh         = -999.0
        rSs         = -999.0
        rSy         = -999.0
        rAquitardKv = -999.0
        rAquiferKv  = -999.0
        CALL vGWMainFile%Kill()
        RETURN
    END IF
    
    !Conversion factors
    CALL vGWMainFile%ReadData(rFactors,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Skip data blocks
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iParamGridID-1
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iNNodes,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL vGWMainFile%ReadData(iNElems,iStat)  ;  IF (iStat .NE. 0) RETURN
        IF (iNElems .GT. 0) THEN
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  
            IF (iStat .NE. 0) RETURN
        END IF
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip the unnecessary data for the parametric grid in question
    CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNNodes,iStat)   ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%ReadData(iNElems,iStat)   ;  IF (iStat .NE. 0) RETURN
    IF (iNElems .GT. 0) THEN
        CALL vGWMainFile%SkipDataBlocks(1,iStat)  
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Read the parametric data
    iNLayers = SIZE(rKh , DIM=2)
    DO indx=1,iNNodes
        CALL vGWMainFile%ReadData(rDummyArray8,iStat)  ;  IF (iStat .NE. 0) RETURN
        rKh(indx,1)         = rDummyArray8(4) * rFactors(2)
        rSs(indx,1)         = rDummyArray8(5) * rFactors(3)
        rSy(indx,1)         = rDummyArray8(6) * rFactors(4)
        rAquitardKv(indx,1) = rDummyArray8(7) * rFactors(5)
        rAquiferKv(indx,1)  = rDummyArray8(8) * rFactors(6)
        DO indxLayer=2,iNLayers
            CALL vGWMainFile%ReadData(rDummyArray5,iStat)  ;  IF (iStat .NE. 0) RETURN
            rKh(indx,indxLayer)         = rDummyArray5(1) * rFactors(2)
            rSs(indx,indxLayer)         = rDummyArray5(2) * rFactors(3)
            rSy(indx,indxLayer)         = rDummyArray5(3) * rFactors(4)
            rAquitardKv(indx,indxLayer) = rDummyArray5(4) * rFactors(5)
            rAquiferKv(indx,indxLayer)  = rDummyArray5(5) * rFactors(6)
        END DO
    END DO
    
    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWParametricAquiferParameters
  
  
  ! -------------------------------------------------------------
  ! --- GET INITIAL GW HEADS
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeadsIC(cGWMainFileName,iNodeIDs,Stratigraphy,rGWHeadsIC,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWMainFileName
    INTEGER,INTENT(IN)                :: iNodeIDs(:)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rGWHeadsIC(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    TYPE(GenericFileType) :: vGWMainFile
    INTEGER               :: iNData,indx,iNNodes,iDummy
    CHARACTER             :: ALine*10
    
    !Open main gw file
    CALL vGWMainFile%New(cGWMainFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='gw main input file',iStat=iStat)
    IF (iStat .NE. 0) RETURN
    
    !Read away the first line that lists the version number
    CALL vGWMainFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip 2 data blocks (this will get us to optional hydrograph output data)
    CALL vGWMainFile%SkipDataBlocks(2,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Skip groundwater hydrograph output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip element face flow output data
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+1
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Skip aquifer parameters
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL vGWMainFile%SkipDataBlocks(2,iStat)   ;  IF (iStat .NE. 0) RETURN
    !Aquifer parameters are provided via parametric grids
    IF (iNData .GT. 0) THEN
        DO indx=1,iNData
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
            CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
            CALL vGWMainFile%ReadData(iDummy,iStat)   ;  IF (iStat .NE. 0) RETURN
            IF (iDummy .GT. 0) THEN
                CALL vGWMainFile%SkipDataBlocks(1,iStat)  
                IF (iStat .NE. 0) RETURN
            END IF
            CALL vGWMainFile%SkipDataBlocks(1,iStat)  ;  IF (iStat .NE. 0) RETURN
        END DO
    !Aquifer parameters are provided at each node and layer
    ELSE
        CALL vGWMainFile%SkipDataBlocks(1,iStat)
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Skip anomaly hydrualic conductivities
    CALL vGWMainFile%ReadData(iNData,iStat)  ;  IF (iStat .NE. 0) RETURN
    DO indx=1,iNData+2
        CALL vGWMainFile%ReadData(ALine,iStat)
        IF (iStat .NE. 0) RETURN
    END DO
    
    !Now, we are at initial conditions
    iNNodes = SIZE(iNodeIDs)
    CALL ReadInitialHeads(vGWMainFile,iNNodes,iNodeIDs,Stratigraphy,rGWHeadsIC,iStat)
    
    !Close file
    CALL vGWMainFile%Kill()
    
  END SUBROUTINE GetGWHeadsIC
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING WHEN AppGW OBJECT IS FULLY INSTANTIATED
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeads_ForALayer_GivenAppGW(AppGW,iNNodes,iNLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    CLASS(AppGWType)              :: AppGW
    INTEGER,INTENT(IN)            :: iNNodes,iNLayers,iLayer
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rGWHeads(:,:)    !rGWHeads in (node,time) combination
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31) :: ThisProcedure = ModName // 'GetGWHeads_ForALayer_GivenAppGW'
    CHARACTER(:),ALLOCATABLE     :: cFileName
    
    !Initialize
    iStat = 0
    
    IF (AppGW%GWHyd%IsAllHeadOutputDefined()) THEN
        !Get the name of the text output file and convert it to the same name but HDF version
        CALL AppGW%GWHyd%GetAllHeadOutputFileName(cFileName)
        cFileName(LEN_TRIM(cFileName)-2:) = 'hdf'
        
        !The hdf file should already have been created by now but still check that it exists
        IF (DoesFileExist(cFileName)) THEN
            CALL GetGWHeads_ForALayer_GivenFile(cFileName,iNNodes,iNLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
        ELSE
            MessageArray(1) = 'File '//cFileName//' cannot be found for data retrieval!'
            MessageArray(2) = 'Groundwater heads at a layer will be retrieved from the text output file.'
            MessageArray(3) = 'This may take a substantially long time.'
            CALL LogMessage(MessageArray(1:3),f_iWarn,ThisProcedure)
            CALL AppGW%GWHyd%ReadGWHeadsAll_ForALayer(iNNodes,iLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,AppGW%FactHead,rFact_LT,rOutputDates,rGWHeads,iStat)
        END IF
    ELSE
        MessageArray(1) = 'GW heads at all nodes for a layer cannot be retrieved '
        MessageArray(2) = 'this output file was not generated for the model!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
    END IF
        
  END SUBROUTINE GetGWHeads_ForALayer_GivenAppGW
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING WHEN AppGW OBJECT IS NOT FULLY INSTANTIATED
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeads_ForALayer_GivenFile(cFileName,NNodes,NLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    INTEGER,INTENT(IN)            :: NNodes,NLayers,iLayer
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rGWHeads(:,:)    !rGWHeads in (node,time) combination
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER               :: FileReadCode,indxS,indxL
    REAL(8)               :: rValues(NNodes*NLayers,SIZE(rOutputDates))
    TYPE(GenericFileType) :: InFile
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Julian dates for data
    CALL GetJulianDatesBetweenTimeStampsWithTimeIncrement(TimeStep%DeltaT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputDates)
    
    !Read data
    CALL InFile%ReadData(cOutputBeginDateAndTime,1,rValues,FileReadCode,iStat)
    
    !Transfer read data to permenant array
    indxS    = (iLayer-1)*NNodes + 1
    indxL    = iLayer * NNodes
    rGWHeads = rValues(indxS:indxL,:)
    IF (rFact_LT .NE. 1.0) rGWHeads = rGWHeads * rFact_LT
    
    !Close file
    CALL InFile%Kill()
        
  END SUBROUTINE GetGWHeads_ForALayer_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET NODAL STORAGES (AND TEHIR DERIVATIVES, IF DESIRED)
  ! -------------------------------------------------------------
  SUBROUTINE GetNodalStorages(AppGW,AppGrid,Stratigraphy,rNodalStor,rdNodalStor)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rNodalStor(AppGrid%NNodes,Stratigraphy%NLayers)
    REAL(8),OPTIONAL,INTENT(OUT)      :: rdNodalStor(AppGrid%NNodes,Stratigraphy%NLayers)
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    !Compute
    ASSOCIATE (pTopElev    => Stratigraphy%TopElev    , &
               pBottomElev => Stratigraphy%BottomElev , &
               pHead       => AppGW%State%Head        , &
               pSs         => AppGW%Nodes%Ss          , &
               pSy         => AppGW%Nodes%Sy          )
        !Compute storage
        !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(indxLayer,indxNode) 
        !$OMP DO COLLAPSE(2)
        DO indxLayer=1,Stratigraphy%NLayers
            DO indxNode=1,AppGrid%NNodes
                IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                    rNodalStor(indxNode,indxLayer) = 0.0
                    CYCLE
                END IF
                
                IF (pHead(indxNode,indxLayer) .LT. pTopElev(indxNode,indxLayer)) THEN
                    rNodalStor(indxNode,indxLayer) = (pHead(indxNode,indxLayer)-pBottomElev(indxNode,indxLayer)) * pSy(indxNode,indxLayer) 
                ELSE
                    rNodalStor(indxNode,indxLayer) = (pHead(indxNode,indxLayer)-pTopElev(indxNode,indxLayer)) * pSs(indxNode,indxLayer)      &
                                                   + (pTopElev(indxNode,indxLayer)-pBottomElev(indxNode,indxLayer)) * pSy(indxNode,indxLayer)
                END IF          
            END DO
        END DO
        !$OMP END DO NOWAIT
        
        !Compute derivative of storage, if requested
        IF (PRESENT(rdNodalStor)) THEN
            !$OMP DO COLLAPSE(2)
            DO indxLayer=1,Stratigraphy%NLayers
                DO indxNode=1,AppGrid%NNodes
                    IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                        rdNodalStor(indxNode,indxLayer) = 0.0
                        CYCLE
                    END IF
                    
                    IF (pHead(indxNode,indxLayer) .LT. pTopElev(indxNode,indxLayer)) THEN
                        rdNodalStor(indxNode,indxLayer) = pSy(indxNode,indxLayer) 
                    ELSE
                        rdNodalStor(indxNode,indxLayer) = pSs(indxNode,indxLayer)      
                    END IF          
                END DO
            END DO
            !$OMP END DO 
        END IF
        !$OMP END PARALLEL 
    END ASSOCIATE
    
  END SUBROUTINE GetNodalStorages
  

  ! -------------------------------------------------------------
  ! --- GET AQUIFER PARAMETERS WITHOUT AppGW OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferParameters_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rKh,rKv,rAquitardKv,rSs,rSy,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(OUT)               :: rKh(:,:),rKv(:,:),rAquitardKv(:,:),rSs(:,:),rSy(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER                  :: iDummy,indx,iNSkip,iNNodes,iNLayers,iGWNodeIDs(AppGrid%NNodes),indxNode,indxLayer
    REAL(8)                  :: rDummy,rHeads(AppGrid%NNodes,Stratigraphy%NLayers),rFactor
    CHARACTER                :: cALine*3000,cOverwriteFileName*1200,cSubsidenceFileName*1200,cVarTimeUnit*6,cBCFileName*1200, &
                                cUnitFlow*10
    LOGICAL                  :: lTecPlotFlag_Defined,lSubsidence_Defined,lIsForInquiry
    TYPE(GenericFileType)    :: AppGWParamFile
    TYPE(GWNodeType)         :: Nodes
    TYPE(GWStateType)        :: GWState
    TYPE(AppSubsidenceType)  :: AppSubsidence
    TYPE(AppBCType)          :: AppBC                                   
    CHARACTER,ALLOCATABLE    :: cCountLines(:)*50
    CHARACTER(:),ALLOCATABLE :: cAbsPathFileName
    COMPLEX                  :: StrmConnectivity(0)
    
    !Initialize
    iStat         = 0
    iNNodes       = AppGrid%NNodes
    iNLayers      = Stratigraphy%NLayers
    iGWNodeIDs    = AppGrid%AppNode%ID
    lIsForInquiry = .TRUE.
    
    !Allocate parameter arrays
    ALLOCATE (Nodes%Kh(iNNodes,iNLayers)         , &
              Nodes%Kv(iNNodes,iNLayers)         , &
              Nodes%AquitardKv(iNNodes,iNLayers) , &
              Nodes%LeakageV(iNNodes,iNLayers)   , &
              Nodes%Ss(iNNodes,iNLayers)         , &
              Nodes%Sy(iNNodes,iNLayers)         )
    Nodes%Kh         = 0.0
    Nodes%Kv         = 0.0
    Nodes%AquitardKv = 0.0
    Nodes%LeakageV   = 0.0
    Nodes%Ss         = 0.0
    Nodes%Sy         = 0.0
    
    !Instantiate State data
    CALL GWState%New(iNNodes,iNLayers,iStat)  
    IF (iStat .NE. 0) GOTO 10

    !Open file
    CALL AppGWParamFile%New(FileName=cGWFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='groundwater data main input',iStat=iStat)
    IF (iStat .EQ. -1) GOTO 10
    
    !Read away the first version number line to avoid any errors
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !BACKWARD COMPATIBILITY: Check if there are 19 or 20 lines of entry to see if flag (dictating gw heads or 
    !                        depth-to-groundwater will be printed to TecPlot output) is provided
    CALL AppGWParamFile%ReadData(cCountLines,iStat)  ;  IF (iStat .NE. 0) GOTO 10
    IF (SIZE(cCountLines) .EQ. 19) THEN
        lTecPlotFlag_Defined = .FALSE.
    ELSE
        lTecPlotFlag_Defined = .TRUE.
    END IF
    CALL AppGWParamFile%RewindFile()
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Boundary conditions data filename
    CALL AppGWParamFile%ReadData(cBCFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cBCFileName = StripTextUntilCharacter(cBCFileName,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cBCFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cBCFileName)),cWorkingDirectory,cAbsPathFileName)
    cBCFileName = cAbsPathFileName
    
    !Read away input not-needed lines
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Tile drains/subsurface irrigation
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Pumping
    
    !Subsidence filename
    CALL AppGWParamFile%ReadData(cSubsidenceFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cSubsidenceFileName = StripTextUntilCharacter(cSubsidenceFileName,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cSubsidenceFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cSubsidenceFileName)),cWorkingDirectory,cAbsPathFileName)
    cSubsidenceFileName = cAbsPathFileName

    !Parameter over-write file name
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    cOverwriteFileName = cAbsPathFileName

    !Read away not-needed lines
    CALL AppGWParamFile%ReadData(rDummy,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Head conversion factor
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Head unit
    CALL AppGWParamFile%ReadData(rDummy,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Flow conversion factor
    
    !Read flow unit after conversion
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(cALine) ; cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)
    cUnitFlow = ADJUSTL(TRIM(cALine))

    !Read away non-needed lines
    CALL AppGWParamFile%ReadData(rDummy,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Velocity conversion factor
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Velocity unit    
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Output file for velocities at cell centroids
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Vertical flow output file
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Output file for heads at all nodes and layers
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Head Tecplot output file     
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Velocity Tecplot output file
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Groundwater budget output
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Z-Budget binary output filename
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Final results output file    
    IF (lTecPlotFlag_Defined)   &                                              !TecPlot flag, if provided, to print gw heads or depth-to-groundwater 
        CALL AppGWParamFile%ReadData(iDummy,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    CALL AppGWParamFile%ReadData(iDummy,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  !Parameters print option
    
    !Read away groundwater hydrograph output data
    CALL AppGWParamFile%ReadData(iNSkip,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    CALL AppGWParamFile%ReadData(rDummy,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    DO indx=1,iNSkip
        CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    END DO
    
    !Read away element face flow output data
    CALL AppGWParamFile%ReadData(iNSkip,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    DO indx=1,iNSkip
        CALL AppGWParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) GOTO 10  
    END DO

    !Aquifer parameters
    CALL ReadAquiferParameters(iNLayers,AppGrid,TimeStep,AppGWParamFile,cVarTimeUnit,Nodes,iStat)
    IF (iStat .EQ. -1) GOTO 10
    
    !Initial conditions
    CALL ReadInitialHeads(AppGWParamFile,iNNodes,iGWNodeIDs,Stratigraphy,rHeads,iStat)
    IF (iStat .EQ. -1) GOTO 10
    GWState%Head = rHeads

    !Instantiate the boundary conditions data and overwrite the initial conditions if necessary
    CALL AppBC%New(lIsForInquiry,cBCFileName,cWorkingDirectory,AppGrid,Stratigraphy,iGWNodeIDs,cUnitFlow,TimeStep,GWState%Head,iStat)
    IF (iStat .EQ. -1) GOTO 10

    !Assign previous head as current head
    GWState%Head_P = GWState%Head
    
    !Instantiate subsidence; this has to be done after AppGW initial conditions are processed
    CALL AppSubsidence%New(lIsForInquiry,cSubsidenceFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat)
    IF (iStat .EQ. -1) GOTO 10
    lSubsidence_Defined = AppSubsidence%IsDefined()

    !Aquifer overwrite parameters
    IF (cOverwriteFileName .NE. '') THEN
        CALL OverwriteParameters(cOverwriteFileName,AppGrid,cVarTimeUnit,TimeStep%TrackTime,lSubsidence_Defined,Nodes,AppSubsidence,iStat)
        IF (iStat .EQ. -1) GOTO 10
    END IF
    
    !Process aquifer parameters for use in simulation
    CALL ProcessAquiferParameters(AppGrid,Stratigraphy,lSubsidence_Defined,AppSubsidence,Nodes,GWState,iStat)
    IF (iStat .EQ. -1) GOTO 10

    !Return variables
    rKh         = Nodes%Kh        
    rKv         = Nodes%Kv        
    rAquitardKv = Nodes%AquitardKv
    rSs         = Nodes%Ss        
    rSy         = Nodes%Sy   
    
    !Convert time unit of aquifer parameters
    rFactor    = TimeIntervalConversion(TimeStep%Unit,cVarTimeUnit)
    rKh         = rKh         * rFactor
    rKv         = rKv         * rFactor
    rAquitardKv = rAquitardKv * rFactor

    !Convert Ss and Sy to unitless values
    DO indxLayer=1,iNLayers
        DO indxNode=1,iNNodes
            rSs(indxNode,indxLayer) = rSs(indxNode,indxLayer) / AppGrid%AppNode(indxNode)%Area
            rSy(indxNode,indxLayer) = rSy(indxNode,indxLayer) / AppGrid%AppNode(indxNode)%Area
        END DO
    END DO
    
10  IF (iStat .NE. 0) THEN
        rKh         = -999.9
        rKv         = -999.9
        rAquitardKv = -999.9
        rSs         = -999.9
        rSy         = -999.9
    END IF
    
    CALL AppGWParamFile%Kill()
    CALL AppSubsidence%Kill()
    CALL AppBC%Kill()
    CALL GWState%Kill()
    CALL Nodes%Kill()
    
  END SUBROUTINE GetAquiferParameters_FromFile
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER HORIZONTAL HYDRAULIC CONDUCTIVITY WITHOUT AppGW OBJECT 
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferKh_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rAquiferKh,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(OUT)               :: rAquiferKh(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    REAL(8) :: rDummy(SIZE(rAquiferKh,DIM=1),SIZE(rAquiferKh,DIM=2))
    
    CALL GetAquiferParameters_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rAquiferKh,rDummy,rDummy,rDummy,rDummy,iStat)
    
  END SUBROUTINE GetAquiferKh_FromFile
   
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER VERTICAL HYDRAULIC CONDUCTIVITY WITHOUT AppGW OBJECT 
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferKv_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rAquiferKv,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(OUT)               :: rAquiferKv(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    REAL(8) :: rDummy(SIZE(rAquiferKv,DIM=1),SIZE(rAquiferKv,DIM=2))
    
    CALL GetAquiferParameters_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rDummy,rAquiferKv,rDummy,rDummy,rDummy,iStat)
    
  END SUBROUTINE GetAquiferKv_FromFile
   
  
  ! -------------------------------------------------------------
  ! --- GET AQUITARD VERTICAL HYDRUALIC CONDUCTIVITY WITHOUT AppGW OBJECT 
  ! -------------------------------------------------------------
  SUBROUTINE GetAquitardKv_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rAquitardKv,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(OUT)               :: rAquitardKv(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    REAL(8) :: rDummy(SIZE(rAquitardKv,DIM=1),SIZE(rAquitardKv,DIM=2))
    
    CALL GetAquiferParameters_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rDummy,rDummy,rAquitardKv,rDummy,rDummy,iStat)
    
  END SUBROUTINE GetAquitardKv_FromFile
   
  
  ! -------------------------------------------------------------
  ! --- GET SPECIFIC YIELD WITHOUT AppGW OBJECT 
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferSy_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rSy,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(OUT)               :: rSy(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    REAL(8) :: rDummy(SIZE(rSy,DIM=1),SIZE(rSy,DIM=2))
    
    CALL GetAquiferParameters_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rDummy,rDummy,rDummy,rDummy,rSy,iStat)
    
  END SUBROUTINE GetAquiferSy_FromFile
   
  
  ! -------------------------------------------------------------
  ! --- GET STORAGE COEFFICIENT WITHOUT AppGW OBJECT (AFTER SPECIFIC STORAGE IS MULTIPLIED BY AQUIFER THICKNESS)
  ! -------------------------------------------------------------
  SUBROUTINE GetAquiferSs_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rSs,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cGWFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(OUT)               :: rSs(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    REAL(8) :: rDummy(SIZE(rSs,DIM=1),SIZE(rSs,DIM=2))
    
    CALL GetAquiferParameters_FromFile(cGWFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,rDummy,rDummy,rDummy,rSs,rDummy,iStat)
    
  END SUBROUTINE GetAquiferSs_FromFile
   
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER HORIZONTAL HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferKh(AppGW,Kh)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Kh(:,:)
    
    Kh = AppGW%Nodes%Kh
    
  END SUBROUTINE GetAquiferKh
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER VERTICAL HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferKv(AppGW,Kv)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Kv(:,:)
    
    Kv = AppGW%Nodes%Kv
    
  END SUBROUTINE GetAquiferKv
  
  
  ! -------------------------------------------------------------
  ! --- GET AQUITARD VERTICAL HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquitardKv(AppGW,Kv)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Kv(:,:)
    
    Kv = AppGW%Nodes%AquitardKv
    
  END SUBROUTINE GetAquitardKv
  
  
  ! -------------------------------------------------------------
  ! --- GET SPECIFIC YIELD
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferSy(AppGW,AppGrid,Sy)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: Sy(:,:)
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    DO indxLayer=1,SIZE(Sy,DIM=2)
        DO indxNode=1,AppGrid%NNodes
            Sy(indxNode,indxLayer) = AppGW%Nodes%Sy(indxNode,indxLayer) / AppGrid%AppNode(indxNode)%Area
        END DO
    END DO
    
  END SUBROUTINE GetAquiferSy
  
  
  ! -------------------------------------------------------------
  ! --- GET STORAGE COEFFICIENT (AFTER SPECIFIC STORAGE IS MULTIPLIED BY AQUIFER THICKNESS)
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetAquiferSs(AppGW,AppGrid,Ss)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: Ss(:,:)
    
    !Local variables
    INTEGER :: indxNode,indxLayer
    
    DO indxLayer=1,SIZE(Ss,DIM=2)
        DO indxNode=1,AppGrid%NNodes
            Ss(indxNode,indxLayer) = AppGW%Nodes%Ss(indxNode,indxLayer) / AppGrid%AppNode(indxNode)%Area
        END DO
    END DO
    
  END SUBROUTINE GetAquiferSs
   
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF GW HEAD OR SUBSIDENCE HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(AppGW,iLocationType) RESULT(NHydrographs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER                     :: NHydrographs
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            NHydrographs = AppGW%GWHyd%GetNGWHeadHydrographs()
        
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) THEN
                NHydrographs = AppGW%AppSubsidence%GetNHydrographs()
            ELSE
                NHydrographs = 0
            END IF
            
        CASE (f_iLocationType_TileDrainObs)
            NHydrographs = AppGW%AppTileDrain%GetNHydrographs(f_iTileDrain)
    END SELECT
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(AppGW,iLocationType,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHeadHydrographIDs(IDs)
        
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%GetHydrographIDs(IDs)
            
        CASE (f_iLocationType_TileDrainObs)
            IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%GetHydrographIDs(f_iTileDrain,IDs)
    END SELECT
    
  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD, SUBSIDENCE OR TILE DRAIN HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(AppGW,iLocationType,GridX,GridY,XHyd,YHyd)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationType
    REAL(8),INTENT(IN)          :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)         :: XHyd(:),YHyd(:)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHeadHydrographCoordinates(GridX,GridY,XHyd,YHyd)
        
        CASE (f_iLocationType_SubsidenceObs)
            IF (AppGW%lSubsidence_Defined) THEN
                CALL AppGW%AppSubsidence%GetHydrographCoordinates(GridX,GridY,XHyd,YHyd)
            END IF
            
        CASE (f_iLocationType_TileDrainObs)
            IF (AppGW%lTileDrain_Defined) THEN
                CALL AppGW%AppTileDrain%GetHydrographCoordinates(f_iTileDrain,GridX,GridY,XHyd,YHyd)
            END IF
    END SELECT
    
  END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET FEATURE NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetNames(AppGW,iLocationType,cNamesList)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: iLocationType
    CHARACTER(LEN=*),INTENT(OUT) :: cNamesList(:) !Assumes array was dimensioned previously based on the number of hydrographs
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHeadHydrographNames(cNamesList)
        
        CASE (f_iLocationType_SubsidenceObs)
            CALL AppGW%AppSubsidence%GetHydrographNames(cNamesList)
            
        CASE (f_iLocationType_TileDrainObs)
            CALL AppGW%AppTileDrain%GetHydrographNames(cNamesList)
            
    END SELECT
    
  END SUBROUTINE GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD, SUBSIDENCE OR TILE DRAIN HYDROGRAPH FOR A GIVEN HYDROGRAPH INDEX FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrograph_GivenAppGW(AppGW,TimeStep,iNNodes,iLocationType,iLocationIndex,iLayer,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cInterval
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: iLocationType,iLocationIndex,iLayer,iNNodes
    REAL(8),INTENT(IN)              :: rFact_LT,rFact_VL
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rDates(:),rValues(:)
    INTEGER,INTENT(OUT)             :: iDataUnitType,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModNAme // 'GetHydrograph_GivenAppGW'
    CHARACTER(:),ALLOCATABLE     :: cFileName
    
    !Get filename
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Node)
            CALL AppGW%GWHyd%GetAllHeadOutputFileName(cFileName)
        CASE (f_iLocationType_GWHeadObs)
            CALL AppGW%GWHyd%GetGWHydOutputFileName(cFileName)
        CASE (f_iLocationType_SubsidenceObs)
            CALL AppGW%AppSubsidence%GetHydOutputFileNAme(cFileName)
        CASE (f_iLocationType_TileDrainObs)
            CALL AppGW%AppTileDrain%GetHydOutputFileName(cFileName)
        END SELECT
        
    !Return with error if filename is empty
    IF (LEN_TRIM(cFileName) .EQ. 0) THEN
        CALL SetLastMessage('Requested hydrograph data is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
        
    !Replace filename extension with hdf
    cFileName = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf' 
    
    !Retrieve data
    CALL GetHydrograph_GivenFile(cFileName,TimeStep,iNNodes,iLocationType,iLocationIndex,iLayer,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)
        
  END SUBROUTINE GetHydrograph_GivenAppGW
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD, SUBSIDENCE OR TILE DRAIN HYDROGRAPH FOR A GIVEN HYDROGRAPH INDEX FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrograph_GivenFile(cFileName,TimeStep,iNNodes,iLocationType,iLocationIndex,iLayer,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName,cBeginDate,cEndDate,cInterval
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: iLocationType,iLocationIndex,iLayer,iNNodes
    REAL(8),INTENT(IN)              :: rFact_LT,rFact_VL
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rDates(:),rValues(:)
    INTEGER,INTENT(OUT)             :: iDataUnitType,iStat
    
    !Local variables
    INTEGER               :: iColNo,iFileReadCode,iNData,iDeltaT_InMinutes,iNDataReturn,iNIntervals,iErrorCode
    REAL(8)               :: rFactor,rDummy
    CHARACTER             :: cDate*f_iTimeStampLength
    TYPE(GenericFileType) :: InFile
    REAL(8),ALLOCATABLE   :: rValues_Local(:),rDates_Local(:)
    
    !Set data retrival parameters based on location type
    SELECT CASE (iLocationType)          
        CASE (f_iLocationType_Node)
            iColNo           = (iLayer - 1) * iNNodes + iLocationIndex 
            iDataUnitType    = f_iDataUnitType_Length
            rFactor          = rFact_LT
            
        CASE (f_iLocationType_GWHeadObs , f_iLocationType_SubsidenceObs) 
            iColNo           = iLocationIndex
            iDataUnitType    = f_iDataUnitType_Length
            rFactor          = rFact_LT
            
        CASE (f_iLocationType_TileDrainObs)
            iColNo           = iLocationIndex
            iDataUnitType    = f_iDataUnitType_Volume
            rFactor          = rFact_VL                        
    END SELECT
        
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .NE. 0) GOTO 10
    
    !Number of timesteps for which data will be read, and allocate array to read data
    iNData = NPeriods(TimeStep%DELTAT_InMinutes,cBeginDate,cEndDate) + 1 
    ALLOCATE (rValues_Local(iNData) , rDates_Local(iNData))
    
    !Julian dates for data
    CALL GetJulianDatesBetweenTimeStampsWithTimeIncrement(TimeStep%DELTAT_InMinutes,cBeginDate,cEndDate,rDates_Local)
    
    !Read data
    CALL InFile%ReadData(cBeginDate,1,iColNo,rValues_Local,iFileReadCode,iStat)
    IF (iStat .NE. 0) GOTO 10
    
    !Number of timesteps to sample data
    CALL CTimeStep_To_RTimeStep(cInterval,rDummy,iDeltaT_InMinutes,iStat)  
    IF (iStat .NE. 0) GOTO 10
    iNDataReturn = NPeriods(iDELTAT_InMinutes,cBeginDate,cEndDate) + 1
    ALLOCATE (rDates(iNDataReturn) , rValues(iNDataReturn))
    
    !Sample read data into return argument
    IF (iNData .EQ. iNDataReturn) THEN
        rValues = rValues_Local * rFactor
        rDates  = rDates_Local
    ELSE
        cDate       = IncrementTimeStamp(cBeginDate,iDeltaT_InMinutes,1)
        iNIntervals = NPeriods(TimeStep%DELTAT_InMinutes,cBeginDate,cDate)
        rValues     = rValues_Local(iNIntervals::iNIntervals) * rFactor
        rDates      = rDates_Local(iNIntervals::iNIntervals)
    END IF
    
    !Close file
10  CALL InFile%Kill()
    
    !Clear memory
    DEALLOCATE (rValues_Local , rDates_Local , STAT=iErrorCode)
    
  END SUBROUTINE GetHydrograph_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF HYDROGRAPH TYPES AVAILABLE FOR RETRIEVAL
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographTypeList(AppGW,cHydTypeList,iHydLocationTypeList,cHydFileList)
    CLASS(AppGWType),INTENT(IN)              :: AppGW
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cHydTypeList(:),cHydFileList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iHydLocationTypeList(:)
    
    !Local variables
    INTEGER                  :: iErrorCode,iHydLocationTypeList_Local(5),iCount
    CHARACTER(LEN=100)       :: cHydTypeList_Local(5)
    CHARACTER(LEN=500)       :: cHydFileList_Local(5)
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    iCount = 0
    
    !GW head observations
    CALL AppGW%GWHyd%GetGWHydOutputFileName(cFileName)
    IF (ALLOCATED(cFileName)) THEN
        iCount                             = iCount + 1
        iHydLocationTypeList_Local(iCount) = f_iLocationType_GWHeadObs
        cHydTypeList_Local(iCount)         = f_cDescription_GWHyd_AtWell
        cHydFileList_Local(iCount)         = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have been copied into an HDF file
    END IF
    
    !GW heads at all nodes and layers
    CALL AppGW%GWHyd%GetAllHeadOutputFileName(cFileName)
    IF (ALLOCATED(cFileName)) THEN
        iCount                             = iCount + 1
        iHydLocationTypeList_Local(iCount) = f_iLocationType_Node
        cHydTypeList_Local(iCount)         = f_cDescription_GWHyd_AtNodeLayer
        cHydFileList_Local(iCount)         = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have been copied into an HDF file
    END IF
    
    !Subsidence observations
    CALL AppGW%AppSubsidence%GetHydOutputFileName(cFileName)
    IF (ALLOCATED(cFileName)) THEN
        iCount                             = iCount + 1
        iHydLocationTypeList_Local(iCount) = f_iLocationType_SubsidenceObs
        cHydTypeList_Local(iCount)         = f_cDescription_SubsHyd
        cHydFileList_Local(iCount)         = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have been copied into an HDF file
    END IF

    !Tile drain observations
    CALL AppGW%AppTileDrain%GetHydOutputFileName(cFileName)
    IF (ALLOCATED(cFileName)) THEN
        iCount                             = iCount + 1
        iHydLocationTypeList_Local(iCount) = f_iLocationType_TileDrainObs
        cHydTypeList_Local(iCount)         = f_cDescription_TDHyd
        cHydFileList_Local(iCount)         = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have been copied into an HDF file
    END IF
    
    !Store data in return variables
    DEALLOCATE (iHydLocationTypeList , cHydTypeList , cHydFileList , STAT=iErrorCode)
    ALLOCATE (iHydLocationTypeList(iCount) , cHydTypeList(iCount) , cHydFileList(iCount)) 
    iHydLocationTypeList = iHydLocationTypeList_Local(1:iCount)
    cHydTypeList         = cHydTypeList_Local(1:iCount)        
    cHydFileList         = cHydFileList_Local(1:iCount)           
        
  END SUBROUTINE GetHydrographTypeList
  
  
  ! -------------------------------------------------------------
  ! --- GET AVERAGE AG. PUMPING-WEIGHTED DEPTH-TO-GW FOR DEFINED ZONES
  ! -------------------------------------------------------------
  SUBROUTINE GetZoneAgPumpingAverageDepthToGW(AppGW,AppGrid,Stratigraphy,iElems,iElemZones,iZoneList,rElemAgAreas,rZoneAgAreas,rAveDepthToGW)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iElems(:),iElemZones(:),iZoneList(:)
    REAL(8),INTENT(IN)                :: rElemAgAreas(:),rZoneAgAreas(:)
    REAL(8),INTENT(OUT)               :: rAveDepthToGW(:)
    
    !Local variables
    INTEGER :: indxLayer,indxNode,iVertex(4),iNVertex,iNode,indxPump,iElem,indxZone,    &
               iNLayers,iLoc,iZone
    REAL(8) :: rDepthToGW_Node(AppGrid%NNodes,Stratigraphy%NLayers),rPumpSum(SIZE(iZoneList)), &
               rPump,rPumps(4,Stratigraphy%NLayers),rTotalPump
    
    !Initialize
    iNLayers      = Stratigraphy%NLayers
    rPumpSum      = 0.0
    rAveDepthToGW = 0.0
    
    !Calculate depth-to-gw at each node
    DO indxLayer=1,iNLayers
        DO indxNode=1,AppGrid%NNodes
            rDepthToGW_Node(indxNode,indxLayer) = MAX(0.0 , Stratigraphy%GSElev(indxNode)-MAX(AppGW%State%Head(indxNode,indxLayer),Stratigraphy%BottomElev(indxNode,indxLayer)))
        END DO
    END DO
    
    !Process element pumps
    DO indxPump=1,AppGW%AppPumping%GetNElemPumps()
        iElem = AppGW%AppPumping%GetElement(indxPump,f_iPump_ElemPump) 
        
        !Cycle if element is not listed as part of the zones
        iLoc = LocateInList(iElem,iElems)
        IF (iLoc .LT. 1) CYCLE
        
        !Cycle if element has no ag land
        IF (rElemAgAreas(iLoc) .EQ. 0.0) CYCLE
        
        !Zone number and its index in return average-pumping array
        iZone    = iElemZones(iLoc)
        indxZone = LocateInList(iZone,iZoneList)
        
        !Cycle if no ag land in zone
        IF (rZoneAgAreas(indxZone) .EQ. 0.0) CYCLE
        
        !Nodal data
        iVertex  = AppGrid%Vertex(:,iElem)
        iNVertex = AppGrid%NVertex(iElem)
        CALL AppGW%AppPumping%GetActualNodeLayerPump_ForAPump(indxPump,f_iPump_ElemPump,rPumps(1:iNVertex,:))
        rTotalPump = SUM(rPumps(1:iNVertex,:))
        IF (rTotalPump .EQ. 0.0) THEN
            rPumps = 0.0
        ELSE
            rPumps(1:iNVertex,:) = rPumps(1:iNVertex,:) / rTotalPump
        END IF
        
        !Cycle through layers and nodes
        DO indxLayer=1,iNLayers
            DO indxNode=1,iNVertex
                iNode                   = iVertex(indxNode)
                rPump                   = rPumps(indxNode,indxLayer)
                rPumpSum(indxZone)      = rPumpSum(indxZone) + rPump
                rAveDepthToGW(indxZone) = rAveDepthToGW(indxZone) + rDepthToGW_Node(iNode,indxLayer) * rPump
            END DO
        END DO
    END DO
    
    !Process wells
    DO indxPump=1,AppGW%AppPumping%GetNWells()
        iElem = AppGW%AppPumping%GetElement(indxPump,f_iPump_Well) 

        !Cycle if element is not listed as part of the zones
        iLoc = LocateInList(iElem,iElems)
        IF (iLoc .LT. 1) CYCLE
        
        !Cycle if element has no ag land
        IF (rElemAgAreas(iLoc) .EQ. 0.0) CYCLE
        
        !Zone number and its index in return average-pumping array
        iZone    = iElemZones(iLoc)
        indxZone = LocateInList(iZone,iZoneList)
        
        !Cycle if no ag land in zone
        IF (rZoneAgAreas(indxZone) .EQ. 0.0) CYCLE
        
        !Nodal data
        iVertex  = AppGrid%Vertex(:,iElem)
        iNVertex = AppGrid%NVertex(iElem)
        CALL AppGW%AppPumping%GetActualNodeLayerPump_ForAPump(indxPump,f_iPump_Well,rPumps(1:iNVertex,:))
        rTotalPump = SUM(rPumps(1:iNVertex,:))
        IF (rTotalPump .EQ. 0.0) THEN
            rPumps = 0.0
        ELSE
            rPumps(1:iNVertex,:) = rPumps(1:iNVertex,:) / rTotalPump
        END IF
        
        !Cycle through layers and nodes
        DO indxLayer=1,iNLayers
            DO indxNode=1,iNVertex
                iNode                   = iVertex(indxNode)
                rPump                   = rPumps(indxNode,indxLayer)
                rPumpSum(indxZone)      = rPumpSum(indxZone) + rPump
                rAveDepthToGW(indxZone) = rAveDepthToGW(indxZone) + rDepthToGW_Node(iNode,indxLayer) * rPump
            END DO
        END DO
    END DO
        
    !Calculate weighted average
    DO indxZone=1,SIZE(iZoneList)
        IF (rPumpSum(indxZone) .NE. 0.0) THEN
            rAveDepthToGW(indxZone) = rAveDepthToGW(indxZone) / rPumpSum(indxZone)
        ELSE
            rAveDepthToGW(indxZone) = -999.0
        END IF
    END DO
    
  END SUBROUTINE GetZoneAgPumpingAverageDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET AVERAGE AG. PUMPING-WEIGHTED DEPTH-TO-GW
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionAgPumpingAverageDepthToGW(AppGW,AppGrid,Stratigraphy,ElemAgAreas,RegionAgAreas,AveDepthToGW)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: ElemAgAreas(:),RegionAgAreas(:)
    REAL(8),INTENT(OUT)               :: AveDepthToGW(:)
    
    !Local variables
    INTEGER :: indxLayer,indxNode,iVertex(4),iRegion,NVertex,iNode,indxRegion,indxPump,iElem,    &
               NLayers
    REAL(8) :: DepthToGW_Node(AppGrid%NNodes,Stratigraphy%NLayers),PumpSum(AppGrid%NSubregions), &
               Pump,rPumps(4,Stratigraphy%NLayers),rTotalPump
    
    !Initialize
    NLayers      = Stratigraphy%NLayers
    PumpSum      = 0.0
    AveDepthToGW = 0.0
    
    !Calculate depth-to-gw at each node
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxNode=1,AppGrid%NNodes
            DepthToGW_Node(indxNode,indxLayer) = MAX(0.0 , Stratigraphy%GSElev(indxNode)-MAX(AppGW%State%Head(indxNode,indxLayer),Stratigraphy%BottomElev(indxNode,indxLayer)))
        END DO
    END DO
    
    !Process element pumps
    DO indxPump=1,AppGW%AppPumping%GetNElemPumps()
        iElem   = AppGW%AppPumping%GetElement(indxPump,f_iPump_ElemPump) 
        iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Cycle if no ag land in subregion
        IF (RegionAgAreas(iRegion) .EQ. 0.0) CYCLE
        
        !Cycle if element has no ag land
        IF (ElemAgAreas(iElem) .EQ. 0.0) CYCLE
        
        !Nodal data
        iVertex = AppGrid%Vertex(:,iElem)
        NVertex = AppGrid%NVertex(iElem)
        CALL AppGW%AppPumping%GetActualNodeLayerPump_ForAPump(indxPump,f_iPump_ElemPump,rPumps(1:NVertex,:))
        rTotalPump = SUM(rPumps(1:NVertex,:))
        IF (rTotalPump .EQ. 0.0) THEN
            rPumps = 0.0
        ELSE
            rPumps(1:NVertex,:) = rPumps(1:NVertex,:) / rTotalPump
        END IF
        
        !Cycle through layers and nodes
        DO indxLayer=1,NLayers
            DO indxNode=1,NVertex
                iNode                 = iVertex(indxNode)
                Pump                  = rPumps(indxNode,indxLayer)
                PumpSum(iRegion)      = PumpSum(iRegion) + Pump
                AveDepthToGW(iRegion) = AveDepthToGW(iRegion) + DepthToGW_Node(iNode,indxLayer) * Pump
            END DO
        END DO
    END DO
    
    !Process wells
    DO indxPump=1,AppGW%AppPumping%GetNWells()
        iElem   = AppGW%AppPumping%GetElement(indxPump,f_iPump_Well) 
        iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Cycle if no ag land in subregion
        IF (RegionAgAreas(iRegion) .EQ. 0.0) CYCLE
        
        !Cycle if element has no ag land
        IF (ElemAgAreas(iElem) .EQ. 0.0) CYCLE
        
        !Nodal data
        iVertex = AppGrid%Vertex(:,iElem)
        NVertex = AppGrid%NVertex(iElem)
        CALL AppGW%AppPumping%GetActualNodeLayerPump_ForAPump(indxPump,f_iPump_Well,rPumps(1:NVertex,:))
        rTotalPump = SUM(rPumps(1:NVertex,:))
        IF (rTotalPump .EQ. 0.0) THEN
            rPumps = 0.0
        ELSE
            rPumps(1:NVertex,:) = rPumps(1:NVertex,:) / rTotalPump
        END IF
        
        !Cycle through layers and nodes
        DO indxLayer=1,NLayers
            DO indxNode=1,NVertex
                iNode                 = iVertex(indxNode)
                Pump                  = rPumps(indxNode,indxLayer)
                PumpSum(iRegion)      = PumpSum(iRegion) + Pump
                AveDepthToGW(iRegion) = AveDepthToGW(iRegion) + DepthToGW_Node(iNode,indxLayer) * Pump
            END DO
        END DO
    END DO
        
    !Calculate weighted average
    DO indxRegion=1,AppGrid%NSubregions
        IF (PumpSum(indxRegion) .NE. 0.0) THEN
            AveDepthToGW(indxRegion) = AveDepthToGW(indxRegion) / PumpSum(indxRegion)
        ELSE
            AveDepthToGW(indxRegion) = -999.0
        END IF
    END DO
    
  END SUBROUTINE GetSubregionAgPumpingAverageDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET PUMP DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpDestination(AppGW,iPumpType,Destination)
    CLASS(AppGWType),INTENT(IN)           :: AppGW
    INTEGER,INTENT(IN)                    :: iPumpType
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    CALL AppGW%AppPumping%GetPumpDestination(iPumpType , Destination)
            
  END SUBROUTINE GetPumpDestination
  
  
  ! -------------------------------------------------------------
  ! --- GET WELLS AS SUPPLY DATA
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSupplySpecs(AppGW,WellSpecs,ElemPumpSpecs)
    CLASS(AppGWType),INTENT(IN)              :: AppGW
    TYPE(SupplyType),ALLOCATABLE,INTENT(OUT) :: WellSpecs(:),ElemPumpSpecs(:)
    
    !Return if pumping is not defined
    IF (.NOT. AppGW%lPumping_Defined) THEN
        ALLOCATE(WellSpecs(0) ,  ElemPumpSpecs(0))
        RETURN
    END IF
    
    CALL AppGW%AppPumping%GetSupplySpecs(f_iPump_Well,WellSpecs)
    CALL AppGW%AppPumping%GetSupplySpecs(f_iPump_ElemPump,ElemPumpSpecs)
    
  END SUBROUTINE GetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET WEIGTHED-AVERAGE SPECIFIC YIELD AT A SPECIFIED LAYER AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementSy(AppGW,AppGrid,Stratigraphy,iLayer,Sy)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iLayer
    REAL(8),INTENT(OUT)               :: Sy(:)
    
    !Local variables
    INTEGER :: iLayerLocal,indxNode
    REAL(8) :: SyNode(AppGrid%NNodes)
    
    !Initialize
    DO indxNode=1,AppGrid%NNodes
        IF (Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            iLayerLocal = iLayer
        ELSE 
            iLayerLocal = Stratigraphy%GetActiveLayerBelow(indxNode,iLayer)
            IF (iLayerLocal .LE. 0) THEN
                iLayerLocal = Stratigraphy%GetActiveLayerAbove(indxNode,iLayer)
                IF (iLayerLocal .LE. 0) THEN
                    SyNode(indxNode) = 0.0
                    CYCLE
                END IF
            END IF
        END IF
        SyNode(indxNode) = AppGW%Nodes%Sy(indxNode,iLayerLocal) / AppGrid%AppNode(indxNode)%Area
    END DO
    
    !Specific yield at each element
    CALL AppGrid%AreaAverage_ElemData_From_NodeData(SyNode,Sy)
    
  END SUBROUTINE GetElementSy
  
  
  ! -------------------------------------------------------------
  ! --- GET DEPTH-TO-GW AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementDepthToGW(AppGW,AppGrid,Stratigraphy,lPrevious,DepthToGW)
    CLASS(AppGWType),TARGET,INTENT(IN) :: AppGW
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)  :: Stratigraphy
    LOGICAL,INTENT(IN)                 :: lPrevious
    REAL(8),INTENT(OUT)                :: DepthToGW(:)
    
    !Local variables
    INTEGER         :: indxNode
    REAL(8)         :: NodeDepthToGW(AppGrid%NNodes)
    REAL(8),POINTER :: pGWHead(:,:)
    
    !Initialize
    IF (lPrevious) THEN
        pGWHead => AppGW%State%Head_P
    ELSE
        pGWHead => AppGW%State%Head
    END IF
        
    !Depth to the groundwater at each element
    DO indxNode=1,AppGrid%NNodes
        NodeDepthToGW(indxNode) = MAX(0.0  ,  Stratigraphy%GSElev(indxNode)-pGWHead(indxNode,Stratigraphy%TopActiveLayer(indxNode)))
    END DO
    CALL AppGrid%AreaAverage_ElemData_From_NodeData(NodeDepthToGW,DepthToGW)

  END SUBROUTINE GetElementDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE INTEBED THICKNESS AT (node,layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidenceInterbedThick_All(AppGW,lPrevious,rInterbedThick)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8),INTENT(OUT)         :: rInterbedThick(:,:)
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%GetInterbedThickAll(lPrevious,rInterbedThick)
    ELSE
        rInterbedThick = 0.0
    END IF
    
  END SUBROUTINE GetSubsidenceInterbedThick_All
    

  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT (node,layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidence_All(AppGW,Subs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: Subs(:,:)
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%GetSubsidence_All(Subs)
    ELSE
        Subs = 0.0
    END IF
    
  END SUBROUTINE GetSubsidence_All
    

  ! -------------------------------------------------------------
  ! --- GET SUBSIDENCE AT ALL NODES OF A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidenceAtLayer(AppGW,iLayer,Subs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLayer
    REAL(8),INTENT(OUT)         :: Subs(:)
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%GetSubsidenceAtLayer(iLayer,Subs)
    ELSE
        Subs = 0.0
    END IF
    
  END SUBROUTINE GetSubsidenceAtLayer
    

  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW TO ELEMENT AT ITS VERTEX WITH DEFINED B.C. AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtElementNodeLayer(AppGW,iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: iBCType,iElem,indxVertex,iLayer
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: rFlow
    LOGICAL,OPTIONAL,INTENT(OUT) :: lAddToRHS
    
    CALL AppGW%AppBC%GetBoundaryFlowAtElementNodeLayer(iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)

  END SUBROUTINE GetBoundaryFlowAtElementNodeLayer
  
    
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW AT A NODE WITH DEFINED B.C. AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtNodeLayer(AppGW,iBCType,iNode,iLayer,rFlow)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iBCType,iNode,iLayer
    REAL(8),INTENT(OUT)         :: rFlow
    
    CALL AppGW%AppBC%GetBoundaryFlowAtNodeLayer(iBCType,iNode,iLayer,rFlow)
    
  END SUBROUTINE GetBoundaryFlowAtNodeLayer
  
    
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW WITH DEFINED B.C. AT AALL NODES AND LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtNodeLayer_All(AppGW,iBCType,rFlows)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iBCType
    REAL(8),INTENT(OUT)         :: rFlows(:,:)
    
    CALL AppGW%AppBC%GetBoundaryFlowAtNodeLayer_All(iBCType,rFlows)
    
  END SUBROUTINE GetBoundaryFlowAtNodeLayer_All
  
    
  ! -------------------------------------------------------------
  ! --- COMPUTE HORIZONTAL FLOW BETWEEN TWO VERTICES OF AN ELEMENT
  ! --- Precondition: Both nodes are active
  ! -------------------------------------------------------------
  PURE FUNCTION GetHorizontalFlow(AppGW,VertexIndex_I,VertexIndex_J,iElem,iLayer,rBottomElev_I,rBottomElev_J,AppGrid) RESULT(rFlow)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: VertexIndex_I,VertexIndex_J,iElem,iLayer
    REAL(8),INTENT(IN)           :: rBottomElev_I,rBottomElev_J
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8)                      :: rFlow
    
    !Local variables
    INTEGER :: indx,iRow,jCol,NVertex,Node_I,Node_J
    REAL(8) :: rAlpha,rHead_I,rHead_J,rElemTransmissivity,rSaturatedThick
    
    iRow    = MIN(VertexIndex_I,VertexIndex_J)
    jCol    = MAX(VertexIndex_I,VertexIndex_J)
    Node_I  = AppGrid%Vertex(VertexIndex_I,iElem)
    Node_J  = AppGrid%Vertex(VertexIndex_J,iElem)
    NVertex = AppGrid%NVertex(iElem)
    indx    = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
    rAlpha  = AppGrid%AppElement(iElem)%Integral_DELShpI_DELShpJ(indx)
    IF (rAlpha .GT. 0.0) THEN
        rFlow = 0.0
        RETURN
    END IF
    
    rElemTransmissivity = AppGW%ElemTransmissivity(iElem,iLayer)
    rHead_I             = AppGW%State%Head(Node_I,iLayer)
    rHead_J             = AppGW%State%Head(Node_J,iLayer)
    
    rFlow = rAlpha * rElemTransmissivity * (rHead_I-rHead_J)
    
    !Outflow
    IF (rFlow .LT. 0.0) THEN
        rSaturatedThick = rHead_I - (rBottomElev_I + f_rScaleElevation)
        rFlow           = rFlow / (1d0 + FEXP(-f_rSmoothStepP * rSaturatedThick))
    !Inflow
    ELSE
        rSaturatedThick = rHead_J - (rBottomElev_J + f_rScaleElevation)
        rFlow           = rFlow / (1d0 + FEXP(-f_rSmoothStepP * rSaturatedThick))
    END IF    
    
  END FUNCTION GetHorizontalFlow
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ROTATION BETWEEN TWO NODES
  ! --- Precondition: Both nodes are active
  ! -------------------------------------------------------------
  PURE FUNCTION GetRotation(AppGW,VertexIndex_I,VertexIndex_J,iElem,iLayer,AppGrid) RESULT(rRotation)
    CLASS(AppGWType),INTENT(IN)  :: AppGW
    INTEGER,INTENT(IN)           :: VertexIndex_I,VertexIndex_J,iElem,iLayer
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8)                      :: rRotation
    
    !Local variables
    INTEGER :: indx,iRow,jCol,NVertex,Node_J
    REAL(8) :: Alpha,Head_J,ElemTransmissivity
    
    iRow    = MIN(VertexIndex_I,VertexIndex_J)
    jCol    = MAX(VertexIndex_I,VertexIndex_J)
    Node_J  = AppGrid%Vertex(VertexIndex_J,iElem)
    NVertex = AppGrid%NVertex(iElem)
    indx    = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
    Alpha   = AppGrid%AppElement(iElem)%Integral_Rot_DELShpI_DELShpJ(indx)
    
    ElemTransmissivity = AppGW%ElemTransmissivity(iElem,iLayer)
    Head_J             = AppGW%State%Head(Node_J,iLayer)
    
    rRotation = Alpha * ElemTransmissivity * Head_J
    IF (VertexIndex_I .GT. VertexIndex_J) rRotation = -rRotation
    
  END FUNCTION GetRotation
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT STORAGE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetElementStorageAtLayer(AppGW,iLayer,AppGrid,Stratigraphy,rElemStorage)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rElemStorage(AppGrid%NElements)
    
    !Local variables
    INTEGER :: indxNode
    REAL(8) :: rTopElev,rBottomElev,Head,rNodalStorage(AppGrid%NNodes)
  
    !Compute nodal storage
    DO indxNode=1,AppGrid%NNodes
        !Cycle if node is inactive
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rNodalStorage(indxNode) = 0.0
            CYCLE
        END IF
        
        !Initialize
        rTopElev    = Stratigraphy%TopElev(indxNode,iLayer)
        rBottomElev = Stratigraphy%BottomElev(indxNode,iLayer)
        Head        = AppGW%State%Head(indxNode,iLayer)
        
        !Compute nodal storage
        IF (Head .GE. rTopElev) THEN  
            rNodalStorage(indxNode) = (Head-rTopElev) * AppGW%Nodes%Ss(indxNode,iLayer)  +  (rTopElev-rBottomElev) * AppGW%Nodes%Sy(indxNode,iLayer)
        ELSE
            rNodalStorage(indxNode) = (Head-rBottomElev) * AppGW%Nodes%Sy(indxNode,iLayer) 
        END IF
        
    END DO
    
    !Distribute nodal storage to element storage
    CALL AppGrid%NodeData_To_ElemData(rNodalStorage,rElemStorage)
    
  END SUBROUTINE GetElementStorageAtLayer
    
    
  ! -------------------------------------------------------------
  ! --- COMPUTE CHANGE IN STORAGE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetChangeInStorageAtLayer(AppGW,iLayer,NNodes,Stratigraphy,rStorChange,Storativity)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer,NNodes
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rStorChange(NNodes)
    REAL(8),INTENT(OUT)               :: Storativity(NNodes)
    
    !Local variables
    INTEGER :: indxNode
    REAL(8) :: rTopElev,rBottomElev,Head,Head_P,Storativity_P
  
    !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxNode,rTopElev,rBottomElev,Head,Head_P,Storativity_P) 
    DO indxNode=1,NNodes
        !Cycle if node is inactive
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rStorChange(indxNode) = 0.0
            Storativity(indxNode) = 0.0
            CYCLE
        END IF
        
        !Initialize
        rTopElev      = Stratigraphy%TopElev(indxNode,iLayer)
        rBottomElev   = Stratigraphy%BottomElev(indxNode,iLayer)
        Head          = AppGW%State%Head(indxNode,iLayer)
        Head_P        = AppGW%State%Head_P(indxNode,iLayer)
        Storativity_P = AppGW%State%Storativity_P(indxNode,iLayer)
        
        !Define storativity
        IF (Head .GE. rTopElev) THEN  
            Storativity(indxNode) = AppGW%Nodes%Ss(indxNode,iLayer)
        ELSE
            Storativity(indxNode) = AppGW%Nodes%Sy(indxNode,iLayer)
        END IF
        
        !Compute change in storage
        rStorChange(indxNode) = Storativity(indxNode) * (Head-rTopElev)        &  
                              + Storativity_P         * (rTopElev-Head_P)  
        
    END DO
    !$OMP END PARALLEL DO
    
  END SUBROUTINE GetChangeInStorageAtLayer
    
    
  ! -------------------------------------------------------------
  ! --- COMPUTE VERTICAL FLOW AT ALL NODES IN A LAYER BETWEEN THAT LAYER AND THE ACTIVE LAYER BELOW
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetVerticalFlowAtNodesLayer(AppGW,iLayer,NNodes,Stratigraphy,rVertFlow)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer,NNodes
    TYPE(StratigraphyTYpe),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rVertFLow(:)
    
    CALL VerticalFlow_ComputeAtNodesLayer(iLayer,NNodes,Stratigraphy,AppGW%State%Head,AppGW%Nodes%LeakageV,rVertFlow)
    
  END SUBROUTINE GetVerticalFlowAtNodesLayer
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE UPWARD AND DOWNWARD VERTICAL FLOW AT ALL ELEMENTS IN A LAYER BETWEEN THAT LAYER AND THE ACTIVE LAYER BELOW
  ! -------------------------------------------------------------
  SUBROUTINE GetVerticalElementUpwardDownwardFlow_AtLayer(AppGW,iLayer,AppGrid,Stratigraphy,rVertFlow_Upward,rVertFlow_Downward)
    CLASS(AppGWType),INTENT(IN)       :: AppGW
    INTEGER,INTENT(IN)                :: iLayer
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyTYpe),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(OUT)               :: rVertFlow_Upward(AppGrid%NElements),rVertFlow_Downward(AppGrid%NElements)
    
    CALL VerticalFlow_ComputeElementsUpwardDownward_AtLayer(iLayer,AppGrid,Stratigraphy,AppGW%State%Head,AppGW%Nodes%LeakageV,rVertFlow_Upward,rVertFlow_Downward)
    
  END SUBROUTINE GetVerticalElementUpwardDownwardFlow_AtLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF NODES WITH A SPECIFIED B.C. TYPE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetNodesWithBCType(AppGW,iLayer,iBCType,iNodes)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iLayer,iBCType
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iNodes(:)
    
    CALL AppGW%AppBC%GetNodesWithBCType(iLayer,iBCType,iNodes)
    
  END SUBROUTINE GetNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF NODES WITH A SPECIFIED B.C. TYPE AT A LAYER
  ! -------------------------------------------------------------
  FUNCTION GetNNodesWithBCType(AppGW,iLayer,iBCType) RESULT(NNodes)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLayer,iBCType
    INTEGER                     :: NNodes
    
    IF (AppGW%lAppBC_Defined) THEN
        NNodes =  AppGW%AppBC%GetNNodesWithBCType(iLayer,iBCType)
    ELSE
        NNodes = 0
    END IF
    
  END FUNCTION GetNNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET Z-BUDGET FILE NAME
  ! -------------------------------------------------------------
  FUNCTION GetZBudgetRawFileName(AppGW) RESULT(cFileName)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    CHARACTER(:),ALLOCATABLE    :: cFileName
    
    IF (LEN_TRIM(AppGW%cZBudRawFileName) .EQ. 0) THEN
        ALLOCATE(CHARACTER(LEN=1) :: cFileName)
        cFileName = ' '
    ELSE
        ALLOCATE(CHARACTER(LEN=LEN_TRIM(AppGW%cZBudRawFileName)) :: cFileName )
        cFileName = TRIM(AppGW%cZBudRawFileName)
    END IF
    
  END FUNCTION GetZBudgetRawFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER HEADS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHeads_All(AppGW,lPrevious,Heads)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8),INTENT(OUT)         :: Heads(:,:)
    
    IF (lPrevious) THEN   
        Heads = AppGW%State%Head_P
    ELSE
        Heads = AppGW%State%Head
    END IF
    
  END SUBROUTINE GetHeads_All
  
    
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER HEAD AT ONE NODE AND LAYER
  ! -------------------------------------------------------------
  PURE FUNCTION GetHead_AtOneNodeLayer(AppGW,iNode,iLayer,lPrevious) RESULT(Head)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iNode,iLayer
    LOGICAL,INTENT(IN)          :: lPrevious
    REAL(8)                     :: Head
    
    IF (lPrevious) THEN   
        Head = AppGW%State%Head_P(iNode,iLayer)
    ELSE
        Head = AppGW%State%Head(iNode,iLayer)
    END IF
    
  END FUNCTION GetHead_AtOneNodeLayer
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TILE DRAINS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDrain(AppGW) RESULT(NDrain)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: NDrain
    
    NDrain = AppGW%AppTileDrain%GetNDrain()
    
  END FUNCTION GetNDrain
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SUBSURFACE IRRIGATION
  ! -------------------------------------------------------------
  PURE FUNCTION GetNSubIrig(AppGW) RESULT(NSubIrig)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: NSubIrig
    
    NSubIrig = AppGW%AppTileDrain%GetNSubIrig()
    
  END FUNCTION GetNSubIrig
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetTileDrainIDs(AppGW,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%GetDrainIDs(IDs)
    
  END SUBROUTINE GetTileDrainIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER NODES CORRESPONDING TO TILE DRAINS OR SUBSURFACE IRRIGATION AND CORRESPONDING LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainNodesLayers(AppGW,iType,iGWNodes,iGWNodeLayers)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iType
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iGWNodes(:),iGWNodeLayers(:)
    
    CALL AppGW%AppTileDrain%GetGWNodesLayers(iType,iGWNodes,iGWNodeLayers)
    
  END SUBROUTINE GetTileDrainNodesLayers


  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN OR SUBSURFACE IRRIGATION FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainFlows(AppGW,iType,rFlows)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iType
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rFlows(:)
    
    CALL AppGW%AppTileDrain%GetFlows(iType,rFlows)
    
  END SUBROUTINE GetTileDrainFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN FLOWS TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE GetTileDrainFlowsToStreams(AppGW,QDrain)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: QDrain(:)
    
    CALL AppGW%AppTileDrain%GetFlowsToStreams(QDrain)
    
  END SUBROUTINE GetTileDrainFlowsToStreams
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF WELLS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNWells(AppGW) RESULT(N)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: N
    
    N = AppGW%AppPumping%GetNWells()
    
  END FUNCTION GetNWells
  
   
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENT PUMPING 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNElemPumps(AppGW) RESULT(N)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER                     :: N
    
    N = AppGW%AppPumping%GetNElemPumps() 
    
  END FUNCTION GetNElemPumps
  
   
  ! -------------------------------------------------------------
  ! --- GET WELL IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetWellIDs(AppGW,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    IF (SIZE(IDs) .GT. 0) CALL AppGW%AppPumping%GetWellIDs(IDs)
    
  END SUBROUTINE GetWellIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET ELEMENT PUMPING IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElemPumpIDs(AppGW,IDs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(OUT)         :: IDs(:)
    
    IF (SIZE(IDs) .GT. 0) CALL AppGW%AppPumping%GetElemPumpIDs(IDs)
    
  END SUBROUTINE GetElemPumpIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET WELL COORDINATES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetWellCoordinates(AppGW,rX,rY)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rX(:),rY(:)
    
    IF (SIZE(rX) .GT. 0) CALL AppGW%AppPumping%GetWellCoordinates(rX,rY)
    
  END SUBROUTINE GetWellCoordinates
  
   
  ! -------------------------------------------------------------
  ! --- GET ALL WELL PERFORATION TOP AND BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetWellPerfTopBottom(AppGW,rTop,rBottom)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rTop(:),rBottom(:)
    
    CALL AppGW%AppPumping%GetWellPerfTopBottom(rTop,rBottom)
    
  END SUBROUTINE GetWellPerfTopBottom
  
   
  ! -------------------------------------------------------------
  ! --- GET ACTUAL NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalPumpActual(AppGW,NodalPumpActual)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: NodalPumpActual(:,:)
    
    CALL AppGW%AppPumping%GetNodalPumpActual(NodalPumpActual)
    
  END SUBROUTINE GetNodalPumpActual
  
      
  ! -------------------------------------------------------------
  ! --- GET REQUIRED NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalPumpRequired(AppGW,NodalPumpRequired)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: NodalPumpRequired(:,:)
    
    CALL AppGW%AppPumping%GetNodalPumpRequired(NodalPumpRequired)
    
  END SUBROUTINE GetNodalPumpRequired
  
      
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING FOR PUMPING TYPE 
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpActual(AppGW,iPumpType,PumpActual)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iPumpType
    REAL(8),ALLOCATABLE,INTENT(OUT) :: PumpActual(:)
    
    CALL AppGW%AppPumping%GetPumpActual(iPumpType,PumpActual)
    
  END SUBROUTINE GetPumpActual
  

  ! -------------------------------------------------------------
  ! --- GET THE PURPOSE OF PUMPING (IF IT SERVES AG, URBAN OR BOTH) BEFORE SUPPLY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpPurpose(AppGW,iPumpType,iPumps,iAgOrUrban,iStat)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iPumpType,iPumps(:)
    INTEGER,INTENT(OUT)         :: iAgOrUrban(:),iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'GetPumpPurpose'
    
    IF (AppGW%lPumping_Defined) THEN
        CALL AppGW%AppPumping%GetPumpingPurpose(iPumpType,iPumps,iAgOrUrban,iStat)
    ELSE
        iStat = -1
        CALL SetLastMessage('Pumping is not simulated so purposes for pumping cannot be retrieved!',f_iFatal,ThisProcedure)
    END IF
    
  END SUBROUTINE GetPumpPurpose
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT ALL ELEMENTS (INCLUDES ELEMENT AND WELL PUMPING) 
  ! -------------------------------------------------------------
  SUBROUTINE GetElementPumpActual(AppGW,rPump) 
    CLASS(AppGWType),INTENT(IN) :: AppGW
    REAL(8),INTENT(OUT)         :: rPump(:)
    
    rPump = AppGW%AppPumping%GetElementPumpActual(SIZE(rPump))
    
   END SUBROUTINE GetElementPumpActual

  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT (ELEMENT,LAYER) 
  ! -------------------------------------------------------------
  FUNCTION GetActualPumpingAtElementLayerNode(AppGW,iElem,iLayer,indxNode,iPumpType) RESULT(Pumping)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iElem,iLayer,indxNode,iPumpType
    REAL(8)                     :: Pumping
    
    Pumping = AppGW%AppPumping%GetActualPumpingAtElementLayerNode(iElem,iLayer,indxNode,iPumpType)
    
   END FUNCTION GetActualPumpingAtElementLayerNode

  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT NUMBER WHERE PUMPING OCCURS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetPumpElement(AppGW,indxPump,iPumpType) RESULT(iElem)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: indxPump,iPumpType
    INTEGER                     :: iElem
    
    iElem = AppGW%AppPumping%GetElement(indxPump,iPumpType)
  
  END FUNCTION GetPumpElement
  
  
  ! -------------------------------------------------------------
  ! --- GET PUMPING LAYER FACTORS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetLayerPumpFactors(AppGW,indxPump,iPumpType,Factors)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: indxPump,iPumpType
    REAL(8),INTENT(OUT)         :: Factors(:)
    
    CALL AppGW%AppPumping%GetLayerFactors(indxPump,iPumpType,Factors)
    
  END SUBROUTINE GetLayerPumpFactors
    

  ! -------------------------------------------------------------
  ! --- GET AGRICULTURAL AND URBAN PUMPING SUPPLY TO EACH ELEMENT 
  ! -------------------------------------------------------------
  SUBROUTINE GetSupply(AppGW,WellDestConnector,ElemPumpDestConnector,PumpSupply_Ag,PumpSupply_Urb)
    CLASS(AppGWType),INTENT(IN)                     :: AppGW
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    REAL(8),INTENT(OUT)                             :: PumpSupply_Ag(:),PumpSupply_Urb(:)
    
    CALL AppGW%AppPumping%GetSupply(WellDestConnector,ElemPumpDestConnector,PumpSupply_Ag,PumpSupply_Urb)
    
  END SUBROUTINE GetSupply 
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA FOR PUMPING ADJUSTMENT 
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyAdjustData(AppGW,iPumpType,iColAdjust,PumpRequired,PumpMax,PumpActual,IrigFracs)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iPumpType
    INTEGER,INTENT(OUT)         :: iColAdjust(:)
    REAL(8),INTENT(OUT)         :: PumpRequired(:),PumpMax(:),PumpActual(:),IrigFracs(:)
    
    CALL AppGW%AppPumping%GetSupplyAdjustData(iPumpType,iColAdjust,PumpRequired,PumpMax,PumpActual,IrigFracs)
    
  END SUBROUTINE GetSupplyAdjustData
    

  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetiColAdjust(AppGW,iPumpType,iColAdjust)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iPumpType
    INTEGER,INTENT(OUT)         :: iColAdjust(:)
    
    CALL AppGW%AppPumping%GetiColAdjust(iPumpType,iColAdjust)
    
  END SUBROUTINE GetiColAdjust


  ! -------------------------------------------------------------
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(AppGW,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(AppGWType),INTENT(IN)              :: AppGW
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    !Local variables
    INTEGER                  :: iErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (iBudgetTypeList , iBudgetLocationTypeList , cBudgetDescriptions , cBudgetFiles , STAT=iErrorCode)
         
    !Get the list if there is a Budget generated
    IF (AppGW%lGWBudFile_Defined) THEN
        ALLOCATE (iBudgetTypeList(1) , iBudgetLocationTypeList(1) , cBudgetDescriptions(1) , cBudgetFiles(1))
        CALL AppGW%GWBudFile%GetFileName(cFileName)
        cBudgetFiles(1)            = cFileName
        iBudgetTypeList(1)         = f_iBudgetType_GW
        iBudgetLocationTypeList(1) = f_iLocationType_Subregion
        cBudgetDescriptions(1)     = f_cDescription_GWBudget
    ELSE
        ALLOCATE (iBudgetTypeList(0) , iBudgetLocationTypeList(0) , cBudgetDescriptions(0) , cBudgetFiles(0))
    END IF
     
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppGW,iLocationIndex,iNCols,iStat)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iLocationIndex
    INTEGER,INTENT(OUT)         :: iNCols,iStat       
        
    IF (AppGW%lGWBudFile_Defined) THEN
        CALL AppGW%GWBudFile%GetNDataColumns(iLocationIndex,iNCols,iStat)  !Include Time column
        iNCols = iNCols - 1  !Exclude Time column
    ELSE
        iStat  = 0
        iNCols = 0
    END IF
     
  END SUBROUTINE GetBudget_NColumns


  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppGW,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(AppGWType),INTENT(IN)              :: AppGW
    INTEGER,INTENT(IN)                       :: iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER                                       :: iNCols,iErrorCode
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    IF (AppGW%lGWBudFile_Defined) THEN
        !Get number of columns (includes Time column)
        CALL AppGW%GWBudFile%GetNDataColumns(iLocationIndex,iNCols,iStat)  
        IF (iStat .NE. 0) RETURN
        
        !Get column titles (includes (Time column)
        ALLOCATE (cColTitles_Local(iNCols))
        cColTitles_Local = AppGW%GWBudFile%GetFullColumnHeaders(iLocationIndex,iNCols)
        
        !Insert units
        CALL AppGW%GWBudFile%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
        
        !Remove time column
        iNCols = iNCols - 1
        ALLOCATE (cColTitles(iNCols))
        cColTitles = ADJUSTL(cColTitles_Local(2:))
        
        !Clear memory
        DEALLOCATE (cColTitles_Local , STAT=iErrorCode)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF
     
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM AppGW OBJECT FOR A SPECIFED SUBREGION
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppGW(AppGW,iSubregionIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(AppGWType),INTENT(IN)              :: AppGW      
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iSubregionIndex
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)      !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    IF (AppGW%lGWBudFile_Defined) THEN
        CALL GetBudget_MonthlyFlows_GivenFile(AppGW%GWBudFile,iSubregionIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    ELSE
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
        iStat = 0
    END IF
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppGW


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE FOR A SPECIFED SUBREGION
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iSubregionIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iSubregionIndex
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,PARAMETER   :: iReadCols(13) = [2,3,4,5,6,7,8,9,10,11,12,13,14]
    INTEGER             :: iDimActual,iNTimeSteps
    REAL(8),ALLOCATABLE :: rValues(:,:)
    
    !Get simulation time steps and allocate array to read data
    iNTimeSteps = Budget%GetNTimeSteps()
    ALLOCATE (rValues(14,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
    
    !Read data
    CALL Budget%ReadData(iSubregionIndex,iReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Store values in return argument
    ALLOCATE (rFlows(12,iDimActual) , cFlowNames(12))
    rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(3,1:iDimActual)   !Change in storage
    rFlows(2,:)  = rValues(4,1:iDimActual)                             !Deep Percolation       
    rFlows(3,:)  = rValues(5,1:iDimActual)                             !Gain from Stream       
    rFlows(4,:)  = rValues(6,1:iDimActual)                             !Recharge               
    rFlows(5,:)  = rValues(7,1:iDimActual)                             !Gain from Lake         
    rFlows(6,:)  = rValues(8,1:iDimActual)                             !Boundary Inflow        
    rFlows(7,:)  = rValues(9,1:iDimActual)                             !Subsidence             
    rFlows(8,:)  = rValues(10,1:iDimActual)                            !Subsurface Irrigation  
    rFlows(9,:)  = -rValues(11,1:iDimActual)                           !Tile Drain Outflow     
    rFlows(10,:) = -rValues(12,1:iDimActual)                           !Pumping                
    rFlows(11,:) = -rValues(13,1:iDimActual)                           !Outflow to Root Zone   
    rFlows(12,:) = rValues(14,1:iDimActual)                            !Net Subsurface Inflow 
    
    !Flow names
    cFlowNames     = ''
    cFlowNames(1)  = 'Change in Storage'      
    cFlowNames(2)  = 'Deep Percolation'       
    cFlowNames(3)  = 'Gain from Stream'       
    cFlowNames(4)  = 'Recharge'               
    cFlowNames(5)  = 'Gain from Lake'         
    cFlowNames(6)  = 'Boundary Inflow'        
    cFlowNames(7)  = 'Subsidence'             
    cFlowNames(8)  = 'Subsurface Irrigation'  
    cFlowNames(9)  = 'Tile Drain Outflow'     
    cFlowNames(10) = 'Pumping'                
    cFlowNames(11) = 'Outflow to Root Zone'   
    cFlowNames(12) = 'Net Subsurface Inflow'    
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile


  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppGW,iSubregionIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    INTEGER,INTENT(IN)          :: iSubregionIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)          :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)         :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER :: indx
    
    IF (AppGW%lGWBudFile_Defined) THEN
        !Read data
        DO indx=1,SIZE(iCols)
            CALL AppGW%GWBudFile%ReadData(iSubregionIndex,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
        END DO
    ELSE
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
    
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE CHANGE IN STORAGE FOR A SUBREGION FROM AppGW OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_CumGWStorChange_GivenAppGW(AppGW,iSubregionIndex,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutDates,rCumStorChange,iStat)
    CLASS(AppGWType),INTENT(IN)     :: AppGW
    INTEGER,INTENT(IN)              :: iSubregionIndex
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cOutputInterval
    REAL(8),INTENT(IN)              :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rOutDates(:),rCumStorChange(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    IF (AppGW%lGWBudFile_Defined) THEN
        CALL GetBudget_CumGWStorChange_GivenFile(AppGW%GWBudFile,iSubregionIndex,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutDates,rCumStorChange,iStat)
    ELSE
        ALLOCATE (rOutDates(0) , rCumStorChange(0))
        iStat = 0
    END IF

  END SUBROUTINE GetBudget_CumGWStorChange_GivenAppGW
    
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE CHANGE IN STORAGE FOR A SUBREGION FROM GW BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_CumGWStorChange_GivenFile(Budget,iSubregionIndex,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutDates,rCumStorChange,iStat)
    TYPE(BudgetType),INTENT(IN)     :: Budget      !Assumes Budget file is already open
    INTEGER,INTENT(IN)              :: iSubregionIndex
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cOutputInterval
    REAL(8),INTENT(IN)              :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rOutDates(:),rCumStorChange(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    INTEGER,PARAMETER                 :: f_iReadCols(2) = [2,3]
    INTEGER                           :: iDimActual,iNTimeSteps,indx,iInterval_InMinutes
    REAL(8)                           :: rDeltaT   
    REAL(8),ALLOCATABLE               :: rValues(:,:)
    CHARACTER(LEN=f_iTimeStampLength) :: cTimeZero
    
    !Get simulation time steps and allocate array to read data
    iNTimeSteps = Budget%GetNTimeSteps()
    ALLOCATE (rValues(3,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
    
    !Read data
    CALL Budget%ReadData(iSubregionIndex,f_iReadCols,cOutputInterval,cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Store values in return argument
    ALLOCATE (rOutDates(iDimActual+1) , rCumStorChange(iDimActual+1))
    rOutDates(2:iDimActual+1) = rValues(1,1:iDimActual)
    rCumStorChange(1)         = 0.0
    DO indx=1,iDimActual
        rCumStorChange(indx+1) = rCumStorChange(indx) + rValues(3,indx) - rValues(2,indx) 
    END DO
    
    !Calculate first date as t=0
    CALL CTimeStep_To_RTimeStep(cOutputInterval,rDeltaT,iInterval_InMinutes,iStat)  ;  IF (iStat .NE. 0) RETURN
    cTimeZero    = IncrementTimeStamp(cBeginDate,iInterval_InMinutes,-1)
    rOutDates(1) = TimeStampToJulian(cTimeZero)
    
  END SUBROUTINE GetBudget_CumGWStorChange_GivenFile
    
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SET HEADS
  ! -------------------------------------------------------------
  SUBROUTINE SetHeads(AppGW,rHeads)
    CLASS(AppGWType),INTENT(INOUT) :: AppGW
    REAL(8),INTENT(IN)             :: rHeads(:)
    
    AppGW%State%Head = RESHAPE(rHeads,SHAPE(AppGW%State%Head))

  END SUBROUTINE SetHeads
  
  
  ! -------------------------------------------------------------
  ! --- SET BOUNDARY CONDITION NODES WITH A CERTAIN TYPE OF B.C.
  ! -------------------------------------------------------------
  SUBROUTINE SetBCNodes(AppGW,iNodes,iLayers,iBCType,iStat,iTSCols,iTSColsMaxBCFlow,rConductances,rConstrainingBCHeads)
    CLASS(AppGWType)            :: AppGW
    INTEGER,INTENT(IN)          :: iNodes(:),iLayers(:),iBCType
    INTEGER,INTENT(OUT)         :: iStat
    INTEGER,OPTIONAL,INTENT(IN) :: iTSCols(:),iTSColsMaxBCFlow(:)
    REAL(8),OPTIONAL,INTENT(IN) :: rConductances(:),rConstrainingBCHeads(:)
    
    CALL AppGW%AppBC%SetBCNodes(iNodes,iLayers,iBCType,iStat,iTSCols,iTSColsMaxBCFlow,rConductances,rConstrainingBCHeads) 
    AppGW%lAppBC_Defined = .TRUE.
    
  END SUBROUTINE SetBCNodes
  
  
  ! -------------------------------------------------------------
  ! --- SET BOUNDARY CONDITION 
  ! -------------------------------------------------------------
  SUBROUTINE SetBC(AppGW,iNode,iLayer,iBCType,iNodeIDs,iStat,rFlow,rHead,rMaxBCFlow,rConductance,rConstrainingHeadBC)
    CLASS(AppGWType)            :: AppGW
    INTEGER,INTENT(IN)          :: iNode,iLayer,iBCType,iNodeIDs(:)
    INTEGER,INTENT(OUT)         :: iStat
    REAL(8),OPTIONAL,INTENT(IN) :: rFlow,rHead,rMaxBCFlow,rConductance,rConstrainingHeadBC
    
    CALL AppGW%AppBC%SetBC(iNode,iLayer,iBCType,iNodeIDs,iStat,rFlow,rHead,rMaxBCFlow,rConductance,rConstrainingHeadBC) 
    IF (iStat .NE. 0) RETURN
    
    !Update flag to check if BC data is defined
    AppGW%lAppBC_Defined = AppGW%AppBC%IsDefined()
    
  END SUBROUTINE SetBC
  
  
  ! -------------------------------------------------------------
  ! --- SET VELOCITIES AT NODES
  ! -------------------------------------------------------------
  SUBROUTINE SetVelocities(AppGW,Vx,Vy,Vz)
    CLASS(AppGWType)   :: AppGW
    REAL(8),INTENT(IN) :: Vx(:,:),Vy(:,:),Vz(:,:)
    
    AppGW%State%Vx = Vx
    AppGW%State%Vy = Vy
    AppGW%State%Vz = Vz

  END SUBROUTINE SetVelocities
  
  
  ! -------------------------------------------------------------
  ! --- SET IRRIGATION FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetIrigFracsRead(AppGW,IrigFrac)
    CLASS(AppGWType)   :: AppGW
    REAL(8),INTENT(IN) :: IrigFrac(:)
    
    CALL AppGW%AppPumping%SetIrigFracsRead(IrigFrac)

  END SUBROUTINE SetIrigFracsRead
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplySpecs(AppGW,SupplyDestConnector,iPumpType,PumpRequired,IrigFracs,SupplyToDest)
    CLASS(AppGWType)                         :: AppGW
    TYPE(SupplyDestinationConnectorType)     :: SupplyDestConnector
    INTEGER,INTENT(IN)                       :: iPumpType
    REAL(8),INTENT(IN)                       :: PumpRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: SupplyToDest(:)
    
    CALL AppGW%AppPumping%SetSupplySpecs(SupplyDestConnector,iPumpType,PumpRequired,IrigFracs,SupplyToDest)

  END SUBROUTINE SetSupplySpecs
  
    
  ! -------------------------------------------------------------
  ! --- SET REQUIRED PUMPING VALUES FOR A LIST OF WELLS/ELEMENT PUMPS
  ! -------------------------------------------------------------
  SUBROUTINE SetPumpRequired(AppGW,iPumpType,iPumpList,rPumpRequired)
    CLASS(AppGWType)   :: AppGW
    INTEGER,INTENT(IN) :: iPumpType,iPumpList(:)
    REAL(8),INTENT(IN) :: rPumpRequired(:)
    
    CALL AppGW%AppPumping%SetPumpRequired(iPumpType,iPumpList,rPumpRequired)
    
  END SUBROUTINE SetPumpRequired
  
  
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PRINT OUT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppGW,OutFile)
    CLASS(AppGWType),INTENT(IN) :: AppGW
    TYPE(GenericFileType)       :: OutFile
    
    CALL OutFile%WriteData(AppGW%State%Head)
    CALL OutFile%WriteData(AppGW%State%Head_P)
    CALL OutFile%WriteData(AppGW%State%Storativity_P) 
    CALL OutFile%WriteData(AppGW%RegionalStorage) 
    CALL OutFile%WriteData(AppGW%RegionalStorage_P) 
    
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%PrintRestartData(OutFile)
    
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%PrintRestartData(OutFile)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppGW,TimeStep,lEndOfSimulation,AppGrid,Stratigraphy,QPERC,QNETP,RRecvLoss,FaceFlows,SWShedFaceFlows,RSWShedIn,GWToRZFlows,StrmGWConnector,LakeGWConnector)
    CLASS(AppGWType)                     :: AppGW
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    LOGICAL,INTENT(IN)                   :: lEndOfSimulation
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    REAL(8),INTENT(IN)                   :: QPERC(:),QNETP(:),RRecvLoss(:),FaceFlows(:,:),RSWShedIn(:),GWToRZFlows(:),SWShedFaceFlows(:,:)
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
    
    !Print tile drains related results
    CALL AppGW%AppTileDrain%PrintResults(TimeStep,lEndOfSimulation)
    
    !Print vertical flows
    CALL VerticalFlowOutput_PrintResults(AppGrid,Stratigraphy,AppGW%State%Head,AppGW%Nodes%LeakageV,AppGW%FactFlow,TimeStep,lEndOfSimulation,AppGW%VerticalFlowOutput)
  
    !Print gw budget values
    IF (AppGW%lGWBudFile_Defined)   &
        CALL WriteGWFlowsToBudFile(AppGrid,QPERC,QNETP,RRecvLoss,FaceFlows,RSWShedIn,GWToRZFlows,StrmGWConnector,LakeGWConnector,AppGW)
    
    !Print hydrographs including heads at all layers, Tecplot output and face flows
    CALL AppGW%GWHyd%PrintResults(AppGrid,Stratigraphy,AppGW%AppBC,AppGW%State,FaceFlows,SWShedFaceFlows,AppGW%FactHead,AppGW%FactFlow,AppGW%FactVelocity,TimeStep,lEndOfSimulation)
    
    !Print boundary node flows
    IF (AppGW%lAppBC_Defined)   &
        CALL AppGW%AppBC%PrintResults(AppGW%FactFlow,TimeStep,lEndOfSimulation)
    
    !Subsidence related output
    IF (AppGW%lSubsidence_Defined)  &
        CALL AppGW%AppSubsidence%PrintResults(AppGrid,Stratigraphy,TimeStep,lEndOfSimulation)
    
    !Pumping output
    CALL AppGW%AppPumping%PrintResults(AppGrid%NElements,lEndOfSimulation,TimeStep)
    
    !Print end-of-simulation heads
    IF (lEndOfSimulation) THEN
        IF (AppGW%lFinalHeadsFile_Defined) CALL PrintFinalHeads(AppGW%State%Head,TimeStep,AppGrid%AppNode%ID,AppGW%FinalHeadsFile)
    END IF
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- WRITE RAW GW BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteGWFlowsToBudFile(AppGrid,QPERC,QNETP,RRecvLoss,FaceFlows,RSWShedIn,GWToRZFlows,StrmGWConnector,LakeGWConnector,AppGW)
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8),INTENT(IN)                   :: QPERC(:),QNETP(:),RRecvLoss(:),FaceFlows(:,:),RSWShedIn(:),GWToRZFlows(:)
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(LakeGWConnectorTYpe),INTENT(IN) :: LakeGWConnector
    TYPE(AppGWType)                      :: AppGW
  
    !Local variables
    INTEGER                                  :: iNRegions
    REAL(8)                                  :: rDummyArray(f_iNGWBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RPerc,RDeepPerc,RStreamGWFlows,RRecharge,RLakeGWFlows,RBound,RSubIrig,     &
                                                RTileDrain,RPump,RSubsidence_P,RSubsidence,RError,RSubInflow,RGWToRZFlows, &
                                                RGWReturnFlow
    
    !Initialize
    iNRegions = AppGrid%NSubregions
    
    !Regional percolation
    RPerc(1:iNRegions) = AppGrid%AccumElemValuesToSubregions(QPERC)
    RPerc(iNRegions+1) = SUM(RPerc(1:iNRegions))
    
    !Regional deep percolation
    RDeepPerc(1:iNRegions) = AppGrid%AccumElemValuesToSubregions(QNETP)
    RDeepPerc(iNRegions+1) = SUM(RDeepPerc(1:iNRegions))
    
    !Stream-gw interaction
    RStreamGWFlows(1:iNRegions) = StrmGWConnector%GetSubregionalFlows(AppGrid,lInsideModel=.TRUE.)      !(+: flow from stream to gw)
    RStreamGWFlows(iNRegions+1) = SUM(RStreamGWFlows(1:iNRegions))
    
    !Recharge as diversion recovarable losses and from pumping component
    RRecharge(1:iNRegions) = AppGW%AppPumping%GetSubregionalRecharge(AppGrid) + RRecvLoss
    RRecharge(iNRegions+1) = SUM(RRecharge(1:iNRegions))
    
    !Lake-gw interaction
    RLakeGWFlows(1:iNRegions) = LakeGWConnector%GetSubregionalFlows(AppGrid)      !(+: flow from lake to gw)
    RLakeGWFlows(iNRegions+1) = SUM(RLakeGWFlows(1:iNRegions))

    !Flow from boundary conditions including small watersheds
    IF (AppGW%lAppBC_Defined) THEN
        RBound(1:iNRegions) = AppGW%AppBC%GetSubregionalFlows(AppGrid)
        RBound(iNRegions+1) = SUM(RBound(1:iNRegions))
    ELSE
        RBound = 0.0
    END IF
    RBound = RBound + RSWShedIn
    
    !Subsurface irrigation
    RSubIrig(1:iNRegions) = AppGW%AppTileDrain%GetSubregionalFlows(f_iSubIrig,AppGrid)
    RSubIrig(iNRegions+1) = SUM(RSubIrig(1:iNRegions))
    
    !Tile drains
    RTileDrain(1:iNRegions) = AppGW%AppTileDrain%GetSubregionalFlows(f_iTileDrain,AppGrid)
    RTileDrain(iNRegions+1) = SUM(RTileDrain(1:iNRegions))
    
    !Pumping
    RPump(1:iNRegions) = AppGW%AppPumping%GetSubregionalPumping(AppGrid)             
    RPump(iNRegions+1) = SUM(RPump(1:iNRegions))
    
    !GW return flow
    RGWReturnFlow(1:iNRegions) = AppGrid%AccumNodeValuesToSubregions(AppGW%rGWReturnFlow)
    RGWReturnFlow(iNRegions+1) = SUM(RGWReturnFlow(1:iNRegions))
    
    !GW to root zone flows
    RGWToRZFlows(1:iNRegions) = AppGrid%AccumElemValuesToSubregions(GWToRZFlows)             
    RGWToRZFlows(iNRegions+1) = SUM(RGWToRZFlows(1:iNRegions))
    
    IF (AppGW%lSubsidence_Defined) THEN
        !Cumulative subsidence at the current time step
        RSubsidence(1:iNRegions) = AppGW%AppSubsidence%GetSubregionalCumSubsidence(AppGrid%NSubregions,lPreviousTS=.FALSE.)
        RSubsidence(iNRegions+1) = SUM(RSubsidence(1:iNRegions))

        !Cumulative subsidence at the previous time step
        RSubsidence_P(1:iNRegions) = AppGW%AppSubsidence%GetSubregionalCumSubsidence(AppGrid%NSubregions,lPreviousTS=.TRUE.)
        RSubsidence_P(iNRegions+1) = SUM(RSubsidence_P(1:iNRegions))
    ELSE
        RSubsidence   = 0.0
        RSubsidence_P = 0.0
    END IF
    
    !Regional net subsurface inflow from adjacent subregions
    RSubInflow = ComputeSubregionalGWFlowExchange(AppGrid,FaceFlows)
    
    !Mass balance error
    RError =  AppGW%RegionalStorage_P      &
            - AppGW%RegionalStorage        &
            + RDeepPerc                    &
            + RStreamGWFlows               &
            + RRecharge                    &
            + RLakeGWFlows                 &
            + RBound                       &
            + RSubsidence - RSubsidence_P  &
            + RSubIrig                     &
            + RTileDrain                   &
            - RPump                        &
            - RGWReturnFlow                &
            - RGWToRZFlows                 &
            + RSubInflow

    !Store budget data in array
    rDummyArray(1,:)  = RPerc 
    rDummyArray(2,:)  = AppGW%RegionalStorage_P
    rDummyArray(3,:)  = AppGW%RegionalStorage    
    rDummyArray(4,:)  = RDeepPerc 
    rDummyArray(5,:)  = RStreamGWFlows 
    rDummyArray(6,:)  = RRecharge 
    rDummyArray(7,:)  = RLakeGWFlows
    rDummyArray(8,:)  = RBound 
    rDummyArray(9,:)  = RSubsidence-RSubsidence_P
    rDummyArray(10,:) = RSubIrig 
    rDummyArray(11,:) =-RTileDrain 
    rDummyArray(12,:) = RPump 
    rDummyArray(13,:) = RGWReturnFlow 
    rDummyArray(14,:) = RGWToRZFlows 
    rDummyArray(15,:) = RSubInflow 
    rDummyArray(16,:) = RError
    rDummyArray(17,:) = RSubsidence
   
    !Write data
    CALL AppGW%GWBudFile%WriteData(rDummyArray)
    
  END SUBROUTINE WriteGWFlowsToBudFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT-OUT FINAL AQUIFER PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintAquiferParameters(iGWNodeIDs,GWNodes)
    INTEGER,INTENT(IN)          :: iGWNodeIDs(:)
    TYPE(GWNodeType),INTENT(IN) :: GWNodes
    
    !Local variables
    INTEGER   :: indxNode,indxLayer,NNodes,NLayers
    CHARACTER :: Text*500
    
    !Initialize
    NNodes  = SIZE(GWNodes%Kh , DIM=1)
    NLayers = SIZE(GWNodes%Kh , DIM=2)
    
    !Print parameters
    CALL LogMessage('',f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT('-',100),f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT(' ',30)//'AQUIFER PARAMETER VALUES FOR EACH NODE',f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT(' ',12)//'*** Note: Values Below are After '//'Multiplication by Conversion Factors ***',f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT('-',100),f_iMessage,'',f_iFILE)
    WRITE (Text,'(A,2X,5(A,2X))')             &
        '   NODE','        PKH             '   &
                 ,'        PS              '   &
                 ,'        PN              '   &
                 ,'        PV              '   &
                 ,'        PL              '   
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)
    
    DO indxNode=1,NNodes
      DO indxLayer=1,NLayers                                                                                          
        IF (indxLayer .EQ. 1) THEN                                                                                          
          WRITE (Text,'(I7,2X,5(1PG24.15E3,2X))')                                                               &                                                                                          
               iGWNodeIDs(indxNode) ,GWNodes%Kh(indxNode,indxLayer)   ,GWNodes%Ss(indxNode,indxLayer) ,GWNodes%Sy(indxNode,indxLayer) ,GWNodes%AquitardKv(indxNode,indxLayer)   ,GWNodes%Kv(indxNode,indxLayer)                                                                                             
        ELSE                                                                                          
          WRITE (Text,'(9X,5(1PG24.15E3,2X))')                                                                  &                                                                                          
                         GWNodes%Kh(indxNode,indxLayer)   ,GWNodes%Ss(indxNode,indxLayer) ,GWNodes%Sy(indxNode,indxLayer) ,GWNodes%AquitardKv(indxNode,indxLayer)   ,GWNodes%Kv(indxNode,indxLayer)                                                                                              
        END IF                                                                                          
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)                                                                                          
      END DO                                                                                          
    END DO  
        
    CALL LogMessage('',f_iMessage,'',f_iFILE)

  END SUBROUTINE PrintAquiferParameters
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT END-OF-SIMULATION HEADS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalHeads(Heads,TimeStep,NodeIDs,OutFile)
    REAL(8),INTENT(IN)            :: Heads(:,:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NodeIDs(:)
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER   :: indxLayer,indxNode,NLayers,NNodes
    REAL(8)   :: rHeadWork(SIZE(Heads,DIM=2))
    CHARACTER :: SimulationTime*21,Text*1000,cLayer*7
    
    !Initialize
    NNodes  = SIZE(Heads,DIM=1)
    NLayers = SIZE(Heads,DIM=2)
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF
    
    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C ***** GROUNDWATER HEADS AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('     1.0                           / FACTHP')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    Text = 'C      ID           HP[1]'
    DO indxLayer=2,NLayers
        cLayer = ADJUSTL('HP['//TRIM(IntToText(indxLayer))//']')
        WRITE (Text,'(3A)') TRIM(Text),REPEAT(' ',18-LEN_TRIM(cLayer)),TRIM(cLayer)
    END DO
    CALL OutFile%WriteData(TRIM(Text))
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    
    !Print final heads
    DO indxNode=1,NNodes
        rHeadWork = Heads(indxNode,:)
        WRITE (Text,'(I8,100F18.6)') NodeIDs(indxNode),rHeadWork
        CALL OutFile%WriteData(Text)
    END DO
    
    END SUBROUTINE PrintFinalHeads

  
  
   
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ GW RETURN FLOW DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadGWReturnFlowDestinations(iStrmNodeIDs,iLakeIDs,iGWNodeIDs,AppGWParamFile,lSimulateGWReturnFlow,iGWReturnFlowDestType,iGWReturnFlowDest,iStat)
    INTEGER,INTENT(IN)                  :: iStrmNodeIDs(:),iLakeIDs(:),iGWNodeIDs(:)
    TYPE(GenericFileType),INTENT(INOUT) :: AppGWParamFile
    LOGICAL,INTENT(OUT)                 :: lSimulateGWReturnFlow
    INTEGER,INTENT(OUT)                 :: iGWReturnFlowDestType(:),iGWReturnFlowDest(:)
    INTEGER,INTENT(OUT)                 :: iStat
  
    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'ReadGWReturnFlowDestinations'
    INTEGER                      :: indxNode,iNodeID,iNode,iDummyArray(3),iDestinationType,iSimGWReturnFlow, &
                                    iStrmNodeID,iStrmNode,iLakeID,iLake
    LOGICAL                      :: lProcessed(SIZE(iGWNodeIDs))
    INTEGER,PARAMETER            :: f_iDestinationTypeList(3) = [f_iFlowDest_Outside  , &
                                                                 f_iFlowDest_StrmNode , &
                                                                 f_iFlowDest_Lake     ]
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
    !Are we simulating gw return flow
    CALL AppGWParamFile%ReadData(iSimGWReturnFlow,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iSimGWReturnFlow .EQ. 1) THEN
        lSimulateGWReturnFlow = .TRUE.
    ELSE
        lSimulateGWReturnFlow = .FALSE.
    END IF        
    IF (.NOT. lSimulateGWReturnFlow) RETURN
    
    DO indxNode=1,SIZE(iGWNodeIDs)
        CALL AppGWParamFile%ReadData(iDummyArray,iStat)  ;  IF (iStat .NE. 0) RETURN
        
        !Check that element ID is legit
        iNodeID = iDummyArray(1)
        iNode   = LocateInList(iNodeID,iGWNodeIDs)
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Node ID '//TRIM(IntToText(iNodeID))//' listed for groundwater return flow destination data is not modeled!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Check if elemenet was listed before
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Node '//TRIM(IntToText(iNodeID))//' is listed more than once for groundwater return flow destination data!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode) = .TRUE.
    
        !Check that destination type is legit
        iDestinationType = iDummyArray(2)
        IF (LocateInList(iDestinationType,f_iDestinationTypeList) .EQ. 0) THEN
            CALL SetLastMessage ('Groundwater flow destination type for node ' // TRIM(IntToText(iNodeID)) // ' is not accepted!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iGWReturnFlowDestType(iNode) = iDestinationType
        
        !Check that destination ID is legit
        SELECT CASE (iDestinationType)
            CASE (f_iFlowDest_Outside)
                iGWReturnFlowDest(iNode) = 0
                
            CASE (f_iFlowDest_StrmNode)
                iStrmNodeID = iDummyArray(3)
                iStrmNode   = LocateInList(iStrmNodeID,iStrmNodeIDs)
                IF (iStrmNode .EQ. 0) THEN
                    MessageArray(1) = 'Stream node number ' // TRIM(IntToText(iStrmNodeID)) // ' listed for groundwater node ' // TRIM(IntToText(iNodeID)) 
                    MessageArray(2) = ' as groundwater return flow destination is not in the model!'
                    CALL SetLastMessage (MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                iGWReturnFlowDest(iNode) = iStrmNode
                
            CASE (f_iFlowDest_Lake)
                iLakeID = iDummyArray(3)
                iLake   = LocateInList(iLakeID,iLakeIDs)
                IF (iLake .EQ. 0) THEN
                    MessageArray(1) = 'Lake number ' // TRIM(IntToText(iLakeID)) // ' listed for groundwater node ' // TRIM(IntToText(iNodeID)) 
                    MessageArray(2) = ' as groundwater return flow destination is not in the model!'
                    CALL SetLastMessage (MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                iGWReturnFlowDest(iNode) = iLake
        END SELECT        
    END DO

  END SUBROUTINE ReadGWReturnFlowDestinations
    
    
  ! -------------------------------------------------------------
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppGW,InFile,iStat)
    CLASS(AppGWType)      :: AppGW
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(OUT)   :: iStat
    
    CALL InFile%ReadData(AppGW%State%Head,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGW%State%Head_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGW%State%Storativity_P,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL InFile%ReadData(AppGW%RegionalStorage,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL InFile%ReadData(AppGW%RegionalStorage_P,iStat)  ;  IF (iStat .EQ. -1) RETURN     
    
    IF (AppGW%lSubsidence_Defined) THEN
        CALL AppGW%AppSubsidence%ReadRestartData(InFile,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    IF (AppGW%lTileDrain_Defined) CALL AppGW%AppTileDrain%ReadRestartData(InFile,iStat)

  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL HEADS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialHeads(AppGWParamFile,NNodes,NodeIDs,Stratigraphy,Heads,iStat)
    TYPE(GenericFileType)             :: AppGWParamFile
    INTEGER,INTENT(IN)                :: NNodes,NodeIDs(NNodes)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8)                           :: Heads(NNodes,Stratigraphy%NLayers)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'ReadInitialHeads'
    INTEGER                      :: indxNode,indxLayer,iActiveLayerAbove,index,ID
    REAL(8)                      :: rDummyArray(Stratigraphy%NLayers+1),rFactor
    LOGICAL                      :: lProcessed(NNodes)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
    !Read conversion factor
    CALL AppGWParamFile%ReadData(rFactor,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read initial heads and process
    DO indxNode=1,NNodes
        CALL AppGWParamFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Make sure node ID is legit
        ID = INT(rDummyArray(1))
        CALL ConvertID_To_Index(ID,NodeIDs,index)
        IF (index .EQ. 0) THEN
            CALL SetLastMessage('Node ID '//TRIM(IntToText(ID))//' listed for initial groundwater heads is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure same node is not listed more than once
        IF (lProcessed(index)) THEN
            CALL SetLastMessage('Node ID '//TRIM(IntToText(ID))//' is listed more than once for initial groundwater heads!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
            
        !Tag node as processed
        lProcessed(index) = .TRUE.
                
        !Apply conversion factor
        rDummyArray(2:) = rDummyArray(2:) * rFactor 
        
        !Make sure initial head is above aquifer bottom and head at inactive node is equal to head in active node above
        DO indxLayer=1,Stratigraphy%NLayers
            IF (Stratigraphy%ActiveNode(index,indxLayer)) THEN
                rDummyArray(indxLayer+1) = MAX(rDummyArray(indxLayer+1) , Stratigraphy%BottomElev(index,indxLayer))
            ELSE
                IF (indxLayer .EQ. 1) THEN
                    rDummyArray(indxLayer+1) = Stratigraphy%BottomElev(index,indxLayer)
                    CYCLE
                END IF
                iActiveLayerAbove = Stratigraphy%GetActiveLayerAbove(index,indxLayer)
                IF (iActiveLayerAbove .GT. 0) THEN
                    rDummyArray(indxLayer+1) = rDummyArray(iActiveLayerAbove+1)
                ELSE
                    rDummyArray(indxLayer+1) = Stratigraphy%BottomElev(index,indxLayer)
                END IF
            END IF
        END DO
        Heads(index,:) = rDummyArray(2:)
    END DO
    
    !Make sure all nodes are processed
    DO indxNode=1,NNodes
        IF (.NOT. lProcessed(indxNode)) THEN
            CALL SetLastMessage('Initial groundwater heads at node '//TRIM(IntToText(NodeIDs(indxNode)))//' are not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            EXIT
        END IF
    END DO
    
  END SUBROUTINE ReadInitialHeads
  
  
  ! -------------------------------------------------------------
  ! --- READ AQUIFER PARAMETER DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadAquiferParameters(NLayers,AppGrid,TimeStep,InFile,VarTimeUnit,GWNodes,iStat)
    INTEGER,INTENT(IN)            :: NLayers
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(GenericFileType)         :: InFile
    CHARACTER(LEN=*),INTENT(OUT)  :: VarTimeUnit
    TYPE(GWNodeType)              :: GWNodes
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'ReadAquiferParameters'
    INTEGER                      :: NGroup,indxNode,indxLayer,ID,NNodes,NEBK,IEBK,indxBK,iNode,index,         &
                                    NodeIDs(AppGrid%NNodes),ElementIDs(AppGrid%NElements)
    REAL(8)                      :: rDummyArray(6),rFactors(6),Factor,Fact,rDummyArray1(2+NLayers),           &
                                    rDummy3DArray(AppGrid%NNodes,NLayers,5),BK(NLayers)
    CHARACTER                    :: cTimeUnit_Kh*6,cTimeUnit_AquitardV*6,cTimeUnit_Kv*6,cTimeUnitMin*6,       &
                                    cTimeUnit_AnomalyKh*6,ALine*200
    LOGICAL                      :: lProcessed(AppGrid%NNodes)
    
    !Initialize
    iStat   = 0
    
    !Inform user
    CALL EchoProgress('   Reading aquifer parameters...')
    
    !Initialize
    NNodes     = AppGrid%NNodes
    NodeIDs    = AppGrid%AppNode%ID
    lProcessed = .FALSE.
    
    !Read number of parameteric grids
    CALL InFile%ReadData(NGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Conversion factors
    CALL InFile%ReadData(rFactors,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Time units
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
      cTimeUnit_Kh        = ADJUSTL(ALine)
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
      cTimeUnit_AquitardV = ADJUSTL(ALine)
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
      cTimeUnit_Kv        = ADJUSTL(ALine)
      
    !Make sure time units are valid if time tracking simulation
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(cTimeUnit_Kh) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer horizontal hydraulic conductivity is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_AquitardV) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquitard vertical conductivity is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_Kv) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer vertical hydraulic conductivity is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
      
    !Find minimum of the three time units and convert other two into the minimum time unit
    IF (cTimeUnit_Kh .TULE. cTimeUnit_AquitardV) THEN
        cTimeUnitMin = cTimeUnit_Kh
    ELSE
        cTimeUnitMin = cTimeUnit_AquitardV
    END IF
    IF (cTimeUnit_Kv .TULE. cTimeUnitMin) cTimeUnitMin = cTimeUnit_Kv
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_Kh)         ;  rFactors(2) = rFactors(2) * Factor
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_AquitardV)  ;  rFactors(5) = rFactors(5) * Factor
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_Kv)         ;  rFactors(6) = rFactors(6) * Factor
    
    !Save time unit into persistent variable
    VarTimeUnit = cTimeUnitMin
    
    !Non-parametric data input
    IF (NGroup .EQ. 0) THEN
        DO indxNode=1,NNodes
          DO indxLayer=1,NLayers
            IF (indxLayer .EQ. 1) THEN
                CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Check that node ID is legit
                ID = INT(rDummyArray(1))  
                CALL ConvertID_To_Index(ID,NodeIDs,index)
                IF (index .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for aquifer parameters is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Check that node is not listed more than once
                IF (lProcessed(index)) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for aquifer parameter entry!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Tag node as processed
                lProcessed(index) = .TRUE.
            ELSE
                CALL InFile%ReadData(rDummyArray(2:),iStat)  ;  IF (iStat .EQ. -1) RETURN
            END IF
            GWNodes%Kh(index,indxLayer)         = rDummyArray(2) * rFactors(2)
            GWNodes%Ss(index,indxLayer)         = rDummyArray(3) * rFactors(3)
            GWNodes%Sy(index,indxLayer)         = rDummyArray(4) * rFactors(4)
            GWNodes%AquitardKv(index,indxLayer) = rDummyArray(5) * rFactors(5)
            GWNodes%Kv(index,indxLayer)         = rDummyArray(6) * rFactors(6)
          END DO
        END DO
        
        !Check all nodes are processed
        DO indxNode=1,NNodes
            IF (.NOT. lProcessed(indxNode)) THEN
                CALL SetLastMessage('Aquifer parameters are not defined at node '//TRIM(IntToText(NodeIDs(indxNode)))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    
    !Parametric data input
    IF (NGroup .GT. 0) THEN

        !Read the parameter values at parametric nodes and compute the interpolation coefficients for finite element nodes
        CALL GetValuesFromParametricGrid(InFile,AppGrid%GridType,NodeIDs,NGroup,rFactors,.FALSE.,'aquifer paremeters',rDummy3DArray,iStat)
        IF (iStat .EQ. -1) RETURN

        !Initialize parameter values
        DO indxLayer=1,NLayers
            DO indxNode=1,NNodes
                GWNodes%Kh(indxNode,indxLayer)         = rDummy3DArray(indxNode,indxLayer,1)
                GWNodes%Ss(indxNode,indxLayer)         = rDummy3DArray(indxNode,indxLayer,2)
                GWNodes%Sy(indxNode,indxLayer)         = rDummy3DArray(indxNode,indxLayer,3)
                GWNodes%AquitardKv(indxNode,indxLayer) = rDummy3DArray(indxNode,indxLayer,4)
                GWNodes%Kv(indxNode,indxLayer)         = rDummy3DArray(indxNode,indxLayer,5)
            END DO
        END DO
    END IF
    
    !Read anomaly hydraulic conductivity and overwrite the previous values 
    CALL InFile%ReadData(NEBK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(Fact,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;  ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
    cTimeUnit_AnomalyKh = ADJUSTL(ALine)
    Factor = TimeIntervalConversion(cTimeUnitMin,cTimeUnit_AnomalyKh)  ;  Fact = Fact * Factor 
    IF (NEBK .GT. 0) ElementIDs = AppGrid%AppElement%ID
    DO indxBK=1,NEBK
        CALL InFile%ReadData(rDummyArray1,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IEBK = rDummyArray1(2)
        CALL ConvertID_To_Index(IEBK,ElementIDs,index)
        IF (index .EQ. 0) THEN
            CALL LogMessage('Element '//TRIM(IntToText(IEBK))//' listed for anomaly hydraulic conductivity is not in the model! Skipping...',f_iInfo,ThisProcedure)
            CYCLE
        END IF
        BK = rDummyArray1(3:) * Fact
        DO indxNode=1,AppGrid%NVertex(index)
            iNode = AppGrid%Vertex(indxNode,index)
            DO indxLayer=1,NLayers
                GWNodes%Kh(iNode,indxLayer) = BK(indxLayer)
            END DO
        END DO
    END DO

  END SUBROUTINE ReadAquiferParameters

  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AppGW,AppGrid,Stratigraphy,lPumpAdjusted,TimeStep,iStat)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    LOGICAL,INTENT(IN)                :: lPumpAdjusted
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(OUT)               :: iStat
    
    !Read time series boundary conditions
    CALL AppGW%AppBC%ReadTSData(AppGrid%AppNode%ID,TimeStep,Stratigraphy%BottomElev,AppGW%State%Head,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read pumping
    CALL AppGW%AppPumping%ReadTSData(AppGrid,Stratigraphy,AppGW%Nodes%Kh,AppGW%State%Head,lPumpAdjusted,TimeStep,iStat)
    
  END SUBROUTINE ReadTSData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- RESTORE PUMPING TO READ VALUES
  ! -------------------------------------------------------------
  SUBROUTINE RestorePumpingToReadValues(AppGW,AppGrid,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    CALL AppGW%AppPumping%RestorePumpingToReadValues(AppGrid,Stratigraphy,AppGW%Nodes%Kh,AppGW%State%Head)
    
  END SUBROUTINE RestorePumpingToReadValues
  
  
  ! -------------------------------------------------------------
  ! --- SET ACTUAL PUMPING TO REQUIRED PUMPING AND DISTRIBUTE PUMPING TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE ResetActualPumping(AppGW,AppGrid,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    CALL AppGW%AppPumping%ResetActualPumping(AppGrid,Stratigraphy,AppGW%Nodes%Kh,AppGW%State%Head)
    
  END SUBROUTINE ResetActualPumping
  
  
  ! -------------------------------------------------------------
  ! --- PREPARE AQUIFER PARAMETERS TO BE USED IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE ProcessAquiferParameters(AppGrid,Stratigraphy,lSubsidence_Defined,AppSubsidence,GWNodes,GWState,iStat)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    LOGICAL,INTENT(IN)                :: lSubsidence_Defined
    TYPE(AppSubsidenceType)           :: AppSubsidence
    TYPE(GWNodeType)                  :: GWNodes
    TYPE(GWStateType)                 :: GWState
    INTEGER,INTENT(OUT)               :: iStat
    
    !Linked list type to report incorrect Ss values
    TYPE,EXTENDS(GenericLinkedListType) :: ProblemSsNodesType
    END TYPE ProblemSsNodesType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ProcessAquiferParameters'
    INTEGER                      :: indxNode,indxLayer,iActiveLayerAbove,ID
    REAL(8)                      :: rAquiferThickness,Area,TopElevAboveLayer,BottomElevAboveLayer,  &
                                    rInterbedThick(AppGrid%NNodes,Stratigraphy%NLayers),DConfine,   &
                                    TopElev,BottomElev,ALU,ALL,rGWHead
    
    !Initialize
    iStat = 0
    
    !Process subsidence related data    
    IF (lSubsidence_Defined) THEN
        CALL AppSubsidence%ProcessSubsidenceParameters(GWState%Head)
        CALL AppSubsidence%GetInterbedThickAll(.FALSE.,rInterbedThick) 
    ELSE
        rInterbedThick = 0.0
    END IF
    
    !Process parameters
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxNode=1,AppGrid%NNodes
            ID   = AppGrid%AppNode(indxNode)%ID
            Area = AppGrid%AppNode(indxNode)%Area
            IF (Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                TopElev    = Stratigraphy%TopElev(indxNode,indxLayer)
                BottomElev = Stratigraphy%BottomElev(indxNode,indxLayer)
                rGWHead    = GWState%Head(indxNode,indxLayer)
                
                !Aquifer thickness
                rAquiferThickness = TopElev - BottomElev 
                
                !Make sure interbed thickness is less than aquifer thickness
                IF (lSubsidence_Defined) THEN
                    IF (rAquiferThickness .LE. rInterbedThick(indxNode,indxLayer)) THEN
                        MessageArray(1) = 'Aquifer thickness at node '//TRIM(IntToText(ID))//' and layer '//TRIM(IntToText(indxLayer))//' is less than interbed thickness!'
                        WRITE(MessageArray(2),'(A,F12.6)') 'Aquifer thickness  = ',rAquiferThickness
                        WRITE(MessageArray(3),'(A,F12.6)') 'Interbed thickness = ',rInterbedThick(indxNode,indxLayer)
                        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
                
                !Storage coefficient
                GWNodes%Ss(indxNode,indxLayer) = GWNodes%Ss(indxNode,indxLayer) * (rAquiferThickness-rInterbedThick(indxNode,indxLayer))
                GWNodes%Ss(indxNode,indxLayer) = GWNodes%Ss(indxNode,indxLayer) * Area
                
                !Specific yield
                GWNodes%Sy(indxNode,indxLayer) = GWNodes%Sy(indxNode,indxLayer) * Area
                
                !Make sure storage coefficient is less than or equal to specific yield (this is enforced so that a negative storage is not computed)
                GWNodes%Ss(indxNode,indxLayer) = MIN(GWNodes%Ss(indxNode,indxLayer) , GWNodes%Sy(indxNode,indxLayer))   
                
                !If the top of the aquifer layer is equal to the ground surface elevation (i.e. unconfined aquifer)
                ! then set storage coeff. equal to specific yield
                IF (TopElev .EQ. Stratigraphy%GSElev(indxNode)) GWNodes%Ss(indxNode,indxLayer) = GWNodes%Sy(indxNode,indxLayer)
                
                !Leakage coefficient
                IF (indxLayer .GT. 1) THEN
                    iActiveLayerAbove = Stratigraphy%GetActiveLayerAbove(indxNode,indxLayer)
                    IF (iActiveLayerAbove .GT. 0) THEN
                        TopElevAboveLayer    = Stratigraphy%TopElev(indxNode,iActiveLayerAbove)
                        BottomElevAboveLayer = Stratigraphy%BottomElev(indxNode,iActiveLayerAbove)
                        DConfine             = BottomElevAboveLayer - TopElev  !Thickness of overlaying aquitard
                        IF (DConfine .GT. 0.0) THEN
                            GWNodes%LeakageV(indxNode,indxLayer) = GWNodes%AquitardKv(indxNode,indxLayer) / DConfine * Area
                        ELSE
                            IF (GWNodes%Kv(indxNode,iActiveLayerAbove).GT.0.0  .AND.  GWNodes%Kv(indxNode,indxLayer).GT.0.0) THEN
                                ALU                                  = (TopElevAboveLayer-BottomElevAboveLayer) / GWNodes%Kv(indxNode,iActiveLayerAbove)
                                ALL                                  = (TopElev-BottomElev) / GWNodes%Kv(indxNode,indxLayer)
                                GWNodes%LeakageV(indxNode,indxLayer) = Area / (0.5*(ALU+ALL))
                            ELSE
                                !Zero out vertical leakage
                                !Do nothing; leakage is instantiated as zero by default
                            END IF
                        END IF
                    ELSE
                        !Zero out vertical leakage
                        !Do nothing; leakage is instantiated as zero by default
                    END IF
                ELSE
                    !Zero out vertical leakage
                    !Do nothing; leakage is instantiated as zero by default
                END IF
                
                !Storage coeff. used for the previous time step
                IF (rGWHead .GE. TopElev) THEN
                    GWState%Storativity_P(indxNode,indxLayer) = GWNodes%Ss(indxNode,indxLayer)
                ELSE
                    GWState%Storativity_P(indxNode,indxLayer) = GWNodes%Sy(indxNode,indxLayer)
                END IF
                
                !Make sure Kh is not negative
                IF (GWNodes%Kh(indxNode,indxLayer) .LT. 0.0) THEN
                    MessageArray(1) = 'Hydraulic conductivity is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node ',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes%Kh(indxNode,indxLayer),')'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure storage coeff. is not negative
                IF (GWNodes%Ss(indxNode,indxLayer) .LT. 0.0) THEN
                    MessageArray(1) = 'Specific storage is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node ',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes%Ss(indxNode,indxLayer),')'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure specific yield is not negative
                IF (GWNodes%Sy(indxNode,indxLayer) .LT. 0.0) THEN
                    MessageArray(1) = 'Specific yield is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node ',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes%Sy(indxNode,indxLayer),')'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF

                !Make sure vertical leakage is not negative
                IF (GWNodes%LeakageV(indxNode,indxLayer) .LT. 0.0) THEN
                    MessageArray(1) = 'Vertical leakage is less than zero '
                    WRITE (MessageArray(2),'(5A,F9.3,A)') 'at node ',TRIM(IntToText(ID)),', layer ',TRIM(IntToText(indxLayer)),' (',GWNodes%LeakageV(indxNode,indxLayer),')'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
            ELSE
                !Zero out aquifer parameters for inactive nodes
                !Do nothing; the values are already instantiated with zero values
            END IF            
        END DO        
    END DO
    
  END SUBROUTINE ProcessAquiferParameters
  
  
  ! -------------------------------------------------------------
  ! --- OVERWRITE GROUNDWATER PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE OverwriteParameters(cFileName,AppGrid,VarTimeUnit,TrackTime,lSubsidence_Defined,GWNodes,AppSubsidence,iStat)
    CHARACTER(LEN=*),INTENT(IN)  :: cFileName,VarTimeUnit
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    LOGICAL,INTENT(IN)           :: TrackTime,lSubsidence_Defined
    TYPE(GWNodeType)             :: GWNodes
    TYPE(AppSubsidenceType)      :: AppSubsidence
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'OverwriteParameters'
    INTEGER                      :: NWrite,indx,iNode,iLayer,index,NodeIDs(AppGrid%NNodes)
    REAL(8)                      :: rFactors(7),rDummyArrayNoSubs(7),rDummyArraySubs(9),Factor,   &
                                    ElasticSC(SIZE(GWNodes%Kh,DIM=1),SIZE(GWNodes%Kh,DIM=2)),           &
                                    InelasticSC(SIZE(GWNodes%Kh,DIM=1),SIZE(GWNodes%Kh,DIM=2))
    CHARACTER                    :: ALine*500,cTimeUnit_Kh*6,cTimeUnit_AquitardV*6,cTimeUnit_Kv*6
    TYPE(GenericFileType)        :: OverwriteFile
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL OverwriteFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='aquifer parameter over-write',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
   
    !Read number of overwrite data; return if it is zero
    CALL OverwriteFile%ReadData(NWrite,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NWrite .EQ. 0) RETURN
    
    !Conversion factors
    CALL OverwriteFile%ReadData(rFactors,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Time units
    CALL OverwriteFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
      cTimeUnit_Kh        = ADJUSTL(ALine)
    CALL OverwriteFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
      cTimeUnit_AquitardV = ADJUSTL(ALine)
    CALL OverwriteFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
      cTimeUnit_Kv        = ADJUSTL(ALine)
      
    !Make sure time units are valid if time tracking simulation
    IF (TrackTime) THEN
        IF (IsTimeIntervalValid(cTimeUnit_Kh) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer horizontal hydraulic conductivity in over-write file is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_AquitardV) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquitard vertical conductivity in over-write file is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (IsTimeIntervalValid(cTimeUnit_Kv) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for aquifer vertical hydraulic conductivity in over-write file is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Convert time units to the one specified for the aquifer parameters
    Factor = TimeIntervalConversion(VarTimeUnit,cTimeUnit_Kh)         ;  rFactors(1) = rFactors(1) * Factor
    Factor = TimeIntervalConversion(VarTimeUnit,cTimeUnit_AquitardV)  ;  rFactors(4) = rFactors(4) * Factor
    Factor = TimeIntervalConversion(VarTimeUnit,cTimeUnit_Kv)         ;  rFactors(5) = rFactors(5) * Factor

    !Read and process data
    NodeIDs = AppGrid%AppNode%ID
    IF (lSubsidence_Defined) THEN
        ElasticSC   = -1.0
        InelasticSC = -1.0
        DO indx=1,NWrite
          CALL OverwriteFile%ReadData(rDummyArraySubs,iStat)  ;  IF (iStat .EQ. -1) RETURN
          iNode  = INT(rDummyArraySubs(1))
          CALL ConvertID_To_Index(iNode,NodeIDs,index)
          IF (index .EQ. 0) THEN
              CALL LogMessage('Node number '//TRIM(IntTotext(iNode))//' listed for aquifer parameter overwrite is not part of the model! Skipping...',f_iInfo,ThisProcedure)
              CYCLE
          END IF
          iLayer = INT(rDummyArraySubs(2))
          IF (rDummyArraySubs(3) .GE. 0.0)  GWNodes%Kh(index,iLayer)         = rDummyArraySubs(3) * rFactors(1) 
          IF (rDummyArraySubs(4) .GE. 0.0)  GWNodes%Ss(index,iLayer)         = rDummyArraySubs(4) * rFactors(2) 
          IF (rDummyArraySubs(5) .GE. 0.0)  GWNodes%Sy(index,iLayer)         = rDummyArraySubs(5) * rFactors(3) 
          IF (rDummyArraySubs(6) .GE. 0.0)  GWNodes%AquitardKv(index,iLayer) = rDummyArraySubs(6) * rFactors(4) 
          IF (rDummyArraySubs(7) .GE. 0.0)  GWNodes%Kv(index,iLayer)         = rDummyArraySubs(7) * rFactors(5) 
          IF (rDummyArraySubs(8) .GE. 0.0)  ElasticSC(index,iLayer)          = rDummyArraySubs(8) * rFactors(6) 
          IF (rDummyArraySubs(9) .GE. 0.0)  InelasticSC(index,iLayer)        = rDummyArraySubs(9) * rFactors(7) 
        END DO
        CALL AppSubsidence%OverwriteParameters(AppGrid,ElasticSC,InelasticSC)
    ELSE
        DO indx=1,NWrite
          CALL OverwriteFile%ReadData(rDummyArrayNoSubs,iStat)  ;  IF (iStat .EQ. -1) RETURN
          iNode = INT(rDummyArrayNoSubs(1))
          CALL ConvertID_To_Index(iNode,NodeIDs,index)
          IF (index .EQ. 0) THEN
              CALL LogMessage('Node number '//TRIM(IntTotext(iNode))//' listed for aquifer parameter overwrite is not part of the model! Skipping...',f_iInfo,ThisProcedure)
              CYCLE
          END IF
          iLayer = INT(rDummyArrayNoSubs(2))
          IF (rDummyArrayNoSubs(3) .GE. 0.0)  GWNodes%Kh(index,iLayer)         = rDummyArrayNoSubs(3) * rFactors(1)   
          IF (rDummyArrayNoSubs(4) .GE. 0.0)  GWNodes%Ss(index,iLayer)         = rDummyArrayNoSubs(4) * rFactors(2)   
          IF (rDummyArrayNoSubs(5) .GE. 0.0)  GWNodes%Sy(index,iLayer)         = rDummyArrayNoSubs(5) * rFactors(3)   
          IF (rDummyArrayNoSubs(6) .GE. 0.0)  GWNodes%AquitardKv(index,iLayer) = rDummyArrayNoSubs(6) * rFactors(4)   
          IF (rDummyArrayNoSubs(7) .GE. 0.0)  GWNodes%Kv(index,iLayer)         = rDummyArrayNoSubs(7) * rFactors(5)   
        END DO
    END IF
    
    !Close file
    CALL OverwriteFile%Kill()

  END SUBROUTINE OverwriteParameters
  
  
  ! -------------------------------------------------------------
  ! --- UPDATES RHS VECTOR (USED IN LINE SERACH FOR NEWTON METHOD AND IN JFNK METHOD)
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRHS(AppGW,AppGrid,Stratigraphy,NetElemSource,Matrix)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: NetElemSource(:)
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: NNodes,iLayer,indxNode,iNodes(1)
    REAL(8)           :: NetElemSourceNode(AppGrid%NNodes),rUpdateRHS(1),rStor(AppGrid%NNodes,Stratigraphy%NLayers) , &
                         rdStor(AppGrid%NNodes,Stratigraphy%NLayers)
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Initialize
    NNodes = AppGrid%NNodes
    
    !Storage at each node
    CALL AppGW%GetNodalStorages(AppGrid,Stratigraphy,rStor,rdStor)
    
    !Compute element transmissivities
    CALL ComputeElemTransmissivities(AppGrid,Stratigraphy,AppGW)
    
    !Compute effect of change in storage (also set the diagonal for inactive nodes to 1.0)
    CALL ApplyChangeInStorage_ToRHS(NNodes,Stratigraphy,AppGW,Matrix)
    
    !Compute effect of horizontal flows on r.h.s vector and coefficient matrix
    CALL ApplyHorizontalFlows_ToRHS(AppGrid,Stratigraphy,AppGW,Matrix)
    
    !Compute effect of subsidence
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%ComputeRHS(Stratigraphy,AppGW%State%Head,AppGW%State%Head_P,rStor,Matrix)
    
    !Effect of net source to top active layer
    CALL AppGrid%ElemData_To_NodeData(NetElemSource,NetElemSourceNode)
    !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxNode,iLayer,iNodes,rUpdateRHS) 
    DO indxNode=1,NNodes
        iLayer = Stratigraphy%TopActiveLayer(indxNode)
        IF (iLayer .LT. 1) CYCLE
        iNodes(1)     = (iLayer-1)*NNodes + indxNode
        rUpdateRHS(1) = - NetElemSourceNode(indxNode)
        CALL Matrix%UpdateRHS(iCompIDs,iNodes,rUpdateRHS)
    END DO
    !$OMP END PARALLEL DO
    
    !Simulate tile drains/subsurface irrigation
    IF (AppGW%lTileDrain_Defined)  &
        CALL AppGW%AppTileDrain%ComputeRHS(NNodes,AppGW%State%Head,Matrix)
    
    !Effect of vertical flows
    IF (Stratigraphy%NLayers .GT. 1)  &
        CALL ApplyVerticalFlows_ToRHS(Stratigraphy,NNodes,AppGW%Nodes%LeakageV,AppGW%State,Matrix)
    
    !Simulate pumping/recharge
    IF (AppGW%lPumping_Defined)  &
        CALL AppGW%AppPumping%ComputeRHS(AppGrid,Stratigraphy,rStor,Matrix)
    
    !Simulate conversion of gw that is above ground surface to surface flow
    IF (AppGW%lSimulateGWReturnFlow)  &
        CALL ComputeGWReturnFlow_RHS(AppGrid,Stratigraphy,AppGW,Matrix)
    
    !Simulate boundary conditions (must be the last to be simulated for the entire simulation in case any
    !  specified head b.c. are defined; flow at specified head b.c. is equal to the computed RHS vector entry)
    IF (AppGW%lAppBC_Defined)  &
        CALL AppGW%AppBC%ComputeRHS(NNodes,AppGW%State%Head,Stratigraphy%BottomElev,rStor,Matrix)
    
  END SUBROUTINE ComputeRHS
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE GROUNDWATER FLOW AND RELATED FLOW PROCESSES
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppGW,AppGrid,Stratigraphy,NetElemSource,Matrix)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: NetElemSource(:)
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: NNodes,iLayer,indxNode,iNodes(1)
    REAL(8)           :: NetElemSourceNode(AppGrid%NNodes),rUpdateRHS(1),rStor(AppGrid%NNodes,Stratigraphy%NLayers) , &
                         rdStor(AppGrid%NNodes,Stratigraphy%NLayers)
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Inform user
    CALL EchoProgress('Simulating groundwater flows...')
    
    !Initialize
    NNodes = AppGrid%NNodes
    
    !Storage at each node
    CALL AppGW%GetNodalStorages(AppGrid,Stratigraphy,rStor,rdStor)
    
    !Compute element transmissivities
    CALL ComputeElemTransmissivities(AppGrid,Stratigraphy,AppGW)
    
    !Compute effect of change in storage (also set the diagonal for inactive nodes to 1.0)
    CALL ApplyChangeInStorage(NNodes,Stratigraphy,AppGW,Matrix)
    
    !Compute effect of horizontal flows on r.h.s vector and coefficient matrix
    CALL ApplyHorizontalFlows(AppGrid,Stratigraphy,AppGW,Matrix)
    
    !Compute effect of subsidence
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%Simulate(Stratigraphy,AppGW%State%Head,AppGW%State%Head_P,rStor,rdStor,Matrix)
    
    !Effect of net source to top active layer
    CALL AppGrid%ElemData_To_NodeData(NetElemSource,NetElemSourceNode)
    !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxNode,iLayer,iNodes,rUpdateRHS) 
    DO indxNode=1,NNodes
        iLayer = Stratigraphy%TopActiveLayer(indxNode)
        IF (iLayer .LT. 1) CYCLE
        iNodes(1)     = (iLayer-1)*NNodes + indxNode
        rUpdateRHS(1) = - NetElemSourceNode(indxNode)
        CALL Matrix%UpdateRHS(iCompIDs,iNodes,rUpdateRHS)
    END DO
    !$OMP END PARALLEL DO
    
    !Simulate tile drains/subsurface irrigation
    IF (AppGW%lTileDrain_Defined)  &
        CALL AppGW%AppTileDrain%Simulate(NNodes,AppGW%State%Head,Matrix)
    
    !Effect of vertical flows
    IF (Stratigraphy%NLayers .GT. 1)  &
        CALL ApplyVerticalFlows(Stratigraphy,NNodes,AppGW%Nodes%LeakageV,AppGW%State,Matrix)
    
    !Simulate pumping/recharge
    IF (AppGW%lPumping_Defined)  &
        CALL AppGW%AppPumping%Simulate(AppGrid,Stratigraphy,rStor,rdStor,Matrix)
    
    !Simulate conversion of gw that is above ground surface to surface flow
    IF (AppGW%lSimulateGWReturnFlow)  &
        CALL ComputeGWReturnFlow(AppGrid,Stratigraphy,AppGW,Matrix)
    
    !Simulate boundary conditions (must be the last to be simulated for the entire simulation in case any
    !  specified head b.c. are defined; flow at specified head b.c. is equal to the computed RHS vector entry)
    IF (AppGW%lAppBC_Defined)  &
        CALL AppGW%AppBC%Simulate(NNodes,AppGW%State%Head,Stratigraphy%BottomElev,rStor,rdStor,Matrix)
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF AQUIFER RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppGW,NewUnit)
    CLASS(AppGWType)            :: AppGW
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert time unit of aquifer parameters
    Factor                 = TimeIntervalConversion(NewUnit,AppGW%VarTimeUnit)
    AppGW%VarTimeUnit      = NewUnit
    AppGW%Nodes%Kh         = AppGW%Nodes%Kh         * Factor
    AppGW%Nodes%Kv         = AppGW%Nodes%Kv         * Factor
    AppGW%Nodes%AquitardKv = AppGW%Nodes%AquitardKv * Factor
    AppGW%Nodes%LeakageV   = AppGW%Nodes%LeakageV   * Factor
    
    !Convert time unit for boundary conditions
    CALL AppGW%AppBC%ConvertTimeUnit(NewUnit)
    
    !Convert time unit for tile drain parameters
    IF (AppGW%lTileDrain_Defined)  &
        CALL AppGW%AppTileDrain%ConvertTimeUnit(NewUnit)
  
  END SUBROUTINE ConvertTimeUnit


  ! -------------------------------------------------------------
  ! --- UPDATE PUMPING DISTRIBUTION FACTORS
  ! -------------------------------------------------------------
  SUBROUTINE UpdatePumpDistFactors(AppGW,WellDestConnector,ElemPumpDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)
    CLASS(AppGWType)                                :: AppGW
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    INTEGER,INTENT(IN)                              :: iDestType
    REAL(8),INTENT(IN)                              :: DestAgArea(:),DestUrbArea(:)
    
    CALL AppGW%AppPumping%UpdatePumpDistFactors(WellDestConnector,ElemPumpDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)

  END SUBROUTINE UpdatePumpDistFactors
  
  
  ! -------------------------------------------------------------
  ! --- RESET PUMPING IRRIGATION FRACTIONS TO THOSE READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE ResetIrigFracs(AppGW)
    CLASS(AppGWType) :: AppGW
    
    CALL AppGW%AppPumping%ResetIrigFracs()
 
  END SUBROUTINE ResetIrigFracs
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE PUMPING TO MEET DEMAND GOES TO MODELED DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE CheckSupplyDestinationConnection(AppGW,WellDestConnector,ElemPumpDestConnector,iStat)
    CLASS(AppGWType),INTENT(IN)                     :: AppGW
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    INTEGER,INTENT(OUT)                             :: iStat
  
    CALL AppGW%AppPumping%CheckSupplyDestinationConnection(WellDestConnector,ElemPumpDestConnector,iStat)
    
  END SUBROUTINE CheckSupplyDestinationConnection 
  
  
  ! -------------------------------------------------------------
  ! --- MODIFY HEADS USING A DELTA TERM
  ! -------------------------------------------------------------
  SUBROUTINE UpdateHeads(AppGW,HDelta)
    CLASS(AppGWType)   :: AppGW
    REAL(8),INTENT(IN) :: HDelta(:)
    
    AppGW%State%Head = AppGW%State%Head + RESHAPE(-HDelta,SHAPE(AppGW%State%Head))
    
  END SUBROUTINE UpdateHeads
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE AQUIFER STORAGE
  ! -------------------------------------------------------------
  SUBROUTINE UpdateStorage(AppGW,AppGrid,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    CALL ComputeRegionalStorage(AppGrid,Stratigraphy,AppGW)
    
    IF (AppGW%lSubsidence_Defined) CALL AppGW%AppSubsidence%UpdateSubsidence(AppGrid,Stratigraphy,AppGW%State%Head,AppGW%State%Head_P)

  END SUBROUTINE UpdateStorage
  
  
  ! -------------------------------------------------------------
  ! --- PREPARE GW BUDGET BINARY FILE HEADER DATA
  ! -------------------------------------------------------------
  FUNCTION PrepareGWBudgetHeader(NTIME,TimeStep,AppGrid) RESULT(Header)
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(BudgetHeaderType)        :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 256  , &
                                   NTitles            = 4    , &
                                   NColumnHeaderLines = 4    
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    INTEGER                     :: iCount,indxLocation,indxCol,NRegions,I
    CHARACTER(LEN=18),PARAMETER :: FParts(f_iNGWBudColumns) = ['PERC'               ,&
                                                               'BEGIN_STORAGE'      ,& 
                                                               'END_STORAGE'        ,& 
                                                               'DEEP_PERC'          ,& 
                                                               'GAIN_FROM_STRM'     ,& 
                                                               'RECHARGE'           ,& 
                                                               'GAIN_FROM_LAKE'     ,& 
                                                               'BOUNDARY_INFLOW'    ,& 
                                                               'SUBSIDENCE'         ,& 
                                                               'SUBSURF_IRRIGATION' ,& 
                                                               'TILE_DRAINS'        ,& 
                                                               'PUMPING'            ,&
                                                               'GW_RETURN_FLOW'     ,&
                                                               'FLOW_TO_ROOTZONE'   ,&
                                                               'NET_SUBSURF_INFLOW' ,&
                                                               'DISCREPANCY'        ,&
                                                               'CUM_SUBSIDENCE'     ]
    
    !Initialize
    NRegions = AppGrid%NSubregions
                                                      
    !Increment the initial simulation time to represent the data begin date for budget binary output files  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    TextTime = ArrangeText(TRIM(UnitT),17)
    
    !Budget descriptor
    Header%cBudgetDescriptor = 'groundwater budget'

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    ALLOCATE (Header%Areas(NRegions+1))
    Header%NAreas            = NRegions + 1
    Header%Areas(1:NRegions) = AppGrid%AppSubregion%Area
    Header%Areas(NRegions+1) = SUM(AppGrid%AppSubregion%Area)

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles) , pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM GROUNDWATER PACKAGE (v4.0-'//TRIM(IWFMKernelVersion%GetVersion())//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('GROUNDWATER BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,*(F13.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    Header%NLocations = NRegions + 1
    ALLOCATE (Header%cLocationNames(NRegions+1))
    Header%cLocationNames(1:NRegions) = AppGrid%AppSubregion%Name 
    Header%cLocationNames(NRegions+1) = 'ENTIRE MODEL AREA'
    
    !Locations
    ALLOCATE (Header%Locations(1)                                                          , &
              Header%Locations(1)%cFullColumnHeaders(f_iNGWBudColumns+1)                   , &
              Header%Locations(1)%iDataColumnTypes(f_iNGWBudColumns)                       , &
              Header%Locations(1)%iColWidth(f_iNGWBudColumns+1)                            , &
              Header%Locations(1)%cColumnHeaders(f_iNGWBudColumns+1,NColumnHeaderLines)    , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = f_iNGWBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                      
      pLocation%cFullColumnHeaders(2:) = f_cBudgetColumnTitles
      pLocation%iDataColumnTypes       = [f_iVR ,&  !Percolation
                                          f_iVLB,&  !Beginning storage
                                          f_iVLE,&  !Ending storage
                                          f_iVR ,&  !Deep perc
                                          f_iVR ,&  !Gain from stream
                                          f_iVR ,&  !Recharge
                                          f_iVR ,&  !Gain from lake
                                          f_iVR ,&  !Boundary inflow
                                          f_iVR ,&  !Subsidence
                                          f_iVR ,&  !Subsurface irrigation
                                          f_iVR ,&  !Tile drain outflow
                                          f_iVR ,&  !Pumping
                                          f_iVR ,&  !GW return flow
                                          f_iVR ,&  !Outflow to root zone
                                          f_iVR ,&  !Net subsurface inflow
                                          f_iVR ,&  !Discrepancy
                                          f_iVLE]   !Cumulative subsidence
      pLocation%iColWidth              = [17,(13,I=1,f_iNGWBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        pColumnHeaders(:,1) = (/'                 ','              ','     Beginning','       Ending ','      Deep    ','     Gain from','              ','     Gain from','      Boundary','              ','    Subsurface','    Tile Drain','              ','      GW      ','  Outflow to  ','Net Subsurface','              ','    Cumulative'/)
        pColumnHeaders(:,2) = (/'      Time       ','   Percolation','      Storage ','       Storage','   Percolation','      Stream  ','      Recharge','       Lake   ','       Inflow ','    Subsidence','    Irrigation','     Outflow  ','     Pumping  ','  Return Flow ','  Root Zone   ','    Inflow    ','   Discrepancy','    Subsidence'/)
        pColumnHeaders(:,3) = (/      TextTime     ,'              ','        (+)   ','         (-)  ','       (+)    ','        (+)   ','         (+)  ','        (+)   ','        (+)   ','        (+)   ','        (+)   ','       (-)    ','       (-)    ','       (-)    ','     (-)      ','     (+)      ','       (=)    ','              '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,*(A14))'
        pFormatSpecs(2)     = '(A17,*(A14))'
        pFormatSpecs(3)     = '(A17,*(A14))'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(f_iNGWBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNGWBudColumns*(NRegions+1)) , pDSSOutput%iDataTypes(1))
      iCount = 1
      DO indxLocation=1,NRegions+1
        DO indxCol=1,f_iNGWBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_GW_BUD/'                                                //  &  !A part
                                          TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                          'VOLUME/'                                                      //  &  !C part
                                          '/'                                                            //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = f_iPER_CUM
    END ASSOCIATE

    
  END FUNCTION PrepareGWBudgetHeader
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE THE STATE OF THE GW SYSTEM IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppGW,Stratigraphy)
    CLASS(AppGWType)                  :: AppGW
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    !Local variables
    INTEGER :: indxNode,indxLayer,iNLayers,iNNodes
    
    !Initialize
    iNLayers = Stratigraphy%NLayers
    iNNodes  = SIZE(AppGW%State%Head,DIM=1)
    
    AppGW%State%Head_P = AppGW%State%Head
    DO indxLayer=1,iNLayers
        DO indxNode=1,iNNodes
            IF (AppGW%State%Head(indxNode,indxLayer) .GE. Stratigraphy%TopElev(indxNode,indxLayer)) THEN
                AppGW%State%Storativity_P(indxNode,indxLayer) = AppGW%Nodes%Ss(indxNode,indxLayer)
            ELSE
                AppGW%State%Storativity_P(indxNode,indxLayer) = AppGW%Nodes%Sy(indxNode,indxLayer)
            END IF
        END DO
    END DO
    
    !Advance subregional storages
    AppGW%RegionalStorage_P = AppGW%RegionalStorage
    
    !Advance state for subsidence
    CALL AppGW%AppSubsidence%AdvanceState()
    
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL STORAGES
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRegionalStorage(AppGrid,Stratigraphy,AppGW)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType)                   :: AppGW
    
    !Local variables
    INTEGER :: indxLayer
    REAL(8) :: rNodalStor(AppGrid%NNodes,Stratigraphy%NLayers)
    
    !Regional storage is computed only when groundwater budget file is generated; return if regional storage is not needed
    IF (.NOT. ALLOCATED(AppGW%RegionalStorage)) RETURN
    
    !Get the nodal storages
    CALL AppGW%GetNodalStorages(AppGrid,Stratigraphy,rNodalStor)
    
    !Aggregate the nodal storages over layers for each subregion
    AppGW%RegionalStorage = 0.0
    DO indxLayer=1,Stratigraphy%NLayers
        AppGW%RegionalStorage(1:AppGrid%NSubregions) = AppGW%RegionalStorage(1:AppGrid%NSubregions) + AppGrid%AccumNodeValuesToSubregions(rNodalStor(:,indxLayer))
    END DO
    
    !Model-wide storage
    AppGW%RegionalStorage(AppGrid%NSubregions+1) = SUM(AppGW%RegionalStorage(1:AppGrid%NSubregions))
        
  END SUBROUTINE ComputeRegionalStorage
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE NET GW FLOW EXCHANGE BETWEEN SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION ComputeSubregionalGWFlowExchange(AppGrid,FaceFlows) RESULT(RSubFlow)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: FaceFlows(:,:)
    REAL(8)                      :: RSubFlow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER             :: indxRegion,indxRegion2,NSubregions,indxFace,iFace,iElem(2),iElemReg(2)
    REAL(8)             :: NetFlowReg1_IN
    INTEGER,ALLOCATABLE :: Faces(:)
    
    !Initialize
    NSubregions = AppGrid%NSubregions
    RSubFlow    = 0.0
    
    DO indxRegion=1,NSubregions
        DO indxRegion2=indxRegion+1,NSubregions+1
            CALL AppGrid%GetSubregionInterfaces(indxRegion,indxRegion2,Faces)
            IF (.NOT. ALLOCATED(Faces)) CYCLE
            DO indxFace=1,SIZE(Faces)
                iFace    = Faces(indxFace)
                iElem    = AppGrid%AppFace%Element(:,iFace)
                WHERE (iElem .EQ. 0) 
                  iElemReg = NSubregions+1
                ELSE WHERE
                  iElemReg = AppGrid%AppElement(iElem)%Subregion
                END WHERE
                NetFlowReg1_IN        = SUM(FaceFlows(iFace,:)) 
                RSubFlow(iElemReg(1)) = RSubFlow(iElemReg(1)) + NetFlowReg1_IN
                RSubFlow(iElemReg(2)) = RSubFlow(iElemReg(2)) - NetFlowReg1_IN 
            END DO
        END DO
    END DO
    
  END FUNCTION ComputeSubregionalGWFlowExchange
  
  
  ! -------------------------------------------------------------
  ! --- RESET THE GW HEADS TO HEADS AT THE BEGINNING OF TIME STEP
  ! --- ALSO APPLY TIME SERIES HEAD B.C.
  ! -------------------------------------------------------------
  SUBROUTINE ResetHeads(AppGW)
    CLASS(AppGWType) :: AppGW
    
    !Initialize
    AppGW%State%Head = AppGW%State%Head_P
    
    !Reset heads at time-series specified head b.c. locations
    IF (AppGW%lAppBC_Defined) CALL AppGW%AppBC%ResetSpecifiedHeadBC(AppGW%State%Head)
    
  END SUBROUTINE ResetHeads
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE AVERAGE ELEMENT TRANSMISSIVITIES AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeElemTransmissivities(AppGrid,Stratigraphy,AppGW)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType)                   :: AppGW
    
    !Local variables
    INTEGER :: indxElem,indxLayer,NVertex,Vertex(4),indxVertex,iNode
    REAL(8) :: TE,VertexArea(4),ElemArea,Kh,Head,TopElev,BottomElev 
    
    !$OMP PARALLEL DO COLLAPSE(2) DEFAULT(PRIVATE) SHARED(Stratigraphy,AppGrid,AppGW) 
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxElem=1,AppGrid%NElements
            !Initialize
            TE                    = 0.0
            Vertex                = AppGrid%Vertex(:,indxElem)
            NVertex               = AppGrid%NVertex(indxElem)
            VertexArea(1:NVertex) = AppGrid%AppElement(indxElem)%VertexArea(1:NVertex)
            ElemArea              = AppGrid%AppElement(indxElem)%Area
            
            !Iterate over vertices
            DO indxVertex=1,NVertex
                iNode      = Vertex(indxVertex)
                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) CYCLE
                TopElev    = Stratigraphy%TopElev(iNode,indxLayer)
                BottomElev = Stratigraphy%BottomElev(iNode,indxLayer)
                Kh         = AppGW%Nodes%Kh(iNode,indxLayer)
                Head       = AppGW%State%Head(iNode,indxLayer)
                TE         = TE + VertexArea(indxVertex) * Kh * MAX(MIN(Head,TopElev)-BottomElev ,  0.0)
            END DO
             
            !Store in persistent array
            AppGW%ElemTransmissivity(indxElem,indxLayer) = TE / ElemArea
            
        END DO
    END DO
    !$OMP END PARALLEL DO
    
  END SUBROUTINE ComputeElemTransmissivities

  
  ! -------------------------------------------------------------
  ! --- COMPUTE SURFACE FLOW WHEN GW HEAD IS ABOVE GROUND SURFACE
  ! --- Note: Computed only for top aquifer layer and if it doesn't have a confining layer 
  ! -------------------------------------------------------------
  SUBROUTINE ComputeGWReturnFlow(AppGrid,Stratigraphy,AppGW,Matrix)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(INOUT)     :: AppGW
    TYPE(MatrixType),INTENT(INOUT)    :: Matrix
    
    !Local variables
    INTEGER           :: indxNode,iTopActiveLayer,iNodes(1),iNNodes
    REAL(8)           :: rGSElev,rHeadDiff,rDiff,rStorativity,rConductance,rUpdateValues(1),rGWReturnFlowMax,  &
                         rGWReturnFlow,rdGWReturnFlow,rHead
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Initialize
    iNNodes = AppGrid%NNodes
    
    !Loop through nodes for first layer
    !$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(iNNodes,Stratigraphy,AppGW,Matrix,iCompIDs) 
    DO indxNode=1,iNNodes
        !Do we need to compute GW return flow? Note that, GW return flows are all set to zero during instantiation so no need to redo it 
        IF (.NOT. AppGW%lComputeGWReturnFlowAtNode(indxNode)) CYCLE
        
        iTopActiveLayer = Stratigraphy%TopActiveLayer(indxNode)
        rGSElev         = Stratigraphy%GSElev(indxNode)
        
        !Compute GW return flow
        rHead     = AppGW%State%Head(indxNode,iTopActiveLayer)
        rHeadDiff = rHead - rGSElev 
        IF (rHeadDiff .GT. 0.0) THEN
            rConductance                  = 100.0 * AppGW%Nodes%Ss(indxNode,iTopActiveLayer)                           !Choose a large conductance to quickly reduce the head down to GSE
            rGWReturnFlow                 = rHeadDiff * rConductance
            rStorativity                  = AppGW%Nodes%Sy(indxNode,iTopActiveLayer)  
            rGWReturnFlowMax              = (rHead - Stratigraphy%BottomElev(indxNode,iTopActiveLayer)) * rStorativity  !This is defined to avoid any "sloshing" of gw up and down if the GWReturnflow is too large
            AppGW%rGWReturnFlow(indxNode) = MIN(rGWReturnFlow , rGWReturnFlowMax) 
            iNodes(1)                     = (iTopActiveLayer-1)*iNNodes + indxNode
            
            !Update RHS vector
            rUpdateValues(1) = AppGW%rGWReturnFlow(indxNode)
            CALL Matrix%UpdateRHS(iCompIDs,iNodes,rUpdateValues)
            
            !Update COEFF matrix
            rDiff            = rGWReturnFlowMax - rGWReturnFlow
            rdGWReturnFlow   = 0.5d0 * rConductance * (1d0 + rHeadDiff/SQRT(rHeadDiff*rHeadDiff+f_rSmoothMaxP))
            rUpdateValues(1) = rStorativity - (0.5d0 * (1d0 + rDiff/SQRT(rDiff*rDiff+f_rSmoothMaxP))) * (rStorativity - rdGWReturnFlow)
            CALL Matrix%UpdateCOEFF(f_iGWComp,iNodes(1),1,iCompIDs,iNodes,rUpdateValues)
        ELSE
            AppGW%rGWReturnFlow(indxNode) = 0.0
        END IF
    END DO
    !$OMP END PARALLEL DO
        
  END SUBROUTINE ComputeGWReturnFlow
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE SURFACE FLOW WHEN GW HEAD IS ABOVE GROUND SURFACE TO BE USED FOR RHS VECTOR ONLY
  ! --- Note: Computed only for top aquifer layer and if it doesn't have a confining layer 
  ! -------------------------------------------------------------
  SUBROUTINE ComputeGWReturnFlow_RHS(AppGrid,Stratigraphy,AppGW,Matrix)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(INOUT)     :: AppGW
    TYPE(MatrixType),INTENT(INOUT)    :: Matrix
    
    !Local variables
    INTEGER           :: indxNode,iTopActiveLayer,iNodes(1),iNNodes
    REAL(8)           :: rGSElev,rHeadDiff,rStorativity,rConductance,rUpdateValues(1),rGWReturnFlowMax,  &
                         rGWReturnFlow,rHead
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Initialize
    iNNodes = AppGrid%NNodes
    
    !Loop through nodes for first layer
    !$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(iNNodes,Stratigraphy,AppGW,Matrix,iCompIDs) 
    DO indxNode=1,iNNodes
        !Do we need to compute GW return flow? Note that, GW return flows are all set to zero during instantiation so no need to redo it 
        IF (.NOT. AppGW%lComputeGWReturnFlowAtNode(indxNode)) CYCLE
        
        iTopActiveLayer = Stratigraphy%TopActiveLayer(indxNode)
        rGSElev         = Stratigraphy%GSElev(indxNode)
        
        !Compute GW return flow
        rHead     = AppGW%State%Head(indxNode,iTopActiveLayer)
        rHeadDiff = rHead - rGSElev 
        IF (rHeadDiff .GT. 0.0) THEN
            rConductance                  = 100.0 * AppGW%Nodes%Ss(indxNode,iTopActiveLayer)                           !Choose a large conductance to quickly reduce the head down to GSE
            rGWReturnFlow                 = rHeadDiff * rConductance
            rStorativity                  = AppGW%Nodes%Sy(indxNode,iTopActiveLayer)  
            rGWReturnFlowMax              = (rHead - Stratigraphy%BottomElev(indxNode,iTopActiveLayer)) * rStorativity  !This is defined to avoid any "sloshing" of gw up and down if the GWReturnflow is too large
            AppGW%rGWReturnFlow(indxNode) = MIN(rGWReturnFlow , rGWReturnFlowMax) 
            iNodes(1)                     = (iTopActiveLayer-1)*iNNodes + indxNode
            
            !Update RHS vector
            rUpdateValues(1) = AppGW%rGWReturnFlow(indxNode)
            CALL Matrix%UpdateRHS(iCompIDs,iNodes,rUpdateValues)            
        ELSE
            AppGW%rGWReturnFlow(indxNode) = 0.0
        END IF
    END DO
    !$OMP END PARALLEL DO
    
  END SUBROUTINE ComputeGWReturnFlow_RHS
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF HORIZONTAL FLOWS TO RHS VECTOR ONLY 
  ! -------------------------------------------------------------
  SUBROUTINE ApplyHorizontalFlows_ToRHS(AppGrid,Stratigraphy,AppGW,Matrix)  
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxElem,indxVertex_I,Vertex(4),NVertex,iNode,NNodes,iDim,iBase,   &
                         indxVertex_J,indx,iRow,jCol,jNode,iGWNode     
    REAL(8)           :: rHead_I,rHead_J,rAlpha,rValue,rHeadDiff,ElemTransmissivity,rFactor_I,        &
                         rFlow(4),rHeadArray(4),rUpdateRHS(AppGrid%NNodes*Stratigraphy%NLayers),      &
                         Integral_DELShpI_DELShpJ(6),rBottomElevs(4),rExp,rSaturatedThick,rFactor,    &
                         rExpPlusOne,rSaturatedThick_I,rExp_I
    INTEGER,PARAMETER :: iCompIDs(4) = [f_iGWComp , f_iGWComp , f_iGWComp , f_iGWComp] 
    REAL(8),PARAMETER :: f_rHugeNumber = 1d100
    
    !Initialize
    NNodes     = AppGrid%NNodes
    rUpdateRHS = 0.0
    
    LAYER_LOOP:  &
    !*********
    DO indxLayer=1,Stratigraphy%NLayers
        iBase = (indxLayer-1) * NNodes
        ELEMENT_LOOP:  &
        !***********
        DO indxElem=1,AppGrid%NElements
            !Initialize
            ElemTransmissivity               = AppGW%ElemTransmissivity(indxElem,indxLayer)
            IF (ElemTransmissivity .EQ. 0.0) CYCLE
            Vertex                           = AppGrid%Vertex(:,indxElem)
            NVertex                          = AppGrid%NVertex(indxElem)
            rHeadArray(1:NVertex)            = AppGW%State%Head(Vertex(1:NVertex),indxLayer)
            rBottomElevs(1:NVertex)          = Stratigraphy%BOttomElev(Vertex(1:NVertex),indxLayer)
            iDim                             = SIZE(AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ)
            Integral_DELShpI_DELShpJ(1:iDim) = AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ
            
            !Iterate over vertices
            OUTER_VERTEX_LOOP:  &
            !****************
            DO indxVertex_I=1,NVertex
                iNode     = Vertex(indxVertex_I)
                iGWNode   = iBase + iNode
                !Skip computations if node is inactive
                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) THEN
                    CYCLE
                END IF
                rFlow             = 0.0
                rHead_I           = rHeadArray(indxVertex_I)
                rSaturatedThick_I = rHead_I - (rBottomElevs(indxVertex_I) + f_rScaleElevation)  !Start scaling down transmissivity a bit above bottom elevation so that head doesn't fall below the bottom too much
                rExp_I            = FEXP(-f_rSmoothStepP * rSaturatedThick_I)
                IF (rExp_I .LT. f_rHugeNumber) THEN
                    rExpPlusOne = 1d0 + rExp_I
                    rFactor_I   = 1d0 / rExpPlusOne
                END IF

                INNER_VERTEX_LOOP:   &
                !****************
                DO indxVertex_J=1,NVertex
                    IF (indxVertex_J .EQ. indxVertex_I) CYCLE
                    jNode          = Vertex(indxVertex_J)
                    IF (.NOT. Stratigraphy%ActiveNode(jNode,indxLayer)) CYCLE
                    iRow      = MIN(indxVertex_I,indxVertex_J)
                    jCol      = MAX(indxVertex_I,indxVertex_J)
                    indx      = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
                    rAlpha    = Integral_DELShpI_DELShpJ(indx)
                    IF (rAlpha .GE. 0.0) CYCLE
                    rHead_J   = rHeadArray(indxVertex_J)
                    rHeadDiff = rHead_I - rHead_J
                    rValue    = rAlpha * ElemTransmissivity
                    
                    !Calculate flow and correct it if drying node 
                    rFlow(indxVertex_J) = -rValue * rHeadDiff
                    !Outflow from node
                    IF (rFlow(indxVertex_J) .GT. 0.0) THEN
                        IF (rExp_I .LT. f_rHugeNumber) THEN
                            rFlow(indxVertex_J) = rFactor_I * rFlow(indxVertex_J)
                        ELSE
                            !This means there is no flow between the two nodes because node I is dry and no flow between nodes; no need to update matrix equation
                            rFlow(indxVertex_J) = 0.0
                        END IF
                    !Inflow to node
                    ELSE
                        rSaturatedThick = rHead_J - (rBottomElevs(indxVertex_J) + f_rScaleElevation)  !Start scaling down transmissivity a bit above bottom elevation so that head doesn't fall below the bottom too much
                        rExp            = FEXP(-f_rSmoothStepP * rSaturatedThick)
                        IF (rExp .LT. f_rHugeNumber) THEN
                            rExpPlusOne         = 1d0 + rExp
                            rFactor             = 1d0 / rExpPlusOne
                            rFlow(indxVertex_J) = rFactor * rFlow(indxVertex_J)
                        ELSE
                            !This means there is no flow between the two nodes because node J is dry and no flow between nodes; no need to update matrix equation
                            rFlow(indxVertex_J) = 0.0
                        END IF
                    END IF
                    
                END DO INNER_VERTEX_LOOP
  
                !Accumulate coefficients and RHS for node I
                rUpdateRHS(iGWNode) = rUpdateRHS(iGWNode) + SUM(rFlow)
  
            END DO OUTER_VERTEX_LOOP
        END DO ELEMENT_LOOP
    END DO LAYER_LOOP
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)
    
  END SUBROUTINE ApplyHorizontalFlows_ToRHS

    
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF HORIZONTAL FLOWS TO MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyHorizontalFlows(AppGrid,Stratigraphy,AppGW,Matrix)  
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxElem,indxVertex_I,Vertex(4),NVertex,iNode,NNodes,iCount,       &
                         indxVertex_J,indx,iRow,jCol,jNode,iGWNode,iNodes(4),iDim,iBase     
    REAL(8)           :: rHead_I,rHead_J,rAlpha,rValue,rHeadDiff,ElemTransmissivity,rUpdateCOEFF(4),  &
                         rFlow(4),rHeadArray(4),rUpdateRHS(AppGrid%NNodes*Stratigraphy%NLayers),      &
                         Integral_DELShpI_DELShpJ(6),rBottomElevs(4),rExp,rSaturatedThick,rFactor,    &
                         rExpPlusOne,rUpdateCOEFF_Add(4),rSaturatedThick_I,rExp_I,rExpDiv_I,rFactor_I
    INTEGER,PARAMETER :: iCompIDs(4) = [f_iGWComp , f_iGWComp , f_iGWComp , f_iGWComp] 
    REAL(8),PARAMETER :: f_rHugeNumber = 1d100
    
    !Initialize
    NNodes     = AppGrid%NNodes
    rUpdateRHS = 0.0
    
    LAYER_LOOP:  &
    !*********
    DO indxLayer=1,Stratigraphy%NLayers
        iBase = (indxLayer-1) * NNodes
        ELEMENT_LOOP:  &
        !***********
        DO indxElem=1,AppGrid%NElements
            !Initialize
            ElemTransmissivity               = AppGW%ElemTransmissivity(indxElem,indxLayer)
            IF (ElemTransmissivity .EQ. 0.0) CYCLE
            Vertex                           = AppGrid%Vertex(:,indxElem)
            NVertex                          = AppGrid%NVertex(indxElem)
            rHeadArray(1:NVertex)            = AppGW%State%Head(Vertex(1:NVertex),indxLayer)
            rBottomElevs(1:NVertex)          = Stratigraphy%BOttomElev(Vertex(1:NVertex),indxLayer)
            iDim                             = SIZE(AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ)
            Integral_DELShpI_DELShpJ(1:iDim) = AppGrid%AppElement(indxElem)%Integral_DELShpI_DELShpJ
            
            !Iterate over vertices
            OUTER_VERTEX_LOOP:  &
            !****************
            DO indxVertex_I=1,NVertex
                iNode     = Vertex(indxVertex_I)
                iGWNode   = iBase + iNode
                iNodes(1) = iGWNode
                !Skip computations if node is inactive
                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) THEN
                    CALL Matrix%SetCOEFF(f_iGWComp,iNodes(1),f_iGWComp,iNodes(1),1d0)
                    CYCLE
                END IF
                iCount            = 1
                rUpdateCOEFF      = 0.0
                rUpdateCOEFF_Add  = 0.0
                rFlow             = 0.0
                rHead_I           = rHeadArray(indxVertex_I)
                rSaturatedThick_I = rHead_I - (rBottomElevs(indxVertex_I) + f_rScaleElevation)  !Start scaling down transmissivity a bit above bottom elevation so that head doesn't fall below the bottom too much
                rExp_I            = FEXP(-f_rSmoothStepP * rSaturatedThick_I)
                IF (rExp_I .LT. f_rHugeNumber) THEN
                    rExpPlusOne = 1d0 + rExp_I
                    rExpDiv_I   = rExp_I / (rExpPlusOne*rExpPlusOne)
                    rFactor_I   = 1d0 / rExpPlusOne
                END IF

                INNER_VERTEX_LOOP:   &
                !****************
                DO indxVertex_J=1,NVertex
                    IF (indxVertex_J .EQ. indxVertex_I) CYCLE
                    iCount         = iCount + 1
                    jNode          = Vertex(indxVertex_J)
                    iNodes(iCount) = iBase + jNode
                    IF (.NOT. Stratigraphy%ActiveNode(jNode,indxLayer)) CYCLE
                    iRow      = MIN(indxVertex_I,indxVertex_J)
                    jCol      = MAX(indxVertex_I,indxVertex_J)
                    indx      = (iRow-1)*NVertex - iRow*(iRow-1)/2+jCol - iRow
                    rAlpha    = Integral_DELShpI_DELShpJ(indx)
                    IF (rAlpha .GE. 0.0) CYCLE
                    rHead_J   = rHeadArray(indxVertex_J)
                    rHeadDiff = rHead_I - rHead_J
                    rValue    = rAlpha * ElemTransmissivity
                    
                    !Calculate flow and correct it if drying node; also calculate Jacobian matrix entries 
                    rFlow(indxVertex_J) = -rValue * rHeadDiff
                    !Outflow from node
                    IF (rFlow(indxVertex_J) .GT. 0.0) THEN
                        IF (rExp_I .LT. f_rHugeNumber) THEN
                            rFlow(indxVertex_J) = rFactor_I * rFlow(indxVertex_J)
                            !Jacobian matrix -- diagonal entry for node I
                            rUpdateCOEFF_Add(indxVertex_J) = - rValue * rHeadDiff * f_rSmoothStepP * rExpDiv_I - rFactor_I * rValue
                            !Jacobian matrix -- entry for node J connecting to node I
                            rUpdateCOEFF(iCount) = rFactor_I * rValue
                        ELSE
                            !This means there is no flow between the two nodes because node I is dry and no flow between nodes; no need to update matrix equation
                            rFlow(indxVertex_J) = 0.0
                        END IF
                    !Inflow to node
                    ELSE
                        rSaturatedThick = rHead_J - (rBottomElevs(indxVertex_J) + f_rScaleElevation)  !Start scaling down transmissivity a bit above bottom elevation so that head doesn't fall below the bottom too much
                        rExp            = FEXP(-f_rSmoothStepP * rSaturatedThick)
                        IF (rExp .LT. f_rHugeNumber) THEN
                            rExpPlusOne         = 1d0 + rExp
                            rFactor             = 1d0 / rExpPlusOne
                            rFlow(indxVertex_J) = rFactor * rFlow(indxVertex_J)
                            !Jacobian matrix -- diagonal entry for node I
                            rUpdateCOEFF_Add(indxVertex_J) =  - rFactor * rValue  
                            !Jacobian matrix -- entry for node J connecting to node I
                            rUpdateCOEFF(iCount) = -rValue * rHeadDiff * f_rSmoothStepP * rExp / (rExpPlusOne*rExpPlusOne) + rFactor * rValue
                        ELSE
                            !This means there is no flow between the two nodes because node J is dry and no flow between nodes; no need to update matrix equation
                            rFlow(indxVertex_J) = 0.0
                        END IF
                    END IF
                    
                END DO INNER_VERTEX_LOOP
  
                !Accumulate coefficients and RHS for node I
                rUpdateRHS(iGWNode) = rUpdateRHS(iGWNode) + SUM(rFlow)
                rUpdateCOEFF(1)     = SUM(rUpdateCOEFF_Add) 
  
                !Update Jacobian
                CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,NVertex,iCompIDs(1:NVertex),iNodes(1:NVertex),rUpdateCOEFF(1:NVertex))
                
            END DO OUTER_VERTEX_LOOP
        END DO ELEMENT_LOOP
    END DO LAYER_LOOP
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)
    
  END SUBROUTINE ApplyHorizontalFlows

    
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF CHANGE IN STORAGE TO RHS 
  ! -------------------------------------------------------------
  SUBROUTINE ApplyChangeInStorage_ToRHS(NNodes,Stratigraphy,AppGW,Matrix)  
    INTEGER,INTENT(IN)                :: NNodes
    Type(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iGWNode
    REAL(8)           :: Storativity(NNodes,Stratigraphy%NLayers),rStorChange(NNodes,Stratigraphy%NLayers),  &
                         rUpdateRHS(NNodes*Stratigraphy%NLayers)
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Compute nodal storativity and storage change
    DO indxLayer=1,Stratigraphy%NLayers
        CALL GetChangeInStorageAtLayer(AppGW,indxLayer,NNodes,Stratigraphy,rStorChange(:,indxLayer),Storativity(:,indxLayer))
    END DO    
    
    !$OMP PARALLEL DO COLLAPSE(2) DEFAULT(SHARED) PRIVATE(indxLayer,indxNode,iGWNode) 
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxNode=1,NNodes            
            !GW Node
            iGWNode = (indxLayer-1)*NNodes + indxNode
            
            !Cycle if node is inactive, set the diagonal for the node to 1.0
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                rUpdateRHS(iGWNode) = 0.0
                CYCLE
            END IF
            
            !R.H.S. value
            rUpdateRHS(iGWNode) = rStorChange(indxNode,indxLayer)
            
        END DO
    END DO
    !$OMP END PARALLEL DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)

  END SUBROUTINE ApplyChangeInStorage_ToRHS
    

  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF CHANGE IN STORAGE TO MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyChangeInStorage(NNodes,Stratigraphy,AppGW,Matrix)  
    INTEGER,INTENT(IN)                :: NNodes
    Type(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)        :: AppGW
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iNodeIDs(1),iGWNode
    REAL(8)           :: Storativity(NNodes,Stratigraphy%NLayers),rStorChange(NNodes,Stratigraphy%NLayers),  &
                         rUpdateValues(1),rUpdateRHS(NNodes*Stratigraphy%NLayers)
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Compute nodal storativity and storage change
    DO indxLayer=1,Stratigraphy%NLayers
        CALL GetChangeInStorageAtLayer(AppGW,indxLayer,NNodes,Stratigraphy,rStorChange(:,indxLayer),Storativity(:,indxLayer))
    END DO    
    
    !$OMP PARALLEL DO COLLAPSE(2) DEFAULT(SHARED) PRIVATE(indxLayer,indxNode,iGWNode,iNodeIDs,rUpdateValues) 
    DO indxLayer=1,Stratigraphy%NLayers
        DO indxNode=1,NNodes            
            !GW Node
            iGWNode = (indxLayer-1)*NNodes + indxNode
            
            !Cycle if node is inactive, set the diagonal for the node to 1.0
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                rUpdateRHS(iGWNode) = 0.0
                CALL Matrix%SetCOEFF(f_iGWComp,iGWNode,f_iGWComp,iGWNode,1d0)
                CYCLE
            END IF
            
            !R.H.S. value
            rUpdateRHS(iGWNode) = rStorChange(indxNode,indxLayer)
            
            !Coefficient matrix
            iNodeIDs(1)      = iGWNode
            rUpdateValues(1) = Storativity(indxNode,indxLayer)
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs,iNodeIDs,rUpdateValues)

        END DO
    END DO
    !$OMP END PARALLEL DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)

  END SUBROUTINE ApplyChangeInStorage
    

  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF VERTICAL FLOWS TO RHS VECTOR ONLY 
  ! -------------------------------------------------------------
  SUBROUTINE ApplyVerticalFlows_ToRHS(Stratigraphy,NNodes,LeakageV,State,Matrix)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NNodes
    REAL(8),INTENT(IN)                :: LeakageV(:,:)
    TYPE(GWStateType),INTENT(IN)      :: State
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iBase,iActiveLayerBelow(NNodes),iLayerBelow,   &
                         iGWNode,iGWNode_Below
    REAL(8)           :: VerticalFlow(NNodes),rUpdateRHS(NNodes*Stratigraphy%NLayers)
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iGWComp,f_iGWComp]
    
    !Initialize
    rUpdateRHS = 0.0
    
    DO indxLayer=1,Stratigraphy%NLayers-1
        iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(indxLayer)
        CALL VerticalFlow_ComputeAtNodesLayer(indxLayer,NNodes,Stratigraphy,State%Head,LeakageV,VerticalFlow)
        iBase             = (indxLayer-1) * NNodes
        !$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(indxLayer,NNodes,iBase,Stratigraphy,iActiveLayerBelow,VerticalFlow,rUpdateRHS) 
        DO indxNode=1,NNodes
            !Indices for node in consideration
            iGWNode   = iBase + indxNode
            
            !Cycle if node is inactive
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
            
            !Active layer below and relevant indicies; cycle if no active layer below
            iLayerBelow   = iActiveLayerBelow(indxNode)
            IF (iLayerBelow .LE. 0) CYCLE
            iGWNode_Below = (iLayerBelow-1)*NNodes + indxNode
            
            !R.H.S. values
            rUpdateRHS(iGWNode)       = rUpdateRHS(iGWNode)       - VerticalFlow(indxNode)
            rUpdateRHS(iGWNode_Below) = rUpdateRHS(iGWNode_Below) + VerticalFlow(indxNode)
            
        END DO
        !$OMP END PARALLEL DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)

  END SUBROUTINE ApplyVerticalFlows_ToRHS
    
    
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF VERTICAL FLOWS TO MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyVerticalFlows(Stratigraphy,NNodes,LeakageV,State,Matrix)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NNodes
    REAL(8),INTENT(IN)                :: LeakageV(:,:)
    TYPE(GWStateType),INTENT(IN)      :: State
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iBase,iNodeIDs(2),iActiveLayerBelow(NNodes),iLayerBelow,   &
                         iGWNode,iGWNode_Below
    REAL(8)           :: VerticalFlow(NNodes),rUpdateCOEFF(2),rUpdateRHS(NNodes*Stratigraphy%NLayers),  &
                         rUpdateCOEFF_Keep(2),rdVertFlow_dH(NNodes),rdVertFlow_dHb(NNodes)
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iGWComp,f_iGWComp]
    
    !Initialize
    rUpdateRHS = 0.0
    
    DO indxLayer=1,Stratigraphy%NLayers-1
        iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(indxLayer)
        CALL VerticalFlow_ComputeAtNodesLayer(indxLayer,NNodes,Stratigraphy,State%Head,LeakageV,VerticalFlow)
        CALL VerticalFlow_ComputeDerivativesAtNodesLayer(indxLayer,NNodes,Stratigraphy,State%Head,LeakageV,VerticalFlow,rdVertFlow_dH,rdVertFlow_dHb)
        iBase             = (indxLayer-1) * NNodes
        !$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(indxLayer,NNodes,iBase,Stratigraphy,iActiveLayerBelow,VerticalFlow,rdVertFlow_dH,rdVertFlow_dHb,Matrix,iCompIDs,rUpdateRHS) 
        DO indxNode=1,NNodes
            !Indices for node in consideration
            iGWNode   = iBase + indxNode
            
            !Cycle if node is inactive
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
            
            !Active layer below and relevant indicies; cycle if no active layer below
            iLayerBelow   = iActiveLayerBelow(indxNode)
            IF (iLayerBelow .LE. 0) CYCLE
            iGWNode_Below = (iLayerBelow-1)*NNodes + indxNode
            
            !Node numbers 
            iNodeIDs(1) = iGWNode
            iNodeIDs(2) = iGWNode_Below
            
            !Update row of COEFF matrix for current node
            rUpdateCOEFF_Keep(1) = -rdVertFlow_dH(indxNode)
            rUpdateCOEFF_Keep(2) = -rdVertFlow_dHb(indxNode)
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodeIDs,rUpdateCOEFF)
            
            !Update row of COEFF matrix for node below current node
            rUpdateCOEFF = -rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode_Below,2,iCompIDs,iNodeIDs,rUpdateCOEFF)

            !R.H.S. values
            rUpdateRHS(iGWNode)       = rUpdateRHS(iGWNode)       - VerticalFlow(indxNode)
            rUpdateRHS(iGWNode_Below) = rUpdateRHS(iGWNode_Below) + VerticalFlow(indxNode)
            
        END DO
        !$OMP END PARALLEL DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)

  END SUBROUTINE ApplyVerticalFlows
    
    
  ! -------------------------------------------------------------
  ! --- REGISTER GW COMPONENT WITH MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(AppGrid,Stratigraphy,Matrix,iStat)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(MatrixType)                  :: Matrix
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER                    :: NNodes,NLayers,indxNode,indxLayer,nConnectedNodes,iConnectedNodes(20),  &
                                  iActiveLayerBelow(AppGrid%NNodes),iActiveLayerAbove(AppGrid%NNodes),    &
                                  iNode,iOffset
    TYPE(ConnectivityListType) :: ConnectivityLists(AppGrid%NNodes*Stratigraphy%NLayers)
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('Registering groundwater component with matrix...')
    
    !Initialize grid related variables
    NNodes  = AppGrid%NNodes
    NLayers = Stratigraphy%NLayers
    
    !Add component to matrix
    CALL Matrix%AddComponent(f_iGWComp,NNodes*NLayers,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile connectivity list for Matrix
    DO indxLayer=1,NLayers
        iActiveLayerAbove = Stratigraphy%GetAllActiveLayerAbove(indxLayer)         
        iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(indxLayer) 
        iOffset           = (indxLayer-1) * NNodes
        DO indxNode=1,NNodes
            nConnectedNodes                    = AppGrid%AppNode(indxNode)%NConnectedNode + 1
            iNode                              = iOffset + indxNode
            iConnectedNodes(1)                 = iNode
            iConnectedNodes(2:nConnectedNodes) = iOffset + AppGrid%AppNode(indxNode)%ConnectedNode
            IF (iActiveLayerBelow(indxNode) .GT. 0) THEN
                nConnectedNodes                    = nConnectedNodes + 1
                iConnectedNodes(nConnectedNodes)   = (iActiveLayerBelow(indxNode)-1)*NNodes + indxNode
            END IF
            IF (iActiveLayerAbove(indxNode) .GT. 0) THEN
                nConnectedNodes                    = nConnectedNodes + 1
                iConnectedNodes(nConnectedNodes)   = (iActiveLayerAbove(indxNode)-1)*NNodes + indxNode
            END IF
            ConnectivityLists(iNode)%nConnectedNodes = nConnectedNodes
            ALLOCATE (ConnectivityLists(iNode)%ConnectedNodes(nConnectedNodes))
            ConnectivityLists(iNode)%ConnectedNodes = iConnectedNodes(1:nConnectedNodes)            
        END DO
    END DO
    
    !Add connectivity list to Matrix
    CALL Matrix%AddConnectivity(f_iGWComp,1,NNodes*NLayers,f_iGWComp,ConnectivityLists,iStat)   
        
  END SUBROUTINE RegisterWithMatrix
    
    
  ! -------------------------------------------------------------
  ! ---TRANSFER ANY TEXT/DSS OUTPUT TO HDF FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppGW,TimeStep,NTIME,iStat)
    CLASS(AppGWType)              :: AppGW
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: NNodes,NLayers
    
    !Initialize
    NNodes  = SIZE(AppGW%Nodes%Kh , DIM=1)
    NLayers = SIZE(AppGW%Nodes%Kh , DIM=2)
    
    !Head at all nodes and user-specified hydrograph locations
    CALL AppGW%GWHyd%TransferOutputToHDF(NNodes,NLayers,NTIME,TimeStep,AppGW%FactHead,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Subsidence
    CALL AppGW%AppSubsidence%TransferOutputToHDF(NTIME,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Tile drain
    CALL AppGW%AppTileDrain%TransferOutputToHDF(NTIME,TimeStep,iStat)
    
  END SUBROUTINE TransferOutputToHDF
    
    
  ! -------------------------------------------------------------
  ! --- REMOVE ALL BOUNDARY CONDITIONS AT NODE, LAYER
  ! -------------------------------------------------------------
  SUBROUTINE RemoveBC(AppGW,iNodes,iLayers,iStat)
    CLASS(AppGWType)    :: AppGW
    INTEGER,INTENT(IN)  :: iNodes(:),iLayers(:)
    INTEGER,INTENT(OUT) :: iStat
    
    CALL AppGW%AppBC%RemoveBC(iNodes,iLayers,iStat) 
    AppGW%lAppBC_Defined = AppGW%AppBC%IsDefined()
    
  END SUBROUTINE RemoveBC
    
    
  ! -------------------------------------------------------------
  ! --- COMPILE NODES WHERE GW RETURN FLOW WILL BE COMPUTED
  ! -------------------------------------------------------------
  SUBROUTINE CompileNodesWithGWReturnFlow(Stratigraphy,AppBC,StrmGWConnector,LakeGWConnector,rGWReturnFlow,lComputeGWReturnFlow)
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(AppBCType),INTENT(IN)           :: AppBC
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
    REAL(8),INTENT(INOUT)                :: rGWReturnFlow(:)
    LOGICAL,INTENT(INOUT)                :: lComputeGWReturnflow(:)
    
    !Local variables
    INTEGER             :: iNNodes,iNode,indxLayer,indxNode,iTopActiveLayer
    REAL(8)             :: rConductance
    INTEGER,ALLOCATABLE :: iGWNodeList(:)
    REAL(8),ALLOCATABLE :: rConductances(:)
    
    !Initialize
    iNNodes = SIZE(Stratigraphy%TopActiveLayer)
    
    !Initailize arrays; assume gw return flow will be computed at all nodes initially
    rGWReturnFlow        = 0.0
    lComputeGWReturnFlow = .TRUE.
    
    !Don't calculate it if a confining layer exists or there are no active layers
    DO indxNode=1,iNNodes
        iTopActiveLayer = Stratigraphy%TopActiveLayer(indxNode)
        IF (iTopActiveLayer .LT. 1) THEN
            lComputeGWReturnFlow(indxNode) = .FALSE.
            CYCLE
        END IF
        IF (Stratigraphy%TopElev(indxNode,iTopActiveLayer) .LT. Stratigraphy%GSElev(indxNode)) lComputeGWReturnFlow(indxNode) = .FALSE.
    END DO
    
    !Don't calculate GW return flow at stream nodes only when conductance is non-zero
    CALL StrmGWConnector%GetAllGWNodes(iGWNodeList)
    CALL StrmGWConnector%GetConductances(rConductances)
    DO indxNode=1,SIZE(iGWNodeList)
        iNode = iGWNodeList(indxNode)
        IF (rConductances(indxNode) .GT. 0.0) lComputeGWReturnFlow(iNode) = .FALSE.
    END DO
    
    !Don't calculate it at lake nodes only when conductance is non-zero
    CALL LakeGWConnector%GetGWNodes(iGWNodeList)
    CALL LakeGWConnector%GetMaxConductances(rConductances)
    DO indxNode=1,SIZE(iGWNodeList)
        iNode = iGWNodeList(indxNode)
        IF (rConductances(indxNode) .GT. 0.0) lComputeGWReturnFlow(iNode) = .FALSE.
    END DO
    
    !Don't calculate it at GHB b.c. nodes only when conductance is non-zero
    DO indxLayer=1,Stratigraphy%NLayers
        CALL AppBC%GetNodesWithBCType(indxLayer,f_iGHBCID,iGWNodeList)
        DO indxNode=1,SIZE(iGWNodeList)
            iNode = iGWNodeList(indxNode)
            IF (Stratigraphy%TopActiveLayer(iNode) .EQ. indxLayer) THEN
                rConductance = AppBC%GetConductance_AtANode(iNode,indxLayer,f_iGHBCID)
                IF (rConductance .GT. 0.0) lComputeGWReturnFlow(iNode) = .FALSE.
            END IF
        END DO
    END DO
    
    !Don't calculate it at constraint GHB b.c. nodes only when conductance is non-zero
    DO indxLayer=1,Stratigraphy%NLayers
        CALL AppBC%GetNodesWithBCType(indxLayer,f_iConstrainedGHBCID,iGWNodeList)
        DO indxNode=1,SIZE(iGWNodeList)
            iNode = iGWNodeList(indxNode)
            IF (Stratigraphy%TopActiveLayer(iNode) .EQ. indxLayer) THEN
                rConductance = AppBC%GetConductance_AtANode(iNode,indxLayer,f_iConstrainedGHBCID)
                IF (rConductance .GT. 0.0) lComputeGWReturnFlow(iNode) = .FALSE.
            END IF
        END DO
    END DO

  END SUBROUTINE CompileNodesWithGWReturnFlow
    
END MODULE