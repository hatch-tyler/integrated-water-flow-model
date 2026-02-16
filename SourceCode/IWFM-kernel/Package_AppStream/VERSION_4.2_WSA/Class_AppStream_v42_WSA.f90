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
MODULE Class_AppStream_v42_WSA
  USE IWFM_Kernel_Version           , ONLY: ReadVersion
  USE MessageLogger                 , ONLY: SetLastMessage                  , & 
                                            EchoProgress                    , &
                                            MessageArray                    , &
                                            f_iFatal
  USE GeneralUtilities              , ONLY: LocateInList                    , &
                                            ShellSort                       , &
                                            EstablishAbsolutePathFileName   , &
                                            StripTextUntilCharacter         , & 
                                            CleanSpecialCharacters          , &
                                            UpperCase                       , & 
                                            ArrangeText                     , &
                                            IntToText, &
                                   f_cInlineCommentChar
  USE TimeSeriesUtilities           , ONLY: TimeStepType                    , &
                                            IncrementTimeStamp
  USE IOInterface                   , ONLY: GenericFileType
  USE Package_Misc                  , ONLY: f_iStrmComp                     , &
                                            f_iFlowDest_StrmNode             
  USE Package_Discretization        , ONLY: AppGridType                     , &
                                            StratigraphyType                
  USE Package_ComponentConnectors   , ONLY: StrmGWConnectorType             , &
                                            StrmLakeConnectorType           , &
                                            f_iStrmToLakeFlow               , &
                                            f_iLakeToStrmFlow                 
  USE Package_Matrix                , ONLY: MatrixType                      
  USE Package_Budget                , ONLY: BudgetHeaderType                , &
                                            f_cVolumeUnitMarker             , &
                                            f_cLocationNameMarker           , &
                                            f_iVR                           , &
                                            f_iPER_CUM
  USE Package_PrecipitationET       , ONLY: ETType
  USE Class_AppStream_v42           , ONLY: AppStream_v42_Type              , &
                                            ReadFractionsForGW              , &
                                            CompileUpstrmNodes 
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
  PUBLIC :: AppStream_v42_WSA_Type   , &
            ReadFractionsForGW       , &
            CompileUpstrmNodes 
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION STREAMS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AppStream_v42_Type) :: AppStream_v42_WSA_Type
      REAL(8),ALLOCATABLE :: rWSAs(:)
  CONTAINS
      PROCEDURE,PASS :: SetAllComponents                                !Overwrites parent method
      PROCEDURE,PASS :: SetDynamicComponent                             !Overwrites parent method
      PROCEDURE,PASS :: KillImplementation                              !Overwrites parent method
      PROCEDURE,PASS :: PrintResults                                    !Overwrites parent method
      PROCEDURE,PASS :: GetWSAs_AtSomeNodes
      PROCEDURE,PASS :: Simulate_UsingWSA
      PROCEDURE,PASS :: ComputeRHS_UsingWSA
      PROCEDURE,PASS :: Simulate_UsingHistFlows
      PROCEDURE,PASS :: ComputeRHS_UsingHistFlows
  END TYPE AppStream_v42_WSA_Type
    
    
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNStrmBudColumns = 19
  CHARACTER(LEN=30),PARAMETER :: cBudgetColumnTitles(f_iNStrmBudColumns) = ['Upstream Inflow (+)'             , &
                                                                            'Downstream Outflow (-)'          , &
                                                                            'Tributary Inflow (+)'            , &
                                                                            'Tile Drain (+)'                  , &
                                                                            'GW Return Flow (+)'              , &
                                                                            'Runoff (+)'                      , &
                                                                            'Return Flow (+)'                 , &
                                                                            'Diversion Spills (+)'            , &
                                                                            'Pond Drain (+)'                  , &
                                                                            'Gain from GW_Inside Model (+)'   , &
                                                                            'Gain from GW_Outside Model (+)'  , &
                                                                            'Gain from Lake (+)'              , &
                                                                            'Riparian ET (-)'                 , &
                                                                            'Surface Evaporation (-)'         , &
                                                                            'Diversion (-)'                   , &
                                                                            'By-pass Flow (-)'                , &
                                                                            'Water Supply Adjustment (+)'     , & 
                                                                            'Discrepancy (=)'                 , &
                                                                            'Diversion Shortage'              ]

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 25
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'Class_AppStream_v42_WSA::'
  
  
  
CONTAINS

    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,cPackageVersion,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,ETData,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v42_WSA_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                        :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)               :: cFileName,cSimWorkingDirectory,cPackageVersion
    TYPE(TimeStepType),INTENT(IN)             :: TimeStep
    INTEGER,INTENT(IN)                        :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy      !Not used in this version
    TYPE(ETType),INTENT(IN)                   :: ETData
    TYPE(GenericFileType)                     :: BinFile
    TYPE(StrmLakeConnectorType)               :: StrmLakeConnector
    TYPE(StrmGWConnectorType)                 :: StrmGWConnector
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'SetAllComponents'
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Instantiating streams')
    
    !Read the preprocessed data for streams
    CALL AppStream%SetStaticComponentFromBinFile(BinFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Set the dynamic part of AppStream
    CALL AppStream%SetDynamicComponent(IsForInquiry,cFileName,cSimWorkingDirectory,cPackageVersion,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,ETData,StrmLakeConnector,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppStream%lRouted) THEN
        IF (AppStream%NStrmNodes .GT. 0) THEN
            IF (SIZE(AppStream%State) .EQ. 0) THEN
                MessageArray(1) = 'For proper simulation of streams, relevant stream data files must'
                MessageArray(2) = 'be specified when stream nodes are defined in Pre-Processor.'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END IF
    
  END SUBROUTINE SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC PART OF STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,cPackageVersion,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,ETData,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v42_WSA_Type)     :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory,cPackageVersion
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy      !Not used in this versions
    TYPE(ETType),INTENT(IN)           :: ETData
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'SetDynamicComponent'
    INTEGER                      :: indxNode,iReachIDs(AppStream%NReaches),iStrmNodeIDs(AppStream%NStrmNodes),indx,iNStrmNodes
    LOGICAL                      :: lRoutedStreams
    TYPE(GenericFileType)        :: MainFile
    CHARACTER                    :: cVersionFull*250  
    CHARACTER(LEN=1000)          :: ALine,DiverFileName,DiverSpecFileName,BypassSpecFileName,DiverDetailBudFileName,ReachBudRawFileName
    TYPE(BudgetHeaderType)       :: BudHeader
    CHARACTER(:),ALLOCATABLE     :: cVersionSim,cAbsPathFileName
    
    !Initailize
    iStat          = 0
    iNStrmNodes    = AppStream%NStrmNodes
    iStrmNodeIDs   = AppStream%Nodes%ID
    lRoutedStreams = AppStream%lRouted
    cVersionFull   = '4.2-' // TRIM(cPackageVersion)
  
    !Open main file
    CALL MainFile%New(FileName=cFileName,InputFile=.TRUE.,Descriptor='main stream data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that version numbers from Pre-processor and Simulation match
    CALL ReadVersion(MainFile,'STREAM',cVersionSim,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (TRIM(cVersionSim) .NE. '4.2') THEN
        MessageArray(1) = 'Stream Component versions used in Pre-Processor and Simulation must match!'
        MessageArray(2) = 'Version number in Pre-Processor = 4.2' 
        MessageArray(3) = 'Version number in Simulation    = ' // TRIM(cVersionSim)
        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Allocate memory for stream states
    IF (.NOT. ALLOCATED(AppStream%State)) ALLOCATE (AppStream%State(iNStrmNodes))
    
    !Initialize related files
    !-------------------------
    
    !Stream inflow file
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(ALine)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AppStream%StrmInflowData%New(cAbsPathFileName,cWorkingDirectory,TimeStep,iNStrmNodes,iStrmNodeIDs,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Diversion specs file name
    CALL MainFile%ReadData(DiverSpecFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        DiverSpecFileName = StripTextUntilCharacter(DiverSpecFileName,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(DiverSpecFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(DiverSpecFileName)),cWorkingDirectory,cAbsPathFileName)
        DiverSpecFileName = cAbsPathFileName
    END IF
    
    !Bypass specs file name
    CALL MainFile%ReadData(BypassSpecFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        BypassSpecFileName = StripTextUntilCharacter(BypassSpecFileName,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(BypassSpecFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(BypassSpecFileName)),cWorkingDirectory,cAbsPathFileName)
        BypassSpecFileName = cAbsPathFileName
    END IF
    
    !Diversions file name
    CALL MainFile%ReadData(DiverFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        DiverFileName = StripTextUntilCharacter(DiverFileName,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(DiverFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(DiverFileName)),cWorkingDirectory,cAbsPathFileName)
        DiverFileName = cAbsPathFileName
    END IF
    
    !Stream reach budget raw file
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        ReachBudRawFileName = StripTextUntilCharacter(ALine,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(ReachBudRawFileName)
        IF (ReachBudRawFileName .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ReachBudRawFileName)),cWorkingDirectory,cAbsPathFileName)
            ReachBudRawFileName = cAbsPathFileName 
        END IF
    END IF
    
    !Diversion details raw file
    CALL MainFile%ReadData(DiverDetailBudFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (lRoutedStreams) THEN
        DiverDetailBudFileName = StripTextUntilCharacter(DiverDetailBudFileName,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(DiverDetailBudFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(DiverDetailBudFileName)),cWorkingDirectory,cAbsPathFileName)
        DiverDetailBudFileName = cAbsPathFileName
    END IF
    
    !Diversions and bypasses
    IF (lRoutedStreams) THEN
        CALL AppStream%AppDiverBypass%New(IsForInquiry,DiverSpecFileName,BypassSpecFileName,DiverFileName,DiverDetailBudFileName,cWorkingDirectory,TRIM(cVersionFull),NTIME,TimeStep,AppStream%NStrmNodes,iStrmNodeIDs,iLakeIDs,AppStream%Reaches,AppGrid,StrmLakeConnector,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Reach IDs 
    iReachIDs = AppStream%Reaches%ID

    !Prepare reach budget output file
    IF (lRoutedStreams) THEN
        IF (ReachBudRawFileName .NE. '') THEN
            IF (IsForInquiry) THEN
                CALL AppStream%StrmReachBudRawFile%New(ReachBudRawFileName,iStat)
                IF (iStat .EQ. -1) RETURN
            ELSE
                !Sort reach IDs for budget printing in order
                ALLOCATE (AppStream%iPrintReachBudgetOrder(AppStream%NReaches))
                AppStream%iPrintReachBudgetOrder = [(indx,indx=1,AppStream%NReaches)]
                CALL ShellSort(iReachIDs,AppStream%iPrintReachBudgetOrder)
                !Restore messed iReachID array
                iReachIDs = AppStream%Reaches%ID
                !Prepare budget header
                BudHeader = PrepareStreamBudgetHeader(AppStream%NReaches,AppStream%iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,TRIM(cVersionFull),cReachNames=AppStream%Reaches%cName)
                CALL AppStream%StrmReachBudRawFile%New(ReachBudRawFileName,BudHeader,iStat)
                IF (iStat .EQ. -1) RETURN
                CALL BudHeader%Kill()
            END IF
            AppStream%StrmReachBudRawFile_Defined = .TRUE.
        END IF
    END IF
    
    !Hydrograph printing
    CALL AppStream%StrmHyd%New(lRoutedStreams,IsForInquiry,cWorkingDirectory,iNStrmNodes,iStrmNodeIDs,TimeStep,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream budget at selected nodes
    CALL AppStream%StrmNodeBudget%New(lRoutedStreams,IsForInquiry,cWorkingDirectory,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,TRIM(cVersionFull),PrepareStreamBudgetHeader,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream bed parameters for stream-gw connectivity, wetted perimeter and stream length for each node
    ALLOCATE (AppStream%rWetPerimeter(iNStrmNodes))
    CALL StrmGWConnector%CompileConductance(MainFile,AppGrid,Stratigraphy,iNStrmNodes,iStrmNodeIDs,AppStream%Nodes%BottomElev,AppStream%Nodes%rLength,iStat,AppStream%rWetPerimeter)
    IF (iStat .EQ. -1) RETURN
    
    !If non-routed streams, return at this point
    IF (.NOT. AppStream%lRouted) THEN
        CALL MainFile%Kill()
        RETURN
    END IF
    
    !Stream evaporation data
    CALL AppStream%StrmEvap%New(MainFile,TimeStep,ETData,cWorkingDirectory,iNStrmNodes,iStrmNodeIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Set the heads to the bottom elevation
    DO indxNode=1,iNStrmNodes
        AppStream%State(indxNode)%Head   = AppStream%Nodes(indxNode)%BottomElev + 2.0
        AppStream%State(indxNode)%Head_P = AppStream%State(indxNode)%Head
    END DO
    
    !Close main file
    CALL MainFile%Kill() 

    !Allocate memory for WSAs
    ALLOCATE (AppStream%rWSAs(AppStream%NStrmNodes))
    AppStream%rWSAs = 0.0

  END SUBROUTINE SetDynamicComponent
  
  
  
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL STREAM DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE KillImplementation(AppStream)
    CLASS(AppStream_v42_WSA_Type) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (AppStream%rWSAs , STAT=ErrorCode)
    
    !Kill parent data
    CALL AppStream%AppStream_v42_Type%KillImplementation()
    
  END SUBROUTINE KillImplementation
  
  
  
  
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
  ! --- GET WSAs AT SOME NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetWSAs_AtSomeNodes(AppStream,iNodes,rWSAs)
   CLASS(AppStream_v42_WSA_Type),INTENT(IN) :: AppStream
   INTEGER,INTENT(IN)                       :: iNodes(:)
   REAL(8),INTENT(OUT)                      :: rWSAs(:)
   
   rWSAs = AppStream%rWSAs(iNodes)
   
  END SUBROUTINE GetWSAs_AtSomeNodes
  
  
  
  
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
  ! --- PRINT OUT SIMULATION RESULTS WITH WSA
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppStream,TimeStep,lEndOfSimulation,rGWReturnFlows,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET,BottomElev,StrmGWConnector,StrmLakeConnector)
    CLASS(AppStream_v42_WSA_Type)          :: AppStream
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    LOGICAL,INTENT(IN)                     :: lEndOfSimulation
    REAL(8),INTENT(IN)                     :: rGWReturnFlows(:),QTRIB(:),QROFF(:),QRTRN(:),QRPONDDRAIN(:),QTDRAIN(:),QRVET(:),BottomElev(:)
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
     
    !Echo progress
    CALL EchoProgress('Printing results of stream simulation')
    
    !Print stream flow hydrographs
    IF (AppStream%StrmHyd%IsOutFileDefined()) &
      CALL AppStream%StrmHyd%PrintResults(AppStream%State,BottomElev,TimeStep,lEndOfSimulation)
    
    !Print stream reach budget
    IF (AppStream%StrmReachBudRawFile_Defined) CALL WriteStrmReachFlowsToBudRawFile(AppStream,rGWReturnFlows,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET,StrmGWConnector,StrmLakeConnector)
    
    !Print stream node budget
    IF (AppStream%StrmNodeBudget%StrmNodeBudRawFile_Defined) CALL WriteStrmNodeFlowsToBudRawFile(AppStream,rGWReturnFlows,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET,StrmGWConnector,StrmLakeConnector)
    
    !Print diversion details
    CALL AppStream%AppDiverBypass%PrintResults()
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- WRITE RAW STREAM REACH BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteStrmReachFlowsToBudRawFile(AppStream,rGWRtrnFlws,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET,StrmGWConnector,StrmLakeConnector)
    TYPE(AppStream_v42_WSA_Type)                       :: AppStream
    REAL(8),DIMENSION(AppStream%NStrmNodes),INTENT(IN) :: rGWRtrnFlws,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET
    TYPE(StrmGWConnectorType),INTENT(IN)               :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN)             :: StrmLakeConnector
    
    !Local variables
    INTEGER                               :: indxReach,indxReach1,iNode,iUpstrmReach,iUpstrmNode,indx,     &
                                             iDownstrmNode,iReach
    REAL(8)                               :: DummyArray(f_iNStrmBudColumns,AppStream%NReaches) 
    REAL(8),DIMENSION(AppStream%NReaches) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,Runoff, &
                                             ReturnFlows,StrmGWFlows_InModel,StrmGWFlows_OutModel,Error,   &
                                             LakeInflows,Diversions,Bypasses,DiversionShorts,RiparianET,   &
                                             WSA,SurfaceEvap,PondDrains,rSpills,rGWReturnFlows
    
    !Initialize           
    UpstrmFlows = 0.0
    
    !Iterate over reaches
    DO indxReach=1,AppStream%NReaches
        iReach        = AppStream%iPrintReachBudgetOrder(indxReach)
        iUpstrmNode   = AppStream%Reaches(iReach)%UpstrmNode
        iDownstrmNode = AppStream%Reaches(iReach)%DownstrmNode
        !Upstream flows
        DO indxReach1=1,AppStream%Reaches(iReach)%NUpstrmReaches
            iUpstrmReach           = AppStream%Reaches(iReach)%UpstrmReaches(indxReach1)
            iNode                  = AppStream%Reaches(iUpstrmReach)%DownstrmNode
            UpstrmFlows(indxReach) = UpstrmFlows(indxReach) + AppStream%State(iNode)%Flow
        END DO
        IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(indxReach) = UpstrmFlows(indxReach) + SUM(AppStream%StrmInflowData%Inflows(iUpstrmNode:iDownstrmNode))
        
        !Tributary flows
        TributaryFlows(indxReach) = SUM(QTRIB(iUpstrmNode:iDownstrmNode))
        
        !Inflows from tile drains
        DrainInflows(indxReach) = SUM(QTDRAIN(iUpstrmNode:iDownstrmNode))
        
        !GW return flows
        rGWReturnFlows(indxReach) = SUM(rGWRtrnFlws(iUpstrmNode:iDownstrmNode))
        
        !Runoff
        Runoff(indxReach) = SUM(QROFF(iUpstrmNode:iDownstrmNode))
        
        !Return flow
        ReturnFlows(indxReach) = SUM(QRTRN(iUpstrmNode:iDownstrmNode))
        
        !Pond drain
        PondDrains(indxReach) = SUM(QRPONDDRAIN(iUpstrmNode:iDownstrmNode))
        
        !Stream-gw interaction occuring inside model domain
        !(+: flow from stream to groundwater)
        StrmGWFlows_InModel(indxReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode,lInsideModel=.TRUE.)
        
        !Stream-gw interaction occuring ousideside model domain
        !(+: flow from stream to groundwater)
        StrmGWFlows_OutModel(indxReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode,lInsideModel=.FALSE.)
        
        !Inflow from lakes
        LakeInflows(indxReach) = 0.0
        DO indx=iUpstrmNode,iDownstrmNode
            LakeInflows(indxReach) = LakeInflows(indxReach) + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indx)
        END DO
        
        !Riparian ET
        RiparianET(indxReach) = SUM(QRVET(iUpstrmNode:iDownstrmNode))
      
        !Surface evaporation
        SurfaceEvap(indxReach) = SUM(AppStream%StrmEvap%rEvap(iUpstrmNode:iDownstrmNode))
      
        !Downstream flows
        DownstrmFlows(indxReach) = AppStream%State(AppStream%Reaches(iReach)%DownStrmNode)%Flow
        
        !WSAs
        WSA(indxReach) = SUM(AppStream%rWSAs(iUpstrmNode:iDownstrmNode))
      
    END DO
    
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetReachDiversions(AppStream%NReaches,AppStream%Reaches)
    Diversions = Diversions(AppStream%iPrintReachBudgetOrder)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetReachNetBypass(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches)
    Bypasses = Bypasses(AppStream%iPrintReachBudgetOrder)
    
    !Spills
    CALL AppStream%AppDiverBypass%GetDiversionSpillsIntoAllReaches(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches,rSpills)
    rSpills = rSpills(AppStream%iPrintReachBudgetOrder)
    
    !Error
    Error =  UpstrmFlows          &
           - DownstrmFlows        &
           + TributaryFlows       &
           + DrainInflows         &
           + rGWReturnFlows       &
           + Runoff               &
           + ReturnFlows          &
           + rSpills              &
           + PondDrains           &
           + StrmGWFlows_InModel  &
           + StrmGWFlows_OutModel &
           + LakeInflows          &
           - RiparianET           &
           - SurfaceEvap          &
           - Diversions           &
           - Bypasses             &
           + WSA 
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetReachDiversionShort(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches)
    DiversionShorts = DiversionShorts(AppStream%iPrintReachBudgetOrder)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = TributaryFlows
    DummyArray(4,:)  = DrainInflows
    DummyArray(5,:)  = rGWReturnFlows
    DummyArray(6,:)  = Runoff
    DummyArray(7,:)  = ReturnFlows
    DummyArray(8,:)  = rSpills
    DummyArray(9,:)  = PondDrains
    DummyArray(10,:) = StrmGWFlows_InModel
    DummyArray(11,:) = StrmGWFlows_OutModel
    DummyArray(12,:) = LakeInflows
    DummyArray(13,:) = RiparianET
    DummyArray(14,:) = SurfaceEvap
    DummyArray(15,:) = Diversions
    DummyArray(16,:) = Bypasses
    DUmmyArray(17,:) = WSA
    DummyArray(18,:) = Error
    DummyArray(19,:) = DiversionShorts
    
    !Print out values to binary file
    CALL AppStream%StrmReachBudRawFile%WriteData(DummyArray)

 END SUBROUTINE WriteStrmReachFlowsToBudRawFile
 
 
  ! -------------------------------------------------------------
  ! --- WRITE RAW STREAM NODE BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteStrmNodeFlowsToBudRawFile(AppStream,rGWRtrnFlws,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET,StrmGWConnector,StrmLakeConnector)
    TYPE(AppStream_v42_WSA_Type)                       :: AppStream
    REAL(8),DIMENSION(AppStream%NStrmNodes),INTENT(IN) :: rGWRtrnFlws,QTRIB,QROFF,QRTRN,QRPONDDRAIN,QTDRAIN,QRVET
    TYPE(StrmGWConnectorType),INTENT(IN)               :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN)             :: StrmLakeConnector
    
    !Local variables
    INTEGER                                               :: iNode,indxNode
    REAL(8)                                               :: DummyArray(f_iNStrmBudColumns,AppStream%StrmNodeBudget%NBudNodes) 
    REAL(8),DIMENSION(AppStream%StrmNodeBudget%NBudNodes) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,LakeInflows,  &
                                                             Runoff,ReturnFlows,StrmGWFlows_InModel,StrmGWFlows_OutModel,Error,  &
                                                             Diversions,Bypasses,DiversionShorts,RiparianET,WSA,SurfaceEvap,     &
                                                             PondDrains,rSpills,rGWReturnFlows
    INTEGER,ALLOCATABLE                                   :: UpstrmNodes(:)
    
    !Iterate over nodes
    DO indxNode=1,AppStream%StrmNodeBudget%NBudNodes
      iNode = AppStream%StrmNodeBudget%iBudNodes(indxNode)

      !Upstream flows
      CALL AppStream%GetUpstrmNodes(iNode,UpstrmNodes)
      UpstrmFlows(indxNode) = SUM(AppStream%State(UpStrmNodes)%Flow)
      IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(indxNode) = UpstrmFlows(indxNode) + AppStream%StrmInflowData%Inflows(iNode)
    
      !Tributary flows
      TributaryFlows(indxNode) = QTRIB(iNode)
      
      !Inflows from tile drains
      DrainInflows(indxNode) = QTDRAIN(iNode)
      
      !GW return flows
      rGWReturnFlows(indxNode) = rGWRtrnFlws(iNode)

      !Runoff
      Runoff(indxNode) = QROFF(iNode)

      !Return flow
      ReturnFlows(indxNode) = QRTRN(iNode)
      
      !Pond drain
      PondDrains(indxNode) = QRPONDDRAIN(iNode)
      
      !Stream-gw interaction occuring inside the model
      !(+: flow from stream to groundwater, so multiply with - to represent Gain from GW)
      StrmGWFlows_InModel(indxNode) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iNode,iNode,lInsideModel=.TRUE.)
    
      !Stream-gw interaction occuring outside the model
      !(+: flow from stream to groundwater, so multiply with - to represent Gain from GW)
      StrmGWFlows_OutModel(indxNode) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iNode,iNode,lInsideModel=.FALSE.)
    
      !Inflow from lakes
      LakeInflows(indxNode) = StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,iNode)
      
      !Riparian ET
      RiparianET(indxNode) = QRVET(iNode)
      
      !Surface evaporation
      SurfaceEvap(indxNode) = AppStream%StrmEvap%rEvap(iNode)
      
      !Downstream flows
      DownstrmFlows(indxNode) = AppStream%State(iNode)%Flow
      
      !WSAs
      WSA(indxNode) = AppStream%rWSAs(iNode)
      
    END DO
    
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetNodeDiversions(AppStream%StrmNodeBudget%iBudNodes)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetNodeNetBypass(AppStream%StrmNodeBudget%iBudNodes)
    
    !Received spills
    CALL AppStream%AppDiverBypass%GetDiversionSpillsIntoSomeNodes(AppStream%StrmNodeBudget%iBudNodes,rSpills)
      
    !Error
    Error =  UpstrmFlows          &
           - DownstrmFlows        &
           + TributaryFlows       &
           + DrainInflows         &
           + rGWReturnFlows       &
           + Runoff               &
           + ReturnFlows          &
           + rSpills              &
           + PondDrains           &
           + StrmGWFlows_InModel  &
           + StrmGWFlows_OutModel &
           + LakeInflows          &
           - RiparianET           &
           - SurfaceEvap          &
           - Diversions           &
           - Bypasses             &
           + WSA
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetNodeDiversionShort(AppStream%StrmNodeBudget%iBudNodes)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = TributaryFlows
    DummyArray(4,:)  = DrainInflows
    DummyArray(5,:)  = rGWReturnFlows
    DummyArray(6,:)  = Runoff
    DummyArray(7,:)  = ReturnFlows
    DummyArray(8,:)  = rSpills
    DummyArray(9,:)  = PondDrains
    DummyArray(10,:) = StrmGWFlows_InModel
    DummyArray(11,:) = StrmGWFlows_OutModel
    DummyArray(12,:) = LakeInflows
    DummyArray(13,:) = RiparianET
    DummyArray(14,:) = SurfaceEvap
    DummyArray(15,:) = Diversions
    DummyArray(16,:) = Bypasses
    DummyArray(17,:) = WSA
    DummyArray(18,:) = Error
    DummyArray(19,:) = DiversionShorts
    
    !Print out values to binary file
    CALL AppStream%StrmNodeBudget%StrmNodeBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteStrmNodeFlowsToBudRawFile
  
  
  

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
  ! --- CALCULATE STREAM FLOWS USING WSAs
  ! -------------------------------------------------------------
  SUBROUTINE Simulate_UsingWSA(AppStream,WSA,GWHeads,GWReturnFlow,Runoff,ReturnFlow,PondDrain,TributaryFlow,DrainInflows,RiparianET,ETData,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v42_WSA_Type) :: AppStream
    REAL(8),INTENT(IN)            :: WSA(:),GWHeads(:,:),GWReturnFlow(:),Runoff(:),ReturnFlow(:),PondDrain(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    TYPE(ETType),INTENT(IN)       :: ETData
    REAL(8),INTENT(OUT)           :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)     :: StrmGWConnector
    TYPE(StrmLakeConnectorType)   :: StrmLakeConnector
    TYPE(MatrixType)              :: Matrix
    
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,ErrorCode,iNodeIDs_Connect(1),NNodes, &
                                               NDiver,iAreaCol,iEvapCol
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,rUpdateValues(1),rArea,rEvapPot,   &
                                               rEvapAct,rValue,rBypassOut,rRipET,rWSAAdjusted
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: dFlow_dStage,Inflows,rUpdateRHS,rNetInflows,rSpills_Received
    REAL(8),ALLOCATABLE                     :: HRG(:)
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    INTEGER,PARAMETER                       :: iCompIDs_Connect(1) = [f_iStrmComp]
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes  = SIZE(GWHeads , DIM=1)
    NDiver  = AppStream%AppDiverBypass%NDiver
    Inflows = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Get groundwater heads at stream nodes
    ALLOCATE (HRG(StrmGWConnector%GetnTotalGWNodes()))
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
           
    !Compute stream flows based on rating table
    dFlow_dStage = 0.0
    DO indxNode=1,AppStream%NStrmNodes
        ASSOCIATE (pRatingTable => AppStream%Nodes(indxNode)%RatingTable , &
                   pState       => AppStream%State(indxNode)             )
            CALL pRatingTable%EvaluateAndDerivative(pState%Head,pState%Flow,dFlow_dStage(indxNode))
            pState%Flow = MAX(pState%Flow , 0.0)
        END ASSOCIATE
    END DO
    
    !Initialize bypass flows to zero (only for those that originate within the model)
    DO indx=1,AppStream%AppDiverBypass%NBypass
        IF (AppStream%AppDiverBypass%Bypasses(indx)%iNode_Exp .GT. 0) THEN
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Out      = 0.0
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Received = 0.0
        END IF
    END DO

    !Retrieve diversion spills received at each stream node computed in previous iteration
    CALL AppStream%AppDiverBypass%GetDiversionSpillsIntoAllNodes(rSpills_Received)
            
    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
          
            !Initialize
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            Bypass_Recieved = AppStream%AppDiverBypass%GetBypassReceived_AtADestination(f_iFlowDest_StrmNode,indxNode)
            
            !Inflows at the stream node with known values
            rInflow = Inflows(indxNode)                                       &    !Inflow as defined by the user
                    + GWReturnFlow(indxNode)                                  &    !GW return flow
                    + Runoff(indxNode)                                        &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                    &    !Return flow of applied water 
                    + PondDrain(indxNode)                                     &    !Pond drain from ponded ag 
                    + TributaryFlow(indxNode)                                 &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                  &    !Inflow from tile drains
                    + Bypass_Recieved                                         &    !Received by-pass flows 
                    + rSpills_Received(indxNode)                              &    !Received diversion spills
                    + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)        !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode   = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                rInflow = rInflow + AppStream%State(iNode)%Flow
            END DO
            
            !Diversion
            IF (NDiver .GT. 0) THEN
                rOutflow                                            = MIN(rInflow , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
                AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
            END IF
            
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rInflow-rOutflow,StrmLakeConnector,rBypassOut)
            rOutflow = rOutflow + rBypassOut
        
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                rRipET                   = MIN(RiparianET(indxNode) , rInflow-rOutflow)
                RiparianETFrac(indxNode) = rRipET / RiparianET(indxNode)
                rOutflow                 = rOutflow + rRipET
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
        
            !Stream evaporation
            iAreaCol = AppStream%StrmEvap%iAreaCol(indxNode)
            IF (iAreaCol .EQ. 0) THEN
                rArea = AppStream%rWetPerimeter(indxNode) * AppStream%Nodes(indxNode)%rLength
            ELSE
                rArea = AppStream%StrmEvap%StrmAreaFile%rVAlues(iAreaCol)
            END IF
            iEvapCol = AppStream%StrmEvap%iEvapCol(indxNode)
            IF (iEvapCol .EQ. 0) THEN
                rEvapPot = 0.0
            ELSE
                rEvapPot  = ETData%rValues(iEvapCol) * rArea
            END IF
            rEvapAct                           = MIN(rEvapPot , rInflow-rOutflow)
            AppStream%StrmEvap%rEvap(indxNode) = rEvapAct
            rOutflow                           = rOutflow + rEvapAct
        
            !Net inflow
            rNetInflows(indxNode) = rInflow - rOutflow
            
            !Apply WSA
            IF (WSA(indxNode) .LT. 0.0) THEN
                rWSAAdjusted              = MAX(WSA(indxNode),-rNetInflows(indxNode))
                rNetInflows(indxNode)     = rNetInflows(indxNode) + rWSAAdjusted
                AppStream%rWSAs(indxNode) = rWSAAdjusted
            ELSE
                rNetInflows(indxNode)     = rNetInflows(indxNode) + WSA(indxNode)
                AppStream%rWSAs(indxNode) = WSA(indxNode)
            END IF
        
            !Compute the matrix rhs function and its derivatives w.r.t. stream elevation
            !----------------------------------------------------------------------------
            
            !RHS function
            rUpdateRHS(indxNode) = AppStream%State(indxNode)%Flow - rNetInflows(indxNode)
            
            !Derivative of function w.r.t. stream elevation
            iNodeIDs_Connect(1) = indxNode
            rUpdateValues(1)    = dFlow_dStage(indxNode) 
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxNode,1,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
             
            !Derivative of function w.r.t. stream elevation at upstream nodes that directly feed into stream node in consideration
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode  = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                rValue = -dFlow_dStage(iNode) 
                CALL Matrix%SetCOEFF(f_iStrmComp,indxNode,f_iStrmComp,iNode,rValue)
            END DO
            
        END DO
    END DO

    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iStrmComp,1,rUpdateRHS)
    
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(f_iStrmToLakeFlow,iStrmIDs)
    CALL StrmLakeConnector%GetDestinationIDs(f_iStrmToLakeFlow,iLakeIDs)
    DO indxNode=1,SIZE(iStrmIDs)
      CALL StrmLakeConnector%SetFlow(f_iStrmToLakeFlow,iStrmIDs(indxNode),iLakeIDs(indxNode),AppStream%State(iStrmIDs(indxNode))%Flow)
    END DO

    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,rNetInflows,.TRUE.,Matrix)
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE Simulate_UsingWSA
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE EFFECT OF STREAM FLOWS USING WSAs ON RHS VECTOR ONLY
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRHS_UsingWSA(AppStream,WSA,GWHeads,GWReturnFlow,Runoff,ReturnFlow,PondDrain,TributaryFlow,DrainInflows,RiparianET,ETData,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v42_WSA_Type) :: AppStream
    REAL(8),INTENT(IN)            :: WSA(:),GWHeads(:,:),GWReturnFlow(:),Runoff(:),ReturnFlow(:),PondDrain(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    TYPE(ETType),INTENT(IN)       :: ETData
    REAL(8),INTENT(OUT)           :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)     :: StrmGWConnector
    TYPE(StrmLakeConnectorType)   :: StrmLakeConnector
    TYPE(MatrixType)              :: Matrix
    
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,ErrorCode,NNodes,NDiver,iAreaCol,iEvapCol
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,rArea,rEvapPot,rEvapAct,rBypassOut,    &
                                               rRipET,rWSAAdjusted
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: Inflows,rUpdateRHS,rNetInflows,rSpills_Received
    REAL(8),ALLOCATABLE                     :: HRG(:)
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    
    !Initialize
    NNodes  = SIZE(GWHeads , DIM=1)
    NDiver  = AppStream%AppDiverBypass%NDiver
    Inflows = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Get groundwater heads at stream nodes
    ALLOCATE (HRG(StrmGWConnector%GetnTotalGWNodes()))
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
           
    !Compute stream flows based on rating table
    DO indxNode=1,AppStream%NStrmNodes
        ASSOCIATE (pRatingTable => AppStream%Nodes(indxNode)%RatingTable , &
                   pState       => AppStream%State(indxNode)             )
            pState%Flow = MAX(pRatingTable%Evaluate(pState%Head) , 0.0)
        END ASSOCIATE
    END DO
    
    !Initialize bypass flows to zero (only for those that originate within the model)
    DO indx=1,AppStream%AppDiverBypass%NBypass
        IF (AppStream%AppDiverBypass%Bypasses(indx)%iNode_Exp .GT. 0) THEN
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Out      = 0.0
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Received = 0.0
        END IF
    END DO

    !Retrieve diversion spills received at each stream node computed in previous iteration
    CALL AppStream%AppDiverBypass%GetDiversionSpillsIntoAllNodes(rSpills_Received)
            
    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
          
            !Initialize
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            Bypass_Recieved = AppStream%AppDiverBypass%GetBypassReceived_AtADestination(f_iFlowDest_StrmNode,indxNode)
            
            !Inflows at the stream node with known values
            rInflow = Inflows(indxNode)                                       &    !Inflow as defined by the user
                    + GWReturnFlow(indxNode)                                  &    !GW return flow
                    + Runoff(indxNode)                                        &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                    &    !Return flow of applied water 
                    + PondDrain(indxNode)                                     &    !Pond drain from ponded ag 
                    + TributaryFlow(indxNode)                                 &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                  &    !Inflow from tile drains
                    + Bypass_Recieved                                         &    !Received by-pass flows 
                    + rSpills_Received(indxNode)                              &    !Received diversion spills
                    + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)        !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode   = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                rInflow = rInflow + AppStream%State(iNode)%Flow
            END DO
            
            !Diversion
            IF (NDiver .GT. 0) THEN
                rOutflow                                            = MIN(rInflow , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
                AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
            END IF
            
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rInflow-rOutflow,StrmLakeConnector,rBypassOut)
            rOutflow = rOutflow + rBypassOut
        
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                rRipET                   = MIN(RiparianET(indxNode) , rInflow-rOutflow)
                RiparianETFrac(indxNode) = rRipET / RiparianET(indxNode)
                rOutflow                 = rOutflow + rRipET
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
        
            !Stream evaporation
            iAreaCol = AppStream%StrmEvap%iAreaCol(indxNode)
            IF (iAreaCol .EQ. 0) THEN
                rArea = AppStream%rWetPerimeter(indxNode) * AppStream%Nodes(indxNode)%rLength
            ELSE
                rArea = AppStream%StrmEvap%StrmAreaFile%rVAlues(iAreaCol)
            END IF
            iEvapCol = AppStream%StrmEvap%iEvapCol(indxNode)
            IF (iEvapCol .EQ. 0) THEN
                rEvapPot = 0.0
            ELSE
                rEvapPot  = ETData%rValues(iEvapCol) * rArea
            END IF
            rEvapAct                           = MIN(rEvapPot , rInflow-rOutflow)
            AppStream%StrmEvap%rEvap(indxNode) = rEvapAct
            rOutflow                           = rOutflow + rEvapAct
        
            !Net inflow
            rNetInflows(indxNode) = rInflow - rOutflow
            
            !Apply WSA
            IF (WSA(indxNode) .LT. 0.0) THEN
                rWSAAdjusted              = MAX(WSA(indxNode),-rNetInflows(indxNode))
                rNetInflows(indxNode)     = rNetInflows(indxNode) + rWSAAdjusted
                AppStream%rWSAs(indxNode) = rWSAAdjusted
            ELSE
                rNetInflows(indxNode)     = rNetInflows(indxNode) + WSA(indxNode)
                AppStream%rWSAs(indxNode) = WSA(indxNode)
            END IF
        
            !RHS function
            rUpdateRHS(indxNode) = AppStream%State(indxNode)%Flow - rNetInflows(indxNode)
        END DO
    END DO

    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iStrmComp,1,rUpdateRHS)
    
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(f_iStrmToLakeFlow,iStrmIDs)
    CALL StrmLakeConnector%GetDestinationIDs(f_iStrmToLakeFlow,iLakeIDs)
    DO indxNode=1,SIZE(iStrmIDs)
      CALL StrmLakeConnector%SetFlow(f_iStrmToLakeFlow,iStrmIDs(indxNode),iLakeIDs(indxNode),AppStream%State(iStrmIDs(indxNode))%Flow)
    END DO

    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate_JFNK(NNodes,HRG,AppStream%State%Head,rNetInflows,.TRUE.,Matrix)
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE ComputeRHS_UsingWSA
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE STREAM FLOWS USING HISTORICAL FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate_UsingHistFlows(AppStream,iStrmFlowNodes,rStrmFlows,GWHeads,GWReturnFlow,Runoff,ReturnFlow,PondDrain,TributaryFlow,DrainInflows,RiparianET,ETData,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v42_WSA_Type) :: AppStream
    INTEGER,INTENT(IN)            :: iStrmFlowNodes(:)
    REAL(8),INTENT(IN)            :: rStrmFlows(:),GWHeads(:,:),GWReturnFlow(:),Runoff(:),ReturnFlow(:),PondDrain(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    TYPE(ETType),INTENT(IN)       :: ETData
    REAL(8),INTENT(OUT)           :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)     :: StrmGWConnector
    TYPE(StrmLakeConnectorType)   :: StrmLakeConnector
    TYPE(MatrixType)              :: Matrix
    
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,ErrorCode,iNodeIDs_Connect(1),NNodes,iWSAIndex, &
                                               NDiver,iAreaCol,iEvapCol
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,rUpdateValues(1),rValue,rBypassOut,rRipET,   &
                                               rArea,rEvapPot,rEvapAct
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: dFlow_dStage,Inflows,rUpdateRHS,rNetInflows,rSpills_Received
    REAL(8),ALLOCATABLE                     :: HRG(:)
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    INTEGER,PARAMETER                       :: iCompIDs_Connect(1) = [f_iStrmComp]
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes  = SIZE(GWHeads , DIM=1)
    NDiver  = AppStream%AppDiverBypass%NDiver
    Inflows = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Get groundwater heads at stream nodes
    ALLOCATE (HRG(StrmGWConnector%GetnTotalGWNodes()))
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
    
    !Compute stream flows based on rating table
    dFlow_dStage = 0.0
    DO indxNode=1,AppStream%NStrmNodes
        ASSOCIATE (pRatingTable => AppStream%Nodes(indxNode)%RatingTable , &
                   pState       => AppStream%State(indxNode)             )
            CALL pRatingTable%EvaluateAndDerivative(pState%Head,pState%Flow,dFlow_dStage(indxNode))
            pState%Flow = MAX(pState%Flow , 0.0)
        END ASSOCIATE
    END DO
    
    !Initialize bypass flows to zero (only for those that originate within the model)
    DO indx=1,AppStream%AppDiverBypass%NBypass
        IF (AppStream%AppDiverBypass%Bypasses(indx)%iNode_Exp .GT. 0) THEN
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Out      = 0.0
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Received = 0.0
        END IF
    END DO

    !Retrieve diversion spills received at each stream node computed in previous iteration
    CALL AppStream%AppDiverBypass%GetDiversionSpillsIntoAllNodes(rSpills_Received)
            
    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
          
            !Initialize
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            Bypass_Recieved = AppStream%AppDiverBypass%GetBypassReceived_AtADestination(f_iFlowDest_StrmNode,indxNode)
            
            !Inflows at the stream node with known values
            rInflow = Inflows(indxNode)                                       &    !Inflow as defined by the user
                    + GWReturnFlow(indxNode)                                  &    !GW return flow
                    + Runoff(indxNode)                                        &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                    &    !Return flow of applied water 
                    + PondDrain(indxNode)                                     &    !Pond drain from ponded ag 
                    + TributaryFlow(indxNode)                                 &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                  &    !Inflow from tile drains
                    + Bypass_Recieved                                         &    !Received by-pass flows 
                    + rSpills_Received(indxNode)                              &    !Received diversion spills
                    + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)        !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode     = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                iWSAIndex = LocateInList(iNode,iStrmFlowNodes)
                IF (iWSAIndex .EQ. 0) THEN
                    rInflow  = rInflow + AppStream%State(iNode)%Flow
                ELSE
                    rInflow = rInflow + rStrmFlows(iWSAIndex)
                END IF
            END DO
            
            !Diversion
            IF (NDiver .GT. 0) THEN
                rOutflow                                            = MIN(rInflow , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
                AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
            END IF
            
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rInflow-rOutflow,StrmLakeConnector,rBypassOut)
            rOutflow = rOutflow + rBypassOut
        
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                rRipET                   = MIN(RiparianET(indxNode) , rInflow-rOutflow)
                RiparianETFrac(indxNode) = rRipET / RiparianET(indxNode)
                rOutflow                 = rOutflow + rRipET
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
        
            !Stream evaporation
            iAreaCol = AppStream%StrmEvap%iAreaCol(indxNode)
            IF (iAreaCol .EQ. 0) THEN
                rArea = AppStream%rWetPerimeter(indxNode) * AppStream%Nodes(indxNode)%rLength
            ELSE
                rArea = AppStream%StrmEvap%StrmAreaFile%rVAlues(iAreaCol)
            END IF
            iEvapCol = AppStream%StrmEvap%iEvapCol(indxNode)
            IF (iEvapCol .EQ. 0) THEN
                rEvapPot = 0.0
            ELSE
                rEvapPot  = ETData%rValues(iEvapCol) * rArea
            END IF
            rEvapAct                           = MIN(rEvapPot , rInflow-rOutflow)
            AppStream%StrmEvap%rEvap(indxNode) = rEvapAct
            rOutflow                           = rOutflow + rEvapAct
        
            !Net inflow
            rNetInflows(indxNode) = rInflow - rOutflow
            
            !Compute the matrix rhs function and its derivatives w.r.t. stream elevation
            !----------------------------------------------------------------------------
            
            !RHS function
            rUpdateRHS(indxNode) = AppStream%State(indxNode)%Flow - rNetInflows(indxNode)
            
            !Derivative of function w.r.t. stream elevation
            iNodeIDs_Connect(1) = indxNode
            rUpdateValues(1)    = dFlow_dStage(indxNode) 
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxNode,1,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
             
            !Derivative of function w.r.t. stream elevation at upstream nodes that directly feed into stream node in consideration
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode  = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                rValue = -dFlow_dStage(iNode) 
                CALL Matrix%SetCOEFF(f_iStrmComp,indxNode,f_iStrmComp,iNode,rValue)
            END DO
            
        END DO
    END DO

    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iStrmComp,1,rUpdateRHS)
    
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(f_iStrmToLakeFlow,iStrmIDs)
    CALL StrmLakeConnector%GetDestinationIDs(f_iStrmToLakeFlow,iLakeIDs)
    DO indxNode=1,SIZE(iStrmIDs)
      CALL StrmLakeConnector%SetFlow(f_iStrmToLakeFlow,iStrmIDs(indxNode),iLakeIDs(indxNode),AppStream%State(iStrmIDs(indxNode))%Flow)
    END DO

    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,rNetInflows,.TRUE.,Matrix)
    
    !Impose specified flows
    DO indxNode=1,SIZE(iStrmFlowNodes)
        iNode                       = iStrmFlowNodes(indxNode)
        AppStream%rWSAs(iNode)      = rStrmFlows(indxNode) - AppStream%State(iNode)%Flow
        AppStream%State(iNode)%Flow = rStrmFlows(indxNode)
    END DO
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE Simulate_UsingHistFlows
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE EFFECT OF STREAM FLOWS USING HISTORICAL FLOWS ON RHS VECTOR ONLY
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRHS_UsingHistFlows(AppStream,iStrmFlowNodes,rStrmFlows,GWHeads,GWReturnFlow,Runoff,ReturnFlow,PondDrain,TributaryFlow,DrainInflows,RiparianET,ETData,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v42_WSA_Type) :: AppStream
    INTEGER,INTENT(IN)            :: iStrmFlowNodes(:)
    REAL(8),INTENT(IN)            :: rStrmFlows(:),GWHeads(:,:),GWReturnFlow(:),Runoff(:),ReturnFlow(:),PondDrain(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    TYPE(ETType),INTENT(IN)       :: ETData
    REAL(8),INTENT(OUT)           :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)     :: StrmGWConnector
    TYPE(StrmLakeConnectorType)   :: StrmLakeConnector
    TYPE(MatrixType)              :: Matrix
    
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,ErrorCode,NNodes,iWSAIndex,NDiver,iAreaCol,iEvapCol
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,rBypassOut,rRipET,rArea,rEvapPot,rEvapAct
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: Inflows,rUpdateRHS,rNetInflows,rSpills_Received
    REAL(8),ALLOCATABLE                     :: HRG(:)
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes  = SIZE(GWHeads , DIM=1)
    NDiver  = AppStream%AppDiverBypass%NDiver
    Inflows = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Get groundwater heads at stream nodes
    ALLOCATE (HRG(StrmGWConnector%GetnTotalGWNodes()))
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
    
    !Compute stream flows based on rating table
    DO indxNode=1,AppStream%NStrmNodes
        ASSOCIATE (pRatingTable => AppStream%Nodes(indxNode)%RatingTable , &
                   pState       => AppStream%State(indxNode)             )
            pState%Flow = MAX(pRatingTable%Evaluate(pState%Head) , 0.0)
        END ASSOCIATE
    END DO
    
    !Initialize bypass flows to zero (only for those that originate within the model)
    DO indx=1,AppStream%AppDiverBypass%NBypass
        IF (AppStream%AppDiverBypass%Bypasses(indx)%iNode_Exp .GT. 0) THEN
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Out      = 0.0
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Received = 0.0
        END IF
    END DO

    !Retrieve diversion spills received at each stream node computed in previous iteration
    CALL AppStream%AppDiverBypass%GetDiversionSpillsIntoAllNodes(rSpills_Received)
            
    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
          
            !Initialize
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            Bypass_Recieved = AppStream%AppDiverBypass%GetBypassReceived_AtADestination(f_iFlowDest_StrmNode,indxNode)
            
            !Inflows at the stream node with known values
            rInflow = Inflows(indxNode)                                       &    !Inflow as defined by the user
                    + GWReturnFlow(indxNode)                                  &    !GW return flow
                    + Runoff(indxNode)                                        &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                    &    !Return flow of applied water 
                    + PondDrain(indxNode)                                     &    !Pond drain from ponded ag 
                    + TributaryFlow(indxNode)                                 &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                  &    !Inflow from tile drains
                    + Bypass_Recieved                                         &    !Received by-pass flows 
                    + rSpills_Received(indxNode)                              &    !Received diversion spills
                    + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)        !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode     = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                iWSAIndex = LocateInList(iNode,iStrmFlowNodes)
                IF (iWSAIndex .EQ. 0) THEN
                    rInflow  = rInflow + AppStream%State(iNode)%Flow
                ELSE
                    rInflow = rInflow + rStrmFlows(iWSAIndex)
                END IF
            END DO
            
            !Diversion
            IF (NDiver .GT. 0) THEN
                rOutflow                                            = MIN(rInflow , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
                AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
            END IF
            
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rInflow-rOutflow,StrmLakeConnector,rBypassOut)
            rOutflow = rOutflow + rBypassOut
        
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                rRipET                   = MIN(RiparianET(indxNode) , rInflow-rOutflow)
                RiparianETFrac(indxNode) = rRipET / RiparianET(indxNode)
                rOutflow                 = rOutflow + rRipET
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
        
            !Stream evaporation
            iAreaCol = AppStream%StrmEvap%iAreaCol(indxNode)
            IF (iAreaCol .EQ. 0) THEN
                rArea = AppStream%rWetPerimeter(indxNode) * AppStream%Nodes(indxNode)%rLength
            ELSE
                rArea = AppStream%StrmEvap%StrmAreaFile%rVAlues(iAreaCol)
            END IF
            iEvapCol = AppStream%StrmEvap%iEvapCol(indxNode)
            IF (iEvapCol .EQ. 0) THEN
                rEvapPot = 0.0
            ELSE
                rEvapPot  = ETData%rValues(iEvapCol) * rArea
            END IF
            rEvapAct                           = MIN(rEvapPot , rInflow-rOutflow)
            AppStream%StrmEvap%rEvap(indxNode) = rEvapAct
            rOutflow                           = rOutflow + rEvapAct
        
            !Net inflow
            rNetInflows(indxNode) = rInflow - rOutflow
            
            !RHS function
            rUpdateRHS(indxNode) = AppStream%State(indxNode)%Flow - rNetInflows(indxNode)
        END DO
    END DO

    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iStrmComp,1,rUpdateRHS)
    
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(f_iStrmToLakeFlow,iStrmIDs)
    CALL StrmLakeConnector%GetDestinationIDs(f_iStrmToLakeFlow,iLakeIDs)
    DO indxNode=1,SIZE(iStrmIDs)
      CALL StrmLakeConnector%SetFlow(f_iStrmToLakeFlow,iStrmIDs(indxNode),iLakeIDs(indxNode),AppStream%State(iStrmIDs(indxNode))%Flow)
    END DO

    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate_JFNK(NNodes,HRG,AppStream%State%Head,rNetInflows,.TRUE.,Matrix)
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE ComputeRHS_UsingHistFlows
  
  
  ! -------------------------------------------------------------
  ! --- FUNCTION TO PREPARE THE BUDGET HEADER DATA FOR STREAM BUDGETS
  ! -------------------------------------------------------------
  FUNCTION PrepareStreamBudgetHeader(NLocations,iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,cReachNames,iBudNodes) RESULT(Header)
    INTEGER,INTENT(IN)                   :: NLocations,iPrintReachBudgetOrder(:),iReachIDs(:),iStrmNodeIDs(:),NTIME
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)          :: cVersion
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cReachNames(:)
    INTEGER,OPTIONAL,INTENT(IN)          :: iBudNodes(:)
    TYPE(BudgetHeaderType)               :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 264  , &
                                   NTitles            = 3    , &
                                   NColumnHeaderLines = 4    
    INTEGER                     :: iCount,indxLocation,indxCol,indx,I,ID,iReach,iMaxPathnameLen
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    LOGICAL                     :: lNodeBudOutput
    CHARACTER(LEN=21),PARAMETER :: FParts(f_iNStrmBudColumns) = ['UPSTRM_INFLOW'         , &
                                                                 'DOWNSTRM_OUTFLOW'      , & 
                                                                 'TRIB_INFLOW'           , & 
                                                                 'TILE_DRN'              , & 
                                                                 'GW_RTRN_FLOW'          , & 
                                                                 'RUNOFF'                , & 
                                                                 'RETURN_FLOW'           , &
                                                                 'DIV_SPILL'             , &
                                                                 'POND_DRN'              , & 
                                                                 'GAIN_FRM_GW_INM'       , &
                                                                 'GAIN_FRM_GW_OUTM'      , &
                                                                 'GAIN_FROM_LAKE'        , & 
                                                                 'RIPARIAN_ET'           , &
                                                                 'SURFACE_EVAP'          , &
                                                                 'DIVERSION'             , & 
                                                                 'BYPASS'                , & 
                                                                 'WSA'                   , & 
                                                                 'DISCREPANCY'           , & 
                                                                 'DIV_SHORT'             ]
                                                             
    !Initialize flag for budget type 
    IF (PRESENT(iBudNodes)) THEN
      lNodeBudOutput = .TRUE.
    ELSE
      lNodeBudOutput = .FALSE.
    END IF
   
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
    IF (lNodeBudOutput) THEN
      Header%cBudgetDescriptor = 'stream node budget'
    ELSE
      Header%cBudgetDescriptor = 'stream reach budget'
    END IF

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    Header%NAreas = 0
    ALLOCATE (Header%Areas(0))

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM STREAM PACKAGE (v'//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('STREAM FLOW BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:2) = .TRUE.
        pASCIIOutput%lTitlePersist(3)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,*(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    Header%NLocations = NLocations
    ALLOCATE (Header%cLocationNames(NLocations))
    IF (lNodeBudOutput) THEN
      DO indx=1,NLocations
          ID                          = iStrmNodeIDs(iBudNodes(indx))
          Header%cLocationNames(indx) = 'NODE '//TRIM(IntToText(ID)) 
      END DO
    ELSE
      DO indx=1,NLocations
          iReach                      = iPrintReachBudgetOrder(indx)
          ID                          = iReachIDs(iReach)
          Header%cLocationNames(indx) = TRIM(cReachNames(iReach)) // '(REACH '// TRIM(IntToText(ID)) // ')' 
      END DO
    END IF
    
    !Locations
    ALLOCATE (Header%Locations(1)                                                          , &
              Header%Locations(1)%cFullColumnHeaders(f_iNStrmBudColumns+1)                 , &
              Header%Locations(1)%iDataColumnTypes(f_iNStrmBudColumns)                     , &
              Header%Locations(1)%iColWidth(f_iNStrmBudColumns+1)                          , &
              Header%Locations(1)%cColumnHeaders(f_iNStrmBudColumns+1,NColumnHeaderLines)  , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = f_iNStrmBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                       
      pLocation%cFullColumnHeaders(2:) = cBudgetColumnTitles                               
      pLocation%iDataColumnTypes       = [f_iVR ,&  !Upstream inflow
                                          f_iVR ,&  !Downstream outflow
                                          f_iVR ,&  !Tributary inflow
                                          f_iVR ,&  !Tile drain
                                          f_iVR ,&  !GW return flow
                                          f_iVR ,&  !Runoff
                                          f_iVR ,&  !Return flow
                                          f_iVR ,&  !Diversion spills
                                          f_iVR ,&  !Pond drain
                                          f_iVR ,&  !Gain from GW inside model
                                          f_iVR ,&  !Gain from GW outside model
                                          f_iVR ,&  !Gain from lake
                                          f_iVR ,&  !Riparian ET
                                          f_iVR ,&  !Surface evaporation 
                                          f_iVR ,&  !Diversion
                                          f_iVR ,&  !By-pass flow
                                          f_iVR ,&  !Water supply adjustment 
                                          f_iVR ,&  !Discrepancy
                                          f_iVR ]   !Diversion shortage
      pLocation%iColWidth              = [17,(13,I=1,f_iNStrmBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        TextTime            = ArrangeText(TRIM(UnitT),17)
        pColumnHeaders(:,1) = ['                 ','     Upstream','   Downstream','    Tributary','        Tile ','      GW     ','             ','     Return  ','  Diversion  ','      Pond   ','Gain from GW ',' Gain from GW','    Gain from','   Riparian ','   Surface  ','             ','     By-pass ',' Water Supply','             ','    Diversion']
        pColumnHeaders(:,2) = ['      Time       ','      Inflow ','    Outflow  ','     Inflow  ','        Drain','  Return Flow','       Runoff','      Flow   ','    Spills   ','      Drain  ','inside Model ','outside Model','      Lake   ','      ET    ',' Evaporation','    Diversion','       Flow  ','  Adjustment ','  Discrepancy','    Shortage ']
        pColumnHeaders(:,3) = [           TextTime,'       (+)   ','      (-)    ','      (+)    ','         (+) ','      (+)    ','        (+)  ','      (+)    ','     (+)     ','       (+)   ','     (+)     ','      (+)    ','       (+)   ','      (-)   ','     (-)    ','       (-)   ','       (-)   ','      (+)    ','      (=)    ','             ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,*(A13))'
        pFormatSpecs(2)     = '(A17,*(A13))'
        pFormatSpecs(3)     = '(A17,*(A13))'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(f_iNStrmBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
                                                   
    !Data for DSS output 
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
        ALLOCATE (pDSSOutput%cPathNames(f_iNStrmBudColumns*(Header%NLocations)) , pDSSOutput%iDataTypes(1))
        iMaxPathnameLen = LEN(pDSSOutput%cPathNames(1))
        iCount          = 1
        IF (lNodeBudOutput) THEN
            DO indxLocation=1,Header%NLocations
                DO indxCol=1,f_iNStrmBudColumns
                    pDSSOutput%cPathNames(iCount) = '/IWFM_STRMNODE_BUD/'                                          //  &  !A part
                                                    TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                                    'VOLUME/'                                                      //  &  !C part
                                                    '/'                                                            //  &  !D part
                                                    TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                                    TRIM(FParts(indxCol))//'/'                                            !F part
                    IF (LEN_TRIM(pDSSOutput%cPathNames(iCount)) .EQ. iMaxPathnameLen)   &
                        pDSSOutput%cPathNames(iCount)(iMaxPathnameLen:iMaxPathnameLen) = '/'
                    iCount = iCount+1
                END DO
            END DO
        ELSE
            DO indxLocation=1,Header%NLocations
                DO indxCol=1,f_iNStrmBudColumns
                    pDSSOutput%cPathNames(iCount) = '/IWFM_STRMRCH_BUD/'                                           //  &  !A part
                                                    TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                                    'VOLUME/'                                                      //  &  !C part
                                                    '/'                                                            //  &  !D part
                                                    TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                                    TRIM(FParts(indxCol))//'/'                                            !F part
                    IF (LEN_TRIM(pDSSOutput%cPathNames(iCount)) .EQ. iMaxPathnameLen)   &
                        pDSSOutput%cPathNames(iCount)(iMaxPathnameLen:iMaxPathnameLen) = '/'
                    iCount = iCount+1
                END DO
            END DO
        END IF
        pDSSOutput%iDataTypes = f_iPER_CUM
    END ASSOCIATE
    
  END FUNCTION PrepareStreamBudgetHeader  
  
END MODULE