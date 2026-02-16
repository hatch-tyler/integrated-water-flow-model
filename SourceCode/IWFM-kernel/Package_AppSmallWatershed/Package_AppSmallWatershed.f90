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
!  This program is distributed in the hope that it will be useful
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
MODULE Package_AppSmallWatershed
  USE IWFM_Kernel_Version         , ONLY: ReadVersion                   
  USE TimeSeriesUtilities         , ONLY: TimeStepType                  
  USE IOInterface                 , ONLY: GenericFileType                  
  USE MessageLogger               , ONLY: SetLastMessage              , &
                                          EchoProgress                , &
                                          f_iFatal                      
  USE Package_Discretization      , ONLY: AppGridType                 , &
                                          StratigraphyType            
  USE Package_PrecipitationET     , ONLY: PrecipitationType           , &
                                          ETType                      
  USE Package_Budget              , ONLY: BudgetType                  
  USE Package_Matrix              , ONLY: MatrixType                  
  USE Class_BaseAppSmallWatershed , ONLY: BaseAppSmallWatershedType   , &
                                          f_iSWShedBaseFlowBCID       , &
                                          f_iSWShedPercFlowBCID       , &
                                          f_iBudgetType_SWShed        , & 
                                          f_iSWShedBudComp_RZ         , &
                                          f_iSWShedBudComp_GW 
  USE Class_AppSmallWatershed_v40 , ONLY: AppSmallWaterShed_v40_Type
  USE Class_AppSmallWatershed_v41 , ONLY: AppSmallWaterShed_v41_Type
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
  PUBLIC :: AppSmallWatershedType  , &
            f_iSWShedBaseFlowBCID  , &
            f_iSWShedPercFlowBCID  , &
            f_iBudgetType_SWShed   , & 
            f_iSWShedBudComp_RZ    , &
            f_iSWShedBudComp_GW 
  
  
  ! -------------------------------------------------------------
  ! --- SMALL WATERSHED DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE AppSmallWatershedType
      PRIVATE
      INTEGER                                      :: iComponentVersion = 0
      LOGICAL                                      :: lDefined          = .FALSE. !Flag to check if small watersheds are simulated
      CLASS(BaseAppSmallWatershedType),ALLOCATABLE :: Me
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_NColumns
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenAppSmallWatershed
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetNSmallWatersheds
      PROCEDURE,PASS   :: GetSmallWatershedIDs
      PROCEDURE,PASS   :: GetStreamInflows
      PROCEDURE,PASS   :: GetSubregionalGWInflows
      PROCEDURE,PASS   :: GetNodesWithBCType 
      PROCEDURE,PASS   :: GetNetBCFlowWithBCType
      PROCEDURE,PASS   :: GetNetBCFlowWithBCType_FromSmallWatershed
      PROCEDURE,PASS   :: GetBoundaryFlowAtElementNodeLayer
      PROCEDURE,PASS   :: GetBoundaryFlowAtFaceLayer_AtOneFace    
      PROCEDURE,PASS   :: GetBoundaryFlowAtFaceLayer_AtSomeFaces
      PROCEDURE,PASS   :: GetPercFlow_ForAllSmallwatersheds
      PROCEDURE,PASS   :: GetRootZonePerc_ForAllSmallwatersheds
      PROCEDURE,PASS   :: GetRootZonePerc_ForOneSmallWatershed
      PROCEDURE,PASS   :: GetGWReturnFlow_ForAllSmallwatersheds
      PROCEDURE,PASS   :: GetGWReturnFlow_ForOneSmallwatershed
      PROCEDURE,PASS   :: GetArea
      PROCEDURE,PASS   :: ReadAquiferData
      PROCEDURE,PASS   :: ReadInitialConditions
      PROCEDURE,PASS   :: ReadTSData                         
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: IsDefined
      PROCEDURE,PASS   :: IsBaseFlowSimulated                
      PROCEDURE,PASS   :: IsPercFlowSimulated               
      PROCEDURE,PASS   :: Simulate
      PROCEDURE,PASS   :: AdvanceState                      
      PROCEDURE,PASS   :: UpdateRHS                          
      PROCEDURE,PASS   :: ConvertTimeUnit 
      PROCEDURE,PASS   :: CompileBaseflowPercolationNodes
      GENERIC          :: GetBoundaryFlowAtFaceLayer => GetBoundaryFlowAtFaceLayer_AtOneFace          , &
                                                        GetBoundaryFlowAtFaceLayer_AtSomeFaces
      GENERIC          :: GetBudget_MonthlyFlows     => GetBudget_MonthlyFlows_GivenFile              , &
                                                        GetBudget_MonthlyFlows_GivenAppSmallWatershed
  END TYPE AppSmallWatershedType
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                      :: f_iModNameLen  = 27
  CHARACTER(LEN=f_iModNameLen),PARAMETER :: f_cModName     = 'Package_AppSmallWatershed::'
  
  


  
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
  ! --- NEW SMALL WATERSHED DATABASE
  ! -------------------------------------------------------------
  SUBROUTINE New(AppSWShed,IsForInquiry,cFileName,cCropCoeffFileName,cWorkingDirectory,TimeStep,NTIME,NStrmNodes,iStrmNodeIDs,AppGrid,Stratigraphy,Precip,ET,iStat,cVersionOverride) 
    CLASS(AppSmallWatershedType),INTENT(OUT) :: AppSWShed
    LOGICAL,INTENT(IN)                       :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)              :: cFileName,cCropCoeffFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)            :: TimeStep
    INTEGER,INTENT(IN)                       :: NStrmNodes,iStrmNodeIDs(NStrmNodes),NTIME
    TYPE(AppGridType),INTENT(IN)             :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)        :: Stratigraphy
    TYPE(PrecipitationType),INTENT(IN)       :: Precip
    TYPE(ETType),INTENT(IN)                  :: ET
    INTEGER,INTENT(OUT)                      :: iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)     :: cVersionOverride   !Not used here; in case we want to use another version number with child classes, instead of the version of Package_AppSmallWatershed
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+3) :: ThisProcedure = f_cModName // 'New'
    TYPE(GenericFileType)          :: MainFile
    INTEGER                        :: iGWNodeIDs(AppGrid%NNodes),iErrorCode
    CHARACTER(:),ALLOCATABLE       :: cVersion
    
    !Initialize
    iStat      = 0
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Return if no filename is specified
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating small watershed component...')
    
    !Open main input file and retrive AppSmallWatershed version number
    CALL MainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='small watershed parameters',iStat=iStat) 
    IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(MainFile,'SMALL WATERSHED',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN

    !Close file to reset it
    CALL MainFile%Kill()
    
    !If instantaited, kill AppSmallWatershed object
    IF (ALLOCATED(AppSWShed%Me)) THEN
        CALL AppSWShed%Kill()
        DEALLOCATE (AppSWShed%Me , STAT=iErrorCode)
    END IF
    
    !Allocate memory based on version
    SELECT CASE (TRIM(cVersion))
        CASE ('4.0')
            ALLOCATE(AppSmallWatershed_v40_Type :: AppSWShed%Me)
            AppSWShed%iComponentVersion = 40
        CASE ('4.1')
            ALLOCATE(AppSmallWatershed_v41_Type :: AppSWShed%Me)
            AppSWShed%iComponentVersion = 41
        CASE DEFAULT
            CALL SetLastMessage('Small Watershed Component version number is not recognized ('//TRIM(cVersion)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Now, instantiate
    CALL AppSWShed%Me%New(IsForInquiry,cFileName,cCropCoeffFileName,cWorkingDirectory,TimeStep,NTIME,NStrmNodes,iStrmNodeIDs,AppGrid,Stratigraphy,Precip,ET,iStat,cVersionOverride)
    IF (iStat .NE. 0) RETURN
    
    !If made it this far, set the flag
    AppSWShed%lDefined = .TRUE.
    IF (AppSWShed%GetNSmallWatersheds() .EQ. 0) AppSWShed%lDefined = .FALSE.
    
    !Close main input file
    CALL MainFile%Kill()
      
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
  ! --- KILL AppSmallWatershed OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppSWShed)
    CLASS(AppSmallWatershedType) :: AppSWShed
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%Kill()
        AppSWShed%lDefined          = .FALSE.
        AppSWShed%iComponentVersion = 0
    END IF
    
  END SUBROUTINE Kill
  
  
  
  
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
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(AppSWShed,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(AppSmallWatershedType),INTENT(IN)  :: AppSWShed
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetBudget_List(iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    ELSE
        ALLOCATE (iBudgetTypeList(0),iBudgetLocationTypeList(0),cBudgetDescriptions(0),cBudgetFiles(0))
    END IF
    
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET FILE COLUMNS (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppSWShed,iLocationIndex,iNCols,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iLocationIndex          
    INTEGER,INTENT(OUT)                     :: iNCols,iStat       
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetBudget_NColumns(iLocationIndex,iNCols,iStat)
    ELSE
        iNCols = 0
        iStat  = 0
    END IF
    
  END SUBROUTINE GetBudget_NColumns
     
     
  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppSWShed,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN)  :: AppSWShed
    INTEGER,INTENT(IN)                       :: iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
        
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetBudget_ColumnTitles(iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF
    
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM AppSmallWaterhded OBJECT FOR A SPECIFED WATERSHED
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppSmallWatershed(AppSWShed,iBudCompType,iSWShedIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN)  :: AppSWShed
    INTEGER,INTENT(IN)                       :: iBudCompType,iSWShedIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    CALL AppSWShed%Me%GetBudget_MonthlyFlows_GivenAppSmallWatershed(iBudCompType,iSWShedIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat) 
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppSmallWatershed

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE FOR A SPECIFED WATERSHED
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iBudCompType,iSWShedIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    INTEGER,INTENT(IN)                       :: iBudCompType,iSWShedIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    TYPE(AppSmallWatershedType) :: AppSWShed
    
    ALLOCATE (AppSmallWaterShed_v40_Type :: AppSWShed%Me)  !All small watershed versions have the same Budget file so allocate Me with any version
    CALL AppSWShed%Me%GetBudget_MonthlyFlows_GivenFile(Budget,iBudCompType,iSWShedIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat) 
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppSWShed,iSWShedIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iSWShedIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                      :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                     :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                     :: iDataTypes(:),inActualOutput,iStat
    
    CALL AppSWShed%Me%GetBudget_TSData(iSWShedIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SMALL WATERSHEDS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNSmallWatersheds(AppSWShed) RESULT(iNSWShed)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER                                 :: iNSWShed
    
    IF (AppSWShed%lDefined) THEN
        iNSWShed = AppSWShed%Me%GetNSmallWatersheds()
    ELSE
        iNSWShed = 0
    END IF
    
  END FUNCTION GetNSmallWatersheds
  
  
  ! -------------------------------------------------------------
  ! --- GET SMALL WATERSHED IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSmallWatershedIDs(AppSWShed,IDs)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(OUT)                     :: IDs(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetSmallWatershedIDs(IDs)
    ELSE
        IDs = 0
    END IF
    
  END SUBROUTINE GetSmallWatershedIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FOR ONE SMALL WATERSHED (PERCOLATION WITHIN THE SMALL WATERSHED)
  ! -------------------------------------------------------------
  PURE FUNCTION GetRootZonePerc_ForOneSmallWatershed(AppSWShed,iSWShed) RESULT(rPerc)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iSWShed
    REAL(8)                                 :: rPerc
    
    IF (AppSWShed%lDefined) THEN
        rPerc = AppSWShed%Me%GetRootZonePerc_ForOneSmallWatershed(iSWShed)
    ELSE
        rPerc = 0.0
    END IF
    
  END FUNCTION GetRootZonePerc_ForOneSmallWatershed
  
    
  ! -------------------------------------------------------------
  ! --- GET GW RETURN FLOW FOR ONE SMALL WATERSHED 
  ! -------------------------------------------------------------
  PURE FUNCTION GetGWReturnFlow_ForOneSmallWatershed(AppSWShed,iSWShed) RESULT(rGWReturnFlow)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iSWShed
    REAL(8)                                 :: rGWReturnFlow
    
    IF (AppSWShed%lDefined) THEN
        rGWReturnFlow = AppSWShed%Me%GetGWReturnFlow_ForOneSmallWatershed(iSWShed)
    ELSE
        rGWReturnFlow = 0.0
    END IF
    
  END FUNCTION GetGWReturnFlow_ForOneSmallWatershed
  
    
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FOR ALL SMALL WATERSHEDS (PERCOLATION WITHIN THE SMALL WATERSHED)
  ! -------------------------------------------------------------
  SUBROUTINE GetRootZonePerc_ForAllSmallWatersheds(AppSWShed,rPerc)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: rPerc(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetRootZonePerc_ForAllSmallWatersheds(rPerc)
    ELSE
        rPerc = 0.0
    END IF
    
  END SUBROUTINE GetRootZonePerc_ForAllSmallWatersheds
  
    
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FLOWS FOR ALL SMALL WATERSHEDS
  ! -------------------------------------------------------------
  SUBROUTINE GetPercFlow_ForAllSmallWatersheds(AppSWShed,rPercFlows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: rPercFlows(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetPercFlow_ForAllSmallWatersheds(rPercFlows)
    ELSE
        rPercFlows = 0.0
    END IF
    
  END SUBROUTINE GetPercFlow_ForAllSmallWatersheds
  
    
  ! -------------------------------------------------------------
  ! --- GET GW RETURN FLOW FOR ALL SMALL WATERSHEDS 
  ! -------------------------------------------------------------
  SUBROUTINE GetGWReturnFlow_ForAllSmallWatersheds(AppSWShed,rGWReturnFlows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: rGWReturnFlows(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetGWReturnFlow_ForAllSmallWatersheds(rGWReturnFlows)
    ELSE
        rGWReturnFlows = 0.0
    END IF
    
  END SUBROUTINE GetGWReturnFlow_ForAllSmallWatersheds
  
    
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL INFLOWS
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalGWInflows(AppSWShed,AppGrid) RESULT(RInflows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    REAL(8)                                 :: RInflows(AppGrid%NSubregions)
    
    IF (AppSWShed%lDefined) THEN
        RInflows = AppSWShed%Me%GetSubregionalGWInflows(AppGrid)
    ELSE
        RInflows = 0.0
    END IF
    
  END FUNCTION GetSubregionalGWInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET NET INFLOW INTO STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStreamInflows(AppSWShed,rQTRIB) 
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: rQTRIB(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetStreamInflows(rQTRIB)
    ELSE
        rQTRIB = 0.0
    END IF
    
  END SUBROUTINE GetStreamInflows 
  
  
  ! -------------------------------------------------------------
  ! --- GET NODES WITH BASE FLOW AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetNodesWithBCType(AppSWShed,iLayer,iBCType,iBCNodes)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iLayer,iBCType
    INTEGER,ALLOCATABLE,INTENT(OUT)         :: iBCNodes(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetNodesWithBCType(iLayer,iBCType,iBCNodes)
    ELSE
        ALLOCATE (iBCNodes(0))
    END IF
        
  END SUBROUTINE GetNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FACE FLOW AT A LAYER AT ONE FACE
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtFaceLayer_AtOneFace(AppSWShed,AppGrid,iFace,iLayer,rFlow)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    INTEGER,INTENT(IN)                      :: iFace,iLayer
    REAL(8),INTENT(OUT)                     :: rFlow
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetBoundaryFlowAtFaceLayer_AtOneFace(AppGrid,iFace,iLayer,rFlow)
    ELSE
        rFlow = 0.0
    END IF
    
  END SUBROUTINE GetBoundaryFlowAtFaceLayer_AtOneFace
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FACE FLOW AT A LAYER AT SOME FACES
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtFaceLayer_AtSomeFaces(AppSWShed,AppGrid,iFaces,iLayers,rFlows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    INTEGER,INTENT(IN)                      :: iFaces(:),iLayers(:)
    REAL(8),INTENT(OUT)                     :: rFlows(:)
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetBoundaryFlowAtFaceLayer_AtSomeFaces(AppGrid,iFaces,iLayers,rFlows)
    ELSE
        rFlows = 0.0
    END IF
    
  END SUBROUTINE GetBoundaryFlowAtFaceLayer_AtSomeFaces
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW TO ELEMENT AT ITS VERTEX WITH DEFINED FLOW TYPE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtElementNodeLayer(AppSWShed,iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iBCType,iElem,indxVertex,iLayer
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    REAL(8),INTENT(OUT)                     :: rFlow
    LOGICAL,INTENT(OUT)                     :: lAddToRHS
    
    IF (AppSWShed%lDefined) THEN
        CALL AppSWShed%Me%GetBoundaryFlowAtElementNodeLayer(iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    ELSE
        rFlow     = 0.0
        lAddToRHS = .FALSE.
    END IF

  END SUBROUTINE GetBoundaryFlowAtElementNodeLayer
  

  ! -------------------------------------------------------------
  ! --- GET NET B.C. FLOW AT A NODE WITH A SPECIFIED FLOW TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNetBCFlowWithBCType(AppSWShed,iNode,iLayer,iBCType) RESULT(rFlow)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iNode,iLayer,iBCType
    REAL(8)                                 :: rFlow
    
    IF (AppSWShed%lDefined) THEN
        rFlow = AppSWShed%Me%GetNetBCFlowWithBCType(iNode,iLayer,iBCType)
    ELSE
        rFlow = 0.0
    END IF
    
  END FUNCTION GetNetBCFlowWithBCType

  
  ! -------------------------------------------------------------
  ! --- GET NET FLOW WITH A SPECIFIED B.C. TYPE FROM A SMALL WATERSHED
  ! -------------------------------------------------------------
  FUNCTION GetNetBCFlowWithBCType_FromSmallWatershed(AppSWShed,iBCType,iSWShed) RESULT(rFlow)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iBCType,iSWShed
    REAL(8)                                 :: rFlow
    
    IF (AppSWShed%lDefined) THEN
        rFlow = AppSWShed%Me%GetNetBCFlowWithBCType_FromSmallWatershed(iBCType,iSWShed)
    ELSE
        rFlow = 0.0
    END IF
    
  END FUNCTION GetNetBCFlowWithBCType_FromSmallWatershed
  
  
  ! -------------------------------------------------------------
  ! --- GET AREA OF A SMALL WATERSHED
  ! -------------------------------------------------------------
  PURE FUNCTION GetArea(AppSWShed,iSWShed) RESULT(rArea)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iSWShed
    REAL(8)                                 :: rArea
    
    rArea = AppSWShed%Me%GetArea(iSWShed)
    
  END FUNCTION GetArea
  
  
  
  
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
  ! --- READ AQUIFER DATA
  ! --- * Note: Assumes the VarTimeUnit attribute is set previously
  ! -------------------------------------------------------------
  SUBROUTINE ReadAquiferData(AppSWShed,InFile,TimeStep,iSWShedIDs,iStat)
    CLASS(AppSmallWaterShedType),INTENT(INOUT) :: AppSWShed
    TYPE(GenericFileType),INTENT(INOUT)        :: InFile
    TYPE(TimeStepType),INTENT(IN)              :: TimeStep
    INTEGER,INTENT(IN)                         :: iSWShedIDs(:)
    INTEGER,INTENT(OUT)                        :: iStat
    
    iStat = 0
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%ReadAquiferData(InFile,TimeStep,iSWShedIDs,iStat)
    
  END SUBROUTINE ReadAquiferData
  

  ! -------------------------------------------------------------
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppSWShed,InFile,iStat)
    CLASS(AppSmallWatershedType),INTENT(INOUT) :: AppSWShed
    TYPE(GenericFileType),INTENT(INOUT)        :: InFile
    INTEGER,INTENT(OUT)                        :: iStat
    
    iStat = 0
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%ReadRestartData(InFile,iStat)
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AppSWShed,Precip,ET)
    CLASS(AppSmallWatershedType),INTENT(INOUT) :: AppSWShed
    TYPE(PrecipitationType),INTENT(IN)         :: Precip
    TYPE(ETType),INTENT(IN)                    :: ET
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%ReadTSData(Precip,ET)

  END SUBROUTINE ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialConditions(AppSWShed,InFile,iSWShedIDs,lReadRootZoneIC,iStat)
    CLASS(AppSmallWaterShedType),INTENT(INOUT) :: AppSWShed
    TYPE(GenericFileType),INTENT(INOUT)        :: InFile
    INTEGER,INTENT(IN)                         :: iSWShedIDs(:)
    LOGICAL,INTENT(IN)                         :: lReadRootZoneIC
    INTEGER,INTENT(OUT)                        :: iStat
    
    iStat = 0
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%ReadInitialConditions(InFile,iSWShedIDs,lReadRootZoneIC,iStat)
    
  END SUBROUTINE ReadInitialConditions



  
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
  ! --- PRINT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppSWShed,OutFile)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(GenericFileType),INTENT(INOUT)     :: OutFile
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%PrintRestartData(OutFile)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO PRINT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppSWShed,TimeStep,lEndOfSimulation)
    CLASS(AppSmallWatershedType)  :: AppSWShed
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%PrintResults(TimeStep,lEndOfSimulation)
       
  END SUBROUTINE PrintResults
  
  

  
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
  ! --- ARE SMALL WATERSHEDS SIMULATED?
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppSWShed) RESULT(lDefined)
    CLASS(AppSmallwatershedType),INTENT(IN) :: AppSWShed
    LOGICAL                                 :: lDefined
    
    lDefined = AppSWShed%lDefined
    
  END FUNCTION IsDefined
  

  ! -------------------------------------------------------------
  ! --- IS SMALL WATERSHED BASE FLOW SIMULATED
  ! -------------------------------------------------------------
  FUNCTION IsBaseFlowSimulated(AppSWShed) RESULT(lSimulated)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    LOGICAL                                 :: lSimulated
    
    IF (AppSWShed%lDefined) THEN
        lSimulated = AppSWShed%Me%IsBaseFlowSimulated()
    ELSE
        lSImulated = .FALSE.
    END IF
    
  END FUNCTION IsBaseFlowSimulated

  
  ! -------------------------------------------------------------
  ! --- IS SMALL WATERSHED PERCOLATION FLOW SIMULATED
  ! -------------------------------------------------------------
  FUNCTION IsPercFlowSimulated(AppSWShed) RESULT(lSimulated)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    LOGICAL                                 :: lSimulated
    
    IF (AppSWShed%lDefined) THEN
        lSimulated = AppSWShed%Me%IsPercFlowSimulated()
    ELSE
        lSImulated = .FALSE.
    END IF
    
  END FUNCTION IsPercFlowSimulated

  
  

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
  ! --- ADVANCE THE STATE OF SMALL WATERSHEDS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppSWShed)
    CLASS(AppSmallWatershedType) :: AppSWShed
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%AdvanceState()
    
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE R.H.S. VECTOR OF MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE UpdateRHS(AppSWShed,NNodes,Matrix)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: NNodes
    TYPE(MatrixType)                        :: Matrix
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%UpdateRHS(NNodes,Matrix)
    
  END SUBROUTINE UpdateRHS
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SMALL WATERSHEDS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppSWShed,TimeStep,iStat)
    CLASS(AppSmallWatershedType),INTENT(INOUT) :: AppSWShed
    TYPE(TimeStepType),INTENT(IN)              :: TimeStep
    INTEGER,INTENT(OUT)                        :: iStat
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%Simulate(TimeStep,iStat)
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF SMALL WATERSHED RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppSWShed,cNewUnit)
    CLASS(AppSmallWatershedType) :: AppSWShed
    CHARACTER(LEN=*),INTENT(IN)  :: cNewUnit
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%ConvertTimeUnit(cNewUnit)
      
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE LIST OF GW NODES RECEIVING BASEFLOW AND PERCOLATION
  ! -------------------------------------------------------------
  SUBROUTINE CompileBaseflowPercolationNodes(AppSWShed,iNLayers)
    CLASS(AppSmallWatershedType) :: AppSWShed
    INTEGER,OPTIONAL,INTENT(IN)  :: iNLayers 
    
    IF (AppSWShed%lDefined) CALL AppSWShed%Me%CompileBaseflowPercolationNodes(iNLayers)
    
  END SUBROUTINE CompileBaseflowPercolationNodes  
  
END MODULE