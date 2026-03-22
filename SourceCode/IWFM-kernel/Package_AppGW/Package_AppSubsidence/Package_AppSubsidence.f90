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
MODULE Package_AppSubsidence
  USE IWFM_Kernel_Version     , ONLY: ReadVersion                        
  USE MessageLogger           , ONLY: SetLastMessage         , &
                                      f_iFatal
  USE IOInterface             , ONLY: GenericFileType
  USE TimeSeriesUtilities     , ONLY: TimeStepType
  USE Package_Discretization  , ONLY: AppGridType            , &
                                      StratigraphyType
  USE Package_Matrix          , ONLY: MatrixType
  USE Class_BaseAppSubsidence , ONLY: BaseAppSubsidenceType  , &
                                      f_cDescription_SubsHyd
  USE Class_AppSubsidence_v40 , ONLY: AppSubsidence_v40_Type
  USE Class_AppSubsidence_v50 , ONLY: AppSubsidence_v50_Type
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
  PUBLIC :: AppSubsidenceType      , &
            f_cDescription_SubsHyd
  
  
  ! -------------------------------------------------------------
  ! --- APPLICATION SUBSIDENCE DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppSubsidenceType
      PRIVATE
      INTEGER                                  :: iComponentVersion = 0
      LOGICAL                                  :: lDefined          = .FALSE.
      CLASS(BaseAppSubsidenceType),ALLOCATABLE :: Me
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetHydOutputFileName
      PROCEDURE,PASS   :: GetNHydrographs
      PROCEDURE,PASS   :: GetHydrographIDs
      PROCEDURE,PASS   :: GetHydrographCoordinates
      PROCEDURE,PASS   :: GetHydrographNames
      PROCEDURE,PASS   :: GetSubsidence_All
      PROCEDURE,PASS   :: GetSubsidenceAtLayer
      PROCEDURE,PASS   :: GetInterbedThickAll
      PROCEDURE,PASS   :: GetSubregionalCumSubsidence
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintParameters
      PROCEDURE,PASS   :: IsDefined
      PROCEDURE,PASS   :: TransferOutputToHDF
      PROCEDURE,PASS   :: AdvanceState
      PROCEDURE,PASS   :: OverwriteParameters
      PROCEDURE,PASS   :: ProcessSubsidenceParameters
      PROCEDURE,PASS   :: UpdateSubsidence
      PROCEDURE,PASS   :: Simulate
      PROCEDURE,PASS   :: ComputeRHS
  END TYPE AppSubsidenceType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_AppSubsidence::'

  

  
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
  ! --- NEW SUBSIDENCE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(AppSubsidence,IsForInquiry,cFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat,SubsICFile)
    CLASS(AppSubsidenceType),INTENT(OUT) :: AppSubsidence
    LOGICAL,INTENT(IN)                   :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)          :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)                   :: iGWNodeIDs(:)
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    COMPLEX,INTENT(IN)                   :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    INTEGER,INTENT(OUT)                  :: iStat 
    TYPE(GenericFileType),OPTIONAL       :: SubsICFile
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    TYPE(GenericFileType)       :: SubsidenceParamFile
    CHARACTER(:),ALLOCATABLE    :: cVersion
    
    !Initialize
    iStat = 0
    
    !Return if no filename is defined
    IF (cFileName .EQ. '') RETURN
    
    !Open subsidence file and retrieve version number
    CALL SubsidenceParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(SubsidenceParamFile,'SUBSIDENCE',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file to reset it
    CALL SubsidenceParamFile%Kill()
    
    !If instantaited, kill AppSubsidence object
    IF (ALLOCATED(AppSubsidence%Me)) CALL AppSubsidence%Kill()
    
    !Allocate memory based on version
    SELECT CASE (TRIM(cVersion))
        CASE ('4.0')
            ALLOCATE(AppSubsidence_v40_Type :: AppSubsidence%Me)
            AppSubsidence%iComponentVersion = 40
        CASE ('5.0')
            ALLOCATE(AppSubsidence_v50_Type :: AppSubsidence%Me)
            AppSubsidence%iComponentVersion = 50
        CASE DEFAULT
            CALL SetLastMessage('Subsidence Component version number is not recognized ('//TRIM(cVersion)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Now, instantiate
    CALL AppSubsidence%Me%New(IsForInquiry,cFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat,SubsICFile)
    
    !Set flag
    IF (iStat .EQ. 0) AppSubsidence%lDefined = .TRUE.
    
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
  ! --- KILL SUBSIDENCE COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppSubsidence)
    CLASS(AppSubsidenceType) :: AppSubsidence
  
    !Local variables
    TYPE(AppSubsidenceType) :: Dummy
    
    IF (.NOT. ALLOCATED(AppSubsidence%Me)) RETURN
    
    CALL AppSubsidence%Me%Kill()
    DEALLOCATE (AppSubsidence%Me)
    
    SELECT TYPE(AppSubsidence)
        TYPE IS (AppSubsidenceType)
            AppSubsidence = Dummy
    END SELECT
    
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
  ! --- GET HYDROGRAPH OUTPUT FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetHydOutputFileName(AppSubsidence,cFileName)
    CLASS(AppSubsidenceType),INTENT(IN)  :: AppSubsidence
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    !Local variables
    INTEGER :: iErrorCode
    
    DEALLOCATE (cFileName , STAT=iErrorCode)
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%GetHydOutputFileName(cFileName)
    
  END SUBROUTINE GetHydOutputFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(AppSubsidence) RESULT(NHydrographs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER                             :: NHydrographs
    
    IF (AppSubsidence%lDefined) THEN
        NHydrographs = AppSubsidence%Me%GetNHydrographs()
    ELSE
        NHydrographs = 0
    END IF
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(AppSubsidence,IDs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(OUT)                 :: IDs(:)
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%GetHydrographIDs(IDs)
    
  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(AppSubsidence,GridX,GridY,XHyd,YHyd)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(IN)                  :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)                 :: XHyd(:),YHyd(:)
    
    IF (AppSubsidence%lDefined) THEN
        CALL AppSubsidence%Me%GetHydrographCoordinates(GridX,GridY,XHyd,YHyd)
    ELSE
        XHyd = 0d0
        YHyd = 0d0
    END IF
    
 END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographNames(AppSubsidence,cNamesList)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    CHARACTER(LEN=*),INTENT(OUT)        :: cNamesList(:)  !Assumes array is previously dimensioned based on the number of hydrographs
    
    IF (AppSubsidence%lDefined) THEN
        CALL AppSubsidence%Me%GetHydrographNames(cNamesList)
    ELSE
        cNamesList = ''
    END IF
    
 END SUBROUTINE GetHydrographNames
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT (node,layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidence_All(AppSubsidence,Subs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(OUT)                 :: Subs(:,:)
    
    IF (AppSubsidence%lDefined) THEN
        CALL AppSubsidence%Me%GetSubsidence_All(Subs)
    ELSE
        Subs = 0d0
    END IF
    
  END SUBROUTINE GetSubsidence_All
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBSIDENCE AT ALL NODES OF A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidenceAtLayer(AppSubsidence,iLayer,Subs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                  :: iLayer
    REAL(8),INTENT(OUT)                 :: Subs(:)
    
    IF (AppSubsidence%lDefined) THEN
        CALL AppSubsidence%Me%GetSubsidenceAtLayer(iLayer,Subs)
    ELSE
        Subs = 0d0
    END IF
    
  END SUBROUTINE GetSubsidenceAtLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET INTERBED THICKNESS AT ALL NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInterbedThickAll(AppSubsidence,lPrevious,rInterbedThick)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    LOGICAL,INTENT(IN)                  :: lPrevious
    REAL(8),INTENT(OUT)                 :: rInterbedThick(:,:)
    
    IF (AppSubsidence%lDefined) THEN
         CALL AppSubsidence%Me%GetInterbedThickAll(lPrevious,rInterbedThick)
    ELSE
        rInterbedThick = 0d0
    END IF
    
  END SUBROUTINE GetInterbedThickAll
  

  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL CUMULATIVE SUBSIDENCE
  ! -------------------------------------------------------------
  PURE FUNCTION GetSubregionalCumSubsidence(AppSubsidence,NRegions,lPreviousTS) RESULT(Subs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                  :: NRegions
    LOGICAL,INTENT(IN)                  :: lPreviousTS
    REAL(8)                             :: Subs(NRegions)
    
    IF (AppSubsidence%lDefined) THEN
        Subs = AppSubsidence%Me%GetSubregionalCumSubsidence(NRegions,lPreviousTS)
    ELSE
        Subs = 0d0
    END IF
    
  END FUNCTION GetSubregionalCumSubsidence
  
  
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
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppSubsidence,InFile,iStat)
    CLASS(AppSubsidenceType) :: AppSubsidence
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%ReadRestartData(InFile,iStat)
    
  END SUBROUTINE ReadRestartData

  
  
  
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
  SUBROUTINE PrintRestartData(AppSubsidence,OutFile)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    TYPE(GenericFileType)               :: OutFile
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%PrintRestartData(OutFile)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PROCEDURE FOR SUBSIDENCE-RELATED PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppSubsidence,AppGrid,Stratigraphy,TimeStep,lEndOfSimulation)
    CLASS(AppSubsidenceType)          :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TImeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%PrintResults(AppGrid,Stratigraphy,TimeStep,lEndOfSimulation)
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT SUBSIDENCE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintParameters(AppSubsidence,iGWNodeIDs,NodeAreas)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                  :: iGWNodeIDs(:)
    REAL(8),INTENT(IN)                  :: NodeAreas(:)
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%PrintParameters(iGWNodeIDs,NodeAreas)

  END SUBROUTINE PrintParameters
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PRADICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- IS SUBSIDENCE DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppSubsidence) RESULT(lDefined)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    LOGICAL                             :: lDefined
    
    lDefined = AppSubsidence%lDefined
    
  END FUNCTION IsDefined
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. ENTITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- TRANSFER HEADS AT ALL NODES FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppSubsidence,NTIME,TimeStep,iStat)
    CLASS(AppSubsidenceType)      :: AppSubsidence
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    IF (AppSubsidence%lDefined) THEN
        CALL AppSubsidence%Me%TransferOutputToHDF(NTIME,TimeStep,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE TransferOutputToHDF
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE THE STATE OF THE SUBSIDENCE SYSTEM IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppSubsidence)
    CLASS(AppSubsidenceType) :: AppSubsidence
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%AdvanceState()
        
  END SUBROUTINE AdvanceState  


  ! -------------------------------------------------------------
  ! --- OVERWRITE ELASTIC AND INELASTIC STORAGE COEFFICIENTS
  ! -------------------------------------------------------------
  SUBROUTINE OverwriteParameters(AppSubsidence,AppGrid,ElasticSC,InelasticSC)
    CLASS(AppSubsidenceType)     :: AppSubsidence
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: ElasticSC(:,:),InelasticSC(:,:)
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%OverwriteParameters(AppGrid,ElasticSC,InelasticSC)
    
  END SUBROUTINE OverwriteParameters  
  
  
  ! -------------------------------------------------------------
  ! --- PROCESS SUBSIDENCE PARAMETERS TO BE USED IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE ProcessSubsidenceParameters(AppSubsidence,GWHead)
    CLASS(AppSubsidenceType) :: AppSubsidence
    REAL(8),INTENT(IN)       :: GWHead(:,:)
    
     IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%ProcessSubsidenceParameters(GWHead)
    
  END SUBROUTINE ProcessSubsidenceParameters
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE SUBSIDENCE RELATED TERMS
  ! -------------------------------------------------------------
  SUBROUTINE UpdateSubsidence(AppSubsidence,AppGrid,Stratigraphy,GWHead,GWHead_P)
    CLASS(AppSubsidenceType)          :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:)
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%UpdateSubsidence(AppGrid,Stratigraphy,GWHead,GWHead_P)
    
  END SUBROUTINE UpdateSubsidence
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SUBSIDENCE
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppSubsidence,Stratigraphy,GWHead,GWHead_P,rStorage,rdStorage,Matrix)
    CLASS(AppSubsidenceType)          :: AppSubsidence
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:),rStorage(:,:),rdStorage(:,:)
    TYPE(MatrixType)                  :: Matrix
    
    IF (AppSubsidence%lDefined) CALL AppSubsidence%Me%Simulate(Stratigraphy,GWHead,GWHead_P,rStorage,rdStorage,Matrix)
  
  END SUBROUTINE Simulate  
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE CONTRIBUTION OF SUBSIDENCE TO RHS VECTOR
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRHS(AppSubsidence,Stratigraphy,GWHead,GWHead_P,rStorage,Matrix)
    CLASS(AppSubsidenceType)          :: AppSubsidence
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:),rStorage(:,:)
    TYPE(MatrixType)                  :: Matrix
    
    IF (AppSubsidence%lDefined) THEN
        CALL AppSubsidence%Me%ComputeRHS(Stratigraphy,GWHead,GWHead_P,rStorage,Matrix)
    END IF
  
  END SUBROUTINE ComputeRHS  
  
END MODULE
