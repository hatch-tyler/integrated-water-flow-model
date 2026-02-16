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
MODULE Class_AppSmallWatershed_v40
  USE MessageLogger               , ONLY: SetLastMessage                , &
                                          EchoProgress                  , &
                                          f_iFatal                      
  USE IOInterface                 , ONLY: GenericFileType                   
  USE GeneralUtilities            , ONLY: StripTextUntilCharacter       , &
                                          CleanSpecialCharacters        , &
                                          EstablishAbsolutePathFileName , &
                                          IntToText                     , &
                                          ConvertID_To_Index, &
                                   f_cInlineCommentChar
  USE TimeSeriesUtilities         , ONLY: TimeStepType                  , &       
                                          IsTimeIntervalValid           , &     
                                          TimeIntervalConversion                   
  USE Package_Discretization      , ONLY: AppGridType                   , &
                                          StratigraphyType                  
  USE Package_PrecipitationET     , ONLY: ETType                        , &
                                          PrecipitationType
  USE Package_UnsatZone           , ONLY: f_iKunsatMethodList                  
  USE Class_BaseAppSmallWatershed , ONLY: BaseAppSmallWatershedType
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
  PUBLIC :: AppSmallWatershed_v40_Type 
  
  
  ! -------------------------------------------------------------
  ! --- APPLICATION SMALL WATERSHED DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppSmallWatershedType) :: AppSmallWatershed_v40_Type
  CONTAINS
      PROCEDURE,PASS :: New                => AppSmallWatershed_v40_New
      PROCEDURE,PASS :: KillImplementation => AppSmallWatershed_v40_Kill
      PROCEDURE,PASS :: ReadRootZoneData   => AppSmallWaterShed_v40_ReadRootZoneData
      PROCEDURE,PASS :: ReadTSData         => AppSmallWaterShed_v40_ReadTSData
  END TYPE AppSmallWatershed_v40_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                      :: f_iModNameLen = 29
  CHARACTER(LEN=f_iModNameLen),PARAMETER :: f_cModName    = 'Class_AppSmallWatershed_v40::'
  CHARACTER(LEN=3),PARAMETER             :: f_cVersion    = '4.0'
  
  
  

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
  SUBROUTINE AppSmallWatershed_v40_New(AppSWShed,IsForInquiry,cFileName,cCropCoeffFileName,cWorkingDirectory,TimeStep,NTIME,NStrmNodes,iStrmNodeIDs,AppGrid,Stratigraphy,Precip,ET,iStat,cVersionOverride) 
    CLASS(AppSmallWatershed_v40_Type),INTENT(OUT) :: AppSWShed
    LOGICAL,INTENT(IN)                            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                   :: cFileName,cWorkingDirectory
    CHARACTER(LEN=*),INTENT(IN)                   :: cCropCoeffFileName    !Not used in this version
    TYPE(TimeStepType),INTENT(IN)                 :: TimeStep
    INTEGER,INTENT(IN)                            :: NStrmNodes,iStrmNodeIDs(NStrmNodes),NTIME
    TYPE(AppGridType),INTENT(IN)                  :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)             :: Stratigraphy
    TYPE(PrecipitationType),INTENT(IN)            :: Precip
    TYPE(ETType),INTENT(IN)                       :: ET
    INTEGER,INTENT(OUT)                           :: iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)          :: cVersionOverride   !Not used here; in case we want to use another version number with child classes, instead of the version of Package_AppSmallWatershed
    
    !Local variables
    TYPE(GenericFileType)    :: MainFile
    CHARACTER                :: ALine*1200,cBudFileName*1200,cFinResultsFileName*1200
    CHARACTER(:),ALLOCATABLE :: cAbsPathFileName
    INTEGER                  :: iNSWShed,iGWNodeIDs(AppGrid%NNodes)
    INTEGER,ALLOCATABLE      :: iSWShedIDs(:)
    
    !Initialize
    iStat      = 0
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Return if no filename is specified
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating small watershed component...')
    
    !Open main input file
    CALL MainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='small watershed parameters',iStat=iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !Read away the version number line
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Budget output file name
    CALL MainFile%ReadData(cBudFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cBudFileName = StripTextUntilCharacter(cBudFileName,f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cBudFileName)  
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cBudFileName)),cWorkingDirectory,cAbsPathFileName)
    cBudFileName = cAbsPathFileName
    
    !Final results output name
    CALL MainFile%ReadData(cFinResultsFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cFinResultsFileName = StripTextUntilCharacter(cFinResultsFileName,f_cInlineCommentChar)  
    CALL CleanSpecialCharacters(cFinResultsFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cFinResultsFileName)),cWorkingDirectory,cAbsPathFileName)
    cFinResultsFileName = cAbsPathFileName

    !Number of small watersheds; if zero return
    CALL MainFile%ReadData(iNSWShed,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (iNSWShed .EQ. 0) RETURN
    AppSWShed%iNSWShed = iNSWShed
    
    !Allocate memory
    ALLOCATE (AppSWShed%SmallWatersheds(iNSWShed))

    !Read geospatial parameters
    CALL AppSWShed%ReadGeospatialData(MainFile,TimeStep,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    ALLOCATE (iSWShedIDs(iNSWShed))
    iSWShedIDs = AppSWShed%SmallWatersheds%ID
    
    !Read root zone parameters
    CALL AppSmallWatershed_v40_ReadRootZoneData(AppSWShed,MainFile,TimeStep,iSWShedIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read aquifer related parameters
    CALL AppSWShed%ReadAquiferData(MainFile,TimeStep,iSWShedIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read initial conditions
    CALL AppSWShed%ReadInitialConditions(MainFile,iSWShedIDs,.TRUE.,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the small watershed budget file
    IF (cBudFileName .NE. '')  THEN
        CALL AppSWShed%InitSmallWatershedBudRawFile(IsForInquiry,TRIM(cBudFileName),f_cVersion,iSWShedIDs,NTIME,TimeStep,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Instantiate the file to print final results
    IF (cFinResultsFileName .NE. '') THEN
        IF (IsForInquiry) THEN
            CALL AppSWShed%FinResultsFile%New(FileName=cFinResultsFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='small watersheds final simulation results',iStat=iStat) 
        ELSE
            CALL AppSWShed%FinResultsFile%New(FileName=cFinResultsFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='small watersheds final simulation results',iStat=iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Compile list of nodes recieving baseflow and percolation at each layer
    CALL AppSWShed%CompileBaseflowPercolationNodes(Stratigraphy%NLayers)
    
    !Check if pointed TS data columns actually exist
    CALL AppSWShed%CheckTSDataPointers(Precip,ET,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Close main input file
    CALL MainFile%Kill()
      
  END SUBROUTINE AppSmallWatershed_v40_New 

  
  
  
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
  ! --- KILL AppSmallWatershed_v40 OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE AppSmallWatershed_v40_Kill(AppSWShed)
    CLASS(AppSmallWatershed_v40_Type),INTENT(INOUT) :: AppSWShed    
  END SUBROUTINE AppSmallWatershed_v40_Kill

  
  
  
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
  ! --- READ ROOT ZONE DATA
  ! --- * Note: Assumes the VarTimeUnit attribute is set previously
  ! -------------------------------------------------------------
  SUBROUTINE AppSmallWaterShed_v40_ReadRootZoneData(AppSWShed,InFile,TimeStep,iSWShedIDs,iStat)
    CLASS(AppSmallWaterShed_v40_Type),INTENT(INOUT) :: AppSWShed
    TYPE(GenericFileType),INTENT(INOUT)             :: InFile
    TYPE(TimeStepType),INTENT(IN)                   :: TimeStep
    INTEGER,INTENT(IN)                              :: iSWShedIDs(:)
    INTEGER,INTENT(OUT)                             :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+16) :: ThisProcedure = f_cModName // 'ReadRootZoneData'
    INTEGER                         :: indxSWShed,iSWShedID,iSWShed
    REAL(8)                         :: FactLength,FactCN,FactK,DummyArray(12)
    CHARACTER                       :: cALine*500,cTimeUnitK*6
    LOGICAL                         :: lProcessed(AppSWShed%iNSWShed)
    
    !Initialize
    lProcessed = .FALSE.
    
    !Read solver data and conversion factors
    CALL InFile%ReadData(AppSWShed%RZSolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSWShed%RZSolverData%IterMax,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactLength,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactCN,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactK,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar)  ;  CALL CleanSpecialCharacters(cALine)
    cTimeUnitK = TRIM(ADJUSTL(cALine))
    
    !Make sure time unit is recognized
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(cTimeUnitK) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for small watershed root zone hydraulic conductivity is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !If time unit is not yet set, set it
    IF (AppSWShed%cVarTimeUnit .EQ. '') AppSWShed%cVarTimeUnit = cTimeUnitK
    
    !Convert time unit of soil hydraulic conductivity to the time unit stored in AppSWShed%VarTimeUnit
    FactK = FactK * TimeIntervalConversion(AppSWShed%cVarTimeUnit,cTimeUnitK) 
    
    !Read and process root zone parameters
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%iNSWShed
            CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iSWShedID = INT(DummyArray(1))
            
            !Make sure small watershed ID is legit and has not been already processed
            CALL ConvertID_To_Index(iSWShedID,iSWShedIDs,iSWShed)
            IF (iSWShed .EQ. 0) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' listed for root zone parameter definition is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (lProcessed(iSWShed)) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' is listed more than once for root zone parameter definitions!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iSWShed) = .TRUE.

            pSWSheds(iSWShed)%iColPrecip         = DummyArray(2)
            pSWSheds(iSWShed)%PrecipFactor       = DummyArray(3)
            pSWSheds(iSWShed)%iColET             = DummyArray(4)
            pSWSheds(iSWShed)%Soil%WiltingPoint  = DummyArray(5)
            pSWSheds(iSWShed)%Soil%FieldCapacity = DummyArray(6)
            pSWSheds(iSWShed)%Soil%TotalPorosity = DummyArray(7)
            pSWSheds(iSWShed)%Soil%Lambda        = DummyArray(8)
            pSWSheds(iSWShed)%RootDepth          = DummyArray(9) * FactLength
            pSWSheds(iSWShed)%Soil%HydCond       = DummyArray(10) * FactK * TimeStep%DeltaT
            pSWSheds(iSWShed)%Soil%KunsatMethod  = DummyArray(11)
            pSWSheds(iSWShed)%SMax               = (1000.0/DummyArray(12)-10.0) * FactCN
            
            !Total porosity should be less than 1.0
            IF (pSWSheds(iSWShed)%Soil%TotalPorosity .GT. 1.0) THEN
                CALL SetLastMessage('At small watershed ' // TRIM(IntToText(iSWShedID)) // ' total porosity is greater than 1.0!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Wilting point should be less than field capacity
            IF (pSWSheds(iSWShed)%Soil%WiltingPoint .GE. pSWSheds(iSWShed)%Soil%FieldCapacity) THEN
                CALL SetLastMessage('At small watershed ' // TRIM(IntToText(iSWShedID)) // ' wilting point is greater than or equal to field capacity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Field capacity should be less than or equal to total porosity
            IF (pSWSheds(iSWShed)%Soil%FieldCapacity .GT. pSWSheds(iSWShed)%Soil%TotalPorosity) THEN
                CALL SetLastMessage('At small watershed ' // TRIM(IntToText(iSWShedID)) // ' field capacity is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Convert L/L soil parameters to depth
            pSWSheds(iSWShed)%Soil%WiltingPoint  = pSWSheds(iSWShed)%Soil%WiltingPoint  * pSWSheds(iSWShed)%RootDepth
            pSWSheds(iSWShed)%Soil%FieldCapacity = pSWSheds(iSWShed)%Soil%FieldCapacity * pSWSheds(iSWShed)%RootDepth
            pSWSheds(iSWShed)%Soil%TotalPorosity = pSWSheds(iSWShed)%Soil%TotalPorosity * pSWSheds(iSWShed)%RootDepth
            
            !KunsatMethod must be recognized
            IF (.NOT. ANY(pSWSheds(iSWShed)%Soil%KunsatMethod .EQ. f_iKunsatMethodList)) THEN
                CALL SetLastMessage('Method to compute unsaturated hydraulic conductivity at small watershed '//TRIM(IntToText(iSWShedID))//' is not recognized!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO       
    END ASSOCIATE
    
  END SUBROUTINE AppSmallWaterShed_v40_ReadRootZoneData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppSmallWaterShed_v40_ReadTSData(AppSWShed,Precip,ET)
    CLASS(AppSmallWaterShed_v40_Type),INTENT(INOUT) :: AppSWShed
    TYPE(PrecipitationType),INTENT(IN)              :: Precip
    TYPE(ETType),INTENT(IN)                         :: ET
    
    AppSWShed%SmallWatersheds%Precip = Precip%GetValues(AppSWShed%SmallWatersheds%iColPrecip) * AppSWShed%SmallWatersheds%PrecipFactor
    AppSWShed%SmallWatersheds%ETc    = ET%GetValues(AppSWShed%SmallWatersheds%iColET)

  END SUBROUTINE AppSmallWaterShed_v40_ReadTSData

END MODULE