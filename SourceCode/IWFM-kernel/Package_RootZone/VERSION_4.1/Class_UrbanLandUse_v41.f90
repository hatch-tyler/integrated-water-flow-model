!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2026  
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
MODULE Class_UrbanLandUse_v41
  !$ USE OMP_LIB
  USE MessageLogger            , ONLY: SetLastMessage                   , &
                                       EchoProgress                     , &
                                       MessageArray                     , &
                                       f_iFatal                           
  USE GeneralUtilities         , ONLY: StripTextUntilCharacter          , &
                                       EstablishAbsolutePathFileName    , &
                                       CleanSpecialCharacters           , &
                                       IntToText                        , &
                                       NormalizeArray                   , &
                                       LocateInList                     
  USE TimeSeriesUtilities      , ONLY: TimeStepType                     , &
                                       IncrementTimeStamp               , &
                                       OPERATOR(.TSGT.)
  USE IOInterface              , ONLY: GenericFileType                  , &
                                       RealTSDataInFileType             , &
                                       IntTSDataInFileType              , &
                                       f_IO_SuccessfulRead
  USE Package_Misc             , ONLY: SolverDataType                   , & 
                                       f_iFlowDest_GWElement            , &
                                       f_iLandUse_Urb                   , &
                                       f_iLandUse_UrbIn                 , &
                                       f_iLandUse_UrbOut
  USE Class_BaseRootZone       , ONLY: TrackMoistureDueToSource         , &
                                       CalculateUrbanFracDemand         
  USE Class_GenericLandUse_v41 , ONLY: GenericLandUse_v41_Type          , &
                                       ComputeETFromGW_Max              
  USE Class_LandUseDataFile    , ONLY: LandUseDataFileType              
  USE Package_Discretization   , ONLY: AppGridType                      
  USE Package_PrecipitationET  , ONLY: ETType   
  USE Util_Package_RootZone    , ONLY: ReadRealData                     , &
                                       ReadLandUseAreasForTimePeriod    
  USE Util_RootZone_v41        , ONLY: RootZoneSoil_v41_Type 
  USE Package_UnsatZone        , ONLY: NonPondedLUMoistureRouter               
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
  PUBLIC :: UrbanLandUse_v41_Type
 
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUse_v41_Type) :: Urban_v41_Type
      INTEGER,ALLOCATABLE :: iColReturnFrac(:,:)          !Column number in the return flow fraction data file
      INTEGER,ALLOCATABLE :: iColReuseFrac(:,:)           !Column number in the re-use fraction data file
      INTEGER,ALLOCATABLE :: iColPopulation(:,:)          !Column number in the population data file
      INTEGER,ALLOCATABLE :: iColPerCapitaWaterUse(:,:)   !Column number in the per capita water use data file
      INTEGER,ALLOCATABLE :: iColWaterUseSpec(:,:)        !Column number in the urban water use specs data file
      REAL(8),ALLOCATABLE :: FracDemand(:,:)              !Relative proportion of demand computed by multiplying popultaion with per capita water use to be applied to an element
      REAL(8),ALLOCATABLE :: Demand(:,:)                  !Urban water demand
      REAL(8),ALLOCATABLE :: PerviousFrac(:,:)            !Fraction of pervious area to total urban area
      REAL(8),ALLOCATABLE :: IrigInfilt(:,:)              !Infiltration due to irrigation
      REAL(8),ALLOCATABLE :: Reuse(:,:)                   !Reused return flow 
      REAL(8),ALLOCATABLE :: ReturnFlowIn(:,:)            !Urban indoors return flow
      REAL(8),ALLOCATABLE :: ReturnFlowOut(:,:)           !Urban outdoors return flow
      REAL(8),ALLOCATABLE :: ElemDemandFrac(:,:)          !Ratio of urban demand to the total demand at the element it is located at
  END TYPE Urban_v41_Type
  
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE UrbanLandUse_v41_Type
      TYPE(Urban_v41_Type)        :: UrbData                         !Urban data for each element
      REAL(8),ALLOCATABLE         :: RootDepth(:)                    !Urban root depth for each (element)
      REAL(8)                     :: PerCapitaWaterUseFactor = 1.0   !Conversion factor for water demand
      LOGICAL                     :: lFracDemand_wrt_Area = .FALSE.  !Flag to see if FracDemand is specified by the user as a constant or with respect to urban area in each element
      REAL(8),ALLOCATABLE         :: RegionETPot(:)                  !Regional urban potential ET
      TYPE(LandUseDataFileType)   :: LandUseDataFile                 !Land use data file
      TYPE(RealTSDataInFileType)  :: PerCapitaWaterUseFile           !Urban per capita water use data file
      TYPE(IntTSDataInFileType)   :: PopulationFile                  !Population data file
      TYPE(RealTSDataInFileType)  :: WaterUseSpecsFile               !Urban water use specs data file
  CONTAINS
      PROCEDURE,PASS   :: New                          
      PROCEDURE,PASS   :: Kill 
      PROCEDURE,PASS   :: GetMaxAndMinNetReturnFlowFrac
      PROCEDURE,NOPASS :: GetAreasForTimePeriod_FileNotOpen
      PROCEDURE,PASS   :: GetAreasForTimePeriod_FileOpen
      PROCEDURE,PASS   :: GetElementAppliedWater
      PROCEDURE,PASS   :: SetAreas                     
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: ReadTSData                   
      PROCEDURE,PASS   :: AdvanceAreas                 
      PROCEDURE,PASS   :: SoilMContent_To_Depth        
      PROCEDURE,PASS   :: Simulate                     
      PROCEDURE,PASS   :: ComputeWaterDemand           
      PROCEDURE,PASS   :: ComputeETFromGW_Max             => UrbanLandUse_ComputeETFromGW_Max         
      PROCEDURE,PASS   :: RewindTSInputFilesToTimeStamp          
      GENERIC          :: GetAreasForTimePeriod           => GetAreasForTimePeriod_FileNotOpen     , &
                                                             GetAreasForTimePeriod_FileOpen
  END TYPE UrbanLandUse_v41_Type

  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 24
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_UrbanLandUse_v41::'

  


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
  ! --- NEW URBAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(UrbLand,cFileName,cWorkingDirectory,rFactCN,iNElements,iNSubregions,iElemIDs,lTrackTime,iStat,iColUrbETCoeff)
    CLASS(UrbanLandUse_v41_Type) :: UrbLand
    CHARACTER(LEN=*),INTENT(IN)  :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)           :: rFactCN
    INTEGER,INTENT(IN)           :: iNElements,iNSubregions,iElemIDs(iNElements)
    LOGICAL,INTENT(IN)           :: lTrackTime  
    INTEGER,INTENT(OUT)          :: iStat
    INTEGER,OPTIONAL,ALLOCATABLE :: iColUrbETCoeff(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)  :: ThisProcedure = ModName // 'New'
    CHARACTER                    :: cALine*1000
    INTEGER                      :: iErrorCode,indxElem,iElem,ID,iNCols,                                &
                                    iCol_PerviousFrac,iCol_CN,iCol_InitialAbstraction,iCol_Population,  &
                                    iCol_PerCapitaWaterUse,iCol_FracDemand,iCol_ETc,iCol_ETCoeff,       &
                                    iCol_ReturnFrac,iCol_ReuseFrac,iCol_WaterUseSpec
    REAL(8)                      :: rFACT,rFactor(1),rDummyArray12(12),rDummyArray11(11)
    LOGICAL                      :: lError,lProcessed(iNElements),lInitialAbstractionProvided
    REAL(8),ALLOCATABLE          :: rDummyArray(:,:)
    TYPE(GenericFileType)        :: UrbanDataFile
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    !Initialize
    iStat = 0
   
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Open file
    CALL UrbanDataFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    CALL UrbLand%UrbData%New(iNElements,1,iStat)
    ALLOCATE (UrbLand%UrbData%iColReturnFrac(iNElements,1)         , &          
              UrbLand%UrbData%iColReuseFrac(iNElements,1)          , &           
              UrbLand%UrbData%iColPopulation(iNElements,1)         , &          
              UrbLand%UrbData%iColPerCapitaWaterUse(iNElements,1)  , &   
              UrbLand%UrbData%iColWaterUseSpec(iNElements,1)       , &        
              UrbLand%UrbData%FracDemand(iNElements,1)             , &              
              UrbLand%UrbData%Demand(iNElements,1)                 , &                  
              UrbLand%UrbData%PerviousFrac(iNElements,1)           , &            
              UrbLand%UrbData%IrigInfilt(iNElements,1)             , &              
              UrbLand%UrbData%Reuse(iNElements,1)                  , &                   
              UrbLand%UrbData%ReturnFlowIn(iNElements,1)           , &              
              UrbLand%UrbData%ReturnFlowOut(iNElements,1)          , &              
              UrbLand%UrbData%ElemDemandFrac(iNElements,1)         , &
              UrbLand%RootDepth(iNElements)                        , &
              UrbLand%RegionETPot(iNSubregions)                    , &
              STAT=iErrorCode                                      )
    IF (iErrorCode+iStat .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for urban data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize arrays
    UrbLand%UrbData%iColReturnFrac        = 0
    UrbLand%UrbData%iColReuseFrac         = 0 
    UrbLand%UrbData%iColPopulation        = 0
    UrbLand%UrbData%iColPerCapitaWaterUse = 0 
    UrbLand%UrbData%iColWaterUseSpec      = 0
    UrbLand%UrbData%FracDemand            = 0.0
    UrbLand%UrbData%Demand                = 0.0
    UrbLand%UrbData%PerviousFrac          = 0.0
    UrbLand%UrbData%IrigInfilt            = 0.0
    UrbLand%UrbData%Reuse                 = 0.0
    UrbLand%UrbData%ReturnFlowIn          = 0.0 
    UrbLand%UrbData%ReturnFlowOut         = 0.0 
    UrbLand%UrbData%ElemDemandFrac        = 0.0
    UrbLand%RegionETPot                   = 0.0

    !Land use data file
    CALL UrbanDataFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,'/') 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Urban area file',iNElements,1,lTrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Rooting depths
    CALL UrbanDataFile%ReadData(rFACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL UrbanDataFile%ReadData(UrbLand%RootDepth(1),iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%RootDepth = UrbLand%RootDepth(1) * rFACT
    
    !Urban population file
    CALL UrbanDataFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cALine = StripTextUntilCharacter(cALine,'/') 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%PopulationFile%Init(cAbsPathFileName,cWorkingDirectory,'Population data file',lTrackTime,1,iStat)
    IF (iStat .EQ. -1) RETURN
      
    !Urban per capita water use file
    CALL UrbanDataFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cALine = StripTextUntilCharacter(cALine,'/') 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%PerCapitawaterUseFile%Init(cAbsPathFileName,cWorkingDirectory,'Urban per capita water use data file',lTrackTime,1,.TRUE.,rFactor,[.TRUE.],iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%PerCapitawaterUseFactor = rFactor(1)
      
    !Urban water use specifications data file
    CALL UrbanDataFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,'/') 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%WaterUseSpecsFile%Init(cAbsPathFileName,cWorkingDirectory,'Urban water use specifications',lTrackTime,1,.FALSE.,rFactor,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN

    !Read other data
    IF (PRESENT(iColUrbETCoeff)) THEN
        !BACKWARD COMPATIBILITY: Check if initial abstraction parameter is provided
        CALL UrbanDataFile%ReadData(cALine,iStat) ; IF (iStat .NE. 0) RETURN
        cALine = ADJUSTL(StripTextUntilCharacter(cALine,'/')) 
        CALL CleanSpecialCharacters(cALine)
        READ (cALine(1:LEN_TRIM(cALine)),*,IOSTAT=iErrorCode) rDummyArray12
        IF (iErrorCode .EQ. 0) THEN
            lInitialAbstractionProvided = .TRUE.
            iNCols                      = 12
            iCol_PerviousFrac           = 2
            iCol_CN                     = 3
            iCol_InitialAbstraction     = 4
            iCol_Population             = 5      
            iCol_PerCapitaWaterUse      = 6
            iCol_FracDemand             = 7
            iCol_ETc                    = 8
            iCol_ETCoeff                = 9
            iCol_ReturnFrac             = 10
            iCol_ReuseFrac              = 11
            iCol_WaterUseSpec           = 12
        ELSE
            lInitialAbstractionProvided = .FALSE.
            iNCols                      = 11
            iCol_PerviousFrac           = 2
            iCol_CN                     = 3
            iCol_Population             = 4     
            iCol_PerCapitaWaterUse      = 5 
            iCol_FracDemand             = 6
            iCol_ETc                    = 7
            iCol_ETCoeff                = 8
            iCol_ReturnFrac             = 9
            iCol_ReuseFrac              = 10
            iCol_WaterUseSpec           = 11
        END IF
        CALL UrbanDataFile%BackspaceFile()
        !Then read the data
        ALLOCATE(iColUrbETCoeff(iNElements))
        CALL ReadRealData(UrbanDataFile,'data for urban lands','elements',iNElements,iNCols,iElemIDs,rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ELSE
        !BACKWARD COMPATIBILITY: Check if initial abstraction parameter is provided
        CALL UrbanDataFile%ReadData(cALine,iStat) ; IF (iStat .NE. 0) RETURN
        cALine = ADJUSTL(StripTextUntilCharacter(cALine,'/')) 
        CALL CleanSpecialCharacters(cALine)
        READ (cALine(1:LEN_TRIM(cALine)),*,IOSTAT=iErrorCode) rDummyArray11
        IF (iErrorCode .EQ. 0) THEN
            lInitialAbstractionProvided = .TRUE.
            iNCols                      = 11
            iCol_PerviousFrac           = 2
            iCol_CN                     = 3
            iCol_InitialAbstraction     = 4
            iCol_Population             = 5      
            iCol_PerCapitaWaterUse      = 6
            iCol_FracDemand             = 7
            iCol_ETc                    = 8
            iCol_ReturnFrac             = 9
            iCol_ReuseFrac              = 10
            iCol_WaterUseSpec           = 11
        ELSE
            lInitialAbstractionProvided = .FALSE.
            iNCols                      = 10
            iCol_PerviousFrac           = 2
            iCol_CN                     = 3
            iCol_Population             = 4     
            iCol_PerCapitaWaterUse      = 5 
            iCol_FracDemand             = 6
            iCol_ETc                    = 7
            iCol_ReturnFrac             = 8
            iCol_ReuseFrac              = 9
            iCol_WaterUseSpec           = 10
        END IF
        CALL UrbanDataFile%BackspaceFile()
        !Then, read the data
        CALL ReadRealData(UrbanDataFile,'data for urban lands','elements',iNElements,iNCols,iElemIDs,rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    ASSOCIATE (pUrbData  => UrbLand%UrbData  )
        lProcessed = .FALSE.
        DO indxElem=1,iNElements
            iElem = INT(rDummyArray(indxElem,1))
            !Make sure data for element is not entered more than once
            IF (lProcessed(iElem)) THEN
                ID = iElemIDs(iElem)
                CALL SetLastMessage('Data for urban lands at element '//TRIM(IntToText(ID))//' is defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iElem) = .TRUE.
            
            !Make sure curve numbers are between 0 and 100
            IF (rDummyArray(indxElem,iCol_CN) .LT. 0.0 .OR. rDummyArray(indxElem,iCol_CN) .GT. 100.0) THEN
                ID = iElemIDs(iElem)
                CALL SetLastMessage('Urban curve number at element '//TRIM(IntToText(ID))//' is less than 0.0 or greater 100.0!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF       
            
            pUrbData%PerviousFrac(iElem,1)          =     rDummyArray(indxElem,iCol_PerviousFrac)
            pUrbData%SMax(iElem,1)                  = (1000.0/rDummyArray(indxElem,iCol_CN)-10.0) * rFactCN
            pUrbData%iColPopulation(iElem,1)        = INT(rDummyArray(indxElem,iCol_Population))
            pUrbData%iColPerCapitaWaterUse(iElem,1) = INT(rDummyArray(indxElem,iCol_PerCapitaWaterUse))
            pUrbData%FracDemand(iElem,1)            =     rDummyArray(indxElem,iCol_FracDemand)
            pUrbData%iColETc(iElem,1)               = INT(rDummyArray(indxElem,iCol_ETc))
            pUrbData%iColReturnFrac(iElem,1)        = INT(rDummyArray(indxElem,iCol_ReturnFrac))
            pUrbData%iColReuseFrac(iElem,1)         = INT(rDummyArray(indxElem,iCol_ReuseFrac))
            pUrbData%iColWaterUseSpec(iElem,1)      = INT(rDummyArray(indxElem,iCol_WaterUseSpec))
            IF (lInitialAbstractionProvided) THEN
                IF (rDummyArray(indxElem,iCol_InitialAbstraction) .GT. 0.0) &
                    pUrbData%rInitialAbstraction(iElem,1) = rDummyArray(indxElem,iCol_InitialAbstraction)
            END IF
            IF (PRESENT(iColUrbETCoeff)) iColUrbETCoeff(iElem) = INT(rDummyArray(indxElem,iCol_ETCoeff))
        END DO
      
        !Make sure FRACDM is either entered all negative or all greater than (or equal to) zero
        lError = .FALSE.
        IF (pUrbData%FracDemand(1,1) .LT. 0.0) THEN
            UrbLand%lFracDemand_wrt_Area = .TRUE.
            IF (ANY(pUrbData%FracDemand(2:,1).GE.0.0)) lError = .TRUE.
        ELSE
            UrbLand%lFracDemand_wrt_Area = .FALSE.
            IF (ANY(pUrbData%FracDemand(2:,1).LT.0.0)) lError = .TRUE.
        END IF
        IF (lError) THEN
            MessageArray(1) = 'FRACDM variable in urban main data file must be all specified '
            MessageArray(2) = 'as either -1.0 or greater than (or equal to) zero!'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
                  
        !Normalize the relative fractions to distribute the demands, if fractions are specified by the user as constant
        IF (.NOT. UrbLand%lFracDemand_wrt_Area)  &
            CALL CalculateUrbanFracDemand(iNElements,pUrbData%iColPopulation,pUrbData%iColPerCapitaWaterUse,pUrbData%FracDemand)
        
        !Check for timeseries column pointer errors
        DO indxElem=1,iNElements
            ID = iElemIDs(indxElem)
            CALL UrbLand%PopulationFile%CheckColNum('Urban population file as referenced by element '//TRIM(IntToText(ID)),[pUrbData%iColPopulation(indxElem,1)],.TRUE.,iStat)                          ;  IF (iStat .EQ. -1) RETURN
            CALL UrbLand%PerCapitaWaterUseFile%CheckColNum('Urban per capita water use file as referenced by element '//TRIM(IntToText(ID)),[pUrbData%iColPerCapitaWaterUse(indxElem,1)],.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL UrbLand%WaterUseSpecsFile%CheckColNum('Urban water use specifications file as referenced by element '//TRIM(IntToText(ID)),[pUrbData%iColWaterUseSpec(indxElem,1)],.TRUE.,iStat)       ;  IF (iStat .EQ. -1) RETURN
        END DO

    END ASSOCIATE

    !Initial conditions
    CALL ReadRealData(UrbanDataFile,'initial conditions for urban lands','elements',iNElements,3,iElemIDs,rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(rDummyArray(:,2)) .LT. 0.0   .OR.  &
        MAXVAL(rDummyArray(:,2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    IF (MINVAL(rDummyArray(:,3:)) .LT. 0.0   .OR.  &
        MAXVAL(rDummyArray(:,3:)) .GT. 1.0          ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    lProcessed = .FALSE.
    DO indxElem=1,iNElements
        iElem = INT(rDummyArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('initial conditions for urban lands at element '//TRIM(IntToText(ID))//' is defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                                    = .TRUE.
        UrbLand%UrbData%SoilM_Precip(iElem,1)                = rDummyArray(indxElem,2) * rDummyArray(indxElem,3)
        UrbLand%UrbData%SoilM_AW(iElem,1)                    = rDummyArray(indxElem,3) - UrbLand%UrbData%SoilM_Precip(iElem,1)
        UrbLand%UrbData%SoilM_Precip_P_BeforeUpdate(iElem,1) = UrbLand%UrbData%SoilM_Precip(iElem,1) 
        UrbLand%UrbData%SoilM_Precip_P(iElem,1)              = UrbLand%UrbData%SoilM_Precip(iElem,1) 
        UrbLand%UrbData%SoilM_AW_P_BeforeUpdate(iElem,1)     = UrbLand%UrbData%SoilM_AW(iElem,1) 
        UrbLand%UrbData%SoilM_AW_P(iElem,1)                  = UrbLand%UrbData%SoilM_AW(iElem,1) 
    END DO
    
    !Optional root depth scaling factors for each (crop,element) combination
    ! Check if this data is even available, if not keep the root depths as is
    DEALLOCATE (rDummyArray , STAT=iErrorCode)
    ALLOCATE (rDummyArray(iNElements,2))
    CALL UrbanDataFile%ReadData(rDummyArray,iErrorCode)
    IF (iErrorCode .EQ. 0) THEN
        !Process spatial scaling factors
        lProcessed = .FALSE.
        DO indxElem=1,iNElements
            ID    = INT(rDummyArray(indxElem,1))
            iElem = LocateInList(ID,iElemIDs)
            IF (iElem .EQ. 0) THEN
                CALL SetLastMessage('Element ID '// TRIM(IntToText(ID)) // ' listed for root depth spatial scaling factors for urban lands is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (lProcessed(iElem)) THEN
                CALL SetLastMessage('Root depth spatial distribution factors for urban area at element '//TRIM(IntToText(ID))//' is defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iElem)        = .TRUE.
            UrbLand%RootDepth(iElem) = UrbLand%RootDepth(iElem) * rDummyArray(indxElem,2)
        END DO
    END IF

    !Close file
    CALL UrbanDataFile%Kill()
    
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
  ! --- KILL URBAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(UrbLand)
    CLASS(UrbanLandUse_v41_Type) :: UrbLand

    !Local variables
    INTEGER                     :: ErrorCode
    TYPE(UrbanLandUse_v41_Type) :: Dummy
    
    !Deallocate arrays
    CALL UrbLand%UrbData%Kill()
    DEALLOCATE (UrbLand%UrbData%iColReturnFrac          , &          
                UrbLand%UrbData%iColReuseFrac           , &           
                UrbLand%UrbData%iColPopulation          , &          
                UrbLand%UrbData%iColPerCapitaWaterUse   , &   
                UrbLand%UrbData%iColWaterUseSpec        , &        
                UrbLand%UrbData%FracDemand              , &              
                UrbLand%UrbData%Demand                  , &                  
                UrbLand%UrbData%PerviousFrac            , &            
                UrbLand%UrbData%IrigInfilt              , &              
                UrbLand%UrbData%Reuse                   , &                   
                UrbLand%UrbData%ReturnFlowIn            , &              
                UrbLand%UrbData%ReturnFlowOut           , &              
                UrbLand%UrbData%ElemDemandFrac          , &
                UrbLand%RootDepth                       , &
                UrbLand%RegionETPot                     , &
                STAT = ErrorCode                        )
    
    !Close files
    CALL UrbLand%LandUseDataFile%Kill()
    CALL UrbLand%PerCapitaWaterUseFile%Close()
    CALL UrbLand%PopulationFile%Close()
    CALL UrbLand%WaterUseSpecsFile%Close()
    
    !Assign default values to components
    SELECT TYPE (UrbLand)
        TYPE IS (UrbanLandUse_v41_Type)
            UrbLand = Dummy
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
  ! --- GET URBAN INDOORS OR OUTDOORS OR ALL APPLIED WATER AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementAppliedWater(UrbLand,rElemSupply,iLUType,rAW)
    CLASS(UrbanLandUse_v41_Type ),INTENT(IN) :: UrbLand
    REAL(8),INTENT(IN)                       :: rElemSupply(:)
    INTEGER,INTENT(IN)                       :: iLUType
    REAL(8),INTENT(OUT)                      :: rAW(:)
    
    !Local variables
    INTEGER :: indxElem
    REAL(8) :: rArea,rArea_Indoors,rArea_Outdoors,rIndoorFrac
    
    SELECT CASE (iLUType)
        CASE (f_iLandUse_Urb)
            DO indxElem=1,SIZE(rAW)
                rArea = UrbLand%UrbData%Area(indxElem,1)
                IF (rArea .EQ. 0.0) THEN
                    rAW(indxElem) = 0.0
                    CYCLE
                END IF
                rAW (indxElem) = rElemSupply(indxElem)
            END DO
            
            
        CASE (f_iLandUse_UrbIn)
            DO indxElem=1,SIZE(rAW)
                rArea = UrbLand%UrbData%Area(indxElem,1)
                IF (rArea .EQ. 0.0) THEN
                    rAW(indxElem) = 0.0
                    CYCLE
                END IF
                rArea_Outdoors = rArea * UrbLand%UrbData%PerviousFrac(indxElem,1)
                rArea_Indoors  = rArea - rArea_Outdoors 
                IF (rArea_Indoors .EQ. 0.0) THEN
                    rAW(indxElem) = 0.0
                ELSEIF (rArea_Outdoors .EQ. 0.0) THEN
                    rAW(indxElem) = rElemSupply(indxElem) 
                ELSE
                    rIndoorFrac   = UrbLand%WaterUseSpecsFile%rValues(UrbLand%UrbData%iColWaterUseSpec(indxElem,1))
                    rAW(indxElem) = rElemSupply(indxElem)*rIndoorFrac
                END IF
            END DO
            
            
        CASE (f_iLandUse_UrbOut)
            DO indxElem=1,SIZE(rAW)
                rArea = UrbLand%UrbData%Area(indxElem,1)
                IF (rArea .EQ. 0.0) THEN
                    rAW(indxElem) = 0.0
                    CYCLE
                END IF
                rArea_Outdoors = rArea * UrbLand%UrbData%PerviousFrac(indxElem,1)
                rArea_Indoors  = rArea - rArea_Outdoors 
                IF (rArea_Indoors .EQ. 0.0) THEN
                    rAW(indxElem) = rElemSupply(indxElem)
                ELSEIF (rArea_Outdoors .EQ. 0.0) THEN
                    rAW(indxElem) = 0.0
                ELSE
                    rIndoorFrac   = UrbLand%WaterUseSpecsFile%rValues(UrbLand%UrbData%iColWaterUseSpec(indxElem,1))
                    rAW(indxElem) = rElemSupply(indxElem)*(1d0-rIndoorFrac )
                END IF
            END DO
            
    END SELECT
    
  END SUBROUTINE GetElementAppliedWater
  
  
  ! -------------------------------------------------------------
  ! --- GET MIN AND MAX NET RETURN FLOW FRACTIONS THROUGH THE ENTITE SIMULATION PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(UrbLand,ReturnFracFile,ReuseFracFile,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(UrbanLandUse_v41_Type),INTENT(IN) :: UrbLand
    TYPE(RealTSDataInFileType)              :: ReturnFracFile,ReuseFracFile
    TYPE(TimeStepType),INTENT(IN)           :: FirstTimeStep
    REAL(8),INTENT(OUT)                     :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    INTEGER            :: FileReadCode_Return,FileReadCode_Reuse,indxElem
    REAL(8)            :: rRT,rRU
    
    !Initialize
    TimeStep = FirstTimeStep
    rMaxFrac = 0.0
    rMinFrac = 1.0
    
    !Loop through timesteps and read return flow fractions
    DO
        !Read data
        CALL ReturnFracFile%ReadTSData(TimeStep,'Return flow fractions data',FileReadCode_Return,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReuseFracFile%ReadTSData(TimeStep,'Reuse fractions data',FileReadCode_Reuse,iStat)          ;  IF (iStat .EQ. -1) RETURN
        
        !If new data is read, find min and max
        IF (FileReadCode_Return.EQ.f_IO_SuccessfulRead  .OR.  FileReadCode_Reuse.EQ.f_IO_SuccessfulRead) THEN
            DO indxElem=1,SIZE(UrbLand%UrbData%SMax)
                rRT      = ReturnFracFile%rValues(UrbLand%UrbData%iColReturnFrac(indxElem,1))
                rRU      = ReuseFracFile%rValues(UrbLand%UrbData%iColReuseFrac(indxElem,1))
                rMaxFrac = MAX(rMaxFrac , rRT-rRU)
                rMinFrac = MIN(rMinFrac , rRT-rRU)
            END DO
        END IF
        
        !Advance time
        TimeStep%CurrentDateAndTime = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DELTAT_InMinutes)
        
        !Exit if past the simulation end date
        IF (TimeStep%CurrentDateAndTime .TSGT. TimeStep%EndDateAndTime) EXIT

    END DO
      
  END SUBROUTINE GetMaxAndMinNetReturnFlowFrac
  

  ! -------------------------------------------------------------
  ! --- GET URBAN AREAS FOR AT ALL ELEMENTS FOR A TIME PERIOD
  ! --- Note: This method is not meant to be called during a Simulation
  ! -------------------------------------------------------------
  SUBROUTINE GetAreasForTimePeriod_FileNotOpen(cMainFileName,cWorkingDirectory,cBeginDate,cEndDate,TimeStep,AppGrid,rAreas,iStat)
    CHARACTER(LEN=*),INTENT(IN)   :: cMainFileName,cWorkingDirectory,cBeginDate,cEndDate
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    REAL(8),INTENT(OUT)           :: rAreas(:,:)  !For each (element,time)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER                :: cALine*500
    TYPE(GenericFileType)    :: MainFile
    CHARACTER(:),ALLOCATABLE :: cAreaFileName
   
    !Return if no file name is specified
    IF (cMainFileName .EQ. '') THEN
        rAreas = 0.0
        iStat  = 0
        GOTO 10
    END IF
    
    !Open main file
    CALL MainFile%New(FileName=TRIM(cMainFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .NE. 0) GOTO 10
    
    !Read area filename
    CALL MainFile%ReadData(cALine,iStat)  
    cALine = StripTextUntilCharacter(cALine,'/') 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAreaFileName)
    
    !Retrieve areas
    CALL ReadLandUseAreasForTimePeriod(cAreaFileName,cWorkingDirectory,'Urban area file',cBeginDate,cEndDate,TimeStep,AppGrid,1,1,rAreas,iStat)

10  CALL MainFile%Kill()
    
  END SUBROUTINE GetAreasForTimePeriod_FileNotOpen
  
  
  ! -------------------------------------------------------------
  ! --- GET AREAS OF A SPECIFIED CROP FOR AT ALL ELEMENTS FOR A TIME PERIOD WHEN THE AREA FILE IS ALREADY OPEN
  ! --- Note: This method is not meant to be called during a Simulation
  ! -------------------------------------------------------------
  SUBROUTINE GetAreasForTimePeriod_FileOpen(UrbLand,cBeginDate,cEndDate,TimeStep,AppGrid,rAreas,iStat)
    CLASS(UrbanLandUse_v41_Type),INTENT(INOUT) :: UrbLand
    CHARACTER(LEN=*),INTENT(IN)                :: cBeginDate,cEndDate
    TYPE(TimeStepType),INTENT(IN)              :: TimeStep
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    REAL(8),INTENT(OUT)                        :: rAreas(:,:)  !For each (element,time)
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Retrieve areas
    CALL ReadLandUseAreasForTimePeriod(UrbLand%LandUseDataFile,'Urban area file',cBeginDate,cEndDate,TimeStep,AppGrid,1,rAreas,iStat)
    
  END SUBROUTINE GetAreasForTimePeriod_FileOpen
  
  

  
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
  ! --- SET THE URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE SetAreas(UrbLand,Area)
    CLASS(UrbanLandUse_v41_Type) :: UrbLand
    REAL(8),INTENT(IN)           :: Area(:)
   
    UrbLand%UrbData%Area(:,1) = Area
    
  END SUBROUTINE SetAreas
  
  
  
  
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
  SUBROUTINE ReadRestartData(UrbanLand,InFile,lConvertMoistToDepth,iStat)
    CLASS(UrbanLandUse_v41_Type) :: UrbanLand
    TYPE(GenericFileType)        :: InFile
    LOGICAL,INTENT(IN)           :: lConvertMoistToDepth
    INTEGER,INTENT(OUT)          :: iStat
    
    !Read
    CALL InFile%ReadData(UrbanLand%UrbData%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%ReturnFlowIn,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%ReturnFlowOut,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Oth,iStat)       ;  IF (iStat .EQ. -1) RETURN
    
    !Convert moisture contents to depths, if requested
    IF (lConvertMoistToDepth) THEN
        UrbanLand%UrbData%SoilM_Precip_P(:,1) = UrbanLand%UrbData%SoilM_Precip_P(:,1) * UrbanLand%RootDepth
        UrbanLand%UrbData%SoilM_AW_P(:,1)     = UrbanLand%UrbData%SoilM_AW_P(:,1)     * UrbanLand%RootDepth    
        UrbanLand%UrbData%SoilM_Oth_P(:,1)    = UrbanLand%UrbData%SoilM_Oth_P(:,1)    * UrbanLand%RootDepth   
        UrbanLand%UrbData%SoilM_Precip(:,1)   = UrbanLand%UrbData%SoilM_Precip(:,1)   * UrbanLand%RootDepth  
        UrbanLand%UrbData%SoilM_AW(:,1)       = UrbanLand%UrbData%SoilM_AW(:,1)       * UrbanLand%RootDepth      
        UrbanLand%UrbData%SoilM_Oth(:,1)      = UrbanLand%UrbData%SoilM_Oth(:,1)      * UrbanLand%RootDepth  
    END IF

  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(UrbanLand,TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
    CLASS(UrbanLandUse_v41_Type)  :: UrbanLand
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)            :: rElemAreas(AppGrid%NElements)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode,FileReadCode1
    
    !Initialize
    iStat = 0

    !Echo progress
    CALL EchoProgress('Reading time series data for urban lands')
    
    !Land use areas
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (UrbanLand%LandUseDataFile%lUpdated) UrbanLand%UrbData%Area = TRANSPOSE(UrbanLand%LandUseDataFile%GetAreas(AppGrid%NElements,1))
    
    !Population
    CALL UrbanLand%PopulationFile%ReadTSData(TimeStep,'Population data',FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Water use
    CALL UrbanLand%PerCapitaWaterUseFile%ReadTSData(TimeStep,'Per capita water use data',FileReadCode1,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode1 .EQ. f_IO_SuccessfulRead) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor
    
    !Water use specifications
    CALL UrbanLand%WaterUseSpecsFile%ReadTSData(TimeStep,'Urban water use specifications',FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
  END SUBROUTINE ReadTSData


  
  
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
  SUBROUTINE PrintRestartData(UrbanLand,OutFile)
    CLASS(UrbanLandUse_v41_Type),INTENT(IN) :: UrbanLand
    TYPE(GenericFileType)                   :: OutFile
    
    !Print    
    CALL OutFile%WriteData(UrbanLand%UrbData%Runoff)
    CALL OutFile%WriteData(UrbanLand%UrbData%ReturnFlowIn)
    CALL OutFile%WriteData(UrbanLand%UrbData%ReturnFlowOut)
    CALL OutFile%WriteData(UrbanLand%UrbData%Area_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%Area)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Precip_P(:,1)/UrbanLand%RootDepth)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Precip(:,1)/UrbanLand%RootDepth)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_AW_P(:,1)/UrbanLand%RootDepth)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_AW(:,1)/UrbanLand%RootDepth)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Oth_P(:,1)/UrbanLand%RootDepth)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Oth(:,1)/UrbanLand%RootDepth)
    
  END SUBROUTINE PrintRestartData
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MOISTURE ROUTING
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SIMULATE FLOW PROCESSES AT URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(UrbanLand,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ReuseFrac,ReturnFrac,ElemsToGW,SolverData,lLakeElem,iStat,iColCropCoeff_Urban)
    CLASS(UrbanLandUse_v41_Type)           :: UrbanLand
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(ETType),INTENT(IN)                :: ETData
    TYPE(RootZoneSoil_v41_Type),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                     :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                     :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)        :: SolverData
    LOGICAL,INTENT(IN)                     :: lLakeElem(:)
    INTEGER,INTENT(OUT)                    :: iStat
    INTEGER,OPTIONAL,INTENT(IN)            :: iColCropCoeff_Urban(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'Simulate'
    INTEGER                               :: indxElem,KunsatMethod,iElemID
    REAL(8)                               :: AchievedConv,Area,ETc(AppGrid%NElements),HydCond,TotalPorosity,Area_Indoors,Area_Outdoors,   &
                                             FieldCapacity,TotalPorosityUrban,FieldCapacityUrban,RootDepth,Lambda,        &
                                             AW_Outdoors,AW_Indoors,WiltingPoint,WiltingPointUrban,rSoilM_P,SoilM,Supply, &
                                             GM,PrecipD,rMultip,Excess,Inflow,fRU,fRF,ratio(3),SoilM_P_Array(3),          &
                                             SoilM_Array(3),ETPartition(3),Infilt(3),ETc_effect,rIndoorFrac,rRunoff,      &
                                             rPrecipInfilt,rETa,rPerc,rReturnFlowOut,rIrigInfilt,rReuse,rGMExcess,        &
                                             rETFromGW_Actual,rSoilM_Precip_P,rSoilM_AW_P,rSoilM_Oth_P,rReturnFlowIn,     &
                                             rElemSupply_Indrs_Local(AppGrid%NElements)
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at urban lands')
    
    !Initialize
    ETc = ETData%GetValues(UrbanLand%UrbData%iColETc(:,1),iColCropCoeff_Urban) 
    
    ASSOCIATE (pUrbData => UrbanLand%UrbData)
        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,UrbanLand,lLakeElem,SoilsData,ETc,Precip,DeltaT,GenericMoisture,         &
        !$OMP                                  ReturnFrac,ReuseFrac,SolverData,ElemSupply,ElemsToGW,iStat,rElemSupply_Indrs_Local)       
        !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,96)
        DO indxElem=1,AppGrid%NElements
            !Cycle if Area is zero
            Area = pUrbData%Area(indxElem,1)
            IF (Area .EQ. 0.0) THEN
                pUrbData%Runoff(indxElem,1)          = 0.0
                pUrbData%PrecipInfilt(indxElem,1)    = 0.0                     
                pUrbData%ETa(indxElem,1)             = 0.0                     
                pUrbData%Perc(indxElem,1)            = 0.0                    
                pUrbData%ReturnFlowIn(indxElem,1)    = 0.0
                pUrbData%ReturnFlowOut(indxElem,1)   = 0.0
                pUrbData%IrigInfilt(indxElem,1)      = 0.0
                pUrbData%Reuse(indxElem,1)           = 0.0
                pUrbData%GMExcess(indxElem,1)        = 0.0
                pUrbData%ETFromGW_Actual(indxElem,1) = 0.0
                rElemSupply_Indrs_Local(indxElem)    = 0.0
                CYCLE
            END IF
              
            !Cycle if it is a lake element
            IF (lLakeElem(indxElem)) THEN
                rElemSupply_Indrs_Local(indxElem) = 0.0
                CYCLE
            END IF
            
            !Initialize
            RootDepth          = UrbanLand%RootDepth(indxElem)
            WiltingPoint       = SoilsData(indxElem)%WiltingPoint
            FieldCapacity      = SoilsData(indxElem)%FieldCapacity
            TotalPorosity      = SoilsData(indxElem)%TotalPorosity
            HydCond            = SoilsData(indxElem)%HydCond
            Lambda             = SoilsData(indxElem)%Lambda
            KunsatMethod       = SoilsData(indxElem)%KunsatMethod
            PrecipD            = Precip(indxElem) * DeltaT
            GM                 = GenericMoisture(1,indxElem) * RootDepth * DeltaT
            Area_Outdoors      = Area * pUrbData%PerviousFrac(indxElem,1)
            Area_Indoors       = Area - Area_Outdoors 
            WiltingPointUrban  = WiltingPoint  * RootDepth
            FieldCapacityUrban = FieldCapacity * RootDepth
            TotalPorosityUrban = TotalPorosity * RootDepth
            rSoilM_Precip_P    = pUrbData%SoilM_Precip_P(indxElem,1)
            rSoilM_AW_P        = pUrbData%SoilM_AW_P(indxElem,1)
            rSoilM_Oth_P       = pUrbData%SoilM_Oth_P(indxElem,1)
            rSoilM_P           = rSoilM_Precip_P + rSoilM_AW_P + rSoilM_Oth_P
            IF (Area_Indoors .EQ. 0.0) THEN
                AW_Indoors  = 0.0
                AW_Outdoors = ElemSupply(indxElem) / Area_Outdoors
            ELSEIF (Area_Outdoors .EQ. 0.0) THEN
                AW_Indoors  = ElemSupply(indxElem) / Area_Indoors
                AW_Outdoors = 0.0
            ELSE
                rIndoorFrac = UrbanLand%WaterUseSpecsFile%rValues(pUrbData%iColWaterUseSpec(indxElem,1))
                AW_Indoors  = ElemSupply(indxElem)*rIndoorFrac       / Area_Indoors
                AW_Outdoors = ElemSupply(indxElem)*(1d0-rIndoorFrac) / Area_Outdoors
            END IF
            rElemSupply_Indrs_Local(indxElem) = AW_Indoors * Area_Indoors
            
            !Infiltration and return flow due to applied water
            Supply         = AW_Outdoors * DeltaT
            fRF            = ReturnFrac(pUrbData%iColReturnFrac(indxElem,1))
            fRU            = ReuseFrac(pUrbData%iColReuseFrac(indxElem,1))
            rIrigInfilt    = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
            rReturnFlowOut = Supply - rIrigInfilt
            
            !Total inflow to the root zone
            Inflow = GM + rIrigInfilt
            
            !ET from GW
            rETFromGW_Actual = MIN(ETc(indxElem)*DeltaT , pUrbData%ETFromGW_Max(indxElem,1))
            ETc_effect       = ETc(indxElem)*DeltaT - rETFromGW_Actual
            
            !Simulate
            CALL NonPondedLUMoistureRouter(PrecipD                                    ,  &
                                           pUrbData%rInitialAbstraction(indxElem,1)   ,  &
                                           pUrbData%SMax(indxElem,1)                  ,  &
                                           rSoilM_P                                   ,  &
                                           ETc_effect                                 ,  & 
                                           HydCond                                    ,  & 
                                           TotalPorosityUrban                         ,  & 
                                           FieldCapacityUrban                         ,  & 
                                           WiltingPointUrban                          ,  &
                                           Lambda                                     ,  & 
                                           Inflow                                     ,  &
                                           SolverData%Tolerance*TotalPorosityUrban    ,  &
                                           KunsatMethod                               ,  &
                                           SolverData%IterMax                         ,  &
                                           SoilM                                      ,  & 
                                           rRunoff                                    ,  & 
                                           rPrecipInfilt                              ,  & 
                                           rETa                                       ,  & 
                                           rPerc                                      ,  & 
                                           Excess                                     ,  &
                                           AchievedConv                               ) 
            
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Convergence error in soil moisture routing for urban lands!'
                MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityUrban
                WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                CYCLE
            END IF
            
            !Reduce inflows based on correction for total porosity
            rGMExcess = 0.0
            IF (Excess .NE. 0.0) THEN
                ratio = [rPrecipInfilt , rIrigInfilt , GM]
                CALL NormalizeArray(ratio)
                rRunoff        = rRunoff        + Excess * ratio(1) 
                rReturnFlowOut = rReturnFlowOut + Excess * ratio(2)
                rGMExcess      = Excess * ratio(3)
                rPrecipInfilt  = PrecipD - rRunoff
                rIrigInfilt    = Supply  - rReturnFlowOut
            END IF
            
            !Compute re-use based on return flow
            rReuse = 0.0
            IF (ReturnFrac(pUrbData%iColReturnFrac(indxElem,1)) .GT. 0.0) rReuse = rReturnFlowOut * fRU / fRF
            
            !Compute moisture from precip and irrigation
            SoilM_P_Array = [rSoilM_Precip_P , rSoilM_AW_P , rSoilM_Oth_P]
            Infilt        = [rPrecipInfilt   , rIrigInfilt , GM - rGMExcess]
            CALL TrackMoistureDueToSource(SoilM_P_Array ,  &
                                          Infilt        ,  &
                                          rPerc         ,  &
                                          rETa          ,  &
                                          0d0           ,  &
                                          SoilM_Array   ,  &
                                          ETPartition   )
            pUrbData%SoilM_Precip(indxElem,1) = SoilM_Array(1)
            pUrbData%SoilM_AW(indxElem,1)     = SoilM_Array(2)
            pUrbData%SoilM_Oth(indxElem,1)    = SoilM_Array(3)
            
            !Make sure soil moisture is not less than zero
            IF (ANY(SoilM_Array.LT.0.0)) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Soil moisture content becomes negative at element '//TRIM(IntToText(iElemID))//'.'
                MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
                MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
                MessageArray(4) = 'in the Root Zone Main Input File.'
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                CYCLE
            END IF
            
            !Convert depths to volumetric rates
            rMultip          = Area_Outdoors / DeltaT
            rRunoff          = rRunoff          * rMultip + Precip(indxElem) * Area_Indoors
            rReturnFlowOut   = rReturnFlowOut   * rMultip 
            rReturnFlowIn    = AW_Indoors       * Area_Indoors
            rPrecipInfilt    = rPrecipInfilt    * rMultip
            rIrigInfilt      = rIrigInfilt      * rMultip
            rPerc            = rPerc            * rMultip
            rReuse           = rReuse           * rMultip
            rETFromGW_Actual = rETFromGW_Actual * rMultip
            rETa             = rETa             * rMultip + rETFromGW_Actual  !Includes ET from groundwater
            
            !If surface flow goes to groundwater, update the runoff processes
            IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
                rPerc          = rPerc + rRunoff + rReturnFlowOut + rReturnFlowIn
                rPrecipInfilt  = rPrecipInfilt + rRunoff                       !Runoff and 
                rIrigInfilt    = rIrigInfilt + rReturnFlowOut + rReturnFlowIn  ! return flow are assumed to bypass root zone for proper mass balance       
                rRunoff        = 0.0
                rReturnFlowIn  = 0.0
                rReturnFlowOut = 0.0
            END IF
            
            !Store results in persistent arrays
            pUrbData%Runoff(indxElem,1)          = rRunoff                     
            pUrbData%PrecipInfilt(indxElem,1)    = rPrecipInfilt                     
            pUrbData%ETa(indxElem,1)             = rETa                     
            pUrbData%Perc(indxElem,1)            = rPerc                    
            pUrbData%ReturnFlowIn(indxElem,1)    = rReturnFlowIn
            pUrbData%ReturnFlowOut(indxElem,1)   = rReturnFlowOut
            pUrbData%IrigInfilt(indxElem,1)      = rIrigInfilt
            pUrbData%Reuse(indxElem,1)           = rReuse
            pUrbData%GMExcess(indxElem,1)        = rGMExcess
            pUrbData%ETFromGW_Actual(indxElem,1) = rETFromGW_Actual

        END DO
        !$OMP END DO
        !$OMP END PARALLEL
    END ASSOCIATE
    
  END SUBROUTINE Simulate
  
  
  
  
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
  ! --- COMPUTE GW INFLOW INTO ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE UrbanLandUse_ComputeETFromGW_Max(UrbanLand,DepthToGW,Sy,CapillaryRise)
    CLASS(UrbanLandUse_v41_Type) :: UrbanLand
    REAL(8),INTENT(IN)           :: DepthToGW(:),Sy(:),CapillaryRise(:)
    
    !Local variables
    REAL(8) :: RootDepth(1,SIZE(Sy)),Area(1,SIZE(Sy)),GWInflow(1,SIZE(Sy))
    
    !Initialize
    RootDepth(1,:) = UrbanLand%RootDepth
    Area(1,:)      = UrbanLand%UrbData%Area(:,1) * UrbanLand%UrbData%PerviousFrac(:,1)
    
    CALL ComputeETFromGW_Max(DepthToGW,Sy,RootDepth,CapillaryRise,Area,GWInflow)
    
    UrbanLand%UrbData%ETFromGW_Max(:,1) = GWInflow(1,:)
    
  END SUBROUTINE UrbanLandUse_ComputeETFromGW_Max
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT SOIL INITIAL MOISTURE CONTENTS TO DEPTHS
  ! ---  Note: Called only once at the beginning of simulation
  ! -------------------------------------------------------------
  SUBROUTINE SoilMContent_To_Depth(UrbanLand,NElements,iElemIDs,TotalPorosity,iStat)
    CLASS(UrbanLandUse_v41_Type) :: UrbanLand
    INTEGER,INTENT(IN)           :: NElements,iElemIDs(NElements)
    REAL(8),INTENT(IN)           :: TotalPorosity(:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxElem
    REAL(8)                      :: RootDepth,rDiff
    
    !Initialize
    iStat = 0
    
    !Return if urban lands are not simulated
    IF (.NOT.ALLOCATED(UrbanLand%UrbData%SMax)) RETURN
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    ASSOCIATE (pUrbData => UrbanLand%UrbData) 
        DO indxElem=1,NElements
            rDiff = pUrbData%SoilM_Precip(indxElem,1) + pUrbData%SoilM_AW(indxElem,1) + pUrbData%SoilM_Oth(indxElem,1) - TotalPorosity(indxElem)
            IF (rDiff .GT. 1D-12) THEN  !Secure against roundoff errors
                CALL SetLastMessage('Initial moisture content for urban land at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            RootDepth                           = UrbanLand%RootDepth(indxElem)
            pUrbData%SoilM_Precip(indxElem,1)   = pUrbData%SoilM_Precip(indxElem,1) * RootDepth
            pUrbData%SoilM_AW(indxElem,1)       = pUrbData%SoilM_AW(indxElem,1) * RootDepth
            pUrbData%SoilM_Oth(indxElem,1)      = pUrbData%SoilM_Oth(indxElem,1) * RootDepth
            pUrbData%SoilM_Precip_P(indxElem,1) = pUrbData%SoilM_Precip(indxElem,1)
            pUrbData%SoilM_AW_P(indxElem,1)     = pUrbData%SoilM_AW(indxElem,1)
            pUrbData%SoilM_Oth_P(indxElem,1)    = pUrbData%SoilM_Oth(indxElem,1)
        END DO 
    END ASSOCIATE
    
  END SUBROUTINE SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(UrbanLand) 
    CLASS(UrbanLandUse_v41_Type) :: UrbanLand
    
    UrbanLand%UrbData%Area_P = UrbanLand%UrbData%Area
    
  END SUBROUTINE AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- COMPUTE URBAN WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(UrbanLand)
    CLASS(UrbanLandUse_v41_Type) :: UrbanLand

    ASSOCIATE (pUrbData => UrbanLand%UrbData)
        !Element demand is calculated based on urban area fractions
        IF (UrbanLand%lFracDemand_wrt_Area) THEN
            IF (UrbanLand%PopulationFile%lUpdated  .OR.  UrbanLand%PerCapitaWaterUseFile%lUpdated) THEN
                !Update element demand fractions if needed
                IF (UrbanLand%LandUseDataFile%lUpdated) THEN
                    !Set FracDemand to urban area in each element
                    pUrbData%FracDemand = pUrbData%Area    
                    !Normalize FracDemand
                    CALL CalculateUrbanFracDemand(SIZE(pUrbData%iColPopulation),pUrbData%iColPopulation,pUrbData%iColPerCapitaWaterUse,pUrbData%FracDemand)
                END IF
                
                !Calculate demand
                pUrbData%Demand(:,1) = UrbanLand%PopulationFile%iValues(pUrbData%iColPopulation(:,1)) * UrbanLand%PerCapitaWaterUseFile%rValues(pUrbData%iColPerCapitaWaterUse(:,1))* pUrbData%FracDemand(:,1)                
            END IF
            
        !Element demand is calculated based on user-specified static fractions
        ELSE
            IF (UrbanLand%PopulationFile%lUpdated  .OR.  UrbanLand%PerCapitaWaterUseFile%lUpdated) THEN
                pUrbData%Demand(:,1) = UrbanLand%PopulationFile%iValues(pUrbData%iColPopulation(:,1)) * UrbanLand%PerCapitaWaterUseFile%rValues(pUrbData%iColPerCapitaWaterUse(:,1))* pUrbData%FracDemand(:,1)
            END IF
            
        END IF
    END ASSOCIATE
       
  END SUBROUTINE ComputeWaterDemand

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(UrbanLand,iElemIDs,rElemAreas,TimeStep,iStat)
    CLASS(UrbanLandUse_v41_Type)  :: UrbanLand
    INTEGER,INTENT(IN)            :: iElemIDs(:)
    REAL(8),INTENT(IN)            :: rElemAreas(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep 
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    CALL UrbanLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                   ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL UrbanLand%PopulationFile%File%RewindFile_To_BeginningOfTSData(iStat)                         ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%PopulationFile%ReadTSData(TimeStep,'Population data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL UrbanLand%PerCapitaWaterUseFile%File%RewindFile_To_BeginningOfTSData(iStat)                                   ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%PerCapitaWaterUseFile%ReadTSData(TimeStep,'Per capita water use data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iFileReadCode .EQ. f_IO_SuccessfulRead) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor

    CALL UrbanLand%WaterUseSpecsFile%File%RewindFile_To_BeginningOfTSData(iStat)                                       ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%WaterUseSpecsFile%ReadTSData(TimeStep,'Urban water use specifications',iFileReadCode,iStat)
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE