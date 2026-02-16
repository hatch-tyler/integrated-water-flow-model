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
MODULE RootZone_v413                                                                         
  !$ USE OMP_LIB  
  USE IWFM_Kernel_Version     , ONLY: IWFMKernelVersion
  USE MessageLogger           , ONLY: SetLastMessage                                          , &
                                      EchoProgress                                            , &
                                      MessageArray                                            , &
                                      f_iFatal                                                                               
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter                                 , &
                                      CleanSpecialCharacters                                  , &
                                      EstablishAbsolutePathFileName                           , &
                                      ConvertID_To_Index                                      , &
                                      LocateInList                                            , &
                                      IntToText, &
                                   f_cInlineCommentChar
  USE TimeSeriesUtilities     , ONLY: TimeStepType                                            , &                             
                                      IncrementTimeStamp                                      , &
                                      NPeriods                                                , &
                                      OPERATOR(.TSGE.)
  USE IOInterface             , ONLY: GenericFileType                                         , &
                                      RealTSDataInFileType                                    
  USE Package_Misc            , ONLY: f_iLandUse_Ag                                           , &
                                      f_iLandUse_Urb                                          , &
                                      f_iSupply_Diversion                           
  USE Package_Discretization  , ONLY: AppGridType                                             
  USE Package_PrecipitationET , ONLY: PrecipitationType                                       , &
                                      ETType                                                 
  USE Package_UnsatZone       , ONLY: f_iKUnsatMethodList                                    
  USE Package_Budget          , ONLY: f_iMaxLocationNameLen_Budget  => f_iMaxLocationNameLen     
  USE Util_RootZone_v41       , ONLY: AgRootZoneBudRawFile_New                                , &
                                      LUAreaScaleFactorOutFile_New
  USE RootZone_v412           , ONLY: RootZone_v412_Type                                      , &
                                      AgLWUseBudRawFile_New                                   , &                               
                                      NonPondedAgRootZoneBudRawFile_New                       , &         
                                      PondedAgRootZoneBudRawFile_New                                
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
  PUBLIC :: RootZone_v413_Type 
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RootZone_v412_Type) :: RootZone_v413_Type
      PRIVATE
      INTEGER,ALLOCATABLE        :: iColCropCoeff_NonPondedAg(:,:)            !Column number for non-ponded ag crop coefficient for each (non-ponded crop,element)  
      INTEGER,ALLOCATABLE        :: iColCropCoeff_PondedAg(:,:)               !Column number for ponded ag crop coefficient for each (non-ponded crop,element) 
      INTEGER,ALLOCATABLE        :: iColCropCoeff_Urb(:)                      !Column number for urban ET coefficient for each (element)  
      INTEGER,ALLOCATABLE        :: iColCropCoeff_NVRV(:,:)                   !Column number for native and riparian veg habitat coefficient for each (element)  
  CONTAINS
      PROCEDURE,PASS :: New                      => RootZone_v413_New
      PROCEDURE,PASS :: KillRZImplementation     => RootZone_v413_Kill
      PROCEDURE,PASS :: ComputeWaterDemand       => RootZone_v413_ComputeWaterDemand
      PROCEDURE,PASS :: ComputeFutureWaterDemand => RootZone_v413_ComputeFutureWaterDemand
      PROCEDURE,PASS :: ReadTSData               => RootZone_v413_ReadTSData
      PROCEDURE,PASS :: PrintResults             => RootZone_v413_PrintResults
      PROCEDURE,PASS :: Simulate                 => RootZone_v413_Simulate
  END TYPE RootZone_v413_Type

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'RootZone_v413::'
  

  
  
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
  ! --- NEW ROOT ZONE DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_New(RootZone,IsForInquiry,cProjectNameForDSS,cFileName,cCropCoeffFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs,iLakeIDs)
    CLASS(RootZone_v413_Type)          :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cProjectNameForDSS,cFileName,cWorkingDirectory
    CHARACTER(LEN=*),INTENT(IN)        :: cCropCoeffFileName   
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    INTEGER,OPTIONAL,INTENT(IN)        :: iStrmNodeIDs(:),iLakeIDs(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19)                :: ThisProcedure = ModName // 'RootZone_v413_New'
    CHARACTER(LEN=1000)                         :: cALine,cNonPondedCropFile,cRiceRefugeFile,cUrbanDataFile,cNVRVFile,cAgWaterDemandFile,cGenericMoistureFile
    CHARACTER                                   :: cVersionFull*250
    REAL(8)                                     :: rFACTK,rFactCN,rRegionArea(AppGrid%NSubregions+1),rDummyFactor(1),rDummyArray16(AppGrid%NElements,16), &
                                                   rFACTEXDTH                                                                                           
    INTEGER                                     :: iNElements,iNRegion,iErrorCode,indxElem,iColGenericMoisture(AppGrid%NElements),iElemID,iElem,          &
                                                   iElemIDs(AppGrid%NElements),iSubregionIDs(AppGrid%NSubregions),iFlagETFromGW
    TYPE(GenericFileType)                       :: RootZoneParamFile
    LOGICAL                                     :: lTrackTime,lProcessed(AppGrid%NElements),lLUAreaScaleOutFileDefined
    CHARACTER(LEN=f_iMaxLocationNameLen_Budget) :: cRegionNames(AppGrid%NSubregions+1)
    CHARACTER(:),ALLOCATABLE                    :: cAbsPathFileName
    CHARACTER(LEN=20),ALLOCATABLE               :: cEntryLines(:)
    PROCEDURE(AgLWUseBudRawFile_New),POINTER    :: pAgLWUseBudRawFile_New
    PROCEDURE(AgRootZoneBudRawFile_New),POINTER :: pPondedAgRootZoneBudRawFile_New,pNonPondedAgRootZoneBudRawFile_New
    
    !Initialize
    iStat = 0
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating root zone')

    !Initialize
    cVersionFull                       = 'v4.13-' // TRIM(IWFMKernelVersion%GetVersion())
    iNElements                         = AppGrid%NElements
    iNRegion                           = AppGrid%NSubregions
    iElemIDs                           = AppGrid%AppElement%ID
    iSubregionIDs                      = AppGrid%AppSubregion%ID
    lTrackTime                         = TimeStep%TrackTime
    rRegionArea(1:iNRegion)            = AppGrid%GetSubregionAreaForAll()
    rRegionArea(iNRegion+1)            = SUM(rRegionArea(1:iNRegion))
    cRegionNames                       = ''  ;  cRegionNames(1:iNRegion) = AppGrid%GetSubregionNames()
    cRegionNames(iNRegion+1)           = 'ENTIRE MODEL AREA'
    pAgLWUseBudRawFile_New             => AgLWUseBudRawFile_New
    pPondedAgRootZoneBudRawFile_New    => PondedAgRootZoneBudRawFile_New
    pNonPondedAgRootZoneBudRawFile_New => NonPondedAgRootZoneBudRawFile_New
    
    !Make sure a crop coefficient filename is defined
    IF (LEN_TRIM(cCropCoeffFileName) .EQ. 0) THEN
        CALL SetLastMessage('A crop/habitat coefficient file needs to be defined in the Main Control Input File for Root Zone Component v4.13!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Allocate memory
    ALLOCATE (RootZone%ElemSoilsData(iNElements)                          , &
              RootZone%HydCondPonded(iNElements)                          , &
              RootZone%ElemPrecipData(iNElements)                         , &
              RootZone%ElemSupply(iNElements)                             , &
              RootZone%ElemDevelopedArea(iNElements)                      , &
              RootZone%Ratio_ElemSupplyToRegionSupply_Ag(iNElements)      , &
              RootZone%Ratio_ElemSupplyToRegionSupply_Urb(iNElements)     , &
              RootZone%RSoilM_P(iNRegion+1,3)                             , &  !Includes pond storage; 2nd Dim: 1 = Ag; 2 = Urban; 3 = Native & Riparain Veg
              RootZone%RSoilM(iNRegion+1,3)                               , &  !Includes pond storage; 2nd Dim: 1 = Ag; 2 = Urban; 3 = Native & Riparain Veg
              RootZone%Flags%lLakeElems(iNElements)                       , &
              !RootZone%iColCropCoeff_NonPondedAg(iNCrops,iNElements)     , &  !This array will be allocated in NonPondedAg class since we don't yet know iNCrops value
              !RootZone%iColCropCoeff_PondedAg(iNPondedCrops,iNElements)  , &  !This array will be allocated in PondedAg class since we don't yet know iNPondedCrops value 
              !RootZone%iColCropCoeff_Urb(iNElements)                     , &  !This array will be allocated in UrbanAg class
              !RootZone%iColCropCoeff_NVRV(iNNVRViNElements)              , &  !This array will be allocated in NVRV class
              RootZone%iColSurfaceFlowDestination_Ag(iNElements)          , &
              RootZone%iColSurfaceFlowDestination_UrbIndoors(iNElements)  , &
              RootZone%iColSurfaceFlowDestination_UrbOutdoors(iNElements) , &
              RootZone%iColSurfaceFlowDestination_NVRV(iNElements)        , &
              STAT=iErrorCode                                             )
    IF (iErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for root zone soils data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Stream node and lake IDs
    IF (PRESENT(iStrmNodeIDs)) THEN
        ALLOCATE(RootZone%iStrmNodeIDs(SIZE(iStrmNodeIDs)))
        RootZone%iStrmNodeIDs = iStrmNodeIDs
        RootZone%lStrmNodeIDs_Provided = .TRUE.
    ELSE
        ALLOCATE(RootZone%iStrmNodeIDs(0))
        RootZone%lStrmNodeIDs_Provided = .FALSE.
    END IF
    IF (PRESENT(iLakeIDs)) THEN
        ALLOCATE(RootZone%iLakeIDs(SIZE(iLakeIDs)))
        RootZone%iLakeIDs = iLakeIDs
        RootZone%lLakeIDs_Provided = .TRUE.
    ELSE
        ALLOCATE(RootZone%iLakeIDs(0))
        RootZone%lLakeIDs_Provided = .FALSE.
    END IF
    
    !Initialize lake element flag
    RootZone%Flags%lLakeElems = .FALSE.
    
    !Open file
    CALL RootZoneParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read away the first version number line to avoid any errors
    CALL RootZoneParamFile%ReadData(cALine,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !BACKWARD COMPATIBILITY: Check the number of input lines to see if Land Use Area Scaling Output File is defined
    CALL RootZoneParamFile%ReadData(cEntryLines,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (SIZE(cEntryLines) .EQ. 18) THEN
        lLUAreaScaleOutFileDefined = .TRUE.
    ELSE
        lLUAreaScaleOutFileDefined = .FALSE.
    END IF
    CALL RootZoneParamFile%BackspaceFile(SIZE(cEntryLines))

    !Read solution scheme controls
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%IterMax,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(rFactCN,iStat)                        ;  IF (iStat .EQ. -1) RETURN
    
    !Read flag to see if ET from groundwater will be simulated
    CALL RootZoneParamFile%ReadData(iFlagETFromGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
    SELECT CASE (iFlagETFromGW)
        CASE (0)
            RootZone%Flags%lComputeETFromGW = .FALSE.
        CASE (1)
            RootZone%Flags%lComputeETFromGW = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Flag to simulate root water uptake from groundwater is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT

    !Initialize related files
    !-------------------------
    
    !Non-ponded crops data file
    CALL RootZoneParamFile%ReadData(cNonPondedCropFile,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    cNonPondedCropFile = StripTextUntilCharacter(cNonPondedCropFile,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cNonPondedCropFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cNonPondedCropFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%NonPondedAgRootZone%New(IsForInquiry                                                   , &
                                          cProjectNameForDSS                                             , &
                                          cAbsPathFileName                                               , &
                                          cWorkingDirectory                                              , &
                                          rFactCN                                                        , &
                                          AppGrid                                                        , &
                                          iElemIDs                                                       , &
                                          TimeStep                                                       , &
                                          NTIME                                                          , &
                                          TRIM(cVersionFull)                                             , &
                                          iStat                                                          , &
                                          pAgLWUseBudRawFile_New    = pAgLWUseBudRawFile_New             , &
                                          pAgRootZoneBudRawFile_New = pNonPondedAgRootZoneBudRawFile_New , &
                                          iColCropCoeff             = RootZone%iColCropCoeff_NonPondedAg )
    IF (iStat .EQ. -1) RETURN
       
    !Rice/refuge data file
    CALL RootZoneParamFile%ReadData(cRiceRefugeFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cRiceRefugeFile = StripTextUntilCharacter(cRiceRefugeFile,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cRiceRefugeFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cRiceRefugeFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%PondedAgRootZone%New(IsForInquiry                                                , &
                                       .TRUE.                                                      , &
                                       cProjectNameForDSS                                          , &
                                       cAbsPathFileName                                            , &
                                       cWorkingDirectory                                           , &
                                       rFactCN                                                     , &
                                       AppGrid                                                     , &
                                       iElemIDs                                                    , &
                                       TimeStep                                                    , &
                                       NTIME                                                       , &
                                       TRIM(cVersionFull)                                          , &
                                       iStat                                                       , &
                                       pAgLWUseBudRawFile_New    = pAgLWUseBudRawFile_New          , &
                                       pAgRootZoneBudRawFile_New = pPondedAgRootZoneBudRawFile_New , &
                                       iColCropCoeff             = RootZone%iColCropCoeff_PondedAg )
    IF (iStat .EQ. -1) RETURN
    
    !Urban data file
    CALL RootZoneParamFile%ReadData(cUrbanDataFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cUrbanDataFile = StripTextUntilCharacter(cUrbanDataFile,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cUrbanDataFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cUrbanDataFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%UrbanRootZone%New(cAbsPathFileName                            , &
                                    cWorkingDirectory                           , &
                                    rFactCN                                     , &
                                    iNElements                                  , &
                                    iNRegion                                    , &
                                    iElemIDs                                    , &
                                    lTrackTime                                  , &
                                    iStat                                       , &
                                    iColUrbETCoeff = RootZone%iColCropCoeff_Urb )
    IF (iStat .EQ. -1) RETURN
    
    !Native/riparian veg. data file
    CALL RootZoneParamFile%ReadData(cNVRVFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cNVRVFile = StripTextUntilCharacter(cNVRVFile,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cNVRVFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cNVRVFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%NVRVRootZone%New(.TRUE.                                              , &
                                   cAbsPathFileName                                    , &
                                   cWorkingDirectory                                   , &
                                   rFactCN                                             , &
                                   iNElements                                          , &
                                   iNRegion                                            , &
                                   iElemIDs                                            , &
                                   lTrackTime                                          , &
                                   iStat                                               , &
                                   iStrmNodeIDs          = iStrmNodeIDs                , &
                                   iColHabitatCoeff_NVRV = RootZone%iColCropCoeff_NVRV )
    IF (iStat .EQ. -1) RETURN
    
    !Check if at least one type of land use is specified
    IF (cNonPondedCropFile .EQ. '' ) THEN
        IF (cRiceRefugeFile .EQ. '' ) THEN
            IF (cUrbanDataFile .EQ. '') THEN
                IF (cNVRVFile .EQ. '')  THEN
                    MessageArray(1) = 'At least one type of land use and related data should '
                    MessageArray(2) = 'be specified for the simulation of root zone processes!' 
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
        END IF
    END IF
    
    !Define the component simulation flags
    ASSOCIATE (pFlags => RootZone%Flags)
        IF (RootZone%NonPondedAgRootZone%NCrops .GT. 0)  pFlags%lNonPondedAg_Defined = .TRUE.
        IF (cRiceRefugeFile .NE. '')                     pFlags%lPondedAg_Defined    = .TRUE.
        IF (cUrbanDataFile .NE. '')                      pFlags%lUrban_Defined       = .TRUE.
        IF (cNVRVFile .NE. '')                           pFlags%lNVRV_Defined        = .TRUE.
    END ASSOCIATE
    
    !Total number of land uses
    RootZone%NLands = 0
    IF (RootZone%Flags%lNonPondedAg_Defined) RootZone%NLands = RootZone%NonPondedAgRootZone%NCrops 
    IF (RootZone%Flags%lPondedAg_Defined)    RootZone%NLands = RootZone%NLands + RootZone%PondedAgRootZone%iNCrops  
    IF (RootZone%Flags%lUrban_Defined)       RootZone%NLands = RootZone%NLands + 1
    IF (RootZone%Flags%lNVRV_Defined)        RootZone%NLands = RootZone%NLands + RootZone%NVRVRootZone%iNNVRV 
    
    !Return flow data file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing return flow fractions data file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReturnFracFile%Init(cAbsPathFileName,cWorkingDirectory,'Return flow fractions data file',lTrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Re-use data file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing irrigation water re-use factors data file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReuseFracFile%Init(cAbsPathFileName,cWorkingDirectory,'Irrigation water re-use factors file',lTrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Irrigation period data file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined) THEN
            CALL SetLastMessage('Missing irrigation period data file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%IrigPeriodFile%Init(cAbsPathFileName,cWorkingDirectory,'Irrigation period data file',lTrackTime,1,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Generic moisture data file
    CALL RootZoneParamFile%ReadData(cGenericMoistureFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cGenericMoistureFile = StripTextUntilCharacter(cGenericMoistureFile,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cGenericMoistureFile)
    IF (cGenericMoistureFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cGenericMoistureFile)),cWorkingDirectory,cAbsPathFileName)
        cGenericMoistureFile = cGenericMoistureFile
        RootZone%Flags%lGenericMoistureFile_Defined = .TRUE.
    END IF
    
    !Agricultural water demand file
    CALL RootZoneParamFile%ReadData(cAgWaterDemandFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cAgWaterDemandFile = StripTextUntilCharacter(cAgWaterDemandFile,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cAgWaterDemandFile)
    IF (cAgWaterDemandFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cAgWaterDemandFile)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%AgWaterDemandFile%Init(cAbsPathFileName,cWorkingDirectory,'Agricultural water supply requirement file',lTrackTime,1,.TRUE.,rDummyFactor,[.TRUE.],iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        RootZone%AgWaterDemandFactor = rDummyFactor(1)
    END IF  

    !Land and water use budget HDF5 output file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%LWUseBudRawFile_New(IsForInquiry,cProjectNameForDSS,cAbsPathFileName,TimeStep,NTIME,iNRegion+1,rRegionArea,cRegionNames,'land and water use budget',TRIM(cVersionFull),RootZone%LWUseBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%LWUseBudRawFile_Defined = .TRUE.      
    END IF

    !Root zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%RootZoneBudRawFile_New(IsForInquiry,cProjectNameForDSS,cAbsPathFileName,TimeStep,NTIME,iNRegion+1,rRegionArea,cRegionNames,'root zone budget',TRIM(cVersionFull),RootZone%RootZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%RootZoneBudRawFile_Defined = .TRUE.
    END IF
       
    !Land and water use zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%LWUseZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,TRIM(cVersionFull),RootZone%Flags,AppGrid,RootZone%LWUZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%LWUseZoneBudRawFile_Defined = .TRUE.      
    END IF

    !Root zone zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%RootZoneZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,TRIM(cVersionFull),RootZone%Flags,AppGrid,RootZone%RootZoneZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%RootZoneZoneBudRawFile_Defined = .TRUE.
    END IF
       
    !Land use area scaling factor output file
    IF (lLUAreaScaleOutFileDefined) THEN
        CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
        cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
        CALL CleanSpecialCharacters(cALine)
        IF (cALine .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
            CALL LUAreaScaleFactorOutFile_New(IsForInquiry,cAbsPathFileName,iElemIDs,RootZone%LUAreaScaleFactorOutFile,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF
    END IF
       
    !Conversion factors for soil parameters
    CALL RootZoneParamFile%ReadData(rFACTK,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(rFACTEXDTH,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(cALine,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(cALine)
    RootZone%VarTimeUnit = ADJUSTL(StripTextUntilCharacter(cALine,f_cInlineCommentChar))

    !Surface flow destinations file
    CALL RootZoneParamFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    IF (cALine .EQ. '') THEN
        CALL SetLastMessage('Surface flow destinations data file is missing for the Root Zone component!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%SurfaceFlowDestinationFile%Init(cAbsPathFileName,cWorkingDirectory,'Surface flow destinations data file',.TRUE.,1,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Read soil parameters and surface flow destinations
    CALL RootZoneParamFile%ReadData(rDummyArray16,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pSoilsData  => RootZone%ElemSoilsData  , &
               pPrecipData => RootZone%ElemPrecipData )
        lProcessed = .FALSE.
        DO indxElem=1,iNElements
            iElemID = INT(rDummyArray16(indxElem,1))
            
            !Check if element is in the model
            CALL ConvertID_To_Index(iElemID,iElemIDs,iElem)
            IF (iElem .EQ. 0) THEN
                CALL SetLastMessage('Element '//TRIM(IntToText(iElemID))//' listed for root zone parameter definitions is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Check if it was defined before
            IF (lProcessed(iElem)) THEN
                CALL SetLastMessage('Element '//TRIM(IntToText(iElemID))//' is listed more than once for root zone parameter definitions!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Process data
            lProcessed(iElem)                                      = .TRUE.
            pSoilsData(iElem)%WiltingPoint                         =     rDummyArray16(indxElem,2)      
            pSoilsData(iElem)%FieldCapacity                        =     rDummyArray16(indxElem,3)
            pSoilsData(iElem)%TotalPorosity                        =     rDummyArray16(indxElem,4)
            pSoilsData(iElem)%Lambda                               =     rDummyArray16(indxElem,5)
            pSoilsData(iElem)%HydCond                              =     rDummyArray16(indxElem,6) * rFACTK * TimeStep%DeltaT
            RootZone%HydCondPonded(iElem)                          =     rDummyArray16(indxElem,7)
            pSoilsData(iElem)%KunsatMethod                         = INT(rDummyArray16(indxElem,8))
            pSoilsData(iElem)%CapillaryRise                        =     rDummyArray16(indxElem,9) * rFACTEXDTH
            pPrecipData(iElem)%iColPrecip                          = INT(rDummyArray16(indxElem,10))
            pPrecipData(iElem)%PrecipFactor                        =     rDummyArray16(indxElem,11)
            iColGenericMoisture(iElem)                             = INT(rDummyArray16(indxElem,12))
            RootZone%iColSurfaceFlowDestination_Ag(iElem)          = INT(rDummyArray16(indxElem,13))
            RootZone%iColSurfaceFlowDestination_UrbIndoors(iElem)  = INT(rDummyArray16(indxElem,14))
            RootZone%iColSurfaceFlowDestination_UrbOutdoors(iElem) = INT(rDummyArray16(indxElem,15))
            RootZone%iColSurfaceFlowDestination_NVRV(iElem)        = INT(rDummyArray16(indxElem,16))
           
            !Process ponded Ksat
            IF (RootZone%HydCondPonded(iElem) .EQ. -1.0) THEN
                RootZone%HydCondPonded(iElem) = pSoilsData(iElem)%HydCond
            ELSE
                RootZone%HydCondPonded(iElem) = RootZone%HydCondPonded(iElem) * rFACTK * TimeStep%DeltaT
            END IF
            
            !Make sure hydraulic conductivity is greater than zero
            IF (pSoilsData(iElem)%HydCond .LT. 0.0) THEN
                CALL SetLastMessage('Root zone hydraulic conductivity at element '//TRIM(IntToText(iElemID))//' is less than zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (RootZone%HydCondPonded(iElem) .LT. 0.0) THEN
                CALL SetLastMessage('Root zone hydraulic conductivity for ponded crops at element '//TRIM(IntToText(iElemID))//' is less than zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Method to compute Kunsat must be recognized
            IF (LocateInList(pSoilsData(iElem)%KunsatMethod,f_iKunsatMethodList) .LT. 1) THEN
                CALL SetLastMessage('Method to compute unsaturated hydraulic conductivity at element '//TRIM(IntToText(iElemID))//' is not recognized!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF

            !Wilting point should be less than field capacity
            IF (pSoilsData(iElem)%WiltingPoint .GE. pSoilsData(iElem)%FieldCapacity) THEN
                CALL SetLastMessage('At element ' // TRIM(IntToText(iElemID)) // ' wilting point is greater than or equal to field capacity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        
            !Field capacity should be less than or equal to total porosity
            IF (pSoilsData(iElem)%FieldCapacity .GT. pSoilsData(iElem)%TotalPorosity) THEN
                CALL SetLastMessage('At element ' // TRIM(IntToText(iElemID)) // ' field capacity is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
      
        !Instantiate generic moisture data
        CALL RootZone%GenericMoistureData%New(cGenericMoistureFile,cWorkingDirectory,1,iNElements,iColGenericMoisture,lTrackTime,iStat)
        IF (iStat .EQ. -1) RETURN
    END ASSOCIATE
    
    !Flag to see if ag water demand will be read or not, check for inconsistencies as well
    IF (RootZone%NonPondedAgRootZone%NCrops.GT.0  .OR.  RootZone%PondedAgRootZone%iNCrops.GT.0) THEN
        !Check with non-ponded crops
        IF (ANY(RootZone%NonPondedAgRootZone%iColAgDemand.GT.0)) RootZone%Flags%lReadNonPondedAgWaterDemand = .TRUE.
        
        !Then, check with ponded crops
        IF (ANY(RootZone%PondedAgRootZone%Crops%iColAgDemand.GT.0)) RootZone%Flags%lReadPondedAgWaterDemand = .TRUE.
        
        !Are pointers defined without a defined ag water demand file?
        IF (cAgWaterDemandFile .EQ. '' ) THEN
            IF (RootZone%Flags%lReadNonPondedAgWaterDemand  .OR. RootZone%Flags%lReadPondedAgWaterDemand) THEN 
                CALL SetLastMessage('Data columns from agricultural water supply requirement file is referenced but this file is not specified!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END IF
    
    !Check if time series data column pointers are referring to existing data columns
    CALL CheckTSDataPointers(RootZone,iElemIDs,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL RootZoneParamFile%Kill()
        
  END SUBROUTINE RootZone_v413_New

  
  
  
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
  ! ---KILL ROOT ZONE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_Kill(RootZone)
    CLASS(RootZone_v413_Type) :: RootZone
    
    !Local variable
    INTEGER                  :: iErrorCode
    TYPE(RootZone_v413_Type) :: Dummy
    
    CALL RootZone%RootZone_v412_Type%KillRZImplementation()
    DEALLOCATE (RootZone%iColCropCoeff_NonPondedAg , &    
                RootZone%iColCropCoeff_PondedAg    , &    
                RootZone%iColCropCoeff_Urb         , &    
                RootZone%iColCropCoeff_NVRV        , &    
                STAT=iErrorCode                    )
    
    SELECT TYPE (RootZone)
        TYPE IS (RootZone_v413_Type)
            RootZone = Dummy
    END SELECT 

  END SUBROUTINE RootZone_v413_Kill

  
  
  
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
  ! --- READ ROOT ZONE RELATED TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas,iColCropCoeff_NonPondedAg,iColCropCoeff_PondedAg,iColCropCoeff_Urban,iColCropCoeff_NVRV)
    CLASS(RootZone_v413_Type)          :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ETData
    INTEGER,INTENT(OUT)                :: iStat
    REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)          !In (region, land use) format
    INTEGER,OPTIONAL,INTENT(IN)        :: iColCropCoeff_NonPondedAg(:,:),iColCropCoeff_PondedAg(:,:),iColCropCoeff_Urban(:),iColCropCoeff_NVRV(:,:)  !Not used in this version
    
    !Delgate to parent method but with proper crop coeffcient columns
    CALL RootZone%RootZone_v412_Type%ReadTSData(AppGrid                                                        , &
                                                TimeStep                                                       , &
                                                Precip                                                         , &
                                                ETData                                                         , &
                                                iStat                                                          , &
                                                RegionLUAreas             = RegionLUAreas                      , &
                                                iColCropCoeff_NonPondedAg = RootZone%iColCropCoeff_NonPondedAg , &
                                                iColCropCoeff_PondedAg    = RootZone%iColCropCoeff_PondedAg    , & 
                                                iColCropCoeff_Urban       = RootZone%iColCropCoeff_Urb         , &
                                                iColCropCoeff_NVRV        = RootZone%iColCropCoeff_NVRV        )
    
  END SUBROUTINE RootZone_v413_ReadTSData
  
  
  
  
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
  ! --- PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation,iColCropCoeff_NonPondedAg,iColCropCoeff_PondedAg,iColCropCoeff_Urban,iColCropCoeff_NVRV)
    CLASS(RootZone_v413_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETType),INTENT(IN)       :: ETData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep           !Not used in this version
    LOGICAL,INTENT(IN)            :: lEndOfSimulation   !Not used in this version
    INTEGER,OPTIONAL,INTENT(IN)   :: iColCropCoeff_NonPondedAg(:,:),iColCropCoeff_PondedAg(:,:),iColCropCoeff_Urban(:),iColCropCoeff_NVRV(:,:) !Not used in this version 

    !Delegate to parent method but with the proper crop coefficient column numbers
    CALL RootZone%RootZone_v412_Type%PrintResults(AppGrid                                                       , &
                                                  ETData                                                        , &
                                                  TimeStep                                                      , &
                                                  lEndOfSimulation                                              , &
                                                  iColCropCoeff_NonPondedAg = RootZone%iColCropCoeff_NonPondedAg, &
                                                  iColCropCoeff_PondedAg    = RootZone%iColCropCoeff_PondedAg   , &
                                                  iColCropCoeff_Urban       = RootZone%iColCropCoeff_Urb        , &
                                                  iColCropCoeff_NVRV        = RootZone%iColCropCoeff_NVRV       )
    
  END SUBROUTINE RootZone_v413_PrintResults
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DEMAND COMPUTATIONS AND ROUTING
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_ComputeWaterDemand(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v413_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iElemIDs(AppGrid%NElements),iNElements,iStatArray(3)
    
    !Initialize
    iStat      = 0
    iStatArray = 0
    iNElements = AppGrid%NElements
    iElemIDs   = AppGrid%AppElement%ID
    
    !Return if root zone is not simulated
    IF (RootZone%NLands .EQ. 0) RETURN
    
    !$OMP PARALLEL SECTIONS DEFAULT(PRIVATE) SHARED(RootZone,AppGrid,iNElements,iElemIDs,ETData,TimeStep,iStatArray)
    !$OMP SECTION
    !Riparian ET demand from streams
    IF (RootZone%Flags%lNVRV_Defined)  THEN
        CALL EchoProgress('Computing riparian ET demand from streams...')
        CALL RootZone%NVRVRootZone%ComputeWaterDemand(iNElements                                    , &
                                                      iElemIDs                                      , &
                                                      ETData                                        , &
                                                      TimeStep%DeltaT                               , &
                                                      RootZone%ElemPrecipData%Precip                , &
                                                      RootZone%GenericMoistureData%rGenericMoisture , &
                                                      RootZone%ElemSoilsData                        , &
                                                      RootZone%SolverData                           , &
                                                      RootZone%Flags%lLakeElems                     , &
                                                      iStatArray(1)                                 , &
                                                      iColCropCoeff_NVRV=RootZone%iColCropCoeff_NVRV)
    END IF
    
    !$OMP SECTION
    !Echo progress
    CALL EchoProgress('Computing agricultural water demand...')

    !Compute ag water demand (urban water demands are read in as input)
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        CALL RootZone%NonPondedAgRootZone%ComputeWaterDemand(AppGrid                                         , &
                                                             ETData                                          , &
                                                             TimeStep%DeltaT                                 , &
                                                             RootZone%ElemPrecipData%Precip                  , &
                                                             RootZone%GenericMoistureData%rGenericMoisture   , &
                                                             RootZone%ElemSoilsData                          , &
                                                             RootZone%AgWaterDemandFile%rValues              , &
                                                             RootZone%ReuseFracFile%rValues                  , &
                                                             RootZone%ReturnFracFile%rValues                 , &
                                                             RootZone%IrigPeriodFile%iValues                 , &
                                                             RootZone%SolverData                             , &
                                                             RootZone%Flags%lLakeElems                       , &
                                                             RootZone%Flags%lReadNonPondedAgWaterDemand      , &
                                                             iStatArray(2)                                   , &
                                                             iColCropCoeff=RootZone%iColCropCoeff_NonPondedAg)
    END IF
             
    !$OMP SECTION
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        CALL RootZone%PondedAgRootZone%ComputeWaterDemand(AppGrid                                      , &
                                                          ETData                                       , &
                                                          TimeStep%DeltaT                              , &
                                                          RootZone%ElemPrecipData%Precip               , &
                                                          RootZone%GenericMoistureData%rGenericMoisture, &
                                                          RootZone%ElemSoilsData                       , &
                                                          RootZone%HydCondPonded                       , &
                                                          RootZone%AgWaterDemandFile%rValues           , &
                                                          RootZone%IrigPeriodFile%iValues              , &
                                                          RootZone%Flags%lLakeElems                    , &
                                                          RootZone%Flags%lReadPondedAgWaterDemand      , &
                                                          iStatArray(3)                                , &
                                                          iColCropCoeff=RootZone%iColCropCoeff_PondedAg)
    END IF
    !$OMP END PARALLEL SECTIONS
    
    !Check for errors
    IF (SUM(iStatArray) .NE. 0) THEN
        iStat = -1
        RETURN
    END IF
    
    !Compute demand and supply related fractions
    CALL RootZone%ComputeDemandSupplyRelatedFracs(AppGrid)

  END SUBROUTINE RootZone_v413_ComputeWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE FUTURE AGRICULTURAL WATER DEMAND UNTIL A SPECIFIED DATE
  ! --- Note 1: Effect of GW on demand is ignored to avoid solving gw 
  ! ---         system into the future
  ! --- Note 2: Need to override this method because we need to read crop 
  ! ---         coefficients and later rewind the file. These functionalities 
  ! ---         are not available in the parent class.
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_ComputeFutureWaterDemand(RootZone,AppGrid,TimeStep,Precip,ET,cEndComputeDate,iStat)
    CLASS(RootZone_v413_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(PrecipitationType)       :: Precip
    TYPE(ETType)                  :: ET
    CHARACTER(LEN=*),INTENT(IN)   :: cEndComputeDate
    INTEGER,INTENT(OUT)           :: iStat 
    
    !Local variables
    INTEGER                  :: iNPeriods,iErrorCode,indxTime,iDim,iFileReadCode
    REAL(8)                  :: rDemand_Ag(AppGrid%NElements),rDemand_Urb(AppGrid%NElements)
    TYPE(RootZone_v413_Type) :: RootZone_Work
    TYPE(TimeStepType)       :: TimeStep_Work
    
    !Initialize
    iStat = 0

    !Return if root zone is not simulated
    IF (RootZone%NLands .EQ. 0) RETURN
    
    !Return if future demands are already computed until the date
    IF (ALLOCATED(RootZone%cFutureDemandDates)) THEN
        iDim = SIZE(RootZone%cFutureDemandDates)
        IF (RootZone%cFutureDemandDates(iDim) .TSGE. cEndComputeDate) RETURN
    END IF
    
    !Echo progress
    CALL EchoProgress('Computing future water demand')
    
    !Initialize the working RootZone and TimeStep objects
    RootZone_Work = RootZone
    TimeStep_Work = TimeStep
    
    !Calculate the number of time steps until cEndComputeDate
    iNPeriods = NPeriods(TimeStep%DELTAT_InMinutes,TimeStep%CurrentDateAndTime,cEndComputeDate) + 1
    
    !Allocate memory
    DEALLOCATE (RootZone%cFutureDemandDates , RootZone%rFutureAgElemDemand , RootZone%rFutureUrbElemDemand , STAT=iErrorCode)
    ALLOCATE (RootZone%cFutureDemandDates(iNPeriods) , RootZone%rFutureAgElemDemand(AppGrid%NElements,iNPeriods) , RootZone%rFutureUrbElemDemand(AppGrid%NElements,iNPeriods))

    !Loop through timesteps to simulate future demand
    DO indxTime=1,iNPeriods
        !Demand computation date
        RootZone%cFutureDemandDates(indxTime) = TimeStep_Work%CurrentDateAndTime

        !Read time series data
        CALL Precip%ReadTSData(TimeStep_Work,.TRUE.,iStat)                          ;  IF (iStat .NE. 0) RETURN
        CALL ET%ReadTSData(TimeStep_Work,.TRUE.,iStat)                              ;  IF (iStat .NE. 0) RETURN
        CALL RootZone_Work%ReadTSData(AppGrid,TimeStep_Work,Precip,ET,iStat=iStat)  ;  IF (iStat .NE. 0) RETURN

        !Compute water demand
        CALL RootZone_Work%ComputeWaterDemand(AppGrid,TimeStep_Work,ET,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Retrieve water demands
        CALL RootZone_Work%GetWaterDemandAll(f_iLandUse_Ag,rDemand_Ag)    ;  RootZone%rFutureAgElemDemand(:,indxTime)  = rDemand_Ag
        CALL RootZone_Work%GetWaterDemandAll(f_iLandUse_Urb,rDemand_urb)  ;  RootZone%rFutureUrbElemDemand(:,indxTime) = rDemand_Urb

        !Set the water supply to be equal to water demand and from diversions
        CALL RootZone_Work%ZeroSupply()
        CALL RootZone_Work%SetSupply(rDemand_Ag,f_iSupply_Diversion,f_iLandUse_Ag)
        CALL RootZone_Work%SetSupply(rDemand_Urb,f_iSupply_Diversion,f_iLandUse_Urb)
        
        !Simulate flows
        CALL RootZone_Work%Simulate(AppGrid,TimeStep_Work,ET,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Advance state
        CALL RootZone_Work%AdvanceState()
        
        !Advance time
        TimeStep_Work%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Work%CurrentDateAndTime,TimeStep_Work%DELTAT_InMinutes)
        TimeStep_Work%CurrentTimeStep    = TimeStep_Work%CurrentTimeStep + 1
        
    END DO
    
    !Rewind timeseries input files
    CALL Precip%File%RewindFile_To_BeginningOfTSData(iStat)                                                ;  IF (iStat .NE. 0) RETURN
    CALL Precip%ReadTSData(TimeStep,.TRUE.,iStat)                                                          ;  IF (iStat .NE. 0) RETURN
    CALL ET%File%RewindFile_To_BeginningOfTSData(iStat)                                                    ;  IF (iStat .NE. 0) RETURN
    CALL ET%ReadTSData(TimeStep,.TRUE.,iStat)                                                              ;  IF (iStat .NE. 0) RETURN
    CALL ET%CropCoeffFile%File%RewindFile_To_BeginningOfTSData(iStat)                                      ;  IF (iStat .NE. 0) RETURN
    CALL ET%CropCoeffFile%ReadTSData(TimeStep,'Crop/habitat coefficients data',iFileReadCode,iStat)        ;  IF (iStat .NE. 0) RETURN
    CALL RootZone%RewindTSInputFilesToTimeStamp(AppGrid%AppElement%ID,AppGrid%AppElement%Area,TimeStep,iStat)             

  END SUBROUTINE RootZone_v413_ComputeFutureWaterDemand
  
  
  
    
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
  ! --- SIMULATE SOIL MOISTURE IN ROOT ZONE
  ! --- Note1: In this version, we assume surface flow from an element cannot 
  ! ---        be used as water supply in another element, so we are not
  ! ---        calculating cross-element surface flows and not iterating
  ! --- Note2: Urban return flow is only the outdoors return flow for easy 
  ! ---        budgeting later; rAW_UrbIndoors variable is the urban indoors 
  ! ---        return flow since 100% of urban indoors water is assumed to 
  ! ---        return
  ! --- Note3: Any surface flow from any land use going to gw is not dealt 
  ! ---        with during simulation; it is dealt with later during 
  ! ---        post-processing and data retrieval
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v413_Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v413_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'RootZone_v413_Simulate'
    INTEGER                      :: indxElem,iNElements,iElemID,iNoElemsToGW(0),iStatArray(4)
    REAL(8)                      :: rDeltaT,rArea,rZeroFlow(AppGrid%NElements),                             &    
                                    rIrigSupply_Ag(AppGrid%NElements),rIrigSupply_Urb(AppGrid%NElements),   &
                                    rElemCropSupply(RootZone%NonPondedAgRootZone%NCrops,AppGrid%NElements), &
                                    rElemPondSupply(RootZone%PondedAgRootZone%iNCrops,AppGrid%NElements)
                                    
    !Initialize
    iStat      = 0
    iStatArray = 0
    rZeroFlow  = 0.0

    ASSOCIATE (pElemSupply       => RootZone%ElemSupply                           , &
               pSoilsData        => RootZone%ElemSoilsData                        , &
               pElemPrecip       => RootZone%ElemPrecipData%Precip                , &
               prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture , &
               pReuseFracs       => RootZone%ReuseFracFile%rValues                , &
               pReturnFracs      => RootZone%ReturnFracFile%rValues               , &
               pSolverData       => RootZone%SolverData                           )
               
        !Initialize
        rDeltaT         = TimeStep%DeltaT
        iNElements      = AppGrid%NElements
        rIrigSupply_Ag  = pElemSupply%Diversion_Ag  + pElemSupply%Pumping_Ag
        rIrigSupply_Urb = pElemSupply%Diversion_Urb + pElemSupply%Pumping_Urb
        
        !Check water supply vs. irrigable lands
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem,iElemID,rArea) SCHEDULE(DYNAMIC,500)
        DO indxElem=1,iNElements
            !If this is a lake element, report that to the user
            IF (RootZone%Flags%lLakeElems(indxElem)) THEN
                IF (rIrigSupply_Ag(indxElem)+rIrigSupply_Urb(indxElem) .GT. 0.0) THEN 
                    iElemID         = AppGrid%AppElement(indxElem)%ID
                    MessageArray(1) = 'Element '//TRIM(IntToText(iElemID))//' is a lake element.'
                    MessageArray(2) = 'Water supply for lake elements must be zero!'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    CYCLE
                END IF
            END IF
            !Check ag area vs. ag water supply
            IF (rIrigSupply_Ag(indxElem) .GT. 0.0) THEN
                rArea = 0.0
                IF (RootZone%Flags%lNonPondedAg_Defined) rArea = rArea + SUM(RootZone%NonPondedAgRootZone%Crops%Area(:,indxElem))
                IF (RootZone%Flags%lPondedAg_Defined)    rArea = rArea + SUM(RootZone%PondedAgRootZone%Crops%Area(:,indxElem))
                IF (rArea .EQ. 0.0) THEN
                    iElemID         = AppGrid%AppElement(indxElem)%ID
                    MessageArray(1) = 'Agricultural applied water at element '//TRIM(IntToText(iElemID))//' cannot be non-zero'
                    MessageArray(2) = 'when agricultural area is zero!'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    CYCLE
                END IF
            END IF
            !Check urban area vs. urban water supply
            IF (rIrigSupply_Urb(indxElem) .GT. 0.0) THEN
                IF (RootZone%Flags%lUrban_Defined) THEN
                    rArea = RootZone%UrbanRootZone%UrbData%Area(indxElem,1)
                ELSE
                    rArea = 0.0
                END IF
                IF (rArea .EQ. 0.0) THEN
                    iElemID         = AppGrid%AppElement(indxElem)%ID
                    MessageArray(1) = 'Urban applied water at element '//TRIM(IntToText(iElemID))//' cannot be non-zero'
                    MessageArray(2) = 'when urban area is zero!'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    CYCLE
                END IF
            END IF
        END DO
        !$OMP END PARALLEL DO
        
        !Return if there was an error
        IF (iStat .EQ. -1) RETURN
          
        !$OMP PARALLEL SECTIONS DEFAULT(PRIVATE) SHARED(RootZone,iNElements,AppGrid,rIrigSupply_Ag,ETData,rDeltaT,     &
        !$OMP                                           rElemCropSupply,rElemPondSupply,rIrigSupply_Urb,iNoElemsToGW,  &
        !$OMP                                           rZeroFlow,iStatArray)
        !$OMP SECTION
        !Simulate non-ponded ag lands
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem)
            DO indxElem=1,iNElements
                rElemCropSupply(:,indxElem) = rIrigSupply_Ag(indxElem) * RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac_Ag(:,indxElem)
            END DO
            !$OMP END PARALLEL DO
            CALL RootZone%NonPondedAgRootZone%Simulate(AppGrid                                         , &
                                                       ETData                                          , &
                                                       rDeltaT                                         , &
                                                       pElemPrecip                                     , &
                                                       prGenericMoisture                               , &
                                                       pSoilsData                                      , &
                                                       rElemCropSupply                                 , &
                                                       pReuseFracs                                     , &
                                                       pReturnFracs                                    , &
                                                       iNoElemsToGW                                    , &
                                                       pSolverData                                     , &
                                                       RootZone%Flags%lLakeElems                       , &
                                                       iStatArray(1)                                   , &
                                                       iColCropCoeff=RootZone%iColCropCoeff_NonPondedAg)
        END IF
        
        !$OMP SECTION
        !Simulate ponded ag lands
        IF (RootZone%Flags%lPondedAg_Defined) THEN  
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) SCHEDULE(STATIC,160)
            DO indxElem=1,iNElements
                rElemPondSupply(:,indxElem) = rIrigSupply_Ag(indxElem) * RootZone%PondedAgRootZone%Crops%ElemDemandFrac_Ag(:,indxElem)
            END DO
            !$OMP END PARALLEL DO
            CALL RootZone%PondedAgRootZone%Simulate(AppGrid                                      , &
                                                    ETData                                       , &
                                                    rDeltaT                                      , &
                                                    pElemPrecip                                  , &
                                                    prGenericMoisture                            , &
                                                    pSoilsData                                   , &
                                                    RootZone%HydCondPonded                       , &
                                                    rElemPondSupply                              , &
                                                    iNoElemsToGW                                 , &
                                                    pSolverData                                  , &
                                                    RootZone%Flags%lLakeElems                    , &
                                                    iStatArray(2)                                , &
                                                    iColCropCoeff=RootZone%iColCropCoeff_PondedAg)
        END IF
        
        !$OMP SECTION
        !Simulate urban lands
        IF (RootZone%Flags%lUrban_Defined) THEN 
            CALL RootZone%UrbanRootZone%Simulate(AppGrid                                       , &
                                                 ETData                                        , &
                                                 rDeltaT                                       , &
                                                 pElemPrecip                                   , &
                                                 prGenericMoisture                             , &
                                                 pSoilsData                                    , &
                                                 rIrigSupply_Urb                               , &
                                                 pReuseFracs                                   , &
                                                 pReturnFracs                                  , &
                                                 iNoElemsToGW                                  , &
                                                 pSolverData                                   , &
                                                 RootZone%Flags%lLakeElems                     , &
                                                 iStatArray(3)                                 , &
                                                 iColCropCoeff_Urban=RootZone%iColCropCoeff_Urb)
        END IF
        
        !$OMP SECTION
        !Simulate native and riparian veg lands
        IF (RootZone%Flags%lNVRV_Defined) THEN
            CALL RootZone%NVRVRootZone%Simulate(AppGrid                                       , &
                                                ETData                                        , &
                                                rDeltaT                                       , &
                                                pElemPrecip                                   , &
                                                prGenericMoisture                             , &
                                                pSoilsData                                    , &
                                                rZeroFlow                                     , &
                                                iNoElemsToGW                                  , &
                                                pSolverData                                   , &
                                                RootZone%Flags%lLakeElems                     , &
                                                iStatArray(4)                                 , &
                                                iColCropCoeff_NVRV=RootZone%iColCropCoeff_NVRV)
        END IF 
        !$OMP END PARALLEL SECTIONS
    END ASSOCIATE
               
    !Check for error
    IF (SUM(iStatArray) .NE. 0) iStat = -1
    
  END SUBROUTINE RootZone_v413_Simulate

  
  ! -------------------------------------------------------------
  ! --- CHECK POINTERS TO TIME SERIES DATA COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE CheckTSDataPointers(RootZone,iElemIDs,Precip,ET,iStat)
    TYPE(RootZone_v413_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: iElemIDs(:)
    TYPE(PrecipitationType),INTENT(IN)  :: Precip
    TYPE(ETType),INTENT(IN)             :: ET
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'CheckTSDataPointers'
    INTEGER                      :: indxElem,ID,iMaxKcCol
    
    !First call the inhereted method
    CALL RootZone%RootZone_v412_Type%CheckTSDataPointers(iElemIDs,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check crop coefficient pointers for other land use types
    iMaxKcCol = ET%GetNKcColumns()
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        DO indxElem=1,SIZE(iElemIDs)
            IF (iMaxKcCol .LT. MAXVAL(RootZone%iColCropCoeff_NonPondedAg(:,indxElem))) THEN                
                ID              = iElemIDs(indxElem)
                MessageArray(1) = 'Crop coefficient data column(s) referenced by non-ponded crop data file for'
                MessageArray(2) = 'element '//TRIM(IntToText(ID))//' is greater than the columns in the'
                MessageArray(3) = 'Crop/Habitat Coeffcient Data File!'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        DO indxElem=1,SIZE(iElemIDs)
            IF (iMaxKcCol .LT. MAXVAL(RootZone%iColCropCoeff_PondedAg(:,indxElem))) THEN                
                ID              = iElemIDs(indxElem)
                MessageArray(1) = 'Crop coefficient data column(s) referenced by ponded crop data file for'
                MessageArray(2) = 'element '//TRIM(IntToText(ID))//' is greater than the columns in the'
                MessageArray(3) = 'Crop/Habitat Coeffcient Data File!'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    !Urban
    IF (RootZone%Flags%lUrban_Defined) THEN
        DO indxElem=1,SIZE(iElemIDs)
            IF (iMaxKcCol .LT. RootZone%iColCropCoeff_Urb(indxElem)) THEN                
                ID              = iElemIDs(indxElem)
                MessageArray(1) = 'Crop coefficient data column(s) referenced by urban data file for'
                MessageArray(2) = 'element '//TRIM(IntToText(ID))//' is greater than the columns in the'
                MessageArray(3) = 'Crop/Habitat Coeffcient Data File!'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    !Native and riparian
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indxElem=1,SIZE(iElemIDs)
            IF (iMaxKcCol .LT. MAXVAL(RootZone%iColCropCoeff_NVRV(:,indxElem))) THEN                
                ID              = iElemIDs(indxElem)
                MessageArray(1) = 'Crop coefficient data column(s) referenced by native&riparain vegetation data file for'
                MessageArray(2) = 'element '//TRIM(IntToText(ID))//' is greater than the columns in the'
                MessageArray(3) = 'Crop/Habitat Coeffcient Data File!'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF

  END SUBROUTINE CheckTSDataPointers    
  
END MODULE