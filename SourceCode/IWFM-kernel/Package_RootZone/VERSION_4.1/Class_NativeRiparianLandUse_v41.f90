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
MODULE Class_NativeRiparianLandUse_v41
  !$ USE OMP_LIB
  USE MessageLogger           , ONLY: SetLastMessage                       , &
                                      EchoProgress                         , &
                                      MessageArray                         , &
                                      f_iFatal                               
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter              , &
                                      IntToText                            , &
                                      UpperCase                            , &
                                      CleanSpecialCharacters               , &
                                      EstablishAbsolutePathFilename        , &
                                      NormalizeArray                       , &
                                      ConvertID_To_Index                   , &
                                      AllocArray                           , &
                                      LocateInList, &
                                   f_cInlineCommentChar
  USE TimeSeriesUtilities     , ONLY: TimeStepType                         
  USE IOInterface             , ONLY: GenericFileType  
  USE Package_Misc            , ONLY: SolverDataType                       , &
                                      f_iFlowDest_GWElement                
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource             
  USE Class_GenericLandUse_v41, ONLY: GenericLandUse_v41_Type              , &
                                      ComputeETFromGW_Max
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType                  
  USE Package_Discretization  , ONLY: AppGridType                          
  USE Package_PrecipitationET , ONLY: ETType 
  USE Util_Package_RootZone   , ONLY: ReadRealData                         , &
                                      ReadLandUseAreasForTimePeriod    
  USE Util_RootZone_v41       , ONLY: RootZoneSoil_v41_Type                , &
                                      f_iLenCropCode
  USE Class_RVETFromStrm      , ONLY: RVETFromStrmType 
  USE Package_UnsatZone       , ONLY: NonPondedLUMoistureRouter
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
  PUBLIC :: NativeRiparianLandUse_v41_Type                     
  
  
  ! -------------------------------------------------------------
  ! --- STATIC PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER          :: f_iNNative                  = 1 , &
                                f_iNRiparian                = 1 
  CHARACTER(LEN=f_iLenCropCode),PARAMETER :: f_cVegCodes(2)  = ['NV','RV'] 
  
  
  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUse_v41_Type) :: NativeRiparian_v41_Type
  END TYPE NativeRiparian_v41_Type


  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE NativeRiparianLandUse_v41_Type
      INTEGER                                   :: iNNative                 = 0        !Number of native veg categories
      INTEGER                                   :: iNRiparian               = 0        !Number of riparian veg categories
      INTEGER                                   :: iNNVRV                   = 0        !Total number of native nad riparian vegetation
      CHARACTER(LEN=f_iLenCropCode),ALLOCATABLE :: cVegCodes(:)
      TYPE(NativeRiparian_v41_Type)             :: NVRV
      TYPE(RVETFromStrmType)                    :: RVETFromStrm
      LOGICAL                                   :: lRVETFromStrm_Simulated  = .FALSE.
      REAL(8),ALLOCATABLE                       :: rRootDepth(:)                       !Root depth for each (vegetation) type; native veg first
      REAL(8),ALLOCATABLE                       :: rRegionETPot(:,:)                    !Regional potential ET for each (vegetation,subregion)
      TYPE(LandUseDataFileType)                 :: LandUseDataFile                      !Land use data file
  CONTAINS
      PROCEDURE,PASS   :: New                             
      PROCEDURE,PASS   :: Kill                            
      PROCEDURE,PASS   :: GetRequiredET_AtStrmNodes       
      PROCEDURE,PASS   :: GetRegionalRVETFromStrm         
      PROCEDURE,PASS   :: GetActualET_AtStrmNodes
      PROCEDURE,NOPASS :: GetAreasForTimePeriod
      PROCEDURE,PASS   :: SetAreas                        
      PROCEDURE,PASS   :: SetActualRiparianET_AtStrmNodes 
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: ReadTSData                      
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: IsRVETFromStrmSimulated         
      PROCEDURE,PASS   :: AdvanceAreas                    
      PROCEDURE,PASS   :: AdvanceState                    
      PROCEDURE,PASS   :: SoilMContent_To_Depth           
      PROCEDURE,PASS   :: Simulate                        
      PROCEDURE,PASS   :: ComputeWaterDemand              
      PROCEDURE,PASS   :: ComputeETFromGW_Max => NVRV_ComputeETFromGW_Max
      PROCEDURE,PASS   :: RewindTSInputFilesToTimeStamp 
  END TYPE NativeRiparianLandUse_v41_Type
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 33
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_NativeRiparianLandUse_v41::'

  


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
  ! --- NEW NATIVE AND RIPARIAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(NVRVLand,lReadNVeg,cFileName,cWorkingDirectory,FactCN,NElements,NSubregions,iElemIDs,TrackTime,iStat,iStrmNodeIDs,iColHabitatCoeff_NVRV)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    LOGICAL,INTENT(IN)                    :: lReadNVeg
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)                    :: FACTCN
    INTEGER,INTENT(IN)                    :: NElements,NSubregions,iElemIDs(NElements)
    LOGICAL,INTENT(IN)                    :: TrackTime
    INTEGER,INTENT(OUT)                   :: iStat
    INTEGER,OPTIONAL,INTENT(IN)           :: iStrmNodeIDs(:)
    INTEGER,OPTIONAL,ALLOCATABLE          :: iColHabitatCoeff_NVRV(:,:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)               :: ThisProcedure = ModName // 'New'
    CHARACTER                                 :: cALine*1000
    INTEGER                                   :: iErrorCode,iStrmNodes(NElements),iElem,ID,indxElem,iStrmNodeID,indxVeg,     &
                                                 iNNVRV,iNNative,iNRiparian,indxCN_NV_B,indxCN_NV_E,indxCN_RV_B,indxCN_RV_E,   &
                                                 indxET_NV_B,indxET_NV_E,indxET_RV_B,indxET_RV_E,indx
    REAL(8)                                   :: rFact
    INTEGER,ALLOCATABLE                       :: iTempArray_NVRV(:,:)
    REAL(8),ALLOCATABLE                       :: rDummyArray(:,:)
    LOGICAL                                   :: lProcessed(NElements)
    TYPE(GenericFileType)                     :: NVRVFile
    CHARACTER(LEN=f_iLenCropCode),ALLOCATABLE :: cVegCodes(:)
    CHARACTER(:),ALLOCATABLE                  :: cAbsPathFileName
    
    !Initialzie
    iStat = 0
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Open file
    CALL NVRVFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Are we reading the number of vegetation categories?
    IF (lReadNVeg) THEN
        CALL NVRVFile%ReadData(iNNative,iStat)    ;  IF (iStat .NE. 0) RETURN
        CALL NVRVFile%ReadData(iNRiparian,iStat)  ;  IF (iStat .NE. 0) RETURN
        iNNVRV = iNNAtive + iNRiparian
        !Also read the vegetation codes
        ALLOCATE (cVegCodes(iNNVRV))
        DO indx=1,iNNVRV
            CALL NVRVFile%ReadData(cALine,iStat)  ;  IF (iStat .NE. 0) RETURN
            cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
            CALL CleanSpecialCharacters(cALine)
            cVegCodes(indx) = UpperCase(ADJUSTL(cALine))
            cVegCodes(indx) = ADJUSTR(cVegCodes(indx))
        END DO
    ELSE
        iNNative   = 1
        iNRiparian = 1
        iNNVRV   = iNNative + iNRiparian
    END IF
        
    !Number of native and riparian veg categories
    NVRVLand%iNNative   = iNNative
    NVRVLand%iNRiparian = iNRiparian
    NVRVLand%iNNVRV     = iNNVRV
    
    !Allocate memory
    CALL NVRVLand%NVRV%New(iNNVRV,NElements,iStat)
    ALLOCATE (NVRVLand%cVegCodes(iNNVRV)                      , &
              NVRVLand%rRootDepth(iNNVRV)                     , &
              NVRVLand%rRegionETPot(iNNVRV,NSubregions)       , &
              STAT=iErrorCode                                 )
    IF (iErrorCode+iStat .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for native/riparian vegetation data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize arrays
    NVRVLand%rRootDepth   = 0.0
    NVRVLand%rRegionETPot = 0.0
    IF (lReadNVeg) THEN
        NVRVLand%cVegCodes = cVegCodes
    ELSE
        NVRVLand%cVegCodes = f_cVegCodes
    END IF
    
    !Land use data file
    CALL NVRVFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAbsPathFileName)
    CALL NVRVLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Native and riparian veg. area file',NElements,iNNVRV,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Rooting depths
    CALL NVRVFile%ReadData(rFact,iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indxVeg=1,iNNVRV
        CALL NVRVFile%ReadData(NVRVLand%rRootDepth(indxVeg),iStat)    
        IF (iStat .EQ. -1) RETURN    
        NVRVLand%rRootDepth(indxVeg) = NVRVLand%rRootDepth(indxVeg) * rFact
    END DO
 
    !Read CN, ETc/ETo column pointers, habitat coeff pointer (if asked) and cobbeting stream nodes
    IF (PRESENT(iColHabitatCoeff_NVRV)) THEN
        ALLOCATE (iColHabitatCoeff_NVRV(iNNVRV,NElements) , iTempArray_NVRV(NElements,iNNVRV))
        CALL ReadRealData(NVRVFile,'Curve numbers and evapotranspiration column pointers for native and riparian vegetation','elements',NElements,3*iNNVRV+2,iElemIDs,rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        !Shift columns
        iTempArray_NVRV           = INT(rDummyArray(:,2*iNNVRV+2:3*iNNVRV+1))
        rDummyArray(:,2*iNNVRV+2) = rDummyArray(:,3*iNNVRV+2)
    ELSE
        CALL ReadRealData(NVRVFile,'Curve numbers and evapotranspiration column pointers for native and riparian vegetation','elements',NElements,2*iNNVRV+2,iElemIDs,rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    lProcessed  = .FALSE.
    indxCN_NV_B = 2
    indxCN_NV_E = indxCN_NV_B + iNNative - 1
    indxCN_RV_B = indxCN_NV_E + 1
    indxCN_RV_E = indxCN_RV_B + iNRiparian - 1
    indxET_NV_B = indxCN_RV_E + 1
    indxET_NV_E = indxET_NV_B + iNNative - 1
    indxET_RV_B = indxET_NV_E + 1
    indxET_RV_E = indxET_RV_B + iNRiparian - 1
    DO indxElem=1,NElements
        iElem = INT(rDummyArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Curve numbers and evapotranspiration column pointers for native and riparian vegetation at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                        = .TRUE.
        NVRVLand%NVRV%SMax(1:iNNative,iElem)     = (1000.0/rDummyArray(indxElem,indxCN_NV_B:indxCN_NV_E)-10.0) * FACTCN
        NVRVLand%NVRV%SMax(iNNative+1:,iElem)    = (1000.0/rDummyArray(indxElem,indxCN_RV_B:indxCN_RV_E)-10.0) * FACTCN
        NVRVLand%NVRV%iColETc(1:iNNative,iElem)  = INT(rDummyArray(indxElem,indxET_NV_B:indxET_NV_E))
        NVRVLand%NVRV%iColETc(iNNative+1:,iElem) = INT(rDummyArray(indxElem,indxET_RV_B:indxET_RV_E))
        iStrmNodeID                              = INT(rDummyArray(indxElem,SIZE(rDummyArray,DIM=2)))
        IF (iStrmNodeID .EQ. 0) THEN
            iStrmNodes(iElem) = 0
        ELSE
            IF (PRESENT(iStrmNodeIDs)) THEN 
                CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iStrmNodes(iElem))
                IF (iStrmNodes(iElem) .EQ. 0) THEN
                    ID = iElemIDs(iElem)
                    CALL SetLastMessage('Stream node '//TRIM(IntToText(iStrmNodeID))//' listed to satisfy unmet riparian vegetation ET at element '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            ELSE
                iStrmNodes(iElem) = 0
            END IF
        END IF
        IF (PRESENT(iColHabitatCoeff_NVRV)) THEN
            iColHabitatCoeff_NVRV(:,iElem) = iTempArray_NVRV(indxElem,:)
        END IF
    END DO
    
    !Set the element-stream connections for riparian ET
    CALL NVRVLand%RVETFromStrm%New(iStrmNodes,NVRVLand%iNRiparian,iStat)
    IF (iStat .EQ. -1) RETURN
    NVRVLand%lRVETFromStrm_Simulated = NVRVLand%RVETFromStrm%IsSimulated()
    
    !Initial conditions  
    CALL ReadRealData(NVRVFile,'initial conditions for native and riparian vegetation','elements',NElements,iNNVRV+1,iElemIDs,rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(rDummyArray(:,2:)) .LT. 0.0   .OR.  &
        MAXVAL(rDummyArray(:,2:)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for native and riparian vegetation areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(rDummyArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Initial conditions for native and riparian vegetation at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                                   = .TRUE.
        NVRVLand%NVRV%SoilM_Precip(1:iNNative,iElem)        = rDummyArray(indxElem,2:iNNative+1) 
        NVRVLand%NVRV%SoilM_Precip(iNNative+1:iNNVRV,iElem) = rDummyArray(indxElem,iNNative+2:iNNVRV+1) 
        NVRVLand%NVRV%SoilM_Precip_P(:,iElem)               = NVRVLand%NVRV%SoilM_Precip(:,iElem)
        NVRVLand%NVRV%SoilM_Precip_P_BeforeUpdate(:,iElem)  = NVRVLand%NVRV%SoilM_Precip(:,iElem)
        NVRVLand%NVRV%SoilM_AW(:,iElem)                     = 0.0
        NVRVLand%NVRV%SoilM_AW_P(:,iElem)                   = 0.0
        NVRVLand%NVRV%SoilM_AW_P_BeforeUpdate(:,iElem)      = 0.0
    END DO
    
    !Close file
    CALL NVRVFile%Kill()
    
    !Free memory
    DEALLOCATE (rDummyArray , STAT=iErrorCode)

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
  ! --- KILL NATIVE AND RIPARIAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(NVRVLand)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand

    !Local variables
    INTEGER                              :: iErrorCode
    TYPE(NativeRiparianLandUse_v41_Type) :: Dummy
    
    !Deallocate arrays
    CALL NVRVLand%NVRV%Kill()
    DEALLOCATE (NVRVLand%cVegCodes    , &
                NVRVLand%rRootDepth   , &
                NVRVLand%rRegionETPot , &
                STAT = iErrorCode     )
    
    !Close files
    CALL NVRVLand%LandUseDataFile%Kill()
    
    !Kill the element-to-stream connection
    CALL NVRVLand%RVETFromStrm%Kill()
    
    !Assign default values to components
    SELECT TYPE (NVRVLand)
        TYPE IS (NativeRiparianLandUse_v41_Type)
            NVRVLand = Dummy
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
  ! --- GET ACTUAL RIPARIAN ET AT STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetActualET_AtStrmNodes(NVRVLand,QRVET)
    CLASS(NativeRiparianLandUse_v41_Type),INTENT(IN) :: NVRVLand
    REAL(8),INTENT(OUT)                              :: QRVET(:)

    IF (NVRVLand%lRVETFromStrm_Simulated) THEN
        CALL NVRVLand%RVETFromStrm%GetActualET_AtStrmNodes(QRVET)
    ELSE
        QRVET = 0.0
    END IF

  END SUBROUTINE GetActualET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET REGIONAL ACTUAL RIPARIAN ET
  ! -------------------------------------------------------------
  SUBROUTINE GetRegionalRVETFromStrm(NVRVLand,AppGrid,RRVETFromStrm)
    CLASS(NativeRiparianLandUse_v41_Type),INTENT(IN) :: NVRVLand
    TYPE(AppGridType),INTENT(IN)                     :: AppGrid
    REAL(8),INTENT(OUT)                              :: RRVETFromStrm(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegions
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    IF (NVRVLand%lRVETFromStrm_Simulated) THEN
        CALL NVRVLand%RVETFromStrm%GetActualET_AtRegions(AppGrid,RRVETFromStrm(1:NRegions))
        RRVETFromStrm(NRegions+1) = SUM(RRVETFromStrm(1:NRegions))
    ELSE
        RRVETFromStrm = 0.0
    END IF
    
  END SUBROUTINE GetRegionalRVETFromStrm
  
  
  ! -------------------------------------------------------------
  ! --- GET REQUIRED VOLUMETRIC RIPARIAN ET OUTFLOW AT EACH STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetRequiredET_AtStrmNodes(NVRVLand,RiparianET)
    CLASS(NativeRiparianLandUse_v41_Type),INTENT(IN) :: NVRVLand
    REAL(8),INTENT(OUT)                              :: RiparianET(:)
    
    CALL NVRVLand%RVETFromStrm%GetRequiredET_AtStrmNodes(RiparianET)

  END SUBROUTINE GetRequiredET_AtStrmNodes
  

  ! -------------------------------------------------------------
  ! --- GET NATIVE OR RIPARIAN VEG AREAS FOR AT ALL ELEMENTS FOR A TIME PERIOD
  ! --- Note: This method is not meant to be called during a Simulation
  ! -------------------------------------------------------------
  SUBROUTINE GetAreasForTimePeriod(cMainFileName,cWorkingDirectory,cBeginDate,cEndDate,TimeStep,AppGrid,iNVorRV,rAreas,iStat)
    CHARACTER(LEN=*),INTENT(IN)   :: cMainFileName,cWorkingDirectory,cBeginDate,cEndDate
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: iNVorRV  !1 = NV; 2 = RV
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
    cALine = StripTextUntilCharacter(cALine,f_cInlineCommentChar) 
    CALL CleanSpecialCharacters(cALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cALine)),cWorkingDirectory,cAreaFileName)
    
    !Retrieve areas
    CALL ReadLandUseAreasForTimePeriod(cAreaFileName,cWorkingDirectory,cBeginDate,cEndDate,TimeStep,AppGrid,2,iNVorRV,rAreas,iStat)

10  CALL MainFile%Kill()
    
  END SUBROUTINE GetAreasForTimePeriod
  
  
  
  
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
  ! --- SET ACTUAL RIPARIAN ET
  ! -------------------------------------------------------------
  SUBROUTINE SetActualRiparianET_AtStrmNodes(NVRVLand,RiparianETFrac)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    REAL(8),INTENT(IN)                    :: RiparianETFrac(:)
    
    CALL NVRVLand%RVETFromStrm%SetActualET_AtStrmNodes(RiparianETFrac)
  
  END SUBROUTINE SetActualRiparianET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- SET THE LAND USE AREAS
  ! -------------------------------------------------------------
  SUBROUTINE SetAreas(NVRVLand,Area)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    REAL(8),INTENT(IN)                    :: Area(:,:)
   
    NVRVLand%NVRV%Area = Area
    
  END SUBROUTINE SetAreas
  

  
  
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
  ! --- IS RIPARIAN ET FROM STREAMS SIMULATED?
  ! -------------------------------------------------------------
  FUNCTION IsRVETFromStrmSimulated(NVRVLand) RESULT(lSimulated)
    CLASS(NativeRiparianLandUse_v41_Type),INTENT(IN) :: NVRVLand
    LOGICAL                                          :: lSimulated
    
    lSimulated = NVRVLand%lRVETFromStrm_Simulated
    
  END FUNCTION IsRVETFromStrmSimulated
    
  
  
  
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
  SUBROUTINE ReadRestartData(NVRVLand,InFile,iStat)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    TYPE(GenericFileType)                 :: InFile
    INTEGER,INTENT(OUT)                   :: iStat
    
    CALL InFile%ReadData(NVRVLand%NVRV%Runoff,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%Area_P,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%Area,iStat)              ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%SoilM_Precip_P,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%SoilM_Precip,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%SoilM_AW_P,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%SoilM_AW,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%SoilM_Oth_P,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NVRV%SoilM_Oth,iStat)         ;  IF (iStat .EQ. -1) RETURN
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR NATIVE AND RIPARIAN VEG
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(NVRVLand,TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    INTEGER,INTENT(IN)                    :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)                    :: rElemAreas(AppGrid%NElements)
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    INTEGER :: indxElem
    
    !Initialize
    iStat = 0

    !Echo progress
    CALL EchoProgress('Reading time series data for native and riparian vegitation lands')
    
    !Land use areas
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,rElemAreas,iElemIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (NVRVLand%LandUseDataFile%lUpdated) THEN
        DO indxElem=1,AppGrid%NElements
            NVRVLand%NVRV%Area(:,indxElem) = NVRVLand%LandUseDataFile%rValues(indxElem,2:)
        END DO
    END IF
    
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
  SUBROUTINE PrintRestartData(NVRVLand,OutFile)
    CLASS(NativeRiparianLandUse_v41_Type),INTENT(IN) :: NVRVLand
    TYPE(GenericFileType)                            :: OutFile
    
    CALL OutFile%WriteData(NVRVLand%NVRV%Runoff)
    CALL OutFile%WriteData(NVRVLand%NVRV%Area_P)
    CALL OutFile%WriteData(NVRVLand%NVRV%Area)
    CALL OutFile%WriteData(NVRVLand%NVRV%SoilM_Precip_P)
    CALL OutFile%WriteData(NVRVLand%NVRV%SoilM_Precip)
    CALL OutFile%WriteData(NVRVLand%NVRV%SoilM_AW_P)
    CALL OutFile%WriteData(NVRVLand%NVRV%SoilM_AW)
    CALL OutFile%WriteData(NVRVLand%NVRV%SoilM_Oth_P)
    CALL OutFile%WriteData(NVRVLand%NVRV%SoilM_Oth)
    
  END SUBROUTINE PrintRestartData
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SIMULATION OF ROOT ZONE RUNOFF PROCESSES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE RIPARIAN ET DEMAND FROM STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(NVRVLand,iNElements,iElemIDs,ETData,rDeltaT,rPrecip,rGenericMoisture,SoilsData,SolverData,lLakeElem,iStat,iColCropCoeff_NVRV)
    CLASS(NativeRiparianLandUse_v41_Type)  :: NVRVLand
    INTEGER,INTENT(IN)                     :: iNElements,iElemIDs(iNElements)
    TYPE(ETType),INTENT(IN)                :: ETData
    TYPE(RootZoneSoil_v41_Type),INTENT(IN) :: SoilsData(:)
    REAL(8),INTENT(IN)                     :: rDeltaT,rPrecip(:),rGenericMoisture(:,:)
    TYPE(SolverDataType),INTENT(IN)        :: SolverData
    LOGICAL,INTENT(IN)                     :: lLakeElem(:)
    INTEGER,INTENT(OUT)                    :: iStat
    INTEGER,OPTIONAL,INTENT(IN)            :: iColCropCoeff_NVRV(:,:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'ComputeWaterDemand'
    INTEGER                                :: indxElem,iKunsatMethod,indxVeg,iNNative,iNNVRV,indxRiparian
    REAL(8)                                :: rETFromStrm_Required(NVRVLand%iNRiparian,iNElements),rWiltingPoint,rFieldCapacity,rTotalPorosity,rHydCond,rLambda,  &
                                              rETc(NVRVLand%iNNVRV),rRootDepth,rPrecipD,rSoilM_P,rGM,rETa,rAchievedConv,rSoilM,rRunoff,  &
                                              rExcess,rPrecipInfilt,rPerc,rETc_effect,rWiltingPointVeg,rFieldCapacityVeg,rTotalPorosityVeg
    
    !Initialize
    iStat    = 0
    iNNative = NVRVLand%iNNative
    iNNVRV   = NVRVLand%iNNVRV
    
    !Return if riparian-et-from-streams are not simulated
    IF (.NOT. NVRVLand%lRVETFromStrm_Simulated) RETURN
    
    !Loop over elements
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(iNElements,NVRVLand,rETFromStrm_Required,lLakeElem,SoilsData,ETData,rPrecip,          &
    !$OMP                                  rGenericMoisture,SolverData,iElemIDs,rDeltaT,iStat,iNNative,iNNVRV,iColCropCoeff_NVRV)  
    !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,200)
    DO indxElem=1,iNElements
        !Soil parameters and ETc
        rWiltingPoint  = SoilsData(indxElem)%WiltingPoint  
        rFieldCapacity = SoilsData(indxElem)%FieldCapacity 
        rTotalPorosity = SoilsData(indxElem)%TotalPorosity 
        rHydCond       = SoilsData(indxElem)%HydCond
        rLambda        = SoilsData(indxElem)%Lambda
        iKunsatMethod  = SoilsData(indxElem)%KunsatMethod
        IF (PRESENT(iColCropCoeff_NVRV)) THEN
            rETc       = ETData%GetValues(NVRVLand%NVRV%iColETc(:,indxElem),iColCropCoeff_NVRV(:,indxElem)) * rDeltaT 
        ELSE
            rETc       = ETData%GetValues(NVRVLand%NVRV%iColETc(:,indxElem)) * rDeltaT 
        END IF
        rPrecipD       = rPrecip(indxElem) * rDeltaT
        DO indxVeg=iNNative+1,iNNVRV
            indxRiparian = indxVeg - iNNative
            
            !Cycle if riparian area is zero
            IF (NVRVLand%NVRV%Area(indxVeg,indxElem) .EQ. 0.0) THEN
                rETFromStrm_Required(indxRiparian,indxElem) = 0.0
                CYCLE
            END IF
            
            !Cycle if lake element
            IF (lLakeElem(indxElem)) THEN
                rETFromStrm_Required(indxRiparian,indxElem) = 0.0
                CYCLE
            END IF
            
            !Initialize variables                
            rRootDepth        = NVRVLand%rRootDepth(indxVeg)
            rWiltingPointVeg  = rWiltingPoint  * rRootDepth
            rFieldCapacityVeg = rFieldCapacity * rRootDepth
            rTotalPorosityVeg = rTotalPorosity * rRootDepth
            rETc_effect       = rETc(indxVeg) - MIN(rETc(indxVeg) , NVRVLand%NVRV%ETFromGW_Max(indxVeg,indxElem))
            rSoilM_P          = NVRVLand%NVRV%SoilM_Precip_P(indxVeg,indxElem) + NVRVLand%NVRV%SoilM_AW_P(indxVeg,indxElem) + NVRVLand%NVRV%SoilM_Oth_P(indxVeg,indxElem) 
            rGM               = rGenericMoisture(1,indxElem) * rRootDepth * rDeltaT
            
            !Route moisture mainly to compute actual ET
            CALL NonPondedLUMoistureRouter(rPrecipD                               ,  &
                                           NVRVLand%NVRV%SMax(indxVeg,indxElem)   ,  &
                                           rSoilM_P                               ,  &
                                           rETc_effect                            ,  & 
                                           rHydCond                               ,  & 
                                           rTotalPorosityVeg                      ,  & 
                                           rFieldCapacityVeg                      ,  & 
                                           rWiltingPointVeg                       ,  &
                                           rLambda                                ,  & 
                                           rGM                                    ,  &
                                           SolverData%Tolerance*rTotalPorosityVeg ,  &
                                           iKunsatMethod                          ,  &
                                           SolverData%IterMax                     ,  &
                                           rSoilM                                 ,  & 
                                           rRunoff                                ,  & 
                                           rPrecipInfilt                          ,  & 
                                           rETa                                   ,  & 
                                           rPerc                                  ,  & 
                                           rExcess                                ,  &
                                           rAchievedConv                          ) 
            
            !Generate error if convergence is not achieved
            IF (rAchievedConv .NE. 0.0) THEN
                MessageArray(1) = 'Convergence error in computing required outflow from streams to meet riparian ET!'
                MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemIDs(indxElem)))
                WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*rTotalPorosityVeg
                WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(rAchievedConv)
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                CYCLE
            END IF
            
            !Required stream flow to meet ET
            IF (rETa .LT. rETc_effect) THEN
                rETFromStrm_Required(indxRiparian,indxElem) = (rETc_effect-rETa) * NVRVLand%NVRV%Area(indxVeg,indxElem)
            ELSE
                rETFromStrm_Required(indxRiparian,indxElem) = 0.0
            END IF
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    !Store the riparian ET demand from streams in persistent array
    CALL NVRVLand%RVETFromStrm%SetRequiredET_AtElements(rETFromStrm_Required)
    
  END SUBROUTINE ComputeWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE FLOW PROCESSES 
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(NVRVLand,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ElemsToGW,SolverData,lLakeElem,iStat,iColCropCoeff_NVRV)
    CLASS(NativeRiparianLandUse_v41_Type)  :: NVRVLand
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(ETType),INTENT(IN)                :: ETData
    TYPE(RootZoneSoil_v41_Type),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                     :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:)
    INTEGER,INTENT(IN)                     :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)        :: SolverData
    LOGICAL,INTENT(IN)                     :: lLakeElem(:)
    INTEGER,INTENT(OUT)                    :: iStat
    INTEGER,OPTIONAL,INTENT(IN)            :: iColCropCoeff_NVRV(:,:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'Simulate'
    INTEGER                               :: indxElem,iKunsatMethod,iElemID,indxVeg,iNNVRV
    REAL(8)                               :: rAchievedConv,rETc(NVRVLand%iNNVRV),rHydCond,rTotalPorosity,rArea,rETc_effect,    &
                                             rFieldCapacity,rTotalPorosityCrop,rFieldCapacityCrop,rLambda,rRootDepth,rSupply,  &
                                             rWiltingPoint,rExcess,rWiltingPointCrop,rSoilM,rSoilM_P,rGM,rMultip,rInflow,      &
                                             rRatio(2),rPrecipD,rRiparianETStrm(AppGrid%NElements),rInfilt(3), &
                                             rSoilM_P_Array(3),rSoilM_Array(3),rETPartition(3)
    LOGICAL                               :: lElemFlowToGW
    
    !Initialize
    iStat  = 0
    iNNVRV = NVRVLand%iNNVRV
  
    !Inform user
    CALL EchoProgress('Simulating flows at native and riparian vegetation lands')
    
    !Riparian ET from streams
    rRiparianETStrm = 0.0
    IF (NVRVLand%lRVETFromStrm_Simulated) CALL NVRVLand%RVETFromStrm%GetActualET_AtElements(rRiparianETStrm) 
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,NVRVLand,lLakeElem,ETData,SoilsData,DeltaT,Precip,iStat,GenericMoisture,  &
    !$OMP                                  ElemSupply,ElemsToGW,SolverData,rRiparianETStrm,iNNVRV,iColCropCoeff_NVRV)   
    !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,96)
    DO indxElem=1,AppGrid%NElements
        !Initalize flows
        NVRVLand%NVRV%Runoff(:,indxElem)          = 0.0  
        NVRVLand%NVRV%PrecipInfilt(:,indxElem)    = 0.0     
        NVRVLand%NVRV%ETa(:,indxElem)             = 0.0     
        NVRVLand%NVRV%Perc(:,indxElem)            = 0.0   
        NVRVLand%NVRV%GMExcess(:,indxElem)        = 0.0  
        NVRVLand%NVRV%ETFromGW_Actual(:,indxElem) = 0.0  
        
        !Cycle if it is lake element
        IF (lLakeElem(indxElem)) CYCLE
            
        !Does the surface flow go to groundwater
        IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
            lElemFlowToGW = .TRUE.
        ELSE
            lElemFlowToGW = .FALSE.
        END IF
            
        !Data for simulation
        rWiltingPoint  = SoilsData(indxElem)%WiltingPoint
        rFieldCapacity = SoilsData(indxElem)%FieldCapacity
        rTotalPorosity = SoilsData(indxElem)%TotalPorosity
        rHydCond       = SoilsData(indxElem)%HydCond
        rLambda        = SoilsData(indxElem)%Lambda
        iKunsatMethod  = SoilsData(indxElem)%KunsatMethod
        IF (PRESENT(iColCropCoeff_NVRV)) THEN
            rETc       = ETData%GetValues(NVRVLand%NVRV%iColETc(:,indxElem),iColCropCoeff_NVRV(:,indxElem)) 
        ELSE
            rETc       = ETData%GetValues(NVRVLand%NVRV%iColETc(:,indxElem)) 
        END IF
        rPrecipD       = Precip(indxElem) * DeltaT

        DO indxVeg=1,iNNVRV
            !Cycle if area is zero
            rArea = NVRVLand%NVRV%Area(indxVeg,indxElem)
            IF (rArea .EQ. 0.0) CYCLE
                       
            !Water supply as runoff from upstream elements 
            rSupply = ElemSupply(indxElem) * DeltaT / rArea
            
            !Initialize
            rRootDepth         = NVRVLand%rRootDepth(indxVeg) 
            rTotalPorosityCrop = rTotalPorosity * rRootDepth
            rFieldCapacityCrop = rFieldCapacity * rRootDepth
            rWiltingPointCrop  = rWiltingPoint  * rRootDepth
            rSoilM_P           = NVRVLand%NVRV%SoilM_Precip_P(indxVeg,indxElem) + NVRVLand%NVRV%SoilM_AW_P(indxVeg,indxElem) + NVRVLand%NVRV%SoilM_Oth_P(indxVeg,indxElem)
            rGM                = GenericMoisture(1,indxElem) * rRootDepth * DeltaT
            rInflow            = rSupply + rGM 
            
            !ET from GW
            NVRVLand%NVRV%ETFromGW_Actual(indxVeg,indxElem) = MIN(rETc(indxVeg)*DeltaT , NVRVLand%NVRV%ETFromGW_Max(indxVeg,indxElem))
            rETc_effect                                     = rETc(indxVeg)*DeltaT - NVRVLand%NVRV%ETFromGW_Actual(indxVeg,indxElem)
            
            !Simulate
            CALL NonPondedLUMoistureRouter(rPrecipD                                     ,  &
                                           NVRVLand%NVRV%SMax(indxVeg,indxElem)         ,  &
                                           rSoilM_P                                     ,  &
                                           rETc_effect                                  ,  & 
                                           rHydCond                                     ,  & 
                                           rTotalPorosityCrop                           ,  & 
                                           rFieldCapacityCrop                           ,  & 
                                           rWiltingPointCrop                            ,  &
                                           rLambda                                      ,  & 
                                           rInflow                                      ,  &
                                           SolverData%Tolerance*rTotalPorosityCrop      ,  &
                                           iKunsatMethod                                ,  &
                                           SolverData%IterMax                           ,  &
                                           rSoilM                                       ,  & 
                                           NVRVLand%NVRV%Runoff(indxVeg,indxElem)       ,  & 
                                           NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem) ,  & 
                                           NVRVLand%NVRV%ETa(indxVeg,indxElem)          ,  & 
                                           NVRVLand%NVRV%Perc(indxVeg,indxElem)         ,  & 
                                           rExcess                                      ,  &
                                           rAchievedConv                                ) 
                                         
            !Generate error if convergence is not achieved
            IF (rAchievedConv .NE. 0.0) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Convergence error in soil moisture routing for native vegetation!'
                MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*rTotalPorosityCrop
                WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(rAchievedConv)
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                CYCLE
            END IF
            
            !Reduce total infiltration based on correction for total porosity
            IF (rExcess .NE. 0.0) THEN
                rRatio = [NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem) , rGM]
                CALL NormalizeArray(rRatio)
                NVRVLand%NVRV%Runoff(indxVeg,indxElem)       = NVRVLand%NVRV%Runoff(indxVeg,indxElem) + rExcess * rRatio(1)
                NVRVLand%NVRV%GMExcess(indxVeg,indxElem)     = rExcess * rRatio(2)
                NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem) = rPrecipD - NVRVLand%NVRV%Runoff(indxVeg,indxElem)
            END IF
            
            !Compute moisture from precip and irrigation
            rSoilM_P_Array = [NVRVLand%NVRV%SoilM_Precip_P(indxVeg,indxElem) , NVRVLand%NVRV%SoilM_AW_P(indxVeg,indxElem) , NVRVLand%NVRV%SoilM_Oth_P(indxVeg,indxElem)  ]
            rInfilt        = [NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem)   , 0d0                                        , rGM-NVRVLand%NVRV%GMExcess(indxVeg,indxElem) ]
            CALL TrackMoistureDueToSource(rSoilM_P_Array                       ,  &
                                          rInfilt                              ,  &
                                          NVRVLand%NVRV%Perc(indxVeg,indxElem) ,  &
                                          NVRVLand%NVRV%ETa(indxVeg,indxElem)  ,  &
                                          0d0                                  ,  &
                                          rSoilM_Array                         ,  &
                                          rETPartition                         )
            NVRVLand%NVRV%SoilM_Precip(indxVeg,indxElem) = rSoilM_Array(1)
            NVRVLand%NVRV%SoilM_AW(indxVeg,indxElem)     = rSoilM_Array(2)
            NVRVLand%NVRV%SoilM_Oth(indxVeg,indxElem)    = rSoilM_Array(3)
            
            !Make sure soil moisture is not less than zero
            IF (ANY(rSoilM_Array.LT.0.0)) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Soil moisture content becomes negative at element '//TRIM(IntToText(iElemID))//' for native vegetation.'
                MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
                MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
                MessageArray(4) = 'in the Root Zone Main Input File.'
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                CYCLE
            END IF
            
            !Convert depths to volumetric rates
            rMultip                                         = rArea / DeltaT
            NVRVLand%NVRV%Runoff(indxVeg,indxElem)          = NVRVLand%NVRV%Runoff(indxVeg,indxElem)          * rMultip
            NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem)    = NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem)    * rMultip
            NVRVLand%NVRV%Perc(indxVeg,indxElem)            = NVRVLand%NVRV%Perc(indxVeg,indxElem)            * rMultip
            NVRVLand%NVRV%ETFromGW_Actual(indxVeg,indxElem) = NVRVLand%NVRV%ETFromGW_Actual(indxVeg,indxElem) * rMultip
            NVRVLand%NVRV%ETa(indxVeg,indxElem)             = NVRVLand%NVRV%ETa(indxVeg,indxElem)             * rMultip + NVRVLand%NVRV%ETFromGW_Actual(indxVeg,indxElem)    !Includes ET from groundwater 
            
            !Add ripraian ET to actual ET
            IF (indxVeg .GT. NVRVLand%iNNative) NVRVLand%NVRV%ETa(indxVeg,indxElem) = NVRVLand%NVRV%ETa(indxVeg,indxElem) + rRiparianETStrm(indxElem)
            
            !If surface flow goes to groundwater, update the runoff processes
            IF (lElemFlowToGW) THEN
                NVRVLand%NVRV%Perc(indxVeg,indxElem)         = NVRVLand%NVRV%Perc(indxVeg,indxElem) + NVRVLand%NVRV%Runoff(indxVeg,indxElem)
                NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem) = NVRVLand%NVRV%PrecipInfilt(indxVeg,indxElem) + NVRVLand%NVRV%Runoff(indxVeg,indxElem)        !Runoff is assumed to bypass root zone for proper mass balance    
                NVRVLand%NVRV%Runoff(indxVeg,indxElem)       = 0.0
            END IF
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
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
  ! --- COMPUTE MAXIMUM POSSIBLE ET FROM GW
  ! -------------------------------------------------------------
  SUBROUTINE NVRV_ComputeETFromGW_Max(NVRVLand,DepthToGW,Sy,CapillaryRise)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    REAL(8),INTENT(IN)                    :: DepthToGW(:),Sy(:),CapillaryRise(:)
    
    CALL ComputeETFromGW_Max(DepthToGW,Sy,NVRVLand%rRootDepth,CapillaryRise,NVRVLand%NVRV%Area,NVRVLand%NVRV%ETFromGW_Max)
    
  END SUBROUTINE NVRV_ComputeETFromGW_Max
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT SOIL INITIAL MOISTURE CONTENTS TO DEPTHS
  ! ---  Note: Called only once at the beginning of simulation
  ! -------------------------------------------------------------
  SUBROUTINE SoilMContent_To_Depth(NVRVLand,NElements,iElemIDs,TotalPorosity,iStat)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    INTEGER,INTENT(IN)                    :: NElements,iElemIDs(NElements)
    REAL(8),INTENT(IN)                    :: TotalPorosity(:)
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxElem,inNVRV,indxVeg
    REAL(8)                      :: rRootDepth,rTotalPorosity
    
    !Initailize
    iStat  = 0
    iNNVRV = NVRVLand%iNNVRV
    
    !Return if native and riparian lands are not simulated
    IF (iNNVRV .EQ. 0) RETURN
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    DO indxElem=1,NElements
        rTotalPorosity = TotalPorosity(indxElem)
        DO indxVeg=1,iNNVRV       
            IF ((NVRVLand%NVRV%SoilM_Precip(indxVeg,indxElem) + NVRVLand%NVRV%SoilM_AW(indxVeg,indxElem) + NVRVLand%NVRV%SoilM_Oth(indxVeg,indxElem)) .GT. rTotalPorosity) THEN
                CALL SetLastMessage('Initial moisture content for native/riparian vegetation at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            rRootDepth                                   = NVRVLand%rRootDepth(indxVeg)
            NVRVLand%NVRV%SoilM_Precip(indxVeg,indxElem) = NVRVLand%NVRV%SoilM_Precip(indxVeg,indxElem) * rRootDepth
            NVRVLand%NVRV%SoilM_AW(indxVeg,indxElem)     = NVRVLand%NVRV%SoilM_AW(indxVeg,indxElem)     * rRootDepth
            NVRVLand%NVRV%SoilM_Oth(indxVeg,indxElem)    = NVRVLand%NVRV%SoilM_Oth(indxVeg,indxElem)    * rRootDepth
        END DO
    END DO 
    
    !Equate beginning-of-timestep values to end-of-timestep values
    NVRVLand%NVRV%SoilM_Precip_P = NVRVLand%NVRV%SoilM_Precip
    NVRVLand%NVRV%SoilM_AW_P     = NVRVLand%NVRV%SoilM_AW
    NVRVLand%NVRV%SoilM_Oth_P    = NVRVLand%NVRV%SoilM_Oth

  END SUBROUTINE SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(NVRVLand,lAdvanceAreas) 
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    LOGICAL,INTENT(IN)                    :: lAdvanceAreas
    
    NVRVLand%NVRV%SoilM_Precip_P = NVRVLand%NVRV%SoilM_Precip
    NVRVLand%NVRV%SoilM_AW_P     = NVRVLand%NVRV%SoilM_AW
    NVRVLand%NVRV%SoilM_Oth_P    = NVRVLand%NVRV%SoilM_Oth

    IF (lAdvanceAreas) CALL NVRVLand%AdvanceAreas() 
    
    CALL NVRVLand%RVETFromStrm%AdvanceState()
    
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(NVRVLand) 
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    
    NVRVLand%NVRV%Area_P = NVRVLand%NVRV%Area
    
  END SUBROUTINE AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFIED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(NVRVLand,iElemIDs,rElemAreas,TimeStep,iStat)
    CLASS(NativeRiparianLandUse_v41_Type) :: NVRVLand
    INTEGER,INTENT(IN)                    :: iElemIDs(:)
    REAL(8),INTENT(IN)                    :: rElemAreas(:)
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep 
    INTEGER,INTENT(OUT)                   :: iStat
    
    CALL NVRVLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,rElemAreas,iElemIDs,iStat)
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp
  
END MODULE