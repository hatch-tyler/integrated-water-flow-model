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
MODULE Class_AppSubsidence_v41
  !-----------------------------------------------------------------------------
  ! Version 4.1 of the subsidence component.
  ! Extends version 4.0 by adding AllSubsidenceAtAllNodes HDF5 output.
  !
  ! Input file reading order for v4.1:
  !   #4.1
  !   AllSubsOut HDF5 filename     <-- NEW in v4.1 (before IC)
  !   IC filename
  !   Tecplot output filename
  !   Final results output filename
  !   FACTLTOU
  !   UNITLTOU
  !   Hydrograph output specs (via HydOutputType%New)
  !   Subsidence parameters
  !   IC data
  !-----------------------------------------------------------------------------
  USE MessageLogger           , ONLY: SetLastMessage                , &
                                      EchoProgress                  , &
                                      f_iFatal
  USE IOInterface             , ONLY: GenericFileType               , &
                                      f_iHDF
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter       , &
                                      CleanSpecialCharacters        , &
                                      EstablishAbsolutePathFilename , &
                                      IntToText                     , &
                                      f_cInlineCommentChar
  USE TimeSeriesUtilities     , ONLY: TimeStepType
  USE Package_Discretization  , ONLY: AppGridType                   , &
                                      StratigraphyType
  USE Class_AppSubsidence_v40 , ONLY: AppSubsidence_v40_Type        , &
                                      ReadSubsidenceConfig_v40
  IMPLICIT NONE



! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: AppSubsidence_v41_Type


  ! -------------------------------------------------------------
  ! --- v4.1 SUBSIDENCE TYPE (extends v4.0)
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AppSubsidence_v40_Type) :: AppSubsidence_v41_Type
  CONTAINS
      PROCEDURE,PASS :: New => AppSubsidence_v41_New
  END TYPE AppSubsidence_v41_Type


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 25
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppSubsidence_v41::'



CONTAINS




! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INSTANTIATE v4.1 SUBSIDENCE COMPONENT
  ! --- Reads AllSubsOut HDF5 filename first, then delegates to
  ! --- the shared v4.0 configuration reader for all remaining input.
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v41_New(AppSubsidence,IsForInquiry,cFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat,SubsICFile,NTIME)
    CLASS(AppSubsidence_v41_Type),INTENT(OUT) :: AppSubsidence
    LOGICAL,INTENT(IN)                        :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)               :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)                        :: iGWNodeIDs(:)
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy
    COMPLEX,INTENT(IN)                        :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)             :: TimeStep
    INTEGER,INTENT(OUT)                       :: iStat
    TYPE(GenericFileType),OPTIONAL            :: SubsICFile
    INTEGER,OPTIONAL,INTENT(IN)               :: NTIME

    !Local variables
    CHARACTER(LEN=ModNameLen+21)  :: ThisProcedure = ModName // 'AppSubsidence_v41_New'
    INTEGER                       :: ErrorCode,NNodes,NLayers,NRegn
    CHARACTER                     :: cErrorMsg*300,ALine*1200,cAllSubsOutFileName*1200
    TYPE(GenericFileType)         :: SubsMainFile
    CHARACTER(:),ALLOCATABLE      :: cAbsPathFileName

    !Initialize
    iStat = 0

    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN

    !Inform user
    CALL EchoProgress('   Instantiating subsidence component (v4.1) ...')

    !Initialize
    NNodes  = AppGrid%NNodes
    NRegn   = AppGrid%NSubregions
    NLayers = Stratigraphy%NLayers

    !Open file
    CALL SubsMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='subsidence data main input',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Allocate memory
    ALLOCATE (AppSubsidence%ElasticSC(NNodes,NLayers)        ,  &
              AppSubsidence%InelasticSC(NNodes,NLayers)      ,  &
              AppSubsidence%InterbedThick_P(NNodes,NLayers)  ,  &
              AppSubsidence%InterbedThick(NNodes,NLayers)    ,  &
              AppSubsidence%InterbedThickMin(NNodes,NLayers) ,  &
              AppSubsidence%Subsidence(NNodes,NLayers)       ,  &
              AppSubsidence%CumSubsidence_P(NNodes,NLayers)  ,  &
              AppSubsidence%CumSubsidence(NNodes,NLayers)    ,  &
              AppSubsidence%PreCompactHead(NNodes,NLayers)   ,  &
              AppSubsidence%RegionalCumSubsidence(NRegn)     ,  &
              AppSubsidence%RegionalCumSubsidence_P(NRegn)   ,  &
              STAT=ErrorCode , ERRMSG=cErrorMsg              )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for subsidence parameters!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Initialize values
    AppSubsidence%InterbedThick_P         = 0d0
    AppSubsidence%InterbedThick           = 0d0
    AppSubsidence%InterbedThickMin        = 0d0
    AppSubsidence%Subsidence              = 0d0
    AppSubsidence%CumSubsidence_P         = 0d0
    AppSubsidence%CumSubsidence           = 0d0
    AppSubsidence%PreCompactHead          = HUGE(0d0)
    AppSubsidence%RegionalCumSubsidence_P = 0d0

    !Read away the version line
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !--- v4.1: AllSubsOut HDF5 output file (read BEFORE IC filename) ---
    CALL SubsMainFile%ReadData(cAllSubsOutFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cAllSubsOutFileName = StripTextUntilCharacter(cAllSubsOutFileName,f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cAllSubsOutFileName)
    IF (TRIM(ADJUSTL(cAllSubsOutFileName)) .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cAllSubsOutFileName)),cWorkingDirectory,cAbsPathFileName)
        IF (.NOT. IsForInquiry) THEN
            ALLOCATE (AppSubsidence%AllSubsOutFile , STAT=ErrorCode , ERRMSG=cErrorMsg)
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error allocating memory for AllSubsOut HDF5 file.'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            CALL AppSubsidence%AllSubsOutFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.TRUE., &
                Descriptor='subsidence at all nodes output',iStat=iStat)
            IF (iStat .EQ. -1) RETURN
            !Verify HDF5 format
            IF (AppSubsidence%AllSubsOutFile%iGetFileType() .NE. f_iHDF) THEN
                CALL SetLastMessage('AllSubsOut file must be an HDF5 file (.hdf extension)!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Use NTIME from caller
            IF (.NOT. PRESENT(NTIME)) THEN
                CALL SetLastMessage('NTIME must be provided for subsidence v4.1 AllSubsOut output!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Create HDF5 dataset: /SubsidenceAtAllNodes with NNodes*NLayers columns
            BLOCK
                INTEGER   :: NColumns_HDF(1)
                CHARACTER :: cDataSetName_HDF(1)*24
                NColumns_HDF(1)     = NNodes * NLayers
                cDataSetName_HDF(1) = '/SubsidenceAtAllNodes'
                CALL AppSubsidence%AllSubsOutFile%CreateHDFDataSet(cPathNames=cDataSetName_HDF,NColumns=NColumns_HDF, &
                    NTime=NTIME+1,TimeStep=TimeStep,DataType=0d0,iStat=iStat)
                IF (iStat .EQ. -1) RETURN
            END BLOCK
            AppSubsidence%lAllSubsOutFile_Defined = .TRUE.
            !Print initial zero subsidence
            BLOCK
                REAL(8) :: rZeroData(NNodes*NLayers, 1)
                rZeroData = 0d0
                CALL AppSubsidence%AllSubsOutFile%WriteData(rZeroData)
            END BLOCK
        END IF
    END IF

    !Read remaining configuration via shared v4.0 helper (IC file onward)
    CALL ReadSubsidenceConfig_v40(AppSubsidence,SubsMainFile,IsForInquiry,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat,SubsICFile)
    IF (iStat .EQ. -1) RETURN

    !Close file
    CALL SubsMainFile%Kill()

  END SUBROUTINE AppSubsidence_v41_New


END MODULE Class_AppSubsidence_v41
