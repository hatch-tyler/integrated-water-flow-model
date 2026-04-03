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
                                      PrepareTSDOutputFile          , &
                                      f_iHDF                        , &
                                      f_iDSS
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter       , &
                                      CleanSpecialCharacters        , &
                                      EstablishAbsolutePathFilename , &
                                      IntToText                     , &
                                      ArrangeText                   , &
                                      PrepareTitle                  , &
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
                CALL SetLastMessage('Error allocating memory for AllSubsOut file.'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            CALL AppSubsidence%AllSubsOutFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.TRUE., &
                Descriptor='subsidence at all nodes output',iStat=iStat)
            IF (iStat .EQ. -1) RETURN
            !Make sure DSS file is used only if it is a time-tracking simulation
            IF (AppSubsidence%AllSubsOutFile%iGetFileType() .EQ. f_iDSS) THEN
                IF (.NOT. TimeStep%TrackTime) THEN
                    CALL SetLastMessage('DSS files for subsidence at all nodes can only be used for time-tracking simulations.',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
            !Use NTIME from caller
            IF (.NOT. PRESENT(NTIME)) THEN
                CALL SetLastMessage('NTIME must be provided for subsidence v4.1 AllSubsOut output!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Setup output file based on format (same pattern as AllHeadOutFile_New)
            IF (AppSubsidence%AllSubsOutFile%iGetFileType() .EQ. f_iHDF) THEN
                !HDF5: create dataset with canonical layout
                BLOCK
                    INTEGER   :: NColumns_HDF(1)
                    CHARACTER :: cDataSetName_HDF(1)*24
                    NColumns_HDF(1)     = NNodes * NLayers
                    cDataSetName_HDF(1) = '/SubsidenceAtAllNodes'
                    CALL AppSubsidence%AllSubsOutFile%CreateHDFDataSet(cPathNames=cDataSetName_HDF,NColumns=NColumns_HDF, &
                        NTime=NTIME+1,TimeStep=TimeStep,DataType=0d0,iStat=iStat)
                    IF (iStat .EQ. -1) RETURN
                END BLOCK
                !Print initial zero subsidence
                BLOCK
                    REAL(8) :: rZeroData(NNodes*NLayers, 1)
                    rZeroData = 0d0
                    CALL AppSubsidence%AllSubsOutFile%WriteData(rZeroData)
                END BLOCK
            ELSE
                !Text/DSS: use PrepareTSDOutputFile (same pattern as AllHeadOutFile_New)
                BLOCK
                    INTEGER   :: I, J
                    CHARACTER :: cText*20,cFormatSpec*500,cDataUnit(1)*10,cDataType(1)*10, &
                                 cCPart(1)*32,cFPart(1)*32,cHeader(2,1+NNodes)*50,        &
                                 cHeaderFormat(2)*500,cWorkArray(2)*300,cTitleLines(1)*3000
                    INTEGER   :: iNodeIDs_Local(NNodes)
                    CALL AppGrid%GetNodeIDs(iNodeIDs_Local)
                    cHeader     = ''
                    cTitleLines = ''
                    cText = TRIM(IntToText(NNodes))//'(2X,F10.4)'
                    SELECT CASE (NLayers)
                      CASE (1)
                        cFormatSpec='(A21,'//TRIM(cText)//')'
                      CASE DEFAULT
                        cFormatSpec=                              '(A21,'//TRIM(cText) // ","  //  &
                                    TRIM(IntToText(NLayers-1))//'(/,21X,'//TRIM(cText)//'))'
                    END SELECT
                    cDataUnit(1) = 'FEET'
                    cDataType(1) = 'INST-VAL'
                    cCPart(1)    = 'SUBSIDENCE'
                    cFPart(1)    = 'SUBS_AT_ALL_NODES'
                    cText        = IntToText(NNodes)
                    cWorkArray(1) = ArrangeText('SUBSIDENCE AT ALL NODES',37)
                    cWorkArray(2) = ArrangeText('(UNIT=','FEET',')',37)
                    CALL PrepareTitle(cTitleLines(1),cWorkArray,39,42)
                    WRITE (cHeader(1,1),'(A1,28X,A4)') '*','NODE'
                    WRITE (cHeader(2,1),'(A1,8X,A4)')  '*','TIME'
                    DO I=1,NNodes
                      WRITE (cHeader(2,I+1),'(I7)') iNodeIDs_Local(I)
                    END DO
                    cHeaderFormat(1)='(A33,'//TRIM(cText)//'(A))'
                    cHeaderFormat(2)='(A13,8X,'//TRIM(cText)//'(5X,A7))'
                    CALL PrepareTSDOutputFile(AppSubsidence%AllSubsOutFile              , &
                                              NColumnsOfData          = NNodes          , &
                                              NRowsOfData             = NLayers          , &
                                              OverwriteNColumnsOfData = .FALSE.          , &
                                              FormatSpec              = cFormatSpec      , &
                                              Title                   = cTitleLines      , &
                                              Header                  = cHeader          , &
                                              HeaderFormat            = cHeaderFormat    , &
                                              PrintColumnNo           = .FALSE.          , &
                                              DataUnit                = cDataUnit        , &
                                              DataType                = cDataType        , &
                                              CPart                   = cCPart           , &
                                              FPart                   = cFPart           , &
                                              UnitT                   = TimeStep%Unit    , &
                                              Layers=[((I,J=1,NNodes),I=1,NLayers)]             , &
                                              GWNodes=[((iNodeIDs_Local(I),I=1,NNodes),J=1,NLayers)] , &
                                              iStat=iStat                                )
                    IF (iStat .EQ. -1) RETURN
                END BLOCK
                !Print initial zero subsidence
                BLOCK
                    REAL(8) :: rZeroArray(NLayers, NNodes)
                    CHARACTER :: cSimTime*21
                    rZeroArray = 0d0
                    IF (TimeStep%TrackTime) THEN
                        cSimTime = ADJUSTL(TimeStep%CurrentDateAndTime)
                    ELSE
                        WRITE(cSimTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
                    END IF
                    CALL AppSubsidence%AllSubsOutFile%WriteData(cSimTime,rZeroArray,FinalPrint=.FALSE.)
                END BLOCK
            END IF
            AppSubsidence%lAllSubsOutFile_Defined = .TRUE.
        END IF
    END IF

    !Read remaining configuration via shared v4.0 helper (IC file onward)
    CALL ReadSubsidenceConfig_v40(AppSubsidence,SubsMainFile,IsForInquiry,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat,SubsICFile)
    IF (iStat .EQ. -1) RETURN

    !Close file
    CALL SubsMainFile%Kill()

  END SUBROUTINE AppSubsidence_v41_New


END MODULE Class_AppSubsidence_v41
