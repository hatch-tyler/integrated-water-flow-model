!***********************************************************************
!  IWFM2OBS - Class_HydrographReader
!  Model file discovery (simulation main → component files → .out paths)
!  and hydrograph .out file reading with conversion to temp SMP files.
!
!  Ports the model-file-parsing functionality from the old iwfm2obs
!  (iwfm2obs_2015) into a reusable, modern Fortran module that
!  integrates with the new IWFM2OBS architecture.
!***********************************************************************
MODULE Class_HydrographReader

  USE MessageLogger      , ONLY: MessageLoggerType , &
                                 f_iFatal          , &
                                 f_iWarn           , &
                                 f_iInfo
  USE GeneralUtilities   , ONLY: IntToText         , &
                                 UpperCase         , &
                                 EstablishAbsolutePathFileName
  USE TimeSeriesUtilities, ONLY: DayMonthYearToJulianDate      , &
                                 JulianDateToDayMonthYear      , &
                                 TimeStepType                  , &
                                 IncrementTimeStamp
  USE IOInterface        , ONLY: GenericFileType               , &
                                 RealTSDataInFileType          , &
                                 iGetFileType_FromName         , &
                                 f_iHDF                        , &
                                 f_iDSS
  USE IWFM2OBS_Utilities , ONLY: StripAndClean                 , &
                                 SortStringsIndex              , &
                                 BinarySearchStr               , &
                                 ParseDateFromString           , &
                                 ComputeDateJulian

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: HydrographReaderType
  PUBLIC :: HydrographReader_SetModuleLogger

  CHARACTER(LEN=30), PARAMETER :: cModName = 'Class_HydrographReader'
  TYPE(MessageLoggerType),POINTER,PRIVATE :: ModuleLogger => NULL()

  ! Hydrograph type indices (must match Class_IWFM2OBS)
  INTEGER, PARAMETER, PUBLIC :: iHR_SUBSID = 1
  INTEGER, PARAMETER, PUBLIC :: iHR_TILEDR = 2
  INTEGER, PARAMETER, PUBLIC :: iHR_STREAM = 3
  INTEGER, PARAMETER, PUBLIC :: iHR_GWHEAD = 4
  INTEGER, PARAMETER, PUBLIC :: iHR_NUMHYD = 4

  ! =====================================================================
  ! HydFileInfoType - Information about one hydrograph output
  ! =====================================================================
  TYPE :: HydFileInfoType
    INTEGER              :: iNHyd   = 0         ! number of hydrographs
    CHARACTER(LEN=500)   :: cOutFilePath = ' '   ! .out file path
    CHARACTER(LEN=25), ALLOCATABLE :: cHydIDs(:) ! bore IDs from component main
    INTEGER, ALLOCATABLE :: iLayers(:)           ! IOUTHL layer numbers (GW/subsid only)
    LOGICAL              :: lActive = .FALSE.
  END TYPE HydFileInfoType

  ! =====================================================================
  ! HydrographReaderType - Model discovery and .out reader
  ! =====================================================================
  TYPE :: HydrographReaderType
    CHARACTER(LEN=500)  :: cSimMainFile = ' '
    CHARACTER(LEN=500)  :: cWorkDir     = ' '
    CHARACTER(LEN=500)  :: cSimDir      = ' '  ! Simulation main file directory
    CHARACTER(LEN=500)  :: cGWMainFile  = ' '
    CHARACTER(LEN=500)  :: cStreamMainFile = ' '
    CHARACTER(LEN=500)  :: cTileDrainFile = ' '
    CHARACTER(LEN=500)  :: cSubsidenceFile = ' '
    TYPE(HydFileInfoType) :: HydInfo(iHR_NUMHYD)
    ! Simulation time info
    CHARACTER(LEN=10)   :: cTimeUnit  = ' '
    INTEGER             :: iStartDay  = 0
    INTEGER             :: iStartMon  = 0
    INTEGER             :: iStartYr   = 0
    INTEGER             :: iDateSpec  = 2  ! 1=dd/mm, 2=mm/dd
    LOGICAL             :: lDiscovered = .FALSE.
    CHARACTER(LEN=500)  :: cWellSpecFile = ' '  ! well_specs.dat path (for ResultsExtract fallback)
    INTEGER             :: iNLayers = 4         ! Number of model layers
    ! In-memory model data (populated by ReadDotOutFileDirect)
    REAL(8), ALLOCATABLE    :: rModelData(:,:)   ! (iNTimes, iNFiltered)
    INTEGER, ALLOCATABLE    :: iModelDays(:)     ! Julian days per timestep
    INTEGER, ALLOCATABLE    :: iModelSecs(:)     ! Seconds per timestep
    INTEGER                 :: iNTimes = 0
    CHARACTER(LEN=25), ALLOCATABLE :: cFilteredIDs(:) ! IDs of stored hydrographs
    INTEGER                 :: iNFiltered = 0
  CONTAINS
    PROCEDURE, PASS :: DiscoverModelFiles
    PROCEDURE, PASS :: ReadHydrographToSMP
    PROCEDURE, PASS :: ReadHydrographData
    PROCEDURE, PASS :: Kill
  END TYPE HydrographReaderType

CONTAINS


  ! -------------------------------------------------------------
  ! --- SET MODULE LOGGER
  ! -------------------------------------------------------------
  SUBROUTINE HydrographReader_SetModuleLogger(Logger)
    TYPE(MessageLoggerType),TARGET,INTENT(IN) :: Logger

    ModuleLogger => Logger

  END SUBROUTINE HydrographReader_SetModuleLogger


  ! =====================================================================
  ! ReadNonComment - Read one non-comment line from a Fortran unit
  !   Skips lines starting with C, c, *, #, or blank lines
  ! =====================================================================
  ! =====================================================================
  ! ExtractFilePath - Read data line from GenericFileType, strip comment,
  !   resolve path. Uses kernel IO throughout.
  ! =====================================================================
  SUBROUTINE ExtractFilePath(InFile, cBaseDir, cPath, iStat)
    TYPE(GenericFileType), INTENT(INOUT) :: InFile
    CHARACTER(LEN=*),     INTENT(IN)    :: cBaseDir
    CHARACTER(LEN=*),     INTENT(OUT)   :: cPath
    INTEGER,              INTENT(OUT)   :: iStat

    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(:), ALLOCATABLE :: cAbsPath

    CALL InFile%ReadData(cLine, iStat)
    IF (iStat /= 0) RETURN
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) == 0) THEN
      cPath = ' '
      RETURN
    END IF
    CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cBaseDir), cAbsPath)
    IF (ALLOCATED(cAbsPath)) THEN
      cPath = cAbsPath
    ELSE
      cPath = TRIM(cClean)
    END IF
  END SUBROUTINE ExtractFilePath

  ! =====================================================================
  ! DiscoverModelFiles - Parse simulation main file → component paths
  !
  !   Reads the IWFM simulation main file to extract:
  !     - GW main file path, Stream main file path
  !     - Start date, time unit
  !   Then parses each component main file to find:
  !     - .out hydrograph file paths
  !     - hydrograph IDs (bore names)
  !     - layer numbers (IOUTHL for GW/subsidence)
  !
  !   Adapted from old iwfm2obs.f90 lines 127-502
  ! =====================================================================
  SUBROUTINE DiscoverModelFiles(This, cSimMainFile, cWorkDir, iDateSpec, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),           INTENT(IN)    :: cSimMainFile
    CHARACTER(LEN=*),           INTENT(IN)    :: cWorkDir
    INTEGER,                    INTENT(IN)    :: iDateSpec
    INTEGER,                    INTENT(OUT)   :: iStat

    TYPE(GenericFileType) :: SimFile
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500)  :: cSimDir, cPath
    CHARACTER(:), ALLOCATABLE :: cAbsPath
    INTEGER :: iErr, i, iPos
    INTEGER :: iNOUTH, iNOUTF, iNOUTR, iNSI, iNOUTS, iNTD
    INTEGER :: iHydType, j, iColID
    CHARACTER(LEN=25) :: cID
    INTEGER :: iNumTokens
    CHARACTER(LEN=30) :: cTokens(10)

    iStat = 0
    This%cSimMainFile = cSimMainFile
    This%cWorkDir     = cWorkDir
    This%iDateSpec    = iDateSpec

    ! Determine directory of simulation main file
    cSimDir = cSimMainFile
    iPos = MAX(SCAN(cSimDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cSimDir = cSimDir(1:iPos)
    ELSE
      cSimDir = cWorkDir
    END IF
    This%cSimDir = cSimDir

    ! ==================================================================
    ! 1. Parse simulation main file (via GenericFileType — auto-skips C/c/* comments)
    ! ==================================================================
    CALL SimFile%New(FileName=cSimMainFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                     Descriptor='simulation main file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! 3 title lines (GenericFileType auto-skips C/c/* comment blocks)
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN

    ! File 1: preprocessor output (skip)
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN

    ! File 2: GW main file
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL StripAndClean(cLine, cClean)
    CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cSimDir), cAbsPath)
    This%cGWMainFile = cAbsPath

    ! File 3: Stream main file
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL StripAndClean(cLine, cClean)
    CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cSimDir), cAbsPath)
    This%cStreamMainFile = cAbsPath

    ! Files 4-11: skip remaining 8 file entries
    DO i = 1, 8
      CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    END DO

    ! BDT (start date) — extract first token, then date portion before '_'
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL StripAndClean(cLine, cClean)
    iPos = INDEX(cClean, ' ')
    IF (iPos > 1) cClean = cClean(1:iPos-1)
    iPos = SCAN(cClean, '_')
    IF (iPos > 0) cClean = cClean(1:iPos-1)
    CALL ParseDateFromString(cClean, iDateSpec, This%iStartDay, This%iStartMon, &
         This%iStartYr, iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot parse start date from simulation file', &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Skip 1 line (restart flag), then read time unit
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL StripAndClean(cLine, cClean)
    cClean = UpperCase(cClean)
    IF (cClean(1:4) == '1DAY') THEN
      This%cTimeUnit = '1DAY'
    ELSE IF (cClean(1:5) == '1WEEK') THEN
      This%cTimeUnit = '1WEEK'
    ELSE IF (cClean(1:4) == '1MON') THEN
      This%cTimeUnit = '1MON'
    ELSE IF (cClean(1:5) == '1YEAR') THEN
      This%cTimeUnit = '1YEAR'
    ELSE
      This%cTimeUnit = TRIM(cClean)
    END IF

    CALL SimFile%Kill()

    CALL ModuleLogger%LogMessage('  Simulation file: '//TRIM(cSimMainFile), f_iInfo, cModName)
    CALL ModuleLogger%LogMessage('  GW main file: '//TRIM(This%cGWMainFile), f_iInfo, cModName)
    CALL ModuleLogger%LogMessage('  Stream main file: '//TRIM(This%cStreamMainFile), f_iInfo, cModName)

    ! ==================================================================
    ! 2. Parse GW main file
    ! ==================================================================
    CALL ParseGWMainFile(This, iStat)
    IF (iStat /= 0) RETURN

    ! ==================================================================
    ! 3. Parse Stream main file
    ! ==================================================================
    CALL ParseStreamMainFile(This, iStat)
    IF (iStat /= 0) RETURN

    ! ==================================================================
    ! 4. Parse Tile Drain main file (if found from GW main)
    ! ==================================================================
    IF (LEN_TRIM(This%cTileDrainFile) > 0) THEN
      CALL ParseTileDrainMainFile(This, iStat)
      IF (iStat /= 0) iStat = 0  ! Non-fatal: continue without tile drain
    END IF

    ! ==================================================================
    ! 5. Parse Subsidence main file (if found from GW main)
    ! ==================================================================
    IF (LEN_TRIM(This%cSubsidenceFile) > 0) THEN
      CALL ParseSubsidenceMainFile(This, iStat)
      IF (iStat /= 0) iStat = 0  ! Non-fatal: continue without subsidence
    END IF

    This%lDiscovered = .TRUE.

    ! Report summary
    DO i = 1, iHR_NUMHYD
      IF (This%HydInfo(i)%lActive) THEN
        CALL ModuleLogger%LogMessage('  Discovered '//TRIM(IntToText(This%HydInfo(i)%iNHyd))// &
             ' hydrographs from .out file: '//TRIM(This%HydInfo(i)%cOutFilePath), &
             f_iInfo, cModName)
      END IF
    END DO

  END SUBROUTINE DiscoverModelFiles

  ! =====================================================================
  ! ParseGWMainFile - Extract GW hydrograph info from GW main file
  ! =====================================================================
  SUBROUTINE ParseGWMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    TYPE(GenericFileType) :: GWFile
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500)  :: cGWDir, cPath
    CHARACTER(:), ALLOCATABLE :: cAbsPath
    INTEGER :: iErr, i, iPos, iNOUTH
    INTEGER :: iID, iHydTyp, iOutHL
    REAL(8) :: rX, rY
    CHARACTER(LEN=25) :: cName

    iStat = 0

    ! Determine GW file directory
    cGWDir = This%cGWMainFile
    iPos = MAX(SCAN(cGWDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cGWDir = cGWDir(1:iPos)
    ELSE
      cGWDir = This%cWorkDir
    END IF

    CALL GWFile%New(FileName=This%cGWMainFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                    Descriptor='GW main file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! Read and discard version line (#4.0) and BC file path
    ! GenericFileType does NOT skip # lines, so we need two reads:
    !   1st returns "#4.0" (version), 2nd returns the BC path — both discarded
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF

    ! Tile drain main file path
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(cGWDir), cAbsPath)
      IF (ALLOCATED(cAbsPath)) THEN
        This%cTileDrainFile = cAbsPath
      ELSE
        This%cTileDrainFile = TRIM(cClean)
      END IF
    END IF

    ! Pumping file (skip)
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF

    ! Subsidence file path
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(cGWDir), cAbsPath)
      IF (ALLOCATED(cAbsPath)) THEN
        This%cSubsidenceFile = cAbsPath
      ELSE
        This%cSubsidenceFile = TRIM(cClean)
      END IF
    END IF

    ! Skip 16 more lines (lines 5-20 in GW main), then line 21 = NOUTH
    DO i = 1, 17
      CALL GWFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL ModuleLogger%SetLastMessage('Unexpected end of GW main file at skip line '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CALL GWFile%Kill(); iStat = -1; RETURN
      END IF
    END DO

    ! Current line is NOUTH
    CALL StripAndClean(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNOUTH
    IF (iErr /= 0 .OR. iNOUTH < 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot read NOUTH from GW main file', f_iFatal, cModName)
      CALL GWFile%Kill(); iStat = -1; RETURN
    END IF

    ! FACTXY
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF

    ! GWHYDOUTFL (hydrograph output file path)
    ! Resolve relative to simulation directory (IWFM CWD), not GW file directory
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) THEN; CALL GWFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0 .AND. iNOUTH > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPath)
      IF (ALLOCATED(cAbsPath)) THEN
        cPath = cAbsPath
      ELSE
        cPath = TRIM(cClean)
      END IF
      This%HydInfo(iHR_GWHEAD)%cOutFilePath = cPath
      This%HydInfo(iHR_GWHEAD)%iNHyd = iNOUTH
      This%HydInfo(iHR_GWHEAD)%lActive = .TRUE.
    END IF

    IF (iNOUTH > 0) THEN
      ALLOCATE(This%HydInfo(iHR_GWHEAD)%cHydIDs(iNOUTH), &
               This%HydInfo(iHR_GWHEAD)%iLayers(iNOUTH))

      DO i = 1, iNOUTH
        CALL GWFile%ReadData(cLine, iStat)
        IF (iStat == -1) EXIT
        ! Parse: ID HYDTYP IOUTHL X Y NAME  or  ID HYDTYP IOUTHL IOUTH NAME
        READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL
        IF (iErr /= 0) THEN
          iOutHL = 1
          iHydTyp = 0
        END IF

        ! Extract the name (last token on the line)
        cName = ' '
        IF (iHydTyp == 0) THEN
          ! Format: ID HYDTYP IOUTHL X Y NAME
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, rX, rY, cName
        ELSE
          ! Format: ID HYDTYP IOUTHL IOUTH NAME
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, iID, cName
        END IF
        IF (iErr /= 0) THEN
          WRITE(cName, '(A,I0)') 'HYD', i
        END IF

        This%HydInfo(iHR_GWHEAD)%cHydIDs(i) = ADJUSTL(TRIM(cName))
        This%HydInfo(iHR_GWHEAD)%iLayers(i) = iOutHL
      END DO
    END IF

    CALL GWFile%Kill()

    CALL ModuleLogger%LogMessage('  GW: '//TRIM(IntToText(iNOUTH))//' hydrographs, .out='// &
         TRIM(This%HydInfo(iHR_GWHEAD)%cOutFilePath), f_iInfo, cModName)

  END SUBROUTINE ParseGWMainFile

  ! =====================================================================
  ! ParseStreamMainFile - Extract stream hydrograph info
  ! =====================================================================
  SUBROUTINE ParseStreamMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    TYPE(GenericFileType) :: StrFile
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500) :: cStrDir, cPath
    CHARACTER(:), ALLOCATABLE :: cAbsPath
    INTEGER :: iErr, i, iPos, iNOUTR
    CHARACTER(LEN=25) :: cName
    INTEGER :: iID

    iStat = 0

    IF (LEN_TRIM(This%cStreamMainFile) == 0) RETURN

    cStrDir = This%cStreamMainFile
    iPos = MAX(SCAN(cStrDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cStrDir = cStrDir(1:iPos)
    ELSE
      cStrDir = This%cWorkDir
    END IF

    CALL StrFile%New(FileName=This%cStreamMainFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                     Descriptor='stream main file', iStat=iStat)
    IF (iStat == -1) THEN
      CALL ModuleLogger%LogMessage('  Stream main file not found: '// &
           TRIM(This%cStreamMainFile), f_iInfo, cModName)
      iStat = 0; RETURN
    END IF

    ! Read and discard version line (#4.2 etc.) — not auto-skipped by GenericFileType
    CALL StrFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL StrFile%Kill(); iStat = 0; RETURN; END IF

    ! Skip to NOUTR (7 data lines after version)
    DO i = 1, 7
      CALL StrFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL StrFile%Kill(); iStat = 0; RETURN
      END IF
    END DO

    ! Current line has NOUTR
    CALL StripAndClean(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNOUTR
    IF (iErr /= 0 .OR. iNOUTR <= 0) THEN
      CALL StrFile%Kill(); RETURN
    END IF

    ! Skip 5 lines, then read output file name
    DO i = 1, 6
      CALL StrFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL StrFile%Kill(); iStat = 0; RETURN
      END IF
    END DO

    ! Current line is the stream hydrograph output file path
    ! Resolve relative to simulation directory (IWFM CWD)
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPath)
      IF (ALLOCATED(cAbsPath)) THEN
        cPath = cAbsPath
      ELSE
        cPath = TRIM(cClean)
      END IF
      This%HydInfo(iHR_STREAM)%cOutFilePath = cPath
      This%HydInfo(iHR_STREAM)%iNHyd = iNOUTR
      This%HydInfo(iHR_STREAM)%lActive = .TRUE.
    END IF

    IF (iNOUTR > 0) THEN
      ALLOCATE(This%HydInfo(iHR_STREAM)%cHydIDs(iNOUTR))

      DO i = 1, iNOUTR
        CALL StrFile%ReadData(cLine, iStat)
        IF (iStat == -1) EXIT
        ! Stream format: ID NAME NODE or ID HYDTYP NAME
        ! Extract name (second token for streams)
        cName = ' '
        READ(cLine, *, IOSTAT=iErr) iID, cName
        IF (iErr /= 0) THEN
          WRITE(cName, '(A,I0)') 'STR', i
        END IF
        This%HydInfo(iHR_STREAM)%cHydIDs(i) = ADJUSTL(TRIM(cName))
      END DO
    END IF

    CALL StrFile%Kill()

  END SUBROUTINE ParseStreamMainFile

  ! =====================================================================
  ! ParseTileDrainMainFile - Extract tile drain hydrograph info
  ! =====================================================================
  SUBROUTINE ParseTileDrainMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    TYPE(GenericFileType) :: TDFile
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500) :: cTDDir, cPath
    CHARACTER(:), ALLOCATABLE :: cAbsPath
    INTEGER :: iErr, i, iPos, iNTD, iNSI
    CHARACTER(LEN=25) :: cName
    INTEGER :: iID

    iStat = 0

    cTDDir = This%cTileDrainFile
    iPos = MAX(SCAN(cTDDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cTDDir = cTDDir(1:iPos)
    ELSE
      cTDDir = This%cWorkDir
    END IF

    CALL TDFile%New(FileName=This%cTileDrainFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                    Descriptor='tile drain main file', iStat=iStat)
    IF (iStat == -1) THEN; iStat = 0; RETURN; END IF

    ! Read and discard version line (#4.0) — not auto-skipped by GenericFileType
    CALL TDFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL TDFile%Kill(); iStat = 0; RETURN; END IF

    ! First data line has NTD
    CALL TDFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL TDFile%Kill(); iStat = 0; RETURN; END IF
    CALL StripAndClean(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNTD
    IF (iErr /= 0 .OR. iNTD <= 0) THEN
      CALL TDFile%Kill(); RETURN
    END IF

    ! Skip 3+NTD lines, then read NSI
    DO i = 1, iNTD + 4
      CALL TDFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL TDFile%Kill(); iStat = 0; RETURN
      END IF
    END DO

    CALL StripAndClean(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNSI
    IF (iErr /= 0) THEN
      CALL TDFile%Kill(); RETURN
    END IF

    ! Skip 6+NSI lines to get hydrograph file name
    DO i = 1, iNSI + 7
      CALL TDFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL TDFile%Kill(); iStat = 0; RETURN
      END IF
    END DO

    ! Current line is tile drain hydrograph output file path
    ! Resolve relative to simulation directory (IWFM CWD)
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPath)
      IF (ALLOCATED(cAbsPath)) THEN
        cPath = cAbsPath
      ELSE
        cPath = TRIM(cClean)
      END IF
      This%HydInfo(iHR_TILEDR)%cOutFilePath = cPath
      This%HydInfo(iHR_TILEDR)%iNHyd = iNTD
      This%HydInfo(iHR_TILEDR)%lActive = .TRUE.
    END IF

    IF (iNTD > 0 .AND. This%HydInfo(iHR_TILEDR)%lActive) THEN
      ALLOCATE(This%HydInfo(iHR_TILEDR)%cHydIDs(iNTD))
      DO i = 1, iNTD
        CALL TDFile%ReadData(cLine, iStat)
        IF (iStat == -1) EXIT
        cName = ' '
        READ(cLine, *, IOSTAT=iErr) iID, cName
        IF (iErr /= 0) WRITE(cName, '(A,I0)') 'TD', i
        This%HydInfo(iHR_TILEDR)%cHydIDs(i) = ADJUSTL(TRIM(cName))
      END DO
    END IF

    CALL TDFile%Kill()

  END SUBROUTINE ParseTileDrainMainFile

  ! =====================================================================
  ! ParseSubsidenceMainFile - Extract subsidence hydrograph info
  ! =====================================================================
  SUBROUTINE ParseSubsidenceMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    TYPE(GenericFileType) :: SBFile
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500) :: cSBDir, cPath
    CHARACTER(:), ALLOCATABLE :: cAbsPath
    INTEGER :: iErr, i, iPos, iNOUTS
    INTEGER :: iID, iHydTyp, iOutHL
    REAL(8) :: rX, rY
    CHARACTER(LEN=25) :: cName

    iStat = 0

    cSBDir = This%cSubsidenceFile
    iPos = MAX(SCAN(cSBDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cSBDir = cSBDir(1:iPos)
    ELSE
      cSBDir = This%cWorkDir
    END IF

    CALL SBFile%New(FileName=This%cSubsidenceFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                    Descriptor='subsidence main file', iStat=iStat)
    IF (iStat == -1) THEN; iStat = 0; RETURN; END IF

    ! Read and discard version line (#4.1 or #5.1)
    CALL SBFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL SBFile%Kill(); iStat = 0; RETURN; END IF

    ! Skip 5 lines, then read NOUTS (6 reads total including the version discard above,
    ! but version was already read, so 6 more data lines to reach NOUTS)
    DO i = 1, 6
      CALL SBFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL SBFile%Kill(); iStat = 0; RETURN
      END IF
    END DO

    CALL StripAndClean(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNOUTS
    IF (iErr /= 0 .OR. iNOUTS <= 0) THEN
      CALL SBFile%Kill(); RETURN
    END IF

    ! FACTXY
    CALL SBFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL SBFile%Kill(); iStat = 0; RETURN; END IF

    ! Read subsidence hydrograph output file path
    ! Resolve relative to simulation directory (IWFM CWD)
    CALL SBFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL SBFile%Kill(); iStat = 0; RETURN; END IF
    CALL StripAndClean(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPath)
      IF (ALLOCATED(cAbsPath)) THEN
        cPath = cAbsPath
      ELSE
        cPath = TRIM(cClean)
      END IF
      This%HydInfo(iHR_SUBSID)%cOutFilePath = cPath
      This%HydInfo(iHR_SUBSID)%iNHyd = iNOUTS
      This%HydInfo(iHR_SUBSID)%lActive = .TRUE.
    END IF

    IF (iNOUTS > 0 .AND. This%HydInfo(iHR_SUBSID)%lActive) THEN
      ALLOCATE(This%HydInfo(iHR_SUBSID)%cHydIDs(iNOUTS), &
               This%HydInfo(iHR_SUBSID)%iLayers(iNOUTS))

      DO i = 1, iNOUTS
        CALL SBFile%ReadData(cLine, iStat)
        IF (iStat == -1) EXIT
        READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL
        IF (iErr /= 0) THEN
          iOutHL = 1
          iHydTyp = 0
        END IF
        cName = ' '
        IF (iHydTyp == 0) THEN
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, rX, rY, cName
        ELSE
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, iID, cName
        END IF
        IF (iErr /= 0) WRITE(cName, '(A,I0)') 'SUB', i
        This%HydInfo(iHR_SUBSID)%cHydIDs(i) = ADJUSTL(TRIM(cName))
        This%HydInfo(iHR_SUBSID)%iLayers(i) = iOutHL
      END DO
    END IF

    CALL SBFile%Kill()

  END SUBROUTINE ParseSubsidenceMainFile

  ! =====================================================================
  ! ReadDotOutFile - Read hydrograph .out file and write temp SMP
  !
  !   Reads IWFM hydrograph output in the appropriate format:
  !     GW:         READ(iUnit, '(A22,60000F12.4)') ...
  !     Stream:     READ(iUnit, '(A22,60000F14.2)') ...
  !     Subsid/TD:  READ(iUnit, '(A22,60000F12.2)') ...
  !   Skips *-prefixed header rows.
  !   Writes output as temp SMP file for SMP2SMP consumption.
  !
  !   Adapted from old iwfm2obs.f90 lines 510-588
  ! =====================================================================
  SUBROUTINE ReadHydrographToSMP(This, iHydType, cTempSMPFile, iStat)
    CLASS(HydrographReaderType), INTENT(IN)  :: This
    INTEGER,                     INTENT(IN)  :: iHydType
    CHARACTER(LEN=*),            INTENT(IN)  :: cTempSMPFile
    INTEGER,                     INTENT(OUT) :: iStat

    INTEGER, PARAMETER :: iInUnit = 198, iOutUnit = 199
    INTEGER, PARAMETER :: MAXHYD = 60000
    CHARACTER(LEN=21) :: cTimestamp
    REAL(4), ALLOCATABLE :: rVal(:)
    INTEGER :: iErr, iNHyd, iTime, j, k, iPos
    INTEGER :: iDay, iMon, iYr
    CHARACTER(LEN=1000) :: cLine

    iStat = 0

    IF (.NOT. This%HydInfo(iHydType)%lActive) RETURN
    IF (This%HydInfo(iHydType)%iNHyd <= 0) RETURN

    iNHyd = This%HydInfo(iHydType)%iNHyd
    ALLOCATE(rVal(iNHyd), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot allocate array for '//TRIM(IntToText(iNHyd))// &
           ' hydrographs', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Open the .out file
    OPEN(UNIT=iInUnit, FILE=This%HydInfo(iHydType)%cOutFilePath, &
         STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot open .out file: '// &
           TRIM(This%HydInfo(iHydType)%cOutFilePath), f_iFatal, cModName)
      DEALLOCATE(rVal)
      iStat = -1; RETURN
    END IF

    ! Skip *-prefixed header lines
    DO
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) THEN
        CLOSE(iInUnit); DEALLOCATE(rVal); iStat = -1; RETURN
      END IF
      cLine = ADJUSTL(cLine)
      IF (cLine(1:1) /= '*') THEN
        BACKSPACE(iInUnit)
        EXIT
      END IF
    END DO

    ! Open temp SMP output
    OPEN(UNIT=iOutUnit, FILE=cTempSMPFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CLOSE(iInUnit); DEALLOCATE(rVal)
      CALL ModuleLogger%SetLastMessage('Cannot create temp SMP: '//TRIM(cTempSMPFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Read data lines and compute dates
    iTime = 0
    DO
      iTime = iTime + 1
      rVal = 0.0

      ! Read: first 21 chars = timestamp, rest = values
      SELECT CASE (iHydType)
      CASE (iHR_GWHEAD)
        READ(iInUnit, '(A21,1X,60000F12.4)', IOSTAT=iErr) cTimestamp, (rVal(j), j=1,iNHyd)
      CASE (iHR_STREAM)
        READ(iInUnit, '(A21,1X,60000F14.2)', IOSTAT=iErr) cTimestamp, (rVal(j), j=1,iNHyd)
      CASE DEFAULT  ! SUBSID, TILEDR
        READ(iInUnit, '(A21,1X,60000F12.2)', IOSTAT=iErr) cTimestamp, (rVal(j), j=1,iNHyd)
      END SELECT

      IF (iErr /= 0) EXIT

      ! Parse date directly from timestamp (MM/DD/YYYY_HH:MM)
      cLine = ADJUSTL(TRIM(cTimestamp))
      iPos = SCAN(cLine, '_')
      IF (iPos > 0) cLine = cLine(1:iPos-1)
      CALL ParseDateFromString(cLine, This%iDateSpec, iDay, iMon, iYr, iErr)
      IF (iErr /= 0) THEN
        CALL ComputeDateJulian(This%cTimeUnit, iTime, This%iStartDay, This%iStartMon, &
             This%iStartYr, iDay, iMon, iYr, iErr)
      END IF

      ! Write SMP records for each hydrograph
      DO j = 1, iNHyd
        IF (ALLOCATED(This%HydInfo(iHydType)%cHydIDs)) THEN
          IF (This%iDateSpec == 1) THEN
            WRITE(iOutUnit, 850) TRIM(This%HydInfo(iHydType)%cHydIDs(j)), &
                 iDay, iMon, iYr, DBLE(rVal(j))
          ELSE
            WRITE(iOutUnit, 850) TRIM(This%HydInfo(iHydType)%cHydIDs(j)), &
                 iMon, iDay, iYr, DBLE(rVal(j))
          END IF
        END IF
      END DO
    END DO

850 FORMAT(1X,A,10X,I2.2,'/',I2.2,'/',I4.4,'   00:00:00 ',1PG14.7)

    CLOSE(iInUnit)
    CLOSE(iOutUnit)
    DEALLOCATE(rVal)

    iTime = iTime - 1  ! Subtract failed read
    CALL ModuleLogger%LogMessage('  Read '//TRIM(IntToText(iTime))//' timesteps from '// &
         TRIM(This%HydInfo(iHydType)%cOutFilePath), f_iInfo, cModName)

  END SUBROUTINE ReadHydrographToSMP

  ! =====================================================================
  ! ReadDotOutFileDirect - Read .out file directly into memory
  !   Phase A optimization: only stores columns matching observation IDs.
  !   Eliminates the 2+ GB temp SMP file entirely.
  !
  !   On return, This%rModelData, This%iModelDays, This%iModelSecs,
  !   This%cFilteredIDs, This%iNTimes, This%iNFiltered are populated.
  ! =====================================================================
  SUBROUTINE ReadHydrographData(This, iHydType, cObsIDs, iNObsIDs, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(IN)    :: iHydType
    CHARACTER(LEN=25),           INTENT(IN)    :: cObsIDs(:)
    INTEGER,                     INTENT(IN)    :: iNObsIDs
    INTEGER,                     INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iInUnit = 198
    INTEGER, PARAMETER :: iWSUnit = 199
    REAL(4), ALLOCATABLE :: rVal(:)
    INTEGER, ALLOCATABLE :: iColMap(:)
    CHARACTER(LEN=25), ALLOCATABLE :: cObsSorted(:)
    INTEGER, ALLOCATABLE :: iObsOrder(:)
    CHARACTER(LEN=25) :: cID
    CHARACTER(LEN=120) :: cJunk
    CHARACTER(LEN=1000) :: cLine
    INTEGER :: iErr, iNHyd, j, k, iTime, iNTimes, iPos
    INTEGER :: iDay, iMon, iYr, iJulian
    ! ResultsExtract fallback variables
    INTEGER :: iOutFileNHyd, iNWells, iL
    CHARACTER(LEN=25), ALLOCATABLE :: cWellSpecIDs(:)
    LOGICAL :: lRebuildColMap
    ! HDF5/DSS support
    INTEGER :: iFileType
    TYPE(RealTSDataInFileType) :: HydInFile
    LOGICAL :: lHDF, lFileExists
    CHARACTER(LEN=500) :: cReadPath

    iStat = 0
    This%iNTimes    = 0
    This%iNFiltered = 0

    IF (.NOT. This%HydInfo(iHydType)%lActive) RETURN
    IF (This%HydInfo(iHydType)%iNHyd <= 0) RETURN

    iNHyd = This%HydInfo(iHydType)%iNHyd

    ! Deallocate any previous data
    IF (ALLOCATED(This%rModelData))   DEALLOCATE(This%rModelData)
    IF (ALLOCATED(This%iModelDays))   DEALLOCATE(This%iModelDays)
    IF (ALLOCATED(This%iModelSecs))   DEALLOCATE(This%iModelSecs)
    IF (ALLOCATED(This%cFilteredIDs)) DEALLOCATE(This%cFilteredIDs)

    ! ---- Detect file format from extension ----
    ! Only reads the file IWFM or ResultsExtract actually produced.
    ! Does NOT auto-detect pyiwfm cache files (.hydrograph_cache.hdf).
    cReadPath = This%HydInfo(iHydType)%cOutFilePath
    iFileType = iGetFileType_FromName(cReadPath)
    lHDF = (iFileType == f_iHDF)

    ! Verify file exists
    INQUIRE(FILE=TRIM(cReadPath), EXIST=lFileExists)
    IF (.NOT. lFileExists) THEN
      CALL ModuleLogger%SetLastMessage('Hydrograph file not found: '//TRIM(cReadPath), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! ---- Count timesteps ----
    IF (lHDF) THEN
      ! For HDF5: open file via RealTSDataInFileType (1D, nCol=NHyd)
      ! Same Init overload as kernel's PrepHydInFile_ForInquiry
      CALL HydInFile%Init(cReadPath, 'hydrograph data', BlocksToSkip=0, &
           nCol=iNHyd, iStat=iStat)
      IF (iStat == -1) THEN
        CALL ModuleLogger%SetLastMessage('Cannot open HDF5 hydrograph file: '//TRIM(cReadPath)// &
             '. Ensure it was created by IWFM or ResultsExtract (not pyiwfm cache).', &
             f_iFatal, cModName)
        RETURN
      END IF
      IF (HydInFile%iSize /= iNHyd) THEN
        CALL ModuleLogger%SetLastMessage('HDF5 column count ('//TRIM(IntToText(HydInFile%iSize))// &
             ') does not match expected hydrograph count ('//TRIM(IntToText(iNHyd))// &
             ') in file: '//TRIM(cReadPath), f_iFatal, cModName)
        CALL HydInFile%Close()
        iStat = -1; RETURN
      END IF
      iNTimes = 0  ! Will be set during read
    END IF
    IF (.NOT. lHDF) THEN
      ! For text: count data lines (skip *-header lines)
      OPEN(UNIT=iInUnit, FILE=TRIM(cReadPath), &
           STATUS='OLD', RECL=2000000, IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL ModuleLogger%SetLastMessage('Cannot open hydrograph file: '// &
             TRIM(cReadPath), f_iFatal, cModName)
        iStat = -1; RETURN
      END IF

      iNTimes = 0
      DO
        READ(iInUnit, '(A)', IOSTAT=iErr) cLine
        IF (iErr /= 0) EXIT
        cLine = ADJUSTL(cLine)
        IF (LEN_TRIM(cLine) == 0) CYCLE
        IF (cLine(1:1) == '*') CYCLE
        iNTimes = iNTimes + 1
      END DO
      REWIND(iInUnit)
    END IF

    ! ---- Check if well_specs.dat provides a different column mapping ----
    ! Only applies to GW heads — ResultsExtract may produce .out files with
    ! well_specs-based columns instead of the GW main file's hydrograph list.
    lRebuildColMap = .FALSE.
    iOutFileNHyd = 0
    IF (iHydType == iHR_GWHEAD .AND. LEN_TRIM(This%cWellSpecFile) > 0) THEN
      ! Count wells in well_specs.dat
      iNWells = 0
      OPEN(UNIT=iWSUnit, FILE=This%cWellSpecFile, STATUS='OLD', IOSTAT=iErr)
      IF (iErr == 0) THEN
        READ(iWSUnit, '(A)', IOSTAT=iErr) cLine  ! skip header
        DO
          READ(iWSUnit, '(A)', IOSTAT=iErr) cLine
          IF (iErr /= 0) EXIT
          IF (LEN_TRIM(cLine) > 0) iNWells = iNWells + 1
        END DO
        CLOSE(iWSUnit)
        iOutFileNHyd = iNWells * This%iNLayers
      END IF
    END IF

    IF (iOutFileNHyd > 0 .AND. iOutFileNHyd /= iNHyd) THEN
      CALL ModuleLogger%LogMessage('  well_specs count ('// &
           TRIM(IntToText(iOutFileNHyd))//') differs from GW main ('// &
           TRIM(IntToText(iNHyd))//'). Building well_specs-based mapping.', &
           f_iInfo, cModName)

      IF (LEN_TRIM(This%cWellSpecFile) > 0) THEN
        iNWells = 0
        ! Count wells in well_specs.dat (header + data lines)
        OPEN(UNIT=iWSUnit, FILE=This%cWellSpecFile, STATUS='OLD', IOSTAT=iErr)
        IF (iErr == 0) THEN
          READ(iWSUnit, '(A)', IOSTAT=iErr) cLine  ! skip header
          DO
            READ(iWSUnit, '(A)', IOSTAT=iErr) cLine
            IF (iErr /= 0) EXIT
            IF (LEN_TRIM(cLine) > 0) iNWells = iNWells + 1
          END DO
          REWIND(iWSUnit)

          IF (iNWells * This%iNLayers == iOutFileNHyd) THEN
            ! Well count × layers matches .out column count — build new HydIDs
            iNHyd = iOutFileNHyd
            This%HydInfo(iHydType)%iNHyd = iNHyd

            IF (ALLOCATED(This%HydInfo(iHydType)%cHydIDs)) &
                DEALLOCATE(This%HydInfo(iHydType)%cHydIDs)
            IF (ALLOCATED(This%HydInfo(iHydType)%iLayers)) &
                DEALLOCATE(This%HydInfo(iHydType)%iLayers)
            ALLOCATE(This%HydInfo(iHydType)%cHydIDs(iNHyd), &
                     This%HydInfo(iHydType)%iLayers(iNHyd), STAT=iErr)

            READ(iWSUnit, '(A)', IOSTAT=iErr) cLine  ! skip header again
            j = 0
            DO
              READ(iWSUnit, '(A)', IOSTAT=iErr) cLine
              IF (iErr /= 0) EXIT
              IF (LEN_TRIM(cLine) == 0) CYCLE
              ! Parse first field (well name) — tab-delimited
              cID = ADJUSTL(cLine(1:MIN(25, INDEX(cLine, CHAR(9))-1)))
              IF (LEN_TRIM(cID) == 0) cID = ADJUSTL(cLine(1:25))
              ! Generate NAME%1, NAME%2, ..., NAME%N_LAYERS
              DO iL = 1, This%iNLayers
                j = j + 1
                IF (j > iNHyd) EXIT
                WRITE(This%HydInfo(iHydType)%cHydIDs(j), '(A,A1,I1)') &
                      TRIM(cID), '%', iL
                This%HydInfo(iHydType)%iLayers(j) = iL
              END DO
            END DO

            lRebuildColMap = .TRUE.
            CALL ModuleLogger%LogMessage('  Built '//TRIM(IntToText(iNHyd))// &
                 ' hydrograph IDs from well_specs ('// &
                 TRIM(IntToText(iNWells))//' wells x '// &
                 TRIM(IntToText(This%iNLayers))//' layers)', f_iInfo, cModName)
          ELSE
            CALL ModuleLogger%LogMessage('  well_specs wells ('//TRIM(IntToText(iNWells))// &
                 ') x layers ('//TRIM(IntToText(This%iNLayers))// &
                 ') = '//TRIM(IntToText(iNWells*This%iNLayers))// &
                 ' does not match .out columns ('// &
                 TRIM(IntToText(iOutFileNHyd))//')', f_iWarn, cModName)
          END IF

          CLOSE(iWSUnit)
        ELSE
          CALL ModuleLogger%LogMessage('  Cannot open well_specs file: '// &
               TRIM(This%cWellSpecFile), f_iWarn, cModName)
        END IF
      ELSE
        CALL ModuleLogger%LogMessage('  No well_specs file available for fallback', &
             f_iWarn, cModName)
      END IF
    END IF

    ! ---- Step 1: Build sorted obs ID list for binary search ----
    ALLOCATE(cObsSorted(iNObsIDs), iObsOrder(iNObsIDs), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot allocate obs ID sort arrays', f_iFatal, cModName)
      CLOSE(iInUnit)
      iStat = -1; RETURN
    END IF
    DO j = 1, iNObsIDs
      cObsSorted(j) = UpperCase(ADJUSTL(cObsIDs(j)))
      iObsOrder(j) = j
    END DO
    IF (iNObsIDs > 1) CALL SortStringsIndex(cObsSorted, iObsOrder, 1, iNObsIDs)

    ! ---- Step 2: Map model columns to filtered indices ----
    ALLOCATE(iColMap(iNHyd), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot allocate column map', f_iFatal, cModName)
      DEALLOCATE(cObsSorted, iObsOrder)
      CLOSE(iInUnit)
      iStat = -1; RETURN
    END IF
    iColMap = 0
    This%iNFiltered = 0
    DO j = 1, iNHyd
      IF (.NOT. ALLOCATED(This%HydInfo(iHydType)%cHydIDs)) EXIT
      cID = UpperCase(ADJUSTL(TRIM(This%HydInfo(iHydType)%cHydIDs(j))))
      k = BinarySearchStr(cObsSorted, iNObsIDs, cID)
      IF (k > 0) THEN
        This%iNFiltered = This%iNFiltered + 1
        iColMap(j) = This%iNFiltered
      END IF
    END DO

    DEALLOCATE(cObsSorted, iObsOrder)

    IF (This%iNFiltered == 0) THEN
      CALL ModuleLogger%LogMessage('  No matching IDs between model and obs for direct read', &
           f_iWarn, cModName)
      DEALLOCATE(iColMap)
      CLOSE(iInUnit)
      RETURN
    END IF

    ! Build filtered ID list
    ALLOCATE(This%cFilteredIDs(This%iNFiltered), STAT=iErr)
    DO j = 1, iNHyd
      IF (iColMap(j) > 0) THEN
        This%cFilteredIDs(iColMap(j)) = &
             UpperCase(ADJUSTL(TRIM(This%HydInfo(iHydType)%cHydIDs(j))))
      END IF
    END DO

    CALL ModuleLogger%LogMessage('  Matched '//TRIM(IntToText(This%iNFiltered))//' of '// &
         TRIM(IntToText(iNHyd))//' model hydrographs to observation IDs', &
         f_iInfo, cModName)

    This%iNTimes = iNTimes

    ! ---- Step 4: Allocate model data arrays ----
    ALLOCATE(rVal(iNHyd), STAT=iErr)
    IF (iErr /= 0) THEN
      IF (.NOT. lHDF) CLOSE(iInUnit)
      DEALLOCATE(iColMap)
      CALL ModuleLogger%SetLastMessage('Cannot allocate value array for '// &
           TRIM(IntToText(iNHyd))//' hydrographs', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    IF (lHDF) THEN
      ! ---- HDF5 reading path ----
      ! Read all timesteps from HDF5 via RealTSDataInFileType (same as kernel inquiry)
      ! First, estimate nTimes from HDF5 metadata — read until EOF
      BLOCK
        USE TimeSeriesUtilities, ONLY: TimeStepType, NPeriods, &
             IncrementTimeStamp, CTimeStep_To_RTimeStep
        TYPE(TimeStepType) :: TSLocal
        INTEGER :: iFileReadError, iMaxTimes, iNTimeSteps

        ! Get time info from the HDF5 file (stored as attributes)
        CALL HydInFile%File%GetTimeStepRelatedData(iNTimeSteps, TSLocal)

        ! Use discovered timestep count or large estimate
        IF (iNTimeSteps > 0) THEN
          iMaxTimes = iNTimeSteps
        ELSE
          iMaxTimes = 100000
        END IF

        ALLOCATE(This%rModelData(iMaxTimes, This%iNFiltered), &
                 This%iModelDays(iMaxTimes), &
                 This%iModelSecs(iMaxTimes), STAT=iErr)
        IF (iErr /= 0) THEN
          CALL HydInFile%Close()
          DEALLOCATE(iColMap, rVal)
          CALL ModuleLogger%SetLastMessage('Cannot allocate HDF5 model data arrays', f_iFatal, cModName)
          iStat = -1; RETURN
        END IF
        This%iModelSecs = 0

        iTime = 0
        DO
          CALL HydInFile%ReadTSData(TSLocal, 'hydrograph data', iFileReadError, iStat)
          IF (iStat == -1) EXIT
          IF (iFileReadError /= 0) EXIT

          iTime = iTime + 1
          IF (iTime > iMaxTimes) EXIT

          ! Get date from HDF5 timestep info
          CALL ParseDateFromString(TSLocal%CurrentDateAndTime(1:10), &
               This%iDateSpec, iDay, iMon, iYr, iErr)
          IF (iErr == 0) THEN
            CALL DayMonthYearToJulianDate(iDay, iMon, iYr, iJulian, iErr)
            This%iModelDays(iTime) = iJulian
          ELSE
            ! Fallback: compute date from timestep index
            CALL ComputeDateJulian(This%cTimeUnit, iTime, This%iStartDay, This%iStartMon, &
                 This%iStartYr, iDay, iMon, iYr, iErr)
            CALL DayMonthYearToJulianDate(iDay, iMon, iYr, iJulian, iErr)
            This%iModelDays(iTime) = iJulian
          END IF

          ! Copy matching columns: HydInFile%rValues(j) is 1D array
          DO j = 1, iNHyd
            IF (iColMap(j) > 0) THEN
              This%rModelData(iTime, iColMap(j)) = HydInFile%rValues(j)
            END IF
          END DO

          ! Advance timestep
          TSLocal%CurrentDateAndTime = IncrementTimeStamp(TSLocal%CurrentDateAndTime, &
                                           TSLocal%DeltaT_InMinutes)
          TSLocal%CurrentTimeStep = TSLocal%CurrentTimeStep + 1
        END DO

        This%iNTimes = iTime
        CALL HydInFile%Close()
      END BLOCK

    ELSE
      ! ---- Text reading path (improved: free-format for robustness) ----
      ALLOCATE(This%rModelData(iNTimes, This%iNFiltered), &
               This%iModelDays(iNTimes), &
               This%iModelSecs(iNTimes), STAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iInUnit); DEALLOCATE(iColMap, rVal)
        CALL ModuleLogger%SetLastMessage('Cannot allocate model data ('// &
             TRIM(IntToText(iNTimes))//' x '// &
             TRIM(IntToText(This%iNFiltered))//')', f_iFatal, cModName)
        iStat = -1; RETURN
      END IF
      This%iModelSecs = 0

      ! Skip header lines (*-prefixed)
      DO
        READ(iInUnit, '(A)', IOSTAT=iErr) cLine
        IF (iErr /= 0) EXIT
        cLine = ADJUSTL(cLine)
        IF (cLine(1:1) /= '*') THEN
          BACKSPACE(iInUnit)
          EXIT
        END IF
      END DO

      iTime = 0
      DO
        ! Read: first 21 chars = timestamp (MM/DD/YYYY_HH:MM), rest = values
        rVal = 0.0
        SELECT CASE (iHydType)
        CASE (iHR_GWHEAD)
          READ(iInUnit, '(A21,1X,100000F12.4)', IOSTAT=iErr) cJunk, (rVal(j), j=1,iNHyd)
        CASE (iHR_STREAM)
          READ(iInUnit, '(A21,1X,100000F14.2)', IOSTAT=iErr) cJunk, (rVal(j), j=1,iNHyd)
        CASE DEFAULT
          READ(iInUnit, '(A21,1X,100000F12.2)', IOSTAT=iErr) cJunk, (rVal(j), j=1,iNHyd)
        END SELECT

        IF (iErr /= 0) EXIT
        iTime = iTime + 1
        IF (iTime > iNTimes) EXIT

        ! Parse date directly from the timestamp in the data line
        ! Format: "MM/DD/YYYY_HH:MM     " — extract date before '_'
        cLine = ADJUSTL(TRIM(cJunk))
        iPos = SCAN(cLine, '_')
        IF (iPos > 0) cLine = cLine(1:iPos-1)
        CALL ParseDateFromString(cLine, This%iDateSpec, iDay, iMon, iYr, iErr)
        IF (iErr == 0) THEN
          CALL DayMonthYearToJulianDate(iDay, iMon, iYr, iJulian, iErr)
          This%iModelDays(iTime) = iJulian
        ELSE
          ! Fallback: compute from timestep index (less accurate)
          CALL ComputeDateJulian(This%cTimeUnit, iTime, This%iStartDay, This%iStartMon, &
               This%iStartYr, iDay, iMon, iYr, iErr)
          CALL DayMonthYearToJulianDate(iDay, iMon, iYr, iJulian, iErr)
          This%iModelDays(iTime) = iJulian
        END IF
        This%iModelSecs(iTime) = 0

        ! Copy only matching columns to in-memory array
        DO j = 1, iNHyd
          IF (iColMap(j) > 0) THEN
            This%rModelData(iTime, iColMap(j)) = DBLE(rVal(j))
          END IF
        END DO
      END DO

      This%iNTimes = iTime
      CLOSE(iInUnit)
    END IF

    DEALLOCATE(rVal, iColMap)

    CALL ModuleLogger%LogMessage('  Direct read: '//TRIM(IntToText(This%iNTimes))// &
         ' timesteps x '//TRIM(IntToText(This%iNFiltered))// &
         ' hydrographs loaded to memory', f_iInfo, cModName)

    ! Log model date range from parsed timestamps
    IF (This%iNTimes > 0) THEN
      BLOCK
        INTEGER :: iD1, iM1, iY1, iD2, iM2, iY2, iE
        CHARACTER(LEN=10) :: cStart, cEnd
        CALL JulianDateToDayMonthYear(This%iModelDays(1), iD1, iM1, iY1, iE)
        CALL JulianDateToDayMonthYear(This%iModelDays(This%iNTimes), iD2, iM2, iY2, iE)
        WRITE(cStart, '(I2.2,A1,I2.2,A1,I4.4)') iM1, '/', iD1, '/', iY1
        WRITE(cEnd,   '(I2.2,A1,I2.2,A1,I4.4)') iM2, '/', iD2, '/', iY2
        CALL ModuleLogger%LogMessage('  Model period: '//cStart//' - '//cEnd, f_iInfo, cModName)
      END BLOCK
    END IF

  END SUBROUTINE ReadHydrographData

  ! =====================================================================
  ! Kill - Deallocate
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER :: i

    DO i = 1, iHR_NUMHYD
      IF (ALLOCATED(This%HydInfo(i)%cHydIDs)) DEALLOCATE(This%HydInfo(i)%cHydIDs)
      IF (ALLOCATED(This%HydInfo(i)%iLayers)) DEALLOCATE(This%HydInfo(i)%iLayers)
    END DO
    IF (ALLOCATED(This%rModelData))   DEALLOCATE(This%rModelData)
    IF (ALLOCATED(This%iModelDays))   DEALLOCATE(This%iModelDays)
    IF (ALLOCATED(This%iModelSecs))   DEALLOCATE(This%iModelSecs)
    IF (ALLOCATED(This%cFilteredIDs)) DEALLOCATE(This%cFilteredIDs)
    This%iNTimes    = 0
    This%iNFiltered = 0
    This%lDiscovered = .FALSE.
  END SUBROUTINE Kill

END MODULE Class_HydrographReader
