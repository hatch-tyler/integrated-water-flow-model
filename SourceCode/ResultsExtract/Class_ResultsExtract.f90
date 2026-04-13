!***********************************************************************
!  ResultsExtract - Core Module
!
!  Generalized version of GWHydExtract that handles any all-node output
!  type (HEAD or SUBSIDENCE).  Reads the simulation's all-node output
!  file and generates hydrograph output at user-specified locations
!  using FE interpolation.  Output matches IWFM's native hydrograph
!  format for direct use by IWFM2OBS.
!
!  Auto-discovers PP binary, GW main file, and the appropriate
!  all-node output file from the simulation main file path.
!
!  Key differences from GWHydExtract:
!    - DATATYPE input line selects HEAD or SUBSIDENCE
!    - SUBSIDENCE uses layer summation (not averaging) for Layer=0
!    - Optional incremental mode (cumulative -> incremental) for SUBSIDENCE
!    - Auto-discovers AllSubsOut from subsidence data file
!    - Supports text, DSS, and HDF5 for both input and output
!***********************************************************************
MODULE Class_ResultsExtract

  USE MessageLogger        , ONLY: MessageLoggerType , &
                                    f_iFatal          , &
                                    f_iWarn           , &
                                    f_iInfo
  USE GeneralUtilities     , ONLY: IntToText                        , &
                                    UpperCase                        , &
                                    EstablishAbsolutePathFileName    , &
                                    ArrangeText                      , &
                                    PrepareTitle                     , &
                                    ConvertID_To_Index               , &
                                    StripTextUntilCharacter          , &
                                    CleanSpecialCharacters           , &
                                    f_cInlineCommentChar
  USE TimeSeriesUtilities  , ONLY: TimeStepType              , &
                                    f_iTimeStampLength        , &
                                    IncrementTimeStamp        , &
                                    NPeriods                  , &
                                    CTimeStep_To_RTimeStep
  USE IOInterface          , ONLY: GenericFileType            , &
                                    Real2DTSDataInFileType     , &
                                    PrepareTSDOutputFile       , &
                                    iGetFileType_FromName      , &
                                    f_iDSS                     , &
                                    f_iHDF
  USE Class_AppGrid        , ONLY: AppGridType
  USE Class_Stratigraphy   , ONLY: StratigraphyType
  USE Class_BaseHydrograph , ONLY: f_iHyd_AtXY     , &
                                    f_iHyd_AtNode    , &
                                    f_iHyd_GWHead    , &
                                    f_iHyd_Subsidence

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: ResultsExtractType
  PUBLIC :: ResultsExtract_SetModuleLogger

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_ResultsExtract'
  TYPE(MessageLoggerType),POINTER,PRIVATE :: ModuleLogger => NULL()

  ! Data type flags
  INTEGER, PARAMETER :: f_iDataType_HEAD        = 1
  INTEGER, PARAMETER :: f_iDataType_SUBSIDENCE  = 2

  ! =====================================================================
  ! Local hydrograph types
  !   (Kernel's HydAtNodeType/HydAtXYType have PRIVATE fields and are
  !   not exported, so ResultsExtract maintains compatible public types.)
  ! =====================================================================
  TYPE :: RE_HydAtNodeType
    INTEGER            :: ID       = 0
    INTEGER            :: iLayer   = 0
    INTEGER            :: iNode    = 0    ! Internal index (not user ID)
    INTEGER            :: iElement = 0    ! Element containing the node
    CHARACTER(LEN=30)  :: cName    = ' '
  END TYPE RE_HydAtNodeType

  TYPE :: RE_HydAtXYType
    INTEGER            :: ID       = 0
    INTEGER            :: iLayer   = 0
    INTEGER            :: iElement = 0    ! Element containing (X,Y)
    CHARACTER(LEN=30)  :: cName    = ' '
    REAL(8)            :: X        = 0d0
    REAL(8)            :: Y        = 0d0
    LOGICAL            :: lSkip    = .FALSE.  ! True if outside grid
    INTEGER, ALLOCATABLE :: iNodes(:)
    REAL(8), ALLOCATABLE :: rFactors(:)
  END TYPE RE_HydAtXYType

  TYPE :: RE_OrderedHydType
    INTEGER :: iHydType = 0   ! f_iHyd_AtXY or f_iHyd_AtNode
    INTEGER :: indx     = 0   ! Index into Hyd_AtNode or Hyd_AtXY array
  END TYPE RE_OrderedHydType

  ! =====================================================================
  ! ResultsExtractType - Main application type
  ! =====================================================================
  TYPE :: ResultsExtractType
    ! Data type
    INTEGER             :: iDataType     = f_iDataType_HEAD
    CHARACTER(LEN=20)   :: cDataType     = 'HEAD'
    LOGICAL             :: lIncremental  = .FALSE.
    ! Discovered paths
    CHARACTER(LEN=1000) :: cSimMainFile  = ' '
    CHARACTER(LEN=1000) :: cPPBinaryFile = ' '
    CHARACTER(LEN=1000) :: cGWMainFile   = ' '
    CHARACTER(LEN=1000) :: cAllDataFile  = ' '   ! All-heads or All-subsidence
    CHARACTER(LEN=1000) :: cOutputFile   = ' '
    CHARACTER(LEN=1000) :: cSimDir       = ' '
    CHARACTER(LEN=1000) :: cInputDir     = ' '
    ! Grid and stratigraphy (reuse kernel types)
    TYPE(AppGridType)      :: AppGrid
    TYPE(StratigraphyType) :: Stratigraphy
    ! Time stepping
    TYPE(TimeStepType)     :: TimeStep
    INTEGER                :: NTIME = 0
    ! Unit conversion from GW main file
    REAL(8)            :: rFactOutput = 1d0
    CHARACTER(LEN=30)  :: cUnitOutput = ' '
    ! Hydrograph specifications
    INTEGER :: NHyd = 0, NHyd_AtNode = 0, NHyd_AtXY = 0
    TYPE(RE_HydAtNodeType),  ALLOCATABLE :: Hyd_AtNode(:)
    TYPE(RE_HydAtXYType),    ALLOCATABLE :: Hyd_AtXY(:)
    TYPE(RE_OrderedHydType), ALLOCATABLE :: OrderedHydList(:)
    ! Output file
    TYPE(GenericFileType) :: OutFile
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Run
    PROCEDURE, PASS :: Kill
  END TYPE ResultsExtractType

CONTAINS


  ! -------------------------------------------------------------
  ! --- SET MODULE LOGGER
  ! -------------------------------------------------------------
  SUBROUTINE ResultsExtract_SetModuleLogger(Logger)
    TYPE(MessageLoggerType),TARGET,INTENT(IN) :: Logger

    ModuleLogger => Logger

  END SUBROUTINE ResultsExtract_SetModuleLogger


  ! =====================================================================
  ! New - Initialize: parse input, discover model files, load grid
  !
  ! Input file format:
  !   Line 1: Simulation main file path          (SIMFILE)
  !   Line 2: Data type (HEAD or SUBSIDENCE)      (DATATYPE)
  !   Line 3: Output file path                    (OUTFILE)
  !   Line 4: Number of hydrographs               (NHYD)
  !   Line 5: Coordinate conversion factor        (FACTXY)
  !   Lines 6+: Hydrograph specs
  !     ID  HYDTYP  LAYER  X/NODE  [Y]  NAME
  ! =====================================================================
  SUBROUTINE New(This, cInputFile, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),          INTENT(IN)    :: cInputFile
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=35), PARAMETER :: ThisProcedure = cModName // '::New'
    TYPE(GenericFileType) :: InFile
    CHARACTER(LEN=1000) :: cLine, cClean, cSimFile, cOutFile
    CHARACTER(LEN=1000) :: cInputDir
    CHARACTER(:), ALLOCATABLE :: cAbsPathFileName
    INTEGER :: iErr, iPos, iNHyd, i
    REAL(8) :: rFactXY
    ! Per-hydrograph parsing
    INTEGER :: iID, iHydTyp, iLayer, iNodeID
    REAL(8) :: rX, rY
    CHARACTER(LEN=30) :: cName
    INTEGER :: iAtNodeCount, iAtXYCount

    iStat = 0

    ! Determine input file directory
    cInputDir = cInputFile
    iPos = MAX(SCAN(cInputDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cInputDir = cInputDir(1:iPos)
    ELSE
      cInputDir = './'
    END IF
    This%cInputDir = cInputDir

    ! ================================================================
    ! Step 1: Parse ResultsExtract input file
    ! ================================================================
    CALL InFile%New(FileName=cInputFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                    Descriptor='ResultsExtract input', iStat=iStat)
    IF (iStat == -1) RETURN

    ! Line 1: Simulation main file path
    CALL InFile%ReadData(cLine, iStat)
    IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    cClean = ADJUSTL(TRIM(cClean))
    CALL EstablishAbsolutePathFileName(cClean, TRIM(cInputDir), cAbsPathFileName)
    cSimFile = cAbsPathFileName
    This%cSimMainFile = cSimFile

    ! Line 2: Data type (HEAD or SUBSIDENCE)
    CALL InFile%ReadData(cLine, iStat)
    IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    cClean = UpperCase(ADJUSTL(TRIM(cClean)))
    SELECT CASE (TRIM(cClean))
      CASE ('HEAD')
        This%iDataType    = f_iDataType_HEAD
        This%cDataType    = 'HEAD'
        This%lIncremental = .FALSE.
      CASE ('SUBSIDENCE')
        This%iDataType    = f_iDataType_SUBSIDENCE
        This%cDataType    = 'SUBSIDENCE'
        This%lIncremental = .TRUE.   ! Default: cumulative -> incremental
      CASE ('SUBSIDENCE_CUM', 'SUBSIDENCE_CUMULATIVE')
        This%iDataType    = f_iDataType_SUBSIDENCE
        This%cDataType    = 'SUBSIDENCE'
        This%lIncremental = .FALSE.  ! Keep cumulative
      CASE DEFAULT
        CALL ModuleLogger%SetLastMessage('Unknown DATATYPE: '//TRIM(cClean)// &
             '. Must be HEAD, SUBSIDENCE, or SUBSIDENCE_CUM.', &
             f_iFatal, ThisProcedure)
        iStat = -1; RETURN
    END SELECT

    ! Line 3: Output file path
    CALL InFile%ReadData(cLine, iStat)
    IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    cClean = ADJUSTL(TRIM(cClean))
    CALL EstablishAbsolutePathFileName(cClean, TRIM(cInputDir), cAbsPathFileName)
    cOutFile = cAbsPathFileName
    This%cOutputFile = cOutFile

    ! Line 4: NHYD
    CALL InFile%ReadData(cLine, iStat)
    IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    READ(cClean, *, IOSTAT=iErr) iNHyd
    IF (iErr /= 0 .OR. iNHyd <= 0) THEN
      CALL ModuleLogger%SetLastMessage('Invalid NHYD value in input file', &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF
    This%NHyd = iNHyd

    ! Line 5: FACTXY
    CALL InFile%ReadData(cLine, iStat)
    IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    READ(cClean, *, IOSTAT=iErr) rFactXY
    IF (iErr /= 0) rFactXY = 1d0

    ! Count AtNode vs AtXY (two-pass: first count, then allocate)
    ALLOCATE(This%OrderedHydList(iNHyd))

    ! First pass: count types
    iAtNodeCount = 0
    iAtXYCount   = 0
    DO i = 1, iNHyd
      CALL InFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL ModuleLogger%SetLastMessage('Unexpected end of input at hydrograph '// &
             TRIM(IntToText(i)), f_iFatal, ThisProcedure)
        RETURN
      END IF
      cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
      CALL CleanSpecialCharacters(cClean)
      READ(cClean, *, IOSTAT=iErr) iID, iHydTyp
      IF (iErr /= 0) THEN
        CALL ModuleLogger%SetLastMessage('Cannot parse hydrograph spec line '// &
             TRIM(IntToText(i)), f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF
      IF (iHydTyp == f_iHyd_AtNode) THEN
        iAtNodeCount = iAtNodeCount + 1
      ELSE
        iAtXYCount = iAtXYCount + 1
      END IF
    END DO
    CALL InFile%Kill()

    This%NHyd_AtNode = iAtNodeCount
    This%NHyd_AtXY   = iAtXYCount
    IF (iAtNodeCount > 0) ALLOCATE(This%Hyd_AtNode(iAtNodeCount))
    IF (iAtXYCount > 0)   ALLOCATE(This%Hyd_AtXY(iAtXYCount))

    ! Second pass: populate (re-open and skip 5 header data lines)
    CALL InFile%New(FileName=cInputFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                    Descriptor='ResultsExtract input', iStat=iStat)
    IF (iStat == -1) RETURN
    DO i = 1, 5
      CALL InFile%ReadData(cLine, iStat)
      IF (iStat == -1) RETURN
    END DO

    iAtNodeCount = 0
    iAtXYCount   = 0
    DO i = 1, iNHyd
      CALL InFile%ReadData(cLine, iStat)
      IF (iStat == -1) RETURN
      cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
      CALL CleanSpecialCharacters(cClean)
      READ(cClean, *, IOSTAT=iErr) iID, iHydTyp, iLayer
      IF (iErr /= 0) iLayer = 1

      cName = ' '
      IF (iHydTyp == f_iHyd_AtNode) THEN
        ! Format: ID HYDTYP LAYER NODE NAME
        READ(cClean, *, IOSTAT=iErr) iID, iHydTyp, iLayer, iNodeID, cName
        IF (iErr /= 0) THEN
          iNodeID = 0
          WRITE(cName, '(A,I0)') 'HYD', i
        END IF
        iAtNodeCount = iAtNodeCount + 1
        This%Hyd_AtNode(iAtNodeCount)%ID     = iID
        This%Hyd_AtNode(iAtNodeCount)%iLayer  = iLayer
        This%Hyd_AtNode(iAtNodeCount)%iNode   = iNodeID  ! Will convert to index later
        This%Hyd_AtNode(iAtNodeCount)%cName   = cName
        This%OrderedHydList(i)%iHydType = f_iHyd_AtNode
        This%OrderedHydList(i)%indx     = iAtNodeCount
      ELSE
        ! Format: ID HYDTYP LAYER X Y NAME
        READ(cClean, *, IOSTAT=iErr) iID, iHydTyp, iLayer, rX, rY, cName
        IF (iErr /= 0) THEN
          rX = 0d0; rY = 0d0
          WRITE(cName, '(A,I0)') 'HYD', i
        END IF
        iAtXYCount = iAtXYCount + 1
        This%Hyd_AtXY(iAtXYCount)%ID     = iID
        This%Hyd_AtXY(iAtXYCount)%iLayer  = iLayer
        This%Hyd_AtXY(iAtXYCount)%X       = rX * rFactXY
        This%Hyd_AtXY(iAtXYCount)%Y       = rY * rFactXY
        This%Hyd_AtXY(iAtXYCount)%cName   = cName
        This%OrderedHydList(i)%iHydType = f_iHyd_AtXY
        This%OrderedHydList(i)%indx     = iAtXYCount
      END IF
    END DO
    CALL InFile%Kill()

    CALL ModuleLogger%LogMessage('  Input file: '//TRIM(cInputFile), f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  Data type: '//TRIM(This%cDataType), f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  '//TRIM(IntToText(iNHyd))//' hydrographs specified ('// &
         TRIM(IntToText(This%NHyd_AtXY))//' AtXY, '// &
         TRIM(IntToText(This%NHyd_AtNode))//' AtNode)', f_iInfo, ThisProcedure)

    ! ================================================================
    ! Step 2: Parse simulation main file
    ! ================================================================
    CALL ParseSimMainFile(This, iStat)
    IF (iStat == -1) RETURN

    ! ================================================================
    ! Step 3: Parse GW main file and discover all-data file
    ! ================================================================
    CALL ParseGWMainFile(This, iStat)
    IF (iStat == -1) RETURN

    ! ================================================================
    ! Step 4: Load grid and stratigraphy from PP binary
    ! ================================================================
    CALL LoadGridFromPPBinary(This, iStat)
    IF (iStat == -1) RETURN

    ! ================================================================
    ! Step 5: Process hydrograph specifications (FE interpolation, etc.)
    ! ================================================================
    CALL ProcessHydSpecs(This, iStat)
    IF (iStat == -1) RETURN

    ! ================================================================
    ! Step 6: Prepare output file
    ! ================================================================
    CALL PrepareOutputFile(This, iStat)
    IF (iStat == -1) RETURN

    CALL ModuleLogger%LogMessage('  Initialization complete.', f_iInfo, ThisProcedure)

  END SUBROUTINE New


  ! =====================================================================
  ! ParseSimMainFile - Parse simulation main file for PP binary, GW main,
  !   and time stepping info.  Uses GenericFileType which auto-skips
  !   C/c/* comment lines in ASCII input files.
  ! =====================================================================
  SUBROUTINE ParseSimMainFile(This, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=40), PARAMETER :: ThisProcedure = cModName // '::ParseSimMainFile'
    TYPE(GenericFileType) :: SimFile
    CHARACTER(LEN=1000) :: cLine, cClean, cSimDir
    CHARACTER(:), ALLOCATABLE :: cAbsPathFileName
    INTEGER :: iErr, iPos, i
    CHARACTER(LEN=30) :: cTimeUnit
    REAL(8) :: rDeltaT
    INTEGER :: iDeltaT_InMinutes

    iStat = 0

    ! Determine simulation directory
    cSimDir = This%cSimMainFile
    iPos = MAX(SCAN(cSimDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cSimDir = cSimDir(1:iPos)
    ELSE
      cSimDir = './'
    END IF
    This%cSimDir = cSimDir

    CALL SimFile%New(FileName=This%cSimMainFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                     Descriptor='simulation main file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! 3 title lines (auto-skip of C/c/* comments means we get the data lines)
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN

    ! File 1: PP binary path
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(cSimDir), cAbsPathFileName)
    This%cPPBinaryFile = cAbsPathFileName

    ! File 2: GW main file
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(cSimDir), cAbsPathFileName)
    This%cGWMainFile = cAbsPathFileName

    ! Files 3-11: skip 9 more file entries
    DO i = 1, 9
      CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    END DO

    ! BDT (start date) -- extract first whitespace-delimited token
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    cClean = ADJUSTL(TRIM(cClean))
    iPos = INDEX(cClean, ' ')
    IF (iPos > 1) cClean = cClean(1:iPos-1)
    This%TimeStep%CurrentDateAndTime = cClean
    This%TimeStep%TrackTime = .TRUE.

    ! Skip 1 line (restart flag), then read time unit
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    cTimeUnit = ADJUSTL(TRIM(cClean))
    This%TimeStep%Unit = cTimeUnit(1:6)

    ! Convert time unit to DeltaT_InMinutes
    CALL CTimeStep_To_RTimeStep(cTimeUnit, rDeltaT, iDeltaT_InMinutes, iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot parse time unit: '//TRIM(cTimeUnit), &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF
    This%TimeStep%DeltaT_InMinutes = iDeltaT_InMinutes
    This%TimeStep%DeltaT = rDeltaT

    ! EDT (end date) -- extract first whitespace-delimited token
    CALL SimFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    cClean = ADJUSTL(TRIM(cClean))
    iPos = INDEX(cClean, ' ')
    IF (iPos > 1) cClean = cClean(1:iPos-1)
    This%TimeStep%EndDateAndTime = cClean

    CALL SimFile%Kill()

    ! Compute NTIME
    This%NTIME = NPeriods(iDeltaT_InMinutes, &
                          This%TimeStep%CurrentDateAndTime, &
                          This%TimeStep%EndDateAndTime)
    IF (This%NTIME <= 0) THEN
      CALL ModuleLogger%SetLastMessage('Computed NTIME <= 0. Check BDT/EDT/TimeUnit.', &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    CALL ModuleLogger%LogMessage('  Simulation file: '//TRIM(This%cSimMainFile), &
         f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  PP binary: '//TRIM(This%cPPBinaryFile), &
         f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  GW main: '//TRIM(This%cGWMainFile), &
         f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  BDT='//TRIM(This%TimeStep%CurrentDateAndTime)// &
         '  EDT='//TRIM(This%TimeStep%EndDateAndTime)// &
         '  NTIME='//TRIM(IntToText(This%NTIME)), f_iInfo, ThisProcedure)

  END SUBROUTINE ParseSimMainFile


  ! =====================================================================
  ! ParseGWMainFile - Extract conversion factors and all-data file path
  !
  ! For HEAD: reads AllHeadFile from GW main (data line 14)
  ! For SUBSIDENCE: reads subsidence file path from GW main (data line 4),
  !   then parses that file to find AllSubsOut path
  ! =====================================================================
  SUBROUTINE ParseGWMainFile(This, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=40), PARAMETER :: ThisProcedure = cModName // '::ParseGWMainFile'
    TYPE(GenericFileType) :: GWFile
    CHARACTER(LEN=1000) :: cLine, cClean, cSubsFile
    CHARACTER(:), ALLOCATABLE :: cAbsPathFileName
    INTEGER :: iErr, i

    iStat = 0

    CALL GWFile%New(FileName=This%cGWMainFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                    Descriptor='groundwater main file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! Read and discard version line (#4.0 etc.) — not skipped by SkipComment
    ! since # is not in f_cCommentIndicators (matches kernel's explicit read pattern)
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN

    ! Data lines 1-3: BC, TileDrain, Pumping
    DO i = 1, 3
      CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    END DO

    ! Data line 4: Subsidence filename (save for SUBSIDENCE mode)
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPathFileName)
    cSubsFile = cAbsPathFileName

    ! Data line 5: Parameter over-write filename
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN

    ! Data line 6: FACTLTOU (output conversion factor)
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    READ(cClean, *, IOSTAT=iErr) This%rFactOutput
    IF (iErr /= 0) This%rFactOutput = 1d0

    ! Data line 7: UNITLTOU (output unit string)
    CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    This%cUnitOutput = ADJUSTL(TRIM(cClean))

    SELECT CASE (This%iDataType)

      CASE (f_iDataType_HEAD)
        ! Data lines 8-13: FactFlow, UnitFlow, FactVelocity, UnitVelocity,
        !                  CellVelocityFile, VerticalFlowFile
        DO i = 1, 6
          CALL GWFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN
        END DO

        ! Data line 14: All-heads output file path
        CALL GWFile%ReadData(cLine, iStat)
        IF (iStat == -1) THEN
          CALL ModuleLogger%SetLastMessage('Cannot read all-heads file path from GW main', &
               f_iFatal, ThisProcedure)
          RETURN
        END IF
        cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
        CALL CleanSpecialCharacters(cClean)
        CALL GWFile%Kill()

        IF (LEN_TRIM(ADJUSTL(cClean)) == 0) THEN
          CALL ModuleLogger%SetLastMessage('All-heads output file path is blank in GW main. '// &
               'Simulation must have all-heads output enabled.', &
               f_iFatal, ThisProcedure)
          iStat = -1; RETURN
        END IF
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPathFileName)
        This%cAllDataFile = cAbsPathFileName

      CASE (f_iDataType_SUBSIDENCE)
        CALL GWFile%Kill()
        ! Discover AllSubsOut file from subsidence data file
        CALL ParseSubsidenceFile(This, cSubsFile, iStat)
        IF (iStat == -1) RETURN

    END SELECT

    CALL ModuleLogger%LogMessage('  All-data file: '//TRIM(This%cAllDataFile), &
         f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  Output unit: '//TRIM(This%cUnitOutput)// &
         '  Factor: '//TRIM(IntToText(INT(This%rFactOutput))), &
         f_iInfo, ThisProcedure)

  END SUBROUTINE ParseGWMainFile


  ! =====================================================================
  ! ParseSubsidenceFile - Parse subsidence data file to find AllSubsOut
  !
  ! v4.1/v5.1 subsidence file structure (matching kernel reading pattern
  ! in Class_AppSubsidence_v51.f90:144-150):
  !   Version #4.1 (read and discarded)
  !   Line 1: AllSubsOut file path  <-- what we need
  !   Line 2: IC file
  !   ... (remaining lines)
  ! =====================================================================
  SUBROUTINE ParseSubsidenceFile(This, cSubsFile, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),          INTENT(IN)    :: cSubsFile
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=40), PARAMETER :: ThisProcedure = cModName // '::ParseSubsidenceFile'
    TYPE(GenericFileType) :: SubsFile
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(:), ALLOCATABLE :: cAbsPathFileName

    iStat = 0

    CALL SubsFile%New(FileName=cSubsFile, InputFile=.TRUE., IsTSFile=.FALSE., &
                      Descriptor='subsidence data file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! Read version line (#4.1 or #5.1) - not skipped by SkipComment since # is not
    ! in f_cCommentIndicators; matches kernel pattern in v51.f90:145
    CALL SubsFile%ReadData(cLine, iStat)  ;  IF (iStat == -1) RETURN

    ! AllSubsOut file path (first data line after version; matches v51.f90:148)
    CALL SubsFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN
      CALL ModuleLogger%SetLastMessage('Cannot read AllSubsOut file path from subsidence file. '// &
           'Subsidence version must be >= 4.1.', f_iFatal, ThisProcedure)
      RETURN
    END IF
    cClean = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cClean)
    CALL SubsFile%Kill()

    IF (LEN_TRIM(ADJUSTL(cClean)) == 0) THEN
      CALL ModuleLogger%SetLastMessage('AllSubsOut file path is blank in subsidence file. '// &
           'Check subsidence version (must be >= 4.1 with AllSubsOut enabled).', &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    ! Resolve relative to simulation directory (same convention as kernel, v51.f90:152)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cClean)), TRIM(This%cSimDir), cAbsPathFileName)
    This%cAllDataFile = cAbsPathFileName

    CALL ModuleLogger%LogMessage('  Subsidence file: '//TRIM(cSubsFile), &
         f_iInfo, ThisProcedure)
    CALL ModuleLogger%LogMessage('  AllSubsOut: '//TRIM(This%cAllDataFile), &
         f_iInfo, ThisProcedure)

  END SUBROUTINE ParseSubsidenceFile


  ! =====================================================================
  ! LoadGridFromPPBinary - Load grid and stratigraphy from PP binary file
  !   Uses kernel's AppGridType%New and StratigraphyType%New
  ! =====================================================================
  SUBROUTINE LoadGridFromPPBinary(This, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=40), PARAMETER :: ThisProcedure = cModName // '::LoadGridFromPPBinary'
    TYPE(GenericFileType) :: PPBinFile

    iStat = 0

    CALL PPBinFile%New(FileName=This%cPPBinaryFile, InputFile=.TRUE., &
                       IsTSFile=.FALSE., Descriptor='preprocessor binary', &
                       iStat=iStat)
    IF (iStat == -1) RETURN

    CALL This%AppGrid%New(PPBinFile, iStat)
    IF (iStat == -1) RETURN

    CALL This%Stratigraphy%New(This%AppGrid%NNodes, PPBinFile, iStat)
    IF (iStat == -1) RETURN

    CALL PPBinFile%Kill()

    CALL ModuleLogger%LogMessage('  Grid: '//TRIM(IntToText(This%AppGrid%NNodes))//' nodes, '// &
         TRIM(IntToText(This%AppGrid%NElements))//' elements, '// &
         TRIM(IntToText(This%Stratigraphy%NLayers))//' layers', &
         f_iInfo, ThisProcedure)

  END SUBROUTINE LoadGridFromPPBinary


  ! =====================================================================
  ! ProcessHydSpecs - Compute FE interpolation coefficients for AtXY,
  !   convert node IDs to indices for AtNode.
  !   Uses kernel's AppGrid%FEInterpolate and ConvertID_To_Index.
  ! =====================================================================
  SUBROUTINE ProcessHydSpecs(This, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=40), PARAMETER :: ThisProcedure = cModName // '::ProcessHydSpecs'
    INTEGER :: i, iElem, iNodeIndex
    INTEGER, ALLOCATABLE :: iNodes(:)
    REAL(8), ALLOCATABLE :: rCoeff(:)
    INTEGER :: iNodeIDs(This%AppGrid%NNodes)

    iStat = 0

    CALL This%AppGrid%GetNodeIDs(iNodeIDs)

    ! Process AtXY hydrographs
    DO i = 1, This%NHyd_AtXY
      CALL This%AppGrid%FEInterpolate(This%Hyd_AtXY(i)%X, This%Hyd_AtXY(i)%Y, &
                                       iElem, iNodes, rCoeff)
      IF (iElem == 0) THEN
        CALL ModuleLogger%LogMessage('  WARNING: Hydrograph '//TRIM(IntToText(This%Hyd_AtXY(i)%ID))// &
             ' ('//TRIM(This%Hyd_AtXY(i)%cName)//') is outside the model grid - skipping.', &
             f_iWarn, ThisProcedure)
        This%Hyd_AtXY(i)%lSkip = .TRUE.
        CYCLE
      END IF
      This%Hyd_AtXY(i)%iElement = iElem
      This%Hyd_AtXY(i)%iNodes   = iNodes
      This%Hyd_AtXY(i)%rFactors = rCoeff

      IF (This%Hyd_AtXY(i)%iLayer > This%Stratigraphy%NLayers) THEN
        CALL ModuleLogger%SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtXY(i)%ID))// &
             ': layer '//TRIM(IntToText(This%Hyd_AtXY(i)%iLayer))// &
             ' exceeds model layers ('// &
             TRIM(IntToText(This%Stratigraphy%NLayers))//')', &
             f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF
    END DO

    ! Process AtNode hydrographs (convert user node ID to internal index)
    DO i = 1, This%NHyd_AtNode
      CALL ConvertID_To_Index(This%Hyd_AtNode(i)%iNode, iNodeIDs, iNodeIndex)
      IF (iNodeIndex == 0) THEN
        CALL ModuleLogger%SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtNode(i)%ID))// &
             ': node ID '//TRIM(IntToText(This%Hyd_AtNode(i)%iNode))// &
             ' not found in model grid.', f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF

      This%Hyd_AtNode(i)%iElement = This%AppGrid%AppNode(iNodeIndex)%ElemID_OnCCWSide(1)
      This%Hyd_AtNode(i)%iNode    = iNodeIndex   ! Replace user ID with internal index

      IF (This%Hyd_AtNode(i)%iLayer > This%Stratigraphy%NLayers) THEN
        CALL ModuleLogger%SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtNode(i)%ID))// &
             ': layer '//TRIM(IntToText(This%Hyd_AtNode(i)%iLayer))// &
             ' exceeds model layers ('// &
             TRIM(IntToText(This%Stratigraphy%NLayers))//')', &
             f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF
    END DO

    CALL ModuleLogger%LogMessage('  All hydrograph specifications processed successfully.', &
         f_iInfo, ThisProcedure)

  END SUBROUTINE ProcessHydSpecs


  ! =====================================================================
  ! PrepareOutputFile - Set up output file with IWFM hydrograph format.
  !   Text/DSS: uses kernel's PrepareTSDOutputFile (same as PrepHydOutFile)
  !   HDF5:     uses kernel's CreateHDFDataSet (same as Transfer_To_HDF)
  ! =====================================================================
  SUBROUTINE PrepareOutputFile(This, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=40), PARAMETER :: ThisProcedure = cModName // '::PrepareOutputFile'
    INTEGER :: indx, indx1, NHyd
    INTEGER :: iNodeIDs(This%AppGrid%NNodes), iElemIDs(This%AppGrid%NElements)
    CHARACTER(LEN=500) :: cFormatSpec
    CHARACTER(LEN=3000) :: TitleLines(1), WorkArray(2)
    CHARACTER(LEN=50)  :: Header(5, 1+This%NHyd)
    CHARACTER(LEN=500) :: HeaderFormat(5)
    CHARACTER(LEN=10)  :: DataUnit(1), DataType(1)
    CHARACTER(LEN=32)  :: CPart(1), FPart(1)
    INTEGER :: Layers(This%NHyd), IDs(This%NHyd), GWNodes(This%NHyd), Elements(This%NHyd)
    ! HDF5 output variables
    INTEGER            :: NColumns_HDF(1)
    CHARACTER(LEN=50)  :: cDataSetName_HDF(1)

    iStat = 0
    NHyd  = This%NHyd

    CALL This%AppGrid%GetNodeIDs(iNodeIDs)
    CALL This%AppGrid%GetElementIDs(iElemIDs)

    ! Open file (GenericFileType auto-detects format from extension)
    CALL This%OutFile%New(FileName=This%cOutputFile, InputFile=.FALSE., &
                          IsTSFile=.TRUE., &
                          Descriptor=TRIM(This%cDataType)//' hydrograph output', &
                          iStat=iStat)
    IF (iStat == -1) RETURN

    ! Compile node, layer, and element info for each hydrograph
    DO indx = 1, NHyd
      indx1 = This%OrderedHydList(indx)%indx
      SELECT CASE (This%OrderedHydList(indx)%iHydType)
        CASE (f_iHyd_AtNode)
          IDs(indx)      = This%Hyd_AtNode(indx1)%ID
          Layers(indx)   = This%Hyd_AtNode(indx1)%iLayer
          Elements(indx) = iElemIDs(This%Hyd_AtNode(indx1)%iElement)
          GWNodes(indx)  = iNodeIDs(This%Hyd_AtNode(indx1)%iNode)
        CASE (f_iHyd_AtXY)
          IDs(indx)      = This%Hyd_AtXY(indx1)%ID
          Layers(indx)   = This%Hyd_AtXY(indx1)%iLayer
          Elements(indx) = iElemIDs(This%Hyd_AtXY(indx1)%iElement)
          GWNodes(indx)  = 0
      END SELECT
    END DO

    ! Branch by output file type
    IF (This%OutFile%iGetFileType() == f_iHDF) THEN
      ! ----- HDF5 output (same pattern as Transfer_To_HDF, Class_BaseHydrograph.f90:1054-1057) -----
      NColumns_HDF(1) = NHyd
      SELECT CASE (This%iDataType)
        CASE (f_iDataType_HEAD)
          cDataSetName_HDF(1) = '/cGWHydrographs'     ! matches GWHydrograph.f90:1412
        CASE (f_iDataType_SUBSIDENCE)
          cDataSetName_HDF(1) = '/Subsidence'          ! matches Class_BaseAppSubsidence.f90:609
      END SELECT
      CALL This%OutFile%CreateHDFDataSet(cPathNames=cDataSetName_HDF, NColumns=NColumns_HDF, &
           NTime=This%NTIME+1, TimeStep=This%TimeStep, DataType=0d0, iStat=iStat)
      IF (iStat == -1) RETURN

    ELSE
      ! ----- Text/DSS output (same as PrepHydOutFile, Class_BaseHydrograph.f90:930-948) -----
      cFormatSpec = '(A21,*(2X,F10.3))'

      SELECT CASE (This%iDataType)
        CASE (f_iDataType_HEAD)
          WorkArray(1) = ArrangeText('GROUNDWATER HYDROGRAPH', 37)
          WorkArray(2) = ArrangeText('(UNIT=', This%cUnitOutput, ')', 37)
          CPart(1) = 'HEAD'
          FPart(1) = 'GW_HEAD_HYDROGRAPHS'
        CASE (f_iDataType_SUBSIDENCE)
          IF (This%lIncremental) THEN
            WorkArray(1) = ArrangeText('SUBSIDENCE HYDROGRAPH (INCR)', 37)
          ELSE
            WorkArray(1) = ArrangeText('SUBSIDENCE HYDROGRAPH (CUM)', 37)
          END IF
          WorkArray(2) = ArrangeText('(UNIT=', This%cUnitOutput, ')', 37)
          CPart(1) = 'SUBSIDENCE'
          FPart(1) = 'SUBS_HYDROGRAPHS'
      END SELECT
      CALL PrepareTitle(TitleLines(1), WorkArray(1:2), 39, 42)

      Header = ''
      WRITE(Header(1,1), '(A1,10X,A13)') '*', 'HYDROGRAPH ID'
      WRITE(Header(2,1), '(A1,18X,A5)')  '*', 'LAYER'
      WRITE(Header(3,1), '(A1,19X,A4)')  '*', 'NODE'
      WRITE(Header(4,1), '(A1,16X,A7)')  '*', 'ELEMENT'
      DO indx = 1, NHyd
        WRITE(Header(1, indx+1), '(I7)') IDs(indx)
        WRITE(Header(2, indx+1), '(I7)') Layers(indx)
        WRITE(Header(3, indx+1), '(I7)') GWNodes(indx)
        WRITE(Header(4, indx+1), '(I7)') Elements(indx)
      END DO
      WRITE(Header(5,1), '(A1,8X,A4)') '*', 'TIME'

      HeaderFormat(1:4) = '(A24,2X,*(A7,5X))'
      HeaderFormat(5)   = '(A13,*(A))'

      DataUnit(1) = This%TimeStep%Unit
      DataType(1) = 'PER-AVER'

      CALL PrepareTSDOutputFile(This%OutFile         , &
                                NHyd                  , &
                                1                     , &
                                .TRUE.                , &
                                cFormatSpec           , &
                                TitleLines            , &
                                Header                , &
                                HeaderFormat           , &
                                .FALSE.               , &
                                DataUnit              , &
                                DataType              , &
                                CPart                 , &
                                FPart                 , &
                                This%TimeStep%Unit    , &
                                IDs=IDs               , &
                                Layers=Layers         , &
                                Elements=Elements     , &
                                GWNodes=GWNodes       , &
                                iStat=iStat           )
      IF (iStat == -1) RETURN
    END IF

    CALL ModuleLogger%LogMessage('  Output file: '//TRIM(This%cOutputFile), &
         f_iInfo, ThisProcedure)

  END SUBROUTINE PrepareOutputFile


  ! =====================================================================
  ! Run - Main processing loop: read all-node data, compute hydrographs,
  !   write output.
  !
  !   Input reading:
  !     Text: Real2DTSDataInFile with nRow=NLayers, nCol=NNodes
  !     HDF5: Real2DTSDataInFile with nRow=NNodes*NLayers, nCol=1
  !           (matches single-dataset layout), then unpack to (NLayers,NNodes)
  !     DSS:  Real2DTSDataInFile with DSS pathnames
  !
  !   Output writing:
  !     Text/DSS: WriteData(SimulationTime, values, FinalPrint)
  !     HDF5:     WriteData(matrix(NHyd,1)) per timestep
  ! =====================================================================
  SUBROUTINE Run(This, iStat)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This
    INTEGER,                   INTENT(OUT)   :: iStat

    CHARACTER(LEN=30), PARAMETER :: ThisProcedure = cModName // '::Run'
    TYPE(Real2DTSDataInFileType) :: AllDataInFile
    INTEGER :: iTime, indxHyd, indx, iHydLayer, iHydNode
    INTEGER :: NLayers, NNodes, nVertex, indxVertex, iNode, indxLayer, iCount
    REAL(8), ALLOCATABLE :: rHydValues(:), rPrevHydValues(:), HXY(:)
    REAL(8), ALLOCATABLE :: rAllNodeData(:,:)
    REAL(8), ALLOCATABLE :: rHDFOut(:,:)
    REAL(8) :: rFactors(4)
    INTEGER :: iNodes(4)
    CHARACTER(LEN=21) :: SimulationTime
    TYPE(TimeStepType) :: TSLocal
    INTEGER :: iFileReadError
    LOGICAL :: lFinal, lHDF, lHDFOut
    CHARACTER(LEN=30) :: cDSS_CPart, cDSS_LayerPrefix

    iStat   = 0
    NLayers = This%Stratigraphy%NLayers
    NNodes  = This%AppGrid%NNodes

    ALLOCATE(rHydValues(This%NHyd), rAllNodeData(NLayers, NNodes), HXY(NLayers))
    IF (This%lIncremental) THEN
      ALLOCATE(rPrevHydValues(This%NHyd))
      rPrevHydValues = 0d0
    END IF

    ! Detect input and output file formats
    lHDF    = (iGetFileType_FromName(This%cAllDataFile) == f_iHDF)
    lHDFOut = (This%OutFile%iGetFileType() == f_iHDF)

    IF (lHDFOut) ALLOCATE(rHDFOut(This%NHyd, 1))

    ! ================================================================
    ! Open all-node data file for reading
    ! ================================================================
    IF (iGetFileType_FromName(This%cAllDataFile) == f_iDSS) THEN
      ! DSS: uses separate Init overload with pathnames
      BLOCK
        INTEGER :: iNodeIDs_Local(NNodes), iCnt, iLyr, iNd
        CHARACTER(LEN=80) :: cPathNames(NNodes*NLayers)
        CALL This%AppGrid%GetNodeIDs(iNodeIDs_Local)
        SELECT CASE (This%iDataType)
          CASE (f_iDataType_HEAD)
            cDSS_CPart = 'HEAD'
          CASE (f_iDataType_SUBSIDENCE)
            cDSS_CPart = 'SUBSIDENCE'
        END SELECT
        iCnt = 1
        DO iLyr = 1, NLayers
          DO iNd = 1, NNodes
            cPathNames(iCnt) = '/IWFM/L' // TRIM(IntToText(iLyr)) // &
                 ':GW' // TRIM(IntToText(iNodeIDs_Local(iNd))) // &
                 '/' // TRIM(cDSS_CPart) // '//' // &
                 UpperCase(TRIM(This%TimeStep%Unit)) // &
                 '/GW_' // TRIM(cDSS_CPart) // '_AT_ALL_NODES/'
            iCnt = iCnt + 1
          END DO
        END DO
        CALL AllDataInFile%Init(This%cAllDataFile, &
             TRIM(This%cDataType)//' at all nodes', &
             This%TimeStep%TrackTime, nRow=NLayers, nCol=NNodes, &
             cPathNames=cPathNames, iStat=iStat)
      END BLOCK

    ELSE IF (lHDF) THEN
      ! HDF5: single dataset with NNodes*NLayers columns.
      ! Open with flat dimensions to match dataset shape, then unpack.
      ! (Same Init overload as AllHeadOutFile_ForInquiry_New, GWHydrograph.f90:613)
      CALL AllDataInFile%Init(This%cAllDataFile, &
           TRIM(This%cDataType)//' at all nodes', &
           BlocksToSkip=0, nRow=NNodes*NLayers, nCol=1, iStat=iStat)

    ELSE
      ! Text: data stored as (NLayers, NNodes) matrix per timestep
      CALL AllDataInFile%Init(This%cAllDataFile, &
           TRIM(This%cDataType)//' at all nodes', &
           BlocksToSkip=0, nRow=NLayers, nCol=NNodes, iStat=iStat)
    END IF
    IF (iStat == -1) RETURN

    CALL ModuleLogger%LogMessage('  Processing '//TRIM(IntToText(This%NTIME+1))// &
         ' timesteps...', f_iInfo, ThisProcedure)

    TSLocal = This%TimeStep

    ! ================================================================
    ! Main timestep loop (initial conditions + NTIME steps)
    ! ================================================================
    DO iTime = 1, This%NTIME + 1
      lFinal = (iTime == This%NTIME + 1)

      ! Read one timestep of data
      CALL AllDataInFile%ReadTSData(TSLocal, TRIM(This%cDataType), iFileReadError, iStat)
      IF (iStat == -1) RETURN
      IF (iFileReadError /= 0) THEN
        CALL ModuleLogger%SetLastMessage('Error reading all-data file at timestep '// &
             TRIM(IntToText(iTime)), f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF

      ! Unpack into working array rAllNodeData(NLayers, NNodes)
      IF (lHDF) THEN
        ! HDF5 data is packed as: index = (layer-1)*NNodes + node
        ! (canonical layout from AllHeadOutFile_PrintResults / AllSubsOutFile_PrintResults)
        DO indxLayer = 1, NLayers
          DO iNode = 1, NNodes
            rAllNodeData(indxLayer, iNode) = AllDataInFile%rValues((indxLayer-1)*NNodes + iNode, 1)
          END DO
        END DO
      ELSE
        ! Text/DSS: already in (NLayers, NNodes) layout
        rAllNodeData = AllDataInFile%rValues
      END IF

      ! Compute hydrograph values
      rHydValues = 0d0
      DO indxHyd = 1, This%NHyd
        indx = This%OrderedHydList(indxHyd)%indx
        SELECT CASE (This%OrderedHydList(indxHyd)%iHydType)

          CASE (f_iHyd_AtNode)
            iHydLayer = This%Hyd_AtNode(indx)%iLayer
            iHydNode  = This%Hyd_AtNode(indx)%iNode
            IF (iHydLayer == 0) THEN
              SELECT CASE (This%iDataType)
                CASE (f_iDataType_HEAD)
                  iCount = COUNT(This%Stratigraphy%ActiveNode(iHydNode,:))
                  IF (iCount > 0) THEN
                    rHydValues(indxHyd) = SUM(rAllNodeData(:,iHydNode), &
                         MASK=This%Stratigraphy%ActiveNode(iHydNode,:)) / REAL(iCount,8)
                  END IF
                CASE (f_iDataType_SUBSIDENCE)
                  rHydValues(indxHyd) = SUM(rAllNodeData(:,iHydNode))
              END SELECT
            ELSE
              rHydValues(indxHyd) = rAllNodeData(iHydLayer, iHydNode)
            END IF

          CASE (f_iHyd_AtXY)
            IF (This%Hyd_AtXY(indx)%lSkip) THEN
              rHydValues(indxHyd) = 0d0
              CYCLE
            END IF
            iHydLayer = This%Hyd_AtXY(indx)%iLayer
            nVertex   = SIZE(This%Hyd_AtXY(indx)%iNodes)
            iNodes(1:nVertex)   = This%Hyd_AtXY(indx)%iNodes
            rFactors(1:nVertex) = This%Hyd_AtXY(indx)%rFactors
            IF (iHydLayer == 0) THEN
              SELECT CASE (This%iDataType)
                CASE (f_iDataType_HEAD)
                  HXY    = 0d0
                  iCount = 0
                  DO indxLayer = 1, NLayers
                    IF (ANY(This%Stratigraphy%ActiveNode(iNodes(1:nVertex), indxLayer))) &
                        iCount = iCount + 1
                    DO indxVertex = 1, nVertex
                      iNode = iNodes(indxVertex)
                      HXY(indxLayer) = HXY(indxLayer) + &
                           rAllNodeData(indxLayer, iNode) * rFactors(indxVertex)
                    END DO
                  END DO
                  IF (iCount > 0) THEN
                    rHydValues(indxHyd) = SUM(HXY) / REAL(iCount, 8)
                  END IF
                CASE (f_iDataType_SUBSIDENCE)
                  DO indxLayer = 1, NLayers
                    DO indxVertex = 1, nVertex
                      iNode = iNodes(indxVertex)
                      rHydValues(indxHyd) = rHydValues(indxHyd) + &
                           rAllNodeData(indxLayer, iNode) * rFactors(indxVertex)
                    END DO
                  END DO
              END SELECT
            ELSE
              DO indxVertex = 1, nVertex
                iNode = iNodes(indxVertex)
                rHydValues(indxHyd) = rHydValues(indxHyd) + &
                     rAllNodeData(iHydLayer, iNode) * rFactors(indxVertex)
              END DO
            END IF

        END SELECT
      END DO

      ! Create simulation time string
      IF (TSLocal%TrackTime) THEN
        SimulationTime = ADJUSTL(TSLocal%CurrentDateAndTime)
      ELSE
        WRITE(SimulationTime, '(F10.2,1X,A10)') TSLocal%CurrentTime, ADJUSTL(TSLocal%Unit)
      END IF

      ! Write output (with incremental conversion if needed)
      IF (This%lIncremental) THEN
        IF (iTime == 1) THEN
          ! Initial condition: write zeros (no prior timestep to diff against)
          IF (lHDFOut) THEN
            rHDFOut(:,1) = 0d0
            CALL This%OutFile%WriteData(rHDFOut)
          ELSE
            CALL This%OutFile%WriteData(SimulationTime, rHydValues * 0d0, FinalPrint=lFinal)
          END IF
        ELSE
          IF (lHDFOut) THEN
            rHDFOut(:,1) = rHydValues - rPrevHydValues
            CALL This%OutFile%WriteData(rHDFOut)
          ELSE
            CALL This%OutFile%WriteData(SimulationTime, &
                 rHydValues - rPrevHydValues, FinalPrint=lFinal)
          END IF
        END IF
        rPrevHydValues = rHydValues
      ELSE
        ! Cumulative output (HEAD or SUBSIDENCE_CUM)
        IF (lHDFOut) THEN
          rHDFOut(:,1) = rHydValues
          CALL This%OutFile%WriteData(rHDFOut)
        ELSE
          CALL This%OutFile%WriteData(SimulationTime, rHydValues, FinalPrint=lFinal)
        END IF
      END IF

      ! Advance timestamp
      IF (.NOT. lFinal) THEN
        TSLocal%CurrentDateAndTime = IncrementTimeStamp(TSLocal%CurrentDateAndTime, &
                                         TSLocal%DeltaT_InMinutes)
        TSLocal%CurrentTimeStep = TSLocal%CurrentTimeStep + 1
      END IF
    END DO

    CALL AllDataInFile%Close()

    CALL ModuleLogger%LogMessage('  Extraction complete. '//TRIM(IntToText(This%NHyd))// &
         ' hydrographs written.', f_iInfo, ThisProcedure)

  END SUBROUTINE Run


  ! =====================================================================
  ! Kill - Clean up
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(ResultsExtractType), INTENT(INOUT) :: This

    CALL This%OutFile%Kill()
    IF (ALLOCATED(This%Hyd_AtNode))     DEALLOCATE(This%Hyd_AtNode)
    IF (ALLOCATED(This%Hyd_AtXY))       DEALLOCATE(This%Hyd_AtXY)
    IF (ALLOCATED(This%OrderedHydList)) DEALLOCATE(This%OrderedHydList)

  END SUBROUTINE Kill

END MODULE Class_ResultsExtract
