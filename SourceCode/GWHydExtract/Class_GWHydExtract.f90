!***********************************************************************
!  GWHydExtract - Core Module
!
!  Reads the simulation's "all heads" output file and generates GW
!  hydrograph output at user-specified locations using FE interpolation.
!  Output matches IWFM's native hydrograph format for direct use by
!  IWFM2OBS.
!
!  Auto-discovers PP binary, GW main file, and all-heads file from
!  the simulation main file path.
!***********************************************************************
MODULE Class_GWHydExtract

  USE MessageLogger      , ONLY: SetLastMessage   , &
                                  LogMessage        , &
                                  f_iFatal          , &
                                  f_iWarn           , &
                                  f_iInfo
  USE GeneralUtilities   , ONLY: IntToText         , &
                                  UpperCase         , &
                                  EstablishAbsolutePathFileName , &
                                  ArrangeText       , &
                                  PrepareTitle      , &
                                  ConvertID_To_Index
  USE TimeSeriesUtilities, ONLY: TimeStepType              , &
                                  f_iTimeStampLength        , &
                                  IncrementTimeStamp        , &
                                  NPeriods                  , &
                                  CTimeStep_To_RTimeStep
  USE IOInterface        , ONLY: GenericFileType            , &
                                  Real2DTSDataInFileType     , &
                                  PrepareTSDOutputFile       , &
                                  iGetFileType_FromName      , &
                                  f_iDSS
  USE Class_AppGrid      , ONLY: AppGridType
  USE Class_Stratigraphy , ONLY: StratigraphyType

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: GWHydExtractType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_GWHydExtract'

  ! Hydrograph type flags (same convention as Class_BaseHydrograph)
  INTEGER, PARAMETER :: f_iHyd_AtXY   = 0
  INTEGER, PARAMETER :: f_iHyd_AtNode = 1

  ! =====================================================================
  ! Local hydrograph types (avoid modifying kernel PRIVATE members)
  ! =====================================================================
  TYPE :: GHE_HydAtNodeType
    INTEGER            :: ID       = 0
    INTEGER            :: iLayer   = 0
    INTEGER            :: iNode    = 0    ! Internal index (not user ID)
    INTEGER            :: iElement = 0    ! Element containing the node
    CHARACTER(LEN=30)  :: cName    = ' '
  END TYPE GHE_HydAtNodeType

  TYPE :: GHE_HydAtXYType
    INTEGER            :: ID       = 0
    INTEGER            :: iLayer   = 0
    INTEGER            :: iElement = 0    ! Element containing (X,Y)
    CHARACTER(LEN=30)  :: cName    = ' '
    REAL(8)            :: X        = 0d0
    REAL(8)            :: Y        = 0d0
    INTEGER, ALLOCATABLE :: iNodes(:)
    REAL(8), ALLOCATABLE :: rFactors(:)
  END TYPE GHE_HydAtXYType

  TYPE :: GHE_OrderedHydType
    INTEGER :: iHydType = 0   ! 0=AtXY, 1=AtNode
    INTEGER :: indx     = 0   ! Index into Hyd_AtNode or Hyd_AtXY array
  END TYPE GHE_OrderedHydType

  ! =====================================================================
  ! GWHydExtractType - Main application type
  ! =====================================================================
  TYPE :: GWHydExtractType
    ! Discovered paths
    CHARACTER(LEN=1000) :: cSimMainFile  = ' '
    CHARACTER(LEN=1000) :: cPPBinaryFile = ' '
    CHARACTER(LEN=1000) :: cGWMainFile   = ' '
    CHARACTER(LEN=1000) :: cAllHeadFile  = ' '
    CHARACTER(LEN=1000) :: cOutputFile   = ' '
    CHARACTER(LEN=1000) :: cSimDir       = ' '
    CHARACTER(LEN=1000) :: cInputDir     = ' '
    ! Grid and stratigraphy (reuse kernel types)
    TYPE(AppGridType)      :: AppGrid
    TYPE(StratigraphyType) :: Stratigraphy
    ! Time stepping
    TYPE(TimeStepType)     :: TimeStep
    INTEGER                :: NTIME = 0
    ! Head conversion from GW main file
    REAL(8)            :: rFactHead  = 1d0
    CHARACTER(LEN=30)  :: cUnitHead  = ' '
    ! Hydrograph specifications
    INTEGER :: NHyd = 0, NHyd_AtNode = 0, NHyd_AtXY = 0
    TYPE(GHE_HydAtNodeType),  ALLOCATABLE :: Hyd_AtNode(:)
    TYPE(GHE_HydAtXYType),    ALLOCATABLE :: Hyd_AtXY(:)
    TYPE(GHE_OrderedHydType), ALLOCATABLE :: OrderedHydList(:)
    ! Output file
    TYPE(GenericFileType) :: OutFile
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Run
    PROCEDURE, PASS :: Kill
  END TYPE GWHydExtractType

CONTAINS

  ! =====================================================================
  ! ReadSimDataLine - Read one data line from an IWFM simulation file
  !   IWFM convention: comments have C/c/*/# in column 1.
  !   Data lines are indented (column 1 is space).
  ! =====================================================================
  SUBROUTINE ReadSimDataLine(iUnit, cLine, iStat)
    INTEGER,          INTENT(IN)  :: iUnit
    CHARACTER(LEN=*), INTENT(OUT) :: cLine
    INTEGER,          INTENT(OUT) :: iStat

    iStat = 0
    DO
      READ(iUnit, '(A)', IOSTAT=iStat) cLine
      IF (iStat /= 0) RETURN
      IF (LEN_TRIM(cLine) == 0) CYCLE
      IF (cLine(1:1) == 'C' .OR. cLine(1:1) == 'c' .OR. &
          cLine(1:1) == '*' .OR. cLine(1:1) == '#') CYCLE
      cLine = ADJUSTL(cLine)
      EXIT
    END DO
  END SUBROUTINE ReadSimDataLine

  ! =====================================================================
  ! StripInlineComment - Remove text after '/' delimiter
  ! =====================================================================
  SUBROUTINE StripInlineComment(cLine, cResult)
    CHARACTER(LEN=*), INTENT(IN)  :: cLine
    CHARACTER(LEN=*), INTENT(OUT) :: cResult
    INTEGER :: iPos

    cResult = cLine
    iPos = SCAN(cResult, '/')
    IF (iPos > 1) THEN
      cResult = cResult(1:iPos-1)
    ELSE IF (iPos == 1) THEN
      cResult = ' '
    END IF
    cResult = ADJUSTL(TRIM(cResult))
  END SUBROUTINE StripInlineComment

  ! =====================================================================
  ! ResolveAbsPath - Wrapper for EstablishAbsolutePathFileName
  ! =====================================================================
  SUBROUTINE ResolveAbsPath(cFileName, cDefaultPath, cResult)
    CHARACTER(LEN=*), INTENT(IN)  :: cFileName, cDefaultPath
    CHARACTER(LEN=*), INTENT(OUT) :: cResult
    CHARACTER(:), ALLOCATABLE :: cAbsPath

    CALL EstablishAbsolutePathFileName(cFileName, cDefaultPath, cAbsPath)
    IF (ALLOCATED(cAbsPath)) THEN
      cResult = cAbsPath
    ELSE
      cResult = cFileName
    END IF
  END SUBROUTINE ResolveAbsPath

  ! =====================================================================
  ! New - Initialize: parse input, discover model files, load grid
  ! =====================================================================
  SUBROUTINE New(This, cInputFile, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),        INTENT(IN)    :: cInputFile
    INTEGER,                 INTENT(OUT)   :: iStat

    CHARACTER(LEN=35), PARAMETER :: ThisProcedure = cModName // '::New'
    INTEGER, PARAMETER :: iUnit = 198
    CHARACTER(LEN=1000) :: cLine, cClean, cSimFile, cOutFile
    CHARACTER(LEN=1000) :: cInputDir
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
    ! Step 1: Parse GWHydExtract input file
    ! ================================================================
    OPEN(UNIT=iUnit, FILE=cInputFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open input file: '//TRIM(cInputFile), &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    ! Line 1: Simulation main file path
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot read simulation file path from input', &
           f_iFatal, ThisProcedure)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL StripInlineComment(cLine, cClean)
    CALL ResolveAbsPath(TRIM(cClean), TRIM(cInputDir), cSimFile)
    This%cSimMainFile = cSimFile

    ! Line 2: Output file path
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot read output file path from input', &
           f_iFatal, ThisProcedure)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL StripInlineComment(cLine, cClean)
    CALL ResolveAbsPath(TRIM(cClean), TRIM(cInputDir), cOutFile)
    This%cOutputFile = cOutFile

    ! Line 3: NHYD
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNHyd
    IF (iErr /= 0 .OR. iNHyd <= 0) THEN
      CALL SetLastMessage('Invalid NHYD value in input file', &
           f_iFatal, ThisProcedure)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    This%NHyd = iNHyd

    ! Line 4: FACTXY
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) rFactXY
    IF (iErr /= 0) rFactXY = 1d0

    ! Count AtNode vs AtXY (two-pass: first count, then allocate)
    ! Read all hydrograph specs into temporary storage, then classify
    ALLOCATE(This%OrderedHydList(iNHyd))

    ! First pass: count types
    iAtNodeCount = 0
    iAtXYCount   = 0
    DO i = 1, iNHyd
      CALL ReadSimDataLine(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of input at hydrograph '// &
             TRIM(IntToText(i)), f_iFatal, ThisProcedure)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      CALL StripInlineComment(cLine, cClean)
      READ(cClean, *, IOSTAT=iErr) iID, iHydTyp
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot parse hydrograph spec line '// &
             TRIM(IntToText(i)), f_iFatal, ThisProcedure)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      IF (iHydTyp == f_iHyd_AtNode) THEN
        iAtNodeCount = iAtNodeCount + 1
      ELSE
        iAtXYCount = iAtXYCount + 1
      END IF
    END DO
    CLOSE(iUnit)

    This%NHyd_AtNode = iAtNodeCount
    This%NHyd_AtXY   = iAtXYCount
    IF (iAtNodeCount > 0) ALLOCATE(This%Hyd_AtNode(iAtNodeCount))
    IF (iAtXYCount > 0)   ALLOCATE(This%Hyd_AtXY(iAtXYCount))

    ! Second pass: populate
    OPEN(UNIT=iUnit, FILE=cInputFile, STATUS='OLD', IOSTAT=iErr)
    ! Skip to hydrograph specs: 4 header data lines
    DO i = 1, 4
      CALL ReadSimDataLine(iUnit, cLine, iErr)
    END DO

    iAtNodeCount = 0
    iAtXYCount   = 0
    DO i = 1, iNHyd
      CALL ReadSimDataLine(iUnit, cLine, iErr)
      CALL StripInlineComment(cLine, cClean)
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
    CLOSE(iUnit)

    CALL LogMessage('  Input file: '//TRIM(cInputFile), f_iInfo, ThisProcedure)
    CALL LogMessage('  '//TRIM(IntToText(iNHyd))//' hydrographs specified ('// &
         TRIM(IntToText(This%NHyd_AtXY))//' AtXY, '// &
         TRIM(IntToText(This%NHyd_AtNode))//' AtNode)', f_iInfo, ThisProcedure)

    ! ================================================================
    ! Step 2: Parse simulation main file
    ! ================================================================
    CALL ParseSimMainFile(This, iStat)
    IF (iStat == -1) RETURN

    ! ================================================================
    ! Step 3: Parse GW main file
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

    CALL LogMessage('  Initialization complete.', f_iInfo, ThisProcedure)

  END SUBROUTINE New


  ! =====================================================================
  ! ParseSimMainFile - Parse simulation main file for PP binary, GW main,
  !   and time stepping info
  ! =====================================================================
  SUBROUTINE ParseSimMainFile(This, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    INTEGER,                 INTENT(OUT)   :: iStat

    CHARACTER(LEN=35), PARAMETER :: ThisProcedure = cModName // '::ParseSimMainFile'
    INTEGER, PARAMETER :: iUnit = 199
    CHARACTER(LEN=1000) :: cLine, cClean, cSimDir
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

    OPEN(UNIT=iUnit, FILE=This%cSimMainFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open simulation main file: '// &
           TRIM(This%cSimMainFile), f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    ! 3 title lines: first via ReadSimDataLine, then 2 raw reads
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Unexpected end of simulation main file', &
           f_iFatal, ThisProcedure)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    READ(iUnit, '(A)', IOSTAT=iErr) cLine   ! title 2 (raw, may start with C)
    READ(iUnit, '(A)', IOSTAT=iErr) cLine   ! title 3 (raw, may start with C)

    ! File 1: PP binary path
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    CALL ResolveAbsPath(TRIM(cClean), TRIM(cSimDir), This%cPPBinaryFile)

    ! File 2: GW main file
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    CALL ResolveAbsPath(TRIM(cClean), TRIM(cSimDir), This%cGWMainFile)

    ! Files 3-11: skip 9 more file entries
    DO i = 1, 9
      CALL ReadSimDataLine(iUnit, cLine, iErr)
    END DO

    ! BDT (start date) — extract first whitespace-delimited token
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    cClean = ADJUSTL(TRIM(cLine))
    iPos = INDEX(cClean, ' ')
    IF (iPos > 1) cClean = cClean(1:iPos-1)
    This%TimeStep%CurrentDateAndTime = cClean
    This%TimeStep%TrackTime = .TRUE.

    ! Skip 1 line (restart flag), then read time unit
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    cTimeUnit = ADJUSTL(TRIM(cClean))
    This%TimeStep%Unit = cTimeUnit(1:6)

    ! Convert time unit to DeltaT_InMinutes
    CALL CTimeStep_To_RTimeStep(cTimeUnit, rDeltaT, iDeltaT_InMinutes, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot parse time unit: '//TRIM(cTimeUnit), &
           f_iFatal, ThisProcedure)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    This%TimeStep%DeltaT_InMinutes = iDeltaT_InMinutes
    This%TimeStep%DeltaT = rDeltaT

    ! EDT (end date) — extract first whitespace-delimited token
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    cClean = ADJUSTL(TRIM(cLine))
    iPos = INDEX(cClean, ' ')
    IF (iPos > 1) cClean = cClean(1:iPos-1)
    This%TimeStep%EndDateAndTime = cClean

    CLOSE(iUnit)

    ! Compute NTIME
    This%NTIME = NPeriods(iDeltaT_InMinutes, &
                          This%TimeStep%CurrentDateAndTime, &
                          This%TimeStep%EndDateAndTime)
    IF (This%NTIME <= 0) THEN
      CALL SetLastMessage('Computed NTIME <= 0. Check BDT/EDT/TimeUnit.', &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    CALL LogMessage('  Simulation file: '//TRIM(This%cSimMainFile), &
         f_iInfo, ThisProcedure)
    CALL LogMessage('  PP binary: '//TRIM(This%cPPBinaryFile), &
         f_iInfo, ThisProcedure)
    CALL LogMessage('  GW main: '//TRIM(This%cGWMainFile), &
         f_iInfo, ThisProcedure)
    CALL LogMessage('  BDT='//TRIM(This%TimeStep%CurrentDateAndTime)// &
         '  EDT='//TRIM(This%TimeStep%EndDateAndTime)// &
         '  NTIME='//TRIM(IntToText(This%NTIME)), f_iInfo, ThisProcedure)

  END SUBROUTINE ParseSimMainFile


  ! =====================================================================
  ! ParseGWMainFile - Extract FACTLTOU, UNITLTOU, and all-heads file path
  !
  ! GW main file data line order:
  !   Version line: #4.0  (starts with '#', auto-skipped by ReadSimDataLine)
  !   Data line 1: BC filename
  !   Data line 2: Tile drain filename
  !   Data line 3: Pumping filename
  !   Data line 4: Subsidence filename
  !   Data line 5: Parameter over-write filename
  !   Data line 6: FACTLTOU (head conversion factor)
  !   Data line 7: UNITLTOU (head unit)
  !   Data line 8: FactFlow
  !   Data line 9: UnitFlow
  !   Data line 10: FactVelocity
  !   Data line 11: UnitVelocity
  !   Data line 12: Cell velocity output file
  !   Data line 13: Vertical flow output file
  !   Data line 14: All-heads output file
  !
  ! Note: Version line '#4.0' starts with '#' which ReadSimDataLine
  ! treats as a comment and auto-skips. So we skip 5 data lines
  ! (BC through ParamOverwrite), not 6.
  ! =====================================================================
  SUBROUTINE ParseGWMainFile(This, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    INTEGER,                 INTENT(OUT)   :: iStat

    CHARACTER(LEN=35), PARAMETER :: ThisProcedure = cModName // '::ParseGWMainFile'
    INTEGER, PARAMETER :: iUnit = 200
    CHARACTER(LEN=1000) :: cLine, cClean
    INTEGER :: iErr, i

    iStat = 0

    OPEN(UNIT=iUnit, FILE=This%cGWMainFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open GW main file: '// &
           TRIM(This%cGWMainFile), f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    ! Skip 5 data lines: BC, TileDrain, Pumping, Subsidence, ParamOverwrite
    ! (Version line '#4.0' is auto-skipped by ReadSimDataLine as '#' comment)
    DO i = 1, 5
      CALL ReadSimDataLine(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of GW main file at line '// &
             TRIM(IntToText(i)), f_iFatal, ThisProcedure)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO

    ! Data line 6: FACTLTOU (head conversion factor)
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) This%rFactHead
    IF (iErr /= 0) This%rFactHead = 1d0

    ! Data line 7: UNITLTOU (head unit string)
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    This%cUnitHead = ADJUSTL(TRIM(cClean))

    ! Data lines 8-13: FactFlow, UnitFlow, FactVelocity, UnitVelocity,
    !                  CellVelocityFile, VerticalFlowFile
    DO i = 1, 6
      CALL ReadSimDataLine(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of GW main file at skip line '// &
             TRIM(IntToText(i+7)), f_iFatal, ThisProcedure)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO

    ! Data line 14: All-heads output file path
    CALL ReadSimDataLine(iUnit, cLine, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot read all-heads file path from GW main file', &
           f_iFatal, ThisProcedure)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL StripInlineComment(cLine, cClean)

    CLOSE(iUnit)

    IF (LEN_TRIM(cClean) == 0) THEN
      CALL SetLastMessage('All-heads output file path is blank in GW main file. '// &
           'The simulation must have been configured with all-heads output enabled.', &
           f_iFatal, ThisProcedure)
      iStat = -1; RETURN
    END IF

    ! Resolve relative to simulation directory (matching IWFM convention)
    CALL ResolveAbsPath(TRIM(cClean), TRIM(This%cSimDir), This%cAllHeadFile)

    CALL LogMessage('  All-heads file: '//TRIM(This%cAllHeadFile), &
         f_iInfo, ThisProcedure)
    CALL LogMessage('  Head unit: '//TRIM(This%cUnitHead)// &
         '  Factor: '//TRIM(IntToText(INT(This%rFactHead))), &
         f_iInfo, ThisProcedure)

  END SUBROUTINE ParseGWMainFile


  ! =====================================================================
  ! LoadGridFromPPBinary - Load grid and stratigraphy from PP binary file
  ! =====================================================================
  SUBROUTINE LoadGridFromPPBinary(This, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    INTEGER,                 INTENT(OUT)   :: iStat

    CHARACTER(LEN=35), PARAMETER :: ThisProcedure = cModName // '::LoadGridFromPPBinary'
    TYPE(GenericFileType) :: PPBinFile

    iStat = 0

    ! Open PP binary for reading
    CALL PPBinFile%New(FileName=This%cPPBinaryFile, InputFile=.TRUE., &
                       IsTSFile=.FALSE., Descriptor='preprocessor binary', &
                       iStat=iStat)
    IF (iStat == -1) RETURN

    ! Read grid (ReadProcessedAppGridData)
    CALL This%AppGrid%New(PPBinFile, iStat)
    IF (iStat == -1) RETURN

    ! Read stratigraphy (ReadProcessedStratigraphyData)
    CALL This%Stratigraphy%New(This%AppGrid%NNodes, PPBinFile, iStat)
    IF (iStat == -1) RETURN

    ! Close PP binary - only need grid and stratigraphy
    CALL PPBinFile%Kill()

    CALL LogMessage('  Grid: '//TRIM(IntToText(This%AppGrid%NNodes))//' nodes, '// &
         TRIM(IntToText(This%AppGrid%NElements))//' elements, '// &
         TRIM(IntToText(This%Stratigraphy%NLayers))//' layers', &
         f_iInfo, ThisProcedure)

  END SUBROUTINE LoadGridFromPPBinary


  ! =====================================================================
  ! ProcessHydSpecs - Compute FE interpolation coefficients for AtXY,
  !   convert node IDs to indices for AtNode
  ! =====================================================================
  SUBROUTINE ProcessHydSpecs(This, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    INTEGER,                 INTENT(OUT)   :: iStat

    CHARACTER(LEN=35), PARAMETER :: ThisProcedure = cModName // '::ProcessHydSpecs'
    INTEGER :: i, iElem, iNodeIndex
    INTEGER, ALLOCATABLE :: iNodes(:)
    REAL(8), ALLOCATABLE :: rCoeff(:)
    INTEGER :: iNodeIDs(This%AppGrid%NNodes)

    iStat = 0

    ! Get node IDs for ConvertID_To_Index
    CALL This%AppGrid%GetNodeIDs(iNodeIDs)

    ! Process AtXY hydrographs
    DO i = 1, This%NHyd_AtXY
      CALL This%AppGrid%FEInterpolate(This%Hyd_AtXY(i)%X, This%Hyd_AtXY(i)%Y, &
                                       iElem, iNodes, rCoeff)
      IF (iElem == 0) THEN
        CALL SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtXY(i)%ID))// &
             ' ('//TRIM(This%Hyd_AtXY(i)%cName)//') is outside the model grid.', &
             f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF
      This%Hyd_AtXY(i)%iElement = iElem
      This%Hyd_AtXY(i)%iNodes   = iNodes
      This%Hyd_AtXY(i)%rFactors = rCoeff

      ! Validate layer
      IF (This%Hyd_AtXY(i)%iLayer > This%Stratigraphy%NLayers) THEN
        CALL SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtXY(i)%ID))// &
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
        CALL SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtNode(i)%ID))// &
             ': node ID '//TRIM(IntToText(This%Hyd_AtNode(i)%iNode))// &
             ' not found in model grid.', f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF

      ! Find element containing this node (use first element in connectivity)
      This%Hyd_AtNode(i)%iElement = This%AppGrid%AppNode(iNodeIndex)%ElemID_OnCCWSide(1)
      This%Hyd_AtNode(i)%iNode    = iNodeIndex   ! Replace user ID with internal index

      ! Validate layer
      IF (This%Hyd_AtNode(i)%iLayer > This%Stratigraphy%NLayers) THEN
        CALL SetLastMessage('Hydrograph '//TRIM(IntToText(This%Hyd_AtNode(i)%ID))// &
             ': layer '//TRIM(IntToText(This%Hyd_AtNode(i)%iLayer))// &
             ' exceeds model layers ('// &
             TRIM(IntToText(This%Stratigraphy%NLayers))//')', &
             f_iFatal, ThisProcedure)
        iStat = -1; RETURN
      END IF
    END DO

    CALL LogMessage('  All hydrograph specifications processed successfully.', &
         f_iInfo, ThisProcedure)

  END SUBROUTINE ProcessHydSpecs


  ! =====================================================================
  ! PrepareOutputFile - Set up output file with IWFM hydrograph format
  ! =====================================================================
  SUBROUTINE PrepareOutputFile(This, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    INTEGER,                 INTENT(OUT)   :: iStat

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

    iStat = 0
    NHyd  = This%NHyd

    ! Get node and element IDs
    CALL This%AppGrid%GetNodeIDs(iNodeIDs)
    CALL This%AppGrid%GetElementIDs(iElemIDs)

    ! Open file
    CALL This%OutFile%New(FileName=This%cOutputFile, InputFile=.FALSE., &
                          IsTSFile=.TRUE., &
                          Descriptor='groundwater hydrograph output', &
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

    ! Format spec matches IWFM GW hydrograph output
    cFormatSpec = '(A21,*(2X,F10.3))'

    ! Title
    WorkArray(1) = ArrangeText('GROUNDWATER HYDROGRAPH', 37)
    WorkArray(2) = ArrangeText('(UNIT=', This%cUnitHead, ')', 37)
    CALL PrepareTitle(TitleLines(1), WorkArray(1:2), 39, 42)

    ! Headers (5 rows matching IWFM format)
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

    ! Data unit and type
    DataUnit(1) = This%TimeStep%Unit
    DataType(1) = 'PER-AVER'
    CPart(1)    = 'HEAD'
    FPart(1)    = 'GW_HYDROGRAPHS'

    ! Call PrepareTSDOutputFile
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

    CALL LogMessage('  Output file: '//TRIM(This%cOutputFile), &
         f_iInfo, ThisProcedure)

  END SUBROUTINE PrepareOutputFile


  ! =====================================================================
  ! Run - Main processing loop: read all-heads, compute hydrographs, write
  ! =====================================================================
  SUBROUTINE Run(This, iStat)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This
    INTEGER,                 INTENT(OUT)   :: iStat

    CHARACTER(LEN=25), PARAMETER :: ThisProcedure = cModName // '::Run'
    TYPE(Real2DTSDataInFileType) :: AllHeadInFile
    INTEGER :: iTime, indxHyd, indx, iHydLayer, iHydNode
    INTEGER :: NLayers, NNodes, nVertex, indxVertex, iNode, indxLayer, iCount
    REAL(8), ALLOCATABLE :: rHydValues(:), HXY(:)
    REAL(8) :: rFactors(4)
    INTEGER :: iNodes(4)
    CHARACTER(LEN=21) :: SimulationTime
    TYPE(TimeStepType) :: TSLocal
    INTEGER :: iFileReadError
    LOGICAL :: lFinal

    iStat   = 0
    NLayers = This%Stratigraphy%NLayers
    NNodes  = This%AppGrid%NNodes

    ALLOCATE(rHydValues(This%NHyd), HXY(NLayers))

    ! Open all-heads file for reading (same pattern as AllHeadOutFile_ForInquiry_New)
    IF (iGetFileType_FromName(This%cAllHeadFile) == f_iDSS) THEN
      BLOCK
        INTEGER :: iNodeIDs_Local(NNodes), iCount, indxLayer, indxNode
        CHARACTER(LEN=80) :: cPathNames(NNodes*NLayers)
        CALL This%AppGrid%GetNodeIDs(iNodeIDs_Local)
        iCount = 1
        DO indxLayer = 1, NLayers
          DO indxNode = 1, NNodes
            cPathNames(iCount) = '/IWFM/L' // TRIM(IntToText(indxLayer)) // &
                 ':GW' // TRIM(IntToText(iNodeIDs_Local(indxNode))) // &
                 '/HEAD//' // UpperCase(TRIM(This%TimeStep%Unit)) // &
                 '/GW_HEAD_AT_ALL_NODES/'
            iCount = iCount + 1
          END DO
        END DO
        CALL AllHeadInFile%Init(This%cAllHeadFile, &
             'groundwater head at all nodes', &
             This%TimeStep%TrackTime, nRow=NLayers, nCol=NNodes, &
             cPathNames=cPathNames, iStat=iStat)
      END BLOCK
    ELSE
      CALL AllHeadInFile%Init(This%cAllHeadFile, &
           'groundwater head at all nodes', &
           BlocksToSkip=0, nRow=NLayers, nCol=NNodes, iStat=iStat)
    END IF
    IF (iStat == -1) RETURN

    CALL LogMessage('  Processing '//TRIM(IntToText(This%NTIME+1))// &
         ' timesteps...', f_iInfo, ThisProcedure)

    ! Initialize local timestep for advancing
    TSLocal = This%TimeStep

    ! Loop over NTIME+1 timesteps (initial conditions + NTIME steps)
    DO iTime = 1, This%NTIME + 1
      lFinal = (iTime == This%NTIME + 1)

      ! Read one timestep of heads: rValues(NLayers, NNodes)
      CALL AllHeadInFile%ReadTSData(TSLocal, 'all heads', iFileReadError, iStat)
      IF (iStat == -1) RETURN
      IF (iFileReadError /= 0) THEN
        CALL SetLastMessage('Error reading all-heads file at timestep '// &
             TRIM(IntToText(iTime)), f_iFatal, ThisProcedure)
        iStat = -1; RETURN
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
              ! Average over active layers
              iCount = COUNT(This%Stratigraphy%ActiveNode(iHydNode,:))
              IF (iCount > 0) THEN
                rHydValues(indxHyd) = SUM(AllHeadInFile%rValues(:,iHydNode), &
                     MASK=This%Stratigraphy%ActiveNode(iHydNode,:)) / REAL(iCount,8)
              END IF
            ELSE
              rHydValues(indxHyd) = AllHeadInFile%rValues(iHydLayer, iHydNode)
            END IF

          CASE (f_iHyd_AtXY)
            iHydLayer = This%Hyd_AtXY(indx)%iLayer
            nVertex   = SIZE(This%Hyd_AtXY(indx)%iNodes)
            iNodes(1:nVertex)   = This%Hyd_AtXY(indx)%iNodes
            rFactors(1:nVertex) = This%Hyd_AtXY(indx)%rFactors
            IF (iHydLayer == 0) THEN
              ! Interpolate at each layer, average active
              HXY    = 0d0
              iCount = 0
              DO indxLayer = 1, NLayers
                IF (ANY(This%Stratigraphy%ActiveNode(iNodes(1:nVertex), indxLayer))) &
                    iCount = iCount + 1
                DO indxVertex = 1, nVertex
                  iNode = iNodes(indxVertex)
                  HXY(indxLayer) = HXY(indxLayer) + &
                       AllHeadInFile%rValues(indxLayer, iNode) * rFactors(indxVertex)
                END DO
              END DO
              IF (iCount > 0) THEN
                rHydValues(indxHyd) = SUM(HXY) / REAL(iCount, 8)
              END IF
            ELSE
              ! Specific layer
              DO indxVertex = 1, nVertex
                iNode = iNodes(indxVertex)
                rHydValues(indxHyd) = rHydValues(indxHyd) + &
                     AllHeadInFile%rValues(iHydLayer, iNode) * rFactors(indxVertex)
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

      ! Write output
      CALL This%OutFile%WriteData(SimulationTime, rHydValues, FinalPrint=lFinal)

      ! Advance timestamp
      IF (.NOT. lFinal) THEN
        TSLocal%CurrentDateAndTime = IncrementTimeStamp(TSLocal%CurrentDateAndTime, &
                                         TSLocal%DeltaT_InMinutes)
        TSLocal%CurrentTimeStep = TSLocal%CurrentTimeStep + 1
      END IF
    END DO

    ! Close all-heads input file
    CALL AllHeadInFile%Close()

    CALL LogMessage('  Extraction complete. '//TRIM(IntToText(This%NHyd))// &
         ' hydrographs written.', f_iInfo, ThisProcedure)

  END SUBROUTINE Run


  ! =====================================================================
  ! Kill - Clean up
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(GWHydExtractType), INTENT(INOUT) :: This

    CALL This%OutFile%Kill()
    IF (ALLOCATED(This%Hyd_AtNode))     DEALLOCATE(This%Hyd_AtNode)
    IF (ALLOCATED(This%Hyd_AtXY))       DEALLOCATE(This%Hyd_AtXY)
    IF (ALLOCATED(This%OrderedHydList)) DEALLOCATE(This%OrderedHydList)

  END SUBROUTINE Kill

END MODULE Class_GWHydExtract
