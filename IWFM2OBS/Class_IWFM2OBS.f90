!***********************************************************************
!  IWFM2OBS - Class_IWFM2OBS
!  Main orchestrator: input file parsing, 4 hydrograph types,
!  optional multi-layer target, head differences
!
!  Replaces the old iwfm2obs main program + multilayertarget
!  Input: unified input file format with optional simulation main file
!  Output: PEST SMP, instruction, and PCF files
!
!  NEW: If a simulation main file path is provided as the first data
!  line, .out files are auto-discovered and converted to temp SMP files
!  before the normal SMP2SMP interpolation runs. This combines the
!  capabilities of the old iwfm2obs (direct .out reading) with the new
!  multi-layer T-weighted averaging.
!***********************************************************************
MODULE Class_IWFM2OBS

  USE MessageLogger      , ONLY: SetLastMessage   , &
                                 LogMessage        , &
                                 LogLastMessage    , &
                                 f_iFatal          , &
                                 f_iWarn           , &
                                 f_iInfo
  USE GeneralUtilities   , ONLY: IntToText         , &
                                 UpperCase
  USE Class_SMP2SMP      , ONLY: SMP2SMPType       , &
                                 SMPRecordType     , &
                                 SMPIDGroupType    , &
                                 TokenizeSMPLine
  USE Class_PESTOutput   , ONLY: PESTOutputType
  USE Class_HeadDifference, ONLY: HeadDifferenceType, &
                                  HeadDiffPairType
  USE Class_MultiLayerTarget, ONLY: MultiLayerTargetType
  USE Class_HydrographReader, ONLY: HydrographReaderType, &
                                    iHR_SUBSID, iHR_TILEDR, &
                                    iHR_STREAM, iHR_GWHEAD, &
                                    iHR_NUMHYD

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: IWFM2OBSType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_IWFM2OBS'

  ! Hydrograph type indices (matching original convention)
  INTEGER, PARAMETER :: iSUBSID = 1
  INTEGER, PARAMETER :: iTILEDR = 2
  INTEGER, PARAMETER :: iSTREAM = 3
  INTEGER, PARAMETER :: iGWHEAD = 4
  INTEGER, PARAMETER :: iNUMHYD = 4

  CHARACTER(LEN=12), PARAMETER :: cHydName(4) = &
       (/ 'Subsidence  ', 'Tile Drain  ', 'Stream      ', 'Groundwater ' /)

  ! Temp SMP file names for .out → SMP conversion
  CHARACTER(LEN=20), PARAMETER :: cTempSMP(4) = &
       (/ 'sb_temp_iwfm2obs.smp', 'td_temp_iwfm2obs.smp', &
          'st_temp_iwfm2obs.smp', 'gw_temp_iwfm2obs.smp' /)

  ! =====================================================================
  ! HydTypeConfigType - Configuration for one hydrograph type
  ! =====================================================================
  TYPE :: HydTypeConfigType
    CHARACTER(LEN=500) :: cHydFile  = ' '   ! Model hydrograph SMP file
    CHARACTER(LEN=500) :: cObsFile  = ' '   ! Observation SMP file
    CHARACTER(LEN=500) :: cOutFile  = ' '   ! Output SMP file
    REAL(8)            :: rThreshold = 1.0D0 ! Extrapolation threshold (days)
    CHARACTER(LEN=500) :: cInsFile  = ' '   ! PEST instruction file
    CHARACTER(LEN=500) :: cPCFFile  = ' '   ! PEST PCF file
    LOGICAL            :: lActive   = .FALSE.
    LOGICAL            :: lWriteIns = .FALSE.
  END TYPE HydTypeConfigType

  ! =====================================================================
  ! IWFM2OBSType - Main orchestrator
  ! =====================================================================
  TYPE :: IWFM2OBSType
    TYPE(HydTypeConfigType)      :: HydConfig(iNUMHYD)
    TYPE(SMP2SMPType)            :: Interp
    TYPE(HeadDifferenceType)     :: HeadDiff
    TYPE(MultiLayerTargetType)   :: MultiLayer
    TYPE(HydrographReaderType)   :: HydReader
    LOGICAL                      :: lMultiLayer = .FALSE.
    LOGICAL                      :: lHeadDiff   = .FALSE.
    LOGICAL                      :: lModelMode  = .FALSE. ! .TRUE. = auto-discover .out files
    CHARACTER(LEN=500)           :: cSimMainFile = ' '
    CHARACTER(LEN=500)           :: cHDiffFile  = ' '
    ! Multi-layer file paths
    CHARACTER(LEN=500)           :: cObsWellFile = ' '
    CHARACTER(LEN=500)           :: cElemsFile   = ' '
    CHARACTER(LEN=500)           :: cNodesFile   = ' '
    CHARACTER(LEN=500)           :: cStratFile   = ' '
    CHARACTER(LEN=500)           :: cGWMainFile  = ' '
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Run
    PROCEDURE, PASS :: Kill
  END TYPE IWFM2OBSType

CONTAINS

  ! =====================================================================
  ! ReadNonComment - Read one non-comment line, strip inline comment
  ! =====================================================================
  SUBROUTINE ReadNonComment(iUnit, cLine, iStat)
    INTEGER,          INTENT(IN)  :: iUnit
    CHARACTER(LEN=*), INTENT(OUT) :: cLine
    INTEGER,          INTENT(OUT) :: iStat

    iStat = 0
    DO
      READ(iUnit, '(A)', IOSTAT=iStat) cLine
      IF (iStat /= 0) RETURN
      cLine = ADJUSTL(cLine)
      IF (LEN_TRIM(cLine) == 0) CYCLE
      IF (cLine(1:1) == 'C' .OR. cLine(1:1) == 'c' .OR. &
          cLine(1:1) == '*' .OR. cLine(1:1) == '#') CYCLE
      EXIT
    END DO
  END SUBROUTINE ReadNonComment

  ! =====================================================================
  ! StripComment - Remove inline comment (after '/')
  ! =====================================================================
  SUBROUTINE StripComment(cLine, cResult)
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
  END SUBROUTINE StripComment

  ! =====================================================================
  ! New - Read input file and initialize
  !
  !   Input file format (new, backward compatible):
  !     Line 1: Simulation main file path (blank = explicit SMP mode)
  !     Line 2: Date format (1=dd/mm, 2=mm/dd)
  !     Lines 3+: Same as before (4 hyd blocks of 6 lines each, etc.)
  !
  !   When a simulation main file is provided:
  !     - Auto-discovers .out file paths from component main files
  !     - Converts .out files to temp SMP files
  !     - Overrides cHydFile in each HydConfig with the temp SMP path
  !     - The "Model hydrograph SMP file" line in each block is ignored
  !       but still needs an observation SMP path to be active
  ! =====================================================================
  SUBROUTINE New(This, cInputFile, iStat)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),    INTENT(IN)    :: cInputFile
    INTEGER,             INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 190
    CHARACTER(LEN=500) :: cLine, cClean
    CHARACTER(LEN=1)   :: cYN
    CHARACTER(LEN=500) :: cWorkDir
    INTEGER            :: iErr, iDateSpec, iHyd, i, iPos

    iStat = 0

    ! Determine working directory from input file path
    cWorkDir = cInputFile
    iPos = MAX(SCAN(cWorkDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cWorkDir = cWorkDir(1:iPos)
    ELSE
      cWorkDir = '.'
    END IF

    OPEN(UNIT=iUnit, FILE=cInputFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open input file: '//TRIM(cInputFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! ---- NEW: Simulation main file (first data line) ----
    CALL ReadNonComment(iUnit, cLine, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Error reading first line from input file', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL StripComment(cLine, cClean)

    ! Check if first line is a file path or a date format integer
    ! If it's 1 or 2, it's the old format (no sim main file)
    ! If it's anything else, it's a sim main file path
    IF (TRIM(cClean) == '1' .OR. TRIM(cClean) == '2') THEN
      ! Old format: first line is date format
      READ(cClean, *, IOSTAT=iErr) iDateSpec
      This%lModelMode = .FALSE.
    ELSE IF (LEN_TRIM(cClean) == 0) THEN
      ! Blank = explicit SMP mode, read date format next
      This%lModelMode = .FALSE.
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, cClean)
      READ(cClean, *, IOSTAT=iErr) iDateSpec
    ELSE
      ! New format: first line is simulation main file path
      This%cSimMainFile = cClean
      This%lModelMode = .TRUE.
      ! Read date format next
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, cClean)
      READ(cClean, *, IOSTAT=iErr) iDateSpec
    END IF

    IF (iErr /= 0 .OR. (iDateSpec /= 1 .AND. iDateSpec /= 2)) THEN
      CALL SetLastMessage('Invalid date format (must be 1 or 2)', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL This%Interp%Init(iDateSpec)

    ! ---- Read 4 hydrograph type blocks (6 lines each) ----
    ! Order: GW (4), Stream (3), Tile Drain (2), Subsidence (1)
    DO iHyd = iGWHEAD, iSUBSID, -1
      ! Line 1: Hydrograph/model SMP file (or observation SMP in model mode)
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading '//TRIM(cHydName(iHyd))// &
             ' hydrograph file path', f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      CALL StripComment(cLine, cClean)

      IF (LEN_TRIM(cClean) == 0) THEN
        ! Skip this hydrograph type (blank = inactive)
        This%HydConfig(iHyd)%lActive = .FALSE.
        ! Still need to read 5 more lines
        DO i = 1, 5
          CALL ReadNonComment(iUnit, cLine, iErr)
        END DO
        CYCLE
      END IF

      This%HydConfig(iHyd)%cHydFile = cClean
      This%HydConfig(iHyd)%lActive  = .TRUE.

      ! Line 2: Observation SMP file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cObsFile)

      ! Line 3: Output SMP file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cOutFile)

      ! Line 4: Extrapolation threshold
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, cClean)
      READ(cClean, *, IOSTAT=iErr) This%HydConfig(iHyd)%rThreshold
      IF (iErr /= 0) This%HydConfig(iHyd)%rThreshold = 1.0D0

      ! Line 5: Instruction file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cInsFile)
      This%HydConfig(iHyd)%lWriteIns = (LEN_TRIM(This%HydConfig(iHyd)%cInsFile) > 0)

      ! Line 6: PCF file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cPCFFile)
    END DO

    ! ---- Head differences (Y/N, then file path if Y) ----
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripComment(cLine, cClean)
    cYN = UpperCase(cClean(1:1))
    IF (cYN == 'Y') THEN
      This%lHeadDiff = .TRUE.
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cHDiffFile)
    END IF

    ! ---- Multi-layer target (Y/N, then 5 file paths if Y) ----
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripComment(cLine, cClean)
    cYN = UpperCase(cClean(1:1))
    IF (cYN == 'Y') THEN
      This%lMultiLayer = .TRUE.
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cObsWellFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cElemsFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cNodesFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cStratFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cGWMainFile)
    END IF

    CLOSE(iUnit)

    ! ---- Model discovery mode: convert .out files to temp SMP ----
    IF (This%lModelMode) THEN
      CALL LogMessage('Model discovery mode: parsing simulation main file...', &
           f_iInfo, cModName)
      CALL This%HydReader%DiscoverModelFiles(This%cSimMainFile, cWorkDir, &
           iDateSpec, iStat)
      IF (iStat /= 0) THEN
        CALL LogLastMessage()
        CALL SetLastMessage('Failed to discover model files from: '// &
             TRIM(This%cSimMainFile), f_iFatal, cModName)
        RETURN
      END IF

      ! Convert each discovered .out file to temp SMP and override HydConfig
      DO iHyd = 1, iNUMHYD
        IF (This%HydReader%HydInfo(iHyd)%lActive) THEN
          CALL This%HydReader%ReadDotOutFile(iHyd, cTempSMP(iHyd), iStat)
          IF (iStat /= 0) THEN
            CALL LogLastMessage()
            CALL LogMessage('  Warning: failed to read .out for '// &
                 TRIM(cHydName(iHyd)), f_iWarn, cModName)
            iStat = 0
            CYCLE
          END IF
          ! Override model SMP path with temp file
          This%HydConfig(iHyd)%cHydFile = cTempSMP(iHyd)
          This%HydConfig(iHyd)%lActive  = .TRUE.
          CALL LogMessage('  '//TRIM(cHydName(iHyd))//': .out → '// &
               TRIM(cTempSMP(iHyd)), f_iInfo, cModName)
        END IF
      END DO
    END IF

    ! ---- Initialize multi-layer target if requested ----
    IF (This%lMultiLayer) THEN
      CALL This%MultiLayer%New(This%cNodesFile, This%cElemsFile, &
           This%cStratFile, This%cGWMainFile, This%cObsWellFile, iStat)
      IF (iStat /= 0) RETURN
    END IF

    ! ---- Initialize head differences if requested ----
    IF (This%lHeadDiff) THEN
      CALL This%HeadDiff%New(This%cHDiffFile, iStat)
      IF (iStat /= 0) RETURN
    END IF

    ! Report configuration
    DO iHyd = 1, iNUMHYD
      IF (This%HydConfig(iHyd)%lActive) THEN
        CALL LogMessage('  '//TRIM(cHydName(iHyd))//': '// &
             TRIM(This%HydConfig(iHyd)%cHydFile), f_iInfo, cModName)
      END IF
    END DO
    IF (This%lHeadDiff) &
      CALL LogMessage('  Head differences: '//TRIM(This%cHDiffFile), &
           f_iInfo, cModName)
    IF (This%lMultiLayer) &
      CALL LogMessage('  Multi-layer target: active ('// &
           TRIM(IntToText(This%MultiLayer%GetNObs()))//' wells)', &
           f_iInfo, cModName)

  END SUBROUTINE New

  ! =====================================================================
  ! Run - Execute the interpolation workflow
  ! =====================================================================
  SUBROUTINE Run(This, iStat)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This
    INTEGER,             INTENT(OUT)   :: iStat

    INTEGER :: iHyd, i

    iStat = 0

    ! Process each hydrograph type
    DO iHyd = 1, iNUMHYD
      IF (.NOT. This%HydConfig(iHyd)%lActive) CYCLE

      CALL LogMessage('Processing '//TRIM(cHydName(iHyd))//' hydrographs...', &
           f_iInfo, cModName)

      ! Call SMP2SMP interpolation
      CALL This%Interp%Interpolate( &
           This%HydConfig(iHyd)%cObsFile, &
           This%HydConfig(iHyd)%cHydFile, &
           This%HydConfig(iHyd)%cOutFile, &
           This%HydConfig(iHyd)%rThreshold, &
           This%HydConfig(iHyd)%cInsFile, &
           This%HydConfig(iHyd)%cPCFFile, &
           This%HydConfig(iHyd)%lWriteIns, &
           iStat)
      IF (iStat /= 0) THEN
        CALL LogLastMessage()
        CALL LogMessage('  Error processing '//TRIM(cHydName(iHyd)), &
             f_iWarn, cModName)
        iStat = 0  ! Continue with other types
        CYCLE
      END IF
    END DO

    ! Multi-layer target: post-process GW head output
    IF (This%lMultiLayer .AND. This%HydConfig(iGWHEAD)%lActive) THEN
      CALL LogMessage('Applying multi-layer target averaging...', &
           f_iInfo, cModName)
      CALL ApplyMultiLayerTarget(This, iStat)
      IF (iStat /= 0) THEN
        CALL LogLastMessage()
        CALL LogMessage('  Error in multi-layer processing', f_iWarn, cModName)
        iStat = 0
      END IF
    END IF

  END SUBROUTINE Run

  ! =====================================================================
  ! ApplyMultiLayerTarget - Post-process GW heads with T-weighted averaging
  !
  !   Reads the per-layer SMP output (from SMP2SMP) into memory once,
  !   builds an index of unique IDs, then for each observation well
  !   finds its layer records and computes weighted averages.
  !
  !   Output format matches GW_MultiLayer.out:
  !     Name(25)  Date(MM/DD/YYYY)  Time(HH:MM:SS)  Simulated(F11.2)
  !     T1..T4(4*F12.2)  NewTOS(F12.2)  NewBOS(F12.2)
  !
  !   PEST .ins format: WLT{well:05d}_{timestep:05d} columns 50:60
  ! =====================================================================
  SUBROUTINE ApplyMultiLayerTarget(This, iStat)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This
    INTEGER,             INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iInUnit = 192, iOutUnit = 193
    INTEGER, PARAMETER :: iInsUnit = 194, iPCFUnit = 195
    CHARACTER(LEN=500) :: cOutFile, cInsFile, cPCFFile, cLine
    CHARACTER(LEN=25)  :: cBaseID, cLayerID, cPrevID
    CHARACTER(LEN=30)  :: cTokens(5)
    INTEGER            :: iErr, iWell, iNLayers, iPos, iNCols
    INTEGER            :: iNAll, iRec, i, k, iSeqNum, iWellSeq
    REAL(8), ALLOCATABLE :: rLayerVals(:)
    REAL(8), ALLOCATABLE :: rLayerT(:)
    REAL(8)            :: rTOS, rBOS
    LOGICAL            :: lWriteIns
    ! In-memory record storage
    CHARACTER(LEN=25), ALLOCATABLE :: cAllIDs(:)
    CHARACTER(LEN=30), ALLOCATABLE :: cAllDates(:), cAllTimes(:)
    REAL(8),           ALLOCATABLE :: rAllVals(:)
    ! Contiguous ID index
    INTEGER            :: iNUniq
    CHARACTER(LEN=25), ALLOCATABLE :: cUniqIDs(:)
    INTEGER,           ALLOCATABLE :: iIDStart(:), iIDCount(:)
    ! Per-well layer lookup
    INTEGER,           ALLOCATABLE :: iLayerStart(:), iLayerCount(:)
    INTEGER            :: iNRec, iDay, iMon, iYear, iHH, iMM, iSS
    REAL(8)            :: rWeighted

    iStat = 0
    iNLayers = This%MultiLayer%GetNLayers()
    ALLOCATE(rLayerVals(iNLayers), rLayerT(MAX(iNLayers, 4)), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate layer values array', &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF
    rLayerT = 0.0D0

    ! Build output file paths (_ml suffix)
    cOutFile = TRIM(This%HydConfig(iGWHEAD)%cOutFile)
    iPos = SCAN(cOutFile, '.', BACK=.TRUE.)
    IF (iPos > 0) THEN
      cOutFile = cOutFile(1:iPos-1)//'_ml'//cOutFile(iPos:)
    ELSE
      cOutFile = TRIM(cOutFile)//'_ml'
    END IF

    lWriteIns = This%HydConfig(iGWHEAD)%lWriteIns
    IF (lWriteIns) THEN
      cInsFile = TRIM(This%HydConfig(iGWHEAD)%cInsFile)
      iPos = SCAN(cInsFile, '.', BACK=.TRUE.)
      IF (iPos > 0) THEN
        cInsFile = cInsFile(1:iPos-1)//'_ml'//cInsFile(iPos:)
      ELSE
        cInsFile = TRIM(cInsFile)//'_ml'
      END IF
      cPCFFile = TRIM(This%HydConfig(iGWHEAD)%cPCFFile)
      iPos = SCAN(cPCFFile, '.', BACK=.TRUE.)
      IF (iPos > 0) THEN
        cPCFFile = cPCFFile(1:iPos-1)//'_ml'//cPCFFile(iPos:)
      ELSE
        cPCFFile = TRIM(cPCFFile)//'_ml'
      END IF
    END IF

    ! ---- Step 1: Read entire per-layer SMP into memory ----
    OPEN(UNIT=iInUnit, FILE=This%HydConfig(iGWHEAD)%cOutFile, &
         STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open per-layer SMP: '// &
           TRIM(This%HydConfig(iGWHEAD)%cOutFile), f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Count lines
    iNAll = 0
    DO
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) > 0) iNAll = iNAll + 1
    END DO

    IF (iNAll == 0) THEN
      CLOSE(iInUnit)
      CALL SetLastMessage('Per-layer SMP file is empty', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ALLOCATE(cAllIDs(iNAll), cAllDates(iNAll), cAllTimes(iNAll), &
             rAllVals(iNAll), STAT=iErr)
    IF (iErr /= 0) THEN
      CLOSE(iInUnit)
      CALL SetLastMessage('Cannot allocate memory for SMP records ('// &
           TRIM(IntToText(iNAll))//' lines)', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Read all records
    REWIND(iInUnit)
    iRec = 0
    DO
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) == 0) CYCLE
      CALL TokenizeSMPLine(cLine, cTokens, iNCols)
      IF (iNCols < 4) CYCLE
      iRec = iRec + 1
      cAllIDs(iRec) = UpperCase(ADJUSTL(cTokens(1)))
      cAllDates(iRec) = cTokens(2)
      cAllTimes(iRec) = cTokens(3)
      READ(cTokens(4), *, IOSTAT=iErr) rAllVals(iRec)
      IF (iErr /= 0) rAllVals(iRec) = 0.0D0
    END DO
    iNAll = iRec
    CLOSE(iInUnit)

    CALL LogMessage('  Read '//TRIM(IntToText(iNAll))// &
         ' records from per-layer SMP into memory', f_iInfo, cModName)

    ! ---- Step 2: Build contiguous ID index ----
    iNUniq = 0
    cPrevID = ' '
    DO iRec = 1, iNAll
      IF (cAllIDs(iRec) /= cPrevID) THEN
        iNUniq = iNUniq + 1
        cPrevID = cAllIDs(iRec)
      END IF
    END DO

    ALLOCATE(cUniqIDs(iNUniq), iIDStart(iNUniq), iIDCount(iNUniq), STAT=iErr)
    IF (iErr /= 0) THEN
      iStat = -1; GOTO 900
    END IF

    iNUniq = 0
    cPrevID = ' '
    DO iRec = 1, iNAll
      IF (cAllIDs(iRec) /= cPrevID) THEN
        IF (iNUniq > 0) iIDCount(iNUniq) = iRec - iIDStart(iNUniq)
        iNUniq = iNUniq + 1
        cUniqIDs(iNUniq) = cAllIDs(iRec)
        iIDStart(iNUniq) = iRec
        cPrevID = cAllIDs(iRec)
      END IF
    END DO
    IF (iNUniq > 0) iIDCount(iNUniq) = iNAll - iIDStart(iNUniq) + 1

    CALL LogMessage('  '//TRIM(IntToText(iNUniq))//' unique IDs in index', &
         f_iInfo, cModName)

    ! ---- Step 3: Open output files ----
    OPEN(UNIT=iOutUnit, FILE=cOutFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open multi-layer output: '// &
           TRIM(cOutFile), f_iFatal, cModName)
      iStat = -1; GOTO 900
    END IF
    ! Header matching GW_MultiLayer.out format
    WRITE(iOutUnit, '(A)') &
         'Name                     Date        Time        Simulated' // &
         '         T1          T2          T3          T4' // &
         '      NewTOS      NewBOS'

    IF (lWriteIns) THEN
      OPEN(UNIT=iInsUnit, FILE=cInsFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iOutUnit); iStat = -1; GOTO 900
      END IF
      WRITE(iInsUnit, '(A)') 'pif #'
      WRITE(iInsUnit, '(A)') 'l1'

      OPEN(UNIT=iPCFUnit, FILE=cPCFFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iOutUnit); CLOSE(iInsUnit); iStat = -1; GOTO 900
      END IF
    END IF

    ! ---- Step 4: Process each observation well ----
    ALLOCATE(iLayerStart(iNLayers), iLayerCount(iNLayers))

    iWellSeq = 0
    DO iWell = 1, This%MultiLayer%GetNObs()
      cBaseID = This%MultiLayer%GetObsName(iWell)
      iWellSeq = iWellSeq + 1

      ! Find each layer's records in the index
      iLayerStart = 0
      iLayerCount = 0
      DO k = 1, iNLayers
        WRITE(cLayerID, '(A,A1,I0)') TRIM(cBaseID), '%', k
        cLayerID = UpperCase(cLayerID)
        DO i = 1, iNUniq
          IF (cUniqIDs(i) == cLayerID) THEN
            iLayerStart(k) = iIDStart(i)
            iLayerCount(k) = iIDCount(i)
            EXIT
          END IF
        END DO
      END DO

      ! Use layer 1's record count as the time step count
      iNRec = iLayerCount(1)
      IF (iNRec == 0) CYCLE

      ! Get layer transmissivities and screen TOS/BOS for this well
      CALL This%MultiLayer%GetWellLayerTransmissivities(iWell, rLayerT, rTOS, rBOS)

      ! Compute weighted averages for each time step
      iSeqNum = 0
      DO iRec = 1, iNRec
        ! Gather layer values at this time step
        DO k = 1, iNLayers
          IF (iLayerStart(k) > 0 .AND. iRec <= iLayerCount(k)) THEN
            rLayerVals(k) = rAllVals(iLayerStart(k) + iRec - 1)
          ELSE
            rLayerVals(k) = 0.0D0
          END IF
        END DO
        rWeighted = This%MultiLayer%WeightedAverage(iWell, rLayerVals)

        ! Parse date/time from layer 1 records
        CALL This%Interp%ParseDateStr(cAllDates(iLayerStart(1) + iRec - 1), &
             iDay, iMon, iYear, iErr)
        IF (iErr /= 0) CYCLE
        CALL This%Interp%ParseTimeStr(cAllTimes(iLayerStart(1) + iRec - 1), &
             iHH, iMM, iSS, iErr)
        IF (iErr /= 0) CYCLE

        ! Write GW_MultiLayer.out format line
        IF (This%Interp%iDateSpec == 1) THEN
          WRITE(iOutUnit, 100) cBaseID, iDay, iMon, iYear, &
               iHH, iMM, iSS, rWeighted, &
               (rLayerT(k), k=1,MIN(iNLayers,4)), &
               (0.0D0, k=iNLayers+1,4), &
               rTOS, rBOS
        ELSE
          WRITE(iOutUnit, 100) cBaseID, iMon, iDay, iYear, &
               iHH, iMM, iSS, rWeighted, &
               (rLayerT(k), k=1,MIN(iNLayers,4)), &
               (0.0D0, k=iNLayers+1,4), &
               rTOS, rBOS
        END IF
100     FORMAT(A25,1X,I2.2,'/',I2.2,'/',I4.4,2X,I2.2,':',I2.2,':',I2.2, &
             F11.2,4F12.2,2F12.2)

        ! Write PEST instruction and PCF files (WLT naming, columns 50:60)
        IF (lWriteIns) THEN
          iSeqNum = iSeqNum + 1
          WRITE(iInsUnit, 200) iWellSeq, iSeqNum
200       FORMAT('l1 [WLT',I5.5,'_',I5.5,']50:60')
          WRITE(iPCFUnit, 210) iWellSeq, iSeqNum, rWeighted
210       FORMAT('WLT',I5.5,'_',I5.5,'    ',1PG15.8)
        END IF
      END DO
    END DO

    CLOSE(iOutUnit)
    IF (lWriteIns) THEN
      CLOSE(iInsUnit)
      CLOSE(iPCFUnit)
    END IF

    IF (iStat == 0) THEN
      CALL LogMessage('  Multi-layer output written to: '//TRIM(cOutFile), &
           f_iInfo, cModName)
    END IF

    ! Cleanup
900 CONTINUE
    DEALLOCATE(iLayerStart, iLayerCount)
    DEALLOCATE(rLayerVals, rLayerT)
    DEALLOCATE(cAllIDs, cAllDates, cAllTimes, rAllVals)
    IF (ALLOCATED(cUniqIDs)) DEALLOCATE(cUniqIDs)
    IF (ALLOCATED(iIDStart)) DEALLOCATE(iIDStart)
    IF (ALLOCATED(iIDCount)) DEALLOCATE(iIDCount)

  END SUBROUTINE ApplyMultiLayerTarget

  ! =====================================================================
  ! Kill - Clean up
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This

    CALL This%HeadDiff%Kill()
    IF (This%lMultiLayer) CALL This%MultiLayer%Kill()
    IF (This%lModelMode) CALL This%HydReader%Kill()

  END SUBROUTINE Kill

END MODULE Class_IWFM2OBS
