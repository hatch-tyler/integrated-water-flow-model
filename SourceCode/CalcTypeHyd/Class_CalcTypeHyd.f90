!***********************************************************************
!  CalcTypeHyd - Class_CalcTypeHyd
!  Cluster-weighted type hydrograph computation
!
!  Computes de-meaned weighted dot product of water level time series
!  with fuzzy cluster weights, normalized by sum of non-zero weights.
!
!  Uses IWFM kernel GenericFileType for input, shared IWFM2OBS_Utilities
!  for date parsing and sorted string search.
!***********************************************************************
MODULE Class_CalcTypeHyd

  USE MessageLogger        , ONLY: MessageLoggerType , &
                                    f_iFatal       , &
                                    f_iWarn        , &
                                    f_iInfo
  USE GeneralUtilities     , ONLY: IntToText      , &
                                    UpperCase
  USE TimeSeriesUtilities  , ONLY: DayMonthYearToJulianDate , &
                                    JulianDateToDayMonthYear
  USE IOInterface          , ONLY: GenericFileType
  USE IWFM2OBS_Utilities   , ONLY: StripAndClean           , &
                                    ParseDateFromString     , &
                                    BinarySearchStr         , &
                                    SortStringsIndex

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CalcTypeHydType
  PUBLIC :: CalcTypeHyd_SetModuleLogger

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_CalcTypeHyd'
  TYPE(MessageLoggerType),POINTER,PRIVATE :: ModuleLogger => NULL()

  ! =====================================================================
  ! CalcTypeHydType - Manager for type hydrograph computation
  ! =====================================================================
  TYPE :: CalcTypeHydType
    INTEGER                         :: iNClus      = 0
    INTEGER                         :: iNClusWells = 0
    INTEGER                         :: iNHydro     = 0
    INTEGER                         :: iNVal       = 0
    INTEGER,  ALLOCATABLE           :: iClusID(:)
    REAL(8),  ALLOCATABLE           :: rClusWt(:,:)
    CHARACTER(LEN=25), ALLOCATABLE  :: cWellNames(:)
    ! Sorted well index for O(log N) lookup
    CHARACTER(LEN=25), ALLOCATABLE  :: cWellSorted(:)
    INTEGER, ALLOCATABLE            :: iWellOrder(:)
    REAL(8),  ALLOCATABLE           :: rMonAvg(:,:)
    REAL(8),  ALLOCATABLE           :: rMean(:)
    CHARACTER(LEN=10), ALLOCATABLE  :: cDateStr(:)
    CHARACTER(LEN=50)               :: cPBase     = ' '
    CHARACTER(LEN=10)               :: cStartDate = ' '
    CHARACTER(LEN=10)               :: cEndDate   = ' '
    CHARACTER(LEN=500)              :: cWtsFile   = ' '
    ! Averaging period configuration
    INTEGER                         :: iNAvgPer = 0
    INTEGER                         :: iActvMon(12) = 0
    INTEGER, ALLOCATABLE            :: iAvgMo(:)
    INTEGER, ALLOCATABLE            :: iAvgDay(:)
    LOGICAL                         :: lActive = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Run
    PROCEDURE, PASS :: Kill
  END TYPE CalcTypeHydType

CONTAINS


  ! -------------------------------------------------------------
  ! --- SET MODULE LOGGER
  ! -------------------------------------------------------------
  SUBROUTINE CalcTypeHyd_SetModuleLogger(Logger)
    TYPE(MessageLoggerType),TARGET,INTENT(IN) :: Logger

    ModuleLogger => Logger

  END SUBROUTINE CalcTypeHyd_SetModuleLogger


  ! =====================================================================
  ! StripHashComment - Remove text after '#' (CalcTypeHyd inline comments)
  !   Cannot use StripAndClean (which strips at '/') because dates
  !   like 01/01/1985 contain '/'.
  ! =====================================================================
  SUBROUTINE StripHashComment(cLine, cResult)
    CHARACTER(LEN=*), INTENT(IN)  :: cLine
    CHARACTER(LEN=*), INTENT(OUT) :: cResult
    INTEGER :: iPos

    cResult = cLine
    iPos = INDEX(cResult, '#')
    IF (iPos > 1) cResult = cResult(1:iPos-1)
    ! Also strip tab characters (common in CalcTypeHyd .in files)
    DO iPos = 1, LEN_TRIM(cResult)
      IF (ICHAR(cResult(iPos:iPos)) == 9) cResult(iPos:iPos) = ' '
    END DO
    cResult = ADJUSTL(TRIM(cResult))
  END SUBROUTINE StripHashComment


  ! =====================================================================
  ! New - Read input file and initialize
  ! =====================================================================
  SUBROUTINE New(This, cInputFile, iStat)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),       INTENT(IN)    :: cInputFile
    INTEGER,                INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iInUnit = 180
    CHARACTER(LEN=500) :: cLine, cClean
    CHARACTER(LEN=500) :: cWLFile, cWtsFile
    INTEGER :: iErr, i, j, k, iMM, iDD, iYYYY, iNMon
    INTEGER :: iStartM, iEndM
    INTEGER, ALLOCATABLE :: iMon(:)
    INTEGER :: iActualRows
    CHARACTER(LEN=25) :: cDumNm
    REAL(8) :: rTest

    iStat = 0

    ! ================================================================
    ! Read control file (uses raw I/O — CalcTypeHyd format uses #
    ! inline comments which conflict with IWFM's / comment convention)
    ! ================================================================
    OPEN(UNIT=iInUnit, FILE=cInputFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot open input file: '//TRIM(cInputFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) cWLFile

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) cWtsFile
    This%cWtsFile = cWtsFile

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) This%iNClus

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) This%iNClusWells

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) This%iNHydro

    IF (This%iNClus <= 0 .OR. This%iNClusWells <= 0 .OR. This%iNHydro <= 0) THEN
      CALL ModuleLogger%SetLastMessage('Invalid NCLUS, NCLUSWELLS, or NHYDRO in input file', &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ALLOCATE(This%iClusID(This%iNHydro))
    DO i = 1, This%iNHydro
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      CALL StripHashComment(cLine, cClean)
      READ(cClean, *) This%iClusID(i)
      IF (This%iClusID(i) < 1 .OR. This%iClusID(i) > This%iNClus) THEN
        CALL ModuleLogger%SetLastMessage('Cluster ID '//TRIM(IntToText(This%iClusID(i)))// &
             ' is outside range 1-'//TRIM(IntToText(This%iNClus)), &
             f_iFatal, cModName)
        iStat = -1; RETURN
      END IF
    END DO

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    This%cStartDate = cClean(1:10)

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    This%cEndDate = cClean(1:10)

    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) This%cPBase

    ! Read averaging period configuration
    READ(iInUnit, '(A)', IOSTAT=iErr) cLine
    CALL StripHashComment(cLine, cClean)
    READ(cClean, *) This%iNAvgPer
    ALLOCATE(This%iAvgMo(This%iNAvgPer), This%iAvgDay(This%iNAvgPer))
    This%iActvMon = 0
    DO i = 1, This%iNAvgPer
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      CALL StripHashComment(cLine, cClean)
      READ(cClean, *) iNMon
      ALLOCATE(iMon(iNMon))
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      CALL StripHashComment(cLine, cClean)
      READ(cClean, *) (iMon(j), j=1, iNMon)
      DO j = 1, iNMon
        IF (iMon(j) >= 1 .AND. iMon(j) <= 12) This%iActvMon(iMon(j)) = i
      END DO
      DEALLOCATE(iMon)
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      CALL StripHashComment(cLine, cClean)
      READ(cClean, *) This%iAvgMo(i), This%iAvgDay(i)
    END DO
    CLOSE(iInUnit)

    CALL ModuleLogger%LogMessage('  Start='//TRIM(This%cStartDate)//' End='//TRIM(This%cEndDate)// &
         '  Clusters='//TRIM(IntToText(This%iNClus))// &
         '  Wells='//TRIM(IntToText(This%iNClusWells))// &
         '  Hydros='//TRIM(IntToText(This%iNHydro)), f_iInfo, cModName)

    ! ================================================================
    ! Read cluster weights file
    ! ================================================================
    CALL ModuleLogger%LogMessage('Reading cluster weights: '//TRIM(cWtsFile), f_iInfo, cModName)
    ALLOCATE(This%rClusWt(This%iNClusWells, This%iNClus), &
             This%cWellNames(This%iNClusWells), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot allocate cluster weight arrays', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    OPEN(UNIT=180, FILE=TRIM(cWtsFile), STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot open weights file: '//TRIM(cWtsFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Auto-detect header: if second token is numeric, it's data
    READ(180, '(A)', IOSTAT=iErr) cLine
    IF (iErr == 0) THEN
      READ(cLine, *, IOSTAT=iErr) cDumNm, rTest
      IF (iErr == 0) THEN
        BACKSPACE(180)
      ELSE
        CALL ModuleLogger%LogMessage('  Skipped header line in weights file', f_iInfo, cModName)
      END IF
    END IF

    iActualRows = 0
    DO i = 1, This%iNClusWells
      READ(180, *, IOSTAT=iErr) This%cWellNames(i), &
           (This%rClusWt(i, j), j=1, This%iNClus)
      IF (iErr /= 0) THEN
        IF (i == 1) THEN
          CALL ModuleLogger%SetLastMessage('Cannot read first well from weights file: '// &
               TRIM(cWtsFile), f_iFatal, cModName)
          CLOSE(180); iStat = -1; RETURN
        END IF
        CALL ModuleLogger%LogMessage('  WARNING: Weights file has only '// &
             TRIM(IntToText(i-1))//' rows (expected '// &
             TRIM(IntToText(This%iNClusWells))//')', f_iWarn, cModName)
        This%iNClusWells = i - 1
        EXIT
      END IF
      iActualRows = i
    END DO
    CLOSE(180)

    CALL ModuleLogger%LogMessage('  Read '//TRIM(IntToText(iActualRows))// &
         ' wells from weights file', f_iInfo, cModName)

    ! Build sorted well index for O(log N) lookup
    ALLOCATE(This%cWellSorted(This%iNClusWells), &
             This%iWellOrder(This%iNClusWells))
    DO i = 1, This%iNClusWells
      This%cWellSorted(i) = UpperCase(ADJUSTL(This%cWellNames(i)))
      This%iWellOrder(i) = i
    END DO
    IF (This%iNClusWells > 1) &
      CALL SortStringsIndex(This%cWellSorted, This%iWellOrder, 1, This%iNClusWells)

    ! ================================================================
    ! Set up date arrays using month arithmetic
    ! ================================================================
    CALL ModuleLogger%LogMessage('Setting up date arrays...', f_iInfo, cModName)
    BLOCK
      INTEGER :: iStartMon, iStartYr, iEndMon, iEndYr, iE
      CALL ParseDateFromString(This%cStartDate, 2, iDD, iStartMon, iStartYr, iE)
      CALL ParseDateFromString(This%cEndDate, 2, iDD, iEndMon, iEndYr, iE)
      iStartM = (iStartYr * 12 + iStartMon)
      iEndM   = (iEndYr * 12 + iEndMon)
      This%iNVal = iEndM - iStartM + 1
    END BLOCK

    IF (This%iNVal <= 0) THEN
      CALL ModuleLogger%SetLastMessage('End date must be after start date', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ALLOCATE(This%cDateStr(This%iNVal), STAT=iErr)
    ALLOCATE(This%rMonAvg(This%iNVal, This%iNClusWells), STAT=iErr)
    ALLOCATE(This%rMean(This%iNClusWells), STAT=iErr)
    This%rMonAvg = -9999.0D0

    ! Fill date strings
    BLOCK
      INTEGER :: iStartMon2, iStartYr2, iE2
      CALL ParseDateFromString(This%cStartDate, 2, iDD, iStartMon2, iStartYr2, iE2)
      iMM = iStartMon2
      iYYYY = iStartYr2
    END BLOCK
    DO i = 1, This%iNVal
      IF (i > 1) THEN
        iMM = iMM + 1
        IF (iMM > 12) THEN
          iMM = 1
          iYYYY = iYYYY + 1
        END IF
      END IF
      iDD = 15  ! Default
      IF (This%iActvMon(iMM) > 0) iDD = This%iAvgDay(This%iActvMon(iMM))
      WRITE(This%cDateStr(i), '(I2.2,A1,I2.2,A1,I4.4)') iMM, '/', iDD, '/', iYYYY
    END DO

    ! ================================================================
    ! Read water level data
    ! ================================================================
    CALL ModuleLogger%LogMessage('Reading water levels: '//TRIM(cWLFile), f_iInfo, cModName)
    CALL ReadWaterLevels(This, cWLFile, iStartM, iEndM, iStat)
    IF (iStat /= 0) RETURN

    ! Calculate means
    CALL ComputeMeans(This)
    This%lActive = .TRUE.

  END SUBROUTINE New


  ! =====================================================================
  ! ReadWaterLevels - Read simulated water level data from SMP file
  !   Reads fixed-format (A25,A12,A12,A11) matching GW_Sim_ml_continuous.smp
  !   Uses sorted binary search for O(log N) well lookup
  ! =====================================================================
  SUBROUTINE ReadWaterLevels(This, cWLFile, iStartM, iEndM, iStat)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),       INTENT(IN)    :: cWLFile
    INTEGER,                INTENT(IN)    :: iStartM, iEndM
    INTEGER,                INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 180
    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cWellNm, cWellUC
    CHARACTER(LEN=12)  :: cDateStr, cDumStr
    CHARACTER(LEN=11)  :: cWLStr
    INTEGER :: iErr, i, k, kOrig, iDate, iMM, iYY, iAvgPer
    REAL(8) :: rWL, rTest
    INTEGER, ALLOCATABLE :: iDateID(:), iCnt(:,:)
    INTEGER :: iRecordsRead, iRecordsMatched, iWellsMatched, iSortPos

    iStat = 0

    ! Build date index mapping (absolute month → local index)
    ALLOCATE(iDateID(iEndM), STAT=iErr)
    iDateID = 0
    k = iStartM
    DO i = 1, This%iNVal
      IF (k >= 1 .AND. k <= iEndM) iDateID(k) = i
      k = k + 1
    END DO

    ALLOCATE(iCnt(This%iNVal, This%iNClusWells))
    iCnt = 0

    OPEN(UNIT=iUnit, FILE=cWLFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot open water level file: '//TRIM(cWLFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Auto-detect header: if 4th token is numeric, it's data
    READ(iUnit, '(A)', IOSTAT=iErr) cLine
    IF (iErr == 0) THEN
      BLOCK
        CHARACTER(LEN=25) :: cD1
        CHARACTER(LEN=12) :: cD2, cD3
        INTEGER :: iChk
        READ(cLine, *, IOSTAT=iChk) cD1, cD2, cD3, rTest
        IF (iChk == 0) THEN
          BACKSPACE(iUnit)
        ELSE
          CALL ModuleLogger%LogMessage('  Skipped header line in SMP file', f_iInfo, cModName)
        END IF
      END BLOCK
    END IF

    ! Read data: fixed-format (A25,A12,A12,A11)
    iRecordsRead = 0
    iRecordsMatched = 0
    DO
      READ(iUnit, '(A25,A12,A12,A11)', IOSTAT=iErr) cWellNm, cDateStr, cDumStr, cWLStr
      IF (iErr /= 0) EXIT
      iRecordsRead = iRecordsRead + 1

      ! Check active months
      READ(cDateStr(1:2), *, IOSTAT=iErr) iMM
      IF (iErr /= 0) CYCLE
      IF (iMM < 1 .OR. iMM > 12) CYCLE
      IF (This%iActvMon(iMM) == 0) CYCLE

      ! Remap to representative month/day for this averaging period
      iAvgPer = This%iActvMon(iMM)
      WRITE(cDateStr(1:2), '(I2.2)') This%iAvgMo(iAvgPer)
      WRITE(cDateStr(4:5), '(I2.2)') This%iAvgDay(iAvgPer)

      ! Compute absolute month index for date
      READ(cDateStr(1:2), *) iMM
      READ(cDateStr(7:10), *) iYY
      iDate = iYY * 12 + iMM
      IF (iDate < iStartM .OR. iDate > iEndM) CYCLE
      IF (iDateID(iDate) == 0) CYCLE

      ! Find well via sorted binary search — O(log N)
      cWellUC = UpperCase(ADJUSTL(cWellNm))
      iSortPos = BinarySearchStr(This%cWellSorted, This%iNClusWells, cWellUC)
      IF (iSortPos <= 0) CYCLE
      kOrig = This%iWellOrder(iSortPos)

      ! Parse water level value
      READ(cWLStr, *, IOSTAT=iErr) rWL
      IF (iErr /= 0) CYCLE

      ! Accumulate for averaging
      i = iDateID(iDate)
      iRecordsMatched = iRecordsMatched + 1
      IF (This%rMonAvg(i, kOrig) < -9000.0D0) THEN
        This%rMonAvg(i, kOrig) = rWL
        iCnt(i, kOrig) = 1
      ELSE
        This%rMonAvg(i, kOrig) = This%rMonAvg(i, kOrig) + rWL
        iCnt(i, kOrig) = iCnt(i, kOrig) + 1
      END IF
    END DO
    CLOSE(iUnit)

    ! Compute averages and count matched wells
    iWellsMatched = 0
    DO k = 1, This%iNClusWells
      BLOCK
        LOGICAL :: lHasData
        lHasData = .FALSE.
        DO i = 1, This%iNVal
          IF (iCnt(i, k) > 0) THEN
            This%rMonAvg(i, k) = This%rMonAvg(i, k) / DBLE(iCnt(i, k))
            lHasData = .TRUE.
          END IF
        END DO
        IF (lHasData) iWellsMatched = iWellsMatched + 1
      END BLOCK
    END DO

    CALL ModuleLogger%LogMessage('  Read '//TRIM(IntToText(iRecordsRead))//' records, '// &
         TRIM(IntToText(iRecordsMatched))//' matched, '// &
         TRIM(IntToText(iWellsMatched))//' of '// &
         TRIM(IntToText(This%iNClusWells))//' wells have data', f_iInfo, cModName)

    IF (iWellsMatched == 0) THEN
      CALL ModuleLogger%SetLastMessage('No wells matched between water level file and cluster '// &
           'weights file. Check that well names are consistent.', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    DEALLOCATE(iDateID, iCnt)

  END SUBROUTINE ReadWaterLevels


  ! =====================================================================
  ! ComputeMeans - Calculate mean water level per well
  ! =====================================================================
  SUBROUTINE ComputeMeans(This)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This

    INTEGER :: i, j, iCount
    REAL(8) :: rSum

    DO j = 1, This%iNClusWells
      rSum   = 0.0D0
      iCount = 0
      DO i = 1, This%iNVal
        IF (This%rMonAvg(i, j) > -9000.0D0) THEN
          rSum   = rSum + This%rMonAvg(i, j)
          iCount = iCount + 1
        END IF
      END DO
      IF (iCount > 0) THEN
        This%rMean(j) = rSum / DBLE(iCount)
      ELSE
        This%rMean(j) = 0.0D0
      END IF
    END DO

  END SUBROUTINE ComputeMeans


  ! =====================================================================
  ! Run - Generate type hydrographs and output files
  ! =====================================================================
  SUBROUTINE Run(This, iStat)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This
    INTEGER,               INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iOutUnit = 181
    INTEGER :: iErr, n, i, j, iCls
    REAL(8) :: rSumProduct, rNzWtSum, rTypeHydro
    CHARACTER(LEN=2)  :: cClusStr
    CHARACTER(LEN=50) :: cHeader, cOutFile
    CHARACTER(LEN=20) :: cPstNam
    CHARACTER(LEN=4)  :: cIdStr
    REAL(8), ALLOCATABLE :: rNonZeroWts(:,:), rDeMeaned(:,:)
    INTEGER :: iValid, iMissing, k

    iStat = 0

    IF (.NOT. This%lActive) THEN
      CALL ModuleLogger%SetLastMessage('CalcTypeHyd not initialized', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ALLOCATE(rNonZeroWts(This%iNVal, This%iNClusWells), &
             rDeMeaned(This%iNVal, This%iNClusWells), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL ModuleLogger%SetLastMessage('Cannot allocate work arrays', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Report data coverage
    iValid = 0; iMissing = 0
    DO i = 1, This%iNVal
      DO j = 1, This%iNClusWells
        IF (This%rMonAvg(i, j) > -9000.0D0) THEN
          iValid = iValid + 1
        ELSE
          iMissing = iMissing + 1
        END IF
      END DO
    END DO
    CALL ModuleLogger%LogMessage('  Data coverage: '//TRIM(IntToText(iValid))//' valid, '// &
         TRIM(IntToText(iMissing))//' missing ('// &
         TRIM(IntToText(This%iNVal))//' timesteps x '// &
         TRIM(IntToText(This%iNClusWells))//' wells)', f_iInfo, cModName)

    CALL ModuleLogger%LogMessage('Generating type hydrographs...', f_iInfo, cModName)

    DO n = 1, This%iNHydro
      iCls = This%iClusID(n)
      WRITE(cClusStr, '(I2)') iCls

      ! Build non-zero weight matrix
      rNonZeroWts = 0.0D0
      DO i = 1, This%iNVal
        DO j = 1, This%iNClusWells
          IF (This%rMonAvg(i, j) > -9000.0D0) THEN
            rNonZeroWts(i, j) = This%rClusWt(j, iCls)
          END IF
        END DO
      END DO

      ! Build de-meaned water level matrix
      rDeMeaned = 0.0D0
      DO i = 1, This%iNVal
        DO j = 1, This%iNClusWells
          IF (This%rMonAvg(i, j) > -9000.0D0) THEN
            rDeMeaned(i, j) = This%rMonAvg(i, j) - This%rMean(j)
          END IF
        END DO
      END DO

      ! Set up output file name
      k = SCAN(This%cWtsFile, '.')
      IF (k > 0) THEN
        cHeader = 'sim_'//TRIM(ADJUSTL(This%cWtsFile(1:k-1)))// &
                  '_cls'//TRIM(ADJUSTL(cClusStr))
      ELSE
        cHeader = 'sim_cls'//TRIM(ADJUSTL(cClusStr))
      END IF
      cOutFile = TRIM(ADJUSTL(cHeader))//'.out'

      OPEN(UNIT=iOutUnit, FILE=cOutFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL ModuleLogger%SetLastMessage('Cannot open output file: '//TRIM(cOutFile), &
             f_iFatal, cModName)
        iStat = -1; RETURN
      END IF
      WRITE(iOutUnit, '(A14,A12,A40)') 'PEST_NAME', 'DATE', TRIM(cHeader)

      ! Calculate and write type hydrograph
      DO i = 1, This%iNVal
        rSumProduct = 0.0D0
        DO j = 1, This%iNClusWells
          rSumProduct = rSumProduct + This%rClusWt(j, iCls) * rDeMeaned(i, j)
        END DO

        rNzWtSum = 0.0D0
        DO j = 1, This%iNClusWells
          rNzWtSum = rNzWtSum + rNonZeroWts(i, j)
        END DO

        IF (rNzWtSum /= 0.0D0) THEN
          rTypeHydro = rSumProduct / rNzWtSum
          WRITE(cIdStr, '(I0)') i
          cPstNam = TRIM(ADJUSTL(This%cPBase))// &
                    TRIM(ADJUSTL(cClusStr))//'_'// &
                    TRIM(ADJUSTL(cIdStr))
          WRITE(iOutUnit, '(A14,A12,F20.6)') cPstNam, This%cDateStr(i), rTypeHydro
        END IF
      END DO

      CLOSE(iOutUnit)
      CALL ModuleLogger%LogMessage('  Wrote '//TRIM(cOutFile), f_iInfo, cModName)
    END DO

    DEALLOCATE(rNonZeroWts, rDeMeaned)
    CALL ModuleLogger%LogMessage('Type hydrograph generation complete.', f_iInfo, cModName)

  END SUBROUTINE Run


  ! =====================================================================
  ! Kill - Deallocate all arrays
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This

    IF (ALLOCATED(This%iClusID))     DEALLOCATE(This%iClusID)
    IF (ALLOCATED(This%rClusWt))     DEALLOCATE(This%rClusWt)
    IF (ALLOCATED(This%cWellNames))  DEALLOCATE(This%cWellNames)
    IF (ALLOCATED(This%cWellSorted)) DEALLOCATE(This%cWellSorted)
    IF (ALLOCATED(This%iWellOrder))  DEALLOCATE(This%iWellOrder)
    IF (ALLOCATED(This%rMonAvg))     DEALLOCATE(This%rMonAvg)
    IF (ALLOCATED(This%rMean))       DEALLOCATE(This%rMean)
    IF (ALLOCATED(This%cDateStr))    DEALLOCATE(This%cDateStr)
    IF (ALLOCATED(This%iAvgMo))      DEALLOCATE(This%iAvgMo)
    IF (ALLOCATED(This%iAvgDay))     DEALLOCATE(This%iAvgDay)
    This%lActive = .FALSE.

  END SUBROUTINE Kill

END MODULE Class_CalcTypeHyd
