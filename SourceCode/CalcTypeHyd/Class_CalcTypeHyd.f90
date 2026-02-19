!***********************************************************************
!  CalcTypeHyd - Class_CalcTypeHyd
!  Cluster-weighted type hydrograph computation
!
!  Ported from CalcTypeHyd.for (Fortran 77 fixed-form).
!  Modernized: Fortran 90, modules, allocatable arrays, IWFM kernel
!  date utilities.
!
!  Algorithm: For each cluster, compute de-meaned weighted dot product
!  of water level time series with cluster weights, normalized by
!  the sum of non-zero weights at each time step.
!***********************************************************************
MODULE Class_CalcTypeHyd

  USE MessageLogger    , ONLY: SetLastMessage , &
                               LogMessage     , &
                               f_iFatal       , &
                               f_iInfo
  USE GeneralUtilities , ONLY: IntToText

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CalcTypeHydType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_CalcTypeHyd'

  ! =====================================================================
  ! CalcTypeHydType - Manager for type hydrograph computation
  ! =====================================================================
  TYPE :: CalcTypeHydType
    INTEGER                         :: iNClus      = 0   ! Number of clusters
    INTEGER                         :: iNClusWells = 0   ! Number of wells
    INTEGER                         :: iNHydro     = 0   ! Number of type hydrographs to generate
    INTEGER                         :: iNVal       = 0   ! Number of output time steps
    INTEGER,  ALLOCATABLE           :: iClusID(:)         ! Cluster IDs to generate (nhydro)
    REAL(8),  ALLOCATABLE           :: rClusWt(:,:)       ! Cluster weights (ncluswells, nclus)
    CHARACTER(LEN=25), ALLOCATABLE  :: cWellNames(:)      ! Well names (ncluswells)
    REAL(8),  ALLOCATABLE           :: rMonAvg(:,:)       ! Monthly avg WL (nval, ncluswells)
    REAL(8),  ALLOCATABLE           :: rMean(:)           ! Mean WL per well (ncluswells)
    CHARACTER(LEN=10), ALLOCATABLE  :: cDateStr(:)        ! Output date strings (nval)
    CHARACTER(LEN=50)               :: cPBase     = ' '   ! PEST parameter base name
    CHARACTER(LEN=10)               :: cStartDate = ' '
    CHARACTER(LEN=10)               :: cEndDate   = ' '
    CHARACTER(LEN=50)               :: cWtsFile   = ' '
    ! Averaging period configuration
    INTEGER                         :: iNAvgPer = 0
    INTEGER                         :: iActvMon(12) = 0   ! Month -> averaging period map
    INTEGER, ALLOCATABLE            :: iAvgMo(:)          ! Representative month per period
    INTEGER, ALLOCATABLE            :: iAvgDay(:)         ! Representative day per period
    LOGICAL                         :: lActive = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Run
    PROCEDURE, PASS :: Kill
  END TYPE CalcTypeHydType

CONTAINS

  ! =====================================================================
  ! MonthRange - Count months between two dates (MM/DD/YYYY format)
  ! =====================================================================
  FUNCTION MonthRange(cDateMin, cDateMax) RESULT(iRange)
    CHARACTER(LEN=*), INTENT(IN) :: cDateMin, cDateMax
    INTEGER :: iRange
    INTEGER :: iStartMo, iEndMo, iInterMo
    INTEGER :: iYearMin, iMonMin, iYearMax, iMonMax

    READ(cDateMin(1:2), *) iMonMin
    READ(cDateMin(7:10), *) iYearMin
    READ(cDateMax(1:2), *) iMonMax
    READ(cDateMax(7:10), *) iYearMax

    iStartMo = 12 - iMonMin
    iInterMo = ((iYearMax - iYearMin) - 1) * 12
    iEndMo   = iMonMax

    iRange = iStartMo + iInterMo + iEndMo

  END FUNCTION MonthRange

  ! =====================================================================
  ! Date2Str - Format month/day/year to MM/DD/YYYY string
  ! =====================================================================
  FUNCTION Date2Str(iMM, iDD, iYYYY) RESULT(cDate)
    INTEGER, INTENT(IN) :: iMM, iDD, iYYYY
    CHARACTER(LEN=10) :: cDate

    WRITE(cDate, '(I2.2,A1,I2.2,A1,I4.4)') iMM, '/', iDD, '/', iYYYY

  END FUNCTION Date2Str

  ! =====================================================================
  ! New - Read input file and initialize
  ! =====================================================================
  SUBROUTINE New(This, cInputFile, iStat)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),       INTENT(IN)    :: cInputFile
    INTEGER,                INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 180
    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=50)  :: cWLFile, cWtsFile
    INTEGER :: iErr, i, j, k, iMM, iDD, iYYYY, iNMon, iDum
    INTEGER :: iStartM, iEndM
    CHARACTER(LEN=10) :: cRefDate
    INTEGER, ALLOCATABLE :: iMon(:)

    iStat = 0

    ! Open input file
    OPEN(UNIT=iUnit, FILE=cInputFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open input file: '//TRIM(cInputFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    READ(iUnit, *, IOSTAT=iErr) cWLFile
    READ(iUnit, *, IOSTAT=iErr) cWtsFile
    This%cWtsFile = cWtsFile
    READ(iUnit, *, IOSTAT=iErr) This%iNClus
    READ(iUnit, *, IOSTAT=iErr) This%iNClusWells
    READ(iUnit, *, IOSTAT=iErr) This%iNHydro

    ALLOCATE(This%iClusID(This%iNHydro), STAT=iErr)
    DO i = 1, This%iNHydro
      READ(iUnit, *, IOSTAT=iErr) This%iClusID(i)
    END DO

    READ(iUnit, '(A10)', IOSTAT=iErr) This%cStartDate
    READ(iUnit, '(A10)', IOSTAT=iErr) This%cEndDate
    READ(iUnit, *, IOSTAT=iErr) This%cPBase

    ! Read averaging period configuration
    READ(iUnit, *, IOSTAT=iErr) This%iNAvgPer
    ALLOCATE(This%iAvgMo(This%iNAvgPer), This%iAvgDay(This%iNAvgPer))
    This%iActvMon = 0
    DO i = 1, This%iNAvgPer
      READ(iUnit, *, IOSTAT=iErr) iNMon
      ALLOCATE(iMon(iNMon))
      READ(iUnit, *) (iMon(j), j=1, iNMon)
      DO j = 1, iNMon
        This%iActvMon(iMon(j)) = i
      END DO
      DEALLOCATE(iMon)
      READ(iUnit, *) This%iAvgMo(i), This%iAvgDay(i)
    END DO
    CLOSE(iUnit)

    ! Read cluster weights file
    CALL LogMessage('Reading cluster weights...', f_iInfo, cModName)
    ALLOCATE(This%rClusWt(This%iNClusWells, This%iNClus), &
             This%cWellNames(This%iNClusWells), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate cluster weight arrays', &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    OPEN(UNIT=iUnit, FILE=cWtsFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open weights file: '//TRIM(cWtsFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    READ(iUnit, *, IOSTAT=iErr)  ! Skip header
    DO i = 1, This%iNClusWells
      READ(iUnit, *, IOSTAT=iErr) This%cWellNames(i), &
           (This%rClusWt(i, j), j=1, This%iNClus)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading weight for well '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO
    CLOSE(iUnit)

    ! Set up date arrays
    CALL LogMessage('Setting up date arrays...', f_iInfo, cModName)
    cRefDate = '01/01/1900'
    iStartM = MonthRange(cRefDate, This%cStartDate)
    iEndM   = MonthRange(cRefDate, This%cEndDate)
    This%iNVal = MonthRange(This%cStartDate, This%cEndDate) + 1

    ALLOCATE(This%cDateStr(This%iNVal), STAT=iErr)
    ALLOCATE(This%rMonAvg(This%iNVal, This%iNClusWells), STAT=iErr)
    ALLOCATE(This%rMean(This%iNClusWells), STAT=iErr)
    This%rMonAvg = -9999.0D0

    ! Fill date arrays
    READ(This%cStartDate(1:2), *) iMM
    READ(This%cStartDate(7:10), *) iYYYY
    DO i = 1, This%iNVal
      IF (i > 1 .AND. iMM > 12) THEN
        iMM = 1
        iYYYY = iYYYY + 1
      END IF
      iDD = 15  ! Default day
      IF (This%iActvMon(iMM) > 0) THEN
        iDD = This%iAvgDay(This%iActvMon(iMM))
      END IF
      This%cDateStr(i) = Date2Str(iMM, iDD, iYYYY)
      iMM = iMM + 1
    END DO

    ! Read water level data
    CALL LogMessage('Reading water level data...', f_iInfo, cModName)
    CALL ReadWaterLevels(This, cWLFile, cRefDate, iStartM, iEndM, iStat)
    IF (iStat /= 0) RETURN

    ! Calculate means
    CALL LogMessage('Calculating means...', f_iInfo, cModName)
    CALL ComputeMeans(This)

    This%lActive = .TRUE.

  END SUBROUTINE New

  ! =====================================================================
  ! ReadWaterLevels - Read simulated water level data
  ! =====================================================================
  SUBROUTINE ReadWaterLevels(This, cWLFile, cRefDate, iStartM, iEndM, iStat)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),       INTENT(IN)    :: cWLFile, cRefDate
    INTEGER,                INTENT(IN)    :: iStartM, iEndM
    INTEGER,                INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 180
    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cWellNm
    CHARACTER(LEN=12)  :: cDateStr
    CHARACTER(LEN=12)  :: cDumStr
    CHARACTER(LEN=11)  :: cWLStr
    INTEGER :: iErr, i, k, iDate, iMM, iAvgPer
    REAL(8) :: rWL
    INTEGER, ALLOCATABLE :: iDateID(:), iCnt(:,:)

    iStat = 0

    ! Build date index mapping
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
      CALL SetLastMessage('Cannot open water level file: '//TRIM(cWLFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Skip header (simulated data format)
    READ(iUnit, *, IOSTAT=iErr)

    ! Read data: well_name date time value
    DO
      READ(iUnit, '(A25,A12,A12,A11)', IOSTAT=iErr) cWellNm, cDateStr, cDumStr, cWLStr
      IF (iErr /= 0) EXIT

      ! Check active months
      READ(cDateStr(1:2), *, IOSTAT=iErr) iMM
      IF (iErr /= 0) CYCLE
      IF (This%iActvMon(iMM) == 0) CYCLE

      ! Remap to representative month/day
      iAvgPer = This%iActvMon(iMM)
      WRITE(cDateStr(1:2), '(I2.2)') This%iAvgMo(iAvgPer)
      WRITE(cDateStr(4:5), '(I2.2)') This%iAvgDay(iAvgPer)

      iDate = MonthRange(cRefDate, cDateStr)
      IF (iDate < iStartM .OR. iDate > iEndM) CYCLE
      IF (iDateID(iDate) == 0) CYCLE

      ! Find well index
      k = FindWell(This%cWellNames, This%iNClusWells, cWellNm)
      IF (k <= 0) CYCLE

      ! Parse water level value
      READ(cWLStr, *, IOSTAT=iErr) rWL
      IF (iErr /= 0) CYCLE

      ! Accumulate for averaging
      i = iDateID(iDate)
      IF (This%rMonAvg(i, k) < -9000.0D0) THEN
        This%rMonAvg(i, k) = rWL
        iCnt(i, k) = 1
      ELSE
        This%rMonAvg(i, k) = This%rMonAvg(i, k) + rWL
        iCnt(i, k) = iCnt(i, k) + 1
      END IF
    END DO
    CLOSE(iUnit)

    ! Compute averages
    DO k = 1, This%iNClusWells
      DO i = 1, This%iNVal
        IF (iCnt(i, k) > 0) THEN
          This%rMonAvg(i, k) = This%rMonAvg(i, k) / DBLE(iCnt(i, k))
        END IF
      END DO
    END DO

    DEALLOCATE(iDateID, iCnt)

  END SUBROUTINE ReadWaterLevels

  ! =====================================================================
  ! FindWell - Find well name in array, return index (0 if not found)
  ! =====================================================================
  FUNCTION FindWell(cNames, iN, cTarget) RESULT(iIdx)
    CHARACTER(LEN=25), INTENT(IN) :: cNames(:)
    INTEGER,           INTENT(IN) :: iN
    CHARACTER(LEN=*),  INTENT(IN) :: cTarget
    INTEGER :: iIdx

    INTEGER :: i
    CHARACTER(LEN=25) :: cTgt

    cTgt = ADJUSTL(cTarget)
    iIdx = 0
    DO i = 1, iN
      IF (ADJUSTL(cNames(i)) == cTgt) THEN
        iIdx = i
        RETURN
      END IF
    END DO

  END FUNCTION FindWell

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
  ! Run - Generate type hydrographs and PEST files
  ! =====================================================================
  SUBROUTINE Run(This, iStat)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This
    INTEGER,               INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iOutUnit = 181, iInsUnit = 182
    INTEGER :: iErr, n, i, j, k, iCls
    REAL(8) :: rSumProduct, rNzWtSum, rTypeHydro
    CHARACTER(LEN=2)  :: cClusStr
    CHARACTER(LEN=50) :: cHeader, cOutFile, cInsFile
    CHARACTER(LEN=20) :: cPstNam
    CHARACTER(LEN=4)  :: cIdStr
    REAL(8), ALLOCATABLE :: rNonZeroWts(:,:), rDeMeaned(:,:)

    iStat = 0

    IF (.NOT. This%lActive) THEN
      CALL SetLastMessage('CalcTypeHyd not initialized', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ALLOCATE(rNonZeroWts(This%iNVal, This%iNClusWells), &
             rDeMeaned(This%iNVal, This%iNClusWells), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate work arrays', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    CALL LogMessage('Generating type hydrographs...', f_iInfo, cModName)

    DO n = 1, This%iNHydro
      iCls = This%iClusID(n)
      WRITE(cClusStr, '(I2)') iCls

      CALL LogMessage('  Hydrograph '//TRIM(IntToText(n))//'/'// &
           TRIM(IntToText(This%iNHydro)), f_iInfo, cModName)

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

      ! Set up file names
      k = SCAN(This%cWtsFile, '.')
      IF (k > 0) THEN
        cHeader = 'sim_'//TRIM(ADJUSTL(This%cWtsFile(1:k-1)))// &
                  '_cls'//TRIM(ADJUSTL(cClusStr))
      ELSE
        cHeader = 'sim_cls'//TRIM(ADJUSTL(cClusStr))
      END IF
      cOutFile = TRIM(ADJUSTL(cHeader))//'.out'
      cInsFile = TRIM(ADJUSTL(cHeader))//'.ins'

      ! Open output files
      OPEN(UNIT=iOutUnit, FILE=cOutFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot open output file: '//TRIM(cOutFile), &
             f_iFatal, cModName)
        iStat = -1; RETURN
      END IF
      WRITE(iOutUnit, '(A14,A12,A40)') 'PEST_NAME', 'DATE', TRIM(cHeader)

      OPEN(UNIT=iInsUnit, FILE=cInsFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot open instruction file: '//TRIM(cInsFile), &
             f_iFatal, cModName)
        CLOSE(iOutUnit); iStat = -1; RETURN
      END IF
      WRITE(iInsUnit, '(A5)') 'pif #'
      WRITE(iInsUnit, '(A2)') 'l1'

      ! Calculate and write type hydrograph
      DO i = 1, This%iNVal
        ! Sum product: dot_product(cluster_weights, demeaned_values)
        rSumProduct = 0.0D0
        DO j = 1, This%iNClusWells
          rSumProduct = rSumProduct + This%rClusWt(j, iCls) * rDeMeaned(i, j)
        END DO

        ! Sum of non-zero weights
        rNzWtSum = 0.0D0
        DO j = 1, This%iNClusWells
          rNzWtSum = rNzWtSum + rNonZeroWts(i, j)
        END DO

        ! Compute type hydrograph value
        IF (rNzWtSum /= 0.0D0) THEN
          rTypeHydro = rSumProduct / rNzWtSum

          ! Build PEST parameter name
          WRITE(cIdStr, '(I0)') i
          cPstNam = TRIM(ADJUSTL(This%cPBase))// &
                    TRIM(ADJUSTL(cClusStr))//'_'// &
                    TRIM(ADJUSTL(cIdStr))

          ! Write to output file
          WRITE(iOutUnit, '(A14,A12,F20.6)') cPstNam, This%cDateStr(i), rTypeHydro

          ! Write PEST instruction file line
          WRITE(iInsUnit, '(A)') 'l1 ['//TRIM(ADJUSTL(cPstNam))//']34:46'
        END IF
      END DO

      CLOSE(iOutUnit)
      CLOSE(iInsUnit)
    END DO

    DEALLOCATE(rNonZeroWts, rDeMeaned)

    CALL LogMessage('Type hydrograph generation complete.', f_iInfo, cModName)

  END SUBROUTINE Run

  ! =====================================================================
  ! Kill - Deallocate all arrays
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(CalcTypeHydType), INTENT(INOUT) :: This

    IF (ALLOCATED(This%iClusID))    DEALLOCATE(This%iClusID)
    IF (ALLOCATED(This%rClusWt))    DEALLOCATE(This%rClusWt)
    IF (ALLOCATED(This%cWellNames)) DEALLOCATE(This%cWellNames)
    IF (ALLOCATED(This%rMonAvg))    DEALLOCATE(This%rMonAvg)
    IF (ALLOCATED(This%rMean))      DEALLOCATE(This%rMean)
    IF (ALLOCATED(This%cDateStr))   DEALLOCATE(This%cDateStr)
    IF (ALLOCATED(This%iAvgMo))     DEALLOCATE(This%iAvgMo)
    IF (ALLOCATED(This%iAvgDay))    DEALLOCATE(This%iAvgDay)
    This%lActive = .FALSE.

  END SUBROUTINE Kill

END MODULE Class_CalcTypeHyd
