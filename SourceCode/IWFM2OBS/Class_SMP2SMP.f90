!***********************************************************************
!  IWFM2OBS - Class_SMP2SMP
!  Time interpolation of SMP (bore sample) files
!
!  Ported from John Doherty's PEST utilities (smp2smp / time_interp)
!  Modernized: allocatable arrays, iStat error handling, IWFM kernel
!***********************************************************************
MODULE Class_SMP2SMP

  USE MessageLogger    , ONLY: SetLastMessage , &
                               LogMessage     , &
                               f_iFatal       , &
                               f_iWarn        , &
                               f_iInfo
  USE GeneralUtilities , ONLY: UpperCase      , &
                               IntToText
  USE TimeSeriesUtilities, ONLY: DayMonthYearToJulianDate , &
                                 JulianDateToDayMonthYear , &
                                 IsLeapYear

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: SMP2SMPType      , &
            SMPRecordType    , &
            SMPIDGroupType   , &
            TokenizeSMPLine

  ! =====================================================================
  ! SMPRecordType - one record in an SMP file (ID + time + value)
  ! =====================================================================
  TYPE SMPRecordType
    CHARACTER(LEN=25)   :: cID     = ' '
    INTEGER             :: iNDays  = 0      ! Julian day number
    INTEGER             :: iNSecs  = 0      ! Seconds since midnight
    REAL(8)             :: rValue  = 0.0D0
  END TYPE SMPRecordType

  ! =====================================================================
  ! SMPIDGroupType - all records for one bore ID
  ! =====================================================================
  TYPE SMPIDGroupType
    CHARACTER(LEN=25)                :: cID      = ' '
    INTEGER                          :: iNRec    = 0
    INTEGER                          :: iLocOff  = 0   ! Offset into flat arrays
    INTEGER                          :: iNOut    = 0   ! Count of output records
  END TYPE SMPIDGroupType

  ! =====================================================================
  ! SMP2SMPType - processor for SMP-to-SMP time interpolation
  ! =====================================================================
  TYPE SMP2SMPType
    INTEGER :: iDateSpec = 2   ! 1=dd/mm/yyyy, 2=mm/dd/yyyy
  CONTAINS
    PROCEDURE, PASS :: Init
    PROCEDURE, PASS :: Interpolate
    PROCEDURE, PASS :: ReadSMPIDs
    PROCEDURE, PASS :: ReadSMPData
    PROCEDURE, PASS :: WriteSMPLine
    PROCEDURE, PASS :: ParseDateStr
    PROCEDURE, PASS :: FormatDateStr
    PROCEDURE, PASS :: ParseTimeStr
  END TYPE SMP2SMPType

  ! Module-level parameters
  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_SMP2SMP'

CONTAINS

  ! =====================================================================
  ! TokenizeSMPLine - Split a line into whitespace-delimited tokens
  !   Unlike list-directed READ, treats '/' and ':' as normal characters
  ! =====================================================================
  SUBROUTINE TokenizeSMPLine(cLine, cTokens, iNTokens)
    CHARACTER(LEN=*), INTENT(IN)    :: cLine
    CHARACTER(LEN=*), INTENT(OUT)   :: cTokens(5)
    INTEGER,          INTENT(OUT)   :: iNTokens

    INTEGER :: iLen, iPos, iTok, iStart

    cTokens = ' '
    iNTokens = 0
    iLen = LEN_TRIM(cLine)
    iPos = 1

    DO iTok = 1, 5
      ! Skip whitespace (spaces and tabs)
      DO WHILE (iPos <= iLen)
        IF (cLine(iPos:iPos) /= ' ' .AND. ICHAR(cLine(iPos:iPos)) /= 9) EXIT
        iPos = iPos + 1
      END DO
      IF (iPos > iLen) RETURN

      ! Find end of token
      iStart = iPos
      DO WHILE (iPos <= iLen)
        IF (cLine(iPos:iPos) == ' ' .OR. ICHAR(cLine(iPos:iPos)) == 9) EXIT
        iPos = iPos + 1
      END DO

      iNTokens = iTok
      cTokens(iTok) = cLine(iStart:iPos-1)
    END DO

  END SUBROUTINE TokenizeSMPLine

  ! =====================================================================
  ! Init - Initialize the SMP2SMP processor
  ! =====================================================================
  SUBROUTINE Init(This, iDateSpec)
    CLASS(SMP2SMPType), INTENT(INOUT) :: This
    INTEGER,            INTENT(IN)    :: iDateSpec  ! 1=dd/mm, 2=mm/dd

    This%iDateSpec = iDateSpec

  END SUBROUTINE Init

  ! =====================================================================
  ! ParseDateStr - Parse a date string into day, month, year
  !   Handles both dd/mm/yyyy and mm/dd/yyyy based on iDateSpec
  ! =====================================================================
  SUBROUTINE ParseDateStr(This, cDate, iDay, iMon, iYear, iStat)
    CLASS(SMP2SMPType), INTENT(IN)  :: This
    CHARACTER(LEN=*),   INTENT(IN)  :: cDate
    INTEGER,            INTENT(OUT) :: iDay, iMon, iYear, iStat

    CHARACTER(LEN=20) :: cWork
    INTEGER           :: iLen, i, j, iFirst, iSecond

    iStat = 0
    cWork = ADJUSTL(cDate)
    iLen  = LEN_TRIM(cWork)
    IF (iLen < 8) THEN
      iStat = -1
      RETURN
    END IF

    ! Find first separator (/ or :)
    DO i = 1, iLen
      IF (cWork(i:i) == '/' .OR. cWork(i:i) == ':') EXIT
    END DO
    IF (i >= iLen .OR. i == 1) THEN
      iStat = -1
      RETURN
    END IF
    READ(cWork(1:i-1), *, IOSTAT=iStat) iFirst
    IF (iStat /= 0) RETURN

    ! Find second separator
    DO j = i+1, iLen
      IF (cWork(j:j) == '/' .OR. cWork(j:j) == ':') EXIT
    END DO
    IF (j > iLen .OR. j == i+1) THEN
      iStat = -1
      RETURN
    END IF
    READ(cWork(i+1:j-1), *, IOSTAT=iStat) iSecond
    IF (iStat /= 0) RETURN

    ! Read year
    READ(cWork(j+1:iLen), *, IOSTAT=iStat) iYear
    IF (iStat /= 0) RETURN

    ! Assign day/month based on date format
    IF (This%iDateSpec == 1) THEN
      iDay = iFirst
      iMon = iSecond
    ELSE
      iMon = iFirst
      iDay = iSecond
    END IF

    ! Validate
    IF (iDay < 1 .OR. iDay > 31 .OR. iMon < 1 .OR. iMon > 12) THEN
      iStat = -1
      RETURN
    END IF

  END SUBROUTINE ParseDateStr

  ! =====================================================================
  ! ParseTimeStr - Parse a time string into hours, minutes, seconds
  ! =====================================================================
  SUBROUTINE ParseTimeStr(This, cTime, iHH, iMM, iSS, iStat)
    CLASS(SMP2SMPType), INTENT(IN)  :: This
    CHARACTER(LEN=*),   INTENT(IN)  :: cTime
    INTEGER,            INTENT(OUT) :: iHH, iMM, iSS, iStat

    CHARACTER(LEN=20) :: cWork
    INTEGER           :: iLen, i, j

    iStat = 0
    cWork = ADJUSTL(cTime)
    iLen  = LEN_TRIM(cWork)
    IF (iLen < 5) THEN
      iStat = -1
      RETURN
    END IF

    ! Find first separator
    DO i = 1, iLen
      IF (cWork(i:i) == ':' .OR. cWork(i:i) == '.') EXIT
    END DO
    IF (i >= iLen .OR. i == 1) THEN
      iStat = -1
      RETURN
    END IF
    READ(cWork(1:i-1), *, IOSTAT=iStat) iHH
    IF (iStat /= 0) RETURN

    ! Find second separator
    DO j = i+1, iLen
      IF (cWork(j:j) == ':' .OR. cWork(j:j) == '.') EXIT
    END DO
    IF (j > iLen .OR. j == i+1) THEN
      iStat = -1
      RETURN
    END IF
    READ(cWork(i+1:j-1), *, IOSTAT=iStat) iMM
    IF (iStat /= 0) RETURN

    ! Read seconds
    READ(cWork(j+1:iLen), *, IOSTAT=iStat) iSS
    IF (iStat /= 0) RETURN

    ! Validate
    IF (iHH < 0 .OR. iHH > 23 .OR. iMM < 0 .OR. iMM > 59 .OR. iSS < 0 .OR. iSS > 59) THEN
      iStat = -1
      RETURN
    END IF

  END SUBROUTINE ParseTimeStr

  ! =====================================================================
  ! FormatDateStr - Format day/month/year into date string
  ! =====================================================================
  SUBROUTINE FormatDateStr(This, iDay, iMon, iYear, cDate)
    CLASS(SMP2SMPType), INTENT(IN)  :: This
    INTEGER,            INTENT(IN)  :: iDay, iMon, iYear
    CHARACTER(LEN=10),  INTENT(OUT) :: cDate

    IF (This%iDateSpec == 1) THEN
      WRITE(cDate, '(I2.2,A1,I2.2,A1,I4.4)') iDay, '/', iMon, '/', iYear
    ELSE
      WRITE(cDate, '(I2.2,A1,I2.2,A1,I4.4)') iMon, '/', iDay, '/', iYear
    END IF

  END SUBROUTINE FormatDateStr

  ! =====================================================================
  ! WriteSMPLine - Write one SMP record to a file unit
  !   Preserves exact PEST column-position format
  ! =====================================================================
  SUBROUTINE WriteSMPLine(This, iUnit, cID, iJulDay, iSecs, rValue)
    CLASS(SMP2SMPType), INTENT(IN) :: This
    INTEGER,            INTENT(IN) :: iUnit
    CHARACTER(LEN=*),   INTENT(IN) :: cID
    INTEGER,            INTENT(IN) :: iJulDay, iSecs
    REAL(8),            INTENT(IN) :: rValue

    INTEGER :: iDay, iMon, iYear, iHH, iMM, iSS, iStat

    CALL JulianDateToDayMonthYear(iJulDay, iDay, iMon, iYear, iStat)
    iHH = iSecs / 3600
    iMM = (iSecs - iHH*3600) / 60
    iSS = iSecs - iHH*3600 - iMM*60

    IF (This%iDateSpec == 1) THEN
      WRITE(iUnit, 100) TRIM(cID), iDay, iMon, iYear, iHH, iMM, iSS, rValue
    ELSE
      WRITE(iUnit, 100) TRIM(cID), iMon, iDay, iYear, iHH, iMM, iSS, rValue
    END IF
100 FORMAT(1X,A,10X,I2.2,'/',I2.2,'/',I4.4,3X,I2.2,':',I2.2,':',I2.2,3X,1PG15.8)

  END SUBROUTINE WriteSMPLine

  ! =====================================================================
  ! ReadSMPIDs - Read an SMP file and extract unique bore IDs
  ! =====================================================================
  SUBROUTINE ReadSMPIDs(This, cFile, iUnit, cIDs, iNIDs, iStat)
    CLASS(SMP2SMPType),                INTENT(IN)    :: This
    CHARACTER(LEN=*),                  INTENT(IN)    :: cFile
    INTEGER,                           INTENT(IN)    :: iUnit
    CHARACTER(LEN=25), ALLOCATABLE,    INTENT(OUT)   :: cIDs(:)
    INTEGER,                           INTENT(OUT)   :: iNIDs
    INTEGER,                           INTENT(OUT)   :: iStat

    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cTemp, cPrev
    INTEGER            :: iLine, i, iErr
    INTEGER            :: iCount
    ! First pass: count unique IDs
    CHARACTER(LEN=25), ALLOCATABLE :: cTempIDs(:)

    iStat = 0
    iNIDs = 0
    cPrev = ' '
    iLine = 0

    ! Allocate temporary array (generous initial size)
    ALLOCATE(cTempIDs(100000), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate memory for SMP IDs', f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF

    ! Read through file counting unique IDs
    DO
      iLine = iLine + 1
      READ(iUnit, *, IOSTAT=iErr) cTemp
      IF (iErr /= 0) EXIT

      cTemp = ADJUSTL(cTemp)
      cTemp = UpperCase(cTemp)

      IF (LEN_TRIM(cTemp) == 0) CYCLE

      IF (iNIDs == 0) THEN
        iNIDs = 1
        cTempIDs(1) = cTemp
        cPrev = cTemp
      ELSE IF (cTemp == cPrev) THEN
        CONTINUE
      ELSE
        ! Check for duplicates (non-contiguous)
        DO i = 1, iNIDs
          IF (cTemp == cTempIDs(i)) THEN
            CALL SetLastMessage('Identifier '//TRIM(cTemp)//' used non-contiguously at line '// &
                 TRIM(IntToText(iLine))//' of file '//TRIM(cFile), f_iFatal, cModName)
            iStat = -1
            DEALLOCATE(cTempIDs)
            RETURN
          END IF
        END DO
        iNIDs = iNIDs + 1
        cTempIDs(iNIDs) = cTemp
        cPrev = cTemp
      END IF
    END DO

    ! Copy to output array
    IF (iNIDs > 0) THEN
      ALLOCATE(cIDs(iNIDs), STAT=iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot allocate ID array', f_iFatal, cModName)
        iStat = -1
        DEALLOCATE(cTempIDs)
        RETURN
      END IF
      cIDs(1:iNIDs) = cTempIDs(1:iNIDs)
    END IF

    DEALLOCATE(cTempIDs)

    ! Rewind file for subsequent reading
    REWIND(iUnit, IOSTAT=iErr)

  END SUBROUTINE ReadSMPIDs

  ! =====================================================================
  ! ReadSMPData - Read SMP file data for specified bore IDs
  !   Returns parallel arrays of (julian_day, seconds, value) indexed
  !   by the groups array
  ! =====================================================================
  SUBROUTINE ReadSMPData(This, cFile, iUnit, cIDs, iNIDs, Groups, &
                         iDays, iSecs, rValues, iTotalRec, iStat)
    CLASS(SMP2SMPType),                INTENT(IN)    :: This
    CHARACTER(LEN=*),                  INTENT(IN)    :: cFile
    INTEGER,                           INTENT(IN)    :: iUnit
    CHARACTER(LEN=25),                 INTENT(IN)    :: cIDs(:)
    INTEGER,                           INTENT(IN)    :: iNIDs
    TYPE(SMPIDGroupType), ALLOCATABLE, INTENT(OUT)   :: Groups(:)
    INTEGER,              ALLOCATABLE, INTENT(OUT)   :: iDays(:), iSecs(:)
    REAL(8),              ALLOCATABLE, INTENT(OUT)   :: rValues(:)
    INTEGER,                           INTENT(OUT)   :: iTotalRec
    INTEGER,                           INTENT(OUT)   :: iStat

    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cTemp, cPrev
    CHARACTER(LEN=30)  :: cDateStr, cTimeStr, cValStr, cFlagStr
    CHARACTER(LEN=30)  :: cTokens(5)
    INTEGER            :: iLine, i, iErr, iObs, iInd
    INTEGER            :: iDay, iMon, iYear, iHH, iMM, iSS, iJulian
    INTEGER            :: iCols
    REAL(8)            :: rVal
    LOGICAL            :: lActive
    INTEGER, ALLOCATABLE :: iNMod(:), iLoc(:)

    iStat = 0

    ! Initialize group count arrays
    ALLOCATE(Groups(iNIDs), iNMod(iNIDs), iLoc(iNIDs), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate group arrays', f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF
    iNMod = 0
    iLoc  = 0
    DO i = 1, iNIDs
      Groups(i)%cID = cIDs(i)
    END DO

    ! First pass: count records per ID
    iLine = 0
    cPrev = ' '
    lActive = .FALSE.
    iObs = 0
    DO
      iLine = iLine + 1
      READ(iUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) == 0) CYCLE

      ! Extract first word (bore ID)
      READ(cLine, *, IOSTAT=iErr) cTemp
      IF (iErr /= 0) CYCLE
      cTemp = ADJUSTL(cTemp)
      cTemp = UpperCase(cTemp)

      IF (cTemp == cPrev) THEN
        IF (lActive) iNMod(iObs) = iNMod(iObs) + 1
      ELSE
        cPrev = cTemp
        lActive = .FALSE.
        DO i = 1, iNIDs
          IF (cTemp == cIDs(i)) THEN
            lActive = .TRUE.
            iObs = i
            iNMod(i) = iNMod(i) + 1
            EXIT
          END IF
        END DO
      END IF
    END DO

    ! Compute total and offsets
    iTotalRec = 0
    DO i = 1, iNIDs
      iLoc(i) = iTotalRec + 1
      Groups(i)%iNRec   = iNMod(i)
      Groups(i)%iLocOff = iTotalRec
      iTotalRec = iTotalRec + iNMod(i)
    END DO

    IF (iTotalRec == 0) THEN
      CALL SetLastMessage('No matching identifiers between model and observation files for '// &
           TRIM(cFile), f_iWarn, cModName)
      DEALLOCATE(iNMod, iLoc)
      iStat = -1
      RETURN
    END IF

    ! Allocate data arrays
    ALLOCATE(iDays(iTotalRec), iSecs(iTotalRec), rValues(iTotalRec), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate data arrays', f_iFatal, cModName)
      iStat = -1
      DEALLOCATE(iNMod, iLoc)
      RETURN
    END IF

    ! Second pass: read actual data
    REWIND(iUnit, IOSTAT=iErr)
    iLine = 0
    iInd  = 0
    cPrev = ' '
    lActive = .FALSE.
    iNMod = 0   ! Reset to use as running counters

    DO
      iLine = iLine + 1
      READ(iUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) == 0) CYCLE

      ! Tokenize line by whitespace (avoids '/' being treated as Fortran separator)
      CALL TokenizeSMPLine(cLine, cTokens, iCols)
      IF (iCols < 4) CYCLE
      cTemp    = cTokens(1)
      cDateStr = cTokens(2)
      cTimeStr = cTokens(3)
      cValStr  = cTokens(4)
      IF (iCols >= 5) THEN
        cFlagStr = cTokens(5)
      ELSE
        cFlagStr = ' '
      END IF

      cTemp = ADJUSTL(cTemp)
      cTemp = UpperCase(cTemp)

      IF (cTemp == cPrev) THEN
        IF (.NOT. lActive) CYCLE
      ELSE
        cPrev = cTemp
        lActive = .FALSE.
        DO i = 1, iNIDs
          IF (cTemp == cIDs(i)) THEN
            lActive = .TRUE.
            iObs = i
            EXIT
          END IF
        END DO
        IF (.NOT. lActive) CYCLE
      END IF

      ! Parse date
      CALL This%ParseDateStr(cDateStr, iDay, iMon, iYear, iStat)
      IF (iStat /= 0) THEN
        CALL SetLastMessage('Illegal date at line '//TRIM(IntToText(iLine))//' of file '// &
             TRIM(cFile), f_iFatal, cModName)
        DEALLOCATE(iNMod, iLoc)
        RETURN
      END IF
      CALL DayMonthYearToJulianDate(iDay, iMon, iYear, iJulian, iStat)
      IF (iStat /= 0) THEN
        CALL SetLastMessage('Invalid date at line '//TRIM(IntToText(iLine))//' of file '// &
             TRIM(cFile), f_iFatal, cModName)
        DEALLOCATE(iNMod, iLoc)
        RETURN
      END IF

      ! Parse time
      CALL This%ParseTimeStr(cTimeStr, iHH, iMM, iSS, iStat)
      IF (iStat /= 0) THEN
        CALL SetLastMessage('Illegal time at line '//TRIM(IntToText(iLine))//' of file '// &
             TRIM(cFile), f_iFatal, cModName)
        DEALLOCATE(iNMod, iLoc)
        RETURN
      END IF

      ! Parse value
      READ(cValStr, *, IOSTAT=iErr) rVal
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot read value at line '//TRIM(IntToText(iLine))//' of file '// &
             TRIM(cFile), f_iFatal, cModName)
        iStat = -1
        DEALLOCATE(iNMod, iLoc)
        RETURN
      END IF

      ! Check for 'x' flag (excluded data)
      IF (iCols == 5) THEN
        IF (UpperCase(ADJUSTL(cFlagStr)) == 'X') THEN
          rVal = -1.1D38
        END IF
      END IF

      ! Skip excluded values
      IF (rVal < -1.0D38) CYCLE

      ! Store record
      iNMod(iObs) = iNMod(iObs) + 1
      iInd = iLoc(iObs) + iNMod(iObs) - 1
      IF (iInd > iTotalRec) CYCLE  ! Safety check

      iDays(iInd)   = iJulian
      iSecs(iInd)   = iHH*3600 + iMM*60 + iSS
      rValues(iInd) = rVal
    END DO

    ! Update actual record counts (may be less due to excluded values)
    DO i = 1, iNIDs
      Groups(i)%iNRec = iNMod(i)
    END DO

    DEALLOCATE(iNMod, iLoc)
    iStat = 0

  END SUBROUTINE ReadSMPData

  ! =====================================================================
  ! TimeInterpOneSeries - Interpolate a time series to a target time
  !   Core algorithm ported from Doherty's time_interp
  ! =====================================================================
  SUBROUTINE TimeInterpOneSeries(iNBore, iNDays, iNSecs, rValue, &
                                  iIntDay, iIntSec, rNear, rConst, &
                                  rValInterp, iStat)
    INTEGER,  INTENT(IN)               :: iNBore
    INTEGER,  INTENT(IN), DIMENSION(:) :: iNDays, iNSecs
    REAL(8),  INTENT(IN), DIMENSION(:) :: rValue
    INTEGER,  INTENT(IN)               :: iIntDay, iIntSec
    REAL(8),  INTENT(IN)               :: rNear, rConst
    REAL(8),  INTENT(OUT)              :: rValInterp
    INTEGER,  INTENT(OUT)              :: iStat

    INTEGER  :: i
    REAL(8)  :: rSecFac, rDiff, rDiff1, rDenTime

    iStat   = 0
    rSecFac = 1.0D0 / 86400.0D0

    ! Single sample case
    IF (iNBore == 1) THEN
      rDiff = DBLE(iIntDay - iNDays(1)) + DBLE(iIntSec - iNSecs(1)) * rSecFac
      IF (ABS(rDiff) <= rConst) THEN
        rValInterp = rValue(1)
      ELSE
        IF (rDiff > 0.0D0) THEN
          rValInterp = -9.1D37
        ELSE
          rValInterp = -8.1D37
        END IF
      END IF
      RETURN
    END IF

    ! Check chronological ordering
    DO i = 1, iNBore - 1
      IF (iNDays(i) > iNDays(i+1) .OR. &
          (iNDays(i) == iNDays(i+1) .AND. iNSecs(i) >= iNSecs(i+1))) THEN
        iStat = -1
        RETURN
      END IF
    END DO

    ! Find bracketing samples and interpolate
    DO i = 1, iNBore
      rDiff = DBLE(iNDays(i) - iIntDay) + DBLE(iNSecs(i) - iIntSec) * rSecFac

      IF (rDiff >= 0.0D0) THEN
        ! Target is before or at sample i
        IF (i == 1) THEN
          ! Before first sample
          IF (rDiff <= rConst) THEN
            rValInterp = rValue(1)
          ELSE
            rValInterp = -8.1D37
          END IF
          RETURN
        END IF

        ! Default interpolation (direction = 'med')
        rDenTime = DBLE(iNDays(i) - iNDays(i-1)) + &
                   DBLE(iNSecs(i) - iNSecs(i-1)) * rSecFac
        IF (rDenTime <= 0.0D0) THEN
          iStat = -1
          RETURN
        END IF

        rDiff1 = rDenTime - rDiff

        IF (rDiff1 > rNear .AND. rDiff > rNear) THEN
          rValInterp = -7.1D37
        ELSE
          rValInterp = rValue(i-1) + (rValue(i) - rValue(i-1)) / rDenTime * rDiff1
        END IF

        ! Handle "no data" sentinel values
        IF (rValue(i) < -1.0D38) THEN
          IF (rDiff1 <= rConst) THEN
            rValInterp = rValue(i-1)
          ELSE
            rValInterp = -1.1D38
          END IF
        ELSE IF (rValue(i-1) < -1.0D38) THEN
          IF (rDiff <= rConst) THEN
            rValInterp = rValue(i)
          ELSE
            rValInterp = -1.1D38
          END IF
        END IF

        RETURN
      END IF
    END DO

    ! After last sample
    rDiff1 = DBLE(iIntDay - iNDays(iNBore)) + DBLE(iIntSec - iNSecs(iNBore)) * rSecFac
    IF (rDiff1 <= rConst) THEN
      rValInterp = rValue(iNBore)
    ELSE
      rValInterp = -9.1D37
    END IF

  END SUBROUTINE TimeInterpOneSeries

  ! =====================================================================
  ! Interpolate - Full SMP-to-SMP interpolation workflow
  !   Reads obs SMP, reads model SMP, time-interpolates, writes output
  ! =====================================================================
  SUBROUTINE Interpolate(This, cObsFile, cModFile, cOutFile, rThreshold, &
                          cInsFile, cPCFFile, lWriteIns, iStat)
    CLASS(SMP2SMPType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),   INTENT(IN)    :: cObsFile, cModFile, cOutFile
    REAL(8),            INTENT(IN)    :: rThreshold
    CHARACTER(LEN=*),   INTENT(IN)    :: cInsFile, cPCFFile
    LOGICAL,            INTENT(IN)    :: lWriteIns
    INTEGER,            INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iObsUnit = 101, iModUnit = 102, iOutUnit = 103
    INTEGER, PARAMETER :: iInsUnit = 104, iPCFUnit = 105

    CHARACTER(LEN=25), ALLOCATABLE :: cObsIDs(:)
    TYPE(SMPIDGroupType), ALLOCATABLE :: ModGroups(:)
    INTEGER, ALLOCATABLE :: iModDays(:), iModSecs(:)
    REAL(8), ALLOCATABLE :: rModValues(:)

    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cTemp, cPrev
    CHARACTER(LEN=30)  :: cDateStr, cTimeStr, cValStr, cFlagStr
    CHARACTER(LEN=30)  :: cTokens(5)
    INTEGER            :: iNObsIDs, iTotalMod
    INTEGER            :: iLine, i, iErr, iObs, iOut, iID
    INTEGER            :: iDay, iMon, iYear, iHH, iMM, iSS, iJulian
    INTEGER            :: iIntDays, iIntSecs, iCols, ii
    REAL(8)            :: rObsValue, rIntValue

    iStat = 0

    ! Open files
    OPEN(UNIT=iObsUnit, FILE=cObsFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open observation file: '//TRIM(cObsFile), f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF

    ! Read observation IDs
    CALL This%ReadSMPIDs(cObsFile, iObsUnit, cObsIDs, iNObsIDs, iStat)
    IF (iStat /= 0) THEN
      CLOSE(iObsUnit)
      RETURN
    END IF

    ! Open and read model SMP data
    OPEN(UNIT=iModUnit, FILE=cModFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open model file: '//TRIM(cModFile), f_iFatal, cModName)
      iStat = -1
      CLOSE(iObsUnit)
      RETURN
    END IF

    CALL This%ReadSMPData(cModFile, iModUnit, cObsIDs, iNObsIDs, ModGroups, &
                           iModDays, iModSecs, rModValues, iTotalMod, iStat)
    CLOSE(iModUnit)
    IF (iStat /= 0) THEN
      CLOSE(iObsUnit)
      RETURN
    END IF

    ! Open output file
    OPEN(UNIT=iOutUnit, FILE=cOutFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open output file: '//TRIM(cOutFile), f_iFatal, cModName)
      iStat = -1
      CLOSE(iObsUnit)
      RETURN
    END IF

    ! Open instruction/PCF files if requested
    IF (lWriteIns) THEN
      OPEN(UNIT=iInsUnit, FILE=cInsFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot open instruction file: '//TRIM(cInsFile), f_iFatal, cModName)
        iStat = -1
        CLOSE(iObsUnit); CLOSE(iOutUnit)
        RETURN
      END IF
      WRITE(iInsUnit, '(A)') 'pif #'

      OPEN(UNIT=iPCFUnit, FILE=cPCFFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Cannot open PCF file: '//TRIM(cPCFFile), f_iFatal, cModName)
        iStat = -1
        CLOSE(iObsUnit); CLOSE(iOutUnit); CLOSE(iInsUnit)
        RETURN
      END IF
    END IF

    ! Process observation file line by line
    REWIND(iObsUnit)
    iObs = 0
    iOut = 0
    cPrev = ' '

    DO
      READ(iObsUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) == 0) CYCLE

      ! Tokenize line by whitespace (avoids '/' being treated as Fortran separator)
      CALL TokenizeSMPLine(cLine, cTokens, iCols)
      IF (iCols < 4) CYCLE
      cTemp    = cTokens(1)
      cDateStr = cTokens(2)
      cTimeStr = cTokens(3)
      cValStr  = cTokens(4)
      IF (iCols >= 5) THEN
        cFlagStr = cTokens(5)
      ELSE
        cFlagStr = ' '
      END IF

      cTemp = ADJUSTL(cTemp)
      cTemp = UpperCase(cTemp)

      IF (cTemp /= cPrev) THEN
        iObs = iObs + 1
        iID  = 1
        cPrev = cTemp
      END IF

      IF (iObs > iNObsIDs) EXIT
      IF (ModGroups(iObs)%iNRec == 0) CYCLE

      ! Parse observation date/time
      CALL This%ParseDateStr(cDateStr, iDay, iMon, iYear, iStat)
      IF (iStat /= 0) CYCLE
      CALL DayMonthYearToJulianDate(iDay, iMon, iYear, iIntDays, iStat)
      IF (iStat /= 0) CYCLE
      CALL This%ParseTimeStr(cTimeStr, iHH, iMM, iSS, iStat)
      IF (iStat /= 0) CYCLE
      iIntSecs = iHH*3600 + iMM*60 + iSS

      ! Parse observation value
      READ(cValStr, *, IOSTAT=iErr) rObsValue
      IF (iErr /= 0) CYCLE
      IF (rObsValue < -1.0D38) CYCLE

      ! Check for x flag
      IF (iCols == 5) THEN
        IF (UpperCase(ADJUSTL(cFlagStr)) == 'X') CYCLE
      END IF

      ! Time-interpolate model data to observation time
      ii = ModGroups(iObs)%iLocOff + 1
      CALL TimeInterpOneSeries(ModGroups(iObs)%iNRec, &
                               iModDays(ii:), iModSecs(ii:), rModValues(ii:), &
                               iIntDays, iIntSecs, 1.0D30, rThreshold, &
                               rIntValue, iStat)
      IF (iStat /= 0) THEN
        CALL SetLastMessage('Problem interpolating for '//TRIM(cTemp)//' in file '// &
             TRIM(cModFile), f_iFatal, cModName)
        EXIT
      END IF

      ! Write output if valid
      IF (rIntValue > -1.0D30) THEN
        iOut = iOut + 1
        CALL This%WriteSMPLine(iOutUnit, cTemp, iIntDays, iIntSecs, rIntValue)
        ModGroups(iObs)%iNOut = ModGroups(iObs)%iNOut + 1

        ! Write instruction and PCF files
        IF (lWriteIns) THEN
          WRITE(iInsUnit, 200) TRIM(cTemp), iID
200       FORMAT('l1  [',A,'_',I4.4,']37:56')
          WRITE(iPCFUnit, 210) TRIM(cTemp), iID, rObsValue
210       FORMAT(A,'_',I4.4,'    ',1PG15.8)
          iID = iID + 1
        END IF
      END IF
    END DO

    ! Report results
    CALL LogMessage(TRIM(IntToText(iOut))//' lines written to '//TRIM(cOutFile), &
                    f_iInfo, cModName)

    ! Report unmatched IDs
    DO i = 1, iNObsIDs
      IF (ModGroups(i)%iNRec == 0) THEN
        CALL LogMessage('  Warning: ID '//TRIM(cObsIDs(i))//' not found in model file', &
                        f_iWarn, cModName)
      ELSE IF (ModGroups(i)%iNOut == 0) THEN
        CALL LogMessage('  Warning: ID '//TRIM(cObsIDs(i))//' has no times within model range', &
                        f_iWarn, cModName)
      END IF
    END DO

    ! Clean up
    CLOSE(iObsUnit)
    CLOSE(iOutUnit)
    IF (lWriteIns) THEN
      CLOSE(iInsUnit)
      CLOSE(iPCFUnit)
    END IF

    IF (ALLOCATED(cObsIDs))   DEALLOCATE(cObsIDs)
    IF (ALLOCATED(ModGroups)) DEALLOCATE(ModGroups)
    IF (ALLOCATED(iModDays))  DEALLOCATE(iModDays)
    IF (ALLOCATED(iModSecs))  DEALLOCATE(iModSecs)
    IF (ALLOCATED(rModValues))DEALLOCATE(rModValues)

    iStat = 0

  END SUBROUTINE Interpolate

END MODULE Class_SMP2SMP
