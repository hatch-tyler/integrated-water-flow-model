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
  PUBLIC :: SMP2SMPType          , &
            SMPRecordType        , &
            SMPIDGroupType       , &
            TokenizeSMPLine      , &
            ExpandObsIDsToLayers , &
            ExpandSMPDataToLayers

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
    PROCEDURE, PASS :: InterpolateDirect
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

    INTEGER  :: i, iLo, iHi, iMid
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

    ! Binary search for first sample at or after target time
    ! (replaces linear scan for O(log N) instead of O(N))
    iLo = 1
    iHi = iNBore
    DO WHILE (iLo < iHi)
      iMid = (iLo + iHi) / 2
      rDiff = DBLE(iNDays(iMid) - iIntDay) + DBLE(iNSecs(iMid) - iIntSec) * rSecFac
      IF (rDiff >= 0.0D0) THEN
        iHi = iMid
      ELSE
        iLo = iMid + 1
      END IF
    END DO

    ! iLo is the candidate — check if it's actually at or after target
    i = iLo
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

    ! After last sample (all samples before target)
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

  ! =====================================================================
  ! InterpolateDirect - Interpolate using in-memory model data
  !   Phase A optimization: no temp SMP file needed.
  !   Reads observation SMP streaming, looks up model data by ID using
  !   binary search (Phase B), interpolates, and writes output.
  ! =====================================================================
  SUBROUTINE InterpolateDirect(This, cObsFile, cOutFile, rThreshold, &
                                cInsFile, cPCFFile, lWriteIns, &
                                cFilteredIDs, iNFiltered, &
                                rModelData, iModelDays, iModelSecs, iNTimes, &
                                iStat, iExpandLayers)
    CLASS(SMP2SMPType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),   INTENT(IN)    :: cObsFile, cOutFile
    REAL(8),            INTENT(IN)    :: rThreshold
    CHARACTER(LEN=*),   INTENT(IN)    :: cInsFile, cPCFFile
    LOGICAL,            INTENT(IN)    :: lWriteIns
    CHARACTER(LEN=25),  INTENT(IN)    :: cFilteredIDs(:)
    INTEGER,            INTENT(IN)    :: iNFiltered
    REAL(8),            INTENT(IN)    :: rModelData(:,:)  ! (iNTimes, iNFiltered)
    INTEGER,            INTENT(IN)    :: iModelDays(:)
    INTEGER,            INTENT(IN)    :: iModelSecs(:)
    INTEGER,            INTENT(IN)    :: iNTimes
    INTEGER,            INTENT(OUT)   :: iStat
    INTEGER, OPTIONAL,  INTENT(IN)    :: iExpandLayers  ! >0: auto-expand base IDs

    INTEGER, PARAMETER :: iObsUnit = 101, iOutUnit = 103
    INTEGER, PARAMETER :: iInsUnit = 104, iPCFUnit = 105
    INTEGER, PARAMETER :: iMaxExpand = 20  ! Max supported layers for expansion

    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cTemp, cPrev, cLayerID
    CHARACTER(LEN=30)  :: cTokens(5)
    CHARACTER(LEN=30)  :: cDateStr, cTimeStr, cValStr, cFlagStr
    INTEGER            :: iErr, iObs, iOut, iID, iCol
    INTEGER            :: iDay, iMon, iYear, iHH, iMM, iSS
    INTEGER            :: iIntDays, iIntSecs, iCols, k
    REAL(8)            :: rObsValue, rIntValue
    ! Sorted index for binary search
    CHARACTER(LEN=25), ALLOCATABLE :: cSorted(:)
    INTEGER, ALLOCATABLE           :: iSortIdx(:)
    INTEGER            :: iNMatched, iNUnmatched
    ! Expansion support: when a base obs ID maps to multiple %N model columns
    INTEGER            :: iNExpCols                    ! Number of expanded columns found
    INTEGER            :: iExpCols(iMaxExpand)         ! Column indices for expanded IDs
    CHARACTER(LEN=25)  :: cExpIDs(iMaxExpand)          ! Expanded ID strings
    INTEGER            :: iNExpandLayers, iLyr
    LOGICAL            :: lExpanding
    ! Buffer for contiguous-by-layer output during expansion
    INTEGER, PARAMETER :: iMaxBuf = 2000               ! Max timesteps per bore
    INTEGER            :: iBufDays(iMaxBuf)
    INTEGER            :: iBufSecs(iMaxBuf)
    REAL(8)            :: rBufObs(iMaxBuf)
    REAL(8)            :: rBufInterp(iMaxBuf, iMaxExpand)
    LOGICAL            :: lBufValid(iMaxBuf, iMaxExpand)
    INTEGER            :: iBufCount

    iStat = 0
    iNExpandLayers = 0
    lExpanding = .FALSE.
    IF (PRESENT(iExpandLayers)) THEN
      IF (iExpandLayers > 0) iNExpandLayers = MIN(iExpandLayers, iMaxExpand)
    END IF

    IF (iNFiltered == 0 .OR. iNTimes == 0) THEN
      CALL SetLastMessage('No model data available for direct interpolation', &
           f_iWarn, cModName)
      iStat = -1; RETURN
    END IF

    ! ---- Build sorted index of filtered IDs for O(log N) lookup ----
    ALLOCATE(cSorted(iNFiltered), iSortIdx(iNFiltered), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate sort arrays', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF
    DO k = 1, iNFiltered
      cSorted(k) = cFilteredIDs(k)
      iSortIdx(k) = k
    END DO
    IF (iNFiltered > 1) CALL SortStringsIndexSMP(cSorted, iSortIdx, 1, iNFiltered)

    ! ---- Open files ----
    OPEN(UNIT=iObsUnit, FILE=cObsFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open observation file: '//TRIM(cObsFile), &
           f_iFatal, cModName)
      DEALLOCATE(cSorted, iSortIdx)
      iStat = -1; RETURN
    END IF

    OPEN(UNIT=iOutUnit, FILE=cOutFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open output file: '//TRIM(cOutFile), &
           f_iFatal, cModName)
      CLOSE(iObsUnit); DEALLOCATE(cSorted, iSortIdx)
      iStat = -1; RETURN
    END IF

    IF (lWriteIns) THEN
      OPEN(UNIT=iInsUnit, FILE=cInsFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iObsUnit); CLOSE(iOutUnit); DEALLOCATE(cSorted, iSortIdx)
        iStat = -1; RETURN
      END IF
      WRITE(iInsUnit, '(A)') 'pif #'

      OPEN(UNIT=iPCFUnit, FILE=cPCFFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iObsUnit); CLOSE(iOutUnit); CLOSE(iInsUnit)
        DEALLOCATE(cSorted, iSortIdx)
        iStat = -1; RETURN
      END IF
    END IF

    ! ---- Process observation file line by line ----
    iObs = 0
    iOut = 0
    iNMatched   = 0
    iNUnmatched = 0
    cPrev = ' '
    iID   = 0
    iCol  = 0
    iBufCount = 0

    DO
      READ(iObsUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) == 0) CYCLE

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

      ! When ID changes, look up the new model column
      IF (cTemp /= cPrev) THEN
        ! Flush any buffered expansion records from the previous bore
        IF (iBufCount > 0 .AND. lExpanding) THEN
          DO iLyr = 1, iNExpCols
            DO k = 1, iBufCount
              IF (lBufValid(k, iLyr)) THEN
                iOut = iOut + 1
                CALL This%WriteSMPLine(iOutUnit, cExpIDs(iLyr), &
                     iBufDays(k), iBufSecs(k), rBufInterp(k, iLyr))
                IF (lWriteIns) THEN
                  WRITE(iInsUnit, 200) TRIM(cExpIDs(iLyr)), iID
                  WRITE(iPCFUnit, 210) TRIM(cExpIDs(iLyr)), iID, rBufObs(k)
                  iID = iID + 1
                END IF
              END IF
            END DO
          END DO
        END IF
        iBufCount = 0
        iObs = iObs + 1
        iID  = 1
        cPrev = cTemp
        lExpanding = .FALSE.
        iNExpCols = 0
        ! Binary search for model column (direct match)
        k = BinarySearchStrSMP(cSorted, iNFiltered, cTemp)
        IF (k > 0) THEN
          iCol = iSortIdx(k)
          iNMatched = iNMatched + 1
        ELSE
          iCol = 0
          ! If expansion is active, try ID%1 through ID%N
          IF (iNExpandLayers > 0) THEN
            DO iLyr = 1, iNExpandLayers
              WRITE(cLayerID, '(A,A1,I0)') TRIM(cTemp), '%', iLyr
              cLayerID = UpperCase(cLayerID)
              k = BinarySearchStrSMP(cSorted, iNFiltered, cLayerID)
              IF (k > 0) THEN
                iNExpCols = iNExpCols + 1
                iExpCols(iNExpCols) = iSortIdx(k)
                cExpIDs(iNExpCols) = cLayerID
              END IF
            END DO
            IF (iNExpCols > 0) THEN
              lExpanding = .TRUE.
              iNMatched = iNMatched + 1
            ELSE
              iNUnmatched = iNUnmatched + 1
            END IF
          ELSE
            iNUnmatched = iNUnmatched + 1
          END IF
        END IF
      END IF

      IF (iCol == 0 .AND. .NOT. lExpanding) CYCLE  ! No model data for this obs ID

      ! Parse observation date/time
      CALL This%ParseDateStr(cDateStr, iDay, iMon, iYear, iStat)
      IF (iStat /= 0) THEN
        iStat = 0; CYCLE
      END IF
      CALL DayMonthYearToJulianDate(iDay, iMon, iYear, iIntDays, iStat)
      IF (iStat /= 0) THEN
        iStat = 0; CYCLE
      END IF
      CALL This%ParseTimeStr(cTimeStr, iHH, iMM, iSS, iStat)
      IF (iStat /= 0) THEN
        iStat = 0; CYCLE
      END IF
      iIntSecs = iHH*3600 + iMM*60 + iSS

      ! Parse observation value
      READ(cValStr, *, IOSTAT=iErr) rObsValue
      IF (iErr /= 0) CYCLE
      IF (rObsValue < -1.0D38) CYCLE

      ! Check for x flag
      IF (iCols == 5) THEN
        IF (UpperCase(ADJUSTL(cFlagStr)) == 'X') CYCLE
      END IF

      ! Time-interpolate and write output
      IF (lExpanding) THEN
        ! Buffer this record; flush contiguously by layer when bore changes
        IF (iBufCount < iMaxBuf) THEN
          iBufCount = iBufCount + 1
          iBufDays(iBufCount) = iIntDays
          iBufSecs(iBufCount) = iIntSecs
          rBufObs(iBufCount)  = rObsValue
          DO iLyr = 1, iNExpCols
            CALL TimeInterpOneSeries(iNTimes, iModelDays, iModelSecs, &
                                     rModelData(:, iExpCols(iLyr)), &
                                     iIntDays, iIntSecs, 1.0D30, rThreshold, &
                                     rIntValue, iStat)
            IF (iStat /= 0) THEN
              CALL SetLastMessage('Problem interpolating for '//TRIM(cExpIDs(iLyr)), &
                   f_iFatal, cModName)
              GOTO 900
            END IF
            rBufInterp(iBufCount, iLyr) = rIntValue
            lBufValid(iBufCount, iLyr) = (rIntValue > -1.0D30)
          END DO
        END IF
      ELSE
        ! Direct mode: single column match
        CALL TimeInterpOneSeries(iNTimes, iModelDays, iModelSecs, &
                                 rModelData(:, iCol), &
                                 iIntDays, iIntSecs, 1.0D30, rThreshold, &
                                 rIntValue, iStat)
        IF (iStat /= 0) THEN
          CALL SetLastMessage('Problem interpolating for '//TRIM(cTemp), &
               f_iFatal, cModName)
          GOTO 900
        END IF
        IF (rIntValue > -1.0D30) THEN
          iOut = iOut + 1
          CALL This%WriteSMPLine(iOutUnit, cTemp, iIntDays, iIntSecs, rIntValue)
          IF (lWriteIns) THEN
            WRITE(iInsUnit, 200) TRIM(cTemp), iID
            WRITE(iPCFUnit, 210) TRIM(cTemp), iID, rObsValue
            iID = iID + 1
          END IF
        END IF
      END IF
200   FORMAT('l1  [',A,'_',I4.4,']37:56')
210   FORMAT(A,'_',I4.4,'    ',1PG15.8)
    END DO

    ! Flush remaining buffer after last bore
    IF (iBufCount > 0 .AND. lExpanding) THEN
      DO iLyr = 1, iNExpCols
        DO k = 1, iBufCount
          IF (lBufValid(k, iLyr)) THEN
            iOut = iOut + 1
            CALL This%WriteSMPLine(iOutUnit, cExpIDs(iLyr), &
                 iBufDays(k), iBufSecs(k), rBufInterp(k, iLyr))
            IF (lWriteIns) THEN
              WRITE(iInsUnit, 200) TRIM(cExpIDs(iLyr)), iID
              WRITE(iPCFUnit, 210) TRIM(cExpIDs(iLyr)), iID, rBufObs(k)
              iID = iID + 1
            END IF
          END IF
        END DO
      END DO
    END IF

    ! Report results
900 CONTINUE
    CALL LogMessage(TRIM(IntToText(iOut))//' lines written to '//TRIM(cOutFile), &
                    f_iInfo, cModName)
    IF (iNUnmatched > 0) THEN
      CALL LogMessage('  '//TRIM(IntToText(iNUnmatched))// &
           ' observation IDs not found in model data', f_iWarn, cModName)
    END IF

    ! Clean up
    CLOSE(iObsUnit)
    CLOSE(iOutUnit)
    IF (lWriteIns) THEN
      CLOSE(iInsUnit)
      CLOSE(iPCFUnit)
    END IF
    DEALLOCATE(cSorted, iSortIdx)

    iStat = 0

  END SUBROUTINE InterpolateDirect

  ! =====================================================================
  ! BinarySearchStrSMP - Binary search a sorted CHARACTER(25) array
  !   Returns position if found, 0 if not found
  ! =====================================================================
  FUNCTION BinarySearchStrSMP(cArr, iN, cTarget) RESULT(iPos)
    CHARACTER(LEN=25), INTENT(IN) :: cArr(:)
    INTEGER,           INTENT(IN) :: iN
    CHARACTER(LEN=25), INTENT(IN) :: cTarget
    INTEGER :: iPos

    INTEGER :: iLo, iHi, iMid

    iPos = 0
    iLo = 1
    iHi = iN
    DO WHILE (iLo <= iHi)
      iMid = (iLo + iHi) / 2
      IF (cArr(iMid) == cTarget) THEN
        iPos = iMid
        RETURN
      ELSE IF (cArr(iMid) < cTarget) THEN
        iLo = iMid + 1
      ELSE
        iHi = iMid - 1
      END IF
    END DO
  END FUNCTION BinarySearchStrSMP

  ! =====================================================================
  ! SortStringsIndexSMP - Quicksort CHARACTER(25) array with index array
  !   Sorts cArr(iLo:iHi) in ascending order, reordering iIdx in parallel
  ! =====================================================================
  RECURSIVE SUBROUTINE SortStringsIndexSMP(cArr, iIdx, iLo, iHi)
    CHARACTER(LEN=25), INTENT(INOUT) :: cArr(:)
    INTEGER,           INTENT(INOUT) :: iIdx(:)
    INTEGER,           INTENT(IN)    :: iLo, iHi

    CHARACTER(LEN=25) :: cPivot, cTemp
    INTEGER :: i, j, iTemp

    IF (iLo >= iHi) RETURN

    cPivot = cArr((iLo + iHi) / 2)
    i = iLo
    j = iHi

    DO WHILE (i <= j)
      DO WHILE (cArr(i) < cPivot)
        i = i + 1
      END DO
      DO WHILE (cArr(j) > cPivot)
        j = j - 1
      END DO
      IF (i <= j) THEN
        cTemp = cArr(i); cArr(i) = cArr(j); cArr(j) = cTemp
        iTemp = iIdx(i); iIdx(i) = iIdx(j); iIdx(j) = iTemp
        i = i + 1
        j = j - 1
      END IF
    END DO

    IF (iLo < j) CALL SortStringsIndexSMP(cArr, iIdx, iLo, j)
    IF (i < iHi) CALL SortStringsIndexSMP(cArr, iIdx, i, iHi)
  END SUBROUTINE SortStringsIndexSMP

  ! =====================================================================
  ! ExpandObsIDsToLayers - Expand base observation IDs to per-layer IDs
  !   If obs IDs lack %N suffixes, duplicate each ID as ID%1..ID%NL.
  !   Backward compatible: if all IDs already have %N, no expansion.
  !
  !   Detection: scan for '%' followed by digits at end of each ID.
  !   If ALL IDs have such suffixes, return unchanged (iStat=0).
  !   Otherwise, expand base IDs to per-layer variants.
  !
  !   Optionally filter against cModelIDs to only keep expanded IDs
  !   that exist in the model output.
  ! =====================================================================
  SUBROUTINE ExpandObsIDsToLayers(cIDs, iNIDs, iNLayers, &
                                   cModelIDs, iNModelIDs, iStat)
    CHARACTER(LEN=25), ALLOCATABLE,    INTENT(INOUT) :: cIDs(:)
    INTEGER,                           INTENT(INOUT) :: iNIDs
    INTEGER,                           INTENT(IN)    :: iNLayers
    CHARACTER(LEN=25), OPTIONAL,       INTENT(IN)    :: cModelIDs(:)
    INTEGER,           OPTIONAL,       INTENT(IN)    :: iNModelIDs
    INTEGER,                           INTENT(OUT)   :: iStat

    CHARACTER(LEN=25), ALLOCATABLE :: cNewIDs(:)
    CHARACTER(LEN=25) :: cBase, cExpanded
    INTEGER :: i, k, iErr, iLen, iPos
    INTEGER :: iNSuffixed, iNBase, iNewCount, iMaxNew
    LOGICAL :: lHasSuffix, lUseFilter

    iStat = 0

    IF (iNIDs == 0 .OR. iNLayers <= 0) RETURN

    lUseFilter = PRESENT(cModelIDs) .AND. PRESENT(iNModelIDs)

    ! Count how many IDs already have %N suffixes
    iNSuffixed = 0
    DO i = 1, iNIDs
      IF (HasLayerSuffix(cIDs(i))) iNSuffixed = iNSuffixed + 1
    END DO

    ! If all have suffixes, no expansion needed (backward compat)
    IF (iNSuffixed == iNIDs) RETURN

    iNBase = iNIDs - iNSuffixed

    ! Allocate new array: base IDs * layers + already-suffixed IDs
    iMaxNew = iNBase * iNLayers + iNSuffixed
    ALLOCATE(cNewIDs(iMaxNew), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate expanded ID array', f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF

    iNewCount = 0
    DO i = 1, iNIDs
      IF (HasLayerSuffix(cIDs(i))) THEN
        ! Already has suffix — keep as-is
        iNewCount = iNewCount + 1
        cNewIDs(iNewCount) = cIDs(i)
      ELSE
        ! Expand to ID%1 .. ID%NLayers
        cBase = TRIM(cIDs(i))
        DO k = 1, iNLayers
          WRITE(cExpanded, '(A,A1,I0)') TRIM(cBase), '%', k
          cExpanded = UpperCase(cExpanded)
          ! Optional filtering against model IDs
          IF (lUseFilter) THEN
            IF (.NOT. IDExistsIn(cExpanded, cModelIDs, iNModelIDs)) CYCLE
          END IF
          iNewCount = iNewCount + 1
          IF (iNewCount > iMaxNew) EXIT
          cNewIDs(iNewCount) = cExpanded
        END DO
      END IF
    END DO

    ! Replace original array
    DEALLOCATE(cIDs)
    ALLOCATE(cIDs(iNewCount), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate replacement ID array', f_iFatal, cModName)
      iStat = -1
      DEALLOCATE(cNewIDs)
      RETURN
    END IF
    cIDs(1:iNewCount) = cNewIDs(1:iNewCount)

    CALL LogMessage('Expanded '//TRIM(IntToText(iNBase))//' base obs IDs to '// &
         TRIM(IntToText(iNewCount))//' per-layer IDs ('// &
         TRIM(IntToText(iNLayers))//' layers)', f_iInfo, cModName)

    iNIDs = iNewCount
    DEALLOCATE(cNewIDs)

  CONTAINS

    ! Check if an ID ends with %<digits>
    FUNCTION HasLayerSuffix(cID) RESULT(lResult)
      CHARACTER(LEN=25), INTENT(IN) :: cID
      LOGICAL :: lResult
      INTEGER :: iPct, iL, ic

      lResult = .FALSE.
      iL = LEN_TRIM(cID)
      IF (iL < 3) RETURN
      ! Find last '%'
      iPct = INDEX(cID(1:iL), '%', BACK=.TRUE.)
      IF (iPct == 0 .OR. iPct == iL) RETURN
      ! Check all chars after '%' are digits
      DO ic = iPct + 1, iL
        IF (ICHAR(cID(ic:ic)) < ICHAR('0') .OR. ICHAR(cID(ic:ic)) > ICHAR('9')) RETURN
      END DO
      lResult = .TRUE.
    END FUNCTION HasLayerSuffix

    ! Check if an ID exists in a list
    FUNCTION IDExistsIn(cTarget, cList, iN) RESULT(lFound)
      CHARACTER(LEN=25), INTENT(IN) :: cTarget
      CHARACTER(LEN=25), INTENT(IN) :: cList(:)
      INTEGER,           INTENT(IN) :: iN
      LOGICAL :: lFound
      INTEGER :: j

      lFound = .FALSE.
      DO j = 1, iN
        IF (cList(j) == cTarget) THEN
          lFound = .TRUE.
          RETURN
        END IF
      END DO
    END FUNCTION IDExistsIn

  END SUBROUTINE ExpandObsIDsToLayers

  ! =====================================================================
  ! ExpandSMPDataToLayers - Expand SMP data arrays to per-layer copies
  !   After ReadSMPData reads data with base IDs, this duplicates the
  !   data arrays for each layer so that ID%1..ID%N all have records.
  !
  !   cOrigIDs/iNOrigIDs: the original (base) IDs from ReadSMPIDs
  !   cExpandedIDs/iNExpandedIDs: the expanded IDs from ExpandObsIDsToLayers
  !   Groups, iDays, iSecs, rValues: data from ReadSMPData (base IDs)
  !   Output: new Groups/iDays/iSecs/rValues arrays for expanded IDs
  ! =====================================================================
  SUBROUTINE ExpandSMPDataToLayers(cOrigIDs, iNOrigIDs, iNLayers, &
                                    GroupsIn, iDaysIn, iSecsIn, rValuesIn, iTotalRecIn, &
                                    cExpandedIDs, iNExpandedIDs, &
                                    GroupsOut, iDaysOut, iSecsOut, rValuesOut, iTotalRecOut, &
                                    iStat)
    CHARACTER(LEN=25),                 INTENT(IN)  :: cOrigIDs(:)
    INTEGER,                           INTENT(IN)  :: iNOrigIDs
    INTEGER,                           INTENT(IN)  :: iNLayers
    TYPE(SMPIDGroupType),              INTENT(IN)  :: GroupsIn(:)
    INTEGER,                           INTENT(IN)  :: iDaysIn(:)
    INTEGER,                           INTENT(IN)  :: iSecsIn(:)
    REAL(8),                           INTENT(IN)  :: rValuesIn(:)
    INTEGER,                           INTENT(IN)  :: iTotalRecIn
    CHARACTER(LEN=25),                 INTENT(IN)  :: cExpandedIDs(:)
    INTEGER,                           INTENT(IN)  :: iNExpandedIDs
    TYPE(SMPIDGroupType), ALLOCATABLE, INTENT(OUT) :: GroupsOut(:)
    INTEGER,              ALLOCATABLE, INTENT(OUT) :: iDaysOut(:)
    INTEGER,              ALLOCATABLE, INTENT(OUT) :: iSecsOut(:)
    REAL(8),              ALLOCATABLE, INTENT(OUT) :: rValuesOut(:)
    INTEGER,                           INTENT(OUT) :: iTotalRecOut
    INTEGER,                           INTENT(OUT) :: iStat

    CHARACTER(LEN=25) :: cBase, cExpID
    INTEGER :: i, j, k, iErr, iOff, iSrcOff, iNRec, iPct, iLen

    iStat = 0

    ! Compute total output records: each expanded ID maps to its base's records
    iTotalRecOut = 0
    DO i = 1, iNExpandedIDs
      ! Find base ID for this expanded ID (strip %N suffix)
      cExpID = cExpandedIDs(i)
      iLen = LEN_TRIM(cExpID)
      iPct = INDEX(cExpID(1:iLen), '%', BACK=.TRUE.)
      IF (iPct > 0) THEN
        cBase = cExpID(1:iPct-1)
      ELSE
        cBase = cExpID
      END IF
      cBase = UpperCase(cBase)

      ! Find base in original IDs
      DO j = 1, iNOrigIDs
        IF (TRIM(UpperCase(cOrigIDs(j))) == TRIM(cBase)) THEN
          iTotalRecOut = iTotalRecOut + GroupsIn(j)%iNRec
          EXIT
        END IF
      END DO
    END DO

    ! Allocate output arrays
    ALLOCATE(GroupsOut(iNExpandedIDs), STAT=iErr)
    IF (iErr /= 0) THEN
      iStat = -1; RETURN
    END IF
    IF (iTotalRecOut > 0) THEN
      ALLOCATE(iDaysOut(iTotalRecOut), iSecsOut(iTotalRecOut), &
               rValuesOut(iTotalRecOut), STAT=iErr)
      IF (iErr /= 0) THEN
        iStat = -1; RETURN
      END IF
    END IF

    ! Copy data: for each expanded ID, copy base ID's data
    iOff = 0
    DO i = 1, iNExpandedIDs
      cExpID = cExpandedIDs(i)
      iLen = LEN_TRIM(cExpID)
      iPct = INDEX(cExpID(1:iLen), '%', BACK=.TRUE.)
      IF (iPct > 0) THEN
        cBase = cExpID(1:iPct-1)
      ELSE
        cBase = cExpID
      END IF
      cBase = UpperCase(cBase)

      GroupsOut(i)%cID = cExpID
      GroupsOut(i)%iNRec = 0
      GroupsOut(i)%iLocOff = iOff
      GroupsOut(i)%iNOut = 0

      DO j = 1, iNOrigIDs
        IF (TRIM(UpperCase(cOrigIDs(j))) == TRIM(cBase)) THEN
          iNRec = GroupsIn(j)%iNRec
          GroupsOut(i)%iNRec = iNRec
          IF (iNRec > 0) THEN
            iSrcOff = GroupsIn(j)%iLocOff
            DO k = 1, iNRec
              iDaysOut(iOff + k)   = iDaysIn(iSrcOff + k)
              iSecsOut(iOff + k)   = iSecsIn(iSrcOff + k)
              rValuesOut(iOff + k) = rValuesIn(iSrcOff + k)
            END DO
            iOff = iOff + iNRec
          END IF
          EXIT
        END IF
      END DO
    END DO

    iTotalRecOut = iOff

  END SUBROUTINE ExpandSMPDataToLayers

END MODULE Class_SMP2SMP
