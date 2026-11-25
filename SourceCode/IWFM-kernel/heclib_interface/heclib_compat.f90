!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2025
!  State of California, Department of Water Resources
!
!  HEC-DSS Legacy Compatibility Module
!
!  This module provides backward-compatible interfaces that mimic
!  the original Fortran 77 HECLIB subroutine names (ZOPEN, ZCLOSE,
!  ZSRTS, ZRRTS, etc.) while internally using the new HEC-DSS 7 C API.
!
!  IMPORTANT CHANGES FROM LEGACY HECLIB:
!  1. IFLTAB must be declared as INTEGER(C_LONG_LONG) :: IFLTAB(250)
!     instead of INTEGER :: IFLTAB(600)
!  2. Time series operations now use internal zStructTimeSeries
!  3. Some function behaviors may differ slightly
!
!  Usage:
!    USE heclib_compat
!    INTEGER(C_LONG_LONG) :: IFLTAB(250)
!    CALL ZOPEN(IFLTAB, 'myfile.dss', ISTAT)
!***********************************************************************
MODULE heclib_compat
  USE, INTRINSIC :: ISO_C_BINDING
  USE heclib_c_binding
  IMPLICIT NONE

  PRIVATE

  ! Public legacy-compatible subroutines
  PUBLIC :: ZOPEN, ZCLOSE
  PUBLIC :: ZSRTS, ZRRTS
  PUBLIC :: ZSET
  PUBLIC :: ZFNAME
  PUBLIC :: ZUFPN, ZPATH
  PUBLIC :: ZGINTL, ZOFSET
  PUBLIC :: ZBEGDT, ZINCBK
  PUBLIC :: ZDTYPE, ZDELET
  PUBLIC :: ZCOFIL
  PUBLIC :: IYMDJL, JLIYMD, INCTIM

  ! Public type for compatibility
  PUBLIC :: C_LONG_LONG

CONTAINS

  !--------------------------------------------------------------
  ! ZOPEN - Open a DSS file
  ! Args: IFLTAB, FileName, ErrorCode
  !--------------------------------------------------------------
  SUBROUTINE ZOPEN(IFLTAB, CFNAME, ISTAT)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLTAB(*)
    CHARACTER(LEN=*), INTENT(IN) :: CFNAME
    INTEGER, INTENT(OUT) :: ISTAT

    ISTAT = hec_zopen(IFLTAB, f_to_c_string(TRIM(CFNAME)))

  END SUBROUTINE ZOPEN

  !--------------------------------------------------------------
  ! ZCLOSE - Close a DSS file
  ! Args: IFLTAB
  !--------------------------------------------------------------
  SUBROUTINE ZCLOSE(IFLTAB)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLTAB(*)

    INTEGER(C_INT) :: iStatus
    iStatus = hec_zclose(IFLTAB)

  END SUBROUTINE ZCLOSE

  !--------------------------------------------------------------
  ! ZSRTS - Store regular interval time series data
  ! Args: IFLTAB, PathName, StartDate, StartTime, NValues, Values,
  !       Units, DataType, IPLAN, ErrorCode
  !--------------------------------------------------------------
  SUBROUTINE ZSRTS(IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES, &
                   CUNITS, CTYPE, IPLAN, ISTAT)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLTAB(*)
    CHARACTER(LEN=*), INTENT(IN) :: CPATH
    CHARACTER(LEN=*), INTENT(IN) :: CDATE
    CHARACTER(LEN=*), INTENT(IN) :: CTIME
    INTEGER, INTENT(IN) :: NVALS
    REAL, INTENT(IN) :: VALUES(*)
    CHARACTER(LEN=*), INTENT(IN) :: CUNITS
    CHARACTER(LEN=*), INTENT(IN) :: CTYPE
    INTEGER, INTENT(IN) :: IPLAN
    INTEGER, INTENT(OUT) :: ISTAT

    TYPE(C_PTR) :: tss
    REAL(C_FLOAT), ALLOCATABLE :: fValues(:)
    INTEGER :: i

    ! Convert values to C_FLOAT
    ALLOCATE(fValues(NVALS))
    DO i = 1, NVALS
      fValues(i) = REAL(VALUES(i), C_FLOAT)
    END DO

    ! Create time series struct
    tss = hec_zstructTsNewRegFloats( &
            f_to_c_string(TRIM(CPATH)), &
            fValues, &
            INT(NVALS, C_INT), &
            f_to_c_string(TRIM(CDATE)), &
            f_to_c_string(TRIM(CTIME)), &
            f_to_c_string(TRIM(CUNITS)), &
            f_to_c_string(TRIM(CTYPE)))

    IF (.NOT. C_ASSOCIATED(tss)) THEN
      ISTAT = -1
      DEALLOCATE(fValues)
      RETURN
    END IF

    ! Store the data
    ISTAT = hec_ztsStore(IFLTAB, tss, INT(IPLAN, C_INT))

    ! Free the struct
    CALL hec_zstructFree(tss)
    DEALLOCATE(fValues)

  END SUBROUTINE ZSRTS

  !--------------------------------------------------------------
  ! ZRRTS - Read regular interval time series data
  ! Args: IFLTAB, PathName, StartDate, StartTime, NValues, Values,
  !       Units, DataType, Offset, ErrorCode
  !--------------------------------------------------------------
  SUBROUTINE ZRRTS(IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES, &
                   CUNITS, CTYPE, IOFSET, ISTAT)
    !Legacy HECLIB contract (as relied on by Class_DSSFileType):
    !  - read exactly NVALS regular-interval values starting at the first standard
    !    time at or after CDATE/CTIME (e.g. '31OCT4000','2400' is the Oct value of
    !    monthly end-of-period data)
    !  - missing values are returned as -901.0
    !  - ISTAT = 0 all values found, 1 some values missing, 4 no values found
    !    (including no record in the window), 12 hard error (bad pathname/interval)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLTAB(*)
    CHARACTER(LEN=*), INTENT(IN) :: CPATH
    CHARACTER(LEN=*), INTENT(IN) :: CDATE
    CHARACTER(LEN=*), INTENT(IN) :: CTIME
    INTEGER, INTENT(IN) :: NVALS
    REAL, INTENT(OUT) :: VALUES(*)
    CHARACTER(LEN=*), INTENT(OUT) :: CUNITS
    CHARACTER(LEN=*), INTENT(OUT) :: CTYPE
    INTEGER, INTENT(OUT) :: IOFSET
    INTEGER, INTENT(OUT) :: ISTAT

    REAL, PARAMETER :: rMissing = -901.0
    TYPE(C_PTR) :: tss
    INTEGER :: nRetrieved, nCopy, nMissing, i, iIntl, iStatParse, n1, n2, n3, n4, n5, n6
    INTEGER(C_INT) :: iJulStart, iSecStart, iJulEnd, iSecEnd, iStatus
    CHARACTER(LEN=32) :: tmpUnits, tmpType
    CHARACTER(LEN=65) :: cA, cB, cC, cD, cE, cF
    CHARACTER(LEN=20, KIND=C_CHAR) :: cEndDateC
    CHARACTER(LEN=20) :: cEndDate
    CHARACTER(LEN=4)  :: cEndTime
    REAL, ALLOCATABLE :: rBuf(:)

    CHARACTER(LEN=LEN_TRIM(CPATH)) :: localPath
    CHARACTER(LEN=LEN_TRIM(CDATE)) :: localDate
    CHARACTER(LEN=LEN_TRIM(CTIME)) :: localTime

    ! Initialize outputs
    ISTAT  = 0
    IOFSET = 0
    CUNITS = ''
    CTYPE  = ''
    IF (NVALS .LE. 0) RETURN
    VALUES(1:NVALS) = rMissing

    localPath = TRIM(CPATH)
    localDate = TRIM(CDATE)
    localTime = TRIM(CTIME)

    ! Interval from the E part
    iStatParse = 0
    CALL ZUFPN(cA, n1, cB, n2, cC, n3, cD, n4, cE, n5, cF, n6, &
               localPath, LEN(localPath), iStatParse)
    CALL ZGINTL(iIntl, cE, 0, iStatParse)
    IF (iStatParse .NE. 0 .OR. iIntl .LE. 0) THEN
      ISTAT = 12
      RETURN
    END IF

    ! Time window: NVALS intervals after the start; values are taken from the first
    ! standard time in the window (this covers starts on and off interval boundaries)
    iJulStart = hec_dateToJulian(f_to_c_string(localDate))
    iSecStart = hec_timeStringToSeconds(f_to_c_string(localTime))
    iStatus   = hec_incrementTime(INT(iIntl, C_INT) * 60_C_INT, INT(NVALS, C_INT), &
                                  iJulStart, iSecStart, iJulEnd, iSecEnd)
    IF (iSecEnd .EQ. 0) THEN
      iJulEnd = iJulEnd - 1_C_INT
      iSecEnd = 86400_C_INT
    END IF
    cEndDateC = ''
    iStatus   = hec_julianToDate(iJulEnd, 4_C_INT, cEndDateC, INT(LEN(cEndDateC), C_SIZE_T))
    cEndDate  = ''
    DO i = 1, LEN(cEndDateC)
      IF (cEndDateC(i:i) == C_NULL_CHAR) EXIT
      cEndDate(i:i) = cEndDateC(i:i)
    END DO
    WRITE (cEndTime, '(2I2.2)') iSecEnd / 3600, MOD(iSecEnd / 60, 60)

    tss = hec_zstructTsNewTimes( &
            f_to_c_string(localPath), &
            f_to_c_string(localDate), &
            f_to_c_string(localTime), &
            f_to_c_string(TRIM(cEndDate)), &
            f_to_c_string(cEndTime))

    IF (.NOT. C_ASSOCIATED(tss)) THEN
      ISTAT = 12
      RETURN
    END IF

    ! Retrieve the data (floats, no quality notes)
    iStatus = hec_ztsRetrieve(IFLTAB, tss, DSS_RETRIEVE_REGULAR, 0_C_INT, 0_C_INT)

    IF (iStatus >= 0) THEN
      nRetrieved = get_ts_number_values(tss)
      IF (nRetrieved > 0) THEN
        ALLOCATE (rBuf(nRetrieved))
        CALL get_ts_float_values(tss, rBuf, nRetrieved)
        nCopy = MIN(NVALS, nRetrieved)
        VALUES(1:nCopy) = rBuf(1:nCopy)
      END IF

      ! Get units and type strings
      tmpUnits = get_ts_units(tss)
      tmpType  = get_ts_type(tss)
      CUNITS   = TRIM(tmpUnits)
      CTYPE    = TRIM(tmpType)
    END IF

    ! Free the struct
    CALL hec_zstructFree(tss)

    ! Legacy missing flag and status
    nMissing = 0
    DO i = 1, NVALS
      IF (VALUES(i) .LT. -1.0E30 .OR. VALUES(i) .EQ. -901.0 .OR. VALUES(i) .EQ. -902.0) THEN
        VALUES(i) = rMissing
        nMissing  = nMissing + 1
      END IF
    END DO
    IF (nMissing .EQ. 0) THEN
      ISTAT = 0
    ELSE IF (nMissing .LT. NVALS) THEN
      ISTAT = 1
    ELSE
      ISTAT = 4
    END IF

  END SUBROUTINE ZRRTS

  !--------------------------------------------------------------
  ! ZSET - Set DSS configuration parameter
  ! Args: param, val_string, val_int
  !--------------------------------------------------------------
  SUBROUTINE ZSET(CPARAM, CVAL, IVAL)
    CHARACTER(LEN=*), INTENT(IN) :: CPARAM
    CHARACTER(LEN=*), INTENT(IN) :: CVAL
    INTEGER, INTENT(IN) :: IVAL

    INTEGER(C_INT) :: iStatus

    iStatus = hec_zset( &
                f_to_c_string(TRIM(CPARAM)), &
                f_to_c_string(TRIM(CVAL)), &
                INT(IVAL, C_INT))

  END SUBROUTINE ZSET

  !--------------------------------------------------------------
  ! ZFNAME - Check if DSS file exists and get version
  ! Args: FileName, cName, iNName, lExist
  !--------------------------------------------------------------
  SUBROUTINE ZFNAME(CFNAME, CNAME, INNAME, LEXIST)
    CHARACTER(LEN=*), INTENT(IN) :: CFNAME
    CHARACTER(LEN=*), INTENT(OUT) :: CNAME
    INTEGER, INTENT(OUT) :: INNAME
    LOGICAL, INTENT(OUT) :: LEXIST

    INTEGER(C_INT) :: iVersion

    iVersion = hec_zgetFileVersion(f_to_c_string(TRIM(CFNAME)))

    IF (iVersion > 0) THEN
      INNAME = iVersion
      LEXIST = .TRUE.
      CNAME = CFNAME
    ELSE
      INNAME = 0
      LEXIST = .FALSE.
      CNAME = ''
    END IF

  END SUBROUTINE ZFNAME

  !--------------------------------------------------------------
  ! ZUFPN - Unpack full pathname into parts
  ! Legacy signature: (APart, lenA, BPart, lenB, CPart, lenC,
  !                    DPart, lenD, EPart, lenE, FPart, lenF,
  !                    PathName, lenPath, ErrorCode)
  !--------------------------------------------------------------
  SUBROUTINE ZUFPN(CAPTS, NAPTS, CABTS, NABTS, CACTS, NACTS, &
                   CADTS, NADTS, CAETS, NAETS, CAFTS, NAFTS, &
                   CPATH, NPATH, ISTAT)
    CHARACTER(LEN=*), INTENT(OUT) :: CAPTS, CABTS, CACTS, CADTS, CAETS, CAFTS
    INTEGER, INTENT(OUT) :: NAPTS, NABTS, NACTS, NADTS, NAETS, NAFTS
    CHARACTER(LEN=*), INTENT(IN) :: CPATH
    INTEGER, INTENT(IN) :: NPATH
    INTEGER, INTENT(OUT) :: ISTAT

    CHARACTER(LEN=65, KIND=C_CHAR) :: cPart
    INTEGER(C_INT) :: iStatus
    INTEGER :: i

    ! Initialize outputs
    CAPTS = ''
    CABTS = ''
    CACTS = ''
    CADTS = ''
    CAETS = ''
    CAFTS = ''
    ISTAT = 0

    ! Note: zpathnameGetPart returns:
    !   - Length of part on success (>=0)
    !   - STATUS_NOT_OKAY (-1) on error
    ! So only negative values indicate errors

    ! Get A part (position 1)
    cPart = ''
    iStatus = hec_zpathnameGetPart(f_to_c_string(CPATH(1:NPATH)), 1_C_INT, cPart, 64_C_SIZE_T)
    IF (iStatus < 0) ISTAT = iStatus
    DO i = 1, MIN(LEN(CAPTS), 64)
      IF (cPart(i:i) == C_NULL_CHAR) EXIT
      CAPTS(i:i) = cPart(i:i)
    END DO

    ! Get B part (position 2)
    cPart = ''
    iStatus = hec_zpathnameGetPart(f_to_c_string(CPATH(1:NPATH)), 2_C_INT, cPart, 64_C_SIZE_T)
    IF (iStatus < 0) ISTAT = iStatus
    DO i = 1, MIN(LEN(CABTS), 64)
      IF (cPart(i:i) == C_NULL_CHAR) EXIT
      CABTS(i:i) = cPart(i:i)
    END DO

    ! Get C part (position 3)
    cPart = ''
    iStatus = hec_zpathnameGetPart(f_to_c_string(CPATH(1:NPATH)), 3_C_INT, cPart, 64_C_SIZE_T)
    IF (iStatus < 0) ISTAT = iStatus
    DO i = 1, MIN(LEN(CACTS), 64)
      IF (cPart(i:i) == C_NULL_CHAR) EXIT
      CACTS(i:i) = cPart(i:i)
    END DO

    ! Get D part (position 4)
    cPart = ''
    iStatus = hec_zpathnameGetPart(f_to_c_string(CPATH(1:NPATH)), 4_C_INT, cPart, 64_C_SIZE_T)
    IF (iStatus < 0) ISTAT = iStatus
    DO i = 1, MIN(LEN(CADTS), 64)
      IF (cPart(i:i) == C_NULL_CHAR) EXIT
      CADTS(i:i) = cPart(i:i)
    END DO

    ! Get E part (position 5)
    cPart = ''
    iStatus = hec_zpathnameGetPart(f_to_c_string(CPATH(1:NPATH)), 5_C_INT, cPart, 64_C_SIZE_T)
    IF (iStatus < 0) ISTAT = iStatus
    DO i = 1, MIN(LEN(CAETS), 64)
      IF (cPart(i:i) == C_NULL_CHAR) EXIT
      CAETS(i:i) = cPart(i:i)
    END DO

    ! Get F part (position 6)
    cPart = ''
    iStatus = hec_zpathnameGetPart(f_to_c_string(CPATH(1:NPATH)), 6_C_INT, cPart, 64_C_SIZE_T)
    IF (iStatus < 0) ISTAT = iStatus
    DO i = 1, MIN(LEN(CAFTS), 64)
      IF (cPart(i:i) == C_NULL_CHAR) EXIT
      CAFTS(i:i) = cPart(i:i)
    END DO

    ! Set lengths
    NAPTS = LEN_TRIM(CAPTS)
    NABTS = LEN_TRIM(CABTS)
    NACTS = LEN_TRIM(CACTS)
    NADTS = LEN_TRIM(CADTS)
    NAETS = LEN_TRIM(CAETS)
    NAFTS = LEN_TRIM(CAFTS)

  END SUBROUTINE ZUFPN

  !--------------------------------------------------------------
  ! ZPATH - Create pathname from parts
  ! Args: APart, BPart, CPart, DPart, EPart, FPart, PathName, PathLen
  !--------------------------------------------------------------
  SUBROUTINE ZPATH(CAPTS, CABTS, CACTS, CADTS, CAETS, CAFTS, CPATH, NPATH)
    CHARACTER(LEN=*), INTENT(IN) :: CAPTS, CABTS, CACTS, CADTS, CAETS, CAFTS
    CHARACTER(LEN=*), INTENT(OUT) :: CPATH
    INTEGER, INTENT(OUT) :: NPATH

    CHARACTER(LEN=393, KIND=C_CHAR) :: cPathname
    INTEGER(C_INT) :: iStatus
    INTEGER :: i

    CPATH = ''
    cPathname = ''

    iStatus = hec_zpathnameForm( &
                f_to_c_string(TRIM(CAPTS)), &
                f_to_c_string(TRIM(CABTS)), &
                f_to_c_string(TRIM(CACTS)), &
                f_to_c_string(TRIM(CADTS)), &
                f_to_c_string(TRIM(CAETS)), &
                f_to_c_string(TRIM(CAFTS)), &
                cPathname, 392_C_SIZE_T)

    ! Copy to output (remove null terminator)
    DO i = 1, MIN(LEN(CPATH), 392)
      IF (cPathname(i:i) == C_NULL_CHAR) EXIT
      CPATH(i:i) = cPathname(i:i)
    END DO
    NPATH = LEN_TRIM(CPATH)

  END SUBROUTINE ZPATH

  !--------------------------------------------------------------
  ! ZGINTL - Get interval from pathname E-part
  ! Args: Interval (output), EPart (input), DummyInt, ErrorCode
  !--------------------------------------------------------------
  SUBROUTINE ZGINTL(INTL, CEPART, IDUM, ISTAT)
    INTEGER, INTENT(OUT) :: INTL
    CHARACTER(LEN=*), INTENT(IN) :: CEPART
    INTEGER, INTENT(IN) :: IDUM
    INTEGER, INTENT(INOUT) :: ISTAT

    CHARACTER(LEN=32) :: cEPartUpper
    INTEGER :: i

    ! Convert E-part to uppercase for comparison
    cEPartUpper = CEPART
    DO i = 1, LEN_TRIM(cEPartUpper)
      IF (cEPartUpper(i:i) >= 'a' .AND. cEPartUpper(i:i) <= 'z') THEN
        cEPartUpper(i:i) = CHAR(ICHAR(cEPartUpper(i:i)) - 32)
      END IF
    END DO

    ! Map E-part strings to interval in minutes
    SELECT CASE (TRIM(cEPartUpper))
      CASE ('1MIN')
        INTL = 1
      CASE ('2MIN')
        INTL = 2
      CASE ('3MIN')
        INTL = 3
      CASE ('4MIN')
        INTL = 4
      CASE ('5MIN')
        INTL = 5
      CASE ('6MIN')
        INTL = 6
      CASE ('10MIN')
        INTL = 10
      CASE ('12MIN')
        INTL = 12
      CASE ('15MIN')
        INTL = 15
      CASE ('20MIN')
        INTL = 20
      CASE ('30MIN')
        INTL = 30
      CASE ('1HOUR')
        INTL = 60
      CASE ('2HOUR')
        INTL = 120
      CASE ('3HOUR')
        INTL = 180
      CASE ('4HOUR')
        INTL = 240
      CASE ('6HOUR')
        INTL = 360
      CASE ('8HOUR')
        INTL = 480
      CASE ('12HOUR')
        INTL = 720
      CASE ('1DAY')
        INTL = 1440
      CASE ('1WEEK')
        INTL = 10080
      CASE ('1MON', '1MONTH')
        INTL = 43200  ! Approximate
      CASE ('1YEAR')
        INTL = 525600  ! Approximate
      CASE DEFAULT
        INTL = 0
        ISTAT = -1
    END SELECT

  END SUBROUTINE ZGINTL

  !--------------------------------------------------------------
  ! ZOFSET - Time offset from the standard time of an interval
  ! Args: JulianDate, Minutes, Interval, Flag, Offset
  !--------------------------------------------------------------
  SUBROUTINE ZOFSET(IJULDAY, ITIME, INTL, IFLAG, IOFSET)
    !Legacy HECLIB contract (as relied on by Class_DSSFileType):
    !  - IOFSET is the offset, in minutes, of IJULDAY/ITIME from the standard
    !    (end-of-period) time of interval INTL (minutes)
    !  - IFLAG = 1 also moves IJULDAY/ITIME forward to that standard time, e.g.
    !    01OCT 2400 with a 1MON interval becomes 31OCT 2400; times already on
    !    a standard time are unchanged
    !  - the end of a day is 2400 (ITIME = 1440) of that day, not 0000 of the next
    INTEGER, INTENT(INOUT) :: IJULDAY
    INTEGER, INTENT(INOUT) :: ITIME
    INTEGER, INTENT(IN) :: INTL
    INTEGER, INTENT(IN) :: IFLAG
    INTEGER, INTENT(OUT) :: IOFSET

    INTEGER(C_INT) :: iJul, iSec, iOffset

    IOFSET = 0
    IF (INTL .LE. 0) RETURN

    iJul = INT(IJULDAY, C_INT)
    iSec = INT(ITIME, C_INT) * 60_C_INT

    IF (IFLAG .EQ. 1) THEN
      iOffset = hec_ztsOffsetAdjustToStandard(INT(INTL, C_INT) * 60_C_INT, iJul, iSec)
      IF (iOffset .LT. 0) RETURN   !Not a standard interval; leave the time as is
      IF (iSec .EQ. 0) THEN
        iJul = iJul - 1_C_INT
        iSec = 86400_C_INT
      END IF
      IJULDAY = iJul
      ITIME   = iSec / 60_C_INT
    ELSE
      iOffset = hec_ztsOffset(INT(INTL, C_INT) * 60_C_INT, iJul, iSec)
      IF (iOffset .LT. 0) RETURN
    END IF
    IOFSET = iOffset / 60_C_INT

  END SUBROUTINE ZOFSET

  !--------------------------------------------------------------
  ! ZBEGDT - Get beginning date/time of a data block
  ! Args: Julian, Interval, Year, Month, Day, Block, IFLTAB_elem
  !--------------------------------------------------------------
  SUBROUTINE ZBEGDT(IJULDAY, INTL, IYR, IMON, IDAY, IBLOCK, IFLTAB_ELEM)
    INTEGER, INTENT(IN) :: IJULDAY
    INTEGER, INTENT(IN) :: INTL
    INTEGER, INTENT(OUT) :: IYR
    INTEGER, INTENT(OUT) :: IMON
    INTEGER, INTENT(OUT) :: IDAY
    INTEGER, INTENT(OUT) :: IBLOCK
    INTEGER(C_LONG_LONG), INTENT(IN) :: IFLTAB_ELEM

    INTEGER(C_INT) :: cYear, cMonth, cDay

    ! Convert Julian date to year/month/day
    CALL hec_julianToYearMonthDay(INT(IJULDAY, C_INT), cYear, cMonth, cDay)

    IYR = cYear
    IMON = cMonth
    IDAY = cDay
    IBLOCK = 1  ! Placeholder

  END SUBROUTINE ZBEGDT

  !--------------------------------------------------------------
  ! ZINCBK - Increment block number
  ! Args: Block, Julian, Year, Month, Day
  !--------------------------------------------------------------
  SUBROUTINE ZINCBK(IBLOCK, IJUL, IYR, IMON, IDAY)
    INTEGER, INTENT(INOUT) :: IBLOCK
    INTEGER, INTENT(INOUT) :: IJUL
    INTEGER, INTENT(INOUT) :: IYR
    INTEGER, INTENT(INOUT) :: IMON
    INTEGER, INTENT(INOUT) :: IDAY

    INTEGER(C_INT) :: cYear, cMonth, cDay

    ! Increment the block and move to next month (typical block size)
    IBLOCK = IBLOCK + 1
    IMON = IMON + 1
    IF (IMON > 12) THEN
      IMON = 1
      IYR = IYR + 1
    END IF
    IDAY = 1

    ! Recalculate Julian date
    IJUL = hec_yearMonthDayToJulian(INT(IYR, C_INT), INT(IMON, C_INT), INT(IDAY, C_INT))

  END SUBROUTINE ZINCBK

  !--------------------------------------------------------------
  ! ZDTYPE - Get data type from pathname
  ! Args: IFLTAB, PathName, NHEAD, LRECORDFOUND, CDTYPE, IDTYPE
  !--------------------------------------------------------------
  SUBROUTINE ZDTYPE(IFLTAB, CPATH, NHEAD, LRECORDFOUND, CDTYPE, IDTYPE)
    INTEGER(C_LONG_LONG), INTENT(IN) :: IFLTAB(*)
    CHARACTER(LEN=*), INTENT(IN) :: CPATH
    INTEGER, INTENT(OUT) :: NHEAD
    LOGICAL, INTENT(OUT) :: LRECORDFOUND
    CHARACTER(LEN=*), INTENT(OUT) :: CDTYPE
    INTEGER, INTENT(OUT) :: IDTYPE

    ! Placeholder implementation
    ! In a full implementation, this would query the DSS file
    NHEAD = 0
    LRECORDFOUND = .FALSE.
    CDTYPE = ''
    IDTYPE = 0  ! 0 means record not found

  END SUBROUTINE ZDTYPE

  !--------------------------------------------------------------
  ! ZDELET - Delete a record from DSS file
  ! Args: IFLTAB, PathName, PathLen, Deleted
  !--------------------------------------------------------------
  SUBROUTINE ZDELET(IFLTAB, CPATH, NPATH, LDELETED)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLTAB(*)
    CHARACTER(LEN=*), INTENT(IN) :: CPATH
    INTEGER, INTENT(IN) :: NPATH
    LOGICAL, INTENT(OUT) :: LDELETED

    INTEGER(C_INT) :: iStatus

    iStatus = hec_zdelete(IFLTAB, f_to_c_string(CPATH(1:NPATH)))
    LDELETED = (iStatus == DSS_STATUS_OK)

  END SUBROUTINE ZDELET

  !--------------------------------------------------------------
  ! ZCOFIL - Copy/compress DSS file
  ! Args: IFLTAB_Old, IFLTAB_New, BUFF1, KBUFF1, BUFF2, KBUFF2, BOOL1, BOOL2
  !--------------------------------------------------------------
  SUBROUTINE ZCOFIL(IFLOLD, IFLNEW, BUFF1, KBUFF1, BUFF2, KBUFF2, LOPT1, LOPT2)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLOLD(*)
    INTEGER(C_LONG_LONG), INTENT(INOUT) :: IFLNEW(*)
    REAL, INTENT(INOUT) :: BUFF1(*)
    INTEGER, INTENT(IN) :: KBUFF1
    REAL, INTENT(INOUT) :: BUFF2(*)
    INTEGER, INTENT(IN) :: KBUFF2
    LOGICAL, INTENT(IN) :: LOPT1
    LOGICAL, INTENT(IN) :: LOPT2

    ! Placeholder implementation
    ! Full implementation would copy records from old to new file

  END SUBROUTINE ZCOFIL

  !--------------------------------------------------------------
  ! IYMDJL - Convert year, month, day to Julian date
  ! Returns: Julian date as INTEGER
  !--------------------------------------------------------------
  INTEGER FUNCTION IYMDJL(IY, IM, ID)
    INTEGER, INTENT(IN) :: IY, IM, ID

    IYMDJL = hec_yearMonthDayToJulian( &
               INT(IY, C_INT), &
               INT(IM, C_INT), &
               INT(ID, C_INT))

  END FUNCTION IYMDJL

  !--------------------------------------------------------------
  ! JLIYMD - Convert Julian date to year, month, day
  ! Returns: 0 on success (legacy behavior)
  ! Note: This is called as a function in legacy code
  !--------------------------------------------------------------
  INTEGER FUNCTION JLIYMD(IJUL, IY, IM, ID)
    INTEGER, INTENT(IN) :: IJUL
    INTEGER, INTENT(OUT) :: IY, IM, ID

    INTEGER(C_INT) :: cYear, cMonth, cDay

    CALL hec_julianToYearMonthDay(INT(IJUL, C_INT), cYear, cMonth, cDay)

    IY = cYear
    IM = cMonth
    ID = cDay
    JLIYMD = 0  ! Success

  END FUNCTION JLIYMD

  !--------------------------------------------------------------
  ! INCTIM - Increment a date/time by a number of intervals
  ! Args: Interval, Flag (0: interval in minutes, 1: in days), NPeriods,
  !       StartJulian, StartMinutes, EndJulian, EndMinutes
  ! Returns: 0 (legacy callers ignore the return value)
  ! Note: Monthly and longer intervals (e.g. 43200 minutes = 1MON) are
  !       handled as calendar intervals by HEC-DSS 7 incrementTime.
  !--------------------------------------------------------------
  INTEGER FUNCTION INCTIM(INTL, IFLAG, NPER, JULS, ISTIME, JULE, IETIME)
    INTEGER, INTENT(IN)  :: INTL, IFLAG, NPER, JULS, ISTIME
    INTEGER, INTENT(OUT) :: JULE, IETIME

    INTEGER(C_INT) :: iIntervalSeconds, iJulEnd, iSecEnd, iDummy

    IF (IFLAG .EQ. 1) THEN
      iIntervalSeconds = INT(INTL, C_INT) * 86400_C_INT
    ELSE
      iIntervalSeconds = INT(INTL, C_INT) * 60_C_INT
    END IF

    IF (iIntervalSeconds .EQ. 0 .OR. NPER .EQ. 0) THEN
      JULE   = JULS
      IETIME = ISTIME
    ELSE
      iDummy = hec_incrementTime(iIntervalSeconds, INT(NPER, C_INT), INT(JULS, C_INT), &
                                 INT(ISTIME, C_INT) * 60_C_INT, iJulEnd, iSecEnd)
      JULE   = iJulEnd
      IETIME = iSecEnd / 60
    END IF
    INCTIM = 0

  END FUNCTION INCTIM

END MODULE heclib_compat
