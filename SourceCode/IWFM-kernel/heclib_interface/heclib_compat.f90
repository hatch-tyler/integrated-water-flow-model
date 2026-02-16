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
  PUBLIC :: IYMDJL, JLIYMD

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

    TYPE(C_PTR) :: tss
    INTEGER :: nRetrieved, nCopy
    CHARACTER(LEN=32) :: tmpUnits, tmpType

    ! Local copies of input strings to ensure stable C string temporaries
    CHARACTER(LEN=LEN_TRIM(CPATH)) :: localPath
    CHARACTER(LEN=LEN_TRIM(CDATE)) :: localDate
    CHARACTER(LEN=LEN_TRIM(CTIME)) :: localTime

    ! Initialize outputs
    ISTAT = 0
    IOFSET = 0
    CUNITS = ''
    CTYPE = ''

    ! Make local copies to avoid any issues with string temporaries
    localPath = TRIM(CPATH)
    localDate = TRIM(CDATE)
    localTime = TRIM(CTIME)

    ! Create time series struct with time window
    tss = hec_zstructTsNewTimes( &
            f_to_c_string(localPath), &
            f_to_c_string(localDate), &
            f_to_c_string(localTime), &
            f_to_c_string(''), &  ! End date - empty to use numberValues
            f_to_c_string(''))    ! End time

    IF (.NOT. C_ASSOCIATED(tss)) THEN
      ISTAT = -1
      RETURN
    END IF

    ! Retrieve the data (floats, no quality notes)
    ISTAT = hec_ztsRetrieve(IFLTAB, tss, DSS_RETRIEVE_REGULAR, 0_C_INT, 0_C_INT)

    IF (ISTAT >= 0) THEN
      ! Extract the data from the struct
      nRetrieved = get_ts_number_values(tss)

      ! Copy float values to output array with bounds protection
      IF (nRetrieved > 0 .AND. NVALS > 0) THEN
        nCopy = MIN(NVALS, nRetrieved)
        CALL get_ts_float_values(tss, VALUES(1:nCopy), nCopy)
      END IF

      ! Get units and type strings
      tmpUnits = get_ts_units(tss)
      tmpType = get_ts_type(tss)

      ! Copy to output (trimmed to fit)
      CUNITS = TRIM(tmpUnits)
      CTYPE = TRIM(tmpType)

      ! Set success status
      ISTAT = 0
    END IF

    ! Free the struct
    CALL hec_zstructFree(tss)

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
      CASE ('1MON')
        INTL = 43200  ! Approximate
      CASE ('1YEAR')
        INTL = 525600  ! Approximate
      CASE DEFAULT
        INTL = 0
        ISTAT = -1
    END SELECT

  END SUBROUTINE ZGINTL

  !--------------------------------------------------------------
  ! ZOFSET - Calculate time offset
  ! Args: JulianDate, Minutes, Interval, Flag, Offset
  !--------------------------------------------------------------
  SUBROUTINE ZOFSET(IJULDAY, ITIME, INTL, IFLAG, IOFSET)
    INTEGER, INTENT(INOUT) :: IJULDAY
    INTEGER, INTENT(INOUT) :: ITIME
    INTEGER, INTENT(IN) :: INTL
    INTEGER, INTENT(IN) :: IFLAG
    INTEGER, INTENT(OUT) :: IOFSET

    ! Placeholder implementation
    ! The offset is the number of minutes from the standard time boundary
    IF (INTL > 0) THEN
      IOFSET = MOD(ITIME, INTL)
    ELSE
      IOFSET = 0
    END IF

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

END MODULE heclib_compat
