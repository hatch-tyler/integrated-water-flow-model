!***********************************************************************
!  IWFM DSS Read Diagnostic Test
!
!  Standalone program to test reading DSS pathnames in isolation,
!  without the full IWFM model stack. Useful for diagnosing crashes
!  when reading large DSS files with many pathnames.
!
!  Usage: test_dss_read.exe <dss_file> <num_pathnames> <pathname_prefix>
!
!  Example:
!    test_dss_read.exe climate_data.dss 33561 "/C2VSIMFG/ELEM_"
!    test_dss_read.exe TSDATA_IN.DSS 2 "/SAMPLE_PROBLEM/GAGE"
!
!  The program tests three operations per pathname:
!    1. ZUFPN  - Unpack pathname into A/B/C/D/E/F parts
!    2. ZGINTL - Get time interval from E-part
!    3. ZRRTS  - Read 1 value at Year 4000 (the "Year 4000 flag" check)
!
!  Progress is reported every 1000 pathnames and for the first 5.
!***********************************************************************
PROGRAM test_dss_read
  USE heclib_compat
  USE ISO_C_BINDING
  IMPLICIT NONE

  ! DSS file handle
  INTEGER(C_LONG_LONG) :: IFLTAB(250)

  ! Local variables
  INTEGER :: ISTAT, i, nPaths, Interval, ErrorCode, NVALS, DummyInt, IOFSET
  CHARACTER(LEN=400) :: PathName
  CHARACTER(LEN=65)  :: APart, BPart, CPart, DPart, EPart, FPart
  CHARACTER(LEN=32)  :: cUnits, cType
  REAL               :: rValue(1)

  ! Command line arguments
  CHARACTER(LEN=512) :: DSSFile
  CHARACTER(LEN=20)  :: nPathsArg
  CHARACTER(LEN=256) :: PathPrefix
  CHARACTER(LEN=64)  :: CSuffix

  ! Timing
  REAL :: t_start, t_end, t_zufpn, t_zgintl, t_zrrts
  REAL :: total_zufpn, total_zgintl, total_zrrts
  INTEGER :: nFailed_zufpn, nFailed_zgintl, nFailed_zrrts

  ! Default pathname suffix (C-part, D-part, E-part, F-part)
  CSuffix = '/PRECIP//1MON/V1.5/'

  ! Parse command line arguments
  IF (COMMAND_ARGUMENT_COUNT() < 2) THEN
    WRITE(*,*) 'Usage: test_dss_read <dss_file> <num_pathnames> [pathname_prefix]'
    WRITE(*,*) ''
    WRITE(*,*) 'Arguments:'
    WRITE(*,*) '  dss_file        Path to HEC-DSS file'
    WRITE(*,*) '  num_pathnames   Number of pathnames to test (1..N)'
    WRITE(*,*) '  pathname_prefix Optional prefix (default: /C2VSIMFG/ELEM_)'
    WRITE(*,*) ''
    WRITE(*,*) 'Generated pathnames: <prefix><N>/PRECIP//1MON/V1.5/'
    STOP
  END IF

  CALL GET_COMMAND_ARGUMENT(1, DSSFile)
  CALL GET_COMMAND_ARGUMENT(2, nPathsArg)
  READ(nPathsArg, *, IOSTAT=ErrorCode) nPaths
  IF (ErrorCode /= 0) THEN
    WRITE(*,*) 'ERROR: Could not parse num_pathnames: ', TRIM(nPathsArg)
    STOP 1
  END IF

  PathPrefix = '/C2VSIMFG/ELEM_'
  IF (COMMAND_ARGUMENT_COUNT() >= 3) THEN
    CALL GET_COMMAND_ARGUMENT(3, PathPrefix)
  END IF

  IF (COMMAND_ARGUMENT_COUNT() >= 4) THEN
    CALL GET_COMMAND_ARGUMENT(4, CSuffix)
  END IF

  ! Initialize counters
  total_zufpn  = 0.0
  total_zgintl = 0.0
  total_zrrts  = 0.0
  nFailed_zufpn  = 0
  nFailed_zgintl = 0
  nFailed_zrrts  = 0

  ! Suppress DSS messaging
  CALL ZSET('MLEVEL', '', 0)

  ! Open DSS file
  WRITE(*,*) '================================================'
  WRITE(*,*) 'IWFM DSS Read Diagnostic Test'
  WRITE(*,*) '================================================'
  WRITE(*,*) 'DSS File:    ', TRIM(DSSFile)
  WRITE(*,*) 'Num Paths:   ', nPaths
  WRITE(*,*) 'Path Prefix: ', TRIM(PathPrefix)
  WRITE(*,*) 'Path Suffix: ', TRIM(CSuffix)
  WRITE(*,*) ''

  WRITE(*,*) 'Opening DSS file...'
  CALL ZOPEN(IFLTAB, TRIM(DSSFile), ISTAT)
  WRITE(*,*) 'ZOPEN status:', ISTAT
  IF (ISTAT /= 0) THEN
    WRITE(*,*) 'ERROR: ZOPEN failed with status ', ISTAT
    STOP 1
  END IF
  WRITE(*,*) 'DSS file opened successfully.'
  WRITE(*,*) ''

  ! Loop through pathnames
  WRITE(*,*) 'Beginning pathname loop...'
  CALL CPU_TIME(t_start)

  DO i = 1, nPaths
    ! Build pathname
    WRITE(PathName, '(A,I0,A)') TRIM(PathPrefix), i, TRIM(CSuffix)

    ! --- Test 1: ZUFPN (unpack pathname) ---
    CALL CPU_TIME(t_zufpn)
    CALL ZUFPN(APart, DummyInt, BPart, DummyInt, CPart, DummyInt, &
               DPart, DummyInt, EPart, DummyInt, FPart, DummyInt, &
               PathName, LEN_TRIM(PathName), ErrorCode)
    CALL CPU_TIME(t_zgintl)
    total_zufpn = total_zufpn + (t_zgintl - t_zufpn)

    IF (ErrorCode /= 0) THEN
      nFailed_zufpn = nFailed_zufpn + 1
      IF (nFailed_zufpn <= 5) THEN
        WRITE(*,*) 'ZUFPN FAILED at pathname ', i, ' ErrorCode=', ErrorCode
        WRITE(*,*) '  Pathname: ', TRIM(PathName)
      END IF
      CYCLE
    END IF

    ! --- Test 2: ZGINTL (get interval from E-part) ---
    DummyInt = 1
    CALL ZGINTL(Interval, EPart, DummyInt, DummyInt)
    CALL CPU_TIME(t_zrrts)
    total_zgintl = total_zgintl + (t_zrrts - t_zgintl)

    IF (Interval <= 0) THEN
      nFailed_zgintl = nFailed_zgintl + 1
      IF (nFailed_zgintl <= 5) THEN
        WRITE(*,*) 'ZGINTL returned interval <= 0 at pathname ', i
        WRITE(*,*) '  EPart: ', TRIM(EPart)
      END IF
    END IF

    ! --- Test 3: ZRRTS (Year 4000 flag check) ---
    NVALS = 1
    CALL ZRRTS(IFLTAB, PathName, '01JAN4000', '0001', NVALS, rValue(1), &
               cUnits, cType, IOFSET, ErrorCode)
    CALL CPU_TIME(t_end)
    total_zrrts = total_zrrts + (t_end - t_zrrts)

    IF (ErrorCode /= 0) THEN
      nFailed_zrrts = nFailed_zrrts + 1
    END IF

    ! Progress reporting
    IF (i <= 5 .OR. MOD(i, 1000) == 0 .OR. i == nPaths) THEN
      WRITE(*,'(A,I7,A,I7,A,F8.3,A,A,I6,A,I6,A,I6)') &
        '  Path ', i, ' / ', nPaths, &
        '  elapsed=', (t_end - t_start), 's', &
        '  zufpn_fail=', nFailed_zufpn, &
        '  zgintl_fail=', nFailed_zgintl, &
        '  zrrts_fail=', nFailed_zrrts
    END IF
  END DO

  CALL CPU_TIME(t_end)

  ! Summary
  WRITE(*,*) ''
  WRITE(*,*) '================================================'
  WRITE(*,*) 'RESULTS'
  WRITE(*,*) '================================================'
  WRITE(*,'(A,I7)')      '  Total pathnames:   ', nPaths
  WRITE(*,'(A,F10.3,A)') '  Total CPU time:    ', (t_end - t_start), ' seconds'
  WRITE(*,'(A,F10.3,A)') '  ZUFPN  total:      ', total_zufpn,  ' seconds'
  WRITE(*,'(A,F10.3,A)') '  ZGINTL total:      ', total_zgintl, ' seconds'
  WRITE(*,'(A,F10.3,A)') '  ZRRTS  total:      ', total_zrrts,  ' seconds'
  WRITE(*,'(A,I7)')      '  ZUFPN  failures:   ', nFailed_zufpn
  WRITE(*,'(A,I7)')      '  ZGINTL failures:   ', nFailed_zgintl
  WRITE(*,'(A,I7)')      '  ZRRTS  failures:   ', nFailed_zrrts
  WRITE(*,*) ''

  IF (nFailed_zufpn == 0 .AND. nFailed_zgintl == 0) THEN
    WRITE(*,*) 'All ', nPaths, ' pathnames processed successfully.'
  ELSE
    WRITE(*,*) 'WARNING: Some pathnames had failures. See above for details.'
  END IF

  ! --- Phase 2: Test real-date reads for ALL pathnames ---
  WRITE(*,*) ''
  WRITE(*,*) '================================================'
  WRITE(*,*) 'REAL DATA READ TEST (30 Sep 1973 / 2400) - ALL'
  WRITE(*,*) '================================================'
  WRITE(*,*) 'Reading actual data for all pathnames...'

  nFailed_zrrts = 0
  CALL CPU_TIME(t_start)

  DO i = 1, nPaths
    WRITE(PathName, '(A,I0,A)') TRIM(PathPrefix), i, TRIM(CSuffix)

    NVALS = 1
    CALL ZRRTS(IFLTAB, PathName, '30 Sep 1973', '2400', NVALS, rValue(1), &
               cUnits, cType, IOFSET, ErrorCode)

    IF (ErrorCode /= 0) THEN
      nFailed_zrrts = nFailed_zrrts + 1
      IF (nFailed_zrrts <= 5) THEN
        WRITE(*,*) '  ZRRTS real-date FAILED at path ', i, ' err=', ErrorCode
      END IF
    ELSE IF (i <= 5) THEN
      WRITE(*,'(A,I5,A,F12.4,A,A,A,A)') &
        '  Path ', i, ': value=', rValue(1), ' units=', TRIM(cUnits), ' type=', TRIM(cType)
    END IF

    IF (MOD(i, 5000) == 0 .OR. i == nPaths) THEN
      CALL CPU_TIME(t_end)
      WRITE(*,'(A,I7,A,I7,A,F8.3,A,A,I7)') &
        '  Progress: ', i, ' / ', nPaths, '  elapsed=', (t_end - t_start), 's', &
        '  failures=', nFailed_zrrts
    END IF
  END DO

  CALL CPU_TIME(t_end)
  WRITE(*,'(A,I7,A,I7)') '  Real-date reads: ', nPaths, '  failures: ', nFailed_zrrts
  WRITE(*,'(A,F10.3,A)') '  Time: ', (t_end - t_start), ' seconds'

  ! Close DSS file
  WRITE(*,*) ''
  WRITE(*,*) 'Closing DSS file...'
  CALL ZCLOSE(IFLTAB)
  WRITE(*,*) 'Done.'

END PROGRAM test_dss_read
