!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2025
!  State of California, Department of Water Resources
!
!  HEC-DSS 7 C Library Fortran Bindings
!
!  This module provides ISO_C_BINDING interfaces to the HEC-DSS 7
!  C library functions. The HEC-DSS 7 library uses a C API that
!  differs from the original Fortran 77 HECLIB interface.
!
!  Key differences from legacy HECLIB:
!  - IFLTAB is now long long[250] instead of int[600]
!  - Time series operations use zStructTimeSeries struct
!  - Functions return int status instead of using ISTAT parameter
!***********************************************************************
MODULE heclib_c_binding
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  PRIVATE

  ! Public constants
  PUBLIC :: IFLTAB_SIZE
  PUBLIC :: DSS_STATUS_OK, DSS_STATUS_NOT_OK

  ! Public interfaces
  PUBLIC :: hec_zopen, hec_zclose
  PUBLIC :: hec_ztsRetrieve, hec_ztsStore
  PUBLIC :: hec_zstructTsNew, hec_zstructTsNewTimes
  PUBLIC :: hec_zstructTsNewRegFloats, hec_zstructTsNewRegDoubles
  PUBLIC :: hec_zstructFree
  PUBLIC :: hec_zdelete
  PUBLIC :: hec_zgetFileVersion, hec_zgetVersion
  PUBLIC :: hec_zset, hec_zquery
  PUBLIC :: hec_zpathnameGetPart, hec_zpathnameForm
  PUBLIC :: hec_yearMonthDayToJulian, hec_julianToYearMonthDay

  ! Public helper functions
  PUBLIC :: f_to_c_string, c_to_f_string
  PUBLIC :: c_strlen

  ! -------------------------------------------------------------
  ! --- CONSTANTS
  ! -------------------------------------------------------------
  ! HEC-DSS 7 uses long long[250] for ifltab
  INTEGER, PARAMETER :: IFLTAB_SIZE = 250

  ! Status codes
  INTEGER(C_INT), PARAMETER :: DSS_STATUS_OK = 0
  INTEGER(C_INT), PARAMETER :: DSS_STATUS_NOT_OK = -1

  ! Retrieve flags for ztsRetrieve
  INTEGER(C_INT), PARAMETER, PUBLIC :: DSS_RETRIEVE_REGULAR = 0
  INTEGER(C_INT), PARAMETER, PUBLIC :: DSS_RETRIEVE_TS = 1

  ! Storage flags for ztsStore
  INTEGER(C_INT), PARAMETER, PUBLIC :: DSS_STORE_REGULAR = 0

  ! -------------------------------------------------------------
  ! --- C FUNCTION INTERFACES
  ! -------------------------------------------------------------
  INTERFACE

    !--------------------------------------------------------------
    ! File Open/Close Functions
    !--------------------------------------------------------------

    ! int hec_dss_zopen(long long *ifltab, const char *dssFilename)
    INTEGER(C_INT) FUNCTION hec_zopen(ifltab, dssFilename) &
                   BIND(C, NAME='hec_dss_zopen')
      IMPORT :: C_INT, C_LONG_LONG, C_CHAR
      INTEGER(C_LONG_LONG), INTENT(INOUT) :: ifltab(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: dssFilename(*)
    END FUNCTION hec_zopen

    ! int zclose(long long *ifltab)
    INTEGER(C_INT) FUNCTION hec_zclose(ifltab) BIND(C, NAME='zclose')
      IMPORT :: C_INT, C_LONG_LONG
      INTEGER(C_LONG_LONG), INTENT(INOUT) :: ifltab(*)
    END FUNCTION hec_zclose

    ! int zgetFileVersion(const char *dssFilename)
    INTEGER(C_INT) FUNCTION hec_zgetFileVersion(dssFilename) &
                   BIND(C, NAME='zgetFileVersion')
      IMPORT :: C_INT, C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: dssFilename(*)
    END FUNCTION hec_zgetFileVersion

    ! int zgetVersion(long long *ifltab)
    INTEGER(C_INT) FUNCTION hec_zgetVersion(ifltab) &
                   BIND(C, NAME='zgetVersion')
      IMPORT :: C_INT, C_LONG_LONG
      INTEGER(C_LONG_LONG), INTENT(IN) :: ifltab(*)
    END FUNCTION hec_zgetVersion

    !--------------------------------------------------------------
    ! Time Series Functions
    !--------------------------------------------------------------

    ! int ztsRetrieve(long long *ifltab, zStructTimeSeries *tss,
    !                 int retrieveFlag, int boolRetrieveDoubles, int boolRetrieveQualityNotes)
    INTEGER(C_INT) FUNCTION hec_ztsRetrieve(ifltab, tss, retrieveFlag, &
                                            boolRetrieveDoubles, boolRetrieveQualityNotes) &
                   BIND(C, NAME='ztsRetrieve')
      IMPORT :: C_INT, C_LONG_LONG, C_PTR
      INTEGER(C_LONG_LONG), INTENT(INOUT) :: ifltab(*)
      TYPE(C_PTR), VALUE :: tss
      INTEGER(C_INT), VALUE :: retrieveFlag
      INTEGER(C_INT), VALUE :: boolRetrieveDoubles
      INTEGER(C_INT), VALUE :: boolRetrieveQualityNotes
    END FUNCTION hec_ztsRetrieve

    ! int ztsStore(long long *ifltab, zStructTimeSeries *tss, int storageFlag)
    INTEGER(C_INT) FUNCTION hec_ztsStore(ifltab, tss, storageFlag) &
                   BIND(C, NAME='ztsStore')
      IMPORT :: C_INT, C_LONG_LONG, C_PTR
      INTEGER(C_LONG_LONG), INTENT(INOUT) :: ifltab(*)
      TYPE(C_PTR), VALUE :: tss
      INTEGER(C_INT), VALUE :: storageFlag
    END FUNCTION hec_ztsStore

    !--------------------------------------------------------------
    ! Time Series Struct Creation Functions
    !--------------------------------------------------------------

    ! zStructTimeSeries* zstructTsNew(const char* pathname)
    TYPE(C_PTR) FUNCTION hec_zstructTsNew(pathname) &
                BIND(C, NAME='zstructTsNew')
      IMPORT :: C_PTR, C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pathname(*)
    END FUNCTION hec_zstructTsNew

    ! zStructTimeSeries* zstructTsNewTimes(const char* pathname,
    !                     const char* startDate, const char* startTime,
    !                     const char* endDate, const char* endTime)
    TYPE(C_PTR) FUNCTION hec_zstructTsNewTimes(pathname, startDate, startTime, &
                                                endDate, endTime) &
                BIND(C, NAME='zstructTsNewTimes')
      IMPORT :: C_PTR, C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pathname(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: startDate(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: startTime(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: endDate(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: endTime(*)
    END FUNCTION hec_zstructTsNewTimes

    ! zStructTimeSeries* zstructTsNewRegFloats(const char* pathname,
    !                     float *floatValues, int numberValues,
    !                     const char* startDate, const char* startTime,
    !                     const char* units, const char* type)
    TYPE(C_PTR) FUNCTION hec_zstructTsNewRegFloats(pathname, floatValues, &
                          numberValues, startDate, startTime, units, dataType) &
                BIND(C, NAME='zstructTsNewRegFloats')
      IMPORT :: C_PTR, C_CHAR, C_FLOAT, C_INT
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pathname(*)
      REAL(C_FLOAT), INTENT(IN) :: floatValues(*)
      INTEGER(C_INT), VALUE :: numberValues
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: startDate(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: startTime(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: units(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: dataType(*)
    END FUNCTION hec_zstructTsNewRegFloats

    ! zStructTimeSeries* zstructTsNewRegDoubles(const char* pathname,
    !                     double *doubleValues, int numberValues,
    !                     const char* startDate, const char* startTime,
    !                     const char* units, const char* type)
    TYPE(C_PTR) FUNCTION hec_zstructTsNewRegDoubles(pathname, doubleValues, &
                          numberValues, startDate, startTime, units, dataType) &
                BIND(C, NAME='zstructTsNewRegDoubles')
      IMPORT :: C_PTR, C_CHAR, C_DOUBLE, C_INT
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pathname(*)
      REAL(C_DOUBLE), INTENT(IN) :: doubleValues(*)
      INTEGER(C_INT), VALUE :: numberValues
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: startDate(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: startTime(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: units(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: dataType(*)
    END FUNCTION hec_zstructTsNewRegDoubles

    ! void zstructFree(void *zstruct)
    SUBROUTINE hec_zstructFree(zstruct) BIND(C, NAME='zstructFree')
      IMPORT :: C_PTR
      TYPE(C_PTR), VALUE :: zstruct
    END SUBROUTINE hec_zstructFree

    !--------------------------------------------------------------
    ! Record Deletion
    !--------------------------------------------------------------

    ! int zdelete(long long *ifltab, const char *pathname)
    INTEGER(C_INT) FUNCTION hec_zdelete(ifltab, pathname) &
                   BIND(C, NAME='zdelete')
      IMPORT :: C_INT, C_LONG_LONG, C_CHAR
      INTEGER(C_LONG_LONG), INTENT(INOUT) :: ifltab(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pathname(*)
    END FUNCTION hec_zdelete

    !--------------------------------------------------------------
    ! Configuration Functions
    !--------------------------------------------------------------

    ! int zset(const char *parameter, const char *charVal, int intVal)
    INTEGER(C_INT) FUNCTION hec_zset(parameter, charVal, intVal) &
                   BIND(C, NAME='zset')
      IMPORT :: C_INT, C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: parameter(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: charVal(*)
      INTEGER(C_INT), VALUE :: intVal
    END FUNCTION hec_zset

    ! int zquery(const char *parameter, char *charVal, int sizeCharVal, int *intVal)
    INTEGER(C_INT) FUNCTION hec_zquery(parameter, charVal, sizeCharVal, intVal) &
                   BIND(C, NAME='zquery')
      IMPORT :: C_INT, C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: parameter(*)
      CHARACTER(KIND=C_CHAR), INTENT(OUT) :: charVal(*)
      INTEGER(C_INT), VALUE :: sizeCharVal
      INTEGER(C_INT), INTENT(OUT) :: intVal
    END FUNCTION hec_zquery

    !--------------------------------------------------------------
    ! Pathname Functions
    !--------------------------------------------------------------

    ! int zpathnameGetPart(const char *pathname, int partPosition, char *part, size_t sizeofPart)
    ! partPosition: 1=A, 2=B, 3=C, 4=D, 5=E, 6=F
    INTEGER(C_INT) FUNCTION hec_zpathnameGetPart(pathname, partPosition, part, sizeofPart) &
                   BIND(C, NAME='zpathnameGetPart')
      IMPORT :: C_INT, C_CHAR, C_SIZE_T
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: pathname(*)
      INTEGER(C_INT), VALUE :: partPosition
      CHARACTER(KIND=C_CHAR), INTENT(OUT) :: part(*)
      INTEGER(C_SIZE_T), VALUE :: sizeofPart
    END FUNCTION hec_zpathnameGetPart

    ! int zpathnameForm(const char *aPart, const char *bPart, const char *cPart,
    !                   const char *dPart, const char *ePart, const char *fPart,
    !                   char *pathname, size_t sizeofPathname)
    INTEGER(C_INT) FUNCTION hec_zpathnameForm(apart, bpart, cpart, dpart, epart, fpart, &
                            pathname, sizeofPathname) &
                   BIND(C, NAME='zpathnameForm')
      IMPORT :: C_INT, C_CHAR, C_SIZE_T
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: apart(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: bpart(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: cpart(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: dpart(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: epart(*)
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: fpart(*)
      CHARACTER(KIND=C_CHAR), INTENT(OUT) :: pathname(*)
      INTEGER(C_SIZE_T), VALUE :: sizeofPathname
    END FUNCTION hec_zpathnameForm

    !--------------------------------------------------------------
    ! Date Conversion Functions
    !--------------------------------------------------------------

    ! int yearMonthDayToJulian(int year, int month, int day)
    INTEGER(C_INT) FUNCTION hec_yearMonthDayToJulian(year, month, day) &
                   BIND(C, NAME='yearMonthDayToJulian')
      IMPORT :: C_INT
      INTEGER(C_INT), VALUE :: year
      INTEGER(C_INT), VALUE :: month
      INTEGER(C_INT), VALUE :: day
    END FUNCTION hec_yearMonthDayToJulian

    ! void julianToYearMonthDay(int julian, int *year, int *month, int *day)
    SUBROUTINE hec_julianToYearMonthDay(julian, year, month, day) &
               BIND(C, NAME='julianToYearMonthDay')
      IMPORT :: C_INT
      INTEGER(C_INT), VALUE :: julian
      INTEGER(C_INT), INTENT(OUT) :: year
      INTEGER(C_INT), INTENT(OUT) :: month
      INTEGER(C_INT), INTENT(OUT) :: day
    END SUBROUTINE hec_julianToYearMonthDay

    !--------------------------------------------------------------
    ! C Standard Library Functions
    !--------------------------------------------------------------

    ! size_t strlen(const char *str) - accepts character array
    INTEGER(C_SIZE_T) FUNCTION c_strlen_char(str) BIND(C, NAME='strlen')
      IMPORT :: C_SIZE_T, C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: str(*)
    END FUNCTION c_strlen_char

    ! size_t strlen(const char *str) - accepts C_PTR
    INTEGER(C_SIZE_T) FUNCTION c_strlen(str) BIND(C, NAME='strlen')
      IMPORT :: C_SIZE_T, C_PTR
      TYPE(C_PTR), VALUE :: str
    END FUNCTION c_strlen

  END INTERFACE

  ! -------------------------------------------------------------
  ! --- zStructTimeSeries type for accessing retrieved data
  ! --- This matches the C struct layout for x64 platforms
  ! -------------------------------------------------------------
  TYPE, BIND(C) :: zStructTimeSeries
    INTEGER(C_INT) :: structType
    TYPE(C_PTR) :: pathname
    ! Time information
    INTEGER(C_INT) :: julianBaseDate
    INTEGER(C_INT) :: startJulianDate
    INTEGER(C_INT) :: startTimeSeconds
    INTEGER(C_INT) :: endJulianDate
    INTEGER(C_INT) :: endTimeSeconds
    INTEGER(C_INT) :: timeGranularitySeconds
    INTEGER(C_INT) :: timeIntervalSeconds
    INTEGER(C_INT) :: timeOffsetSeconds
    TYPE(C_PTR) :: times
    INTEGER(C_INT) :: boolRetrieveAllTimes
    ! Data
    INTEGER(C_INT) :: numberValues
    INTEGER(C_INT) :: sizeEachValueRead
    INTEGER(C_INT) :: precision
    TYPE(C_PTR) :: floatValues
    TYPE(C_PTR) :: doubleValues
    TYPE(C_PTR) :: units
    TYPE(C_PTR) :: dataType
    ! ... remaining fields omitted as we only need above for basic reads
  END TYPE zStructTimeSeries

  PUBLIC :: zStructTimeSeries
  PUBLIC :: get_ts_float_values, get_ts_number_values, get_ts_units, get_ts_type

CONTAINS

  ! -------------------------------------------------------------
  ! --- HELPER FUNCTIONS
  ! -------------------------------------------------------------

  !--------------------------------------------------------------
  ! Get number of values from time series struct
  !--------------------------------------------------------------
  FUNCTION get_ts_number_values(tss_ptr) RESULT(nvals)
    TYPE(C_PTR), VALUE :: tss_ptr
    INTEGER :: nvals
    TYPE(zStructTimeSeries), POINTER :: tss

    nvals = 0
    IF (.NOT. C_ASSOCIATED(tss_ptr)) RETURN
    CALL C_F_POINTER(tss_ptr, tss)
    nvals = tss%numberValues
  END FUNCTION get_ts_number_values

  !--------------------------------------------------------------
  ! Get float values from time series struct
  !--------------------------------------------------------------
  SUBROUTINE get_ts_float_values(tss_ptr, values, nvals)
    TYPE(C_PTR), VALUE :: tss_ptr
    INTEGER, INTENT(IN) :: nvals
    REAL, INTENT(OUT) :: values(nvals)
    TYPE(zStructTimeSeries), POINTER :: tss
    REAL(C_FLOAT), POINTER :: fvals(:)
    INTEGER :: i

    values = 0.0
    IF (.NOT. C_ASSOCIATED(tss_ptr)) RETURN
    CALL C_F_POINTER(tss_ptr, tss)

    IF (tss%numberValues > 0 .AND. C_ASSOCIATED(tss%floatValues)) THEN
      CALL C_F_POINTER(tss%floatValues, fvals, [MIN(nvals, tss%numberValues)])
      DO i = 1, MIN(nvals, tss%numberValues)
        values(i) = REAL(fvals(i))
      END DO
    END IF
  END SUBROUTINE get_ts_float_values

  !--------------------------------------------------------------
  ! Get units string from time series struct
  !--------------------------------------------------------------
  FUNCTION get_ts_units(tss_ptr) RESULT(units_str)
    TYPE(C_PTR), VALUE :: tss_ptr
    CHARACTER(LEN=32) :: units_str
    TYPE(zStructTimeSeries), POINTER :: tss

    units_str = ''
    IF (.NOT. C_ASSOCIATED(tss_ptr)) RETURN
    CALL C_F_POINTER(tss_ptr, tss)

    IF (C_ASSOCIATED(tss%units)) THEN
      units_str = c_to_f_string(tss%units)
    END IF
  END FUNCTION get_ts_units

  !--------------------------------------------------------------
  ! Get data type string from time series struct
  !--------------------------------------------------------------
  FUNCTION get_ts_type(tss_ptr) RESULT(type_str)
    TYPE(C_PTR), VALUE :: tss_ptr
    CHARACTER(LEN=32) :: type_str
    TYPE(zStructTimeSeries), POINTER :: tss

    type_str = ''
    IF (.NOT. C_ASSOCIATED(tss_ptr)) RETURN
    CALL C_F_POINTER(tss_ptr, tss)

    IF (C_ASSOCIATED(tss%dataType)) THEN
      type_str = c_to_f_string(tss%dataType)
    END IF
  END FUNCTION get_ts_type

  !--------------------------------------------------------------
  ! Convert Fortran string to C string (null-terminated)
  !--------------------------------------------------------------
  FUNCTION f_to_c_string(f_string) RESULT(c_string)
    CHARACTER(LEN=*), INTENT(IN) :: f_string
    CHARACTER(LEN=LEN_TRIM(f_string)+1, KIND=C_CHAR) :: c_string

    INTEGER :: i, n

    n = LEN_TRIM(f_string)
    DO i = 1, n
      c_string(i:i) = f_string(i:i)
    END DO
    c_string(n+1:n+1) = C_NULL_CHAR

  END FUNCTION f_to_c_string

  !--------------------------------------------------------------
  ! Convert C string pointer to Fortran string
  ! Note: This is a simplified version that uses C strlen
  !--------------------------------------------------------------
  FUNCTION c_to_f_string(c_ptr_in) RESULT(f_string)
    TYPE(C_PTR), VALUE :: c_ptr_in
    CHARACTER(LEN=:), ALLOCATABLE :: f_string

    CHARACTER(KIND=C_CHAR), POINTER :: c_array(:)
    INTEGER(C_SIZE_T) :: length
    INTEGER :: i

    IF (.NOT. C_ASSOCIATED(c_ptr_in)) THEN
      f_string = ''
      RETURN
    END IF

    ! Get length using C strlen function
    length = c_strlen(c_ptr_in)

    IF (length <= 0) THEN
      f_string = ''
      RETURN
    END IF

    ! Allocate and copy
    ALLOCATE(CHARACTER(LEN=length) :: f_string)
    CALL C_F_POINTER(c_ptr_in, c_array, [length])

    DO i = 1, INT(length)
      f_string(i:i) = c_array(i)
    END DO

  END FUNCTION c_to_f_string

END MODULE heclib_c_binding
