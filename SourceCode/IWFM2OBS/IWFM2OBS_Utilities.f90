!***********************************************************************
!  IWFM2OBS - Shared Utilities
!
!  Common helper functions used across IWFM2OBS modules.
!  Eliminates duplication of StripAndClean, sort/search, date parsing,
!  and date computation across Class_HydrographReader, Class_IWFM2OBS,
!  Class_MultiLayerTarget, and Class_SMP2SMP.
!***********************************************************************
MODULE IWFM2OBS_Utilities

  USE GeneralUtilities     , ONLY: StripTextUntilCharacter  , &
                                    CleanSpecialCharacters   , &
                                    f_cInlineCommentChar
  USE TimeSeriesUtilities  , ONLY: DayMonthYearToJulianDate , &
                                    JulianDateToDayMonthYear

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: StripAndClean        , &
            SortStringsIndex     , &
            BinarySearchStr      , &
            ParseDateFromString  , &
            ComputeDateJulian

CONTAINS


  ! =====================================================================
  ! StripAndClean - Strip inline comment and clean special characters
  !   Uses kernel's StripTextUntilCharacter + CleanSpecialCharacters
  ! =====================================================================
  SUBROUTINE StripAndClean(cLine, cResult)
    CHARACTER(LEN=*), INTENT(IN)  :: cLine
    CHARACTER(LEN=*), INTENT(OUT) :: cResult

    cResult = StripTextUntilCharacter(cLine, f_cInlineCommentChar)
    CALL CleanSpecialCharacters(cResult)
    cResult = ADJUSTL(TRIM(cResult))
  END SUBROUTINE StripAndClean


  ! =====================================================================
  ! BinarySearchStr - Binary search a sorted CHARACTER array
  !   Returns position if found, 0 if not found.
  !   Array must be sorted in ascending order.
  ! =====================================================================
  FUNCTION BinarySearchStr(cArr, iN, cTarget) RESULT(iPos)
    CHARACTER(LEN=*), INTENT(IN) :: cArr(:)
    INTEGER,          INTENT(IN) :: iN
    CHARACTER(LEN=*), INTENT(IN) :: cTarget
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
  END FUNCTION BinarySearchStr


  ! =====================================================================
  ! SortStringsIndex - Quicksort CHARACTER array with parallel index array
  !   Sorts cArr(iLo:iHi) in ascending order, reordering iIdx in parallel
  ! =====================================================================
  RECURSIVE SUBROUTINE SortStringsIndex(cArr, iIdx, iLo, iHi)
    CHARACTER(LEN=*), INTENT(INOUT) :: cArr(:)
    INTEGER,          INTENT(INOUT) :: iIdx(:)
    INTEGER,          INTENT(IN)    :: iLo, iHi

    CHARACTER(LEN=LEN(cArr)) :: cPivot, cTemp
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

    IF (iLo < j) CALL SortStringsIndex(cArr, iIdx, iLo, j)
    IF (i < iHi) CALL SortStringsIndex(cArr, iIdx, i, iHi)
  END SUBROUTINE SortStringsIndex


  ! =====================================================================
  ! ParseDateFromString - Parse date from MM/DD/YYYY or DD/MM/YYYY string
  !   iDateSpec: 1 = DD/MM/YYYY, 2 = MM/DD/YYYY
  ! =====================================================================
  SUBROUTINE ParseDateFromString(cDateStr, iDateSpec, iDay, iMon, iYr, iStat)
    CHARACTER(LEN=*), INTENT(IN)  :: cDateStr
    INTEGER,          INTENT(IN)  :: iDateSpec
    INTEGER,          INTENT(OUT) :: iDay, iMon, iYr, iStat

    INTEGER :: iP1, iP2, iV1, iV2, iV3, iErr

    iStat = 0
    iDay = 0; iMon = 0; iYr = 0

    ! Find the two '/' separators
    iP1 = INDEX(cDateStr, '/')
    IF (iP1 == 0) THEN
      iStat = -1; RETURN
    END IF
    iP2 = INDEX(cDateStr(iP1+1:), '/')
    IF (iP2 == 0) THEN
      iStat = -1; RETURN
    END IF
    iP2 = iP1 + iP2

    READ(cDateStr(1:iP1-1), *, IOSTAT=iErr) iV1
    IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF
    READ(cDateStr(iP1+1:iP2-1), *, IOSTAT=iErr) iV2
    IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF
    READ(cDateStr(iP2+1:), *, IOSTAT=iErr) iV3
    IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF

    SELECT CASE (iDateSpec)
      CASE (1)  ! DD/MM/YYYY
        iDay = iV1; iMon = iV2; iYr = iV3
      CASE DEFAULT  ! MM/DD/YYYY (default, IWFM convention)
        iMon = iV1; iDay = iV2; iYr = iV3
    END SELECT
  END SUBROUTINE ParseDateFromString


  ! =====================================================================
  ! ComputeDateJulian - Compute date at given timestep offset from start
  !   Uses kernel's Julian date functions instead of manual calendar math.
  !   Supports 1DAY, 1WEEK, 1MON, 1YEAR time units.
  ! =====================================================================
  SUBROUTINE ComputeDateJulian(cTimeUnit, iTimeStep, iStartDay, iStartMon, iStartYr, &
                                iDay, iMon, iYr, iStat)
    CHARACTER(LEN=*), INTENT(IN)  :: cTimeUnit
    INTEGER,          INTENT(IN)  :: iTimeStep, iStartDay, iStartMon, iStartYr
    INTEGER,          INTENT(OUT) :: iDay, iMon, iYr, iStat

    INTEGER :: iJulStart, iJulNew, iTotalMon, iErr
    INTEGER :: iMonDays(12)
    DATA iMonDays /31,28,31,30,31,30,31,31,30,31,30,31/
    LOGICAL :: lLeap

    iStat = 0

    SELECT CASE (TRIM(cTimeUnit))

    CASE ('1DAY')
      CALL DayMonthYearToJulianDate(iStartDay, iStartMon, iStartYr, iJulStart, iErr)
      IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF
      iJulNew = iJulStart + iTimeStep
      CALL JulianDateToDayMonthYear(iJulNew, iDay, iMon, iYr, iErr)
      IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF

    CASE ('1WEEK')
      CALL DayMonthYearToJulianDate(iStartDay, iStartMon, iStartYr, iJulStart, iErr)
      IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF
      iJulNew = iJulStart + iTimeStep * 7
      CALL JulianDateToDayMonthYear(iJulNew, iDay, iMon, iYr, iErr)
      IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF

    CASE ('1MON')
      ! Month arithmetic: advance by iTimeStep months, use last day of resulting month
      iTotalMon = (iStartYr * 12 + iStartMon - 1) + iTimeStep
      iYr  = iTotalMon / 12
      iMon = MOD(iTotalMon, 12) + 1
      IF (iMon <= 0) THEN
        iMon = iMon + 12
        iYr  = iYr - 1
      END IF
      lLeap = (MOD(iYr,4)==0 .AND. MOD(iYr,100)/=0) .OR. MOD(iYr,400)==0
      IF (lLeap .AND. iMon == 2) THEN
        iDay = 29
      ELSE
        iDay = iMonDays(iMon)
      END IF

    CASE ('1YEAR')
      iYr  = iStartYr + iTimeStep
      iMon = iStartMon
      iDay = iStartDay

    CASE DEFAULT
      ! Assume daily
      CALL DayMonthYearToJulianDate(iStartDay, iStartMon, iStartYr, iJulStart, iErr)
      IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF
      iJulNew = iJulStart + iTimeStep
      CALL JulianDateToDayMonthYear(iJulNew, iDay, iMon, iYr, iErr)
      IF (iErr /= 0) THEN; iStat = -1; RETURN; END IF

    END SELECT

  END SUBROUTINE ComputeDateJulian

END MODULE IWFM2OBS_Utilities
