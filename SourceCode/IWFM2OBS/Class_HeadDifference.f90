!***********************************************************************
!  IWFM2OBS - Class_HeadDifference
!  Head difference computation for pairs of GW observation wells
!***********************************************************************
MODULE Class_HeadDifference

  USE MessageLogger    , ONLY: SetLastMessage , &
                               LogMessage     , &
                               f_iFatal       , &
                               f_iWarn        , &
                               f_iInfo
  USE GeneralUtilities , ONLY: UpperCase      , &
                               IntToText

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: HeadDifferenceType, HeadDiffPairType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_HeadDifference'

  ! =====================================================================
  ! HeadDiffPairType - One pair of well IDs for head differencing
  ! =====================================================================
  TYPE HeadDiffPairType
    CHARACTER(LEN=25) :: cID1 = ' '   ! First well ID
    CHARACTER(LEN=25) :: cID2 = ' '   ! Second well ID
  END TYPE HeadDiffPairType

  ! =====================================================================
  ! Stored interpolation result for head difference matching
  ! =====================================================================
  TYPE :: HDResultType
    INTEGER :: iDay  = 0
    INTEGER :: iMon  = 0
    INTEGER :: iYear = 0
    INTEGER :: iHH   = 0
    INTEGER :: iMM   = 0
    INTEGER :: iSS   = 0
    REAL(8) :: rValue = 0.0D0
  END TYPE HDResultType

  ! =====================================================================
  ! HeadDifferenceType - Manager for head difference pairs
  ! =====================================================================
  TYPE HeadDifferenceType
    INTEGER                                :: iNPairs   = 0
    TYPE(HeadDiffPairType), ALLOCATABLE    :: Pairs(:)
    LOGICAL                                :: lActive   = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Kill
    PROCEDURE, PASS :: GetNPairs
    PROCEDURE, PASS :: ValidateIDs
  END TYPE HeadDifferenceType

CONTAINS

  ! =====================================================================
  ! New - Read head difference pairs file
  ! =====================================================================
  SUBROUTINE New(This, cHDFile, iStat)
    CLASS(HeadDifferenceType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),         INTENT(IN)     :: cHDFile
    INTEGER,                  INTENT(OUT)    :: iStat

    INTEGER, PARAMETER :: iUnit = 189
    INTEGER            :: iErr, iLine, n
    CHARACTER(LEN=25)  :: cID1, cID2

    iStat = 0

    ! Count lines
    OPEN(UNIT=iUnit, FILE=cHDFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open head difference file: '//TRIM(cHDFile), f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF

    iLine = 0
    DO
      READ(iUnit, *, IOSTAT=iErr) cID1
      IF (iErr /= 0) EXIT
      iLine = iLine + 1
    END DO

    IF (iLine == 0) THEN
      CALL SetLastMessage('No pairs in head difference file: '//TRIM(cHDFile), f_iFatal, cModName)
      CLOSE(iUnit)
      iStat = -1
      RETURN
    END IF

    This%iNPairs = iLine
    ALLOCATE(This%Pairs(iLine), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate head difference pairs', f_iFatal, cModName)
      CLOSE(iUnit)
      iStat = -1
      RETURN
    END IF

    ! Read pairs
    REWIND(iUnit)
    DO n = 1, This%iNPairs
      READ(iUnit, *, IOSTAT=iErr) cID1, cID2
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading line '//TRIM(IntToText(n))//' of '//TRIM(cHDFile), &
             f_iFatal, cModName)
        CLOSE(iUnit)
        iStat = -1
        RETURN
      END IF
      This%Pairs(n)%cID1 = UpperCase(ADJUSTL(cID1))
      This%Pairs(n)%cID2 = UpperCase(ADJUSTL(cID2))

      ! Validate: IDs must differ
      IF (This%Pairs(n)%cID1 == This%Pairs(n)%cID2) THEN
        CALL SetLastMessage('Identical IDs in pair at line '//TRIM(IntToText(n))// &
             ' of '//TRIM(cHDFile), f_iFatal, cModName)
        CLOSE(iUnit)
        iStat = -1
        RETURN
      END IF
    END DO
    CLOSE(iUnit)

    This%lActive = .TRUE.

  END SUBROUTINE New

  ! =====================================================================
  ! Kill - Deallocate
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(HeadDifferenceType), INTENT(INOUT) :: This

    IF (ALLOCATED(This%Pairs)) DEALLOCATE(This%Pairs)
    This%iNPairs = 0
    This%lActive = .FALSE.

  END SUBROUTINE Kill

  ! =====================================================================
  ! GetNPairs - Return number of pairs
  ! =====================================================================
  FUNCTION GetNPairs(This) RESULT(iN)
    CLASS(HeadDifferenceType), INTENT(IN) :: This
    INTEGER :: iN
    iN = This%iNPairs
  END FUNCTION GetNPairs

  ! =====================================================================
  ! ValidateIDs - Check all pair IDs exist in observation ID list
  ! =====================================================================
  SUBROUTINE ValidateIDs(This, cObsIDs, iNObs, iStat)
    CLASS(HeadDifferenceType), INTENT(IN)  :: This
    CHARACTER(LEN=25),         INTENT(IN)  :: cObsIDs(:)
    INTEGER,                   INTENT(IN)  :: iNObs
    INTEGER,                   INTENT(OUT) :: iStat

    INTEGER :: n, i
    LOGICAL :: lFound

    iStat = 0

    DO n = 1, This%iNPairs
      ! Check ID1
      lFound = .FALSE.
      DO i = 1, iNObs
        IF (cObsIDs(i) == This%Pairs(n)%cID1) THEN
          lFound = .TRUE.
          EXIT
        END IF
      END DO
      IF (.NOT. lFound) THEN
        CALL SetLastMessage('Head difference ID '//TRIM(This%Pairs(n)%cID1)// &
             ' not found in observation file', f_iFatal, cModName)
        iStat = -1
        RETURN
      END IF

      ! Check ID2
      lFound = .FALSE.
      DO i = 1, iNObs
        IF (cObsIDs(i) == This%Pairs(n)%cID2) THEN
          lFound = .TRUE.
          EXIT
        END IF
      END DO
      IF (.NOT. lFound) THEN
        CALL SetLastMessage('Head difference ID '//TRIM(This%Pairs(n)%cID2)// &
             ' not found in observation file', f_iFatal, cModName)
        iStat = -1
        RETURN
      END IF
    END DO

  END SUBROUTINE ValidateIDs

END MODULE Class_HeadDifference
