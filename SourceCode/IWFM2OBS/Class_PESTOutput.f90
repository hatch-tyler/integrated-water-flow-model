!***********************************************************************
!  IWFM2OBS - Class_PESTOutput
!  PEST instruction file (.ins) and PCF file generation
!***********************************************************************
MODULE Class_PESTOutput

  USE MessageLogger    , ONLY: SetLastMessage , &
                               f_iFatal
  USE GeneralUtilities , ONLY: IntToText

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: PESTOutputType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_PESTOutput'

  ! =====================================================================
  ! PESTOutputType - PEST instruction and PCF file writer
  ! =====================================================================
  TYPE PESTOutputType
    INTEGER            :: iInsUnit = 0
    INTEGER            :: iPCFUnit = 0
    CHARACTER(LEN=500) :: cInsFile = ' '
    CHARACTER(LEN=500) :: cPCFFile = ' '
    LOGICAL            :: lActive  = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Kill
    PROCEDURE, PASS :: WriteInsLine
    PROCEDURE, PASS :: WritePCFLine
  END TYPE PESTOutputType

CONTAINS

  ! =====================================================================
  ! New - Open instruction and PCF files, write header
  ! =====================================================================
  SUBROUTINE New(This, cInsFile, iInsUnit, cPCFFile, iPCFUnit, iStat)
    CLASS(PESTOutputType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),      INTENT(IN)    :: cInsFile, cPCFFile
    INTEGER,               INTENT(IN)    :: iInsUnit, iPCFUnit
    INTEGER,               INTENT(OUT)   :: iStat

    INTEGER :: iErr

    iStat = 0
    This%cInsFile = cInsFile
    This%cPCFFile = cPCFFile
    This%iInsUnit = iInsUnit
    This%iPCFUnit = iPCFUnit

    OPEN(UNIT=iInsUnit, FILE=cInsFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open instruction file: '//TRIM(cInsFile), f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF
    WRITE(iInsUnit, '(A)') 'pif #'

    OPEN(UNIT=iPCFUnit, FILE=cPCFFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open PCF file: '//TRIM(cPCFFile), f_iFatal, cModName)
      CLOSE(iInsUnit)
      iStat = -1
      RETURN
    END IF

    This%lActive = .TRUE.

  END SUBROUTINE New

  ! =====================================================================
  ! Kill - Close instruction and PCF files
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(PESTOutputType), INTENT(INOUT) :: This

    INTEGER :: iErr

    IF (This%lActive) THEN
      CLOSE(This%iInsUnit, IOSTAT=iErr)
      CLOSE(This%iPCFUnit, IOSTAT=iErr)
      This%lActive = .FALSE.
    END IF

  END SUBROUTINE Kill

  ! =====================================================================
  ! WriteInsLine - Write one PEST instruction line
  !   Format: l1  [ID_NNNN]37:56
  ! =====================================================================
  SUBROUTINE WriteInsLine(This, cID, iSeqNum)
    CLASS(PESTOutputType), INTENT(IN) :: This
    CHARACTER(LEN=*),      INTENT(IN) :: cID
    INTEGER,               INTENT(IN) :: iSeqNum

    IF (.NOT. This%lActive) RETURN
    WRITE(This%iInsUnit, 100) TRIM(cID), iSeqNum
100 FORMAT('l1  [',A,'_',I4.4,']37:56')

  END SUBROUTINE WriteInsLine

  ! =====================================================================
  ! WritePCFLine - Write one PEST PCF line
  !   Format: ID_NNNN    value
  ! =====================================================================
  SUBROUTINE WritePCFLine(This, cID, iSeqNum, rObsValue)
    CLASS(PESTOutputType), INTENT(IN) :: This
    CHARACTER(LEN=*),      INTENT(IN) :: cID
    INTEGER,               INTENT(IN) :: iSeqNum
    REAL(8),               INTENT(IN) :: rObsValue

    IF (.NOT. This%lActive) RETURN
    WRITE(This%iPCFUnit, 100) TRIM(cID), iSeqNum, rObsValue
100 FORMAT(A,'_',I4.4,'    ',1PG15.8)

  END SUBROUTINE WritePCFLine

END MODULE Class_PESTOutput
