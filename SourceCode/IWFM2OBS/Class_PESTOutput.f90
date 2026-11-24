!***********************************************************************
!  IWFM2OBS - Class_PESTOutput
!  PEST instruction file (.ins) and PCF file generation
!***********************************************************************
MODULE Class_PESTOutput

  USE MessageLogger    , ONLY: MessageLoggerType , &
                               f_iFatal
  USE GeneralUtilities , ONLY: IntToText      , &
                               NativePathName

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: PESTOutputType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_PESTOutput'

  ! =====================================================================
  ! PESTOutputType - PEST instruction and PCF file writer
  !   Instruction file: one line per model-output SMP line, reading the
  !   value from its exact columns:  l1  [NAME]c1:c2
  !   PCF file (optional): one line per observation:  NAME  value
  ! =====================================================================
  TYPE PESTOutputType
    TYPE(MessageLoggerType),POINTER :: Logger => NULL()
    INTEGER            :: iInsUnit = 0
    INTEGER            :: iPCFUnit = 0
    CHARACTER(LEN=500) :: cInsFile = ' '
    CHARACTER(LEN=500) :: cPCFFile = ' '
    LOGICAL            :: lActive  = .FALSE.
    LOGICAL            :: lPCF     = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Kill
    PROCEDURE, PASS :: WriteInsLine
    PROCEDURE, PASS :: WritePCFLine
  END TYPE PESTOutputType

CONTAINS


  ! =====================================================================
  ! New - Open instruction file (and PCF file if a name is given)
  ! =====================================================================
  SUBROUTINE New(This, Logger, cInsFile, cPCFFile, iStat)
    CLASS(PESTOutputType), INTENT(INOUT) :: This
    TYPE(MessageLoggerType),TARGET,INTENT(INOUT) :: Logger
    CHARACTER(LEN=*),      INTENT(IN)    :: cInsFile, cPCFFile
    INTEGER,               INTENT(OUT)   :: iStat

    INTEGER :: iErr

    iStat = 0
    This%Logger => Logger
    This%cInsFile = cInsFile
    This%cPCFFile = cPCFFile

    OPEN(NEWUNIT=This%iInsUnit, FILE=NativePathName(TRIM(cInsFile)), STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL This%Logger%SetLastMessage('Cannot open instruction file: '//TRIM(cInsFile), f_iFatal, cModName)
      iStat = -1
      RETURN
    END IF
    WRITE(This%iInsUnit, '(A)') 'pif #'

    IF (LEN_TRIM(cPCFFile) > 0) THEN
      OPEN(NEWUNIT=This%iPCFUnit, FILE=NativePathName(TRIM(cPCFFile)), STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CALL This%Logger%SetLastMessage('Cannot open PCF file: '//TRIM(cPCFFile), f_iFatal, cModName)
        CLOSE(This%iInsUnit)
        iStat = -1
        RETURN
      END IF
      This%lPCF = .TRUE.
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
      IF (This%lPCF) CLOSE(This%iPCFUnit, IOSTAT=iErr)
      This%lActive = .FALSE.
      This%lPCF    = .FALSE.
    END IF

  END SUBROUTINE Kill

  ! =====================================================================
  ! WriteInsLine - Write one PEST instruction line
  !   Format: l1  [NAME]c1:c2   (value occupies columns c1..c2 of the line)
  ! =====================================================================
  SUBROUTINE WriteInsLine(This, cName, iCol1, iCol2)
    CLASS(PESTOutputType), INTENT(IN) :: This
    CHARACTER(LEN=*),      INTENT(IN) :: cName
    INTEGER,               INTENT(IN) :: iCol1, iCol2

    IF (.NOT. This%lActive) RETURN
    WRITE(This%iInsUnit, '(A)') 'l1  ['//TRIM(cName)//']'//TRIM(IntToText(iCol1))//':'//TRIM(IntToText(iCol2))

  END SUBROUTINE WriteInsLine

  ! =====================================================================
  ! WritePCFLine - Write one PEST PCF line
  !   Format: NAME    value
  ! =====================================================================
  SUBROUTINE WritePCFLine(This, cName, rObsValue)
    CLASS(PESTOutputType), INTENT(IN) :: This
    CHARACTER(LEN=*),      INTENT(IN) :: cName
    REAL(8),               INTENT(IN) :: rObsValue

    IF (.NOT. This%lPCF) RETURN
    WRITE(This%iPCFUnit, 100) TRIM(cName), rObsValue
100 FORMAT(A,'    ',1PG15.8)

  END SUBROUTINE WritePCFLine

END MODULE Class_PESTOutput
