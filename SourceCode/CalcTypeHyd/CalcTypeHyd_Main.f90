!***********************************************************************
!  CalcTypeHyd - Main Program
!
!  Thin driver for cluster-weighted type hydrograph computation.
!  Follows the IWFM executable pattern.
!
!  Usage:
!    CalcTypeHyd <input_file>
!    CalcTypeHyd                (uses CalcTypeHyd.in)
!***********************************************************************
PROGRAM CalcTypeHyd_Main

  USE ProgramTimer    , ONLY: ProgramTimerType
  USE MessageLogger   , ONLY: MessageLoggerType, f_iInfo
  USE Class_CalcTypeHyd, ONLY: CalcTypeHydType

  IMPLICIT NONE

  TYPE(CalcTypeHydType)    :: App
  TYPE(ProgramTimerType)   :: Timer
  TYPE(MessageLoggerType)  :: Logger
  CHARACTER(LEN=500)       :: cInputFile
  INTEGER                  :: iStat, iNArgs

  ! Start timer
  CALL Timer%Start()

  DO  ! Single-pass block for structured error exit

    ! Open log file
    CALL Logger%SetLogFileName('CalcTypeHyd_Messages.out', iStat)
    IF (iStat == -1) THEN
      CALL Logger%LogLastMessage()
      EXIT
    END IF

    ! Banner
    CALL Logger%LogMessage(' ', f_iInfo, 'CalcTypeHyd')
    CALL Logger%LogMessage('Program CalcTypeHyd - Cluster type hydrograph computation', &
                    f_iInfo, 'CalcTypeHyd')
    CALL Logger%LogMessage(' ', f_iInfo, 'CalcTypeHyd')

    ! Get input file from command line or use default
    iNArgs = COMMAND_ARGUMENT_COUNT()
    IF (iNArgs >= 1) THEN
      CALL GET_COMMAND_ARGUMENT(1, cInputFile)
    ELSE
      cInputFile = 'CalcTypeHyd.in'
    END IF
    cInputFile = ADJUSTL(TRIM(cInputFile))

    ! Initialize
    CALL App%New(cInputFile, iStat)
    IF (iStat == -1) THEN
      CALL Logger%LogLastMessage()
      EXIT
    END IF

    ! Run type hydrograph generation
    CALL App%Run(iStat)
    IF (iStat == -1) THEN
      CALL Logger%LogLastMessage()
    END IF

    ! Clean up
    CALL App%Kill()

    CALL Logger%LogMessage(' ', f_iInfo, 'CalcTypeHyd')
    CALL Logger%LogMessage('NORMAL TERMINATION - CalcTypeHyd', f_iInfo, 'CalcTypeHyd')

    EXIT  ! Normal exit from single-pass block
  END DO

  CALL Timer%Stop()
  CALL Logger%PrintRunTime()
  CALL Logger%Kill()

END PROGRAM CalcTypeHyd_Main
