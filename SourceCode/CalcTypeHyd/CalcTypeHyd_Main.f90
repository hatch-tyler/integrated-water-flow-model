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

  USE ProgramTimer    , ONLY: StartTimer  , &
                               StopTimer
  USE MessageLogger   , ONLY: PrintRunTime , &
                               SetLogFileName, &
                               KillLogFile   , &
                               LogLastMessage, &
                               DefaultLogger , &
                               f_iInfo
  USE Class_CalcTypeHyd, ONLY: CalcTypeHydType

  IMPLICIT NONE

  TYPE(CalcTypeHydType) :: App
  CHARACTER(LEN=500) :: cInputFile
  INTEGER            :: iStat, iNArgs

  ! Start timer
  CALL StartTimer()

  DO  ! Single-pass block for structured error exit

    ! Open log file
    CALL SetLogFileName('CalcTypeHyd_Messages.out', iStat)
    IF (iStat == -1) THEN
      CALL LogLastMessage()
      EXIT
    END IF

    ! Banner
    CALL DefaultLogger%LogMessage(' ', f_iInfo, 'CalcTypeHyd')
    CALL DefaultLogger%LogMessage('Program CalcTypeHyd - Cluster type hydrograph computation', &
                    f_iInfo, 'CalcTypeHyd')
    CALL DefaultLogger%LogMessage(' ', f_iInfo, 'CalcTypeHyd')

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
      CALL LogLastMessage()
      EXIT
    END IF

    ! Run type hydrograph generation
    CALL App%Run(iStat)
    IF (iStat == -1) THEN
      CALL LogLastMessage()
    END IF

    ! Clean up
    CALL App%Kill()

    CALL DefaultLogger%LogMessage(' ', f_iInfo, 'CalcTypeHyd')
    CALL DefaultLogger%LogMessage('NORMAL TERMINATION - CalcTypeHyd', f_iInfo, 'CalcTypeHyd')

    EXIT  ! Normal exit from single-pass block
  END DO

  CALL StopTimer()
  CALL PrintRunTime()
  CALL KillLogFile()

END PROGRAM CalcTypeHyd_Main
