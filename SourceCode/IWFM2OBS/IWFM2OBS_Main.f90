!***********************************************************************
!  IWFM2OBS - Main Program
!
!  Thin driver following the IWFM executable pattern (Iwfm_f2.f90).
!  Reads input file, runs interpolation workflow, reports results.
!
!  Usage:
!    IWFM2OBS <input_file>
!    IWFM2OBS                  (prompts for input file)
!***********************************************************************
PROGRAM IWFM2OBS_Main

  USE ProgramTimer  , ONLY: DefaultTimer  , &
                             ProgramTimerType
  USE MessageLogger , ONLY: SetLogFileName, &
                             KillLogFile   , &
                             LogLastMessage, &
                             DefaultLogger , &
                             f_iInfo
  USE Class_IWFM2OBS, ONLY: IWFM2OBSType

  IMPLICIT NONE

  TYPE(IWFM2OBSType) :: App
  CHARACTER(LEN=500) :: cInputFile
  INTEGER            :: iStat, iNArgs

  ! Start timer
  CALL DefaultTimer%Start()

  DO  ! Single-pass block for structured error exit

    ! Open log file
    CALL SetLogFileName('IWFM2OBS_Messages.out', iStat)
    IF (iStat == -1) THEN
      CALL LogLastMessage()
      EXIT
    END IF

    ! Banner
    CALL DefaultLogger%LogMessage(' ', f_iInfo, 'IWFM2OBS')
    CALL DefaultLogger%LogMessage('Program IWFM2OBS - Hydrograph to PEST SMP converter', &
                    f_iInfo, 'IWFM2OBS')
    CALL DefaultLogger%LogMessage('with multi-layer target support', f_iInfo, 'IWFM2OBS')
    CALL DefaultLogger%LogMessage(' ', f_iInfo, 'IWFM2OBS')

    ! Get input file from command line or prompt
    iNArgs = COMMAND_ARGUMENT_COUNT()
    IF (iNArgs >= 1) THEN
      CALL GET_COMMAND_ARGUMENT(1, cInputFile)
    ELSE
      WRITE(*, '(A)', ADVANCE='NO') ' Enter name of input file: '
      READ(*, '(A)') cInputFile
    END IF
    cInputFile = ADJUSTL(TRIM(cInputFile))

    IF (LEN_TRIM(cInputFile) == 0) THEN
      WRITE(*, '(A)') ' ERROR: No input file specified.'
      EXIT
    END IF

    ! Initialize
    CALL App%New(cInputFile, iStat)
    IF (iStat == -1) THEN
      CALL LogLastMessage()
      EXIT
    END IF

    ! Run interpolation workflow
    CALL App%Run(iStat)
    IF (iStat == -1) THEN
      CALL LogLastMessage()
    END IF

    ! Clean up
    CALL App%Kill()

    CALL DefaultLogger%LogMessage(' ', f_iInfo, 'IWFM2OBS')
    CALL DefaultLogger%LogMessage('NORMAL TERMINATION - IWFM2OBS', f_iInfo, 'IWFM2OBS')

    EXIT  ! Normal exit from single-pass block
  END DO

  CALL DefaultTimer%Stop()
  CALL DefaultLogger%PrintRunTime()
  CALL KillLogFile()

END PROGRAM IWFM2OBS_Main
