!***********************************************************************
!  GWHydExtract - Main Program
!
!  Post-processing tool that reads the simulation's "all heads" output
!  file and generates GW hydrograph output at user-specified locations
!  using FE interpolation, without re-running the model.
!
!  Usage:
!    GWHydExtract <input_file>
!    GWHydExtract                  (prompts for input file)
!***********************************************************************
PROGRAM GWHydExtract_Main

  USE ProgramTimer  , ONLY: StartTimer  , &
                             StopTimer
  USE MessageLogger , ONLY: PrintRunTime , &
                             SetLogFileName, &
                             KillLogFile   , &
                             LogLastMessage, &
                             LogMessage    , &
                             f_iInfo
  USE Class_GWHydExtract, ONLY: GWHydExtractType

  IMPLICIT NONE

  TYPE(GWHydExtractType) :: App
  CHARACTER(LEN=500) :: cInputFile
  INTEGER            :: iStat, iNArgs

  ! Start timer
  CALL StartTimer()

  ! Open log file
  CALL SetLogFileName('GWHydExtract_Messages.out', iStat)
  IF (iStat == -1) THEN
    CALL LogLastMessage()
    GO TO 999
  END IF

  ! Banner
  CALL LogMessage(' ', f_iInfo, 'GWHydExtract')
  CALL LogMessage('Program GWHydExtract - Groundwater Hydrograph Extractor', &
                  f_iInfo, 'GWHydExtract')
  CALL LogMessage('Extracts GW hydrographs from all-heads output file', &
                  f_iInfo, 'GWHydExtract')
  CALL LogMessage(' ', f_iInfo, 'GWHydExtract')

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
    GO TO 999
  END IF

  ! Initialize
  CALL App%New(cInputFile, iStat)
  IF (iStat == -1) THEN
    CALL LogLastMessage()
    GO TO 999
  END IF

  ! Run extraction
  CALL App%Run(iStat)
  IF (iStat == -1) THEN
    CALL LogLastMessage()
  END IF

  ! Clean up
  CALL App%Kill()

  CALL LogMessage(' ', f_iInfo, 'GWHydExtract')
  CALL LogMessage('NORMAL TERMINATION - GWHydExtract', f_iInfo, 'GWHydExtract')

999 CONTINUE
  CALL StopTimer()
  CALL PrintRunTime()
  CALL KillLogFile()

END PROGRAM GWHydExtract_Main
