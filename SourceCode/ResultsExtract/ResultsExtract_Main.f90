!***********************************************************************
!  ResultsExtract - Main Program
!
!  Generalized post-processing tool that reads any all-node output
!  file (HEAD or SUBSIDENCE) and generates hydrograph output at
!  user-specified locations using FE interpolation.
!
!  Usage:
!    ResultsExtract <input_file>
!    ResultsExtract                  (prompts for input file)
!
!  Input file format:
!    SIMFILE         - Path to IWFM simulation main file
!    DATATYPE        - HEAD or SUBSIDENCE (or SUBSIDENCE_CUM)
!    OUTFILE         - Output hydrograph file path
!    NHYD            - Number of hydrograph locations
!    FACTXY          - Coordinate conversion factor
!    ID HYDTYP LAYER X/NODE [Y] NAME  - Hydrograph specifications
!***********************************************************************
PROGRAM ResultsExtract_Main

  USE ProgramTimer  , ONLY: ProgramTimerType
  USE MessageLogger , ONLY: MessageLoggerType, f_iInfo
  USE Class_ResultsExtract, ONLY: ResultsExtractType

  IMPLICIT NONE

  TYPE(ResultsExtractType) :: App
  TYPE(ProgramTimerType)   :: Timer
  TYPE(MessageLoggerType)  :: Logger
  CHARACTER(LEN=500)       :: cInputFile
  INTEGER                  :: iStat, iNArgs

  ! Start timer
  CALL Timer%Start()

  DO  ! Single-pass block for structured error exit

    ! Open log file
    CALL Logger%SetLogFileName('ResultsExtract_Messages.out', iStat)
    IF (iStat == -1) THEN
      CALL Logger%LogLastMessage()
      EXIT
    END IF

    ! Banner
    CALL Logger%LogMessage(' ', f_iInfo, 'ResultsExtract')
    CALL Logger%LogMessage('Program ResultsExtract - Generalized Hydrograph Extractor', &
                    f_iInfo, 'ResultsExtract')
    CALL Logger%LogMessage('Extracts hydrographs from all-node output files (HEAD/SUBSIDENCE)', &
                    f_iInfo, 'ResultsExtract')
    CALL Logger%LogMessage(' ', f_iInfo, 'ResultsExtract')

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
      CALL Logger%LogLastMessage()
      EXIT
    END IF

    ! Run extraction
    CALL App%Run(iStat)
    IF (iStat == -1) THEN
      CALL Logger%LogLastMessage()
    END IF

    ! Clean up
    CALL App%Kill()

    CALL Logger%LogMessage(' ', f_iInfo, 'ResultsExtract')
    CALL Logger%LogMessage('NORMAL TERMINATION - ResultsExtract', f_iInfo, 'ResultsExtract')

    EXIT  ! Normal exit from single-pass block
  END DO

  CALL Timer%Stop()
  CALL Logger%PrintRunTime()
  CALL Logger%Kill()

END PROGRAM ResultsExtract_Main
