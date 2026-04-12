!***********************************************************************
!  IWFM Inquiry-Mode Load Diagnostic Test
!
!  Standalone Fortran driver that loads an existing IWFM model in inquiry
!  mode (lForInquiry=.TRUE.), times the load, exercises a representative
!  set of Get* methods used by pywfm, and exits cleanly.
!
!  Purpose:
!    - Phase 1 profiling of the slow inquiry path (with f_lLogPerfMarkers
!      enabled in MessageLogger.f90, captures the [PERF] timing breakdown
!      written to SimulationMessages.out).
!    - Phase 2/3 verification harness: re-running with the same model
!      after gating optimizations should produce identical getter return
!      values to a pre-Track-3 baseline.
!
!  Usage: test_inquiry_load.exe <Sim_MAIN_File>
!
!  Example:
!    test_inquiry_load.exe C:\Users\hatch\Desktop\c2vsimfg\Simulation\C2VSimFG.in
!
!  Exit codes:
!    0  - load + all getters succeeded
!    1  - argument parsing or path resolution failed
!    2  - Model%New failed
!    3  - one or more getters failed
!***********************************************************************
PROGRAM test_inquiry_load
  USE Package_Model         , ONLY: ModelType
  USE MessageLogger         , ONLY: SetLogFileName, KillLogFile, LogLastMessage
  USE ProgramTimer          , ONLY: StartTimer, StopTimer
  IMPLICIT NONE

  ! Local variables
  TYPE(ModelType)    :: Model
  CHARACTER(LEN=500) :: cSimFileName
  INTEGER            :: iStat, iArg, iAnyGetterFailed
  REAL               :: t_start, t_end

  ! Result holders for getter exercises
  INTEGER                          :: NNodes, NElements, NLayers, NSubregions
  INTEGER                          :: NReaches, NStrmNodes, NLakes, NDiversions
  INTEGER                          :: NBypasses, NWells, NHydrographTypes
  INTEGER, ALLOCATABLE             :: iNodeIDs(:), iElemIDs(:), iSubregionIDs(:)
  REAL(8), ALLOCATABLE             :: rKh(:,:), rSy(:,:), rSs(:,:), rKv(:,:)
  REAL(8), ALLOCATABLE             :: rTopElev(:,:), rBottomElev(:,:)

  iAnyGetterFailed = 0

  ! ----- Parse command line -----
  iArg = COMMAND_ARGUMENT_COUNT()
  IF (iArg .LT. 1) THEN
    WRITE(*,*) '================================================================'
    WRITE(*,*) 'IWFM Inquiry-Mode Load Diagnostic Test'
    WRITE(*,*) '================================================================'
    WRITE(*,*) 'Usage: test_inquiry_load <Sim_MAIN_File>'
    WRITE(*,*) ''
    WRITE(*,*) '  Sim_MAIN_File   Path to the simulation main input file'
    WRITE(*,*) '                  (e.g. C2VSimFG.in)'
    WRITE(*,*) ''
    WRITE(*,*) 'Loads the model with lForInquiry=.TRUE., times the New call,'
    WRITE(*,*) 'and exercises a set of Get* methods to verify the slow inquiry'
    WRITE(*,*) 'path is functional. Profile output (when f_lLogPerfMarkers=.TRUE.'
    WRITE(*,*) 'in MessageLogger.f90) is written to SimulationMessages.out.'
    STOP 1
  END IF

  CALL GET_COMMAND_ARGUMENT(1, cSimFileName)

  WRITE(*,*) '================================================================'
  WRITE(*,*) 'IWFM Inquiry-Mode Load Diagnostic Test'
  WRITE(*,*) '================================================================'
  WRITE(*,*) 'Sim file: ', TRIM(cSimFileName)
  WRITE(*,*) ''

  ! ----- Start timer + open log file -----
  CALL StartTimer()
  CALL SetLogFileName('SimulationMessages.out', iStat)
  IF (iStat .EQ. -1) THEN
    CALL LogLastMessage()
    STOP 2
  END IF

  ! ----- Load the model in INQUIRY mode -----
  WRITE(*,*) 'Calling Model%New(... lForInquiry=.TRUE. ...)'
  CALL CPU_TIME(t_start)
  CALL Model%New('IWFM', cSimFileName, '', lForInquiry=.TRUE., iStat=iStat)
  CALL CPU_TIME(t_end)

  IF (iStat .EQ. -1) THEN
    WRITE(*,*) 'ERROR: Model%New failed'
    CALL LogLastMessage()
    STOP 2
  END IF

  WRITE(*,'(A,F10.3,A)') ' Model%New wall time: ', (t_end - t_start), ' seconds'
  WRITE(*,*) ''

  ! ----- Exercise representative getters -----
  WRITE(*,*) 'Exercising representative Get* methods...'

  ! --- Topology ---
  NNodes = Model%GetNNodes()
  WRITE(*,'(A,I8)')        '  GetNNodes:                ', NNodes
  NElements = Model%GetNElements()
  WRITE(*,'(A,I8)')        '  GetNElements:             ', NElements
  NLayers = Model%GetNLayers()
  WRITE(*,'(A,I8)')        '  GetNLayers:               ', NLayers
  NSubregions = Model%GetNSubregions()
  WRITE(*,'(A,I8)')        '  GetNSubregions:           ', NSubregions

  ! --- Stream / lake / diversion / pumping topology ---
  NReaches = Model%GetNReaches()
  WRITE(*,'(A,I8)')        '  GetNReaches:              ', NReaches
  NStrmNodes = Model%GetNStrmNodes()
  WRITE(*,'(A,I8)')        '  GetNStrmNodes:            ', NStrmNodes
  NLakes = Model%GetNLakes()
  WRITE(*,'(A,I8)')        '  GetNLakes:                ', NLakes
  NDiversions = Model%GetNDiversions()
  WRITE(*,'(A,I8)')        '  GetNDiversions:           ', NDiversions
  CALL Model%GetNBypasses(NBypasses, iStat)
  WRITE(*,'(A,I8)')        '  GetNBypasses:             ', NBypasses
  NWells = Model%GetNWells()
  WRITE(*,'(A,I8)')        '  GetNWells:                ', NWells
  NHydrographTypes = Model%GetNHydrographTypes()
  WRITE(*,'(A,I8)')        '  GetNHydrographTypes:      ', NHydrographTypes

  ! --- ID lookups ---
  ALLOCATE(iNodeIDs(NNodes), iElemIDs(NElements), iSubregionIDs(NSubregions))
  CALL Model%GetNodeIDs(iNodeIDs)
  WRITE(*,'(A,I8,A,I8)')   '  GetNodeIDs[1]:            ', iNodeIDs(1), ' [N]: ', iNodeIDs(NNodes)
  CALL Model%GetElementIDs(iElemIDs)
  WRITE(*,'(A,I8,A,I8)')   '  GetElementIDs[1]:         ', iElemIDs(1), ' [N]: ', iElemIDs(NElements)
  CALL Model%GetSubregionIDs(iSubregionIDs)
  WRITE(*,'(A,I8,A,I8)')   '  GetSubregionIDs[1]:       ', iSubregionIDs(1), ' [N]: ', iSubregionIDs(NSubregions)

  ! --- Aquifer parameters (the Phase 3 prime targets) ---
  ALLOCATE(rKh(NNodes,NLayers), rSy(NNodes,NLayers), rSs(NNodes,NLayers), rKv(NNodes,NLayers))
  ALLOCATE(rTopElev(NNodes,NLayers), rBottomElev(NNodes,NLayers))

  CALL Model%GetAquiferHorizontalK(rKh, iStat)
  IF (iStat .EQ. -1) THEN
    WRITE(*,*) '  ERROR: GetAquiferHorizontalK failed'
    iAnyGetterFailed = iAnyGetterFailed + 1
  ELSE
    WRITE(*,'(A,ES12.4,A,ES12.4)') '  GetAquiferHorizontalK [1,1]: ', rKh(1,1), ' [N,L]: ', rKh(NNodes,NLayers)
  END IF

  CALL Model%GetAquiferSy(rSy, iStat)
  IF (iStat .EQ. -1) THEN
    WRITE(*,*) '  ERROR: GetAquiferSy failed'
    iAnyGetterFailed = iAnyGetterFailed + 1
  ELSE
    WRITE(*,'(A,ES12.4,A,ES12.4)') '  GetAquiferSy [1,1]:          ', rSy(1,1), ' [N,L]: ', rSy(NNodes,NLayers)
  END IF

  CALL Model%GetAquiferSs(rSs, iStat)
  IF (iStat .EQ. -1) THEN
    WRITE(*,*) '  ERROR: GetAquiferSs failed'
    iAnyGetterFailed = iAnyGetterFailed + 1
  ELSE
    WRITE(*,'(A,ES12.4,A,ES12.4)') '  GetAquiferSs [1,1]:          ', rSs(1,1), ' [N,L]: ', rSs(NNodes,NLayers)
  END IF

  CALL Model%GetAquiferVerticalK(rKv, iStat)
  IF (iStat .EQ. -1) THEN
    WRITE(*,*) '  ERROR: GetAquiferVerticalK failed'
    iAnyGetterFailed = iAnyGetterFailed + 1
  ELSE
    WRITE(*,'(A,ES12.4,A,ES12.4)') '  GetAquiferVerticalK [1,1]:   ', rKv(1,1), ' [N,L]: ', rKv(NNodes,NLayers)
  END IF

  CALL Model%GetAquiferTopElev(rTopElev)
  WRITE(*,'(A,ES12.4,A,ES12.4)')   '  GetAquiferTopElev [1,1]:     ', rTopElev(1,1), ' [N,L]: ', rTopElev(NNodes,NLayers)

  CALL Model%GetAquiferBottomElev(rBottomElev)
  WRITE(*,'(A,ES12.4,A,ES12.4)')   '  GetAquiferBottomElev [1,1]:  ', rBottomElev(1,1), ' [N,L]: ', rBottomElev(NNodes,NLayers)

  ! ----- Tear down -----
  WRITE(*,*) ''
  WRITE(*,*) 'Calling Model%Kill()'
  CALL Model%Kill()

  CALL StopTimer()
  CALL KillLogFile()

  WRITE(*,*) ''
  WRITE(*,*) '================================================================'
  IF (iAnyGetterFailed .EQ. 0) THEN
    WRITE(*,*) 'ALL CHECKS PASSED'
    WRITE(*,*) '================================================================'
    WRITE(*,*) ''
    WRITE(*,*) 'For the [PERF] init-timing breakdown, see SimulationMessages.out'
    WRITE(*,*) '(requires f_lLogPerfMarkers=.TRUE. in MessageLogger.f90:113).'
  ELSE
    WRITE(*,'(A,I3,A)') ' ', iAnyGetterFailed, ' GETTER(S) FAILED'
    WRITE(*,*) '================================================================'
    STOP 3
  END IF

END PROGRAM test_inquiry_load
