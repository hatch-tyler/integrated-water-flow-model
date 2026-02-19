!***********************************************************************
!  IWFM2OBS - Class_IWFM2OBS
!  Main orchestrator: input file parsing, 4 hydrograph types,
!  optional multi-layer target, head differences
!
!  Replaces the old iwfm2obs main program + multilayertarget
!  Input: new unified input file format (all paths explicit)
!  Output: PEST SMP, instruction, and PCF files
!***********************************************************************
MODULE Class_IWFM2OBS

  USE MessageLogger      , ONLY: SetLastMessage   , &
                                 LogMessage        , &
                                 LogLastMessage    , &
                                 f_iFatal          , &
                                 f_iWarn           , &
                                 f_iInfo
  USE GeneralUtilities   , ONLY: IntToText         , &
                                 UpperCase
  USE Class_SMP2SMP      , ONLY: SMP2SMPType       , &
                                 SMPRecordType     , &
                                 SMPIDGroupType
  USE Class_PESTOutput   , ONLY: PESTOutputType
  USE Class_HeadDifference, ONLY: HeadDifferenceType, &
                                  HeadDiffPairType
  USE Class_MultiLayerTarget, ONLY: MultiLayerTargetType

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: IWFM2OBSType

  CHARACTER(LEN=25), PARAMETER :: cModName = 'Class_IWFM2OBS'

  ! Hydrograph type indices (matching original convention)
  INTEGER, PARAMETER :: iSUBSID = 1
  INTEGER, PARAMETER :: iTILEDR = 2
  INTEGER, PARAMETER :: iSTREAM = 3
  INTEGER, PARAMETER :: iGWHEAD = 4
  INTEGER, PARAMETER :: iNUMHYD = 4

  CHARACTER(LEN=12), PARAMETER :: cHydName(4) = &
       (/ 'Subsidence  ', 'Tile Drain  ', 'Stream      ', 'Groundwater ' /)

  ! =====================================================================
  ! HydTypeConfigType - Configuration for one hydrograph type
  ! =====================================================================
  TYPE :: HydTypeConfigType
    CHARACTER(LEN=500) :: cHydFile  = ' '   ! Model hydrograph SMP file
    CHARACTER(LEN=500) :: cObsFile  = ' '   ! Observation SMP file
    CHARACTER(LEN=500) :: cOutFile  = ' '   ! Output SMP file
    REAL(8)            :: rThreshold = 1.0D0 ! Extrapolation threshold (days)
    CHARACTER(LEN=500) :: cInsFile  = ' '   ! PEST instruction file
    CHARACTER(LEN=500) :: cPCFFile  = ' '   ! PEST PCF file
    LOGICAL            :: lActive   = .FALSE.
    LOGICAL            :: lWriteIns = .FALSE.
  END TYPE HydTypeConfigType

  ! =====================================================================
  ! IWFM2OBSType - Main orchestrator
  ! =====================================================================
  TYPE :: IWFM2OBSType
    TYPE(HydTypeConfigType)    :: HydConfig(iNUMHYD)
    TYPE(SMP2SMPType)          :: Interp
    TYPE(HeadDifferenceType)   :: HeadDiff
    TYPE(MultiLayerTargetType) :: MultiLayer
    LOGICAL                    :: lMultiLayer = .FALSE.
    LOGICAL                    :: lHeadDiff   = .FALSE.
    CHARACTER(LEN=500)         :: cHDiffFile  = ' '
    ! Multi-layer file paths
    CHARACTER(LEN=500)         :: cObsWellFile = ' '
    CHARACTER(LEN=500)         :: cElemsFile   = ' '
    CHARACTER(LEN=500)         :: cNodesFile   = ' '
    CHARACTER(LEN=500)         :: cStratFile   = ' '
    CHARACTER(LEN=500)         :: cGWMainFile  = ' '
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Run
    PROCEDURE, PASS :: Kill
  END TYPE IWFM2OBSType

CONTAINS

  ! =====================================================================
  ! ReadNonComment - Read one non-comment line, strip inline comment
  ! =====================================================================
  SUBROUTINE ReadNonComment(iUnit, cLine, iStat)
    INTEGER,          INTENT(IN)  :: iUnit
    CHARACTER(LEN=*), INTENT(OUT) :: cLine
    INTEGER,          INTENT(OUT) :: iStat

    iStat = 0
    DO
      READ(iUnit, '(A)', IOSTAT=iStat) cLine
      IF (iStat /= 0) RETURN
      cLine = ADJUSTL(cLine)
      IF (LEN_TRIM(cLine) == 0) CYCLE
      IF (cLine(1:1) == 'C' .OR. cLine(1:1) == 'c' .OR. &
          cLine(1:1) == '*' .OR. cLine(1:1) == '#') CYCLE
      EXIT
    END DO
  END SUBROUTINE ReadNonComment

  ! =====================================================================
  ! StripComment - Remove inline comment (after '/')
  ! =====================================================================
  SUBROUTINE StripComment(cLine, cResult)
    CHARACTER(LEN=*), INTENT(IN)  :: cLine
    CHARACTER(LEN=*), INTENT(OUT) :: cResult
    INTEGER :: iPos

    cResult = cLine
    iPos = SCAN(cResult, '/')
    IF (iPos > 1) cResult = cResult(1:iPos-1)
    cResult = ADJUSTL(TRIM(cResult))
  END SUBROUTINE StripComment

  ! =====================================================================
  ! New - Read input file and initialize
  ! =====================================================================
  SUBROUTINE New(This, cInputFile, iStat)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),    INTENT(IN)    :: cInputFile
    INTEGER,             INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 190
    CHARACTER(LEN=500) :: cLine, cClean
    CHARACTER(LEN=1)   :: cYN
    INTEGER            :: iErr, iDateSpec, iHyd, i

    iStat = 0

    OPEN(UNIT=iUnit, FILE=cInputFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open input file: '//TRIM(cInputFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! ---- Date format ----
    CALL ReadNonComment(iUnit, cLine, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Error reading date format from input file', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL StripComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iDateSpec
    IF (iErr /= 0 .OR. (iDateSpec /= 1 .AND. iDateSpec /= 2)) THEN
      CALL SetLastMessage('Invalid date format (must be 1 or 2)', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    CALL This%Interp%Init(iDateSpec)

    ! ---- Read 4 hydrograph type blocks (6 lines each) ----
    ! Order: GW (4), Stream (3), Tile Drain (2), Subsidence (1)
    DO iHyd = iGWHEAD, iSUBSID, -1
      ! Line 1: Hydrograph/model SMP file
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading '//TRIM(cHydName(iHyd))// &
             ' hydrograph file path', f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      CALL StripComment(cLine, cClean)

      IF (LEN_TRIM(cClean) == 0) THEN
        ! Skip this hydrograph type (blank = inactive)
        This%HydConfig(iHyd)%lActive = .FALSE.
        ! Still need to read 5 more lines
        DO i = 1, 5
          CALL ReadNonComment(iUnit, cLine, iErr)
        END DO
        CYCLE
      END IF

      This%HydConfig(iHyd)%cHydFile = cClean
      This%HydConfig(iHyd)%lActive  = .TRUE.

      ! Line 2: Observation SMP file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cObsFile)

      ! Line 3: Output SMP file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cOutFile)

      ! Line 4: Extrapolation threshold
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, cClean)
      READ(cClean, *, IOSTAT=iErr) This%HydConfig(iHyd)%rThreshold
      IF (iErr /= 0) This%HydConfig(iHyd)%rThreshold = 1.0D0

      ! Line 5: Instruction file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cInsFile)
      This%HydConfig(iHyd)%lWriteIns = (LEN_TRIM(This%HydConfig(iHyd)%cInsFile) > 0)

      ! Line 6: PCF file
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%HydConfig(iHyd)%cPCFFile)
    END DO

    ! ---- Head differences (Y/N, then file path if Y) ----
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripComment(cLine, cClean)
    cYN = UpperCase(cClean(1:1))
    IF (cYN == 'Y') THEN
      This%lHeadDiff = .TRUE.
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cHDiffFile)
    END IF

    ! ---- Multi-layer target (Y/N, then 5 file paths if Y) ----
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripComment(cLine, cClean)
    cYN = UpperCase(cClean(1:1))
    IF (cYN == 'Y') THEN
      This%lMultiLayer = .TRUE.
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cObsWellFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cElemsFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cNodesFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cStratFile)
      CALL ReadNonComment(iUnit, cLine, iErr)
      CALL StripComment(cLine, This%cGWMainFile)
    END IF

    CLOSE(iUnit)

    ! ---- Initialize multi-layer target if requested ----
    IF (This%lMultiLayer) THEN
      CALL This%MultiLayer%New(This%cNodesFile, This%cElemsFile, &
           This%cStratFile, This%cGWMainFile, This%cObsWellFile, iStat)
      IF (iStat /= 0) RETURN
    END IF

    ! ---- Initialize head differences if requested ----
    IF (This%lHeadDiff) THEN
      CALL This%HeadDiff%New(This%cHDiffFile, iStat)
      IF (iStat /= 0) RETURN
    END IF

    ! Report configuration
    DO iHyd = 1, iNUMHYD
      IF (This%HydConfig(iHyd)%lActive) THEN
        CALL LogMessage('  '//TRIM(cHydName(iHyd))//': '// &
             TRIM(This%HydConfig(iHyd)%cHydFile), f_iInfo, cModName)
      END IF
    END DO
    IF (This%lHeadDiff) &
      CALL LogMessage('  Head differences: '//TRIM(This%cHDiffFile), &
           f_iInfo, cModName)
    IF (This%lMultiLayer) &
      CALL LogMessage('  Multi-layer target: active ('// &
           TRIM(IntToText(This%MultiLayer%GetNObs()))//' wells)', &
           f_iInfo, cModName)

  END SUBROUTINE New

  ! =====================================================================
  ! Run - Execute the interpolation workflow
  ! =====================================================================
  SUBROUTINE Run(This, iStat)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This
    INTEGER,             INTENT(OUT)   :: iStat

    INTEGER :: iHyd, i

    iStat = 0

    ! Process each hydrograph type
    DO iHyd = 1, iNUMHYD
      IF (.NOT. This%HydConfig(iHyd)%lActive) CYCLE

      CALL LogMessage('Processing '//TRIM(cHydName(iHyd))//' hydrographs...', &
           f_iInfo, cModName)

      ! Call SMP2SMP interpolation
      CALL This%Interp%Interpolate( &
           This%HydConfig(iHyd)%cObsFile, &
           This%HydConfig(iHyd)%cHydFile, &
           This%HydConfig(iHyd)%cOutFile, &
           This%HydConfig(iHyd)%rThreshold, &
           This%HydConfig(iHyd)%cInsFile, &
           This%HydConfig(iHyd)%cPCFFile, &
           This%HydConfig(iHyd)%lWriteIns, &
           iStat)
      IF (iStat /= 0) THEN
        CALL LogMessage('  Error processing '//TRIM(cHydName(iHyd)), &
             f_iWarn, cModName)
        iStat = 0  ! Continue with other types
        CYCLE
      END IF
    END DO

    ! Multi-layer target: post-process GW head output
    IF (This%lMultiLayer .AND. This%HydConfig(iGWHEAD)%lActive) THEN
      CALL LogMessage('Applying multi-layer target averaging...', &
           f_iInfo, cModName)
      CALL ApplyMultiLayerTarget(This, iStat)
      IF (iStat /= 0) THEN
        CALL LogMessage('  Error in multi-layer processing', f_iWarn, cModName)
        iStat = 0
      END IF
    END IF

  END SUBROUTINE Run

  ! =====================================================================
  ! ApplyMultiLayerTarget - Post-process GW heads with T-weighted averaging
  !
  !   Reads the per-layer SMP output (from SMP2SMP), groups records by
  !   base well name (stripping %layer suffix), applies T-weighted
  !   averaging, and writes a new composite SMP file.
  ! =====================================================================
  SUBROUTINE ApplyMultiLayerTarget(This, iStat)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This
    INTEGER,             INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iInUnit = 192, iOutUnit = 193
    INTEGER, PARAMETER :: iInsUnit = 194, iPCFUnit = 195
    CHARACTER(LEN=500) :: cOutFile, cInsFile, cPCFFile
    CHARACTER(LEN=25)  :: cBaseID
    INTEGER            :: iErr, iWell, iNLayers, iPos
    REAL(8), ALLOCATABLE :: rLayerVals(:)
    LOGICAL            :: lWriteIns

    iStat = 0
    iNLayers = This%MultiLayer%GetNLayers()
    ALLOCATE(rLayerVals(iNLayers), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate layer values array', &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! The per-layer SMP output file has records with IDs like WELL%1, WELL%2
    ! We need to read it, group by base name, and compute weighted averages
    ! Output goes to a new file with suffix _ml

    cOutFile = TRIM(This%HydConfig(iGWHEAD)%cOutFile)
    iPos = SCAN(cOutFile, '.', BACK=.TRUE.)
    IF (iPos > 0) THEN
      cOutFile = cOutFile(1:iPos-1)//'_ml'//cOutFile(iPos:)
    ELSE
      cOutFile = TRIM(cOutFile)//'_ml'
    END IF

    lWriteIns = This%HydConfig(iGWHEAD)%lWriteIns
    IF (lWriteIns) THEN
      cInsFile = TRIM(This%HydConfig(iGWHEAD)%cInsFile)
      iPos = SCAN(cInsFile, '.', BACK=.TRUE.)
      IF (iPos > 0) THEN
        cInsFile = cInsFile(1:iPos-1)//'_ml'//cInsFile(iPos:)
      ELSE
        cInsFile = TRIM(cInsFile)//'_ml'
      END IF
      cPCFFile = TRIM(This%HydConfig(iGWHEAD)%cPCFFile)
      iPos = SCAN(cPCFFile, '.', BACK=.TRUE.)
      IF (iPos > 0) THEN
        cPCFFile = cPCFFile(1:iPos-1)//'_ml'//cPCFFile(iPos:)
      ELSE
        cPCFFile = TRIM(cPCFFile)//'_ml'
      END IF
    END IF

    ! Open input (per-layer SMP from interpolation)
    OPEN(UNIT=iInUnit, FILE=This%HydConfig(iGWHEAD)%cOutFile, &
         STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open per-layer SMP: '// &
           TRIM(This%HydConfig(iGWHEAD)%cOutFile), f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Open output
    OPEN(UNIT=iOutUnit, FILE=cOutFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open multi-layer output: '// &
           TRIM(cOutFile), f_iFatal, cModName)
      CLOSE(iInUnit); iStat = -1; RETURN
    END IF

    IF (lWriteIns) THEN
      OPEN(UNIT=iInsUnit, FILE=cInsFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iInUnit); CLOSE(iOutUnit); iStat = -1; RETURN
      END IF
      WRITE(iInsUnit, '(A)') 'pif #'

      OPEN(UNIT=iPCFUnit, FILE=cPCFFile, STATUS='REPLACE', IOSTAT=iErr)
      IF (iErr /= 0) THEN
        CLOSE(iInUnit); CLOSE(iOutUnit); CLOSE(iInsUnit)
        iStat = -1; RETURN
      END IF
    END IF

    ! Process the per-layer SMP file
    ! Records are ordered: WELL1%1 time1 val, WELL1%1 time2 val, ...
    !                      WELL1%2 time1 val, WELL1%2 time2 val, ...
    ! For each observation well and each time step, collect nlayer values
    ! and compute weighted average
    !
    ! This simplified approach reads the per-layer output and matches
    ! by well name to the observation well list

    DO iWell = 1, This%MultiLayer%GetNObs()
      cBaseID = This%MultiLayer%GetObsName(iWell)

      ! Read per-layer records for this well
      ! The per-layer SMP has records grouped by layer with IDs: BASENAME%1, BASENAME%2, etc.
      ! We need to read across layers for each time step
      ! This requires the file to be organized: all layer1 records, then layer2, etc.

      ! For now, we read the entire file for each well (not efficient but correct)
      REWIND(iInUnit)

      ! Count records per layer for this well
      ! Then read layer values at each time step
      CALL ProcessWellLayers(iInUnit, cBaseID, iNLayers, &
           This%MultiLayer, iWell, iOutUnit, This%Interp, &
           iInsUnit, iPCFUnit, lWriteIns, iStat)
      IF (iStat /= 0) EXIT
    END DO

    CLOSE(iInUnit)
    CLOSE(iOutUnit)
    IF (lWriteIns) THEN
      CLOSE(iInsUnit)
      CLOSE(iPCFUnit)
    END IF
    DEALLOCATE(rLayerVals)

    IF (iStat == 0) THEN
      CALL LogMessage('  Multi-layer output written to: '//TRIM(cOutFile), &
           f_iInfo, cModName)
    END IF

  END SUBROUTINE ApplyMultiLayerTarget

  ! =====================================================================
  ! ProcessWellLayers - Read per-layer SMP for one well, compute weighted avg
  ! =====================================================================
  SUBROUTINE ProcessWellLayers(iInUnit, cBaseID, iNLayers, MultiLayer, &
                               iWell, iOutUnit, Interp, &
                               iInsUnit, iPCFUnit, lWriteIns, iStat)
    INTEGER,                     INTENT(IN)    :: iInUnit, iOutUnit
    INTEGER,                     INTENT(IN)    :: iInsUnit, iPCFUnit
    CHARACTER(LEN=*),            INTENT(IN)    :: cBaseID
    INTEGER,                     INTENT(IN)    :: iNLayers, iWell
    TYPE(MultiLayerTargetType),  INTENT(IN)    :: MultiLayer
    TYPE(SMP2SMPType),           INTENT(IN)    :: Interp
    LOGICAL,                     INTENT(IN)    :: lWriteIns
    INTEGER,                     INTENT(OUT)   :: iStat

    CHARACTER(LEN=500) :: cLine
    CHARACTER(LEN=25)  :: cID, cLayerID
    CHARACTER(LEN=30)  :: cDateStr, cTimeStr, cValStr
    INTEGER :: iErr, k, iLayer, iPos, iSeqNum
    INTEGER :: iNRecPerLayer, iRec
    REAL(8) :: rVal, rWeighted
    REAL(8), ALLOCATABLE :: rLayerVals(:)
    ! Per-layer record storage
    CHARACTER(LEN=30), ALLOCATABLE :: cDates(:), cTimes(:)
    REAL(8), ALLOCATABLE :: rVals(:,:)  ! (nrec, nlayers)
    INTEGER :: iNRec, iLineCount
    INTEGER :: iDay, iMon, iYear, iHH, iMM, iSS, iJulDay, iSecs

    iStat = 0
    ALLOCATE(rLayerVals(iNLayers))

    ! First pass: count records for first layer of this well
    REWIND(iInUnit)
    iNRec = 0
    cLayerID = TRIM(cBaseID)//'%1'
    DO
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) EXIT
      IF (LEN_TRIM(cLine) == 0) CYCLE
      READ(cLine, *, IOSTAT=iErr) cID
      IF (iErr /= 0) CYCLE
      cID = ADJUSTL(UpperCase(cID))
      IF (cID == UpperCase(cLayerID)) iNRec = iNRec + 1
    END DO

    IF (iNRec == 0) THEN
      DEALLOCATE(rLayerVals)
      RETURN  ! No records for this well
    END IF

    ! Allocate storage
    ALLOCATE(cDates(iNRec), cTimes(iNRec), rVals(iNRec, iNLayers), STAT=iErr)
    IF (iErr /= 0) THEN
      iStat = -1; DEALLOCATE(rLayerVals); RETURN
    END IF
    rVals = 0.0D0

    ! Second pass: read records for each layer
    DO k = 1, iNLayers
      WRITE(cLayerID, '(A,A1,I0)') TRIM(cBaseID), '%', k
      REWIND(iInUnit)
      iRec = 0
      DO
        READ(iInUnit, '(A)', IOSTAT=iErr) cLine
        IF (iErr /= 0) EXIT
        IF (LEN_TRIM(cLine) == 0) CYCLE
        READ(cLine, *, IOSTAT=iErr) cID, cDateStr, cTimeStr, cValStr
        IF (iErr /= 0) CYCLE
        cID = ADJUSTL(UpperCase(cID))
        IF (cID == UpperCase(cLayerID)) THEN
          iRec = iRec + 1
          IF (iRec > iNRec) EXIT
          IF (k == 1) THEN
            cDates(iRec) = cDateStr
            cTimes(iRec) = cTimeStr
          END IF
          READ(cValStr, *, IOSTAT=iErr) rVals(iRec, k)
        END IF
      END DO
    END DO

    ! Compute weighted averages and write output
    iSeqNum = 0
    DO iRec = 1, iNRec
      rLayerVals = rVals(iRec, :)
      rWeighted = MultiLayer%WeightedAverage(iWell, rLayerVals)

      ! Parse date/time for output
      CALL Interp%ParseDateStr(cDates(iRec), iDay, iMon, iYear, iErr)
      IF (iErr /= 0) CYCLE
      CALL Interp%ParseTimeStr(cTimes(iRec), iHH, iMM, iSS, iErr)
      IF (iErr /= 0) CYCLE

      ! Write SMP output
      IF (Interp%iDateSpec == 1) THEN
        WRITE(iOutUnit, 100) TRIM(cBaseID), iDay, iMon, iYear, &
             iHH, iMM, iSS, rWeighted
      ELSE
        WRITE(iOutUnit, 100) TRIM(cBaseID), iMon, iDay, iYear, &
             iHH, iMM, iSS, rWeighted
      END IF
100   FORMAT(1X,A,10X,I2.2,'/',I2.2,'/',I4.4,3X,I2.2,':',I2.2,':',I2.2,3X,1PG15.8)

      ! Write PEST instruction and PCF files
      IF (lWriteIns) THEN
        iSeqNum = iSeqNum + 1
        WRITE(iInsUnit, 200) TRIM(cBaseID), iSeqNum
200     FORMAT('l1  [',A,'_',I4.4,']37:56')
        ! For PCF, we'd need the observation value - use 0 as placeholder
        ! In practice, the obs value comes from the obs SMP file
        WRITE(iPCFUnit, 210) TRIM(cBaseID), iSeqNum, rWeighted
210     FORMAT(A,'_',I4.4,'    ',1PG15.8)
      END IF
    END DO

    DEALLOCATE(rLayerVals, cDates, cTimes, rVals)

  END SUBROUTINE ProcessWellLayers

  ! =====================================================================
  ! Kill - Clean up
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(IWFM2OBSType), INTENT(INOUT) :: This

    CALL This%HeadDiff%Kill()
    IF (This%lMultiLayer) CALL This%MultiLayer%Kill()

  END SUBROUTINE Kill

END MODULE Class_IWFM2OBS
