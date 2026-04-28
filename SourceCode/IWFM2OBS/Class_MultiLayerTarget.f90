!***********************************************************************
!  IWFM2OBS - Class_MultiLayerTarget
!  Transmissivity-weighted depth averaging of GW heads across layers
!
!  Ported from MultiLayerTarget utility. Key improvement: uses IWFM
!  FE shape function interpolation (GridType%FEInterpolate_AtCell)
!  instead of naive IDW.
!
!  NOTE: The GW file parsing follows a specific IWFM file structure.
!  If the structure changes between IWFM versions, the skip counts
!  in ReadHydraulicConductivity may need adjustment.
!***********************************************************************
MODULE Class_MultiLayerTarget

  USE MessageLogger    , ONLY: MessageLoggerType , &
                               DefaultLogger     , &
                               f_iFatal       , &
                               f_iInfo
  USE GeneralUtilities , ONLY: IntToText
  USE IWFM2OBS_Utilities, ONLY: StripAndClean
  USE IOInterface      , ONLY: GenericFileType
  USE Class_Grid       , ONLY: GridType

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: MultiLayerTargetType

  CHARACTER(LEN=30), PARAMETER :: cModName = 'Class_MultiLayerTarget'

  ! =====================================================================
  ! ObsWellType - One observation well with screen interval
  ! =====================================================================
  TYPE :: ObsWellType
    CHARACTER(LEN=25) :: cName = ' '
    REAL(8)           :: rX    = 0.0D0
    REAL(8)           :: rY    = 0.0D0
    INTEGER           :: iElem = 0
    REAL(8)           :: rBOS  = 0.0D0     ! Bottom of screen
    REAL(8)           :: rTOS  = 0.0D0     ! Top of screen
    INTEGER           :: iOverwriteLayer = -1  ! -1=use screen, >0=single layer
  END TYPE ObsWellType

  ! =====================================================================
  ! MultiLayerTargetType - Manager for transmissivity-weighted averaging
  ! =====================================================================
  TYPE :: MultiLayerTargetType
    TYPE(GridType)                     :: Grid
    REAL(8), ALLOCATABLE               :: rElevation(:,:)  ! (nnodes, nlayers+1)
    REAL(8), ALLOCATABLE               :: rHK(:,:)         ! (nnodes, nlayers)
    REAL(8), ALLOCATABLE               :: rWeight(:,:)     ! (nobs, nlayers) normalized T-weights
    REAL(8), ALLOCATABLE               :: rRawT(:,:)       ! (nobs, nlayers) raw transmissivity per layer
    REAL(8), ALLOCATABLE               :: rScreenTOS(:)    ! (nobs) effective screen top (clipped to model)
    REAL(8), ALLOCATABLE               :: rScreenBOS(:)    ! (nobs) effective screen bottom (clipped to model)
    TYPE(ObsWellType), ALLOCATABLE     :: ObsWells(:)
    INTEGER                            :: iNObs    = 0
    INTEGER                            :: iNNodes  = 0
    INTEGER                            :: iNLayers = 0
    INTEGER                            :: iNElems  = 0
    LOGICAL                            :: lActive  = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: New
    PROCEDURE, PASS :: Kill
    PROCEDURE, PASS :: GetNObs
    PROCEDURE, PASS :: GetNLayers
    PROCEDURE, PASS :: GetObsName
    PROCEDURE, PASS :: WeightedAverage
    PROCEDURE, PASS :: GetWellLayerTransmissivities
  END TYPE MultiLayerTargetType

CONTAINS


  ! =====================================================================
  ! ExtractFirstInt - Extract the first integer from a line
  !   Strips any text before the number (e.g., "NE 1234" or "/ 1234")
  ! =====================================================================
  SUBROUTINE ExtractFirstInt(cLine, iVal, iStat)
    CHARACTER(LEN=*), INTENT(IN)  :: cLine
    INTEGER,          INTENT(OUT) :: iVal, iStat

    CHARACTER(LEN=500) :: cWork
    INTEGER :: iPos

    iStat = 0
    cWork = ADJUSTL(cLine)

    ! Strip inline comment (after '/')
    iPos = SCAN(cWork, '/')
    IF (iPos > 1) cWork = cWork(1:iPos-1)

    ! Try to read integer from the first token
    READ(cWork, *, IOSTAT=iStat) iVal

  END SUBROUTINE ExtractFirstInt

  ! =====================================================================
  ! New - Read mesh, stratigraphy, HK, obs wells; compute T-weights
  ! =====================================================================
  SUBROUTINE New(This, cNodesFile, cElemsFile, cStratFile, cGWFile, &
                 cObsWellFile, iStat)
    CLASS(MultiLayerTargetType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),           INTENT(IN)     :: cNodesFile
    CHARACTER(LEN=*),           INTENT(IN)     :: cElemsFile
    CHARACTER(LEN=*),           INTENT(IN)     :: cStratFile
    CHARACTER(LEN=*),           INTENT(IN)     :: cGWFile
    CHARACTER(LEN=*),           INTENT(IN)     :: cObsWellFile
    INTEGER,                    INTENT(OUT)    :: iStat

    TYPE(GenericFileType) :: ConfigFile
    CHARACTER(LEN=1000) :: cLine
    INTEGER :: i, k, iErr, iDum, iNReg
    INTEGER :: iNOUTH, iNOUTF, iNGROUP
    REAL(8), ALLOCATABLE :: rDumArr(:), rThick(:)
    REAL(8), ALLOCATABLE :: rInterpHK(:,:), rInterpElev(:,:)
    REAL(8), ALLOCATABLE :: rInterpT(:,:), rObsTrans(:)
    REAL(8) :: rTope, rBote, rTempThick
    REAL(8), ALLOCATABLE :: rCoeff(:)
    INTEGER :: iNVerts
    INTEGER :: iNObsMax, iNObsCount

    iStat = 0

    ! ==================================================================
    ! 1. Read nodes file
    ! ==================================================================
    CALL ConfigFile%New(FileName=cNodesFile, InputFile=.TRUE., &
         IsTSFile=.FALSE., Descriptor='Nodes file', iStat=iStat)
    IF (iStat == -1) RETURN

    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cLine)
    CALL ExtractFirstInt(cLine, This%iNNodes, iErr)
    IF (iErr /= 0 .OR. This%iNNodes <= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot read node count from: '//TRIM(cNodesFile), &
           f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    ALLOCATE(This%Grid%X(This%iNNodes), This%Grid%Y(This%iNNodes), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot allocate node arrays', f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    ! Skip 1 factor/header line (old code read 2, backspaced 1)
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF

    DO i = 1, This%iNNodes
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
      READ(cLine, *, IOSTAT=iErr) iDum, This%Grid%X(i), This%Grid%Y(i)
      IF (iErr /= 0) THEN
        CALL DefaultLogger%SetLastMessage('Error reading node '//TRIM(IntToText(i))// &
             ' from: '//TRIM(cNodesFile), f_iFatal, cModName)
        CALL ConfigFile%Kill(); iStat = -1; RETURN
      END IF
    END DO
    CALL ConfigFile%Kill()

    ! ==================================================================
    ! 2. Read elements file
    ! ==================================================================
    CALL ConfigFile%New(FileName=cElemsFile, InputFile=.TRUE., &
         IsTSFile=.FALSE., Descriptor='Elements file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! Scan for the line containing the number of elements
    ! IWFM elements file: scan until we find a line with 'N' or 'E'
    ! (scan raw line including inline comment, then strip for parsing)
    DO
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL DefaultLogger%SetLastMessage('Unexpected end of elements file: '// &
             TRIM(cElemsFile), f_iFatal, cModName)
        CALL ConfigFile%Kill(); iStat = -1; RETURN
      END IF
      IF (SCAN(cLine, 'NEne') > 0) EXIT
    END DO
    CALL StripAndClean(cLine, cLine)

    CALL ExtractFirstInt(cLine, This%iNElems, iErr)
    IF (iErr /= 0 .OR. This%iNElems <= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot read element count from: '// &
           TRIM(cElemsFile), f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    ALLOCATE(This%Grid%Vertex(4, This%iNElems), &
             This%Grid%NVertex(This%iNElems), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot allocate element arrays', f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF
    This%Grid%Vertex  = 0
    This%Grid%NVertex = 4

    ! Read number of subregions
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cLine)
    CALL ExtractFirstInt(cLine, iNReg, iErr)

    ! Skip subregion definitions: 1 header + (nreg-1) entries
    ! (old code read nreg+1 lines then backspaced; net = skip nreg lines)
    DO i = 1, iNReg
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    END DO

    ! Read element connectivity
    DO i = 1, This%iNElems
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
      READ(cLine, *, IOSTAT=iErr) iDum, &
           This%Grid%Vertex(1,iDum), This%Grid%Vertex(2,iDum), &
           This%Grid%Vertex(3,iDum), This%Grid%Vertex(4,iDum)
      IF (iErr /= 0) THEN
        CALL DefaultLogger%SetLastMessage('Error reading element '//TRIM(IntToText(i))// &
             ' from: '//TRIM(cElemsFile), f_iFatal, cModName)
        CALL ConfigFile%Kill(); iStat = -1; RETURN
      END IF
      ! Determine number of active vertices (0 = triangle)
      IF (This%Grid%Vertex(4, iDum) <= 0) THEN
        This%Grid%NVertex(iDum) = 3
      END IF
    END DO
    CALL ConfigFile%Kill()

    ! ==================================================================
    ! 3. Read stratigraphy file
    ! ==================================================================
    CALL ConfigFile%New(FileName=cStratFile, InputFile=.TRUE., &
         IsTSFile=.FALSE., Descriptor='Stratigraphy file', iStat=iStat)
    IF (iStat == -1) RETURN

    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cLine)
    CALL ExtractFirstInt(cLine, This%iNLayers, iErr)
    IF (iErr /= 0 .OR. This%iNLayers <= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot read layer count from: '//TRIM(cStratFile), &
           f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    ALLOCATE(This%rElevation(This%iNNodes, This%iNLayers+1), &
             This%rHK(This%iNNodes, This%iNLayers), &
             rDumArr(This%iNLayers), &
             rThick(This%iNLayers), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot allocate stratigraphy arrays', &
           f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    ! Skip 1 header line (old code read 2, backspaced 1)
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF

    ! Read stratigraphy: node_id, top_elev, (aquitard_thick, aquifer_thick) x nlayers
    DO i = 1, This%iNNodes
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL DefaultLogger%SetLastMessage('Error reading stratigraphy for node '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CALL ConfigFile%Kill(); RETURN
      END IF
      READ(cLine, *, IOSTAT=iErr) iDum, This%rElevation(i,1), &
           ((rDumArr(k), rThick(k)), k=1, This%iNLayers)
      IF (iErr /= 0) THEN
        CALL DefaultLogger%SetLastMessage('Error parsing stratigraphy for node '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CALL ConfigFile%Kill(); iStat = -1; RETURN
      END IF
      ! Compute layer bottom elevations
      DO k = 1, This%iNLayers
        This%rElevation(i, k+1) = This%rElevation(i, k) - rThick(k)
      END DO
    END DO
    CALL ConfigFile%Kill()
    DEALLOCATE(rDumArr, rThick)

    ! ==================================================================
    ! 4. Read hydraulic conductivity from GW main file
    !    NOTE: Skip counts are IWFM version-specific
    ! ==================================================================
    CALL ConfigFile%New(FileName=cGWFile, InputFile=.TRUE., &
         IsTSFile=.FALSE., Descriptor='GW main file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! GW file starts with a #4.0 version line that GenericFileType
    ! does NOT auto-skip (it only skips C/c/* comments). Read and discard.
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF

    ! Skip to NOUTH: 21 data lines after the version line
    ! (the old ReadNonComment auto-skipped #4.0 as a # comment, then
    !  read 21 data lines; we've already read #4.0 explicitly, so
    !  read 21 more data lines to reach NOUTH)
    DO i = 1, 21
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN
        CALL DefaultLogger%SetLastMessage('Unexpected end of GW file at line '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CALL ConfigFile%Kill(); RETURN
      END IF
    END DO
    CALL StripAndClean(cLine, cLine)
    CALL ExtractFirstInt(cLine, iNOUTH, iErr)

    ! Skip FACTXY + GWHYDOUTFL (old code read 3, backspaced 1 = skip 2)
    CALL ConfigFile%ReadData(cLine, iStat)  ! FACTXY
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    CALL ConfigFile%ReadData(cLine, iStat)  ! GWHYDOUTFL
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF

    ! Skip NOUTH observation entries
    DO i = 1, iNOUTH
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    END DO

    ! Read NOUTF
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cLine)
    CALL ExtractFirstInt(cLine, iNOUTF, iErr)

    ! Skip FCHYDOUTFL + NOUTF flow entries
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    DO i = 1, iNOUTF
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    END DO

    ! Read NGROUP
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    CALL StripAndClean(cLine, cLine)
    CALL ExtractFirstInt(cLine, iNGROUP, iErr)
    IF (iNGROUP > 0) THEN
      CALL DefaultLogger%SetLastMessage('Parametric grid (NGROUP>0) not supported', &
           f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    ! Skip FX + 3 more lines (time units etc.)
    DO i = 1, 4
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF
    END DO

    ! Read HK: for each node, for each layer, one data line
    ! Layer 1: "node_id HK_value"; Layers 2+: "HK_value"
    This%rHK = 0.0D0
    DO i = 1, This%iNNodes
      DO k = 1, This%iNLayers
        CALL ConfigFile%ReadData(cLine, iStat)
        IF (iStat == -1) THEN
          CALL DefaultLogger%SetLastMessage('Error reading HK for node '// &
               TRIM(IntToText(i))//' layer '//TRIM(IntToText(k)), &
               f_iFatal, cModName)
          CALL ConfigFile%Kill(); RETURN
        END IF
        IF (k == 1) THEN
          READ(cLine, *, IOSTAT=iErr) iDum, This%rHK(i, k)
        ELSE
          READ(cLine, *, IOSTAT=iErr) This%rHK(i, k)
        END IF
        IF (iErr /= 0) THEN
          CALL DefaultLogger%SetLastMessage('Error parsing HK for node '// &
               TRIM(IntToText(i))//' layer '//TRIM(IntToText(k)), &
               f_iFatal, cModName)
          CALL ConfigFile%Kill(); iStat = -1; RETURN
        END IF
      END DO
    END DO
    CALL ConfigFile%Kill()

    ! ==================================================================
    ! 5. Read observation well file
    !    Format: name  x  y  element  BOS  TOS  overwrite_layer
    ! ==================================================================
    CALL ConfigFile%New(FileName=cObsWellFile, InputFile=.TRUE., &
         IsTSFile=.FALSE., Descriptor='Observation wells file', iStat=iStat)
    IF (iStat == -1) RETURN

    ! Skip header line
    CALL ConfigFile%ReadData(cLine, iStat)
    IF (iStat == -1) THEN; CALL ConfigFile%Kill(); RETURN; END IF

    ! Single-pass read: pre-allocate, grow if needed
    iNObsMax   = 100
    iNObsCount = 0
    ALLOCATE(This%ObsWells(iNObsMax), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot allocate obs well array', f_iFatal, cModName)
      CALL ConfigFile%Kill(); iStat = -1; RETURN
    END IF

    DO
      CALL ConfigFile%ReadData(cLine, iStat)
      IF (iStat == -1) EXIT   ! EOF — normal termination
      iStat = 0

      iNObsCount = iNObsCount + 1

      ! Grow array if needed
      IF (iNObsCount > iNObsMax) THEN
        BLOCK
          TYPE(ObsWellType), ALLOCATABLE :: TempWells(:)
          ALLOCATE(TempWells(iNObsMax))
          TempWells(1:iNObsMax) = This%ObsWells(1:iNObsMax)
          iNObsMax = iNObsMax * 2
          DEALLOCATE(This%ObsWells)
          ALLOCATE(This%ObsWells(iNObsMax))
          This%ObsWells(1:iNObsCount-1) = TempWells(1:iNObsCount-1)
          DEALLOCATE(TempWells)
        END BLOCK
      END IF

      READ(cLine, *, IOSTAT=iErr) This%ObsWells(iNObsCount)%cName, &
           This%ObsWells(iNObsCount)%rX, This%ObsWells(iNObsCount)%rY, &
           This%ObsWells(iNObsCount)%iElem, This%ObsWells(iNObsCount)%rBOS, &
           This%ObsWells(iNObsCount)%rTOS, &
           This%ObsWells(iNObsCount)%iOverwriteLayer
      IF (iErr /= 0) THEN
        CALL DefaultLogger%SetLastMessage('Error reading obs well '// &
             TRIM(IntToText(iNObsCount))// &
             ' from: '//TRIM(cObsWellFile), f_iFatal, cModName)
        CALL ConfigFile%Kill(); iStat = -1; RETURN
      END IF
    END DO
    CALL ConfigFile%Kill()
    iStat = 0

    This%iNObs = iNObsCount
    IF (This%iNObs == 0) THEN
      CALL DefaultLogger%SetLastMessage('No observation wells in: '//TRIM(cObsWellFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Trim array to actual size
    IF (iNObsMax > This%iNObs) THEN
      BLOCK
        TYPE(ObsWellType), ALLOCATABLE :: TempWells(:)
        ALLOCATE(TempWells(This%iNObs))
        TempWells(1:This%iNObs) = This%ObsWells(1:This%iNObs)
        DEALLOCATE(This%ObsWells)
        ALLOCATE(This%ObsWells(This%iNObs))
        This%ObsWells = TempWells
        DEALLOCATE(TempWells)
      END BLOCK
    END IF

    ! ==================================================================
    ! 6. Compute FE interpolation coefficients and T-weights
    ! ==================================================================
    ALLOCATE(rInterpHK(This%iNObs, This%iNLayers), &
             rInterpElev(This%iNObs, This%iNLayers+1), &
             rInterpT(This%iNObs, This%iNLayers), &
             rObsTrans(This%iNObs), &
             This%rWeight(This%iNObs, This%iNLayers), &
             This%rRawT(This%iNObs, This%iNLayers), &
             This%rScreenTOS(This%iNObs), &
             This%rScreenBOS(This%iNObs), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL DefaultLogger%SetLastMessage('Cannot allocate interpolation arrays', &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    rInterpHK   = 0.0D0
    rInterpElev = 0.0D0
    rInterpT    = 0.0D0
    rObsTrans   = 0.0D0

    ! For each observation well, compute FE interpolation coefficients
    ! and interpolate HK and Elevation to well location
    DO i = 1, This%iNObs
      iNVerts = This%Grid%NVertex(This%ObsWells(i)%iElem)
      ALLOCATE(rCoeff(iNVerts), STAT=iErr)
      IF (iErr /= 0) THEN
        CALL DefaultLogger%SetLastMessage('Cannot allocate coefficient array for well '// &
             TRIM(This%ObsWells(i)%cName), f_iFatal, cModName)
        iStat = -1; RETURN
      END IF

      ! Get FE shape function coefficients at well location
      CALL This%Grid%FEInterpolate_AtCell(This%ObsWells(i)%iElem, &
           This%ObsWells(i)%rX, This%ObsWells(i)%rY, rCoeff)

      ! Interpolate HK and Elevation using FE coefficients
      DO k = 1, This%iNLayers
        rInterpHK(i, k) = 0.0D0
        DO iDum = 1, iNVerts
          rInterpHK(i, k) = rInterpHK(i, k) + &
               rCoeff(iDum) * This%rHK(This%Grid%Vertex(iDum, This%ObsWells(i)%iElem), k)
        END DO
      END DO

      DO k = 1, This%iNLayers + 1
        rInterpElev(i, k) = 0.0D0
        DO iDum = 1, iNVerts
          rInterpElev(i, k) = rInterpElev(i, k) + &
               rCoeff(iDum) * This%rElevation(This%Grid%Vertex(iDum, This%ObsWells(i)%iElem), k)
        END DO
      END DO

      DEALLOCATE(rCoeff)
    END DO

    ! Compute transmissivity weights per layer per well
    This%rRawT      = 0.0D0
    This%rScreenTOS = 0.0D0
    This%rScreenBOS = 0.0D0

    DO i = 1, This%iNObs
      IF (This%ObsWells(i)%iOverwriteLayer == -1) THEN
        ! Compute effective screen TOS/BOS clipped to model extent
        This%rScreenTOS(i) = MIN(This%ObsWells(i)%rTOS, rInterpElev(i, 1))
        This%rScreenBOS(i) = MAX(This%ObsWells(i)%rBOS, rInterpElev(i, This%iNLayers+1))

        ! Use screen interval to determine layer intersection
        DO k = 1, This%iNLayers
          rTope = MIN(This%ObsWells(i)%rTOS, rInterpElev(i, k))
          rBote = MAX(This%ObsWells(i)%rBOS, rInterpElev(i, k+1))
          rTempThick = rTope - rBote
          IF (rTempThick > 0.0D0) THEN
            rInterpT(i, k) = rTempThick * rInterpHK(i, k)
          END IF
          rObsTrans(i) = rObsTrans(i) + rInterpT(i, k)
        END DO
        ! If no intersection, assign to bottom layer
        IF (rObsTrans(i) == 0.0D0) THEN
          rInterpT(i, This%iNLayers) = 1.0D0
          rObsTrans(i) = 1.0D0
        END IF
      ELSE
        ! Single layer override
        k = This%ObsWells(i)%iOverwriteLayer
        This%rScreenTOS(i) = rInterpElev(i, k)
        This%rScreenBOS(i) = rInterpElev(i, k+1)
        rTempThick = rInterpElev(i, k) - rInterpElev(i, k+1)
        rInterpT(i, k) = rTempThick * rInterpHK(i, k)
        rObsTrans(i) = rInterpT(i, k)
        IF (rObsTrans(i) == 0.0D0) THEN
          rInterpT(i, k) = 1.0D0
          rObsTrans(i) = 1.0D0
        END IF
      END IF

      ! Store raw transmissivities and normalize weights
      DO k = 1, This%iNLayers
        This%rRawT(i, k) = rInterpT(i, k)
        This%rWeight(i, k) = rInterpT(i, k) / rObsTrans(i)
      END DO
    END DO

    DEALLOCATE(rInterpHK, rInterpElev, rInterpT, rObsTrans)

    This%lActive = .TRUE.
    CALL DefaultLogger%LogMessage(TRIM(IntToText(This%iNObs))//' observation wells processed '// &
         'for multi-layer target', f_iInfo, cModName)

  END SUBROUTINE New

  ! =====================================================================
  ! Kill - Deallocate all arrays
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(MultiLayerTargetType), INTENT(INOUT) :: This

    IF (ALLOCATED(This%rElevation))  DEALLOCATE(This%rElevation)
    IF (ALLOCATED(This%rHK))         DEALLOCATE(This%rHK)
    IF (ALLOCATED(This%rWeight))     DEALLOCATE(This%rWeight)
    IF (ALLOCATED(This%rRawT))       DEALLOCATE(This%rRawT)
    IF (ALLOCATED(This%rScreenTOS))  DEALLOCATE(This%rScreenTOS)
    IF (ALLOCATED(This%rScreenBOS))  DEALLOCATE(This%rScreenBOS)
    IF (ALLOCATED(This%ObsWells))    DEALLOCATE(This%ObsWells)
    IF (ALLOCATED(This%Grid%X))      DEALLOCATE(This%Grid%X)
    IF (ALLOCATED(This%Grid%Y))      DEALLOCATE(This%Grid%Y)
    IF (ALLOCATED(This%Grid%Vertex)) DEALLOCATE(This%Grid%Vertex)
    IF (ALLOCATED(This%Grid%NVertex))DEALLOCATE(This%Grid%NVertex)
    This%iNObs   = 0
    This%iNNodes = 0
    This%iNLayers = 0
    This%iNElems = 0
    This%lActive = .FALSE.

  END SUBROUTINE Kill

  ! =====================================================================
  ! GetNObs - Return number of observation wells
  ! =====================================================================
  FUNCTION GetNObs(This) RESULT(iN)
    CLASS(MultiLayerTargetType), INTENT(IN) :: This
    INTEGER :: iN
    iN = This%iNObs
  END FUNCTION GetNObs

  ! =====================================================================
  ! GetNLayers - Return number of layers
  ! =====================================================================
  FUNCTION GetNLayers(This) RESULT(iN)
    CLASS(MultiLayerTargetType), INTENT(IN) :: This
    INTEGER :: iN
    iN = This%iNLayers
  END FUNCTION GetNLayers

  ! =====================================================================
  ! GetObsName - Return observation well name
  ! =====================================================================
  FUNCTION GetObsName(This, iWell) RESULT(cName)
    CLASS(MultiLayerTargetType), INTENT(IN) :: This
    INTEGER,                     INTENT(IN) :: iWell
    CHARACTER(LEN=25) :: cName
    cName = This%ObsWells(iWell)%cName
  END FUNCTION GetObsName

  ! =====================================================================
  ! GetWellLayerTransmissivities - Return per-layer T and effective screen
  !   Used by ApplyMultiLayerTarget to write GW_MultiLayer.out extended cols
  ! =====================================================================
  SUBROUTINE GetWellLayerTransmissivities(This, iWell, rLayerT, rTOS, rBOS)
    CLASS(MultiLayerTargetType), INTENT(IN)  :: This
    INTEGER,                     INTENT(IN)  :: iWell
    REAL(8),                     INTENT(OUT) :: rLayerT(:)
    REAL(8),                     INTENT(OUT) :: rTOS, rBOS

    INTEGER :: k

    rLayerT = 0.0D0
    DO k = 1, MIN(This%iNLayers, SIZE(rLayerT))
      rLayerT(k) = This%rRawT(iWell, k)
    END DO
    rTOS = This%rScreenTOS(iWell)
    rBOS = This%rScreenBOS(iWell)

  END SUBROUTINE GetWellLayerTransmissivities

  ! =====================================================================
  ! WeightedAverage - Compute T-weighted average from per-layer values
  !   rLayerValues(k) = head value in layer k for this well
  !   Returns weighted average: sum(value(k)*weight(k)) for k=1..nlayers
  ! =====================================================================
  FUNCTION WeightedAverage(This, iWell, rLayerValues) RESULT(rWeighted)
    CLASS(MultiLayerTargetType), INTENT(IN) :: This
    INTEGER,                     INTENT(IN) :: iWell
    REAL(8),                     INTENT(IN) :: rLayerValues(:)
    REAL(8) :: rWeighted

    INTEGER :: k

    rWeighted = 0.0D0
    DO k = 1, This%iNLayers
      rWeighted = rWeighted + rLayerValues(k) * This%rWeight(iWell, k)
    END DO

  END FUNCTION WeightedAverage

END MODULE Class_MultiLayerTarget
