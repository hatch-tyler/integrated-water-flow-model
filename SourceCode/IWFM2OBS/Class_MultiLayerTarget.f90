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

  USE MessageLogger    , ONLY: SetLastMessage , &
                               LogMessage     , &
                               f_iFatal       , &
                               f_iInfo
  USE GeneralUtilities , ONLY: IntToText
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
  END TYPE MultiLayerTargetType

CONTAINS

  ! =====================================================================
  ! ReadNonComment - Read one non-comment line from a Fortran unit
  !   Skips lines starting with C, c, *, #, or blank lines
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

    INTEGER, PARAMETER :: iUnit = 191
    CHARACTER(LEN=1000) :: cLine
    INTEGER :: i, k, iErr, iDum, iNReg
    INTEGER :: iNOUTH, iNOUTF, iNGROUP
    REAL(8) :: rDum
    REAL(8), ALLOCATABLE :: rDumArr(:), rThick(:)
    REAL(8), ALLOCATABLE :: rInterpHK(:,:), rInterpElev(:,:)
    REAL(8), ALLOCATABLE :: rInterpT(:,:), rObsTrans(:)
    REAL(8) :: rTope, rBote, rTempThick
    REAL(8), ALLOCATABLE :: rCoeff(:)
    INTEGER :: iNVerts

    iStat = 0

    ! ==================================================================
    ! 1. Read nodes file
    ! ==================================================================
    OPEN(UNIT=iUnit, FILE=cNodesFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open nodes file: '//TRIM(cNodesFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ExtractFirstInt(cLine, This%iNNodes, iErr)
    IF (iErr /= 0 .OR. This%iNNodes <= 0) THEN
      CALL SetLastMessage('Cannot read node count from: '//TRIM(cNodesFile), &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ALLOCATE(This%Grid%X(This%iNNodes), This%Grid%Y(This%iNNodes), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate node arrays', f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ! Skip factor/header lines (2 non-comment lines), backspace to data
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    DO i = 1, This%iNNodes
      READ(iUnit, *, IOSTAT=iErr) iDum, This%Grid%X(i), This%Grid%Y(i)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading node '//TRIM(IntToText(i))// &
             ' from: '//TRIM(cNodesFile), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO
    CLOSE(iUnit)

    ! ==================================================================
    ! 2. Read elements file
    ! ==================================================================
    OPEN(UNIT=iUnit, FILE=cElemsFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open elements file: '//TRIM(cElemsFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Scan for the line containing the number of elements
    ! IWFM elements file: scan until we find a line with 'N' or 'E'
    ! (matching original MultiLayerTarget behavior)
    DO
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of elements file: '// &
             TRIM(cElemsFile), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      IF (SCAN(cLine, 'NEne') > 0) EXIT
    END DO

    CALL ExtractFirstInt(cLine, This%iNElems, iErr)
    IF (iErr /= 0 .OR. This%iNElems <= 0) THEN
      CALL SetLastMessage('Cannot read element count from: '// &
           TRIM(cElemsFile), f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ALLOCATE(This%Grid%Vertex(4, This%iNElems), &
             This%Grid%NVertex(This%iNElems), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate element arrays', f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF
    This%Grid%Vertex  = 0
    This%Grid%NVertex = 4

    ! Read number of subregions
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ExtractFirstInt(cLine, iNReg, iErr)

    ! Skip subregion definitions: 1 header + (nreg-1) entries + 1 more
    CALL ReadNonComment(iUnit, cLine, iErr)
    DO i = 1, iNReg - 1
      CALL ReadNonComment(iUnit, cLine, iErr)
    END DO
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    ! Read element connectivity
    DO i = 1, This%iNElems
      READ(iUnit, *, IOSTAT=iErr) iDum, &
           This%Grid%Vertex(1,iDum), This%Grid%Vertex(2,iDum), &
           This%Grid%Vertex(3,iDum), This%Grid%Vertex(4,iDum)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading element '//TRIM(IntToText(i))// &
             ' from: '//TRIM(cElemsFile), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      ! Determine number of active vertices (0 = triangle)
      IF (This%Grid%Vertex(4, iDum) <= 0) THEN
        This%Grid%NVertex(iDum) = 3
      END IF
    END DO
    CLOSE(iUnit)

    ! ==================================================================
    ! 3. Read stratigraphy file
    ! ==================================================================
    OPEN(UNIT=iUnit, FILE=cStratFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open stratigraphy file: '//TRIM(cStratFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ExtractFirstInt(cLine, This%iNLayers, iErr)
    IF (iErr /= 0 .OR. This%iNLayers <= 0) THEN
      CALL SetLastMessage('Cannot read layer count from: '//TRIM(cStratFile), &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ALLOCATE(This%rElevation(This%iNNodes, This%iNLayers+1), &
             This%rHK(This%iNNodes, This%iNLayers), &
             rDumArr(This%iNLayers), &
             rThick(This%iNLayers), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate stratigraphy arrays', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ! Skip 2 header lines, backspace to data
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    ! Read stratigraphy: node_id, top_elev, (aquitard_thick, aquifer_thick) x nlayers
    DO i = 1, This%iNNodes
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading stratigraphy for node '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      READ(cLine, *, IOSTAT=iErr) iDum, This%rElevation(i,1), &
           ((rDumArr(k), rThick(k)), k=1, This%iNLayers)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error parsing stratigraphy for node '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
      ! Compute layer bottom elevations
      DO k = 1, This%iNLayers
        This%rElevation(i, k+1) = This%rElevation(i, k) - rThick(k)
      END DO
    END DO
    CLOSE(iUnit)
    DEALLOCATE(rDumArr, rThick)

    ! ==================================================================
    ! 4. Read hydraulic conductivity from GW main file
    !    NOTE: Skip counts are IWFM version-specific
    ! ==================================================================
    OPEN(UNIT=iUnit, FILE=cGWFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open GW file: '//TRIM(cGWFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Skip 21 non-comment lines; line 21 contains NOUTH
    DO i = 1, 21
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of GW file at line '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO
    CALL ExtractFirstInt(cLine, iNOUTH, iErr)

    ! Skip FACTXY + GWHYDOUTFL + header
    CALL ReadNonComment(iUnit, cLine, iErr)  ! FACTXY
    CALL ReadNonComment(iUnit, cLine, iErr)  ! GWHYDOUTFL
    CALL ReadNonComment(iUnit, cLine, iErr)  ! header
    BACKSPACE(iUnit)

    ! Skip NOUTH observation entries
    DO i = 1, iNOUTH
      CALL ReadNonComment(iUnit, cLine, iErr)
    END DO

    ! Read NOUTF
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ExtractFirstInt(cLine, iNOUTF, iErr)

    ! Skip FCHYDOUTFL + NOUTF flow entries
    CALL ReadNonComment(iUnit, cLine, iErr)
    DO i = 1, iNOUTF
      CALL ReadNonComment(iUnit, cLine, iErr)
    END DO

    ! Read NGROUP
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ExtractFirstInt(cLine, iNGROUP, iErr)
    IF (iNGROUP > 0) THEN
      CALL SetLastMessage('Parametric grid (NGROUP>0) not supported', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ! Skip FX + 3 more lines (time units etc.)
    DO i = 1, 4
      CALL ReadNonComment(iUnit, cLine, iErr)
    END DO

    ! Read HK: for each node, for each layer, one non-comment line
    ! Layer 1: "node_id HK_value"; Layers 2+: "HK_value"
    This%rHK = 0.0D0
    DO i = 1, This%iNNodes
      DO k = 1, This%iNLayers
        CALL ReadNonComment(iUnit, cLine, iErr)
        IF (iErr /= 0) THEN
          CALL SetLastMessage('Error reading HK for node '// &
               TRIM(IntToText(i))//' layer '//TRIM(IntToText(k)), &
               f_iFatal, cModName)
          CLOSE(iUnit); iStat = -1; RETURN
        END IF
        IF (k == 1) THEN
          READ(cLine, *, IOSTAT=iErr) iDum, This%rHK(i, k)
        ELSE
          READ(cLine, *, IOSTAT=iErr) This%rHK(i, k)
        END IF
        IF (iErr /= 0) THEN
          CALL SetLastMessage('Error parsing HK for node '// &
               TRIM(IntToText(i))//' layer '//TRIM(IntToText(k)), &
               f_iFatal, cModName)
          CLOSE(iUnit); iStat = -1; RETURN
        END IF
      END DO
    END DO
    CLOSE(iUnit)

    ! ==================================================================
    ! 5. Read observation well file
    !    Format: name  x  y  element  BOS  TOS  overwrite_layer
    ! ==================================================================
    OPEN(UNIT=iUnit, FILE=cObsWellFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open obs well file: '//TRIM(cObsWellFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Skip header line, count data lines
    READ(iUnit, *, IOSTAT=iErr)
    This%iNObs = 0
    DO
      READ(iUnit, *, IOSTAT=iErr)
      IF (iErr /= 0) EXIT
      This%iNObs = This%iNObs + 1
    END DO

    IF (This%iNObs == 0) THEN
      CALL SetLastMessage('No observation wells in: '//TRIM(cObsWellFile), &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ALLOCATE(This%ObsWells(This%iNObs), STAT=iErr)
    REWIND(iUnit)
    READ(iUnit, *, IOSTAT=iErr)  ! Skip header again

    DO i = 1, This%iNObs
      READ(iUnit, *, IOSTAT=iErr) This%ObsWells(i)%cName, &
           This%ObsWells(i)%rX, This%ObsWells(i)%rY, &
           This%ObsWells(i)%iElem, This%ObsWells(i)%rBOS, &
           This%ObsWells(i)%rTOS, This%ObsWells(i)%iOverwriteLayer
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Error reading obs well '//TRIM(IntToText(i))// &
             ' from: '//TRIM(cObsWellFile), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO
    CLOSE(iUnit)

    ! ==================================================================
    ! 6. Compute FE interpolation coefficients and T-weights
    ! ==================================================================
    ALLOCATE(rInterpHK(This%iNObs, This%iNLayers), &
             rInterpElev(This%iNObs, This%iNLayers+1), &
             rInterpT(This%iNObs, This%iNLayers), &
             rObsTrans(This%iNObs), &
             This%rWeight(This%iNObs, This%iNLayers), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate interpolation arrays', &
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
        CALL SetLastMessage('Cannot allocate coefficient array for well '// &
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
    DO i = 1, This%iNObs
      IF (This%ObsWells(i)%iOverwriteLayer == -1) THEN
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
        rTempThick = rInterpElev(i, k) - rInterpElev(i, k+1)
        rInterpT(i, k) = rTempThick * rInterpHK(i, k)
        rObsTrans(i) = rInterpT(i, k)
        IF (rObsTrans(i) == 0.0D0) THEN
          rInterpT(i, k) = 1.0D0
          rObsTrans(i) = 1.0D0
        END IF
      END IF

      ! Normalize weights
      DO k = 1, This%iNLayers
        This%rWeight(i, k) = rInterpT(i, k) / rObsTrans(i)
      END DO
    END DO

    DEALLOCATE(rInterpHK, rInterpElev, rInterpT, rObsTrans)

    This%lActive = .TRUE.
    CALL LogMessage(TRIM(IntToText(This%iNObs))//' observation wells processed '// &
         'for multi-layer target', f_iInfo, cModName)

  END SUBROUTINE New

  ! =====================================================================
  ! Kill - Deallocate all arrays
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(MultiLayerTargetType), INTENT(INOUT) :: This

    IF (ALLOCATED(This%rElevation)) DEALLOCATE(This%rElevation)
    IF (ALLOCATED(This%rHK))        DEALLOCATE(This%rHK)
    IF (ALLOCATED(This%rWeight))    DEALLOCATE(This%rWeight)
    IF (ALLOCATED(This%ObsWells))   DEALLOCATE(This%ObsWells)
    IF (ALLOCATED(This%Grid%X))     DEALLOCATE(This%Grid%X)
    IF (ALLOCATED(This%Grid%Y))     DEALLOCATE(This%Grid%Y)
    IF (ALLOCATED(This%Grid%Vertex))  DEALLOCATE(This%Grid%Vertex)
    IF (ALLOCATED(This%Grid%NVertex)) DEALLOCATE(This%Grid%NVertex)
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
