!***********************************************************************
!  IWFM2OBS - Class_HydrographReader
!  Model file discovery (simulation main → component files → .out paths)
!  and hydrograph .out file reading with conversion to temp SMP files.
!
!  Ports the model-file-parsing functionality from the old iwfm2obs
!  (iwfm2obs_2015) into a reusable, modern Fortran module that
!  integrates with the new IWFM2OBS architecture.
!***********************************************************************
MODULE Class_HydrographReader

  USE MessageLogger      , ONLY: SetLastMessage   , &
                                 LogMessage        , &
                                 f_iFatal          , &
                                 f_iInfo
  USE GeneralUtilities   , ONLY: IntToText         , &
                                 UpperCase         , &
                                 EstablishAbsolutePathFileName
  USE TimeSeriesUtilities, ONLY: DayMonthYearToJulianDate , &
                                 JulianDateAndMinutesToJulianDate

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: HydrographReaderType

  CHARACTER(LEN=30), PARAMETER :: cModName = 'Class_HydrographReader'

  ! Hydrograph type indices (must match Class_IWFM2OBS)
  INTEGER, PARAMETER, PUBLIC :: iHR_SUBSID = 1
  INTEGER, PARAMETER, PUBLIC :: iHR_TILEDR = 2
  INTEGER, PARAMETER, PUBLIC :: iHR_STREAM = 3
  INTEGER, PARAMETER, PUBLIC :: iHR_GWHEAD = 4
  INTEGER, PARAMETER, PUBLIC :: iHR_NUMHYD = 4

  ! =====================================================================
  ! HydFileInfoType - Information about one hydrograph output
  ! =====================================================================
  TYPE :: HydFileInfoType
    INTEGER              :: iNHyd   = 0         ! number of hydrographs
    CHARACTER(LEN=500)   :: cOutFilePath = ' '   ! .out file path
    CHARACTER(LEN=25), ALLOCATABLE :: cHydIDs(:) ! bore IDs from component main
    INTEGER, ALLOCATABLE :: iLayers(:)           ! IOUTHL layer numbers (GW/subsid only)
    LOGICAL              :: lActive = .FALSE.
  END TYPE HydFileInfoType

  ! =====================================================================
  ! HydrographReaderType - Model discovery and .out reader
  ! =====================================================================
  TYPE :: HydrographReaderType
    CHARACTER(LEN=500)  :: cSimMainFile = ' '
    CHARACTER(LEN=500)  :: cWorkDir     = ' '
    CHARACTER(LEN=500)  :: cGWMainFile  = ' '
    CHARACTER(LEN=500)  :: cStreamMainFile = ' '
    CHARACTER(LEN=500)  :: cTileDrainFile = ' '
    CHARACTER(LEN=500)  :: cSubsidenceFile = ' '
    TYPE(HydFileInfoType) :: HydInfo(iHR_NUMHYD)
    ! Simulation time info
    CHARACTER(LEN=10)   :: cTimeUnit  = ' '
    INTEGER             :: iStartDay  = 0
    INTEGER             :: iStartMon  = 0
    INTEGER             :: iStartYr   = 0
    INTEGER             :: iDateSpec  = 2  ! 1=dd/mm, 2=mm/dd
    LOGICAL             :: lDiscovered = .FALSE.
  CONTAINS
    PROCEDURE, PASS :: DiscoverModelFiles
    PROCEDURE, PASS :: ReadDotOutFile
    PROCEDURE, PASS :: Kill
  END TYPE HydrographReaderType

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
  ! StripInlineComment - Remove text after '/' delimiter
  ! =====================================================================
  SUBROUTINE StripInlineComment(cLine, cResult)
    CHARACTER(LEN=*), INTENT(IN)  :: cLine
    CHARACTER(LEN=*), INTENT(OUT) :: cResult
    INTEGER :: iPos

    cResult = cLine
    iPos = SCAN(cResult, '/')
    IF (iPos > 1) THEN
      cResult = cResult(1:iPos-1)
    ELSE IF (iPos == 1) THEN
      cResult = ' '
    END IF
    cResult = ADJUSTL(TRIM(cResult))
  END SUBROUTINE StripInlineComment

  ! =====================================================================
  ! ExtractFilePath - Read non-comment line, strip inline comment, resolve
  ! =====================================================================
  SUBROUTINE ExtractFilePath(iUnit, cBaseDir, cPath, iStat)
    INTEGER,          INTENT(IN)  :: iUnit
    CHARACTER(LEN=*), INTENT(IN)  :: cBaseDir
    CHARACTER(LEN=*), INTENT(OUT) :: cPath
    INTEGER,          INTENT(OUT) :: iStat

    CHARACTER(LEN=1000) :: cLine, cClean

    CALL ReadNonComment(iUnit, cLine, iStat)
    IF (iStat /= 0) RETURN
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) == 0) THEN
      cPath = ' '
      RETURN
    END IF
    CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cBaseDir), cPath)
  END SUBROUTINE ExtractFilePath

  ! =====================================================================
  ! DiscoverModelFiles - Parse simulation main file → component paths
  !
  !   Reads the IWFM simulation main file to extract:
  !     - GW main file path, Stream main file path
  !     - Start date, time unit
  !   Then parses each component main file to find:
  !     - .out hydrograph file paths
  !     - hydrograph IDs (bore names)
  !     - layer numbers (IOUTHL for GW/subsidence)
  !
  !   Adapted from old iwfm2obs.f90 lines 127-502
  ! =====================================================================
  SUBROUTINE DiscoverModelFiles(This, cSimMainFile, cWorkDir, iDateSpec, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    CHARACTER(LEN=*),           INTENT(IN)    :: cSimMainFile
    CHARACTER(LEN=*),           INTENT(IN)    :: cWorkDir
    INTEGER,                    INTENT(IN)    :: iDateSpec
    INTEGER,                    INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 196
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500)  :: cSimDir, cPath
    INTEGER :: iErr, i, iPos
    INTEGER :: iNOUTH, iNOUTF, iNOUTR, iNSI, iNOUTS, iNTD
    INTEGER :: iHydType, j, iColID
    CHARACTER(LEN=25) :: cID
    INTEGER :: iNumTokens
    CHARACTER(LEN=30) :: cTokens(10)

    iStat = 0
    This%cSimMainFile = cSimMainFile
    This%cWorkDir     = cWorkDir
    This%iDateSpec    = iDateSpec

    ! Determine directory of simulation main file
    cSimDir = cSimMainFile
    iPos = MAX(SCAN(cSimDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cSimDir = cSimDir(1:iPos)
    ELSE
      cSimDir = cWorkDir
    END IF

    ! ==================================================================
    ! 1. Parse simulation main file
    ! ==================================================================
    OPEN(UNIT=iUnit, FILE=cSimMainFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open simulation main file: '// &
           TRIM(cSimMainFile), f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Skip 4 lines to reach GW main file path (line 5)
    DO i = 1, 4
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of simulation main file', &
             f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO

    ! Line 5: GW main file
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cSimDir), This%cGWMainFile)

    ! Line 6: Stream main file
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cSimDir), This%cStreamMainFile)

    ! Skip to start date (8 lines to skip, then read start date)
    DO i = 1, 8
      CALL ReadNonComment(iUnit, cLine, iErr)
    END DO

    ! Read start date
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    ! Extract date portion before the '_' separator
    iPos = SCAN(cClean, '_')
    IF (iPos > 0) cClean = cClean(1:iPos-1)
    ! Parse date: MM/DD/YYYY or DD/MM/YYYY depending on datespec
    CALL ParseDateFromString(cClean, iDateSpec, This%iStartDay, This%iStartMon, &
         This%iStartYr, iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot parse start date from simulation file', &
           f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ! Skip 1 line, then read time unit
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    cClean = ADJUSTL(TRIM(cClean))
    CALL UpperCaseLocal(cClean)
    IF (cClean(1:4) == '1DAY') THEN
      This%cTimeUnit = '1DAY'
    ELSE IF (cClean(1:5) == '1WEEK') THEN
      This%cTimeUnit = '1WEEK'
    ELSE IF (cClean(1:4) == '1MON') THEN
      This%cTimeUnit = '1MON'
    ELSE IF (cClean(1:5) == '1YEAR') THEN
      This%cTimeUnit = '1YEAR'
    ELSE
      This%cTimeUnit = TRIM(cClean)
    END IF

    CLOSE(iUnit)

    CALL LogMessage('  Simulation file: '//TRIM(cSimMainFile), f_iInfo, cModName)
    CALL LogMessage('  GW main file: '//TRIM(This%cGWMainFile), f_iInfo, cModName)
    CALL LogMessage('  Stream main file: '//TRIM(This%cStreamMainFile), f_iInfo, cModName)

    ! ==================================================================
    ! 2. Parse GW main file
    ! ==================================================================
    CALL ParseGWMainFile(This, iStat)
    IF (iStat /= 0) RETURN

    ! ==================================================================
    ! 3. Parse Stream main file
    ! ==================================================================
    CALL ParseStreamMainFile(This, iStat)
    IF (iStat /= 0) RETURN

    ! ==================================================================
    ! 4. Parse Tile Drain main file (if found from GW main)
    ! ==================================================================
    IF (LEN_TRIM(This%cTileDrainFile) > 0) THEN
      CALL ParseTileDrainMainFile(This, iStat)
      IF (iStat /= 0) iStat = 0  ! Non-fatal: continue without tile drain
    END IF

    ! ==================================================================
    ! 5. Parse Subsidence main file (if found from GW main)
    ! ==================================================================
    IF (LEN_TRIM(This%cSubsidenceFile) > 0) THEN
      CALL ParseSubsidenceMainFile(This, iStat)
      IF (iStat /= 0) iStat = 0  ! Non-fatal: continue without subsidence
    END IF

    This%lDiscovered = .TRUE.

    ! Report summary
    DO i = 1, iHR_NUMHYD
      IF (This%HydInfo(i)%lActive) THEN
        CALL LogMessage('  Discovered '//TRIM(IntToText(This%HydInfo(i)%iNHyd))// &
             ' hydrographs from .out file: '//TRIM(This%HydInfo(i)%cOutFilePath), &
             f_iInfo, cModName)
      END IF
    END DO

  END SUBROUTINE DiscoverModelFiles

  ! =====================================================================
  ! ParseGWMainFile - Extract GW hydrograph info from GW main file
  ! =====================================================================
  SUBROUTINE ParseGWMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 197
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500)  :: cGWDir, cPath
    INTEGER :: iErr, i, iPos, iNOUTH
    INTEGER :: iID, iHydTyp, iOutHL
    REAL(8) :: rX, rY
    CHARACTER(LEN=25) :: cName

    iStat = 0

    ! Determine GW file directory
    cGWDir = This%cGWMainFile
    iPos = MAX(SCAN(cGWDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cGWDir = cGWDir(1:iPos)
    ELSE
      cGWDir = This%cWorkDir
    END IF

    OPEN(UNIT=iUnit, FILE=This%cGWMainFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open GW main file: '// &
           TRIM(This%cGWMainFile), f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Line 1: Version/debug flag (skip)
    CALL ReadNonComment(iUnit, cLine, iErr)

    ! Line 2: Tile drain main file path
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cGWDir), This%cTileDrainFile)
    END IF

    ! Line 3: Pumping file (skip)
    CALL ReadNonComment(iUnit, cLine, iErr)

    ! Line 4: Subsidence file path
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cGWDir), This%cSubsidenceFile)
    END IF

    ! Skip 16 more non-comment lines (lines 5-20 in GW main), then line 21 = NOUTH
    DO i = 1, 17
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CALL SetLastMessage('Unexpected end of GW main file at skip line '// &
             TRIM(IntToText(i)), f_iFatal, cModName)
        CLOSE(iUnit); iStat = -1; RETURN
      END IF
    END DO

    ! Current line is NOUTH
    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNOUTH
    IF (iErr /= 0 .OR. iNOUTH < 0) THEN
      CALL SetLastMessage('Cannot read NOUTH from GW main file', f_iFatal, cModName)
      CLOSE(iUnit); iStat = -1; RETURN
    END IF

    ! FACTXY
    CALL ReadNonComment(iUnit, cLine, iErr)

    ! GWHYDOUTFL (hydrograph output file path)
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0 .AND. iNOUTH > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cGWDir), cPath)
      This%HydInfo(iHR_GWHEAD)%cOutFilePath = cPath
      This%HydInfo(iHR_GWHEAD)%iNHyd = iNOUTH
      This%HydInfo(iHR_GWHEAD)%lActive = .TRUE.
    END IF

    ! Skip header line, then read NOUTH hydrograph entries
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    IF (iNOUTH > 0) THEN
      ALLOCATE(This%HydInfo(iHR_GWHEAD)%cHydIDs(iNOUTH), &
               This%HydInfo(iHR_GWHEAD)%iLayers(iNOUTH))

      DO i = 1, iNOUTH
        CALL ReadNonComment(iUnit, cLine, iErr)
        IF (iErr /= 0) EXIT
        ! Parse: ID HYDTYP IOUTHL X Y NAME  or  ID HYDTYP IOUTHL IOUTH NAME
        READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL
        IF (iErr /= 0) THEN
          iOutHL = 1
          iHydTyp = 0
        END IF

        ! Extract the name (last token on the line)
        cName = ' '
        IF (iHydTyp == 0) THEN
          ! Format: ID HYDTYP IOUTHL X Y NAME
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, rX, rY, cName
        ELSE
          ! Format: ID HYDTYP IOUTHL IOUTH NAME
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, iID, cName
        END IF
        IF (iErr /= 0) THEN
          WRITE(cName, '(A,I0)') 'HYD', i
        END IF

        This%HydInfo(iHR_GWHEAD)%cHydIDs(i) = ADJUSTL(TRIM(cName))
        This%HydInfo(iHR_GWHEAD)%iLayers(i) = iOutHL
      END DO
    END IF

    CLOSE(iUnit)

    CALL LogMessage('  GW: '//TRIM(IntToText(iNOUTH))//' hydrographs, .out='// &
         TRIM(This%HydInfo(iHR_GWHEAD)%cOutFilePath), f_iInfo, cModName)

  END SUBROUTINE ParseGWMainFile

  ! =====================================================================
  ! ParseStreamMainFile - Extract stream hydrograph info
  ! =====================================================================
  SUBROUTINE ParseStreamMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 197
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500) :: cStrDir, cPath
    INTEGER :: iErr, i, iPos, iNOUTR
    CHARACTER(LEN=25) :: cName
    INTEGER :: iID

    iStat = 0

    IF (LEN_TRIM(This%cStreamMainFile) == 0) RETURN

    cStrDir = This%cStreamMainFile
    iPos = MAX(SCAN(cStrDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cStrDir = cStrDir(1:iPos)
    ELSE
      cStrDir = This%cWorkDir
    END IF

    OPEN(UNIT=iUnit, FILE=This%cStreamMainFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL LogMessage('  Stream main file not found: '// &
           TRIM(This%cStreamMainFile), f_iInfo, cModName)
      RETURN
    END IF

    ! Skip to NOUTR (7 non-comment lines)
    DO i = 1, 7
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CLOSE(iUnit); RETURN
      END IF
    END DO

    ! Current line has NOUTR
    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNOUTR
    IF (iErr /= 0 .OR. iNOUTR <= 0) THEN
      CLOSE(iUnit); RETURN
    END IF

    ! Skip 5 lines, then read output file name
    DO i = 1, 6
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CLOSE(iUnit); RETURN
      END IF
    END DO

    ! Current line is the stream hydrograph output file path
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cStrDir), cPath)
      This%HydInfo(iHR_STREAM)%cOutFilePath = cPath
      This%HydInfo(iHR_STREAM)%iNHyd = iNOUTR
      This%HydInfo(iHR_STREAM)%lActive = .TRUE.
    END IF

    ! Skip to hydrograph location data, read names
    ! Skip to 'next section header' then read entries
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    IF (iNOUTR > 0) THEN
      ALLOCATE(This%HydInfo(iHR_STREAM)%cHydIDs(iNOUTR))

      DO i = 1, iNOUTR
        CALL ReadNonComment(iUnit, cLine, iErr)
        IF (iErr /= 0) EXIT
        ! Stream format: ID NAME NODE or ID HYDTYP NAME
        ! Extract name (second token for streams)
        cName = ' '
        READ(cLine, *, IOSTAT=iErr) iID, cName
        IF (iErr /= 0) THEN
          WRITE(cName, '(A,I0)') 'STR', i
        END IF
        This%HydInfo(iHR_STREAM)%cHydIDs(i) = ADJUSTL(TRIM(cName))
      END DO
    END IF

    CLOSE(iUnit)

  END SUBROUTINE ParseStreamMainFile

  ! =====================================================================
  ! ParseTileDrainMainFile - Extract tile drain hydrograph info
  ! =====================================================================
  SUBROUTINE ParseTileDrainMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 197
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500) :: cTDDir, cPath
    INTEGER :: iErr, i, iPos, iNTD, iNSI
    CHARACTER(LEN=25) :: cName
    INTEGER :: iID

    iStat = 0

    cTDDir = This%cTileDrainFile
    iPos = MAX(SCAN(cTDDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cTDDir = cTDDir(1:iPos)
    ELSE
      cTDDir = This%cWorkDir
    END IF

    OPEN(UNIT=iUnit, FILE=This%cTileDrainFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) RETURN

    ! First non-comment line has NTD
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNTD
    IF (iErr /= 0 .OR. iNTD <= 0) THEN
      CLOSE(iUnit); RETURN
    END IF

    ! Skip 3+NTD lines, then read NSI
    DO i = 1, iNTD + 4
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CLOSE(iUnit); RETURN
      END IF
    END DO

    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNSI
    IF (iErr /= 0) THEN
      CLOSE(iUnit); RETURN
    END IF

    ! Skip 6+NSI lines to get hydrograph file name
    DO i = 1, iNSI + 7
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CLOSE(iUnit); RETURN
      END IF
    END DO

    ! Current line is tile drain hydrograph output file path
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cTDDir), cPath)
      This%HydInfo(iHR_TILEDR)%cOutFilePath = cPath
      This%HydInfo(iHR_TILEDR)%iNHyd = iNTD
      This%HydInfo(iHR_TILEDR)%lActive = .TRUE.
    END IF

    ! Read hydrograph IDs
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    IF (iNTD > 0 .AND. This%HydInfo(iHR_TILEDR)%lActive) THEN
      ALLOCATE(This%HydInfo(iHR_TILEDR)%cHydIDs(iNTD))
      DO i = 1, iNTD
        CALL ReadNonComment(iUnit, cLine, iErr)
        IF (iErr /= 0) EXIT
        cName = ' '
        READ(cLine, *, IOSTAT=iErr) iID, cName
        IF (iErr /= 0) WRITE(cName, '(A,I0)') 'TD', i
        This%HydInfo(iHR_TILEDR)%cHydIDs(i) = ADJUSTL(TRIM(cName))
      END DO
    END IF

    CLOSE(iUnit)

  END SUBROUTINE ParseTileDrainMainFile

  ! =====================================================================
  ! ParseSubsidenceMainFile - Extract subsidence hydrograph info
  ! =====================================================================
  SUBROUTINE ParseSubsidenceMainFile(This, iStat)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER,                     INTENT(OUT)   :: iStat

    INTEGER, PARAMETER :: iUnit = 197
    CHARACTER(LEN=1000) :: cLine, cClean
    CHARACTER(LEN=500) :: cSBDir, cPath
    INTEGER :: iErr, i, iPos, iNOUTS
    INTEGER :: iID, iHydTyp, iOutHL
    REAL(8) :: rX, rY
    CHARACTER(LEN=25) :: cName

    iStat = 0

    cSBDir = This%cSubsidenceFile
    iPos = MAX(SCAN(cSBDir, '/\', BACK=.TRUE.), 0)
    IF (iPos > 0) THEN
      cSBDir = cSBDir(1:iPos)
    ELSE
      cSBDir = This%cWorkDir
    END IF

    OPEN(UNIT=iUnit, FILE=This%cSubsidenceFile, STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) RETURN

    ! Skip 5 lines, then read NOUTS
    DO i = 1, 6
      CALL ReadNonComment(iUnit, cLine, iErr)
      IF (iErr /= 0) THEN
        CLOSE(iUnit); RETURN
      END IF
    END DO

    CALL StripInlineComment(cLine, cClean)
    READ(cClean, *, IOSTAT=iErr) iNOUTS
    IF (iErr /= 0 .OR. iNOUTS <= 0) THEN
      CLOSE(iUnit); RETURN
    END IF

    ! FACTXY
    CALL ReadNonComment(iUnit, cLine, iErr)

    ! Skip 1 line, then read subsidence hydrograph output file path
    CALL ReadNonComment(iUnit, cLine, iErr)
    CALL StripInlineComment(cLine, cClean)
    IF (LEN_TRIM(cClean) > 0) THEN
      CALL EstablishAbsolutePathFileName(TRIM(cClean), TRIM(cSBDir), cPath)
      This%HydInfo(iHR_SUBSID)%cOutFilePath = cPath
      This%HydInfo(iHR_SUBSID)%iNHyd = iNOUTS
      This%HydInfo(iHR_SUBSID)%lActive = .TRUE.
    END IF

    ! Read hydrograph IDs (same format as GW: ID HYDTYP IOUTHL ...)
    CALL ReadNonComment(iUnit, cLine, iErr)
    BACKSPACE(iUnit)

    IF (iNOUTS > 0 .AND. This%HydInfo(iHR_SUBSID)%lActive) THEN
      ALLOCATE(This%HydInfo(iHR_SUBSID)%cHydIDs(iNOUTS), &
               This%HydInfo(iHR_SUBSID)%iLayers(iNOUTS))

      DO i = 1, iNOUTS
        CALL ReadNonComment(iUnit, cLine, iErr)
        IF (iErr /= 0) EXIT
        READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL
        IF (iErr /= 0) THEN
          iOutHL = 1
          iHydTyp = 0
        END IF
        cName = ' '
        IF (iHydTyp == 0) THEN
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, rX, rY, cName
        ELSE
          READ(cLine, *, IOSTAT=iErr) iID, iHydTyp, iOutHL, iID, cName
        END IF
        IF (iErr /= 0) WRITE(cName, '(A,I0)') 'SUB', i
        This%HydInfo(iHR_SUBSID)%cHydIDs(i) = ADJUSTL(TRIM(cName))
        This%HydInfo(iHR_SUBSID)%iLayers(i) = iOutHL
      END DO
    END IF

    CLOSE(iUnit)

  END SUBROUTINE ParseSubsidenceMainFile

  ! =====================================================================
  ! ReadDotOutFile - Read hydrograph .out file and write temp SMP
  !
  !   Reads IWFM hydrograph output in the appropriate format:
  !     GW:         READ(iUnit, '(A22,60000F12.4)') ...
  !     Stream:     READ(iUnit, '(A22,60000F14.2)') ...
  !     Subsid/TD:  READ(iUnit, '(A22,60000F12.2)') ...
  !   Skips *-prefixed header rows.
  !   Writes output as temp SMP file for SMP2SMP consumption.
  !
  !   Adapted from old iwfm2obs.f90 lines 510-588
  ! =====================================================================
  SUBROUTINE ReadDotOutFile(This, iHydType, cTempSMPFile, iStat)
    CLASS(HydrographReaderType), INTENT(IN)  :: This
    INTEGER,                     INTENT(IN)  :: iHydType
    CHARACTER(LEN=*),            INTENT(IN)  :: cTempSMPFile
    INTEGER,                     INTENT(OUT) :: iStat

    INTEGER, PARAMETER :: iInUnit = 198, iOutUnit = 199
    INTEGER, PARAMETER :: MAXHYD = 60000
    CHARACTER(LEN=120) :: cJunk
    REAL(4), ALLOCATABLE :: rVal(:)
    INTEGER :: iErr, iNHyd, iTime, j, k
    INTEGER :: iDay, iMon, iYr
    INTEGER :: iMonDays(12)
    DATA iMonDays /31,28,31,30,31,30,31,31,30,31,30,31/
    CHARACTER(LEN=1000) :: cLine

    iStat = 0

    IF (.NOT. This%HydInfo(iHydType)%lActive) RETURN
    IF (This%HydInfo(iHydType)%iNHyd <= 0) RETURN

    iNHyd = This%HydInfo(iHydType)%iNHyd
    ALLOCATE(rVal(iNHyd), STAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot allocate array for '//TRIM(IntToText(iNHyd))// &
           ' hydrographs', f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Open the .out file
    OPEN(UNIT=iInUnit, FILE=This%HydInfo(iHydType)%cOutFilePath, &
         STATUS='OLD', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CALL SetLastMessage('Cannot open .out file: '// &
           TRIM(This%HydInfo(iHydType)%cOutFilePath), f_iFatal, cModName)
      DEALLOCATE(rVal)
      iStat = -1; RETURN
    END IF

    ! Skip *-prefixed header lines
    DO
      READ(iInUnit, '(A)', IOSTAT=iErr) cLine
      IF (iErr /= 0) THEN
        CLOSE(iInUnit); DEALLOCATE(rVal); iStat = -1; RETURN
      END IF
      cLine = ADJUSTL(cLine)
      IF (cLine(1:1) /= '*') THEN
        BACKSPACE(iInUnit)
        EXIT
      END IF
    END DO

    ! Open temp SMP output
    OPEN(UNIT=iOutUnit, FILE=cTempSMPFile, STATUS='REPLACE', IOSTAT=iErr)
    IF (iErr /= 0) THEN
      CLOSE(iInUnit); DEALLOCATE(rVal)
      CALL SetLastMessage('Cannot create temp SMP: '//TRIM(cTempSMPFile), &
           f_iFatal, cModName)
      iStat = -1; RETURN
    END IF

    ! Read data lines and compute dates
    iTime = 0
    DO
      iTime = iTime + 1
      rVal = 0.0

      ! Read with format appropriate to hydrograph type
      SELECT CASE (iHydType)
      CASE (iHR_GWHEAD)
        READ(iInUnit, '(A22,60000F12.4)', IOSTAT=iErr) cJunk, (rVal(j), j=1,iNHyd)
      CASE (iHR_STREAM)
        READ(iInUnit, '(A22,60000F14.2)', IOSTAT=iErr) cJunk, (rVal(j), j=1,iNHyd)
      CASE DEFAULT  ! SUBSID, TILEDR
        READ(iInUnit, '(A22,60000F12.2)', IOSTAT=iErr) cJunk, (rVal(j), j=1,iNHyd)
      END SELECT

      IF (iErr /= 0) EXIT

      ! Compute date from time step index and start date + time unit
      CALL ComputeDate(This%cTimeUnit, iTime, This%iStartDay, This%iStartMon, &
           This%iStartYr, iDay, iMon, iYr)

      ! Write SMP records for each hydrograph
      DO j = 1, iNHyd
        IF (ALLOCATED(This%HydInfo(iHydType)%cHydIDs)) THEN
          IF (This%iDateSpec == 1) THEN
            WRITE(iOutUnit, 850) TRIM(This%HydInfo(iHydType)%cHydIDs(j)), &
                 iDay, iMon, iYr, DBLE(rVal(j))
          ELSE
            WRITE(iOutUnit, 850) TRIM(This%HydInfo(iHydType)%cHydIDs(j)), &
                 iMon, iDay, iYr, DBLE(rVal(j))
          END IF
        END IF
      END DO
    END DO

850 FORMAT(1X,A,10X,I2.2,'/',I2.2,'/',I4.4,'   00:00:00 ',1PG14.7)

    CLOSE(iInUnit)
    CLOSE(iOutUnit)
    DEALLOCATE(rVal)

    iTime = iTime - 1  ! Subtract failed read
    CALL LogMessage('  Read '//TRIM(IntToText(iTime))//' timesteps from '// &
         TRIM(This%HydInfo(iHydType)%cOutFilePath), f_iInfo, cModName)

  END SUBROUTINE ReadDotOutFile

  ! =====================================================================
  ! Kill - Deallocate
  ! =====================================================================
  SUBROUTINE Kill(This)
    CLASS(HydrographReaderType), INTENT(INOUT) :: This
    INTEGER :: i

    DO i = 1, iHR_NUMHYD
      IF (ALLOCATED(This%HydInfo(i)%cHydIDs)) DEALLOCATE(This%HydInfo(i)%cHydIDs)
      IF (ALLOCATED(This%HydInfo(i)%iLayers)) DEALLOCATE(This%HydInfo(i)%iLayers)
    END DO
    This%lDiscovered = .FALSE.
  END SUBROUTINE Kill

  ! =====================================================================
  ! Private helper: ComputeDate from timestep index + start date
  ! =====================================================================
  SUBROUTINE ComputeDate(cTimeUnit, iTimeStep, iStartDay, iStartMon, iStartYr, &
                          iDay, iMon, iYr)
    CHARACTER(LEN=*), INTENT(IN)  :: cTimeUnit
    INTEGER,          INTENT(IN)  :: iTimeStep, iStartDay, iStartMon, iStartYr
    INTEGER,          INTENT(OUT) :: iDay, iMon, iYr

    INTEGER :: iDays, iMonDays(12), iTotalMon
    DATA iMonDays /31,28,31,30,31,30,31,31,30,31,30,31/
    LOGICAL :: lLeap

    iDay = iStartDay
    iMon = iStartMon
    iYr  = iStartYr

    SELECT CASE (TRIM(cTimeUnit))
    CASE ('1DAY')
      ! Add iTimeStep days
      iDays = iTimeStep
      CALL AddDays(iDay, iMon, iYr, iDays)

    CASE ('1WEEK')
      iDays = iTimeStep * 7
      CALL AddDays(iDay, iMon, iYr, iDays)

    CASE ('1MON')
      ! Add iTimeStep months
      iTotalMon = (iStartYr * 12 + iStartMon - 1) + iTimeStep
      iYr  = iTotalMon / 12
      iMon = MOD(iTotalMon, 12) + 1
      IF (iMon <= 0) THEN
        iMon = iMon + 12
        iYr  = iYr - 1
      END IF
      ! Use last day of month
      lLeap = (MOD(iYr,4)==0 .AND. MOD(iYr,100)/=0) .OR. MOD(iYr,400)==0
      IF (lLeap .AND. iMon == 2) THEN
        iDay = 29
      ELSE
        iDay = iMonDays(iMon)
      END IF

    CASE ('1YEAR')
      iYr = iStartYr + iTimeStep

    CASE DEFAULT
      ! Assume daily
      iDays = iTimeStep
      CALL AddDays(iDay, iMon, iYr, iDays)
    END SELECT

  END SUBROUTINE ComputeDate

  ! =====================================================================
  ! Private helper: AddDays to a date
  ! =====================================================================
  SUBROUTINE AddDays(iDay, iMon, iYr, iDaysToAdd)
    INTEGER, INTENT(INOUT) :: iDay, iMon, iYr
    INTEGER, INTENT(IN)    :: iDaysToAdd

    INTEGER :: i, iDIM, iMonDays(12)
    DATA iMonDays /31,28,31,30,31,30,31,31,30,31,30,31/
    LOGICAL :: lLeap

    iDay = iDay + iDaysToAdd
    DO WHILE (iDay > 0)
      lLeap = (MOD(iYr,4)==0 .AND. MOD(iYr,100)/=0) .OR. MOD(iYr,400)==0
      IF (lLeap .AND. iMon == 2) THEN
        iDIM = 29
      ELSE
        iDIM = iMonDays(iMon)
      END IF
      IF (iDay <= iDIM) EXIT
      iDay = iDay - iDIM
      iMon = iMon + 1
      IF (iMon > 12) THEN
        iMon = 1
        iYr  = iYr + 1
      END IF
    END DO

  END SUBROUTINE AddDays

  ! =====================================================================
  ! Private helper: Parse date from string (MM/DD/YYYY or DD/MM/YYYY)
  ! =====================================================================
  SUBROUTINE ParseDateFromString(cDateStr, iDateSpec, iDay, iMon, iYr, iStat)
    CHARACTER(LEN=*), INTENT(IN)  :: cDateStr
    INTEGER,          INTENT(IN)  :: iDateSpec
    INTEGER,          INTENT(OUT) :: iDay, iMon, iYr, iStat

    INTEGER :: iSlash1, iSlash2, iP1, iP2, iP3

    iStat = 0
    iSlash1 = SCAN(cDateStr, '/')
    IF (iSlash1 == 0) THEN
      iStat = -1; RETURN
    END IF
    iSlash2 = SCAN(cDateStr(iSlash1+1:), '/')
    IF (iSlash2 == 0) THEN
      iStat = -1; RETURN
    END IF
    iSlash2 = iSlash1 + iSlash2

    READ(cDateStr(1:iSlash1-1), *, IOSTAT=iStat) iP1
    IF (iStat /= 0) RETURN
    READ(cDateStr(iSlash1+1:iSlash2-1), *, IOSTAT=iStat) iP2
    IF (iStat /= 0) RETURN
    READ(cDateStr(iSlash2+1:), *, IOSTAT=iStat) iP3
    IF (iStat /= 0) RETURN

    IF (iDateSpec == 1) THEN
      ! dd/mm/yyyy
      iDay = iP1; iMon = iP2; iYr = iP3
    ELSE
      ! mm/dd/yyyy
      iMon = iP1; iDay = iP2; iYr = iP3
    END IF

  END SUBROUTINE ParseDateFromString

  ! =====================================================================
  ! Private helper: Uppercase a local string
  ! =====================================================================
  SUBROUTINE UpperCaseLocal(cStr)
    CHARACTER(LEN=*), INTENT(INOUT) :: cStr
    INTEGER :: i, ic
    DO i = 1, LEN_TRIM(cStr)
      ic = ICHAR(cStr(i:i))
      IF (ic >= 97 .AND. ic <= 122) cStr(i:i) = CHAR(ic - 32)
    END DO
  END SUBROUTINE UpperCaseLocal

END MODULE Class_HydrographReader
