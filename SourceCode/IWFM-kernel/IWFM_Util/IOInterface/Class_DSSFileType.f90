!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2026
!  State of California, Department of Water Resources
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov
!***********************************************************************
MODULE Class_DSSFileType
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE heclib_compat
  USE GeneralUtilities    , ONLY: UpperCase                                  , &
                                  NativePathName                             , &
                                  IntTotext
  USE TimeSeriesUtilities , ONLY: f_iTimeStampLength                         , &
                                  TimeStampToJulian                          , &
                                  TimeStampToYear4000                        , &
                                  DSSStyleDate                               , &
                                  DSSStyleHoursAfterMidnight                 , &
                                  ExtractDay                                 , &
                                  ExtractMonth                               , &
                                  ExtractYear                                , &
                                  JulianDateAndMinutesToTimeStamp            , &
                                  TimeStampToJulianDateAndMinutes            , &
                                  IncrementJulianDateAndMinutesAfterMidnight , &
                                  DayMonthYearToJulianDate                   , &
                                  AdjustTimeStampWithYear4000                , &  
                                  AdjustRateTypeData                         , &
                                  SetTSDCacheSize                            , &
                                  NPeriods                                   , &
                                  IncrementTimeStamp                         , &
                                  IntervalInMinutes_To_IntervalText          , &
                                  IsLeapYear                                 , &
                                  OPERATOR(.TSGT.)
  USE MessageLogger       , ONLY: MessageLoggerType                          , &
                                  DefaultLogger                              , &
                                  MessageArray                               , &
                                  f_iWarn                                    , &
                                  f_iFatal
  USE Class_BaseFileType  , ONLY: BaseFileType                               , &
                                  GetAUnitNumber                             , &
                                  f_IO_SuccessfulRead                        , &
                                  f_IO_TSData_NotYetTimeToRead               , &
                                  f_IO_TSData_DssReadError
  IMPLICIT NONE




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: DssInFileType   , &
            DssOutFileType


  ! -------------------------------------------------------------
  ! --- PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER          :: f_iDefaultPathNameLength        = 80      , &
                                f_iDefaultPartNameLength        = 32      , &
                                f_iDefaultDataUnitAndTypeLength = 8


  ! -------------------------------------------------------------
  ! --- DATA TYPE THAT KEEPS A RECORD OF ALL OPENED INPUT/OUTPUT DSS FILES
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseFileType) :: DSSFileListNodeType
      PRIVATE
      INTEGER                                          :: DSSFileIndex     = 0
      INTEGER(C_LONG_LONG),DIMENSION(250)              :: IFLTAB           = 0       !Array that describes the DSS file (HEC-DSS 7 uses long long[250])
      INTEGER                                          :: iNConnectedFiles = 0       !Number of DSSIn or DSSOut files pointing to this node
      TYPE(DSSFileListNodeType),POINTER                :: Next          => NULL()
  CONTAINS
      PROCEDURE,PASS :: New  => New_DSSFileListNode
      PROCEDURE,PASS :: Kill => Kill_DSSFileListNode
  END TYPE DSSFileListNodeType
  ! Shared process-wide registry of open DSS files. Thread-safety is
  ! ensured by the DLL's CRITICAL(IWFM_MODEL_MGMT) in IW_Model_New /
  ! IW_Model_WSA_New â€” all DSS opens happen during model construction,
  ! which is serialized. There are no mutations to this list outside
  ! that path (the Kill path clears it non-concurrently at DLL teardown).
  ! If concurrent DSS opens ever become necessary, replace this with a
  ! per-model registry or wrap mutations in a dedicated CRITICAL.
  TYPE(DSSFileListNodeType),POINTER,SAVE :: DSSFileListHead => NULL()
  TYPE(DSSFileListNodeType),POINTER,SAVE :: DSSFileListTail => NULL()


  ! -------------------------------------------------------------
  ! --- DSS INPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseFileType) :: DssInFileType   !BaseFileType component is not used, all that info will be stored in the DSSFileListNode data
      PRIVATE
      TYPE(DSSFileListNodeType),POINTER                   :: pFileData            => NULL()       !DSS file data node that is associated with this DSS input file
      CHARACTER(LEN=f_iTimeStampLength)                   :: cFirstDataDate                   = ''            !Date of the first data in the file  
      REAL(8)                                             :: JulianOfLastDataRead             = -HUGE(0d0)    !Julian date (real number) of the last data that was read from the DSS file for each (pathname)
      INTEGER                                             :: Interval_InMinutes               =  0            !Time interval of data in minutes for each (pathname)
      CHARACTER(LEN=f_iDefaultPathNameLength),ALLOCATABLE :: PathNames(:)                         !Path names used
      LOGICAL,ALLOCATABLE                                 :: Year4000Flag(:)                                  !Flag to represent if the data uses Year 4000 flag for repetetive data, defined for each (pathname)
      LOGICAL,ALLOCATABLE                                 :: RateTypeData(:)                                  !Flag to specify if the data being read is a rate type data defined for each (pathname); this information is used to convert the time-series data based on the time step of the simulation)!
      !--- Use Cache for regular-interval dss data array reads                                
      LOGICAL                                             :: lMatrixCacheBuilt                = .FALSE.       !Indicates whether the cached Matrix has been initialized and populated
      LOGICAL                                             :: lMartixRecycle                   = .FALSE.       !Flag to indicate whether to recycle the cached matrix for 4000 year
      INTEGER                                             :: iMatrixCacheNPeriods             = 0             !Number of time periods currently stored in the cached matrix
      INTEGER                                             :: iMatrixCachePointer              = 1             !the location of pointer in the cached matrix (used when reading data in chunks)
      INTEGER                                             :: iMatrixCacheHECJulianDay         = 0             !HEC Julian date corresponding to the first cached time step in the matrix cache                                           
      INTEGER                                             :: iMatrixCacheMinutesAfterMidnight = 0             !Minutes after midnight corresponding to the first cached time step in the matrix cache
      CHARACTER(LEN=f_iTimeStampLength)                   :: cMatrixCacheStartTS              = ''            !Timestamp corresponding to the first cached time step                                          
      REAL(8),ALLOCATABLE                                 :: rMatrixCacheData(:,:,:)                          !Cached matrix data array with dimensions: (row, column, time)                                      
  CONTAINS
      PROCEDURE,PASS :: New                                  => New_DSSInFile
      PROCEDURE,PASS :: Kill                                 => Kill_DSSInFile
      PROCEDURE,PASS :: GetName                              => GetName_DSSInFile
      PROCEDURE,PASS :: GetDSSPathNames                      => GetDSSPathNames_DssInFile
      PROCEDURE,PASS :: SetParameters_DSSInFile
      PROCEDURE,PASS :: SetRateTypeData                      => SetRateTypeData_DSSInFile
      PROCEDURE,PASS :: Rewind_To_BeginningOfTSData
      PROCEDURE,PASS :: ReadRegularIntervalScalar_DssInFile
      PROCEDURE,PASS :: ReadRegularIntervalArray_DssInFile
      PROCEDURE,PASS :: ReadRegularIntervalMatrix_DssInFile
      PROCEDURE,PASS :: ReadEntireTSD_DSSInFile
      PROCEDURE,PASS :: ReadTimeSeries_ForTimeRange_DSSInFile
      GENERIC        :: ReadData                             => ReadRegularIntervalScalar_DssInFile  , &
                                                                ReadRegularIntervalArray_DssInFile   , &
                                                                ReadRegularIntervalMatrix_DssInFile  , &
                                                                ReadEntireTSD_DSSInFile              , &
                                                                ReadTimeSeries_ForTimeRange_DSSInFile
  END TYPE DssInFileType


  ! -------------------------------------------------------------
  ! --- DSS OUTPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseFileType) :: DssOutFileType   !BaseFileType component is not used, all that info will be stored in the DSSFileListNode data
      PRIVATE
      TYPE(DSSFileListNodeType),POINTER                   :: pFileData             => NULL()     !DSS file data node that is associated with this DSS input file
      INTEGER                                             :: NumberOfDataBatch     = 0           !Number of batch of data to be stored
      INTEGER                                             :: NumberOfDataRows      = 0           !Number of data rows to be stored
      INTEGER                                             :: NumberOfDataColumns   = 0           !Number of data columns to be stored
      INTEGER                                             :: DataPointer           = 0           !Pointer to keep track of the row number to store values before flushing to the hard drive
      CHARACTER(LEN=f_iDefaultPathNameLength),ALLOCATABLE :: PathNames(:)                        !Path names used
      CHARACTER(LEN=8),ALLOCATABLE                        :: DataUnit(:)                         !Units of data
      CHARACTER(LEN=8),ALLOCATABLE                        :: DataType(:)                         !Type of data as given in the HEC-DSS standards
      CHARACTER(LEN=f_iTimeStampLength)                   :: DataStartDateAndTime  = ''          !Starting date and time for the first value in the data storage
      REAL(8),ALLOCATABLE                                 :: ValuesForOutput(:,:,:)              !Array that stores the values to be flushed to the hard drive (will have dimensions NumberOfDataRows x NumberOfDataColumns x NumberOfDataBatch)
  CONTAINS
      PROCEDURE,PASS :: New                                   => New_DSSOutFile
      PROCEDURE,PASS :: Kill                                  => Kill_DSSOutFile
      PROCEDURE,PASS :: GetName                               => GetName_DSSOutFile
      PROCEDURE,PASS :: SetParameters_DSSOutFile_PathNames
      PROCEDURE,PASS :: SetParameters_DSSOutFile_Parts
      PROCEDURE,PASS :: WriteRegularIntervalArray_DssOutFile
      PROCEDURE,PASS :: WriteRegularIntervalMatrix_DssOutFile
      PROCEDURE,PASS :: WriteEntireTSD_DSSOutFile
      GENERIC        :: SetParameters                         => SetParameters_DSSOutFile_PathNames    , &
                                                                 SetParameters_DSSOutFile_Parts
      GENERIC        :: WriteData                             => WriteRegularIntervalArray_DssOutFile  , &
                                                                 WriteRegularIntervalMatrix_DssOutFile , &
                                                                 WriteEntireTSD_DSSOutFile
  END TYPE DssOutFileType


  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE GetDSSFile
      MODULE PROCEDURE GetDSSFile_ByIndex
      MODULE PROCEDURE GETDSSFile_ByName
  END INTERFACE GetDSSFile


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_DSSFileType::'




CONTAINS





! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW DSS FILE LIST NODE
  ! -------------------------------------------------------------
  SUBROUTINE New_DSSFileListNode(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
    CLASS(DSSFileListNodeType)           :: ThisFile    !Not used
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: lInputFile  !Not used
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Not used
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19),PARAMETER :: ThisProcedure = ModName // 'New_DSSFileListNode'
    INTEGER                                :: DSSFileIndex,ErrorCode,AUnitNumber,iLogFileUnit
    LOGICAL                                :: FileIsOpen
    TYPE(DSSFileListNodeType),POINTER      :: pDSSFileNode

    !Initialize
    iStat = 0
    IF (.NOT. ASSOCIATED(ThisFile%Logger)) ThisFile%Logger => DefaultLogger
    IF (PRESENT(FileOpenCode)) FileOpenCode = 0

    !NOTE: DSS file linked list is not thread-safe for concurrent opens.
    !This is acceptable because DSS files are only opened during initialization,
    !not during the simulation loop. Concurrent model creation should use
    !the DLL's OMP CRITICAL(IWFM_MODEL_MGMT) to serialize initialization.

    !If there is no entry in the file list (i.e. first call to this method) set general properties for the DSS file handling methods
    IF (.NOT. ASSOCIATED(DSSFileListHead)) THEN
        !Set the message output unit
        CALL ThisFile%Logger%GetLogFileUnit(iLogFileUnit=iLogFileUnit,iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        CALL ZSET('MUNIT','',iLogFileUnit)

        !Set the level of messaging to error and warning messages only
        CALL ZSET('MLEVEL','',0)
    END IF

    !Check if the DSS file is already open
    CALL CheckOpenStatus(FileName,FileIsOpen,DSSFileIndex)

    !If file is not opened, open it and add to the list of DSS files
    IF (.NOT. FileIsOpen) THEN
      !Add a new node to the file list
      IF (.NOT. ASSOCIATED(DSSFileListHead)) THEN  !Initial entry
          ALLOCATE (DSSFileListHead,STAT=ErrorCode)
          IF (ErrorCode .NE. 0) THEN
              IF (PRESENT(FileOpenCode)) THEN
                  FileOpenCode = ErrorCode
                  RETURN
              ELSE
                  CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for DSS file list for file '//TRIM(ADJUSTL(FileName))//'!',f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
          END IF
          CALL DSSFileListHead%NewBaseFile(FileName)
          DSSFileListHead%DSSFileIndex =  1
          DSSFileListTail              => DSSFileListHead

      !Entries other than initial entry
      ELSE
          ALLOCATE (DSSFileListTail%Next,STAT=ErrorCode)
          IF (ErrorCode .NE. 0) THEN
              IF (PRESENT(FileOpenCode)) THEN
                  FileOpenCode = ErrorCode
                  RETURN
              ELSE
                  CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for DSS file list for file '//TRIM(ADJUSTL(FileName))//'!',f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
          END IF
          DSSFileListTail%Next%DSSFileIndex =  DSSFileListTail%DSSFileIndex + 1
          DSSFileListTail                   => DSSFileListTail%Next
          CALL DSSFileListTail%NewBaseFile(FileName)
      END IF

      !Obtain an unconnected unit number for the DSS file
      AUnitNumber           = GetAUnitNumber()
      DSSFileListTail%UnitN = AUnitNumber
      CALL ZSET('UNIT','',AUnitNumber)

      !Open DSS file
      CALL ZOPEN(DSSFileListTail%IFLTAB,NativePathName(FileName),ErrorCode)

      !Implement actions based on error code
      IF (ErrorCode.NE.0) THEN
          IF (PRESENT(FileOpenCode)) THEN
              FileOpenCode = ErrorCode
              RETURN
          ELSE
              CALL ThisFile%Logger%SetLastMessage('Error in opening file '//TRIM(ADJUSTL(FileName))//'!',f_iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
      END IF
      
      !If made it this far, add 1 to the number of connected files
      DSSFileListTail%iNConnectedFiles = DSSFileListTail%iNConnectedFiles + 1
      
    !If file is already open, then just incerment the number of connected files
    ELSE
        CALL GetDSSFile_ByName(FileName,pDSSFileNode,ThisFile%Logger,iStat)
        IF (iStat .NE. 0) RETURN
        pDSSFileNode%iNConnectedFiles = pDSSFileNode%iNConnectedFiles + 1
        NULLIFY(pDSSFileNode)
    END IF

  END SUBROUTINE New_DSSFileListNode


  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR FOR DSS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE New_DssInFile(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
    CLASS(DssInFileType)                 :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: lInputFile  !Not used
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Not used
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'New_DssInFile'
    TYPE(DSSFileListNodeType)              :: DummyDSSFileNode
    CHARACTER                              :: cName*64
    INTEGER                                :: iNName
    LOGICAL                                :: lExist

    !Initialize
    iStat = 0

    !First check if the DSS file exists
    CALL ZFNAME(FileName,cName,iNName,lExist)
    IF (.NOT.lExist) THEN
        CALL ThisFile%Logger%SetLastMessage('DSS file '//TRIM(FileName)//' does not exist for data retrieval!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Instantiate a DSS file node in the list of DSS files being used
    IF (PRESENT(FileOpenCode)) THEN
        CALL New_DSSFileListNode(DummyDSSFileNode,FileName,lInputFile,FileOpenCode=FileOpenCode,iStat=iStat)
    ELSE
        CALL New_DSSFileListNode(DummyDSSFileNode,FileName,lInputFile,iStat=iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    !Associate the DSS input file with the DSS file node
    CALL GetDSSFile(FileName,ThisFile%pFileData,ThisFile%Logger,iStat)

  END SUBROUTINE New_DssInFile


  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR FOR DSS OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE New_DssOutFile(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
    CLASS(DssOutFileType)                :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: lInputFile  !Not used
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Not used
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    TYPE(DSSFileListNodeType) :: DummyDSSFileNode

    !INitialize
    iStat = 0

    !Instantiate a DSS file node in the list of DSS files being used
    IF (PRESENT(FileOpenCode)) THEN
        CALL New_DSSFileListNode(DummyDSSFileNode,FileName,lInputFile,FileOpenCode=FileOpenCode,iStat=iStat)
    ELSE
        CALL New_DSSFileListNode(DummyDSSFileNode,FileName,lInputFile,iStat=iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    !Associate the DSS output file with the DSS file node
    CALL GetDSSFile(FileName,ThisFile%pFileData,ThisFile%Logger,iStat)

  END SUBROUTINE New_DssOutFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- DESTRUCTOR FOR DSS FILE LIST NODE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_DSSFileListNode(ThisFile,Status)
    CLASS(DSSFileListNodeType)           :: ThisFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Status    !Not used

    !Local variables
    TYPE(DSSFileListNodeType) :: Dummy
    
    !Decrease number of connected files by 1
    ThisFile%iNConnectedFiles = ThisFile%iNConnectedFiles - 1
    
    !If number of connected files is not zero, return
    IF (ThisFile%iNConnectedFiles .GT. 0) RETURN
    
    !Close DSS file
    CALL ZCLOSE(ThisFile%IFLTAB)

    !Clear the BaseFileType component
    CALL ThisFile%KillBaseFile()
    
    !Then clear the rest of the data (do not clear the Next pointer as it may be pointing to a legitimate data node)
    SELECT TYPE (p => ThisFile)
        TYPE IS (DSSFileListNodeType) 
            p = Dummy
    END SELECT 

  END SUBROUTINE Kill_DSSFileListNode


  ! -------------------------------------------------------------
  ! --- DESTRUCTOR FOR DSS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_DSSInFile(ThisFile,Status)
    CLASS(DSSInFileType)                 :: ThisFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Status    !Not used

    !Local variables
    INTEGER             :: iErrorCode
    TYPE(DSSInFileType) :: Dummy

    !First, free the memory from pointer arrays
    DEALLOCATE (ThisFile%PathNames    , &
                ThisFile%Year4000Flag , &
                STAT=iErrorCode       )
    IF (ALLOCATED(ThisFile%RateTypeData)) DEALLOCATE (ThisFile%RateTypeData , STAT=iErrorCode)
    
    !Then, kill the DSS file list node
    IF (ASSOCIATED(ThisFile%pFileData)) CALL ThisFile%pFileData%Kill()
    
    !Reset the rest of the data fields
    SELECT TYPE (p => ThisFile)
        TYPE IS (DSSInFileType)
            p = Dummy
    END SELECT
    ThisFile%pFileData          => NULL()
    
  END SUBROUTINE Kill_DSSInFile


  ! -------------------------------------------------------------
  ! --- DESTRUCTOR FOR DSS OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_DSSOutFile(ThisFile,Status)
    CLASS(DSSOutFileType)                :: ThisFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Status    !Not used

    !Local variables
    INTEGER              :: ErrorCode
    TYPE(DSSOutFileType) :: Dummy

    !First, free the memory from pointer arrays
    DEALLOCATE(ThisFile%PathNames       , &
               ThisFile%DataUnit        , &
               ThisFile%DataType        , &
               ThisFile%ValuesForOutput , &
               STAT=ErrorCode           )

    !Then, kill the DSS file list node
    IF (ASSOCIATED(ThisFile%pFileData)) CALL ThisFile%pFileData%Kill()
    
    !Reset the rest of the data fields
    ThisFile%pFileData            => NULL()
    ThisFile%NumberOfDataBatch    =  Dummy%NumberOfDataBatch
    ThisFile%NumberOfDataRows     =  Dummy%NumberOfDataRows
    ThisFile%NumberOfDataColumns  =  Dummy%NumberOfDataColumns
    ThisFile%DataPointer          =  Dummy%DataPointer
    ThisFile%DataStartDateAndTime =  Dummy%DataStartDateAndTime

  END SUBROUTINE Kill_DSSOutFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GIVEN AN INPUT DSS FILE, RETURN ITS PATHNAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetDSSPathNames_DSSInFile(ThisFile,PathnameList)
    CLASS(DssInFileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),ALLOCATABLE    :: PathNameList(:)

    ALLOCATE (PathNameList(SIZE(ThisFile%PathNames)))
    PathNameList = ThisFile%PathNames

  END SUBROUTINE GetDSSPathNames_DSSInFile


  ! -------------------------------------------------------------
  ! --- GET THE NAME OF A DSS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetName_DSSInFile(ThisFile,cName)
    CLASS(DSSInFileType),INTENT(IN)      :: ThisFile
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cName

    !Local variables
    INTEGER :: ErrorCode,iLen

    !Clear cName just in case
    DEALLOCATE (cName , STAT=ErrorCode)

    !Allocate cName
    iLen = LEN(ThisFile%pFileData%Name)
    ALLOCATE (CHARACTER(LEN=iLen) :: cName)
    cName = ThisFile%pFileData%Name

  END SUBROUTINE GetName_DSSInFile


  ! -------------------------------------------------------------
  ! --- GET THE NAME OF A DSS OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetName_DSSOutFile(ThisFile,cName)
    CLASS(DSSOutFileType),INTENT(IN)     :: ThisFile
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cName

    !Local variables
    INTEGER :: ErrorCode,iLen

    !Clear cName just in case
    DEALLOCATE (cName , STAT=ErrorCode)

    !Allocate cName
    iLen = LEN(ThisFile%pFileData%Name)
    ALLOCATE (CHARACTER(LEN=iLen) :: cName)
    cName = ThisFile%pFileData%Name

  END SUBROUTINE GetName_DSSOutFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SET PARAMETERS FOR DSS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetParameters_DSSInFile(ThisFile,cPathNames,iNColumnsOfData,iStat,iNRowsOfData)
    CLASS(DSSInFileType)                               :: ThisFile
    CHARACTER(LEN=f_iDefaultPathNameLength),INTENT(IN) :: cPathNames(:)
    INTEGER,INTENT(IN)                                 :: iNColumnsOfData
    INTEGER,INTENT(OUT)                                :: iStat
    INTEGER,OPTIONAL,INTENT(IN)                        :: iNRowsOfData

    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER  :: ThisProcedure = ModName // 'SetParameters_DSSInFile'
    CHARACTER(LEN=f_iDefaultPartNameLength) :: cAPart,cBPart,cCPart,cDPart,cEPart,cFPart
    CHARACTER(LEN=32)                       :: cDummyUnits,cDummyType
    CHARACTER(LEN=8)                        :: cInterval
    INTEGER                                 :: nrows,indx,iDummyInt,iNVALS,iErrorCode,Interval_InMinutes
    REAL                                    :: rValue(1)

    !Initialize variables
    iStat = 0
    IF (PRESENT(iNRowsOfData)) THEN
        nrows = iNRowsOfData
    ELSE
        nrows = 1
    END IF

    !Allocate space for the path names
    IF (nrows*iNColumnsOfData .NE. SIZE(cPathNames)) THEN
        CALL ThisFile%Logger%SetLastMessage('Error in sizing of path name array for '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    ALLOCATE (ThisFile%PathNames(nrows*iNColumnsOfData)    , &
              ThisFile%Year4000Flag(nrows*iNColumnsOfData) , &
              STAT=iErrorCode                              )
    IF (iErrorCode .NE. 0) THEN
        CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for the path names for '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    ThisFile%PathNames    = cPathNames
    ThisFile%Year4000Flag = .FALSE.

    !Set the time interval of data in minutes, and check if the data uses Year 4000 flag
    DO indx=1,SIZE(cPathNames)
      CALL ZUFPN(cAPart,iDummyInt           , &
                 cBPart,iDummyInt           , &
                 cCPart,iDummyInt           , &
                 cDPart,iDummyInt           , &
                 cEPart,iDummyInt           , &
                 cFPart,iDummyInt           , &
                 cPathNames(indx)           , &
                 LEN_TRIM(cPathNames(indx)) , &
                 iErrorCode                 )
      IF (iErrorCode .NE. 0) THEN
          CALL ThisFile%Logger%SetLastMessage('Error in finding the interval length of a pathname in file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Set the time interval in minutes; make sure data intervals are all the same
      iDummyInt = 1
      CALL ZGINTL(Interval_InMinutes , &
                  cEPart             , &
                  iDummyInt          , &
                  iDummyInt          )
      SELECT CASE (indx)
        CASE (1)
          ThisFile%Interval_InMinutes = Interval_InMinutes

        CASE DEFAULT
          IF (Interval_InMinutes .NE. ThisFile%Interval_InMinutes) THEN
              CALL ThisFile%Logger%SetLastMessage('The data intervals for records in DSS input file '//ThisFile%pFileData%Name//' should be the same!',f_iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
      END SELECT

      !Set the Year 4000 flag
      !Skip for large files (>1000 pathnames) to avoid C library instability
      !at scale. The Year 4000 flag is a legacy feature rarely used.
      IF (SIZE(cPathNames) .LE. 1000) THEN
          iNVALS = 1
          CALL ZRRTS(ThisFile%pFileData%IFLTAB ,&
                     cPathNames(indx)          ,&
                     '01JAN4000'               ,&
                     '0001'                    ,&
                     iNVALS                    ,&
                     rValue(1)                 ,&
                     cDummyUnits               ,&
                     cDummyType                ,&
                     iDummyInt                 ,&
                     iErrorCode                )
          IF (iErrorCode .EQ. 0) ThisFile%Year4000Flag(indx) = .TRUE.
      END IF
      
      !Annual interval with Year4000 flag (i.e. single entry) is not allowed for consistency with ASCII timeseries data input
      IF (ThisFile%Year4000Flag(indx)) THEN
          cInterval = IntervalInMinutes_To_IntervalText(ThisFile%Interval_InMinutes , 8)
          IF (TRIM(UpperCase(cInterval)) .EQ. '1YEAR') THEN
              MessageArray(1) = 'Annual-interval data with Year-4000 flag is not allowed.'
              MessageArray(2) = 'It appears such a setup is used for pathname'
              MessageArray(3) = TRIM(cPathNames(indx)) // '.'
              CALL ThisFile%Logger%SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF 
      END IF  
    END DO
  END SUBROUTINE SetParameters_DSSInFile


  ! -------------------------------------------------------------
  ! --- SET PARAMETERS FOR DSS OUTPUT FILE WITH WHOLE PATHNAMES SPECIFIED
  ! -------------------------------------------------------------
  SUBROUTINE SetParameters_DSSOutFile_PathNames(ThisFile,PathNames,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfData,NRowsOfData,iStat)
    CLASS(DSSOutFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: PathNames(:),DataUnit(:),DataType(:),SimulationStartTime
    INTEGER,INTENT(IN)          :: NTimeSteps,NColumnsOfData
    INTEGER,OPTIONAL,INTENT(IN) :: NRowsOfData
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+34),PARAMETER :: ThisProcedure = ModName // 'SetParameters_DSSOutFile_PathNames'
    INTEGER,PARAMETER                      :: IPLAN = 0
    INTEGER                                :: SizeDU,SizeDT,nrows,ErrorCode

    !Initialize variables
    iStat = 0
    IF (PRESENT(NRowsOfData)) THEN
        nrows = NRowsOfData
    ELSE
        nrows = 1
    END IF
    SizeDU = SIZE(DataUnit)
    SizeDT = SIZE(DataType)

    !Allocate memory for the storage of time series results
    CALL SetTSDCacheSize(ThisFile%Name                , &
                         ThisFile%ValuesForOutput     , &
                         ThisFile%NumberOfDataBatch   , &
                         ThisFile%NumberOfDataRows    , &
                         ThisFile%NumberOfDataColumns , &
                         NColumnsOfData               , &
                         nrows                        )

    !Allocate space for the path names
    ALLOCATE (ThisFile%PathNames(SIZE(PathNames)),STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for the path names for file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    ThisFile%PathNames = PathNames

    !Check if the length of DataUnit and DataType are acceptable
    CALL CheckLength(DataUnit(1),ThisFile%Logger,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CheckLength(DataType(1),ThisFile%Logger,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Allocate memory for data unit and type, C and F parts of the path name
    ALLOCATE (ThisFile%DataUnit(SIZE(DataUnit)) , &
              ThisFile%DataType(SIZE(DataType)) , &
              STAT=ErrorCode                    )
    IF (ErrorCode .NE. 0) THEN
        CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for the data unit and type for '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Set the data fields
    ThisFile%DataUnit = ADJUSTL(DataUnit)
    ThisFile%DataType = ADJUSTL(DataType)

    !Delete pathnames if they already exist
    !IF (SimulationStartTime .NE. '') CALL DeleteRecords(ThisFile,PathNames,SimulationStartTime,DSSStyleHoursAfterMidnight(SimulationStartTime),NTimeSteps)

  END SUBROUTINE SetParameters_DSSOutFile_PathNames


  ! -------------------------------------------------------------
  ! --- SET PARAMETERS FOR DSS OUTPUT FILE WITH INDIVIDUAL PARTS SPECIFIED
  ! -------------------------------------------------------------
  SUBROUTINE SetParameters_DSSOutFile_Parts(ThisFile,APart,BPart,CPart,EPart,FPart,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfData,NRowsOfData,iStat)
    CLASS(DSSOutFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: APart,EPart
    CHARACTER(LEN=*),INTENT(IN) :: BPart(:),CPart(:),FPart(:),DataUnit(:),DataType(:)
    CHARACTER(LEN=*),INTENT(IN) :: SimulationStartTime
    INTEGER,INTENT(IN)          :: NTimeSteps,NColumnsOfData
    INTEGER,OPTIONAL,INTENT(IN) :: NRowsOfData
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+30),PARAMETER  :: ThisProcedure = ModName // 'SetParameters_DSSOutFile_Parts'
    INTEGER,PARAMETER                       :: IPLAN=0
    INTEGER                                 :: indx,indx1,SizeDU,SizeDT,SizeB,SizeC,SizeF,nrows,ErrorCode
    CHARACTER(LEN=f_iDefaultPartNameLength) :: PartNames(6)

    !Initialize variables
    iStat = 0
    IF (PRESENT(NRowsOfData)) THEN
        nrows=NRowsOfData
    ELSE
        nrows = 1
    END IF
    SizeDU = SIZE(DataUnit)
    SizeDT = SIZE(DataType)
    SizeB  = SIZE(BPart)
    SizeC  = SIZE(CPart)
    SizeF  = SIZE(FPart)

    !Allocate memory for the storage of time series results
    CALL SetTSDCacheSize(ThisFile%Name                , &
                         ThisFile%ValuesForOutput     , &
                         ThisFile%NumberOfDataBatch   , &
                         ThisFile%NumberOfDataRows    , &
                         ThisFile%NumberOfDataColumns , &
                         NColumnsOfData               , &
                         nrows                        )

    !Allocate space for the path names
    ALLOCATE (ThisFile%PathNames(nrows*NColumnsOfData),STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for the path names for file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Check if the length of DataUnit and DataType are acceptable
    CALL CheckLength(DataUnit(1),ThisFile%Logger,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CheckLength(DataType(1),ThisFile%Logger,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Allocate memory for data unit and type, C and F parts of the path name
    ALLOCATE (ThisFile%DataUnit(SIZE(DataUnit)) , &
              ThisFile%DataType(SIZE(DataType)) , &
              STAT=ErrorCode                    )
    IF (ErrorCode .NE. 0) THEN
        CALL ThisFile%Logger%SetLastMessage('Error in allocating memory for the data unit and type for file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Set the data fields
    ThisFile%DataUnit = ADJUSTL(DataUnit)
    ThisFile%DataType = ADJUSTL(DataType)

    !Form path names and store them in the PathNames data field
    PartNames(1) = APart
    PartNames(4) = '' !DPart is ignored
    PartNames(5) = UpperCase(EPart)
    DO indx=1,SIZE(ThisFile%PathNames)
        indx1 = MOD(indx,SizeB)  ;  IF (indx1 .EQ. 0) indx1 = SizeB  ;  PartNames(2) = BPart(indx1)
        indx1 = MOD(indx,SizeC)  ;  IF (indx1 .EQ. 0) indx1 = SizeC  ;  PartNames(3) = CPart(indx1)
        indx1 = MOD(indx,SizeF)  ;  IF (indx1 .EQ. 0) indx1 = SizeF  ;  PartNames(6) = FPart(indx1)
        ThisFile%PathNames(indx)=FormPathName(PartNames)
    END DO

    !Delete pathnames if they already exist
    !IF (SimulationStartTime .NE. '') CALL DeleteRecords(ThisFile,ThisFile%PathNames,SimulationStartTime,DSSStyleHoursAfterMidnight(SimulationStartTime),NTimeSteps)

  END SUBROUTINE SetParameters_DSSOutFile_Parts


  ! -------------------------------------------------------------
  ! --- SET RateTypeData Flag FOR DSS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetRateTypeData_DSSInFile(ThisFile,RateTypeData)
    CLASS(DSSInFileType) :: ThisFile
    LOGICAL,INTENT(IN)   :: RateTypeData(:)

    !Set the flag
    ALLOCATE (ThisFile%RateTypeData(SIZE(RateTypeData)))
    ThisFile%RateTypeData = RateTypeData

    !Make sure that the size of Pathnames and RateTypeData are equal
    !IF (SIZE(ThisFile%PathNames).NE.SIZE(ThisFile%RateTypeData)) &
    !  CALL ThisFile%Logger%LogMessage('RateTypeData flag for '//TRIM(ThisFile%Descriptor)//' is not sized properly!',3,ThisProcedure)

  END SUBROUTINE SetRateTypeData_DSSInFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ REGULAR INTERVAL TIME SERIES SCALAR DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRegularIntervalScalar_DssInFile(ThisFile,SimulationTime,Data,Stat,iStat)
    CLASS(DssInFileType)        :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(OUT)        :: Data
    INTEGER,INTENT(OUT)         :: Stat,iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+35),PARAMETER :: ThisProcedure = ModName // 'ReadRegularIntervalScalar_DssInFile'
    INTEGER                                :: iData(1,1)
    REAL(8)                                :: rData(1,1)

    !Initialize
    iStat = 0

    SELECT TYPE (Data)
        TYPE IS (INTEGER)
            CALL ThisFile%ReadRegularIntervalMatrix_DSSInFile(SimulationTime,iData,Stat,iStat=iStat)
            IF (Stat .EQ. 0) Data = iData(1,1)

        TYPE IS (REAL(8))
            CALL ThisFile%ReadRegularIntervalMatrix_DSSInFile(SimulationTime,rData,Stat,iStat=iStat)
            IF (Stat .EQ. 0) Data = rData(1,1)

        CLASS DEFAULT
            CALL ThisFile%Logger%SetLastMessage('The specified data type cannot be read from file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

  END SUBROUTINE ReadRegularIntervalScalar_DssInFile


  ! -------------------------------------------------------------
  ! --- READ REGULAR INTERVAL TIME SERIES ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRegularIntervalArray_DssInFile(ThisFile,cSimulationTime,Data,iFileReadCode,iStat)
    CLASS(DssInFileType)        :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cSimulationTime
    CLASS(*),INTENT(OUT)        :: Data(:)
    INTEGER,INTENT(OUT)         :: iFileReadCode,iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+34),PARAMETER :: ThisProcedure = ModName // 'ReadRegularIntervalArray_DssInFile'
    INTEGER                                :: iData(1,SIZE(Data))
    REAL(8)                                :: rData(1,SIZE(Data))

    !Initialize
    iStat = 0

    SELECT TYPE (Data)
        TYPE IS (INTEGER)
            CALL ThisFile%ReadRegularIntervalMatrix_DSSInFile(cSimulationTime,iData,iFileReadCode,iStat)
            IF (iFileReadCode .EQ. f_IO_SuccessfulRead) Data = iData(1,:)

        TYPE IS (REAL(8))
            CALL ThisFile%ReadRegularIntervalMatrix_DSSInFile(cSimulationTime,rData,iFileReadCode,iStat)
            IF (iFileReadCode .EQ. f_IO_SuccessfulRead) Data = rData(1,:)

        CLASS DEFAULT
            CALL ThisFile%Logger%SetLastMessage('The specified data type cannot be read from file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

  END SUBROUTINE ReadRegularIntervalArray_DssInFile


  ! -------------------------------------------------------------
  ! --- READ REGULAR INTERVAL TIME SERIES MATRIX DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRegularIntervalMatrix_DssInFile(ThisFile,cSimulationTime,Data,iFileReadCode,iStat)
    CLASS(DssInFileType)        :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cSimulationTime
    CLASS(*),INTENT(OUT)        :: Data(:,:)
    INTEGER,INTENT(OUT)         :: iFileReadCode,iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+35)      :: ThisProcedure = ModName // 'ReadRegularIntervalMatrix_DssInFile'
    INTEGER                           :: iNCol,iNRow
    REAL(8)                                :: rData(SIZE(Data,Dim=1),SIZE(Data,Dim=2))
    CHARACTER(LEN=f_iTimeStampLength) :: cYear4000SimTimeStamp
    CHARACTER                         :: cSimDate*20,cSimTime*4,cYear4000SimDate*20,cYear4000SimTime*4

    !Initialize
    iStat        = 0
    iFileReadCode = f_IO_TSData_NotYetTimeToRead
    iNRow         = SIZE(rData,DIM=1)
    iNCol         = SIZE(rData,DIM=2)

    !Return if it is not time to read data
    IF (TimeStampToJulian(cSimulationTime) .LE. ThisFile%JulianOfLastDataRead) RETURN
    
    !Serve from cached data array if available
    IF (ThisFile%lMatrixCacheBuilt) THEN
        CALL ReadMatrixFromCache(ThisFile,cSimulationTime,iNRow,iNCol,Data,iFileReadCode,iStat)
        RETURN
    END IF

    !Make sure that Data type is recognized for reading
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            !Do nothing
        TYPE IS (INTEGER)
            !Do nothing
        CLASS DEFAULT
            CALL ThisFile%Logger%SetLastMessage('The specified data type cannot be read from file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT

    !Initialize variables that will be used for data reading
    cYear4000SimTimeStamp = TimeStampToYear4000(cSimulationTime)
    cYear4000SimDate      = DSSStyleDate(cYear4000SimTimeStamp)
    cYear4000SimTime      = DSSStyleHoursAfterMidnight(cYear4000SimTimeStamp)
    
    !Convert SimulationTime to DSS style simulation date and time
    cSimDate = DSSStyleDate(cSimulationTime)
    cSimTime = DSSStyleHoursAfterMidnight(cSimulationTime)
    
    !If this is the first time, figure out the date of the first data point and build The cache Matrix
    IF (ThisFile%cFirstDataDate .EQ. '') THEN
        CALL IdentifyFirstDataDate(ThisFile,iNRow,iNCol,iStat) ; IF (iStat .NE. 0) RETURN
    END IF
    CALL BuildMatrixCache(ThisFile,iNRow,iNCol,iStat) ; IF (iStat .NE. 0) RETURN
    CALL ReadMatrixFromCache(ThisFile,cSimulationTime,iNRow,iNCol,Data,iFileReadCode,iStat) 


  CONTAINS


    !################################################################
    !### IDENTIFY DATE OF FIRST DATA IN EACH PATHNAME
    !################################################################
    SUBROUTINE IdentifyFirstDataDate(ThisFile,iNRow,iNCol,iStat)
      TYPE(DSSInFileType),INTENT(INOUT) :: ThisFile
      INTEGER,INTENT(IN)                :: iNRow,iNCol
      INTEGER,INTENT(OUT)               :: iStat

      !Local variables
      INTEGER                           :: iNVals,indx,iErrorCode,iDataTimeOffset,iDay,iMonth,iYear,iHECJulianDate,   &
                                           iIWFMJulianDate,iMinutesAfterMidnight,iOffset,iNPeriods,iHECEndJulianDate, &
                                           iEndMinutesAfterMidnight,iColNum,iRowNum,indxData,iSimulationTimeOffset,   &
                                           iDummy
      REAL(8)                           :: rData(iNRow,iNCol),rJulianOfLastDataRead_Temp
      CHARACTER(LEN=f_iTimeStampLength) :: cWorkTimeStamp,cLastDataDateAndTime
      CHARACTER                         :: cWorkSimDate*20,cWorkSimTime*4,cDataUnit*32,cDataType*32
      REAL,ALLOCATABLE                  :: rValues(:)
      ! Note: IYMDJL, JLIYMD and INCTIM are provided by heclib_compat module

      !Initialize
      iStat         = 0
      !Identify number of data points to read for a 10 year worth of data
      iNVals = NPeriods(ThisFile%Interval_InMinutes,'12/31/2000_24:00','12/31/2010_24:00')
      ALLOCATE (rValues(iNVals))

      DO indx=1,SIZE(ThisFile%PathNames)
          !Adjust SimDate and SimTime if Year4000 flag is used
          IF (ThisFile%Year4000Flag(indx)) THEN
              cWorkTimeStamp = cYear4000SimTimeStamp
              cWorkSimDate   = cYear4000SimDate
              cWorkSimTime   = cYear4000SimTime
          ELSE
              cWorkTimeStamp = cSimulationTime
              cWorkSimDate   = cSimDate
              cWorkSimTime   = cSimTime
          END IF

          !Row and column indicies in rData varaible to put the read data in
          iColNum = MOD(indx,iNCol) ; IF (iColNum .EQ. 0)     iColNum = iNCol
          iRowNum = indx/iNCol      ; IF (iColNum .NE. iNCol) iRowNum = iRowNum + 1
          
          !Read data (keep reading until a legit data is read)
          DO
          CALL ZRRTS(ThisFile%pFileData%IFLTAB, &
                     ThisFile%PathNames(indx) , &
                         cWorkSimDate             , &
                         cWorkSimTime             , &
                         iNVals                   , &
                         rValues                  , &
                         cDataUnit                , &
                         cDataType                , &
                         iDataTimeOffset          , &
                         iErrorCode               )
              IF (iErrorCode .EQ. 0) THEN
                  rData(iRowNum,iColNum) = REAL(rValues(1),8) 
                  EXIT
              ELSEIF (iErrorCode .LE. 3) THEN
                  DO indxData=1,iNVals
                      IF (INT(rValues(indxData)) .EQ. -901) CYCLE
                      IF (INT(rValues(indxData)) .EQ. -902) CYCLE
                      rData(iRowNum,iColNum) = REAL(rValues(indxData),8)
                      cWorkTimeStamp         = IncrementTimeStamp(cWorkTimeStamp,ThisFile%Interval_InMinutes,indxData-1)
                      cWorkSimDate           = DSSStyleDate(cWorkTimeStamp)
                      cWorkSimTime           = DSSStyleHoursAfterMidnight(cWorkTimeStamp)
                      EXIT
                  END DO  
                  EXIT
              ELSEIF (iErrorCode .GT. 10) THEN
                  MessageArray(1) = 'Error in identifying the date of the first data point stored in pathname'
              MessageArray(2) = TRIM(ThisFile%PathNames(indx))
                  CALL ThisFile%Logger%SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
              RETURN
          ELSE
                  cWorkTimeStamp = IncrementTimeStamp(cWorkTimeStamp,ThisFile%Interval_InMinutes,iNVals)
                  cWorkSimDate   = DSSStyleDate(cWorkTimeStamp)
                  cWorkSimTime   = DSSStyleHoursAfterMidnight(cWorkTimeStamp)
          END IF
      END DO

      !Find the date of the data read from the DSS file
          iDay           = ExtractDay(cWorkTimeStamp)
          iMonth         = ExtractMonth(cWorkTimeStamp)
          iYear          = ExtractYear(cWorkTimeStamp)
          iHECJulianDate = IYMDJL(iYear,iMonth,iDay)                                                             !HEC style Julian date
          CALL TimeStampToJulianDateAndMinutes(cWorkTimeStamp,iIWFMJulianDate,iMinutesAfterMidnight)             !IWFM style Julian date
          CALL ZOFSET(iHECJulianDate,iMinutesAfterMidnight,ThisFile%Interval_InMinutes,1,iSimulationTimeOffset)  !Adjust HECJulianDate upwards to the DSS data block boundary
          !If data interval is larger than 1MON (=43200 minutes), use offset as number of months to take care of leap years
          IF (ThisFile%Interval_InMinutes .GT. 43200) THEN
              iOffset   = 43200
              iNPeriods = iDataTimeOffset/43200
          ELSE
              iOffset   = iDataTimeOffset
              iNPeriods = 1
          END IF
          iDummy = INCTIM(iOffset,0,iNPeriods,iHECJulianDate,iMinutesAfterMidnight,iHECEndJulianDate,iEndMinutesAfterMidnight)  !Increment HECJulianDate by ofsset to bring to the DSS data timestamp
          ThisFile%iMatrixCacheHECJulianDay = iHECEndJulianDate
          ThisFile%iMatrixCacheMinutesAfterMidnight = iEndMinutesAfterMidnight
          iDummy = JLIYMD(iHECEndJulianDate,iYear,iMonth,iDay)                                                                  !Find the day,month,year of updated HEC style Julian date
          CALL DayMonthYearToJulianDate(iDay,iMonth,iYear,iIWFMJulianDate)                                                      !Convert date of data read to IWFM style Julian date
          cLastDataDateAndTime = JulianDateAndMinutesToTimeStamp(iIWFMJulianDate,iMinutesAfterMidnight)
          CALL AdjustTimeStampWithYear4000(cLastDataDateAndTime,cSimulationTime,ThisFile%Year4000Flag(indx)) !Convert year-4000 to 
          rJulianOfLastDataRead_Temp = TimeStampToJulian(cLastDataDateAndTime)
          IF (indx .EQ. 1) THEN
              ThisFile%JulianOfLastDataRead = rJulianOfLastDataRead_Temp
          ELSE
              IF (rJulianOfLastDataRead_Temp .NE. ThisFile%JulianOfLastDataRead) THEN
                  iStat           = -1
                  MessageArray(1) = 'Pathname '//TRIM(ThisFile%PathNames(indx))//' is being read at a different time than the others.'
                  MessageArray(2) = 'All pathnames for a particular dataset have to be read at the same time!'
                  CALL ThisFile%Logger%SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  RETURN
              END IF
          END IF
      END DO

      ! Save first DSS data DateAndTime for cache matrix object
      ThisFile%cMatrixCacheStartTS = cLastDataDateAndTime
      ThisFile%cFirstDataDate      = cLastDataDateAndTime

    END SUBROUTINE IdentifyFirstDataDate 
    
    
    !################################################################
    !### Save data into DSS matrix cache for N years worth of data
    !################################################################
    SUBROUTINE BuildMatrixCache(ThisFile,iNRow,iNCol,iStat)
      TYPE(DSSInFileType),INTENT(INOUT) :: ThisFile
      INTEGER,INTENT(IN)                :: iNRow,iNCol
      INTEGER,INTENT(OUT)               :: iStat
      
      !Local variables
      INTEGER                           :: iNVals,indx,iErrorCode,iDataTimeOffset,iNValsOneYear
      INTEGER                           :: iRowcounter,iColcounter
      CHARACTER(LEN=f_iTimeStampLength) :: cWorkTS
      CHARACTER                         :: cWorkSimDate*20,cWorkSimTime*4,cDataUnit*32,cDataType*32
      REAL,ALLOCATABLE                  :: rValues(:)

      iStat       = 0
      iRowcounter = 1
      iColcounter = 0
      
      ! Determine allocation length based on interval
      IF (ThisFile%Interval_InMinutes < 43200) THEN
          iNVals = NPeriods(ThisFile%Interval_InMinutes, '12/31/2009_24:00', '12/31/2010_24:00')      ! Sub-monthly interval  use 1 year
      ELSE
          iNVals = NPeriods(ThisFile%Interval_InMinutes, '12/31/2000_24:00', '12/31/2030_24:00')      ! Monthly or longer  use 30 years
      END IF
      
      !Start timestamp for cache (apply Year4000 if needed)
      IF (ThisFile%Year4000Flag(1)) THEN
          cWorkTS       = TimeStampToYear4000(ThisFile%cMatrixCacheStartTS)
          iNVals        = NPeriods(ThisFile%Interval_InMinutes,cWorkTS,'12/31/4000_24:00') + 1   ! Number of periods between first data timestamp and the end of year 4000
          iNValsOneYear = NPeriods(ThisFile%Interval_InMinutes,'12/31/3999_24:00','12/31/4000_24:00') ! One year worth of periods 
          IF (iNVals .EQ. iNValsOneYear) ThisFile%lMartixRecycle = .TRUE.   ! recycle catche matix for case 4000
          
      ELSE
          cWorkTS = ThisFile%cMatrixCacheStartTS
      END IF
      
      cWorkSimDate = DSSStyleDate(cWorkTS) !convert to dss style to use in zrrts
      cWorkSimTime = DSSStyleHoursAfterMidnight(cWorkTS)
      
      ! Allocate and initilaze cache data array and make sure it is empty
      DEALLOCATE(ThisFile%rMatrixCacheData , STAT=iErrorCode)
      ALLOCATE(ThisFile%rMatrixCacheData(iNRow,iNCol,iNVals))   
      ALLOCATE(rValues(iNVals))

      DO indx=1,SIZE(ThisFile%PathNames)
          
          iColcounter = iColcounter + 1
          IF (iColcounter .GT. iNCol) THEN
              iRowcounter = iRowcounter + 1
              iColcounter = 1
          END IF         
          
          CALL ZRRTS(ThisFile%pFileData%IFLTAB, &
                     ThisFile%PathNames(indx) , &
                     cWorkSimDate             , &
                     cWorkSimTime             , &
                     iNVals                   , &
                     rValues                  , &
                     cDataUnit                , &
                     cDataType                , &
                     iDataTimeOffset          , &
                     iErrorCode               )

          !Accept 0 and the “some missing” cases (<=3) same as  IdentifyFirstDataDate logic
          IF (.NOT.(iErrorCode .EQ. 0 .OR. iErrorCode .LE. 3)) THEN
              MessageArray(1) = 'Error building DSS matrix cache for pathname'
              MessageArray(2) = TRIM(ThisFile%PathNames(indx))
              CALL ThisFile%Logger%SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
              iStat = -1
              DEALLOCATE(rValues)
              RETURN
          END IF
                    
     
          ThisFile%rMatrixCacheData(iRowcounter,iColcounter,:) = REAL(rValues,8)
        
          
      END DO

      DEALLOCATE(rValues)
      ThisFile%iMatrixCacheNPeriods = iNVals
      ThisFile%lMatrixCacheBuilt   = .TRUE.

    END SUBROUTINE BuildMatrixCache
    
    
    !################################################################
    !### Read data from DSS matrix cache
    !################################################################
    SUBROUTINE ReadMatrixFromCache(ThisFile,cSimulationTime,iNRow,iNCol,Data,iFileReadCode,iStat)
      TYPE(DSSInFileType),INTENT(INOUT) :: ThisFile
      CHARACTER(LEN=*),INTENT(IN)       :: cSimulationTime
      INTEGER,INTENT(IN)                :: iNRow,iNCol
      CLASS(*),INTENT(OUT)              :: Data(iNRow,iNCol)
      INTEGER,INTENT(OUT)               :: iFileReadCode,iStat
      
      !Local variables
      INTEGER                           :: iDay,iMonth,iYear,iNValsOneYear,iIWFMJulianDate,iNPeriods,iDummy  
      REAL(8)                           :: rSlice(iNRow,iNCol)
      CHARACTER(LEN=f_iTimeStampLength) :: cLastDataDateAndTime
      LOGICAL                           :: lisFeb28
      ! Note: JLIYMD and INCTIM are provided by heclib_compat module

      iStat         = 0
      iFileReadCode = f_IO_SuccessfulRead
      iNPeriods     = 1
      lisFeb28      = .FALSE.

      ! Rebuild cache  and reset pointer if needed
      IF (ThisFile%iMatrixCachePointer .GT. ThisFile%iMatrixCacheNPeriods) THEN 
          
          ThisFile%iMatrixCachePointer =  1
          ThisFile%cMatrixCacheStartTS = cSimulationTime
          IF (ThisFile%Year4000Flag(1)) THEN
              iNValsOneYear = NPeriods(ThisFile%Interval_InMinutes,'01/01/4000_24:00','01/01/4001_24:00')
              iDummy        = INCTIM(ThisFile%Interval_InMinutes               , &
                                     0                                         , &
                                     -iNValsOneYear                            , &
                                     ThisFile%iMatrixCacheHECJulianDay         , &
                                     ThisFile%iMatrixCacheMinutesAfterMidnight , &
                                     ThisFile%iMatrixCacheHECJulianDay         , &
                                     ThisFile%iMatrixCacheMinutesAfterMidnight )  !bring it back to begining if year 4000
          END IF   
          IF (.NOT. ThisFile%lMartixRecycle) THEN       ! for special case year 4000
              CALL BuildMatrixCache(ThisFile,iNRow,iNCol,iStat) 
              IF (iStat .NE. 0) THEN
                  iFileReadCode = f_IO_TSData_DssReadError
                  RETURN
              END IF
          END IF
      END IF
          
      !Convert DSSHECjulian format to IWFMJulian format to use in main subroutine
      iDummy = JLIYMD(ThisFile%iMatrixCacheHECJulianDay,iYear,iMonth,iDay)   !                                           !Find the day,month,year of updated HEC style Julian date
      CALL DayMonthYearToJulianDate(iDay,iMonth,iYear,iIWFMJulianDate)                                                     !Convert date of data read to IWFM style Julian date
      cLastDataDateAndTime = JulianDateAndMinutesToTimeStamp(iIWFMJulianDate,ThisFile%iMatrixCacheMinutesAfterMidnight)
      IF (cLastDataDateAndTime == "02/28/4000_24:00") lisFeb28 = .TRUE.  ! for monthly it checks 02/29/4000 so thre is no problem
      CALL AdjustTimeStampWithYear4000(cLastDataDateAndTime,cSimulationTime,ThisFile%Year4000Flag(1)) !Convert year-4000 to 
      IF (.NOT. IsLeapYear(ExtractYear(cLastDataDateAndTime)) .AND. lisFeb28) THEN  ! if 28 feb and normal year the skip Feb 29
          iNPeriods = NPeriods(ThisFile%Interval_InMinutes,'02/28/4000_24:00','02/29/4000_24:00') + 1  ! arbitrary date woth one day of data
      END IF
      ThisFile%JulianOfLastDataRead = TimeStampToJulian(cLastDataDateAndTime) 
     
      rSlice = ThisFile%rMatrixCacheData(:,:,ThisFile%iMatrixCachePointer)
     
      
      !Apply rate-type adjustment at the timestep level 
      IF (ALLOCATED(ThisFile%RateTypeData)) THEN
          CALL AdjustRateTypeData(rSlice                                      , &
                                  ThisFile%RateTypeData                       , &
                                  iDataInterval = ThisFile%Interval_InMinutes , &
                                  cLastDataDate = cLastDataDateAndTime        )
      END IF

      SELECT TYPE (Data)
          TYPE IS (REAL(8))
              Data = rSlice
          TYPE IS (INTEGER)
              Data = INT(rSlice)
          CLASS DEFAULT
              CALL ThisFile%Logger%SetLastMessage('The specified data type cannot be read from file '//ThisFile%pFileData%Name//'!',f_iFatal,ThisProcedure)
              iStat         = -1
              iFileReadCode = f_IO_TSData_DssReadError
              RETURN
      END SELECT

      !increase CatcheMatrix timestep and counter (update iMatrixCacheHECJulianDay)
      iDummy                       = INCTIM(ThisFile%Interval_InMinutes               , &
                                            0                                         , &
                                            iNPeriods                                 , &
                                            ThisFile%iMatrixCacheHECJulianDay         , &
                                            ThisFile%iMatrixCacheMinutesAfterMidnight , &
                                            ThisFile%iMatrixCacheHECJulianDay         , &
                                            ThisFile%iMatrixCacheMinutesAfterMidnight )  !Increment HECJulianDate by ofsset to bring to the DSS data timestamp
      ThisFile%iMatrixCachePointer = ThisFile%iMatrixCachePointer + iNPeriods

    END SUBROUTINE ReadMatrixFromCache

  END SUBROUTINE ReadRegularIntervalMatrix_DssInFile



  ! -------------------------------------------------------------
  ! --- READ ENTIRE REGULAR INTERVAL TIME SERIES ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadEntireTSD_DSSInFile(ThisFile,PathName,NVals,BDate,BTime,TSValues,Stat)
    CLASS(DssInFileType)         :: ThisFile
    INTEGER                      :: NVals
    CHARACTER(LEN=80),INTENT(IN) :: PathName
    CHARACTER(LEN=*),INTENT(IN)  :: BDate
    CHARACTER(LEN=4),INTENT(IN)  :: BTime
    REAL(8),INTENT(OUT)          :: TSValues(NVals)
    INTEGER,INTENT(OUT)          :: Stat

    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMeTER :: ThisProcedure = ModNAme // 'ReadEntireTSD_DSSInFile'

    !Initialize
    Stat =- 99

    !Read data
    CALL ReadDataMod(PathName,NVals,BDate,BTime)


  CONTAINS


    !####################################################
    ! --- READ ENTIRE REGULAR INTERVAL TIME SERIES DATA
    !####################################################
    SUBROUTINE ReadDataMod(PathName,NVals,BDate,BTime)
      INTEGER                     :: NVals
      CHARACTER(LEN=*),INTENT(IN) :: PathName,BDate,BTime

      !Local variables
      INTEGER   :: DataTimeOffset,ErrorCode
      CHARACTER :: DataUnit*8,DataType*8,WorkSimDate*20,WorkSimTime*4
      REAL      :: ValuesLocal(NVals)


      !Initialize variables
      WorkSimDate = BDate
      WorkSimTime = BTime

      !Read data
      CALL ZRRTS(ThisFile%pFileData%IFLTAB, &
                 PathName                 , &
                 WorkSimDate              , &
                 WorkSimTime              , &
                 NVALS                    , &
                 ValuesLocal              , &
                 DataUnit                 , &
                 DataType                 , &
                 DataTimeOffset           , &
                 ErrorCode                )
      TSValues = REAL(ValuesLocal,8)

      !Check if the data was read without any problems
      IF (ErrorCode .NE. 0) THEN
          Stat=2
          MessageArray(1) = 'Error in reading data from pathname'
          MessageArray(2) = TRIM(PathName)
          CALL ThisFile%Logger%LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
          RETURN
      ELSE
          Stat=0
      END IF

    END SUBROUTINE ReadDataMOD

  END SUBROUTINE ReadEntireTSD_DSSInFile


  ! -------------------------------------------------------------
  ! --- READ REGULAR INTERVAL TIME SERIES DATA FOR A PATHNAME FOR A TIME RANGE
  ! -------------------------------------------------------------
  SUBROUTINE ReadTimeSeries_ForTimeRange_DSSInFile(ThisFile,iPathNameIndex,cBeginDateAndTime,cEndDateAndTime,nActualOutput,Data,rDataDates,Stat,iStat)
    CLASS(DssInFileType)        :: ThisFile
    INTEGER,INTENT(IN)          :: iPathNameIndex
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    CLASS(*),INTENT(OUT)        :: Data(:)
    REAL(8),INTENT(OUT)         :: rDataDates(:)
    INTEGER,INTENT(OUT)         :: nActualOutput,Stat,iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+37),PARAMETER :: ThisProcedure = ModName // 'ReadTimeSeries_ForTimeRange_DSSInFile'
    INTEGER                                :: nData,ErrorCode,indx,DataTimeOffset
    REAL,ALLOCATABLE                       :: rValuesLocal(:)
    REAL(8)                                :: rData(SIZE(Data)),rReadValues(SIZE(ThisFile%PathNames))
    CHARACTER(LEN=f_iTimeStampLength)      :: cCurrentDateAndTime
    CHARACTER(20)                          :: BeginDate
    CHARACTER(4)                           :: BeginTime
    CHARACTER(8)                           :: DataUnit,DataType
    REAL,PARAMETER                         :: r901 = -901.0 , &
                                              r902 = -902.0

    !Initialize
    iStat               = 0
    Stat                = 0
    nActualOutput       = 0
    cCurrentDateAndTime = cBeginDateAndTime

    !Number of periods between beginning and ending output; adjust based on if data is given with year 4000 flag or not
    IF (ThisFile%Year4000Flag(iPathNameIndex)) THEN
        DO
            CALL ReadRegularIntervalArray_DssInFile(ThisFile,cCurrentDateAndTime,rReadValues,Stat,iStat=iStat)
            IF (iStat .EQ. -1) RETURN
            IF (Stat .NE. 0) RETURN
            nActualOutput             = nActualOutput + 1
            rData(nActualOutput)      = rReadValues(iPathNameIndex)
            rDataDates(nActualOutput) = TimeStampToJulian(cCurrentDateAndTime)
            cCurrentDateAndTime       = IncrementTimeStamp(cCurrentDateAndTime,ThisFile%Interval_InMinutes,1)
            IF (cCurrentDateAndTime .TSGT. cEndDateAndTime) EXIT
        END DO

        !Transfer data to return variable
        SELECT TYPE (Data)
            TYPE IS (REAL(8))
                Data = rData
        END SELECT

    ELSE
        !Number of data intervals between adjusted output begin date and end date
        nData = NPeriods(ThisFile%Interval_InMinutes,cBeginDateAndTime,cEndDateAndTime) + 1

        !Define date read begin date and time
        BeginDate = DSSStyleDate(cBeginDateAndTime)
        BeginTime = DSSStyleHoursAfterMidnight(cBeginDateAndTime)

        !Read data
        ALLOCATE (rValuesLocal(nData))
        CALL ZRRTS(ThisFile%pFileData%IFLTAB          , &
                   ThisFile%PathNames(iPathNameIndex) , &
                   BeginDate                          , &
                   BeginTime                          , &
                   nData                              , &
                   rValuesLocal                       , &
                   DataUnit                           , &
                   DataType                           , &
                   DataTimeOffset                     , &
                   ErrorCode                          )

        !Process error code
        SELECT CASE (ErrorCode)
            CASE (0)  !No errors
                rData(1:nData) = rValuesLocal
                nActualOutput  = nData
                !Define data dates
                DO indx=1,nActualOutput
                    rDataDates(indx)    = TimeStampToJulian(cCurrentDateAndTime)
                    cCurrentDateAndTime = IncrementTimeStamp(cCurrentDateAndTime,ThisFile%Interval_InMinutes,1)
                END DO

            CASE (1,2,3,4)  !Data came in with missing values
                DO indx=1,nData
                    IF (rValuesLocal(indx).NE.r901  .AND.  rValuesLocal(indx).NE.r902) THEN
                        nActualOutput             = nActualOutput + 1
                        rData(nActualOutput)      = rValuesLocal(indx)
                        rDataDates(nActualOutput) = TimeStampToJulian(cCurrentDateAndTime)
                    END IF
                    cCurrentDateAndTime = IncrementTimeStamp(cCurrentDateAndTime,ThisFile%Interval_InMinutes,1)
                END DO

        END SELECT

        !Store data in the return variable
        SELECT TYPE (Data)
            TYPE IS (REAL(8))
                Data = rData
        END SELECT

    END IF

  END SUBROUTINE ReadTimeSeries_ForTimeRange_DSSInFile





! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE REGULAR INTERVAL ARRAY TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteRegularIntervalArray_DssOutFile(ThisFile,SimulationTime,Data,FinalPrint)
    CLASS(DssOutFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(IN)         :: Data(:)
    LOGICAL,INTENT(IN)          :: FinalPrint

    !Local variables
    CHARACTER(LEN=ModNameLen+36),PARAMETER :: ThisProcedure = ModName // 'WriteRegularIntervalArray_DssOutFile'
    REAL(8)                                :: rData(1,SIZE(Data))

    !Initialize
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            rData(1,:) = Data
            CALL WriteRegularIntervalMatrix_DssOutFile(ThisFile,SimulationTime,rData,FinalPrint)

        CLASS DEFAULT
            CALL ThisFile%Logger%LogMessage('Array of data type cannot be written to file '//ThisFile%pFileData%Name//'!',f_iWarn,ThisProcedure)

    END SELECT

  END SUBROUTINE WriteRegularIntervalArray_DssOutFile


  ! -------------------------------------------------------------
  ! --- WRITE REGULAR INTERVAL MATRIX TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteRegularIntervalMatrix_DssOutFile(ThisFile,SimulationTime,Data,FinalPrint)
    CLASS(DssOutFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(IN)         :: Data(:,:)
    LOGICAL,INTENT(IN)          :: FinalPrint

    !Local variables
    CHARACTER(LEN=ModNameLen+37),PARAMETER         :: ThisProcedure = ModName // 'WriteRegularIntervalMatrix_DssOutFile'
    INTEGER,PARAMETER                              :: IPLAN=0
    INTEGER                                        :: DataPointer,NumberOfDataBatch,indx,indx_row,indx_col,indxDU,indxDT,NValues,  &
                                                      NPathNames,SizeDU,SizeDT,ErrorCode
    INTEGER(C_LONG_LONG)                           :: IFLTAB(250)
    CHARACTER(LEN=f_iDefaultPathNameLength)        :: PathName
    CHARACTER                                      :: StartDate*20,StartTime*4
    CHARACTER(LEN=f_iDefaultDataUnitAndTypeLength) :: DataUnit,DataType
    REAL                                           :: Values(ThisFile%NumberOfDataBatch)

    !Initialize variables
    DataPointer       = ThisFile%DataPointer  ;  IF (DataPointer .EQ. 0) ThisFile%DataStartDateAndTime = SimulationTime
    NumberOfDataBatch = ThisFile%NumberOfDataBatch

    !Update the stored results
    DataPointer = DataPointer + 1
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            ThisFile%ValuesForOutput(:,:,DataPointer) = Data
        CLASS DEFAULT
            CALL ThisFile%Logger%LogMessage('Array of data type acnnot be written to file '//ThisFile%pFileData%Name//'!',f_iWarn,ThisProcedure)
            RETURN
    END SELECT

    !If this is the first entry in the storage, also store the date and time
    !IF (ThisFile%DataStartDateAndTime .EQ. '') ThisFile%DataStartDateAndTime = SimulationTime

    !If the storage is full or it is the end of simulation, dump the contents into the file
    IF (DataPointer.EQ.NumberOfDataBatch .OR. FinalPrint) THEN
      !Number of values to store in each path
      IF (FinalPrint) THEN
        NValues = DataPointer
      ELSE
        NValues = NumberOfDataBatch
      END IF
      IF (NValues .EQ. 0) RETURN

      !Obtain DSS style date of first value in the storage
      StartDate = DSSStyleDate(ThisFile%DataStartDateAndTime)

      !Obtain start time in 24 hour style
      StartTime = DSSStyleHoursAfterMidnight(ThisFile%DataStartDateAndTime)

      !IFLTAB
      IFLTAB = ThisFile%pFileData%IFLTAB

      !Number of path names to be accessed
      NPathNames = SIZE(ThisFile%PathNames)

      !Size of the DataUnit and DataType arrays
      SizeDU = SIZE(ThisFile%DataUnit)
      SizeDT = SIZE(ThisFile%DataType)

      !Print out the values
      indx = 0
      DO indx_row=1,ThisFile%NumberOfDataRows
          DO indx_col=1,ThisFile%NumberOfDataColumns
              indx              = indx+1
              indxDU            = MOD(indx,SizeDU) ; IF (indxDU .EQ. 0) indxDU = SizeDU ; DataUnit = ThisFile%DataUnit(indxDU)
              indxDT            = MOD(indx,SizeDT) ; IF (indxDT .EQ. 0) indxDT = SizeDT ; DataType = ThisFile%DataType(indxDT)
              PathName          = ThisFile%PathNames(indx)
              Values(1:NValues) = REAL(ThisFile%ValuesForOutput(indx_row,indx_col,1:NValues),4)
              CALL ZSRTS(IFLTAB           , &
                         PathName         , &
                         StartDate        , &
                         StartTime        , &
                         NValues          , &
                         Values(1:NValues), &
                         DataUnit         , &
                         DataType         , &
                         IPLAN            , &
                         ErrorCode        )
              IF (ErrorCode .GT. 10) THEN
                  MessageArray(1) = 'Error in writing data to path for file '//ThisFile%pFileData%Name
                  MessageArray(2) = PathName
                  CALL ThisFile%Logger%LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
                  RETURN
              END IF
          END DO
      END DO
      !Initialize the data storage
      DataPointer               = 0
      ThisFile%pFileData%IFLTAB = IFLTAB
    END IF

    !Update DataPointer field
    ThisFile%DataPointer = DataPointer

  END SUBROUTINE WriteRegularIntervalMatrix_DssOutFile


  ! -------------------------------------------------------------
  ! --- WRITE ENTIRE REGULAR INTERVAL TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteEntireTSD_DSSOutFile(ThisFile,r,PathName,NValues,BeginDate,BeginTime)
    CLASS(DssOutFileType) :: ThisFile
    INTEGER,INTENT(IN)    :: NValues
    REAL(8),INTENT(IN)    :: r(NValues)
    CHARACTER,INTENT(IN)  :: PathName*80,BeginDate*10,BeginTime*4

    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER         :: ThisProcedure = ModName // 'WriteEntireTSD_DSSOutFile'
    INTEGER,PARAMETER                              :: IPLAN=0
    INTEGER                                        :: SizeDU,SizeDT,ErrorCode
    INTEGER(C_LONG_LONG)                           :: IFLTAB(250)
    CHARACTER(LEN=f_iDefaultDataUnitAndTypeLength) :: DataUnit,DataType
    REAL                                           :: Values(NValues)

    !Initialize
    Values  = REAL(r,4)
    IFLTAB  = ThisFile%pFileData%IFLTAB

    !Size of the DataUnit and DataType arrays
    SizeDU   = SIZE(ThisFile%DataUnit)
    SizeDT   = SIZE(ThisFile%DataType)
    DataUnit = ThisFile%DataUnit(1)
    DataType = ThisFile%DataType(1)

    !Print out the values
    CALL ZSRTS(IFLTAB           , &
               PathName         , &
               BeginDate        , &
               BeginTime        , &
               NValues          , &
               Values           , &
               DataUnit         , &
               DataType         , &
               IPLAN            , &
               ErrorCode        )
    IF (ErrorCode .GT. 10) THEN
        MessageArray(1) = 'Error in writing data to pathname in file'//ThisFile%pFileData%Name
        MessageArray(2) = PathName
        CALL ThisFile%Logger%LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
        RETURN
    END IF
    ThisFile%pFileData%IFLTAB = IFLTAB

  END SUBROUTINE WriteEntireTSD_DSSOutFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF A DSS FILE IS ALREADY OPENED
  ! -------------------------------------------------------------
  SUBROUTINE CheckOpenStatus(FileName,FileIsOpen,DSSFileIndex)
    CHARACTER(LEN=*),INTENT(IN) :: FileName
    LOGICAL,INTENT(OUT)         :: FileIsOpen
    INTEGER,INTENT(OUT)         :: DSSFileIndex

    !Local variables
    TYPE(DSSFileListNodeType),POINTER :: CurrentNode

    !Initialize
    CurrentNode  => DSSFileListHead
    FileIsOpen   = .FALSE.
    DSSFileIndex = 0

    !Check open status
    IF (.NOT. ASSOCIATED(CurrentNode)) RETURN
    IF (CurrentNode%DSSFileIndex .LT. 1) RETURN
    DO
      IF (TRIM(ADJUSTL(UpperCase(NativePathName(FileName)))) .EQ. TRIM(ADJUSTL(UpperCase(CurrentNode%Name)))) THEN
          FileIsOpen   = .TRUE.
          DSSFileIndex = CurrentNode%DSSFileIndex
          RETURN
      END IF
      IF (.NOT. ASSOCIATED(CurrentNode%Next)) EXIT
      IF (CurrentNode%Next%DSSFileIndex .LT. 1) EXIT
      CurrentNode => CurrentNode%Next
    END DO

  END SUBROUTINE CheckOpenStatus


  ! -------------------------------------------------------------
  ! --- GIVEN A DSS FILE INDEX, LOCATE IT IN THE DSS FILE LIST
  ! -------------------------------------------------------------
  SUBROUTINE GetDSSFile_ByIndex(DSSFileIndex,DSSFile,Logger,iStat)
    INTEGER,INTENT(IN)                            :: DSSFileIndex
    TYPE(DSSFileListNodeType),POINTER,INTENT(OUT) :: DSSFile
    TYPE(MessageLoggerType),TARGET,INTENT(INOUT)  :: Logger
    INTEGER,INTENT(OUT)                           :: iStat

    !Local variables
    CHARACTER(LEN=ModNAmeLen+18),PARAMETER :: ThisProcedure = ModName // 'GetDSSFile_ByIndex'

    !Initialize
    iStat   =  0
    DSSFile => DSSFileListHead

    !Find the file in the list
    DO
        IF (DSSFile%DSSFileIndex .EQ. DSSFileIndex) RETURN
        IF (.NOT. ASSOCIATED(DSSFile%Next)) THEN
            CALL Logger%SetLastMessage('File with index '//TRIM(IntToText(DSSFileIndex))//' cannot be located in the list of opened DSS files!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        DSSFile => DSSFile%Next
    END DO

  END SUBROUTINE GetDSSFile_ByIndex


  ! -------------------------------------------------------------
  ! --- GIVEN A DSS FILE NAME, LOCATE IT IN THE DSS FILE LIST
  ! -------------------------------------------------------------
  SUBROUTINE GetDSSFile_ByName(FileName,DSSFile,Logger,iStat)
    CHARACTER(LEN=*),INTENT(IN)                   :: FileName
    TYPE(DSSFileListNodeType),POINTER,INTENT(OUT) :: DSSFile
    TYPE(MessageLoggerType),TARGET,INTENT(INOUT)  :: Logger
    INTEGER,INTENT(OUT)                           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'GetDSSFile_ByName'

    !Initialize
    iStat   =  0
    DSSFile => DSSFileListHead

    !Find the file in the list
    DO
        IF (ALLOCATED(DSSFile%Name)) THEN
            IF (DSSFile%Name .EQ. FileName) RETURN
        END IF
        IF (.NOT. ASSOCIATED(DSSFile%Next)) THEN
            CALL Logger%SetLastMessage('File '//TRIM(FileName)//' cannot be located in the list of opened DSS files!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        DSSFile => DSSFile%Next
    END DO

  END SUBROUTINE GetDSSFile_ByName


  ! -------------------------------------------------------------
  ! --- CHECK THE LENGTH OF DATA UNIT AND TYPE AGAINST THE DEFAULT
  ! -------------------------------------------------------------
  SUBROUTINE CheckLength(String,Logger,iStat)
    CHARACTER(LEN=*),INTENT(IN)                  :: String
    TYPE(MessageLoggerType),TARGET,INTENT(INOUT)  :: Logger
    INTEGER,INTENT(OUT)                           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+11),PARAMETER :: ThisProcedure = ModName // 'CheckLength'

    !Initialize
    iStat = 0

    IF (LEN_TRIM(ADJUSTL(String)) .GT. f_iDefaultDataUnitAndTypeLength) THEN
      CALL Logger%SetLastMessage('DataUnit or DataType is longer than allowed!',f_iFatal,ThisProcedure)
      iStat = -1
    END IF

  END SUBROUTINE CheckLength


  ! -------------------------------------------------------------
  ! --- DELETE RECORDS, IF THEY EXIST  ***
  ! --- *** Adopted from ZSRTSX routine of HECLIB
  ! -------------------------------------------------------------
  SUBROUTINE DeleteRecords(OldFile,PathName,StartDate,StartTime,NValues,iStat)
    TYPE(DSSOutFileType)        :: OldFile
    CHARACTER(LEN=*),INTENT(IN) :: PathName(:),StartDate,StartTime
    INTEGER,INTENT(IN)          :: NValues
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=MoDNameLen+13),PARAMETER  :: ThisProcedure = MoDName // 'DeleteRecords'
    INTEGER                                 :: indx,Interval,DummyInt,JULS,ISTIME,IOFSET,JULE,IETIME,    &
                                               IYR,IMON,IDAY,JYR,JMON,JDAY,IBLOCK,JULSD,JULAST,IDTYPE,   &
                                               NDAY,JUL,LenRecordToDelete,ErrorCode, &
                                               ScratchFileUnitN,iHour,iMin
    INTEGER(C_LONG_LONG)                    :: IFLTAB_Scratch(250)
    CHARACTER(LEN=32)                       :: APart,BPart,CPart,DPart,EPart,FPart
    CHARACTER(LEN=9)                        :: CDATE1
    CHARACTER(LEN=f_iDefaultPathNameLength) :: RecordToDelete
    CHARACTER(LEN=3)                        :: CDTYPE
    LOGICAL                                 :: Deleted,RecordFound,ScratchFileCreated
    INTEGER,PARAMETER                       :: KBUFF1=1500,KBUFF2=1500
    REAL                                    :: BUFF1(KBUFF1),BUFF2(KBUFF2)

    !Initialize
    iStat              = 0
    ScratchFileCreated = .FALSE.

    !Get time window
    CALL DayMonthYearToJulianDate(ExtractDay(StartDate),ExtractMonth(StartDate),ExtractYear(StartDate),JULS,ErrorCode)
    !CALL DATJUL ( StartDate, JULS, ErrorCode)
    READ (StartTime(1:2),*) iHour
    READ (StartTime(3:4),*) iMin
    ISTIME = iHour*60 + iMin
    IF (ISTIME .EQ. 0) THEN
        JULS   = JULS - 1
        ISTIME = 1440
    END IF

    !Find block dates, re-do the pathnames and delete records
    DO indx=1,SIZE(PathName)

      !Check if pathname exists
      CALL ZDTYPE(OldFile%pFileData%IFLTAB,PathName(indx),DummyInt,RecordFound,CDTYPE,IDTYPE)
      IF (IDTYPE .EQ. 0) CYCLE

      ScratchFileCreated = .TRUE.
      CALL ZUFPN(APart ,DummyInt          , &
                 BPart ,DummyInt          , &
                 CPart ,DummyInt          , &
                 DPart ,DummyInt          , &
                 EPart ,DummyInt          , &
                 FPart ,DummyInt          , &
                 PathName(indx)           , &
                 LEN_TRIM(PathName(indx)) , &
                 ErrorCode                )
      IF (ErrorCode .NE. 0) THEN
          CALL OldFile%Logger%SetLastMessage('Error in finding the interval length of the pathname '//TRIM(PathName(indx))//'!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      ErrorCode = 1
      CALL ZGINTL(Interval,EPart,DummyInt,ErrorCode)

      !If the time is not on the standard boundaries, adjust it and determine the time offset, in minutes.
      CALL ZOFSET (JULS,ISTIME,Interval,1,IOFSET)

      !Compute the ending time from the number of values
      CALL IncrementJulianDateAndMinutesAfterMidnight(Interval,NValues-1,JULS,ISTIME,JULE,IETIME)

      !Obtain the date of the first block
      CALL ZBEGDT (JULS, Interval, IYR, IMON, IDAY, IBLOCK, OldFile%pFileData%IFLTAB(2))
      CALL DayMonthYearToJulianDate(IDAY,IMON,IYR,JULSD)

      !Get the date of the last block
      CALL ZBEGDT (JULE, Interval, JYR, JMON, JDAY, IBLOCK, OldFile%pFileData%IFLTAB(2))
      CALL DayMonthYearToJulianDate(JDAY,JMON,JYR,JULAST)

      !Loop over blocks
      DO
          !Get the new D (Date) part
          CDATE1 = DSSStyleDate(IDAY,IMON,IYR)
          CALL DayMonthYearToJulianDate(IDAY,IMON,IYR,JULSD)

          !Form the new pathname
          RecordToDelete = ''
          CALL ZPATH (APart,BPart,CPart,CDATE1,EPart,FPart,RecordToDelete,LenRecordToDelete)

          !Get the time of the next data block
          CALL ZINCBK (IBLOCK, JUL, IYR, IMON, IDAY)

          !If weekly data, set so that the data is always on Saturday at 2400 hours.
          IF (Interval .EQ. 10080) THEN
              NDAY   = MOD(JUL,7) + 1
              JUL   = JUL - NDAY + 1
              NDAY  = MOD(JULSD,7) + 1
              JULSD = JULSD - NDAY + 1
          END IF

          !Delete the record
          CALL ZDELET (OldFile%pFileData%IFLTAB,RecordToDelete,LenRecordToDelete,Deleted)

          !Quit if all data blocks are found and deleted
          IF (JUL .GT. JULAST) EXIT

      END DO

    END DO

    IF (.NOT. ScratchFileCreated) RETURN

    !Open a temporary DSS file
    ScratchFileUnitN = GetAUnitNumber()
    CALL ZSET('UNIT','',ScratchFileUnitN)
    CALL ZOPEN(IFLTAB_Scratch,'DSSScratch.DSS',ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL OldFile%Logger%SetLastMessage('Error in opening temporary file to squeeze '//TRIM(OldFile%pFileData%Name),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Copy the records to scratch file and squeeze out the deleted records
    CALL ZSET('COMP','ON',0)
    CALL ZCOFIL(OldFile%pFileData%IFLTAB,IFLTAB_Scratch,BUFF1,KBUFF1,BUFF2,KBUFF2,.FALSE.,.FALSE.)

    !Close the old file
    CALL ZCLOSE(OldFile%pFileData%IFLTAB)

    !Delete old file; then re-open it
    OPEN (UNIT=OldFile%pFileData%UnitN , FILE=OldFile%pFileData%Name)  ;  CLOSE (UNIT=OldFile%pFileData%UnitN , STATUS='DELETE')
    CALL ZSET('UNIT','',OldFile%pFileData%UnitN)
    CALL ZOPEN(OldFile%pFileData%IFLTAB,OldFile%pFileData%Name,ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL OldFile%Logger%SetLastMessage('Error in opening file '//TRIM(OldFile%pFileData%Name)//' after squeezing!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Copy the records from scratch file to the re-opened old file
    CALL ZSET('COMP','ON',0)
    CALL ZCOFIL(IFLTAB_Scratch,OldFile%pFileData%IFLTAB,BUFF1,KBUFF1,BUFF2,KBUFF2,.FALSE.,.FALSE.)

    !Close and delete the scratch file
    CALL ZCLOSE(IFLTAB_Scratch)
    OPEN (UNIT=ScratchFileUnitN , FILE='DSSScratch.DSS' , IOSTAT=ErrorCode)  ;  CLOSE (UNIT=ScratchFileUnitN , STATUS='DELETE' , IOSTAT=ErrorCode)

  END SUBROUTINE DeleteRecords


  ! -------------------------------------------------------------
  ! --- FORM PATH NAME
  ! -------------------------------------------------------------
  FUNCTION FormPathName(PartNames) RESULT(CPATH)
    CHARACTER(LEN=*),INTENT(IN)             :: PartNames(6)
    CHARACTER(LEN=f_iDefaultPathNameLength) :: CPATH

    !Local variables
    INTEGER                                 :: LengthOfCPath
    CHARACTER(LEN=f_iDefaultPartNameLength) :: LocalPartNames(6)

    !Initialize
    LocalPartNames = ADJUSTL(PartNames)

    !Form the CPATH by a call to HEC-DSS library function
    CALL ZPATH(LocalPartNames(1) ,&
               LocalPartNames(2) ,&
               LocalPartNames(3) ,&
               LocalPartNames(4) ,&
               LocalPartNames(5) ,&
               LocalPartNames(6) ,&
               CPATH             ,&
               LengthOfCPath     )

  END FUNCTION FormPathName


  ! -------------------------------------------------------------
  ! --- REWIND DSS INPUT FILE
  ! --- Note: DSS files are not really rewound; the relevant arguments are 
  ! ---       set properly so that the data can be read into cache in a 
  ! ---       random fashion
  ! -------------------------------------------------------------
  SUBROUTINE Rewind_To_BeginningOfTSData(ThisFile)
    CLASS(DSSInFileType) :: ThisFile

    !Local variables
    TYPE(DSSInFileType) :: Dummy

    ThisFile%JulianOfLastDataRead             = -HUGE(0d0)
    ThisFile%lMatrixCacheBuilt                = Dummy%lMatrixCacheBuilt
    ThisFile%iMatrixCacheNPeriods             = Dummy%iMatrixCacheNPeriods
    ThisFile%iMatrixCachePointer              = Dummy%iMatrixCachePointer
    ThisFile%iMatrixCacheHECJulianDay         = Dummy%iMatrixCacheHECJulianDay                                                      
    ThisFile%iMatrixCacheMinutesAfterMidnight = Dummy%iMatrixCacheMinutesAfterMidnight            
    ThisFile%cMatrixCacheStartTS              = ThisFile%cFirstDataDate                                

  END SUBROUTINE Rewind_To_BeginningOfTSData


END MODULE
