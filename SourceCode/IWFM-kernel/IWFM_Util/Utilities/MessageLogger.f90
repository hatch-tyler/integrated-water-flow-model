!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2025
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
!
! Refactored to type-based design (no module-level SAVE state for the logger).
! MessageLoggerType encapsulates all logger state as a derived type.
! Backward-compatible wrapper procedures delegate to DefaultLogger.
!
!***********************************************************************
MODULE MessageLogger
  USE ProgramTimer
  USE ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
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

  ! New type-based API
  PUBLIC :: MessageLoggerType
  PUBLIC :: DefaultLogger

  ! Backward-compatible wrapper procedures and constants
  PUBLIC :: SetLogFileName               , &
            SetFlagToEchoProgress        , &
            SetDefaultMessageDestination , &
            SetLastMessage               , &
            GetLastMessage               , &
            GetLogFileUnit               , &
            KillLogFile                  , &
            LogMessage                   , &
            LogLastMessage               , &
            IsLogFileDefined             , &
            PrintRunTime                 , &
            MessageArray                 , &
            EchoProgress                 , &
            GetMessageCounts             , &
            f_iYesEchoProgress           , &
            f_iNoEchoProgress            , &
            f_iSCREEN_FILE               , &
            f_iSCREEN                    , &
            f_iFILE                      , &
            f_iMessage                   , &
            f_iInfo                      , &
            f_iWarn                      , &
            f_iFatal                     , &
            f_lLogPerfMarkers


  ! -------------------------------------------------------------
  ! --- FLAG DEFINITIONS TO ECHO PROGRAM PROGRESS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER      :: f_iYesEchoProgress = 1  , &
                            f_iNoEchoProgress  = 0


  ! -------------------------------------------------------------
  ! --- FLAGS FOR MESSAGE DESTINATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iSCREEN_FILE = 1 , &
                       f_iSCREEN      = 2 , &
                       f_iFILE        = 3


  ! -------------------------------------------------------------
  ! --- MESSAGE SEVERITY LEVELS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iMessage = 0 , &
                       f_iInfo    = 1 , &
                       f_iWarn    = 2 , &
                       f_iFatal   = 3


  ! -------------------------------------------------------------
  ! --- PERFORMANCE PROFILING MARKERS
  ! ---
  ! --- Set to .TRUE. to enable [PERF] init-timing markers in the
  ! --- simulation log. Off in production; flip to .TRUE. and
  ! --- rebuild when profiling startup costs.
  ! -------------------------------------------------------------
  LOGICAL,PARAMETER :: f_lLogPerfMarkers = .FALSE.


  ! -------------------------------------------------------------
  ! --- DATA DEFINITIONS
  ! -------------------------------------------------------------
  CHARACTER(LEN=1),PARAMETER  :: f_cLineFeed               = CHAR(10)
  CHARACTER(LEN=11),PARAMETER :: f_cDefaultLogFileName     = 'Message.log'
  INTEGER,PARAMETER           :: f_iLogFile_NOT_Defined    = -999
  CHARACTER(LEN=1000)         :: MessageArray(200)
  !$OMP THREADPRIVATE(MessageArray)


  ! -------------------------------------------------------------
  ! --- MESSAGE LOGGER TYPE
  ! -------------------------------------------------------------
  TYPE :: MessageLoggerType
      INTEGER                  :: iUnitN                  = f_iLogFile_NOT_Defined
      CHARACTER(:),ALLOCATABLE :: cName
      INTEGER                  :: iFlagEchoProgress       = f_iNoEchoProgress
      INTEGER                  :: iMessageDestination     = f_iSCREEN_FILE
      LOGICAL                  :: lWarningsGenerated      = .FALSE.
      LOGICAL                  :: lConsoleExists          = .TRUE.
      INTEGER                  :: iLastMessageType        = f_iInfo
      CHARACTER(LEN=5000)      :: cLastMessage            = ''
      CHARACTER(LEN=200)       :: cLastMessageProcedure   = ''
      INTEGER                  :: iInfoCount              = 0
      INTEGER                  :: iWarnCount              = 0
      INTEGER                  :: iFatalCount             = 0
      LOGICAL                  :: lForceInfoWarnToScreen  = .TRUE.
      LOGICAL                  :: lTimestamp              = .FALSE.
  CONTAINS
      PROCEDURE,PASS :: Init                  => Logger_Init
      PROCEDURE,PASS :: Kill                  => Logger_Kill
      PROCEDURE,PASS :: SetLogFileName        => Logger_SetLogFileName
      PROCEDURE,PASS :: SetEchoProgress       => Logger_SetEchoProgress
      PROCEDURE,PASS :: SetMessageDest        => Logger_SetMessageDest
      PROCEDURE,PASS :: SetLastMessage_Arr    => Logger_SetLastMessage_Array
      PROCEDURE,PASS :: SetLastMessage_Str    => Logger_SetLastMessage_Single
      GENERIC        :: SetLastMessage        => SetLastMessage_Arr, SetLastMessage_Str
      PROCEDURE,PASS :: GetLastMessage        => Logger_GetLastMessage
      PROCEDURE,PASS :: GetLogFileUnit        => Logger_GetLogFileUnit
      PROCEDURE,PASS :: GetMessageCounts      => Logger_GetMessageCounts
      PROCEDURE,PASS :: LogMsg_Single         => Logger_LogSingleMessage
      PROCEDURE,PASS :: LogMsg_Array          => Logger_LogMessageArray
      GENERIC        :: LogMessage            => LogMsg_Single, LogMsg_Array
      PROCEDURE,PASS :: LogLastMessage        => Logger_LogLastMessage
      PROCEDURE,PASS :: IsLogFileDefined      => Logger_IsLogFileDefined
      PROCEDURE,PASS :: PrintRunTime          => Logger_PrintRunTime
      PROCEDURE,PASS :: EchoProgress          => Logger_EchoProgress
      PROCEDURE,PASS :: SetForceInfoWarnToScreen => Logger_SetForceInfoWarnToScreen
      PROCEDURE,PASS :: SetTimestampLogging   => Logger_SetTimestampLogging
  END TYPE MessageLoggerType


  ! -------------------------------------------------------------
  ! --- DEFAULT INSTANCE (backward compatibility)
  ! This singleton exists because 136+ source files call module-
  ! level wrapper subroutines (SetLastMessage, LogMessage, etc.)
  ! that delegate to DefaultLogger internally. New code should
  ! use instance-based MessageLoggerType passed as an argument.
  ! Thread safety: the DLL's CRITICAL(IWFM_MODEL_MGMT) serializes
  ! model creation; each model's logger is set before simulation.
  ! Future: eliminate this singleton by converting all log call
  ! sites to type-bound self%Logger%SetLastMessage() style,
  ! making each derived type carry its own logger reference.
  ! The existing ModuleLogger pointer infrastructure (91 packages)
  ! is a stepping stone toward that goal.
  ! -------------------------------------------------------------
  TYPE(MessageLoggerType),SAVE,TARGET :: DefaultLogger


  ! -------------------------------------------------------------
  ! --- MISC. DEFINITIONS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName = 'MessageLogger::'


  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS (backward-compatible wrappers)
  ! -------------------------------------------------------------
  INTERFACE LogMessage
      MODULE PROCEDURE LogSingleMessage
      MODULE PROCEDURE LogMessageArray
  END INTERFACE LogMessage

  INTERFACE SetLastMessage
      MODULE PROCEDURE SetLastMessage_Array
      MODULE PROCEDURE SetLastMessage_Single
  END INTERFACE SetLastMessage



CONTAINS



! *************************************************************
! *************************************************************
! *************************************************************
! ***** PRIVATE HELPERS
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE FORMATTED MESSAGE TO A SINGLE OUTPUT UNIT
  ! -------------------------------------------------------------
  SUBROUTINE WriteFormattedMessage(iUnit,cFmt,cAdvance,cSeverityTag,cMessageArray,iNMessage,cProgName,lUseBanner)
    INTEGER,INTENT(IN)          :: iUnit,iNMessage
    CHARACTER(LEN=*),INTENT(IN) :: cFmt,cAdvance,cSeverityTag,cMessageArray(:),cProgName
    LOGICAL,INTENT(IN)          :: lUseBanner

    !Local variables
    INTEGER :: indx

    IF (lUseBanner) THEN
        WRITE (iUnit,FMT=cFmt,ADVANCE=cAdvance) ' '
        WRITE (iUnit,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
    END IF
    IF (LEN_TRIM(cSeverityTag) .GT. 0) THEN
        WRITE (iUnit,FMT=cFmt,ADVANCE=cAdvance) '* '//TRIM(cSeverityTag)//': '
    END IF
    DO indx=1,iNMessage
        WRITE (iUnit,FMT=cFmt,ADVANCE=cAdvance) TRIM(cMessageArray(indx))
    END DO
    IF (LEN_TRIM(cProgName) .GT. 0) THEN
        WRITE (iUnit,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
    END IF
    IF (lUseBanner) THEN
        WRITE (iUnit,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
    END IF

  END SUBROUTINE WriteFormattedMessage


  ! -------------------------------------------------------------
  ! --- PRIMITIVE ERROR HANDLER IF ERRORS OCCUR BEFORE LOG FILE IS OFFICIALLY OPEN
  ! -------------------------------------------------------------
  SUBROUTINE Logger_PrimitiveErrorHandler(this,cMessage,iStat)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cMessage
    INTEGER,INTENT(OUT)                    :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'PrimitiveErrorHandler'

    CALL this%SetMessageDest(f_iSCREEN,iStat)
    CALL this%LogMessage(cMessage,f_iFatal,ThisProcedure)

    CALL this%SetLastMessage(cMessage,f_iFatal,ThisProcedure)
    iStat = -1

  END SUBROUTINE Logger_PrimitiveErrorHandler



! *************************************************************
! *************************************************************
! *************************************************************
! ***** TYPE-BOUND PROCEDURES
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- OPEN A NEW LOG FILE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_Init(this,cLogFileName,iStat)
    CLASS(MessageLoggerType)    :: this
    CHARACTER(LEN=*),INTENT(IN) :: cLogFileName
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: iErrorCode,iUnitNumber
    CHARACTER                   :: cErrorMsg*500
    LOGICAL                     :: lOpen

    !Initialize
    iStat = 0

    !If log file is already open, return
    IF (this%iUnitN .NE. f_iLogFile_NOT_Defined) THEN
        CALL this%SetLastMessage('A log file has already been defined!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF

    !Get an unconnected file unit number
    DO iUnitNumber=8,508
        INQUIRE (UNIT=iUnitNumber,OPENED=lOpen)
        IF (.NOT.lOpen) EXIT
    END DO
    this%iUnitN = iUnitNumber

    !Open log file
    IF (LEN_TRIM(cLogFileName) .EQ. 0) THEN
        ALLOCATE (CHARACTER(LEN(f_cDefaultLogFileName)) :: this%cName)
        this%cName  = f_cDefaultLogFileName
    ELSE
        ALLOCATE (CHARACTER(LEN_TRIM(cLogFileName)) :: this%cName)
        this%cName  = cLogFileName
    END IF
    OPEN (UNIT=this%iUnitN,FILE=this%cName,IOMSG=cErrorMsg,IOSTAT=iErrorCode)
    IF (iErrorCode .NE. 0) THEN
        MessageArray(1) = 'Error in opening the log file!'
        MessageArray(2) = TRIM(cErrorMsg)
        CALL this%SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
    END IF

  END SUBROUTINE Logger_Init


  ! -------------------------------------------------------------
  ! --- CLOSE LOG FILE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_Kill(this)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this

    !Local variables
    INTEGER                  :: iErrorCode
    TYPE(MessageLoggerType)  :: DummyLogger

    CLOSE (this%iUnitN , IOSTAT=iErrorCode)
    IF (ALLOCATED(this%cName)) DEALLOCATE(this%cName , STAT=iErrorCode)
    this%iUnitN                = DummyLogger%iUnitN
    this%iFlagEchoProgress     = DummyLogger%iFlagEchoProgress
    this%iMessageDestination   = DummyLogger%iMessageDestination
    this%lWarningsGenerated    = DummyLogger%lWarningsGenerated
    this%lConsoleExists        = DummyLogger%lConsoleExists
    this%iLastMessageType      = DummyLogger%iLastMessageType
    this%cLastMessage          = DummyLogger%cLastMessage
    this%cLastMessageProcedure = DummyLogger%cLastMessageProcedure
    this%iInfoCount            = DummyLogger%iInfoCount
    this%iWarnCount            = DummyLogger%iWarnCount
    this%iFatalCount           = DummyLogger%iFatalCount
    this%lForceInfoWarnToScreen = DummyLogger%lForceInfoWarnToScreen
    this%lTimestamp            = DummyLogger%lTimestamp

  END SUBROUTINE Logger_Kill


  ! -------------------------------------------------------------
  ! --- SET LOG FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetLogFileName(this,cFileName,iStat)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cFileName
    INTEGER,INTENT(OUT)                    :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'SetLogFileName'

    !Initialize
    iStat = 0

    !Check if log file is already instantiated
    IF (this%iUnitN .NE. f_iLogFile_NOT_Defined) THEN
        CALL this%SetLastMessage('Error in opening new log file! A log file is already created.',f_iFatal,ThisProcedure)
        iStat = -1
    ELSE
        CALL this%Init(cFileName,iStat)
    END IF

  END SUBROUTINE Logger_SetLogFileName


  ! -------------------------------------------------------------
  ! --- SET FLAG TO ECHO PROGRAM PROGRESS TO SCREEN OR NOT
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetEchoProgress(this,iFlag,iStat)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    INTEGER,INTENT(IN)                     :: iFlag
    INTEGER,INTENT(OUT)                    :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SetFlagToEchoProgress'

    !Initialize
    iStat = 0

    !Check if iFlag is recognized
    IF (iFlag.NE.f_iYesEchoProgress .AND. iFlag.NE.f_iNoEchoProgress) THEN
        CALL this%SetLastMessage('Flag to echo program progress is not recognized!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Set flag
    this%iFlagEchoProgress = iFlag

  END SUBROUTINE Logger_SetEchoProgress


  ! -------------------------------------------------------------
  ! --- SET DEFAULT MESSAGE DESTINATION
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetMessageDest(this,iDestination,iStat)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    INTEGER,INTENT(IN)                     :: iDestination
    INTEGER,INTENT(OUT)                    :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'SetDefaultMessageDestination'
    LOGICAL                      :: lDestRecognized

    !Initialize
    iStat = 0

    !Check if Destination is recognized
    lDestRecognized = iDestination .EQ. f_iSCREEN      .OR.  &
                      iDestination .EQ. f_iFILE        .OR.  &
                      iDestination .EQ. f_iSCREEN_FILE
    IF (.NOT. lDestRecognized) THEN
        IF (this%iUnitN .EQ. f_iLogFile_NOT_Defined) THEN
            CALL Logger_PrimitiveErrorHandler(this,'Message destination is not recognized!',iStat)
        ELSE
            CALL this%SetLastMessage('Message destination is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
        END IF
        RETURN
    END IF

    !Set DefaultMessageDestination
    this%iMessageDestination = iDestination

  END SUBROUTINE Logger_SetMessageDest


  ! -------------------------------------------------------------
  ! --- SET THE LAST MESSAGE USING AN ARRAY OF MESSAGES
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetLastMessage_Array(this,cMessageArray,iErrorLevel,cProgName)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)                     :: iErrorLevel

    !Local variables
    INTEGER :: indx

    !Initialize
    this%cLastMessage = '*   ' // cMessageArray(1)

    !Stitch the message arrays into a single string
    DO indx=2,SIZE(cMessageArray)
        this%cLastMessage = TRIM(this%cLastMessage) // f_cLineFeed // '*   ' // TRIM(cMessageArray(indx))
    END DO

    !Program name
    this%cLastMessageProcedure = TRIM(cProgName)

    !Error level
    this%iLastMessageType = iErrorLevel

  END SUBROUTINE Logger_SetLastMessage_Array


  ! -------------------------------------------------------------
  ! --- SET THE LAST MESSAGE USING A SINGLE MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetLastMessage_Single(this,cMessage,iErrorLevel,cProgName)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cMessage,cProgName
    INTEGER,INTENT(IN)                     :: iErrorLevel

    !Local variables
    CHARACTER(LEN=LEN_TRIM(cMessage)) :: cMessageArray(1)

    !Initialize
    cMessageArray(1) = TRIM(cMessage)

    CALL this%SetLastMessage(cMessageArray,iErrorLevel,cProgName)

  END SUBROUTINE Logger_SetLastMessage_Single


  ! -------------------------------------------------------------
  ! --- GET THE LAST MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_GetLastMessage(this,cMessage)
    CLASS(MessageLoggerType),INTENT(IN) :: this
    CHARACTER(LEN=*),INTENT(OUT)        :: cMessage

    !Local variables
    INTEGER                                          :: iLenMessage,iLenLastMessage
    CHARACTER(LEN=LEN_TRIM(this%cLastMessage)+110)   :: cMessageLocal

    SELECT CASE (this%iLastMessageType)
        CASE (f_iInfo)
            cMessageLocal = '* INFO:'
        CASE (f_iWarn)
            cMessageLocal = '* WARN:'
        CASE (f_iFatal)
            cMessageLocal = '* FATAL:'
    END SELECT
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // TRIM(this%cLastMessage)
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '*   (' // TRIM(this%cLastMessageProcedure) // ')'

    iLenMessage     = LEN(cMessage)
    iLenLastMessage = LEN_TRIM(cMessageLocal)

    cMessage = ''
    IF (iLenMessage .LT. iLenLastMessage) THEN
        cMessage = cMessageLocal(1:iLenMessage)
    ELSE
        cMessage(1:iLenLastMessage) = cMessageLocal
    END IF

  END SUBROUTINE Logger_GetLastMessage


  ! -------------------------------------------------------------
  ! --- GET THE LOG FILE UNIT
  ! -------------------------------------------------------------
  SUBROUTINE Logger_GetLogFileUnit(this,iLogFileUnit,iStat,cFileName)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    INTEGER,INTENT(OUT)                    :: iLogFileUnit,iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)   :: cFileName

    !Initialize
    iStat = 0

    IF (this%iUnitN .EQ. f_iLogFile_NOT_Defined) THEN
        IF (PRESENT(cFileName)) THEN
            CALL this%Init(cFileName,iStat)
        ELSE
            CALL this%Init('',iStat=iStat)
        ENDIF
    END IF
    IF (iStat .EQ. -1) RETURN

    iLogFileUnit = this%iUnitN

  END SUBROUTINE Logger_GetLogFileUnit


  ! -------------------------------------------------------------
  ! --- GET MESSAGE COUNTS
  ! -------------------------------------------------------------
  SUBROUTINE Logger_GetMessageCounts(this,iInfoCount,iWarnCount,iFatalCount)
    CLASS(MessageLoggerType),INTENT(IN) :: this
    INTEGER,INTENT(OUT)                 :: iInfoCount,iWarnCount,iFatalCount

    iInfoCount  = this%iInfoCount
    iWarnCount  = this%iWarnCount
    iFatalCount = this%iFatalCount

  END SUBROUTINE Logger_GetMessageCounts


  ! -------------------------------------------------------------
  ! --- CHECK IF LOG FILE IS ALREADY DEFINED
  ! -------------------------------------------------------------
  FUNCTION Logger_IsLogFileDefined(this) RESULT(lIsDefined)
    CLASS(MessageLoggerType),INTENT(IN) :: this
    LOGICAL                             :: lIsDefined

    lIsDefined = (this%iUnitN .NE. f_iLogFile_NOT_Defined)

  END FUNCTION Logger_IsLogFileDefined


  ! -------------------------------------------------------------
  ! --- SET FORCE INFO/WARN TO SCREEN FLAG
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetForceInfoWarnToScreen(this,lFlag)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    LOGICAL,INTENT(IN)                     :: lFlag

    this%lForceInfoWarnToScreen = lFlag

  END SUBROUTINE Logger_SetForceInfoWarnToScreen


  ! -------------------------------------------------------------
  ! --- SET TIMESTAMP LOGGING FLAG
  ! -------------------------------------------------------------
  SUBROUTINE Logger_SetTimestampLogging(this,lEnable)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    LOGICAL,INTENT(IN)                     :: lEnable

    this%lTimestamp = lEnable

  END SUBROUTINE Logger_SetTimestampLogging


  ! -------------------------------------------------------------
  ! --- CORE LOGGING PROCEDURE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_LogAllMessageTypes(this,cMessageArray,iErrorLevel,cProgName,iDestination,cFmt,cAdvance)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cMessageArray(:),cProgName,cFmt,cAdvance
    INTEGER,INTENT(IN)                     :: iErrorLevel,iDestination

    !Local variables
    INTEGER                               :: iUnitN,indx,iNMessage,iStat
    LOGICAL                               :: lWillPrintToFile,lWillPrintToScreen
    CHARACTER(LEN=1005)                   :: cSeveralMessages(SIZE(cMessageArray))
    CHARACTER(LEN=30)                     :: cTimestamp

    !Defaults
    lWillPrintToFile   = .FALSE.
    lWillPrintToScreen = .FALSE.
    SELECT CASE (iDestination)
        CASE (f_iSCREEN_FILE)
            lWillPrintToFile   = .TRUE.
            lWillPrintToScreen = .TRUE.
        CASE (f_iFILE)
            lWillPrintToFile   = .TRUE.
            lWillPrintToScreen = .FALSE.
        CASE (f_iSCREEN)
            lWillPrintToFile   = .FALSE.
            lWillPrintToScreen = .TRUE.
    END SELECT
    iNMessage = SIZE(cMessageArray)

    !Optional timestamp prefix
    cTimestamp = ''
    IF (this%lTimestamp) THEN
        BLOCK
            CHARACTER(LEN=30) :: cDate,cTime
            CALL DATE_AND_TIME(DATE=cDate,TIME=cTime)
            cTimestamp = '['//cTime(1:2)//':'//cTime(3:4)//':'//cTime(5:6)//'] '
        END BLOCK
    END IF

    !Copy messages, applying prefix for non-message severity
    IF (iErrorLevel .NE. f_iMessage) THEN
        DO indx=1,iNMessage
            cSeveralMessages(indx) = TRIM(cTimestamp)//'*   '//TRIM(cMessageArray(indx))
        END DO
    ELSE
        DO indx=1,iNMessage
            cSeveralMessages(indx) = TRIM(cTimestamp)//TRIM(cMessageArray(indx))
        END DO
    END IF

    !Check if a log file is instantiated
    IF (iDestination .NE. f_iSCREEN) THEN
        IF (this%iUnitN .EQ. f_iLogFile_NOT_Defined) CALL this%Init('',iStat)
    END IF

    IF (this%iUnitN .NE. f_iLogFile_NOT_Defined) iUnitN = this%iUnitN

    !Evaluate error severity and print out message
    SELECT CASE (iErrorLevel)
        CASE DEFAULT
            this%iFatalCount = this%iFatalCount + 1
            IF (lWillPrintToFile) &
                CALL WriteFormattedMessage(iUnitN,cFmt,cAdvance,'FATAL',cSeveralMessages,iNMessage, &
                    'Incorrect error level returned from procedure '//TRIM(ADJUSTL(cProgName)),.TRUE.)
            CALL WriteFormattedMessage(ERROR_UNIT,cFmt,cAdvance,'FATAL',cSeveralMessages,iNMessage, &
                'Incorrect error level returned from procedure '//TRIM(ADJUSTL(cProgName)),.TRUE.)
            CALL PrintRunTime()
            CALL KillLogFile()
            STOP

        CASE (f_iMessage)
            IF (lWillPrintToFile) CALL WriteFormattedMessage(iUnitN,cFmt,cAdvance,'',cSeveralMessages,iNMessage,'',.FALSE.)
            IF (lWillPrintToScreen) CALL WriteFormattedMessage(OUTPUT_UNIT,cFmt,cAdvance,'',cSeveralMessages,iNMessage,'',.FALSE.)

        CASE (f_iInfo)
            this%iInfoCount = this%iInfoCount + 1
            this%lWarningsGenerated = .TRUE.
            IF (lWillPrintToFile) &
                CALL WriteFormattedMessage(iUnitN,cFmt,cAdvance,'INFO ',cSeveralMessages,iNMessage,cProgName,.FALSE.)
            IF (lWillPrintToScreen .OR. this%lForceInfoWarnToScreen) &
                CALL WriteFormattedMessage(OUTPUT_UNIT,cFmt,cAdvance,'INFO ',cSeveralMessages,iNMessage,cProgName,.FALSE.)

        CASE (f_iWarn)
            this%iWarnCount = this%iWarnCount + 1
            this%lWarningsGenerated = .TRUE.
            IF (lWillPrintToFile) &
                CALL WriteFormattedMessage(iUnitN,cFmt,cAdvance,'WARN ',cSeveralMessages,iNMessage,cProgName,.FALSE.)
            IF (lWillPrintToScreen .OR. this%lForceInfoWarnToScreen) &
                CALL WriteFormattedMessage(OUTPUT_UNIT,cFmt,cAdvance,'WARN ',cSeveralMessages,iNMessage,cProgName,.FALSE.)

        CASE (f_iFatal)
            this%iFatalCount = this%iFatalCount + 1
            IF (lWillPrintToFile) &
                CALL WriteFormattedMessage(iUnitN,cFmt,cAdvance,'FATAL',cSeveralMessages,iNMessage,cProgName,.TRUE.)
            CALL WriteFormattedMessage(ERROR_UNIT,cFmt,cAdvance,'FATAL',cSeveralMessages,iNMessage,cProgName,.TRUE.)
            CALL PrintRunTime()
            CALL KillLogFile()
            STOP
    END SELECT

  END SUBROUTINE Logger_LogAllMessageTypes


  ! -------------------------------------------------------------
  ! --- LOG A SINGLE MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_LogSingleMessage(this,Message,ErrorLevel,ProgName,Destination,Fmt,Advance)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: Message
    CHARACTER(LEN=*),INTENT(IN)            :: ProgName
    INTEGER,INTENT(IN)                     :: ErrorLevel
    INTEGER,OPTIONAL,INTENT(IN)            :: Destination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)   :: Fmt,Advance

    !Local variables
    INTEGER                          :: iLocalDestination
    CHARACTER(LEN=50)                :: LocalFmt
    CHARACTER(LEN=3)                 :: LocalAdvance
    CHARACTER(LEN=LEN_TRIM(Message)) :: LocalMessage(1)

    !Set variables
    LocalMessage(1) = TRIM(Message)
    IF (PRESENT(Destination)) THEN
        iLocalDestination = Destination
    ELSE
        iLocalDestination = this%iMessageDestination
    END IF

    IF (PRESENT(Fmt)) THEN
        LocalFmt = Fmt
    ELSE
        LocalFmt = '(A)'
    END IF

    IF (PRESENT(Advance)) THEN
        LocalAdvance = Advance
    ELSE
        LocalAdvance = 'YES'
    END IF

    CALL Logger_LogAllMessageTypes(this,LocalMessage,iErrorLevel=ErrorLevel,cProgName=ProgName, &
                                   iDestination=iLocalDestination,cFmt=LocalFmt,cAdvance=LocalAdvance)

  END SUBROUTINE Logger_LogSingleMessage


  ! -------------------------------------------------------------
  ! --- LOG AN ARRAY OF MESSAGES
  ! -------------------------------------------------------------
  SUBROUTINE Logger_LogMessageArray(this,cMessageArray,iErrorLevel,cProgName,iDestination,cFmt,cAdvance)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)                     :: iErrorLevel
    INTEGER,OPTIONAL,INTENT(IN)            :: iDestination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)   :: cFmt,cAdvance

    !Local variables
    INTEGER           :: iLocalDestination
    CHARACTER(LEN=50) :: cLocalFmt
    CHARACTER(LEN=3)  :: cLocalAdvance

    !Set variables
    IF (PRESENT(iDestination)) THEN
        iLocalDestination = iDestination
    ELSE
        iLocalDestination = this%iMessageDestination
    END IF
    IF (PRESENT(cFmt)) THEN
        cLocalFmt = cFmt
    ELSE
        cLocalFmt = '(A)'
    END IF
    IF (PRESENT(cAdvance)) THEN
        cLocalAdvance = cAdvance
    ELSE
        cLocalAdvance = 'YES'
    END IF

    CALL Logger_LogAllMessageTypes(this,cMessageArray=cMessageArray,iErrorLevel=iErrorLevel, &
                                   cProgName=cProgName,iDestination=iLocalDestination, &
                                   cFmt=cLocalFmt,cAdvance=cLocalAdvance)

  END SUBROUTINE Logger_LogMessageArray


  ! -------------------------------------------------------------
  ! --- LOG THE LAST SAVED MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE Logger_LogLastMessage(this,iDestination,cAdvance)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    INTEGER,OPTIONAL,INTENT(IN)            :: iDestination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)   :: cAdvance

    !Local variables
    INTEGER   :: iDestination_Local
    CHARACTER :: cAdvance_Local*3,cMessageLocal*5000

    !Set destination
    IF (PRESENT(iDestination)) THEN
        iDestination_Local = iDestination
    ELSE
        iDestination_Local = this%iMessageDestination
    END IF

    !Set advancement of line
    IF (PRESENT(cAdvance)) THEN
        cAdvance_Local = TRIM(cAdvance)
    ELSE
        cAdvance_Local = 'YES'
    END IF

    !Prepare message for logging
    cMessageLocal = '*******************************************************************************'
    SELECT CASE (this%iLastMessageType)
        CASE (f_iInfo)
            cMessageLocal = TRIM(cMessageLocal) //  f_cLineFeed // '* INFO: '
        CASE (f_iWarn)
            cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '* WARN: '
        CASE (f_iFatal)
            cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '* FATAL: '
    END SELECT
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // TRIM(this%cLastMessage)
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '*   (' // TRIM(this%cLastMessageProcedure) // ')' &
                    // f_cLineFeed // '*******************************************************************************'

    !Log the message
    CALL this%LogMessage(TRIM(cMessageLocal),f_iMessage,'',Destination=iDestination_Local,Advance=cAdvance_Local)

  END SUBROUTINE Logger_LogLastMessage


  ! -------------------------------------------------------------
  ! --- PRINT RUN TIME BEFORE STOPPING PROGRAM
  ! -------------------------------------------------------------
  SUBROUTINE Logger_PrintRunTime(this,iDestination)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    INTEGER,OPTIONAL,INTENT(IN)            :: iDestination

    !Local variables
    CHARACTER(LEN=200) :: cRunTimeString
    CHARACTER(LEN=20)  :: cInfoCount,cWarnCount

    !Local timer variables
    INTEGER          :: Hour,Minute
    REAL(8)          :: Second
    CHARACTER(LEN=5) :: CHour,CMinute

    !Get the program run-time via backward-compatible wrappers
    IF (.NOT. TimerStopped()) CALL StopTimer()
    CALL GetRunTime(Hour,Minute,Second)
    WRITE (CHour,'(I5)') Hour
    WRITE (CMinute,'(I5)') Minute
    CHour   = ADJUSTL(CHour)
    CMinute = ADJUSTL(CMinute)

    cRunTimeString = 'TOTAL RUN TIME: '
    IF (Hour .GT. 0) THEN
        cRunTimeString = TRIM(cRunTimeString)//' '// &
                         TRIM(CHour)//' HOURS '    // &
                         TRIM(CMinute)//' MINUTES '
    ELSE IF (Minute .GT. 0) THEN
        cRunTimeString = TRIM(cRunTimeString)//' '//TRIM(CMinute)//' MINUTES '
    END IF
    WRITE (cRunTimeString,'(A,1X,F6.3,A)') TRIM(cRunTimeString),Second,' SECONDS'

    !Prepare the final message
    MessageArray(1) = NEW_LINE('a') // REPEAT('*',50)
    MessageArray(2) = TRIM(cRunTimeString)
    IF (this%iInfoCount .GT. 0 .OR. this%iWarnCount .GT. 0) THEN
        WRITE (cInfoCount,'(I20)') this%iInfoCount
        WRITE (cWarnCount,'(I20)') this%iWarnCount
        MessageArray(3) = 'WARNINGS: '//TRIM(ADJUSTL(cWarnCount))//'  INFORMATIONAL: '//TRIM(ADJUSTL(cInfoCount))
        IF (ALLOCATED(this%cName)) THEN
            MessageArray(4) = 'FOR DETAILS CHECK FILE '''//TRIM(this%cName)//'''.'//f_cLineFeed//REPEAT('*',50)
        ELSE
            MessageArray(4) = REPEAT('*',50)
        END IF
        IF (PRESENT(iDestination)) THEN
            CALL this%LogMessage(MessageArray(1:4),f_iMessage,'',iDestination=iDestination)
        ELSE
            CALL this%LogMessage(MessageArray(1:4),f_iMessage,'')
        END IF
    ELSE
        MessageArray(3) = REPEAT('*',50)
        IF (PRESENT(iDestination)) THEN
            CALL this%LogMessage(MessageArray(1:3),f_iMessage,'',iDestination=iDestination)
        ELSE
            CALL this%LogMessage(MessageArray(1:3),f_iMessage,'')
        END IF
    END IF

  END SUBROUTINE Logger_PrintRunTime


  ! -------------------------------------------------------------
  ! --- ECHO PROGRESS OF THE PROGRAM ONTO SCREEN
  ! -------------------------------------------------------------
  SUBROUTINE Logger_EchoProgress(this,cText,lAdvance)
    CLASS(MessageLoggerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)            :: cText
    LOGICAL,OPTIONAL,INTENT(IN)            :: lAdvance

    !Local variables
    CHARACTER :: cAdvance*3

    !Initialize
    IF (PRESENT(lAdvance)) THEN
        IF (lAdvance) THEN
            cAdvance = 'YES'
        ELSE
            cAdvance = 'NO'
        END IF
    ELSE
        cAdvance = 'YES'
    END IF

    IF (this%iFlagEchoProgress .EQ. f_iYesEchoProgress) &
        CALL this%LogMessage(cText,f_iMessage,'',Advance=cAdvance)

  END SUBROUTINE Logger_EchoProgress



! *************************************************************
! *************************************************************
! *************************************************************
! ***** BACKWARD-COMPATIBLE WRAPPER PROCEDURES
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- SET LOG FILE NAME (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE SetLogFileName(cFileName,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER,INTENT(OUT)         :: iStat

    CALL DefaultLogger%SetLogFileName(cFileName,iStat)

  END SUBROUTINE SetLogFileName


  ! -------------------------------------------------------------
  ! --- SET FLAG TO ECHO PROGRAM PROGRESS (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE SetFlagToEchoProgress(iFlag,iStat)
    INTEGER,INTENT(IN)  :: iFlag
    INTEGER,INTENT(OUT) :: iStat

    CALL DefaultLogger%SetEchoProgress(iFlag,iStat)

  END SUBROUTINE SetFlagToEchoProgress


  ! -------------------------------------------------------------
  ! --- SET DEFAULT MESSAGE DESTINATION (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE SetDefaultMessageDestination(iDestination,iStat)
    INTEGER,INTENT(IN)  :: iDestination
    INTEGER,INTENT(OUT) :: iStat

    CALL DefaultLogger%SetMessageDest(iDestination,iStat)

  END SUBROUTINE SetDefaultMessageDestination


  ! -------------------------------------------------------------
  ! --- SET LAST MESSAGE - ARRAY (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE SetLastMessage_Array(cMessageArray,iErrorLevel,cProgName)
    CHARACTER(LEN=*),INTENT(IN) :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)          :: iErrorLevel

    CALL DefaultLogger%SetLastMessage(cMessageArray,iErrorLevel,cProgName)

  END SUBROUTINE SetLastMessage_Array


  ! -------------------------------------------------------------
  ! --- SET LAST MESSAGE - SINGLE (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE SetLastMessage_Single(cMessage,iErrorLevel,cProgName)
    CHARACTER(LEN=*),INTENT(IN) :: cMessage,cProgName
    INTEGER,INTENT(IN)          :: iErrorLevel

    CALL DefaultLogger%SetLastMessage(cMessage,iErrorLevel,cProgName)

  END SUBROUTINE SetLastMessage_Single


  ! -------------------------------------------------------------
  ! --- GET LAST MESSAGE (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE GetLastMessage(cMessage)
    CHARACTER(LEN=*),INTENT(OUT) :: cMessage

    CALL DefaultLogger%GetLastMessage(cMessage)

  END SUBROUTINE GetLastMessage


  ! -------------------------------------------------------------
  ! --- GET LOG FILE UNIT (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE GetLogFileUnit(iLogFileUnit,iStat,cFileName)
    INTEGER,INTENT(OUT)                  :: iLogFileUnit,iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cFileName

    IF (PRESENT(cFileName)) THEN
        CALL DefaultLogger%GetLogFileUnit(iLogFileUnit,iStat,cFileName)
    ELSE
        CALL DefaultLogger%GetLogFileUnit(iLogFileUnit,iStat)
    END IF

  END SUBROUTINE GetLogFileUnit


  ! -------------------------------------------------------------
  ! --- KILL LOG FILE (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE KillLogFile()

    CALL DefaultLogger%Kill()

  END SUBROUTINE KillLogFile


  ! -------------------------------------------------------------
  ! --- LOG SINGLE MESSAGE (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE LogSingleMessage(Message,ErrorLevel,ProgName,Destination,Fmt,Advance)
    CHARACTER(LEN=*),INTENT(IN)          :: Message
    CHARACTER(LEN=*),INTENT(IN)          :: ProgName
    INTEGER,INTENT(IN)                   :: ErrorLevel
    INTEGER,OPTIONAL,INTENT(IN)          :: Destination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Fmt,Advance

    IF (PRESENT(Destination) .AND. PRESENT(Fmt) .AND. PRESENT(Advance)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Destination,Fmt,Advance)
    ELSE IF (PRESENT(Destination) .AND. PRESENT(Fmt)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Destination,Fmt)
    ELSE IF (PRESENT(Destination) .AND. PRESENT(Advance)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Destination,Advance=Advance)
    ELSE IF (PRESENT(Fmt) .AND. PRESENT(Advance)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Fmt=Fmt,Advance=Advance)
    ELSE IF (PRESENT(Destination)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Destination)
    ELSE IF (PRESENT(Fmt)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Fmt=Fmt)
    ELSE IF (PRESENT(Advance)) THEN
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName,Advance=Advance)
    ELSE
        CALL DefaultLogger%LogMessage(Message,ErrorLevel,ProgName)
    END IF

  END SUBROUTINE LogSingleMessage


  ! -------------------------------------------------------------
  ! --- LOG MESSAGE ARRAY (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE LogMessageArray(cMessageArray,iErrorLevel,cProgName,iDestination,cFmt,cAdvance)
    CHARACTER(LEN=*),INTENT(IN)          :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)                   :: iErrorLevel
    INTEGER,OPTIONAL,INTENT(IN)          :: iDestination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cFmt,cAdvance

    IF (PRESENT(iDestination) .AND. PRESENT(cFmt) .AND. PRESENT(cAdvance)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,iDestination,cFmt,cAdvance)
    ELSE IF (PRESENT(iDestination) .AND. PRESENT(cFmt)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,iDestination,cFmt)
    ELSE IF (PRESENT(iDestination) .AND. PRESENT(cAdvance)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,iDestination,cAdvance=cAdvance)
    ELSE IF (PRESENT(cFmt) .AND. PRESENT(cAdvance)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,cFmt=cFmt,cAdvance=cAdvance)
    ELSE IF (PRESENT(iDestination)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,iDestination)
    ELSE IF (PRESENT(cFmt)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,cFmt=cFmt)
    ELSE IF (PRESENT(cAdvance)) THEN
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName,cAdvance=cAdvance)
    ELSE
        CALL DefaultLogger%LogMessage(cMessageArray,iErrorLevel,cProgName)
    END IF

  END SUBROUTINE LogMessageArray


  ! -------------------------------------------------------------
  ! --- LOG LAST MESSAGE (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE LogLastMessage(iDestination,cAdvance)
    INTEGER,OPTIONAL,INTENT(IN)          :: iDestination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cAdvance

    IF (PRESENT(iDestination) .AND. PRESENT(cAdvance)) THEN
        CALL DefaultLogger%LogLastMessage(iDestination,cAdvance)
    ELSE IF (PRESENT(iDestination)) THEN
        CALL DefaultLogger%LogLastMessage(iDestination)
    ELSE IF (PRESENT(cAdvance)) THEN
        CALL DefaultLogger%LogLastMessage(cAdvance=cAdvance)
    ELSE
        CALL DefaultLogger%LogLastMessage()
    END IF

  END SUBROUTINE LogLastMessage


  ! -------------------------------------------------------------
  ! --- CHECK IF LOG FILE IS DEFINED (wrapper)
  ! -------------------------------------------------------------
  FUNCTION IsLogFileDefined() RESULT(lIsDefined)
    LOGICAL :: lIsDefined

    lIsDefined = DefaultLogger%IsLogFileDefined()

  END FUNCTION IsLogFileDefined


  ! -------------------------------------------------------------
  ! --- PRINT RUN TIME (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE PrintRunTime(iDestination)
    INTEGER,OPTIONAL,INTENT(IN) :: iDestination

    IF (PRESENT(iDestination)) THEN
        CALL DefaultLogger%PrintRunTime(iDestination)
    ELSE
        CALL DefaultLogger%PrintRunTime()
    END IF

  END SUBROUTINE PrintRunTime


  ! -------------------------------------------------------------
  ! --- ECHO PROGRESS (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE EchoProgress(cText,lAdvance)
    CHARACTER(LEN=*),INTENT(IN) :: cText
    LOGICAL,OPTIONAL,INTENT(IN) :: lAdvance

    IF (PRESENT(lAdvance)) THEN
        CALL DefaultLogger%EchoProgress(cText,lAdvance)
    ELSE
        CALL DefaultLogger%EchoProgress(cText)
    END IF

  END SUBROUTINE EchoProgress


  ! -------------------------------------------------------------
  ! --- GET MESSAGE COUNTS (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE GetMessageCounts(iInfoCount,iWarnCount,iFatalCount)
    INTEGER,INTENT(OUT) :: iInfoCount,iWarnCount,iFatalCount

    CALL DefaultLogger%GetMessageCounts(iInfoCount,iWarnCount,iFatalCount)

  END SUBROUTINE GetMessageCounts


END MODULE MessageLogger
