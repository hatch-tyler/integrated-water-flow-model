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
MODULE MessageLogger
  USE ProgramTimer
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
            f_iYesEchoProgress           , &
            f_iNoEchoProgress            , &
            f_iSCREEN_FILE               , &
            f_iSCREEN                    , &
            f_iFILE                      , &
            f_iMessage                   , &
            f_iInfo                      , &
            f_iWarn                      , &
            f_iFatal    


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
  ! --- DATA DEFINITIONS
  ! -------------------------------------------------------------
  CHARACTER(LEN=1),PARAMETER  :: f_cLineFeed               = CHAR(10)
  CHARACTER(LEN=11),PARAMETER :: f_cDefaultLogFileName     = 'Message.log'
  INTEGER,PARAMETER           :: f_iLogFile_NOT_Defined    = -999
  CHARACTER(LEN=1000)         :: MessageArray(200)         

  
  ! -------------------------------------------------------------
  ! --- LOG FILE DATA TYPE
  ! -------------------------------------------------------------
  TYPE LogFileType
      PRIVATE
      INTEGER                  :: iUnitN                = f_iLogFile_NOT_Defined
      CHARACTER(:),ALLOCATABLE :: cName                 
      INTEGER                  :: iFlagEchoProgress     = f_iNoEchoProgress
      INTEGER                  :: iMessageDestination   = f_iSCREEN_FILE
      LOGICAL                  :: lWarningsGenerated    = .FALSE. 
      LOGICAL                  :: lConsoleExists        = .TRUE.
      INTEGER                  :: iLastMessageType      = f_iInfo
      CHARACTER(LEN=5000)      :: cLastMessage          = ''
      CHARACTER(LEN=200)       :: cLastMessageProcedure = ''
  CONTAINS
      PROCEDURE,PASS :: New
  END TYPE LogFileType
  TYPE(LogFileType),SAVE :: MyLogFile
  
  
  ! -------------------------------------------------------------
  ! --- MISC. DEFINITIONS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName = 'MessageLogger::'
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  !Overload the message logging methods
  INTERFACE LogMessage
      MODULE PROCEDURE LogSingleMessage
      MODULE PROCEDURE LogMessageArray
  END INTERFACE LogMessage
  
  !Overload saving the last message
  INTERFACE SetLastMessage
      MODULE PROCEDURE SetLastMessage_Array
      MODULE PROCEDURE SetLastMessage_Single
  END INTERFACE SetLastMessage



CONTAINS


    
    
! *************************************************************
! *************************************************************
! *************************************************************
! ***** CONSTRUCTOR
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- OPEN A NEW LOG FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(LogFile,cLogFileName,iStat)
    CLASS(LogFileType)          :: LogFile
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
    IF (LogFile%iUnitN .NE. f_iLogFile_NOT_Defined) THEN
        CALL SetLastMessage('A log file has already been defined!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
    !Get an unconnected file unit number
    DO iUnitNumber=8,508
        INQUIRE (UNIT=iUnitNumber,OPENED=lOpen)
        IF (.NOT.lOpen) EXIT
    END DO
    MyLogFile%iUnitN = iUnitNumber
    
    !Open log file
    IF (LEN_TRIM(cLogFileName) .EQ. 0) THEN
        ALLOCATE (CHARACTER(LEN(f_cDefaultLogFileName)) :: MyLogFile%cName)
        MyLogFile%cName  = f_cDefaultLogFileName
    ELSE
        ALLOCATE (CHARACTER(LEN_TRIM(cLogFileName)) :: MyLogFile%cName)
        MyLogFile%cName  = cLogFileName
    END IF
    OPEN (UNIT=MyLogFile%iUnitN,FILE=MyLogFile%cName,IOMSG=cErrorMsg,IOSTAT=iErrorCode)
    IF (iErrorCode .NE. 0) THEN
        MessageArray(1) = 'Error in opening the log file!'
        MessageArray(2) = TRIM(cErrorMsg)
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
  END SUBROUTINE New

  
  

! *************************************************************
! *************************************************************
! *************************************************************
! ***** DESTRUCTOR
! *************************************************************
! *************************************************************
! *************************************************************
  
  ! -------------------------------------------------------------
  ! --- CLOSE LOG FILE
  ! -------------------------------------------------------------  
  SUBROUTINE KillLogFile()

    !Local variables
    INTEGER           :: iErrorCode
    TYPE(LogFileType) :: DummyFile
    
    CLOSE (MyLogFile%iUnitN , IOSTAT=iErrorCode)
    DEALLOCATE(MyLogFile%cName , STAT=iErrorCode)
    MyLogFile = DummyFile
    
  END SUBROUTINE KillLogFile

  
  

! *************************************************************
! *************************************************************
! *************************************************************
! ***** SETTERS
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- SET FLAG TO ECHO PROGRAM PROGRESS TO SCREEN OR NOT
  ! -------------------------------------------------------------
  SUBROUTINE SetFlagToEchoProgress(iFlag,iStat)
    INTEGER,INTENT(IN)  :: iFlag
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SetFlagToEchoProgress'
    
    !Initialize
    iStat = 0
    
    !Check if iFlag is recognized
    IF (iFlag.NE.f_iYesEchoProgress .AND. iFlag.NE.f_iNoEchoProgress) THEN
        CALL SetLastMessage('Flag to echo program progress is not recognized!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Set flag
    MyLogFile%iFlagEchoProgress = iFlag
    
  END SUBROUTINE SetFlagToEchoProgress
  

  ! -------------------------------------------------------------
  ! --- SET DEFAULT MESSAGE DESTINATION
  ! -------------------------------------------------------------
  SUBROUTINE SetDefaultMessageDestination(iDestination,iStat)
    INTEGER,INTENT(IN)  :: iDestination
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'SetDefaultMessageDestination'
    LOGICAL                      :: lDestRecognized
    
    !Initilaize
    iStat = 0

    !Check if Destination is recognized
    lDestRecognized = iDestination .EQ. f_iSCREEN      .OR.  &
                      iDestination .EQ. f_iFILE        .OR.  &
                      iDestination .EQ. f_iSCREEN_FILE     
    IF (.NOT. lDestRecognized) THEN
        IF (MyLogFile%iUnitN .EQ. f_iLogFile_NOT_Defined) THEN 
            CALL PrimitiveErrorHandler('Message destination is not recognized!',iStat)
        ELSE
            CALL SetLastMessage('Message destination is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
        END IF
        RETURN
    END IF

    !Set DefaultMessageDestination
    MyLogFile%iMessageDestination = iDestination
    
  END SUBROUTINE SetDefaultMessageDestination
   

  ! -------------------------------------------------------------
  ! --- SET THE LOG FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE SetLogFileName(cFileName,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'SetLogFileName'
    
    !Initilaize
    iStat = 0

    !Check if log file is already instantiated
    IF (MyLogFile%iUnitN .NE. f_iLogFile_NOT_Defined) THEN
        CALL SetLastMessage('Error in opening new log file! A log file is already created.',f_iFatal,ThisProcedure) 
        iStat = -1
    ELSE
        CALL MyLogFile%New(cFileName,iStat)     
    END IF
    
  END SUBROUTINE SetLogFileName

  
  ! -------------------------------------------------------------
  ! --- SET THE LAST MESSAGE AND MESSAGE LEVEL USING AN ARRAY OF MESSAGES
  ! -------------------------------------------------------------
  SUBROUTINE SetLastMessage_Array(cMessageArray,iErrorLevel,cProgName)
    CHARACTER(LEN=*),INTENT(IN) :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)          :: iErrorLevel
    
    !Local variables
    INTEGER :: indx
    
    !Initialize
    MyLogFile%cLastMessage = '*   ' // cMessageArray(1)
    
    !Stich the message arrays into a single string
    DO indx=2,SIZE(cMessageArray)
        MyLogFile%cLastMessage = TRIM(MyLogFile%cLastMessage) // f_cLineFeed // '*   ' // TRIM(cMessageArray(indx))
    END DO

    !Program name 
    MyLogFile%cLastMessageProcedure = TRIM(cProgName) 
    
    !Error level
    MyLogFile%iLastMessageType = iErrorLevel
    
  END SUBROUTINE SetLastMessage_Array
  
  
  ! -------------------------------------------------------------
  ! --- SET THE LAST MESSAGE AND MESSAGE LEVEL USING A SINGLE MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE SetLastMessage_Single(cMessage,iErrorLevel,cProgName)
    CHARACTER(LEN=*),INTENT(IN) :: cMessage,cProgName
    INTEGER,INTENT(IN)          :: iErrorLevel
    
    !Local variables
    CHARACTER(LEN=LEN_TRIM(cMessage)) :: cMessageArray(1)
    
    !Initialize
    cMessageArray(1) = TRIM(cMessage)
    
    CALL SetLastMessage_Array(cMessageArray,iErrorLevel,cProgName)

  END SUBROUTINE SetLastMessage_Single
  
  
  
  
! *************************************************************
! *************************************************************
! *************************************************************
! ***** GETTERS
! *************************************************************
! *************************************************************
! *************************************************************
  
  ! -------------------------------------------------------------
  ! --- GET THE LAST MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE GetLastMessage(cMessage)
    CHARACTER(LEN=*),INTENT(OUT) :: cMessage
    
    !Local variables
    INTEGER                                             :: iLenMessage,iLenLastMessage
    CHARACTER(LEN=LEN_TRIM(MyLogFile%cLastMessage)+110) :: cMessageLocal
    
    SELECT CASE (MyLogFile%iLastMessageType)
        CASE (f_iInfo)
            cMessageLocal = '* INFO:'  
        CASE (f_iWarn)
            cMessageLocal = '* WARN:' 
        CASE (f_iFatal)
            cMessageLocal = '* FATAL:' 
    END SELECT
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // TRIM(MyLogFile%cLastMessage)
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '*   (' // TRIM(MyLogFile%cLastMessageProcedure) // ')'     
    
    iLenMessage     = LEN(cMessage)
    iLenLastMessage = LEN_TRIM(cMessageLocal)
    
    cMessage = ''
    IF (iLenMessage .LT. iLenLastMessage) THEN    
        cMessage = cMessageLocal(1:iLenMessage)
    ELSE
        cMessage(1:iLenLastMessage) = cMessageLocal
    END IF
    
  END SUBROUTINE GetLastMessage
  
  
  ! -------------------------------------------------------------
  ! --- GET THE LOG FILE UNIT
  ! -------------------------------------------------------------
  SUBROUTINE GetLogFileUnit(iLogFileUnit,iStat,cFileName)
    INTEGER,INTENT(OUT)                  :: iLogFileUnit,iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cFileName
    
    !Initialize
    iStat = 0

    IF (MyLogFile%iUnitN .EQ. f_iLogFile_NOT_Defined) THEN
        IF (PRESENT(cFileName)) THEN
            CALL MyLogFile%New(cFileName,iStat)
        ELSE
            CALL MyLogFile%New('',iStat=iStat)
        ENDIF
    END IF
    IF (iStat .EQ. -1) RETURN
      
    iLogFileUnit = MyLogFile%iUnitN
      
  END SUBROUTINE GetLogFileUnit


  
  
! *************************************************************
! *************************************************************
! *************************************************************
! ***** UTILITIES
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF LOG FILE IS ALREADY DEFINED
  ! -------------------------------------------------------------
  FUNCTION IsLogFileDefined() RESULT(lIsDefined)
    LOGICAL :: lIsDefined

    IF (MyLogFile%iUnitN .EQ. f_iLogFile_NOT_Defined) THEN
        lIsDefined = .FALSE.
    ELSE
        lIsDefined = .TRUE.
    END IF

  END FUNCTION IsLogFileDefined


  ! -------------------------------------------------------------
  ! --- PRIMITIVE ERROR HANDLER IF ERRORS OCCUR BEFORE LOG FILE IS OFFICIALLY OPEN
  ! -------------------------------------------------------------
  SUBROUTINE PrimitiveErrorHandler(cMessage,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cMessage
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'PrimitiveErrorHandler'

    CALL SetDefaultMessageDestination(f_iSCREEN,iStat)
    CALL LogMessage(cMessage,f_iFatal,ThisProcedure)
    
    CALL SetLastMessage(cMessage,f_iFatal,ThisProcedure)
    iStat = -1

  END SUBROUTINE PrimitiveErrorHandler


  ! -------------------------------------------------------------
  ! --- PRINT OUT ALL TYPES OF MESSAGES (SINGLE VS. ARRAY)
  ! -------------------------------------------------------------
  SUBROUTINE LogAllMessageTypes(cMessageArray,iErrorLevel,cProgName,iDestination,cFmt,cAdvance)
    CHARACTER(LEN=*),INTENT(IN) :: cMessageArray(:),cProgName,cFmt,cAdvance
    INTEGER,INTENT(IN)          :: iErrorLevel,iDestination

    !Local variables
    INTEGER                               :: iUnitN,indx,iNMessage,iStat
    LOGICAL                               :: lWillPrintToFile,lWillPrintToScreen
    CHARACTER(LEN=5+LEN(MessageArray(1))) :: cSeveralMessages(SIZE(cMessageArray))

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
    iNMessage                     = SIZE(cMessageArray)
    cSeveralMessages(1:iNMessage) = cMessageArray

    !Check if a log file is instantiated
    IF (iDestination .NE. f_iSCREEN) THEN
        IF (MyLogFile%iUnitN .EQ. f_iLogFile_NOT_Defined) CALL MyLogFile%New('',iStat)
    END IF

    !Modify messages based on the error level
    IF (iErrorLevel .NE. f_iMessage) THEN
        DO indx=1,iNMessage
            cSeveralMessages(indx) = '*   '//TRIM(cSeveralMessages(indx))
        END DO
    END IF

    IF (MyLogFile%iUNitN .NE. f_iLogFile_NOT_Defined) iUnitN = MyLogFile%iUnitN
    !Evaluate error severity and print out message
    SELECT CASE (iErrorLevel)
        CASE DEFAULT
            IF (lWillPrintToFile) THEN
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) ' '
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*'
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '* FATAL:' 
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*   Incorrect error level returned from procedure '//TRIM(ADJUSTL(cProgName))
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*'
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
            END IF
            !Always print to error unit
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) ' '
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*'
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '* FATAL:' 
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*   Incorrect error level returned from procedure '//TRIM(ADJUSTL(cProgName))
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*'
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
            CALL PrintRunTime()
            CALL KillLogFile()
            STOP 
        
        CASE (f_iMessage)
            IF (lWillPrintToFile) CALL PrintMessageArray(f_iFILE)
            IF (lWillPrintToScreen) CALL PrintMessageArray(f_iSCREEN)
          
        CASE (f_iInfo)
            MyLogFile%lWarningsGenerated = .TRUE.
            IF (lWillPrintToFile) THEN
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '* INFO : '
                CALL PrintMessageArray(f_iFILE)
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
            END IF
            !Always print to screen
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '* INFO : '
            CALL PrintMessageArray(f_iSCREEN)
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
            
        CASE (f_iWarn)
            MyLogFile%lWarningsGenerated = .TRUE.
            IF (lWillPrintToFile) THEN
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '* WARN : '
                CALL PrintMessageArray(f_iFILE)
                WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
            END IF
            !Always write to screen  
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '* WARN : '
            CALL PrintMessageArray(f_iSCREEN)
            WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
        
        CASE (f_iFatal)
          IF (lWillPrintToFile) THEN
              WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) ' '
              WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
              WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '* FATAL: '
              CALL PrintMessageArray(f_iFILE)
              WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
              WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
          END IF
          !Always print to error unit
          WRITE (*,FMT=cFmt,ADVANCE=cAdvance) ' '
          WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
          WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '* FATAL: '
          CALL PrintMessageArray(f_iSCREEN)
          WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*   ('//TRIM(ADJUSTL(cProgName))//')'
          WRITE (*,FMT=cFmt,ADVANCE=cAdvance) '*******************************************************************************'
          CALL PrintRunTime()
          CALL KillLogFile()
          STOP
    END SELECT

  
  CONTAINS

    !PRINT MESSAGE ARRAY
    SUBROUTINE PrintMessageArray(iDest)
      INTEGER,INTENT(IN) :: iDest
      
      !Local variables
      INTEGER :: indxMessage
      
      !Print
      IF (iDest .EQ. f_iFILE) THEN
          DO indxMessage=1,iNMessage
              WRITE (iUnitN,FMT=cFmt,ADVANCE=cAdvance) TRIM(cSeveralMessages(indxMessage))
          END DO
      ELSE IF (iDest .EQ. f_iSCREEN) THEN
          DO indxMessage=1,iNMessage
              WRITE (*,FMT=cFmt,ADVANCE=cAdvance) TRIM(cSeveralMessages(indxMessage))
          END DO
      END IF
            
    END SUBROUTINE PrintMessageArray

  END SUBROUTINE LogAllMessageTypes


  ! -------------------------------------------------------------
  ! --- GATEWAY SUBROUTINE FOR SINGLE MESSAGE LOGGING (integer destination)
  ! -------------------------------------------------------------
  SUBROUTINE LogSingleMessage(Message,ErrorLevel,ProgName,Destination,Fmt,Advance)
    CHARACTER(LEN=*),INTENT(IN) :: Message
    CHARACTER(LEN=*),INTENT(IN) :: ProgName
    INTEGER,INTENT(IN)          :: ErrorLevel
    INTEGER,OPTIONAL,INTENT(IN) :: Destination
    CHARACTER(LEN=*),OPTIONAL   :: Fmt,Advance

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
        iLocalDestination = MyLogFile%iMessageDestination
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

    !Transfer control to LogAllMessageTypes
    CALL LogAllMessageTypes(LocalMessage,iErrorLevel=ErrorLevel,cProgName=ProgName,iDestination=iLocalDestination,cFmt=LocalFmt,cAdvance=LocalAdvance)
      
  END SUBROUTINE LogSingleMessage
 

  ! -------------------------------------------------------------
  ! --- GATEWAY SUBROUTINE FOR ARRAY OF MESSAGE LOGGING
  ! -------------------------------------------------------------
  SUBROUTINE LogMessageArray(cMessageArray,iErrorLevel,cProgName,iDestination,cFmt,cAdvance)
    CHARACTER(LEN=*),INTENT(IN)          :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)                   :: iErrorLevel
    INTEGER,OPTIONAL,INTENT(IN)          :: iDestination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cFmt,cAdvance

    !Local variables
    INTEGER           :: iLocalDestination
    CHARACTER(LEN=50) :: cLocalFmt
    CHARACTER(LEN=3)  :: cLocalAdvance

    !Set variables
    IF (PRESENT(iDestination)) THEN
        iLocalDestination = iDestination
    ELSE
        iLocalDestination = MyLogFile%iMessageDestination
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

    !Transfer control to LogAllMessageTypes
    CALL LogAllMessageTypes(cMessageArray=cMessageArray,iErrorLevel=iErrorLevel,cProgName=cProgName,iDestination=iLocalDestination,cFmt=cLocalFmt,cAdvance=cLocalAdvance)
      
  END SUBROUTINE LogMessageArray


  ! -------------------------------------------------------------
  ! --- PRINT OUT ALL THE LAST SAVED MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE LogLastMessage(iDestination,cAdvance)
    INTEGER,OPTIONAL,INTENT(IN)          :: iDestination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cAdvance
    
    !Local variables
    INTEGER   :: iDestination_Local
    CHARACTER :: cAdvance_Local*3,cMessageLocal*5000
    
    
    !Set destination
    IF (PRESENT(iDestination)) THEN
        iDestination_Local = iDestination
    ELSE
        iDestination_Local = MyLogFile%iMessageDestination
    END IF
  
    !Set advancement of line
    IF (PRESENT(cAdvance)) THEN
        cAdvance_Local = TRIM(cAdvance)
    ELSE
        cAdvance_Local = 'YES'
    END IF
    
    !Prepare message for logging
    cMessageLocal = '*******************************************************************************'
    SELECT CASE (MyLogFile%iLastMessageType)
        CASE (f_iInfo)
            cMessageLocal = TRIM(cMessageLocal) //  f_cLineFeed // '* INFO: '  
        CASE (f_iWarn)
            cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '* WARN: ' 
        CASE (f_iFatal)
            cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '* FATAL: ' 
    END SELECT
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // TRIM(MyLogFile%cLastMessage)
    cMessageLocal = TRIM(cMessageLocal) // f_cLineFeed // '*   (' // TRIM(MyLogFile%cLastMessageProcedure) // ')' // f_cLineFeed // '*******************************************************************************'
    
    !Log the message
    CALL LogMessage(TRIM(cMessageLocal),f_iMessage,'',Destination=iDestination_Local,Advance=cAdvance_Local)
    
  END SUBROUTINE LogLastMessage
  
  
  ! -------------------------------------------------------------
  ! --- PRINT RUN TIME BEFORE STOPPING PROGRAM
  ! -------------------------------------------------------------
  SUBROUTINE PrintRunTime(iDestination)
    INTEGER,OPTIONAL,INTENT(IN) :: iDestination

    !Local variables
    INTEGER          :: Hour, Minute
    REAL(8)          :: Second
    CHARACTER(LEN=5) :: CHour,CMinute

    !Get the program run-time
    IF (.NOT. TimerStopped) CALL StopTimer()
    CALL GetRunTime(Hour,Minute,Second)
    WRITE (CHour,'(I5)') Hour
    WRITE (CMinute,'(I5)') Minute
    CHour   = ADJUSTL(CHour)
    CMinute = ADJUSTL(CMinute)

    !Prepare the final message
    MessageArray(1) = NEW_LINE('a') // REPEAT('*',50)
    MessageArray(2) = 'TOTAL RUN TIME: '
    IF (Hour.GT.0) THEN
        MessageArray(2) = TRIM(MessageArray(2))//' '// &
                          TRIM(CHour)//' HOURS '    // &
                          TRIM(CMinute)//' MINUTES '
    ELSE IF (Minute.GT.0) THEN
        MessageArray(2) = TRIM(MessageArray(2))//' '//TRIM(CMinute)//' MINUTES '
    END IF
    WRITE (MessageArray(2),'(A,1X,F6.3,A)') TRIM(MessageArray(2)),Second,' SECONDS'
    IF (MyLogFile%lWarningsGenerated) THEN
        MessageArray(3) = 'WARNINGS/INFORMATIONAL MESSAGES ARE GENERATED!'//f_cLineFeed
        IF (MyLogFile%iUnitN .NE. f_iLogFile_NOT_Defined)  &
            MessageArray(3) = TRIM(MessageArray(3)) // 'FOR DETAILS CHECK FILE ''' // TRIM(MyLogFile%cName) // '''.' // f_cLineFeed
        MessageArray(3) = TRIM(MessageArray(3)) // REPEAT('*',50)
    ELSE                               
        MessageArray(3) = REPEAT('*',50)
    END IF

    !Print the final message
    IF (PRESENT(iDestination)) THEN
        CALL LogMessage(MessageArray(1:3),f_iMessage,'',iDestination=iDestination)
    ELSE
        CALL LogMessage(MessageArray(1:3),f_iMessage,'')
    END IF

  END SUBROUTINE PrintRunTime


  ! -------------------------------------------------------------
  ! --- ECHO PROGRESS OF THE PROGRAM ONTO SCREEN
  ! -------------------------------------------------------------
  SUBROUTINE EchoProgress(cText,lAdvance)
    CHARACTER(LEN=*),INTENT(IN) :: cText
    LOGICAL,OPTIONAL,INTENT(IN) :: lAdvance
    
    !Local variables
    CHARACTER :: cAdvance*3
    
    !Initailize
    IF (PRESENT(lAdvance)) THEN
        IF (lAdvance) THEN
            cAdvance = 'YES'
        ELSE
            cAdvance = 'NO'
        END IF
    ELSE
        cAdvance = 'YES'
    END IF

    IF (MyLogFile%iFlagEchoProgress .EQ. f_iYesEchoProgress) CALL LogMessage(cText,f_iMessage,'',Advance=cAdvance)
        
  END SUBROUTINE EchoProgress
  
  
END MODULE