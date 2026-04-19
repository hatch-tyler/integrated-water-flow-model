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
MODULE Class_RootDepthFracDataFile
  USE MessageLogger        , ONLY: MessageLoggerType     , &
                                   f_iFatal
  USE IOInterface          , ONLY: RealTSDataInFileType  
  USE TimeseriesUtilities  , ONLY: TimeStepType
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
  PUBLIC :: RootDepthFracDataFile_SetModuleLogger , &
            RootDepthFracDataFileType


  ! -------------------------------------------------------------
  ! --- ROOT DEPTH FRACTION DATA FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: RootDepthFracDataFileType
    TYPE(MessageLoggerType),POINTER :: Logger => NULL()
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: RootDepthFracDataFile_ReadTSData
      GENERIC        :: ReadTSData                       => RootDepthFracDataFile_ReadTSData
  END TYPE RootDepthFracDataFileType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  ! --- MODULE-LEVEL LOGGER
  ! -------------------------------------------------------------
  TYPE(MessageLoggerType), POINTER, PRIVATE :: ModuleLogger => NULL()


  INTEGER,PARAMETER                   :: ModNameLen = 29
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_RootDepthFracDataFile::'




CONTAINS


  ! -------------------------------------------------------------
  ! --- SET MODULE-LEVEL LOGGER
  ! -------------------------------------------------------------
  SUBROUTINE RootDepthFracDataFile_SetModuleLogger(Logger)
    TYPE(MessageLoggerType), TARGET, INTENT(IN) :: Logger
    ModuleLogger => Logger
  END SUBROUTINE RootDepthFracDataFile_SetModuleLogger


  ! -------------------------------------------------------------
  ! --- NEW ROOT DEPTH FRACTIONS DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(RootDepthFracDataFile,Logger,cFileName,cWorkingDirectory,TrackTime,iStat)
    CLASS(RootDepthFracDataFileType)             :: RootDepthFracDataFile
    TYPE(MessageLoggerType),POINTER,INTENT(IN)   :: Logger
    CHARACTER(LEN=*),INTENT(IN)                  :: cFileName,cWorkingDirectory
    LOGICAL,INTENT(IN)                           :: TrackTime
    INTEGER,INTENT(OUT)                          :: iStat

    !Local variables
    REAL(8) :: Factor(1)

    !Initialize
    iStat = 0
    RootDepthFracDataFile%Logger => Logger
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Instantiate
    CALL RootDepthFracDataFile%Init(cFileName,cWorkingDirectory,'root depth fractions data',TrackTime,BlocksToSkip=1,lFactorDefined=.FALSE.,Factor=Factor,iStat=iStat)

  END SUBROUTINE New

    
  ! -------------------------------------------------------------
  ! --- KILL ROOT DEPTH FRACTIONS DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill(RootDepthFracDataFile)
    CLASS(RootDepthFracDataFileType) :: RootDepthFracDataFile
    
    CALL RootDepthFracDataFile%Close()
    
  END SUBROUTINE Kill
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ ROOT DEPTH FRACTIONS DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootDepthFracDataFile_ReadTSData(RootDepthFracDataFile,TimeStep,iStat)
    CLASS(RootDepthFracDataFileType) :: RootDepthFracDataFile
    TYPE(TimeStepType),INTENT(IN)    :: TimeStep
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'RootDepthFracDataFile_ReadTSData'
    INTEGER                      :: FileErrorCode
    
    !Initialize
    iStat = 0
    
    !Read data
    CALL RootDepthFracDataFile%ReadTSData(TimeStep,'root depth fractions data',FileErrorCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Return if new data is not read
    IF (FileErrorCode .NE. 0) RETURN
    
    !Make sure all values are between 0.0 and 1.0
    IF (ANY(RootDepthFracDataFile%rValues .LT. 0.0))  THEN
        CALL RootDepthFracDataFile%Logger%SetLastMessage('Root depth fractions cannot be less than zero!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (ANY(RootDepthFracDataFile%rValues .GT. 1.0))  THEN
        CALL RootDepthFracDataFile%Logger%SetLastMessage('Root depth fractions cannot be greater than zero!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
      
  END SUBROUTINE RootDepthFracDataFile_ReadTSData
  
  
END MODULE