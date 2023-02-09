!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2022  
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
MODULE Class_AtmosphericData
  USE IOInterface          , ONLY: RealTSDataInFileType
  USE TimeSeriesUtilities  , ONLY: TimeStepType
  USE MessageLogger        , ONLY: EchoProgress         , &
                                   SetLastMessage       , &
                                   f_iFatal 
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
  PUBLIC :: AtmosphericDataType 
  

  ! -------------------------------------------------------------
  ! --- RELATED DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: AtmosphericDataType
      PRIVATE
      REAL(8)                  :: Fact      = 1.0        !Conversion factor
      CHARACTER(:),ALLOCATABLE :: cDataName              !Name of data     
  CONTAINS
      PROCEDURE,PASS :: New   
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: GetValues       
      PROCEDURE,PASS :: AtmosphericData_ReadTSData  
      PROCEDURE,PASS :: AtmosphericData_ReadTSData_ForTimeRange
      PROCEDURE,PASS :: IsDefined
      GENERIC        :: ReadTSData                              => AtmosphericData_ReadTSData              , &
                                                                   AtmosphericData_ReadTSData_ForTimeRange
  END TYPE AtmosphericDataType


  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AtmosphericData::'




CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INITIALIZE ATMOSPHERIC DATA TIME SERIES DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(AtmosphericData,FileName,cWorkingDirectory,cDataName,TimeStep,iStat) 
    CLASS(AtmosphericDataType),INTENT(OUT) :: AtmosphericData
    CHARACTER(LEN=*),INTENT(IN)            :: FileName,cWorkingDirectory,cDataName
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(OUT)                    :: iStat

    !Local variables
    REAL(8) :: Factor(1)
    LOGICAL :: DummyArray(1) = [.TRUE.]
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (FileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating '//TRIM(cDataName)//' data...')

    !Instantiate
    CALL AtmosphericData%Init(FileName,cWorkingDirectory,TRIM(cDataName)//' data file',TimeStep%TrackTime,1,.TRUE.,Factor,DummyArray,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    AtmosphericData%Fact      = Factor(1)
    AtmosphericData%cDataName = TRIM(cDataName)

  END SUBROUTINE New




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL ATMOSPHERIC DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AtmosphericData)
    CLASS(AtmosphericDataType) :: AtmosphericData
    
    !Local variables
    INTEGER                   :: ErrorCode
    TYPE(AtmosphericDataType) :: DummyData
    
    !Close related time-series data
    CALL AtmosphericData%RealTSDataInFileType%Close()
    
    !Set attributes to default valiues
    AtmosphericData%Fact = DummyData%Fact    
    DEALLOCATE (AtmosphericData%cDataName , STAT=ErrorCode)
    
  END SUBROUTINE Kill
  
  
  
  
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
  ! --- GET ATMOSPHERIC DATA VALUES
  ! -------------------------------------------------------------
  PURE FUNCTION GetValues(AtmosphericData,iCols) RESULT(Values)
    CLASS(AtmosphericDataType),INTENT(IN) :: AtmosphericData
    INTEGER,INTENT(IN)                    :: iCols(:)
    REAL(8)                               :: Values(SIZE(iCols))
    
    Values = AtmosphericData%rValues(iCols)
    
  END FUNCTION GetValues
  
  
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READER
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ DATA FOR A TIME RANGE FOR A COLUMN
  ! -------------------------------------------------------------
  SUBROUTINE AtmosphericData_ReadTSData_ForTimeRange(AtmosphericData,iCol,lForInquiry,lCheckForNegativity,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,FileReadCode,iStat)
    CLASS(AtmosphericDataType)  :: AtmosphericData
    INTEGER,INTENT(IN)          :: iCol       
    LOGICAL,INTENT(IN)          :: lForInquiry,lCheckForNegativity
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    INTEGER,INTENT(OUT)         :: nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputValues(:),rOutputDates(:)
    INTEGER,INTENT(OUT)         :: FileReadCode,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+39) :: ThisProcedure = ModName // 'AtmosphericData_ReadTSData_ForTimeRange'
    
    !Read data
    CALL AtmosphericData%ReadTSData(iCol,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Unit conversion
    rOutputValues(1:nActualOutput) = rOutputValues(1:nActualOutput) * AtmosphericData%Fact
    
    !Check for negativity, if desired
    IF (lCheckForNegativity) THEN
        IF (ANY(rOutputValues(1:nActualOutput) .LT. 0.0)) THEN
            CALL SetLastMessage('Timeseries input for '//TRIM(AtmosphericData%cDataName)//' data cannot be zero!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Rewind file if necessary
    IF (lForInquiry) CALL AtmosphericData%File%RewindFile_To_BeginningOfTSData(iStat)

  END SUBROUTINE AtmosphericData_ReadTSData_ForTimeRange
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR A TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE AtmosphericData_ReadTSData(AtmosphericData,TimeStep,lCheckForNegativity,iStat)
    CLASS(AtmosphericDataType)    :: AtmosphericData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lCheckForNegativity
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'AtmosphericData_ReadTSData' 
    INTEGER                      :: FileReadCode

    !Print progress
    CALL EchoProgress('Reading time series '//AtmosphericData%cDataName//' data...')

    !Read data
    CALL AtmosphericData%ReadTSData(TimeStep,AtmosphericData%cDataName,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN

    !If error code returned was zero (data read successfully), scale atmospheric data
    IF (FileReadCode .EQ. 0) THEN
        AtmosphericData%rValues = AtmosphericData%rValues * AtmosphericData%Fact
        !Check for negativity if desired
        IF (lCheckForNegativity) THEN
            IF (ANY(AtmosphericData%rValues .LT. 0.0)) THEN
                CALL SetLastMessage('Timeseries input for '//TRIM(AtmosphericData%cDataName)//' data cannot be zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END IF
    
  END SUBROUTINE AtmosphericData_ReadTSData



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
  ! --- CHECK IF DATA IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AtmosphericData) RESULT(lDefined)
    CLASS(AtmosphericDataType),INTENT(IN) :: AtmosphericData
    LOGICAL                               :: lDefined
    
    IF (AtmosphericData%iSize .EQ. 0) THEN
      lDefined = .FALSE.
    ELSE
      lDefined = .TRUE.
    END IF
    
  END FUNCTION IsDefined
  
END MODULE