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
! This module uses DATE_AND_TIME function instead of CPU_TIME to compute the
! elapsed run time.  This is because CPU_TIME does not produce correct results
! when the executable is run in parallel.  DATE_AND_TIME seems to be more robust
! in sequential and parallel executions.
!
! Can Dogrul ; 1/22/08
!
! Refactored to type-based design (no module-level SAVE state).
! Backward-compatible wrapper procedures delegate to DefaultTimer.
!
!***********************************************************************
MODULE ProgramTimer
  USE ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  IMPLICIT NONE

  PRIVATE

  ! -------------------------------------------------------------
  ! --- CONSTANTS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: MAX_SECTION_NAME = 64
  INTEGER,PARAMETER :: MAX_SECTIONS     = 32


  ! -------------------------------------------------------------
  ! --- SECTION TIMER TYPE
  ! -------------------------------------------------------------
  TYPE :: SectionTimerType
      CHARACTER(LEN=MAX_SECTION_NAME) :: cName           = ''
      INTEGER                         :: StartValues(8)  = 0
      INTEGER                         :: EndValues(8)    = 0
      REAL(8)                         :: rAccumulatedSec = 0.0d0
      INTEGER                         :: iCallCount      = 0
      LOGICAL                         :: lRunning        = .FALSE.
  END TYPE SectionTimerType


  ! -------------------------------------------------------------
  ! --- PROGRAM TIMER TYPE
  ! -------------------------------------------------------------
  TYPE :: ProgramTimerType
      LOGICAL                  :: lStopped         = .FALSE.
      LOGICAL                  :: lStarted         = .FALSE.
      INTEGER                  :: StartTimeValues(8) = 0
      INTEGER                  :: EndTimeValues(8)   = 0
      TYPE(SectionTimerType)   :: Sections(MAX_SECTIONS)
      INTEGER                  :: nSections        = 0
  CONTAINS
      PROCEDURE,PASS :: Start          => Timer_Start
      PROCEDURE,PASS :: Stop           => Timer_Stop
      PROCEDURE,PASS :: GetRunTime     => Timer_GetRunTime
      PROCEDURE,PASS :: FormatRunTime  => Timer_FormatRunTime
      PROCEDURE,PASS :: StartSection   => Timer_StartSection
      PROCEDURE,PASS :: StopSection    => Timer_StopSection
      PROCEDURE,PASS :: GetSectionTime => Timer_GetSectionTime
      PROCEDURE,PASS :: PrintReport    => Timer_PrintReport
  END TYPE ProgramTimerType


  ! -------------------------------------------------------------
  ! --- DEFAULT INSTANCE (backward compatibility)
  ! -------------------------------------------------------------
  TYPE(ProgramTimerType),SAVE,TARGET :: DefaultTimer


  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  ! New type-based API
  PUBLIC :: ProgramTimerType

  ! Backward-compatible wrapper procedures
  PUBLIC :: StartTimer    , &
            StopTimer     , &
            GetRunTime    , &
            TimerStopped


CONTAINS


! *************************************************************
! *************************************************************
! **** PRIVATE HELPER: CONVERT DATE_AND_TIME VALUES TO SECONDS
! *************************************************************
! *************************************************************
  FUNCTION DateTimeToSeconds(iValues) RESULT(rSeconds)
    INTEGER,INTENT(IN) :: iValues(8)
    REAL(8)            :: rSeconds

    !Local variables
    INTEGER :: iJulian,iYear,iMonth,iDay
    REAL(8) :: rHour,rMin,rSec,rmSec

    rHour   = REAL(iValues(5),8)
    rMin    = REAL(iValues(6),8)
    rSec    = REAL(iValues(7),8)
    rmSec   = REAL(iValues(8),8)
    iDay    = iValues(3)
    iMonth  = iValues(2)
    iYear   = iValues(1)
    iJulian = iDay - 32075 + 1461*(iYear+4800+(iMonth-14)/12)/4  &
              + 367*(iMonth-2-(iMonth-14)/12*12)/12               &
              - 3*((iYear+4900+(iMonth-14)/12)/100)/4
    rSeconds = REAL(iJulian,8)*86400.0d0 + rHour*3600.0d0 + rMin*60.0d0 + rSec + rmSec/1000.0d0

  END FUNCTION DateTimeToSeconds


! *************************************************************
! *************************************************************
! **** TYPE-BOUND PROCEDURES
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- START TIMER
  ! -------------------------------------------------------------
  SUBROUTINE Timer_Start(this)
    CLASS(ProgramTimerType),INTENT(INOUT) :: this

    CALL DATE_AND_TIME(VALUES = this%StartTimeValues)
    this%lStopped = .FALSE.
    this%lStarted = .TRUE.

  END SUBROUTINE Timer_Start


  ! -------------------------------------------------------------
  ! --- STOP TIMER
  ! -------------------------------------------------------------
  SUBROUTINE Timer_Stop(this)
    CLASS(ProgramTimerType),INTENT(INOUT) :: this

    CALL DATE_AND_TIME(VALUES = this%EndTimeValues)
    this%lStopped = .TRUE.

  END SUBROUTINE Timer_Stop


  ! -------------------------------------------------------------
  ! --- GET RUN TIME
  ! -------------------------------------------------------------
  SUBROUTINE Timer_GetRunTime(this,Hour,Minute,Second)
    CLASS(ProgramTimerType),INTENT(IN) :: this
    INTEGER,INTENT(OUT)                :: Hour,Minute
    REAL(8),INTENT(OUT)                :: Second

    !Local variables
    REAL(8) :: RunTime

    !Initialize
    Hour   = 0
    Minute = 0
    Second = 0.0d0

    !If timer hasn't been started at all run time cannot be computed; return
    IF (.NOT. this%lStarted) RETURN

    RunTime = DateTimeToSeconds(this%EndTimeValues) - DateTimeToSeconds(this%StartTimeValues)
    Hour    = INT(RunTime/3600.0d0)
    Minute  = INT((RunTime - Hour*3600.0d0)/60.0d0)
    Second  = RunTime - Hour*3600.0d0 - Minute*60.0d0

  END SUBROUTINE Timer_GetRunTime


  ! -------------------------------------------------------------
  ! --- FORMAT RUN TIME AS STRING
  ! -------------------------------------------------------------
  SUBROUTINE Timer_FormatRunTime(this,cRunTimeString)
    CLASS(ProgramTimerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(OUT)          :: cRunTimeString

    !Local variables
    INTEGER          :: Hour,Minute
    REAL(8)          :: Second
    CHARACTER(LEN=5) :: CHour,CMinute

    !Stop timer if not already stopped
    IF (.NOT. this%lStopped) CALL this%Stop()

    CALL this%GetRunTime(Hour,Minute,Second)
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

  END SUBROUTINE Timer_FormatRunTime


  ! -------------------------------------------------------------
  ! --- START A NAMED SECTION TIMER
  ! -------------------------------------------------------------
  SUBROUTINE Timer_StartSection(this,cSectionName)
    CLASS(ProgramTimerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)           :: cSectionName

    !Local variables
    INTEGER :: indx

    !Look for existing section
    DO indx=1,this%nSections
        IF (TRIM(this%Sections(indx)%cName) .EQ. TRIM(cSectionName)) THEN
            CALL DATE_AND_TIME(VALUES = this%Sections(indx)%StartValues)
            this%Sections(indx)%lRunning  = .TRUE.
            this%Sections(indx)%iCallCount = this%Sections(indx)%iCallCount + 1
            RETURN
        END IF
    END DO

    !Not found — create new section
    IF (this%nSections .GE. MAX_SECTIONS) RETURN
    this%nSections = this%nSections + 1
    indx = this%nSections
    this%Sections(indx)%cName           = cSectionName
    this%Sections(indx)%rAccumulatedSec = 0.0d0
    this%Sections(indx)%iCallCount      = 1
    this%Sections(indx)%lRunning        = .TRUE.
    CALL DATE_AND_TIME(VALUES = this%Sections(indx)%StartValues)

  END SUBROUTINE Timer_StartSection


  ! -------------------------------------------------------------
  ! --- STOP A NAMED SECTION TIMER
  ! -------------------------------------------------------------
  SUBROUTINE Timer_StopSection(this,cSectionName)
    CLASS(ProgramTimerType),INTENT(INOUT) :: this
    CHARACTER(LEN=*),INTENT(IN)           :: cSectionName

    !Local variables
    INTEGER :: indx,iEndValues(8)
    REAL(8) :: rElapsed

    CALL DATE_AND_TIME(VALUES = iEndValues)

    DO indx=1,this%nSections
        IF (TRIM(this%Sections(indx)%cName) .EQ. TRIM(cSectionName)) THEN
            IF (.NOT. this%Sections(indx)%lRunning) RETURN
            rElapsed = DateTimeToSeconds(iEndValues) - DateTimeToSeconds(this%Sections(indx)%StartValues)
            this%Sections(indx)%rAccumulatedSec = this%Sections(indx)%rAccumulatedSec + rElapsed
            this%Sections(indx)%lRunning = .FALSE.
            this%Sections(indx)%EndValues = iEndValues
            RETURN
        END IF
    END DO

  END SUBROUTINE Timer_StopSection


  ! -------------------------------------------------------------
  ! --- GET ACCUMULATED TIME FOR A NAMED SECTION
  ! -------------------------------------------------------------
  SUBROUTINE Timer_GetSectionTime(this,cSectionName,rSeconds)
    CLASS(ProgramTimerType),INTENT(IN) :: this
    CHARACTER(LEN=*),INTENT(IN)        :: cSectionName
    REAL(8),INTENT(OUT)                :: rSeconds

    !Local variables
    INTEGER :: indx

    rSeconds = 0.0d0

    DO indx=1,this%nSections
        IF (TRIM(this%Sections(indx)%cName) .EQ. TRIM(cSectionName)) THEN
            rSeconds = this%Sections(indx)%rAccumulatedSec
            RETURN
        END IF
    END DO

  END SUBROUTINE Timer_GetSectionTime


  ! -------------------------------------------------------------
  ! --- PRINT TIMER REPORT FOR ALL SECTIONS
  ! -------------------------------------------------------------
  SUBROUTINE Timer_PrintReport(this,iUnit)
    CLASS(ProgramTimerType),INTENT(INOUT) :: this
    INTEGER,OPTIONAL,INTENT(IN)           :: iUnit

    !Local variables
    INTEGER          :: indx,iOut,Hour,Minute
    REAL(8)          :: Second,rTotalSec,rPct
    CHARACTER(LEN=5) :: CHour,CMinute

    !Determine output unit
    IF (PRESENT(iUnit)) THEN
        iOut = iUnit
    ELSE
        iOut = OUTPUT_UNIT
    END IF

    !No sections to report
    IF (this%nSections .EQ. 0) RETURN

    !Stop timer if not already stopped to get total time
    IF (.NOT. this%lStopped) CALL this%Stop()
    CALL this%GetRunTime(Hour,Minute,Second)
    rTotalSec = Hour*3600.0d0 + Minute*60.0d0 + Second

    !Print header
    WRITE (iOut,'(A)') ''
    WRITE (iOut,'(A)') REPEAT('*',66)
    WRITE (iOut,'(A)') 'SECTION TIMING REPORT'
    WRITE (iOut,'(A)') REPEAT('-',66)
    WRITE (iOut,'(A40,A8,A12,A6)') 'Section','Calls','Time (s)','%'
    WRITE (iOut,'(A)') REPEAT('-',66)

    !Print each section
    DO indx=1,this%nSections
        IF (rTotalSec .GT. 0.0d0) THEN
            rPct = 100.0d0 * this%Sections(indx)%rAccumulatedSec / rTotalSec
        ELSE
            rPct = 0.0d0
        END IF
        WRITE (iOut,'(A40,I8,F12.3,F6.1)') &
            this%Sections(indx)%cName,      &
            this%Sections(indx)%iCallCount,  &
            this%Sections(indx)%rAccumulatedSec, &
            rPct
    END DO

    !Print total
    WRITE (iOut,'(A)') REPEAT('-',66)
    WRITE (CHour,'(I5)') Hour
    WRITE (CMinute,'(I5)') Minute
    CHour   = ADJUSTL(CHour)
    CMinute = ADJUSTL(CMinute)
    WRITE (iOut,'(A40,8X,F12.3,A6)') 'Total',rTotalSec,'100.0'
    WRITE (iOut,'(A)') REPEAT('*',66)

  END SUBROUTINE Timer_PrintReport


! *************************************************************
! *************************************************************
! **** BACKWARD-COMPATIBLE WRAPPER PROCEDURES
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- START TIMER (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE StartTimer()

    CALL DefaultTimer%Start()

  END SUBROUTINE StartTimer


  ! -------------------------------------------------------------
  ! --- STOP TIMER (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE StopTimer()

    CALL DefaultTimer%Stop()

  END SUBROUTINE StopTimer


  ! -------------------------------------------------------------
  ! --- GET RUN TIME (wrapper)
  ! -------------------------------------------------------------
  SUBROUTINE GetRunTime(Hour,Minute,Second)
    INTEGER,INTENT(OUT) :: Hour,Minute
    REAL(8),INTENT(OUT) :: Second

    CALL DefaultTimer%GetRunTime(Hour,Minute,Second)

  END SUBROUTINE GetRunTime


  ! -------------------------------------------------------------
  ! --- TIMER STOPPED FLAG (wrapper function replacing SAVE variable)
  ! -------------------------------------------------------------
  FUNCTION TimerStopped() RESULT(lStopped)
    LOGICAL :: lStopped

    lStopped = DefaultTimer%lStopped

  END FUNCTION TimerStopped


END MODULE ProgramTimer
