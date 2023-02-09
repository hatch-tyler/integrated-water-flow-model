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
MODULE Class_IrigFracFile
  USE MessageLogger        , ONLY: SetLastMessage       , &
                                   MessageArray         , &
                                   f_iFatal
  USE GeneralUtilities     , ONLY: IntToText
  USE TimeSeriesUtilities  , ONLY: TimeStepType
  USE IOInterface          , ONLY: RealTSDataInFileType 
  USE Package_AppGW        , ONLY: AppGWType
  USE Package_AppStream    , ONLY: AppStreamType
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
  PUBLIC  :: IrigFracFileType   
  
  
  ! -------------------------------------------------------------
  ! --- IRRIGATION FRACTIONS DATA FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: IrigFracFileType
    PRIVATE
  CONTAINS
    PROCEDURE,PASS :: New 
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: IrigFracFile_ReadTSData
    GENERIC        :: ReadTSData              => IrigFracFile_ReadTSData
  END TYPE
   
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_IrigFracFile::'
  
  
  
    
CONTAINS

    
    

  ! -------------------------------------------------------------
  ! --- NEW IRRIGATION FRACTIONS FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(IrigFracFile,cFileName,cWorkingDirectory,TimeStep,iStat) 
    CLASS(IrigFracFileType),INTENT(OUT) :: IrigFracFile
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    REAL(8) :: DummyFactor(1)
    
    !Initialize
    iStat = 0
    
    !If no file name is specified, return
    IF (cFileName .EQ. '') RETURN
    
    !Instantiate the data type
    CALL IrigFracFile%Init(cFileName,cWorkingDirectory,'irrigation fractions data file',TimeStep%TrackTime,BlocksToSkip=1,lFactorDefined=.FALSE.,Factor=DummyFactor,iStat=iStat)
    
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- KILL IRRIGATION FRACTIONS FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill(IrigFracFile)
    CLASS(IrigFracFileType) :: IrigFracFile
    
    CALL IrigFracFile%Close()
    
  END SUBROUTINE Kill
    
  
  ! -------------------------------------------------------------
  ! --- READ IRRIGATION FRACTIONS DATA
  ! -------------------------------------------------------------
  SUBROUTINE IrigFracFile_ReadTSData(IrigFracFile,AppStream,lDiversionAdjusted,AppGW,lPumpingAdjusted,TimeStep,iStat)
    CLASS(IrigFracFileType)       :: IrigFracFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppStreamType)           :: AppStream
    TYPE(AppGWType)               :: AppGW
    LOGICAL,INTENT(IN)            :: lDiversionAdjusted,lPumpingAdjusted
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReadTSData'
    INTEGER                      :: FileReadCode,indxCol
    
    !Read time series data
    CALL IrigFracFile%ReadTSData(TimeStep,'Irrigations fractions data',FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
        
    !Update the supply irrigtaion fractions if read with no problems
    SELECT CASE (FileReadCode)
        !It wasn't time to read
        CASE (-1)
            IF (lDiversionAdjusted) CALL AppStream%ResetIrigFracs()
            IF (lPumpingAdjusted) CALL AppGW%ResetIrigFracs()
          
        !Data was read with sucess
        CASE (0)
            !Make sure that irrigation fraction is either 1.0 (ag water) or 0.0 (urban water)
            DO indxCol=1,IrigFracFile%GetNDataColumns()
                IF (IrigFracFile%rValues(indxCol) .NE. 1.0) THEN
                    IF (IrigFracFile%rValues(indxCol) .NE. 0.0) THEN
                        MessageArray(1) = 'Column '//TRIM(IntToText(indxCol))//' of the Irrigation Fractions Data File indicates that'
                        MessageArray(2) = 'a water supply is serving both agricultural and urban water demands.'
                        MessageArray(3) = 'This is no longer supported due to computational difficulties. A supply can'
                        MessageArray(4) = 'now be either an agricultural water supply or urban water supply, but not both.'
                        CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
            END DO
            !Fraction of diversion to be used for agricultural purposes
            CALL AppStream%SetIrigFracsRead(IrigFracFile%rValues)
            !Fraction of pumping to be used for agricultural purposes
            CALL AppGW%SetIrigFracsRead(IrigFracFile%rValues)
    END SELECT

  END SUBROUTINE IrigFracFile_ReadTSData
  
  
END MODULE