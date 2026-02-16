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
PROGRAM IWFM_F1
  !$ USE OMP_LIB
  USE ProgramTimer      , ONLY: StartTimer       , &
                                StopTimer
  USE MessageLogger     , ONLY: PrintRunTime     , &
                                SetLogFileName   , &
                                KillLogFile      , &
                                LogLastMessage
  USE Package_Misc      , ONLY: Print_Screen     , &
                                Get_Main_File 
  USE Package_Model     , ONLY: ModelType
  USE IWFM_Version      , ONLY: IWFMVersion                                   
  IMPLICIT NONE

  !Local variables
  TYPE(ModelType) :: Model
  INTEGER         :: iStat
  CHARACTER       :: cPPFileName*500
  
  
  !Set environment for parallel processing
  !$ CALL KMP_SET_BLOCKTIME(0)                                !Let threads sleep right away 
  !$ CALL OMP_SET_NUM_THREADS(MIN(OMP_GET_NUM_PROCS() , 16))  !Set number of threads to minimum of 16 or number of available processors 
  !$ CALL OMP_SET_MAX_ACTIVE_LEVELS(2)                        !Maximum 2 levels of nested paralellization
  !$ CALL KMP_SET_STACKSIZE_S(16777216)                       !Set thread stack size to 16MB
  
  
  !Start timer
  CALL StartTimer()
  
  !Set message log file
  CALL SetLogFileName('PreprocessorMessages.out',iStat)
  IF (iStat .EQ. -1) THEN
      CALL LogLastMessage()
      
  ELSE
      !Display opening screen and obtain inpout file name(s)
      CALL Print_screen('Program: Pre-Processor',IWFMVersion)
      CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cPPFileName)
      IF (TRIM(cPPFileName) .EQ. '-about') THEN
          CALL Model%PrintVersionNumbers()
          STOP
      END IF

      !Instantiate the static component of the model
      CALL Model%New(cPPFileName,lRoutedStreams=.TRUE.,lPrintBinFile=.TRUE.,iStat=iStat)
      IF (iStat .EQ. -1) CALL LogLastMessage()
  END IF
  
  !Print run-time 
  CALL StopTimer()
  CALL PrintRunTime()
  
  !Close message log file
  CALL KillLogFile()

END
