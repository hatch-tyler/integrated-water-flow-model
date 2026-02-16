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
PROGRAM IWFM_F2
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
  CHARACTER       :: cSimFileName*500,cWSAFileName*500
  INTEGER         :: iStat
  !$ INTEGER      :: iBlockTime

  !Set parallel thread environment
  !$ iBlockTime = KMP_GET_BLOCKTIME()
  !$ CALL KMP_SET_BLOCKTIME(0)                                !Let therads sleep right away
  !$ CALL OMP_SET_NUM_THREADS(MIN(OMP_GET_NUM_PROCS() , 16))  !Set number of threads to minimum of 16 or number of available processors
  !$ CALL OMP_SET_MAX_ACTIVE_LEVELS(2)                        !Maximum 2 levels of nested paralellization
  !$ CALL KMP_SET_STACKSIZE_S(16777216)                       !Set thread stack size to 16MB

  
  !Start program timer
  CALL StartTimer()


  !Standard output file
  CALL SetLogFileName('SimulationMessages.out',iStat)
  IF (iStat .EQ. -1) THEN
      CALL LogLastMessage()
  
  ELSE
      !Display opening screen and obtain inpout file name(s)
      CALL Print_screen('Program: Simulation',IWFMVersion)
      CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cSimFileName,cWSAFileName)
      IF (TRIM(cSimFileName) .EQ. '-about') THEN
          CALL Model%PrintVersionNumbers()
          STOP
      END IF

      !Instantiate model
      CALL Model%New('IWFM',cSimFileName,cWSAFileName,lForInquiry=.FALSE.,iStat=iStat)
           
      !If an error, print it and stop
      IF (iStat .EQ. -1) THEN
          CALL LogLastMessage()
          
      !Otherwise, simulate
      ELSE
          CALL Model%Simulate(0,iStat)
          IF (iStat .EQ. -1) CALL LogLastMessage()
          
      END IF
  END IF
  
  !Kill model and clear memory
  CALL Model%Kill()
  
  !Complete the simulation and print model run time 
  CALL StopTimer()
  CALL PrintRunTime()
  CALL KillLogFile()
  
  !$ CALL KMP_SET_BLOCKTIME(iBlockTime)
  
END
