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
    
!--Abstract base class for all transport methods
MODULE Class_TransportBase
  USE TransportDataReader    , ONLY: TransportDataReaderType
  USE Package_AppGW          , ONLY: AppGWType       
  USE TimeSeriesUtilities    , ONLY: TimeStepType                   
  USE IOInterface            , ONLY: GenericFileType                     
  USE Package_Discretization , ONLY: AppGridType                , &
                                     StratigraphyType             
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
  PUBLIC :: TransportMethodType , &
            f_IKIND
  
  
  ! -------------------------------------------------------------
  ! --- DOUBLE PRECISION KIND DESCRIPTOR
  ! -------------------------------------------------------------
  INTEGER, PARAMETER :: f_IKIND=8
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen  = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'TransportBase::'
   
   
  ! -------------------------------------------------------------
  ! --- TRANSPORT TIME STEP TYPE
  ! -------------------------------------------------------------    
  TYPE, EXTENDS(TimeStepType) :: LagTransTimeStepType
      INTEGER :: NumFlowTimeSteps
      INTEGER :: nsubTimeSteps
      INTEGER :: NumOutTimeSteps
      INTEGER :: OutSkip
      INTEGER :: RelSkip
  END TYPE LagTransTimeStepType


  !-------------------------
  ! Abstract root type
  !-------------------------
  TYPE, ABSTRACT :: TransportMethodType
      CLASS(TransportDataReaderType), POINTER :: DataProvider => NULL()
      LOGICAL                                 :: lInitialized   = .FALSE.
      LOGICAL                                 :: lDefined       = .FALSE.
      TYPE(GenericFileType),ALLOCATABLE       :: ResultsFile                         !File to print final simulation results
      TYPE(LagTransTimeStepType)              :: TimeStep
  CONTAINS
      PROCEDURE(InitializeInterface),DEFERRED :: New
      PROCEDURE(SimulateInterface),DEFERRED   :: SimulateOneTimeStep
      PROCEDURE(OutputResults), DEFERRED      :: PrintResults
      PROCEDURE,NOPASS                        :: CalculatePoreVelocity
      PROCEDURE                               :: AttachDataProivider
  END TYPE TransportMethodType
  

  ABSTRACT INTERFACE
        SUBROUTINE InitializeInterface(This, DataProvider,  IsForInquiry,cFileName,cWorkingDirectory, AppGrid,Stratigraphy,TimeStep,  iStat)
          IMPORT                                       :: TransportMethodType, TransportDataReaderType,  AppGridType, StratigraphyType, TimeStepType
          CLASS(TransportMethodType),INTENT(INOUT)     :: This
          CLASS(TransportDataReaderType),INTENT(INOUT) :: DataProvider
          LOGICAL,INTENT(IN)                           :: IsForInquiry
          CHARACTER(LEN=*),INTENT(IN)                  :: cFileName,cWorkingDirectory
          TYPE(AppGridType),INTENT(IN)                 :: AppGrid
          TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy
          TYPE(TimeStepType),INTENT(IN)                :: TimeStep
          INTEGER,INTENT(OUT)                          :: iStat
        END SUBROUTINE InitializeInterface
        
        SUBROUTINE SimulateInterface(This, AppGW, AppGrid,Stratigraphy,TimeStep,  iStat)
          IMPORT                                   :: TransportMethodType, AppGWType, AppGridType, StratigraphyType, TimeStepType
          CLASS(TransportMethodType),INTENT(INOUT) :: This
          TYPE(AppGWType),INTENT(IN)               :: AppGW  
          TYPE(AppGridType),INTENT(IN)             :: AppGrid
          TYPE(StratigraphyType),INTENT(IN)        :: Stratigraphy
          TYPE(TimeStepType),INTENT(IN)            :: TimeStep
          INTEGER,INTENT(OUT)                      :: iStat
        END SUBROUTINE SimulateInterface
          
        SUBROUTINE OutputResults (This, AppGW, AppGrid, Stratigraphy, TimeStep, iStat)
          IMPORT                                   :: TransportMethodType, AppGWType, AppGridType, StratigraphyType,TimeStepType
          CLASS(TransportMethodType),INTENT(INOUT) :: This
          TYPE(AppGWType),INTENT(IN)               :: AppGW
          TYPE(AppGridType),INTENT(IN)             :: AppGrid
          TYPE(StratigraphyType),INTENT(IN)        :: Stratigraphy
          TYPE(TimeStepType),INTENT(IN)            :: TimeStep
          INTEGER,INTENT(OUT)                      :: iStat
        END SUBROUTINE OutputResults

   END INTERFACE


CONTAINS
   
    
  ! -------------------------------------------------------------
  ! --- ATTACH  DATA PROVIDER FOR SOLUTE TRANSPORT
  ! -------------------------------------------------------------                                                                         
  SUBROUTINE AttachDataProivider(This, DataProvider)
    CLASS(TransportMethodType), INTENT(INOUT)             :: This
    CLASS(TransportDataReaderType), TARGET, INTENT(INOUT) :: DataProvider
    
    This%DataProvider => DataProvider
        
  END SUBROUTINE AttachDataProivider
    
      
  ! -------------------------------------------------------------
  ! --- CALCULATE PORE VELOCITY
  ! -------------------------------------------------------------
  SUBROUTINE CalculatePoreVelocity(Porosity, Vx, Vy, Vz)
    REAL(KIND=f_IKIND),INTENT(IN)         :: Porosity(:,:)
    REAL(KIND=f_IKIND),INTENT(INOUT)      :: Vx(:,:),Vy(:,:),Vz(:,:)
  
    !-Calculate pore-velocity
    Vx = Vx / Porosity
    Vy = Vy / Porosity
    Vz = Vz / Porosity
  
  END SUBROUTINE CalculatePoreVelocity
  

END MODULE  Class_TransportBase