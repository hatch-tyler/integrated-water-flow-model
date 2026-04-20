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
MODULE Package_AppGW
  USE MessageLogger        , ONLY: MessageLoggerType
  USE Package_AppTileDrain , ONLY: f_iTileDrain                      , &
                                   f_iSubIrig
  USE Package_AppPumping   , ONLY: f_iPump_Well                      , &
                                   f_iPump_ElemPump
  USE Class_AppSubsidence_v40, ONLY: AppSubsidence_v40_SetModuleLogger
  USE Class_AppSubsidence_v50, ONLY: AppSubsidence_v50_SetModuleLogger
  USE Class_AppGW   ! Re-export all public entities (AppGWType, constants, etc.)
  USE GWHydrograph         , ONLY: GWHydrograph_SetModuleLogger
  USE Class_LayerBC        , ONLY: LayerBC_SetModuleLogger
  USE Class_Well           , ONLY: Well_SetModuleLogger
  USE Class_ElementPumping , ONLY: ElemPump_SetModuleLogger
  IMPLICIT NONE

  ! All public entities from Class_AppGW are re-exported via USE without ONLY
  PUBLIC :: AppGW_SetAllModuleLoggers

CONTAINS


  ! -------------------------------------------------------------
  ! --- PROPAGATE LOGGER TO ALL SUB-MODULES
  ! -------------------------------------------------------------
  SUBROUTINE AppGW_SetAllModuleLoggers(Logger)
    TYPE(MessageLoggerType), TARGET, INTENT(INOUT) :: Logger

    !Main orchestrator
    CALL AppGW_SetModuleLogger(Logger)

    !Hydrograph
    CALL GWHydrograph_SetModuleLogger(Logger)

    !Boundary conditions
    CALL LayerBC_SetModuleLogger(Logger)

    !Pumping
    CALL Well_SetModuleLogger(Logger)
    CALL ElemPump_SetModuleLogger(Logger)

    !Subsidence
    CALL AppSubsidence_v40_SetModuleLogger(Logger)
    CALL AppSubsidence_v50_SetModuleLogger(Logger)

  END SUBROUTINE AppGW_SetAllModuleLoggers

END MODULE
