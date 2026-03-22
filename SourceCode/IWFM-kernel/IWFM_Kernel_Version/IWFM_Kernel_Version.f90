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
MODULE IWFM_Kernel_Version
  USE Class_Version  , ONLY: ReadVersion , &
                             VersionType
  IMPLICIT NONE
  

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: ReadVersion        , &
            IWFMKernelVersion  
            
  
  ! -------------------------------------------------------------
  ! --- DATA TYPE TO INHERET THE VERSION DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(VersionType) :: IWFMKernelVersionType
    PRIVATE
  CONTAINS
    PROCEDURE,PASS,PUBLIC :: GetVersion => IWFMKernel_GetVersion 
  END TYPE IWFMKernelVersionType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTTIES
  ! -------------------------------------------------------------
  TYPE(IWFMKernelVersionType) :: IWFMKernelVersion


  
CONTAINS



  ! -------------------------------------------------------------
  ! --- GET DYNAMICALLY CREATED VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION IWFMKernel_GetVersion(Version) RESULT(cVrs)
    CLASS(IWFMKernelVersionType) :: Version
    CHARACTER(:),ALLOCATABLE     :: cVrs
    
    INCLUDE 'IWFMKernelVersion.fi'
        
    !First generate the version number
    Version%VersionType = Version%New(f_cTag,f_cBranch,f_cHash)
    
    !Then, retrieve the version number
    cVrs = Version%VersionType%GetVersion()
    
  END FUNCTION IWFMKernel_GetVersion
          
END MODULE 