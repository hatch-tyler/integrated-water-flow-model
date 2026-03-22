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
MODULE IWFM_Version
  USE Class_Version , ONLY: ReadVersion , &
                            VersionType
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: ReadVersion  , &
            IWFMVersion

  ! -------------------------------------------------------------
  ! --- DATA TYPE TO INHERIT THE VERSION DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(VersionType) :: IWFMVersionType
    PRIVATE
  CONTAINS
    PROCEDURE,PASS,PUBLIC :: GetVersion => IWFM_GetVersion 
  END TYPE IWFMVersionType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTTIES
  ! -------------------------------------------------------------
  TYPE(IWFMVersionType) :: IWFMVersion

  

CONTAINS



  ! -------------------------------------------------------------
  ! --- GET DYNAMICALLY CREATED VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION IWFM_GetVersion(Version) RESULT(cVrs)
    CLASS(IWFMVersionType)   :: Version
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    INCLUDE 'IWFMVersion.fi'
        
    !First generate the version number
    IF (.NOT. Version%IsDefined())   &
      Version%VersionType = Version%VersionType%New(f_cTag,f_cBranch,f_cHash)
    
    !Then, retrieve the version number
    cVrs = Version%VersionType%GetVersion()
    
  END FUNCTION IWFM_GetVersion
  
END MODULE IWFM_Version