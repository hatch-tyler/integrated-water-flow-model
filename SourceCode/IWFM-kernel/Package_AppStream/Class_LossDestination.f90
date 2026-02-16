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
MODULE Class_LossDestination
  USE MessageLogger     , ONLY: SetLastMessage , &
                                MessageArray   , &
                                f_iFatal
  USE GeneralUtilities
  USE IOInterface
  USE Package_Misc
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
  PUBLIC :: LossDestinationType   , &
            LossDestination_New
  

  ! -------------------------------------------------------------
  ! --- LOSS DESTINATION DATA TYPE
  ! -------------------------------------------------------------
  TYPE LossDestinationType
    INTEGER             :: iNDest        = 0
    INTEGER,ALLOCATABLE :: iDestList(:)
    REAL(8),ALLOCATABLE :: rFracs(:)
  END TYPE LossDestinationType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_LossDestination::'
    


CONTAINS



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE LossDestination_New(iNDiver,iDiverIDs,iDestIDs,cBypassOrDiver,cDestDescription,InFile,LossDestinations,iStat)
    INTEGER,INTENT(IN)               :: iNDiver,iDiverIDs(iNDiver),iDestIDs(:)
    CHARACTER(LEN=*),INTENT(IN)      :: cBypassOrDiver,cDestDescription
    TYPE(GenericFileType)            :: InFile
    TYPE(LossDestinationType),TARGET :: LossDestinations(iNDiver)
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19)      :: ThisProcedure = ModName // 'LossDestination_New'
    INTEGER                           :: iNDest,indxDiver,ID,indxDest,iErrorCode,iDiver
    REAL(8)                           :: rDummyArray(4),rDummyArray2(2)
    LOGICAL                           :: lProcessed(iNDiver)
    INTEGER,ALLOCATABLE               :: iDestList(:)
    TYPE(LossDestinationType),POINTER :: pDest
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
    !Iterate over diversions
    DO indxDiver=1,iNDiver
        !Read data
        CALL InFile%ReadData(rDummyArray,iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Diversion ID
        ID = INT(rDummyArray(1))
        CALL ConvertID_To_Index(ID,iDiverIDs,iDiver)
        IF (iDiver .EQ. 0) THEN
            CALL SetLastMessage(TRIM(cBypassOrDiver)//' ID '//TRIM(IntToText(ID))//' listed for '//TRIM(cDestDescription)//' is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure same diversion ID is not used
        IF (lProcessed(iDiver)) THEN
            CALL SetLastMessage(TRIM(cBypassOrDiver)//' ID '//TRIM(IntToText(ID))//' is used more than once for '//TRIM(cDestDescription)//' description!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iDiver) = .TRUE.
        
        pDest => LossDestinations(iDiver)
        
        !Number of destinations
        iNDest       = INT(rDummyArray(2))
        pDest%iNDest = iNDest
        IF (iNDest .EQ. 0) CYCLE
        
        !Allocate memory
        DEALLOCATE (iDestList , STAT=iErrorCode)
        ALLOCATE (iDestList(iNDest) , pDest%iDestList(iNDest) , pDest%rFracs(iNDest) ,STAT=iErrorCode)
        IF (iErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for '//TRIM(cDestDescription)//' for '//TRIM(LowerCase(cBypassOrDiver))//' '//TRIM(IntToText(ID))//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iDestList(1)    = INT(rDummyArray(3))
        pDest%rFracs(1) =     rDummyArray(4)
        
        !Read and assign rest of data
        DO indxDest=2,iNDest
            CALL InFile%ReadData(rDummyArray2,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iDestList(indxDest)    = INT(rDummyArray2(1))
            pDest%rFracs(indxDest) =     rDummyArray2(2)
        END DO
        
        !Convert destination IDs to indices
        DO indxDest=1,iNDest
            IF (iDestList(indxDest) .EQ. 0) THEN
                pDest%iDestList(indxDest) = 0
                CYCLE
            END IF
            CALL ConvertID_To_Index(iDestList(indxDest),iDestIDs,pDest%iDestList(indxDest))
            IF (pDest%iDestList(indxDest) .EQ. 0) THEN
                CALL SetLastMessage('Destination '//TRIM(IntToText(iDestList(indxDest)))//' for '//TRIM(cDestDescription)//' listed for '//TRIM(LowerCase(cBypassOrDiver))//' ID '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Normalize the fractions
        IF (iNDest .GT. 0) CALL NormalizeArray(pDest%rFracs)
      
    END DO
    
  END SUBROUTINE LossDestination_New
  
END MODULE