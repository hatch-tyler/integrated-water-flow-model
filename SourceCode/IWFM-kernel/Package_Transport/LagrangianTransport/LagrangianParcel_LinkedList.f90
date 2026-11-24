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
MODULE LagrangianParcel_LinkedList
 ! USE LagrangianRNG
  USE MessageLogger  , ONLY: SetLastMessage  , &
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
  PUBLIC :: LPNodeType       ,  &
            LPLinkedListType  
  
  
  ! -------------------------------------------------------------
  ! --- NODE TYPE REPRESENTING A LAGRANGIAN PARTICLE
  ! -------------------------------------------------------------
  TYPE LPNodeType
      INTEGER                   :: ID
      INTEGER                   :: Igridx
      INTEGER                   :: Igridy
      INTEGER                   :: Igridz
      INTEGER                   :: Elem
      INTEGER,ALLOCATABLE       :: RNGState(:)
      REAL(KIND=8)              :: xcord
      REAL(KIND=8)              :: ycord
      REAL(KIND=8)              :: zcord
      LOGICAL                   :: Ingrid
      TYPE(LPNodeType), POINTER :: Next=>NULL()
  END TYPE LPNodeType  
  
  
  ! -------------------------------------------------------------
  ! --- LP LINKED LIST TYPE
  ! -------------------------------------------------------------
  !--Define link list for LPs
  TYPE  LPLinkedListType
      TYPE (LPNodeType), POINTER :: Head=>NULL()
      TYPE (LPNodeType), POINTER :: Tail=>NULL()        ! Pointer to last node
      INTEGER                    :: TotNumParcels, NumParDelete
  CONTAINS
      PROCEDURE :: Initialize => InitalizeList
      PROCEDURE :: InsertLP   => InsertNode
      PROCEDURE :: RemoveLP   => RemoveNode
  END TYPE LPLinkedListType
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen =29
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'LagrangianParcel_LinkedList::'
  

CONTAINS


  ! -------------------------------------------------------------
  ! --- INITIALIZE THE LIST
  ! -------------------------------------------------------------
  SUBROUTINE InitalizeList(This)
    CLASS(LPLinkedListType), INTENT(INOUT) :: This
    
    This%Head=>NULL()
    This%Tail=>NULL()
    This%TotNumParcels=0   
    
  END SUBROUTINE InitalizeList
      
  
  ! -------------------------------------------------------------
  ! --- INSERT PARCELS TO THE LIST 
  ! -------------------------------------------------------------
  SUBROUTINE InsertNode (This, xcrd, ycrd, zcrd, ielem, iStat) 
    CLASS(LPLinkedListType), INTENT(INOUT) :: This
    REAL(KIND=8), INTENT(IN)               :: xcrd, ycrd, zcrd
    INTEGER, INTENT (IN)                   :: ielem
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    TYPE (LPNodeType), POINTER :: new_node
    INTEGER                    :: new_id
    
    iStat=0
    !--Allocate New Node for LP
    ALLOCATE(new_node)
    
    IF (.NOT. ASSOCIATED( This%Tail) ) THEN
        new_id=1
     ELSE
        new_id=This%Tail%ID+1
    ENDIF
    
    new_node%next=>NULL ()
    new_node%ID=new_id
    !--Insert new node the list at the end using the tail pointer
    IF (.NOT. ASSOCIATED(This%Head)) THEN
        This%Head=>new_node
        This%Tail=>new_node
    ELSE
        This%Tail%Next=>new_node
        This%Tail=>new_node
    ENDIF
    
        This%Tail%Xcord=xcrd
        This%Tail%Ycord=ycrd
        This%Tail%Zcord=zcrd
        This%Tail%Elem=ielem 
        This%Tail%Ingrid=.TRUE.          ! within domain Ingrid=T; Outside the domain Ingrid=F    
        
    This%TotNumParcels=This%TotNumParcels+1
      
  END SUBROUTINE InsertNode
    
   
  ! -------------------------------------------------------------
  ! --- REMOVE A LP FROM THE LIST
  ! -------------------------------------------------------------
  SUBROUTINE RemoveNode (This, ParcelID, nextNode, found, iStat)
    CLASS(LPLinkedListType), INTENT(INOUT) :: This
    INTEGER, INTENT (IN)                   :: ParcelID
    TYPE (LPNodeType), POINTER, INTENT(OUT):: nextNode
    LOGICAL, INTENT(OUT)                   :: found
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    TYPE (LPNodeType), POINTER             :: current, previous
    INTEGER                                :: icount
    
    !--Initialize
    iStat=0
    found=.FALSE.
    
    IF(.NOT. ASSOCIATED(This%Head) ) RETURN
    
    current     =>This%Head
    previous  =>NULL()
    
    !--Special case: Delete head node
    IF (current%ID==ParcelID) THEN
        nextNode =>current%next          !-save next node before deallocation
        This%Head=>current%next              !-promote next node to head 
        DEALLOCATE(current)
        

        IF (.NOT. ASSOCIATED (This%Head))THEN
            This%Tail=>NULL() ! List now empty
        END IF
  
        This%TotNumParcels=This%TotNumParcels-1
        This%NumParDelete=This%NumParDelete+1                
        found=.TRUE.
        RETURN
    ENDIF
    
    !--Traverse to the specifed node and remove it
    icount=0
    DO WHILE (ASSOCIATED(current))
   
            IF (ASSOCIATED(current) .AND. current%ID==ParcelID) THEN
                previous%next=>current%next    
                nextNode => current%Next 
                IF (.NOT.ASSOCIATED(previous%next) ) THEN
                        This%Tail=> previous ! update tail if the last node was deleted
                END IF
                DEALLOCATE(current)
                
                This%TotNumParcels=This%TotNumParcels-1
                This%NumParDelete=This%NumParDelete+1
                found=.TRUE.
                RETURN
            ENDIF
            
            previous =>current
            current  => current%next

    END DO
    
  END SUBROUTINE RemoveNode
    
END MODULE LagrangianParcel_LinkedList
    
    
    
    