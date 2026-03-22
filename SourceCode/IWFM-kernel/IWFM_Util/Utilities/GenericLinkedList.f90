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
MODULE GenericLinkedList
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
  PUBLIC :: GenericLinkedListType  
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC NODE TYPE
  ! -------------------------------------------------------------
  TYPE LLNodeType
      CLASS(*),ALLOCATABLE     :: Value
      TYPE(LLNodeType),POINTER :: pNext  => NULL()
  END TYPE LLNodeType
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC LIST TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: GenericLinkedListType
      PRIVATE
      INTEGER                  :: iNNodes  =  0        !Number of nodes in the list
      TYPE(LLNodeType),POINTER :: pHead    => NULL()   !Head of the list
      TYPE(LLNodeType),POINTER :: pTail    => NULL()   !Tail of the list
      TYPE(LLNodeType),POINTER :: pCurrent => NULL()   !Current node in the list
  CONTAINS
      PROCEDURE,NON_OVERRIDABLE,PASS :: AddNode         
      PROCEDURE,NON_OVERRIDABLE,PASS :: GetNNodes       
      PROCEDURE,NON_OVERRIDABLE,PASS :: GetCurrentValue 
      PROCEDURE,NON_OVERRIDABLE,PASS :: Reset           
      PROCEDURE,NON_OVERRIDABLE,PASS :: Next            
      PROCEDURE,NON_OVERRIDABLE,PASS :: Delete          
      PROCEDURE,NON_OVERRIDABLE,PASS :: GetArray        => GenericLinkedList_ConvertToIntegerArray
  END TYPE GenericLinkedListType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'GenericLinkedList::'
  
    
  
  
CONTAINS
    
    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- DELETE LIST 
  ! -------------------------------------------------------------
  SUBROUTINE Delete(List)
    CLASS(GenericLinkedListType),TARGET :: List
    
    !Local variables
    INTEGER                  :: indx
    TYPE(LLNodeType),POINTER :: pNext
    
    !Return if list is empty
    IF (List%iNNodes .EQ. 0) RETURN
    
    !Delete nodes one by one
    DO indx=1,List%iNNodes-1
        pNext => List%pHead%pNext
        DEALLOCATE (List%pHead%Value)
        NULLIFY (List%pHead%pNext)
        DEALLOCATE (List%pHead)
        List%pHead => pNext
    END DO
    DEALLOCATE (List%pHead%Value)
    NULLIFY (List%pHead%pNext)
    DEALLOCATE (List%pHead)
    NULLIFY (List%pTail)
    NULLIFY (List%pCurrent)
    
    !Set the number of data nodes to zero
    List%iNNodes = 0
    
  END SUBROUTINE Delete



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES IN LIST
  ! -------------------------------------------------------------
  PURE FUNCTION GetNNodes(List) RESULT(iNNodes)
    CLASS(GenericLinkedListType),INTENT(IN) :: List
    INTEGER                                 :: iNNodes
    
    iNNodes = List%iNNodes
    
  END FUNCTION GetNNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA STORED IN THE NODE POINTED BY pCurrent
  ! -------------------------------------------------------------
  FUNCTION GetCurrentValue(List) RESULT(pValue)
    CLASS(GenericLinkedListType),TARGET,INTENT(IN) :: List
    CLASS(*),POINTER                               :: pValue
    
    IF (ASSOCIATED(List%pCurrent)) THEN
        ALLOCATE (pValue , SOURCE=List%pCurrent%Value)
        pValue => List%pCurrent%Value
    ELSE
        NULLIFY(pValue)
    ENDIF

  END FUNCTION GetCurrentValue 
  
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CONVERT AN INTEGER LINKED-LIST TO ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE GenericLinkedList_ConvertToIntegerArray(List,iArray,iStat)
    CLASS(GenericLinkedListType),TARGET,INTENT(IN) :: List
    INTEGER,ALLOCATABLE,INTENT(INOUT)              :: iArray(:)
    INTEGER,INTENT(OUT)                            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+39) :: ThisProcedure = ModName // 'GenericLinkedList_ConvertToIntegerArray'
    INTEGER                      :: indx,iErrorCode
    CHARACTER                    :: cErrorMsg*200
    
    !Initialize
    iStat = 0
    
    !Deallocate array in case it is already allocated
    DEALLOCATE (iArray ,STAT=iErrorCode)
    
    !Make sure that the list is not empty
    IF (List%iNNodes .EQ. 0) THEN
        ALLOCATE (iArray(0))
        RETURN
    END IF
    
    !Allocate return array
    ALLOCATE (iArray(List%iNNodes) , STAT=iErrorCode , ERRMSG=cErrorMsg)
    IF (iErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory to convert a linked list to an integer array.'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store integer linked list in the return array
    CALL List%Reset()
    DO indx=1,List%iNNodes
        SELECT TYPE (pValue => List%pCurrent%Value)
           TYPE IS(INTEGER)
              iArray(indx) = pValue 
        END SELECT
        CALL List%Next()
    END DO
    
  END SUBROUTINE GenericLinkedList_ConvertToIntegerArray
  
  
  ! -------------------------------------------------------------
  ! --- ADD A NODE TO THE LIST
  ! -------------------------------------------------------------
  SUBROUTINE AddNode(List,ValueToStore,iStat)
    CLASS(GenericLinkedListType) :: List
    CLASS(*),INTENT(IN)          :: ValueToStore
    INTEGER,INTENT(OUT)          :: iStat
    
    !Initialize
    iStat = 0
    
    IF (List%iNNodes .EQ. 0) THEN
        ALLOCATE (List%pHead)
        ALLOCATE (List%pHead%Value , SOURCE=ValueToStore)
        List%pTail => List%pHead
    ELSE
        ALLOCATE (List%pTail%pNext)
        List%pTail => List%pTail%pNext
        ALLOCATE (List%pTail%Value , SOURCE=ValueToStore)
    END IF
    
    List%pCurrent => List%pTail
    List%iNNodes  =  List%iNNodes + 1
    
  END SUBROUTINE AddNode
    

  ! -------------------------------------------------------------
  ! --- RESET THE LIST 
  ! -------------------------------------------------------------
  SUBROUTINE Reset(List)
    CLASS(GenericLinkedListType) :: List
    
    List%pCurrent => List%pHead
    
  END SUBROUTINE Reset
  
  
  ! -------------------------------------------------------------
  ! --- MOVE CURENT POINTER TO NEXT NODE IN LIST 
  ! --- Pre-condition: Initial pCurrent must not point to pTail
  ! -------------------------------------------------------------
  SUBROUTINE Next(List)
    CLASS(GenericLinkedListType) :: List
    
    List%pCurrent => List%pCurrent%pNext
    
  END SUBROUTINE Next
  
  
END MODULE