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
MODULE Class_RVETFromStrm
  USE MessageLogger           , ONLY: SetLastMessage              , &
                                      MessageArray                , & 
                                      f_iFatal                      
  USE Package_Discretization  , ONLY: AppGridType                 
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
  PUBLIC :: RVETFromStrmType                           


  ! -------------------------------------------------------------
  ! --- ELEMENT-TO-STREAM-NODE CONNECTION
  ! -------------------------------------------------------------
  TYPE ElemToStrmType
      PRIVATE
      INTEGER             :: iStrmNode          = 0
      REAL(8),ALLOCATABLE :: rETStrm_Required(:)     !For each (riparian veg)
      REAL(8),ALLOCATABLE :: rETStrm_Actual(:)       !For each (riparian veg)
  END TYPE ElemToStrmType
  

  ! -------------------------------------------------------------
  ! --- STREAM-NODE-TO-ELEMENT CONNECTION
  ! -------------------------------------------------------------
  TYPE StrmToElemType
      PRIVATE
      INTEGER             :: iNElems   = 0
      INTEGER,ALLOCATABLE :: iElems(:)
  END TYPE StrmToElemType

  
  ! -------------------------------------------------------------
  ! --- RIPARIAN ET FROM STREAM DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE RVETFromStrmType
      PRIVATE
      INTEGER                          :: iNStrmNodes              = 0       !Maximum stream node number from which riparian ET is taken out of (may be less than the total number of stream nodes simulated)
      TYPE(ElemToStrmType),ALLOCATABLE :: ElemToStrm(:)                     
      TYPE(StrmToElemType),ALLOCATABLE :: StrmToElem(:)
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: GetActualET_AtElements        
      PROCEDURE,PASS :: GetActualET_AtStrmNodes      
      PROCEDURE,PASS :: GetActualET_AtRegions         
      PROCEDURE,PASS :: GetRequiredET_AtStrmNodes
      PROCEDURE,PASS :: GetUnmetET_AtElements         
      PROCEDURE,PASS :: GetUnmetET_AtStrmReaches      
      PROCEDURE,PASS :: GetUnmetET_AtStrmNodes        
      PROCEDURE,PASS :: SetActualET_AtStrmNodes       
      PROCEDURE,PASS :: SetRequiredET_AtElements      
      PROCEDURE,PASS :: IsSimulated
      PROCEDURE,PASS :: AdvanceState
  END TYPE RVETFromStrmType
  
  
  ! -------------------------------------------------------------
  ! --- MISC VARIABLES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_RVETFromStrm::'
  
  
  
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
  ! --- INSTANTIATE ELEMENT-TO-STREAM CONNECTIVITY
  ! -------------------------------------------------------------
  SUBROUTINE New(RVETFromStrm,iStrmNodes,iNRiparian,iStat)
    CLASS(RVETFromStrmType) :: RVETFromStrm
    INTEGER,INTENT(IN)      :: iStrmNodes(:),iNRiparian
    INTEGER,INTENT(OUT)     :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: iErrorCode,indxElem,iStrmNode,iNStrmNodes,iNElements

    !Initialize
    iStat      = 0
    iNElements = SIZE(iStrmNodes)
    
    !Obtain the maximum stream node number
    iNStrmNodes = MAXVAL(iStrmNodes)
    
    !Return if streams are not simulated
    IF (iNStrmNodes .EQ. 0) RETURN
  
    !Otherwise allocate memory
    RVETFromStrm%iNStrmNodes = iNStrmNodes
    ALLOCATE (RVETFromStrm%ElemToStrm(iNElements) , RVETFromStrm%StrmToElem(iNStrmNodes) , STAT=iErrorCode)
    IF (iErrorCode .NE. 0) THEN
        MessageArray(1) = 'Error allocating memory for element-to-stream node connectivity ' 
        MessageArray(2) = 'for the simulation of riparian vegetation!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    DO indxElem=1,iNElements
        ALLOCATE (RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Required(iNRiparian) , &
                  RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Actual(iNRiparian)   ) 
    END DO               
    
    !Read data and instantiate variables
    DO indxElem=1,iNElements 
       iStrmNode = iStrmNodes(indxElem)
       
       !Add stream node to element data
       RVETFromStrm%ElemToStrm(indxElem)%iStrmNode        = MAX(iStrmNode , 0)
       RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Required = 0.0
       RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Actual   = 0.0
       
       !Add element to stream node data
       IF (iStrmNode .GT. 0) CALL AddElemToStrmNode(indxElem,RVETFromStrm%StrmToElem(iStrmNode))
       
    END DO
    
  END SUBROUTINE New 
  
  
  ! -------------------------------------------------------------
  ! --- ADD ELEMENT ID TO STREAM DATA 
  ! -------------------------------------------------------------
  SUBROUTINE AddElemToStrmNode(iElem,StrmToElem)
    INTEGER,INTENT(IN)   :: iElem
    TYPE(StrmToElemType) :: StrmToElem
    
    !Local variables
    INTEGER             :: iNElems
    INTEGER,ALLOCATABLE :: iTempArray(:)
    
    !Initialize
    iNElems = StrmToElem%iNElems
    
    !Store old data in temporary array and append new element ID
    ALLOCATE (iTempArray(iNElems+1))
    iTempArray(1:iNElems) = StrmToElem%iElems
    iNElems               = iNElems + 1
    iTempArray(iNElems  ) = iElem
    
    !Trasfer new info to the persistent arrays
    CALL MOVE_ALLOC(iTempArray , StrmToElem%iElems)
    StrmToElem%iNElems = iNElems
       
  END SUBROUTINE AddElemToStrmNode
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL RVETFromStrm OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(RVETFromStrm)
    CLASS(RVETFromStrmType) :: RVETFromStrm
    
    !Local variables
    INTEGER                :: iErrorCode,indx
    TYPE(RVETFromStrmType) :: Dummy
    
    DO indx=1,RVETFromStrm%iNStrmNodes
        DEALLOCATE (RVETFromStrm%StrmToElem(indx)%iElems ,STAT=iErrorCode)
    END DO
    DO indx=1,SIZE(RVETFromStrm%ElemToStrm)
        DEALLOCATE (RVETFromStrm%ElemToStrm(indx)%rETStrm_Required , RVETFromStrm%ElemToStrm(indx)%rETStrm_Actual ,STAT=iErrorCode)
    END DO
    DEALLOCATE (RVETFromStrm%StrmToElem , RVETFromStrm%ElemToStrm , STAT=iErrorCode)
    
    SELECT TYPE (RVETFromStrm)
        TYPE IS (RVETFromStrmType)
            RVETFromStrm = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  
  
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
  ! --- GET REQUIRED OUTFLOW DUE TO RIPARIAN ET AT ALL STREAM NODES 
  ! -------------------------------------------------------------
  SUBROUTINE GetRequiredET_AtStrmNodes(RVETFromStrm,rETOutflow)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFromStrm
    REAL(8),INTENT(OUT)                :: rETOutflow(:)
    
    !Local variables
    INTEGER :: indxStrm,indxElem,iElem
    
    !Initialize
    rETOutflow = 0.0
    
    !Return if riparian ET met by streams is not simulated
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) RETURN
    
    !Compile 
    DO indxStrm=1,RVETFromStrm%iNStrmNodes
        DO indxElem=1,RVETFromStrm%StrmToElem(indxStrm)%iNElems
            iElem                = RVETFromStrm%StrmToElem(indxStrm)%iElems(indxElem)
            rETOutflow(indxStrm) = rETOutflow(indxStrm) + SUM(RVETFromStrm%ElemToStrm(iElem)%rETStrm_Required)
        END DO
    END DO
    
  END SUBROUTINE GetRequiredET_AtStrmNodes

  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL RIPARIAN ET FROM STREAMS AT SUBREGIONS AFFILIATION 
  ! -------------------------------------------------------------
  SUBROUTINE GetActualET_AtRegions(RVETFromStrm,AppGrid,rRegionalET)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFRomStrm
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    REAL(8),INTENT(OUT)                :: rRegionalET(:)
    
    !Local variables
    INTEGER :: indxElem
    REAL(8) :: rElemValues(AppGrid%NElements)
    
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) THEN
        rRegionalET = 0.0
        RETURN
    ELSE
        DO indxElem=1,AppGrid%NElements
            rElemValues(indxElem) = SUM(RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Actual)
        END DO
        rRegionalET = AppGrid%AccumElemValuesToSubregions(rElemValues)
    END IF
    
  END SUBROUTINE GetActualET_AtRegions
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL RIPARIAN ET AT STREAM NODES 
  ! -------------------------------------------------------------
  SUBROUTINE GetActualET_AtStrmNodes(RVETFromStrm,rQRVET)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFromStrm
    REAL(8),INTENT(OUT)                :: rQRVET(:)
    
    !Local variables
    INTEGER :: indxNode,indxElem,iElem
    
    !Initialize
    rQRVET = 0.0
    
    !Compile
    ASSOCIATE (pStrmToElem => RVETFromStrm%StrmToElem , &
               pElemToStrm => RVETFromStrm%ElemToStrm )
        
        DO indxNode=1,SIZE(rQRVET)
            IF (indxNode .GT. RVETFromStrm%iNStrmNodes) CYCLE
            DO indxElem=1,pStrmToElem(indxNode)%iNElems
                iElem            = pStrmToElem(indxNode)%iElems(indxElem)
                rQRVET(indxNode) = rQRVET(indxNode) + SUM(pElemToStrm(iElem)%rETStrm_Actual)
            END DO
        END DO
        
    END ASSOCIATE
    
  END SUBROUTINE GetActualET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL RIPARIAN ET AT ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetActualET_AtElements(RVETFromStrm,rElemET)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFRomStrm
    REAL(8),INTENT(OUT)                :: rElemET(:)
    
    !Local variables
    INTEGER :: indxElem
    
    DO indxElem=1,SIZE(RVETFromStrm%ElemToStrm)
        rElemET(indxElem) = SUM(RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Actual)
    END DO
    
  END SUBROUTINE GetActualET_AtElements
  
  
  ! -------------------------------------------------------------
  ! --- GET UNMET RIPARIAN ET FROM STREAMS AT STREAM NODES 
  ! -------------------------------------------------------------
  FUNCTION GetUnmetET_AtStrmNodes(RVETFromStrm,iNR) RESULT(rUnmetET)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFromStrm
    INTEGER,INTENT(IN)                 :: iNR
    REAL(8)                            :: rUnmetET(iNR)
    
    !Local variables
    INTEGER :: indxNode,indxElem,iElem
    
    !Initialize
    rUnmetET = 0.0
    
    !Return if the process is not simulated
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) RETURN
    
    !Compile
    ASSOCIATE (pStrmToElem => RVETFromStrm%StrmToElem  , &
               pElemToStrm => RVETFromStrm%ElemToStrm  )
        
        DO indxNode=1,RVETFromStrm%iNStrmNodes
            DO indxElem=1,pStrmToElem(indxNode)%iNElems
                iElem              = pStrmToElem(indxNode)%iElems(indxElem)
                rUnmetET(indxNode) = rUnmetET(indxNode) + SUM(pElemToStrm(iElem)%rETStrm_Required - pElemToStrm(iElem)%rETStrm_Actual)
            END DO
        END DO
    
    END ASSOCIATE
               
  END FUNCTION GetUnmetET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET UNMET RIPARIAN ET FROM STREAMS AT STREAM REACHES 
  ! -------------------------------------------------------------
  FUNCTION GetUnmetET_AtStrmReaches(RVETFromStrm,iNReaches,iUpstrm,iDownstrm) RESULT(rUnmetET)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFromStrm
    INTEGER,INTENT(IN)                 :: iNReaches,iUpstrm(iNReaches),iDownstrm(iNReaches)
    REAL(8)                            :: rUnmetET(iNReaches)
    
    !Local variables
    INTEGER :: indxElem,iElem,indxReach,iUpstrmNode,iDownstrmNode,indxNode
    
    !Initialize
    rUnmetET = 0.0
    
    !Return if the process is not simulated
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) RETURN
    
    !Compile
    ASSOCIATE (pStrmToElem => RVETFromStrm%StrmToElem  , &
               pElemToStrm => RVETFromStrm%ElemToStrm  )
        
        DO indxReach=1,iNReaches
            iUpstrmNode   = iUpstrm(indxReach)
            iDownstrmNode = iDownstrm(indxReach)
            DO indxNode=iUpstrmNode,iDownstrmNode
                IF (indxNode .GT. RVETFromStrm%iNStrmNodes) CYCLE
                DO indxElem=1,pStrmToElem(indxNode)%iNElems
                    iElem               = pStrmToElem(indxNode)%iElems(indxElem)
                    rUnmetET(indxReach) = rUnmetET(indxReach) + SUM(pElemToStrm(iElem)%rETStrm_Required - pElemToStrm(iElem)%rETStrm_Actual)
                END DO
            END DO
        END DO
        
    END ASSOCIATE
    
  END FUNCTION GetUnmetET_AtStrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET UNMET RIPARIAN VEG ET FROM STREAM AT ALL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION GetUnmetET_AtElements(RVETFRomStrm,iNElements) RESULT(rUnmetET)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFromStrm
    INTEGER,INTENT(IN)                 :: iNElements
    REAL(8)                            :: rUnmetET(iNElements)
    
    !Local variables
    INTEGER :: indxElem
    
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) THEN
        rUnmetET = 0.0
        RETURN
    ELSE
        DO indxElem=1,iNElements
            rUnmetET(indxElem) = SUM(RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Required - RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Actual)
        END DO
    END IF

  END FUNCTION GetUnmetET_AtElements
   
  


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SET REQUIRED ELEMENT RIPARIAN VEG ET FROM STREAM AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE SetRequiredET_AtElements(RVETFromStrm,rRVETRequired)
    CLASS(RVETFromStrmType) :: RVETFromStrm
    REAL(8),INTENT(IN)      :: rRVETRequired(:,:)
    
    !Local variables
    INTEGER :: indxElem
    
    DO indxElem=1,SIZE(RVETFromStrm%ElemToStrm)
        RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Required = rRVETRequired(:,indxElem)
    END DO

  END SUBROUTINE SetRequiredET_AtElements
  

  ! -------------------------------------------------------------
  ! --- SET ELEMENT LEVEL ACTUAL ET FED BY STREAMS WHEN THEY ARE DEFINED AT STREAM NODES 
  ! -------------------------------------------------------------
  SUBROUTINE SetActualET_AtStrmNodes(RVETFromStrm,rETActualFrac)
    CLASS(RVETFromStrmType) :: RVETFromStrm
    REAL(8),INTENT(IN)      :: rETActualFrac(:)
    
    !Local variables
    INTEGER :: indxStrm,indxElem,iElem
    
    !Return if the process is not simulated
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) RETURN
    
    !Compile actual ET at element level
    ASSOCIATE (pStrmToElem => RVETFromStrm%StrmToElem  , &
               pElemToStrm => RVETFromStrm%ElemToStrm  )
            
        DO indxStrm=1,RVETFromStrm%iNStrmNodes
            DO indxElem=1,pStrmToElem(indxStrm)%iNElems
                iElem                             = pStrmToElem(indxStrm)%iElems(indxElem)
                pElemToStrm(iElem)%rETStrm_Actual = pElemToStrm(iElem)%rETStrm_Required * rETActualFrac(indxStrm)
            END DO
        END DO
    
    END ASSOCIATE
    
  END SUBROUTINE SetActualET_AtStrmNodes
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- IS THIS PROCESS SIMULATED 
  ! -------------------------------------------------------------
  FUNCTION IsSimulated(RVETFromStrm) RESULT(lSimulated)
    CLASS(RVETFromStrmType),INTENT(IN) :: RVETFromStrm
    LOGICAL                            :: lSimulated
    
    IF (RVETFromStrm%iNStrmNodes .EQ. 0) THEN
        lSimulated = .FALSE.
    ELSE
        lSimulated = .TRUE.
    END IF
    
  END FUNCTION IsSimulated



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
  ! --- ADVANCE STATE OF THE SYSTEM 
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(RVETFromStrm)
    CLASS(RVETFromStrmType) :: RVETFromStrm
    
    !Local variables
    INTEGER :: indxElem
    
    DO indxElem=1,SIZE(RVETFromStrm%ElemToStrm)
        RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Required = 0.0
        RVETFromStrm%ElemToStrm(indxElem)%rETStrm_Actual   = 0.0
    END DO
    
  END SUBROUTINE AdvanceState

END MODULE