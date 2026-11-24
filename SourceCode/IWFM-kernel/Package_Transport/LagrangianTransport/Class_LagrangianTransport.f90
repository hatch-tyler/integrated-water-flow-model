!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2026  
!  State of California, Department of Water Resources DT
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
MODULE Class_LagrangianTransport
  !$ USE OMP_LIB
  USE  rng_par_zig,                 ONLY: rng_uni                       , &
                                          rng_init                      , &
                                          rng_norm
  USE  iso_fortran_env,             ONLY: int64
  USE TransportDataReader,          ONLY: TransportDataReaderType       , &
                                          GWVelocityFieldType
  USE Class_TransportBase,          ONLY: TransportMethodType           , &
                                          f_IKIND
  USE GeneralUtilities,             ONLY: GetUniqueArrayComponents      , &
                                          ShellSort
  USE Package_Discretization ,      ONLY: AppGridType                   , &
                                          StratigraphyType
  USE LagrangianParcel_LinkedList , ONLY: LPLinkedListType              , &
                                          LPNodeType
  USE Package_AppGW               , ONLY: AppGWType        
  USE Package_AppPumping,           ONLY: AppPumpingType  
  USE Class_Pumping               , ONLY: PumpingType                   , &
                                          GetPumpPurpose                , &
                                          UpdatePumpValues              , &
                                          ComputePumpActual             , &
                                          DistributePumpToNodes         , &
                                          ComputerFPumpCol              
  USE MessageLogger               , ONLY: LogMessage                    , &
                                          SetLastMessage                , &
                                          EchoProgress                  , &
                                          IsLogFileDefined              , &
                                          MessageArray                  , &
                                          f_iFILE                       , &
                                          f_iMessage                    , &
                                          f_iFatal                      , &
                                          f_iWarn                       , &
                                          f_iInfo                       
  USE GeneralUtilities            , ONLY: StripTextUntilCharacter       , &
                                          IntToText                     , &
                                          ArrangeText                   , &
                                          UpperCase                     , &
                                          CleanSpecialCharacters        , &
                                          EstablishAbsolutePathFilename , &
                                          ConvertID_To_Index            , &
                                          f_cLineFeed
  USE TimeSeriesUtilities         , ONLY: TimeStepType                  , &
                                          IncrementTimeStamp            , &
                                          IsTimeStampValid              , &
                                          IsTimeIntervalValid           , &
                                          f_iTimeStampLength            , &    
                                          StripTimeStamp                , &
                                          NPeriods                      , &
                                          TimeIntervalConversion        
  USE IOInterface                 , ONLY: GenericFileType               , & 
                                          RealTSDataInFileType          , &
                                          PrepareTSDOutputFile          , &
                                          iGetFileType_FromName         , &
                                          f_iTXT                        , &
                                          f_iDSS                        , & 
                                          f_iUNKNOWN                          
 
  
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
  PUBLIC :: LagrangianTransportType , &
            ReleaseType             , &
            SourceType
         
  
  ! -------------------------------------------------------------
  ! --- NEIGHBORING ELEMENTS DATA TYPE
  ! -------------------------------------------------------------
  TYPE NeighborElementsType
      INTEGER,ALLOCATABLE :: Elements(:)
  END TYPE NeighborElementsType

  
  ! -------------------------------------------------------------
  ! --- MAXIMUM ALLOWABLE SUBTIME STEPS FOR TRANSPORT 
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iMAX_SUBTIMESTEPS = 20

  
  ! -------------------------------------------------------------
  ! --- PARTICLE RELAESE TYPE
  ! -------------------------------------------------------------    
  TYPE ReleaseType
      INTEGER   :: ID
      INTEGER   :: ElemID
      INTEGER   :: npar_rel
      INTEGER   :: npar_dt
      INTEGER   :: nsubDT=1
      INTEGER   :: Tstart
      INTEGER   :: Tend
      INTEGER   :: npar_dtsub(f_iMAX_SUBTIMESTEPS)
      REAL (8)  :: Xorg
      REAL (8)  :: Yorg
      REAL (8)  :: Zorg
      REAL (8)  :: FlowRate
      REAL (8)  :: t_spill
  END TYPE ReleaseType
  
  
  ! -------------------------------------------------------------
  ! --- CONTAMINANT SOURCE DATA TYPE
  ! -------------------------------------------------------------    
  TYPE, EXTENDS( ReleaseType) :: SourceType
      REAL (8)  :: Conc
      REAL (8)  :: MassRate
      REAL (8)  :: tot_mass
      REAL (8)  :: MassReleased
  END TYPE SourceType

  
  ! -------------------------------------------------------------
  ! ---ABSTRACT LAGRANGIAN BASE
  ! -------------------------------------------------------------
  TYPE, EXTENDS(TransportMethodType),ABSTRACT :: LagrangianTransportType
      INTEGER                               :: NumLyrs=0                   ! Number of layers
      INTEGER                               :: Num_Parcels=0               ! Total number of lagrangian parcels used in the simulation   
      INTEGER                               :: NumSources=0                ! Number of sources
      INTEGER                               :: SimulationType              ! Simulation coupled with flow or using saved velocity and head files              
      INTEGER                               :: LLElem, URElem              ! Lower left and upper right corner element numbers
      INTEGER                               :: AvgElemLength               ! Average element length
      INTEGER                               :: iSolnType                   ! Time integration option
      REAL(KIND=f_IKIND)                    :: LwrLeft(3)                  ! X, Y, and Z Coordinates of lower left corner of the solute transport domain
      REAL(KIND=f_IKIND)                    :: UprRight(3)                 ! X, Y, and Z Coordinates of upper right corner of the solute transport domain
      REAL(KIND=f_IKIND)                    :: DX                          ! Concentration grid size in x-direction
      REAL(KIND=f_IKIND)                    :: DY                          ! Concentration grid size in y-direction 
      REAL(KIND=f_IKIND),ALLOCATABLE        :: AvgDomainTopElv(:,:)        ! Average elevation of the top of an element at each (layer,element)
      REAL(KIND=f_IKIND),ALLOCATABLE        :: AvgDomainBotElv(:,:)        ! Average elevation of the bottom of an element at each (layer,element)
      REAL(KIND=f_IKIND),ALLOCATABLE        :: AvgTopElv(:)                ! Average top-most elevation for each (element)
      REAL(KIND=f_IKIND),ALLOCATABLE        :: AvgBotElv(:)                ! Average bottom-most elevation for each (element) 
      REAL(KIND=f_IKIND),POINTER            :: Vx(:,:)                     ! X-Velocity array for each (node, layer)
      REAL(KIND=f_IKIND),POINTER            :: Vy(:,:)                     ! Y-Velocity array for each (node, layer)
      REAL(KIND=f_IKIND),POINTER            :: Vz(:,:)                     ! Z-Velocity array for each (node, layer)
      REAL(KIND=f_IKIND),POINTER            :: GWHead(:)                   ! Groundwater head from flow solution
      CHARACTER(LEN=f_iTimeStampLength)     :: Tstart, Tend                ! Starting and ending time of transport simulation
      TYPE(LPLinkedListType)                :: LPList                      ! Linked-list of Lagrangian parcels
      TYPE(GenericFileType),ALLOCATABLE     :: InfoOutFile                 ! File to print debug output 
      TYPE(NeighborElementsType),ALLOCATABLE:: NeighborElems(:)            !List of neighbor elements at each (element)
      TYPE(AppPumpingType), PUBLIC          :: AppPumping                  !Pumping component
  CONTAINS
      PROCEDURE,PASS                        :: InterpolateVelocity
      PROCEDURE,PASS                        :: Get_LP_Layer
      PROCEDURE,PASS                        :: Get_LP_Elem 
      PROCEDURE,NOPASS                      :: Is_LP_Inside_Quad
      PROCEDURE,NOPASS                      :: Is_LP_Inside_Triangle      
      PROCEDURE,PASS                        :: Traverse_neighbr_elements
      PROCEDURE,PASS                        :: DomainConfigData 
      PROCEDURE,PASS                        :: ApplyReflectionBC 
      PROCEDURE,PASS                        :: ApplyAbsorbBC
      PROCEDURE,PASS                        :: DisplaceLPs
      PROCEDURE,PASS                        :: DisperseLPs
      PROCEDURE,PASS                        :: AdvectLPs_Euler
      PROCEDURE,PASS                        :: AdvectLPs_RK4
      PROCEDURE,PASS                        :: AdvectLPs_RK2
      PROCEDURE,PASS                        :: CalcDispCoeff 
      PROCEDURE,PASS                        :: GetAquiferParameters
      PROCEDURE,PASS                        :: CalcAdvectionDT
      PROCEDURE,PASS                        :: CalcDispersionDT
      PROCEDURE,NOPASS                      :: AdjustTranspTimeStep
      PROCEDURE,PASS                        :: SetVelocityBounds
      PROCEDURE,PASS                        :: SetTransportTimeStep
      PROCEDURE,PASS                        :: Update_ElementAndLayer
      PROCEDURE,NOPASS                      :: InitializeRandomNumberGenerator
  END TYPE LagrangianTransportType
  
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 26
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'LagrangianTransport::'

 

CONTAINS
    
    

    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- COMPILE TRANSPORT DATA
  ! -------------------------------------------------------------
  SUBROUTINE DomainConfigData(LagTrans, AppGrid,Stratigraphy)
    CLASS(LagrangianTransportType),INTENT(INOUT) :: LagTrans
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy
    
    !Local variables
    INTEGER :: iNLayers,iNElements,indxLayer,indxElem,iNVertex,iVertex(4),indxVertex,nID,iCount,iNode,indxElemNew,iElem_New, &
               iNeighborElems(25), AvgElemLen
    REAL(8) :: XCL,YCL,rCoeff(4),rAvgDomainTopElv,rAvgDomainBotElv
    
    !Initialize
    AvgElemLen=0.0
    iNElements = AppGrid%NElements
    iNLayers   = Stratigraphy%NLayers
    
    !Allocate arrays
    ALLOCATE (LagTrans%AvgDomainTopElv(iNLayers,iNElements) , LagTrans%AvgDomainBotElv(iNLayers,iNElements) , &
                           LagTrans%AvgTopElv(iNElements)    , LagTrans%AvgBotElv(iNElements)    , &
                           LagTrans%NeighborElems(iNElements)                                              )
    
    LagTrans%AvgDomainTopElv=0.0d0;   LagTrans%AvgDomainBotElv=0.0d0
    LagTrans%AvgTopElv=0.0d0 ; LagTrans%AvgBotElv =0.0d0
    
    !Compute top and bottom elevations of each aquifer layer at centroid of each element, average topmost and bottommost elevations, and surrounding elements for each element
    DO indxElem=1,iNElements
        iVertex  = AppGrid%Vertex(:,indxElem)  
        iNVertex = AppGrid%NVertex(indxElem)
        
        !Top and bottom aquifer layer elevations at element centroid
        CALL AppGrid%Centroid(indxElem,XCL,YCL)
        CALL AppGrid%FEInterpolate_AtCell(indxElem,XCL, YCL,rCoeff(1:iNVertex))
        DO indxLayer=1,iNLayers
            LagTrans%AvgDomainTopElv(indxLayer,indxElem) = SUM(rCoeff(1:iNVertex) * Stratigraphy%TopElev(iVertex(1:iNVertex),indxLayer)) 
            LagTrans%AvgDomainBotElv(indxLayer,indxElem) = SUM(rCoeff(1:iNVertex) * Stratigraphy%BottomElev(iVertex(1:iNVertex),indxLayer)) 
        END DO
        
        !Average top-most and bottom-most elevations at the element
        rAvgDomainTopElv = 0.0d0
        rAvgDomainBotElv = 0.0d0
        DO indxVertex=1,3
            nID              = iVertex(indxVertex)
            rAvgDomainTopElv = rAvgDomainTopElv + Stratigraphy%TopElev(nID,1)
            rAvgDomainBotElv = rAvgDomainBotElv + Stratigraphy%BottomElev(nID,iNLayers)
        END DO
        nID = iVertex(4)
        IF (nID .GT. 0) THEN
            rAvgDomainTopElv = rAvgDomainTopElv + Stratigraphy%TopElev(nID,1)
            rAvgDomainBotElv = rAvgDomainBotElv + Stratigraphy%BottomElev(nID,iNLayers)
        END IF
        LagTrans%AvgTopElv(indxElem) = rAvgDomainTopElv / REAL(iNVertex,8)
        LagTrans%AvgBotElv(indxElem) = rAvgDomainBotElv / REAL(iNVertex,8)
        
        !Compile surrounding elements
        iCount = 0
        DO indxVertex=1,iNVertex
            iNode = iVertex(indxVertex)
            DO indxElemNew=1,SIZE(AppGrid%AppNode(iNode)%SurroundingElement)
                iElem_New = AppGrid%AppNode(iNode)%SurroundingElement(indxElemNew)
                IF (iElem_New .NE. indxElem) THEN
                    iCount                 = iCount + 1
                    iNeighborElems(iCount) = iElem_New
                END IF
            END DO
        END DO
  
        !Get unique neighboring element numbers
        CALL GetUniqueArrayComponents(iNeighborElems(1:iCount),LagTrans%NeighborElems(indxElem)%Elements)
        
        !Sort neighboring element numbers
        CALL ShellSort(LagTrans%NeighborElems(indxElem)%Elements)
        
        !Average Element length
        AvgElemLen=AvgElemLen+SQRT(AppGrid%AppElement(indxElem)%Area)
  
    END DO
    
    LagTrans%AvgElemLength=AvgElemLen/FLOAT(iNElements)
    
    LagTrans%lDefined = .TRUE.
    
  END SUBROUTINE DomainConfigData

    
  ! -------------------------------------------------------------
  ! --- FIGURE OUT THE LAYER NUMBER FOR THIS LP
  ! -------------------------------------------------------------
  SUBROUTINE Get_LP_Layer(LagTrans, Stratigraphy, ID, indElem, zcord, iLayer, iStat) 
    CLASS(LagrangianTransportType),INTENT(IN)   :: LagTrans
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy    
    INTEGER, INTENT (IN)                        :: ID,indElem
    REAL (KIND=8), INTENT(IN)                   :: zcord
    INTEGER,INTENT(OUT)                         :: iLayer,iStat
    
    !-Local Variables
    CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'Get_LP_Layer'

    !-Initialize
    iStat = 0
    
    iLayer = 1
    DO WHILE (LagTrans%AvgDomainTopElv(iLayer,indElem)>=zcord  .AND. iLayer <Stratigraphy%NLayers) 
        iLayer = iLayer+1
    ENDDO
    iLayer = iLayer-1
      
    IF (iLayer == 0) THEN
        CALL SetLastMessage('LP released:  '//TRIM(IntToText(ID))//' is outside the transport domain!',f_iFatal,ThisProcedure)
        iStat = -1
    ENDIF
    
  END SUBROUTINE Get_LP_Layer
  
  
  ! -------------------------------------------------------------
  ! --- FIGURE OUT THE ELEMENT NUMBER THAT THE PARTICLE IS LOCATED. 
  ! --- *Note: Traverse through all neighbouring elements (starting  from the previous element) 
  ! ---        Neighbouring elements should be calculated first
  ! ---        In this function, the winding number algorithm is applied by checking the  
  ! ---        cross products of the edges of the quadrilateral and the vectors from the  
  ! ---        point to the vertices of the quadrilateral. If all cross products have the same 
  ! ---        sign (positive or negative), the point is inside the quadrilateral. Otherwise, it's outside.
  ! -------------------------------------------------------------
  SUBROUTINE Get_LP_Elem (LagTrans, AppGrid, ielem, xcrd, ycrd, new_elem, found, iStat) 
    CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    INTEGER, INTENT(IN)                       :: ielem
    INTEGER, INTENT(OUT)                      :: new_elem, iStat
    REAL(KIND=8), INTENT (IN)                 :: xcrd, ycrd
    LOGICAL, INTENT (OUT)                     :: found
    
    !-Local Variables
    INTEGER             :: I, nID, Vertex(4)
    REAL (KIND=8)       :: x_vertex(4), y_vertex(4)
    LOGICAL             :: is_inside
    
    found=.FALSE.
    is_inside=.FALSE.

    new_elem=0
    
    !--First check if the particle is still in the current element
    Vertex=AppGrid%Vertex(:, ielem)
         
    DO I=1,3
        nID=Vertex(I)
        x_vertex(I)=AppGrid%GridType%X(nID)
        y_vertex(I)=AppGrid%GridType%Y(nID) 
    ENDDO

    nID=Vertex(4) 
    IF ( nID>0) THEN   ! Quadlilataral mesh   
        x_vertex(I)=AppGrid%GridType%X(nID)
        y_vertex(I)=AppGrid%GridType%Y(nID) 
        is_inside=Is_LP_Inside_Quad(xcrd, ycrd, x_vertex, y_vertex)                     
        IF (is_inside) THEN  
            found=.TRUE.
            new_elem=ielem
        ELSE  !-LP has moved out of the current element-so check within the neighboring elements
            CALL Traverse_neighbr_elements (LagTrans, AppGrid, ielem ,xcrd, ycrd, new_elem, iStat)  
            IF (new_elem>0) THEN
                found=.TRUE.
            ENDIF
        ENDIF
                    
    ELSE     ! Triangular mesh
        is_inside= Is_LP_Inside_Triangle ( xcrd, ycrd, x_vertex(1),x_vertex(2), x_vertex(3), y_vertex(1), y_vertex(2), y_vertex(3) )
        IF (is_inside) THEN
            found=.TRUE.
            new_elem=ielem                        
        ELSE !-LP has moved out of the current element-so check within the neighboring elements                           
            CALL Traverse_neighbr_elements (LagTrans, AppGrid, ielem ,xcrd, ycrd, new_elem, iStat)   ;  IF (iStat .EQ. -1) RETURN
            IF (new_elem>0) THEN
                found=.TRUE.
            ENDIF
        ENDIF  
    END IF
      
  END SUBROUTINE Get_LP_Elem
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF LAGRANGIAN PARTICLE IS INSIDE A TRAINGULAR ELEMENT 
  ! --- *Note: A(x1,y1), B(x2,y2), C(x3,y3) are the 3 nodes and P is the location of the Lagrangian particle
  ! ---        If P is inside the triangle Area ABC= Area PAB+Area PBC+ Area PAC
  ! ---        Use barycentric coordinate method
  ! -------------------------------------------------------------
  FUNCTION Is_LP_Inside_Triangle ( xlp,  ylp, x1, x2, x3, y1, y2, y3   )  RESULT(is_inside)
    REAL(KIND=8), INTENT(IN)                 :: xlp,  ylp, x1, x2, x3, y1, y2, y3
    LOGICAL                                  :: is_inside

    !Local variables
    REAL, PARAMETER :: f_rEpsilon=0.05   
    REAL (KIND=8)   :: denom, a, b, c
    
    denom=((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3))
    a = ((y2 - y3)*(xlp - x3) + (x3 - x2)*(ylp - y3)) / denom
    b = ((y3 - y1)*(xlp - x3) + (x1 - x3)*(ylp - y3)) / denom
    c = 1.0 - a - b
    
    is_inside = (0.0-f_rEpsilon <= a .and. a <= 1.0+f_rEpsilon) .and. (0.0-f_rEpsilon <= b .and. b <= 1.0+f_rEpsilon) .and. (0.0-f_rEpsilon <= c .and. c <= 1.0+f_rEpsilon)

  END FUNCTION Is_LP_Inside_Triangle
   
  
  ! -------------------------------------------------------------
  ! --- CHECK IF LAGRANGIAM PARTICLE IS INSIDE A QUADRILATERAL ELEMENT
  ! -------------------------------------------------------------
  FUNCTION Is_LP_Inside_Quad (  xlp, ylp, x_vertex, y_vertex   )  RESULT(is_inside)
    REAL(KIND=8), INTENT(IN)                 :: xlp, ylp, x_vertex(4), y_vertex(4)
    LOGICAL                                  :: is_inside
    
    !Local variables
    REAL (KIND=8) :: x1, x2, x3, x4, y1, y2, y3, y4
    
    is_inside=.TRUE.
    
    x1=x_vertex(1); x2=x_vertex(2); x3=x_vertex(3); x4=x_vertex(4) 
    y1=y_vertex(1); y2=y_vertex(2); y3=y_vertex(3); y4=y_vertex(4) 

    is_inside=  Is_LP_Inside_Triangle (xlp,  ylp, x1, x2, x3, y1, y2, y3) .OR.  Is_LP_Inside_Triangle ( xlp,  ylp, x1, x3, x4, y1, y3, y4    )
    
  END FUNCTION Is_LP_Inside_Quad

   
  ! -------------------------------------------------------------
  ! --- TRAVERSE NEIGHBORING ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE  Traverse_neighbr_elements (LagTrans, AppGrid, ielem, xcord, ycord, jelem, iStat) 
    CLASS(LagrangianTransportType),INTENT(IN):: LagTrans
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    INTEGER, INTENT(IN)                     :: ielem
    INTEGER, INTENT(OUT)                    :: jelem, iStat
    REAL (KIND=8), INTENT(IN)               :: xcord, ycord

    !Local variables
    INTEGER       :: ielem_new,kelem, nID, Vertex(4),  I, II, nNbrElems
    REAL (KIND=8) :: x_vertex(4), y_vertex(4)
    CHARACTER     :: Text*2000
    LOGICAL       :: is_inside, found
    
    iStat=0
    is_inside=.FALSE.
    jelem=ielem
    II=0   
    ! Traverse through neighboring elements to find out where the LP is located
    nNbrElems=SIZE(LagTrans%NeighborElems(ielem)%Elements)
    kelem=0
    DO WHILE ( is_inside==.FALSE. .AND. kelem+1<= nNbrElems)
             kelem=kelem+1
             x_vertex=0.0d0
             y_vertex=0.0d0
             ielem_new=LagTrans%NeighborElems(ielem)%Elements(kelem)
             Vertex=AppGrid%Vertex(:, ielem_new)  
             DO I=1, 3
                  nID=Vertex(I)
                  x_vertex(I)=AppGrid%GridType%X(nID)
                  y_vertex(I)=AppGrid%GridType%Y(nID) 
             ENDDO
             nID=Vertex(4) 
             IF  (nID>0 ) THEN
                 x_vertex(I)=AppGrid%GridType%X(nID)
                 y_vertex(I)=AppGrid%GridType%Y(nID)       
                 is_inside=Is_LP_Inside_Quad( xcord, ycord, x_vertex, y_vertex)
             ELSE 
                 is_inside= Is_LP_Inside_Triangle ( xcord, ycord, x_vertex(1),x_vertex(2), x_vertex(3), y_vertex(1), y_vertex(2), y_vertex(3) )
             ENDIF
    END DO

    IF (is_inside ) THEN
           jelem=ielem_new
           found=.TRUE.
    ELSE IF ( NOT(is_inside)  ) THEN
           found=.FALSE.
           WRITE(Text, ' (A) ') 'LP has moved out of neighboring elements, please ensure the released coordinates are  within the domain'
           CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
           iStat=-1
           RETURN
    ENDIF 

  END SUBROUTINE Traverse_neighbr_elements
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF THE LP HAS MOVED OUT OF DOMAIN AND IF IT'S MOVED OUT OF DOMAIN APPLY REFELECTION BOUNDARY CONDITION
  ! -------------------------------------------------------------
  SUBROUTINE ApplyReflectionBC(LagTrans, ielem, xcord, ycord, zcord)
    CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
    INTEGER, INTENT(IN)                       :: ielem
    REAL (KIND=8), INTENT(INOUT)              :: xcord, ycord, zcord
    
    !-Local Variables
    REAL (KIND=8) :: z_offset,  x_offset, y_offset, AvgTopElv, AvgBotElv
    REAL (KIND=8) :: XLwrLeft, XUprRight, YLwrLeft, YUprRight
    
    XLwrLeft= LagTrans%LwrLeft(1)
    YLwrLeft= LagTrans%LwrLeft(2)
    
    XUprRight=LagTrans%UprRight(1)
    YUprRight=LagTrans%UprRight(2)
    
    AvgTopElv=LagTrans%AvgTopElv(ielem)
    AvgBotElv=LagTrans%AvgBotElv(ielem)
    
                  
    !--Check if the parcel has crossed the top of the layer 1 and Reset the z coordinate
    IF ( zcord> AvgTopElv ) THEN  
           z_offset= zcord- AvgTopElv
           zcord= AvgTopElv-z_offset
    ENDIF
    
    !--Check if the parcel has crossed the bottom of the bottom layer and Reset the z coordinate
    IF (  zcord< AvgBotElv) THEN  
            z_offset=AvgBotElv-zcord
            zcord= AvgBotElv+z_offset
    ENDIF  
    
    !--Check if the parcel has crossed the western boundary and Reset X coordiante
    IF ( xcord<XLwrLeft  ) THEN
        x_offset=XLwrLeft-xcord
        xcord=XLwrLeft+x_offset
    END IF
      !--Check if the parcel has crossed the eastern boundary and Reset X coordiante
    IF (  xcord>XUprRight ) THEN                 
        x_offset=xcord-XUprRight
        xcord=XUprRight-x_offset
    END IF
    
    !--Check if the parcel has crossed the southern boundary and Reset Y coordiante
    IF ( ycord<YLwrLeft  ) THEN
        y_offset=YLwrLeft-ycord
        ycord=YLwrLeft+y_offset
    END IF
    
    !--Check if the parcel has crossed the nothern boundary and Reset Y coordiante
    IF ( ycord >YUprRight ) THEN
       y_offset=ycord-YUprRight 
       ycord=YUprRight-y_offset
    END IF
    
  END SUBROUTINE ApplyReflectionBC         
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF THE LP HAS MOVED OUT OF DOMAIN AND IF IT'S MOVED OUT OF DOMAIN REMOVE THE PARCEL
  ! -------------------------------------------------------------
  SUBROUTINE ApplyAbsorbBC(LagTrans, ielem, xcord, ycord, zcord, Remove)
    CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
    INTEGER, INTENT(IN)                       :: ielem
    REAL (KIND=8), INTENT(INOUT)              :: xcord, ycord, zcord
    LOGICAL, INTENT(OUT)                      :: Remove
    
    !-Local Variables
    REAL (KIND=8) :: AvgTopElv, AvgBotElv
    REAL (KIND=8) :: XLwrLeft, XUprRight, YLwrLeft, YUprRight
    
    Remove=.FALSE.
    
    XLwrLeft= LagTrans%LwrLeft(1)
    YLwrLeft= LagTrans%LwrLeft(2)
    
    XUprRight=LagTrans%UprRight(1)
    YUprRight=LagTrans%UprRight(2)
    
    AvgTopElv=LagTrans%AvgTopElv(ielem)
    AvgBotElv=LagTrans%AvgBotElv(ielem)
                  
    !--Check if the parcel has crossed the top of the layer 1 and Reset the z coordinate
    IF ( zcord> AvgTopElv  .OR. zcord< AvgBotElv .OR. xcord<XLwrLeft .OR.  xcord>XUprRight  &
         .OR. ycord<YLwrLeft  .OR. ycord >YUprRight ) THEN  
           Remove=.TRUE.
    ENDIF
    
  END SUBROUTINE ApplyAbsorbBC  
 
  
! -------------------------------------------------------------
! --- SET VELOCITY BOUNDS TO AVOID UNPHYSCIALLY HIGH VELOCITIES
! -------------------------------------------------------------
 SUBROUTINE SetVelocityBounds (LagTrans,AppGW,Umax, Vmax, TimeStep, iStat) 
   CLASS(LagrangianTransportType),INTENT(INOUT):: LagTrans
   TYPE(AppGWType),INTENT(IN)                  :: AppGW
   TYPE(TimeStepType),INTENT(IN)               :: TimeStep
   REAL(KIND=f_IKIND),INTENT(INOUT)            :: Umax, Vmax
   INTEGER,INTENT(OUT)                         :: iStat
 
   !-Local variables
   CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'SetVelocityBounds'
   CHARACTER                   :: Text*500
   REAL(KIND=f_IKIND)          :: TimeConvFact, LengthConvFact
 
   
   !Initialize
   iStat = 0
   
   IF (TimeStep%UNIT=='12HOUR') THEN
       TimeConvFact=0.5     
   ELSEIF (TimeStep%UNIT=='1DAY') THEN
       TimeConvFact=1.0  
   ELSEIF (TimeStep%UNIT=='1WEEK') THEN
       TimeConvFact=7.0                     
   ELSEIF (TimeStep%UNIT=='1MON') THEN
       TimeConvFact=30.
   ELSEIF (TimeStep%UNIT=='1YEAR') THEN
       TimeConvFact=365.0
   ELSE  
       CALL SetLastMessage('Flow model time unit is not recognized!',f_iFatal,ThisProcedure)
       iStat = -1
       RETURN    
   ENDIF
   
   IF(LagTrans%DataProvider%lFlowSimulated) THEN
       IF (AppGW%UnitHead .EQ.'ft.' .OR. AppGW%UnitHead .EQ.'FEET' .OR. AppGW%UnitHead(1:2) .EQ.'f' ) THEN
           LengthConvFact=1.0
       ELSEIF (AppGW%UnitHead .EQ.'m' .OR. AppGW%UnitHead .EQ.'METERS' .OR. AppGW%UnitHead(1:2) .EQ. 'm') THEN
           LengthConvFact=3.2808
       ELSE
           CALL SetLastMessage('Flow model unit is not recognized!',f_iFatal,ThisProcedure)
           iStat = -1
           RETURN    
       ENDIF 
   ELSE
      LengthConvFact=LagTrans%DataProvider%HeadConvFact 
   ENDIF
   
   
    !--Printout the maximum horizontal velocity
    IF(LagTrans%DataProvider%lFlowSimulated) THEN 
       WRITE(Text, ' (A)') ''
       CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
       WRITE(Text, ' (A, F12.3, 1X, A,A,A) ') '*  Maximum horizontal velocity is                              : ', &
                                               MAX(Umax,Vmax) , TRIM(ADJUSTR(AppGW%UnitHead(1:2))), '/', TRIM(ADJUSTL(TimeStep%UNIT(2:4)))
       CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

    ELSE
       IF(LagTrans%DataProvider%HeadConvFact==1.0) THEN
               WRITE(Text, ' (A)') ''
               CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
               WRITE(Text, ' (A, F12.3, 1X, A,A,A) ') '*  Maximum horizontal velocity is                              : ', &
                                               MAX(Umax,Vmax) , 'ft', '/', TRIM(ADJUSTL(TimeStep%UNIT(2:4)))
               CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

       ELSE
               WRITE(Text, ' (A)') ''
               CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
               WRITE(Text, ' (A, F12.3, 1X, A,A,A) ') '*  Maximum horizontal velocity is                              : ', &
                                               MAX(Umax,Vmax) , 'm', '/', TRIM(ADJUSTL(TimeStep%UNIT(2:4)))
               CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

       ENDIF
    ENDIF
   
   !--Set up the max allowed velocity (currently set it to 100 ft/day) 
   IF (.NOT.LagTrans%DataProvider%VelocityBounds) THEN 
       LagTrans%DataProvider%HorVelocityMax=LagTrans%DataProvider%HorVelocityMax*TimeConvFact/LengthConvFact
   ENDIF    
   IF (Umax>LagTrans%DataProvider%HorVelocityMax) THEN
       Umax=LagTrans%DataProvider%HorVelocityMax
       LagTrans%DataProvider%VelocityBounds=.TRUE.
   ENDIF
   IF (Vmax>LagTrans%DataProvider%HorVelocityMax) THEN
       Vmax=LagTrans%DataProvider%HorVelocityMax
       LagTrans%DataProvider%VelocityBounds=.TRUE.
   ENDIF 
   
   !--Printout the capped horizontal velocity 
   IF(LagTrans%DataProvider%VelocityBounds)THEN
     IF(LagTrans%DataProvider%lFlowSimulated) THEN 
       WRITE(Text, ' (A, F12.3, 1X, A,A,A) ') '*  Horizontal velocity is capped at                            :', &
                                               LagTrans%DataProvider%HorVelocityMax , TRIM(ADJUSTR(AppGW%UnitHead(1:2))), '/', TRIM(ADJUSTL(TimeStep%UNIT(2:4)))
     ELSE
       IF(LagTrans%DataProvider%HeadConvFact==1) THEN
               WRITE(Text, ' (A, F12.3, 1X, A,A,A) ') '*  Horizontal velocity is capped at                            :', &
                                               LagTrans%DataProvider%HorVelocityMax , 'ft', '/', TRIM(ADJUSTL(TimeStep%UNIT(2:4)))
       ELSE
               WRITE(Text, ' (A, F12.3, 1X, A,A,A) ') '*  Horizontal velocity is capped at                            :', &
                                               LagTrans%DataProvider%HorVelocityMax , 'm', '/', TRIM(ADJUSTL(TimeStep%UNIT(2:4)))
       ENDIF
     ENDIF
     CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
   ENDIF
     
 END SUBROUTINE SetVelocityBounds
  
 
 ! -------------------------------------------------------------
 ! --- SET TRANSPORT VELOCITY BASED ON STABILITY CRITERIA
 ! -------------------------------------------------------------
 SUBROUTINE SetTransportTimeStep (LagTrans, LagrangianType, AppGW, Vx, Vy, Vz ,TimeStep,ADJTIME, iStat) 
   CLASS(LagrangianTransportType),INTENT(INOUT) :: LagTrans
   CHARACTER(*), INTENT(IN)                     :: LagrangianType
   TYPE(AppGWType),INTENT(IN)                   :: AppGW
   REAL(KIND=8), INTENT (IN)                    :: Vx(:,:), Vy(:,:), Vz(:,:) 
   TYPE(TimeStepType),INTENT(IN)                :: TimeStep
   INTEGER,INTENT(OUT)                          :: iStat
   LOGICAL,INTENT(OUT)                          :: ADJTIME 

   !-Local variables
   CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'SetTransportTimeStep'
   INTEGER                     :: nsubDT
   REAL(KIND=f_IKIND)          :: TranspDT, TranspDT_adv, TranspDT_disp, Umax, Vmax, Wmax
   CHARACTER                   :: Text*500
   LOGICAL                     :: ADJTIME_adv, ADJTIME_disp

   !Initialize
   iStat=0
   ADJTIME=.FALSE.
   

   !-Reset transport timesteps to the flow model time step
   LagTrans%TimeStep%DeltaT=TimeStep%DeltaT
   LagTrans%TimeStep%nsubTimeSteps=1

   TranspDT=TimeStep%DeltaT

   
   Umax=MAXVAL(Vx(:,:))  
   Vmax=MAXVAL(Vy(:,:)) 
   Wmax=MAXVAL(Vz(:,:)) 
   

   !--Set up the max horizontal allowed velocity (currently set it to 100 ft/day) 
   CALL LagTrans%SetVelocityBounds (AppGW,Umax, Vmax,TimeStep, iStat) 

   WRITE(Text, '(A)') ' '
   CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
   
   !--Check the Advection Stability Criteria (CFL) and Dispersion Stability Criteria
   SELECT CASE (TRIM(ADJUSTL(UpperCase(LagrangianType))))  
   CASE ("SOLUTETRANSPORT")
       CALL LagTrans%CalcAdvectionDT( Umax, Vmax,TimeStep,TranspDT_adv, ADJTIME_adv, iStat ) 
       CALL LagTrans%CalcDispersionDT( Umax, Vmax, Wmax,TimeStep,TranspDT_disp, ADJTIME_disp, iStat)
       !--Pick the minimum of advection and dispersion criteria
       IF (ADJTIME_adv .AND. ADJTIME_disp) THEN
          TranspDT=MIN(TranspDT_adv ,TranspDT_disp)
          ADJTIME=.TRUE.          
          WRITE(Text, '(A)') '*  CFL and dispersion criteria are not satisfied. Transport time step is being adjusted...'
          CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
       ELSEIF (ADJTIME_adv) THEN
          TranspDT=TranspDT_adv
          ADJTIME=.TRUE.
          WRITE(Text, '(A)') '*  CFL criteria is not satisfied. Transport time step is being adjusted...'
          CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
       ELSEIF (ADJTIME_disp) THEN
          TranspDT=TranspDT_disp
          ADJTIME=.TRUE. 
          WRITE(Text, '(A)') '*  Dispersion criteria is not satisfied. Transport time step is being adjusted...'
          CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
       ELSE
           TranspDT=TimeStep%DeltaT 
           WRITE(Text, '(A)') '*  CFL and dispersion criteria are satisfied. Transport time step is same as flow time step.'
           CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
       ENDIF
       

   CASE ("PARTICLETRACKING") 
       CALL LagTrans%CalcAdvectionDT( Umax, Vmax,TimeStep,TranspDT,ADJTIME, iStat ) 
   CASE DEFAULT
       CALL SetLastMessage('Unknown Lagrangian Type!',f_iFatal,ThisProcedure)
       iStat = -1
   END SELECT
   
   !--If time step was adjusted then save the stable time step
   IF ( ADJTIME) THEN
       !-Use a multiplier for creating equal subtime steps
       CALL LagTrans%AdjustTranspTimeStep(TimeStep,TranspDT, nsubDT,iStat)
       
       WRITE(Text, ' (A, F8.3, 1X, A) ')     '*  Adjusted transport time step to satisfy stability criteria  : ', TranspDT , TimeStep%UNIT(2:4)
       CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

       LagTrans%TimeStep%DeltaT=TranspDT
       LagTrans%TimeStep%nsubTimeSteps=nsubDT
   ENDIF

 END SUBROUTINE SetTransportTimeStep
 
 
 ! -------------------------------------------------------------
 ! --- CALCULATE TIME STEP FOR ADVECTION CRITERIA-CFL CONDITION 
 ! -------------------------------------------------------------
 SUBROUTINE CalcAdvectionDT (LagTrans, Umax, Vmax, TimeStep,TranspDT,ADJTIME, iStat )  
   CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
   REAL(KIND=f_IKIND),INTENT(IN)             :: Umax, Vmax
   TYPE(TimeStepType),INTENT(IN)             :: TimeStep
   REAL(KIND=f_IKIND),INTENT(OUT)            :: TranspDT
   INTEGER,INTENT(OUT)                       :: iStat
   LOGICAL,INTENT(OUT)                       :: ADJTIME  

   !-Local Variables
   CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'CalcAdvectionDT'
   REAL(KIND=f_IKIND)           :: dx, dy
   
   !Initialize
   iStat=0

   dx=LagTrans%DX
   dy=LagTrans%DY

   TranspDT=TimeStep%DeltaT
   
  !--Check the Advection Time Step Criteria (CFL condition)
   IF (TranspDT .LE. dx/ABS(Umax)   .and.   TranspDT  .LE.   dy/ABS(Vmax)) THEN
       ADJTIME=.FALSE.
   ELSE 
       ADJTIME=.TRUE.
       TranspDT =MIN(dx/ABS(Umax),dy/ABS(Vmax))      
   ENDIF
   
   IF (TranspDT < 0) THEN 
       CALL SetLastMessage('Error calculating timestep for CFL stability criteria !',f_iFatal,ThisProcedure)
       iStat=-1
       RETURN
   ENDIF

 END SUBROUTINE CalcAdvectionDT
 
 
 ! -------------------------------------------------------------
 ! --- CALCULATE TIME STEP FOR ADVECTION CRITERIA 
 ! -------------------------------------------------------------
 SUBROUTINE CalcDispersionDT (LagTrans,  Umax, Vmax,Wmax, TimeStep,TranspDT,ADJTIME, iStat )  
   CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
   REAL(KIND=f_IKIND),INTENT(IN)             :: Umax, Vmax, Wmax
   TYPE(TimeStepType),INTENT(IN)             :: TimeStep
   REAL(KIND=f_IKIND),INTENT(OUT)            :: TranspDT
   INTEGER,INTENT(OUT)                       :: iStat
   LOGICAL,INTENT(OUT)                       :: ADJTIME  

   !-Local Variables
   CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'CalcAdvectionDT'
   REAL(KIND=f_IKIND)           :: dx, dy
   REAL(KIND=f_IKIND)           :: LngDispMax,  DispCoeff_xx, DispCoeff_yy, Dxx, Dyy, Dzz, Dxy, Dxz, Dyz
   
   !Initialize
   iStat=0

   dx=LagTrans%DX
   dy=LagTrans%DY

   TranspDT=TimeStep%DeltaT
   
   !--Calculate max dispersion coefficients 
   LngDispMax= MAXVAL(LagTrans%DataProvider%Nodes%LongDispersivity(:,:)) 
   CALL LagTrans%CalcDispCoeff ( Umax, Vmax, Wmax, LngDispMax, Dxx, Dyy, Dzz, Dxy, Dxz, Dyz, iStat);  IF (iStat .EQ. -1) RETURN
   
   DispCoeff_xx= Dxx  
   DispCoeff_yy= Dyy  

      
   !--Check for the Dispersion Time Step Criteria 
   IF ( TranspDT .LE. 0.5*dx*dx/DispCoeff_xx .and. TranspDT .LE. 0.5*dy*dy/DispCoeff_yy) THEN
       ADJTIME=.FALSE.
   ELSE 
       ADJTIME=.TRUE.
       TranspDT =MIN(0.5*dx*dx/DispCoeff_xx  , 0.5*dy*dy/DispCoeff_yy)
   ENDIF
   
   IF (TranspDT < 0) THEN 
       CALL SetLastMessage('Error calculating timestep for dispersion stability criteria !',f_iFatal,ThisProcedure)
       iStat=-1
       RETURN
   ENDIF

 END SUBROUTINE CalcDispersionDT 
 
 
 ! -------------------------------------------------------------
 ! --- SUBDIVIDE TRANSPORT TIME STEP TO MEET STABILITY CRITERIA
 ! -------------------------------------------------------------
 SUBROUTINE AdjustTranspTimeStep(TimeStep,TranspDT,nsubDT, iStat)
   TYPE(TimeStepType),INTENT(IN)    :: TimeStep 
   REAL(KIND=f_IKIND),INTENT(INOUT) :: TranspDT
   INTEGER,INTENT(OUT)              :: nsubDT
   INTEGER,INTENT(OUT)              :: iStat    

   
   !Local variables
   REAL(KIND=f_IKIND) :: FlowDT
   CHARACTER          :: Text*500

   !Initialize
   iStat=0
   
   FlowDT=TimeStep%DeltaT
   nsubDT=2
   DO WHILE (TranspDT < FlowDT/FLOAT(nsubDT) .AND. nsubDT .LT. f_iMAX_SUBTIMESTEPS)
       nsubDT=nsubDT+1
   ENDDO
   TranspDT=FlowDT/FLOAT(nsubDT)

   
   IF ( nsubDT .GT. f_iMAX_SUBTIMESTEPS ) THEN 
       WRITE(Text, '(A)')  '*** WARN: Transport timestep limited to maximum allowed subdivisions; CFL criterion may be violated'
       CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
   ENDIF
   
 END SUBROUTINE AdjustTranspTimeStep 
    
  
 ! -------------------------------------------------------------
 ! --- CALCULATE DISPERSION COEFFICIENTS 
 ! -------------------------------------------------------------
 SUBROUTINE CalcDispCoeff (LagTrans,  U_vel, V_vel, W_vel,  LongDispersivity, Dxx, Dyy, Dzz, Dxy, Dxz, Dyz, iStat )  
   CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
   REAL (KIND=f_IKIND), INTENT(IN)           :: LongDispersivity, U_vel, V_vel, W_vel
   REAL (KIND=f_IKIND), INTENT(OUT)          :: Dxx, Dyy, Dzz, Dxy, Dxz, Dyz
   INTEGER,INTENT(OUT)                       :: iStat    

   
   !-Local Variables
   CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'GetDispCoeff'
   REAL (KIND=f_IKIND)          :: Usq,Vsq,Wsq, Vtot, HorTrnsDisp, VerTrnsDisp
   
   iStat=0
      
   Usq=U_vel*U_vel
   Vsq=V_vel*V_vel
   Wsq=W_vel*W_vel
   Vtot=SQRT(Usq+Vsq+Wsq)
       
   HorTrnsDisp=LagTrans%DataProvider%Nodes%RatioHorzTransLong*LongDispersivity
   VerTrnsDisp=LagTrans%DataProvider%Nodes%RatioVertTransLong*LongDispersivity
   
   Dxx=LongDispersivity*(Usq)/Vtot+HorTrnsDisp*(Vsq )/Vtot  +VerTrnsDisp*(Wsq)/Vtot 
   Dyy=LongDispersivity*(Vsq)/Vtot+HorTrnsDisp*(Usq)/Vtot+VerTrnsDisp*(Wsq)/Vtot   
   Dzz=LongDispersivity*(Wsq)/Vtot+VerTrnsDisp*(Usq)/Vtot+VerTrnsDisp*(Vsq)/Vtot   
   
   Dxy=(LongDispersivity-HorTrnsDisp)*ABS(U_vel)*ABS(V_vel)/Vtot
   Dxz=(LongDispersivity-VerTrnsDisp)*ABS(U_vel)*ABS(W_vel)/Vtot
   Dyz=(LongDispersivity-VerTrnsDisp)*ABS(V_vel)*ABS(W_vel)/Vtot
       
   IF(Dxy < 0.0 .OR. Dxz < 0.0 .OR.Dyz < 0.0 ) THEN
       CALL SetLastMessage('Error calculating dispersion coefficients !',f_iFatal,ThisProcedure)
       iStat = -1
       RETURN
   ENDIF
       
 END SUBROUTINE CalcDispCoeff
 

 ! -------------------------------------------------------------
 ! --- GET AQUIFER PARAMETERS
 ! --- *Note: Calculate element porosity and longitudinal dispersivity  at parcel 
 ! ---        location based on nodal values using FEM interpolation
 ! ---        then calculate dispersion cofficients 
 ! -------------------------------------------------------------
 SUBROUTINE GetAquiferParameters (LagTrans, AppGrid, xcrd, ycrd, indxLayer, indxElem, U_Vel, V_Vel, W_Vel,  Dxx, Dyy, Dzz, Dxy, Dxz, Dyz, iStat)  
   CLASS(LagrangianTransportType),INTENT(IN)  :: LagTrans
   TYPE(AppGridType),INTENT(IN)               :: AppGrid
   INTEGER,INTENT(IN)                         :: indxLayer,  indxElem
   REAL (KIND=f_IKIND), INTENT(IN)            :: xcrd, ycrd, U_Vel, V_Vel, W_Vel 
   REAL (KIND=f_IKIND), INTENT(OUT)           :: Dxx, Dyy, Dzz, Dxy, Dxz, Dyz
   INTEGER,INTENT(OUT)                        :: iStat    
   
   !-Local Variables
   CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'GetAquiferParameters'
   INTEGER                      :: NVertex, Vertex(4)
   REAL (KIND=f_IKIND)          :: Coeff(4)
   REAL (KIND=f_IKIND)          :: LongDispersivity

   iStat=0
   
   !--Interpolate nodal attributes (porosity, longitudinal dispersivity, and layer top and bottom elevations) to grid centers
   NVertex = AppGrid%NVertex(indxElem)
   Vertex  = AppGrid%Vertex(:,indxElem)
   CALL AppGrid%FEInterpolate_AtCell(indxElem,xcrd, ycrd,Coeff(1:NVertex))
   LongDispersivity = SUM(Coeff(1:NVertex) * LagTrans%DataProvider%Nodes%LongDispersivity(Vertex(1:NVertex), indxLayer))   
   IF(LongDispersivity < 0.0 ) THEN
       CALL SetLastMessage('Error calculating longitudinal dispersivity !',f_iFatal,ThisProcedure)
       iStat = -1
       RETURN
   ENDIF
 
   !--Calculate the dispersion coefficients
   CALL LagTrans%CalcDispCoeff ( U_Vel, V_Vel, W_Vel, LongDispersivity, Dxx, Dyy, Dzz, Dxy, Dxz, Dyz, iStat)
   
     
 END SUBROUTINE GetAquiferParameters

 

 
 
 ! -------------------------------------------------------------
 ! --- INTERPOLATE VELOCITY
 ! --- *Note: Use FEM interpolation in 2D qudalilateral/triangle
 ! ---        horizontal plane and use linear interpolation in vertical direction
 ! ---        When transport is simulated using saved velocity data
 ! ---        use linear interpolation in time for particle tracking-RK4
 ! -------------------------------------------------------------
 SUBROUTINE InterpolateVelocity (LagTrans, AppGrid, Stratigraphy, xlp, ylp, zlp, jlyr, indxElem, Velocity, TimeInterpolate, Current, fVelTimeIntrp, interpolated3d_u, interpolated3d_v, interpolated3d_w, iStat)
   CLASS(LagrangianTransportType),INTENT(IN)    :: LagTrans
   TYPE(AppGridType),INTENT(IN)                 :: AppGrid
   TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy   
   TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
   INTEGER, INTENT(IN)                          :: jlyr, indxElem
   LOGICAL, INTENT (IN)                         :: TimeInterpolate, Current
   INTEGER, INTENT(OUT)                         :: iStat
   REAL(KIND=f_IKIND), INTENT (IN)              :: xlp, ylp, zlp, fVelTimeIntrp
   REAL(KIND=f_IKIND), INTENT (OUT)             :: interpolated3d_u, interpolated3d_v, interpolated3d_w

   !-Local Variables
   INTEGER                    :: NVertex, Vertex(4)
   REAL(KIND=f_IKIND), POINTER:: Vx(:,:), Vy(:,:), Vz(:,:), VxNext(:,:), VyNext(:,:), VzNext(:,:)
   REAL (KIND=f_IKIND)        :: interp2d_Lyr1_u, interp2d_Lyr1_v,  interp2d_Lyr1_w,  interp2d_Lyr2_u, interp2d_Lyr2_v,  interp2d_Lyr2_w , LyrTopElev, LyrBotElev
   REAL (KIND=f_IKIND)        :: interp2d_Lyr1_u_nextT,interp2d_Lyr1_v_nextT,interp2d_Lyr1_w_nextT, interp2d_Lyr2_u_nextT, interp2d_Lyr2_v_nextT,  interp2d_Lyr2_w_nextT
   REAL (KIND=f_IKIND)        :: Coeff(4)
  
   
   iStat=0
  
   Vx=>Velocity%Vx
   Vy=>Velocity%Vy
   Vz=>Velocity%Vz

   !--IF velocity is read from file interpolate velocity in time
   IF (.NOT.LagTrans%DataProvider%lFlowSimulated ) THEN 
       
     IF (.NOT. Current) THEN
          Vx=>Velocity%VxNext
          Vy=>Velocity%VyNext
          Vz=>Velocity%VzNext
     ENDIF
     
     IF (TimeInterpolate) THEN
       VxNext=>Velocity%VxNext
       VyNext=>Velocity%VyNext
       VzNext=>Velocity%VzNext
     ENDIF

   ENDIF
       
   
   !--Finite element 2D interpolation using FEM shape functions-top layer
   NVertex = AppGrid%NVertex(indxElem)
   Vertex  = AppGrid%Vertex(:,indxElem)
   CALL AppGrid%FEInterpolate_AtCell(indxElem,xlp, ylp,Coeff(1:NVertex))
   interp2d_Lyr1_u= SUM(Coeff(1:NVertex)*Vx(Vertex(1:NVertex),jlyr))   
   interp2d_Lyr1_v= SUM(Coeff(1:NVertex)*Vy(Vertex(1:NVertex),jlyr))  
   interp2d_Lyr1_w= SUM(Coeff(1:NVertex)*Vz(Vertex(1:NVertex),jlyr))  
   
   !--For Runge Kutta time integration 
   IF ( TimeInterpolate .AND. .NOT. LagTrans%DataProvider%lFlowSimulated ) THEN
       CALL AppGrid%FEInterpolate_AtCell(indxElem,xlp, ylp,Coeff(1:NVertex))
       interp2d_Lyr1_u_nextT= SUM(Coeff(1:NVertex)*VxNext(Vertex(1:NVertex),jlyr))   
       interp2d_Lyr1_v_nextT= SUM(Coeff(1:NVertex)*VyNext(Vertex(1:NVertex),jlyr))  
       interp2d_Lyr1_w_nextT= SUM(Coeff(1:NVertex)*VzNext(Vertex(1:NVertex),jlyr))  

       !--Linear interpolation of velocity in time for saved velocity run
       interp2d_Lyr1_u=(1.0-fVelTimeIntrp)*interp2d_Lyr1_u+ fVelTimeIntrp*interp2d_Lyr1_u_nextT
       interp2d_Lyr1_v=(1.0-fVelTimeIntrp)*interp2d_Lyr1_v+ fVelTimeIntrp*interp2d_Lyr1_v_nextT
       interp2d_Lyr1_w=(1.0-fVelTimeIntrp)*interp2d_Lyr1_w+ fVelTimeIntrp*interp2d_Lyr1_w_nextT
   ENDIF

   
   !--2D Bilinear/Linear interpolation of velocity of the bottom layer
   !--Finite element 2D interpolation using FEM shape functions-bottom layer
   CALL AppGrid%FEInterpolate_AtCell(indxElem,xlp, ylp,Coeff(1:NVertex))
   interp2d_Lyr2_u= SUM(Coeff(1:NVertex)*Vx(Vertex(1:NVertex),jlyr+1))  
   interp2d_Lyr2_v= SUM(Coeff(1:NVertex)*Vy(Vertex(1:NVertex),jlyr+1))  
   interp2d_Lyr2_w= SUM(Coeff(1:NVertex)*Vz(Vertex(1:NVertex),jlyr+1))  
   
   
   !--For Runge Kutta time integration of velocity 
   IF ( TimeInterpolate .AND..NOT. LagTrans%DataProvider%lFlowSimulated) THEN
       CALL AppGrid%FEInterpolate_AtCell(indxElem,xlp, ylp,Coeff(1:NVertex))
       interp2d_Lyr2_u_nextT= SUM(Coeff(1:NVertex)*VxNext(Vertex(1:NVertex),jlyr+1))   
       interp2d_Lyr2_v_nextT= SUM(Coeff(1:NVertex)*VyNext(Vertex(1:NVertex),jlyr+1))  
       interp2d_Lyr2_w_nextT= SUM(Coeff(1:NVertex)*VzNext(Vertex(1:NVertex),jlyr+1))  

       !--Linear interpolation of velocity in time for saved velocity run
       interp2d_Lyr2_u=(1.0-fVelTimeIntrp)*interp2d_Lyr2_u+ fVelTimeIntrp*interp2d_Lyr2_u_nextT
       interp2d_Lyr2_v=(1.0-fVelTimeIntrp)*interp2d_Lyr2_v+ fVelTimeIntrp*interp2d_Lyr2_v_nextT
       interp2d_Lyr2_w=(1.0-fVelTimeIntrp)*interp2d_Lyr2_w+ fVelTimeIntrp*interp2d_Lyr2_w_nextT
   ENDIF

   !--1D Linear interpolation in the vertical direction using already 2d interpolated velocity
   CALL Get_ElemTopBotElev(AppGrid, Stratigraphy, jlyr, indxElem, LyrTopElev, LyrBotElev)
   CALL InterpolateVelocity_1D (  zlp, LyrTopElev, LyrBotElev,  interp2d_Lyr1_u,  interp2d_Lyr2_u, interpolated3d_u)
   CALL InterpolateVelocity_1D (  zlp, LyrTopElev, LyrBotElev,  interp2d_Lyr1_v,  interp2d_Lyr2_v,  interpolated3d_v)
   CALL InterpolateVelocity_1D (  zlp, LyrTopElev, LyrBotElev,  interp2d_Lyr1_w, interp2d_Lyr2_w,  interpolated3d_w)
           
 END SUBROUTINE InterpolateVelocity

 
 ! -------------------------------------------------------------
 ! --- INTERPOLATE VELOCITY IN 1-D
 ! --- *Note: Use linear  interpolation to caculate velocity at given particle 
 ! ---        used for interpolating only in the vertical direction
 ! -------------------------------------------------------------
 SUBROUTINE InterpolateVelocity_1D ( zlp, z1, z2, u1, u2,  interpolated_vel)
   REAL(KIND=8), INTENT(IN)  :: zlp, z1, z2, u1, u2
   REAL(KIND=8), INTENT(OUT) :: interpolated_vel
   
   interpolated_vel=u1+(zlp-z1)/(z2-z1)*(u2-u1)
   
 END SUBROUTINE  InterpolateVelocity_1D        
  
  
 ! -------------------------------------------------------------
 ! --- CALCULATE THE TRANSPORT OF THE LP DUE TO ADVECTION AND DISPERSION
 ! -------------------------------------------------------------
 SUBROUTINE DisplaceLPs( LagTrans, AppGrid,Stratigraphy, x_pos, y_pos, z_pos, ilayer, iElem, Velocity, dt, iTrns, LagrangianType, iStat)
   CLASS(LagrangianTransportType),INTENT(IN)    :: LagTrans
   TYPE(AppGridType),INTENT(IN)                 :: AppGrid
   TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy 
   TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
   REAL (KIND=f_IKIND), INTENT(IN)              :: dt
   INTEGER, INTENT (IN)                         :: iLayer, iElem, iTrns
   INTEGER, INTENT (OUT)                        :: iStat
   REAL (KIND=f_IKIND), INTENT(INOUT)           :: x_pos, y_pos, z_pos
   CHARACTER(*), INTENT(IN)                     :: LagrangianType
    
   !-Local variables
   REAL (KIND=f_IKIND)         :: U_Vel, V_Vel, W_Vel
   CHARACTER(LEN=ModNameLen+35):: ThisProcedure = ModName // 'DisplaceLPS'
   CHARACTER(LEN=500)          :: Text
   iStat=0
   
       SELECT CASE (TRIM(ADJUSTL(UpperCase(LagrangianType))))  
       CASE ("SOLUTETRANSPORT")
           !--Advection
           CALL LagTrans%AdvectLPs_RK2 (AppGrid,Stratigraphy,x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel, V_Vel, W_Vel, dt, iTrns, iStat ) 

           !--Dispersion
           CALL LagTrans%DisperseLPs(AppGrid, x_pos, y_pos, z_pos,ilayer, iElem,U_Vel, V_Vel, W_Vel,dt, iStat )

       CASE ("PARTICLETRACKING") 
           !--Advection
             SELECT CASE (INT(LagTrans%iSolnType)) 
               CASE(1)
                  CALL LagTrans%AdvectLPs_Euler (AppGrid,Stratigraphy,x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel, V_Vel, W_Vel, dt, iStat )
                CASE(2) 
                   CALL LagTrans%AdvectLPs_RK2 (AppGrid,Stratigraphy, x_pos, y_pos, z_pos, Velocity, iLayer, iElem, U_Vel, V_Vel, W_Vel,dt, iTrns, iStat)
               CASE(3) 
                   CALL LagTrans%AdvectLPs_RK4 (AppGrid,Stratigraphy, x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel, V_Vel, W_Vel, dt, iTrns, iStat)
               CASE DEFAULT
                   WRITE(Text, ' (A) ') 'Unknown Time Integration Types, defaulting to RK4'
                   CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
                   CALL LagTrans%AdvectLPs_RK4 (AppGrid,Stratigraphy, x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel, V_Vel, W_Vel, dt,iTrns, iStat)
               END SELECT

       CASE DEFAULT
           CALL SetLastMessage('Unknown Lagrangian Type: !',f_iFatal,ThisProcedure)
           iStat = -1
           RETURN
       END SELECT

 END SUBROUTINE DisplaceLPs
 
 
 ! -------------------------------------------------------------
 ! --- CALCULATE THE ADVECTION OF THE LPS USING 1ST ORDER EULER METHOD
 ! --- *Note: Advection velocity is calculated based on the nodal interpolated velocities
 ! -------------------------------------------------------------
  SUBROUTINE AdvectLPs_Euler(LagTrans, AppGrid,Stratigraphy, x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel_interp, V_Vel_interp, W_Vel_interp, dt, iStat)
   CLASS(LagrangianTransportType),INTENT(IN)    :: LagTrans
   TYPE(AppGridType),INTENT(IN)                 :: AppGrid
   TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy 
   TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
   REAL (KIND=f_IKIND), INTENT(IN)              :: dt
   INTEGER, INTENT (IN)                         :: ilayer, iElem
   REAL (KIND=f_IKIND), INTENT(INOUT)           :: x_pos, y_pos, z_pos
   REAL (KIND=f_IKIND), INTENT(OUT)             :: U_Vel_interp, V_Vel_interp, W_Vel_interp
   INTEGER, INTENT (OUT)                        :: iStat
   
   !-Local variables
   REAL(KIND=f_IKIND), PARAMETER :: fVelTimeIntrp=1.0
   REAL(KIND=f_IKIND), POINTER :: Vx(:,:), Vy(:,:), Vz(:,:)
   
   LOGICAL :: TimeInterpolate, Current
    
   iStat=0
   
   Current =.TRUE. 
   TimeInterpolate=.FALSE. 
   
   Vx=>Velocity%Vx
   Vy=>Velocity%Vy
   Vz=>Velocity%Vz

   CALL LagTrans%InterpolateVelocity(AppGrid  , &
                               Stratigraphy   , &
                               x_pos          , &
                               y_pos          , &
                               z_pos          , &
                               iLayer         , &
                               iElem          , &
                               Velocity       , &
                               TimeInterpolate, &
                               Current        , &
                               fVelTimeIntrp  , &
                               U_Vel_interp   , &
                               V_Vel_interp   , &
                               W_Vel_interp   , &
                               iStat); IF (iStat .EQ. -1) RETURN
   
   !-Calculate the New particle locations  use 1st order Euler scheme 
   x_pos = x_pos+U_Vel_interp*dt       
   y_pos = y_pos+V_Vel_interp*dt        
   z_pos = z_pos+W_Vel_interp*dt    
   
  END SUBROUTINE AdvectLPs_Euler
  
  
  
 ! -------------------------------------------------------------
 ! --- CALCULATE THE ADVECTION OF THE LPS USING 2ND ORDER RUNGE-KUTTA METHOD (MID-POINT)
 ! --- *Note: Advection velocity is calculated based on the nodal interpolated velocities
 ! -------------------------------------------------------------
  SUBROUTINE AdvectLPs_RK2(LagTrans, AppGrid,Stratigraphy, x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel_interp, V_Vel_interp, W_Vel_interp, dt, iTrns, iStat)
  
   CLASS(LagrangianTransportType),INTENT(IN)    :: LagTrans
   TYPE(AppGridType),INTENT(IN)                 :: AppGrid
   TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy 
   TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
   REAL (KIND=f_IKIND), INTENT(IN)              :: dt
   INTEGER, INTENT (IN)                         :: ilayer, iElem, iTrns
   REAL (KIND=f_IKIND), INTENT(INOUT)           :: x_pos, y_pos, z_pos
   REAL (KIND=f_IKIND), INTENT(OUT)             :: U_Vel_interp, V_Vel_interp, W_Vel_interp
   INTEGER, INTENT (OUT)                        :: iStat

   
   !-Local variables
   INTEGER              :: jLayer, jElem
   REAL (KIND=f_IKIND)  :: k1x, k2x, k1y, k2y, k1z, k2z 
   REAL (KIND=f_IKIND)  :: x_pos_new, y_pos_new,z_pos_new, fVelTimeIntrp_k1, fVelTimeIntrp_k2
   
   iStat=0
   
   jElem=iElem
   jLayer=iLayer

   IF (.NOT.LagTrans%DataProvider%lFlowSimulated ) THEN 
       fVelTimeIntrp_k1=dt*FLOAT(iTrns-1)/LagTrans%DataProvider%TimeStep%DeltaT 
       fVelTimeIntrp_k2=dt*FLOAT(iTrns)/LagTrans%DataProvider%TimeStep%DeltaT
   ELSE
       fVelTimeIntrp_k1=0.0
       fVelTimeIntrp_k2=0.5
   ENDIF
   
   !--Step 1: Velocity current particle location
   CALL LagTrans%InterpolateVelocity(AppGrid   , &
                               Stratigraphy    , &
                               x_pos           , &
                               y_pos           , &
                               z_pos           , &
                               jLayer          , &
                               jElem           , &
                               Velocity        , &
                               .FALSE.         , &
                               .TRUE.          , &
                               fVelTimeIntrp_k1, &
                               k1x             , &
                               k1y             , &
                               k1z             , &
                               iStat); IF (iStat .EQ. -1) RETURN
   
      !-Mid point position
      x_pos_new=x_pos+dt/2*k1x
      y_pos_new=y_pos+dt/2*k1y
      z_pos_new=z_pos+dt/2*k1z
   
      !--Update element and layer 
      CALL Update_ElementAndLayer (LagTrans, AppGrid, Stratigraphy, iElem, x_pos_new,y_pos_new, z_pos_new, jElem, jLayer, iStat)

     !-Step 2: Velocity at mid point  
      CALL LagTrans%InterpolateVelocity(AppGrid  , &
                               Stratigraphy      , &
                               x_pos_new         , &
                               y_pos_new         , &
                               z_pos_new         , &
                               jLayer            , &
                               jElem             , &
                               Velocity          , &
                               .TRUE.            , &
                               .TRUE.            , &
                               fVelTimeIntrp_k2  , &
                               k2x               , &
                               k2y               , &
                               k2z               , &
                               iStat); IF (iStat .EQ. -1) RETURN
      
   
       !--Update the new particle locations 
       x_pos = x_pos+dt*k2x       
       y_pos = y_pos+dt*k2y      
       z_pos = z_pos+dt*k2z    
   
       U_Vel_interp=k2x   
       V_Vel_interp=k2y   
       W_Vel_interp=k2z   

   
  END SUBROUTINE AdvectLPs_RK2  
  
  
 ! -------------------------------------------------------------
 ! --- CALCULATE THE ADVECTION OF THE LPS USING 4TH ORDER RUNGE-KUTTA METHOD
 ! --- *Note: Advection velocity is calculated based on the nodal interpolated velocities
 ! -------------------------------------------------------------
  SUBROUTINE AdvectLPs_RK4(LagTrans, AppGrid,Stratigraphy, x_pos, y_pos, z_pos, Velocity, iLayer, iElem,U_Vel_interp, V_Vel_interp, W_Vel_interp, dt, iTrns, iStat)
   CLASS(LagrangianTransportType),INTENT(IN)    :: LagTrans
   TYPE(AppGridType),INTENT(IN)                 :: AppGrid
   TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy 
   TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
   REAL (KIND=f_IKIND), INTENT(IN)              :: dt
   INTEGER, INTENT (IN)                         :: ilayer, iElem, iTrns
   REAL (KIND=f_IKIND), INTENT(INOUT)           :: x_pos, y_pos, z_pos
   REAL (KIND=f_IKIND), INTENT(OUT)             :: U_Vel_interp, V_Vel_interp, W_Vel_interp
   INTEGER, INTENT (OUT)                        :: iStat

   

   !-Local variables
   INTEGER              :: jLayer, jElem
   REAL (KIND=f_IKIND)  :: k1x, k2x, k3x,k4x, k1y, k2y, k3y,k4y, k1z, k2z, k3z, k4z
   REAL (KIND=f_IKIND)  :: x_pos_new, y_pos_new,z_pos_new, fVelTimeIntrp_k1, fVelTimeIntrp_k2, fVelTimeIntrp_k3, fVelTimeIntrp_k4
   
   iStat=0
   
   
   IF (.NOT.LagTrans%DataProvider%lFlowSimulated ) THEN 
       fVelTimeIntrp_k1=dt*FLOAT(iTrns-1)/LagTrans%DataProvider%TimeStep%DeltaT
       fVelTimeIntrp_k2=dt*(FLOAT(iTrns-1)+0.5)/LagTrans%DataProvider%TimeStep%DeltaT
       fVelTimeIntrp_k3=fVelTimeIntrp_k2
       fVelTimeIntrp_k4=dt*FLOAT(iTrns)/LagTrans%DataProvider%TimeStep%DeltaT
   ELSE
       fVelTimeIntrp_k1=0.0
       fVelTimeIntrp_k2=0.5
       fVelTimeIntrp_k3=0.5
       fVelTimeIntrp_k4=1.0
   ENDIF

   
   !--Step 1: Velocity at current position
   CALL LagTrans%InterpolateVelocity(AppGrid   , &
                               Stratigraphy    , &
                               x_pos           , &
                               y_pos           , &
                               z_pos           , &
                               iLayer          , &
                               iElem           , &
                               Velocity        , &
                               .FALSE.         , &
                               .TRUE.          , &
                               fVelTimeIntrp_k1, &
                               k1x             , &
                               k1y             , &
                               k1z             , &
                               iStat); IF (iStat .EQ. -1) RETURN
     !--Mid point location 
      x_pos_new=x_pos+dt/2*k1x
      y_pos_new=y_pos+dt/2*k1y
      z_pos_new=z_pos+dt/2*k1z
   
      !--Update Element number and Layer number
      CALL Update_ElementAndLayer (LagTrans, AppGrid, Stratigraphy, iElem,x_pos_new,y_pos_new, z_pos_new, jElem, jLayer, iStat)

     !--Step 2: Veloicty at mid point
      CALL LagTrans%InterpolateVelocity(AppGrid  , &
                               Stratigraphy      , &
                               x_pos_new         , &
                               y_pos_new         , &
                               z_pos_new         , &
                               jLayer            , &
                               jElem             , &
                               Velocity          , &
                               .TRUE.            , &
                               .TRUE.            , &
                               fVelTimeIntrp_k2  , &
                               k2x               , &
                               k2y               , &
                               k2z               , &
                               iStat); IF (iStat .EQ. -1) RETURN
      
      !--Mid point position using K2
      x_pos_new=x_pos+dt/2*k2x
      y_pos_new=y_pos+dt/2*k2y
      z_pos_new=z_pos+dt/2*k2z
   
      !--Update Element number and Layer number
      CALL Update_ElementAndLayer (LagTrans, AppGrid, Stratigraphy, iElem,x_pos_new,y_pos_new, z_pos_new, jElem, jLayer, iStat)
      
      
      !--Step 3: Velocity at mid point
      CALL LagTrans%InterpolateVelocity(AppGrid  , &
                               Stratigraphy      , &
                               x_pos_new         , &
                               y_pos_new         , &
                               z_pos_new         , &
                               jLayer            , &
                               jElem             , &
                               Velocity          , &
                               .TRUE.            , &
                               .TRUE.            , &
                               fVelTimeIntrp_k3  , &
                               k3x               , &
                               k3y               , &
                               k3z               , &
                               iStat); IF (iStat .EQ. -1) RETURN
      
      !--Mid point position using K3
      x_pos_new=x_pos+dt*k3x
      y_pos_new=y_pos+dt*k3y
      z_pos_new=z_pos+dt*k3z
   
      !--Update Element number and Layer number
      CALL Update_ElementAndLayer (LagTrans, AppGrid, Stratigraphy, iElem,x_pos_new,y_pos_new, z_pos_new, jElem, jLayer, iStat)
      
      !--Step 4: Velocity at end point
      CALL LagTrans%InterpolateVelocity(AppGrid  , &
                               Stratigraphy      , &
                               x_pos_new         , &
                               y_pos_new         , &
                               z_pos_new         , &
                               jLayer            , &
                               jElem             , &
                               Velocity          , & 
                               .FALSE.           , &
                               .FALSE.           , &
                               fVelTimeIntrp_k4  , &
                               k4x               , &
                               k4y               , &
                               k4z               , &
                               iStat); IF (iStat .EQ. -1) RETURN
   
   !- End point position
   x_pos = x_pos+dt/6.0*(k1x+2*k2x+2*k3x+k4x)      
   y_pos = y_pos+dt/6.0*(k1y+2*k2y+2*k3y+k4y)        
   z_pos = z_pos+dt/6.0*(k1z+2*k2z+2*k3z+k4z) 
   
   
   U_Vel_interp=(k1x+2*k2x+2*k3x+k4x)/6.0
   V_Vel_interp=(k1y+2*k2y+2*k3y+k4y)/6.0 
   W_Vel_interp=(k1z+2*k2z+2*k3z+k4z)/6.0
   
  END SUBROUTINE AdvectLPs_RK4
   
  
  ! -------------------------------------------------------------
  ! --- UDPATE THE ELEMENT NUMBER AND LAYER NUMBER AFTER EACH DISPLACEMENT
  ! -------------------------------------------------------------
  SUBROUTINE Update_ElementAndLayer (LagTrans, AppGrid,Stratigraphy,iElem, x_pos, y_pos, z_pos,jElem,jLayer,iStat)
                                     
   CLASS(LagrangianTransportType),INTENT(IN)    :: LagTrans
   TYPE(AppGridType),INTENT(IN)                 :: AppGrid
   TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy 
   INTEGER, INTENT (IN)                         :: iElem
   INTEGER, INTENT (OUT)                        :: jlayer, jElem
   REAL (KIND=f_IKIND), INTENT(INOUT)           :: x_pos, y_pos, z_pos
   INTEGER, INTENT (OUT)                        :: iStat
   
   !-Local variables
   CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'UpdateElement&Layer'
   LOGICAL                      :: found

       !--Update the element number: If the particle has moved to a new element figure out the new element number 
       CALL Get_LP_Elem (LagTrans, AppGrid, iElem,  x_pos,  y_pos, jElem, found, iStat)
       IF ( .NOT. found) jElem=iElem
       IF (iStat.EQ.-1) THEN
           CALL SetLastMessage('Element number calculated is wrong!',f_iFatal,ThisProcedure)
           RETURN
       ENDIF
       
       !-- Check if the LP has moved out of the domain-Apply boundary conditions
       CALL ApplyReflectionBC (LagTrans, jElem, x_pos,  y_pos, z_pos) 

       !--Update the layer number: If the parcel has moved to another layer figure out the new layer
       CALL Get_LP_Layer( LagTrans, Stratigraphy, 1, jElem, z_pos, jLayer, iStat)  
       IF (iStat.EQ.-1) THEN
           CALL SetLastMessage('LP moved outside the vertical transport domain!',f_iFatal,ThisProcedure)
           RETURN 
       ENDIF         

  END SUBROUTINE Update_ElementAndLayer
   
   
  ! -------------------------------------------------------------
  ! --- CALCULATE THE TRANSPORT OF THE LP DUE TO ADVECTION AND DIFFUSION
  ! --- *Note: Dispersion velocity is calculated the Random Walk method    
  ! -------------------------------------------------------------
  SUBROUTINE DisperseLPs(LagTrans, AppGrid, x_pos, y_pos, z_pos,ilayer, iElem,U_Vel, V_Vel, W_Vel,dt, iStat)
    CLASS(LagrangianTransportType),INTENT(IN) :: LagTrans
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    REAL (KIND=f_IKIND),INTENT(IN)            :: dt, U_Vel, V_Vel, W_Vel
    INTEGER, INTENT (IN)                      :: ilayer, iElem
    INTEGER, INTENT (OUT)                     :: iStat 
    REAL (KIND=f_IKIND), INTENT(INOUT)        :: x_pos, y_pos, z_pos
    
    !-Local variables
    REAL (KIND=f_IKIND) :: u_ran, v_ran, w_ran
    REAL (KIND=f_IKIND) :: randx, randy, randz  ! Random number generator requires real 4   
    REAL (KIND=f_IKIND) :: Dxx, Dyy, Dzz, Dxy, Dxz, Dyz

 
    iStat=0
    
        !--First calculate the dispersion coefficients
        CALL LagTrans%GetAquiferParameters (AppGrid, x_pos, y_pos,ilayer,iElem,U_Vel, V_Vel, W_Vel,Dxx, Dyy, Dzz, Dxy, Dxz, Dyz, iStat )
 
        !--Assign each parcel unique seed so deleting parceles won't impact reproducibility   
        #ifdef _OPENMP
        CALL  GenerateStandardNormalRandomZig(randx, iStat)
        CALL  GenerateStandardNormalRandomZig(randy, iStat)
        CALL  GenerateStandardNormalRandomZig(randz, iStat)
        #else
        CALL GenerateRandomNumberRW3D(randx)
        CALL GenerateRandomNumberRW3D(randy)
        CALL GenerateRandomNumberRW3D(randz)
        #endif


        u_ran=(SQRT(2.0*Dxx/dt))*randx      ! Random velocity component along X-dir
        v_ran=(SQRT(2.0*Dyy/dt))*randy      ! Random velocity component along Y-dir
        w_ran=(SQRT(2.0*Dzz/dt))*randz      ! Random velocity component along Y-dir    
                 
        !--Update the new particle position         
        x_pos = x_pos+u_ran*dt       
        y_pos = y_pos+v_ran*dt        
        z_pos = z_pos+W_ran*dt     

  END SUBROUTINE DisperseLPs
  
  
  ! -------------------------------------------------------------
  ! --- INITIALIZE  RANDOM NUMBER FOR 
  ! --- RANDOM_NUMBER  GENERATOR
  ! --- *Note: one seed values is used for every simulation
  ! ---   so the random numbers generated are reproducible
  ! -------------------------------------------------------------
  SUBROUTINE InitDispersionRNG(seed_value)
    INTEGER, INTENT(IN)  :: seed_value
    INTEGER, ALLOCATABLE :: seed(:)
    INTEGER :: n, i
  
    CALL RANDOM_SEED(size=n)
    ALLOCATE(seed(n))
  
    ! Initialize seed array with one integer
    DO i = 1, n
        seed(i) = seed_value + i*37   ! spread values so all elements change
    END DO
  
    CALL RANDOM_SEED(put=seed)
    DEALLOCATE(seed)
  END SUBROUTINE InitDispersionRNG
  
  
  ! -------------------------------------------------------------
  ! --- GENERATE GAUSSIAN RANDOM NUMBER 
  ! --- *Note: s=standard devation, am=mean
  ! -------------------------------------------------------------
  FUNCTION RandGaussian(std_dev, mean) RESULT(gaussRN)
    REAL, INTENT(IN) :: std_dev, mean
    REAL             :: gaussRN
    REAL             :: a, y
    INTEGER          :: i

    a = 0.0
    DO i = 1, 12
      CALL RANDOM_NUMBER(y)
      a = a + y
    END DO

    gaussRN = (a - 6.0)*std_dev + mean
  END FUNCTION RandGaussian
      
      
  !-------------------------------------------------------------
  !--- GENERATE STANDARD NORMAL RANDOM NUMBER 
  !--- *Note: This subroutine is copied from MODPATH-RW
  !---  SOURCE CODE
  !-------------------------------------------------------------
  SUBROUTINE    GenerateStandardNormalRandomZig (random_value, iStat)
    REAL(KIND=f_IKIND), INTENT(OUT) :: random_value
    INTEGER, INTENT (OUT)           :: iStat
    ! local
    INTEGER, PARAMETER:: nvals = 12
    INTEGER           :: m, threadId
    REAL(KIND=f_IKIND):: r,p 
    
    iStat=0
  
       #ifdef _OPENMP
            threadId = omp_get_thread_num()
        #else
            threadId = 0
        #endif
    
            r = 0d0
            DO m=1,nvals
              CALL  rng_uni(p,threadId, iStat)
              r = r + p
            END DO
            random_value = r - 6.d0
   
            RETURN
  
  END SUBROUTINE GenerateStandardNormalRandomZig
  
  
  ! -------------------------------------------------------------
  ! --- GENERATE STANDARD NORMAL RANDOM NUMBER 
  ! --- *Note: This subroutine is copied from MODPATH-RW
  ! ---  SOURCE CODE
  ! -------------------------------------------------------------
  SUBROUTINE   GenerateRandomNumberRW3D (random_value ) 
    REAL(KIND=f_IKIND), INTENT(OUT) :: random_value
 
    ! Local variables
    REAL(KIND=f_IKIND) :: harvest(12)
  
    CALL RANDOM_NUMBER (harvest)
    random_value = SUM(harvest)-6.d0
  
  END SUBROUTINE GenerateRandomNumberRW3D
  
  
  ! -------------------------------------------------------------
  ! --- INITIALIZE  RANDOM NUMBER BASED ON 
  ! --- THE RANDOM NUMBER  GENERATOR TYPE
  ! -------------------------------------------------------------
  SUBROUTINE   InitializeRandomNumberGenerator ( ) 
   ! RNG parallel ifort
    INTEGER(int64), DIMENSION(2) :: seedzigrng
    INTEGER(int64), PARAMETER    :: baseseed(2) = [INT(Z'1DADBEEFBAADD0D0', int64), &
                                                INT(Z'5BADD0D0DEADBEEF', int64)]
    INTEGER :: ompNumThreads
    
    #ifdef _OPENMP
       ompNumThreads = omp_get_max_threads()
       seedzigrng    = baseseed
       CALL rng_init(ompNumThreads, seedzigrng) 
    #else
       ompNumThreads = 1
       CALL InitDispersionRNG(123456) 
    #endif
  
  END SUBROUTINE InitializeRandomNumberGenerator


  ! -------------------------------------------------------------
  ! --- COMPUTE TOP AND BOTTOM ELEVATIONS OF AN AQUIFER LAYER AT AN ELEMENT 
  ! -------------------------------------------------------------
  SUBROUTINE Get_ElemTopBotElev (AppGrid, Stratigraphy, ilyr, ielem, zcord1, zcord2)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy    
    INTEGER , INTENT (IN)             :: ielem, ilyr
    REAL (KIND=f_IKIND), INTENT (OUT)       :: zcord1, zcord2
        
    !-Local Variables
    INTEGER :: Vertex(4), nID, I
    
    Vertex=AppGrid%Vertex(:,ielem)  
    zcord1=0.0d0
    zcord2=0.0d0
    DO I=1, 3
       nID=Vertex(I)
        zcord1=zcord1+Stratigraphy%TopElev(nID,ilyr)
        zcord2=zcord2+Stratigraphy%BottomElev(nID,ilyr)
    ENDDO
    
    nID=Vertex(4)
    IF (nID>0)  THEN
        zcord1=zcord1+Stratigraphy%TopElev(nID,ilyr)
        zcord2=zcord2+Stratigraphy%BottomElev(nID,ilyr)
        zcord1=zcord1/4.
        zcord2=zcord2/4.
    ELSE
        zcord1=zcord1/3.
        zcord2=zcord2/3.
    ENDIF
    
  END SUBROUTINE Get_ElemTopBotElev

  
END MODULE  Class_LagrangianTransport