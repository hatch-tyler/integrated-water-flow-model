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
MODULE  TransportManager
  USE TransportDataReader       , ONLY: TransportDataReaderType
  USE Class_TransportBase       , ONLY: TransportMethodType
  USE Package_ParticleTracking  , ONLY: ParticleTrackingType             
  USE Package_LagrangianSolTrans, ONLY: SoluteTransportLPType            
  USE Package_AppGW             , ONLY: AppGWType                        
  USE MessageLogger             , ONLY: SetLastMessage                   , &
                                        EchoProgress                     , &
                                        f_iFatal                         
  USE GeneralUtilities          , ONLY: StripTextUntilCharacter          , &
                                        CleanSpecialCharacters           , &
                                        UpperCase                        , &
                                        EstablishAbsolutePathFileName    
  USE TimeSeriesUtilities       , ONLY: TimeStepType                     , &
                                        IncrementTimeStamp               
  USE IOInterface               , ONLY: GenericFileType                                     
  USE Package_Discretization    , ONLY: AppGridType                      , &
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
  PUBLIC :: TransportManagerType
  

  ! -------------------------------------------------------------
  ! TRANSPORT MANAGER
  ! -------------------------------------------------------------
  TYPE TransportManagerType
      PRIVATE
      TYPE(TransportDataReaderType)          :: DataProvider
      CLASS(TransportMethodType),ALLOCATABLE :: LagrangianParticleTracking
      CLASS(TransportMethodType),ALLOCATABLE :: LagrangianSoluteTransport
      CLASS(TransportMethodType),ALLOCATABLE :: FiniteElementSoluteTransport
      CHARACTER(:),ALLOCATABLE               :: cSIMWorkingDirectory           
      INTEGER                                :: iMethodSolTrans              = 0
      INTEGER                                :: iMethodParTrack              = 0
      LOGICAL                                :: lSolTransDefined             = .FALSE.      !Flag to check if solute transport is defined
      LOGICAL                                :: lParTrackDefined             = .FALSE.      !Flag to check if particle tracking is defined
      LOGICAL                                :: lCoupledSolution             = .FALSE.      !Flag for flow and transport coupled solution 
      LOGICAL                                :: lDefined                     = .FALSE.      !Flag to check if Transport is simulated
      LOGICAL                                :: lFlowSimulated               = .FALSE.      !Flag to check if flow is simulated  
  CONTAINS
      PROCEDURE :: New                    => InitializeTransportManager
      PROCEDURE :: CreateSoluteTransport
      PROCEDURE :: RunCommonDataProvider
      PROCEDURE :: ReadTSData
      PROCEDURE :: SimulateOneTimeStep
      PROCEDURE :: PrintResults 
      PROCEDURE :: AdvanceTime
      PROCEDURE :: IsTransportDefined 
      PROCEDURE :: IsFlowSimulated
      PROCEDURE :: IsEndOfSimulation
  END TYPE
      

  ! -------------------------------------------------------------
  ! --- SIMULATION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: nTransSIM_InputFiles              = 2   ! Solute transport and particle tracking
     
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen  = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'TransportManager::'
   

CONTAINS
 
  ! -------------------------------------------------------------
  ! --- INITIALIZE TRANPORT MANAGER 
  ! -------------------------------------------------------------
  SUBROUTINE InitializeTransportManager(Transport,  IsForInquiry,  cFileName, cWorkingDirectory, AppGrid,Stratigraphy,TimeStep, iStat)
    CLASS(TransportManagerType),  INTENT(INOUT) :: Transport
    LOGICAL,INTENT(IN)                          :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                 :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep
    INTEGER,INTENT(OUT)                         :: iStat
    
    !--Local variables
    CHARACTER(LEN=ModNameLen+17):: ThisProcedure = ModName // 'TransportMainFile'
    TYPE(GenericFileType)       :: TransManagerFile
    CHARACTER                   ::  ALine*200
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    CHARACTER                   :: STProjectFileNames(nTransSIM_InputFiles)*1000

    iStat = 0

    !--Return if cFileName is empty
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating transport component...')
     
    !--Simulation working directory
    Transport%cSIMWorkingDirectory = cWorkingDirectory
    
    !Open transport manager file
    CALL TransManagerFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,Descriptor='Transport Manager',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
     !--Read aquifer parameter data files names for solute transport simulation
    CALL ReadTransportMethods( TransManagerFile, STProjectFileNames ,  iStat) 
    IF (iStat .EQ. -1) RETURN
    
    IF ( STProjectFileNames(1) .NE. " ") THEN
              Transport%lSolTransDefined=.TRUE.
              Transport%DataProvider%lSolTransDefined=.TRUE.
    ELSE
              Transport%lSolTransDefined=.FALSE.
              Transport%DataProvider%lSolTransDefined=.FALSE.   
    END IF
    
    IF ( STProjectFileNames(2) .NE. " ") THEN
              Transport%lParTrackDefined=.TRUE.
              Transport%DataProvider%lParTrackDefined=.TRUE.
    ELSE
             Transport%lParTrackDefined=.FALSE.
             Transport%DataProvider%lParTrackDefined=.FALSE.
    END IF
    
    IF ( STProjectFileNames(1) .EQ. " "  .AND.  STProjectFileNames(2) .EQ. " ") THEN
        CALL SetLastMessage('Neither solute transport nor particle tracking file is defined !',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !--Open common transport data file
    CALL TransManagerFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN     
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (iStat .EQ. -1) RETURN
    ELSE
        CALL SetLastMessage('Transport common data file is not defined !',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !--Set up common data provider 
    CALL  Transport%RunCommonDataProvider(cAbsPathFileName,  AppGrid, Stratigraphy, TimeStep,  cWorkingDirectory, iStat)
    IF (iStat .EQ. -1) RETURN
    
    !--Create Solute Transport Type
    IF (Transport%lSolTransDefined) THEN
            CALL Transport%CreateSoluteTransport ( IsForInquiry, STProjectFileNames(1), cWorkingDirectory, AppGrid, Stratigraphy, TimeStep,   iStat)
            IF (iStat .EQ. -1) RETURN
    END IF
     
    !--Create Particle Tracking Type
    IF (Transport%lParTrackDefined) THEN
        Transport%LagrangianParticleTracking = CreateLagrangian(Transport%DataProvider, "ParticleTracking", IsForInquiry, STProjectFileNames(2), Transport%cSIMWorkingDirectory, AppGrid, Stratigraphy, TimeStep,   iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
        
    ! Transport is defined and if so is flow simulated
    IF (Transport%lSolTransDefined) THEN
        Transport%lDefined=.TRUE.
        IF (Transport%LagrangianSoluteTransport%DataProvider%lFlowSimulated) THEN
            Transport%lFlowSimulated=.TRUE.
        END IF
    END IF
         
    IF (Transport%lParTrackDefined  ) THEN
        Transport%lDefined=.TRUE.
        IF (Transport%LagrangianParticleTracking%DataProvider%lFlowSimulated) THEN
            Transport%lFlowSimulated=.TRUE.
        END IF
    END IF

  END SUBROUTINE  InitializeTransportManager
 
  
  ! -------------------------------------------------------------
  ! --- PROVIDE COMMON DATA TO ALL TRANSPORT TYPES
  ! -------------------------------------------------------------
  SUBROUTINE RunCommonDataProvider(This, cCommDataFileName, AppGrid, Stratigraphy, TimeStep,  cWorkingDirectory, iStat)
    CLASS(TransportManagerType), INTENT(INOUT) :: This
    TYPE(AppGridType), INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)         :: Stratigraphy
    TYPE(TimeStepType), INTENT(IN)             :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)                :: cWorkingDirectory , cCommDataFileName
    INTEGER, INTENT(OUT)                       :: iStat

    ! Initialize data service (loads shared datasets)
    CALL This%DataProvider%InitializeDataReader(cCommDataFileName, AppGrid, Stratigraphy, TimeStep,  cWorkingDirectory, iStat)
  
  END SUBROUTINE RunCommonDataProvider


  ! -------------------------------------------------------------
  ! --- CREATE LAGRANGIAN TRANSPORT TYPES
  ! -------------------------------------------------------------
  FUNCTION CreateLagrangian( DataProvider, LagrangianType, IsForInquiry, cFileName, cWorkingDirectory, AppGrid, Stratigraphy, TimeStep, iStat)   RESULT(Transport)
    CLASS(TransportMethodType), ALLOCATABLE      :: Transport
    CLASS(TransportDataReaderType), INTENT(INOUT):: DataProvider
    CHARACTER(*), INTENT(IN)                     :: LagrangianType, cFileName, cWorkingDirectory
    LOGICAL, INTENT(IN)                          :: IsForInquiry
    TYPE(AppGridType), INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType), INTENT(IN)               :: TimeStep
    INTEGER, INTENT(OUT)                         :: iStat
    
    iStat=0

    SELECT CASE (TRIM(ADJUSTL(UpperCase(LagrangianType))))  
        CASE ("SOLUTETRANSPORT")
            ALLOCATE(SoluteTransportLPType :: Transport)
            CALL Transport%New(DataProvider, IsForInquiry,  cFileName,  cWorkingDirectory, AppGrid, Stratigraphy, TimeStep,  iStat)
        
        CASE ("PARTICLETRACKING") 
            ALLOCATE(ParticleTrackingType :: Transport)
            CALL Transport%New(DataProvider, IsForInquiry, cFileName,  cWorkingDirectory, AppGrid, Stratigraphy, TimeStep,  iStat)
        
        CASE DEFAULT
            WRITE(*,*) "Unknown Lagrangian Type:", TRIM(LagrangianType)
            iStat = -1
    END SELECT
        
  END FUNCTION CreateLagrangian

  
  ! -------------------------------------------------------------
  ! --- CREATE SOLUTE TRANSPORT METHODS
  ! -------------------------------------------------------------
  SUBROUTINE CreateSoluteTransport (Transport,  IsForInquiry, cInFileName, cWorkingDirectory, AppGrid, Stratigraphy, TimeStep, iStat)  
    CLASS(TransportManagerType),  INTENT(INOUT)  :: Transport
    CHARACTER(*), INTENT(IN)                     :: cInFileName, cWorkingDirectory
    LOGICAL, INTENT(IN)                          :: IsForInquiry
    TYPE(AppGridType), INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType), INTENT(IN)               :: TimeStep
    INTEGER, INTENT(OUT)                         :: iStat
    
    !-Local variables
    INTEGER              :: NNodes, NLayers
    CHARACTER            :: cSolTransFile*1000
    TYPE(GenericFileType):: SolTransBaseFile
    
    NNodes    = AppGrid%GetNNodes()
    NLayers   = Stratigraphy%GetNLayers()
    
    iStat=0
    
    ALLOCATE( Transport%DataProvider%Nodes%LongDispersivity(NNodes,NLayers)       , &
              Transport%DataProvider%Nodes%InitialConcentration(NNodes,NLayers) )   
              Transport%DataProvider%Nodes%LongDispersivity  = 0.0d0
              Transport%DataProvider%Nodes%InitialConcentration  = 0.0d0
                   
    !--Open Solute Transport Base file
    CALL SolTransBaseFile%New(FileName=TRIM(cInFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Solute transport base file',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
                                      
    !--Read aquifer dispersivity data
    CALL Transport%DataProvider%ReadAquiferDispersivity( AppGrid, SolTransBaseFile, NLayers, cWorkingDirectory,  iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !--Read initial concentrations
    CALL Transport%DataProvider%ReadInitialConcentration( AppGrid, SolTransBaseFile, NLayers, cWorkingDirectory,  iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !--Read solute transport solution method 1=Lagrangian; 2=FEM
    CALL SolTransBaseFile%ReadData(Transport%iMethodSolTrans,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !--Read the name of the solute transport method data file
    CALL SolTransBaseFile%ReadData(cSolTransFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(cSolTransFile)
    cSolTransFile = ADJUSTL(StripTextUntilCharacter(cSolTransFile,'/'))
    
    SELECT CASE (Transport%iMethodSolTrans)
       CASE (1)  ! Instantiate Lagrangian Parcel Method
           Transport%LagrangianSoluteTransport=CreateLagrangian(Transport%DataProvider, "SoluteTransport", IsForInquiry, cSolTransFile, Transport%cSIMWorkingDirectory, AppGrid, Stratigraphy, TimeStep,   iStat)
           IF (iStat .EQ. -1) RETURN 
       CASE (2)  ! Instantiate FEM method
           
       CASE DEFAULT
           iStat = -1;
           RETURN   
    END SELECT
      
  END SUBROUTINE CreateSoluteTransport


  ! -------------------------------------------------------------
  ! --- SIMULATE ONE TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE SimulateOneTimeStep(Transport, AppGW, AppGrid, Stratigraphy, TimeStep,  iStat)
    CLASS(TransportManagerType),  INTENT(INOUT):: Transport
    TYPE(AppGWType), INTENT(IN)          :: AppGW
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    INTEGER,INTENT(OUT)                  :: iStat

    IF (Transport%lSolTransDefined) THEN
        SELECT CASE (Transport%iMethodSolTrans)
            CASE (1)  ! Simulate Lagrangian Parcel Method
                IF (Transport%LagrangianSoluteTransport%DataProvider%TimeStep%CurrentTimeStep .LE. Transport%LagrangianSoluteTransport%TimeStep%EndTime ) THEN	
                     CALL Transport%LagrangianSoluteTransport%SimulateOneTimeStep(AppGW, AppGrid, Stratigraphy, TimeStep,  iStat)
                ENDIF
                IF (iStat .EQ. -1) RETURN
            CASE (2)  ! FEM method
                
            CASE DEFAULT
                iStat = -1;
                RETURN   
        END SELECT
    END IF
        
    IF (Transport%lParTrackDefined) THEN
        ! Simulate Particle tracking
        IF (Transport%LagrangianParticleTracking%DataProvider%TimeStep%CurrentTimeStep .LE. Transport%LagrangianParticleTracking%TimeStep%EndTime) THEN	
            CALL Transport%LagrangianParticleTracking%SimulateOneTimeStep(AppGW,  AppGrid, Stratigraphy, TimeStep,  iStat)
        ENDIF     
        IF (iStat .EQ. -1) RETURN
    END IF
            
  END SUBROUTINE SimulateOneTimeStep
  

  ! -------------------------------------------------------------
  ! --- PRINT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(Transport, AppGW, AppGrid, Stratigraphy, TimeStep, iStat)
    CLASS(TransportManagerType),  INTENT(INOUT) :: Transport
    TYPE(AppGWType),INTENT(IN)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep
    !LOGICAL, INTENT(IN)                         :: lEndOfSimulation
    INTEGER,INTENT(OUT)                         :: iStat

    IF (Transport%lSolTransDefined) THEN
        SELECT CASE (Transport%iMethodSolTrans)
            CASE (1)  ! Print Results for Lagrangian Parcel Method
                CALL Transport%LagrangianSoluteTransport%PrintResults(AppGW, AppGrid, Stratigraphy, TimeStep, iStat)
            CASE (2)  ! Print Results for FEM method
                
            CASE DEFAULT
                iStat = -1;
                RETURN   
        END SELECT
     END IF
        
    IF (Transport%lParTrackDefined) THEN
        ! Print Results for Particle tracking
        CALL Transport%LagrangianParticleTracking%PrintResults(AppGW, AppGrid, Stratigraphy, TimeStep, iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
            
   END SUBROUTINE PrintResults


  ! -------------------------------------------------------------
  ! --- READ TRANSPORT OPTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadTransportMethods(InFile,  STProjectFileNames, iStat)
    TYPE(GenericFileType)       :: InFile  
    INTEGER,INTENT(OUT)         :: iStat
    CHARACTER(LEN=*),INTENT(OUT):: STProjectFileNames(:)

    !Local variables
    INTEGER               ::  indx, iLineCount

    iStat=0
  
    iLineCount=SIZE(STProjectFileNames)
    DO indx=1,iLineCount
        CALL InFile%ReadData(STProjectFileNames(indx),iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL CleanSpecialCharacters(STProjectFileNames(indx))
        STProjectFileNames(indx) = ADJUSTL(StripTextUntilCharacter(STProjectFileNames(indx),'/'))
    END DO
    
  END SUBROUTINE ReadTransportMethods
  

  ! -------------------------------------------------------------
  ! --- READ FLOW DATA IF NEEDED
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(Transport,AppGrid,Stratigraphy,TimeStep,iStat)
    CLASS(TransportManagerType),INTENT(INOUT) :: Transport
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)             :: TimeStep
    INTEGER,INTENT(OUT)                       :: iStat
    
    !If Flow is simulated along with Transport, return
    IF (Transport%lFlowSimulated) THEN
        iStat = 0
        RETURN
    END IF
    
    !Read flow data, if only Transport is being simulated
    CALL Transport%DataProvider%LoadTSData(AppGrid,Stratigraphy,TimeStep,iStat)

  END SUBROUTINE ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE TIME 
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceTime(Transport) 
    CLASS(TransportManagerType),INTENT(INOUT) :: Transport
    
    !Return if Transport is not simulated
    IF (.NOT. Transport%lDefined) RETURN
    
    ASSOCIATE (pTimeStep => Transport%DataProvider%TimeStep)
        !Increment time step
        pTimeStep%CurrentTimeStep = pTimeStep%CurrentTimeStep + 1
        
        !Increment simulation time and check if end-of-transport simulation has been reached
        pTimeStep%CurrentDateAndTime = IncrementTimeStamp(pTimeStep%CurrentDateAndTime,pTimeStep%DELTAT_InMinutes)
        IF (Transport%lSolTransDefined .AND. .NOT. Transport%lParTrackDefined) THEN
             IF (pTimeStep%CurrentDateAndTime .EQ. Transport%LagrangianSoluteTransport%TimeStep%EndDateAndTime ) &
                 Transport%DataProvider%lEndOfTransSimulation = .TRUE. 
        ELSEIF (Transport%lParTrackDefined .AND. .NOT. Transport%lSolTransDefined ) THEN
            IF (pTimeStep%CurrentDateAndTime .EQ. Transport%LagrangianParticleTracking%TimeStep%EndDateAndTime ) &
                 Transport%DataProvider%lEndOfTransSimulation = .TRUE. 
        ELSEIF (Transport%lSolTransDefined .AND. Transport%lParTrackDefined) THEN   
             IF (pTimeStep%CurrentDateAndTime .EQ. pTimeStep%EndDateAndTime)  &
                 Transport%DataProvider%lEndOfTransSimulation = .TRUE.  
        ENDIF
            
    END ASSOCIATE
    
  END SUBROUTINE AdvanceTime
  
  
  ! -------------------------------------------------------------
  ! --- IS TRANSPORT  SIMULATED (SOLUTE TRANSPORT OR PARTICLE TRACKING)
  ! -------------------------------------------------------------
  PURE FUNCTION IsTransportDefined(Transport) RESULT(lDefined)
    CLASS(TransportManagerType),INTENT(IN) :: Transport
    LOGICAL                                :: lDefined
    
    lDefined =Transport%lDefined  
    
  END FUNCTION IsTransportDefined
  
  
  ! -------------------------------------------------------------
  ! --- IS FLOW SIMULATED WITH TRANSPORT  
  ! -------------------------------------------------------------
  PURE FUNCTION IsFlowSimulated(Transport) RESULT(lFlowSimulated)
    CLASS(TransportManagerType),INTENT(IN) :: Transport
    LOGICAL                                :: lFlowSimulated
    
    lFlowSimulated = Transport%DataProvider%lFlowSimulated
    
  END FUNCTION IsFlowSimulated
  

  ! -------------------------------------------------------------
  ! --- CHECK IF END OF TRANSPORT SIMULATION
  ! -------------------------------------------------------------
  PURE FUNCTION IsEndOfSimulation(Transport) RESULT(lEndOfSimulation)
    CLASS(TransportManagerType),INTENT(IN) :: Transport
    LOGICAL                                :: lEndOfSimulation
    
        lEndOfSimulation = Transport%DataProvider%lEndOfTransSimulation
    
    END FUNCTION IsEndOfSimulation

 END MODULE  TransportManager