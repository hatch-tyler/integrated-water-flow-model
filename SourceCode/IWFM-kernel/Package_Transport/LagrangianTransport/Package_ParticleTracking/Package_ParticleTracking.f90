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
MODULE Package_ParticleTracking
    USE TransportDataReader  ,        ONLY: TransportDataReaderType         , &
                                            GWSPNodeType                    , &
                                            GWVelocityFieldType             , &
                                            StripLeadingDelimiters
    USE Class_TransportBase,          ONLY: TransportMethodType, f_IKIND
    USE Class_LagrangianTransport,    ONLY: LagrangianTransportType         , &
                                            ReleaseType
    USE LagrangianParcel_LinkedList , ONLY: LPLinkedListType                , &
                                            LPNodeType
    USE Package_AppGW               , ONLY: AppGWType           
    USE MessageLogger               , ONLY: LogMessage                      , &
                                            SetLastMessage                  , &
                                            EchoProgress                    , &
                                            IsLogFileDefined                , &
                                            MessageArray                    , &
                                            f_iFILE                         , &
                                            f_iMessage                      , &
                                            f_iFatal                        , &
                                            f_iWarn                         , &
                                            f_iInfo                         
  USE GeneralUtilities              , ONLY: StripTextUntilCharacter         , &
                                            IntToText                       , &
                                            ArrangeText                     , &
                                            UpperCase                       , &
                                            CleanSpecialCharacters          , &
                                            EstablishAbsolutePathFilename   , &
                                            ConvertID_To_Index              , &
                                            f_cLineFeed                     
  USE TimeSeriesUtilities           , ONLY: TimeStepType                    , &
                                            IncrementTimeStamp              , &
                                            IsTimeStampValid                , &
                                            IsTimeIntervalValid             , &
                                            f_iTimeStampLength              , &    
                                            StripTimeStamp                  , &
                                            NPeriods                        , &
                                            TimeStampToJulian               , &
                                            TimeIntervalConversion        
  USE IOInterface                   ,  ONLY: GenericFileType                , & 
                                            RealTSDataInFileType            , &
                                            PrepareTSDOutputFile            , &
                                            iGetFileType_FromName           , &
                                            f_iTXT                          , &
                                            f_iDSS                          , & 
                                            f_iUNKNOWN                          
  USE Package_Discretization        ,  ONLY: AppGridType                    , &
                                            StratigraphyType                , &
                                            GetValuesFromParametricGrid
  IMPLICIT NONE
  
    
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: ParticleTrackingType
  
  
  ! -------------------------------------------------------------
  !---PARTICLE TRACKING DATA TYPE
  ! -------------------------------------------------------------
  TYPE, EXTENDS(LagrangianTransportType) :: ParticleTrackingType
      LOGICAL                         :: lForward =.FALSE.
      LOGICAL                         :: lBackward=.FALSE.
      CHARACTER(8)                    :: TrackType
      TYPE(ReleaseType), ALLOCATABLE  :: Source(:)
      TYPE(GWSPNodeType)              :: Nodes
  CONTAINS
    PROCEDURE       :: New                 => NewParticleTracking
    PROCEDURE       :: SimulateOneTimeStep => SimulateOneParticleTrackingTimeStep
    PROCEDURE       :: PrintResults        =>PrintParticleTrackingResults
    PROCEDURE, PASS ::ReleaseLPs
  END TYPE  ParticleTrackingType
  
 
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 26
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_ParticleTracking::'

  
  
        
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
  ! --- INSTANTIATE PARTICLE TRACKING OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE NewParticleTracking(This, DataProvider, IsForInquiry,cFileName,cWorkingDirectory, AppGrid,Stratigraphy,TimeStep, iStat)
    CLASS(ParticleTrackingType),INTENT(INOUT)    :: This
    CLASS(TransportDataReaderType), INTENT(INOUT):: DataProvider
    LOGICAL,INTENT(IN)                           :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                  :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)                :: TimeStep
    INTEGER,INTENT(OUT)                          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'ParticleTrackingMainFile'
    INTEGER                     :: NNodes, NLayers,ErrorCode,NElements,iType
    INTEGER                     :: Nsrcs, iSrc, NumParcels  ! Total Particle Tracking time,  Time step skip for output, number of lagrangian parcels used in the simulation       
    INTEGER,ALLOCATABLE         :: Nodes(:)
    REAL(8),ALLOCATABLE         :: Coeff(:)
    REAL(8)                     :: JulianEnd, JulianEnd1, area_min, char_len
    TYPE(GenericFileType)       :: ParTrackFile
    CHARACTER                   :: cErrorMsg*500, Istring*5

    
    !--Initialize
    iStat     = 0
    NNodes    = AppGrid%GetNNodes()
    NElements = AppGrid%GetNElements()
    NLayers   = Stratigraphy%GetNLayers()
    
    !CALL GetElementAreas
    
    !--Return if cFileName is empty
    IF (cFileName .EQ. '') RETURN
    
    !--Inform user
    CALL EchoProgress('Instantiating particle tracking component...')
    
    !--Open Particle Tracking  parameter file
    CALL ParTrackFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Particle Tracking main parameter',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
         
    !--Read particle tracking time related data
    CALL ReadTrackingTimeData (ParTrackFile                     , &
                                This%TimeStep%RelSkip           , &      
                                This%TimeStep%OutSkip           , &
                                This%TimeStep%CurrentDateAndTime, &
                                This%TimeStep%EndDateAndTime    , &
                                iStat)
    IF (iStat .EQ. -1) THEN
        CALL SetLastMessage('Error Reading Particle Tracking Time Data !',f_iFatal,ThisProcedure)
        RETURN
    END IF
    
    !--Set the initial transport time step similar to flow time ste
    This%TimeStep%DeltaT=TimeStep%DeltaT
    
    !--Calculate characteristic length of the smallest element for stability check    
    area_min=MINVAL(AppGrid%AppElement(:)%Area)
    char_len=SQRT(area_min)
    This%dx=char_len
    This%dy=char_len
       
     !--Calculate the number of time steps of particle tracking simulation
    This%TimeStep%EndTime = NPeriods(TimeStep%DELTAT_InMinutes, This%TimeStep%CurrentDateAndTime, This%TimeStep%EndDateAndTime)
    IF (This%TimeStep%EndTime .LE. 0) THEN
        CALL ParTrackFile%Kill()
        RETURN
    END IF
       
    !--Read transport domain data
    CALL ReadDomainData(ParTrackFile , &
                        This%LwrLeft , &
                        This%UprRight, &
                        iStat        )
    IF (iStat .EQ. -1) RETURN
    
    !--Read total number of Lagrangian Parcels- NumParcels
    CALL ParTrackFile%ReadData(NumParcels,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NumParcels .LE. 0) THEN
        CALL ParTrackFile%Kill()
        CALL SetLastMessage('Error Reading Number of Particles !',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    This%Num_Parcels =NumParcels

    
    !--Find the element number that the point belongs to-interpolation part is not needed here only uses ContainedInElement function
    CALL AppGrid%FEInterpolate(This%LwrLeft(1),This%LwrLeft(2),  This%LLElem, Nodes,Coeff)   
    CALL AppGrid%FEInterpolate(This%UprRight(1),This%UprRight(2),  This%URElem,Nodes,Coeff)   
    
    !--Read particle tracking type, simulation type and file 
    CALL ParticleTrackingSimulationType( ParTrackFile,  This%TrackType, iStat) 
    IF (iStat .EQ. -1) RETURN 
    
    !--Read Time Integration Method 
    CALL ParTrackFile%ReadData(iType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (iType .LE. 0 .OR. iType .GE.4) THEN
        CALL ParTrackFile%Kill()
        CALL SetLastMessage('Error Reading Solution Type !',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    This%iSolnType =iType

     
    !--Attach common data
    CALL This%AttachDataProivider(DataProvider)

    !First check if solute transport is run together set the end time to latest time
    !First Convert time stamps to Julian dates for comparison

    JulianEnd  = TimeStampToJulian(This%DataProvider%TimeStep%EndDateAndTime) 
    JulianEnd1 = TimeStampToJulian(This%TimeStep%EndDateAndTime)

    IF(JulianEnd1> JulianEnd) THEN
       This%DataProvider%TimeStep%EndDateAndTime=This%TimeStep%EndDateAndTime
    ENDIF

    This%DataProvider%TimeStep%EndTime = MAX(This%TimeStep%EndTime,This%DataProvider%TimeStep%EndTime)


    !--Define forward or backward tracking 
    SELECT CASE(TRIM( This%TrackType))
        CASE("FORWARD")    
                This%lForward=.TRUE.
                This%DataProvider%lForwardTracking=.TRUE.
        CASE("BACKWARD")
                This%lBackward                      = .TRUE.
                This%DataProvider%lBackwardTracking = .TRUE.
                IF (This%DataProvider%lFlowSimulated .OR.  This%DataProvider%lSolTransDefined  ) THEN
                    IF (This%DataProvider%lFlowSimulated ) THEN
                            CALL SetLastMessage('Backward particle tracking is not possible with flow simulation. Use saved velocity and head files from a previous IWFM run-RunType Option 2  !',f_iFatal,ThisProcedure)
                     ELSE IF (This%DataProvider%lSolTransDefined  ) THEN
                            CALL SetLastMessage('Backward particle tracking is not possible with solute transport and should be run alone. Turn off solute transport  !',f_iFatal,ThisProcedure)
                     END IF
                    iStat = -1
                    RETURN    
                END IF

        CASE DEFAULT
                iStat = -1
                RETURN   
    END SELECT

    !--Read number of sources
    CALL ParTrackFile%ReadData(Nsrcs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF ( Nsrcs .GT. This%Num_Parcels ) THEN
        CALL ParTrackFile%Kill()
        CALL SetLastMessage('Number of sources (NUMSRC) is greater than number of parcels (NUMPAR) !',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    This%NumSources = Nsrcs     
    
    !--Allocate memory for sources 
    IF (.NOT. ALLOCATED(This%Source)) THEN
        ALLOCATE (This%Source(Nsrcs) ,  STAT=ErrorCode ,ERRMSG=cErrorMsg             )
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for Particle Tracking sources!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF    
    ENDIF
    
    !--Read Source Data
    CALL ReadSourceData( ParTrackFile, TimeStep, This%LwrLeft, This%UprRight , This%NumSources, This%Source ,iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !--Find the element of each source
    DO iSrc =1, This%NumSources
        CALL AppGrid%FEInterpolate(This%Source(iSrc)%Xorg,This%Source(iSrc)%Yorg,  This%Source(iSrc)%ElemID, Nodes,Coeff)   
        IF (This%Source(iSrc)%ElemID .LE. 0) THEN
              WRITE (Istring, '(I5)') iSrc
            CALL SetLastMessage('Coordinates for source  '//TRIM(iString)//' are outside the transport domain!',f_iFatal,ThisProcedure)
            iStat = -1
        RETURN
        END IF    

    END DO

    !--Initialize variables and arrays
    CALL InitializeParticleTracking(This, AppGrid, Stratigraphy, iStat ) 
    IF (iStat .EQ. -1) RETURN
    
    !--Open files for output
    CALL  OpenParticleTrackingFile(ParTrackFile,IsForInquiry, cWorkingDirectory, This,iStat )
    IF (iStat .EQ. -1) RETURN 
    
    !--If made it this far, set the flag
    This%lInitialized = .TRUE.

    !--Close particle tracking parameter file
    CALL ParTrackFile%Kill()

  END SUBROUTINE NewParticleTracking 
 
  
  ! -------------------------------------------------------------
  ! --- SIMULATE PARTICLE TRACKING
  ! --- *Note: Simulates particle tracking using Lagrangian particle method
  ! -------------------------------------------------------------
  SUBROUTINE SimulateOneParticleTrackingTimeStep(This, AppGW,  AppGrid,Stratigraphy,TimeStep,  iStat)
    CLASS(ParticleTrackingType), INTENT(INOUT):: This
    TYPE(AppGWType),INTENT(IN)              :: AppGW
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)       :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)           :: TimeStep
    INTEGER,INTENT(OUT)                     :: iStat


    !--Local Variables
    INTEGER           :: t_spill_end, iTrns
    REAL(KIND=f_IKIND):: CurrentTime
    CHARACTER         :: Text*500
    LOGICAL           :: ADJTIME
    
    iStat=0
    
    CurrentTime=TimeStep%CurrentTime  
    
    !--Release Parcels based on user input 
    t_spill_end=MAXVAL(This%Source(:)%Tend)  
     
    !--Read velocity from saved file for (1) backward particle tracking  (2) reruning solute transport or (3) rerunning forward particle tracking     
    IF (.NOT. This%DataProvider%lFlowSimulated ) THEN
        !-If  only transport is simulated using a saved IWFM veloicty file
        IF (This%lBackward ) THEN
            !-Backward particle tracking: reading velocity backwards starting from the end of flow simulation
            CALL EchoProgress('Simulating backward particle tracking using saved a velocity file...') 
            WRITE(Text,'(A)')  f_cLineFeed // '   *** SIMULATING BACKWARD PARTICLE TRACKING USING SAVED VELOCITY FILE'
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

        ELSE
            !-Forward particle tracking: reading velocity from the IWFM saved velocity file
            CALL EchoProgress('Simulating forward particle tracking using saved a velocity and head file...') 
            WRITE(Text,'(A)')  f_cLineFeed // '   *** SIMULATING FORWARD PARTICLE TRACKING USING SAVED VELOCITY & HEAD FILES'
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        ENDIF 
    ELSE 
        !-Transport is simulated with flow and flow is read at each time step from flow simulation
        CALL EchoProgress('Simulating particle tracking with flow simulation...') 
        WRITE(Text,'(A)') f_cLineFeed // '   *** SIMULATING PARTICLE TRACKING WITH FLOW SIMULATION'
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        
        IF (.NOT. This%DataProvider%lVelocityLoaded) THEN

            CALL AppGW%GetVelocities(This%DataProvider%Nodes%Velocity%Vx, This%DataProvider%Nodes%Velocity%Vy, This%DataProvider%Nodes%Velocity%Vz)

            CALL This%CalculatePoreVelocity( This%DataProvider%Nodes%Porosity , & 
                                    This%DataProvider%Nodes%Velocity%Vx       , &
                                    This%DataProvider%Nodes%Velocity%Vy       , &
                                    This%DataProvider%Nodes%Velocity%Vz        ) 
    
        END IF

    ENDIF 
    
    !--Pointer to simulated velocity data
    This%Vx=> This%DataProvider%Nodes%Velocity%Vx
    This%Vy=> This%DataProvider%Nodes%Velocity%Vy
    This%Vz=> This%DataProvider%Nodes%Velocity%Vz

   
    !--Calculate time step based on the CFL condition
    CALL This%SetTransportTimeStep("ParticleTracking", AppGW, This%Vx, This%Vy, This%Vz ,TimeStep, ADJTIME, iStat) 
    IF (iStat .NE. 0) RETURN
    
    !This%TimeStep%nsubTimeSteps=1
        

    WRITE(Text, ' (A, F8.3, 1X, A) ') '*  Simulated Particle Tracking Time Step                       :', This%TimeStep%DeltaT , TimeStep%UNIT(2:4)
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
    WRITE(Text, ' (A)') ''
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
    CALL LogMessage('!'//REPEAT('-',50)//'!',f_iMessage,'',f_iFILE) 


     
    !--Transport subtime loop
    DO iTrns=1, This%TimeStep%nsubTimeSteps

        IF (This%TimeStep%nsubTimeSteps>1) THEN
            CALL EchoProgress('    Particle Tracking sub time step: ' //TRIM(IntToText(TimeStep%CurrentTimeStep))//"-"// TRIM(IntToText(iTrns)) )
            CALL LogMessage('!--Particle Tracking sub time step: ' //TRIM(IntToText(TimeStep%CurrentTimeStep))//"-"// TRIM(IntToText(iTrns)),f_iMessage,'',f_iFILE) 
        ENDIF 

        !--Release particles
        IF   ( TimeStep%CurrentTimeStep ==1 .OR.  MOD(TimeStep%CurrentTimeStep, This%TimeStep%RelSkip)==0) THEN
            CALL ReleaseLPs (This, AppGrid, Stratigraphy, This%DataProvider%Nodes%Velocity ,TimeStep, iTrns, iStat) 
        ENDIF
    
       !--Transport particles
        CALL TransportLPs (This,AppGrid, Stratigraphy, This%DataProvider%Nodes%Velocity, iTrns, iStat) 
     
        CALL LogMessage('!'//REPEAT('-',50)//'!',f_iMessage,'',f_iFILE) 

    END DO
    

  END SUBROUTINE SimulateOneParticleTrackingTimeStep 
    

  ! -------------------------------------------------------------
  ! --- PRINT PARTICLE TRACKING SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintParticleTrackingResults(This, AppGW, AppGrid, Stratigraphy, TimeStep, iStat)
    CLASS(ParticleTrackingType), INTENT(INOUT)::This
    TYPE(AppGWType),INTENT(IN)                :: AppGW
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy    
    TYPE(TimeStepType),INTENT(IN)             :: TimeStep
    INTEGER,INTENT(OUT)                       :: iStat
    
    !--Local variables
    CHARACTER                  :: SimulationTime*21,Text*100
    TYPE (LPNodeType), POINTER :: current
    
    !-Initialize
    iStat = 0
    
    IF (TimeStep%DELTAT .EQ. This%TimeStep%OutSkip .OR. &
        MOD(TimeStep%CurrentTimeStep,  This%TimeStep%OutSkip) .EQ. 0 ) THEN
            
        !Create the simulation time
        IF (TimeStep%TrackTime) THEN
            SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
        ELSE
            WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
        END IF
  
        !Print particle locations at each output time step
        Current=> This%LPList%Head
        DO WHILE (ASSOCIATED (current))    
            WRITE (Text,'(A16, I6, 3(f20.2,1x), 5x, I6 )' )	TimeStep%CURRENTDATEANDTIME,  Current%ID, Current%xcord, Current%ycord, Current%zcord, Current%Igridz
            CALL This%ResultsFile%WriteData(TRIM(Text)) 
            Current=>Current%Next
        END DO
    END IF
  
  END SUBROUTINE PrintParticleTrackingResults
 
  

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
  ! --- KILL PARTICLE TRACKING OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(ParticleTracking)
    CLASS(ParticleTrackingType) :: ParticleTracking
    
    !Local variables
    TYPE(ParticleTrackingType) :: Dummy
    
    IF (.NOT. ParticleTracking%lDefined) RETURN    
    
    SELECT TYPE (ParticleTracking)
        TYPE IS (ParticleTrackingType)
            ParticleTracking = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  

  ! -------------------------------------------------------------
  ! --- IS PARTICLE TRACKING  SIMULATED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(ParticleTracking) RESULT(lDefined)
    CLASS(ParticleTrackingType),INTENT(IN) :: ParticleTracking
    LOGICAL                                :: lDefined
    
    lDefined = ParticleTracking%lDefined
    
  END FUNCTION IsDefined
  
  
  ! -------------------------------------------------------------
  ! --- READ BASIC DATA FOR PARTICLE TRACKING
  ! -------------------------------------------------------------
  SUBROUTINE ReadBasicData(InFile, Porosity , iStat)
    TYPE(GenericFileType)          :: InFile  
    INTEGER,INTENT(OUT)            :: iStat
    REAL(KIND=f_IKIND) ,INTENT(OUT):: porosity
  
    !-Read aquifer porosity
    CALL InFile%ReadData(porosity,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (porosity .EQ. 0) THEN
        CALL InFile%Kill()
        RETURN
    END IF   
         
  END SUBROUTINE ReadBasicData
    
  
  ! -------------------------------------------------------------
  ! --- READ TRACKING TIME DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTrackingTimeData(InFile, RelSkip, Iskip,Tstart, Tend, iStat)
    TYPE(GenericFileType)           :: InFile  
    INTEGER,INTENT(OUT)             :: RelSkip, Iskip,iStat
    CHARACTER                       :: cALine*1000
    
    !--Local variables
    CHARACTER(LEN=ModNameLen+47),PARAMETER:: ThisProcedure = ModName // 'Read Tracking Time Data'
    CHARACTER(LEN=f_iTimeStampLength)     :: Tstart, Tend
    
    iStat=0
    
    !Read total particle tracking  release frequency- Nt_Skip
    CALL InFile%ReadData(RelSkip,iStat);  IF (iStat .EQ. -1) RETURN
    IF (RelSkip .EQ. 0) THEN
        CALL InFile%Kill()
        RETURN
    END IF

    !Read total particle tracking  output skip- Nt_Skip
    CALL InFile%ReadData(Iskip,iStat);  IF (iStat .EQ. -1) RETURN
    IF (Iskip .EQ. 0) THEN
        CALL InFile%Kill()
        RETURN
    END IF
    
   !-Get the starting date of particle tracking simulation
    CALL InFile%ReadData(cALine,iStat);  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(cALine)
    cALine = ADJUSTL(cALine)
    cALine = cALine(1:f_iTimeStampLength)
    IF (IsTimeStampValid(cALine)) THEN
        Tstart = StripTimeStamp(cALine)
    ELSE
        CALL SetLastMessage('Simulation ending time should be in MM/DD/YYYY_hh:mm format!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !-Get the end date of particle tracking simulation
    CALL InFile%ReadData(cALine,iStat);  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(cALine)
    cALine = ADJUSTL(cALine)
    cALine = cALine(1:f_iTimeStampLength)
    IF (IsTimeStampValid(cALine)) THEN
        Tend = StripTimeStamp(cALine)
    ELSE
        CALL SetLastMessage('Simulation ending time should be in MM/DD/YYYY_hh:mm format!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF 

    
  END SUBROUTINE ReadTrackingTimeData 

  
  ! -------------------------------------------------------------
  ! --- READ TRANSPORT DOMAIN DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadDomainData(InFile, LL_XYZCord,UR_XYZCord, iStat)
    TYPE(GenericFileType)          :: InFile  
    INTEGER,INTENT(OUT)            :: iStat
    REAL(KIND=f_IKIND) ,INTENT(OUT):: LL_XYZCord(3), UR_XYZCord(3)
 
    !Local Variables
    REAL(KIND=f_IKIND) :: LLpoint(3), URpoint(3)
    
    iStat=0
 
    !Read bottom lower left corner of the solute transport domain-LwrLeft
    CALL InFile%ReadData(LLpoint,iStat);  IF (iStat .EQ. -1) RETURN
    LL_XYZCord=LLpoint

    !Read top right corner of the solute transport domain-LwrLeft
    CALL InFile%ReadData(URpoint,iStat);  IF (iStat .EQ. -1) RETURN
    UR_XYZCord=URpoint

  END SUBROUTINE ReadDomainData 
     
     
  ! ------------------------------------------------------------------------------
  ! --- READ PARTICLE TRACKING TYPE AND SIMULATION TYPE
  ! ------------------------------------------------------------------------------
  SUBROUTINE ParticleTrackingSimulationType(InFile, TrackType, iStat)
    TYPE(GenericFileType)   :: InFile  
    INTEGER, INTENT (OUT)   :: iStat
    CHARACTER(8),INTENT(OUT):: TrackType 
    
    !Local variables
    CHARACTER                    :: ALine*2000
     
    !Read particle tracking type-forward or backward
     CALL InFile%ReadData(ALine,iStat);  IF (iStat .EQ. -1) RETURN;  ALine = StripTextUntilCharacter(ALine,'/');  CALL CleanSpecialCharacters(ALine)
     TrackType=TRIM(ADJUSTL(ALine))
     IF (TrackType .NE. "FORWARD "  .AND.  TrackType .NE. "BACKWARD ") THEN
            CALL InFile%Kill()
            RETURN
     END IF
  END SUBROUTINE ParticleTrackingSimulationType
     
  
  ! -------------------------------------------------------------
  ! --- READ PARTICLE RELASE SOURCE DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadSourceData( InFile,TimeStep, LL_XYZCord,UR_XYZCord,Nsrcs, SRC, iStat)
    TYPE(GenericFileType)           :: InFile  
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: Nsrcs
    REAL(KIND=f_IKIND),INTENT(IN)   :: LL_XYZCord(3), UR_XYZCord(3)
    TYPE(ReleaseType),INTENT(INOUT) :: SRC(Nsrcs)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'ReadContSourceData'
    INTEGER                      :: I, pos1, pos2, pos3, pos4, pos5
    REAL(KIND=f_IKIND)           :: xcrd, ycrd, zcrd
    CHARACTER                    :: Istring*5, cALine*1000
    CHARACTER(LEN=f_iTimeStampLength):: start_date,  end_date, Token(5)

    !--Read source data including release location start and end time
    DO I=1,Nsrcs
        
        CALL InFile%ReadData(cALine,iStat) ;  IF (iStat .EQ. -1) RETURN
        
            CALL StripLeadingDelimiters(cALine)
            pos1 = SCAN(cALine, ' '//CHAR(9)//',')
            Token(1) = cALine(1:pos1-1)
            cALine = ADJUSTL(cALine(pos1+1:))
            CALL StripLeadingDelimiters(cALine)
    
            pos2 = SCAN(cALine, ' '//CHAR(9)//',')
            Token(2) = cALine(1:pos2-1)
            cALine = ADJUSTL(cALine(pos2+1:))
            CALL StripLeadingDelimiters(cALine)
    
            pos3 = SCAN(cALine, ' '//CHAR(9)//',')
            Token(3) = cALine(1:pos3-1)
            cALine = ADJUSTL(cALine(pos3+1:))
            CALL StripLeadingDelimiters(cALine)
    
            pos4 = SCAN(cALine, ' '//CHAR(9)//',')
            Token(4) = cALine(1:pos4-1)
            cALine = ADJUSTL(cALine(pos4+1:))
            CALL StripLeadingDelimiters(cALine)
            
            pos5 = SCAN(cALine, ' '//CHAR(9)//',')
            IF (pos5 .GT. 0) THEN
                Token(5) = cALine(1:pos5-1)
            ELSE
                Token(5) = TRIM(cALine)          ! fallback if no delimiter found
            END IF
        
        READ(Token(1), *)  xcrd
        READ(Token(2), *)  ycrd
        READ(Token(3), *)  zcrd
        
        SRC(I)%Xorg =xcrd
        SRC(I)%Yorg =ycrd
        SRC(I)%Zorg =zcrd
        SRC(I)%ID=I

        WRITE (Istring, '(I5)') I
        IF (SRC(I)%Xorg .LT. LL_XYZCord(1)  .OR.   SRC(I)%Xorg .GT. UR_XYZCord(1)) THEN
            CALL SetLastMessage('X coordinate for source '//TRIM(Istring)//' is outside the transport domain!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        ENDIF           
        IF (SRC(I)%Yorg .LT. LL_XYZCord(2)  .OR.   SRC(I)%Yorg .GT. UR_XYZCord(2)) THEN
            CALL SetLastMessage('Y coordinate for source '//TRIM(Istring)//' is outside the transport domain!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        ENDIF                      
        IF (SRC(I)%Xorg .LT. LL_XYZCord(1)  .OR.   SRC(I)%Xorg .GT. UR_XYZCord(1)) THEN
            CALL SetLastMessage('Z coordinate for source '//TRIM(Istring)//' is outside the transport domain!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        ENDIF
        
        !-Simulation starting time       
        start_date = TRIM(Token(4))
        IF (IsTimeStampValid(start_date)) THEN
            start_date = StripTimeStamp(start_date)
        ELSE
            CALL SetLastMessage('Simulation starting time should be in MM/DD/YYYY_hh:mm format!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

        !-Simulation ending time
        end_date   = TRIM(Token(5))
        IF (IsTimeStampValid(end_date)) THEN
            end_date = StripTimeStamp(end_date)
        ELSE
            CALL SetLastMessage('Simulation starting time should be in MM/DD/YYYY_hh:mm format!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
       !--Calculate the number of time steps of particle tracking simulation
       SRC(I)%Tstart =NPeriods( TimeStep%DELTAT_InMinutes,  TimeStep%CurrentDateAndTime, start_date)+1
       SRC(I)%Tend = NPeriods( TimeStep%DELTAT_InMinutes,  TimeStep%CurrentDateAndTime, end_date)
       
       SRC(I)%T_Spill  =  SRC(I)%Tend- SRC(I)%Tstart
    END DO
  END SUBROUTINE ReadSourceData
  
  
  ! -------------------------------------------------------------
  ! --- OPEN FILES FOR TRANSPORT OUTPUT
  ! -------------------------------------------------------------
  SUBROUTINE OpenParticleTrackingFile(InFile,IsForInquiry,cWorkingDirectory, ParTrack,iStat) 
    TYPE(GenericFileType),INTENT(INOUT)       :: InFile  
    CLASS(ParticleTrackingType),INTENT(INOUT) :: ParTrack
    LOGICAL,INTENT(IN)                        :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)               :: cWorkingDirectory
    INTEGER,INTENT(OUT)                       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'OpenParticleTrackingFile'
    INTEGER                      :: iNCol 
    CHARACTER                    :: ALine*2000,cUnitT
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    CHARACTER                    :: cFormatSpec*30,cTitleLines(1)*800,cHeaderFormat(9)*75,FPart(1)*32, cDataUnit(1)*10,cDataType(1)*10
    CHARACTER,ALLOCATABLE        :: cHeaders(:,:)*50, CPart(:)*32 


    !-Initialize
    iNCol=1
  
    !--Final simulation results output file
    CALL InFile%ReadData(ALine,iStat);  IF (iStat .EQ. -1) RETURN;  ALine = StripTextUntilCharacter(ALine,'/');  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (ParTrack%ResultsFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL ParTrack%ResultsFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='Final particle tracking output',iStat=iStat)  
        ELSE
            CALL ParTrack%ResultsFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='Final particle tracking output',iStat=iStat)  
        END IF
        IF (iStat .EQ. -1) RETURN
        IF (ParTrack%ResultsFile%iGetFileType() .NE. f_iTXT) THEN
            CALL SetLastMessage('End-of-simulation particle tracking output file must be a text file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
            CALL SetLastMessage('No particle tracking output file name is specified!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END IF
    
    !-Print-out format
    cFormatSpec = '(A16,'//TRIM(IntToText(iNCol))//'F14.3)'

    !-Title lines
    cTitleLines(1) = 'C' // REPEAT('-',100)
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C                 PARTICLE LOCATIONS AT EACH TIME STEP '
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C' // REPEAT('-',100)
    
    !-Column headers and format
    ALLOCATE(cHeaders(1,6)) 
      
    cHeaders(1,1) = '  TIME'  
    cHeaders(1,2) = '  PARTICLE_ID'
    cHeaders(1,3) = '  XCOORD'
    cHeaders(1,4) = '  YCOORD'
    cHeaders(1,5) = '  ZCOORD'
    cHeaders(1,6) = '  LAYER'
    
    cHeaderFormat='(A8, A20, A13, A19, A23, A15 )'


   
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(ParTrack%ResultsFile                        , &
                              NColumnsOfData          = iNCol             , &
                              NRowsOfData             = 1                 , &
                              OverwriteNColumnsOfData = .FALSE.           , &
                              FormatSpec              = TRIM(cFormatSpec) , &
                              Title                   = cTitleLines       , &
                              Header                  = cHeaders          , &
                              HeaderFormat            = cHeaderFormat     , &
                              PrintColumnNo           = .FALSE.           , &
                              DataUnit                = cDataUnit         , &
                              DataType                = cDataType         , &
                              CPart                   = CPart             , &
                              FPart                   = FPart             , &        
                              UnitT                   = cUnitT            , &
                              iStat                   = iStat             )
  
  END SUBROUTINE OpenParticleTrackingFile
  
  
  ! -------------------------------------------------------------
  ! --- INITIALIZE SOLUTE TRANSPORT VARIABLES
  ! -------------------------------------------------------------
  SUBROUTINE InitializeParticleTracking(ParTrack, AppGrid, Stratigraphy,  iStat ) 
    CLASS(ParticleTrackingType),INTENT(INOUT) :: ParTrack
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy    
    INTEGER,INTENT(OUT)                       :: iStat  
    
    !Local variables
    REAL(KIND=f_IKIND)             :: t_spill

    
    !Initialize
    iStat = 0

    !--Number of layers
    ParTrack%NumLyrs=Stratigraphy%NLayers

    !ParTrack%TimeStep%NumTimeSteps=INT (ParTrack%TimeStep%EndTime/ParTrack%TimeStep%DeltaT) !No of time steps 
    ParTrack%TimeStep%NumOutTimeSteps=INT (ParTrack%TimeStep%EndTime/(ParTrack%TimeStep%DeltaT*ParTrack%TimeStep%OutSkip)) !No of output time steps 
      
    !-- Compute water quality related data (aquifer elevations at elements and surrounding elements)
    IF (.NOT. ParTrack%lDefined) THEN
        CALL ParTrack%DomainConfigData(AppGrid,Stratigraphy)
    END IF
    
    !--End time of the spills
    t_spill=MAXVAL(ParTrack%Source(:)%T_Spill)
    
    !-Number of particles relased per source per time step-set it to one 
    ParTrack%Source(:)%npar_dt=1   

    !--Create a Linked List to store LPs
    CALL ParTrack%LPList%Initialize( )
  
  END SUBROUTINE InitializeParticleTracking
  
  
  ! -------------------------------------------------------------
  !--- SEARCH NEAREST ELEMENT GIVEN X AND Y COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE Nearest_Node(AppGrid, xcrd, ycrd, iNode)    
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    INTEGER,INTENT(OUT)            :: iNode
    REAL(KIND=f_IKIND),INTENT (IN) :: xcrd, ycrd   
  
    !Local variables
    INTEGER            :: I
    REAL(KIND=f_IKIND) :: dist_min, dist
  
    dist_min=1e10
    DO I =1, AppGrid%NNodes   
        !!--Check the distance between the cell center and the node
        dist=sqrt( (AppGrid%X(I)-xcrd)**2.+ (AppGrid%Y(I)-ycrd)**2. ) 
        IF (dist < dist_min) THEN
            dist_min=dist
            iNode=I
        ENDIF     
    END DO
    
  END SUBROUTINE Nearest_Node

        
  ! -------------------------------------------------------------
  ! --- RELEASE LAGRANGIAN PARTICLES 
  ! -------------------------------------------------------------
  SUBROUTINE ReleaseLPs(ParTrack, AppGrid,Stratigraphy, Velocity, TimeStep,iTrns, iStat) 
    CLASS(ParticleTrackingType),INTENT(INOUT)    :: ParTrack
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy 
    TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
    TYPE(TimeStepType),INTENT(IN)                :: TimeStep    
    INTEGER,INTENT(IN)                           :: iTrns
    INTEGER,INTENT(OUT)                          :: iStat  


    !-Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReleaseParticles'
    INTEGER                      :: k, ip, np ,ilyr , npar !(np=new parcel being added)
    INTEGER                      :: new_elem, CurrentTimeStep 
    REAL(KIND=f_IKIND)           :: dtsmall, fTimeIntrp
    REAL(KIND=f_IKIND)           :: CurrentTime
    CHARACTER                    :: Text*500
    TYPE (LPNodeType), POINTER   :: current
    LOGICAL                      :: found, Remove

    
    !Initialize
    iStat       =  0 
    ilyr        =  0
    CurrentTime =  TimeStep%CurrentTime
    CurrentTimeStep=TimeStep%CurrentTimeStep

    current     => ParTrack%LPList%Tail

    !-Find the tail position to add LPs
    IF (.NOT. ASSOCIATED (current)) THEN
        np=1
    ELSE
        np=ParTrack%LPList%Tail%ID+1
    ENDIF
    
    npar=0
    DO k=1, ParTrack%NumSources   
        IF (CurrentTimeStep >= ParTrack%Source(k)%Tstart   .AND. CurrentTimeStep <=ParTrack%Source(k)%Tend .AND. ParTrack%Num_Parcels >= npar) THEN
            DO ip=1,ParTrack%Source(k)%npar_dt   
                
                !-Add a LP at the tail of the link
                CALL  ParTrack%LPList%InsertLP (ParTrack%Source(k)%Xorg, ParTrack%Source(k)%Yorg, ParTrack%Source(k)%Zorg, ParTrack%Source(k)%ElemID , iStat )  
                IF (iStat .EQ. -1) THEN   
                    CALL SetLastMessage('Error inserting LP node ID '//TRIM(IntToText(np))//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                ENDIF
                 
                Current=>ParTrack%LPList%Tail
                
                !--For continous release figure out the small time step for each release
                dtsmall=ParTrack%TimeStep%DeltaT/float(ParTrack%Source(k)%npar_dt)*ip
                
                !--Calculate the layer number
                IF (ip==1) THEN
                    CALL ParTrack%Get_LP_Layer( Stratigraphy, Current%ID, Current%Elem, Current%zcord,  ilyr, iStat) 
                    IF(ilyr <= 0) THEN
                        CALL  ParTrack%LPList%RemoveLP ( np, Current , found, iStat )  ! after deleting the current node next node will be current
                        IF (np >=1) THEN
                            np=np-1
                        END IF
                        CYCLE
                    ENDIF
                    IF (iStat .EQ. -1) RETURN
                END IF
                Current%Igridz=ilyr
                
                !--Calculate the new parcel location due to advection 
                fTimeIntrp=0.5*dtsmall*ParTrack%TimeStep%DeltaT*FLOAT(iTrns)/TimeStep%DeltaT
                CALL ParTrack%DisplaceLPs (AppGrid  , &
                                 Stratigraphy      , &
                                 current%xcord     , &
                                 current%ycord     , &
                                 current%zcord     , &
                                 current%Igridz    , &
                                 current%Elem      , &
                                 Velocity          , &
                                 ParTrack%TimeStep%DeltaT , &
                                 iTrns               , &
                                 "PARTICLETRACKING" , &
                                 iStat); 
        
                IF (iStat.EQ.-1) THEN
                    CALL SetLastMessage('Displacement calculated for LP:  '//TRIM(IntToText(Current%ID))//' is wrong!',f_iFatal,ThisProcedure)
                    RETURN
                ENDIF 

                
                
                !-- Check if the LP has moved out of the domain-Apply boundary conditions
                CALL ParTrack%ApplyAbsorbBC (Current%Elem, Current%xcord,  Current%ycord, Current%zcord, Remove) 
                IF(Remove)THEN
                    CALL  ParTrack%LPList%RemoveLP ( np, Current , found, iStat )  ! after deleting the current node next node will be current
                    IF (np >=1) THEN
                        np=np-1
                    END IF
                    IF (iStat .EQ. -1) THEN   
                        CALL SetLastMessage('Error removing LP node ID '//TRIM(IntToText(np))//'!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    ENDIF
                    CYCLE
                ENDIF

                
                !-Update the layer number: If the parcel has moved to another layer figure out the new layer
                CALL ParTrack%Get_LP_Layer ( Stratigraphy,  Current%ID, Current%Elem, Current%zcord, ilyr, iStat)  
                IF (iStat .EQ. -1) RETURN
                Current%Igridz=ilyr
                   
                !--Update the element number: If the particle has moved to a new element figure out the new element number 
                CALL ParTrack%Get_LP_Elem (AppGrid, Current%Elem, Current%xcord,  Current%ycord, new_elem, found, iStat)
                IF ( found)  Current%Elem=new_elem
                
                np=np+1
                npar=npar+1
                
            END DO
        ENDIF
    END DO     
    
    WRITE(Text,'(A)') '       Number of particles released: ' // TRIM(IntToText(npar))
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
  
  END SUBROUTINE ReleaseLPs

  
  ! -------------------------------------------------------------
  ! --- TRANSPORT LAGRANGIAN PARTICLES CORRESPONDS TO RELEASES
  ! -------------------------------------------------------------
  SUBROUTINE TransportLPs(ParTrack, AppGrid,Stratigraphy, Velocity, iTrns, iStat) 
    CLASS(ParticleTrackingType),INTENT(INOUT)    :: ParTrack
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy   
    INTEGER,INTENT(IN)                           :: iTrns
    TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
    INTEGER,INTENT(OUT)                          :: iStat  


    !-Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'TransportParticles'
    INTEGER                      :: np , ilyr , npar !(np=new parcel being added)
    INTEGER                      :: new_elem
    CHARACTER                    :: Text*500
    TYPE(LPNodeType), POINTER    :: current
    LOGICAL                      :: found, Remove
    

    !Initialize
    iStat        =  0 
    ilyr         =  0
    
    current      => ParTrack%LPList%Head
    
    npar=0
    DO WHILE (ASSOCIATED (current))    
    
        np=current%ID     
        
        !-- Check if the LP has moved out of the domain-Apply boundary conditions
        CALL ParTrack%ApplyAbsorbBC ( current%Elem, current%xcord,  current%ycord, current%zcord, Remove) 
        IF(Remove)THEN
            CALL  ParTrack%LPList%RemoveLP ( np, Current , found, iStat )  ! after deleting the current node next node will be current
            IF (np >=1) THEN
                np=np-1
            END IF
            IF (iStat .EQ. -1) THEN   
                CALL SetLastMessage('Error removing LP node ID '//TRIM(IntToText(np))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            ENDIF
            CYCLE
        ENDIF
        
        !--Calculate the new parcel location due to advection
        !fTimeIntrp=0.5*ParTrack%TimeStep%DeltaT*FLOAT(iTrns)/TimeStep%DeltaT
        CALL ParTrack%DisplaceLPs (AppGrid  , &
                                 Stratigraphy      , &
                                 current%xcord     , &
                                 current%ycord     , &
                                 current%zcord     , &
                                 current%Igridz    , &
                                 current%Elem      , &
                                 Velocity          , &
                                 ParTrack%TimeStep%DeltaT , &
                                 iTrns             , &
                                 "PARTICLETRACKING" , &
                                 iStat); 
        
        IF (iStat.EQ.-1) THEN
            CALL SetLastMessage('Displacement calculated for LP:  '//TRIM(IntToText(Current%ID))//' is wrong!',f_iFatal,ThisProcedure)
            RETURN
        ENDIF 
       
        !-- Check if the LP has moved out of the domain-Apply boundary conditions
        CALL ParTrack%ApplyAbsorbBC ( current%Elem, current%xcord,  current%ycord, current%zcord, Remove) 
        IF(Remove)THEN
            CALL  ParTrack%LPList%RemoveLP ( np, Current , found, iStat )  ! after deleting the current node next node will be current
            IF (np >=1) THEN
                np=np-1
            END IF
            IF (iStat .EQ. -1) THEN   
                CALL SetLastMessage('Error removing LP node ID '//TRIM(IntToText(np))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            ENDIF
           CYCLE
        ENDIF
        !--Update the element number: If the particle has moved to a new element figure out the new element number 
        CALL ParTrack%Get_LP_Elem (AppGrid, Current%Elem, Current%xcord,  Current%ycord, new_elem, found, iStat)
        IF ( found)  Current%Elem=new_elem

        
        !-Update the layer number: If the parcel has moved to another layer figure out the new layer
        CALL ParTrack%Get_LP_Layer( Stratigraphy, Current%ID, Current%Elem, Current%zcord, ilyr, iStat)  
        IF (iStat .EQ. -1) RETURN
        Current%Igridz=ilyr
        
        !--Move to the next LP in the linked list
        current=>current%Next
        npar=npar+1
            
    END DO
    
    WRITE(Text, '(A,I12)') '       Number of particles advected: ' // TRIM(IntToText(npar)) 
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
    WRITE(Text,'(A)')      '       Number of particles removed : ' // TRIM(IntToText(ParTrack%LPList%NumParDelete)) 
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

  END SUBROUTINE TransportLPs
  
END MODULE Package_ParticleTracking