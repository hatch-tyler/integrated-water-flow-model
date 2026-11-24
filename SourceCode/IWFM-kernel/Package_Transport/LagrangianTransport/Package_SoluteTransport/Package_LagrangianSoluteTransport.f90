!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2026  
!  State of California, Department of Water Resources 
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
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
MODULE Package_LagrangianSolTrans 
    !$ USE OMP_LIB
    USE TransportDataReader  ,        ONLY: TransportDataReaderType         , &
                                            GWSPNodeType                    , &
                                            GWVelocityFieldType             , &
                                            StripLeadingDelimiters
    USE Class_TransportBase,          ONLY: TransportMethodType             , &
                                            f_IKIND
    USE Class_LagrangianTransport,    ONLY: LagrangianTransportType         , &
                                            ReleaseType, SourceType
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
                                            IsTimeIntervalValid             , &
                                            TimeIntervalConversion          , &
                                            IsTimeStampValid                , &
                                            IsTimeIntervalValid             , &
                                            f_iTimeStampLength              , &    
                                            StripTimeStamp                  , &
                                            TimeStampToJulian               , &
                                            NPeriods                    
  USE IOInterface                   , ONLY: GenericFileType                 , & 
                                            RealTSDataInFileType            , &
                                            PrepareTSDOutputFile            , &
                                            iGetFileType_FromName           , &
                                            f_iTXT                          , &
                                            f_iDSS                          , & 
                                            f_iUNKNOWN                          
  USE Package_Discretization        , ONLY: AppGridType                     , &
                                            StratigraphyType                , &
                                            GetValuesFromParametricGrid
  USE Class_PumpsAtElem             , ONLY: PumpsAtElemType                 , &
                                            PumpsAtElem_New                                                     
  IMPLICIT NONE
  
    
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: SoluteTransportLPType
    
  
  ! -------------------------------------------------------------
  ! --- GRID TYPE FOR CONCENTRATION CALCULATIONS
  ! -------------------------------------------------------------    
  TYPE ConcGridType    
      INTEGER            :: Npar= 0               !Number of parcels in each model grid
      INTEGER            :: iNode, iElem
      INTEGER            :: nParRemDT=0           !Number of parcels to be removed based on pumped volumes 
      REAL(KIND=f_IKIND) :: Xcord
      REAL(KIND=f_IKIND) :: Ycord
      REAL(KIND=f_IKIND) :: Zcord
      REAL(KIND=f_IKIND) :: Thickness
      REAL(KIND=f_IKIND) :: InitialConcentration =0.d0
      REAL(KIND=f_IKIND) :: Porosity= 0.d0
      REAL(KIND=f_IKIND) :: LongDispersivity = 0.d0
      REAL(KIND=f_IKIND) :: ResidualMass = 0.d0
  END TYPE ConcGridType


  ! -------------------------------------------------------------
  !---SOLUTE TRANSPORT DATA TYPE
  ! -------------------------------------------------------------
  TYPE, EXTENDS(LagrangianTransportType) :: SoluteTransportLPType
      INTEGER                            :: NumXgrids                    ! Number of X grids
      INTEGER                            :: NumYgrids                    ! Number of Y grids
      REAL(KIND=f_IKIND)                 :: TSpill_End                   ! Last time step of the discharges
      REAL(KIND=f_IKIND)                 :: MassLP                       ! Mass of Lagrangian parcel
      REAL(KIND=f_IKIND)                 :: TotalMass                    ! Total mass released to the groundwater system 
      REAL(KIND=f_IKIND)                 :: ConcConvFact                 ! Conversion factor for concentration 
      REAL(KIND=f_IKIND)                 :: DispCoeff_xx_max             ! Dispersion Coefficients
      REAL(KIND=f_IKIND)                 :: DispCoeff_yy_max             ! Dispersion Coefficients
      TYPE(ConcGridType),ALLOCATABLE     :: Grid(:,:,:)                  ! Grid information for calculating concentration
      TYPE(SourceType), ALLOCATABLE      :: Source(:)                    ! Source information
  CONTAINS
      PROCEDURE :: New                 => NewSoluteTransport
      PROCEDURE :: SimulateOneTimeStep => SimulateOneSoluteTransportTimeStep
      PROCEDURE :: PrintResults        =>PrintSoluteTransportResults
      PROCEDURE :: GetCurrentElement
      PROCEDURE :: Count_LPs
      PROCEDURE :: CalcNumParReleaseSubDT  
      PROCEDURE :: ApplySinks
  END TYPE  SoluteTransportLPType
  
 
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 26
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_SoluteTransportLP::'

  
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
  ! --- INSTANTIATE SOLUTE TRANSPORT  OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE NewSoluteTransport(This, DataProvider, IsForInquiry,cFileName, cWorkingDirectory, AppGrid,Stratigraphy,TimeStep, iStat)
    CLASS(SoluteTransportLPType),INTENT(INOUT)    :: This
    CLASS(TransportDataReaderType), INTENT(INOUT) :: DataProvider
    LOGICAL,INTENT(IN)                            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                   :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)                  :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)             :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)                 :: TimeStep
    INTEGER,INTENT(OUT)                           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23)     :: ThisProcedure = ModName // 'SoluteTransportMainFile'
    INTEGER                          :: NNodes, NLayers,ErrorCode,NElements
    INTEGER                          :: Nsrcs, iSrc, NumParcels   ! Total solute trnasport  time,  Time step skip for output, number of lagrangian parcels used in the simulation       
    INTEGER,ALLOCATABLE              :: Nodes(:)
    REAL(8),ALLOCATABLE              :: Coeff(:)
    REAL(KIND=f_IKIND)               :: ConcConvFact
    TYPE(GenericFileType)            :: SolTranspFile
    CHARACTER                        :: cErrorMsg*500, text

    
    !Initialize
    iStat     = 0
    NNodes    = AppGrid%GetNNodes()
    NElements = AppGrid%GetNElements()
    NLayers   = Stratigraphy%GetNLayers()
    
    !--Initialize Random Number Generator
    CALL This%InitializeRandomNumberGenerator ( ) 

    !Return if cFileName is empty
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating solute transport component...')
    
    !Open Soute Transport Module
    CALL SolTranspFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Solute transport main parameter',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !--Read simulation time related data
    CALL ReadTrackingTimeData (SolTranspFile                         ,&
                                    This%TimeStep%OutSkip            ,&
                                    This%TimeStep%CurrentDateAndTime ,&
                                    This%TimeStep%EndDateAndTime     ,&
                                    iStat); 
    IF (iStat .EQ. -1) THEN
        CALL SetLastMessage('Error Reading Transport Time Data !',f_iFatal,ThisProcedure)
        RETURN
    END IF
    
    !--Set the initial transport time step similar to flow time ste
    This%TimeStep%DeltaT=TimeStep%DeltaT

    !--Calculate the number of time steps of solute transport simulation
    This%TimeStep%EndTime = NPeriods(TimeStep%DELTAT_InMinutes, This%TimeStep%CurrentDateAndTime, This%TimeStep%EndDateAndTime)
 

    IF (This%TimeStep%EndTime .LE. 0) THEN
        CALL SetLastMessage('Solute transport end time is not defined!',f_iFatal,ThisProcedure)
        CALL SolTranspFile%Kill()
        RETURN
    END IF

    !--Read transport domain data
    CALL ReadDomainData(SolTranspFile     , &
                            This%DX       , &
                            This%DY       , &
                            This%LwrLeft  , &
                            This%UprRight , &
                            iStat)
    IF (iStat .EQ. -1)  THEN
        CALL SetLastMessage('Error Reading Transport Domain Data !',f_iFatal,ThisProcedure)
        RETURN
    END IF

    !--Find the element number that the point belongs to-interpolation part is not needed here only uses ContainedInElement function
    CALL AppGrid%FEInterpolate(This%LwrLeft(1),This%LwrLeft(2),  This%LLElem, Nodes,Coeff) 
    IF (This%LLElem .LE. 0) THEN
            CALL SetLastMessage('Lower limit of the transport domain is outside the IWFM domain !',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END IF 
    CALL AppGrid%FEInterpolate(This%UprRight(1),This%UprRight(2),  This%URElem,Nodes,Coeff)   
    IF (This%URElem .LE. 0) THEN
            CALL SetLastMessage('Upper limit is of the transport domain is outside the IWFM domain !',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END IF 
    
    !Read total number of Lagrangian Parcels- NumParcels
    CALL SolTranspFile%ReadData(NumParcels,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NumParcels .LE. 0) THEN
        CALL SetLastMessage('Number of Largangian parcels  is not defined !',f_iFatal,ThisProcedure)
        iStat = -1
        CALL SolTranspFile%Kill()
        RETURN
    END IF
    This%Num_Parcels =NumParcels
    
    !--Read conversion factor for concentration
    CALL SolTranspFile%ReadData(ConcConvFact,iStat) 
    IF (ConcConvFact .LE. 0 .OR. iStat .EQ. -1) THEN
           CALL SetLastMessage('Concentration conversion factor not defined or read error!',f_iFatal,ThisProcedure)
           CALL SolTranspFile%Kill()
           RETURN
    END IF
    This%ConcConvFact=ConcConvFact

    !--Read number of sources
    CALL SolTranspFile%ReadData(Nsrcs,iStat)  
    IF ( Nsrcs .GT. This%Num_Parcels  .OR. iStat .EQ. -1) THEN
        CALL SolTranspFile%Kill()
        CALL SetLastMessage('Number of sources (NUMSRC) is greater than number of parcels (NUMPAR) !',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    This%NumSources = Nsrcs     
    
    !-Allocate memory for sources 
    IF (.NOT. ALLOCATED(This%Source)) THEN
        ALLOCATE (This%Source(Nsrcs) ,  STAT=ErrorCode ,ERRMSG=cErrorMsg )
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for solute transport sources!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF    
    ENDIF
    
    !--Read Source Data
    CALL ReadSourceData( SolTranspFile, TimeStep, This%LwrLeft, This%UprRight , This%NumSources, This%Source ,iStat) 
    IF (iStat .EQ. -1) RETURN

    !--Find the element of each source
    DO iSrc =1, This%NumSources
        CALL AppGrid%FEInterpolate(This%Source(iSrc)%Xorg,This%Source(iSrc)%Yorg,  This%Source(iSrc)%ElemID, Nodes,Coeff)   
        IF (This%Source(iSrc)%ElemID .LE. 0) THEN
                WRITE (text, '(I10)')  iSrc
                CALL SetLastMessage('Location of the source '//text//' is outside the IWFM domain !',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END IF 
    END DO
    
    !--Attach common data
    CALL This%AttachDataProivider(DataProvider)
    This%DataProvider%TimeStep%EndDateAndTime=This%TimeStep%EndDateAndTime       
    This%DataProvider%TimeStep%EndTime = This%TimeStep%EndTime
    
    !--Initialize variables and arrays
    CALL InitializeSolTransVariablesArrays(This,AppGrid, Stratigraphy, iStat ) 
    IF (iStat .EQ. -1) RETURN 

    !--Open files for output
    CALL OpenSoluteTranspOutputFiles(This, SolTranspFile,IsForInquiry, cWorkingDirectory, iStat )
    IF (iStat .EQ. -1) RETURN 

    !--If made it this far, set the flag
    This%lInitialized = .TRUE.
   
    !Close particle tracking parameter file
    CALL SolTranspFile%Kill()

  END SUBROUTINE NewSoluteTransport 
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SOLUTE TRANSPORT USING LAGRANGIAN METHOD
  ! --- *Note: Simulates solute transport using Lagrangian particle method
  ! -------------------------------------------------------------
  SUBROUTINE SimulateOneSoluteTransportTimeStep(This, AppGW, AppGrid,Stratigraphy,TimeStep,  iStat)
    CLASS(SoluteTransportLPType), INTENT(INOUT) :: This
    TYPE(AppGWType),INTENT(IN)                  :: AppGW
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep
    INTEGER,INTENT(OUT)                         :: iStat

    !Local Variables
    INTEGER                                     :: I, iTrns, t_spill_end, NNodes, NLayers, tot_npar_released
    REAL(KIND=f_IKIND)                          :: CurrentTime
    REAL(KIND=f_IKIND), ALLOCATABLE, TARGET     :: GWHeads(:,:)
    CHARACTER                                   :: Text*500
    LOGICAL                                     :: ADJTIME  

        !Initialize
        iStat=0
               
        NNodes     = AppGrid%NNodes
        NLayers    =Stratigraphy%NLayers
        
        ALLOCATE (GWHeads(NNodes, NLayers)) 
  
        CurrentTime=TimeStep%CurrentTime  
        
        !--Release Parcels based on user input 
        t_spill_end=MAXVAL(This%Source(:)%Tend)  
     
        !--Read velocity from saved file for (1)  reruning solute transport or (2) rerunning forward particle tracking     
        IF (.NOT.  This%DataProvider%lFlowSimulated) THEN
            !-If only transport is simulated using a saved IWFM veloicty file
                CALL EchoProgress('Simulating Lagrangian Parcel Solute Transport using saved velocity and head files...') 
                WRITE(Text,'(A)')  f_cLineFeed // '   *** SIMULATING LAGRANGIAN PARCEL SOLUTE TRANSPORT USING SAVED VELOCITY & HEAD FILES'
                CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
                

                This%DataProvider%lVelocityLoaded=.TRUE.

                !--Pointer to simulated heads
                This%GWHead=> This%DataProvider%Nodes%GWHead
        
       ELSE 
            !-Transport is simulated with flow and flow is read at each time step from flow simulation
            CALL EchoProgress('SimulatingLagrangian Parcel Solute Transport with flow simulation...') 
            WRITE(Text,'(A)') f_cLineFeed // '   *** SIMULATING LAGRANGIAN PARCEL SOLUTE TRANSPORT WITH FLOW SIMULATION'
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
            
            
            CALL AppGW%GetVelocities(This%DataProvider%Nodes%Velocity%Vx, This%DataProvider%Nodes%Velocity%Vy, This%DataProvider%Nodes%Velocity%Vz)
            
            CALL This%CalculatePoreVelocity( This%DataProvider%Nodes%Porosity , & 
                                    This%DataProvider%Nodes%Velocity%Vx       , &
                                    This%DataProvider%Nodes%Velocity%Vy       , &
                                    This%DataProvider%Nodes%Velocity%Vz         ) 
    
            This%DataProvider%lVelocityLoaded=.TRUE.

            CALL AppGW%GetHeads_All(.FALSE.,GWHeads)
            This%GWHead=>GWHeads(:,1) 
              
       ENDIF 
       
         
        !--Pointer to simulated velocity data
        This%Vx=> This%DataProvider%Nodes%Velocity%Vx
        This%Vy=> This%DataProvider%Nodes%Velocity%Vy
        This%Vz=> This%DataProvider%Nodes%Velocity%Vz

        !--Print initial release conditions
        tot_npar_released=0
        IF (TimeStep%CurrentTimeStep==1  ) THEN
            DO I=1,This%NumSources                        
                tot_npar_released=tot_npar_released+ This%Source(I)%npar_dt*(This%Source(I)%T_spill/TimeStep%DeltaT)
            ENDDO  

            WRITE(Text, ' (A) ') ''
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)  
            WRITE(Text, '(A, I12)')     '*  Total Number of parcels used in the simulation is           :',tot_npar_released
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)
            WRITE(Text, '(A, F12.3, A)') '*  Mass of a parcel is                                         :' ,This%MassLP,' kg'
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)
        
        ENDIF
    
        !--Calculate time step based on the CFL condition
        CALL This%SetTransportTimeStep("SoluteTransport", AppGW, This%Vx, This%Vy, This%Vz ,TimeStep, ADJTIME, iStat) 
        IF (ADJTIME) CALL This%CalcNumParReleaseSubDT( TimeStep, iStat) 
        IF (iStat .NE. 0) RETURN
        

        WRITE(Text, ' (A, F8.3, 1X, A) ') '*  Simulated Solute Transport Time Step                        :', This%TimeStep%DeltaT , TimeStep%UNIT(2:4)
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        WRITE(Text, ' (A)') ''
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        CALL LogMessage('!'//REPEAT('-',50)//'!',f_iMessage,'',f_iFILE) 


        !--Transport subtime loop
        DO iTrns=1, This%TimeStep%nsubTimeSteps

            IF (This%TimeStep%nsubTimeSteps>1) THEN
                CALL EchoProgress('    Solute Transport sub time step: ' //TRIM(IntToText(TimeStep%CurrentTimeStep))//"-"// TRIM(IntToText(iTrns)) )
                CALL LogMessage('!--Solute Transport sub time step: ' //TRIM(IntToText(TimeStep%CurrentTimeStep))//"-"// TRIM(IntToText(iTrns)),f_iMessage,'',f_iFILE) 
            ENDIF 

            !--Release particles
            IF ( TimeStep%CurrentTimeStep <= t_spill_end )  THEN
                CALL ReleaseLPs (This, AppGrid, Stratigraphy, This%DataProvider%Nodes%Velocity,TimeStep, iTrns, iStat) 
            ENDIF
    
            !--Transport particles
            CALL TransportLPs (This, AppGrid, Stratigraphy, This%DataProvider%Nodes%Velocity , TimeStep, iTrns, iStat) 
                 
            CALL LogMessage('!'//REPEAT('-',50)//'!',f_iMessage,'',f_iFILE) 

        END DO
        
        !--Apply sink terms-remove parcels based on the pumped volumes if only flow is simulated
        IF (This%DataProvider%lFlowSimulated ) THEN
            CALL ApplySinks (This, AppGW, AppGrid, Stratigraphy, iStat) 
        ENDIF
         
        WRITE(Text, '(A,I12)') '       Number of parcels advected: ' // TRIM(IntToText(This%LPList%TotNumParcels)) 
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

        WRITE(Text, '(A,I12)') '       Number of parcels removed : ' // TRIM(IntToText(This%LPList%NumParDelete)) 
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        
        WRITE(Text, '(A,I12)') '       Total number of parcels   : ' // TRIM(IntToText(This%LPList%TotNumParcels+This%LPList%NumParDelete)) 
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

       
  END SUBROUTINE SimulateOneSoluteTransportTimeStep 
    

  ! -------------------------------------------------------------
  ! --- OPEN FILES FOR TRANSPORT OUTPUT
  ! -------------------------------------------------------------
  SUBROUTINE OpenSoluteTranspOutputFiles(SolTrnsp, InFile,IsForInquiry, cWorkingDirectory, iStat) 
    CLASS(SoluteTransportLPType), INTENT(INOUT) :: SolTrnsp
    TYPE(GenericFileType), INTENT  (INOUT)      :: InFile  
    LOGICAL,INTENT(IN)                          :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                 :: cWorkingDirectory
    INTEGER,INTENT(OUT)                         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+27) :: ThisProcedure = ModName // 'OpenSoluteTranspOutputFiles'
    INTEGER                      :: ix, iy, iz
    INTEGER                      :: iNCol, indx  
    REAL(KIND=f_IKIND)           :: xcrd, ycrd, zcrd
    CHARACTER                    :: ALine*2000,char_xcrd*20, char_ycrd*20, char_zcrd*20,cUnitT, char_row*20, char_col*20, char_lay*20
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    CHARACTER                    :: cFormatSpec*30,cTitleLines(1)*800,cHeaderFormat(9)*75,FPart(1)*32, cDataUnit(1)*10,cDataType(1)*10
    CHARACTER,ALLOCATABLE        :: cHeaders(:,:)*50,CPart(:)*32 


    !-Initialize
    iNCol=SolTrnsp%NumXgrids*SolTrnsp%NumYgrids*SolTrnsp%NumLyrs
  
    !--Final simulation results output file
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (SolTrnsp%ResultsFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL SolTrnsp%ResultsFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='Final solute transport output',iStat=iStat)  
        ELSE
            CALL SolTrnsp%ResultsFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='Final solute transport output',iStat=iStat)  
        END IF
        IF (iStat .EQ. -1) RETURN
        IF (SolTrnsp%ResultsFile%iGetFileType() .NE. f_iTXT) THEN
            CALL SetLastMessage('End-of-simulation solute transport output file must be a text file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL SetLastMessage('No solute transport output file name is specified!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
        
    !-Print-out format
    cFormatSpec = '(A16,'//TRIM(IntToText(iNCol))//'F14.3)'

    !-Title lines
    cTitleLines(1) = 'C' // REPEAT('-',100)
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C                 CONCENTRATIONS AT SOLUTE TRANSPORT GRIDS'
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C  (UNIT=As determined by the input unit and the conversion factor-FACTCONC)'
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C' // REPEAT('-',100)
    
    !-Column headers and format
    ALLOCATE (cHeaders(7,1+iNCol))
      
    cHeaders(1,1) = '  XCOORD'  
    cHeaders(2,1) = '  YCOORD'
    cHeaders(3,1) = '  ZCOORD'
    cHeaders(4,1) = '  ROW'
    cHeaders(5,1) = '  COLUMN'
    cHeaders(6,1) = '  LAYER'    
    cHeaders(7,1) = '  INDEX'    
    
    indx=2
    DO iz=1, SolTrnsp%NumLyrs
      DO  ix=1, SolTrnsp%NumXgrids	
        DO  iy=1, SolTrnsp%NumYgrids
            xcrd= SolTrnsp%Grid(ix,iy,iz)%Xcord      
            ycrd=SolTrnsp%Grid(ix,iy,iz)%Ycord     
            zcrd=SolTrnsp%Grid(ix,iy,iz)%Zcord
           
             WRITE (char_xcrd, '(F12.2)') xcrd   
             WRITE (char_ycrd, '(F12.2)') ycrd  
             WRITE (char_zcrd, '(F12.2)') zcrd  
             WRITE (char_row, '(I10)') iy
             WRITE (char_col, '(I10)') ix
             WRITE (char_lay, '(I10)') iz

            cHeaders(1, indx)=char_xcrd
            cHeaders(2, indx)=char_ycrd
            cHeaders(3, indx)=char_zcrd
            cHeaders(4, indx)=char_col
            cHeaders(5, indx)=char_row
            cHeaders(6, indx)=char_lay
            
            indx=indx+1
            
        ENDDO
      ENDDO
    ENDDO
    

    cHeaders(7,2:)   = [(TRIM(IntTotext(indx)),indx=1,iNCol)]
    cHeaderFormat(1) = '(A9,8X,' // TRIM(IntToText(iNCol)) // 'A14)' 
    cHeaderFormat(2) = '(A9,8X,' // TRIM(IntToText(iNCol)) // 'A14)'
    cHeaderFormat(3) = '(A9,8X,' // TRIM(IntToText(iNCol)) // 'A14)'
    cHeaderFormat(4) = '(A6,11X,' // TRIM(IntToText(iNCol+1)) // 'A14)'
    cHeaderFormat(5) ='(A9,8X,' // TRIM(IntToText(iNCol+1)) // 'A14)'
    cHeaderFormat(6) = '(A8,9X,' // TRIM(IntToText(iNCol+1)) // 'A14)'  
    cHeaderFormat(7) = '(A8,9X,' // TRIM(INTToText(iNCol)) // 'A14)'

    !Data unit and type for DSS files
    cDataUnit = ''
    cDataType = 'INST-VAL'
 
   
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(SolTrnsp%ResultsFile           , &
                              NColumnsOfData          = iNCol                   , &
                              NRowsOfData             = 1                       , &
                              OverwriteNColumnsOfData = .FALSE.                 , &
                              FormatSpec              = TRIM(cFormatSpec)       , &
                              Title                   = cTitleLines             , &
                              Header                  = cHeaders                , &
                              HeaderFormat            = cHeaderFormat           , &
                              PrintColumnNo           = .FALSE.                 , &
                              DataUnit                = cDataUnit               , &
                              DataType                = cDataType               , &
                              CPart                   = CPart                   , &
                              FPart                   = FPart                   , &        
                              UnitT                   = cUnitT                  , &
                              iStat                   = iStat                   )
  

  END SUBROUTINE OpenSoluteTranspOutputFiles
  
  
  ! -------------------------------------------------------------
  ! --- PRINT SOLUTE TRANSPORT RELATED RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintSoluteTransportResults(This, AppGW, AppGrid, Stratigraphy, TimeStep,iStat)
    CLASS(SoluteTransportLPType),INTENT(INOUT):: This
    TYPE(AppGWType),INTENT(IN)                :: AppGW
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy
    TYPE(TImeStepType),INTENT(IN)             :: TimeStep
     INTEGER,INTENT(OUT)                      :: iStat
     
    !Local variables
    INTEGER                         :: NCol_SolTranspPrint, NRow_SolTranspPrint
    INTEGER                         :: indxLayer,indxNode, ix, iy, Dim1, Dim2, Dim3
    REAL(KIND=f_IKIND), ALLOCATABLE :: DummyArray(:,:)  
    LOGICAL                         :: lEndOfSimulation
    CHARACTER                       :: SimulationTime*21, Text*500
    
    iStat=0
    
    IF (TimeStep%DELTAT .EQ. This%TimeStep%OutSkip                                                    .OR. &
        MOD(TimeStep%CurrentTimeStep,  This%TimeStep%OutSkip) .EQ. 0                                  .OR. &
        This%DataProvider%TimeStep%CurrentDateAndTime .EQ. This%DataProvider%TimeStep%EndDateAndTime) THEN
            
        Dim1=SIZE(This%Grid,DIM=1)
        Dim2=SIZE(This%Grid,DIM=2)
        Dim3=SIZE(This%Grid,DIM=3)
    
        NCol_SolTranspPrint =Dim1*Dim2*Dim3
        NRow_SolTranspPrint = 1 

        IF (.NOT. ALLOCATED(DummyArray) ) ALLOCATE(DummyArray(NRow_SolTranspPrint, NCol_SolTranspPrint ))
            DummyArray=0.0d00
                    
        !--Update layer 1 head and thus thickness 
        CALL GetUpdatedHead( This, AppGW, AppGrid, Stratigraphy, iStat)
        IF (iStat .EQ. -1) RETURN
                  
        !Prepare data for printing
        indxNode=0  
        DO indxLayer=1,Dim3
            DO ix=1, Dim1
                DO iy=1,Dim2
                    indxNode=indxNode+1
                    DummyArray(1,indxNode) = This%Grid(ix, iy, indxLayer)%InitialConcentration+     &
                                            This%Grid(ix, iy, indxLayer)%Npar*This%MassLP/          &
                                            (This%DX*This%DY*This%Grid(ix,iy,indxLayer)%Thickness)  &
                                            /This%Grid(ix,iy,indxLayer)%Porosity*This%ConcConvFact                                                          
                ENDDO
            END DO
        END DO
        
        WRITE(Text, '(A)') '' 
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        WRITE(Text, '(A,F10.3)') '       Maximum solute concentration: ', MAXVAL(DummyArray)
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        CALL LogMessage('!'//REPEAT('-',50)//'!',f_iMessage,'',f_iFILE) 

                   
        !Create the simulation time
        IF (TimeStep%TrackTime) THEN
            SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
        ELSE
            WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
        END IF
  
        !Print out the results
        IF (This%DataProvider%TimeStep%CurrentDateAndTime .EQ. This%DataProvider%TimeStep%EndDateAndTime ) lEndOfSimulation=.TRUE. 
        CALL This%ResultsFile%WriteData(SimulationTime,DummyArray,FinalPrint=lEndOfSimulation)
                    
    END IF
                
    
  END SUBROUTINE PrintSoluteTransportResults
       
    
  
  
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
  SUBROUTINE Kill(SoluteTransport)
    CLASS(SoluteTransportLPType) :: SoluteTransport
    
    !Local variables
    TYPE(SoluteTransportLPType) :: Dummy
    
    IF (.NOT. SoluteTransport%lDefined) RETURN    
    
    SELECT TYPE (SoluteTransport)
        TYPE IS (SoluteTransportLPType)
            SoluteTransport = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  

  ! -------------------------------------------------------------
  ! --- IS PARTICLE TRACKING  SIMULATED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(SoluteTransport) RESULT(lDefined)
    CLASS(SoluteTransportLPType),INTENT(IN) :: SoluteTransport
    LOGICAL                                 :: lDefined
    
    lDefined = SoluteTransport%lDefined
    
  END FUNCTION IsDefined
   
  
  ! -------------------------------------------------------------
  ! --- READ BASIC DATA FOR PARTICLE TRACKING
  ! -------------------------------------------------------------
  SUBROUTINE ReadBasicData(InFile, Porosity , iStat)
    TYPE(GenericFileType) :: InFile  
    INTEGER,INTENT(OUT)   :: iStat
    REAL(KIND=f_IKIND) ,INTENT(OUT)  :: porosity
  
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
  SUBROUTINE ReadTrackingTimeData(InFile, Iskip,Tstart, Tend, iStat)
    TYPE(GenericFileType)           :: InFile  
    INTEGER,INTENT(OUT)             :: Iskip,iStat
    CHARACTER                       :: cALine*1000
    
    !Local variables
    CHARACTER(LEN=ModNameLen+47),PARAMETER:: ThisProcedure = ModName // 'Read Tracking Time Data'
    CHARACTER(LEN=f_iTimeStampLength)     :: Tstart, Tend
    
    iStat=0
      
    !Read solute transport output skip- Nt_Skip
    CALL InFile%ReadData(Iskip,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (Iskip .LE. 0) THEN
        CALL SetLastMessage('Output time skip is not defined !',f_iFatal,ThisProcedure)
        iStat = -1
        CALL InFile%Kill()
        RETURN
    END IF
    
    
   !-Get the starting date of solute transport simulation
    CALL InFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
    
    !-Get the end date of solute transport simulation
    CALL InFile%ReadData(cALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
  SUBROUTINE ReadDomainData(InFile,delx,dely,LL_XYZCord,UR_XYZCord, iStat)
    TYPE(GenericFileType)          :: InFile  
    INTEGER,INTENT(OUT)            :: iStat
    REAL(KIND=f_IKIND),INTENT(OUT) :: delx, dely, LL_XYZCord(3), UR_XYZCord(3)
    
    !Local Variables
    CHARACTER(LEN=ModNameLen+23) :: ThisProcedure = ModName // 'SoluteTransportMainFile'
    REAL(KIND=f_IKIND)           :: LLpoint(3), URpoint(3)
    
    iStat=0
    
    !Read concentraion grid size-dx
    CALL InFile%ReadData(delx,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (delx .LE. 0) THEN
        CALL SetLastMessage('DX is not defined !',f_iFatal,ThisProcedure)
        iStat = -1
        CALL InFile%Kill()
        RETURN
    END IF

    !Read concentraion grid size-dy
    CALL InFile%ReadData(dely,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (dely .LE. 0) THEN
        CALL SetLastMessage('DY is not defined!',f_iFatal,ThisProcedure)
        iStat = -1
        CALL InFile%Kill()
        RETURN
    END IF

  
    !Read bottom lower left corner of the solute transport domain-LwrLeft
    CALL InFile%ReadData(LLpoint,iStat)  ;  IF (iStat .EQ. -1) RETURN
    LL_XYZCord=LLpoint

    !Read top right corner of the solute transport domain-LwrLeft
    CALL InFile%ReadData(URpoint,iStat)  ;  IF (iStat .EQ. -1) RETURN
    UR_XYZCord=URpoint
  
  END SUBROUTINE ReadDomainData 
     
          
  ! ------------------------------------------------------------------------------
  ! --- READ TRANSPORT SIMULATION TYPE-WITH FLOW OR WITHOUT FLOW
  ! ------------------------------------------------------------------------------
  SUBROUTINE SoluteTransportSimulationType(InFile, SimulationType,  iStat)
    TYPE(GenericFileType) :: InFile  
    INTEGER,INTENT(OUT)   :: SimulationType, iStat
      
    !Read particle tracking type-forward or backward
    CALL InFile%ReadData(SimulationType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (SimulationType .LT. 1 .OR.  SimulationType .GT. 2 ) THEN
        CALL InFile%Kill()
        RETURN
    END IF

  END SUBROUTINE SoluteTransportSimulationType
     
     
  ! -------------------------------------------------------------
  ! --- READ CONTINOUS SOURCE DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadSourceData(InFile,TimeStep, LL_XYZCord,UR_XYZCord,Nsrcs, SRC, iStat)
    TYPE(GenericFileType)           :: InFile 
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: Nsrcs
    REAL(KIND=f_IKIND),INTENT(IN)   :: LL_XYZCord(3), UR_XYZCord(3)
    TYPE(SourceType), INTENT(INOUT) :: SRC(Nsrcs)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18)    :: ThisProcedure = ModName // 'ReadContSourceData'
    INTEGER                         :: I,pos1, pos2, pos3, pos4, pos5, pos6,pos7
    CHARACTER                       :: Istring*5, cALine*1000
    CHARACTER(LEN=f_iTimeStampLength):: start_date, end_date, Token(7)
   
    
    !--Read source data including release location, concentration, volumetric flow rate, start and end time
    DO I=1,Nsrcs
        CALL InFile%ReadData(cALine,iStat) ;  IF (iStat .EQ. -1) RETURN
        
            !cALine = ADJUSTL(cALine)
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
            Token(5) = cALine(1:pos5-1)
            cALine = ADJUSTL(cALine(pos5+1:))
            CALL StripLeadingDelimiters(cALine)


            pos6 = SCAN(cALine, ' '//CHAR(9)//',')
            Token(6) = cALine(1:pos6-1)
            cALine = ADJUSTL(cALine(pos6+1:))
            CALL StripLeadingDelimiters(cALine)
            
            pos7 = SCAN(cALine, ' '//CHAR(9)//',')
            IF (pos7 .GT. 0) THEN
                Token(7) = cALine(1:pos7-1)
            ELSE
                Token(7) = TRIM(cALine)          
            END IF

             
        READ(Token(1), *)  SRC(I)%Xorg
        READ(Token(2), *)  SRC(I)%Yorg
        READ(Token(3), *)  SRC(I)%Zorg
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
        
        READ(Token(4), *) SRC(I)%Conc
        READ(Token(5), *) SRC(I)%FlowRate
        
        !-Simulation starting time       
        start_date = TRIM(Token(6))
        IF (IsTimeStampValid(start_date)) THEN
            start_date = StripTimeStamp(start_date)
        ELSE
            CALL SetLastMessage('Simulation starting time should be in MM/DD/YYYY_hh:mm format!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

        !-Simulation ending time
        end_date   = TRIM(Token(7))
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
       SRC(I)%T_Spill  =  SRC(I)%Tend- SRC(I)%Tstart+1
    END DO
    
  END SUBROUTINE ReadSourceData
  
  
  ! -------------------------------------------------------------
  ! --- INITIALIZE SOLUTE TRANSPORT VARIABLES
  ! -------------------------------------------------------------
  SUBROUTINE InitializeSolTransVariablesArrays(SolTrnsp,  AppGrid, Stratigraphy,  iStat ) 
    CLASS(SoluteTransportLPType),INTENT(INOUT) :: SolTrnsp
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)          :: Stratigraphy    
    INTEGER,INTENT(OUT)                        :: iStat  
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18)   :: ThisProcedure = ModName // 'InitializeSoluteTransport'
    INTEGER                        :: I, ix, iy, iz, iNode, indxElem, NVertex, num_xgrids, num_ygrids, num_zgrids
    INTEGER                        :: tot_npar_released, Vertex(4)
    REAL(KIND=f_IKIND)             :: t_spill, xcrd, ycrd, LyrTopElev, LyrBotElev, Coeff(4)
    CHARACTER                      :: Text*500

    !Initialize
    iStat = 0
    
    !--Number of layers
    SolTrnsp%NumLyrs=Stratigraphy%NLayers

    !--No of output time steps 
    SolTrnsp%TimeStep%NumOutTimeSteps=INT (SolTrnsp%TimeStep%EndTime/(SolTrnsp%TimeStep%DeltaT*SolTrnsp%TimeStep%OutSkip)) 
    
    num_ygrids=nint((SolTrnsp%UprRight(2)-SolTrnsp%LwrLeft(2))/SolTrnsp%DX)          !No of x-grids
    num_xgrids=nint((SolTrnsp%UprRight(1)-SolTrnsp%LwrLeft(1) ) /SolTrnsp%DY)        !No of y-grids
    num_zgrids=SolTrnsp%NumLyrs                                                      !No of z-grids
    SolTrnsp%NumXgrids=num_xgrids
    SolTrnsp%NumYgrids=num_ygrids
    
    !--Release Parcels Corresponding to Discharges 
    SolTrnsp%TSpill_End=MAXVAL(SolTrnsp%Source(:)%Tend  )

     !-- Compute solute transport domain related data (aquifer elevations at elements and surrounding elements)
    CALL SolTrnsp%DomainConfigData(AppGrid,Stratigraphy)

    ALLOCATE (SolTrnsp%Grid( 1:num_xgrids,1:num_ygrids,1:num_zgrids))
    SolTrnsp%Grid(:,:,:)%iElem=0

    !--Calculate grid coordinates and elevations (for  layer 1( unconfined) thus thickness is the water table elevation-layer bottom-updated at everytime step) 
    DO iz=1, SolTrnsp%NumLyrs
        DO  ix=1, SolTrnsp%NumXgrids	
            DO  iy=1, SolTrnsp%NumYgrids
                
                xcrd=ix*SolTrnsp%DX+SolTrnsp%LwrLeft(1)
                ycrd=SolTrnsp%UprRight(2)-iy*SolTrnsp%DY
                
               SolTrnsp%Grid(ix,iy,iz)%Xcord=xcrd
               SolTrnsp%Grid(ix,iy,iz)%Ycord=ycrd           
               !--Do the nearest neighbor search to find out the grid elevations
               IF (SolTrnsp%Grid(ix,iy,iz)%iElem == 0) THEN
                   CALL SolTrnsp%GetCurrentElement(AppGrid, xcrd, ycrd, indxElem)  
                    SolTrnsp%Grid(ix,iy,:)%iElem=indxElem
               END IF
               indxElem= SolTrnsp%Grid(ix,iy,iz)%iElem
               !--Interpolate nodal attributes (porosity, longitudinal dispersivity, and layer top and bottom elevations) to grid centers
               NVertex = AppGrid%NVertex(indxElem)
               Vertex  = AppGrid%Vertex(:,indxElem)
               Coeff=0.0d0
               CALL AppGrid%FEInterpolate_AtCell(indxElem,xcrd, ycrd,Coeff(1:NVertex))
               IF (MINVAL(Coeff) < 0.0  ) THEN
                    WRITE(Text,  '(A, 3(1X,I5))')  'Fatal Error-wrong nearest element in grid (ix,iy)-Lagrangian solute transport:', ix, iy
                    CALL SetLastMessage(TRIM(Text),f_iFILE,ThisProcedure)    
                    iStat=-1
                    RETURN                   
                END IF
               SolTrnsp%Grid(ix,iy,iz)%Porosity= SUM(Coeff(1:NVertex) * SolTrnsp%DataProvider%Nodes%Porosity(Vertex(1:NVertex),iz))   
               SolTrnsp%Grid(ix,iy,iz)%LongDispersivity = SUM(Coeff(1:NVertex) * SolTrnsp%DataProvider%Nodes%LongDispersivity(Vertex(1:NVertex), iz))  
               SolTrnsp%Grid(ix,iy,iz)%InitialConcentration = SUM(Coeff(1:NVertex) * SolTrnsp%DataProvider%Nodes%InitialConcentration(Vertex(1:NVertex), iz))  
               LyrTopElev  = SUM(Coeff(1:NVertex) * Stratigraphy%TopElev(Vertex(1:NVertex), iz) )    
               LyrBotElev= SUM(Coeff(1:NVertex) * Stratigraphy%BottomElev(Vertex(1:NVertex), iz) ) 
               SolTrnsp%Grid(ix,iy,iz)%iNode=iNode
               SolTrnsp%Grid(ix,iy,iz)%Thickness=ABS(LyrTopElev-LyrBotElev)
               IF (SolTrnsp%Grid(ix,iy,iz)%Thickness < 1.0d-10) THEN
                    WRITE(Text,  '(A, 3(1X,I5))')  'Fatal Error-zero element thickness in (ix, iy, iz)-Lagrangian solute transport :', ix, iy, iz
                    CALL SetLastMessage(TRIM(Text),f_iFILE,ThisProcedure)    
                    iStat=-1
                    RETURN               
               ENDIF
               SolTrnsp%Grid(ix,iy,iz)%Zcord   =0.5*(LyrTopElev+LyrBotElev)
            ENDDO
        ENDDO
    ENDDO
    
    !--End time of the spills
    t_spill=MAXVAL(SolTrnsp%Source(:)%T_Spill)
    
    !--Convert Source concentration into mass rate 
    SolTrnsp%TotalMass=0.0d00
    DO I=1, SolTrnsp%NumSources  
         SolTrnsp%Source(I)%MassRate=SolTrnsp%Source(I)%Conc/1000*SolTrnsp%Source(I)%FlowRate 
         SolTrnsp%TotalMass=SolTrnsp%TotalMass+SolTrnsp%Source(I)%MassRate*SolTrnsp%Source(I)%T_spill  
    ENDDO
    
    !--Mass of a LP
    SolTrnsp%MassLP=SolTrnsp%TotalMass/FLOAT(SolTrnsp%Num_Parcels)
    
    tot_npar_released=0
    DO I=1,SolTrnsp%NumSources       
        SolTrnsp%Source(I)%npar_rel=int(SolTrnsp%Source(I)%MassRate*SolTrnsp%Source(i)%T_Spill/SolTrnsp%TotalMass*SolTrnsp%Num_Parcels)
        SolTrnsp%Source(I)%npar_dt=int( SolTrnsp%Source(I)%npar_rel/SolTrnsp%Source(i)%T_Spill*SolTrnsp%TimeStep%DeltaT)
        SolTrnsp%Source(I)%tot_mass=SolTrnsp%Source(I)%npar_dt*SolTrnsp%TimeStep%DeltaT*int((SolTrnsp%Source(I)%T_Spill/SolTrnsp%TimeStep%DeltaT))
        tot_npar_released=tot_npar_released+ SolTrnsp%Source(I)%npar_dt*SolTrnsp%Source(i)%T_Spill            
        SolTrnsp%Source(I)%MassReleased=0.0d0
    ENDDO
    
    !--Update the number of parcels used for the simulation
    SolTrnsp%Num_Parcels=tot_npar_released
    
    !--Update the mass of a LP
    SolTrnsp%MassLP=SolTrnsp%TotalMass/FLOAT(SolTrnsp%Num_Parcels)

    !--Create a Linke List to store LPs
    CALL SolTrnsp%LPList%Initialize( )
    
    
    SolTrnsp%DispCoeff_xx_max=0.0d00
    SolTrnsp%DispCoeff_yy_max=0.0d00
  
  END SUBROUTINE InitializeSolTransVariablesArrays
  
 
  ! -------------------------------------------------------------
  ! --- GET THE UPDATED HEAD VALUES
  ! -------------------------------------------------------------
  SUBROUTINE GetUpdatedHead(SolTrnsp, AppGW, AppGrid, Stratigraphy,  iStat ) 
    CLASS(SoluteTransportLPType),INTENT(INOUT) :: SolTrnsp
    TYPE(AppGWType),INTENT(IN)                 :: AppGW
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)          :: Stratigraphy    
    INTEGER,INTENT(OUT)                        :: iStat  
    
     !Local variables
    CHARACTER(LEN=ModNameLen+23)   :: ThisProcedure = ModName//'UpdatedHeadFromFlowSoln'
    INTEGER                        :: iNode, ix, iy, iz, indxElem, NVertex
    INTEGER                        :: Vertex(4)
    REAL(KIND=f_IKIND)             :: xcrd, ycrd, LyrTopElev, LyrBotElev, Coeff(4)
    REAL(KIND=f_IKIND)             :: GWHead(AppGrid%NNodes,Stratigraphy%NLayers)
    CHARACTER                      :: Text*500


    iStat=0
    
    !--Get updated heads if flow is simulated together
    IF ( SolTrnsp%DataProvider%lFlowSimulated) THEN
        CALL AppGW%GetHeads_All(.FALSE.,GWHead)
    ELSE
        !-- Assign the layer 1 top elevation based on the simulated head value
        DO iNode =1, AppGrid%NNodes
              GWHead(iNode,1)=SolTrnsp%DataProvider%Nodes%GWHead(iNode)	
        END DO
    END IF
    
    !--Calculate grid coordinates and elevations (for  layer 1( unconfined) thus thickness is the water table elevation-layer bottom-updated at everytime step) 
    iz=1
    DO  ix=1, SolTrnsp%NumXgrids	
        DO  iy=1, SolTrnsp%NumYgrids
            xcrd=ix*SolTrnsp%DX+SolTrnsp%LwrLeft(1)
            ycrd=SolTrnsp%UprRight(2)-iy*SolTrnsp%DY
                
            indxElem=SolTrnsp%Grid(ix,iy,iz)%iElem
              
            !--Interpolate nodal attributes (porosity, longitudinal dispersivity, and layer top and bottom elevations) to grid centers
            NVertex = AppGrid%NVertex(indxElem)
            Vertex  = AppGrid%Vertex(:,indxElem)
            CALL AppGrid%FEInterpolate_AtCell(indxElem,xcrd, ycrd,Coeff(1:NVertex))
            !--update the thickness only in unconfined layer (layer1) 
            LyrTopElev  = SUM(Coeff(1:NVertex) * GWHead(Vertex(1:NVertex), iz) )    
            LyrBotElev= SUM(Coeff(1:NVertex) * Stratigraphy%BottomElev(Vertex(1:NVertex), iz) ) 
            !-if the groundwater level is above the ground surface make sure use the ground surface to calculate thickness
            SolTrnsp%Grid(ix,iy,iz)%Thickness=MIN( SolTrnsp%Grid(ix,iy,iz)%Thickness, ABS(LyrTopElev-LyrBotElev))
            IF (SolTrnsp%Grid(ix,iy,iz)%Thickness < 1.0d-10) THEN
                WRITE(Text,  '(A, 3(1X,I5))')  'Fatal Error-zero element thickness in (ix, iy, iz)-Lagrangian solute transport :', ix, iy, iz
                CALL SetLastMessage(TRIM(Text),f_iFILE,ThisProcedure)    
                iStat=-1
                RETURN               
            ENDIF
        ENDDO
    ENDDO
    
  END SUBROUTINE GetUpdatedHead
  
  
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
        !--Check the distance between the cell center and the node
        dist=sqrt( (AppGrid%X(I)-xcrd)**2.+ (AppGrid%Y(I)-ycrd)**2. ) 
        IF (dist < dist_min) THEN
            dist_min=dist
            iNode=I
        ENDIF     
    END DO
    
  END SUBROUTINE Nearest_Node
  
  
  ! -------------------------------------------------------------
  ! --- SEARCH NEAREST ELEMENT GIVEN X AND Y COORDINATES
  ! --- *Note: This is a bit too expensive, need an efficient method here
  ! -------------------------------------------------------------
  SUBROUTINE GetCurrentElement(This, AppGrid, xcrd, ycrd, iElem)    
    CLASS(SoluteTransportLPType),INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    INTEGER, INTENT (OUT)                      :: iElem
    REAL(KIND=f_IKIND) , INTENT (IN)           :: xcrd, ycrd   
  
    !Local variables
    INTEGER            :: I, jElem, nID , Vertex(4)
    REAL(KIND=f_IKIND) :: elem_center_x, elem_center_y, dist_min, dist 
    REAL (KIND=f_IKIND):: x_vertex(4), y_vertex(4)
    LOGICAL            :: is_inside

    dist_min=1e10
    DO jElem =1, AppGrid%NElements  
        !-Get element centroid
        CALL AppGrid%Centroid(jElem,elem_center_x,elem_center_y)       
        !--Check the distance between the cell center and element center
        dist=sqrt(( elem_center_x-xcrd)**2.+ (elem_center_y-ycrd)**2.) 
        IF (dist < dist_min) THEN
            dist_min=dist
            iElem=jElem
            
            !--Check if the point is within this element only when the distance become smaller (less than average element length)
            IF (dist <This%AvgElemLength) THEN
                Vertex=AppGrid%Vertex(:, iElem)
                DO I=1, 3
                    nID=Vertex(I)
                    x_vertex(I)=AppGrid%GridType%X(nID)
                    y_vertex(I)=AppGrid%GridType%Y(nID) 
                ENDDO
                 nID=Vertex(4) 
                IF ( nID>0) THEN   ! Quadlilataral mesh   
                    x_vertex(I)=AppGrid%GridType%X(nID)
                    y_vertex(I)=AppGrid%GridType%Y(nID) 
                    is_inside=This%Is_LP_Inside_Quad(xcrd, ycrd, x_vertex, y_vertex)                     
                ELSE  ! Triangular mesh
                     is_inside= This%Is_LP_Inside_Triangle (xcrd, ycrd, x_vertex(1),x_vertex(2), x_vertex(3), y_vertex(1), y_vertex(2), y_vertex(3) )
                END IF
                IF (is_inside) THEN
                        EXIT
                END IF
            END IF
            
        END IF 
    END DO
        
  END SUBROUTINE GetCurrentElement
  
  
  ! -------------------------------------------------------------
  ! --- RELEASE LAGRANGIAN PARCEL CORRESPONDS TO SPILL DISCHARGES
  ! -------------------------------------------------------------
  SUBROUTINE ReleaseLPs(SolTrnsp,AppGrid,Stratigraphy, Velocity, TimeStep, iTrns,iStat) 
    CLASS(SoluteTransportLPType),INTENT(INOUT)   :: SolTrnsp
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)                :: TimeStep
    TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
    INTEGER,INTENT(IN)                           :: iTrns
    INTEGER,INTENT(OUT)                          :: iStat  

    !-Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReleaseLPs'
    INTEGER                      :: k, ip, np, ilyr, num_par_dt 
    INTEGER                      :: npar,new_elem, CurrentTimeStep
    REAL(KIND=f_IKIND)           :: dtsmall
    CHARACTER                    :: Text*500
    TYPE (LPNodeType), POINTER   :: current
    LOGICAL                      :: found

    !Initialize
    iStat = 0 
    
    !--Initialize the particle grid particle numbers
    SolTrnsp%Grid%Npar=0
    SolTrnsp%Grid%nParRemDT=0
    SolTrnsp%Grid%ResidualMass=0.0d0
    npar=0

    CurrentTimeStep=TimeStep%CurrentTimeStep
    
    ilyr=0
    
    current=>SolTrnsp%LPList%Tail

    !-Find the tail position to add LPs
    IF (.NOT. ASSOCIATED (current)) THEN
        np=1
    ELSE
        np=SolTrnsp%LPList%Tail%ID+1
    ENDIF

    npar=0
    DO k=1, SolTrnsp%NumSources 
        IF (CurrentTimeStep >= SolTrnsp%Source(k)%Tstart   .and. CurrentTimeStep <= SolTrnsp%Source(k)%Tend ) THEN
 
            IF(SolTrnsp%TimeStep%nsubTimeSteps==1) THEN
                num_par_dt=SolTrnsp%Source(k)%npar_dt
            ELSE
                num_par_dt=SolTrnsp%Source(k)%npar_dtsub(iTrns)
            ENDIF
             
            DO ip=1,num_par_dt 
                
                !-Add a LP at the tail of the link
               CALL  SolTrnsp%LPList%InsertLP (SolTrnsp%Source(k)%Xorg, SolTrnsp%Source(k)%Yorg, SolTrnsp%Source(k)%Zorg, SolTrnsp%Source(k)%ElemID , iStat )  
               IF (iStat .EQ. -1) THEN   
                        CALL SetLastMessage('Error inserting LP node ID '//TRIM(IntToText(np))//'!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
               ENDIF
               Current=>SolTrnsp%LPList%Tail
               
               CALL RANDOM_SEED(get=Current%RNGState)
                
               !--For continous release figure out the small time step for each release
               dtsmall=SolTrnsp%TimeStep%DeltaT/float(num_par_dt)*ip
              
             
               !--Calculate the layer number
               IF (ip==1) THEN
                   CALL SolTrnsp%Get_LP_Layer( Stratigraphy, Current%ID, Current%Elem, Current%zcord, ilyr, iStat) 
                   IF (iStat==-1) THEN
                       CALL SetLastMessage('LP released:  '//TRIM(IntToText(Current%ID))//' is outside the transport domain!',f_iFatal,ThisProcedure)
                       RETURN
                   ENDIF         
               END IF
               Current%Igridz=ilyr
              
               !--Calculate the new parcel location due to advection and diffusion
               CALL SolTrnsp%DisplaceLPs (AppGrid , &
                                Stratigraphy      , &
                                current%xcord     , &
                                current%ycord     , &
                                current%zcord     , &
                                current%Igridz    , &
                                current%Elem      , &
                                Velocity          , &
                                dtsmall           , &
                                iTrns             , &
                                "SOLUTETRANSPORT" , &                 
                                iStat); 
               
               IF (iStat.EQ.-1) THEN
                   CALL SetLastMessage('Displacement calculated for LP:  '//TRIM(IntToText(Current%ID))//' is wrong!',f_iFatal,ThisProcedure)
                   RETURN
               ENDIF 
               
              !--Update the element number: If the particle has moved to a new element figure out the new element number 
               CALL SolTrnsp%Get_LP_Elem (AppGrid, Current%Elem, Current%xcord,  Current%ycord, new_elem, found, iStat)
               IF (iStat .EQ. -1) RETURN
               IF ( found)  Current%Elem=new_elem

               
               !-- Check if the LP has moved out of the domain-Apply boundary conditions
               CALL SolTrnsp%ApplyReflectionBC (Current%Elem, Current%xcord, Current%ycord, Current%zcord) 
              
               !-Update the layer number: If the parcel has moved to another layer figure out the new layer
               CALL SolTrnsp%Get_LP_Layer( Stratigraphy, Current%ID, Current%Elem, Current%zcord, ilyr, iStat)  
               IF (iStat.EQ.-1) THEN
                   CALL SetLastMessage('LP released:  '//TRIM(IntToText(Current%ID))//' is outside the transport domain!',f_iFatal,ThisProcedure)
                   RETURN
               ENDIF         
               Current%Igridz=ilyr
                           
               !--Update the number of parcels in each grid
                CALL SolTrnsp%Count_LPs (Current, iStat ) 
                IF (iStat .NE. 0) RETURN 
                np=np+1
                npar=npar+1    

            END DO
        ENDIF
    END DO   

    WRITE(Text, ' (A,I12 ) ') '       Number of parcels released: ' // TRIM(IntToText(npar))
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 

  END SUBROUTINE ReleaseLPs
  
  
  ! -------------------------------------------------------------
  ! --- TRNASPORT LAGRANGIAN PARCEL CORRESPONDS TO SPILL DISCHARGES
  ! -------------------------------------------------------------
  SUBROUTINE TransportLPs(SolTrnsp, AppGrid,Stratigraphy, Velocity, TimeStep, iTrns, iStat) 
    CLASS(SoluteTransportLPType),INTENT(INOUT)   :: SolTrnsp
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy  
    TYPE(TimeStepType),INTENT(IN)                :: TimeStep 
    INTEGER,INTENT(IN)                           :: iTrns
    TYPE (GWVelocityFieldType),POINTER,INTENT(IN):: Velocity 
    INTEGER,INTENT(OUT)                          :: iStat  


    !-Local variables
    CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'TransportLPs'
    INTEGER                      :: np, ilyr 
    INTEGER                      :: new_elem , npar, CurrentTimeStep
    REAL (KIND=f_IKIND)          :: FlowTimeStep
    TYPE (LPNodeType), POINTER   :: current
    LOGICAL                      :: found

    !Initialize
    iStat = 0 
  
    ilyr=0
    
    FlowTimeStep =  TimeStep%DeltaT
    CurrentTimeStep=TimeStep%CurrentTimeStep

    !--Reset the particle grid particle numbers
    SolTrnsp%Grid%Npar=0
    SolTrnsp%Grid%nParRemDT=0
    SolTrnsp%Grid%ResidualMass=0.0d0
    npar=0
    current=>SolTrnsp%LPList%Head
    DO WHILE (ASSOCIATED (current))    
        
        np=current%ID     
        
        !-- Check if the LP has moved out of the domain-Apply boundary conditions
        CALL SolTrnsp%ApplyReflectionBC (current%Elem, current%xcord, current%ycord, current%zcord) 

        !--Calculate the new parcel location due to advection and diffusion
        CALL SolTrnsp%DisplaceLPs (AppGrid           , &
                            Stratigraphy             , &
                            current%xcord            , &
                            current%ycord            , &
                            current%zcord            , &
                            current%Igridz           , &
                            current%Elem             , &
                            Velocity                 , &
                            SolTrnsp%TimeStep%DeltaT , &
                            iTrns                    , &
                            "SOLUTETRANSPORT"        , &
                            iStat); 

        IF (iStat.EQ.-1) THEN
            CALL SetLastMessage('Displacement calculated for LP:  '//TRIM(IntToText(Current%ID))//' is wrong!',f_iFatal,ThisProcedure)
            RETURN
        ENDIF 

        !--Update the element number: If the particle has moved to a new element figure out the new element number 
        CALL SolTrnsp%Get_LP_Elem (AppGrid, Current%Elem, Current%xcord,  Current%ycord, new_elem, found, iStat)
        IF ( found)  Current%Elem=new_elem
        IF (iStat.EQ.-1) THEN
            CALL SetLastMessage('Element number calculated for LP:  '//TRIM(IntToText(Current%ID))//' is wrong!',f_iFatal,ThisProcedure)
            RETURN
        ENDIF
        
        !-- Check if the LP has moved out of the domain-Apply boundary conditions
        CALL SolTrnsp%ApplyReflectionBC (current%Elem, current%xcord, current%ycord, current%zcord) 

        !--Update the layer number: If the parcel has moved to another layer figure out the new layer
        CALL SolTrnsp%Get_LP_Layer( Stratigraphy, Current%ID, Current%Elem, Current%zcord, ilyr, iStat)  
        IF (iStat.EQ.-1) THEN
            CALL SetLastMessage('LP released:  '//TRIM(IntToText(Current%ID))//' is outside the vertical transport domain!',f_iFatal,ThisProcedure)
            RETURN 
        ENDIF         
        Current%Igridz=ilyr
        
        !--Update the number of parcels in each grid
        CALL SolTrnsp%Count_LPs (Current, iStat ) 
        IF (iStat .NE. 0) RETURN 
        !--Move to the next LP in the linked list
        current=>current%Next
        npar=npar+1       

    END DO

  END SUBROUTINE TransportLPs
  
  
  ! -------------------------------------------------------------
  ! --- ACCOUNT FOR SINKS-DELETE PARCELS AT PUMPING WELLS
  ! -------------------------------------------------------------
  SUBROUTINE ApplySinks(SolTrnsp, AppGW,  AppGrid,Stratigraphy, iStat) 
    CLASS(SoluteTransportLPType),INTENT(INOUT) :: SolTrnsp
    TYPE(AppGWType),INTENT(IN)                 :: AppGW
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)          :: Stratigraphy  
    INTEGER,INTENT(OUT)                        :: iStat  


    !-Local variables
    CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'TransportLPs'
    INTEGER                      :: np, npar_delete
    INTEGER                      :: ix, iy,indxPump,indElem,indxLayer, iPump
    REAL(KIND=f_IKIND)           :: VolWaterRemoved, ContamiantMassRemoved, ContaminantConc
    REAL(KIND=f_IKIND)           :: gridVolume, gridArea, ElementArea
    REAL(KIND=f_IKIND)           :: rElemPump(AppGrid%NElements), Pfactors(Stratigraphy%NLayers)
    REAL(KIND=f_IKIND)           :: PElemfactors(Stratigraphy%NLayers), PWellfactors(Stratigraphy%NLayers)
    TYPE (LPNodeType), POINTER   :: current
    LOGICAL                      :: found
    TYPE(PumpsAtElemType)        :: ElemPumpsAtElem, WellPumpsAtElem

    !-Initialize
    iStat=0
    
    !--Calculate the number of parcels that should be removed corresponds to pumping 
    !--and the contamainant concentration
    DO ix=1,SolTrnsp%NumXgrids
        DO iy=1,SolTrnsp%NumYgrids
           DO indxLayer=1,SolTrnsp%NumLyrs
    
             IF (SolTrnsp%Grid(ix, iy,indxLayer)%Npar > 0) THEN
                 
                indElem= SolTrnsp%Grid(ix,iy,indxLayer)%iElem

                !--Pumping volume for this time step-both element and well pumping
                CALL AppGW%GetElementPumpActual(rElemPump)
                
                IF (rElemPump(indElem)>0 )THEN
                    !-Element pumping contribution
                    ElemPumpsAtElem=AppGW%AppPumping%ElemPumpsAtElems(indElem)
                    IF(ElemPumpsAtElem%nPumps>0) THEN
                        PElemfactors=0.0d00
                        DO indxPump=1,ElemPumpsAtElem%nPumps
                            iPump   = ElemPumpsAtElem%iPumpIDs(indxPump)
                            PElemfactors=PElemfactors+AppGW%AppPumping%ElemPumps(iPump)%rLayerFactor
                        END DO
                        PElemfactors=PElemfactors/FLOAT(ElemPumpsAtElem%nPumps)
                    ENDIF
                
                    !--Well pumping contribution
                    WellPumpsAtElem=AppGW%AppPumping%WellsAtElems(indElem)
                    IF (WellPumpsAtElem%nPumps > 0) THEN 
                        PWellfactors=0.0d00
                        DO indxPump=1,WellPumpsAtElem%nPumps
                            iPump   = WellPumpsAtElem%iPumpIDs(indxPump)
                            PWellfactors=PWellfactors+AppGW%AppPumping%ElemPumps(iPump)%rLayerFactor
                        END DO
                        PWellfactors=PWellfactors/FLOAT(WellPumpsAtElem%nPumps)
                    ENDIF
                    
                    !--Calculate the fractions of pumping for each layer
                    IF(ElemPumpsAtElem%nPumps>0 .OR. WellPumpsAtElem%nPumps > 0 ) THEN
                        Pfactors=0.0d00
                        IF (ElemPumpsAtElem%nPumps>0 .AND. WellPumpsAtElem%nPumps==0) THEN
                           Pfactors=PElemfactors
                        ELSEIF (ElemPumpsAtElem%nPumps==0 .AND. WellPumpsAtElem%nPumps > 0) THEN
                           Pfactors=PWellfactors
                        ELSEIF (ElemPumpsAtElem%nPumps>0 .AND. WellPumpsAtElem%nPumps > 0) THEN
                           Pfactors=0.5*(PWellfactors+PElemfactors)
                        ENDIF
                        
                        VolWaterRemoved=rElemPump(indElem)*Pfactors(indxLayer)
    
                        ElementArea=AppGrid%AppElement(indElem)%Area
                        gridArea=(SolTrnsp%DX*SolTrnsp%DY)
                        gridVolume=(SolTrnsp%DX*SolTrnsp%DY*SolTrnsp%Grid(ix,iy,indxLayer)%Thickness)
                        
                        !-Contaminant concentration of the current grid
                        ContaminantConc=SolTrnsp%Grid(ix, iy,indxLayer)%Npar*SolTrnsp%MassLP/(gridVolume*SolTrnsp%Grid(ix,iy,indxLayer)%Porosity)
                        
                       !-Mass of contaminant to be removed 
                       ContamiantMassRemoved=ContaminantConc*VolWaterRemoved*gridArea/ElementArea  
                       SolTrnsp%Grid(ix,iy,indxLayer)%nParRemDT=FLOOR(ContamiantMassRemoved/SolTrnsp%massLP)
                   ENDIF
                ENDIF 
             ENDIF
             
           ENDDO
        ENDDO
    ENDDO
    
   !--Remove the LPs at pumping well locations
    npar_delete=0
    current=>SolTrnsp%LPList%Head
    DO WHILE (ASSOCIATED (current))    
        
        np=current%ID   
        
        iy=CEILING((SolTrnsp%UprRight(2)-current%ycord )/SolTrnsp%DY)   ! Row                                 
        ix=CEILING((current%xcord-SolTrnsp%LwrLeft(1))/SolTrnsp%DX)     ! Column
        indxLayer=Current%Igridz                                        ! Layer
        
        !--Remove the LP as long the number of parcels to be removed is greater than zero
        IF ( SolTrnsp%Grid(ix,iy,indxLayer)%nParRemDT >0) THEN
            CALL  SolTrnsp%LPList%RemoveLP ( np, Current , found, iStat )  
            IF (iStat .EQ. -1) THEN   
                CALL SetLastMessage('Error removing LP node ID '//TRIM(IntToText(np))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            ENDIF
            !--Update the number of parcels to be deleted
            SolTrnsp%Grid(ix,iy,indxLayer)%nParRemDT=SolTrnsp%Grid(ix,iy,indxLayer)%nParRemDT-1
            npar_delete=npar_delete+1
            !--Update the number of parcels in each grid
            SolTrnsp%Grid(ix,iy,indxLayer)%Npar=SolTrnsp%Grid(ix,iy,indxLayer)%Npar-1    
        ENDIF
        
        !--Move to the next LP in the linked list
        current=>current%Next
        
    ENDDO
    
  END SUBROUTINE ApplySinks
  

  
  ! -------------------------------------------------------------
  ! --- CALCULATE THE NUMBER OF PARCELS RELASED FOR SUBTIME STEPS 
  ! -------------------------------------------------------------
  SUBROUTINE CalcNumParReleaseSubDT(SolTrnsp, TimeStep, iStat) 
    CLASS(SoluteTransportLPType),INTENT(INOUT) :: SolTrnsp
    TYPE(TimeStepType),INTENT(IN)              :: TimeStep    
    INTEGER,INTENT(OUT)                        :: iStat    

    !-Local Varaibles
    INTEGER                   :: I,J, tot_npar_released, npar_dist_sum
    INTEGER                   :: NumSources_ThisTime, npar_dtsub, npar_sum_actual
    CHARACTER                 :: Text*500
 
    
    !Initialize
    iStat = 0
    tot_npar_released=0
    
    !--Stability criteria fails and therefore subdivide transport time step and distribute the number of parcels
    !--released at one flow step to each sub time step keeping the total same

    npar_dist_sum=0
    npar_sum_actual=0
    NumSources_ThisTime=0
    DO I=1,SolTrnsp%NumSources 
       IF (TimeStep%CurrentTimeStep >= SolTrnsp%Source(I)%Tstart .AND. TimeStep%CurrentTimeStep <= SolTrnsp%Source(I)%Tend ) THEN
            npar_dtsub= INT(SolTrnsp%Source(I)%npar_dt/SolTrnsp%TimeStep%nsubTimeSteps)    
            DO J=1,SolTrnsp%TimeStep%nsubTimeSteps 
                SolTrnsp%Source(I)%npar_dtsub(J)=npar_dtsub 
                npar_dist_sum=npar_dist_sum+npar_dtsub
            ENDDO
            npar_sum_actual=npar_sum_actual+SolTrnsp%Source(I)%npar_dt
            NumSources_ThisTime=NumSources_ThisTime+1
       ENDIF
    ENDDO
        
    !-If the number of parcels released per time step (npar_dt) is not evenly divisible then distribute the remainder until all adds to the total
    IF (npar_dist_sum < npar_sum_actual) THEN 
        DO J=1,SolTrnsp%TimeStep%nsubTimeSteps  
            DO I=1,SolTrnsp%NumSources
                DO WHILE ( npar_dist_sum <npar_sum_actual .AND. TimeStep%CurrentTimeStep >= SolTrnsp%Source(I)%Tstart  &
                                                          .AND. TimeStep%CurrentTimeStep <= SolTrnsp%Source(I)%Tend )
                          SolTrnsp%Source(I)%npar_dtsub(J)=SolTrnsp%Source(I)%npar_dtsub(J)+1
                          npar_dist_sum=npar_dist_sum+1
                ENDDO                        
            ENDDO
        ENDDO
    ENDIF
        
    !--Print out the number of parcels released per subtime step at each location and the total masses released
    !--Simulating solute transport based on the subtime step 
    WRITE(Text, ' (A) ') ' '
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)
    DO I=1,SolTrnsp%NumSources
        IF (TimeStep%CurrentTimeStep >= SolTrnsp%Source(I)%Tstart   .and. TimeStep%CurrentTimeStep <= SolTrnsp%Source(I)%Tend ) THEN
           DO J=1,SolTrnsp%TimeStep%nsubTimeSteps 
                WRITE(Text,'(A)') '*  Number of parcels released at location: ' // &
                TRIM(IntToText(I)) // ' per subtime step ' // &
                TRIM(IntToText(J)) // ' = ' // &
                TRIM(IntToText(SolTrnsp%Source(I)%npar_dtsub(J)))                
                CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
           ENDDO
        ENDIF
    ENDDO
    WRITE(Text, ' (A) ') ' '
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
    

    !--Print the released mass
    DO I=1,SolTrnsp%NumSources 
        IF (TimeStep%CurrentTimeStep >= SolTrnsp%Source(I)%Tstart   .and. TimeStep%CurrentTimeStep <= SolTrnsp%Source(I)%Tend ) THEN
            SolTrnsp%Source(I)%MassReleased=SolTrnsp%Source(I)%npar_dt*SolTrnsp%MassLP        
            WRITE(Text, ' (A, I8, A,  F12.3, A) ') '*  Total mass released per time step at location:',I, "=", SolTrnsp%Source(I)%MassReleased,' kg'
            CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        ENDIF
    ENDDO
    CALL LogMessage('!'//REPEAT('-',50)//'!',f_iMessage,'',f_iFILE) 

  END SUBROUTINE CalcNumParReleaseSubDT 
  
  
  ! -------------------------------------------------------------
  ! --- COUNT THE NUMBER OF LPS IN EACH CARTESIAN GRID
  ! -------------------------------------------------------------
  SUBROUTINE Count_LPs (SolTrnsp, Current, iStat )                      
    CLASS(SoluteTransportLPType),INTENT(INOUT) :: SolTrnsp
    TYPE (LPNodeType), POINTER, INTENT(INOUT)  :: current
    INTEGER,INTENT(OUT)                        :: iStat    
    
    !-Local Varaibles
    INTEGER :: ix, iy,iz
    
    iStat=0

    IF (Current%Ingrid)     THEN       
        iy=CEILING((SolTrnsp%UprRight(2)-Current%ycord )/SolTrnsp%DY)   ! Row                                 
        ix=CEILING((Current%xcord-SolTrnsp%LwrLeft(1))/SolTrnsp%DX)       ! Column
        iz=Current%Igridz
                               
        Current%Igridx=ix
        Current%Igridy=iy 
        Current%Igridz=iz                     
        SolTrnsp%Grid(ix,iy,iz)%Npar=SolTrnsp%Grid(ix,iy,iz)%Npar+1
    ELSE
        iStat=-1
        RETURN
    ENDIF

  END SUBROUTINE Count_LPs

END MODULE Package_LagrangianSolTrans
    
    
    
