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
MODULE TransportDataReader
  USE MessageLogger          , ONLY: LogMessage                    , &
                                     SetLastMessage                , &
                                     EchoProgress                  , &
                                     f_iFILE                       , &
                                     f_iMessage                    , &
                                     f_iFatal                 
  USE Class_AsciiFileType    , ONLY:AsciiTSDInFileType         
  USE GeneralUtilities       , ONLY: IntToText                     , &
                                     StripTextUntilCharacter       , &
                                     CleanSpecialCharacters        , &
                                     EstablishAbsolutePathFileName , &
                                     ConvertID_To_Index            , &
                                     f_cLineFeed                                 
  USE TimeSeriesUtilities    , ONLY: TimeStepType                  , &
                                     LeapYearCorrection            , &
                                     NPeriods                      , &
                                     f_iTimeStampLength
  USE IOInterface            , ONLY: GenericFileType               , & 
                                     f_iTXT                      
  USE Package_Discretization , ONLY: AppGridType                   , &
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
  PUBLIC :: TransportDataReaderType , &
            GWSPNodeType            , &
            GWVelocityFieldType     , &
            StripLeadingDelimiters

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 26
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'TransportDataReader::'


  ! -------------------------------------------------------------
  ! --- DOUBLE PRECISION KIND DESCRIPTOR
  ! -------------------------------------------------------------
   INTEGER, PARAMETER :: f_IKIND=8

   
  ! -------------------------------------------------------------
  ! --- GW VELOCITY DATA TYPE FOR TRANSPORT
  ! -------------------------------------------------------------
  TYPE GWVelocityFieldType
      INTEGER                    :: nLine_TecplotVelBlock
      INTEGER                    :: nLineSkip_TecplotVelFile
      REAL(KIND=f_IKIND),ALLOCATABLE :: Vx(:,:), Vy(:,:), Vz(:,:)            ! Velocity components at each (node,layer)
      REAL(KIND=f_IKIND),ALLOCATABLE :: VxNext(:,:), VyNext(:,:), VzNext(:,:)   ! Velocity components at each (node,layer)
      REAL(KIND=f_IKIND)         :: Time                        
      REAL(KIND=f_IKIND)         :: ConvFact                    ! Conversion factor for velocity
  END TYPE GWVelocityFieldType
 
  
  ! -------------------------------------------------------------
  ! --- GW NODE DATA TYPE FOR TRANSPORT
  ! -------------------------------------------------------------
  TYPE GWSPNodeType
      REAL(KIND=f_IKIND),ALLOCATABLE        :: Porosity(:,:)            ! Aquifer porosity at each (node,layer)
      REAL(KIND=f_IKIND),ALLOCATABLE        :: LongDispersivity(:,:)    ! Aquifer longitudinal dispersivity at each (node,layer)
      REAL(KIND=f_IKIND),ALLOCATABLE        :: InitialConcentration(:,:)! Aquifer longitudinal dispersivity at each (node,layer)
      REAL(KIND=f_IKIND)                    :: RatioHorzTransLong       ! Ratio of Horizontal Transverse to Longitudinal Dispersivity 
      REAL(KIND=f_IKIND)                    :: RatioVertTransLong       ! Ratio of Vertical Transverse to Longitudinal Dispersivity
      TYPE (GWVelocityFieldType), POINTER   :: Velocity                 ! Velocity components at each (node,layer)
      REAL(KIND=f_IKIND),POINTER            :: GWHead(:)                ! Simulated head at each node in layer 1
      REAL(KIND=f_IKIND)                    :: Constant                 ! Aquifer parameters are specified as constant or not
  END TYPE GWSPNodeType
  

  TYPE, EXTENDS(TimeStepType) :: SolTransTimeStepType
      INTEGER                             :: NumFlowTimeSteps
      CHARACTER(LEN=f_iTimeStampLength)   :: StartDateAndTime
  END TYPE SolTransTimeStepType

  TYPE TransportDataReaderType 
      TYPE(SolTransTimeStepType)           :: TimeStep                       
      TYPE(GWSPNodeType)                   :: Nodes
      TYPE(GenericFileType)                :: CommonTransDataFile
      TYPE(GenericFileType),ALLOCATABLE    :: PorosityFile
      TYPE(GenericFileType),ALLOCATABLE    :: DispersivityFile
      TYPE(GenericFileType),ALLOCATABLE    :: InitialConcentrationFile
      TYPE(GenericFileType),ALLOCATABLE    :: SavedVelocityFile
      TYPE(AsciiTSDInFileType),ALLOCATABLE :: SavedHeadsFile
      REAL(KIND=f_IKIND)                   :: HeadConvFact              = 1.0
      REAL(KIND=f_IKIND)                   :: HorVelocityMax            = 100.0    !ft/day 
      LOGICAL                              :: VelocityBounds            = .FALSE.
      LOGICAL                              :: lFlowSimulated            = .FALSE.
      LOGICAL                              :: lVelocityLoaded           = .FALSE.  
      LOGICAL                              :: lBackwardTracking         = .FALSE.
      LOGICAL                              :: lForwardTracking          = .FALSE.  
      LOGICAL                              :: lSolTransDefined          = .FALSE.
      LOGICAL                              :: lParTrackDefined          = .FALSE.     
      LOGICAL                              :: lEndOfTransSimulation     = .FALSE.
  CONTAINS
      PROCEDURE        :: InitializeDataReader
      PROCEDURE        :: ReadAquiferDispersivity
      PROCEDURE        :: ReadInitialConcentration
      PROCEDURE        :: LoadTSData
      !PROCEDURE,NOPASS :: CalculatePoreVelocity
      PROCEDURE,NOPASS :: StripLeadingDelimiters
  END TYPE TransportDataReaderType
      
      
CONTAINS
    
    
  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMOMRY FOR ARRAYS AND READ STATIC PARAMETERS
  ! -------------------------------------------------------------    
  SUBROUTINE InitializeDataReader(This, cInFileName, AppGrid, Stratigraphy, TimeStep, cWorkingDirectory, iStat)
    CLASS(TransportDataReaderType), INTENT(INOUT) :: This
    TYPE(AppGridType), INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)            :: Stratigraphy
    TYPE(TimeStepType), INTENT(IN)                :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)                   :: cInFileName, cWorkingDirectory    
    INTEGER,INTENT(OUT)                           :: iStat

    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'InitializeDataReader'
    INTEGER                      :: NNodes, NLayers, RunType
    
    !Initialize
    iStat=0

    NNodes    = AppGrid%GetNNodes()
    NLayers   = Stratigraphy%GetNLayers()
    
    ALLOCATE( This%Nodes%Porosity(NNodes,NLayers) )   
    This%Nodes%Porosity = 0.0d0

    !--Open common transport data file
    CALL This%CommonTransDataFile%New(FileName=TRIM(cInFileName),InputFile=.TRUE.,Descriptor='Transport common data file',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
     !--Read aquifer porosity data for solute transport simulation
    CALL ReadAquiferPorosity(This, AppGrid, NLayers, cWorkingDirectory,  iStat)
    IF (iStat .EQ. -1) RETURN
    
    !--Read option to specify run type (1=with flow simulation; 2=without flow simulation)
    CALL This%CommonTransDataFile%ReadData( RunType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (RunType==1) THEN
       This%lFlowSimulated=.TRUE.
    ELSE
        This%lFlowSimulated=.FALSE.
    ENDIF

    IF (.NOT. ASSOCIATED(This%Nodes%Velocity) ) THEN
        ALLOCATE(This%Nodes%Velocity)
        ALLOCATE(This%Nodes%Velocity%Vx(1:NNodes, 1:NLayers),This%Nodes%Velocity%Vy(1:NNodes, 1:NLayers),This%Nodes%Velocity%Vz(1:NNodes, 1:NLayers))
        This%Nodes%Velocity%Vx=0.0d00
        This%Nodes%Velocity%Vy=0.0d00
        This%Nodes%Velocity%Vz=0.0d00
        IF (.NOT.This%lFLowSimulated) THEN
            ALLOCATE(This%Nodes%Velocity%VxNext(1:NNodes, 1:NLayers),This%Nodes%Velocity%VyNext(1:NNodes, 1:NLayers),This%Nodes%Velocity%VzNext(1:NNodes, 1:NLayers))
            This%Nodes%Velocity%VxNext=0.0d00
            This%Nodes%Velocity%VyNext=0.0d00
            This%Nodes%Velocity%VzNext=0.0d00
        ENDIF
    END IF
     
    IF (.NOT.ASSOCIATED(This%Nodes%GWHead)) THEN
        ALLOCATE (This%Nodes%GWHead(NNodes))
    END IF
    
    !Set transport time variables similar to flow time variables. End time will be replaced later  
    This%TimeStep%TimeStepType = TimeStep    

    !--If flow is not simulated open saved velocity file and reads only header
    IF (.NOT. This%lFlowSimulated) THEN   ! Only transport model is simulated and read velocity from saved file
        !--Open saved velocity file and read away the header
        CALL OpenSavedVelocityFile(This, cWorkingDirectory, iStat)
        IF (iStat .EQ. -1) RETURN 
                
        !--Read velocity conversion factor
        CALL This%CommonTransDataFile%ReadData(This%Nodes%Velocity%ConvFact,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (This%Nodes%Velocity%ConvFact .LT. 0 ) THEN
            CALL SetLastMessage('Velocity conversion factor is less than zero!',f_iFatal,ThisProcedure)
            iStat=-1
            RETURN
        END IF
        
        !--Open saved velocity file and read away the header
        CALL OpenSavedHeadsFile(This, cWorkingDirectory, iStat)  ;  IF (iStat .EQ. -1) RETURN  
        
        CALL This%CommonTransDataFile%ReadData(This%HeadConvFact,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (This%HeadConvFact .LT. 0 ) THEN
            CALL SetLastMessage('Velocity conversion factor is less than zero!',f_iFatal,ThisProcedure)
            iStat=-1
            RETURN
        END IF
       
        !--Calculate the number of time steps of solute transport simulation
        This%TimeStep%NumFlowTimeSteps= NPeriods(TimeStep%DELTAT_InMinutes, This%TimeStep%CurrentDateAndTime, This%TimeStep%EndDateAndTime)

    END IF

  END SUBROUTINE InitializeDataReader

       
  ! -------------------------------------------------------------
  ! --- LOAD TIME SERIES DATA
  ! -------------------------------------------------------------    
  SUBROUTINE LoadTSData(This,  AppGrid, Stratigraphy, TimeStep, iStat)
    CLASS(TransportDataReaderType),INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)                :: TimeStep
    INTEGER,INTENT(OUT)                          :: iStat
        
    !--Load simulated  head data at each time step
    CALL LoadVelocityField(This, AppGrid, Stratigraphy, TimeStep, iStat)
    CALL LoadHeads(This, AppGrid, Stratigraphy, TimeStep,  iStat)
        
  END SUBROUTINE LoadTSData
      
  
  ! -------------------------------------------------------------
  ! --- LOAD VELOCITY DATA
  ! -------------------------------------------------------------    
  SUBROUTINE LoadVelocityField(This,  AppGrid, Stratigraphy, TimeStep, iStat)
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)          :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local variables
    CHARACTER :: Text*500
    
    !Initialize
    iStat = 0

    IF (TimeStep%CurrentTimeStep>This%TimeStep%NumFlowTimeSteps .AND. MOD(TimeStep%CurrentTimeStep, This%TimeStep%NumFlowTimeSteps)==1) THEN
        !-Start recycling  IWFM saved velocity and head data
        CALL EchoProgress('Flow simulation data file ended. Recycling velocity and head data from file...') 
        WRITE(Text,'(A)')  f_cLineFeed // '   *** END OF VELOCITY AND HEAD DATA. BEGIN RECYCLING SAVED VELOCITY & HEAD DATA'
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
        CALL LogMessage(' ',f_iMessage,'',f_iFILE) 
        CALL RewindVelTecplotFile(This%SavedVelocityFile, This%Nodes%Velocity%nLineSkip_TecplotVelFile,  iStat)   ;  IF (iStat .NE. 0) RETURN
    END IF
    IF (This%lBackwardTracking) THEN
        CALL ReadVelocityBackward(This, AppGrid, Stratigraphy, TimeStep, iStat );  IF (iStat .NE. 0) RETURN
    ELSE   !-Solute transport or forward particle tracking
        CALL ReadVelocityFromFile(This, AppGrid, Stratigraphy, TimeStep, iStat );  IF (iStat .NE. 0) RETURN
    END IF 
        
  END SUBROUTINE LoadVelocityField
 
 
  ! -------------------------------------------------------------
  ! --- REWIND TS OUTPUT FILE FOR RESTARTING
  ! -------------------------------------------------------------
  SUBROUTINE RewindVelTecplotFile(ThisFile, f_inLineSkip, iStat) 
    TYPE(GenericFileType) :: ThisFile
    INTEGER,INTENT(IN)    :: f_inLineSkip  
    INTEGER,INTENT(OUT)   :: iStat  
 
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'RewindVelTecplotFile'
    CHARACTER                    :: ALine*2000
    INTEGER                      :: I

    iStat=0

    !--Rewind the velocity file
     CALL ThisFile%RewindFile

     !--Read the header sections
     DO I=1, f_inLineSkip
         CALL ThisFile%ReadData(Aline,iStat)  ;  
         IF (iStat .EQ. -1) THEN
             CALL SetLastMessage('Error rewinding velocity file!',f_iFatal,ThisProcedure)
             RETURN
         ENDIF
     ENDDO

  END SUBROUTINE RewindVelTecplotFile
  

  ! -------------------------------------------------------------
  ! --- OPEN SAVED VELOCITY FILE FROM A PREVIOUS RUN
  ! --- Note: Opens velocity file and reads header sections
  ! -------------------------------------------------------------
  SUBROUTINE OpenSavedVelocityFile(This, cWorkingDirectory, iStat) 
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    CHARACTER(LEN=*),INTENT(IN)                 :: cWorkingDirectory
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'OpenSavedVelocityFile'
    INTEGER, PARAMETER           :: f_inline_header=3, f_innode_con=37171 
    INTEGER                      :: iNCol, f_inlines
    CHARACTER                    :: ALine*2000
    CHARACTER (LEN=150)          :: Line2, Line3
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    
    !-Initialize
    iNCol=1
    
    !--Velocity file saved from a previous run
    CALL This%CommonTransDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (This%SavedVelocityFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    
        CALL This%SavedVelocityFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='Saved velocity file',iStat=iStat)  
    
        IF (iStat .EQ. -1) RETURN
        IF (This%SavedVelocityFile%iGetFileType() .NE. f_iTXT) THEN
            CALL SetLastMessage('Saved velocity file must be a text file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE   
        CALL SetLastMessage('No velocity file name is specified!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF    
    
    !--Calculate the number of lines to skip until reaching velocity header 
    f_inlines=0
    DO 
        CALL This%SavedVelocityFile%ReadData(Aline,iStat)  ;  IF (iStat .EQ. -1) RETURN
        f_inlines=f_inlines+1
        IF (ALine(1:4)=='TEXT') THEN
             This%Nodes%Velocity%nLineSkip_TecplotVelFile=f_inlines-1
            EXIT
        ENDIF
    ENDDO
    
    !--Count the number of lines in the velocity blocks
    CALL This%SavedVelocityFile%ReadData(Line2,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (Line2(1:4) /= "ZONE") THEN
        CALL This%SavedVelocityFile%Kill()
        CALL SetLastMessage('Tecplot velocity file format error at the first time step!',f_iFatal,ThisProcedure)
        iStat=-1
        RETURN
    END IF  
    
    f_inlines=0
    DO 
        CALL This%SavedVelocityFile%ReadData(Line3,iStat)  ;  IF (iStat .EQ. -1) RETURN
        f_inlines=f_inlines+1
        IF (Line3(1:4)=='TEXT') THEN
            CALL This%SavedVelocityFile%BackspaceFile()
            This%Nodes%Velocity%nLine_TecplotVelBlock=f_inlines-1                  
            EXIT
        ENDIF
    END DO
    
    CALL This%SavedVelocityFile%BackspaceFile(This%Nodes%Velocity%nLine_TecplotVelBlock+2)

  END SUBROUTINE OpenSavedVelocityFile
  

  ! -------------------------------------------------------------
  ! --- READ VELOCITY FROM A PREVIOUSLY SAVED TECPLOT FORMAT FROM A PREVOIUS IWFM RUN
  ! --- *Note: Velocity data for the current and next time steps are read and saved 
  ! ---        for RK2 and RK4 time integration for particle tracking. For solute
  ! ---        transport only current time step is used in Euler integration
  ! -------------------------------------------------------------
  SUBROUTINE ReadVelocityFromFile(This, AppGrid, Stratigraphy, TimeStep, iStat) 
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)          :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep   
    INTEGER,INTENT(OUT)                         :: iStat  
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20)    :: ThisProcedure = ModName // 'ReadVelocityFromFile'
    INTEGER, PARAMETER              :: f_inlines=3621, f_inchnk=100, f_invel=362148      ! Parameters from IWFM velocity output file 
    INTEGER                         :: I, J,num_nodes, iend, nVelVals, nlay
    REAL (KIND=f_IKIND)             :: ConvFactVel
    REAL (KIND=f_IKIND), ALLOCATABLE:: tmpBarray(:), tmpVel(:)
    REAL (KIND=f_IKIND)             :: tmpArray(f_inchnk)
    CHARACTER (LEN=150)             :: Line1, Line2
    CHARACTER (LEN=20)              :: AString, Text*500
  
    !Initialize
    iStat = 0
    nVelVals=AppGrid%NNodes*Stratigraphy%NLayers*3
    ALLOCATE (tmpVel(nVelVals)); tmpVel=0.0d00
    
                

     !--Use velocity data read in the previous time step
     IF (TimeStep%CurrentTimeStep > 1 ) THEN
       This%Nodes%Velocity%Vx=This%Nodes%Velocity%VxNext
       This%Nodes%Velocity%Vy=This%Nodes%Velocity%VyNext
       This%Nodes%Velocity%Vz=This%Nodes%Velocity%VzNext
     ENDIF
    
    !--Start reading velocity data from file
    !-Read header
    CALL This%SavedVelocityFile%ReadData(Line1,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (Line1(1:4) /= "TEXT") THEN
        CALL This%SavedVelocityFile%Kill()
        WRITE (AString, '(A20)') TimeStep%CurrentTimeStep
        CALL SetLastMessage('Velocity read error at:  '//TRIM(AString)//' !',f_iFatal,ThisProcedure)
        iStat=-1
        RETURN
    END IF   
  
    
    CALL This%SavedVelocityFile%ReadData(Line2,iStat)  ;  IF (iStat .EQ. -1) RETURN
    WRITE(Text, ' (A3, A147) ') "!--", Line2 
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
  
    tmpVel=0.0d0   
    !-Read all velocity data until the last line of the block
    j=1
    DO i=1,This%Nodes%Velocity%nLine_TecplotVelBlock-1
        CALL This%SavedVelocityFile%ReadData(tmpArray,iStat)  ;  IF (iStat .EQ. -1) RETURN           
        tmpVel(j:j+f_inchnk-1)=tmpArray
        j=j+f_inchnk
    ENDDO
    !-Read the last line of the block
    iend=(nVelVals-j+1)

    ALLOCATE (tmpBarray(1:iend)); tmpBarray=0.0d00
    CALL This%SavedVelocityFile%ReadData(  tmpBarray  ,iStat)  ;  IF (iStat .EQ. -1) RETURN       
    tmpVel(j:nVelVals)=tmpBarray
    
    num_nodes = AppGrid%NNodes
    
    ConvFactVel=This%Nodes%Velocity%ConvFact
    nlay=Stratigraphy%NLayers
    DO i=1,nlay
        This%Nodes%Velocity%VxNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-3)+1:num_nodes*(3*i-2))*ConvFactVel
        This%Nodes%Velocity%VyNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-2)+1:num_nodes*(3*i-1))*ConvFactVel
        This%Nodes%Velocity%VzNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-1)+1:num_nodes*(3*i))*ConvFactVel
    ENDDO  

    !-Caculate pore velocity 
    This%Nodes%Velocity%VxNext=This%Nodes%Velocity%VxNext/This%Nodes%Porosity
    This%Nodes%Velocity%VyNext=This%Nodes%Velocity%VyNext/This%Nodes%Porosity
    This%Nodes%Velocity%VzNext=This%Nodes%Velocity%VzNext/This%Nodes%Porosity
    
    
    !--Read velocity in the next time step only during the first time step 
    IF (TimeStep%CurrentTimeStep ==1 ) THEN
        
        !-First save velocity data read for the 1st time step as current velocities
        This%Nodes%Velocity%Vx=This%Nodes%Velocity%VxNext
        This%Nodes%Velocity%Vy=This%Nodes%Velocity%VyNext
        This%Nodes%Velocity%Vz=This%Nodes%Velocity%VzNext

        !-Read header
        CALL This%SavedVelocityFile%ReadData(Line1,iStat);  IF (iStat .EQ. -1) RETURN
        IF (Line1(1:4) /= "TEXT") THEN
            CALL This%SavedVelocityFile%Kill()
            WRITE (AString, '(A20)') TimeStep%CurrentTimeStep
            CALL SetLastMessage('Velocity read error at:  '//TRIM(AString)//' !',f_iFatal,ThisProcedure)
            iStat=-1
            RETURN
        END IF   
  
    
        CALL This%SavedVelocityFile%ReadData(Line2,iStat)  ;  IF (iStat .EQ. -1) RETURN
        WRITE(Text, ' (A3, A147) ') "!--", Line2 
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
  
        tmpVel=0.0d0   
        !-Read all velocity data until the last line of the block
        j=1
        DO i=1,This%Nodes%Velocity%nLine_TecplotVelBlock-1
            CALL This%SavedVelocityFile%ReadData(tmpArray,iStat)  ;  IF (iStat .EQ. -1) RETURN           
            tmpVel(j:j+f_inchnk-1)=tmpArray
            j=j+f_inchnk
        ENDDO
        !-Read the last line of the block
        iend=(nVelVals-j+1)

        tmpBarray=0.0d00
        CALL This%SavedVelocityFile%ReadData(  tmpBarray  ,iStat)  ;  IF (iStat .EQ. -1) RETURN       
        tmpVel(j:nVelVals)=tmpBarray
    
        DO i=1, nlay
            This%Nodes%Velocity%VxNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-3)+1: num_nodes*(3*i-2))*ConvFactVel
            This%Nodes%Velocity%VyNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-2)+1: num_nodes*(3*i-1))*ConvFactVel
            This%Nodes%Velocity%VzNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-1)+1: num_nodes*(3*i))*ConvFactVel
        ENDDO   
        
        !-Caculate pore velocity 
        This%Nodes%Velocity%VxNext=This%Nodes%Velocity%VxNext/This%Nodes%Porosity
        This%Nodes%Velocity%VyNext=This%Nodes%Velocity%VyNext/This%Nodes%Porosity
        This%Nodes%Velocity%VzNext=This%Nodes%Velocity%VzNext/This%Nodes%Porosity
      
        
    ENDIF
    

    DEALLOCATE (tmpBarray)
    
  END SUBROUTINE ReadVelocityFromFile
  
     
  ! -------------------------------------------------------------
  ! --- READ VELOCITY FROM A PREVIOUSLY SAVED TECPLOT FORMAT FROM A PREVOIUS IWFM RUN
  ! --- * Note: Read velocity data starting from the end of the simulation period
  ! ---         until the begining of the simulation period to be used in backward 
  ! ---         particle tracking
  ! -------------------------------------------------------------
  SUBROUTINE ReadVelocityBackward(This, AppGrid, Stratigraphy, TimeStep, iStat) 
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)          :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep   
    INTEGER,INTENT(OUT)                         :: iStat  

    
    !Local variables
    CHARACTER(LEN=ModNameLen+20)    :: ThisProcedure = ModName // 'ReadVelocityBackward'
    INTEGER, PARAMETER              ::  f_inchnk=100 !,  nLineSkip_TecplotVelFile=67354     ! Parameters from IWFM velocity Tecplot output file 
    INTEGER                         :: I, J, num_nodes, iend, CurrentTimeStep, nVelVals, nlay, iT
    REAL (KIND=f_IKIND)             :: ConvFactVel
    REAL (KIND=f_IKIND), ALLOCATABLE:: tmpBarray(:), tmpVel(:)
    REAL (KIND=f_IKIND)             :: tmpArray(f_inchnk)
    CHARACTER (LEN=150)             :: Line1, Line2, Line3
    CHARACTER (LEN=20)              :: AString , Text*500
  
    !Initialize
    iStat = 0
    nVelVals=AppGrid%NNodes*Stratigraphy%NLayers*3
    
    ALLOCATE (tmpVel(nVelVals)); tmpVel=0.0d00
    
     CurrentTimeStep=MOD(TimeStep%CurrentTimeStep-1, This%TimeStep%NumFlowTimeSteps)+1
     
    !-First skip velocity data until reaching the appropriate time step
     IF (MOD(TimeStep%CurrentTimeStep, This%TimeStep%NumFlowTimeSteps)==1) THEN
        DO J=1, This%TimeStep%NumFlowTimeSteps - CurrentTimeStep-1
            CALL This%SavedVelocityFile%ReadData(Line1,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL This%SavedVelocityFile%ReadData(Line2,iStat)  ;  IF (iStat .EQ. -1) RETURN
                DO 
                    CALL This%SavedVelocityFile%ReadData(Line3,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    IF (Line3(1:4)=='TEXT') THEN
                        CALL This%SavedVelocityFile%BackspaceFile()
                        EXIT
                    ENDIF
                END DO
        END DO
     END IF 
     

    !-- Read velocity for the two last time steps for Runge Kutta time integration  
    DO iT=1,2
     
        !-Read header
        CALL This%SavedVelocityFile%ReadData(Line1,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (Line1(1:4) /= "TEXT") THEN
            CALL This%SavedVelocityFile%Kill()
            WRITE (AString, '(A20)') TimeStep%CurrentTimeStep
            CALL SetLastMessage('Velocity read error at:  '//TRIM(AString)//' !',f_iFatal,ThisProcedure)
            iStat=-1
            RETURN
        END IF   
    
        CALL This%SavedVelocityFile%ReadData(Line2,iStat)  ;  IF (iStat .EQ. -1) RETURN
        WRITE(Text, ' (A3, A147) ') "!--", Line2 
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
    
        tmpVel=0.0d0   
        !-Read all velocity data until the last line of the block
        j=1
        DO i=1,This%Nodes%Velocity%nLine_TecplotVelBlock-1
            CALL This%SavedVelocityFile%ReadData(tmpArray,iStat)  ;  IF (iStat .EQ. -1) RETURN           
            tmpVel(j:j+f_inchnk-1)=tmpArray
            j=j+f_inchnk
        ENDDO
        !-Read the last line of the block
        iend=(nVelVals-j+1)
        
        IF (.NOT.ALLOCATED (tmpBarray)) THEN 
            ALLOCATE (tmpBarray(1:iend))
        ENDIF 
  
        tmpBarray=0.0d00
        CALL This%SavedVelocityFile%ReadData(  tmpBarray  ,iStat)  ;  IF (iStat .EQ. -1) RETURN       
        tmpVel(j:nVelVals)=tmpBarray
    
        num_nodes= AppGrid%NNodes
    
        ConvFactVel=This%Nodes%Velocity%ConvFact
        !-For backward tracking use the opposite sign of the velocity
        nlay=Stratigraphy%NLayers
        IF (iT==1) THEN
            DO i=1, nlay
                This%Nodes%Velocity%VxNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-3) +1: num_nodes*(3*i-2) )*(-1)*ConvFactVel
                This%Nodes%Velocity%VyNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-2) +1: num_nodes*(3*i-1) )*(-1)*ConvFactVel
                This%Nodes%Velocity%VzNext(1:num_nodes,i)=tmpVel(num_nodes*(3*i-1) +1: num_nodes*(3*i)  )*(-1)*ConvFactVel
            ENDDO
    
            !-Caculate pore velocity 
            This%Nodes%Velocity%VxNext=This%Nodes%Velocity%VxNext/This%Nodes%Porosity
            This%Nodes%Velocity%VyNext=This%Nodes%Velocity%VyNext/This%Nodes%Porosity
            This%Nodes%Velocity%VzNext=This%Nodes%Velocity%VzNext/This%Nodes%Porosity
        
        ELSE
            DO i=1, nlay
                This%Nodes%Velocity%Vx(1:num_nodes,i)=tmpVel(num_nodes*(3*i-3) +1: num_nodes*(3*i-2) )*(-1)*ConvFactVel
                This%Nodes%Velocity%Vy(1:num_nodes,i)=tmpVel(num_nodes*(3*i-2) +1: num_nodes*(3*i-1) )*(-1)*ConvFactVel
                This%Nodes%Velocity%Vz(1:num_nodes,i)=tmpVel(num_nodes*(3*i-1) +1: num_nodes*(3*i)  )*(-1)*ConvFactVel
            ENDDO
            !-Caculate pore velocity 
            This%Nodes%Velocity%Vx=This%Nodes%Velocity%Vx/This%Nodes%Porosity
            This%Nodes%Velocity%Vy=This%Nodes%Velocity%Vy/This%Nodes%Porosity
            This%Nodes%Velocity%Vz=This%Nodes%Velocity%Vz/This%Nodes%Porosity
        ENDIF
    ENDDO
    
    
    DEALLOCATE (tmpBarray)
    
    CALL This%SavedVelocityFile%BackspaceFile( (This%Nodes%Velocity%nLine_TecplotVelBlock+2)*3 )
    
  END SUBROUTINE ReadVelocityBackward    

  
  ! -------------------------------------------------------------
  ! --- LOAD HEAD DATA
  ! -------------------------------------------------------------    
  SUBROUTINE LoadHeads (This,  AppGrid, Stratigraphy, TimeStep, iStat)
    TYPE(TransportDataReaderType),  INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)                  :: AppGrid
    TYPE(StratigraphyType), INTENT(IN)            :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)                 :: TimeStep
    INTEGER,INTENT(OUT)                           :: iStat
    
    !Local variables
    CHARACTER :: Text*500
    
    !Initialize
    iStat=0
    
    IF ( This%lSolTransDefined) THEN
            IF (TimeStep%CurrentTimeStep>This%TimeStep%NumFlowTimeSteps .AND. MOD(TimeStep%CurrentTimeStep, This%TimeStep%NumFlowTimeSteps)==1) THEN
                    This%SavedHeadsFile%NumberOfBlocksToSkip=0
                    CALL  This%SavedHeadsFile%Rewind_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
                    WRITE(Text, ' (A) ') " "
                    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
                    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
             END IF
            CALL ReadHeadsFromFile(This, AppGrid, Stratigraphy,  TimeStep, iStat) 
            IF (iStat .EQ. -1) RETURN
    END IF        
      
  END SUBROUTINE LoadHeads

  
  ! -------------------------------------------------------------
  ! --- OPEN SAVED HEADS FILE FROM A PREVIOUS RUN
  ! -------------------------------------------------------------
  SUBROUTINE OpenSavedHeadsFile(This, cWorkingDirectory, iStat) 
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    CHARACTER(LEN=*),INTENT(IN)                 :: cWorkingDirectory
    INTEGER,INTENT(OUT)                         :: iStat
  
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'OpenSavedHeadsFile'
    INTEGER, PARAMETER           :: f_inline_header=3, f_innode_con=37171, f_inLineSkip_HeadFile=6
    INTEGER                      :: iNCol,FileOpenCode
    CHARACTER                    :: ALine*2000
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
  
    !-Initialize
    iNCol=1
  
    !--Heads file saved from a previous run
    CALL This%CommonTransDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)  
        FileOpenCode=0
        ALLOCATE (AsciiTSDInFileType :: This%SavedHeadsFile)
        CALL This%SavedHeadsFile%New(cAbsPathFileName,lInputFile=.TRUE.,FileOpenCode=FileOpenCode,iStat=iStat)
        IF (iStat .EQ. -1) THEN
           CALL SetLastMessage('Could not open saved head file!',f_iFatal,ThisProcedure)
           RETURN
        ENDIF
    END IF
    
  END SUBROUTINE OpenSavedHeadsFile
  

  ! -------------------------------------------------------------
  ! --- READ SAVED HEADS FILE FROM A PREVIOUS RUN
  ! --- For now only layer 1 head is read to calculate concentration in saturated zone
  ! --- Heads in all layers will be required for tightly coupled modeling
  ! -------------------------------------------------------------
  SUBROUTINE ReadHeadsFromFile(This, AppGrid, Stratigraphy,  TimeStep, iStat) 
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)               :: TimeStep   
    INTEGER,INTENT(OUT)                         :: iStat  
        
    !-Local variables
    INTEGER, PARAMETER                :: days(12) = [  31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
    INTEGER                           :: I, FileReadCode, forcing_index, month, year, DaysInMonth, start_month , start_year
    REAL (KIND=f_IKIND)               :: tmpArray(1,AppGrid%NNodes*Stratigraphy%NLayers)
    CHARACTER(LEN=f_iTimeStampLength) :: CurrentDateAndTime = ''

        
    iStat=0
        
    IF (TimeStep%CurrentTimeStep==1) THEN
        READ( TimeStep%CurrentDateAndTime(1:2), *) start_month
        READ ( TimeStep%CurrentDateAndTime(7:10), *) start_year
    END IF
        
    tmpArray(1,:)=0.0d00
    IF (.NOT. ASSOCIATED( This%Nodes%GWHead)) THEN
        ALLOCATE ( This%Nodes%GWHead(AppGrid%NNodes*Stratigraphy%NLayers))
    END IF
    This%Nodes%GWHead(:)=0.0d00
        
    !-Reset time stamp to look when data is recycled
    IF (  This%TimeStep%CurrentTimeStep> This%TimeStep%NumFlowTimeSteps) THEN
        READ( This%TimeStep%StartDateAndTime(1:2), *) start_month
        READ (This%TimeStep%StartDateAndTime(7:10), *) start_year
        forcing_index = MOD(TimeStep%CurrentTimeStep-1, This%TimeStep%NumFlowTimeSteps) + 1
        month = MOD(start_month - 1 + forcing_index - 1, 12) + 1
        year  = start_year + (start_month - 1 + forcing_index - 1) / 12
        DaysInMonth = days(month)
        WRITE(CurrentDateAndTime,'(I2.2,"/",I2.2,"/",I4.4,"_24:00")') month,DaysInMonth ,year
        CurrentDateAndTime= LeapYearCorrection(CurrentDateAndTime)
    ELSE 
        CurrentDateAndTime= TimeStep%CurrentDateAndTime
    END IF
    CALL This%SavedHeadsFile%ReadData(CurrentDateAndTime,tmpArray,FileReadCode,iStat,TimeStep%TrackTime)
    IF (iStat .EQ. -1) RETURN

       
    DO I =1, AppGrid%NNodes
            This%Nodes%GWHead(I) =tmpArray(1,I)
    END DO
        
  END SUBROUTINE ReadHeadsFromFile
    
 
  ! -------------------------------------------------------------
  ! --- OPEN AQUIFER PARAMETER FILE FOR TRANSPORT
  ! -------------------------------------------------------------
  SUBROUTINE OpenAquiferParameterFile(This, InFile, cWorkingDirectory, Property, iStat) 
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    TYPE(GenericFileType), INTENT  (INOUT)      :: InFile  
    CHARACTER(LEN=*),INTENT(IN)                 :: cWorkingDirectory, Property
    INTEGER,INTENT(OUT)                         :: iStat
  
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'OpenAquiferParameterFile'
    INTEGER                      :: iNCol
    CHARACTER                    :: ALine*2000, Text*200
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
  
    !-Initialize
    iNCol=1
  
    !--Aquifer parameter for solute transport
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
            
    IF (ALine .NE. '') THEN
                
        SELECT CASE (TRIM(PROPERTY))
                    
        CASE ("Porosity")
            IF (.NOT. ALLOCATED(This%PorosityFile) ) ALLOCATE (This%PorosityFile)
                CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
                CALL This%PorosityFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='Transport aquifer porosity file',iStat=iStat)  
                IF (iStat .EQ. -1) RETURN
                IF (This%PorosityFile%iGetFileType() .NE. f_iTXT) THEN
                    CALL SetLastMessage('Transport aquifer porosity file must be a text file!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
            END IF

        CASE ("Dispersivity") 
                        IF (.NOT. ALLOCATED(This%DispersivityFile)) ALLOCATE (This%DispersivityFile)
                        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
                        CALL This%DispersivityFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='Transport aquifer dispersivity file',iStat=iStat)  
                        IF (iStat .EQ. -1) RETURN
                        IF (This%DispersivityFile%iGetFileType() .NE. f_iTXT) THEN
                            CALL SetLastMessage('Solute transport dispersivity file must be a text file!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF
                                
        CASE ("InitialConcentration") 
                        IF (.NOT. ALLOCATED(This%InitialConcentrationFile)) ALLOCATE (This%InitialConcentrationFile)
                        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
                        CALL This%InitialConcentrationFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='Transport aquifer dispersivity file',iStat=iStat)  
                        IF (iStat .EQ. -1) RETURN
                        IF (This%InitialConcentrationFile%iGetFileType() .NE. f_iTXT) THEN
                            CALL SetLastMessage('Solute transport initial concentration file must be a text file!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF                    
                                
            CASE DEFAULT
                    WRITE(Text, ' (A) ') "!--Unknown transport property to read! "
                    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE) 
                    iStat = -1
                    RETURN
                          
        END SELECT
    ELSE
        CALL SetLastMessage('No aquifer property file name is specified for '//TRIM(ADJUSTL(PROPERTY)) //'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN 
    END IF   
    
  END  SUBROUTINE OpenAquiferParameterFile
      

  ! -------------------------------------------------------------
  ! --- READ AQUIFER PROSITY DATA FOR SOLUTE TRANSPORT
  ! -------------------------------------------------------------                                                                         
  SUBROUTINE ReadAquiferPorosity( This, AppGrid, NLayers, cWorkingDirectory,  iStat)
    TYPE(TransportDataReaderType),INTENT(INOUT) :: This
    INTEGER,INTENT(IN)                          :: NLayers
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    INTEGER,INTENT(OUT)                         :: iStat
    CHARACTER(LEN=*),INTENT(IN)                 :: cWorkingDirectory 

    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'ReadAquiferPorosity'
    INTEGER                      :: AqParamOption, indxNode,indxLayer,ID,NNodes,index,         &
                                    NodeIDs(AppGrid%NNodes) 
    REAL(KIND=f_IKIND)           :: rDummyArray(2), rFactor
    LOGICAL                      :: lProcessed(AppGrid%NNodes)
        
    !Initialize
    iStat   = 0
        
    !Inform user
    CALL EchoProgress(' Reading aquifer parameters for transport...')
        
    !Initialize
    NNodes     = AppGrid%NNodes
    NodeIDs    = AppGrid%AppNode%ID
    lProcessed = .FALSE.
        
    !Open porosity file
    CALL OpenAquiferParameterFile ( This, This%CommonTransDataFile, cWorkingDirectory, "Porosity", iStat )
    IF (iStat .NE. 0) RETURN

        
    !Read option to specify porosity 
    CALL This%PorosityFile%ReadData( AqParamOption,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AqParamOption==1) THEN
        This%Nodes%Constant=.TRUE.
    ELSE
        This%Nodes%Constant=.FALSE.
    ENDIF
        
        
    !Conversion factors
    CALL This%PorosityFile%ReadData(rFactor,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
    !Non-parametric data input-constant value for each layer
    IF (AqParamOption == 1) THEN
        
        DO indxLayer=1,NLayers
            CALL This%PorosityFile%ReadData(rDummyArray(1:2),iStat)  ;  IF (iStat .EQ. -1) RETURN
            This%Nodes%Porosity(:,indxLayer) = rDummyArray(2) * rFactor
        ENDDO
        
        !Non-parametric data input-specify parameters at each node 
        ELSE IF (AqParamOption  == 2) THEN
            DO indxNode=1,NNodes
                DO indxLayer=1,NLayers
                IF (indxLayer .EQ. 1) THEN
                    CALL This%PorosityFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            
                    !Check that node ID is legit
                    ID = INT(rDummyArray(1))  
                    CALL ConvertID_To_Index(ID,NodeIDs,index)
                    IF (index .EQ. 0) THEN
                        CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for aquifer parameters is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
            
                    !Check that node is not listed more than once
                    IF (lProcessed(index)) THEN
                        CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for aquifer parameter entry!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
            
                    !Tag node as processed
                    lProcessed(index) = .TRUE.
                ELSE
                    CALL This%PorosityFile%ReadData(rDummyArray(1:2),iStat)  ;  IF (iStat .EQ. -1) RETURN
                END IF
                This%Nodes%Porosity(index,indxLayer) = rDummyArray(2) * rFactor
                END DO
            END DO
    
            !Check all nodes are processed
            DO indxNode=1,NNodes
                IF (.NOT. lProcessed(indxNode)) THEN
                    CALL SetLastMessage('Aquifer porosity is not defined at node '//TRIM(IntToText(NodeIDs(indxNode)))//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
    END IF

  END SUBROUTINE ReadAquiferPorosity
                
     
  ! -------------------------------------------------------------
  ! --- READ AQUIFER DISPERSIVITY DATA FOR SOLUTE TRANSPORT
  ! -------------------------------------------------------------
  SUBROUTINE ReadAquiferDispersivity (This, AppGrid, InFile,NLayers, cWorkingDirectory, iStat )
    CLASS(TransportDataReaderType),INTENT(INOUT) :: This
    INTEGER,INTENT(IN)                           :: NLayers
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(GenericFileType),INTENT(INOUT)          :: InFile
    INTEGER,INTENT(OUT)                          :: iStat
    CHARACTER(LEN=*),INTENT(IN)                  :: cWorkingDirectory 

    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'ReadAquiferDispersivity'
    INTEGER                      :: AqParamOption, indxNode,indxLayer,ID,NNodes,index,         &
                                    NodeIDs(AppGrid%NNodes), LocMax(2) 
    REAL(KIND=f_IKIND)           :: rDummyArray(2),rFactor,LngDispMax
    REAL(KIND=f_IKIND)           :: RatioHorzTransLong,RatioVertTransLong 
    LOGICAL                      :: lProcessed(AppGrid%NNodes)
    
    !Initialize
    iStat   = 0
    
    !Inform user
    CALL EchoProgress('   Reading aquifer parameters for solute transport...')
    
    !Initialize
    NNodes     = AppGrid%NNodes
    NodeIDs    = AppGrid%AppNode%ID
    lProcessed = .FALSE.
    
    !Open Dispersivity file
    CALL OpenAquiferParameterFile ( This, InFile, cWorkingDirectory, "Dispersivity", iStat )
    IF (iStat .NE. 0) RETURN

    !Read option to specify  dispersivity
    CALL This%DispersivityFile%ReadData(AqParamOption,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Conversion factors
    CALL This%DispersivityFile%ReadData(rFactor,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Non-parametric data input-constant value for each layer
    IF (AqParamOption  == 1) THEN
         DO indxLayer=1,NLayers
              CALL This%DispersivityFile%ReadData(rDummyArray(1:2),iStat)  ;  IF (iStat .EQ. -1) RETURN
              This%Nodes%LongDispersivity(:,indxLayer) = rDummyArray(2) * rFactor
         ENDDO
         
    !Non-parametric data input-specify parameters at each node 
    ELSE IF (AqParamOption  == 2) THEN
        DO indxNode=1,NNodes
          DO indxLayer=1,NLayers
            IF (indxLayer .EQ. 1) THEN
                CALL This%DispersivityFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Check that node ID is legit
                ID = INT(rDummyArray(1))  
                CALL ConvertID_To_Index(ID,NodeIDs,index)
                IF (index .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for aquifer parameters is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Check that node is not listed more than once
                IF (lProcessed(index)) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for aquifer parameter entry!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Tag node as processed
                lProcessed(index) = .TRUE.
            ELSE
                CALL This%DispersivityFile%ReadData(rDummyArray(1:2),iStat)  ;  IF (iStat .EQ. -1) RETURN
            END IF
            This%Nodes%LongDispersivity(index,indxLayer) = rDummyArray(2) * rFactor
          END DO
        END DO
        
        !Check all nodes are processed
        DO indxNode=1,NNodes
            IF (.NOT. lProcessed(indxNode)) THEN
                CALL SetLastMessage('Aquifer parameters are not defined at node '//TRIM(IntToText(NodeIDs(indxNode)))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    
    LngDispMax=MAXVAL(This%Nodes%LongDispersivity(:,:))
    LocMax=MAXLOC(This%Nodes%LongDispersivity(:,:))
    
    !-Read ratio of  RatioHorTransLong
    CALL This%DispersivityFile%ReadData(RatioHorzTransLong,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (RatioHorzTransLong .EQ. 0) THEN
        CALL InFile%Kill() 
        RETURN
    END IF
    This%Nodes%RatioHorzTransLong=RatioHorzTransLong
      
    !-Read ratio of  RatioVerTransLong
    CALL This%DispersivityFile%ReadData(RatioVertTransLong,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (RatioVertTransLong .EQ. 0) THEN
        CALL InFile%Kill() 
        RETURN
    END IF
    This%Nodes%RatioVertTransLong=RatioVertTransLong
    
  END SUBROUTINE ReadAquiferDispersivity    
    

  ! -------------------------------------------------------------
  ! --- READ INITIAL CONCENTRATION FOR SOLUTE TRANSPORT
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialConcentration(This, AppGrid, InFile,NLayers, cWorkingDirectory, iStat )
    CLASS(TransportDataReaderType),INTENT(INOUT) :: This
    INTEGER,INTENT(IN)                           :: NLayers
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(GenericFileType),INTENT(INOUT)          :: InFile
    INTEGER,INTENT(OUT)                          :: iStat
    CHARACTER(LEN=*),INTENT(IN)                  :: cWorkingDirectory 
  
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ReadInitialConcentration'
    INTEGER                      :: AqParamOption, indxNode,indxLayer,ID,NNodes,index,         &
                                    NodeIDs(AppGrid%NNodes)
    REAL(KIND=f_IKIND)           :: rDummyArray(2),rFactor 
    LOGICAL                      :: lProcessed(AppGrid%NNodes)
  
    
    !Initialize
    iStat   = 0
    
    !Inform user
    CALL EchoProgress('   Reading aquifer parameters for solute transport...')
    
    !Initialize
    NNodes     = AppGrid%NNodes
    NodeIDs    = AppGrid%AppNode%ID
    lProcessed = .FALSE.
    
  
    !Open initial concentration files
    CALL OpenAquiferParameterFile ( This, InFile, cWorkingDirectory, "InitialConcentration", iStat )
    IF (iStat .NE. 0) RETURN
     
    !Read option to specify  concentration
    CALL This%InitialConcentrationFile%ReadData(AqParamOption,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Conversion factors
    CALL This%InitialConcentrationFile%ReadData(rFactor,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Non-parametric data input-constant value for each layer
    IF (AqParamOption  == 1) THEN
         DO indxLayer=1,NLayers
              CALL This%InitialConcentrationFile%ReadData(rDummyArray(1:2),iStat)  ;  IF (iStat .EQ. -1) RETURN
              This%Nodes%InitialConcentration(:,indxLayer) = rDummyArray(2) * rFactor
         ENDDO      
    !Non-parametric data input-specify parameters at each node 
    ELSE IF (AqParamOption  == 2) THEN
        DO indxNode=1,NNodes
          DO indxLayer=1,NLayers
            IF (indxLayer .EQ. 1) THEN
                CALL This%InitialConcentrationFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Check that node ID is legit
                ID = INT(rDummyArray(1))  
                CALL ConvertID_To_Index(ID,NodeIDs,index)
                IF (index .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for initial concentration is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Check that node is not listed more than once
                IF (lProcessed(index)) THEN
                    CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for initial concentration entry!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Tag node as processed
                lProcessed(index) = .TRUE.
            ELSE
                CALL This%InitialConcentrationFile%ReadData(rDummyArray(1:2),iStat)  ;  IF (iStat .EQ. -1) RETURN
            END IF
            This%Nodes%InitialConcentration(index,indxLayer) = rDummyArray(2) * rFactor
          END DO
        END DO
        
        !Check all nodes are processed
        DO indxNode=1,NNodes
            IF (.NOT. lProcessed(indxNode)) THEN
                CALL SetLastMessage('Initial concentration is not defined at node '//TRIM(IntToText(NodeIDs(indxNode)))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
  
  END SUBROUTINE ReadInitialConcentration

  
  ! -------------------------------------------------------------
  ! --- STRIP LEADING DELIMITERS TO HANDLE ALL DELIMITER COMBINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE StripLeadingDelimiters(cLine)
    CHARACTER(LEN=*), INTENT(INOUT) :: cLine
    
    DO WHILE (LEN_TRIM(cLine) .GT. 0)
        SELECT CASE (cLine(1:1))
            CASE (' ', ',')
                cLine = cLine(2:)
            CASE (CHAR(9))          
                cLine = cLine(2:)
            CASE DEFAULT
                RETURN
        END SELECT
    END DO

  END SUBROUTINE StripLeadingDelimiters
  
END MODULE  TransportDataReader