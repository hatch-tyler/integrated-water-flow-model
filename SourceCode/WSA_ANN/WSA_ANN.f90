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
MODULE WSA_ANN
  USE MessageLogger               , ONLY: SetLastMessage                 , &
                                          LogMessage                     , &
                                          MessageArray                   , &
                                          f_iWarn                        , &
                                          f_iFatal                       , &
                                          f_iSCREEN_FILE
  USE GeneralUtilities            , ONLY: LocateInList                   , &
                                          ShellSort                      , &   
                                          IntToText                      , &
                                          CleanSpecialCharacters         , &
                                          StripTextUntilCharacter        , &
                                          ReplaceString                  , &
                                          ArrangeText                    , & 
                                          PrepareTitle                   , & 
                                          FirstLocation                  , &
                                          UpperCase                      , &
                                          EstablishAbsolutePathFileName  , &
                                          GetUniqueArrayComponents       , &
                                          f_cLineFeed, &
                                   f_cInlineCommentChar
  USE TimeSeriesUtilities         , ONLY: TimeStepType                   , &
                                          ExtractMonth
  USE GenericLinkedList           , ONLY: GenericLinkedListType
  USE IOInterface                 , ONLY: GenericFileType                , &
                                          RealTSDataInFileType           , &
                                          PrepareTSDOutputFile           , &
                                          f_iDSS
  USE Package_Misc                , ONLY: f_iLocationType_StrmNode       , &
                                          f_iLocationType_Element        , &
                                          f_iLocationType_Subregion      , &
                                          f_iLocationType_Node           , &
                                          f_iLocationType_StrmReach 
  USE Package_Discretization      , ONLY: AppGridType
  USE Package_AppStream           , ONLY: AppStreamType             
  USE Package_RootZone            , ONLY: RootZoneType
  USE Package_AppGW               , ONLY: AppGWType
  USE Package_ComponentConnectors , ONLY: StrmGWConnectorType
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
  ! --- PUBLIC VARIABLES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC  :: WSA_ANN_Type
  
  
  ! -------------------------------------------------------------
  ! --- VARIABLE TYPE FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iNVarTypes                  = 14 
  INTEGER,PARAMETER :: f_iVarType_GenericTS          = 1 , &
                       f_iVarType_Season             = 2 , &
                       f_iVarType_SurfaceInflow      = 3 , &
                       f_iVarType_Runoff             = 4 , &
                       f_iVarType_Return             = 5 , &
                       f_iVarType_PondDrain          = 6 , &
                       f_iVarType_GainFromGW         = 7 , &
                       f_iVarType_DiversionNetBypass = 8 , &
                       f_iVarType_DeepPercRecharge   = 9 , &
                       f_iVarType_Pumping            = 10, &
                       f_iVarType_Precip             = 11, &
                       f_iVarType_ETa                = 12, &
                       f_iVarType_AgUrbArea          = 13, &
                       f_iVarType_NVRVArea           = 14
  INTEGER,PARAMETER :: f_iVarTypeList(f_iNVarTypes) = [f_iVarType_GenericTS          , &
                                                       f_iVarType_Season             , &
                                                       f_iVarType_SurfaceInflow      , &
                                                       f_iVarType_Runoff             , &
                                                       f_iVarType_Return             , &
                                                       f_iVarType_PondDrain          , &
                                                       f_iVarType_GainFromGW         , &
                                                       f_iVarType_DiversionNetBypass , &
                                                       f_iVarType_DeepPercRecharge   , &
                                                       f_iVarType_Pumping            , &
                                                       f_iVarType_Precip             , &
                                                       f_iVarType_ETa                , &
                                                       f_iVarType_AgUrbArea          , &
                                                       f_iVarType_NVRVArea           ]
  CHARACTER(LEN=17),PARAMETER :: f_cDSSCParts(f_iNVarTypes) = ['GENERIC_TS_VAR'    , &
                                                               'SEASON'            , &
                                                               'SURFACE_INFLOW'    , &
                                                               'RUNOFF'            , &
                                                               'RETURN_FLOW'       , &
                                                               'POND_DRN'          , &
                                                               'GAIN_FROM_GW'      , &
                                                               'DIVER_NETBYPASS'   , &
                                                               'DEEPPERC_RECHARGE' , &
                                                               'PUMPING'           , &
                                                               'PRECIPITATION'     , &
                                                               'ETA'               , &
                                                               'AG_URB_AREA'       , &
                                                               'NVRV_AREA'         ]
  
  
  ! -------------------------------------------------------------
  ! --- ALLOWED LOCATION TYPE LIST
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iLocationTypeList(5) = [f_iLocationType_StrmNode  , &
                                                 f_iLocationType_Element   , &
                                                 f_iLocationType_Subregion , &
                                                 f_iLocationType_Node      , &
                                                 f_iLocationType_StrmReach ]
                           
  
  ! -------------------------------------------------------------
  ! --- ANN NO-SCALING FLAG
  ! -------------------------------------------------------------
  REAL(8),PARAMETER :: f_rNoScaling = -999.0    
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS DESCRIBING MODEL RUN MODE
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iHistoricalRun = 1 , &
                       f_iANNRun        = 2
                       
                       
  ! -------------------------------------------------------------
  ! --- FLAGS DESCRIBING ACTIVATION FUNCTIONS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iAct_None    = 0 , &
                       f_iAct_ReLU    = 1 , &
                       f_iAct_Sigmoid = 2
                       
                       
  ! -------------------------------------------------------------
  ! --- VARIABLE AGGREGATION DATA TYPE
  ! -------------------------------------------------------------
  TYPE VarAggregationType
      PRIVATE
      INTEGER             :: iVarIndex
      INTEGER             :: iLocationType = f_iLocationType_StrmNode
      INTEGER             :: iNLocations   = 0 
      INTEGER,ALLOCATABLE :: iLocations(:)  
  END TYPE VarAggregationType
  
  
  ! -------------------------------------------------------------
  ! --- HISTORICAL STREAM FLOW INPUT FILE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: HistStrmFlowFileType
      REAL(8)             :: rFactor          = 1.0
      INTEGER,ALLOCATABLE :: iStrmFlowCols(:)                        !Column numbers in the historical stream flow input data for each WSA location
  END TYPE HistStrmFlowFileType
  
  
  ! -------------------------------------------------------------
  ! --- VARIABLE AGGREGATION DATA LIST TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLinkedListType) :: VarAggregationListType
      INTEGER,ALLOCATABLE :: iUniqueVarIndexList(:)
  CONTAINS
      PROCEDURE,PASS :: GetUniqueVarIndexList
  END TYPE VarAggregationListType
  
  
  ! -------------------------------------------------------------
  ! --- ANN LAYER DATA TYPE
  ! -------------------------------------------------------------
  TYPE ANNLayerType
      INTEGER             :: iNNeurons = 1             !Number of neurons in the layer
      INTEGER,ALLOCATABLE :: iActivation(:)            !Activation function
      REAL(8),ALLOCATABLE :: rBias(:)                  !Bias at each (neuron)
      REAL(8),ALLOCATABLE :: rWeights(:,:)             !Weights at each connection listed for each (neuron of previous layer,neuron of this layer)
      REAL(8),ALLOCATABLE :: rOutputValues(:)          !Output value from each (neuron)
  END TYPE ANNLayerType
  
  
  ! -------------------------------------------------------------
  ! --- ANN MODEL DATA TYPE
  ! -------------------------------------------------------------
  TYPE ANNModelType
      INTEGER                        :: iNLayers     = 1              !Number of layers (exculudes input layer but includes output layer)
      TYPE(ANNLayerType),ALLOCATABLE :: Layers(:)                     !Data for each (layer)
      LOGICAL                        :: lScalingIsOn = .FALSE.        !Flag to check if scaling of input and output values is on
      REAL(8),ALLOCATABLE            :: rVarMin(:)                    !Minimum value for each input (variable) for scaling
      REAL(8),ALLOCATABLE            :: rVarMax(:)                    !Maximum value for each input (variable) for scaling
      REAL(8)                        :: rOutputMin   = f_rNoScaling   !Minimum value for output value for unscaling 
      REAL(8)                        :: rOutputMax   = f_rNoScaling   !Maximum value for output value for unscaling 
      REAL(8)                        :: rConvFactor  = 1.0            !Factor to convert ANN output to IWFM units
  END TYPE ANNModelType
  
  
  ! -------------------------------------------------------------
  ! --- WSA ANN DATA TYPE
  ! -------------------------------------------------------------
  TYPE WSA_ANN_Type
      PRIVATE
      INTEGER                                  :: iRunMode             = f_iHistoricalRun !Model run mode (historical for training purposes or ANN mode for scenrio runs)
      INTEGER                                  :: iNWSA                = 0                !Number of WSAs to be calculated
      INTEGER,ALLOCATABLE                      :: IDs(:)                                  !ID number for each (WSA)
      INTEGER,ALLOCATABLE                      :: iStrmNodes(:)                           !Stream node indices at which WSAs will be applied for each (WSA)
      CHARACTER(LEN=15),ALLOCATABLE            :: cNames(:)                               !Name of each (WSA)
      INTEGER                                  :: iNVars               = 0                !Number of ANN input variables
      INTEGER,ALLOCATABLE                      :: iVarIDs(:)                              !ID number for  each (variable)
      INTEGER,ALLOCATABLE                      :: iVarTypes(:)                            !Type of  each (variable)
      INTEGER,ALLOCATABLE                      :: iGenericTSCol(:)                        !Column number in the generic variables file for each (variable)
      REAL(8),ALLOCATABLE                      :: rVarFactors(:)                          !Conversion factor for each (variable)
      CHARACTER(LEN=10),ALLOCATABLE            :: cVarUnits(:)                            !Unit of each (variable)         
      REAL(8),ALLOCATABLE                      :: rVarValues(:,:)                         !Aggregate variable value for each (variable,WSA)
      TYPE(VarAggregationListType),ALLOCATABLE :: VarAggregation(:)                       !Data describing over which model features each variable will be agrregated for each (WSA) 
      TYPE(ANNModelType),ALLOCATABLE           :: ANN(:)                                  !ANN model data for each (WSA)
      LOGICAL                                  :: lANN_Defined                = .FALSE.   !Flag to check if ANN model parameters are defined
      TYPE(HistStrmFlowFileType)               :: StrmFlowTSInFile                        !Timeseries input file for historical stream flows to be used during ANN training  
      LOGICAL                                  :: lStrmFlowTSInFile_Defined   = .FALSE.   !Flag to check if timeseries historical stream flow data file is defined 
      TYPE(RealTSDataInFileType)               :: GenericVarTSInFile                      !Timeseries input file for generic variables  
      LOGICAL                                  :: lGenericVarTSInFile_Defined = .FALSE.   !Flag to check if timeseries generic input file is defined 
      TYPE(GenericFileType)                    :: VarOutFile                              !File to print out aggregated variable values
      LOGICAL                                  :: lVarOutFile_Defined         = .FALSE.   !Flag to check input variable output file is defined
      TYPE(GenericFileType)                    :: WSAOutFile                              !File to print out calculated WSAs
      LOGICAL                                  :: lWSAOutFile_Defined         = .FALSE.   !Flag to check if WSA output file is defined
      REAL(8)                                  :: rFactorWSAOut               = 1.0       !Unit conversion factor for WSA output
      CHARACTER(LEN=10)                        :: cUnitWSAOut                 = ''        !WSA output unit 
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: GetNWSA
      PROCEDURE,PASS :: GetStrmNodes
      PROCEDURE,PASS :: GetSpecifiedStrmFlows
      PROCEDURE,PASS :: GetWSAs_AtAllNodes
      PROCEDURE,PASS :: ReadTSData 
      PROCEDURE,PASS :: PrintResults
      PROCEDURE,PASS :: Calculate
      PROCEDURE,PASS :: ZeroOut
      PROCEDURE,PASS :: IsHistoricalRun
  END TYPE WSA_ANN_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 9
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'WSA_ANN::'
  
  
  
  
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
  ! --- INITIALIZE WSA ANN
  ! -------------------------------------------------------------
  SUBROUTINE New(WSA,cFileName,cSIMWorkingDirectory,TimeStep,AppGrid,AppStream,iStat)
    CLASS(WSA_ANN_Type)            :: WSA
    CHARACTER(LEN=*),INTENT(IN)    :: cFileName,cSIMWorkingDirectory
    TYPE(TImeStepType),INTENT(IN)  :: TimeStep 
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    TYPE(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3),PARAMETER :: ThisProcedure = ModName // 'New'
    INTEGER                               :: indx,ID,iStrmNodeID,iNVars,ErrorCode,iLoc
    REAL(8)                               :: rFactor(1)
    CHARACTER                             :: ALine*500,cName*500,cErrorMsg*500,cVarOutFileName*1000,cANNParamFile*1000, &
                                             cWSAOutFileName*1000,cGenericTSInFile*1000,cStrmFlowTSInFile*1000
    TYPE(GenericFileType)                 :: MainInputFile
    INTEGER,ALLOCATABLE                   :: iStrmNodeIDs(:)
    CHARACTER(:),ALLOCATABLE              :: cAbsPathFileName
    
    !Make sure that streams are simulated
    IF (.NOT. AppStream%IsDefined()) THEN
        MessageArray(1) = 'There are no streams simulated in this model.'
        MessageArray(2) = 'Water Supply Adjustment (WSA) can only be applied to stream nodes!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open main input file
    CALL EstablishAbsolutePathFileName(cFileName,cSIMWorkingDirectory,cAbsPathFileName)
    CALL MainInputFile%New(cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='WSA ANN input file',iStat=iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Number of WSAs to be calculated
    CALL MainInputFile%ReadData(WSA%iNWSA, iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Historical stream flow input file
    CALL MainInputFile%ReadData(cStrmFlowTSInFile,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL CleanSpecialCharacters(cStrmFlowTSInFile)
    cStrmFlowTSInFile = ADJUSTL(StripTextUntilCharacter(cStrmFlowTSInFile,f_cInlineCommentChar))
    IF (LEN_TRIM(cStrmFlowTSInFile) .GT. 0) THEN
        CALL EstablishAbsolutePathFileName(TRIM(cStrmFlowTSInFile),cSIMWorkingDirectory,cAbsPathFileName)
        CALL WSA%StrmFlowTSInFile%Init(cAbsPathFileName,cSIMWorkingDirectory,'Timeseries input data file for historical stream flows to calculate historical WSAs',TimeStep%TrackTime,1,.TRUE.,Factor=rFactor,RateTypeData=[.TRUE.],iStat=iStat)
        IF (iStat .NE. 0) RETURN
        WSA%StrmFlowTSInFile%rFactor  = rFactor(1)
        WSA%lStrmFlowTSInFile_Defined = .TRUE.
    END IF
    ALLOCATE (WSA%StrmFlowTSInFile%iStrmFlowCols(WSA%iNWSA))
    
    !Get stream node and reach IDs
    ALLOCATE (iStrmNodeIDs(AppStream%GetNStrmNodes()))
    CALL AppStream%GetStrmNodeIDs(iStrmNodeIDs)
    
    !Read the WSA IDs, stream node IDs where they will be applied and the historical stream flow data columns
    ALLOCATE (WSA%IDs(WSA%iNWSA) , WSA%iStrmNodes(WSA%iNWSA)  ,  WSA%cNames(WSA%iNWSA))
    DO indx=1,WSA%iNWSA
        CALL MainInputFile%ReadData(ALine , iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL CleanSpecialCharacters(ALine)
        ALine = ADJUSTL(ALine)
        ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar,Back=.TRUE.)
        CALL ReplaceString(ALine,',',' ',iStat)  ;  IF (iStat .NE. 0) RETURN
        READ(ALine,*,IOSTAT=ErrorCode,IOMSG=cErrorMsg) ID,iStrmNodeID,WSA%StrmFlowTSInFile%iStrmFlowCols(indx),cName
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('An error occurred reading WSA application location and WSA name!'//f_cLineFeed//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        WSA%IDs(indx)    = ID
        WSA%cNames(indx) = TRIM(ADJUSTL(cName))
               
        !Check that WSA ID is not a repeat
        IF (LocateInList(ID,WSA%IDs(1:indx-1)) .GT. 0) THEN
            CALL SetLastMessage('WSA ID '//TRIM(IntToText(ID))//' is used more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure stream node is legit
        WSA%iStrmNodes(indx) = LocateInList(iStrmNodeID,iStrmNodeIDs)
        IF (WSA%iStrmNodes(indx) .EQ. 0) THEN
            CALL SetLastMessage('Stream node '//TRIM(IntToText(iStrmNodeID))//' listed for WSA application is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure more than one WSA is not applied to the same stream node
        IF (LocateInList(WSA%iStrmNodes(indx),WSA%iStrmNodes(1:indx-1)) .GT. 0) THEN
            CALL SetLastMessage('Stream node '//TRIM(IntToText(iStrmNodeID))//' is listed for more than one WSA!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Check that stream flow column number is non-zero if filename is provided
        IF (WSA%lStrmFlowTSInFile_Defined) THEN
            IF (WSA%StrmFlowTSInFile%iStrmFlowCols(indx) .LT. 1) THEN
                MessageArray(1) = 'Historical stream flow column numbers (IFWCOL) must be'
                MessageArray(2) = ' greater than zero when historical flow filename is'
                MessageArray(3) = ' specified for the computation of historical WSAs!'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END DO
    
    !ANN parameter file
    CALL MainInputFile%ReadData(cANNParamFile,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL CleanSpecialCharacters(cANNParamFile)
    cANNParamFile = ADJUSTL(StripTextUntilCharacter(cANNParamFile,f_cInlineCommentChar))
    
    !Make sure that if historical stream flow filename is specified, ANN parameter filename is left blank and vice versa
    IF (LEN_TRIM(cStrmFlowTSInFile) .GT. 0) THEN
        IF (LEN_TRIM(cANNParamFile) .GT. 0) THEN
            MessageArray(1) = 'Either historical stream flows file or ANN parameter file must be specified!'
            MessageArray(2) = 'To generate historical WSAs for ANN training purposes, specify historical stream flow file.'
            MessageArray(3) = 'To use trained ANNs to calculate predicted WSAs, specify ANN parameter file.'
            CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        ELSE
            WSA%iRunMode = f_iHistoricalRun
        END IF
    ELSE
        IF (LEN_TRIM(cANNParamFile) .GT. 0)  &
            WSA%iRunMode = f_iANNRun
    END IF

    !Generic timeseries variable input file 
    CALL MainInputFile%ReadData(cGenericTSInFile,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL CleanSpecialCharacters(cGenericTSInFile)
    cGenericTSInFile = ADJUSTL(StripTextUntilCharacter(cGenericTSInFile,f_cInlineCommentChar))
    IF (LEN_TRIM(cGenericTSInFile) .GT. 0) THEN
        CALL EstablishAbsolutePathFileName(TRIM(cGenericTSInFile),cSIMWorkingDirectory,cAbsPathFileName)
        CALL WSA%GenericVarTSInFile%Init(cAbsPathFileName,cSIMWorkingDirectory,'Timeseries input data file for WSA generic variables',TimeStep%TrackTime,1,.FALSE.,iStat=iStat)
        IF (iStat .NE. 0) RETURN
        WSA%lGenericVarTSInFile_Defined = .TRUE.
    END IF
    
    !Variable value output file
    CALL MainInputFile%ReadData(cVarOutFileName,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL CleanSpecialCharacters(cVarOutFileName)
    cVarOutFileName = ADJUSTL(StripTextUntilCharacter(cVarOutFileName,f_cInlineCommentChar))
    
    !Calculated WSA value output file, output conversion factor and unit
    CALL MainInputFile%ReadData(cWSAOutFileName,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL CleanSpecialCharacters(cWSAOutFileName)
    cWSAOutFileName = ADJUSTL(StripTextUntilCharacter(cWSAOutFileName,f_cInlineCommentChar))
    CALL MainInputFile%ReadData(WSA%rFactorWSAOut,iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL MainInputFile%ReadData(ALine,iStat)              ;  IF (iStat .NE. 0) RETURN
    CALL CleanSpecialCharacters(ALine)
    WSA%cUnitWSAOut = ADJUSTL(StripTextUntilCharacter(ALine,f_cInlineCommentChar))
        
    !Number of ANN input variables
    CALL MainInputFile%ReadData(iNVars , iStat)  ;  IF (iStat .NE. 0) RETURN
    WSA%iNVars = iNVars
    
    !Input variable types and conversion factors
    ALLOCATE (WSA%iVarIDs(iNVars) , WSA%iVarTypes(iNVars) , WSA%rVarFactors(iNVars) , WSA%cVarUnits(iNVars) , WSA%iGenericTSCol(iNVars))
    DO indx=1,iNVars
        CALL MainInputFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL CleanSpecialCharacters(ALine)
        ALine = ADJUSTL(StripTextUntilCharacter(ALine,f_cInlineCommentChar))
        CALL ReplaceString(ALine,',',' ',iStat)  ;  IF (iStat .NE. 0) RETURN
        READ (ALine,*) WSA%iVarIDs(indx),WSA%iVarTypes(indx)
        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
        
        !Make sure variable ID is not repeated
        IF (LocateInList(WSA%iVarIDs(indx),WSA%iVarIDs(1:indx-1)) .GT. 0) THEN
            CALL SetLastMessage('Variable ID number '//TRIM(IntToText(WSA%iVarIDs(indx)))//' for WSA calculations is defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        SELECT CASE (WSA%iVarTypes(indx))
            CASE (f_iVarType_GenericTS)
                !Try to read 2 inputs first
                READ(ALine,*,IOSTAT=ErrorCode) WSA%iGenericTSCol(indx),WSA%rVarFactors(indx)
                !Read 1 input if not succesful
                IF (ErrorCode .NE. 0) THEN
                    READ(ALine,*,IOSTAT=ErrorCode) WSA%iGenericTSCol(indx)
                    IF (ErrorCode .NE. 0) THEN
                        MessageArray(1) = 'A column number from the generic timeseries variable file must be'
                        MessageArray(2) = 'specified when variable type for WSA calculation is defined as '//TRIM(IntToText(f_iVarType_GenericTS))//'!'
                        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    ELSE
                        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
                        WSA%rVarFactors(indx) = 1.0
                    END IF
                ELSE
                    iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
                    iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
                END IF
                !Read unit if provided
                IF (LEN_TRIM(ALine) .EQ. 0) THEN
                    WSA%cVarUnits(indx) = ''
                ELSE
                    WSA%cVarUnits(indx) = TRIM(ALine)
                END IF
                
                
            CASE (f_iVarType_Season)
                WSA%iGenericTSCol(indx) = 0
                WSA%rVarFactors(indx)   = 1.0
                WSA%cVarUnits(indx)     = ''
                
                
            CASE DEFAULT
                READ(ALine,*) WSA%rVarFactors(indx)
                iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
                WSA%cVarUnits(indx)     = TRIM(ALine)
                WSA%iGenericTSCol(indx) = 0
                
        END SELECT

        !Make sure variable type is recognized
        IF (LocateInList(WSA%iVarTypes(indx),f_iVarTypeList) .EQ. 0) THEN
            CALL SetLastMessage('Variable type '//TRIM(IntToText(WSA%iVarTypes(indx)))//' listed for the calculation of WSA is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
    END DO
    
    !Make sure that generic variable timeseries input file is defined if such variable type is used
    IF (ANY(WSA%iVarTypes .EQ. f_iVarType_GenericTS)) THEN
        IF (.NOT. WSA%lGenericVarTSInFile_Defined) THEN
            CALL SetLastMessage('Generic timeseries input file must be specified when variable type '//TRIM(IntToText(f_iVarType_GenericTS))//' is used!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Compile variable aggregation methods
    CALL CompileVarAggregation(MainInputFile,AppGrid,AppStream,iStrmNodeIDs,WSA,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Allocate memory for aggregated variable values and WSA ANN model data
    ALLOCATE (WSA%rVarValues(iNVars,WSA%iNWSA))
    
    !Instantiate variable value output file
    IF (LEN_TRIM(cVarOutFileName) .GT. 0)  THEN
        CALL VarOutFile_New(WSA,cVarOutFileName,cSIMWorkingDirectory,TimeStep,iStat)  
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Instantiate ANN models
    IF (LEN_TRIM(cANNParamFile) .GT. 0) THEN
        CALL ANNModel_New(WSA,cANNParamFile,cSIMWorkingDirectory,iStat)
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Instantiate WSA output file only if either ANN parameters or historical stream flows are defined
    IF (LEN_TRIM(cWSAOutFileName) .GT. 0) THEN
        IF (LEN_TRIM(cANNParamFile) .EQ. 0  .AND.  LEN_TRIM(cStrmFlowTSInFile) .EQ. 0) THEN
            MessageArray(1) = 'WSA output file can only be generated when either ANN '
            MessageArray(2) = ' parameters or historical stream flows are defined!'
            CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure,iDestination=f_iSCREEN_FILE)
        ELSE
            CALL WSAOutFile_New(WSA,cWSAOutFileName,cSIMWorkingDirectory,TimeStep,iStat)
        END IF
    END IF
    
    !Close file
    CALL MainInputFile%Kill()

  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE VARIABLE VALUE OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE VarOutFile_New(WSA,cFileName,cSIMWorkingDirectory,TimeStep,iStat)
    TYPE(WSA_ANN_Type)            :: WSA
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cSIMWorkingDirectory
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'VarOutFile_New'
    INTEGER                                :: iCount,indxVar,indxWSA,iVarType,iNColumns,iVar
    CHARACTER                              :: cFormatSpec*30,FPart*32(1),cText*6,cTitleLines*1000(1),cWorkArray*100(3),       &
                                              cHeaderFormat*50(6)
    CHARACTER,ALLOCATABLE                  :: cDataUnits*10(:),cDataTypes*10(:),CPart*32(:),cHeaders*50(:,:),cMiscArray*50(:)
    CHARACTER(:),ALLOCATABLE               :: cAbsPathFileName
    
    !Number of print-out columns
    iNColumns = 0
    DO indxWSA=1,WSA%iNWSA
        iNColumns = iNColumns + SIZE(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList)
    END DO
    
    !Allocate variables
    ALLOCATE (cDataUnits(iNColumns) , cDataTypes(iNColumns) , CPart(iNColumns) , cHeaders(6,iNColumns+1) , cMiscArray(iNColumns))
    
    !Absolute filepath
    CALL EstablishAbsolutePathFileName(cFileName,cSIMWorkingDirectory,cAbsPathFileName)
    
    !Open file
    CALL WSA%VarOutFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='water supply adjustment ANN input variables output',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Make sure DSS file is used only if it is a time-tracking simulation
    IF (WSA%VarOutFile%iGetFileType() .EQ. f_iDSS) THEN
        IF (.NOT. TimeStep%TrackTime) THEN
            CALL SetLastMessage('DSS files for printing of the input variables for the ANN model of the water supply adjustments can only be used for time-tracking simulations.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

    !Prepare file for print-out
    cFormatSpec = '(A16,' // TRIM(IntToText(iNColumns)) //'(2X,F12.4))' 
    iCount = 0
    DO indxWSA=1,WSA%iNWSA
        DO indxVar=1,SIZE(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList)
            iCount             = iCount + 1
            iVar               = WSA%VarAggregation(indxWSA)%iUniqueVarIndexList(indxVar)
            iVarType           = WSA%iVarTypes(iVar)
            cDataUnits(iCount) = WSA%cVarUnits(iVar)
            CPart(iCount)      = f_cDSSCParts(iVar)
            SELECT CASE (iVarType)
                CASE (f_iVarType_GenericTS , f_iVarType_Season)
                    cDataTypes(iCount) = 'INST-VAL'
                CASE DEFAULT
                    cDataTypes(iCount) = 'PER-CUM'
            END SELECT
            cMiscArray(iCount) = TRIM(WSA%cNames(indxWSA)) // ':' // 'VAR' // TRIM(IntToText(iVarType))  
        END DO
    END DO
    FPart(1) = 'WSA_INPUT_VARIABLE'

    !Prepare header lines
    cWorkArray(1) = ''
    cWorkArray(2) = ArrangeText('INPUT VARIABLES FOR WSA ANN',35)
    cWorkArray(3) = ''
    CALL PrepareTitle(cTitleLines(1),cWorkArray,37,42)
    cHeaders(1,1) = '*'
    cHeaders(2,1) = '*            WSA'
    cHeaders(3,1) = '*    VARIABLE ID'
    cHeaders(4,1) = '*  VARIABLE TYPE'
    cHeaders(5,1) = '*           UNIT'
    cHeaders(6,1) = '*     TIME' 
    iCount        = 1
    DO indxWSA=1,WSA%iNWSA
        DO indxVar=1,SIZE(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList)
            iCount             = iCount + 1
            iVar               = WSA%VarAggregation(indxWSA)%iUniqueVarIndexList(indxVar)
            cHeaders(1,iCount) = ''
            cHeaders(2,iCount) = WSA%cNames(indxWSA) 
            cHeaders(3,iCount) = TRIM(IntToText(WSA%iVarIDs(iVar)))
            cHeaders(4,iCount) = TRIM(IntToText(WSA%iVarTypes(iVar)))
            cHeaders(5,iCount) = ADJUSTL(WSA%cVarUnits(iVar))
            cHeaders(6,iCount) = ''
        END DO
    END DO
    cText            = IntToText(iNColumns)
    cHeaderFormat(1) = '(A,'//TRIM(cText)//'A1)'
    cHeaderFormat(2) = '(A16,'//TRIM(cText)//'(2X,A12))'
    cHeaderFormat(3) = '(A16,'//TRIM(cText)//'(2X,A12))'
    cHeaderFormat(4) = '(A16,'//TRIM(cText)//'(2X,A12))'
    cHeaderFormat(5) = '(A16,'//TRIM(cText)//'(2X,A12))'
    cHeaderFormat(6) = '(A10,'//TRIM(cText)//'A1)'
    
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(WSA%VarOutFile                            , &
                              NColumnsOfData          = iNColumns       , &
                              NRowsOfData             = 1               , &
                              OverwriteNColumnsOfData = .FALSE.         , &
                              FormatSpec              = cFormatSpec     , &
                              Title                   = cTitleLines     , &
                              Header                  = cHeaders        , &
                              HeaderFormat            = cHeaderFormat   , &
                              PrintColumnNo           = .FALSE.         , &
                              DataUnit                = cDataUnits      , &
                              DataType                = cDataTypes      , &
                              CPart                   = CPart           , &
                              FPart                   = FPart           , &
                              UnitT                   = TimeStep%Unit   , &
                              MiscArray               = cMiscArray      , &
                              iStat                   = iStat           )  
    
    !If made it this far, set the flag
    WSA%lVarOutFile_Defined = .TRUE.
    
  END SUBROUTINE VarOutFile_New
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE CALCULATED WSA VALUE OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE WSAOutFile_New(WSA,cFileName,cSIMWorkingDirectory,TimeStep,iStat)
    TYPE(WSA_ANN_Type)            :: WSA
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cSIMWorkingDirectory
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'WSAOutFile_New'
    INTEGER                                :: indxWSA,iNWSA
    CHARACTER                              :: cFormatSpec*30,cDataUnits*10(WSA%iNWSA),FPart*32(1),cText*6,      &
                                              cDataTypes*10(1),CPart*32(WSA%iNWSA),cTitleLines*1000(1),         &
                                              cWorkArray*100(2),cHeaders*50(4,WSA%iNWSA+1),cHeaderFormat*50(4), &
                                              cMiscArray*50(WSA%iNWSA)
    CHARACTER(:),ALLOCATABLE               :: cAbsPathFileName
    
    !Initializes
    iNWSA = WSA%iNWSA
    
    !Absolute filepath
    CALL EstablishAbsolutePathFileName(cFileName,cSIMWorkingDirectory,cAbsPathFileName)
    
    !Open file
    CALL WSA%WSAOutFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='calculated water supply adjustment output',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Make sure DSS file is used only if it is a time-tracking simulation
    IF (WSA%WSAOutFile%iGetFileType() .EQ. f_iDSS) THEN
        IF (.NOT. TimeStep%TrackTime) THEN
            CALL SetLastMessage('DSS files for printing of the calculated water supply adjustments can only be used for time-tracking simulations.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

    !Prepare file for print-out
    cFormatSpec = '(A16,' // TRIM(IntToText(iNWSA)) //'(2X,F15.4))' 
    DO indxWSA=1,WSA%iNWSA
        CPart(indxWSA)      = 'WSA_'//UpperCase(TRIM(WSA%cNames(indxWSA)))
        cMiscArray(indxWSA) = 'WSA_'//TRIM(WSA%cNames(indxWSA))  
    END DO
    cDataUnits    = WSA%cUnitWSAOut
    cDataTypes(1) = 'PER-CUM'
    FPart(1)      = 'WSA'

    !Prepare header lines
    cWorkArray(1) = ArrangeText('CALCULATED WATER SUPPLY ADJUSTMENTS',39)
    cWorkArray(2) = ArrangeText('(UNIT=',TRIM(WSA%cUnitWSAOut),')',39)
    CALL PrepareTitle(cTitleLines(1),cWorkArray,41,46)
    cHeaders(1,1) = '*'
    cHeaders(2,1) = '*         WSA ID'
    cHeaders(3,1) = '*       WSA NAME'
    cHeaders(4,1) = '*     TIME' 
    DO indxWSA=1,iNWSA
        cHeaders(1,indxWSA+1) = ''
        cHeaders(2,indxWSA+1) = TRIM(IntToText(WSA%IDs(indxWSA))) 
        cHeaders(3,indxWSA+1) = WSA%cNames(indxWSA)
        cHeaders(4,indxWSA+1) = ''
    END DO
    cText            = IntToText(iNWSA)
    cHeaderFormat(1) = '(A1,'//TRIM(cText)//'A1)'
    cHeaderFormat(2) = '(A16,'//TRIM(cText)//'(2X,A15))'
    cHeaderFormat(3) = '(A16,'//TRIM(cText)//'(2X,A15))'
    cHeaderFormat(4) = '(A10,'//TRIM(cText)//'A1)'
    
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(WSA%WSAOutFile                            , &
                              NColumnsOfData          = iNWSA           , &
                              NRowsOfData             = 1               , &
                              OverwriteNColumnsOfData = .FALSE.         , &
                              FormatSpec              = cFormatSpec     , &
                              Title                   = cTitleLines     , &
                              Header                  = cHeaders        , &
                              HeaderFormat            = cHeaderFormat   , &
                              PrintColumnNo           = .FALSE.         , &
                              DataUnit                = cDataUnits      , &
                              DataType                = cDataTypes      , &
                              CPart                   = CPart           , &
                              FPart                   = FPart           , &
                              UnitT                   = TimeStep%Unit   , &
                              MiscArray               = cMiscArray      , &
                              iStat                   = iStat           )  
    
    !If made it this far, set the flag
    WSA%lWSAOutFile_Defined = .TRUE.
    
  END SUBROUTINE WSAOutFile_New
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE ANN MODELS
  ! -------------------------------------------------------------
  SUBROUTINE ANNModel_New(WSA,cFileName,cSIMWorkingDirectory,iStat)
    TYPE(WSA_ANN_Type)          :: WSA
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cSIMWorkingDirectory
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'ANNModel_New'
    INTEGER                                :: iNWSA,indxWSA,iWSAID,iNLayers,iWSA,iVarID,indxVar,iLoc, &
                                              iNConnections,iNNeurons,iLayer,iNeuron,iNVars,iVar,     &
                                              indxNeuron,iNWSAVars,iActCode
    REAL(8)                                :: rDummyArray6(6),rDummyArray3(3)
    CHARACTER                              :: ALine*1000
    TYPE(GenericFileType)                  :: InFile
    CHARACTER(:),ALLOCATABLE               :: cAbsPathFileName
    
    !Initailize
    iNWSA  = WSA%iNWSA
    iNVars = WSA%iNVars
    
    !Establish absolute path
    CALL EstablishAbsolutePathFileName(cFileName,cSIMWorkingDirectory,cAbsPathFileName)
    
    !Open file
    CALL InFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='WSA ANN model parameter input',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory for ANN models
    ALLOCATE (WSA%ANN(iNWSA))
    
    !Number of layers and neurons for each WSA
    DO indxWSA=1,iNWSA
        !Read a line of data
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
        
        !WSA ID and number of layers
        READ(ALine,*) iWSAID,iNLayers
        
        !Make sure WSA ID is recognized
        iWSA = LocateInList(iWSAID,WSA%IDs)
        IF (iWSA .EQ. 0) THEN
            CALL SetLastMessage('WSA ID '//TRIM(IntToText(iWSAID))//' listed for ANN model parameters is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        ASSOCIATE (pANN => WSA%ANN(iWSA))
            !Store number of layers (add 1 for output layer)
            pANN%iNLayers = iNLayers + 1 
            
            !Allocate memory for layer data (include ouput layer)
            ALLOCATE (pANN%Layers(iNLayers+1))
            READ (ALine,*) iWSAID,iNLayers,pANN%Layers(1:iNLayers)%iNNeurons
        END ASSOCIATE
    END DO
    
    !Min and max values for scaling and conversion factors and WSA units
    DO indxWSA=1,iNWSA
        !Number of variables used in the ANN model for the WSA
        iNWSAVars = SIZE(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList)
        
        !Read first line of data
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL CleanSpecialCharacters(ALine)
        ALine = StripTextUntilCharacter(ADJUSTL(ALine),f_cInlineCommentChar)
        CALL ReplaceString(Aline,',',' ',iStat)  ;  IF (iStat .NE. 0) RETURN
            
        !Check that WSA ID is valid
        READ (ALine,*) iWSAID
        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
        iWSA = LocateInList(iWSAID,WSA%IDs)
        IF (iWSA .EQ. 0) THEN
            CALL SetLastMessage('WSA ID '//TRIM(IntToText(iWSAID))//' listed for ANN model scaling parameters is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Rest of the data
        READ (ALine,*) rDummyArray6
        
        !Check that input variable ID is valid and used as a variable for the WSA
        iVarID = INT(rDummyArray6(4))
        iVar   = LocateInList(iVarID,WSA%iVarIDs)
        IF (iVar .EQ. 0) THEN
            CALL SetLastMessage('Input variable ID '//TRIM(IntToText(iVarID))//' listed for ANN model scaling parameters is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iVar = LocateInList(iVar,WSA%VarAggregation(iWSA)%iUniqueVarIndexList)
        IF (iVar .EQ. 0) THEN
            CALL SetLastMessage('Input variable ID '//TRIM(IntToText(iVarID))//' listed for scaling parameters is not used as a variable for WSA ID '//TRIM(IntToText(iWSAID))//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        ASSOCIATE (pANN => WSA%ANN(iWSA))
            !Allocate memory
            ALLOCATE (pANN%rVarMin(iNWSAVars) , pANN%rVarMax(iNWSAVars))
            pANN%rVarMin = f_rNoScaling
            pANN%rVarMax = f_rNoScaling
                        
            !Store values
            pANN%rConvFactor   = rDummyArray6(1)
            pANN%rOutputMin    = rDummyArray6(2)
            pANN%rOutputMax    = rDummyArray6(3)
            pANN%rVarMin(iVar) = rDummyArray6(5)
            pANN%rVarMax(iVar) = rDummyArray6(6)
            
            !Read the rest of the input for the WSA
            DO indxVar=2,iNWSAVars
                CALL InFile%ReadData(rDummyArray3,iStat)  ; IF (iStat .NE. 0) RETURN

                !Check that input variable ID is valid and used as a variable for the WSA
                iVarID = INT(rDummyArray3(1))
                iVar   = LocateInList(iVarID,WSA%iVarIDs)
                IF (iVar .EQ. 0) THEN
                    CALL SetLastMessage('Input variable ID '//TRIM(IntToText(iVarID))//' listed for ANN model scaling parameters is not valid!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                iVar = LocateInList(iVar,WSA%VarAggregation(iWSA)%iUniqueVarIndexList)
                IF (iVar .EQ. 0) THEN
                    CALL SetLastMessage('Input variable ID '//TRIM(IntToText(iVarID))//' listed for scaling parameters is not used as a variable for WSA ID '//TRIM(IntToText(iWSAID))//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        
                !Store values
                pANN%rVarMin(iVar) = rDummyArray3(2)
                pANN%rVarMax(iVar) = rDummyArray3(3)
            END DO
            
            !If none of the min/max values are set to no-scaling flag; then store this info
            IF (ANY(pANN%rVarMin .EQ. f_rNoScaling)) CYCLE
            IF (ANY(pANN%rVarMax .EQ. f_rNoScaling)) CYCLE
            IF (pANN%rOutputMin  .EQ. f_rNoScaling)  CYCLE
            IF (pANN%rOutputMax  .EQ. f_rNoScaling)  CYCLE
            pANN%lScalingIsOn = .TRUE.
        END ASSOCIATE
    END DO
    
    !Read weights and biases
    DO
        !Read a line of data
        CALL InFile%ReadData(ALine,iStat)  
        IF (iStat .NE. 0) THEN
            iStat = 0  !Assumes this is because we hit the end of file
            EXIT
        END IF
        CALL CleanSpecialCharacters(ALine)
        IF (LEN_TRIM(ALine) .EQ. 0) EXIT
        
        !Break the line of data into pieces
        READ (ALine,*) iWSAID,iLayer,iNeuron
        
        !Make sure WSA ID is valid
        iWSA = LocateInList(iWSAID,WSA%IDs)
        IF (iWSA .EQ. 0) THEN
            CALL SetLastMessage('WSA ID '//TRIM(IntToText(iWSAID))//' listed for ANN weights and biases is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Process rest of data for the WSA
        ASSOCIATE (pANN => WSA%ANN(iWSA))
            iNNeurons = pANN%Layers(iLayer)%iNNeurons
            IF (iLayer .EQ. 1) THEN
                iNConnections = SIZE(WSA%VarAggregation(iWSA)%iUniqueVarIndexList)
            ELSE
                iNConnections = pANN%Layers(iLayer-1)%iNNeurons
            END IF

            !Allocate memory
            ALLOCATE (pANN%Layers(iLayer)%iActivation(iNNeurons)            , &
                      pANN%Layers(iLayer)%rBias(iNNeurons)                  , &
                      pANN%Layers(iLayer)%rWeights(iNConnections,iNNeurons) , &
                      pANN%Layers(iLayer)%rOutputValues(iNNeurons)          )
            
            !Store data in persistent arrays
            READ (ALine,*) iWSAID,iLayer,iNeuron,pANN%Layers(iLayer)%iActivation(iNeuron),pANN%Layers(iLayer)%rBias(iNeuron),pANN%Layers(iLayer)%rWeights(:,iNeuron)

            !Make sure activation code is legit
            iActCode = pANN%Layers(iLayer)%iActivation(iNeuron)
            IF (.NOT. iActCode .EQ. f_iAct_None) THEN
                IF (.NOT. iActCode .EQ. f_iAct_ReLU) THEN
                    IF (.NOT. iActCode .EQ. f_iAct_Sigmoid) THEN
                        CALL SetLastMessage('Activation code '//TRIM(IntToText(iActCode))//' listed for WSA ID '//TRIM(IntToText(iWSAID))//', layer '//TRIM(IntToText(iLayer))//' and neuron '//TRIM(IntToText(iNeuron))//' is not recognized!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
            END IF
            
            !Read data for the rest of the neurons
            DO indxNeuron=2,iNNeurons
                CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .NE. 0) RETURN
                CALL CleanSpecialCharacters(ALine)
                READ (ALine,*) iNeuron
                READ (ALine,*) iNeuron,pANN%Layers(iLayer)%iActivation(iNeuron),pANN%Layers(iLayer)%rBias(iNeuron),pANN%Layers(iLayer)%rWeights(:,iNeuron)
   
                !Make sure activation code is legit
                iActCode = pANN%Layers(iLayer)%iActivation(iNeuron)
                IF (.NOT. iActCode .EQ. f_iAct_None) THEN
                    IF (.NOT. iActCode .EQ. f_iAct_ReLU) THEN
                        IF (.NOT. iActCode .EQ. f_iAct_Sigmoid) THEN
                            CALL SetLastMessage('Activation code '//TRIM(IntToText(iActCode))//' listed for WSA ID '//TRIM(IntToText(iWSAID))//', layer '//TRIM(IntToText(iLayer))//' and neuron '//TRIM(IntToText(iNeuron))//' is not recognized!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF
                    END IF
                END IF
            END DO
        END ASSOCIATE
    END DO
    
    !If made this far, set the flag
    WSA%lANN_Defined = .TRUE.
    
    !Close file
    CALL InFile%Kill()
    
  END SUBROUTINE ANNModel_New

    
    
    
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
  ! --- KILL WSA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(WSA)
    CLASS(WSA_ANN_Type) :: WSA
    
    !Local variables
    INTEGER            :: iErrorCode
    TYPE(WSA_ANN_Type) :: Dummy
    
    !Clear allocated memory
    DEALLOCATE (WSA%IDs            , &
                WSA%iStrmNodes     , &
                WSA%cNames         , &
                WSA%iVarIDs        , &
                WSA%iVarTypes      , &
                WSA%iGenericTSCol  , &
                WSA%rVarFactors    , &
                WSA%cVarUnits      , &
                WSA%rVarValues     , &
                WSA%VarAggregation , &
                STAT=iErrorCode    )
    
    !Close historical stream inflow file
    IF (WSA%lStrmFlowTSInFile_Defined) THEN
        DEALLOCATE (WSA%StrmFlowTSInFile%iStrmFlowCols , STAT=iErrorCode)
        CALL WSA%StrmFlowTSInFile%Close()
    END IF
    
    !Close generic variable data file
    CALL WSA%GenericVarTSInFile%Close()
    
    !Close variable output file
    CALL WSA%VarOutFile%Kill()
    
    !Close WSA output file
    CALL WSA%WSAOutFile%Kill()
    
    !Restore the object to default values
    SELECT TYPE (WSA)
        TYPE IS (WSA_ANN_Type)
            WSA = Dummy
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
  ! --- GET NUMBER OF STREAM NODES WITH WSA
  ! -------------------------------------------------------------
  PURE FUNCTION GetNWSA(WSA) RESULT(iNWSA)
    CLASS(WSA_ANN_Type),INTENT(IN) :: WSA
    INTEGER                        :: iNWSA
    
    iNWSA = WSA%iNWSA
    
  END FUNCTION GetNWSA
  
  
  ! -------------------------------------------------------------
  ! --- GET WSA VALUES AT ALL STREAM NODES
  ! --- (Only a handful will be non-zero)
  ! -------------------------------------------------------------
  SUBROUTINE GetWSAs_AtAllNodes(WSA,rValues)
    CLASS(WSA_ANN_Type),INTENT(IN) :: WSA
    REAL(8),INTENT(OUT)            :: rValues(:)
    
    !Local variables
    INTEGER :: indxWSA,iOutputLayer,iStrmNode
    
    !Initialize
    rValues = 0.0
    
    !Return if ANNs are not defined
    IF (.NOT. WSA%lANN_Defined) RETURN
    
    DO indxWSA=1,WSA%iNWSA
        iOutputLayer       = WSA%ANN(indxWSA)%iNLayers
        iStrmNode          = WSA%iStrmNodes(indxWSA)
        rValues(iStrmNode) = WSA%ANN(indxWSA)%Layers(iOutputLayer)%rOutputValues(1)
    END DO
    
  END SUBROUTINE GetWSAs_AtAllNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODES WITH WSA
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmNodes(WSA,iStrmNodes)
    CLASS(WSA_ANN_Type),INTENT(IN) :: WSA
    INTEGER,INTENT(OUT)            :: iStrmNodes(:)
    
    iStrmNodes = WSA%iStrmNodes
    
  END SUBROUTINE GetStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET SPECIFIED STREAM FLOWS AT NODES WITH WSA
  ! -------------------------------------------------------------
  SUBROUTINE GetSpecifiedStrmFlows(WSA,rSpecStrmFlows)
    CLASS(WSA_ANN_Type),INTENT(IN) :: WSA
    REAL(8),INTENT(OUT)            :: rSpecStrmFlows(:)
    
    rSpecStrmFlows = WSA%StrmFlowTSInFile%rValues(WSA%StrmFlowTSInFile%iStrmFlowCols)
    
  END SUBROUTINE GetSpecifiedStrmFlows

  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF UNIQUE VARIABLE INDICIES USD FOR A WSA
  ! -------------------------------------------------------------
  SUBROUTINE GetUniqueVarIndexList(VarAggregationList,iVarIndexList)
    CLASS(VarAggregationListType),INTENT(IN) :: VarAggregationList
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iVarIndexList(:)
    
    !Local variables
    INTEGER          :: iErrorCode ,iCount,iVarIndex,iLoc,indx,        &
                        iTempList(VarAggregationList%GetNNodes())
    CLASS(*),POINTER :: pCurrent
    
    !Initialize 
    iCount = 0
    DEALLOCATE (iVarIndexList , STAT=iErrorCode)
    
    !Traverse through list and compile unique variable indices
    CALL VarAggregationList%Reset()
    DO indx=1,VarAggregationList%GetNNodes()
        pCurrent  => VarAggregationList%GetCurrentValue()
        SELECT TYPE (pCurrent)
            TYPE IS (VarAggregationType)
                iVarIndex =  pCurrent%iVarIndex
                iLoc      =  LocateInList(iVarIndex,iTempList(1:iCount))
                IF (iLoc .EQ. 0) THEN
                    iCount            = iCount + 1
                    iTempList(iCount) = iVarIndex
                END IF
        END SELECT 
        CALL VarAggregationList%Next()
    END DO
    
    !Store data in the return array
    ALLOCATE (iVarIndexList(iCount))
    iVarIndexList = iTempList(1:iCount)
    CALL ShellSort(iVarIndexList)
    
  END SUBROUTINE GetUniqueVarIndexList

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READTIME SERIES INPUT DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(WSA,TimeStep,iStat)
    CLASS(WSA_ANN_Type)           :: WSA
    TYPE(TimeSTepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat 
    
    !Local variables
    INTEGER :: iFileReadCode,indx,iCol
    
    !Initialize
    iStat = 0
    
    !Read generic timeseries variable data
    IF (WSA%lGenericVarTSInFile_Defined) THEN
        CALL WSA%GenericVarTSInFile%ReadTSData(TimeStep,'Generic timeseries variable data for WSA calculation',iFileReadCode,iStat)
        IF (iStat .NE. 0) RETURN
        !Apply conversion factors if data is read succesfully
        IF (iFileReadCode .EQ. 0) THEN
            DO indx=1,WSA%iNVars
                IF (WSA%iVarTypes(indx) .EQ. f_iVarType_GenericTS) THEN
                    iCol                                 = WSA%iGenericTSCol(indx)
                    WSA%GenericVarTSInFile%rValues(iCol) = WSA%GenericVarTSInFile%rValues(iCol) * WSA%rVarFactors(indx)
                END IF
            END DO
        END IF
    END IF
    
    !Read historical stream flows
    IF (.NOT. WSA%lStrmFlowTSInFile_Defined) RETURN
    CALL WSA%StrmFlowTSInFile%ReadTSData(TimeStep,'Historical stream flows for historical WSA calculation',iFileReadCode,iStat)
    IF (iStat .NE. 0) RETURN
    !Apply conversion factors if data is read succesfully
    IF (iFileReadCode .EQ. 0)   &
        WSA%StrmFlowTSInFile%rValues = WSA%StrmFlowTSInFile%rValues * WSA%StrmFlowTSInFile%rFactor
            
  END SUBROUTINE ReadTSData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PRINT AGGREGATED VARIBLE VALUES
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(WSA,lEndOfSimulation,TimeStep,AppStream)
    CLASS(WSA_ANN_Type)            :: WSA
    LOGICAL,INTENT(IN)             :: lEndOfSimulation
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    TYPE(AppStreamType),INTENT(IN) :: AppStream
    
    !Local variables
    INTEGER   :: indxWSA,iNLayers,iCount,iNCols
    REAL(8)   :: rWSAs(WSA%iNWSA),rVarValues_Work(WSA%iNWSA*WSA%iNVars)
    CHARACTER :: SimulationTime*21
    
    !Create the simulation time
    SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)

    !Print out the variables
    IF (WSA%lVarOutFile_Defined) THEN
        iCount = 0
        DO indxWSA=1,WSA%iNWSA
            iNCols                                  = SIZE(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList)
            rVarValues_Work(iCount+1:iCount+iNCols) = WSA%rVarValues(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList,indxWSA)
            iCount                                  = iCount + iNCols
        END DO
        CALL WSA%VarOutFile%WriteData(SimulationTime,rVarValues_Work(1:iCount),FinalPrint=lEndOfSimulation)   
    END IF
    
    !Print out calculated WSAs
    IF (WSA%lWSAOutFile_Defined) THEN
        IF (WSA%iRunMode .EQ. f_iHistoricalRun) THEN
            CALL AppStream%GetWSAs_AtSomeNodes(WSA%iStrmNodes,rWSAs)
        ELSE
            DO indxWSA=1,WSA%iNWSA
                iNLayers       = WSA%ANN(indxWSA)%iNLayers
                rWSAs(indxWSA) = WSA%ANN(indxWSA)%Layers(iNLayers)%rOutputValues(1) 
            END DO
        END IF
        rWSAS = rWSAs * WSA%rFactorWSAOut  !WSA values are stored in IWFM units so we are converting them to output units
        CALL WSA%WSAOutFile%WriteData(SimulationTime,rWSAs,FinalPrint=lEndOfSimulation)
    END IF
    
  END SUBROUTINE PrintResults
  
  
  
  
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
  ! --- CALCULATE WSA
  ! -------------------------------------------------------------
  SUBROUTINE Calculate(WSA,QROFF,QRTRN,QRPONDDRN,QTRIB,QERELS,QDEEPPERC,TimeStep,AppGrid,RootZone,AppStream,AppGW,StrmGWConnector,iStat)
    CLASS(WSA_ANN_Type)                  :: WSA
    REAL(8),INTENT(IN)                   :: QROFF(:),QRTRN(:),QRPONDDRN(:),QTRIB(:),QERELS(:),QDEEPPERC(:)
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(RootZoneType),INTENT(IN)        :: RootZone
    TYPE(AppStreamType),INTENT(IN)       :: AppStream
    TYPE(AppGWType),INTENT(IN)           :: AppGW
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    INTEGER :: indxWSA,iNVars
    REAL(8) :: rVarValues_Work(WSA%iNVars)
    
    !Return if WSAs are not simulated
    IF (WSA%iNWSA .EQ. 0) THEN
        iStat = 0
        RETURN
    END IF
    
    !Aggregate variables
    CALL AggregateVars(WSA,QROFF,QRTRN,QRPONDDRN,QTRIB,QERELS,QDEEPPERC,TimeStep,AppGrid,RootZone,AppStream,AppGW,StrmGWConnector,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Return if ANNs are not defined
    IF (.NOT. WSA%lANN_Defined) RETURN
    
    !Loop over WSAs
    DO indxWSA=1,WSA%iNWSA
        iNVars                    = SIZE(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList)
        rVarValues_Work(1:iNVars) = WSA%rVarValues(WSA%VarAggregation(indxWSA)%iUniqueVarIndexList,indxWSA)
        CALL ExecuteANNModel(WSA%ANN(indxWSA),1,rVarValues_Work(1:iNVars))
    END DO
    
    
  CONTAINS
  
  
    ! #############################################################
    ! ### APPLY ANN MODEL
    ! #############################################################
    RECURSIVE SUBROUTINE ExecuteANNModel(ANN,iLayer,rInput)
      TYPE(ANNModelType)        :: ANN
      INTEGER,INTENT(IN)        :: iLayer
      REAL(8),TARGET,INTENT(IN) :: rInput(:)
      
      !Local variables
      INTEGER         :: indxNeuron
      REAL(8)         :: rVal
      REAL(8),TARGET  :: rActivation(SIZE(ANN%rVarMin))
      REAL(8),POINTER :: pInput(:)
      
      !Initialize
      iStat  =  0
      pInput => rInput
      
      !If first layer, scale input variables if desired
      IF (iLayer .EQ. 1) THEN
          IF (ANN%lScalingIsOn) THEN
              rActivation =  (rInput - ANN%rVarMin) / (ANN%rVarMax-ANN%rVarMin)
              pInput      => rActivation 
          END IF  
      END IF  
      
      !Calculate
      DO indxNeuron=1,ANN%Layers(iLayer)%iNNeurons
          rVal = SUM(pInput * ANN%Layers(iLayer)%rWeights(:,indxNeuron)) + ANN%Layers(iLayer)%rBias(indxNeuron)
          IF (iLayer .LT. ANN%iNLayers) THEN
              !Sigmoid activation
              IF (ANN%Layers(iLayer)%iActivation(indxNeuron) .EQ. f_iAct_Sigmoid) THEN
                  ANN%Layers(iLayer)%rOutputValues(indxNeuron) = 1d0 / (1d0+EXP(-rVal)) 
              !ReLU activation
              ELSEIF (ANN%Layers(iLayer)%iActivation(indxNeuron) .EQ. f_iAct_ReLU) THEN  
                  ANN%Layers(iLayer)%rOutputValues(indxNeuron) = MAX(0.0,rVal) 
              !No activation 
              ELSE
                  ANN%Layers(iLayer)%rOutputValues(indxNeuron) = rVal
              END IF  
          ELSE
              ANN%Layers(iLayer)%rOutputValues(indxNeuron) = rVal
          END IF  
      END DO
      
      !Next layer
      IF (iLayer .LT. ANN%iNLayers) THEN
          CALL ExecuteANNModel(ANN,iLayer+1,ANN%Layers(iLayer)%rOutputValues)
      !If this is the output layer, unscale, apply conversion factor and limit at MIN and MAX values
      ELSE
          ANN%Layers(iLayer)%rOutputValues = ANN%Layers(iLayer)%rOutputValues * (ANN%rOutputMax - ANN%rOutputMin) + ANN%rOutputMin         
          ANN%Layers(iLayer)%rOutputValues = MAX(MIN(ANN%Layers(iLayer)%rOutputValues , ANN%rOutputMax) , ANN%rOutputMin)         
          ANN%Layers(iLayer)%rOutputValues = ANN%rConvFactor * ANN%Layers(iLayer)%rOutputValues         
      END IF  
      
    END SUBROUTINE ExecuteANNModel
    
  END SUBROUTINE Calculate
  
  
  ! -------------------------------------------------------------
  ! --- AGGREGATE VARIABLES
  ! -------------------------------------------------------------
  SUBROUTINE AggregateVars(WSA,QROFF,QRTRN,QRPONDDRN,QTRIB,QERELS,QDEEPPERC,TimeStep,AppGrid,RootZone,AppStream,AppGW,StrmGWConnector,iStat)
    TYPE(WSA_ANN_Type)                   :: WSA
    REAL(8),INTENT(IN)                   :: QROFF(:),QRTRN(:),QRPONDDRN(:),QTRIB(:),QERELS(:),QDEEPPERC(:)
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(RootZoneType),INTENT(IN)        :: RootZone
    TYPE(AppStreamType),INTENT(IN)       :: AppStream
    TYPE(AppGWType),INTENT(IN)           :: AppGW
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'AggregateVars'
    INTEGER                                :: indxWSA,iVarType,iLocType,iWSAID,ErrorCode,iNLocs,iVar, &
                                              indxLoc,indxUpNode,iUpNode,iGenericTSCol,iMonth
    REAL(8),DIMENSION(SIZE(QROFF))         :: rStrmFlows,rGainFromGW,rGainFromStrm,rNetBypassOutflows
    REAL(8),DIMENSION(AppGrid%NElements)   :: rElemPrecip,rElemETa,rElemPump,rElemLUAreas,rElemAgUrbAreas,rElemNVRVAreas
    CLASS(*),POINTER                       :: pData
    INTEGER,POINTER                        :: pLocations(:)
    TYPE(VarAggregationType),TARGET        :: aVarAggregation
    INTEGER,ALLOCATABLE                    :: iUpstrmNodes(:)    
    REAL(8),ALLOCATABLE                    :: rWorkArray(:)
    
    !Initialize
    iStat          = 0
    WSA%rVarValues = 0.0
    
    !Get stream flows, gain from GW, net bypass inflows
    CALL AppStream%GetFlows(rStrmFlows)
    CALL AppStream%GetNetBypassInflows(rNetBypassOutflows)             ;  rNetBypassOutFlows = -rNetBypassOutflows 
    CALL StrmGWConnector%GetFlowAtAllStrmNodes(.TRUE.,rGainFromStrm)   ;  rGainFromGW        = -rGainFromStrm
    CALL StrmGWConnector%GetFlowAtAllStrmNodes(.FALSE.,rGainFromStrm)  ;  rGainFromGW        = rGainFromGW - rGainFromStrm
    
    !Get element precip, ETa, ag-urban and native-riparian areas
    CALL RootZone%GetElementPrecip(AppGrid%AppElement%Area,rElemPrecip)
    CALL RootZone%GetElementActualET(AppGrid%AppElement%Subregion,rElemETa)
    CALL RootZone%GetElementAgAreas(rElemAgUrbAreas)
    CALL RootZone%GetElementUrbanAreas(rElemLUAreas)  ;  rElemAgUrbAreas = rElemAgUrbAreas + rElemLUAreas
    CALL RootZone%GetElementNativeVegAreas(rElemNVRVAreas)
    
    !Get element pumping
    CALL AppGW%GetElementPumpActual(rElemPump)
    
    !Loop over each WSA and aggregate its input variables
    DO indxWSA=1,WSA%iNWSA
        iWSAID = WSA%IDs(indxWSA)
        
        !Reset WSA variable aggregation data linked list
        CALL WSA%VarAggregation(indxWSA)%Reset()
        
        !Loop over variable aggregation
        DO 
            !Obtain the variable aggregation data
            pData => WSA%VarAggregation(indxWSA)%GetCurrentValue()
            IF (.NOT. ASSOCIATED(pData)) EXIT
            SELECT TYPE(pData)
                TYPE IS (VarAggregationType)
                    aVarAggregation = pData
            END SELECT
            
            !Variable and location types
            iVar       =  aVarAggregation%iVarIndex
            iVarType   =  WSA%iVarTypes(iVar)
            iLocType   =  aVarAggregation%iLocationType
            iNLocs     =  aVarAggregation%iNLocations
            pLocations => aVarAggregation%iLocations
                    
            !Work array
            DEALLOCATE (rWorkArray , STAT=ErrorCode)
            ALLOCATE (rWorkArray(iNLocs))
            
            !Aggregate
            SELECT CASE (iVarType)
                CASE (f_iVarType_GenericTS)
                    iGenericTSCol                = WSA%iGenericTSCol(iVar)
                    WSA%rVarValues(iVar,indxWSA) = WSA%GenericVarTSInFile%rValues(iGenericTSCol)
                
                CASE (f_iVarType_Season)
                    iMonth = ExtractMonth(TimeStep%CurrentDateAndTime)
                    !Winter
                    IF (iMonth.GE.1  .AND. iMonth.LE.3) THEN
                        WSA%rVarValues(iVar,indxWSA) = 485.0
                    !Spring    
                    ELSEIF (iMonth.GE.4  .AND.  iMonth.LE.6) THEN
                        WSA%rVarValues(iVar,indxWSA) = 491.0
                    !Summer    
                    ELSEIF (iMonth.GE.7  .AND.  iMonth.LE.9) THEN
                        WSA%rVarValues(iVar,indxWSA) = 496.0
                    !Fall
                    ELSE
                        WSA%rVarValues(iVar,indxWSA) = 416.0
                    END IF
                    
                CASE (f_iVarType_SurfaceInflow)
                    !User-defined boundary inflows 
                    CALL AppStream%GetInflows_AtSomeNodes(pLocations,rWorkArray)
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rWorkArray)
                    !Tributary inflows
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(QTRIB(pLocations))
                    !Flows into exterior nodes (relative to the iLocations list) from other model nodes
                    DO indxLoc=1,iNLocs
                        CALL AppStream%GetUpstrmNodes(pLocations(indxLoc),iUpstrmNodes) 
                        DO indxUpNode=1,SIZE(iUpstrmNodes)
                            iUpNode = iUpstrmNodes(indxUpNode)
                            IF (LocateInList(iUpNode,pLocations) .EQ. 0) THEN
                                WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + rStrmFlows(iUpNode)
                            END IF
                        END DO
                    END DO
                    
                CASE (f_iVarType_Runoff)    
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(QROFF(pLocations))
                    
                CASE (f_iVarType_Return)    
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(QRTRN(pLocations))
                    
                CASE (f_iVarType_PondDrain)    
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(QRPONDDRN(pLocations))
                    
                CASE (f_iVarType_GainFromGW)    
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rGainFromGW(pLocations))

                CASE (f_iVarType_DiversionNetBypass)  
                    CALL AppStream%GetActualDiversions_AtSomeNodes(pLocations,rWorkArray,iStat)  ;  IF (iStat .NE. 0) RETURN
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rWorkArray) + SUM(rNetBypassOutflows(pLocations))
                   
                CASE (f_iVarType_Precip)     
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rElemPrecip(pLocations))

                CASE (f_iVarType_ETa)     
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rElemETa(pLocations))

                CASE (f_iVarType_DeepPercRecharge)     
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(QERELS(pLocations)) + SUM(QDEEPPERC(pLocations))

                CASE (f_iVarType_Pumping)     
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rElemPump(pLocations))
                    
                CASE (f_iVarType_AgUrbArea)
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rElemAgUrbAreas(pLocations))

                CASE (f_iVarType_NVRVArea)
                    WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) + SUM(rElemNVRVAreas(pLocations))

            END SELECT
                
            !Convert variable unit
            WSA%rVarValues(iVar,indxWSA) = WSA%rVarValues(iVar,indxWSA) * WSA%rVarFactors(iVar)
            
            !Advance to the next aggregation data
            CALL WSA%VarAggregation(indxWSA)%Next()
        END DO
    END DO
    
  END SUBROUTINE AggregateVars
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE VARIABLE AGGREGATION METHODS
  ! -------------------------------------------------------------
  SUBROUTINE CompileVarAggregation(InFile,AppGrid,AppStream,iStrmNodeIDs,WSA,iStat)
    TYPE(GenericFileType)          :: InFile
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    TYPE(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,TARGET,INTENT(IN)      :: iStrmNodeIDs(:)
    TYPE(WSA_ANN_Type)             :: WSA
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local INTEGER generic list data type
    TYPE,EXTENDS(GenericLinkedListType) :: LocationListType
    END TYPE LocationListType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21),PARAMETER :: ThisProcedure = ModName // 'CompileVarAggregation'
    INTEGER                                :: iStatLocal,iWSAID,iLoc,iWSA,iLocationType,ErrorCode,iLocID,iLocIndex,indx, &
                                              iNLocations,indx_S,indx_L,iVarID,iVar
    INTEGER,TARGET                         :: iNodeIDS(AppGrid%NNodes),iElemIDs(AppGrid%NElements),iRegionIDs(AppGrid%NSubregions)
    CHARACTER                              :: ALine*5000,cLocation*12
    TYPE(VarAggregationType)               :: aVarAggregation,aDummy
    TYPE(LocationListType)                 :: LocationList
    INTEGER,POINTER                        :: pIDs(:)
    INTEGER,ALLOCATABLE                    :: iReachNodes(:),iStrmNodes(:),iElements(:)
    INTEGER,ALLOCATABLE,TARGET             :: iReachIDs(:)
    
    !Initialize
    iNodeIDs   = AppGrid%AppNode%ID
    iElemIDs   = AppGrid%AppElement%ID
    iRegionIDS = AppGrid%AppSubregion%ID
    ALLOCATE (iReachIDs(AppStream%GetNReaches()))  ;  CALL AppStream%GetReachIDs(iReachIDs)
    
    !Allocate memory
    ALLOCATE (WSA%VarAggregation(WSA%iNWSA))
    
    !Read until the end of file
    DO
        !Read one line of data for processing
        CALL InFile%ReadData(ALine,iStatLocal)
        IF (iStatLocal .NE. 0) EXIT
        CALL CleanSpecialCharacters(ALine)
        ALine = ADJUSTL(ALine)
        ALine = StripTextUntilCharacter(ALine,f_cInlineCommentChar)
        CALL ReplaceString(ALine,',',' ',iStat)  ;  IF (iStat .NE. 0) RETURN
        
        !Exit if no more data lines
        IF (LEN_TRIM(ALine) .EQ. 0) EXIT
        
        !WSA and variable IDs
        READ (ALine,*) iWSAID,iVarID
        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
        
        !Make sure WSA and variable IDs are recognized
        iWSA = LocateInList(iWSAID,WSA%IDs)
        IF (iWSA .EQ. 0) THEN
            CALL SetLastMessage('WSA ID '//TRIM(IntToText(iWSAID))//' listed in variable aggregation methods is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iVar = LocateInList(iVarID,WSA%iVarIDs)
        IF (iVar .EQ. 0) THEN
            CALL SetLastMessage('Variable ID '//TRIM(IntToText(iVarID))//' listed for variable aggregation for WSA '//TRIM(IntToText(iWSAID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Initialize variable aggregation data
        DEALLOCATE(aVarAggregation%iLocations , STAT=ErrorCode)
        aVarAggregation           = aDummy
        aVarAggregation%iVarIndex = iVar
        
        !If no more data, add node to WSA info and cycle
        IF (LEN_TRIM(ALine) .EQ. 0) THEN
            CALL WSA%VarAggregation(iWSA)%AddNode(aVarAggregation,iStat)  ;  IF (iStat .NE. 0) RETURN
            CYCLE
        END IF
        
        !Initialize tempory location list
        CALL LocationList%Delete()
        
        !Read location type and make sure it is legit 
        READ (ALine,*) iLocationType
        iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
        IF (LocateInList(iLocationType,f_iLocationTypeList) .EQ. 0) THEN
            CALL SetLastMessage('Location type '//TRIM(IntToText(iLocationType))//' listed for variable aggregation for WSA '//TRIM(IntToText(iWSAID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        aVarAggregation%iLocationType = iLocationType
        SELECT CASE (iLocationType)
            CASE (f_iLocationType_Node)
                pIDs      => iNodeIDs
                cLocation =  'Node'
            CASE (f_iLocationType_Element)
                pIDs      => iElemIDs
                cLocation =  'Element'
            CASE (f_iLocationType_Subregion)
                pIDs      => iRegionIDs
                cLocation =  'Subregion'
            CASE (f_iLocationType_StrmNode)
                pIDs      => iStrmNodeIDs
                cLocation =  'Stream node'
            CASE (f_iLocationType_StrmReach)    
                pIDs      => iReachIDs
                cLocation =  'Stream reach'
        END SELECT
        
        !Read location list
        DO
            !Read location ID
            READ (ALine,*) iLocID
            iLoc = FirstLocation(' ',ALine)  ;  ALine = ADJUSTL(ALine(iLoc:))
            
            !Process location ID
            iLocIndex = LocateInList(iLocID, pIDs)
            IF (iLocIndex .EQ. 0) THEN
                CALL SetLastMessage(TRIM(cLocation)//' '//TRIM(IntToText(iLocID))//' listed for WSA '//TRIM(IntToText(iWSAID))//' variable aggregation locations is not in the IWFM model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            CALL LocationList%AddNode(iLocIndex,iStat)  ;  IF (iStat .NE. 0) RETURN
            
            !Exit if no more location IDs are listed
            IF (LEN_TRIM(ALine) .EQ. 0) EXIT
        END DO
        
        !Convert linked-list location list to array
        CALL LocationList%GetArray(aVarAggregation%iLocations,iStat)  ;  IF (iStat .NE. 0) RETURN 
        aVarAggregation%iNLocations = SIZE(aVarAggregation%iLocations)
        
        !Check consistency b/w variable type and location type for aggregation
        CALL CheckVariableLocationAggregationConsistency(WSA%iVarTypes,aVarAggregation,iStat)   ;  IF (iStat .NE. 0) RETURN
        
        !Convert stream reaches to stream nodes
        IF (iLocationType .EQ. f_iLocationType_StrmReach) THEN
            !Find the total number of stream nodes associated with the reaches
            iNLocations = 0
            DO indx=1,aVarAggregation%iNLocations
                iNLocations = iNLocations + AppStream%GetReachNNodes(aVarAggregation%iLocations(indx))
            END DO
            !Compile stream nodes
            ALLOCATE (iReachNodes(iNLocations))
            indx_S = 0
            DO indx=1,aVarAggregation%iNLocations
                indx_L                       = indx_S + AppStream%GetReachNNodes(aVarAggregation%iLocations(indx))
                CALL AppStream%GetReachStrmNodes(aVarAggregation%iLocations(indx),iStrmNodes,iStat)  ;  IF (iStat .NE. 0) RETURN
                iReachNodes(indx_S+1:indx_L) = iStrmNodes
                indx_S                       = indx_L
            END DO
            !Save stream nodes in the variable aggregation data
            aVarAggregation%iNLocations   = iNLocations
            aVarAggregation%iLocationType = f_iLocationType_StrmNode
            CALL MOVE_ALLOC(iReachNodes , aVarAggregation%iLocations)
        END IF
        
        !Convert subregions to elements
        IF (iLocationType .EQ. f_iLocationType_Subregion) THEN
            !Total number of elements associated with the subregions
            iNLocations = SUM(AppGrid%AppSubregion(aVarAggregation%iLocations)%NRegionElements)
            !Compile elements
            ALLOCATE (iElements(iNLocations))
            indx_S = 0
            DO indx=1,aVarAggregation%iNLocations
                indx_L                     = indx_S + AppGrid%AppSubregion(aVarAggregation%iLocations(indx))%NRegionElements
                iElements(indx_S+1:indx_L) = AppGrid%AppSubregion(aVarAggregation%iLocations(indx))%RegionElements
                indx_S                     = indx_L
            END DO
            !Save elements in the variable aggregation data
            aVarAggregation%iNLocations   = iNLocations
            aVarAggregation%iLocationType = f_iLocationType_Element
            CALL MOVE_ALLOC(iElements , aVarAggregation%iLocations)
        END IF
        
        !Add the variable aggregation data to WSA info
        CALL WSA%VarAggregation(iWSA)%AddNode(aVarAggregation,iStat)  ;  IF (iStat .NE. 0) RETURN
        
    END DO
    
    !Compile list of unique variable indices for each WSA
    DO indx=1,WSA%iNWSA
        CALL WSA%VarAggregation(indx)%GetUniqueVarIndexList(WSA%VarAggregation(indx)%iUniqueVarIndexList)
    END DO
    
    !Clear memory
    CALL LocationList%Delete()
    NULLIFY(pIDs)
    DEALLOCATE (aVarAggregation%iLocations , STAT=ErrorCode)
    
  END SUBROUTINE CompileVarAggregation
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF VARIABLE TYPE AND LISTED LOCATIONS FOR AGGREGATION ARE CONSISTENT
  ! -------------------------------------------------------------
  SUBROUTINE CheckVariableLocationAggregationConsistency(iVarTypes,aVarAggregation,iStat)
    INTEGER,INTENT(IN)                  :: iVarTypes(:)
    TYPE(VarAggregationType),INTENT(IN) :: aVarAggregation
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+43),PARAMETER :: ThisProcedure = ModName // 'CheckVariableLocationAggregationConsistency'
    INTEGER                                :: iVarType,iLocationType  
    
    !Initialize
    iStat         = 0
    iVarType      = iVarTypes(aVarAggregation%iVarIndex)
    iLocationType = aVarAggregation%iLocationType
    
    !Check consistency
    SELECT CASE (iVarType)
        CASE (f_iVarType_GenericTS , f_iVarType_Season)
            !Do nothing

        CASE (f_iVarType_SurfaceInflow , f_iVarType_Runoff , f_iVarType_Return , f_iVarType_PondDrain , f_iVarType_GainFromGW , f_iVarType_DiversionNetBypass)
            IF (.NOT. (iLocationType .EQ. f_iLocationType_StrmNode  .OR.  &
                       iLocationType .EQ. f_iLocationtype_StrmReach       )) THEN
                MessageArray(1) = 'For WSA variable aggregation, location type must be either '//TRIM(IntToText(f_iLocationType_StrmNode))
                MessageArray(2) = ' or '//TRIM(IntToText(f_iLocationType_StrmReach))//' when variable type is '//TRIM(IntToText(iVarType))//'!' 
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
        CASE (f_iVarType_Precip , f_iVarType_ETa , f_iVarType_DeepPercRecharge ,  f_iVarType_AgUrbArea , f_iVarType_NVRVArea)
            IF (.NOT. (iLocationType .EQ. f_iLocationType_Element  .OR.  &
                       iLocationType .EQ. f_iLocationType_Subregion      )) THEN
                MessageArray(1) = 'For WSA variable aggregation, location type must be either '//TRIM(IntToText(f_iLocationType_Element))
                MessageArray(2) = ' or '//TRIM(IntToText(f_iLocationType_Subregion))//' when variable type is '//TRIM(IntToText(iVarType))//'!' 
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF 
        
        CASE (f_iVarType_Pumping)
            IF (.NOT. (iLocationType .EQ. f_iLocationType_Element   .OR.  &
                       iLocationType .EQ. f_iLocationType_Subregion       )) THEN
                MessageArray(1) = 'For WSA variable aggregation, location type must be either '//TRIM(IntToText(f_iLocationType_Element))
                MessageArray(2) = ' or '//TRIM(IntToText(f_iLocationType_Subregion))//' when variable type is '//TRIM(IntToText(iVarType))//'!' 
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF 
                       
    END SELECT
        
  END SUBROUTINE CheckVariableLocationAggregationConsistency
  
  
  ! -------------------------------------------------------------
  ! --- ZERO OUT WSAs
  ! -------------------------------------------------------------
  SUBROUTINE ZeroOut(WSA)
    CLASS(WSA_ANN_Type) :: WSA
    
    !Local variables
    INTEGER :: indxWSA,iOutputLayer
    
    IF (.NOT. WSA%lANN_Defined) RETURN
    
    DO indxWSA=1,WSA%iNWSA
        iOutputLayer                                        = WSA%ANN(indxWSA)%iNLayers
        WSA%ANN(indxWSA)%Layers(iOutputLayer)%rOutputValues = 0.0
    END DO
    
  END SUBROUTINE ZeroOut
  
  
  ! -------------------------------------------------------------
  ! --- WHAT RUN MODE IS THIS?
  ! -------------------------------------------------------------
  FUNCTION IsHistoricalRun(WSA) RESULT(lHistRun)
    CLASS(WSA_ANN_Type),INTENT(IN) :: WSA
    LOGICAL                        :: lHistRun
    
    IF (WSA%iRunMode .EQ. f_iHistoricalRun) THEN
        lHistRun = .TRUE.
    ELSE
        lHistRun = .FALSE.
    END IF
    
  END FUNCTION IsHistoricalRun

END MODULE