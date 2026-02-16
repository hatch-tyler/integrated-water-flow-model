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
MODULE Package_PrecipitationET
  USE MessageLogger          , ONLY: SetLastMessage       , &
                                     f_iFatal
  USE IOInterface            , ONLY: RealTSDataInFileType
  USE TimeSeriesUtilities    , ONLY: TimeStepType
  USE Class_AtmosphericData  , ONLY: AtmosphericDataType
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
  PUBLIC :: PrecipitationType , &
            ETType                              

  
  ! -------------------------------------------------------------
  ! --- PRECIPITATION DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AtmosphericDataType) :: PrecipitationType
  END TYPE PrecipitationType
  
  
  ! -------------------------------------------------------------
  ! --- ET DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AtmosphericDataType) :: ETType
      LOGICAL                    :: lCropCoeffFile_Defined = .FALSE.
      TYPE(RealTSDataInFileType) :: CropCoeffFile
  CONTAINS
      PROCEDURE,PASS :: New                                     => ETType_New
      PROCEDURE,PASS :: Kill                                    => ETType_Kill
      PROCEDURE,PASS :: GetValues                               => ETType_GetValues
      PROCEDURE,PASS :: GetNKcColumns                           => ETType_GetNKcColumns
      PROCEDURE,PASS :: IsUpdated                               => ETType_IsUpdated
      PROCEDURE,PASS :: AtmosphericData_ReadTSData              => ETType_ReadTSData
      PROCEDURE,PASS :: AtmosphericData_ReadTSData_ForTimeRange => ETType_ReadTSData_ForTimeRange
      PROCEDURE,PASS :: CheckColNum_ForKc                       => ETType_CheckColNum_ForKc
  END TYPE ETType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                      :: f_iModNameLen = 25
  CHARACTER(LEN=f_iModNameLen),PARAMETER :: f_cModName    = 'Package_PrecipitationET::'


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
  ! --- NEW ET DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE ETType_New(AtmosphericData,cFileName,cWorkingDirectory,cDataName,TimeStep,iStat,cCropCoeffFileName) 
    CLASS(ETType),INTENT(OUT)            :: AtmosphericData      !This is ETData
    CHARACTER(LEN=*),INTENT(IN)          :: cFileName,cWorkingDirectory,cDataName
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    INTEGER,INTENT(OUT)                  :: iStat
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cCropCoeffFileName   !Optionally, to be used with ET data type
    
    !Local variables
    REAL(8) :: rDummyFactor(1)
    
    !First, instantiate the parent component of the ETData object
    ASSOCIATE (pETData => AtmosphericData)
        CALL pETData%AtmosphericDataType%New(cFileName,cWorkingDirectory,cDataName,TimeStep,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Now, instantiate crop coeffcient file, if provided
        IF (PRESENT(cCropCoeffFileName)) THEN
            !Is a filename actually defined
            IF (LEN_TRIM(cCropCoeffFileName) .GT. 0) THEN
                !Open file
                CALL pETData%CropCoeffFile%Init(cCropCoeffFileName,cWorkingDirectory,'Crop/habitat coefficient data file',TimeStep%TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
                IF (iStat .NE. 0) RETURN
                
                !Set flag
                pETData%lCropCoeffFile_Defined = .TRUE.
            END IF
        END IF
    END ASSOCIATE
    
  END SUBROUTINE ETType_New
  
  
  
  
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
  ! --- KILL ET DATA
  ! -------------------------------------------------------------
  SUBROUTINE ETType_Kill(AtmosphericData)
    CLASS(ETType),INTENT(INOUT) :: AtmosphericData
    
    ASSOCIATE (pETData => AtmosphericData)
        !Kill parent object
        CALL pETData%AtmosphericDataType%Kill()
        
        !Kill the extension data
        IF (pETData%lCropCoeffFile_Defined) THEN
            CALL pETData%CropCoeffFile%Close()
            pETData%lCropCoeffFile_Defined = .FALSE.
        END IF
    END ASSOCIATE
    
  END SUBROUTINE ETType_Kill

  
  

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
  ! --- GET ET DATA VALUES
  ! -------------------------------------------------------------
  FUNCTION ETType_GetValues(AtmosphericData,iCols,iCropCoeffCols) RESULT(rValues)
    CLASS(ETType),INTENT(IN)    :: AtmosphericData
    INTEGER,INTENT(IN)          :: iCols(:)
    INTEGER,OPTIONAL,INTENT(IN) :: iCropCoeffCols(:) 
    REAL(8)                     :: rValues(SIZE(iCols))
    
    !Local variables
    REAL(8) :: rCropCoeff(SIZE(iCols))
    
    ASSOCIATE (pETData => AtmosphericData)
        !First retreive ET data
        rValues = pETData%AtmosphericDataType%GetValues(iCols)
        
        !Then retrieve crop coeffcients, if available
        IF (pETData%lCropCoeffFile_Defined) THEN
            IF (PRESENT(iCropCoeffCols)) THEN
                rCropCoeff = pETData%CropCoeffFile%rValues(iCropCoeffCols)
                rValues    = rValues * rCropCoeff
            END IF
        END IF
    END ASSOCIATE
    
  END FUNCTION ETType_GetValues
  
  
  ! -------------------------------------------------------------
  ! --- GET ET DATA VALUES
  ! -------------------------------------------------------------
  FUNCTION ETType_GetNKcColumns(ETData) RESULT(iNColumns)
    CLASS(ETType),INTENT(IN) :: ETData
    INTEGER                  :: iNColumns
    
    IF (ETData%lCropCoeffFile_Defined) THEN
        iNColumns = ETData%CropCoeffFile%GetNDataColumns()
    ELSE
        iNColumns = 0
    END IF
    
  END FUNCTION ETType_GetNKcColumns
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ DATA FOR A TIME RANGE FOR A COLUMN
  ! -------------------------------------------------------------
  SUBROUTINE ETType_ReadTSData_ForTimeRange(AtmosphericData,iCol,lForInquiry,lCheckForNegativity,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,iFileReadCode,iStat,iColCoeff)
    CLASS(ETType),INTENT(INOUT) :: AtmosphericData
    INTEGER,INTENT(IN)          :: iCol       
    LOGICAL,INTENT(IN)          :: lForInquiry,lCheckForNegativity
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    INTEGER,INTENT(OUT)         :: nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputValues(:),rOutputDates(:)
    INTEGER,INTENT(OUT)         :: iFileReadCode,iStat
    INTEGER,OPTIONAL,INTENT(IN) :: iColCoeff
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+30) :: ThisProcedure = f_cModName // 'ETType_ReadTSData_ForTimeRange'
    INTEGER                         :: inActualOutput_Coeff
    REAL(8)                         :: rCoeffs(SIZE(rOutputValues)),rOutputDates_Coeff(SIZE(rOutputDates))
    
    ASSOCIATE (pETData => AtmosphericData)
        !Read parent data
        CALL pETData%AtmosphericDataType%ReadTSData(iCol,lForInquiry,lCheckForNegativity,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,iFileReadCode,iStat)
        IF (iStat .NE. 0) RETURN
        
        !If defined, read crop/habitat coeffcient
        IF (pETData%lCropCoeffFile_Defined) THEN
            !Read data
            CALL pETData%CropCoeffFile%ReadTSData(iColCoeff,cBeginDateAndTime,cEndDateAndTime,inActualOutput_Coeff,rCoeffs,rOutputDates_Coeff,iFileReadCode,iStat)
            IF (iStat .NE. 0) RETURN
            
            !Make sure that actual output for ET and coeff are the same
            IF (inActualOutput_Coeff .NE. nActualOutput) THEN
                CALL SetLastMessage('Time intervals for ETo and Kc input data do not match!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Compute ETc
            rOutputValues = rOutputValues * rCoeffs
            
            !Rewind file if necessary
            IF (lForInquiry) CALL pETData%CropCoeffFile%File%RewindFile_To_BeginningOfTSData(iStat)
        END IF
    END ASSOCIATE 
    
  END SUBROUTINE ETType_ReadTSData_ForTimeRange
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR A TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE ETType_ReadTSData(AtmosphericData,TimeStep,lCheckForNegativity,iStat)
    CLASS(ETType),INTENT(INOUT)   :: AtmosphericData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lCheckForNegativity
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    ASSOCIATE (pETData => AtmosphericData)
        !Read ET data
        CALL pETData%AtmosphericDataType%ReadTSData(TimeStep,lCheckForNegativity,iStat)
        IF (iStat .NE. 0) RETURN
        
        !If defined, read crop/habitat coefficient data
        IF (pETData%lCropCoeffFile_Defined) THEN
            CALL pETData%CropCoeffFile%ReadTSData(TimeStep,'crop/habitat coeffcient data',iFileReadCode,iStat)
        END IF
    END ASSOCIATE 

  END SUBROUTINE ETType_ReadTSData
  
  
  
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
  ! --- CHECK IF THERE ARE ENOUGH Kc COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE ETType_CheckColNum_ForKc(ETData,cFileDescriptor,iColPointers,lCheckMinColNum,iStat)
    CLASS(ETType),INTENT(IN)    :: ETData
    CHARACTER(LEN=*),INTENT(IN) :: cFileDescriptor
    INTEGER,INTENT(IN)          :: iColPointers(:)
    LOGICAL,INTENT(IN)          :: lCheckMinColNum
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL ETData%CropCoeffFile%CheckColNum(cFileDescriptor,iColPointers,lCheckMinColNum,iStat)
    
  END SUBROUTINE ETType_CheckColNum_ForKc

  
  ! -------------------------------------------------------------
  ! --- CHECK IF TIMESERIES INPUT DATA IS UPDATED
  ! -------------------------------------------------------------
  PURE FUNCTION ETType_IsUpdated(TSDataInFile) RESULT(lUpdated)
    CLASS(ETType),INTENT(IN) :: TSDataInFile
    LOGICAL                  :: lUpdated
    
    ASSOCIATE (pETData => TSDataInFile)
        IF (pETData%lCropCoeffFile_Defined) THEN
            IF (pETData%AtmosphericDataType%IsUpdated() .OR. pETData%CropCoeffFile%IsUpdated()) THEN
                lUpdated = .TRUE.
            ELSE
                lUpdated = .FALSE.
            END IF
        ELSE
            lUpdated = pETData%AtmosphericDataType%IsUpdated()
        END IF
    END ASSOCIATE
      
  END FUNCTION ETType_IsUpdated
  
  
END MODULE