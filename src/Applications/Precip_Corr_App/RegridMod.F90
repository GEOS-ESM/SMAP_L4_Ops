      module RegridMod

      use TypesMod
      use GridMod, GridError=>errorHandler

      private
      public :: regrid
      public :: errorHandler

      integer, parameter :: RegridLFOErrorHandle = 1
      integer, parameter :: RegridCPCUErrorHandle = 2
      integer, parameter :: RegridPrecipErrorHandle = 3
      integer, parameter :: RegridCPCUCorrErrorHandle = 4
      integer, parameter :: RegridGenericErrorHandle = 5

      interface regrid

      module procedure regridLFO
      module procedure regridCPCU
      module procedure regridPrecip
      module procedure regridCPCUCorr
      module procedure regridGeneric

      end interface regrid

      integer :: errorCode = 0

      contains

!******************************************************************************
      integer function regridCPCU(inData,outData,method,eodMethod)
!******************************************************************************
! English Name: Regrid CPC Unified (CPCU) Data
! -------------
!
! Purpose: Regrids Climate Prediction Center "Unified" (CPCU) precipitation
! -------- data to the output grid specification using the supplied
!          interpolation method.
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! inData          CPCUDataType       IN  CPCU data to be regridded.
!
! outData         CPCUDataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined on input.
!
! method               integer       IN  external function implementing the
!                                        interpolation method to be used for
!                                        regridding the input grids to the
!                                        output grids.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           11/08/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (CPCUDataType), intent(in)  :: inData
      type (CPCUDataType), intent(inout) :: outData
      integer, external :: method
      integer, external, optional :: eodMethod

!     Local Variables
!     ---------------

      integer :: rc
      type (GenericDataType) :: iData,oData
      integer :: this = RegridCPCUErrorHandle

!     Initialize
!     ==========

      rc = 0
      regridCPCU = -1

!     Interpolate CPCU grids to new grid
!     ==================================

      iData%grid = inData%grid
      oData%grid = outData%grid

      iData%data => inData%data
      oData%data => outData%data

      if (present(eodMethod)) then
        rc = eodMethod(iData,oData)
        if (isError(this,1,rc) .ne. 0) return
      else
        rc = method(iData,oData)
        if (isError(this,1,rc) .ne. 0) return
      endif

      regridCPCU = 0

      end function regridCPCU

!******************************************************************************
      integer function regridLFO(inData,outData,method)
!******************************************************************************
! English Name: Regrid Land Forcing (LFO) data
! -------------
!
! Purpose: Regrids LFO data to the output grid specification using the
! -------- supplied interpolation function.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This routine only operates on equally spaced lat/lon grids.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! inData           LFODataType       IN  LFO data to be interpolated.
!
! outData          LFODataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined on input.
!
! method               integer       IN  external function implementing the
!                                        interpolation method to be used for
!                                        regridding the input grids to the
!                                        output grids.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           11/08/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (LFODataType), intent(in) :: inData
      type (LFODataType), intent(inout) :: outData
      integer, external :: method

!     Local Variables
!     ---------------

      integer :: rc
      type (GenericDataType) :: iData,oData
      integer :: this = RegridLFOErrorHandle

!     Initialize
!     ==========

      rc = 0
      regridLFO = -1

!     Interpolate LFO grids to new grid
!     =================================

      iData%grid = inData%grid
      oData%grid = outData%grid

      iData%data => inData%prectot
      oData%data => outData%prectot
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

      iData%data => inData%precls 
      oData%data => outData%precls 
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

      iData%data => inData%precsno
      oData%data => outData%precsno
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

      iData%data => inData%preccon
      oData%data => outData%preccon
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

      iData%data => inData%airtemp
      oData%data => outData%airtemp
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

      regridLFO = 0

      end function regridLFO

!******************************************************************************
      integer function regridPrecip(inData,outData,method)
!******************************************************************************
! English Name: Regrid Precipitation 
! -------------
!
! Purpose: Regrids precipitation data to the output grid specification using the
! -------- supplied interpolation function.
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! inData        PrecipDataType       IN  precipitation data to be interpolated.
!
! outData       PrecipDataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined on input.
!
! method               integer       IN  external function implementing the
!                                        interpolation method to be used for
!                                        regridding the input grids to the
!                                        output grids.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           11/08/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (PrecipDataType), intent(in)  :: inData
      type (PrecipDataType), intent(inout) :: outData
      integer, external :: method

!     Local Variables
!     ---------------

      integer :: rc
      type (GenericDataType) :: iData,oData
      integer :: this = RegridPrecipErrorHandle

!     Initialize
!     ==========

      rc = 0
      regridPrecip = -1

!     Interpolate Precipitation grids to new grid
!     ===========================================

      iData%grid = inData%grid
      oData%grid = outData%grid

      iData%data => inData%data
      oData%data => outData%data
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return
      
      regridPrecip = 0

      end function regridPrecip

!******************************************************************************
      integer function regridGeneric(inData,outData,method)
!******************************************************************************
! English Name: Regrid Generic Grid Type
! -------------
!
! Purpose: Regrids generic data to the output grid specification using the
! -------- supplied interpolation function.
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! inData       GenericDataType       IN  data to be interpolated.
!
! outData      GenericDataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined on input.
!
! method               integer       IN  external function implementing the
!                                        interpolation method to be used for
!                                        regridding the input grids to the
!                                        output grids.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/16/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GenericDataType), intent(in)  :: inData
      type (GenericDataType), intent(inout) :: outData
      integer, external :: method

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = RegridGenericErrorHandle

!     Initialize
!     ==========

      rc = 0
      regridGeneric = -1

!     Interpolate to the new grid
!     ===========================

      rc = method(inData,outData)
      if (isError(this,1,rc) .ne. 0) return
      
      regridGeneric = 0

      end function regridGeneric

!******************************************************************************
      integer function regridCPCUCorr(inData,outData,method,eodMethod)
!******************************************************************************
! English Name: Regrid CPCU Correction Data
! -------------
!
! Purpose: Regrids correction fields to the output grid specification using the
! -------- supplied interpolation function.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This routine only operates on equally spaced lat/lon grids.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! inData      CPCUCorrDataType       IN  CPCU correction data to be 
!                                        interpolated.
!
! outData     CPCUCorrDataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined on input.
!
! method               integer       IN  external function implementing the
!                                        interpolation method to be used for
!                                        regridding the input grids to the
!                                        output grids.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           11/08/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (CPCUCorrDataType), intent(in) :: inData
      type (CPCUCorrDataType), intent(inout) :: outData
      integer, external :: method
      integer, external, optional :: eodMethod

!     Local Variables
!     ---------------

      integer :: rc
      type (GenericDataType) :: iData,oData
      integer :: this = RegridCPCUCorrErrorHandle

!     Initialize
!     ==========

      rc = 0
      regridCPCUCorr = -1

!     Interpolate CPCU correction grids to new grid
!     =============================================

      iData%grid = inData%grid
      oData%grid = outData%grid

      iData%data => inData%factor
      oData%data => outData%factor
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

      iData%data => inData%residual
      oData%data => outData%residual
      rc = method(iData,oData)
      if (isError(this,1,rc) .ne. 0) return

!     Interpolate EOD grid
!     ====================

      iData%data => inData%eod
      oData%data => outData%eod

      if (present(eodMethod)) then
        rc = eodMethod(iData,oData)
        if (isError(this,1,rc) .ne. 0) return
      else
        rc = method(iData,oData)
        if (isError(this,1,rc) .ne. 0) return
      endif

      regridCPCUCorr = 0

      end function regridCPCUCorr

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc)
!******************************************************************************
! English Name: Is There an Error
! -------------
!
! Purpose: Module error handler registry.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. All module errors are registered in this function. Any change to
! ------    the meaning or definition of the registered error event codes
!           must be reflected in this function.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! errorHandle          integer       IN  Unique integer identifying the
!                                        calling function or subroutine.
!
! errorEvent           integer       IN  Non-zero error code defining the
!                                        error condition to be checked.
!
! rc                   integer       IN  return code associated with the
!                                        error event. A non-zero value
!                                        indicates that the error has
!                                        occurred.
!
! isError              integer      OUT  function return value:
!
!                                        0: no error occurred (rc=0)
!                                       !0: error event code (errorEvent)
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!            6/06/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: rc
      integer, intent(in) :: errorHandle
      integer, intent(in) :: errorEvent

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

!       Regrid LFO Error Handle
!       =======================

        case (RegridLFOErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'RegridLFO (error#',code,'): interpolation error'

          case default

            print *, 'RegridLFO (error#',code,'): unknown error'

        end select

!       Regrid CPCU Error Handle
!       ========================

        case (RegridCPCUErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'RegridCPCU (error#',code,'): interpolation error'

          case default

            print *, 'RegridCPCU (error#',code,'): unknown error'

        end select

!       Regrid Precip Error Handle
!       ==========================

        case (RegridPrecipErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'RegridPrecip (error#',code,'): interpolation error'

          case default

            print *, 'RegridPrecip (error#',code,'): unknown error'

        end select

!       Regrid Generic Error Handle
!       ===========================

        case (RegridGenericErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'RegridGeneric (error#',code,'): interpolation error'

          case default

            print *, 'RegridGeneric (error#',code,'): unknown error'

        end select

!       Regrid CPCU Correction Error Handle
!       ===================================

        case (RegridCPCUCorrErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'RegridCPCUCorr (error#',code,'): interpolation error'

          case default

            print *, 'RegridCPCUCorr (error#',code,'): unknown error'

        end select

        case default

        print *, 'isError: no handle for event:'
        print *, 'handle = ',errorHandle
        print *, 'event  = ',errorEvent

      end select

      isError   = -1
      errorCode = errorEvent

      end function isError

!******************************************************************************
      integer function errorHandler()
!******************************************************************************
!     Module Public Error Handler

      implicit none

      integer :: ilen

      errorHandler = errorCode
      errorCode = 0

      return

      end function errorHandler

      end module RegridMod
