!******************************************************************************
      module LFOGEOSFPMod
!******************************************************************************
! English Name: Land Forcing - GEOS Module
! -------------
!
! Purpose: Implements the land forcing (LFO) I/O API for reading data
! -------- from GEOS-5 data files.
!
! Language: Fortran 90
! ---------
!
! See Also: LFOBaseMod, GRIDMod, ESMF CFIO
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! LFOReadFile()        integer      PUB  Reads land forcing data from a GEOS-5
!                                        SDF-type file for the specified
!                                        date/time.
!
!   filename            string       IN  name of file containing land forcing
!                                        data.
!
!   idate              integer       IN  date of desired data as ccyymmdd
!
!   itime              integer       IN  time of desired data as hhmmss
!
!   data           LFODataType      OUT  returned land forcing data
!
!   LFOReadFile        integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
!
! errorHandler()       integer      PUB  module error handler to be queried
!                                        after all invocations of the public
!                                        interface routines.
!
!                                        0: success
!                                       !0: error occurred
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!            6/06/2012      J.Ardizzone  created.
!******************************************************************************

      use time
      use TypesMod
      use GridMod, GridError => errorHandler

      use ESMF_CFIOGridMOD
      use ESMF_CFIOVarInfoMOD
      use ESMF_CFIOFileMOD
      use ESMF_CFIOSdfMOD
      use ESMF_CFIOMOD

      private
      public :: LFOfactor
      public :: LFOReadFile
      public :: errorHandler

      real, parameter :: LFOfactor = 86400.0

      integer, parameter :: LFOReadFileErrorHandle   = 1
      integer, parameter :: LFOReadTempErrorHandle   = 2
      integer, parameter :: LFOGetAvgTempErrorHandle = 3

      type (GenericDataType), dimension(2) :: LFOTemp

      integer :: currentDay = 0
      integer :: errorCode = 0

      contains

!******************************************************************************
      integer function LFOReadFile(filename,idate,itime,data)
!******************************************************************************
! English Name: Read File
! -------------
!
! Purpose: Reads land forcing data from a GEOS-5 SDF-type file for the 
! -------- specified date/time.
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
! filename              string       IN  name of file containing land forcing
!                                        data.
!
! idate                integer       IN  date of desired data as ccyymmdd
!
! itime                integer       IN  time of desired data as hhmmss
!
! data             LFODataType      OUT  returned land forcing data
!
! LFOReadFile          integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
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

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      type (LFODataType), intent(inout) :: data
      character(len=*), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      integer :: fmode = 1
      type(ESMF_CFIO) :: cfio
      integer, save :: heap = 0
      real*4, dimension(:,:), pointer, save :: buf => null()
      integer :: nx,ny,km,lm,ngatts
      integer :: this = LFOReadFileErrorHandle

!     Initialize
!     ==========

      heap = GRIDAllocate(heap)

      LFOReadFile = -1
      call GRIDDeallocate(data%grid)
      if (isError(this,7,GRIDError()) .ne. 0) return

!     Access the input file for reading
!     =================================

      cfio =  ESMF_CFIOCreate(cfioObjName='LFO', rc=rc)
      if (isError(this,1,rc) .ne. 0) return

      call ESMF_CFIOSet(cfio, fName=filename, rc=rc)
      if (isError(this,2,rc) .ne. 0) return

      call ESMF_CFIOFileOpen(cfio,fmode,rc=rc)
      if (isError(this,3,rc) .ne. 0) return

      call CFIO_DimInquire (cfio%fid, nx, ny, km, lm, cfio%mVars, ngatts, rc=rc)
      if (isError(this,4,rc) .ne. 0) return

      if (nx .le. 0 .or. ny .le. 0) rc = -1
      if (isError(this,5,rc) .ne. 0) return

!     Allocate memory for storing land forcing fields
!     ===============================================

      data%grid%nx = nx
      data%grid%ny = ny
      data%grid%dx = 360.0 / nx
      data%grid%dy = 180.0 / (ny - 1)
      data%grid%xStart = cfio%grids(1)%lon(1)
      data%grid%yStart = cfio%grids(1)%lat(1)
      data%grid%undef  = cfio%varObjs(1)%amiss
      data%grid%idate  = idate
      data%grid%itime  = itime
      data%grid%scaled = .false.

      rc = GRIDAllocate(data)
      if (isError(this,8,GRIDError()) .ne. 0) return

      if (associated(buf)) deallocate(buf)
      allocate(buf(nx,ny),stat=rc)
      if (isError(this,8,rc) .ne. 0) return

!     Read land forcing fields into data buffers
!     ==========================================

      call ESMF_CFIOVarRead(cfio,'PRECLS',buf,date=idate, curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='PRECLS') .ne. 0) return
      data%precls = dble(buf)

      call ESMF_CFIOVarRead(cfio,'PRECSNO',buf,date=idate,curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='PRECSNO') .ne. 0) return
      data%precsno = dble(buf)

      call ESMF_CFIOVarRead(cfio,'PRECCU',buf,date=idate,curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='PRECCU') .ne. 0) return
      data%preccon = dble(buf)

      call ESMF_CFIOFileClose(cfio)

!     Derive the average temperature
!     from the instantaneous files.
!     ==============================

      rc = LFOGetAvgTemp(data)
      if (isError(this,6,rc,varName='TLML') .ne. 0) return

!     Calculate the total precipitation.
!     Restrict the range of precipitation
!     values: 0 <= prec <= prectot.
!     ===================================

      rc = LFOSumPrecip(data)
      rc = LFORangeCheck(data)

!     Clean-up and return
!     ===================

      call GRIDCloseHeap()
      if (associated(buf)) deallocate(buf)

      LFOReadFile = 0
      return 

      end function LFOReadFile

!******************************************************************************
      integer function LFOSumPrecip(data)
!******************************************************************************

      implicit none

      real :: undef
      type (LFODataType), intent(inout) :: data

      LFOSumPrecip = 0

      undef = data%grid%undef

      data%prectot = undef

      where (data%precls .ne. undef) data%prectot = data%precls +  &
                                              data%preccon + data%precsno

      end function LFOSumPrecip

!******************************************************************************
      integer function LFORangeCheck(data)
!******************************************************************************

      implicit none

      real :: undef
      type (LFODataType), intent(inout) :: data

      LFORangeCheck = 0

      undef = data%grid%undef

      where (data%precls  .ne. undef) &
      data%precls  = max(0.0,min(data%precls,data%prectot))

      where (data%precsno  .ne. undef) &
      data%precsno  = max(0.0,min(data%precsno,data%prectot))

      where (data%preccon .ne. undef) &
      data%preccon  = max(0.0,min(data%preccon,data%prectot))

      end function LFORangeCheck

!******************************************************************************
      integer function LFOGetAvgTemp(data)
!******************************************************************************
! English Name: Read Three Days
! -------------
!
! Purpose: Reads in two days of land forcing data and calculates the
! -------- average temperature centered on the date/time of the supplied data.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Input files are assumed to contain hourly data.
! ------ 
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! data             LFODataType    INOUT  LFO data. On input, time average
!                                        precipitation with defined grid 
!                                        parameters. On output, time averaged
!                                        air temperature added. 
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/17/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

      type (LFODataType), intent(inout) :: data

!     Local Variables
!     ---------------

      integer :: rc
      integer :: idate
      integer :: itime
      integer :: prevDate,prevTime
      integer :: nextDate,nextTime
      character(len=1024) :: filename
      character(len=1024), save :: fNameTmplt

      integer :: this = LFOGetAvgTempErrorHandle

      rc = 0
      LFOGetAvgTemp = -1

      idate = data%grid%idate
      itime = data%grid%itime

      if (currentDay .ne. 0) then

!       Since a two day period has already been
!       loaded, simply shift the data buffer and add
!       the next day to arrive at a two day period
!       bracketing current date. 
!       ============================================

        LFOtemp = cshift(LFOTemp,1)
        nextDate = idate; nextTime = itime;
        call tm_inctime(nextDate,nextTime,0,30,0)
        call tm_string(nextDate,nextTime,fNameTmplt,filename)
        rc = LFOReadTemp(filename,nextDate,nextTime,LFOTemp(2))
        if (isError(this,1,rc,idate=nextDate) .ne. 0) return

      else

        call getenv('LFO_INST_FILENAME_TEMPLATE',fNameTmplt)
        if (len_trim(fNameTmplt) .le. 0) rc = 1
        if (isError(this,2,rc) .ne. 0) return

!       Load two days of data
!       bracketing the specified date.
!       ==============================

        prevDate = idate; prevTime = itime;
        call tm_inctime(prevDate,prevTime,0,-30,0)
        call tm_string(prevDate,prevTime,fNameTmplt,filename)
        rc = LFOReadTemp(filename,prevDate,prevTime,LFOtemp(1))
        if (isError(this,1,rc,idate=prevDate) .ne. 0) return

        nextDate = idate; nextTime = itime;
        call tm_inctime(nextDate,nextTime,0,30,0)
        call tm_string(nextDate,nextTime,fNameTmplt,filename)
        rc = LFOReadTemp(filename,nextDate,nextTime,LFOtemp(2))
        if (isError(this,1,rc,idate=nextDate) .ne. 0) return

      endif

!     Calculate the average temperature
!     =================================

      data%airtemp = data%grid%undef

      where (LFOtemp(1)%data .ne. LFOtemp(1)%grid%undef) 
        data%airtemp = 0.5 * (LFOtemp(1)%data + LFOtemp(2)%data)
      endwhere

      currentDay    = idate
      LFOGetAvgTemp = 0

      end function LFOGetAvgTemp

!******************************************************************************
      integer function LFOReadTemp(filename,idate,itime,data)
!******************************************************************************
! English Name: Read Temperature
! -------------
!
! Purpose: Reads in temperature data from GEOS-5 instantaneous land forcing
! -------- files and returns the average temperature centered at the
!          specified date/time.
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
! filename              string       IN  name of file containing time averaged
!                                        land forcing data (tavg1_2d_lfo). 
!
! idate                integer       IN  date of desired data as ccyymmdd
!
! itime                integer       IN  time of desired data as hhmmss
!
! data             LFODataType    INOUT  LFO data. On input, time average
!                                        precipitation with defined grid
!                                        parameters. On output, time averaged
!                                        air temperature added.
!
! LFOReadTemp          integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
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

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      type (GenericDataType), intent(inout) :: data
      character(len=*), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      real*4 :: undef
      integer :: fmode = 1
      type(ESMF_CFIO) :: cfio
      integer, save :: heap = 0
      real*4, dimension(:,:), pointer, save :: buf => null()
      integer :: nx,ny,km,lm,ngatts
      integer :: this = LFOReadTempErrorHandle

!     Initialize
!     ==========

      heap = GRIDAllocate(heap)

      LFOReadTemp = -1
      call GRIDDeallocate(data%grid)
      if (isError(this,7,GRIDError()) .ne. 0) return

!     Access the input file for reading
!     =================================

      cfio =  ESMF_CFIOCreate(cfioObjName='LFO_INST', rc=rc)
      if (isError(this,1,rc) .ne. 0) return

      call ESMF_CFIOSet(cfio, fName=filename, rc=rc)
      if (isError(this,2,rc) .ne. 0) return

      call ESMF_CFIOFileOpen(cfio,fmode,rc=rc)
      if (isError(this,3,rc) .ne. 0) return

      call CFIO_DimInquire (cfio%fid, nx, ny, km, lm, cfio%mVars, ngatts, rc=rc)
      if (isError(this,4,rc) .ne. 0) return

      if (nx .le. 0 .or. ny .le. 0) rc = -1
      if (isError(this,5,rc) .ne. 0) return

      data%grid%nx = nx
      data%grid%ny = ny
      data%grid%dx = 360.0 / nx
      data%grid%dy = 180.0 / (ny - 1)
      data%grid%xStart = cfio%grids(1)%lon(1)
      data%grid%yStart = cfio%grids(1)%lat(1)
      data%grid%undef  = cfio%varObjs(1)%amiss
      data%grid%idate  = idate
      data%grid%itime  = itime

      undef = data%grid%undef
      
      rc = GRIDAllocate(data)
      if (isError(this,8,GRIDError()) .ne. 0) return

      if (associated(buf)) deallocate(buf)
      allocate(buf(nx,ny),stat=rc)
      if (isError(this,8,rc) .ne. 0) return

!     Read land forcing fields into data buffers
!     ==========================================

      call ESMF_CFIOVarRead(cfio,'TLML',buf,date=idate, curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='TLML') .ne. 0) return
      data%data = dble(buf)

      call ESMF_CFIOFileClose(cfio)

!     Close file and return
!     =====================

      call GRIDCloseHeap()
      if (associated(buf)) deallocate(buf)

      LFOReadTemp = 0
      return 

      end function LFOReadTemp

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,varName,idate)
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
! varName               string   OPT,IN  variable name associated with an
!                                        error event.
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
      integer, intent(in), optional :: idate
      character(len=*), intent(in), optional :: varName

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

!       LFOReadFile Error Handle
!       ========================

        case (LFOReadFileErrorHandle, LFOReadTempErrorHandle)

        select case (errorEvent)

          case (1,2,3)

            print *, 'LFORead (error#',code,'): Error creating CFIO object'

          case (4)

            print *, 'LFORead (error#',code,'): Error retrieving file dimensions'

          case (5)

            print *, 'LFORead (error#',code,'): bad lat/lon dimension'

          case (6)

            print *, 'LFORead (error#',code,'): error reading data'
            print *, 'LFORead: variable = ',varName

          case (7)

            print *, 'LFORead (error#',code,'): error deallocating data'

          case (8)

            print *, 'LFORead (error#',code,'): error allocating memory'

          case default

            print *, 'LFORead (error#',code,'): unknown error'

        end select

!       LFOGetAvgTemp Error Handle
!       ==========================

        case (LFOGetAvgTempErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOGetAvgTemp (error#',code,'): Error reading ', &
                     'temperature data.'
            print *, 'LFOGetAvgTemp: idate = ',idate

          case (2)

            print *, 'LFOGetAvgTemp (error#',code,'): environment variable', &
                     ' undefined for LFO_INST_FILENAME_TEMPLATE'

          case default

            print *, 'LFOGetAvgTemp: (error#',code,'): unknown error'

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

      end module LFOGEOSFPMod
