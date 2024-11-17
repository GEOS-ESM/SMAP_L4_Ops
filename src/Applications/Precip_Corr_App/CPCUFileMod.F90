      module CPCUFileMod

      use TypesMod
      use GridMod, GridError=>errorHandler
      use RegridMethodMod, RegridError=>errorHandler
      use time

      private
      public :: CPCUfactor
      public :: CPCURead2Days
      public :: CPCUScaleDaily
      public :: CPCUGetDay
      public :: CPCUGetEOD
      public :: CPCUReadFile
      public :: CPCUReadEOD
      public :: CPCU_EODHours
      public :: errorHandler

      real, parameter :: CPCUfactor = 0.1

      integer, parameter :: CPCURead2DaysErrorHandle = 1
      integer, parameter :: CPCUReadFileErrorHandle = 2
      integer, parameter :: CPCUReadEODErrorHandle = 3
      integer, parameter :: CPCUGetDayErrorHandle = 4
      integer, parameter :: CPCUGetEODErrorHandle = 5
      integer, parameter :: CPCUScaleDailyErrorHandle = 6
      integer, parameter :: CPCUScaleDayErrorHandle = 7

      integer, parameter :: CPCU_NLON  = 720
      integer, parameter :: CPCU_NLAT  = 360
      integer, parameter :: CPCU_UNIT  = 78
      real, parameter    :: CPCU_DX    = 0.5
      real, parameter    :: CPCU_DY    = 0.5
      real, parameter    :: CPCU_STLON = 0.25
      real, parameter    :: CPCU_STLAT = -89.75
      real, parameter    :: CPCU_UNDEF = -999.0

      integer, parameter, dimension(7) :: CPCU_EODHours=(/6,12,15,18,24,27,30/)

      integer :: errorCode = 0
      integer :: currentDay = 0
      integer :: last_pentad = 0
      type (CPCUDataType) :: EODData
      type (CPCUDataType), dimension(2) :: CPCUData
      type (CPCUDataType) :: CPCUscale

      contains

!******************************************************************************
      integer function CPCURead2Days(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Read Two Days
! -------------
!
! Purpose: Reads in two days of CPC Unified daily precipitation from the
! -------- "real-time" files at the specified day and the previous day.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Real-time CPCU files are retrieved from:
! ------    ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/RT/
!
!        2. This function assumes that requests for data are chronological.
!           This is necessary to optimize data storage and reduce duplicate
!           I/O requests due to the revolving 2-day period. 
!           
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! fNameTmplt            string       IN  filename template using GrADS time
!                                        token conventions (see module prolog).
!
! idate                integer       IN  specified (current) date as ccyymmdd.
!
! itime                integer       IN  time as hhmmss (ignored)
!
! CPCURead2Days        integer      OUT  function return value:
!
!                                        0: successful return
!                                       -1: error occurred 
!
! CPCUData(2)     CPCUDataType    INOUT  internal data buffer containing the
!                                        2-day period of daily precipitation
!                                        data as follows:
!
!                                        (1): current day
!                                        (2): next day
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!            6/14/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt

!     Local Variables
!     ---------------

      integer :: rc
      integer :: nextDate,nextTime
      integer :: currDate,currTime

      integer :: this = CPCURead2DaysErrorHandle

      CPCURead2Days = -1

      if (currentDay .ne. 0) then

!       Since a 2-day period has already been loaded,
!       simply shift the data buffer and add the
!       next day to arrive at the 2-day period.
!       ============================================

        CPCUdata = cshift(CPCUdata,1)

        nextDate = idate; nextTime = itime;
        call tm_inctime(nextDate,nextTime,24,0,0)
        rc = CPCUReadFile(fNameTmplt,nextDate,nextTime,CPCUdata(2))
        if (isError(this,1,rc,idate=nextDate) .ne. 0) return

      else

!       Load two days of data including the day
!       following the current day.
!       =======================================

        currDate = idate; currTime = 0;
        rc = CPCUReadFile(fNameTmplt,currDate,currTime,CPCUdata(1))
        if (isError(this,1,rc,idate=currDate) .ne. 0) return

        nextDate = idate; nextTime = itime;
        call tm_inctime(nextDate,nextTime,24,0,0)
        rc = CPCUReadFile(fNameTmplt,nextDate,nextTime,CPCUdata(2))
        if (isError(this,1,rc,idate=nextDate) .ne. 0) return

      endif

      currentDay   = idate
      CPCURead2Days = 0

      end function CPCURead2Days

!******************************************************************************
      integer function CPCUScaleDaily(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Scale the CPCU Daily Observations
! -------------
!
! Purpose: Reads in global CPCU/GPCP scale factors and applies the scaling
! -------- factors to the CPCU data.
!
! Language: Fortran 90
! ---------
!
! Prerequisites: CPCURead2Days()
! --------------
!
! Notes: 1. The scale factors are applied to ensure consistency between the
! ------    precipitation corrections based on CPCU and GPCPv2.2 data. See the
!           following document for more information:
!
!           "Technical Report Series on Global Modeling and Data Assimilation,
!            Volume 35 Randal D. Koster, Editor: Observation-Corrected
!            Precipitation Estimates in GEOS-5"
!
! Usage: rc = CPCUScaleDaily(fNameTmplt,idate,itime)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! fNameTmplt            string       IN  filename template using Unix date
!                                        command conventions (see module prolog)
!
! idate                integer       IN  specified (current) date as ccyymmdd.
!
! itime                integer       IN  time as hhmmss (ignored)
!
! rc                   integer      OUT  function return value:
!
!                                        0: successful return
!                                       -1: error occurred 
!
! exception                              Thrown Exceptions:
!
!                                        1: error scaling data
!
! CPCUdata        CPCUDataType    INOUT  On input, internal data buffer
!                                        containing daily global CPCU
!                                        precipitation. On output, scaled
!                                        precipitation (see CPCUscale).
!
! CPCUscale       CPCUDataType      OUT  Scale factors read from file for
!                                        pentad corresponding to the buffered
!                                        CPCU days.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/29/2014      J.Ardizzone  created.
!           03/24/2015      J.Ardizzone  adapted from CMAP scale function to
!                                        apply scaling to CPCU data.
!******************************************************************************

      implicit none
      integer :: this = CPCUScaleDailyErrorHandle

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt

!     Local Variables
!     ---------------

      integer :: rc
      integer :: ndate

      CPCUScaleDaily = 0
      if (len_trim(fNameTmplt) .le. 0) return

      rc = 0
      CPCUScaleDaily = -1

!     Scale the two days of CPCU data.
!     ================================

      ndate = CPCUdata(1)%grid%idate
      rc = CPCUScaleDay(fNameTmplt,ndate,CPCUdata(1))
      if (isError(this,1,rc,idate=ndate) .ne. 0) return

      ndate = CPCUdata(2)%grid%idate
      rc = CPCUScaleDay(fNameTmplt,ndate,CPCUdata(2))
      if (isError(this,1,rc,idate=ndate) .ne. 0) return

      CPCUScaleDaily = 0

      return

      end function CPCUScaleDaily

!******************************************************************************
      integer function CPCUScaleDay(fNameTmplt,idate,dayOfData)
!******************************************************************************
! English Name: Scale a CPCU Day of Data
! -------------
!
! Purpose: Reads in global CPCU/GPCP scale factors and applies the scaling
! -------- factors to a single day of CPCU data.
!
! Language: Fortran 90
! ---------
!
! Prerequisites: CPCURead2Days()
! --------------
!
! Notes: 1. The scale factors are applied to ensure consistency between the
! ------    precipitation corrections based on CPCU and GPCPv2.2 data. See the
!           following document for more information:
!
!           "Technical Report Series on Global Modeling and Data Assimilation,
!            Volume 35 Randal D. Koster, Editor: Observation-Corrected
!            Precipitation Estimates in GEOS-5"
!
! Usage: rc = CPCUScaleDay(fNameTmplt,idate,itime)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! fNameTmplt            string       IN  filename template using Unix date
!                                        command conventions (see module prolog)
!
! idate                integer       IN  day as ccyymmdd.
!
! itime                integer       IN  time as hhmmss (ignored)
!
! rc                   integer      OUT  function return value:
!
!                                        0: successful return
!                                       -1: error occurred
!
! exception                              Thrown Exceptions:
!
!                                        1: error reading scaling data
!
! CPCUscale       CPCUDataType      OUT  Scale factors read from file for
!                                        pentad corresponding to the buffered
!                                        CPCU days.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           03/24/2015      J.Ardizzone  created.
!******************************************************************************

      implicit none
      integer :: this = CPCUScaleDayErrorHandle

      integer, intent(in) :: idate
      character(len=*), intent(in) :: fNameTmplt
      type (CPCUDataType), intent(inout) :: dayOfData

!     Local Variables
!     ---------------

      integer :: rc
      type (PentadDataType) :: pentad

      CPCUScaleDay = 0
      if (dayOfData%grid%scaled) return

      CPCUScaleDay = -1

!     Refresh scaling factors if a new pentad is needed.
!     ==================================================

      pentad = tm_pentad(idate)

      if (pentad%index .ne. last_pentad) then 

        print *, 'Reading CPCU scale factors for pentad: ',pentad%index

        rc = CPCUReadFile(fNameTmplt,idate,0,CPCUscale,pentad=pentad%index)
        if (isError(this,1,rc,idate=idate) .ne. 0) return

        last_pentad = pentad%index

      endif

!     Apply the scale factors
!     =======================

      where (CPCUscale%data .eq. CPCUscale%grid%undef) &
                 dayOfData%data = dayOfData%grid%undef

      where (dayOfData%data .ne. dayOfData%grid%undef) &
                 dayOfData%data = dayOfData%data * CPCUscale%data

      dayOfData%grid%scaled = .true.

      CPCUScaleDay = 0

      end function CPCUScaleDay
!******************************************************************************
      integer function CPCUReadFile(filename,idate,itime,precip,pentad)
!******************************************************************************
! English Name: Read File
! -------------
!
! Purpose: Reads precipitation data from a CPCU global gauge-based
! -------- precipitation file. This function can also be used to read CPCU
!          EOD or scaling files.
!
! Language: Fortran 90
! ---------
!
! See Also: CPCUReadEOD(), CPCUScaleDay()
! ---------
!
! Notes: 1. CPCU daily precipitation files are flat binary (little endian). 
! ------    There is no information other than the precipitation data.
!           Therefore, users must exercise caution when associating a date/time 
!           with a filename. Use filename templates if the date/time is part of
!           the filename (see "filename" argument).
!
!        2. Real-time CPCU files are retrieved from:
!           ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/RT/
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  name of file containing CPCU daily
!                                        precipitation data. The filename
!                                        may be a template using the GrADS
!                                        time token conventions.
!
! idate                integer       IN  date of data on file as ccyymmdd
!
! itime                integer       IN  time of data on file as hhmmss
!                                        (ignored)
!
! precip          CPCUDataType      OUT  returned CPCU data
!
! CPCUReadFile         integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
!
!                                        Error handler event codes:
!
!                                        1: error opening file
!                                        2: error allocating memory
!                                        3: error reading data
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           06/14/2012      J.Ardizzone  created.
!           03/24/2015      J.Ardizzone  added code to read scale factors from
!                                        CPCU/GPCP pentad file.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      integer, intent(in), optional :: pentad
      character(len=*), intent(in) :: filename
      type (CPCUDataType), intent(inout) :: precip

!     Local Variables
!     ---------------

      integer :: i,j
      integer :: word_size
      integer :: rc,nx,ny,iunit
      integer, save :: heap = 0
      character(len=1024) :: fname
      integer :: this = CPCUReadFileErrorHandle
      real*4, dimension(CPCU_NLON,CPCU_NLAT) :: buf

      CPCUReadFile = -1

      nx    = CPCU_NLON
      ny    = CPCU_NLAT
      iunit = CPCU_UNIT

      word_size = 4
      if (present(pentad)) word_size = 8

      heap = GRIDAllocate(heap)

      call GRIDDeallocate(precip%grid)
      if (isError(this,1,GRIDError()) .ne. 0) return

!     Open the CPCU Precipitation file.
!     =================================

      call tm_string(idate,0,filename,fname)
      open(unit=iunit,file=fname,form='unformatted', &
           access='direct',recl=nx*ny*word_size,status='old',iostat=rc)
      if (isError(this,2,rc,fName=fname) .ne. 0) return

!     Allocate space and read file
!     ============================

      precip%grid%nx = nx
      precip%grid%ny = ny
      rc = GRIDAllocate(precip)
      if (isError(this,1,GRIDError()) .ne. 0) return

      if (present(pentad)) then

        read(iunit,rec=pentad,iostat=rc) precip%data
        if (isError(this,2,rc,fName=fname) .ne. 0) return

      else

        read(iunit,rec=1,iostat=rc) buf
        if (isError(this,2,rc,fName=fname) .ne. 0) return

        precip%data = dble(buf)

      endif

      where (precip%data .le. -900.0) precip%data = CPCU_UNDEF

      where (precip%data .ne. CPCU_UNDEF) &
              precip%data = max(0.0,precip%data)

      precip%grid%dx       = CPCU_DX
      precip%grid%dy       = CPCU_DY
      precip%grid%idate    = idate
      precip%grid%itime    = 0
      precip%grid%undef    = CPCU_UNDEF
      precip%grid%xStart   = CPCU_STLON
      precip%grid%yStart   = CPCU_STLAT
      precip%grid%scaled   = .false.

      close (unit=iunit)

      call GRIDCloseHeap()

      CPCUReadFile = 0

      end function CPCUReadFile

!******************************************************************************
      integer function CPCUReadEOD(filename)
!******************************************************************************

      implicit none

      character(len=*), intent(in) :: filename

      integer :: rc
      integer :: this = CPCUReadEODErrorHandle

      CPCUReadEOD = -1

      rc = CPCUReadFile(filename,19990101,0,EODData)
      if (isError(this,1,rc,fName=filename) .ne. 0) return

      CPCUReadEOD = 0

      end function CPCUReadEOD

!******************************************************************************
      integer function CPCUGetDay(day,dailyPrecip)
!******************************************************************************

      implicit none 

      integer, intent(in) :: day
      type (CPCUDataType), intent(inout) :: dailyPrecip

      integer :: rc
      integer :: this = CPCUGetDayErrorHandle

      rc = 0
      CPCUGetDay = -1

      if (currentDay .eq. 0) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      if (day .lt. 1 .or. day .gt. 2) rc = 2
      if (isError(this,2,rc) .ne. 0) return

      dailyPrecip%grid = CPCUData(day)%grid
      dailyPrecip%data => CPCUData(day)%data

      CPCUGetDay = 0

      end function CPCUGetDay

!******************************************************************************
      integer function CPCUGetEOD(eod)
!******************************************************************************

      implicit none 

      type (CPCUDataType), intent(inout) :: eod

      integer :: rc
      integer :: this = CPCUGetEODErrorHandle

      rc = 0
      CPCUGetEOD = -1

      if (currentDay .eq. 0) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      eod%grid =  EODData%grid
      eod%data => EODData%data

      CPCUGetEOD = 0

      end function CPCUGetEOD

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,fName,idate)
!******************************************************************************
! English Name: Is There an Error
! -------------
!
! Purpose: Module error handler and registry.
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
! rc                   integer       IN  Return code associated with the
!                                        error event. A non-zero value
!                                        indicates that the error has
!                                        occurred.
!
! fName                 string   OPT,IN  optional filename argument
!
! idate                integer   OPT,IN  optional date argument (ccyymmdd)
!
! isError              integer      OUT  Function return value:
!
!                                        0: no error occurred (rc=0)
!                                       -1: error occurred
!
! errorCode            integer     PRIV  Recorded error event code to be
!                                        reported by errorHandler().
!
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!            6/14/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: rc
      integer, intent(in) :: errorEvent
      integer, intent(in) :: errorHandle
      integer, intent(in), optional :: idate
      character(len=*), intent(in), optional :: fName

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

        case (CPCUReadFileErrorHandle)

        select case (errorEvent)

          case (2,4)

            print *, 'CPCUReadFile (error #',code,'): error reading file'
            print *, 'CPCUReadFile: filename = ',fName

          case (1,3)

            print *, 'CPCUReadFile (error #',code,'): error allocating memory'

          case default

            print *, 'CPCUReadFile: (error #',code,'): unknown error'

        end select

        case (CPCUReadEODErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CPCUReadEOD (error #',code,'): error reading file'
            print *, 'CPCUReadEOD: filename = ',fName

          case default

            print *, 'CPCUReadEOD: (error #',code,'): unknown error'

        end select

!       CPCUGetDay Error Handle
!       =======================

        case (CPCUGetDayErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CPCUGetDay (error#',code,'): no CPCU data.'

          case (2)

            print *, 'CPCUGetDay (error#',code,'): requested day must be', &
                     ' between 1 and 2'

          case default

            print *, 'CPCUGetDay: (error #',code,'): unknown error'

        end select

!       CPCUGetEOD Error Handle
!       =======================

        case (CPCUGetEODErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CPCUGetEOD (error#',code,'): no CPCU data.'

          case default

            print *, 'CPCUGetEOD: (error #',code,'): unknown error'

        end select

!       CPCUScaleDaily Error Handle
!       ===========================

        case (CPCUScaleDailyErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CPCUScaleDaily (error#',code,'): error scaling CPCU ', &
                     'data. idate = ',idate

          case default

            print *, 'CPCUScaleDaily: (error #',code,'): unknown error'

        end select

!       CPCUScaleDay Error Handle
!       =========================

        case (CPCUScaleDayErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CPCUScaleDay (error#',code,'): error reading CPCU ', &
                     'scaling file. idate = ',idate

          case default

            print *, 'CPCUScaleDay: (error #',code,'): unknown error'

        end select

        case (CPCURead2DaysErrorHandle)

        select case (errorEvent)

          case (1,2)

            print *, 'CPCURead2Days (error#',code,'): error reading CPCU data'
            print *, 'CPCURead2Days: idate = ',idate

          case default

            print *, 'CPCURead2Days: (error#',code,'): unknown error'

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

      errorHandler = errorCode
      errorCode = 0

      return

      end function errorHandler

      end module CPCUFileMod
