      module LFOBaseMod

      use time
      use TypesMod
      use GridMod, GRIDError=>errorHandler

#include "LFO.h"

      use ESMF_CFIOGridMOD
      use ESMF_CFIOVarInfoMOD
      use ESMF_CFIOFileMOD
      use ESMF_CFIOSdfMOD
      use ESMF_CFIOMOD

      private
      public :: LFOReadDay
      public :: LFORead3Days
      public :: LFOReadPentad
      public :: LFOGetDay
      public :: LFOGetData
      public :: LFOReadConfig
      public :: LFOWriteFile
      public :: LFOGetFactor
      public :: LFOScalePrecip
      public :: errorHandler

      integer, parameter :: LFO_SCALE_UNIT = 54
      integer, parameter :: MAX_NUM_DAYS = 144

      integer, parameter :: LFORead3DaysErrorHandle = 1
      integer, parameter :: LFOReadDayErrorHandle   = 2
      integer, parameter :: LFOGetDayErrorHandle    = 3
      integer, parameter :: LFOReadConfigErrorHandle = 4
      integer, parameter :: LFOWriteFileErrorHandle  = 5
      integer, parameter :: LFOReadPentadErrorHandle = 6
      integer, parameter :: LFOScalePrecipErrorHandle = 7
      integer, parameter :: LFOScaleDataErrorHandle = 8
      integer, parameter :: LFOReadScaleFileErrorHandle = 9

      type VarInfoType

        character (len=1024) :: vName
        character (len=1024) :: vTitle
        character (len=1024) :: standardName
        character (len=1024) :: vUnits
        real*4               :: amiss
        real*4               :: scaleFactor
        real*4               :: addOffSet
        real*4, dimension(2) :: validRange

      end type VarInfoType

      type GlobalAttrType

        character (len=1024) :: title
        character (len=1024) :: history
        character (len=1024) :: convention
        character (len=1024) :: institution
        character (len=1024) :: contact
        character (len=1024) :: references
        character (len=1024) :: source
        character (len=1024) :: comment

      end type GlobalAttrType

      type (GlobalAttrType) :: globalAttr
      type (VarInfoType), dimension(3) :: varInfo
      type (LFODataType), dimension(MAX_NUM_DAYS) :: LFOData

      namelist /VarInfoNamelist/ varInfo
      namelist /GlobalAttrNamelist/ globalAttr

      integer :: currentDay = 0
      integer :: last_pentad = 0
      integer :: errorCode = 0

      type (GenericDataType) :: LFOscale

      contains

!******************************************************************************
      integer function LFORead3Days(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Read Three Days
! -------------
!
! Purpose: Reads in three days of land forcing data centered on the specified
! -------- date/time. See "LFODataType" for a full description of the retrieved
!          data fields.
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
! fNameTmplt            string       IN  filename template using GrADS time
!                                        token conventions (see module prolog).
!
! idate                integer       IN  center date of 3-day period as
!                                        ccyymmdd.
!
! itime                integer       IN  starting time of 24hr period as
!                                        hhmmss. For example:
!
!                                        0: start 24hr period at 00Z
!                                        60000: start 24hr period at 06Z
!                                        63000: start 24hr period at 06:30Z
!
!                                        Note: for starting times other than
!                                        00Z, the end of the 24hr period will
!                                        extend into the next day ending at
!                                        itime - 1 hour.
!
! LFORead3Days         integer      OUT  function return value:
!
!                                        0: successful return
!                                       -1: error occurred 
!
!                                        Thrown Error Exceptions:
!
!                                        1: error reading day of data.
!
! LFOData(72)      LFODataType    INOUT  internal data buffer containing the
!                                        3-day period of hourly retrieved land
!                                        forcing data (see module prolog).
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!            6/01/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt

!     Local Variables
!     ---------------

      integer :: rc
      integer :: prevDate,prevTime
      integer :: currDate,currTime
      integer :: nextDate,nextTime

      integer :: this = LFORead3DaysErrorHandle

      LFORead3Days = -1

      if (currentDay .ne. 0) then

!       Since a three day period has already been
!       loaded, simply shift the data buffer and add
!       the next day to arrive at a three day period
!       centered on the current date. 
!       ============================================

        LFOdata = cshift(LFOdata,24)
        nextDate = idate; nextTime = itime;
        call tm_inctime(nextDate,nextTime,24,0,0)
        rc = LFOReadDay(fNameTmplt,nextDate,nextTime,LFOData(49:72))
        if (isError(this,1,rc,idate=nextDate) .ne. 0) return

      else

!       Load three days of data centered
!       on the specified date.
!       ================================

        print *, 'Reading in 3 days of data'

        prevDate = idate; prevTime = itime;
        call tm_inctime(prevDate,prevTime,-24,0,0)
        rc = LFOReadDay(fNameTmplt,prevDate,prevTime,LFOdata(1:24))
        if (isError(this,1,rc,idate=prevDate) .ne. 0) return

        currDate = idate; currTime = itime;
        rc = LFOReadDay(fNameTmplt,currDate,currTime,LFOdata(25:48))
        if (isError(this,1,rc,idate=currDate) .ne. 0) return

        nextDate = idate; nextTime = itime;
        call tm_inctime(nextDate,nextTime,24,0,0)
        rc = LFOReadDay(fNameTmplt,nextDate,nextTime,LFOdata(49:72))
        if (isError(this,1,rc,idate=nextDate) .ne. 0) return

      endif

      currentDay   = idate
      LFORead3Days = 0

      end function LFORead3Days

!******************************************************************************
      integer function LFOReadPentad(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Read Land Forcing for Pentad
! -------------
!
! Purpose: Reads in a pentad of land forcing data containing the specified day.
! -------- See "LFODataType" for a full description of the retrieved data
!          fields.
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
! fNameTmplt            string       IN  filename template using Unix date
!                                        token conventions (see module prolog).
!
! idate                integer       IN  date as ccyymmdd
!
! itime                integer       IN  starting time of 24hr period as
!                                        hhmmss. For example:
!
!                                        0: start 24hr period at 00Z
!                                        60000: start 24hr period at 06Z
!                                        63000: start 24hr period at 06:30Z
!
!                                        Note: for starting times other than
!                                        00Z, the end of the 24hr period will
!                                        extend into the next day ending at
!                                        itime - 1 hour.
!
! LFOReadPentad        integer      OUT  function return value:
!
!                                        0: successful return
!                                       -1: error occurred 
!
!                                        Thrown Error Exceptions:
!
!                                        1: error reading day of data.
!
! LFOData(:)       LFODataType    INOUT  internal data buffer containing the
!                                        period of hourly retrieved land
!                                        forcing data (see module prolog).
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/17/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt

      integer :: rc
      integer :: shour, ehour
      integer :: ndate,ntime,edate,etime
      type (PentadDataType) :: pentad

      integer :: this = LFOReadPentadErrorHandle

!     Determine the pentad (exit if already read).
!     ============================================

      pentad = tm_pentad(idate)

      LFOReadPentad = 0
      if (pentad%index .eq. last_pentad) return

!     Read in data for the pentad
!     ===========================

      LFOReadPentad = -1

      shour = 1
      ndate = pentad%sdate
      ntime = itime
      edate = pentad%edate
      etime = itime

      do while (tm_compare(ndate,ntime,edate,etime) .le. 0)

        ehour = shour + 23
        rc = LFOReadDay(fNameTmplt,ndate,ntime,LFOData(shour:ehour))
        if (isError(this,1,rc,idate=ndate) .ne. 0) return

        shour = shour + 24
        call tm_inctime(ndate,ntime,24,0,0)

      end do

      last_pentad = pentad%index
      LFOReadPentad = 0

      end function LFOReadPentad

!******************************************************************************
      integer function LFOReadDay(fNameTmplt,idate,itime,dayOfData)
!******************************************************************************
! English Name: Read Day
! -------------
!
! Purpose: Reads in 24 hours of land forcing data for the specified date/time.
! --------
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
! fNameTmplt            string           filename template using GrADS time
!                                        token conventions (see module prolog).
!
! idate                integer       IN  date of day to be read: ccyymmdd
!
! itime                integer       IN  starting time of 24hr period as
!                                        hhmmss. For example:
!
!                                        0: start 24hr period at 00Z
!                                        60000: start 24hr period at 06Z
!                                        63000: start 24hr period at 06:30Z
!
!                                        Note: for starting times other than
!                                        00Z, the end of the 24hr period will
!                                        extend into the next day ending at
!                                        itime - 1 hour.
!
! dayOfData(24)    LFODataType      OUT  one day of retrieved land forcing
!                                        data (see LFOdata in module prolog).
!
! LFOReadDay           integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
!
!                                        Thrown Error Exceptions:
!
!                                        1: Error reading file.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Software Integration and Visualization Office - SIVO)
!
! Modified:       Date           Author  Description
! ---------   
!            6/01/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt
      type (LFODataType), dimension(24), intent(inout) :: dayOfData

!     Local Variables
!     ---------------

      integer :: i,rc
      integer :: currDate,currTime
      character(len=1024) :: filename
      integer :: this = LFOReadDayErrorHandle

      LFOReadDay = -1

      currdate = idate
      currTime = itime
      
      do i = 1,24

        call tm_string(currDate,currTime,fNameTmplt,filename)
        rc = LFOReadFile(filename,currDate,currTime,dayOfData(i))
        if (isError(this,1,rc,fName=filename) .ne. 0) return

        call tm_inctime(currDate,currTime,1,0,0)

      end do

      LFOReadDay = 0

      end function LFOReadDay

!******************************************************************************
      integer function LFOGetDay(day,dayOfData)
!******************************************************************************

!     Argument List
!     -------------

      integer, intent(in) :: day
      type (LFODataType), dimension(24), intent(inout) :: dayOfData

!     Local Variables
!     ---------------

      integer :: rc
      integer :: istart,iend,i,j
      integer :: this = LFOGetDayErrorHandle

      rc = 0
      LFOGetDay = -1

      if (currentDay .eq. 0 .and. last_pentad .eq. 0) rc = 1
      if (isError(this,1,rc) .ne. 0) return

!     if (day .lt. 1 .or. day .gt. 3) rc = 2
!     if (isError(this,2,rc) .ne. 0) return

      istart = (day - 1) * 24 + 1
      iend   = istart + 23

      dayOfData%grid    =  LFOData(istart:iend)%grid

      do i = 1,24
        j = istart + i - 1
        dayOfData(i) = LFOData(j)
      end do

      LFOGetDay = 0

      end function LFOGetDay

!******************************************************************************
      integer function LFOGetData(data)
!******************************************************************************

      type (LFODataType), dimension(:), intent(out) :: data

      integer :: ndim

      ndim = min(size(data),MAX_NUM_DAYS)

      data(1:ndim) = LFOdata(1:ndim)

      LFOGetData = 0

      end function LFOGetData

!******************************************************************************
      integer function LFOScalePrecip(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Scale the LFO Precipitation Data
! -------------
!
! Purpose: Reads in global GPCP2.2/MERRA-II scale factors and applies the
! -------- scaling factors to the LFO Precip data.
!
! Language: Fortran 90
! ---------
!
! Prerequisites: LFOReadDay()
! --------------
!
! Notes: 1. The scale factors are applied to ensure that the LFO precip
! ------    everywhere (including Africa and high Latitudes) is consistent with
!           the GPCP2.2 climatology.
!
!           See following document for more information:
!
!           "Technical Report Series on Global Modeling and Data Assimilation,
!            Volume 35 Randal D. Koster, Editor: Observation-Corrected
!            Precipitation Estimates in GEOS-5"
!
! Usage: rc = LFOScalePrecip(fNameTmplt,idate,itime)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! fNameTmplt            string       IN  filename template using Unix date
!                                        command conventions (see module prolog)
!
! idate                integer       IN  specified (current) date as ccyymmdd.
!                                        Currently not used.
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
! LFOdata         LFODataType     INOUT  On input, internal data buffer
!                                        containing global LFO
!                                        precipitation. On output, scaled
!                                        precipitation (see LFOscale).
!
! LFOscale        LFODataType       OUT  Scale factors read from file for
!                                        pentad corresponding to the buffered
!                                        LFO days (most recent pentad only).
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/07/2016      J.Ardizzone  created.
!******************************************************************************

      implicit none
      integer :: this = LFOScalePrecipErrorHandle

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt

!     Local Variables
!     ---------------

      integer :: rc
      integer :: i,ndate

      LFOScalePrecip = 0
      if (len_trim(fNameTmplt) .le. 0) return

      rc = 0
      LFOScalePrecip = -1

!     Scale the LFO precipitation data
!     ================================
     
      do i = 1,MAX_NUM_DAYS

        if (LFOdata(i)%grid%id .le. 0) exit
        if (LFOdata(i)%grid%scaled) cycle

        ndate = LFOdata(i)%grid%idate
        rc = LFOScaleData(fNameTmplt,ndate,LFOdata(i))
        if (isError(this,1,rc,idate=ndate) .ne. 0) return

      end do

      LFOScalePrecip = 0

      return

      end function LFOScalePrecip

!******************************************************************************
      integer function LFOScaleData(fNameTmplt,idate,buf)
!******************************************************************************
! English Name: Scale Data
! -------------
!
! Purpose: Reads in scale factors and applies the scaling factors to a
! -------- single instance of GEOS LFO precipitation data.
!
! Language: Fortran 90
! ---------
!
! Prerequisites: LFOScalePrecip()
! --------------
!
! Notes:
! ------
!
! Usage: rc = LFOScaleData(fNameTmplt,idate,buf)
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
! buf              LFODataType    INOUT  on input, unscaled LFO precipitation
!                                        data. On output, scaled precipitation.
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
! LFOscale     GenericDataType      OUT  Scale factors read from file for
!                                        pentad corresponding to the specified
!                                        date (see idate).
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/08/2016      J.Ardizzone  created.
!******************************************************************************

      implicit none
      integer :: this = LFOScaleDataErrorHandle

      integer, intent(in) :: idate
      character(len=*), intent(in) :: fNameTmplt
      type (LFODataType), intent(inout) :: buf

!     Local Variables
!     ---------------

      integer :: rc
      type (PentadDataType) :: pentad

      LFOScaleData = 0
      if (buf%grid%scaled) return

      LFOScaleData = -1

!     Refresh scaling factors if a new pentad is needed.
!     ==================================================

      pentad = tm_pentad(idate)

      if (pentad%index .ne. last_pentad) then 

        print *, 'Reading LFO scale factors for pentad: ', pentad%index

        rc = LFOReadScaleFile(fNameTmplt,pentad,buf%grid,LFOscale)
        if (isError(this,1,rc,idate=idate) .ne. 0) return

        last_pentad = pentad%index

      endif

!     Apply the scale factors
!     =======================

      where (LFOscale%data .ne. LFOscale%grid%undef)
        buf%prectot = buf%prectot * LFOscale%data 
        buf%precls  = buf%precls  * LFOscale%data
        buf%precsno = buf%precsno * LFOscale%data
        buf%preccon = buf%preccon * LFOscale%data
      end where

      buf%grid%scaled = .true.

      LFOScaleData = 0

      end function LFOScaleData
!******************************************************************************
      integer function LFOReadScaleFile(filename,pentad,grid,factor)
!******************************************************************************
! English Name: Read Scale File
! -------------
!
! Purpose: Reads precipitation GPCP2.2 / MERRA-II scaling factors from a
! -------- file.
!
! Language: Fortran 90
! ---------
!
! See Also: LFOScalePrecip(), LFOScaleData()
! ---------
!
! Notes: 1. 
! ------   
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  name of file containing scaling
!                                        factors (see note-1).
!
! pentad        PentadDataType       IN  desired pentad (scale file has one
!                                        record for each pentad).
!
! grid            GridInfoType       IN  grid information (used to determine
!                                        the grid dimensions of the input
!                                        scaling file).
!
! factor       GenericDataType      OUT  returned scale factors.
!
! LFOReadScaleFile     integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
!
!                                        Error handler event codes:
!
!                                        1: error opening or reading file
!                                        2: error in memory management
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/08/2016      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GridInfoType), intent(in) :: grid
      character(len=*), intent(in) :: filename
      type (PentadDataType), intent(in) :: pentad
      type (GenericDataType), intent(inout) :: factor

!     Local Variables
!     ---------------

      integer, save :: heap = 0
      integer :: rc,reclen,iunit
      character(len=1024) :: fname
      integer :: this = LFOReadScaleFileErrorHandle

      LFOReadScaleFile = -1

      iunit  = LFO_SCALE_UNIT
      reclen = grid%nx * grid%ny * 8

      heap = GRIDAllocate(heap)

      call GRIDDeallocate(factor%grid)
      if (isError(this,1,GRIDError()) .ne. 0) return

!     Open the LFO scaling file.
!     ==========================

      call tm_string(pentad%date,0,filename,fname)
      open(unit=iunit,file=fname,form='unformatted', &
           access='direct',recl=reclen,status='old',iostat=rc)
      if (isError(this,2,rc,fName=fname) .ne. 0) return

!     Allocate space and read file
!     ============================

      factor%grid%nx     = grid%nx
      factor%grid%ny     = grid%ny
      factor%grid%dx     = grid%dx
      factor%grid%dy     = grid%dy
      factor%grid%xStart = grid%xStart
      factor%grid%yStart = grid%yStart
      factor%grid%idate  = pentad%sdate
      factor%grid%itime  = 0

      rc = GRIDAllocate(factor)
      if (isError(this,1,GRIDError()) .ne. 0) return

      read(iunit,rec=pentad%index,iostat=rc) factor%data
      if (isError(this,2,rc,fName=fname) .ne. 0) return

      close (unit=iunit)

      call GRIDCloseHeap()

      LFOReadScaleFile = 0

      end function LFOReadScaleFile

!******************************************************************************
      integer function LFOReadConfig(filename)
!******************************************************************************
! English Name: Read Configuration
! -------------
!
! Purpose: Reads namelist file containing information for describing the
! -------- LFO output file and content.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. See also: VarInfoNamelist, GlobalAttrNamelist
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  name of file containing the fortran 90
!                                        namelist information.
!
! VarInfo(:)       VarInfoType      OUT  variable information as follows:
!
!                                        (1): total precipitation
!                                        (2): total snowfall
!                                        (3): total convective
!
! GlobalAttr    GlobalAttrType      OUT  global file attributes
!
! LFOReadConfig        integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error opening namelist file
!                                        2: error reading VarInfoNamelist
!                                        3: error reading GlobalAttrNamelist
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/11/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      character (len=*), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = LFOReadConfigErrorHandle

!     Read in namelists
!     =================

      LFOReadConfig = -1

      open (unit=5,file=filename,status='old',iostat=rc)
      if (isError(this,1,rc) .ne. 0) return

      read(5,nml=VarInfoNamelist,iostat=rc)
      if (isError(this,2,rc) .ne. 0) return

      read(5,nml=GlobalAttrNamelist,iostat=rc)
      if (isError(this,3,rc) .ne. 0) return

      close(unit=5)

      LFOReadConfig = 0

      end function LFOReadConfig

!******************************************************************************
      integer function LFOWriteFile(fNameTmplt,LFOout)
!******************************************************************************
! English Name: Write File
! -------------
!
! Purpose: Writes LFO data (corrected precipitation) to an output data set.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. LFOReadConfig() must be called before invoking this routine.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! fNameTmplt            string       IN  filename template containing time
!                                        tokens to be resolved as a function
!                                        of the date/time.
!
! LFOout           LFODataType       IN  output land forcing data to be
!                                        written to output. 
!
! VarInfo(:)       VarInfoType       IN  variable information
!                                        (see LFOReadConfig)
!
! GlobalAttr    GlobalAttrType       IN  global file attributes
!                                        (see LFOReadConfig)
!
! LFOWriteFile         integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error allocating memory
!                                        2: CFIO grid error
!                                        3: CFIO variable error
!                                        4: CFIO initialize error
!                                        5: CFIO file create error
!                                        6: CFIO write error
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/11/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      character (len=*), intent(in) :: fNameTmplt
      type (LFODataType), dimension(:), intent(in) :: LFOout

!     Local Variables
!     ---------------

      integer :: ilen
      integer :: nx,ny
      integer :: i,j,iVar,rc
      integer :: idate,itime
      real :: dx,dy,xStart,yStart
      character (len=1024) :: str
      character (len=1024) :: filename
      character (len=1024) :: outputFormat
      real*4, dimension(:), pointer, save :: lat => null()
      real*4, dimension(:), pointer, save :: lon => null()
      real*4, dimension(:,:), pointer, save :: buf => null()

      type (ESMF_CFIO) :: cfio
      type (ESMF_CFIOGrid) :: grid
      type (ESMF_CFIOVarInfo), dimension(3) :: vars

      integer :: this = LFOWriteFileErrorHandle

!     Initialize
!     ==========

      LFOWriteFile = -1

      nx = LFOout(1)%grid%nx
      ny = LFOout(1)%grid%ny
      dx = LFOout(1)%grid%dx
      dy = LFOout(1)%grid%dy

      xStart = LFOout(1)%grid%xStart
      yStart = LFOout(1)%grid%yStart

      if (associated(lat)) deallocate(lat)
      if (associated(lon)) deallocate(lon)
      if (associated(buf)) deallocate(buf)

      allocate(lon(nx),stat=rc)
      if (isError(this,1,rc) .ne. 0) return
      allocate(lat(ny),stat=rc)
      if (isError(this,1,rc) .ne. 0) return
      allocate(buf(nx,ny),stat=rc)
      if (isError(this,1,rc) .ne. 0) return

      lon = (/(i,i=0,nx-1)/) * dx + xStart
      lat = (/(j,j=0,ny-1)/) * dy + yStart

!     Write out data for each time segment
!     ====================================

      do i = 1,size(LFOout)

!       Resolve filename based on date/time
!       -----------------------------------

        idate = LFOout(i)%grid%idate
        itime = LFOout(i)%grid%itime
        call tm_string(idate,itime,fNameTmplt,filename)

!       Set grid attributes
!       -------------------

        grid = ESMF_CFIOGridCreate(gName='pcorrect',rc=rc)
        if (isError(this,2,rc) .ne. 0) return

        call ESMF_CFIOGridSet(grid, &
                    im=nx, &
                    jm=ny, &
                    lon=lon, &
                    lat=lat, &
                    coordinate='pressure', &
                    standardName='atmosphere_pressure_coordinate', &
                    rc=rc)

        if (isError(this,2,rc) .ne. 0) return

!       Set variable attributes
!       -----------------------

        do iVar = 1,3

          vars(iVar) = ESMF_CFIOVarInfoCreate(vName=varInfo(iVar)%vName,rc=rc)
          if (isError(this,3,rc) .ne. 0) return

          call ESMF_CFIOVarInfoSet(vars(iVar), &
                      grid=grid, &
                      vName=varInfo(iVar)%vName, &
                      vTitle=varInfo(iVar)%vTitle, &
                      standardName=varInfo(iVar)%standardName, &
                      vUnits=varInfo(iVar)%vUnits, &
                      amiss=varInfo(iVar)%amiss, &
                      scaleFactor=varInfo(iVar)%scaleFactor, &
                      addOffSet=varInfo(iVar)%addOffset, &
                      validRange=varInfo(iVar)%validRange, &
                      twoDimVar=.true., &
                      rc=rc)
          if (isError(this,3,rc) .ne. 0) return

        end do

!       Register all information with main I/O
!       data structure.
!       --------------------------------------
        
        cfio =  ESMF_CFIOCreate(cfioObjName='lfo', rc=rc)
        if (isError(this,4,rc) .ne. 0) return

        call ESMF_CFIOSet(cfio, &
                    fName=filename, &
                    title=globalAttr%title, &
                    history=globalAttr%history, &
                    convention=globalAttr%convention, &
                    institution=globalAttr%institution, &
                    contact=globalAttr%contact, &
                    references=globalAttr%references, &
                    source=globalAttr%source, &
                    comment=globalAttr%comment, &
                    date=idate, &
                    BegTime=itime, &
                    timeInc=10000, &
                    grid=grid, &
                    varObjs=vars, &
                    rc=rc)

        if (isError(this,4,rc) .ne. 0) return

!       Write data to the output file
!       -----------------------------

        outputFormat='SDF'
        call getenv('LFOFILEFORMAT',str)
        if (len_trim(str) .gt. 0) outputFormat=str

        print *, trim(outputFormat),':',trim(cfio%fName)
        call ESMF_CFIOFileCreate(cfio, format = trim(outputFormat),rc=rc)
        if (isError(this,5,rc) .ne. 0) return

        buf = max(0.0,LFOout(i)%precls) 
        call ESMF_CFIOVarWrite(cfio, &
              trim(varInfo(1)%vName),buf, &
              date=idate, &
              curTime=itime, &
              rc=rc)

        if (isError(this,6,rc) .ne. 0) return

        buf = max(0.0,LFOout(i)%precsno)
        call ESMF_CFIOVarWrite(cfio, &
              trim(varInfo(2)%vName),buf, &
              date=idate, &
              curTime=itime, &
              rc=rc)

        if (isError(this,6,rc) .ne. 0) return

        buf = max(0.0,LFOout(i)%preccon)
        call ESMF_CFIOVarWrite(cfio, &
              trim(varInfo(3)%vName),buf, &
              date=idate, &
              curTime=itime, &
              rc=rc)

        if (isError(this,6,rc) .ne. 0) return

!       Close file
!       ----------

        call ESMF_CFIOFileClose(cfio)

      end do

!     Clean up and exit
!     =================

      if (associated(lat)) deallocate(lat)
      if (associated(lon)) deallocate(lon)
      if (associated(buf)) deallocate(buf)

      LFOWriteFile = 0

      end function LFOWriteFile

!******************************************************************************
      real function LFOGetFactor()
!******************************************************************************

      LFOGetFactor = LFOfactor

      end function LFOGetFactor

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,idate,itime,fName)
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
! idate                integer   OPT,IN  date as ccyymmcc
!
! itime                integer   OPT,IN  time as hhmmss
!
! fName                 string   OPT,IN  name of file
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

      integer, intent(in) :: errorHandle
      integer, intent(in) :: errorEvent
      integer, intent(in) :: rc

      integer, intent(in), optional :: idate
      integer, intent(in), optional :: itime
      character(len=*), intent(in), optional :: fName

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

!       LFOReadDay Error Handle
!       =======================

        case (LFOReadDayErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOReadDay (error#',code,'): Error reading file'
            print *, 'LFOReadDay: filename = ',fName

          case default

            print *, 'LFOReadDay: (error#',code,'): unknown error'

        end select

!       LFORead3Days Error Handle
!       =========================

        case (LFORead3DaysErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFORead3Days (error#',code,'): Error reading day of data'
            print *, 'LFORead3Days: idate = ',idate

          case default

            print *, 'LFORead3Days: (error#',code,'): unknown error'

        end select

!       LFOGetDay Error Handle
!       ======================

        case (LFOGetDayErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOGetDay (error#',code,'): no LFO data.'

          case (2)

            print *, 'LFOGetDay (error#',code,'): requested day must be', \
                     ' between 1 and 3'

          case default

            print *, 'LFOGetDay: (error#',code,'): unknown error'

        end select

!       LFOReadConfig Error Handle
!       ==========================

        case (LFOReadConfigErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOReadConfig (error#',code,'): Error opening namelist'

          case (2)

            print *, 'LFOReadConfig (error#',code,'): ', &
                     'Error reading namelist, VarInfoNamelist'

          case (3)

            print *, 'LFOReadConfig (error#',code,'): ', &
                     'Error reading namelist, GlobalAttrNamelist'

          case default

            print *, 'LFOReadConfig (error#',code,'): unknown error'

        end select

!       LFOScalePrecip Error Handle
!       ===========================

        case (LFOScalePrecipErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOScalePrecip (error#',code,'): Error scaling LFO ', &
                     'precipitation: date = ',idate

          case default

            print *, 'LFOScalePrecip (error#',code,'): unknown error'

        end select

!       LFOScaleData Error Handle
!       =========================

        case (LFOScaleDataErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOScaleData (error#',code,'): error occurred reading ', &
                     'scale file: date = ',idate

          case default

            print *, 'LFOScaleData (error#',code,'): unknown error'

        end select

!       LFOReadScaleFile Error Handle
!       =============================

        case (LFOReadScaleFileErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOReadScaleFile (error#',code,'): error in memory ', &
                     'allocation/deallocation'

          case (2)

            print *, 'LFOReadScaleFile (error#',code,'): error accessing or ', &
                     'reading scale file: "',trim(fName),'"'

          case default

            print *, 'LFOReadScaleFile (error#',code,'): unknown error'

        end select

!       LFOWriteFile Error Handle
!       ==========================

        case (LFOWriteFileErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LFOWriteFile (error#',code,'): error allocating memory'

          case (2)

            print *, 'LFOWriteFile (error#',code,'): CFIO grid error'

          case (3)

            print *, 'LFOWriteFile (error#',code,'): CFIO variable error'

          case (4)

            print *, 'LFOWriteFile (error#',code,'): CFIO set error'

          case (5)

            print *, 'LFOWriteFile (error#',code,'): CFIO file create error'

          case (6)

            print *, 'LFOWriteFile (error#',code,'): CFIO write error'

          case default

            print *, 'LFOWriteFile (error#',code,'): unknown error'

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

      end module LFOBaseMod
