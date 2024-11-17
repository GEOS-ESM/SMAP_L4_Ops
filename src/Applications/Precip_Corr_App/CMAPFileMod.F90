      module CMAPFileMod

      use TypesMod
      use GridMod, GridError=>errorHandler
      use RegridMethodMod, RegridError=>errorHandler
      use time

      private
      public :: CMAPfactor
      public :: CMAPReadPentad
      public :: CMAPScalePentad
      public :: CMAPGetPentad
      public :: CMAPReadFile
      public :: errorHandler

      real, parameter :: CMAPfactor = 1.0

      integer, parameter :: CMAPReadPentadErrorHandle = 1
      integer, parameter :: CMAPScalePentadErrorHandle = 2
      integer, parameter :: CMAPReadFileErrorHandle = 3
      integer, parameter :: CMAPGetPentadErrorHandle = 4

      integer, parameter :: CMAP_NLON  = 144
      integer, parameter :: CMAP_NLAT  = 72
      integer, parameter :: CMAP_UNIT  = 34
      real, parameter    :: CMAP_DX    = 2.5
      real, parameter    :: CMAP_DY    = 2.5
      real, parameter    :: CMAP_STLON = 1.25
      real, parameter    :: CMAP_STLAT = -88.75
      real, parameter    :: CMAP_UNDEF = -999.0

      integer :: errorCode = 0
      integer :: last_pentad = 0
      type (CMAPDataType) :: CMAPData
      type (CMAPDataType) :: CMAPscale

      contains

!******************************************************************************
      integer function CMAPReadPentad(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Read CPC Merged Analysis of Precipitation (CMAP) Pentad Data
! -------------
!
! Purpose: Reads in a global CMAP record for the pentad containing the
! -------- specified date/time.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Files are obtained from:
! ------    ftp://ftp.cpc.ncep.noaa.gov/precip/cmap/pentad_rt
!
!           The real-time version is the intended input for this method:
!
!           Example: cmap_pen_rt_v0011_out.lnx.2014
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
! CMAPReadPentad       integer      OUT  function return value:
!
!                                        0: successful return
!                                       -1: error occurred 
!
!                                        Thrown Exceptions:
!
!                                        1: error reading CMAP file
!
! CMAPData        CMAPDataType    INOUT  internal data buffer containing a
!                                        pentad of global precipitation.
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

!     Local Variables
!     ---------------

      integer :: rc
      type (PentadDataType) :: pentad
      integer :: this = CMAPReadPentadErrorHandle

!     Determine the pentad (exit if already read).
!     ============================================

      CMAPReadPentad = 0

      pentad = tm_pentad(idate)
      if (pentad%index .eq. last_pentad) return

!     Read data for the pentad.
!     =========================

      CMAPReadPentad = -1

      print *, 'Reading CMAP data for pentad: ',pentad%index
      rc = CMAPReadFile(fNameTmplt,idate,pentad%index,CMAPdata)
      if (isError(this,1,rc,idate=idate) .ne. 0) return

      last_pentad    = pentad%index
      CMAPReadPentad = 0

      end function CMAPReadPentad

!******************************************************************************
      integer function CMAPScalePentad(fNameTmplt,idate,itime)
!******************************************************************************
! English Name: Scale the CMAP Pentad
! -------------
!
! Purpose: Reads in global CMAP scale factors for the current pentad and
! -------- applies the factors to the CMAP data.
!
! Language: Fortran 90
! ---------
!
! Prerequisites: CMAPReadPentad()
! --------------
!
! Notes: 1. The scale factors are applied to ensure consistency between the
! ------    precipitation corrections based on CMAP and GPCPv2.1 data. See the
!           following document for more information:
!
!           "Technical Report Series on Global Modeling and Data Assimilation,
!            Volume 35 Randal D. Koster, Editor: Observation-Corrected
!            Precipitation Estimates in GEOS-5"
!
! Usage: rc = CMAPScalePentad(fNameTmplt,idate,itime)
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
!                                        1: pentad does not match data
!                                        2: error reading CMAP file
!
! CMAPdata        CMAPDataType    INOUT  On input, internal data buffer
!                                        containing a pentad of global
!                                        precipitation. On output, scaled
!                                        precipitation (see CMAPscale).
!
! CMAPscale       CMAPDataType      OUT  Scale factors read from file for
!                                        pentad corresponding to the specified
!                                        date.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/29/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none
      integer :: this = CMAPScalePentadErrorHandle

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character(len=*), intent(in) :: fNameTmplt

!     Local Variables
!     ---------------

      integer :: rc
      type (PentadDataType) :: pentad

!     Determine the pentad (exit if already read).
!     ============================================

      rc = 0
      CMAPScalePentad = -1

!     Read data for the pentad.
!     =========================

      pentad = tm_pentad(idate)
      if (pentad%index .ne. last_pentad) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      print *, 'Reading CMAP scale factors for pentad: ',pentad%index
      rc = CMAPReadFile(fNameTmplt,idate,pentad%index,CMAPscale,double=.true.)
      if (isError(this,2,rc,idate=idate) .ne. 0) return

!     Apply the scale factors
!     =======================

      where (CMAPscale%data .eq. CMAPscale%grid%undef) &
                 CMAPdata%data = CMAPdata%grid%undef

      where (CMAPdata%data .ne. CMAPdata%grid%undef) &
                 CMAPdata%data = CMAPdata%data * CMAPscale%data

      CMAPScalePentad = 0

      end function CMAPScalePentad

!******************************************************************************
      integer function CMAPReadFile(filename,idate,pentad,precip,double)
!******************************************************************************
! English Name: Read CPC Merged Analysis of Precipitation (CMAP) File
! -------------
!
! Purpose: Reads global precipitation data for the specified pentad from a CMAP
! -------- real-timeprecipitation file. 
!
! Language: Fortran 90
! ---------
!
! See Also: CMAPReadEOD()
! ---------
!
! Notes: 1. CMAP daily precipitation files are flat binary (little endian). 
! ------    There is no information other than the precipitation data.
!           Therefore, users must exercise caution when associating a date/time 
!           with a filename. Use filename templates if the date/time is part of
!           the filename (see "filename" argument).
!
!        2. Real-time CMAP files are retrieved from:
!           ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/RT/
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  name of file containing CMAP daily
!                                        precipitation data. The filename
!                                        may be a template using the GrADS
!                                        time token conventions.
!
! idate                integer       IN  date of data on file as ccyymmdd
!
! itime                integer       IN  time of data on file as hhmmss
!                                        (ignored)
!
! precip     CMAPDataType           OUT  returned CMAP data
!
! CMAPReadFile         integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error allocating memory
!                                        2: error reading file
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
      integer :: this = CMAPReadFileErrorHandle

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: pentad
      logical, optional, intent(in) :: double
      character(len=*), intent(in) :: filename
      type (CMAPDataType), intent(inout) :: precip

!     Local Variables
!     ---------------

      integer :: rc
      integer, save :: heap = 0
      integer :: i,j,nx,ny,iunit,word_size

      character(len=1024) :: fname
      real*4, dimension(CMAP_NLON,CMAP_NLAT) :: buf

      CMAPReadFile = -1

      nx    = CMAP_NLON
      ny    = CMAP_NLAT
      iunit = CMAP_UNIT

      word_size = 4
      if (present(double) .and. double) word_size = 8

      heap = GRIDAllocate(heap)

      call GRIDDeallocate(precip%grid)
      if (isError(this,1,GRIDError()) .ne. 0) return

!     Open the CMAP Daily Precipitation file.
!     =======================================

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

      if (word_size .eq. 8) then

        read(iunit,rec=pentad,iostat=rc) precip%data
        if (isError(this,2,rc,fName=fname) .ne. 0) return

      else

        read(iunit,rec=pentad,iostat=rc) buf
        if (isError(this,2,rc,fName=fname) .ne. 0) return

        precip%data = dble(buf)

      endif

!     Perform sanity check on values.
!     Populate data structure.
!     ===============================

      where (precip%data .le. -999.0) precip%data = CMAP_UNDEF

      where (precip%data .ne. CMAP_UNDEF) &
              precip%data = max(0.0,precip%data)

      precip%grid%dx       = CMAP_DX
      precip%grid%dy       = CMAP_DY
      precip%grid%idate    = idate
      precip%grid%itime    = 0
      precip%grid%undef    = CMAP_UNDEF
      precip%grid%xStart   = CMAP_STLON
      precip%grid%yStart   = CMAP_STLAT

      close (unit=iunit)

      call GRIDCloseHeap()

      CMAPReadFile = 0

      end function CMAPReadFile

!******************************************************************************
      integer function CMAPGetPentad(precip)
!******************************************************************************

      implicit none 

      type (CMAPDataType), intent(inout) :: precip

      integer :: rc
      integer :: this = CMAPGetPentadErrorHandle

      rc = 0
      CMAPGetPentad = -1

      if (last_pentad .eq. 0) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      precip%grid = CMAPData%grid
      precip%data => CMAPData%data

      CMAPGetPentad = 0

      end function CMAPGetPentad

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
!           12/17/2014      J.Ardizzone  created.
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

        case (CMAPReadFileErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CMAPReadFile (error #',code,'): error allocating memory'

          case (2)

            print *, 'CMAPReadFile (error #',code,'): error reading file'
            print *, 'CMAPReadFile: filename = ',trim(fName)

          case default

            print *, 'CMAPReadFile: (error #',code,'): unknown error'

        end select

!       CMAPGetPentad Error Handle
!       ==========================

        case (CMAPGetPentadErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CMAPGetPentad (error#',code,'): no CMAP data.'

          case default

            print *, 'CMAPGetPentad: (error #',code,'): unknown error'

        end select

        case (CMAPReadPentadErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'CMAPReadPentad (error#',code,'): error reading CMAP data'
            print *, 'CMAPReadPentad: idate = ',idate

          case default

            print *, 'CMAPReadPentad: (error#',code,'): unknown error'

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

      end module CMAPFileMod
