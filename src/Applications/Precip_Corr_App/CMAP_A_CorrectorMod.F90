!******************************************************************************
      module CMAP_A_CorrectorMod
!******************************************************************************
! English Name: CMAP-A Corrector Module
! -------------
!
! Purpose: Provides methods for executing the precipitation correction
! -------- algorithm using method CMAP Algorithm A. See the following document
!          for more information:
!
!          "Technical Report Series on Global Modeling and Data Assimilation,
!           Volume 35 Randal D. Koster, Editor: Observation-Corrected
!           Precipitation Estimates in GEOS-5"
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
! correctPrecip()      integer      PUB  Implements the API for correcting
!                                        precipitation.
!
! getArgs()            integer      PUB  Retrieves command-line arguments and
!                                        configuration parameters.
!
! UserInterfaceType    derived      PUB  Derived type used to store parameters
!                                        retrieved by getArgs().
!
! errorHandler()       integer      PUB  Error handler for this module. Invoke
!                                        the error handler to check for an
!                                        exception thrown by a method of this
!                                        module. The handler is usually mapped
!                                        to a unique name to distinguish it
!                                        from other handlers. For example:
!
!                                        use CMAP_A_CorrectorMod, &
!                                                  CorrectorError=>errorHandler
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/04/2015      J.Ardizzone  created.
!******************************************************************************

      use time
      use TypesMod
      use RegridMethodMod, RegridMethodError=>errorHandler
      use GridMod, GridError=>errorHandler
      use LFOBaseMod, LFOBaseError=>errorHandler
      use CMAPFileMod, CMAPFileError=>errorHandler
      use RegridMod, RegridError=>errorHandler

!     Module Accessibility
!     ====================

      private
      public :: getArgs
      public :: correctPrecip
      public :: UserInterfaceType
      public :: errorHandler

!     Constants
!     =========

      real, parameter :: zeroThreshold = 1.0e-4

!     Error Handle Definitions
!     ========================

      integer, parameter :: CorrectPrecipErrorHandle = 1
      integer, parameter :: GetArgsErrorHandle = 2
      integer, parameter :: ComputeLFOAvgErrorHandle = 3
      integer, parameter :: InterpGenericErrorHandle = 4
      integer, parameter :: InterpCPCUCorrErrorHandle = 5
      integer, parameter :: ComputeCorrectionErrorHandle = 6
      integer, parameter :: ApplyCorrectionErrorHandle = 7

!     Derived Type: User Interface
!     ============================

      type UserInterfaceType

        integer :: start_date,end_date
        real :: exchange_dlon, exchange_dlat

        character(len=1024) :: config
        character(len=1024) :: in_file
        character(len=1024) :: cmap_file
        character(len=1024) :: cmap_scale_file
        character(len=1024) :: out_file

      end type UserInterfaceType

!     Polymorphic definitions for interpolating
!     data based on derived data type
!     =========================================

      interface interpolate

        module procedure interpGeneric
        module procedure interpCPCUCorr

      end interface interpolate

!     Polymorphic definitions for computing
!     the average based on derived data type
!     ======================================

      interface computeAverage

        module procedure computeLFOAvg

      end interface computeAverage

!     Global variables
!     ================

      integer :: errorCode = 0
      integer :: save_pentad = 0
      type (GridInfoType) :: LFOgrid
      type (GridInfoType) :: CMAPgrid

      contains

!******************************************************************************
      integer function correctPrecip(idate)
!******************************************************************************
! English Name: Correct Precipitation
! -------------
!
! Purpose: Implements the precipitation corrector API using CMAP data. This
! -------- method implements algorithm "A": correction factors calculated on
!          the observation grid and then interpolated to the model grid.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This corrector module assumes that the input land forcing data
! ------    to be corrected consists of hourly precipitation data with an
!           assigned time centered on the hour: (eg 00:30,1:30,2:30,...). It
!           also assumes that the day to be corrected starts at 00:30Z of the 
!           date and ends at 23:30Z of the same day.
!
!        2. CMAP Corrector "A" uses aggregated pentads. Land forcing (LFO) data
!           from GEOS-5 must be available for the entire pentad containing the
!           day to be corrected.
!
!        3. This corrector will retain corrected LFO data for each day of the
!           pentad containing the specified day. It is more efficient to 
!           invoke this corrector over 5+ days without exiting.
!
! Usage: rc = correctPrecip(idate)
! ------ exception = errorHandle()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer       IN  date denoting the day of precipitation
!                                        to be corrected: ccyymmdd
!
! rc                   integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception issued
!
! exception            integer      OUT  Thrown Exception:
!
!                                        1: error retrieving command-line args
!                                        2: error reading CMAP data
!                                        3: error reading CMAP scaling data
!                                        4: error reading land forcing data
!                                        5: error fetching CMAP data
!                                        6: error fetching LFO data
!                                        7: error allocating grid space
!                                        8: error computing LFO mean precip
!                                        9: interpolation error
!                                       10: error computing correction factor
!                                       11: error applying correction factor
!                                       12: error writing corrected LFO
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/28/2014      J.Ardizzone  created.
!           01/04/2015      J.Ardizzone  updated error messaging.
!******************************************************************************
      implicit none
      integer :: this = CorrectPrecipErrorHandle

!     Argument List
!     -------------

      integer, intent(in) :: idate

!     Local Variables
!     ---------------

      integer :: rc
      integer, save :: heap = 0
      integer :: day,num_hours

      type (CMAPDataType) :: cmap
      type (PentadDataType) :: pentad
      type (UserInterfaceType) :: args
      type (GenericDataType) :: avg_cg, avg_mg
      type (LFODataType), dimension(144) :: lfo
      type (CPCUCorrDataType) :: corr_cg, corr_mg

!     Initialize
!     ==========

      correctPrecip = -1

      rc = getArgs(args)
      if (isError(this,1,rc) .ne. 0) return

      rc = setExchangeParams(args%exchange_dlon,args%exchange_dlat)

      heap = GRIDAllocate(heap)

!     Retrieve date/time information
!     for the pentad containing the
!     specified date.
!     ==============================

      pentad    = tm_pentad(idate)
      day       = pentad%offset + 1
      num_hours = pentad%num_days * 24

!     Write out precipitation data if the
!     requested date is within the prior 
!     pentad correction period. 
!     ====================================

      if (pentad%index .eq. save_pentad) then

        print *, 'Writing corrected precipitation data for ',idate

        rc = LFOGetDay(day,lfo)
        if (isError(this,6,rc) .ne. 0) return

        rc = LFOWriteFile(args%out_file,lfo(1:24))
        if (isError(this,12,rc) .ne. 0) return

        correctPrecip = 0
        return

      endif

      save_pentad = pentad%index

!     Read in CMAP data
!     =================

      print *, 'Reading in CMAP data for pentad: ' ,idate

      rc = CMAPReadPentad(args%cmap_file,idate,0)
      if (isError(this,2,rc) .ne. 0) return

      rc = CMAPScalePentad(args%cmap_scale_file,idate,0)
      if (isError(this,3,rc) .ne. 0) return

!     Read in land forcing data for the
!     pentad containing the specified date
!     ====================================

      print *, 'Reading in LFO data for pentad: ', idate

      rc = LFOReadPentad(args%in_file,idate,3000)
      if (isError(this,4,rc) .ne. 0) return

!     Retrieve CMAP data
!     ==================

      rc = CMAPGetPentad(cmap)
      if (isError(this,5,rc) .ne. 0) return

      CMAPgrid = cmap%grid

!     Retrieve Land Forcing Data
!     ==========================

      rc = LFOGetData(lfo(1:num_hours))
      if (isError(this,6,rc) .ne. 0) return

      LFOgrid = lfo(1)%grid

!     Compute the average total precipitation
!     for the pentad.
!     =======================================

      print *, 'Computing the mean total precipitation: ', idate

      avg_mg%grid = LFOgrid
      rc = GRIDAllocate(avg_mg)
      if (isError(this,7,rc) .ne. 0) return
 
      rc = computeAverage(lfo(1:num_hours),avg_mg)
      if (isError(this,8,rc) .ne. 0) return

!     Interpolate the LFO (model) average
!     precipitation to the CMAP grid.
!     ===================================

      print *, 'Interpolating the mean to the cmap grid: ', idate

      avg_cg%grid = CMAPgrid
      rc = GRIDAllocate(avg_cg)
      if (isError(this,7,rc) .ne. 0) return

      rc = interpolate(avg_mg,avg_cg,rotate=180.0)
      if (isError(this,9,rc) .ne. 0) return

!     Compute the correction factors on the
!     CMAP grid.
!     =====================================

      print *, 'Computing the correction factors: ', idate
      
      corr_cg%grid = CMAPgrid
      rc = GRIDAllocate(corr_cg)
      if (isError(this,7,rc) .ne. 0) return

      rc = computeCorrection(cmap,avg_cg,corr_cg)
      if (isError(this,10,rc) .ne. 0) return

!     Interpolate the correction factor back
!     to the LFO (model) grid.
!     ======================================

      print *, 'Interpolating the correction factors to the model grid: ', idate

      corr_mg%grid = LFOgrid
      rc = GRIDAllocate(corr_mg)
      if (isError(this,7,rc) .ne. 0) return

      rc = interpolate(corr_cg,corr_mg,rotate=-180.0)
      if (isError(this,9,rc) .ne. 0) return

      where (corr_mg%factor .eq. corr_mg%grid%undef) corr_mg%factor = 1.0

!     Apply the correction to the model
!     land forcing data
!     =================================

      print *, 'Applying the correction: ', idate

      rc = applyCorrection(corr_mg,lfo(1:num_hours))
      if (isError(this,11,rc) .ne. 0) return

!     Write out corrected LFO precipitation
!     for the specified date.
!     =====================================

      print *, 'Writing corrected precipitation data for ',idate

      rc = LFOGetDay(day,lfo)
      if (isError(this,6,rc) .ne. 0) return

      rc = LFOWriteFile(args%out_file,lfo(1:24))
      if (isError(this,12,rc) .ne. 0) return

!     Free Memory allocated under the heap ID
!     associated with this method.
!     =======================================

      call GRIDDeallocate(heap)

      correctPrecip = 0

      end function correctPrecip
!******************************************************************************
      integer function interpGeneric(in_data,out_data,rotate)
!******************************************************************************
! English Name: Interpolate
! -------------
!
! Purpose: Interpolates data from the input grid to the output grid for the
! -------- derived data type: GenericDataType.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This method operates on regular lat/lon grids. 
! ------
!        2. This method can be invoked using its compile-time polymorphic
!           interface name, interpolate().
!
! Usage: rc = interpolate(in_grid,out_grid,rotate=degrees)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! in_data      GenericDataType       IN  Data on input grid.
!
! out_data     GenericDataType    INOUT  Data on output grid.
!
! degrees                 real   OPT,IN  Degrees of rotation needed to align
!                                        input grid with output grid.
!
! rc                   integer      OUT  Function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
! exception            integer           Thrown exception:
!
!                                        0: no exception
!                                        1: error allocating grid space
!                                        2: grid rotation error
!                                        3: grid interpolation error
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/19/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none
      integer :: this = InterpGenericErrorHandle

!     Argument List
!     -------------

      type (GenericDataType), intent(in) :: in_data
      type (GenericDataType), intent(inout) :: out_data
      real, optional, intent(in) :: rotate

!     Local Variables
!     ---------------

      integer :: rc
      real :: rotation
      type (GenericDataType) :: tmp

!     Rotate the input grid to align with
!     the output grid.
!     ===================================

      interpGeneric = -1

      rotation = 0.0
      if (present(rotate)) rotation = rotate

      tmp%grid = in_data%grid
      tmp%grid%xStart = tmp%grid%xStart + rotation

      rc = GRIDAllocate(tmp)
      if (isError(this,1,rc) .ne. 0) return

      rc = GRIDrotate(in_data,tmp)
      if (isError(this,2,rc) .ne. 0) return

!     Interpolate to the CMAP grid.
!     =============================

      rc = regrid(tmp,out_data,exchangeGrid)
      if (isError(this,3,rc) .ne. 0) return

      call GRIDDeallocate(tmp%grid)

      interpGeneric = 0

      end function interpGeneric

!******************************************************************************
      integer function interpCPCUCorr(in_data,out_data,rotate)
!******************************************************************************
! English Name: Interpolate
! -------------
!
! Purpose: Interpolates data from the input grid to the output grid for the
! -------- derived data type: CPCUCorrDataType.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This method operates on regular lat/lon grids. 
! ------
!        2. This method can be invoked using its compile-time polymorphic
!           interface name, interpolate().
!
! Usage: rc = interpolate(in_grid,out_grid,rotate=degrees)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! in_data     CPCUCorrDataType       IN  Data on input grid.
!
! out_data    CPCUCorrDataType    INOUT  Data on output grid.
!
! degrees                 real   OPT,IN  Degrees of rotation needed to align
!                                        input grid with output grid.
!
! rc                   integer      OUT  Function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
! exception            integer           Thrown exception:
!
!                                        0: no exception
!                                        1: grid interpolation error
!
!
! Interface:              Type   Access  Description
! ----------                     Intent
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/19/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none
      integer :: this = InterpCPCUCorrErrorHandle

!     Argument List
!     -------------

      type (CPCUCorrDataType), intent(in) :: in_data
      type (CPCUCorrDataType), intent(inout) :: out_data
      real, optional, intent(in) :: rotate

!     Local Variables
!     ---------------

      integer :: rc
      real :: rotation
      type (GenericDataType) :: in_tmp, out_tmp

!     Set constant parameters.
!     ========================

      interpCPCUCorr = -1

      rotation = 0.0
      if (present(rotate)) rotation = rotate

      in_tmp%grid = in_data%grid
      out_tmp%grid = out_data%grid

!     Interpolate each array element
!     contained in CPCUCorrDataType.
!     ==============================

      in_tmp%data => in_data%factor
      out_tmp%data => out_data%factor
      rc = interpolate(in_tmp,out_tmp,rotate=rotation)
      if (isError(this,1,rc) .ne. 0) return

      in_tmp%data => in_data%residual
      out_tmp%data => out_data%residual
      rc = interpolate(in_tmp,out_tmp,rotate=rotation)
      if (isError(this,1,rc) .ne. 0) return

      interpCPCUCorr = 0

      end function interpCPCUCorr

!******************************************************************************
      integer function computeLFOAvg(sample,mean)
!******************************************************************************
! English Name: Compute LFO Average
! -------------
!
! Purpose: Computes the average total precipitation by summing over the range
! -------- of temporal indices indicated by the dimension.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The data type for the output average field is not the same as the
! ------    input array type. GenericDataType is used since the mean is only
!           calculated for the total precipitation.
!
!        2. The average value is set to undefined if any grid points are
!           undefined within the sample.
!
! Usage: rc = computeLFOAvg(sample,mean)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! sample(:)        LFODataType       IN  Input land forcing data containing the
!                                        total precipitation field to be
!                                        averaged.
!
! mean         GenericDataType    INOUT  Average total precipitation.
!
! rc                   integer      OUT  Function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
! exception            integer           Thrown exception:
!
!                                        0: no exception
!                                        1: error allocating memory
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/18/2014      J.Ardizzone  created.
!******************************************************************************
      implicit none
      integer :: this = ComputeLFOAvgErrorHandle

!     Argument List
!     -------------

      type (LFODataType), dimension(:), intent(in) :: sample
      type (GenericDataType), intent(inout) :: mean

!     Local Variables
!     ---------------

      real :: undef
      integer :: n,rc
      integer :: ndim
      type (GenericDataType) :: num_count

      computeLFOAvg = 0

      ndim  = size(sample)
      if (ndim .le. 0) return

      undef = sample(1)%grid%undef

!     Allocate temporary memory
!     =========================

      computeLFOAvg = -1

      num_count%grid = sample(1)%grid
      rc = GRIDallocate(num_count)
      if (isError(this,1,rc) .ne. 0) return

!     Compute sum
!     ===========

      mean%data      = 0.0
      num_count%data = 0.0

      do n = 1,ndim

        where (sample(n)%prectot .ne. undef)
          mean%data      = mean%data + sample(n)%prectot
          num_count%data = num_count%data + 1.0
        endwhere

      end do

!     Compute average and return
!     Note: One or more undefined grid
!     points yield undefined for the average
!     ======================================

      where (nint(num_count%data) .eq. ndim) 
        mean%data = mean%data / num_count%data
      elsewhere
        mean%data = undef
      endwhere

      call GRIDDeallocate(num_count%grid)

      computeLFOAvg = 0

      end function computeLFOAvg

!******************************************************************************
      integer function computeCorrection(cmap,lfo,corr)
!******************************************************************************
! English Name: Compute Correction Factors
! -------------
!
! Purpose: Computes the CMAP correction factor and residual for adjusting the
! -------- LFO data.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The "zeroThreshold" parameter specifies the minimum precipitation
! ------    amount considered significant.
!
! Usage: rc = computeCorrection(cmap,lfo,corr)
! ------ exception = errorHandle()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! cmap            CMAPDataType       IN  CMAP precipitation.
!
! lfo          GenericDataType       IN  LFO (model) mean precipitation on
!                                        the CMAP grid.
!
! corr         CPCUCorrDataType     OUT  computed CMAP correction factors:
!
!                                        factor: ratio of CMAP total
!                                        precipitation to LFO (model) total 
!                                        precipitation where the LFO total
!                                        precipitation is significant (note-1).
!
!                                        residual: difference between CMAP
!                                        daily total precip and the LFO total
!                                        where LFO total precipitation is
!                                        insignificant (see note-1).
!
! rc                   integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: error occurred
!
! exception                              Thrown Exception: none implemented
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           12/27/2014      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (CMAPDataType), intent(in) :: cmap
      type (GenericDataType), intent(in) :: lfo
      type (CPCUCorrDataType), intent(inout) :: corr

!     Local Variables
!     ---------------

      real :: CF
      real :: LF
      integer :: i,j

      computeCorrection = 0

      CF = CMAPfactor
      LF = LFOGetFactor()

      corr%factor   = 1.0
      corr%residual = 0.0

      do j = 1,cmap%grid%ny
        do i = 1,cmap%grid%nx

          if (lfo%data(i,j) .eq. lfo%grid%undef) cycle
          if (cmap%data(i,j) .eq. cmap%grid%undef) cycle

          if (lfo%data(i,j)*LF .gt. zeroThreshold) &
          corr%factor(i,j)   = (cmap%data(i,j)*CF) / (lfo%data(i,j)*LF)

          if (lfo%data(i,j)*LF .le. zeroThreshold) &
          corr%residual(i,j) = max((cmap%data(i,j)*CF) - (lfo%data(i,j)*LF),0.0)

        end do
      end do

      corr%residual = corr%residual / LF

      end function computeCorrection

!******************************************************************************
      integer function applyCorrection(corr,lfo)
!******************************************************************************
! English Name: Apply Correction
! -------------
!
! Purpose: Applies the CMAP correction factors to the land forcing
! -------- precipitation data.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The land forcing data is corrected in-place. The input precipitation
! ------    is modified and returned as the corrected output data. This is a
!           design feature to avoid duplicating the many hours of data contained
!           in a pentad.
!
! Usage: rc = applyCorrection(corr,lfo)
! ------ exception = errorHandle()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! corr(:)     CPCUCorrDataType       IN  CMAP correction factors [see 
!                                        computeCorrection()].
!
! lfo(:)           LFODataType       IN  On input, uncorrected land forcing
!                                        data. On output, corrected land
!                                        forcing data.
!
! rc                   integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: error occurred
!
! exception                              Thrown Exception: none implemented
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/28/2014      J.Ardizzone  created.
!******************************************************************************
      implicit none
      integer :: this = ApplyCorrectionErrorHandle

!     Argument List
!     -------------

      type (CPCUCorrDataType), intent(in) :: corr
      type (LFODataType), dimension(:), intent(inout) :: lfo

!     Local Variables
!     ---------------

      real :: undef
      integer :: hour,num_hours

!     Initialize
!     ==========

      applyCorrection = -1

      num_hours = size(lfo)
      undef     = lfo(1)%grid%undef

!     Apply correction factor for each
!     hour of the day
!     ================================

      do hour = 1,num_hours

        lfo(hour)%precsno = min(lfo(hour)%precsno, lfo(hour)%prectot)
        lfo(hour)%preccon = min(lfo(hour)%preccon, lfo(hour)%prectot)

        where (lfo(hour)%prectot .ne. undef .and. corr%factor .ne. undef) &
          lfo(hour)%prectot = max(lfo(hour)%prectot * corr%factor, 0.0)

        where (lfo(hour)%precsno .ne. undef .and. corr%factor .ne. undef) &
          lfo(hour)%precsno = max(lfo(hour)%precsno * corr%factor, 0.0)

        where (lfo(hour)%preccon .ne. undef .and. corr%factor .ne. undef) &
          lfo(hour)%preccon = max(lfo(hour)%preccon * corr%factor, 0.0)

      end do

      applyCorrection = 0

      end function applyCorrection
      
!******************************************************************************
      integer function getArgs(args)
!******************************************************************************
! English Name: Get Arguments
! -------------
!
! Purpose: Retrieves command-line arguments and configuration parameters.
! -------- Provides the user interface for executing methods implemented by
!          this module.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Filename templates follow the conventions established by the Unix
! ------    date command for time tokens (e.g. %Y, %m, %d, %H, %M, %S).
!
! Usage: rc = getArgs(args)
! ------ exception = errorHandler()
!
!        This method reads a configuration file containing the corrector
!        namelist as follows:
!
!        &corrector
!
!          in_file =
!          out_file =
!          cmap_file =
!          cmap_scale_file =
!          exchange_dlon = 
!          exchange_dlat =
!        /
!
!        It also retrieves the command-line arguments as follows:
!
!        [config] [start_date] [end_date]
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! args       UserInterfaceType      OUT  Returned data structure containing
!                                        the command-line arguments and
!                                        configuration parameters.
!
!  config               string       *   Name of configuration file containing
!                                        input namelists.
!
!  start_date          integer       *   Starting day to be processed (ccyymmdd)
!
!  end_date            integer       *   Ending day to be processed (ccyymmdd).
!
!  in_file              string       *   Filename template of the input GEOS
!                                        land-forcing files (see note-1). For
!                                        example:
!
!                                        /data/input.%Y%m%d_%H30z
!
!  out_file             string       *   Filename template of the output GEOS
!                                        land-forcing files (see note-1). For
!                                        example:
!
!                                        /data/output.%Y%m%d_%H30z
!
! rc                   integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception issued
!
! exception            integer      OUT  Thrown Exception:
!
!                                        0: no exception
!                                        1: incorrect number of arguments
!                                        2: invalid date format
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/28/2014      J.Ardizzone  created.
!******************************************************************************
      implicit none
      integer :: this = GetArgsErrorHandle

!     Argument List
!     -------------

      type (UserInterfaceType), intent(out) :: args

!     Local Variables
!     ---------------

      integer :: rc
      character (len=1024) :: str

      real :: exchange_dlon, exchange_dlat
      character (len=1024) :: in_file, out_file, cmap_file, cmap_scale_file

      namelist /corrector/ in_file, out_file, cmap_file, cmap_scale_file, &
                           exchange_dlon, exchange_dlat

      rc = 0
      getArgs = -1

!     Initialize
!     ==========

      in_file         = ''
      out_file        = ''
      cmap_file       = ''
      cmap_scale_file = ''
      exchange_dlon   = 0.0
      exchange_dlat   = 0.0

!     Check Usage
!     ===========

      if (iargc() .ne. 3) rc = 1
      if (isError(this,1,rc)) return

!     Retrieve the command-line arguments
!     ===================================

      call getarg(1,args%config)

      call getarg(2,str)
      read(unit=str,fmt='(i)') args%start_date
      if (args%start_date .lt. 19000000) rc = 1
      if (isError(this,2,rc,arg=str)) return

      call getarg(3,str)
      read(unit=str,fmt='(i)') args%end_date
      if (args%end_date .lt. 19000000) rc = 1
      if (isError(this,2,rc,arg=str)) return

!     Read in the configuration
!     =========================

      open (unit=5,file=args%config,status='old',iostat=rc)
      if (isError(this,3,rc,arg=args%config) .ne. 0) return

      read(5,nml=corrector,iostat=rc)
      if (isError(this,4,rc,arg=args%config) .ne. 0) return

      close (unit=5)

!     Save configuration (perform sanity check)
!     =========================================

      args%in_file         = in_file
      args%out_file        = out_file
      args%cmap_file       = cmap_file
      args%cmap_scale_file = cmap_scale_file
      args%exchange_dlon   = exchange_dlon
      args%exchange_dlat   = exchange_dlat

      if (len_trim(in_file) .eq. 0)   rc = isError(this,5,-1,arg='in_file')
      if (len_trim(out_file) .eq. 0)  rc = isError(this,5,-1,arg='out_file')
      if (len_trim(cmap_file) .eq. 0) rc = isError(this,5,-1,arg='cmap_file')

      if (len_trim(cmap_scale_file) .eq. 0) &
                                rc = isError(this,5,-1,arg='cmap_scale_file')

      if (exchange_dlon .le. 0) rc = isError(this,5,-1,arg='exchange_dlon')
      if (exchange_dlat .le. 0) rc = isError(this,5,-1,arg='exchange_dlat')

      if (rc .eq. 0) getArgs = 0

      end function getArgs
!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,arg)
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
! arg                   string   OPT,IN  command-line argument
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
      character(len=*), intent(in), optional :: arg

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

        case (CorrectPrecipErrorHandle)
!       ===============================

        select case (errorEvent)

          case (1)

            print *, ''

          case (2)

            print *, 'correctPrecip (error#',code,'): error reading CMAP data'

          case (3)

            print *, 'correctPrecip (error#',code,'): error reading CMAP ', &
                     'scaling file'

          case (4)

            print *, 'correctPrecip (error#',code,'): error reading LFO data'

          case (5)

            print *, 'correctPrecip (error#',code,'): error retrieving CMAP data'

          case (6)

            print *, 'correctPrecip (error#',code,'): error retrieving LFO data'

          case (7)

            print *, 'correctPrecip (error#',code,'): error allocating ', &
                     'grid space'

          case (8)

            print *, 'correctPrecip (error#',code,'): error computing LFO ', &
                     'mean precipitation'

          case (9)

            print *, 'correctPrecip (error#',code,'): interpolation error'

          case (10)

            print *, 'correctPrecip (error#',code,'): error computing ', &
                     'correction factors'

          case (11)

            print *, 'correctPrecip (error#',code,'): error applying ', &
                     'correction factors'

          case (12)

            print *, 'correctPrecip (error#',code,'): error writing to output'

          case default

            print *, 'correctPrecip (error#',code,'): unknown error'

        end select

        case (ComputeLFOAvgErrorHandle)
!       ===============================

        select case (errorEvent)

          case (1)

            print *, 'computeLFOAvg (error#',code,'): ', &
                     'error allocating grid space'

          case default

            print *, 'computeLFOAvg (error#',code,'): unknown error'

        end select

        case (InterpGenericErrorHandle)
!       ===============================

        select case (errorEvent)

          case (1)

            print *, 'interpGeneric (error#',code,'): error allocating ', &
                     'grid space'

          case (2)

            print *, 'interpGeneric (error#',code,'): grid rotation error'

          case (3)

            print *, 'interpGeneric (error#',code,'): grid interpolation error'

          case default

            print *, 'interpGeneric (error#',code,'): unknown error'

        end select

        case (InterpCPCUCorrErrorHandle)
!       ================================

        select case (errorEvent)

          case (1)

            print *, 'interpCPCUCorr (error#',code,'): grid interpolation error'

          case default

            print *, 'interpCPCUCorr (error#',code,'): unknown error'

        end select

        case (GetArgsErrorHandle)
!       =========================

        select case (errorEvent)

          case (1)

            print *, 'Usage: correctPrecip [config] [sccyymmdd] [eccyymmdd]'
            print *, ' '

          case (2)

            print *, 'getArgs (error#',code,'): bad date format: ', &
                     'date = ',trim(arg)

          case (3)

            print *, 'getArgs (error#',code,'): unable to access config ', &
                     'file "',trim(arg),'"'

          case (4)

            print *, 'getArgs (error#',code,'): error reading corrector ', &
                     'namelist from config file "',trim(arg),'"'

          case (5)

            print *, 'getArgs (error#',code,'): the following configuration ', &
                     'parameter is missing or incorrect: "',trim(arg),'"'

          case default

            print *, 'getArgs (error#',code,'): unknown error'

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

      end module CMAP_A_CorrectorMod
