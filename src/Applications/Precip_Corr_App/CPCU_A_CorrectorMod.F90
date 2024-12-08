!******************************************************************************
      module CPCU_A_CorrectorMod
!******************************************************************************
! English Name: CPCU-A Corrector Module
! -------------
!
! Purpose: Provides methods for executing the precipitation correction
! -------- algorithm using method CPCU Algorithm A. See the following document
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
!                                        use CPCU_A_CorrectorMod, &
!                                                  CorrectorError=>errorHandler
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/02/2015      J.Ardizzone  created.
!******************************************************************************

      use TypesMod
      use RegridMethodMod, RegridMethodError=>errorHandler
      use GridMod, GridError=>errorHandler
      use LFOBaseMod, LFOBaseError=>errorHandler
      use CPCUFileMod, CPCUFileError=>errorHandler
      use RegridMod, RegridError=>errorHandler

      private
      public :: getArgs
      public :: correctPrecip
      public :: UserInterfaceType
      public :: errorHandler

      real, parameter    :: zeroThreshold = 1.0e-4
      real, parameter    :: LAT_TRANS_MIN = 50.0
      real, parameter    :: LAT_TRANS_MAX = 60.0

      integer :: IUMASK = 61

      integer, parameter :: CorrectPrecipErrorHandle = 1
      integer, parameter :: GetArgsErrorHandle = 2
      integer, parameter :: ComputeDailyPrecipErrorHandle = 3
      integer, parameter :: ApplyCorrectionErrorHandle = 4
      integer, parameter :: ApplyResidualErrorHandle = 5
      integer, parameter :: ApplyMaskErrorHandle = 6

      type UserInterfaceType

        integer :: start_date,end_date
        real    :: exchange_dlon, exchange_dlat
        logical :: scale_cpcu, scale_lfo
        logical :: exclude_africa, latitude_tapering, maskout

        character(len=1024) :: config
        character(len=1024) :: eod_file
        character(len=1024) :: in_file
        character(len=1024) :: cpcu_file
        character(len=1024) :: cpcu_scale_file
        character(len=1024) :: lfo_scale_file
        character(len=1024) :: out_file
        character(len=1024) :: mask_file

      end type UserInterfaceType

      integer :: errorCode = 0
      type (GridInfoType) :: LFOgrid
      type (GridInfoType) :: CPCUgrid

      contains

!******************************************************************************
      integer function correctPrecip(idate)
!******************************************************************************
! English Name: Correct Precipitation
! -------------
!
! Purpose: Implements the precipitation corrector API using CPCU data. This 
! -------- corrector also implements latitude weighting/tapering and Africa
!          masking.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This corrector module assumes that the input land forcing data
! ------    to be corrected consists of hourly precipitation data with an
!           assigned time centered on the hour: (eg 1:30,2:30,...). It also
!           assumes that the day to be corrected starts at 6:30Z of the 
!           date and ends at 5:30Z of the next day. This convention was adopted
!           to align with the CPCU end-of-day values which range from 6Z to
!           30Z (see CPCU_EODHours).
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer       IN  date denoting the day of precipitation
!                                        to be corrected: ccyymmdd
!
! correctPrecip        integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception issued
!
!                                        Thrown Exceptions:
!
!                                        1: error retrieving command-line args
!                                        2: error reading CPCU daily precip
!                                        3: error reading CPCU EOD data
!                                        4: error reading land forcing data
!                                        5: error fetching CPCU data
!                                        6: error fetching LFO data
!                                        7: error allocating memory
!                                        8: error applying correction factor
!                                        9: error applying residual
!                                       10: error writing corrected LFO
!                                       11: error applying CPCU scaling factors
!                                       12: error applying LFO scaling factors
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/02/2015      J.Ardizzone  created.
!           01/11/2016      J.Ardizzone  added LFO scaling.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate

!     Local Variables
!     ---------------

      integer :: i
      integer :: rc
      integer, save :: heap = 0
      integer :: this = CorrectPrecipErrorHandle

      type (GenericDataType) :: mask
      type (UserInterfaceType) :: args
      type (CPCUDataType) :: in_eod,eod
      type (LFODataType), dimension(72) :: LFOin
      type (LFODataType), dimension(24) :: LFOout
      type (PrecipDataType), dimension(2) :: precip
      type (CPCUDataType), dimension(2) :: in_cpcu,cpcu
      type (CPCUCorrDataType), dimension(2) :: correction

      correctPrecip = -1

!     Initialize
!     ==========

      rc = getArgs(args)
      if (isError(this,1,rc) .ne. 0) return

      rc = setExchangeParams(args%exchange_dlon,args%exchange_dlat)

      heap = GRIDAllocate(heap)

!     Read in CPCU data
!     =================

      print *, 'Reading in CPCU data for ',idate

      rc = CPCURead2Days(args%cpcu_file,idate,0)
      if (isError(this,2,CPCUFileError()) .ne. 0) return

      if (args%scale_cpcu) then 
        rc = CPCUScaleDaily(args%cpcu_scale_file,idate,0)
        if (isError(this,11,CPCUFileError()) .ne. 0) return
      endif

      rc = CPCUReadEOD(args%eod_file)
      if (isError(this,3,CPCUFileError()) .ne. 0) return

!     Read in 3-days of land forcing data
!     ===================================

      print *, 'Reading in LFO data for ', idate

      rc = LFORead3Days(args%in_file,idate,63000)
      if (isError(this,4,LFOBaseError()) .ne. 0) return

      if (args%scale_lfo) then
        rc = LFOScalePrecip(args%lfo_scale_file,idate,63000)
        if (isError(this,12,LFOBaseError()) .ne. 0) return
      endif

!     Retrieve CPCU data
!     ==================

      rc = CPCUGetDay(1,in_cpcu(1))
      if (isError(this,5,CPCUFileError()) .ne. 0) return

      rc = CPCUGetDay(2,in_cpcu(2))
      if (isError(this,5,CPCUFileError()) .ne. 0) return

      rc = CPCUGetEOD(in_eod)
      if (isError(this,5,CPCUFileError()) .ne. 0) return

      CPCUgrid = in_cpcu(1)%grid

!     Retrieve Land Forcing Data
!     ==========================

      rc = LFOGetDay(1,LFOin( 1:24))
      if (isError(this,6,LFOBaseError()) .ne. 0) return
      rc = LFOGetDay(2,LFOin(25:48))
      if (isError(this,6,LFOBaseError()) .ne. 0) return
      rc = LFOGetDay(3,LFOin(49:72))
      if (isError(this,6,LFOBaseError()) .ne. 0) return

      LFOgrid = LFOin(1)%grid

!     Rotate the CPCU grid to be
!     consistent with the GEOS
!     starting longitude convention.
!     ==============================

      print *, 'Rotating CPCU data for ',idate

      CPCUgrid%xStart = CPCUgrid%xStart - 180.0

      cpcu%grid = CPCUgrid
      eod%grid  = CPCUgrid

      rc = GRIDAllocate(cpcu)
      if (isError(this,7,GridError()) .ne. 0) return
      rc = GRIDAllocate(eod)
      if (isError(this,7,GridError()) .ne. 0) return

      rc = GRIDrotate(in_cpcu,cpcu)
      rc = GRIDrotate(in_eod,eod)

!     Compute Daily precipitation from hourly
!     hourly LFO data
!     =======================================

      print *, 'Computing daily total precipitation for ',idate

      precip%grid = CPCUgrid
      rc = GRIDallocate(precip)
      if (isError(this,7,GridError()) .ne. 0) return

      rc = computeDailyPrecip(eod,LFOin(1:48),precip(1))
      rc = computeDailyPrecip(eod,LFOin(25:72),precip(2))

!     Compute CPCU correction factors and residuals
!     for adjusting LFO precipitation
!     =============================================

      print *, 'Computing CPCU correction factors for ',idate

      correction%grid = CPCUgrid
      rc = GRIDallocate(correction)
      if (isError(this,7,GridError()) .ne. 0) return
      correction(1)%eod = eod%data
      correction(2)%eod = eod%data

      call computeCorrection(precip(1),cpcu(1),correction(1))
      call computeCorrection(precip(2),cpcu(2),correction(2))

!     Correct the LFO precipitation data
!     ==================================

      LFOout%grid = LFOin(25:48)%grid
      rc = GRIDallocate(LFOout)
      if (isError(this,7,rc) .ne. 0) return

      print *, 'Applying correction factors for ',idate
      rc = applyCorrection(correction,LFOin(25:48),LFOout)
      if (isError(this,8,rc) .ne. 0) return
     
      print *, 'Applying residual for ',idate
      rc = applyResidual(correction,LFOin(25:48),LFOout)
      if (isError(this,9,rc) .ne. 0) return

      if (args%latitude_tapering) then 

        print *, 'Applying latitude weighting for ',idate
        rc = applyLatWeight(LFOin(25:48),LFOout)

      endif

      if (args%exclude_africa) then

        print *, 'Applying correction mask for ',idate
        rc = applyMask(args%mask_file,LFOin(25:48),LFOout,maskout=args%maskout)

      endif

!     if (args%maskout) then

!       print *, 'Masking uncorrected data for ',idate

!       mask%grid =  cpcu_mg(1)%grid
!       mask%data => cpcu_mg(1)%data
!       rc = maskdata(LFOout,mask)

!     endif

!     Write out corrected LFO precipitation data
!     ==========================================

      print *, 'Writing corrected precipitation data for ',idate

      rc = LFOWriteFile(args%out_file,LFOout)
      if (isError(this,10,rc) .ne. 0) return

      call GRIDDeallocate(heap)

      correctPrecip = 0

      end function correctPrecip

!******************************************************************************
      integer function computeDailyPrecip(eod,lfo,dailyAvg)
!******************************************************************************
! English Name: Compute Daily Precipitation
! -------------
!
! Purpose: Sums up hourly total precipitation to arrive at a daily average that
! -------- aligns with the end-of-day boundary of the CPCU data.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Specific conventions are assumed for the CPCU and LFO data;
! ------    specifically, the CPCU end-of-day (EOD) hour is assumed to be 
!           between 6 and 30. Consequently, the LFO 48 hour period is required
!           to start at 6Z and will intersect 3-days.
!
!        2. This routine assumes that there are no undefined values in the
!           LFO precipitation fields. 
!
!        3. Returned precipitation is on the CPCU grid.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! eod             CPCUDataType       IN  CPCU end-of-day hour information.
!                             
! lfo(:)           LFODataType       IN  48 hours of land forcing data starting
!                                        at 6Z. Only the total precipitation
!                                        information is used.
!
! dailyAvg      PrecipDataType      OUT  Average daily precipitation aligned
!                                        with the CPCU end-of-day. For example,
!                                        if the end of day is 6Z, then the 
!                                        daily total is the sum from 6Z of day-1
!                                        to 5Z of day-2. For end-of-day of 30,
!                                        the daily total is from 6Z of day-2 to
!                                        5Z of day-3.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           06/21/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (CPCUDataType), intent(in) :: eod
      type (LFODataType), dimension(:), intent(in) :: lfo
      type (PrecipDataType), intent(inout) :: dailyAvg

!     Local Variables
!     ---------------

      integer :: rc
      integer :: h1,h2
      integer :: eodIndex,eodHour
      type (PrecipDataType) :: inPrecip
      type (PrecipDataType) :: outPrecip
      integer :: this = ComputeDailyPrecipErrorHandle

      rc = 0
      computeDailyPrecip = -1

      dailyAvg%data = 0.0

!     Allocate local grids
!     ====================

      inPrecip%grid = lfo(1)%grid
      outPrecip%grid = eod%grid

      rc = GRIDAllocate(inPrecip)
      if (isError(this,1,rc) .ne. 0) return
      rc = GRIDAllocate(outPrecip)
      if (isError(this,1,rc) .ne. 0) return

!     Compute daily average precipitation
!     for each EOD
!     ===================================

      do eodIndex = 1,size(CPCU_EODHours)

        eodHour   = CPCU_EODHours(eodIndex)
        h1 = eodHour - 6 + 1
        h2 = h1 + 23

!       Compute the LFO daily average precip for
!       the current EOD

        call averagePrecip(lfo(h1:h2),inPrecip)

!       Regrid the daily average to the CPCU
!       grid.

        rc = regrid(inPrecip,outPrecip,exchangeGrid)
        if (isError(this,2,rc) .ne. 0) return

!       Only record the daily average for grid
!       points with the current EOD

        where (nint(eod%data) .eq. eodHour)
          dailyAvg%data = outPrecip%data
        end where

      end do

      call GRIDDeallocate(inPrecip%grid)
      call GRIDDeallocate(outPrecip%grid)

      computeDailyPrecip = 0

      end function computeDailyPrecip

!******************************************************************************
      subroutine averagePrecip(lfo,precip)
!******************************************************************************

      implicit none

      type (LFODataType), dimension(:), intent(in) :: lfo
      type (PrecipDataType), intent(inout) :: precip

      real :: count
      integer :: i,n

      n = size(lfo)
      precip%data = 0.0

      do i = 1,n
        precip%data = precip%data + lfo(i)%prectot
      end do

      count = n
      precip%data = precip%data / count

      end subroutine averagePrecip

!******************************************************************************
      subroutine computeCorrection(precip,cpcu,correction)
!******************************************************************************
! English Name: Compute CPCU Correction
! -------------
!
! Purpose: Computes the CPCU correction factor and residual for adjusting the
! -------- LFO data.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The "zeroThreshold" parameter specifies the minimum precipitation
! ------    amount considered significant.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! precip        PrecipDataType       IN  LFO daily precipitation total aligned
!                                        with the CPCU EOD.
!
! cpcu            CPCUDataType       IN  CPCU daily precipitation total aligned
!                                        with the CPCU EOD (by definition).
!
! correction   CPCUCorrDataType     OUT  computed CPCU correction factors:
!
!                                        factor: ratio of CPCU daily total
!                                        precip to LFO daily total where LFO
!                                        total precip is significant (note-1).
!
!                                        residual: difference between CPCU
!                                        daily total precip and the LFO total
!                                        where LFO total precip is insignificant
!                                        (see note-1).
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           06/26/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (CPCUDataType), intent(in) :: cpcu
      type (CPCUCorrDataType), intent(inout) :: correction
      type (PrecipDataType), intent(in) :: precip

!     Local Variables
!     ---------------

      real :: CF
      real :: LF
      real :: undef

      CF = CPCUfactor
      LF = LFOGetFactor()

      undef  = cpcu%grid%undef
      correction%factor = 1.0
      correction%residual = 0.0

      where (precip%data*LF .gt. zeroThreshold .and. cpcu%data .ne. undef)
        correction%factor   = (cpcu%data*CF) / (precip%data*LF)
        correction%factor = min(correction%factor, 10.)
        ! the following equestion catpures missing precip due to factor capping
        correction%residual = max((cpcu%data*CF) - (precip%data*LF)*correction%factor,0.0)
      end where

      where (precip%data*LF .le. zeroThreshold .and. cpcu%data .ne. undef)
        correction%residual = max((cpcu%data*CF) - (precip%data*LF),0.0)
      end where

      correction%residual = correction%residual / LF

      end subroutine computeCorrection

!******************************************************************************
      integer function applyCorrection(correction,LFOin,LFOout)
!******************************************************************************
! English Name: Apply Correction
! -------------
!
! Purpose: Applies the CPCU correction to the land forcing precipitation.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Correction information is partitioned into two days and is used
! ------    as a function of the end-of-day (EOD) and the hour of day (HOD).
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! correction(:)   CPCUCorrDataType   IN  CPCU correction information (see
!                                        note-1).
!
! LFOin            LFODataType       IN  Land forcing data to be corrected.
!
! LFOout           LFODataType      OUT  Corrected land forcing data (precip).
!                                        Data is corrected for each hour in
!                                        the 24-hour period. The first hour
!                                        is expected to be 6Z since the CPCU
!                                        end-of-day ranges from 6Z-30Z (6Z of
!                                        the next day).
!
! applyCorrection      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: error occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error allocating memory
!                                        2: grid interpolation error
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           07/10/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (LFODataType), dimension(24), intent(in) :: LFOin
      type (LFODataType), dimension(24), intent(inout) :: LFOout
      type (CPCUCorrDataType), dimension(2), intent(in) :: correction

!     Local Variables
!     ---------------

      integer :: rc
      integer :: day
      integer :: eodHour
      integer :: eodIndex
      integer :: hour,hourOfDay
      type (GenericDataType) :: factor
      type (GenericDataType) :: factor_mg
      real, dimension(:,:), pointer :: eod => null()

      integer :: this = ApplyCorrectionErrorHandle

!     Initialize
!     ==========

      applyCorrection = -1

      factor%grid = CPCUgrid
      factor_mg%grid = LFOgrid

      eod => correction(1)%eod
      rc = GRIDAllocate(factor)
      if (isError(this,1,rc) .ne. 0) return
      rc = GRIDAllocate(factor_mg)
      if (isError(this,1,rc) .ne. 0) return

!     Apply correction factor for each
!     hour of the day
!     ================================

      do hour = 1,24

        factor%data = 1.0

!       Aggregate correction factors based
!       on end of day.

        do eodIndex = 1,size(CPCU_EODHours)

!         The starting hour for the LFO data is actually 6:30Z which is
!         rounded to 7Z.

          eodHour   = CPCU_EODHours(eodIndex)
          hourOfDay = hour + 6

!         If the end-of-day (EOD) hour is greater than or equal to the hour of 
!         the day (HOD), then the day-1 period [EOD-24,EOD] encompasses the HOD.
!         Otherwise, the HOD falls in the day-2 period [EOD,EOD+24]. 

          day = 1
          if (eodHour .lt. hourOfDay) day = 2

          where (nint(eod) .eq. eodHour) factor%data = correction(day)%factor

        end do

!       Interpolate the aggregated correction factors
!       to the model grid.

        rc = regrid(factor,factor_mg,exchangeGrid)
        if (isError(this,2,rc) .ne. 0) return
        where (factor_mg%data .eq. LFOgrid%undef) factor_mg%data = 1.0

!       Apply the correction factors

        LFOout(hour)%precls  = LFOin(hour)%precls  * factor_mg%data
        LFOout(hour)%preccon = LFOin(hour)%preccon * factor_mg%data
        LFOout(hour)%precsno = LFOin(hour)%precsno * factor_mg%data

      end do

      call GRIDDeallocate(factor%grid)
      call GRIDDeallocate(factor_mg%grid)

      applyCorrection = 0

      end function applyCorrection

!******************************************************************************
      integer function applyResidual(correction,LFOin,LFOout)
!******************************************************************************
! English Name: Apply Residual Correction
! -------------
!
! Purpose: Adds residual CPCU precipitation to liquid and frozen precipitation
! -------- totals for the early morning hours of each timezone.
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
! correction  CorrectionDataType     IN  CPCU correction information.
!
! LFOin            LFODataType       IN  Land forcing data for the 24 hour
!                                        period to be corrected.
!
! LFOout           LFODataType      OUT  Corrected land forcing data for the
!                                        24 hour period.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           07/02/2012      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (LFODataType), dimension(24), intent(in) :: LFOin
      type (LFODataType), dimension(24), intent(inout) :: LFOout
      type (CPCUCorrDataType), dimension(2), intent(in) :: correction

!     Local Variables
!     ---------------

      integer :: rc
      integer :: hIndex
      real    :: startLon,endLon
      type (TimeZoneDataType) :: timezone
      integer :: TZindex,utcTime,localHour
      integer :: this = ApplyResidualErrorHandle
      type (CPCUCorrDataType), dimension(2) :: corr_mg

      applyResidual = -1

!     First regrid the correction data to
!     the model grid.
!     ===================================

      corr_mg%grid = LFOgrid
      rc = GRIDAllocate(corr_mg)
      if (isError(this,1,rc) .ne. 0) return

      rc = regrid(correction(1),corr_mg(1),exchangeGrid, &
                                     eodMethod=nearestNeighbor)
      if (isError(this,2,rc) .ne. 0) return

      rc = regrid(correction(2),corr_mg(2),exchangeGrid, &
                                     eodMethod=nearestNeighbor)
      if (isError(this,2,rc) .ne. 0) return

      where (corr_mg(1)%factor .eq. LFOgrid%undef) corr_mg(1)%factor = 1.0
      where (corr_mg(2)%factor .eq. LFOgrid%undef) corr_mg(2)%factor = 1.0
      where (corr_mg(1)%residual .eq. LFOgrid%undef) corr_mg(1)%residual = 0.0
      where (corr_mg(2)%residual .eq. LFOgrid%undef) corr_mg(2)%residual = 0.0
      where (corr_mg(1)%residual .lt. 0.0) corr_mg(1)%residual = 0.0
      where (corr_mg(2)%residual .lt. 0.0) corr_mg(2)%residual = 0.0

!     Bundle EOD information into the timezone data
!     structure for convenience.
!     =============================================

      timezone%grid = LFOout(1)%grid
      timezone%eod  => corr_mg(1)%eod
      timezone%grid%undef = corr_mg(1)%grid%undef

!     Add CPCU residual to the early morning hours
!     of each timezone.
!     ============================================

      do TZindex = 1,24

!       There are 15 degrees longitude per timezone
!       -------------------------------------------

        startLon = -180 + (TZindex-1) * 15.0
        endLon   = -180 + TZindex * 15.0

        rc = gridMap(timezone%grid,reset=.true.)
        rc = gridMap(timezone%grid,ilon1=startLon,elon2=endLon)

!       Spread precipitation residual evenly over
!       the early morning hours (0-3am)
!       -----------------------------------------

        do localHour = 0,2 

          timezone%localHour = localHour
          utcTime = mod( (localHour + 1 + 12 - TZindex), 24)
          if (utcTime .lt. 6) utcTime = utcTime + 24

          timezone%utcTime = utcTime

          hIndex = utcTime - 6 + 1
          call addResidual(timezone,corr_mg,LFOin(hIndex),LFOout(hIndex))

        end do

      end do

!     Clean-up and exit
!     =================

      call GRIDDeallocate(corr_mg(1)%grid)
      call GRIDDeallocate(corr_mg(2)%grid)

      applyResidual = 0

      end function applyResidual

!******************************************************************************
      subroutine addResidual(timezone,correction,LFOin,LFOout)
!******************************************************************************
! English Name: Add Residual
! -------------
!
! Purpose: Adds residual CPCU precipitation to liquid and frozen precipitation
! -------- totals for the early morning hour within the specified timezone.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This routine is invoked by applyResidual().
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! timezone    TimezoneDataType       IN  temporal and spatial information
!                                        defining the timezone. CPCU end-of-day
!                                        information is bundled into this
!                                        data structure.
!
! correction    CPCUCorrDataType     IN  CPCU correction information.
!
! LFOin            LFODataType       IN  Land forcing data for the local early
!                                        morning hour within the timezone.
!
! LFOout           LFODataType      OUT  Corrected land forcing data for the
!                                        local early morning hour within the
!                                        timezone. CPCU residual precipitation
!                                        is added to the liquid and frozen
!                                        totals.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           07/02/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (LFODataType), intent(in) :: LFOin
      type (LFODataType), intent(inout) :: LFOout
      type (TimezoneDataType), intent(in) :: timezone
      type (CPCUCorrDataType), dimension(2), intent(in) :: correction

!     Local Variables
!     ---------------

      integer :: day
      integer :: utcTime
      integer :: i,j,i1,i2,ny

      real, pointer :: eod(:,:) => null()

      ny      = timezone%grid%ny
      i1      = timezone%grid%i1
      i2      = timezone%grid%i2
      utcTime = timezone%utcTime
      eod     =>timezone%eod

!     Add residual to liquid and frozen precipitation
!     within the specified timezone.
!     ===============================================

      do j = 1,ny

        do i = i1,i2

          if (eod(i,j) .eq. timezone%grid%undef) cycle

          day = 1
          if (nint(eod(i,j)) .le. utcTime) day = 2

          if (LFOin%airtemp(i,j) .ge. 273.15) then
            LFOout%precls(i,j)  = LFOout%precls(i,j) + \
                                    8.0*correction(day)%residual(i,j)
          else
            LFOout%precsno(i,j) = LFOout%precsno(i,j) + \
                                    8.0*correction(day)%residual(i,j)
          endif

!         LFOout%precls(i,j)  = LFOout%precls(i,j) + \
!                                   8.0*correction(day)%residual(i,j)
!
!         if (LFOin%airtemp(i,j) .lt. 273.15) then
!
!           LFOout%precsno(i,j) = LFOout%precsno(i,j) + \
!                                   8.0*correction(day)%residual(i,j)
!         endif

        end do

      end do

      end subroutine addResidual

!******************************************************************************
      integer function applyLatWeight(LFOin,LFOout)
!******************************************************************************
! English Name: Apply Latitude Weighting
! -------------
!
! Purpose: Adjusts the CPCU correction between LAT_TRANS_MIN and
! -------- LAT_TRANS_MAX using a linear in latitude weighting factor between
!          one and zero such that there is diminishing correction with
!          increasing latitude.
!          
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Correction information is partitioned into two days and is used
! ------    as a function of the end-of-day (EOD) and the hour of day (HOD).
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! correction(:)   CPCUCorrDataType   IN  CPCU correction information (see
!                                        note-1).
!
! LFOin            LFODataType       IN  Land forcing data to be corrected.
!
! LFOout           LFODataType      OUT  Corrected land forcing data (precip).
!                                        Data is corrected for each hour in
!                                        the 24-hour period. The first hour
!                                        is expected to be 6Z since the CPCU
!                                        end-of-day ranges from 6Z-30Z (6Z of
!                                        the next day).
!
! applyCorrection      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: error occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error allocating memory
!                                        2: grid interpolation error
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           07/10/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (LFODataType), dimension(24), intent(in) :: LFOin
      type (LFODataType), dimension(24), intent(inout) :: LFOout

!     Local Variables
!     ---------------

      integer :: j
      integer :: hour
      real :: lat, alat, weight
      type (GridInfoType) :: grid
      type (LFODataType) :: model, corrected

      grid = LFOin(1)%grid

!     Apply latitude weighting for
!     each hour of the day.
!     ============================

      do j = 1,grid%ny

!       Calculate latitude weight
 
        lat = (j - 1) * grid%dy + grid%yStart
        alat = abs(lat)

        if (alat .gt. LAT_TRANS_MAX) then
          weight = 0.0
        elseif (alat .lt. LAT_TRANS_MIN) then
          weight = 1.0
        else
          weight = (LAT_TRANS_MAX - alat) / (LAT_TRANS_MAX - LAT_TRANS_MIN)
        endif

!       Apply the weight to each
!       hour of the day.

        do hour = 1,24

          corrected = LFOout(hour)
          model     = LFOin(hour)

          where (corrected%precls(:,j) .ne. grid%undef)

            corrected%precls(:,j)  = weight * corrected%precls(:,j)   + &
                                          (1.0 - weight) * model%precls(:,j)

            corrected%preccon(:,j) = weight * corrected%preccon(:,j)   + &
                                         (1.0 - weight) * model%preccon(:,j)

            corrected%precsno(:,j)  = weight * corrected%precsno(:,j) + &
                                         (1.0 - weight) * model%precsno(:,j)

          endwhere

        end do

      end do

      applyLatWeight = 0

      end function applyLatWeight
!******************************************************************************
      integer function applyMask(filename,LFOin,LFOout,maskout)
!******************************************************************************
! English Name: Apply Mask
! -------------
!
! Purpose: Reverts corrected precipitation values to the original model
! -------- values where the mask is set to "1". The mask specifies those points
!          where the CPCU data is not reliable. Alternatively, masked values
!          can be set to undefined (see maskout optional argument).
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The masking dataset is discovered using the environment variable,
! ------    "CPCU_MASK_FILENAME".
!
!        2. The mask file specified by "CPCU_MASK_FILENAME" is a flat binary
!           file consisting of the characters "1" or "0" and having the 
!           dimension and orientation of the input model grid.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! LFOin            LFODataType       IN  Model Land forcing data.
!
! LFOout           LFODataType    INOUT  Corrected land forcing data (precip).
!                                        On output, corrected data points
!                                        coincident to a mask value of "1" are
!                                        set to the model data value (see LFOin)
!                                        or undefined (see maskout).
!
! maskout              logical   OPT,IN  When true, causes the masked data
!                                        locations to be set to undefined.
!
! applyMask            integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: error occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error accessing mask file
!                                        2: error reading mask file
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2013      J.Ardizzone  created.
!           01/05/2015      J.Ardizzone  added optional "maskout" argument.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      logical, optional, intent(in) :: maskout
      character (len=*), intent(in) :: filename
      type (LFODataType), dimension(24), intent(in) :: LFOin
      type (LFODataType), dimension(24), intent(inout) :: LFOout

!     Local Variables
!     ---------------

      integer :: rc,hour,reclen
      type (GridInfoType) :: grid
      type (LFODataType) :: corrected,model
      character, dimension(:,:), pointer, save :: mask => null()

      integer :: this = ApplyMaskErrorHandle

      applyMask = -1
      grid = LFOin(1)%grid

!     Allocate space for the mask
!     ===========================

      if (associated(mask)) deallocate(mask)
      allocate(mask(grid%nx,grid%ny),stat=rc)
      if (isError(this,1,rc) .ne. 0) return

      reclen = grid%nx * grid%ny

!     Read the masking file
!     =====================

      open (unit=IUMASK, file=filename, form='unformatted', &
            access='direct',recl=reclen,status='old',iostat=rc)
      if (isError(this,2,rc,arg=filename) .ne. 0) return

      read(IUMASK,rec=1,iostat=rc) mask
      if (isError(this,3,rc,arg=filename) .ne. 0) return

      close (unit=IUMASK)

!     Apply the mask. Values are replaced
!     where the mask value is '1'.
!     ===================================

      do hour = 1,24

        corrected = LFOout(hour)
        model     = LFOin(hour)

        if (present(maskout) .and. maskout) then

          where (mask .eq. '1') 
            corrected%precls = model%grid%undef
            corrected%preccon = model%grid%undef
            corrected%precsno = model%grid%undef
          endwhere

        else

          where (mask .eq. '1') 
            corrected%precls = model%precls
            corrected%preccon = model%preccon
            corrected%precsno = model%precsno
          endwhere

        endif

      end do
      
!     Free local memory and return
!     ============================

      if (associated(mask)) deallocate(mask)

      applyMask = 0

      end function applyMask

!******************************************************************************
      integer function maskdata(LFOdata,mask)
!******************************************************************************

      implicit none

      type (GenericDataType), intent(in) :: mask
      type (LFODataType), dimension(:), intent(inout) :: LFOdata

      integer :: hour
      type (LFODataType) :: precip

      do hour = 1, size(LFOdata)

        precip = LFOdata(hour)

        where (mask%data .eq. mask%grid%undef)

          precip%precls = precip%grid%undef
          precip%preccon = precip%grid%undef
          precip%precsno = precip%grid%undef

        endwhere

      end do

      maskdata = 0

      end function maskdata

!******************************************************************************
      integer function getArgs(args)
!******************************************************************************
! English Name: Get Arguments
! -------------
!
! Purpose: Retrieves command-line arguments for the CPCU Corrector interface.
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
! args       UserInterfaceType      OUT  returned data structure containing
!                                        the command-line arguments.
!
! getArgs              integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception issued
!
!                                        Thrown Exceptions:
!
!                                        1: incorrect number of arguments
!                                        2: invalid date format
!                                        3: error reading land forcing data
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
      logical :: scale_cpcu, scale_lfo
      logical :: exclude_africa, latitude_tapering, maskout
      character (len=1024) :: in_file, out_file, cpcu_file, cpcu_scale_file, &
                              lfo_scale_file, eod_file, mask_file

      namelist /corrector/ in_file, out_file, cpcu_file, cpcu_scale_file, &
                           lfo_scale_file, eod_file, mask_file, &
                           exclude_africa, latitude_tapering, maskout, &
                           exchange_dlon, exchange_dlat, scale_cpcu, scale_lfo

      rc = 0
      getArgs = -1

!     Initialize
!     ==========

      in_file           = ''
      out_file          = ''
      cpcu_file         = ''
      cpcu_scale_file   = ''
      lfo_scale_file    = ''
      eod_file          = ''
      mask_file         = ''
      exclude_africa    = .false.
      latitude_tapering = .false.     
      maskout           = .false.
      scale_cpcu        = .false.
      scale_lfo         = .false.
      exchange_dlon     = 0.0
      exchange_dlat     = 0.0

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
      if (isError(this,3,rc) .ne. 0) return

      read(5,nml=corrector,iostat=rc)
      if (isError(this,4,rc) .ne. 0) return

      close (unit=5)

!     Save configuration (perform sanity check)
!     =========================================

      args%in_file           = in_file
      args%out_file          = out_file
      args%cpcu_file         = cpcu_file
      args%scale_cpcu        = scale_cpcu
      args%cpcu_scale_file   = cpcu_scale_file
      args%scale_lfo         = scale_lfo
      args%lfo_scale_file    = lfo_scale_file
      args%eod_file          = eod_file
      args%mask_file         = mask_file
      args%exclude_africa    = exclude_africa
      args%latitude_tapering = latitude_tapering
      args%maskout           = maskout
      args%exchange_dlon     = exchange_dlon
      args%exchange_dlat     = exchange_dlat

      if (len_trim(in_file) .eq. 0)   rc = isError(this,5,-1,arg='in_file')
      if (len_trim(out_file) .eq. 0)  rc = isError(this,5,-1,arg='out_file')
      if (len_trim(cpcu_file) .eq. 0) rc = isError(this,5,-1,arg='cpcu_file')
      if (len_trim(eod_file) .eq. 0)  rc = isError(this,5,-1,arg='eod_file')

      if (len_trim(mask_file) .eq. 0 .and. exclude_africa) &
                                      rc = isError(this,5,-1,arg='mask_file')

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

!       correctPrecipError Handle
!       =========================

        case (correctPrecipErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'correctPrecip (error#',code,'): see usage'

          case (2)

            print *, 'correctPrecip (error#',code,'): error reading CPCU data'

          case (3)

            print *, 'correctPrecip (error#',code,'): error reading EOD data'

          case (4)

            print *, 'correctPrecip (error#',code,'): error reading LFO data'

          case (5)

            print *, 'correctPrecip (error#',code,'): error retrieving CPCU data'

          case (6)

            print *, 'correctPrecip (error#',code,'): error retrieving LFO data'

          case (7)

            print *, 'correctPrecip (error#',code,'): error allocating memory'

          case (8)

            print *, 'correctPrecip (error#',code,'): error applying', &
                     ' correction factors'

          case (9)

            print *, 'correctPrecip (error#',code,'): error applying', &
                     ' residual factors'

          case (10)

            print *, 'correctPrecip (error#',code,'): error writing to output'

          case (11)

            print *, 'correctPrecip (error#',code,'): error applying CPCU ', &
                     'scaling factors'

          case (12)

            print *, 'correctPrecip (error#',code,'): error applying LFO ', &
                     'scaling factors'

          case default

            print *, 'correctPrecip (error#',code,'): unknown error'

        end select

        case (ComputeDailyPrecipErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'computeDailyPrecip (error#',code,'): ', &
                     'error allocating memory'

          case (2)

            print *, 'computeDailyPrecip (error#',code,'): ', &
                     'grid interpolation error'

          case default

            print *, 'computeDailyPrecip (error#',code,'): unknown error'

        end select

        case (ApplyCorrectionErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'applyCorrection (error#',code,'): ', &
                     'error allocating memory'

          case (2)

            print *, 'applyCorrection (error#',code,'): ', &
                     'grid interpolation error'

          case default

            print *, 'applyCorrection (error#',code,'): unknown error'

        end select

        case (ApplyResidualErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'applyResidual (error#',code,'): ', &
                     'error allocating memory'

          case (2)

            print *, 'applyResidual (error#',code,'): ', &
                     'grid interpolation error'

          case default

            print *, 'applyResidual (error#',code,'): unknown error'

        end select

        case (ApplyMaskErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'applyMask (error#',code,'): ', &
                     'error allocating memory'

          case (2)

            print *, 'applyMask (error#',code,'): ', &
                     'unable to access masking file: "',trim(arg),'"'
          case (3)

            print *, 'applyMask (error#',code,'): ', &
                     'error reading masking file: "',trim(arg),'"'

          case default

            print *, 'applyMask (error#',code,'): unknown error'

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

      end module CPCU_A_CorrectorMod
