      module RegridMethodMod

      use TypesMod
      use GridMod, GridError=>errorHandler

      private
      public :: gridMap
      public :: setExchangeParams
      public :: nearestNeighbor
      public :: exchangeGrid
      public :: errorHandler

      integer, parameter :: NearestNeighborErrorHandle = 1
      integer, parameter :: GridMapErrorHandle = 2
      integer, parameter :: ExchangeGridErrorHandle = 3
      integer, parameter :: SetExchangeGridErrorHandle = 4

!     real :: EXCHANGE_DLON = 1.0/16.0
!     real :: EXCHANGE_DLAT = 1.0/8.0
      real :: EXCHANGE_DLON = 1.0/8.0
      real :: EXCHANGE_DLAT = 1.0/4.0

      integer :: errorCode = 0

      contains

!******************************************************************************
      integer function nearestNeighbor(inData,outData)
!******************************************************************************
! English Name: Nearest Neighbor
! -------------
!
! Purpose: Regrids data using a nearest neighbor method.
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
! inData       GenericDataType       IN  data to be regridded.
!
! outData      GenericDataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined in this
!                                        data structure as input.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/09/2014      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (GenericDataType), intent(in) :: inData
      type (GenericDataType), intent(inout) :: outData

!     Local Variables
!     ---------------

      integer :: rc
      real :: lat,lon
      integer :: i1,i2,j1,j2
      integer :: nx1,nx2,ny1,ny2
      real :: dx1,dx2,xStart1,xStart2
      real :: dy1,dy2,yStart1,yStart2

      integer :: this = NearestNeighborErrorHandle

      rc = 0
      nearestNeighbor = -1

      if (.not. isValidGrid(inData%grid)) rc = 1
      if (.not. isValidGrid(outData%grid)) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      nx1     = inData%grid%nx
      ny1     = inData%grid%ny
      dx1     = inData%grid%dx
      dy1     = inData%grid%dy
      xStart1 = inData%grid%xStart
      yStart1 = inData%grid%yStart

      nx2     = outData%grid%nx
      ny2     = outData%grid%ny
      dx2     = outData%grid%dx
      dy2     = outData%grid%dy
      xStart2 = outData%grid%xStart
      yStart2 = outData%grid%yStart

      outdata%data = outData%grid%undef

!     Perform nearest neighbor data transfer.
!     =======================================

      do j2 = 1,ny2

        lat = yStart2 + (j2 - 1) * dy2

        j1 = nint( (lat - yStart1) / dy1 + 1.0)
        if (j1 .lt.   1) j1 = 1
        if (j1 .gt. ny1) j1 = ny1

        do i2 = 1,nx2

          lon = xStart2 + (i2 - 1) * dx2
          if (lon .lt. xStart1) lon = lon + 360.0

          i1 = nint( (lon - xStart1) / dx1 + 1.0)
          if (i1 .lt. 1) i1 = i1 + nx1
          if (i1 .gt. nx1) i1 = i1 - nx1

          if (inData%data(i1,j1) .eq. inData%grid%undef) cycle

          outData%data(i2,j2) = inData%data(i1,j1)

        end do

      end do

      nearestNeighbor = 0

      end function nearestNeighbor

!******************************************************************************
      integer function exchangeGrid(inData,outData)
!******************************************************************************
! English Name: Exchange Grid
! -------------
!
! Purpose: Regrids data using an intermediate exchange grid selected to conserve
! -------- mass.
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
! inData       GenericDataType       IN  data to be regridded.
!
! outData      GenericDataType    INOUT  regridded output data. The output grid
!                                        parameters must be defined in this
!                                        data structure as input.
!
! exchangeGrid    GridInfoType       IN  exchange grid parameters. See
!                                        setExchangeGrid()
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           11/26/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (GenericDataType), intent(in) :: inData
      type (GenericDataType), intent(inout) :: outData

!     Local Variables
!     ---------------

      integer :: rc
      integer :: nx,ny
      integer :: N,N_min
      integer :: i,j,i_fg,j_fg
      integer :: f_x_lon,f_x_lat
      integer :: t_x_lon,t_x_lat
      integer :: this = ExchangeGridErrorHandle

      type (GridInfoType) :: fg,tg,xg

      real :: fg_lat,fg_lon
      real :: tg_lat,tg_lon
      real, pointer, dimension(:,:), save :: fgData => null()
      real, pointer, dimension(:,:), save :: tgData => null()
      real, pointer, dimension(:,:), save :: xgData => null()

      type (GenericDataType), save :: exchangeData
      integer, pointer, dimension(:,:), save :: N_tg => null()

!     Initialize
!     ==========

      exchangeGrid = -1

      fg = inData%grid
      tg = outData%grid

      fgData => inData%data
      tgData => outData%data

!     Set up exchange grid
!     ====================

      rc = setExchangeGrid(fg,tg,exchangeData%grid)
      if (isError(this,1,rc) .ne. 0) return

      if (associated(N_tg)) deallocate(N_tg)
      allocate(N_tg(tg%nx,tg%ny),stat=rc)
      if (isError(this,2,rc) .ne. 0) return

      xg = exchangeData%grid
      if (associated(exchangeData%data)) deallocate(exchangeData%data)
      allocate(exchangeData%data(xg%nx,xg%ny),stat=rc)
      if (isError(this,2,rc) .ne. 0) return
      xgData => exchangeData%data

!     Disaggregate data onto exchange grid
!     ====================================

      xgData = fg%undef

      f_x_lon = nint(fg%dx / xg%dx)
      f_x_lat = nint(fg%dy / xg%dy)

      do j = fg%j1,fg%j2

        j_fg   = min(max(1,j),fg%ny)

        fg_lat = (j - 1) * fg%dy + fg%yStart
        xg%j1  = nint( (fg_lat - xg%yStart) / xg%dy + 1.0)
        xg%j2  = xg%j1 + f_x_lat - 1

        do i = fg%i1,fg%i2

          i_fg = i
          if (i_fg .le. 0) i_fg = i_fg + fg%nx
          if (i_fg .gt. fg%nx) i_fg = i_fg - fg%nx

          fg_lon = (i - 1) * fg%dx + fg%xStart
          xg%i1 = nint( (fg_lon - xg%xStart) / xg%dx + 1.0)
          xg%i2 = xg%i1 + f_x_lon - 1

          xgData(xg%i1:xg%i2,xg%j1:xg%j2) = fgData(i_fg,j_fg)

        end do

      end do

!     Aggregate data back onto destination grid
!     =========================================

      tgData = 0.0
      N_tg   = 0

      t_x_lon = nint(tg%dx / xg%dx)
      t_x_lat = nint(tg%dy / xg%dy)

      do j = 1,tg%ny

        tg_lat = (j - 1) * tg%dy + tg%yStart
        xg%j1  = nint( (tg_lat - xg%yStart) / xg%dy + 1.0)
        xg%j2  = xg%j1 + t_x_lat - 1

        do i = 1,tg%nx

          tg_lon = (i - 1) * tg%dx + tg%xStart
          xg%i1 = nint( (tg_lon - xg%xStart) / xg%dx + 1.0)
          xg%i2 = xg%i1 + t_x_lon - 1

          tgData(i,j) = sum(xgData(xg%i1:xg%i2,xg%j1:xg%j2), &
                        mask = xgData(xg%i1:xg%i2,xg%j1:xg%j2) .ne. fg%undef)

          N           = count(xgData(xg%i1:xg%i2,xg%j1:xg%j2) .ne. fg%undef)

          N_tg(i,j)   = N_tg(i,j) + N

        end do

      end do

!     Normalize: Set points not visited to undefined.
!     Also, enforce minimum requirements for sampling.
!     ===============================================

      N_min = t_x_lon * t_x_lat

      where (N_tg .ge. N_min)
        outData%data = tgData / N_tg
      elsewhere
        outData%data = tg%undef
      endwhere

!     Clean up and exit
!     =================

      if (associated(N_tg)) deallocate(N_tg)
      if (associated(exchangeData%data)) deallocate(exchangeData%data)

      exchangeGrid = 0

      end function exchangeGrid

!******************************************************************************
      integer function setExchangeParams(dlon,dlat)
!******************************************************************************

      real, intent(in) :: dlon,dlat

      EXCHANGE_DLON = dlon
      EXCHANGE_DLAT = dlat

      setExchangeParams = 0

      end function setExchangeParams

!******************************************************************************
      integer function setExchangeGrid(fg,tg,xg)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GridInfoType), intent(inout) :: fg
      type (GridInfoType), intent(inout) :: tg
      type (GridInfoType), intent(out) :: xg

!     Local Variables
!     ---------------

      integer :: rc
      integer :: ipts
      real :: fglon1,fglon2
      real :: tglon1,tglon2
      real :: fglat1,fglat2
      real :: tglat1,tglat2
      real :: minlon,maxlon
      real :: minlat,maxlat

      integer :: this = SetExchangeGridErrorHandle

      rc = 0

!     Check grid dimensions
!     =====================

      if (.not. isValidGrid(fg)) rc = 1
      if (.not. isValidGrid(tg)) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      if (EXCHANGE_DLON .le. 0.0) rc = 1
      if (EXCHANGE_DLAT .le. 0.0) rc = 1
      if (isError(this,2,rc) .ne. 0) return

!     Define the edge of the grid cells.
!     Input grids must specify the starting
!     center of the grid cells.
!     =====================================

      fg%xStart = fg%xStart - fg%dx / 2.0
      tg%xStart = tg%xStart - tg%dx / 2.0
      fg%yStart = fg%yStart - fg%dy / 2.0
      tg%yStart = tg%yStart - tg%dy / 2.0

      fg%j1 = 1; fg%j2 = fg%ny
      fg%i1 = 1; fg%i2 = fg%nx

!     Define the boundaries of the exchange
!     grid. Note: grids are expected to be 
!     periodic in longitude (i.e. 360 degrees).
!     =========================================

      fglon1 = fg%xStart
      fglon2 = fg%xStart + fg%nx * fg%dx

      tglon1 = tg%xStart
      tglon2 = tg%xStart + tg%nx * tg%dx

      fglat1 = fg%yStart
      fglat2 = fg%yStart + fg%ny * fg%dy

      tglat1 = tg%yStart
      tglat2 = tg%yStart + tg%ny * tg%dy

!     Make sure that the "to grid" longitude edges
!     are contained within the "from grid" edges by
!     expanding the "from grid" as needed.
!     =============================================
     
      if (fglon1 .ge. tglon1) then

        ipts = ceiling( (fglon1 - tglon1) / fg%dx )
        fglon1 = fglon1 - (ipts * fg%dx)
        fg%i1  = fg%i1 - ipts

      endif

      if (fglon2 .le. tglon2) then

        ipts = ceiling( (tglon2 - fglon2) / fg%dx )
        fglon2 = fglon2 + (ipts * fg%dx)
        fg%i2  = fg%i2 + ipts

      endif

!     Set the exchange grid parameters.
!     =================================

      minlon = min(fglon1,tglon1)
      maxlon = max(fglon2,tglon2)
      minlat = min(fglat1,tglat1)
      maxlat = max(fglat2,tglat2)

      xg%dx     = EXCHANGE_DLON
      xg%dy     = EXCHANGE_DLAT
      xg%xStart = minlon
      xg%yStart = minlat
      xg%nx     = nint( (maxlon - minlon) / xg%dx)
      xg%ny     = nint( (maxlat - minlat) / xg%dy)

      print *, 'Exchange grid: ',xg%dx,xg%dy,xg%xStart,xg%yStart, &
                           xg%nx,xg%ny,minlat,maxlat,minlon,maxlon

      setExchangeGrid = 0

      end function setExchangeGrid

!******************************************************************************
      integer function gridMap(grid,lat1,lat2,lon1,lon2, &
                                    ilat1,ilat2,ilon1,ilon2, &
                                    elat1,elat2,elon1,elon2,reset)
!******************************************************************************
! English Name: Grid Map
! -------------
!
! Purpose: Maps latitude and longitude to the nearest grid point
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
! grid            GridInfoType    INOUT  on input, information defining the
!                                        grid: xstart,ystart,dx,dy,nx,ny.
!                                        On output, mapped grid indices
!                                        corresponding to the specified
!                                        lat/lon(s): i1,i2,j1,j2
!
! lat1                    real   OPT,IN  nearest southernmost latitude (j1) 
! lat2                    real   OPT,IN  nearest northernmost latitude (j2) 
! lon1                    real   OPT,IN  nearest westernmost longitude (i1)
! lon2                    real   OPT,IN  nearest easternmost longitude (i2)
! ilat1                   real   OPT,IN  inclusive southernmost latitude (j1) 
! ilat2                   real   OPT,IN  inclusive northernmost latitude (j2) 
! ilon1                   real   OPT,IN  inclusive westernmost longitude (i1)
! ilon2                   real   OPT,IN  inclusive easternmost longitude (i2)
! elat1                   real   OPT,IN  exclusive southernmost latitude (j1) 
! elat2                   real   OPT,IN  exclusive northernmost latitude (j2) 
! elon1                   real   OPT,IN  exclusive westernmost longitude (i1)
! elon2                   real   OPT,IN  exclusive easternmost longitude (i2)
!
! reset                logical   OPT,IN  reset flag. If true, grid indices are
!                                        reset to span the full dimensions:
!                                        [i1,i2] = [1,nx], [j1,j2] = [1,ny]
!
! gridMap              integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: invalid grid parameters
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           11/14/2012      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (GridInfoType), intent(INOUT) :: grid
      real, optional, intent(in) :: lat1,ilat1,elat1
      real, optional, intent(in) :: lat2,ilat2,elat2
      real, optional, intent(in) :: lon1,ilon1,elon1
      real, optional, intent(in) :: lon2,ilon2,elon2
      logical, optional, intent(in) :: reset

!     Local Variables
!     ---------------

      real :: x,y
      real :: lon
      integer :: rc
      integer :: this = GridMapErrorHandle

      gridMap = -1

!     Check for valid grid dimensions
!     ===============================

      rc = 0
      if (grid%nx .le. 0) rc = 1
      if (grid%ny .le. 0) rc = 1
      if (isError(this,1,rc) .ne. 0) return

!     Reset Option
!     ============

      if (present(reset)) then

        if (reset) then

          grid%i1 = 1; grid%i2 = grid%nx
          grid%j1 = 1; grid%j2 = grid%ny

        endif

      endif

!     Lat1 Option (nearest south latitude)
!     ===========

      if (present(lat1)) then 
        grid%j1 = nint( (lat1 - grid%yStart) / grid%dy + 1.0)
      endif

!     iLat1 Option (inclusive south latitude)
!     ============

      if (present(ilat1)) then

        y = (ilat1 - grid%yStart) / grid%dy + 1.0
        grid%j1 = ceiling(y)

      endif

!     eLat1 Option (exclusive south latitude)
!     ============

      if (present(elat1)) then

        y = (elat1 - grid%yStart) / grid%dy + 1.0
        grid%j1 = ceiling(y)
        if (mod(y,1.0) .eq. 0.0) grid%j1 = grid%j1 + 1

      endif

!     Lat2 Option (nearest north latitude)
!     ===========

      if (present(lat2)) then
        grid%j2 = nint( (lat2 - grid%yStart) / grid%dy + 1.0)
      endif

!     iLat2 Option (inclusive north latitude)
!     ============

      if (present(ilat2)) then

        y = (ilat2 - grid%yStart) / grid%dy + 1.0
        grid%j2 = floor(y)

      endif

!     eLat2 Option (exclusive north latitude)
!     ============

      if (present(elat2)) then

        y = (elat2 - grid%yStart) / grid%dy + 1.0
        grid%j2 = floor(y)
        if (mod(y,1.0) .eq. 0.0) grid%j2 = grid%j2 - 1

      endif

!     Lon1 Option (nearest west longitude)
!     ===========

      if (present(lon1)) then

        lon = lon1
        if (lon .lt. grid%xStart) lon = lon + 360.0

        grid%i1 = nint( (lon - grid%xStart) / grid%dx + 1.0)

      endif

!     iLon1 Option (inclusive west longitude)
!     ============

      if (present(ilon1)) then

        lon = ilon1
        if (lon .lt. grid%xStart) lon = lon + 360.0

        x = (lon - grid%xStart) / grid%dx + 1.0
        grid%i1 = ceiling(x)

      endif

!     eLon1 Option (exclusive west longitude)
!     ============

      if (present(elon1)) then

        lon = elon1
        if (lon .lt. grid%xStart) lon = lon + 360.0
        
        x = (lon - grid%xStart) / grid%dx + 1.0
        grid%i1 = ceiling(x)
        if (mod(x,1.0) .eq. 0.0) grid%i1 = grid%i1 + 1

      endif

!     Lon2 Option (nearest east longitude)
!     ===========

      if (present(lon2)) then

        lon = lon2
        if (lon .lt. grid%xStart) lon = lon + 360.0

        grid%i2 = nint( (lon - grid%xStart) / grid%dx + 1.0)

      endif

!     iLon2 Option (inclusive east longitude)
!     ============

      if (present(ilon2)) then

        lon = ilon2
        if (lon .lt. grid%xStart) lon = lon + 360.0
        
        x = (lon - grid%xStart) / grid%dx + 1.0
        grid%i2 = floor(x)

      endif

!     eLon2 Option (exclusive east longitude)
!     ============

      if (present(elon2)) then

        lon = elon2
        if (lon .lt. grid%xStart) lon = lon + 360.0
         
        x = (lon - grid%xStart) / grid%dx + 1.0
        grid%i2 = floor(x)
        if (mod(x,1.0) .eq. 0.0) grid%i2 = grid%i2 - 1

      endif

!     Check bounds of indices
!     =======================

      if (grid%j1 .lt. 1) grid%j1 = 1
      if (grid%j1 .gt. grid%ny)  grid%j1 = grid%ny

      if (grid%j2 .lt. 1) grid%j2 = 1
      if (grid%j2 .gt. grid%ny)  grid%j2 = grid%ny

      if (grid%i1 .lt. 1) grid%i1 = grid%i1 + grid%nx
      if (grid%i1 .gt. grid%nx) grid%i1 = grid%i1 - grid%nx

      if (grid%i2 .lt. 1) grid%i2 = grid%i2 + grid%nx
      if (grid%i2 .gt. grid%nx) grid%i2 = grid%i2 - grid%nx

      gridMap = 0

      end function gridMap

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

!       Nearest Neighbor Error Handle
!       =============================

        case (NearestNeighborErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'nearestNeighbor (error#',code,'): invalid grid dimensions'

          case default

            print *, 'nearestNeighbor (error#',code,'): unknown error'

        end select

!       Grid Map Error Handle
!       =====================

        case (GridMapErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'gridMap (error#',code,'): invalid grid dimensions'

          case default

            print *, 'gridMap (error#',code,'): unknown error'

        end select

!       Set Exchange Grid Error Handle
!       ==============================

        case (SetExchangeGridErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'setExchangeGrid (error#',code,'): invalid grid dimensions'

          case (2)

            print *, 'setExchangeGrid (error#',code,'): exchange grid', &
                     ' parameters are undefined'

          case default

            print *, 'setExchangeGrid (error#',code,'): unknown error'

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

      end module RegridMethodMod
