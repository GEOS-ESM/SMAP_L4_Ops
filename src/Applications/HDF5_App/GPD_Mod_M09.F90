!******************************************************************************
      module GPD_Mod
!******************************************************************************
! English Name: Grid Parameter Definition Module
! -------------
!
! Purpose: Provides methods for calculating various grids (defined by NSIDC
! -------- grid parameter files) and retrieving navigational information.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The current implementation only contains options for the EASE 
! ------    version-2 "global" cyclindrical grids. Other options will be
!           added.
!
!        2. A namelist can be optionally read to re-define the default grid.
!
!        3. This module implements the inverse calculation (grid indices to 
!           lat/lons). The forward problem is solved by using large arrays that
!           retain the index associate with each lat/lon out to a specified
!           number of digits. For example, longitude 145.678 is located in a
!           privately stored array: col_index(145678). This paradigm is a 
!           quick referencing method that aligns with the idea of "setting" a
!           grid once (i.e. all calculations are performed once). This
!           implementation is subject to change.
!
!        4. "GPDsetGrid()" must be called before accessing any of the methods
!           that return navigational information.
!
!        5. "GPDsetGrid()" must be called after redefining grid parameters.
!
!        6. The codes are currently implemented to return the centerpoint of the
!           grid boxes.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!    
! GPDread()           function      PUB  Reads a GPD namelist containing the 
!                                        parameters that define a grid (see
!                                        note-5).
!
! GPDgetIndex()       function      PUB  Maps lat/lon locations to grid indices
!                                        for the defined grid.
!
! GPDgetRowIndex()    function      PUB  Maps latitudes to grid indices for 
!                                        the defined grid.
!
! GPDgetColIndex()    function      PUB  Maps longitudes to grid indices for 
!                                        the defined grid.
!
! GPDgetLatitude()    function      PUB  Returns a copy of the grid latitudes.
!
! GPDgetLongitude()   function      PUB  Returns a copy of the grid latitudes.
!
! GPDsetGrid()        function      PUB  Performs all grid calculations for
!                                        retrieving navigational information
!                                        (see note-4).
!
! GPDgetGrid()        function      PUB  Retrieves a copy of the current grid
!                                        settings.
!
! errorHandler()      function      PUB  Error handler for this module. Recast
!                                        to the desired name on the "use"
!                                        statement line:
!
!                                        use GPD_Mod, GPDerror=>errorHandler
!
!                                        The status of all function calls can
!                                        be queried with the following
!                                        command:
!
!                                        if (GPDerror() .ne. 0) stop
!
! GridParamDefType     derived      PUB  Data structure containing grid
!                                        parameter definitions. The contained
!                                        elements are defined in the NSIDC
!                                        grid paramter files. The "name"
!                                        parameter is a unique identifier for
!                                        the desired grid (not currently
!                                        used for determining grid mapping
!                                        routines).
!
!                                        default: 9 km Global EASE-Grid-2.0
!
! &grid               namelist      PUB  namelist read by "GPDread()". The
!                                        namelist parameters are defined by
!                                        the elements within the derived type,
!                                        "GridParamDefType".
! 
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/26/2013      J.Ardizzone  created.
!******************************************************************************

      private

!     Public Interface
!     ----------------

      public :: GPDread
      public :: GPDsetGrid
      public :: GPDgetGrid
      public :: GPDgetLatitude
      public :: GPDgetLongitude
      public :: GPDgetRowIndex
      public :: GPDgetColIndex
      public :: GPDgetIndex
      public :: errorHandler

      public :: GridParamDefType

!     Constants
!     ---------

      real, parameter :: UNDEF = 1.0e+15

      integer, parameter :: IUNIT = 20

      integer, parameter :: LON_PRECISION = 3
      integer, parameter :: LAT_PRECISION = 3

      integer, parameter :: LON_SCALE_FACTOR = 10**LON_PRECISION
      integer, parameter :: LAT_SCALE_FACTOR = 10**LAT_PRECISION

      integer, parameter :: MIN_SCALED_LON =   0 * LON_SCALE_FACTOR
      integer, parameter :: MAX_SCALED_LON = 360 * LON_SCALE_FACTOR

      integer, parameter :: MIN_SCALED_LAT = -90 * LAT_SCALE_FACTOR
      integer, parameter :: MAX_SCALED_LAT =  90 * LAT_SCALE_FACTOR

      double precision, parameter :: PI = 3.1415926535897932

!     Error Handles
!     -------------

      integer, parameter :: ReadErrorHandle         = 1
      integer, parameter :: SetGridErrorHandle      = 2
      integer, parameter :: GetRowIndexErrorHandle  = 3
      integer, parameter :: GetColIndexErrorHandle  = 4
      integer, parameter :: GetLatitudeErrorHandle  = 5
      integer, parameter :: GetLongitudeErrorHandle = 6
      integer, parameter :: SetGridEASE2ErrorHandle = 7
      integer, parameter :: AllocateErrorHandle     = 8

!     Type: Grid Parameter Definition
!     Default: EASE2_M09km
!     -------------------------------

      type GridParamDefType

        character (len=20) :: name = 'EASE2_M09km         '

        integer :: grid_width  = 3856
        integer :: grid_height = 1624

        double precision :: map_scale             = 9008.055210146
        double precision :: map_eccentricity      = 0.081819190843
        double precision :: map_equatorial_radius = 6378137.0

        double precision :: map_reference_latitude        =  0.0
        double precision :: map_reference_longitude       =  0.0
        double precision :: map_second_reference_latitude = 30.0

        double precision :: map_southern_bound =  -90.0
        double precision :: map_northern_bound =   90.0
        double precision :: map_western_bound  = -180.0
        double precision :: map_eastern_bound  =  180.0

        double precision :: map_latitude_interval  = 15.0
        double precision :: map_longitude_interval = 30.0

      end type GridParamDefType

!     Type: Grid Parameter Data
!     -------------------------

      type GridParamDataType

        real, dimension(:), pointer :: latitude
        real, dimension(:), pointer :: longitude

        integer, dimension(:), pointer :: col_index
        integer, dimension(:), pointer :: row_index

      end type GridParamDataType

!     Global Variables
!     ----------------

      integer :: EXCEPTION
      type (GridParamDefType) :: GPD
      type (GridParamDataType) :: GPDD

      contains

!******************************************************************************
      integer function GPDread(filename)
!******************************************************************************
! English Name: Read Grid Parameter Definition (GPD)
! -------------
!
! Purpose: Reads a GPD namelist containing the parameters that define a grid.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. See module prolog for a description of namelist parameters
! ------    Input parameters have the same names as the elements of
!           GridParamDefType.
!
!        2. No sanity checks performed on input parameters.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  Namelist file name to be read.
!
! grid                namelist       IN  Namelist parameters read from file.
!                                        See note-1.
!
! GPDread              integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error accessing namelist
!                                        2: error reading namelist
!
! GPD         GridParamDefType      OUT  Data structure containing the
!                                        retrieved namelist grid parameters.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/19/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      character (len=*) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = ReadErrorHandle

      character (len=20) :: name

      integer :: grid_width
      integer :: grid_height

      double precision :: map_scale
      double precision :: map_equatorial_radius
      double precision :: map_eccentricity

      double precision :: map_reference_latitude
      double precision :: map_reference_longitude
      double precision :: map_second_reference_latitude

      double precision :: map_southern_bound
      double precision :: map_northern_bound
      double precision :: map_western_bound
      double precision :: map_eastern_bound
      double precision :: map_latitude_interval
      double precision :: map_longitude_interval

      namelist/grid/ name, &
                     grid_width,grid_height, &
                     map_scale,map_equatorial_radius,map_eccentricity, &
                     map_reference_latitude,map_reference_longitude, &
                     map_second_reference_latitude,map_southern_bound, &
                     map_northern_bound,map_western_bound,map_eastern_bound, &
                     map_latitude_interval,map_longitude_interval

      rc = 0
      GPDread = -1

!     Read grid definition from namelist
!     ==================================

      open (unit=IUNIT,file=filename,status='old',iostat=rc)
      if (isError(this,1,rc,arg=filename) .ne. 0) return

      read(IUNIT,nml=grid,iostat=rc)
      if (isError(this,2,rc,arg=filename) .ne. 0) return

      close(unit=IUNIT)

!     Save the grid parameters
!     ========================

      GPD%name = name

      GPD%grid_width  = grid_width
      GPD%grid_height = grid_height

      GPD%map_scale                     = map_scale
      GPD%map_equatorial_radius         = map_equatorial_radius
      GPD%map_eccentricity              = map_eccentricity
      GPD%map_reference_latitude        = map_reference_latitude
      GPD%map_reference_longitude       = map_reference_longitude
      GPD%map_second_reference_latitude = map_second_reference_latitude

      GPD%map_southern_bound     = map_southern_bound
      GPD%map_northern_bound     = map_northern_bound
      GPD%map_western_bound      = map_western_bound
      GPD%map_eastern_bound      = map_eastern_bound
      GPD%map_latitude_interval  = map_latitude_interval
      GPD%map_longitude_interval = map_longitude_interval

      GPDread = 0

      end function GPDread

!******************************************************************************
      integer function GPDgetIndex(latitude,longitude,row_index,col_index)
!******************************************************************************
! English Name: Get Index
! -------------
!
! Purpose: Maps lat/lon locations to grid indices for the defined grid.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! latitude(:)             real       IN  array of latitudes to be mapped to
!                                        grid column indices.
!
! longitude(:)            real       IN  array of longtiudes to be mapped to
!                                        grid column indices.
!
! row_index(:)            real      OUT  array of grid row indices for each
!                                        of the input latitudes.
!
! col_index(:)            real      OUT  array of grid col indices for each
!                                        of the input longitudes.
!
! GPDgetIndex          integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions: none
!
! GPD        GridParamDefType        IN  data structure containing the grid
!                                        parameter definitions.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           03/26/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(:), intent(in) :: latitude
      real, dimension(:), intent(in) :: longitude
      real, dimension(:), intent(out) :: row_index
      real, dimension(:), intent(out) :: col_index

!     Local Variables
!     ---------------

      integer :: rc
      integer :: n,nloc
      integer :: cols,rows

      double precision :: r0,s0
      double precision :: lat,lon
      double precision :: sin_phi, sin_phi1, cos_phi1
      double precision :: q, qp, kz, a, e, e2, e4, e6
      double precision :: x,y,beta,phi,lam

      rc = 0
      GPDgetIndex = -1

!     Derive non-varying parameters
!     =============================

      rows = GPD%grid_height
      cols = GPD%grid_width

      a  = GPD%map_equatorial_radius

      e  = GPD%map_eccentricity
      e2 = e * e
      e4 = e2 * e2
      e6 = e4 * e2

      r0 = (cols - 1)/2.0
      s0 = (rows - 1)/2.0

      sin_phi1 = sin(GPD%map_second_reference_latitude*PI/180)
      cos_phi1 = cos(GPD%map_second_reference_latitude*PI/180)
      kz = cos_phi1/sqrt(1.0-e2*sin_phi1*sin_phi1)

      qp = 1.0-((1.0-e2)/(2.0*e) * log((1.0-e)/(1.0+e)))

!     Derive indices for each location
!     ================================

      nloc = size(latitude)

      do n = 1,nloc

        lat = latitude(n)

        lon = longitude(n) - GPD%map_reference_longitude
        if (lon .lt. -180.0) lon = lon + 360.0
        if (lon .gt.  180.0) lon = lon - 360.0

        lam = lon * PI / 180.0
        x = a*kz*lam

        col_index(n) = r0 + (x/GPD%map_scale)

        phi = lat * PI / 180.0
        sin_phi = sin(phi)

        q = (1.0-e2)*((sin_phi/(1.0-e2*sin_phi*sin_phi))-(1.0/(2.0*e)) &
                                     * log((1.0-e*sin_phi)/(1.0+e*sin_phi)))

        y = (a*q) / (2.0*kz)
        row_index(n) = s0 - (y/GPD%map_scale)

      end do

      GPDgetIndex = 0

      end function GPDgetIndex

!******************************************************************************
      integer function GPDgetRowIndex(latitude,row_index)
!******************************************************************************
! English Name: Get Row Index
! -------------
!
! Purpose: Maps latitudes to grid indices for the defined grid.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------ 2. This routine assumes that the specified latitudes are actual
!           grid coordinates with no offsets.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! latitude(:)             real       IN  array of latitudes to be mapped to
!                                        grid column indices (see note-2).
!
! row_index(:)         integer      OUT  array of grid row indices for each
!                                        of the input latitudes.
!
! GPDgetRowIndex       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: no grid defined
!                                        2: no grid index for one or more
!                                           latitudes
!
! GPDD       GridParamDataType       IN  data structure containing the grid
!                                        navigation data.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           03/20/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(:), intent(in) :: latitude
      integer, dimension(:), intent(out) :: row_index

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GetRowIndexErrorHandle

      rc = 0
      GPDgetRowIndex = -1

!     Make sure that the grid has been
!     defined.
!     ================================

      if (.not. associated(GPDD%row_index)) rc = -1
      if (isError(this,1,rc) .ne. 0) return

!     Map the latitudes to grid indices.
!     ==================================

      row_index = -1

      where (latitude .ge. -90.0 .and. latitude .le. 90) &
        row_index = GPDD%row_index(nint(latitude*LAT_SCALE_FACTOR))

!     Check for unmapped latitudes
!     ============================

      if (count(row_index .eq. -1) .ne. 0) rc = -1
      if (isError(this,2,rc) .ne. 0) return

      GPDgetRowIndex = 0

      end function GPDgetRowIndex

!******************************************************************************
      integer function GPDgetColIndex(longitude,col_index)
!******************************************************************************
! English Name: Get Column Index
! -------------
!
! Purpose: Maps longitudes to grid indices for the defined grid.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------ 2. This routine assumes that the specified longitudes are actual
!           grid coordinates with no offsets.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! longitude(:)            real       IN  array of longitudes to be mapped to
!                                        grid column indices (see note-2).
!
! col_index(:)         integer      OUT  array of grid column indices for each
!                                        of the input longitudes.
!
! GPDgetColIndex       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: no grid defined
!                                        2: no grid index for one or more
!                                           longitudes
!
! GPDD       GridParamDataType       IN  data structure containing the grid
!                                        navigation data.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/20/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(:), intent(in) :: longitude
      integer, dimension(:), intent(out) :: col_index

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GetColIndexErrorHandle

      rc = 0
      GPDgetColIndex = -1

!     Make sure that the grid has been
!     defined.
!     ================================

      if (.not. associated(GPDD%col_index)) rc = -1
      if (isError(this,1,rc) .ne. 0) return

!     Map the longitudes to grid indices.
!     ===================================

      col_index = -1

      where (longitude .lt. 0 .and. longitude .ge. -180) &
        col_index = GPDD%col_index(nint((longitude+360)*LON_SCALE_FACTOR))

      where (longitude .ge. 0 .and. longitude .le. 360) &
        col_index = GPDD%col_index(nint(longitude*LON_SCALE_FACTOR))

!     Check for unmapped longitudes
!     =============================

      if (count(col_index .eq. -1) .ne. 0) rc = -1
      if (isError(this,2,rc) .ne. 0) return

      GPDgetColIndex = 0

      end function GPDgetColIndex

!******************************************************************************
      integer function GPDgetLatitude(latitude)
!******************************************************************************
! English Name: Get latitude
! -------------
!
! Purpose: Returns a copy of the grid latitudes.
! --------
!
! Language: Fortran 90
! ---------
!
! See Also: GPDgetLongitude()
! -------- 
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! latitude(:)             real      OUT  array of grid latitudes.
!
! GPDgetLatitude       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: no grid defined
!
! GPDD       GridParamDataType       IN  data structure containing the grid
!                                        navigation data.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           03/20/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(:), intent(out) :: latitude

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GetLatitudeErrorHandle

      rc = 0
      GPDgetLatitude = -1

!     Make sure that the grid has been
!     defined.
!     ================================

      if (.not. associated(GPDD%latitude)) rc = -1
      if (isError(this,1,rc) .ne. 0) return

!     Copy the latitudes
!     ==================
      
      latitude = GPDD%latitude

      GPDgetLatitude = 0

      end function GPDgetLatitude

!******************************************************************************
      integer function GPDgetLongitude(longitude)
!******************************************************************************
! English Name: Get latitude
! -------------
!
! Purpose: Returns a copy of the grid longitudes.
! --------
!
! Language: Fortran 90
! ---------
!
! See Also: GPDgetLatitude()
! --------    
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! longitude(:)            real      OUT  array of grid longitudes
!
! GPDgetLongiude       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: no grid defined
!
! GPDD       GridParamDataType       IN  data structure containing the grid
!                                        navigation data.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           03/20/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(:), intent(out) :: longitude

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GetLongitudeErrorHandle

      rc = 0
      GPDgetLongitude = -1

!     Make sure that the grid has been
!     defined.
!     ================================

      if (.not. associated(GPDD%longitude)) rc = -1
      if (isError(this,1,rc) .ne. 0) return

!     Copy the longitudes
!     ===================

      longitude = GPDD%longitude

      GPDgetLongitude = 0

      end function GPDgetLongitude

!******************************************************************************
      integer function GPDsetGrid()
!******************************************************************************

      implicit none

      integer :: rc
      integer :: this = SetGridErrorHandle

      rc = 0
      GPDsetGrid = -1

      rc = GPDsetGridEASE2()
      if (isError(this,1,rc) .ne. 0) return

      GPDsetGrid = 0

      end function GPDsetGrid

!******************************************************************************
      integer function GPDgetGrid(grid)
!******************************************************************************

      type (GridParamDefType), intent(out) :: grid

      grid = GPD
      GPDgetGrid = 0

      end function GPDgetGrid

!******************************************************************************
      integer function GPDsetGridEASE2()
!******************************************************************************
! English Name: Set EASE-2 Grid
! -------------
!
! Purpose: Derives the navigational parameters for an EASE version-2 grid.
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
! GPDsetGridEASE2      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error allocating memory
!                                        2: error mapping latitude
!                                        3: error mapping longitude
!
! GPD         GridParamDefType       IN  Data structure containing the
!                                        parameters that define a grid.
!
! GPDD       GridParamDataType      OUT  Data structure containing the
!                                        allocated grids with defined
!                                        navigation data.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/20/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Local Variables
!     ---------------

      integer :: rc
      integer :: i,j
      integer :: address
      integer :: cols,rows
      integer :: this = SetGridEASE2ErrorHandle

      integer :: edge   = 1
      integer :: center = 2

      double precision :: r0,s0
      double precision :: sin_phi1, cos_phi1
      double precision :: qp, kz, a, e, e2, e4, e6
      double precision, dimension(2) :: col,row
      double precision, dimension(2) :: lat,lon
      double precision, dimension(2) :: x,y,beta,phi,lam

      rc = 0
      GPDsetGridEASE2 = -1

      rc = GPDallocate()
      if (isError(this,1,rc) .ne. 0) return
      
!     Derive non-varying parameters
!     =============================

      rows = GPD%grid_height
      cols = GPD%grid_width

      a  = GPD%map_equatorial_radius

      e  = GPD%map_eccentricity
      e2 = e * e
      e4 = e2 * e2
      e6 = e4 * e2

      r0 = (cols - 1)/2.0
      s0 = (rows - 1)/2.0

      sin_phi1 = sin(GPD%map_second_reference_latitude*PI/180)
      cos_phi1 = cos(GPD%map_second_reference_latitude*PI/180)
      kz = cos_phi1/sqrt(1.0-e2*sin_phi1*sin_phi1)

      qp = 1.0-((1.0-e2)/(2.0*e) * log((1.0-e)/(1.0+e)))

!     Calculate Latitudes
!     ===================

      do j = 0,rows-1

        row(edge)   = j - 0.5
        row(center) = j

        y = GPD%map_scale * (s0 - row)
        beta = asin((2.0*y*kz)/(a*qp))

        phi = beta+(((e2/3.0)+((31.0/180.0)*e4)+((517.0/5040.0)*e6)) &
                *sin(2.0*beta))+((((23.0/360.0)*e4)+((251.0/3780.0)*e6)) &
                    *sin(4.0*beta))+(((761.0/45360.0)*e6)*sin(6.0*beta))

        lat  = phi*180.0/PI

        GPDD%latitude(j+1) = lat(center)

        address = nint(lat(edge)*LAT_SCALE_FACTOR)
        if (address .lt. MIN_SCALED_LAT) rc = -1
        if (address .gt. MAX_SCALED_LAT) rc = -1
        if (GPDD%row_index(address) .ne. -1) rc = -1
        if (isError(this,2,rc,darg=lat(edge)) .ne. 0) return

        GPDD%row_index(address) = j 

      end do

!     Calculate Longitudes
!     ====================

      do i = 0,cols-1

        col(edge)   = i - 0.5
        col(center) = i

        x = GPD%map_scale * (col - r0)
        lam = x/(a*kz)

        lon = lam*180.0/PI + GPD%map_reference_longitude
        where(lon .lt. -180) lon = -180
       
        GPDD%longitude(i+1) = lon(center)

        if (lon(edge) .lt. 0) lon(edge) = lon(edge) + 360
        address = nint(lon(edge)*LON_SCALE_FACTOR)

        if (address .lt. MIN_SCALED_LON) rc = -1
        if (address .gt. MAX_SCALED_LON) rc = -1
        if (GPDD%col_index(address) .ne. -1) rc = -1
        if (isError(this,3,rc,darg=lon(edge)) .ne. 0) return

        GPDD%col_index(address) = i

     end do

!    Fill each grid box with the
!    edge index.
!    ===========================

     j = -1
     do address = MAX_SCALED_LAT,MIN_SCALED_LAT,-1
       if (GPDD%row_index(address) .ne. -1) j = GPDD%row_index(address)
       GPDD%row_index(address) = j
     end do

     i = -1
     do address = MIN_SCALED_LON,MAX_SCALED_LON
       if (GPDD%col_index(address) .ne. -1) i = GPDD%col_index(address)
       GPDD%col_index(address) = i
     end do

     where (GPDD%col_index .eq. -1) GPDD%col_index = i

!    Longitude 0 and 360 should yield
!    the same index if defined.
!    ================================

     GPDD%col_index(MAX_SCALED_LON) = GPDD%col_index(MIN_SCALED_LON)

     GPDsetGridEASE2 = 0

     end function GPDsetGridEASE2

!******************************************************************************
     integer function GPDallocate()
!******************************************************************************
! English Name: Allocate
! -------------
!
! Purpose: Allocates memory for storing navigational information.
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
! GPDallocate          integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: no grid defined or invalid 
!                                           dimensions
!                                        2: error allocating memory
!
! GPD         GridParamDefType       IN  Data structure containing the
!                                        parameters that define a grid.
!
! GPDD       GridParamDataType      OUT  Data structure containing the 
!                                        allocated grids
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/19/2013      J.Ardizzone  created.
!******************************************************************************

     implicit none

!    Local Variables
!    ---------------

     integer :: rc
     integer :: rows,cols
     integer :: this = AllocateErrorHandle

     rc = 0
     GPDallocate = -1

!    Deallocate previously allocated
!    memory.
!    ===============================

     if (associated(GPDD%latitude))   deallocate(GPDD%latitude)
     if (associated(GPDD%longitude))  deallocate(GPDD%longitude)
     if (associated(GPDD%row_index))  deallocate(GPDD%row_index)
     if (associated(GPDD%col_index))  deallocate(GPDD%col_index)

!    Perform Sanity Check
!    ====================

     rows = GPD%grid_height
     cols = GPD%grid_width

     if (rows .le. 0) rc = -1
     if (cols .le. 0) rc = -1
     if (isError(this,1,rc) .ne. 0) return

!    Allocate space for navigational
!    information. Note that row/col
!    lookup is optimized by scaling
!    lat/lons into unique integers.
!    The allocated dimension range
!    reflects this strategy.
!    ===============================

     allocate(GPDD%latitude(rows),stat=rc)
     if (isError(this,2,rc) .ne. 0) return
     allocate(GPDD%longitude(cols),stat=rc)
     if (isError(this,2,rc) .ne. 0) return
     allocate(GPDD%row_index(MIN_SCALED_LAT:MAX_SCALED_LAT),stat=rc)
     if (isError(this,2,rc) .ne. 0) return
     allocate(GPDD%col_index(MIN_SCALED_LON:MAX_SCALED_LON),stat=rc)
     if (isError(this,2,rc) .ne. 0) return

!    Initialize navigational vectors
!    ===============================

     GPDD%latitude  = UNDEF
     GPDD%longitude = UNDEF
     GPDD%row_index  = -1
     GPDD%col_index  = -1

     GPDallocate = 0

     end function GPDallocate

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,arg,iarg,farg,darg)
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
! arg                   string   OPT,IN  string argument
!
! iarg                 integer   OPT,IN  integer argument
!
! farg                    real   OPT,IN  real argument
!
! darg        double precision   OPT,IN  double precision argument
!
! isError              integer      OUT  function return value:
!
!                                        0: no error occurred (rc=0)
!                                       !0: error event code (errorEvent)
!
! EXCEPTION            integer      OUT exception code (set to "errorEvent")
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!            3/08/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: rc
      integer, intent(in) :: errorHandle
      integer, intent(in) :: errorEvent

      real, intent(in), optional :: farg
      integer, intent(in), optional :: iarg
      character(len=*), intent(in), optional :: arg
      double precision, intent(in), optional :: darg

!     Local Variables
!     ---------------

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

!       GPDread Error Handle
!       ====================

        case (ReadErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GPDread (error#',code,'): error accessing namelist: ', &
                                                       '"',trim(arg),'"'

          case (2)

            print *, 'GPDread (error#',code,'): error reading namelist: ', &
                                                       '"',trim(arg),'"'

          case default

            print *, 'GPDread (error#',code,'): unknown error'

        end select

!       GPDGetRowIndex Error Handle
!       ===========================

        case (GetRowIndexErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GPDgetRowIndex (error#',code,'): no grid defined'

          case (2)

            print *, 'GPDgetRowIndex (error#',code,'): no grid index for ', &
                                                       'one or more latitudes'

          case default

            print *, 'GPDgetRowIndex (error#',code,'): unknown error'

        end select

!       GPDGetColIndex Error Handle
!       ===========================

        case (GetColIndexErrorHandle)

        select case (errorEvent)

          case (1)
        
            print *, 'GPDgetColIndex (error#',code,'): no grid defined'
        
          case (2)

            print *, 'GPDgetColIndex (error#',code,'): no grid index for ', &
                                                       'one or more longitudes'

          case default

            print *, 'GPDgetColIndex (error#',code,'): unknown error'

        end select

!       GPDGetLatitude Error Handle
!       ===========================

        case (GetLatitudeErrorHandle)

        select case (errorEvent)

          case (1)
        
            print *, 'GPDgetLatitude (error#',code,'): no grid defined'
        
          case default

            print *, 'GPDgetLatitude (error#',code,'): unknown error'

        end select

!       GPDGetLongitude Error Handle
!       ============================

        case (GetLongitudeErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GPDgetLongitude (error#',code,'): no grid defined'

          case default

            print *, 'GPDgetLongitude (error#',code,'): unknown error'

        end select

!       GPDSetGrid Error Handle
!       =======================

        case (SetGridErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GPDsetGrid (error#',code,'): error setting grid'

          case default

            print *, 'GPDsetGrid (error#',code,'): unknown error'

        end select

!       GPDSetGridEASE2 Error Handle
!       ============================

        case (SetGridEASE2ErrorHandle)
        
        select case (errorEvent)
        
          case (1)
        
            print *, 'GPDsetGridEASE2 (error#',code,'): error allocating memory'

          case (2)

            print *, 'GPDsetGridEASE2 (error#',code,'): error mapping ', &
                                                          'latitude: ',darg

          case (3)

            print *, 'GPDsetGridEASE2 (error#',code,'): error mapping ', &
                                                          'longitude: ',darg
      
          case default
      
            print *, 'GPDsetGridEASE2 (error#',code,'): unknown error'

        end select

!       GPDallocate Error Handle
!       ========================

        case (AllocateErrorHandle)
        
        select case (errorEvent)
        
          case (1)
        
            print *, 'GPDallocate (error#',code,'): no grid defined or ', &
                                                         'invalid dimensions'
        
          case (2)
        
            print *, 'GPDallocate (error#',code,'): error allocating memory'
      
          case default
      
            print *, 'GPDallocate (error#',code,'): unknown error'

        end select

        case default

        print *, 'isError: no handle for event:'
        print *, 'handle = ',errorHandle
        print *, 'event  = ',errorEvent

      end select

      isError   = -1
      EXCEPTION = errorEvent

      end function isError

!******************************************************************************
      integer function errorHandler()
!******************************************************************************
!     Module Public Error Handler

      implicit none

      integer :: ilen

      errorHandler = EXCEPTION
      EXCEPTION = 0

      return

      end function errorHandler

      end module GPD_Mod
