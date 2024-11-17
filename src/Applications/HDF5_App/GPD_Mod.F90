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
!        3. "GPDsetGrid()" must be called before accessing any of the methods
!           that return navigational information.
!
!        4. "GPDsetGrid()" must be called after redefining grid parameters.
!
!        5. The codes are currently implemented to return the centerpoint of the
!           grid boxes.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!    
! GPDread()           function      PUB  Reads a GPD namelist containing the 
!                                        parameters that define a grid (see
!                                        note-4).
!
! GPDgetIndex()       function      PUB  Maps lat/lon locations to grid indices
!                                        for the defined grid.
!
! GPDgetLatitude()    function      PUB  Returns a copy of the grid latitudes.
!
! GPDgetLongitude()   function      PUB  Returns a copy of the grid latitudes.
!
! GPDgetYProj()       function      PUB  Returns a copy of the grid y-projection
!                                        coordinates.
!
! GPDgetXProj()       function      PUB  Returns a copy of the grid x-projection
!                                        coordinates.
!
! GPDsetGrid()        function      PUB  Performs all grid calculations for
!                                        retrieving navigational information
!                                        (see note-3).
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
!           04/24/2018      J.Ardizzone  added X and Y projection coordinates.
!******************************************************************************

      private

!     Public Interface
!     ----------------

      public :: GPDread
      public :: GPDgetdef
      public :: GPDsetGrid
      public :: GPDgetGrid
      public :: GPDgetLatitude
      public :: GPDgetLongitude
      public :: GPDgetXProj
      public :: GPDgetYProj
      public :: GPDgetIndex
      public :: errorHandler

      public :: GridParamDefType

!     Constants
!     ---------

      real, parameter :: UNDEF = 1.0e+15

      integer, parameter :: IUNIT = 20

      double precision, parameter :: PI = 3.1415926535897932

!     Error Handles
!     -------------

      integer, parameter :: ReadErrorHandle         = 1
      integer, parameter :: SetGridErrorHandle      = 2
      integer, parameter :: GetLatitudeErrorHandle  = 3
      integer, parameter :: GetLongitudeErrorHandle = 4
      integer, parameter :: SetGridEASE2ErrorHandle = 5
      integer, parameter :: AllocateErrorHandle     = 6
      integer, parameter :: GetYProjErrorHandle     = 7
      integer, parameter :: GetXProjErrorHandle     = 8

!     Type: Grid Parameter Definition
!     Default: EASE2_M09km
!     -------------------------------

      type GridParamDefType

        character (len=20) :: name = 'EASE2_M09km         '
        character (len=30) :: mapping_name = 'lambert_cylindrical_equal_area'

        integer :: grid_width  = 3856
        integer :: grid_height = 1624
        integer :: base        = 0

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
        double precision, dimension(:), pointer :: y
        double precision, dimension(:), pointer :: x

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
      integer function GPDgetYProj(yproj)
!******************************************************************************
! English Name: Get Y-Projection Coordinate
! -------------
!
! Purpose: Returns a copy of the grid Y-projection coordinate.
! --------
!
! Language: Fortran 90
! ---------
!
! See Also: GPDgetXProj()
! -------- 
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! yproj(:)              double      OUT  array of grid y coordinates.
!
! GPDgetYProj          integer      OUT  function return value:
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
!           04/24/2018      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      double precision, dimension(:), intent(out) :: yproj

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GetYProjErrorHandle

      rc = 0
      GPDgetYProj = -1

!     Make sure that the grid has been
!     defined.
!     ================================

      if (.not. associated(GPDD%y)) rc = -1
      if (isError(this,1,rc) .ne. 0) return

!     Copy the Y Projection Coordinates
!     =================================
      
      yproj = GPDD%y

      GPDgetYProj = 0

      end function GPDgetYProj

!******************************************************************************
      integer function GPDgetXProj(xproj)
!******************************************************************************
! English Name: Get X-Projection Coordinate
! -------------
!
! Purpose: Returns a copy of the grid X-projection coordinate.
! --------
!
! Language: Fortran 90
! ---------
!
! See Also: GPDgetXProj()
! -------- 
!
! Notes: 1. GPDsetGrid() is a prerequisite
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! xproj(:)              double      OUT  array of grid x coordinates.
!
! GPDgetXProj          integer      OUT  function return value:
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
!           04/24/2018      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      double precision, dimension(:), intent(out) :: xproj

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GetXProjErrorHandle

      rc = 0
      GPDgetXProj = -1

!     Make sure that the grid has been
!     defined.
!     ================================

      if (.not. associated(GPDD%x)) rc = -1
      if (isError(this,1,rc) .ne. 0) return

!     Copy the X Projection Coordinates
!     =================================
      
      xproj = GPDD%x

      GPDgetXProj = 0

      end function GPDgetXProj

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

      double precision :: r0,s0
      double precision :: sin_phi1, cos_phi1
      double precision :: qp, kz, a, e, e2, e4, e6
      double precision :: col,row
      double precision :: lat,lon
      double precision :: x,y,beta,phi,lam

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

        row = j

        y = GPD%map_scale * (s0 - row)
        beta = asin((2.0*y*kz)/(a*qp))

        phi = beta+(((e2/3.0)+((31.0/180.0)*e4)+((517.0/5040.0)*e6)) &
                *sin(2.0*beta))+((((23.0/360.0)*e4)+((251.0/3780.0)*e6)) &
                    *sin(4.0*beta))+(((761.0/45360.0)*e6)*sin(6.0*beta))

        lat  = phi*180.0/PI

        GPDD%y(j+1)        = y
        GPDD%latitude(j+1) = lat

      end do

!     Calculate Longitudes
!     ====================

      do i = 0,cols-1

        col = i

        x = GPD%map_scale * (col - r0)
        lam = x/(a*kz)

        lon = lam*180.0/PI + GPD%map_reference_longitude
        if (lon .lt. -180) lon = -180
       
        GPDD%x(i+1)         = x
        GPDD%longitude(i+1) = lon

     end do

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
     if (associated(GPDD%x)) deallocate(GPDD%x)
     if (associated(GPDD%y)) deallocate(GPDD%y)

!    Perform Sanity Check
!    ====================

     rows = GPD%grid_height
     cols = GPD%grid_width

     if (rows .le. 0) rc = -1
     if (cols .le. 0) rc = -1
     if (isError(this,1,rc) .ne. 0) return

!    Allocate space for navigational
!    information.
!    ===============================

     allocate(GPDD%latitude(rows),stat=rc)
     if (isError(this,2,rc) .ne. 0) return
     allocate(GPDD%longitude(cols),stat=rc)
     if (isError(this,2,rc) .ne. 0) return

     allocate(GPDD%y(rows),stat=rc)
     if (isError(this,2,rc) .ne. 0) return
     allocate(GPDD%x(cols),stat=rc)
     if (isError(this,2,rc) .ne. 0) return

!    Initialize navigational vectors
!    ===============================

     GPDD%latitude  = UNDEF
     GPDD%longitude = UNDEF
     GPDD%y = UNDEF
     GPDD%x = UNDEF

     GPDallocate = 0

     end function GPDallocate

!******************************************************************************
      type (GridParamDefType) function GPDgetdef()
!******************************************************************************

      GPDgetdef = GPD

      end function GPDgetdef

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

!       GPDGetLatitude Error Handle
!       ===========================

        case (GetLatitudeErrorHandle)

        select case (errorEvent)

          case (1)
        
            print *, 'GPDgetLatitude (error#',code,'): no grid defined'
        
          case default

            print *, 'GPDgetLatitude (error#',code,'): unknown error'

        end select

!       GPDGetYProj Error Handle
!       ========================

        case (GetYProjErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GPDgetYProj (error#',code,'): no grid defined'

          case default

            print *, 'GPDgetYProj (error#',code,'): unknown error'

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

!       GPDGetXProj Error Handle
!       ========================

        case (GetXProjErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GPDgetXProj (error#',code,'): no grid defined'

          case default

            print *, 'GPDgetXProj (error#',code,'): unknown error'

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
