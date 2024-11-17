      module tileMod

      use LDAS_TileCoordType
      use GPD_Mod, gridError=>errorHandler

      private

      public :: TILEread
      public :: TileInfoType
      public :: Tile2Grid
      public :: errorHandler

      integer, parameter :: IUNIT = 54
      integer, parameter :: ReadErrorHandle = 1
      integer, parameter :: AllocateErrorHandle = 2

      type TileInfoType

        integer :: numTiles = 0
        integer, dimension(:), pointer :: gridMap
        type(tile_coord_type), dimension(:), pointer :: coord

      end type TileInfoType

      integer :: EXCEPTION = 0

      contains

!******************************************************************************
      integer function TILEread(filename,tiles)
!******************************************************************************
! English Name: Read LDAS Tile Information
! -------------
!
! Purpose: Reads an LDAS tile coordinate file and returns the retrieved data.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This function uses the LDAS library routines for reading the
! ------    coordinate information. Changes to the LDAS codes may cause
!           this function to fail.
!
!        2. The LDAS libraries provide a method for reading the tile 
!           coordinate files. However, there was no way to query the number
!           of tiles for pre-allocating the tile buffers. This function
!           implements the query by explicitly reading the first record
!           containing the number of tiles. This creates a coupling dependency
!           that was unavoidable. 
!
!        3. The grid mapping index array (gridmap) element is allocated but not
!           populated by this function. See TILE2grid(). 
!           
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  filename of the input tile coordinate
!                                        file.
!
! tiles           TileInfoType      OUT  data structure containing the
!                                        retrieved tile coordinate information 
!                                        (see note-3)
!
! TILE2read            integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: unable to access tile coordinate
!                                           file.
!
!                                        2: error reading tile coordinate file.
!
!                                        3: error allocating memory.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           05/02/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (TileInfoType), intent(out) :: tiles
      character (len=*), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: n
      integer :: rc
      integer :: numTiles
      character(len=1024) :: fname
      integer :: this = ReadErrorHandle

      TILEread = -1
      rc = TILEdeallocate(tiles)

!     Establish data stream for tile coordinate
!     information.
!     =========================================

      open (unit=IUNIT,file=filename,form='unformatted',status='old',iostat=rc)
      if (isError(this,1,rc,arg=filename) .ne. 0) return

!     Retrieve the number of tiles and
!     allocate tile data buffers.
!     ================================

      read (IUNIT,iostat=rc) numTiles
      if (isError(this,2,rc,arg=filename) .ne. 0) return

      tiles%numTiles = numTiles

      rc = TILEallocate(tiles)
      if (isError(this,3,rc) .ne. 0) return

!     Read in the tile coordinate information
!     =======================================

      rewind(IUNIT)
      call io_tile_coord_type( 'r', IUNIT, numTiles, tiles%coord)

      TILEread = 0

      close (unit=IUNIT)

      end function TILEread

!******************************************************************************
      integer function TILE2grid(tiles,grid)
!******************************************************************************
! English Name: Tile to Grid
! -------------
!
! Purpose: Calculates the grid index for each tile based on the destination
! -------- grid parameters. The result is a convenient indexing array that
!          will automatically map the tiles to grid space.
!
! Language: Fortran 90
! ---------
!
! Prerequisites: TILEread()
! --------------
!
! Notes: 1. The tile coordinate file ingested by TILEread() does not contain
! ------    the dimension of the associated grid. Therefore, users must supply
!           the correct grid parameters to this function to derive the proper
!           grid mapping.
!
!        2. The grid mapping index array has a rank of one. Indices, however,
!           are calculated based on the rank-2 grid.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! tiles           TileInfoType    INOUT  data structure containing the
!                                        tile coordinate information. On output,
!                                        the "gridMap" element will be populated
!                                        with the grid indices (see note-1).
!
! grid        GridParamDefType       IN  grid parameters defining the grid
!                                        associated with the tiles.
!
! TILE2grid            integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions: none
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           05/04/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (TileInfoType), intent(inout) :: tiles
      type (GridParamDefType), intent(in) :: grid

!     Local Variables
!     ---------------

      integer :: n
      integer :: i_indg,j_indg,k_indg

      TILE2grid = 0

!     Populate the tile to grid indexing array
!     ========================================

      do n = 1,tiles%numTiles

        i_indg = tiles%coord(n)%i_indg - (grid%base - 1)
        j_indg = tiles%coord(n)%j_indg - (grid%base - 1)
        k_indg = (j_indg - 1) * grid%grid_width + i_indg

        tiles%gridMap(n) = k_indg

      end do

      end function TILE2grid

!******************************************************************************
      integer function TILEallocate(tiles)
!******************************************************************************
! English Name:
! -------------
!
! Purpose: Allocated memory for storing tile coordinate and transformation
! -------- information.
!
! Language: Fortran 90
! ---------
!
! Notes: This function is invoked by TILEread().
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! tiles           TileInfoType    INOUT  data structure for storing tile
!                                        coordinate information. The number of
!                                        tiles must be defined on input. 
!                                        Coordinate and transformation arrays
!                                        will be allocated for the specified
!                                        number of tiles on output.
!
! TILEallocate         integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: unable to allocate memory
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           05/03/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (TileInfoType), intent(inout) :: tiles

!     Local Variables
!     ---------------

      integer :: rc,numTiles
      integer :: this = AllocateErrorHandle

      rc = 0
      TILEallocate = -1

!     Allocate space for storing the tile
!     coordinate information.
!     ===================================

      numTiles = tiles%numTiles

      allocate(tiles%gridMap(numTiles),stat=rc)
      if (isError(this,1,rc) .ne. 0) return

      allocate(tiles%coord(numTiles),stat=rc)
      if (isError(this,1,rc) .ne. 0) return

      TILEallocate = 0

      end function TILEallocate

!******************************************************************************
      integer function TILEdeallocate(tiles)
!******************************************************************************
! English Name: Tile Deallocate
! -------------
!
! Purpose: Deallocates memory used to store tile coordinate and transformation
! -------- information. 
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
! tiles           TileInfoType    INOUT  data structure for storing tile
!                                        coordinate information. Arrays for
!                                        storing coordinate information are
!                                        are deallocated and the number of
!                                        tiles is reset to zero.
!                                        
! TILEdeallocate       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions: none
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           05/03/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

      type (TileInfoType), intent(inout) :: tiles

      TILEdeallocate = 0

      tiles%numTiles = 0
      if (associated(tiles%gridMap)) deallocate(tiles%gridMap)
      if (associated(tiles%coord)) deallocate(tiles%coord)

      end function TILEdeallocate
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
! EXCEPTION            integer      OUT exception code (set to "errorEvent")
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           05/03/2013      J.Ardizzone  created.
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

!       Read Error Handle
!       =================

        case (ReadErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'TILEread (error#',code,'): error accessing ', &
                                           'Tile coord file: ',trim(arg)

          case (2)

            print *, 'TILEread (error#',code,'): error reading ', &
                                           'Tile coord file: ',trim(arg)

          case (3)

            print *, 'TILEread (error#',code,'): error allocating memory'

          case default

            print *, 'TILEread (error#',code,'): unknown error'

        end select

!       Allocate Error Handle
!       =====================

        case (AllocateErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'TILEallocate (error#',code,'): unable to allocate memory'

          case default

            print *, 'TILEallocate (error#',code,'): unknown error'

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

      end module tileMod
