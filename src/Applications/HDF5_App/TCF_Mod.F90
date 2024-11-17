!******************************************************************************
      module TCF_Mod
!******************************************************************************
! English Name: TCF Module
! -------------
!
! Purpose: Provides all methods for reading and delivering data from
! -------- sample TCF binary files.
!
! Language: Fortran 90
! ---------
!
! See Also: typesMod.F90, interfaceMod.F90
! ---------
!
! Notes: 1. interfaceMod.getArgs() is a prerequisite for all methods.
! ------ 2. namelist I/O is provided through interfaceMod.F90 and is used
!           to guide the methods for reading the LDAS binary files that are
!           not self-descriptive.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! LDASopen()          function      PUB  Opens the LDAS namelist and data file
!                                        for the geophysical data collection.
!                                        Also reads and saves header information
!                                        from the LDAS data file.
!
! LDASread()         interface      PUB  public interface for invoking LDAS
!                                        "LDASread" methods.
!
! LDASreadGroup()     function      PUB  Reads "group" namelist block. A "group"
!                                        namelist block always precedes the
!                                        "element" namelist blocks.
!
! LDASreadElement()   function      PUB  Reads and returns data sets (elements)
!                                        from an input LDAS geophysical data 
!                                        collection file. I/O is guided by the
!                                        "element" namelist blocks.
!
! LDASclose()         function      PUB  Closes the LDAS geophysical data file
!                                        and namelist.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/11/2013      J.Ardizzone  created.
!******************************************************************************

      use typesMod
      use SDS_Mod, sdsError=>errorHandler
      use GPD_Mod, gridError=>errorHandler
      use tileMod, tileError=>errorHandler
      use interfaceMod, interfaceError=>errorHandler

      private

      public :: LDASopen
      public :: LDASread
      public :: LDASclose
      public :: errorHandler

      integer, parameter :: IUNIT=10

      integer, parameter :: OpenErrorHandle = 1
      integer, parameter :: ReadGroupErrorHandle  = 2
      integer, parameter :: ReadElementErrorHandle = 3
      integer, parameter :: ReadDataErrorHandle = 4
      integer, parameter :: GetCoordErrorHandle = 5

      interface LDASread

        module procedure LDASreadGroup
        module procedure LDASreadElement

      end interface LDASread

      integer :: EXCEPTION = 0

      type (TileInfoType) :: TILES

      contains

!******************************************************************************
      integer function LDASopen(filename,fileInfo)
!******************************************************************************
! English Name: LDAS Open
! -------------
!
! Purpose: Opens the LDAS namelist and data file for the geophysical data
! -------- collection. Also reads and saves header information from the LDAS
!          data file.
!
! Language: Fortran 90
! ---------
!
! See Also: interfaceMod.F90
! ---------
!
! Notes: 1. interfaceMod.getArgs() is a prerequisite.
! ------ 2. This function reads the first namelist block (&file).
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  filename of the LDAS geophysical data
!                                        collection file.
!
! fileInfo        FileInfoType      OUT  general file information retrieved
!                                        from the "file" namelist block.
!
! LDASopen             integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error reading namelist
!                                        2: environment variabe not set
!                                        3: error reading tile coordinate file
!                                        4: error accessing LDAS file
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      character (len=*), intent(in) :: filename
      type (FileInfoType), intent(out) :: fileInfo

!     Local Variables
!     ---------------

      integer :: n
      integer :: rc
      integer :: cols,rows
      character(len=MAX_STR_LEN) :: fname
      type (GridParamDefType) :: grid
      integer :: this = OpenErrorHandle

!     Retrieve "file" information
!     block from namelist
!     ===========================

      rc = openNamelist()
      if (isError(this,1,rc) .ne. 0) return

      rc = readFileInfo(fileInfo)
      if (isError(this,1,rc) .ne. 0) return

      fileInfo%filename = filename

!     Now open the binary tile file for the
!     L4_SM geophysical collection.
!     ======================================

!     rc = GPDread('EASE2_M01km.gpd')
!     rc = GPDsetGrid()
      rc = GPDgetGrid(grid)

      cols = grid%grid_width
      rows = grid%grid_height

      open (unit=IUNIT,file=filename,form='unformatted',status='old', &
             access='direct',recl=cols*rows*4,iostat=rc)
      if (isError(this,4,rc,arg=filename) .ne. 0) return

      LDASopen = 0

      end function LDASopen

!******************************************************************************
      integer function LDASreadGroup(group)
!******************************************************************************
! English Name: LDAS Read Group
! -------------
!
! Purpose: Reads a "group" namelist block. A "group" namelist block always 
! -------- precedes the "element" namelist blocks.
!
! Language: Fortran 90
! ---------
!
! See Also: interfaceMod.F90
! ---------
!
! Notes: 1. interfaceMod.getArgs() is a prerequisite.
! ------ 2. LDASopen() is a prerequisite.
!        3. This function is a wrapper for interfaceMod.readGroup()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!    
! group          GroupInfoType      OUT  data structure containing the contents
!                                        of the "group" namelist block.
!
! LDASreadGroup        integer      OUT  function return value:
!
!                                       -1: exception occurred
!                                       -2: end of file
!
!                                        Thrown Exceptions
!
!                                        1: error reading namelist
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/11/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GroupInfoType), intent(out) :: group

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = ReadGroupErrorHandle

!     Read "group" namelist block
!     ===========================

      LDASreadGroup = -2
      rc = readGroupInfo(group)
      if (rc .eq. -2) return

      LDASreadGroup = -1
      if (isError(this,1,rc) .ne. 0) return

      LDASreadGroup = 0

      end function LDASreadGroup

!******************************************************************************
      integer function LDASreadElement(element)
!******************************************************************************
! English Name: LDAS Read Element
! -------------
!
! Purpose: Reads and returns data sets (elements) from an input LDAS
! -------- geophysical data collection file. I/O is guided by the "element"
!          namelist blocks.
!
! Language: Fortran 90
! ---------
!
! See Also: interfaceMod.F90, LDASopen(), LDASreadGroup()
! ---------
!
! Notes: 1. interfaceMod.getArgs() is a prerequisite.
! ------ 2. LDASopen() is a prerequisite.
!        3. LDASreadGroup() is a prerequisite.
!        4. This function reads element namelist blocks (&element).
!        5. The element namelist block is augmented with coordinate elements
!           since the lat/lon information is not explicitly contained in the
!           LDAS file. Therefore, the namelist element blocks are not read
!           until coordinate information has been issued.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! element      ElementDataType      OUT  returned data set (element)
!
! LDASreadElement      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!                                       -2: end of elements
!
!                                        Thrown Exceptions
!
!                                        1: error reading namelist
!                                        2: error retrieving data for element
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/11/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(out) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rows,cols
      integer :: this = ReadElementErrorHandle

      type (GridParamDefType) :: grid

!     Initialize
!     ==========

      LDASreadElement = -1

      rc = GPDgetGrid(grid)

!     Purge prior allocations for
!     this element and re-initialize
!     with grid information.

      call SDSdeallocate(element)

      cols = grid%grid_width
      rows = grid%grid_height

!     Set dimensions and chunk size. The chunk
!     sizes are set to partition the data along
!     the equator to yield two chunks - northern
!     and southern hemisphere.
!     ==========================================

      element%info%rank = 2
      element%info%dims(1:2)   = (/cols,rows/)
!     element%info%chunks(1:2) = (/cols,rows/2/)
      element%info%chunks(1:2) = (/cols,rows/)

!     Read element information from 
!     namelist
!     =============================

      LDASreadElement = -2
      rc = readDataInfo(element%info)
      if (rc .eq. -2) return

      LDASreadElement = -1
      if (isError(this,1,rc) .ne. 0) return

!     Populate the element with data.
!     ===============================

      if (isCoord(element)) then 

        rc = getCoord(element)
        if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      else

        rc = readData(element)
        if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

        element%info%coordinates  = 'latitude longitude'

      endif

      LDASreadElement = 0

      end function LDASreadElement

!******************************************************************************
      integer function readData(element)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer, save :: irec = 0
      integer :: this = ReadDataErrorHandle

      real*4, dimension(:), pointer :: f32
      integer*4, dimension(:), pointer :: i32
      character(len=24), dimension(:), pointer :: str24

      rc = 0
      readData = -1

!     Allocate memory for the new
!     element.
!     ===========================

      rc = SDSallocate(element)
      if (isError(this,1,rc) .ne. 0) return

!     Read element data from LDAS file
!     ================================

      irec = irec + 1

      if (associated(element%f32)) then

        allocate(f32(TILES%numTiles),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        element%f32 = SDStransfer(element%info%fill_value,SDS_F32_FILL)

        read(IUNIT,rec=irec,iostat=rc) f32
        if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

        where (f32 .lt. -9000.0) f32 = SDS_F32_FILL
        element%f32(TILES%gridmap) = f32

        deallocate(f32)

      elseif (associated(element%i32)) then

        allocate(i32(TILES%numTiles),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        element%i32 = SDStransfer(element%info%fill_value,SDS_I32_FILL)

        read(IUNIT,rec=irec,iostat=rc) i32
        if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

        where (i32 .lt. -9000) i32 = SDS_I32_FILL
        element%i32(TILES%gridmap) = i32

        deallocate(i32)

      elseif (associated(element%u32)) then

        allocate(i32(TILES%numTiles),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        element%u32 = SDStransfer(element%info%fill_value,SDS_U32_FILL)

        read(IUNIT,rec=irec,iostat=rc) i32
        if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

        where (i32 .lt. -9000) i32 = SDS_U32_FILL
        element%u32(TILES%gridmap) = i32

        deallocate(i32)

      elseif (associated(element%buf)) then

        allocate(str24(TILES%numTiles),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        element%buf = trim(SDStransfer(element%info%fill_value,SDS_STR_FILL))

        read(IUNIT,rec=irec,iostat=rc) str24
        if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

        element%buf(TILES%gridmap) = str24

        deallocate(str24)

      else

        rc = 1

      endif

      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      readData = 0

      end function readData

!******************************************************************************
      integer function getCoord(element)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: i,j
      integer :: rows,cols
      integer :: this = GetCoordErrorHandle

      integer, dimension(:,:), pointer :: col_index
      integer, dimension(:,:), pointer :: row_index

      real, dimension(:,:), pointer :: longitude
      real, dimension(:,:), pointer :: latitude

      getCoord = -1
      cols = element%info%dims(1)
      rows = element%info%dims(2)

!     Create the element
!     ==================

      if (isLatitude(element)) then

        allocate(latitude(1,rows),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        rc = SDSallocate(element,real_size=32)
        if (isError(this,1,rc) .ne. 0) return
        rc = GPDgetLatitude(latitude(1,:))
        if (isError(this,2,rc) .ne. 0) return

        element = latitude

        deallocate(latitude)

      elseif (isLongitude(element)) then

        allocate(longitude(cols,1),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        rc = SDSallocate(element,real_size=32)
        if (isError(this,1,rc) .ne. 0) return
        rc = GPDgetLongitude(longitude(:,1))
        if (isError(this,2,rc) .ne. 0) return

        element = longitude

        deallocate(longitude)

      elseif (isRowIndex(element)) then

        allocate(row_index(1,rows),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        rc = SDSallocate(element,uint_size=32)
        if (isError(this,1,rc) .ne. 0) return
        row_index(1,:) = (/(j,j=0,rows-1)/)

        element = row_index

        deallocate(row_index)

      elseif (isColIndex(element)) then

        allocate(col_index(cols,1),stat=rc)
        if (isError(this,1,rc) .ne. 0) return

        rc = SDSallocate(element,uint_size=32)
        if (isError(this,1,rc) .ne. 0) return
        col_index(:,1) = (/(i,i=0,cols-1)/)

        element = col_index

        deallocate(col_index)

      endif

      getCoord = 0

      end function getCoord

!******************************************************************************
      integer function LDASclose()
!******************************************************************************
! English Name: LDAS Close
! -------------
!
! Purpose: Closes the LDAS geophysical data file and namelist.
! --------
!
! Language: Fortran 90
! ---------
!
! See Also: interfaceMod.F90, LDASopen()
! ---------
!
! Notes: 1. interfaceMod.getArgs() is a prerequisite.
! ------ 2. LDASopen() is a prerequisite.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! LDASclose            integer      OUT  function return value:
!
!                                        0: normal
!                                        1: exception occurred
!
!                                        Thrown Exceptions: none
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/11/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

      integer :: rc

      close(unit=IUNIT)
      rc = closeNamelist()

      LDASclose = 0

      end function LDASclose

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
!            3/08/2013      J.Ardizzone  created.
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

!       LDASopen Error Handle
!       =====================

        case (OpenErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LDASopen (error#',code,'): error reading namelist'

          case (2)

            print *, 'LDASopen (error#',code,'): environment variable ', &
                                          'not set: LDAS_TILE_COORD_FILE'

          case (3)

            print *, 'LDASopen (error#',code,'): error reading LDAS ', &
                                                       'coordinate file'

          case (4)

            print *, 'LDASopen (error#',code,'): error accessing ', &
                                                 'LDAS file: "',trim(arg),'"'

          case default

            print *, 'LDASopen (error#',code,'): unknown error'

        end select

!       LDASreadElement Error Handle
!       ============================

        case (ReadElementErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LDASreadElement (error#',code,'): error reading namelist'

          case (2)

            print *, 'LDASreadElement (error#',code,'): error retrieving ', &
                                            'data for element: "',trim(arg),'"'

          case default

            print *, 'LDASreadElement (error#',code,'): unknown error'

        end select

!       readData Error Handle
!       =====================

        case (ReadDataErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'readData (error#',code,'): error allocating memory'

          case (2)

            print *, 'readData (error#',code,'): error retrieving data ', &
                                             'for element: "',trim(arg),'"'

          case (3)

            print *, 'readData (error#',code,'): input data type is ', &
                                            'unexpected for: "',trim(arg),'"'

          case default

            print *, 'readData (error#',code,'): unknown error'

        end select

!       getCoord Error Handle
!       =====================

        case (GetCoordErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'getCoord (error#',code,'): error allocating memory'

          case (2)

            print *, 'getCoord (error#',code,'): error retrieving ', &
                                  'coordinate information for: "',trim(arg),'"'
          case default

            print *, 'getCoord (error#',code,'): unknown error'

        end select

!       LDASreadGroup Error Handle
!       ==========================

        case (ReadGroupErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'LDASreadGroup (error#',code,'): error reading namelist'

          case default

            print *, 'LDASreadGroup (error#',code,'): unknown error'

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

      end module TCF_Mod
