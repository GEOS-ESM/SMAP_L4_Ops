!******************************************************************************
      module GRIDMod
!******************************************************************************
! English Name: Grid Module
! -------------
!
! Purpose: Implements an API for allocating and deallocating derived grid
! -------- types (see TypesMod) by centralizing the heap management of all
!          derived type grid elements.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The grid elements of the derived grid types are described using
! ------    an element of type "GridInfoType". This elements must contain the
!           desired grid dimensions upon invoking any of the allocation
!           methods for derived types.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! GRIDAllocate       interface      PUB  Polymorphic function name used to
!                                        access methods for allocating and
!                                        managing memory. Invoke the
!                                        "malloc___" and "calloc___" methods 
!                                        below using this name with the 
!                                        corresponding arguments.
!
!   mallocHeap()      function      PUB  Sets a unique identifier for 
!                                        partitioning heap allocation by
!                                        associating an integer id to all
!                                        pointer allocations in the registry.
!
!   mallocLFO()       function      PUB  Allocates memory for grid elements
!                                        of derived type, "LFODataType".
!
!   callocLFO()       function      PUB  Allocates memory for grid elements
!                                        of one or more instances of derived
!                                        type, "LFODataType".
!
!   mallocCPCU()      function      PUB  Allocates memory for grid elements
!                                        of derived type, "CPCUDataType".
!
!   mallocCPCUCorr()  function      PUB  Allocates memory for grid elements
!                                        of derived type, "CPCUCorrDataType".
!
!   callocCPCUCorr()  function      PUB  Allocates memory for grid elements
!                                        of one or more instances of derived
!                                        type, "CPCUCorrDataType".
!
!   mallocPrecip()    function      PUB  Allocates memory for grid elements
!                                        of derived type, "PrecipDataType".
!
!   callocPrecip()    function      PUB  Allocates memory for grid elements
!                                        of one or more instances of derived
!                                        type, "PrecipDataType".
!
!   mallocGeneric()   function      PUB  Allocates memory for grid elements
!                                        of derived type, "GenericDataType".
!
!   mallocReal()      function      PUB  Allocates memory for a two dimensional
!                                        grid of type, "real".
!
!   mallocInteger()   function      PUB  Allocates memory for a two dimensional
!                                        grid of type, "integer".
!
! GRIDDeallocate     interface      PUB  Polymorphic subroutine name used to
!                                        access methods for deallocating
!                                        memory and memory management
!                                        constructs. Invoke the "free___"
!                                        methods below using this name with the
!                                        corresponding arguments.
!
!   freeHeap()      subroutine      PUB  Frees/deallocates all memory 
!                                        associated with the specified
!                                        heap ID and removes the corresponding
!                                        entries from the registry.
!
!   freeGrid()      subroutine      PUB  Frees/deallocates all memory
!                                        associated with the specified grid of
!                                        type "GridInfoType" which contains
!                                        all registered information from the
!                                        allocation process.
!
!   freeAll()       subroutine      PUB  Frees/deallocates all memory currently
!                                        referenced in the registry. This clears
!                                        all prior allocations and resets the
!                                        registry.
!
! GRIDCloseHeap()   subroutine      PUB  Closes the currently active heap. 
!                                        Subsequent grid management will
!                                        reference the heap activated prior 
!                                        to this one.
!
! errorHandler()      function      PUB  Module error handler for trapping
!                                        thrown exceptions. Recast this 
!                                        method to avoid conflict with other
!                                        module error handlers.
!
!                         <<REGISTRY>>
!
! NX                   integer     PRIV  inner (longitude) dimension of grid
!                                        to be allocated.
!
! NY                   integer     PRIV  outer (latitude) dimension of grid
!                                        to be allocated.
!
! NUMGRIDS             integer     PRIV  number of grids stored in registry.
!
! GRIDS(:)        GridDataType     PRIV  grid registry containing the
!                                        information of the stored grid.
!
! iHEAP                integer     PRIV  active heap identifier.
!
! HEAP                 integer     PRIV  heap pool counter for deriving
!                                        unique identifiers.
!
! iSTACK               integer     PRIV  heap stack counter to track number
!                                        of opened heaps.
!
! STACK(MAXDIM)        integer     PRIV  heap stack. Most recently activated
!                                        heap identifier is at position,
!                                        "iStack".
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/10/2013      J.Ardizzone  created.
!******************************************************************************

      use TypesMod

!     Public API
!     ----------

      private
      public :: GRIDRotate
      public :: GRIDAllocate
      public :: GRIDDeallocate
      public :: GRIDCloseHeap
      public :: isValidGrid
      public :: gridCompare
      public :: gridNotCompare
      public :: errorHandler

!     Constants
!     ---------

      integer, parameter :: MAXDIM = 1000
      integer, parameter :: GRIDRegisterErrorHandle = 1
      integer, parameter :: GRIDAllocateErrorHandle = 2
      integer, parameter :: GRIDDeallocateErrorHandle = 3

!     Derived data type used by the
!     registry to track allocated grids
!     ---------------------------------

      type GridDataType

        integer :: id
        integer :: heap
        real, dimension(:,:), pointer :: rptr => null()
        integer, dimension(:,:), pointer :: iptr => null()

      end type GridDataType

!     Grid Allocation Interface
!     -------------------------

      interface GRIDAllocate

        module procedure mallocHeap
        module procedure mallocReal
        module procedure mallocInteger

        module procedure mallocLFO 
        module procedure callocLFO
        module procedure mallocCMAP
        module procedure mallocCPCU
        module procedure callocCPCU
        module procedure mallocGeneric
        module procedure mallocPrecip
        module procedure callocPrecip
        module procedure mallocCPCUCorr
        module procedure callocCPCUCorr

      end interface GRIDAllocate

!     Grid Deallocation Interface
!     ---------------------------

      interface GRIDDeallocate

        module procedure freeAll
        module procedure freeGrid
        module procedure freeHeap

      end interface GRIDDeallocate

!     Grid Rotate Interface
!     ---------------------

      interface GRIDRotate
        module procedure rotateCMAP
        module procedure rotateGeneric
        module procedure rotateCPCU
        module procedure mrotateCPCU
        module procedure rotateGrid
      end interface GRIDRotate

      interface operator (.eq.)
        module procedure gridCompare
      end interface

      interface operator (.ne.)
        module procedure gridNotCompare
      end interface

!     Registry
!     --------

      integer :: ID = 0
      integer :: NX = 0
      integer :: NY = 0
      integer :: HEAP = 0
      integer :: iHEAP = 0
      integer :: NUMGRIDS = 0
      integer :: errorCode = 0
      integer :: iSTACK = 0
      integer, dimension(0:MAXDIM+1) :: STACK = 0
      type (GridDataType), dimension(MAXDIM) :: GRIDS

      contains

!******************************************************************************
      integer function GRIDRegister(grid)
!******************************************************************************
! English Name: Grid Register
! -------------
!
! Purpose: Registers information for the specified grid by associating it with
! -------- a unique identifier and saving the grid dimensions for use by 
!          related methods.
!
! Language: Fortran 90
! ---------
!
! See Also: mallocHeap()
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! grid            GridInfoType    INOUT  grid information
!
!   nx                 integer       IN  inner (longitude) dimension of grid
!                                        to be allocated.
!
!   ny                 integer       IN  outer (latitude) dimension of grid
!                                        to be allocated.
!
!   id                 integer      OUT  unique grid identifier from registry
!
!   heap               integer      OUT  unique heap identifier from registry
!
!                         <<REGISTRY>>
!
! ID                   integer    INOUT  grid pool counter for deriving
!                                        unique identifiers (incremented by 1
!                                        on output).
!    
! NX                   integer      OUT  inner (longitude) dimension of grid
!                                        to be allocated.
!
! NY                   integer      OUT  outer (latitude) dimension of grid
!                                        to be allocated.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/10/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

      type (GridInfoType), intent(inout) :: grid

      integer :: rc
      integer :: this = GRIDRegisterErrorHandle

      rc = 0
      GRIDRegister = -1

      NX = grid%nx
      NY = grid%ny

      if (NX .le. 0 .or. NY .le. 0) rc = 1
      if (isError(this,1,rc) .ne. 0) return

      ID = ID + 1
      grid%id   = ID
      grid%heap = iHEAP

      GRIDRegister = 0

      end function GRIDRegister

!******************************************************************************
      integer function mallocHeap(heapID)
!******************************************************************************
! English Name: Allocate Heap
! -------------
!
! Purpose: Sets a unique identifier for partitioning heap allocation by
! -------- associating an integer id to all pointer allocations in the registry.
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
! heapID               integer       IN  Unique heap identifier. A zero value
!                                        allocates a new heap identifier.
!
! mallocHeap           integer      OUT  function return value:
!
!                                        =heapID: Returned input arg if
!                                            previously allocated.
!                                        >0: Newly allocated unique identifier
!                                        -1: Exception occurred
!
!                                        Thrown Exceptions:
!
!                                        5: maximum heaps exceeded
!
!                         <<REGISTRY>>
!
! iHEAP                integer      OUT  activated heap identifier.
!
! HEAP                 integer      OUT  heap pool counter for deriving
!                                        unique identifiers.
!
! iSTACK               integer      OUT  heap stack counter to track number
!                                        of opened heaps.
!
! STACK(MAXDIM)        integer      OUT  heap stack. Most recently activated
!                                        heap identifier is at position,
!                                        "iStack".
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: heapID

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

!     Initialize
!     ==========

      rc = 0
      iHEAP = heapID
      mallocHeap = -1

!     Allocate a new heap identifier if
!     the input heap ID is zero. Otherwise
!     use the specified heap identifier.
!     ====================================
 
      if (heapID .eq. 0) then
        HEAP = HEAP + 1
        iHEAP = HEAP
      endif

!     Push the heap id onto the top of the
!     stack to indicate that this is the
!     active heap identifier.
!     ====================================

      if (iSTACK .ge. MAXDIM) rc = 1
      if (isError(this,5,rc,method='mallocHeap') .ne. 0) return

      iSTACK = iSTACK + 1
      STACK(iSTACK) = iHEAP

      mallocHeap = iHEAP

      end function mallocHeap

!******************************************************************************
      integer function mallocCPCU(dataType)
!******************************************************************************
! English Name: Allocate Climate Prediction Center "Unified" precipitation.
! -------------
!
! Purpose: Allocates memory for grid elements of derived type, "CPCUDataType".
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
! dataType        CPCUDataType   INOUT   On input, grid dimensions to be
!                                        allocated. On output, allocated grid
!                                        elements.
!
! mallocCPCU           integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error registering grid elements
!                                        2: error allocating memory
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

      type (CPCUDataType), intent(inout) :: dataType

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocCPCU = -1

      rc = GRIDRegister(dataType%grid)
      if (isError(this,1,rc,method='mallocCPCU') .ne. 0) return

      rc = GRIDAllocate(dataType%data)
      if (isError(this,2,rc,method='mallocCPCU') .ne. 0) return

      mallocCPCU = 0

      end function mallocCPCU

!******************************************************************************
      integer function mallocCMAP(dataType)
!******************************************************************************
! English Name: Allocate CPC Merged Analysis of Precipitation (CMAP)
! -------------
!
! Purpose: Allocates memory for grid elements of derived type, "CMAPDataType".
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
! dataType        CMAPDataType   INOUT   On input, grid dimensions to be
!                                        allocated. On output, allocated grid
!                                        elements.
!
! mallocCMAP           integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error registering grid elements
!                                        2: error allocating memory
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           12/15/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

      type (CMAPDataType), intent(inout) :: dataType

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocCMAP = -1

      rc = GRIDRegister(dataType%grid)
      if (isError(this,1,rc,method='mallocCMAP') .ne. 0) return

      rc = GRIDAllocate(dataType%data)
      if (isError(this,2,rc,method='mallocCMAP') .ne. 0) return

      mallocCMAP = 0

      end function mallocCMAP

!******************************************************************************
      integer function mallocGeneric(dataType)
!******************************************************************************
! English Name: Allocate Generic
! -------------
!
! Purpose: Allocates memory for grid elements of derived type, 
! -------- "GenericDataType".
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
! dataType     GenericDataType    INOUT  On input, grid dimensions to be
!                                        allocated. On output, allocated grid
!                                        elements.
!
! mallocGeneric        integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error registering grid elements
!                                        2: error allocating memory
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.

!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GenericDataType), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocGeneric = -1

      rc = GRIDRegister(dataType%grid)
      if (isError(this,1,rc,method='mallocGeneric') .ne. 0) return

      rc = GRIDAllocate(dataType%data)
      if (isError(this,2,rc,method='mallocGeneric') .ne. 0) return

      mallocGeneric = 0

      end function mallocGeneric

!******************************************************************************
      integer function mallocLFO(dataType)
!******************************************************************************
! English Name: Allocate Land Forcing
! -------------
!
! Purpose: Allocates memory for grid elements of derived type, "LFODataType".
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
! dataType       LFODataType     INOUT   On input, grid dimensions to be
!                                        allocated. On output, allocated grid
!                                        elements.
!
! mallocLFO          integer       OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error registering grid elements
!                                        2: error allocating memory
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (LFODataType), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocLFO = -1

      rc = GRIDRegister(dataType%grid)
      if (isError(this,1,rc,method='mallocLFO') .ne. 0) return

      rc = GRIDAllocate(dataType%prectot)
      if (isError(this,2,rc,method='mallocLFO') .ne. 0) return

      rc = GRIDAllocate(dataType%precls)
      if (isError(this,2,rc,method='mallocLFO') .ne. 0) return

      rc = GRIDAllocate(dataType%preccon)
      if (isError(this,2,rc,method='mallocLFO') .ne. 0) return

      rc = GRIDAllocate(dataType%precsno)
      if (isError(this,2,rc,method='mallocLFO') .ne. 0) return

      rc = GRIDAllocate(dataType%airtemp)
      if (isError(this,2,rc,method='mallocLFO') .ne. 0) return

      mallocLFO = 0

      end function mallocLFO

!******************************************************************************
      integer function callocLFO(dataType)
!******************************************************************************
! English Name: Allocate Land Forcing Array
! -------------
!
! Purpose: Allocates memory for grid elements of one or more instances of
! -------- derived type, "LFODataType".
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
! dataType(:)      LFODataType     INOUT On input, grid dimensions to be
!                                        allocated for each instance. On output,
!                                        allocated grid elements for each
!                                        instance.
!
! callocLFO            integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        6: error allocating array elements
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (LFODataType), dimension(:), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: i,rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      callocLFO = -1

      do i = 1,size(dataType)
        rc = mallocLFO(dataType(i))
        if (isError(this,6,rc,method='callocLFO') .ne. 0) return
      end do

      callocLFO = 0

      end function callocLFO

!******************************************************************************
      integer function mallocPrecip(dataType)
!******************************************************************************
! English Name: Allocate Precipitation 
! -------------
!
! Purpose: Allocates memory for grid elements of derived type,
! -------- "PrecipDataType".
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
! dataType      PrecipDataType   INOUT   On input, grid dimensions to be
!                                        allocated. On output, allocated grid
!                                        elements.
!
! mallocPrecip         integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error registering grid elements
!                                        2: error allocating memory
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (PrecipDataType), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocPrecip = -1

      rc = GRIDRegister(dataType%grid)
      if (isError(this,1,rc,method='mallocPrecip') .ne. 0) return

      rc = GRIDAllocate(dataType%data)
      if (isError(this,2,rc,method='mallocPrecip') .ne. 0) return

      mallocPrecip = 0

      end function mallocPrecip

!******************************************************************************
      integer function callocPrecip(dataType)
!******************************************************************************
! English Name: Allocate Precipitation Array
! -------------
!
! Purpose: Allocates memory for grid elements of one or more instances of
! -------- derived type, "PrecipDataType".
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
! dataType(:)   PrecipDataType     INOUT On input, grid dimensions to be
!                                        allocated for each instance. On output,
!                                        allocated grid elements for each
!                                        instance.
!
! callocPrecip         integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        6: error allocating array elements
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (PrecipDataType), dimension(:), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: i,rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      callocPrecip = -1

      do i = 1,size(dataType)
        rc = mallocPrecip(dataType(i))
        if (isError(this,6,rc,method='callocPrecip') .ne. 0) return
      end do

      callocPrecip = 0

      end function callocPrecip

!******************************************************************************
      integer function mallocCPCUCorr(dataType)
!******************************************************************************
! English Name: Allocate CPCU Correction
! -------------
!
! Purpose: Allocates memory for grid elements of derived type,
! -------- "CPCUCorrDataType".
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
! dataType    CPCUCorrDataType   INOUT   On input, grid dimensions to be
!                                        allocated. On output, allocated grid
!                                        elements.
!
! mallocCPCUCorr       integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        1: error registering grid elements
!                                        2: error allocating memory
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

      type (CPCUCorrDataType), intent(inout) :: dataType

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocCPCUCorr = -1

      rc = GRIDRegister(dataType%grid)
      if (isError(this,1,rc,method='mallocCPCUCorr') .ne. 0) return

      rc = GRIDAllocate(dataType%factor)
      if (isError(this,2,rc,method='mallocCPCUCorr') .ne. 0) return

      rc = GRIDAllocate(dataType%residual)
      if (isError(this,2,rc,method='mallocCPCUCorr') .ne. 0) return

      rc = GRIDAllocate(dataType%eod)
      if (isError(this,2,rc,method='mallocCPCUCorr') .ne. 0) return

      mallocCPCUCorr = 0

      end function mallocCPCUCorr

!******************************************************************************
      integer function callocCPCU(dataType)
!******************************************************************************
! English Name: Allocate CPCU Array
! -------------
!
! Purpose: Allocates memory for grid elements of one or more instances of
! -------- derived type, "CPCUDataType".
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
! dataType(:) CPCUDataType        INOUT  on input, grid dimensions to be
!                                        allocated for each instance. On output,
!                                        allocated grid elements for each
!                                        instance.
!
! callocCPCU           integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        6: error allocating array elements
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/11/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (CPCUDataType), dimension(:), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: i,rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      callocCPCU = -1

!     Allocate each array element.
!     ============================

      do i = 1,size(dataType)
        rc = mallocCPCU(dataType(i))
        if (isError(this,6,rc,method='callocCPCU') .ne. 0) return
      end do

      callocCPCU = 0

      end function callocCPCU

!******************************************************************************
      integer function callocCPCUCorr(dataType)
!******************************************************************************
! English Name: Allocate CPCU Correction Array
! -------------
!
! Purpose: Allocates memory for grid elements of one or more instances of
! -------- derived type, "CPCUCorrDataType".
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
! dataType(:) CPCUCorrDataType    INOUT  On input, grid dimensions to be
!                                        allocated for each instance. On output,
!                                        allocated grid elements for each
!                                        instance.
!
! callocCPCUCorr       integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        6: error allocating array elements
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (CPCUCorrDataType), dimension(:), intent(inout) :: dataType

!     Local Variables
!     ---------------

      integer :: i,rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      callocCPCUCorr = -1

!     Allocate each array element.
!     ============================

      do i = 1,size(dataType)
        rc = mallocCPCUCorr(dataType(i))
        if (isError(this,6,rc,method='callocCPCUCorr') .ne. 0) return
      end do

      callocCPCUCorr = 0

      end function callocCPCUCorr

!******************************************************************************
      integer function mallocReal(dataType)
!******************************************************************************
! English Name: Allocate Real
! -------------
!
! Purpose: Allocates memory for a 2-dimensional grid of type, "real".
! -------- 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. GRIDRegister() is a prerequisite for this function.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! dataType(:,:)           real     OUT   allocated two dimensional grid.
!
! mallocReal           integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        3: maximum number of grids exceeded
!                                        4: unable to allocate memory
!
!                         <<REGISTRY>>
!
! NX                   integer      IN   inner (longitude) dimension of grid
!                                        to be allocated.
!
! NY                   integer      IN   outer (latitude) dimension of grid
!                                        to be allocated.
!
! NUMGRIDS             integer   INOUT   number of grids stored in registry.
!
! GRIDS(:)        GridInfoType     OUT   grid registry containing the
!                                        information of the stored grid.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(:,:), pointer :: dataType

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocReal = -1

!     Allocate memory and store the grid
!     information in the registry.
!     ==================================

      if (NUMGRIDS .ge. MAXDIM) rc = 1
      if (isError(this,3,rc,method='mallocReal') .ne. 0) return

      allocate(dataType(NX,NY),stat=rc)
      if (isError(this,4,rc,method='mallocReal') .ne. 0) return

      NUMGRIDS = NUMGRIDS + 1
      GRIDS(NUMGRIDS)%id   = ID
      GRIDS(NUMGRIDS)%heap = iHEAP
      GRIDS(NUMGRIDS)%rptr => dataType
      nullify(GRIDS(NUMGRIDS)%iptr)

      mallocReal = 0

      end function mallocReal

!******************************************************************************
      integer function mallocInteger(dataType)
!******************************************************************************
! English Name: Allocate Integer
! -------------
!
! Purpose: Allocates memory for a 2-dimensional grid of type, "integer".
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. GRIDRegister() is a prerequisite for this function.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! dataType(:,:)        integer     OUT   allocated two dimensional grid.
!
! mallocInteger        integer     OUT   function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
!                                        Thrown Exceptions:
!
!                                        3: maximum number of grids exceeded
!                                        4: unable to allocate memory
!
!                         <<REGISTRY>>
!
! NX                   integer      IN   inner (longitude) dimension of grid
!                                        to be allocated.
!
! NY                   integer      IN   outer (latitude) dimension of grid
!                                        to be allocated.
!
! NUMGRIDS             integer   INOUT   number of grids stored in registry.
!
! GRIDS(:)        GridInfoType     OUT   grid registry containing the
!                                        information of the stored grid.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, dimension(:,:), pointer :: dataType

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = GRIDAllocateErrorHandle

      rc = 0
      mallocInteger = -1

      if (NUMGRIDS .ge. MAXDIM) rc = 1
      if (isError(this,3,rc,method='mallocInteger') .ne. 0) return

      allocate(dataType(NX,NY),stat=rc)
      if (isError(this,4,rc,method='mallocInteger') .ne. 0) return

      NUMGRIDS = NUMGRIDS + 1
      GRIDS(NUMGRIDS)%id   = ID
      GRIDS(NUMGRIDS)%heap = iHEAP
      GRIDS(NUMGRIDS)%iptr => dataType
      nullify(GRIDS(NUMGRIDS)%rptr)

      mallocInteger = 0

      end function mallocInteger

!******************************************************************************
      integer function rotateCPCU(inData,outData)
!******************************************************************************

      implicit none

      type (CPCUDataType), intent(in) :: inData
      type (CPCUDataType), intent(inout) :: outData

      integer :: iShift

      rotateCPCU = 0

      iShift = GRIDrotate(inData%grid,outData%grid)

      outData%data = inData%data
      outData%data = cshift(outData%data,iShift,dim=1)

      end function rotateCPCU

!******************************************************************************
      integer function mrotateCPCU(inData,outData)
!******************************************************************************

      implicit none

      type (CPCUDataType), dimension(:), intent(in) :: inData
      type (CPCUDataType), dimension(:), intent(inout) :: outData

      integer :: i,iShift

      mrotateCPCU = 0

      do i = 1,size(inData)

        iShift = GRIDrotate(inData(i)%grid,outData(i)%grid)

        outData(i)%data = inData(i)%data
        outData(i)%data = cshift(outData(i)%data,iShift,dim=1)

      end do

      end function mrotateCPCU

!******************************************************************************
      integer function rotateCMAP(inData,outData)
!******************************************************************************

      implicit none

      type (CMAPDataType), intent(in) :: inData
      type (CMAPDataType), intent(inout) :: outData

      integer :: iShift

      rotateCMAP = 0

      iShift = GRIDrotate(inData%grid,outData%grid)

      outData%data = inData%data
      outData%data = cshift(outData%data,iShift,dim=1)

      end function rotateCMAP

!******************************************************************************
      integer function rotateGeneric(inData,outData)
!******************************************************************************

      implicit none

      type (GenericDataType), intent(in) :: inData
      type (GenericDataType), intent(inout) :: outData

      integer :: iShift

      rotateGeneric = 0

      iShift = GRIDrotate(inData%grid,outData%grid)

      outData%data = inData%data
      outData%data = cshift(outData%data,iShift,dim=1)

      end function rotateGeneric

!******************************************************************************
      integer function rotateGrid(fg,tg)
!******************************************************************************

      implicit none

      type (GridInfoType), intent(in) :: fg
      type (GridInfoType), intent(in) :: tg

      rotateGrid = 0

      if (.not. isValidGrid(fg)) return
      if (.not. isValidGrid(tg)) return

      rotateGrid = int( (tg%xStart - fg%xStart) / fg%dx)

      end function rotateGrid

!******************************************************************************
      logical function isValidGrid(grid)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GridInfoType), intent(in) :: grid

      isValidGrid = .false.
      if (grid%nx .le. 0) return
      if (grid%ny .le. 0) return
      if (grid%dx .le. 0) return
      if (grid%dy .le. 0) return

      isValidGrid = .true.

      end function isValidGrid

!******************************************************************************
      function gridCompare(grid1,grid2) result (boolean)
!******************************************************************************

      logical :: boolean
      type (GridInfoType), intent(in) :: grid1
      type (GridInfoType), intent(in) :: grid2

      boolean = .false.

      if (grid1%nx     .ne. grid2%nx) return
      if (grid1%ny     .ne. grid2%ny) return
      if (grid1%dx     .ne. grid2%dx) return
      if (grid1%dy     .ne. grid2%dy) return
      if (grid1%xStart .ne. grid2%xStart) return
      if (grid1%yStart .ne. grid2%yStart) return

      boolean = .true.

      end function gridCompare

!******************************************************************************
      function gridNotCompare(grid1,grid2) result (boolean)
!******************************************************************************

      logical :: boolean
      type (GridInfoType), intent(in) :: grid1
      type (GridInfoType), intent(in) :: grid2

      boolean = .false.
      if (grid1 .eq. grid2) return

      boolean = .true.

      end function gridNotCompare

!******************************************************************************
      subroutine freeAll()
!******************************************************************************
! English Name: Free All
! -------------
!
! Purpose: Frees/deallocates all memory currently referenced in the registry.
! -------- This clears all prior allocations and resets the registry.
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
!                         <<REGISTRY>>
!
! NUMGRIDS             integer    INOUT  On input, number of grids stored in 
!                                        registry. On output, set to zero.
!
! GRIDS(:)        GridInfoType    INOUT  On input, registry containing the
!                                        information of the stored grids. On
!                                        output, all registry information is
!                                        reset an initialized state.
!
! ID                   integer      OUT  grid pool counter for deriving
!                                        unique identifiers (reset to zero)
!
! iHEAP                integer      OUT  heap identifier (reset to zero)
!
! HEAP                 integer      OUT  heap pool counter for deriving
!                                        unique identifiers (reset to zero)
!
! iSTACK               integer      OUT  heap stack counter to track number
!                                        of opened heaps (reset to zero)
!
! STACK(MAXDIM)        integer      OUT  heap stack (emptied)
!
! Thrown Exceptions: none
! ------------------
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Local Variables
!     ---------------

      integer :: n

!     Deallocate memory and clear the registry.
!     =========================================

      do n = 1,NUMGRIDS

        GRIDS(n)%id   = -1
        GRIDS(n)%heap = -1
        if (associated(GRIDS(n)%iptr)) deallocate(GRIDS(n)%iptr)
        if (associated(GRIDS(n)%rptr)) deallocate(GRIDS(n)%rptr)

      end do

      ID = 0
      NUMGRIDS = 0

      HEAP   = 0
      iHEAP  = 0
      iSTACK = 0
      STACK  = 0

      end subroutine freeAll

!******************************************************************************
      subroutine freeHeap(heapID)
!******************************************************************************
! English Name: Free Heap
! -------------
!
! Purpose: Frees/deallocates all memory currently in the registry that is 
! -------- associated with the specified heap identifier.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The heap to be deleted must be the active heap. Heaps accessed
! ------    or allocated after the specified heap must be deallocated or
!           closed before invoking this function.
!
! See Also: GRIDCloseHeap()
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! heapID               integer       IN  Heap identifier associated with the
!                                        grids to be deallocated.
!
!                         <<REGISTRY>>
!
! NUMGRIDS             integer    INOUT  On input, number of grids stored in
!                                        registry. On output, number of grids
!                                        remaining after elimination of the
!                                        specified heap.
!
! GRIDS(:)        GridInfoType    INOUT  On input, registry containing the
!                                        information of the stored grids. On
!                                        output, registry information for the
!                                        grids remaining after elimination of
!                                        the specified heap.
!
! iHEAP                integer      OUT  Next heap identifier on the stack
!                                        (i.e. result of "popping" the stack).
!
! iSTACK               integer      OUT  Index of the top of the stack after
!                                        "popping".
!
! STACK(MAXDIM)        integer      OUT  heap stack after "popping".
!
! Thrown Exceptions: 
! ------------------
!
! 1: Specified heap is not the active heap.
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           01/11/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: heapID

!     Local Variables
!     ---------------

      integer :: rc
      integer :: n,k
      integer :: this = GRIDDeallocateErrorHandle

!     Heap to be deleted must be the active heap
!     ==========================================

      if (STACK(iSTACK) .ne. heapID) rc = 1
      if (isError(this,1,rc,method='freeHeap') .ne. 0) return

!     Delete grids with the specified heap ID
!     from the grid registry.
!     =======================================

      k = 0
      do n = 1,NUMGRIDS

        if (GRIDS(n)%heap .eq. heapID) then

          GRIDS(n)%id   = -1
          GRIDS(n)%heap = -1
          if (associated(GRIDS(n)%iptr)) deallocate(GRIDS(n)%iptr)
          if (associated(GRIDS(n)%rptr)) deallocate(GRIDS(n)%rptr)

        else

          k = k + 1
          GRIDS(k) = GRIDS(n)

        endif

      end do

      NUMGRIDS = k

!     Pop the heap stack (next entry
!     will now be the active heap)
!     ==============================

      STACK(iSTACK) = 0
      iSTACK = max(0,iSTACK-1)
      iHEAP = STACK(iSTACK)

      end subroutine freeHeap

!******************************************************************************
      subroutine GRIDCloseHeap()
!******************************************************************************
! English Name: Close Heap
! -------------
!
! Purpose: Closes the currently active heap. Subsequent grid management will
! -------- reference the heap activated prior to this one. 
!
! Language: Fortran 90
! ---------
!
! See Also: mallocHeap(), freeHeap()
! --------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
!                         <<REGISTRY>>
!    
! iHEAP                integer      OUT  Next heap identifier on the stack
!                                        (i.e. result of "popping" the stack).
!
! iSTACK               integer    INOUT  On input, index of the currently
!                                        active heap. On output, index of the
!                                        next heap on the stack.
!
! STACK(MAXDIM)        integer    INOUT  heap stack after "popping".
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/17/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Pop the heap stack (next entry
!     will now be the active heap)
!     ==============================

      STACK(iSTACK) = 0
      iSTACK = max(0,iSTACK-1)
      iHEAP = STACK(iSTACK)

      end subroutine GRIDCloseHeap

!******************************************************************************
      subroutine freeGrid(grid)
!******************************************************************************

      implicit none

      type (GRIDInfoType), intent(in) :: grid

      integer :: n,k

      k = 0
      do n = 1,NUMGRIDS

        if (GRIDS(n)%id .eq. grid%id) then

          GRIDS(n)%id   = -1
          GRIDS(n)%heap = -1
          if (associated(GRIDS(n)%iptr)) deallocate(GRIDS(n)%iptr)
          if (associated(GRIDS(n)%rptr)) deallocate(GRIDS(n)%rptr)

        else

          k = k + 1
          GRIDS(k) = GRIDS(n)

        endif

      end do

      NUMGRIDS = k

      end subroutine freeGrid
!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,method)
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
! method                string   OPT,IN  identifier for the method throwing
!                                        the exception.
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
      character(len=*), intent(in), optional :: method

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

!       Grid Register Error Handle
!       ==========================

        case (GRIDRegisterErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GRIDRegister (error#',code,'): uallocatable dimensions'

          case default

            print *, ': unknown error'

        end select

!       Grid Allocate Error Handle
!       ==========================

        case (GRIDAllocateErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GRIDAllocate (error#',code,'): ', \
                     'error registering grid. Method = ',method
          case (2)

            print *, 'GRIDAllocate (error#',code,'): ', \
                     'error allocating space for grid. Method = ',method

          case (3)

            print *, 'GRIDAllocate (error#',code,'): ', \
                     'maximum number of grids exceeded. Method = ',method

          case (4)

            print *, 'GRIDAllocate (error#',code,'): ', \
                     'unable to allocate memory. Method = ',method

          case (5)

            print *, 'GRIDAllocate (error#',code,'): ', \
                     'stack dimension exceeded. Method = ',method

          case (6)

            print *, 'GRIDAllocate (error#',code,'): ', \
                     'error during allocation. Method = ',method

          case default

            print *, 'GRIDAllocate (error#',code,'): unknown error', \
                     ' Method = ',method

        end select

        case (GRIDDeallocateErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'GRIDDeallocate (error#',code,'): ', \
                     'Heap to be deleted must be the active heap. ', \
                     'One or more other heaps were not closed. ', \
                     'Method = ',method

          case default

            print *, 'GRIDDeallocate (error#',code,'): unknown error', \
                     ' Method = ',method

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

      end module GRIDMod
