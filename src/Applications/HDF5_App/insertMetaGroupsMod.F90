!******************************************************************************
      module insertMetaGroupsMod
!******************************************************************************

      use typesMod
      use HDF5FormatMod, DAACError=>errorHandler

      private

      public :: getArgs
      public :: METAopen
      public :: METAread
      public :: METAwrite
      public :: METAclose
      public :: errorHandler

      public :: CommandLineType

      integer, parameter :: ReadErrorHandle         = 1
      integer, parameter :: WriteErrorHandle        = 2
      integer, parameter :: OpenErrorHandle         = 3
      integer, parameter :: GetArgsErrorHandle      = 4
      integer, parameter :: OpenNamelistErrorHandle = 5

      type MetaDataType

        integer :: num_groups
        type (GroupInfoType), dimension(MAX_DEPTH) :: groups

      end type MetaDataType

      type CommandLineType

        character (len=MAX_STR_LEN) namelist
        character (len=MAX_STR_LEN) HDF5Fname

      end type CommandLineType

      interface closeGroups

        module procedure closeGroupsAll
        module procedure closeGroupsListed

      end interface closeGroups

      integer :: EXCEPTION = 0
      type (MetaDataType) :: META
      character (len=MAX_STR_LEN) :: NAMELIST_FILE

      contains

!******************************************************************************
      integer function METAopen(filename)
!******************************************************************************
! English Name: Open
! -------------
!
! Purpose: Opens the input files required for inserting metadata into the
! -------- specified HDF file.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. This method opens three files: (1) the specified HDF file, (2) the
! ------    namelist file specified on the command-line and (3) the companion
!           configuration file with the same base name as the HDF file suffixed
!           with ".cfg". The configuration file contains the values for 
!           resolving dynamic metadata parameters in the namelist.
!
!        2. getArgs() is a prerequisite
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  File name of the HDF file to receive
!                                        the metadata (see note-1).
!
! METAopen             integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!     
!                                        1: error opening HDF file
!                                        2: error opening namelist containing
!                                           input metadata
!
! NAMELIST_FILE         string       IN  File name of namelist file containing
!                                        the metadata (see getArgs()).
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/06/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      character (len=MAX_STR_LEN), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = OpenErrorHandle

!     Initialize
!     ==========

      META%num_groups = 0
      META%groups(1)%name = ' '

!     Open the HDF file to receive the
!     metadata.
!     ===============================

      rc = DAACopen(filename)
      if (isError(this,1,rc,arg=filename) .ne. 0) return

!     Open the namelist
!     =================

      rc = openNamelist()
      if (isError(this,2,rc,arg=NAMELIST_FILE) .ne. 0) return

      METAopen = 0

      end function METAopen

!******************************************************************************
      integer function getArgs(args)
!******************************************************************************
! English Name: Get Arguments
! -------------
!
! Purpose: Provides interface for retrieving command-line arguments.
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
! args         CommandLineType      OUT  retrieved command-line arguments.
!
! getArgs              integer      OUT  function return value:
!
!                                        0: normal
!                                        1: no arguments specified (assume
!                                           help is needed)
!                                       -1: exception thrown
!
!                                        Thrown Exceptions:
!
!                                        1: user requested usage help
!                                        2: invalid argument
!                                        3: incorrect number of arguments
!
! NAMELIST_FILE         string      OUT  copy of specified namelist argument. 
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Synoptic Evaluation Group)
!
! Modified:       Date           Author  Description
! ---------   
!           03/08/2013      J.Ardizzone  created.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (CommandLineType), intent(out) :: args

!     Local Variables
!     ---------------

      integer :: rc
      character (len=MAX_STR_LEN) :: str
      integer :: k,n,len,nargs
      integer :: this = GetArgsErrorHandle

!     Retrieve Arguments
!     ==================

      k     = 1
      nargs = command_argument_count()

!     Simply print usage if no
!     arguments specified
!     ========================

      if (nargs .eq. 0) then
        getArgs = 1
        call usage(1)
        return
      endif

      rc      = 0
      getArgs = -1

      do n = 1,nargs

        rc = 0

        call get_command_argument(n, str)
        len = len_trim(str)

!       Trap optional arguments
!       -----------------------

        if (str(1:len) .eq. '-h' .or. str(1:len) .eq. '-help') then 

          getArgs = 1
          call usage(1)
          return

        elseif (str(1:1) .eq. '-') then

          rc = 1
          if (isError(this,2,rc,arg=str) .ne. 0) return

!       Trap mandatory arguments in order
!       ---------------------------------

        elseif (k .eq. 1) then

          k = k + 1
          call get_command_argument(n,args%namelist)
          NAMELIST_FILE = args%namelist

        elseif (k .eq. 2) then

          k = k + 1
          call get_command_argument(n,args%HDF5Fname)

        else

          rc = 1
          if (isError(this,3,rc) .ne. 0) return

        endif

      end do

!     Check number of mandatory arguments
!     ===================================

      if (k-1 .ne. 2) rc = 1
      if (isError(this,3,rc) .ne. 0) return

      getargs = 0

      end function getargs

!******************************************************************************
      integer function openNamelist()
!******************************************************************************
! English Name: Open Namelist
! -------------
!
! Purpose: Establishes I/O handle for the input namelist.
! --------
!
! Language: Fortran 90
! ---------
!
! See Also: getArgs(), closeNamelist()
! ---------
!
! Notes: 1. "getArgs()" is a prerequisite
! ------
!        2. This method currently reserves unit 5 for namelist I/O
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! openNamelist         integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error accessing namelist file
!
! NAMELIST_FILE         string       IN  filename of the input namelist file
!                                        (see getArgs()).
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

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = OpenNamelistErrorHandle

      openNamelist = -1

      open (unit=5,file=NAMELIST_FILE,status='old',iostat=rc)
      if (isError(this,1,rc,arg=NAMELIST_FILE) .ne. 0) return

      openNamelist = 0

      end function openNamelist

!******************************************************************************
      integer function closeNamelist()
!******************************************************************************

      implicit none

      close (unit=5)
      closeNamelist = 0

      end function closeNamelist

!******************************************************************************
      integer function METAread(attribute)
!******************************************************************************
! English Name: Read Metadata Information
! -------------
!
! Purpose: Reads "meta" namelist block.
! --------
!
! See Also: openNamelist(), closeNamelist()
! ---------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. "openNamelist()" is a prerequisite.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! attribute  AttributeDataType      OUT  data structure containing the contents
!                                        of the "meta" namelist block.
!
! METAread             integer	    OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!                                       -2: end of namelist
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
!           04/05/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (AttributeDataType), intent(out) :: attribute

!     Local Variables
!     ---------------

      integer :: rc
      integer :: i,ilen
      integer :: this = ReadErrorHandle

!     Namelist
!     --------

      character (len=MAX_STR_LEN) :: name
      character (len=MAX_STR_LEN), dimension(MAX_NUM_ATTR) :: value

      integer :: int_size
      integer :: real_size
      integer :: uint_size

      namelist /meta/ name,value,int_size,real_size,uint_size

!     Initialize
!     ==========

      name      = ' '
      value     = ' '
      real_size = 0
      int_size  = 0
      uint_size = 0

!     Read namelist block
!     ===================

      METAread = -2
      read(5,nml=meta,iostat=rc)
      if (rc .lt. 0) return

      METAread = -1
      if (isError(this,1,rc) .ne. 0) return

      call METAdeallocate(attribute)

!     Record namelist information
!     ===========================

      attribute%name      = name
      attribute%long_name = name
      attribute%value     = value
      attribute%bit_size = (/real_size,int_size,uint_size/)

!     The attribute name is the basename of
!     the fully specified "group" path.
!     =====================================

      i    = index(name,'/',back=.true.)
      ilen = len_trim(name)
      if (i .gt. 0) attribute%name = name(i+1:ilen)

!     No negative bit sizes. Should also
!     check for illegal positive values.

      where (attribute%bit_size .lt. 0) attribute%bit_size = 0

!     Allocate space for vector attributes
!     ====================================

      rc = METAallocate(attribute)

      Metaread = 0

      end function METAread

!******************************************************************************
      integer function METAallocate(attribute)
!******************************************************************************

      implicit none

      type (AttributeDataType), intent(inout) :: attribute

!     Local Variables
!     ---------------

      integer :: rc
      integer :: num_value
      integer, dimension(3) :: bit_size

      if ( .not. isaVector(attribute)) return

      num_value = METAcount(attribute)

      bit_size = attribute%bit_size

      if (bit_size(1) .eq. 32) allocate(attribute%f32(num_value),stat=rc)
      if (bit_size(1) .eq. 64) allocate(attribute%f64(num_value),stat=rc)

      if (bit_size(2) .eq. 32) allocate(attribute%i32(num_value),stat=rc)
      if (bit_size(2) .eq. 16) allocate(attribute%i16(num_value),stat=rc)
      if (bit_size(2) .eq.  8) allocate(attribute%i8 (num_value),stat=rc)

      if (bit_size(3) .eq. 32) allocate(attribute%u32(num_value),stat=rc)
      if (bit_size(3) .eq. 16) allocate(attribute%u16(num_value),stat=rc)
      if (bit_size(3) .eq.  8) allocate(attribute%u8 (num_value),stat=rc)

      allocate(attribute%str(num_value),stat=rc)

      rc = METAparse(attribute)

      METAallocate = 0

      end function METAallocate

!******************************************************************************
      subroutine METAdeallocate(attribute)
!******************************************************************************

      implicit none

      type (AttributeDataType), intent(inout) :: attribute

      if (associated(attribute%f32)) deallocate(attribute%f32)
      if (associated(attribute%f64)) deallocate(attribute%f64)
      if (associated(attribute%i8 )) deallocate(attribute%i8 )
      if (associated(attribute%i16)) deallocate(attribute%i16)
      if (associated(attribute%i32)) deallocate(attribute%i32)
      if (associated(attribute%int)) deallocate(attribute%int)
      if (associated(attribute%u8 )) deallocate(attribute%u8 )
      if (associated(attribute%u16)) deallocate(attribute%u16)
      if (associated(attribute%u32)) deallocate(attribute%u32)
      if (associated(attribute%str)) deallocate(attribute%str)

      end subroutine METAdeallocate

!******************************************************************************
      integer function METAcount(attribute)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (AttributeDataType), intent(in) :: attribute

!     Local Variables
!     ---------------

      integer :: n
      character (len=MAX_STR_LEN) :: value

      METAcount = MAX_NUM_ATTR

      do n = MAX_NUM_ATTR,1,-1

        value = attribute%value(n)
        if (len_trim(value) .gt. 0) exit

        METAcount = METAcount - 1

      end do

      end function METAcount

!******************************************************************************
      integer function METAparse(attribute)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (AttributeDataType), intent(inout) :: attribute

!     Local Variables
!     ---------------

      integer :: num_attr

      num_attr = size(attribute%str)
      attribute%str = attribute%value(1:num_attr)

      METAparse = num_attr

      end function METAparse

!******************************************************************************
      integer function isaVector(attribute)
!******************************************************************************

!     Argument List
!     -------------

      type (AttributeDataType), intent(in) :: attribute

      isaVector = 0
      if (METAcount(attribute) .le. 1) return 

      isaVector = 1

      end function isaVector

!******************************************************************************
      integer function METAwrite(attribute)
!******************************************************************************
! English Name: Write
! -------------
!
! Purpose: 
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
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/06/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (AttributeDataType), intent(inout) :: attribute

!     Local Variables
!     ---------------

      integer :: rc
      integer :: n,num_groups
      integer :: this = WriteErrorHandle

      type (GroupInfoType) :: group

!     Create the "group" path of the attribute
!     ========================================

      METAwrite = -1

      n = 0
      num_groups = META%num_groups

      do while (nextGroup(attribute%long_name,group))
        
        n = n + 1
        if (trim(META%groups(n)%name) .eq. trim(group%name)) cycle

        if (num_groups .gt. 0) call closeGroups(META%groups(n:num_groups))

        num_groups = 0

        rc = DAACwrite(group)
        if (isError(this,1,rc,arg=group%name) .ne. 0) return

        META%groups(n)  = group

      end do

      META%num_groups = n

!     Now write out the attribute value
!     =================================

      rc = DAACwrite(attribute)

      METAwrite = 0

      end function METAwrite

!******************************************************************************
      integer function METAclose()
!******************************************************************************

      implicit none

      integer :: rc

      call closeGroups()
      rc = DAACclose()
      rc = closeNamelist()

      METAclose = 0

      end function METAclose

!******************************************************************************
      logical function nextGroup(path,group)
!******************************************************************************

      implicit none

      character (len=*), intent(in) :: path
      type (GroupInfoType), intent(out) :: group

      integer :: i
      integer, save :: iend
      integer, save :: istart
      character (len=MAX_STR_LEN), save :: activePath

      nextGroup = .false.

      if (trim(activePath) .ne. trim(path)) then

        activePath = path

        istart = 1
        iend   = index(trim(path),'/',back=.true.)
        if (iend .le. 0) return

      endif

      if (istart .gt. iend) return

      i = index(path(istart:iend),'/')
      if (i .le. 0) i = iend - istart + 2

      group%name = path(istart:istart+i-2)
      istart = istart + i

      nextGroup = .true.

      end function nextGroup
      
!******************************************************************************
      subroutine closeGroupsListed(groups)
!******************************************************************************

      implicit none

      type (GroupInfoType), dimension(:), intent(inout) :: groups

      integer :: rc
      integer :: n,num_groups

      num_groups = size(groups)

      do n = num_groups,1,-1
        rc = DAACclose(groups(n))
      end do

      end subroutine closeGroupsListed

!******************************************************************************
      subroutine closeGroupsAll()
!******************************************************************************

      implicit none

      if (META%num_groups .le. 0) return
      call closeGroups(META%groups(1:META%num_groups))

      META%num_groups = 0

      end subroutine closeGroupsAll

!******************************************************************************
      subroutine usage(errorEvent)
!******************************************************************************
! English Name: Usage
! -------------
!
! Purpose: Prints the command-line arguments
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
! errorEvent           integer       IN  Non-zero error code defining the
!                                        error condition motivating the usage
!                                        print.
!
! EXCEPTION            integer      OUT exception code (set to "errorEvent")
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/08/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: errorEvent

!     Print Usage
!     ===========
                                                                                
      print *, ' '
      print *, 'Usage: insert_iso_groups (-h | -help) [namelist] [HDF5 Filename]'

      print *, ' '
      print *, 'where'
      print *, ' '
      print *, '(-h | -help) - prints this summary.'
      print *, 'namelist - namelist describing the HDF "group" metadata'
      print *, 'HDF5 Filename - name of file to be augmented with ISO'
      print *, 'metadata groups. A configuration file with the same base'
      print *, 'name suffixed with ".cfg" is also expected as input.'

      EXCEPTION = errorEvent

      end subroutine usage

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

!       GetArgs Error Handle
!       ====================

        case (GetArgsErrorHandle)

        select case (errorEvent)

          case (1)

            call usage(1)

          case (2)

            print *, 'getArgs (error#',code,'): invalid argument: ', &
                                                        '"',trim(arg),'"'
            call usage(2)

          case (3)

            print *, 'getArgs (error#',code,'): incorrect number of arguments'
            call usage(3)

          case default

            print *, 'getArgs (error#',code,'): unknown error'

        end select

!       Open Error Handle
!       =================

        case (OpenErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'METAopen (error#',code,'): error accessing HDf file: ', &
                                                              '"',trim(arg),'"'

          case (2)

            print *, 'METAopen (error#',code,'): error accessing namelist: ', &
                                                              '"',trim(arg),'"'

          case default

            print *, 'METAopen (error#',code,'): unknown error'

        end select

!       openNamelist Error Handle
!       =========================

        case (OpenNamelistErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'openNamelist (error#',code,'): error accessing ', &
                                                 'namelist: "',trim(arg),'"'
          case default

            print *, 'openNamelist (error#',code,'): unknown error'

        end select

!       Read Error Handle
!       =================

        case (ReadErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'METAread (error#',code,'): error reading ', &
                                             'metafile namelist block'
          case default

            print *, 'METAread (error#',code,'): unknown error'

        end select

!       Write Error Handle
!       ==================

        case (WriteErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'METAwrite (error#',code,'): error writing group: ', &
                                                            '"',trim(arg),'"' 
          case default

            print *, 'METAwrite (error#',code,'): unknown error'

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

      end module insertMetaGroupsMod
