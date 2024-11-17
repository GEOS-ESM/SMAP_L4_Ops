!******************************************************************************
      module interfaceMod
!******************************************************************************
! English Name: Interface 
! -------------
!
! Purpose: Provides the user interface routines for main drivers.
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
! getArgs()           function      PUB  Retrieves command-line arguments.
!
! openNamelist()      function      PUB  Establishes I/O handle for the
!                                        input namelist.
!
! readFileInfo()      function      PUB  Reads "file" namelist block.
!
! readGroupInfo()     function      PUB  Reads "group" namelist block.
!
! readDataInfo()      function      PUB  Reads "element" namelist block.
!
! closeNamelist()     function      PUB  Closes I/O handle for opened namelist
!                                        file.
!
! errorHandler()      function      PUB  Module error handler for trapping
!                                        thrown exceptions. Recast this
!                                        method to avoid conflict with other
!                                        module error handlers.
!
! CommandLineType      derived      PUB  data structure for storing command-
!                                        line arguments.
!
!   startDate          integer        *  initial date (ccyymmdd).
!   startTime          integer        *  initial time (hhmmss).
!   endDate            integer        *  ending date (ccyymmdd).
!   endTime            integer        *  ending time (hhmmss).
!   timeStep           integer        *  time step in seconds.
!   namelist            string        *  namelist describing the LDAS collection
!   LDASFnameTmplt      string        *  filename template for input LDAS file.
!   DAACFnameTmplt      string        *  filename template for output DAAC file.
!
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Synoptic Evaluation Group)
!
! Modified:       Date           Author  Description
! ---------   
!           03/08/2013      J.Ardizzone  created.
!******************************************************************************

      use typesMod
      use GPD_Mod, gridError=>errorHandler

      private

      public :: getArgs
      public :: openNamelist
      public :: readFileInfo
      public :: readGroupInfo
      public :: readDataInfo
      public :: closeNamelist
      public :: errorHandler

      public :: CommandLineType

      real, parameter :: DEFAULT_FILL = 1.0e+15

      integer, parameter :: GetArgsErrorHandle       = 1
      integer, parameter :: OpenNamelistErrorHandle  = 2
      integer, parameter :: ReadFileInfoErrorHandle  = 3
      integer, parameter :: ReadGroupInfoErrorHandle = 4
      integer, parameter :: ReadDataInfoErrorHandle  = 5

      type CommandLineType

        integer startDate
        integer startTime
        integer endDate
        integer endTime
        integer timeStep

        character (len=MAX_STR_LEN) namelist
        character (len=MAX_STR_LEN) LDASFnameTmplt
        character (len=MAX_STR_LEN) DAACFnameTmplt

      end type CommandLineType

      integer :: EXCEPTION = 0
      integer :: ELEMENT_COUNT = 0
      character (len=MAX_STR_LEN) :: NAMELIST_FILE

      contains

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
!                                        4: invalid option
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

      getArgs = -1

      do n = 1,nargs

        rc = 0

        call get_command_argument(n,str)
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
          call get_command_argument(n,str)
          read(unit=str(1:len),fmt='(i)') args%startDate
          print *, 'startDate = ',args%startDate
          if (args%startDate .lt. 19000000) rc = 1
          if (isError(this,2,rc,arg=str) .ne. 0) return

        elseif (k .eq. 2) then

          k = k + 1
          call get_command_argument(n,str)
          read(unit=str(1:len),fmt='(i)') args%startTime
          if (args%startTime .lt. 0) rc = 1
          if (isError(this,2,rc,arg=str) .ne. 0) return

        elseif (k .eq. 3) then

          k = k + 1
          call get_command_argument(n,str)
          read(unit=str(1:len),fmt='(i)') args%endDate
          if (args%endDate .lt. 19000000) rc = 1
          if (isError(this,2,rc,arg=str) .ne. 0) return

        elseif (k .eq. 4) then

          k = k + 1
          call get_command_argument(n,str)
          read(unit=str(1:len),fmt='(i)') args%endTime
          if (args%endTime .lt. 0) rc = 1
          if (isError(this,2,rc,arg=str) .ne. 0) return

        elseif (k .eq. 5) then

          k = k + 1
          call get_command_argument(n,str)
          read(unit=str(1:len),fmt='(i)') args%timeStep
          if (args%timeStep .le. 0) rc = 1
          if (isError(this,2,rc,arg=str) .ne. 0) return

        elseif (k .eq. 6) then

          k = k + 1
          call get_command_argument(n,args%namelist)
          NAMELIST_FILE = args%namelist

        elseif (k .eq. 7) then

          k = k + 1
          call get_command_argument(n,args%LDASFnameTmplt)

        elseif (k .eq. 8) then

          call get_command_argument(n,args%DAACFnameTmplt)

        else

          rc = 1
          if (isError(this,3,rc) .ne. 0) return

        endif

      end do

!     Check number of mandatory arguments
!     ===================================

      if (k .ne. 8) rc = 1
      if (isError(this,3,rc) .ne. 0) return

!     There is currently no argument reserved
!     for specifying the GRID type so we just
!     initialize to the default for now.
!     =======================================

      rc = GPDsetGrid()

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
! Notes: 1, "getArgs()" is a prerequisite
! ------
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
      integer function readFileInfo(fileInfo)
!******************************************************************************
! English Name: Read File Information
! -------------
!
! Purpose: Reads "file" namelist block.
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
! fileInfo        FileInfoType      OUT  data structure containing the contents
!                                        of the "file" namelist block.
!
! readFileInfo         integer	    OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
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
!           03/09/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (FileInfoType), intent(out) :: fileInfo

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = ReadFileInfoErrorHandle

      character (len=MAX_STR_LEN) :: title
      character (len=MAX_STR_LEN) :: history
      character (len=MAX_STR_LEN) :: conventions
      character (len=MAX_STR_LEN) :: institution
      character (len=MAX_STR_LEN) :: contact
      character (len=MAX_STR_LEN) :: references
      character (len=MAX_STR_LEN) :: source
      character (len=MAX_STR_LEN) :: comment

      namelist /file/ title,history,conventions,institution,contact, &
                      references,source,comment

      readFileInfo = -1

      title       = ' '
      history     = ' '
      conventions = ' '
      institution = ' '
      contact     = ' '
      references  = ' '
      source      = ' '
      comment     = ' '

      read(5,nml=file,iostat=rc)
      if (isError(this,1,rc) .ne. 0) return

      fileInfo%title       = title
      fileInfo%history     = history
      fileInfo%conventions = conventions
      fileInfo%institution = institution
      fileInfo%contact     = contact
      fileInfo%references  = references
      fileInfo%source      = source
      fileInfo%comment     = comment

      readFileInfo = 0

      end function readFileInfo

!******************************************************************************
      integer function readGroupInfo(groupInfo)
!******************************************************************************
! English Name: Read Group Information
! -------------
!
! Purpose: Reads "group" namelist block.
! --------
!
! See Also: openNamelist(), readFileInfo(), closeNamelist()
! ---------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. "openNamelist()" is a prerequisite.
! ------ 2. "readFileInfo()" is a prerequisite.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! groupInfo      GroupInfoType      OUT  data structure containing the contents
!                                        of the "group" namelist block.
!
! readGroupInfo        integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!                                       -2: end of file
!
!                                        Thrown Exceptions
!
!                                        1: error reading namelist
!
! ELEMENT_COUNT        integer      OUT  number of elements in the group as
!                                        specified in the namelist
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

      type (GroupInfoType), intent(out) :: groupInfo

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = ReadGroupInfoErrorHandle

      character (len=MAX_STR_LEN) :: name
      character (len=MAX_STR_LEN) :: coordinates
      integer :: num_elements
      integer :: shave_bits

      namelist /group/ name,num_elements,shave_bits,coordinates

      name = ' '
      coordinates = ' '
      num_elements = 0
      shave_bits = -1
      ELEMENT_COUNT = 0

      readGroupInfo = -2
      read(5,nml=group,iostat=rc)
      if (rc .lt. 0) return

      readGroupInfo = -1
      if (isError(this,1,rc) .ne. 0) return

      groupInfo%name         = name
      groupInfo%coordinates  = coordinates
      groupInfo%num_elements = num_elements
      groupInfo%shave_bits   = shave_bits

      ELEMENT_COUNT = num_elements

      readGroupInfo = 0

      end function readGroupInfo

!******************************************************************************
      integer function readDataInfo(dataInfo)
!******************************************************************************
! English Name: Read Data Information
! -------------
!
! Purpose: Reads "element" namelist block. The word "element" is used to refer
! -------- to scientific data sets (SDSs) stored in hierarchical data formats.
!
! See Also: openNamelist(), readGroupInfo(), closeNamelist()
! ---------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. "openNamelist()" is a prerequisite.
! ------ 2. "readGroupInfo()" is a prerequisite.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! dataInfo        DataInfoType      OUT  data structure containing the contents
!                                        of the "element" namelist block.
!
! readDataInfo         integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!                                       -2: end of data
!
!                                        Thrown Exceptions
!
!                                        1: error reading namelist
!
! ELEMENT_COUNT        integer    INOUT  on input, number of elements in the 
!                                        group; on output, number of elements
!                                        remaining.
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

      type (DataInfoType), intent(out) :: dataInfo

!     Local Variables
!     ---------------

      integer :: rc
      integer :: this = ReadDataInfoErrorHandle

      character (len=MAX_STR_LEN)  :: name
      character (len=MAX_STR_LEN)  :: long_name
      character (len=MAX_STR_LEN)  :: standard_name
      character (len=MAX_STR_LEN)  :: units
      character (len=MAX_STR_LEN)  :: axis
      character (len=MAX_STR_LEN)  :: coordinates
      character (len=MAX_STR_LEN)  :: fill_value
      character (len=MAX_STR_LEN)  :: valid_max
      character (len=MAX_STR_LEN)  :: valid_min

      integer :: int_size
      integer :: real_size
      integer :: uint_size
      integer :: str_size
      integer :: shave_bits

      namelist /element/ name,long_name,units,coordinates, &
                         fill_value,valid_max,valid_min, &
                         real_size,int_size,uint_size,str_size, &
                         shave_bits,standard_name,axis

      readDataInfo = -2
      if (ELEMENT_COUNT .eq. 0) return

      readDataInfo = -1

      long_name       = ' '
      standard_name   = ' '
      units           = ' '
      axis            = ' '
      coordinates     = ' '
      valid_min       = ' '
      valid_max       = ' '
      fill_value      = ' '
      real_size       = 0
      int_size        = 0
      uint_size       = 0
      str_size        = 0
      shave_bits      = -1

      read(5,nml=element,iostat=rc)
      if (isError(this,1,rc) .ne. 0) return

      dataInfo%name          = name
      dataInfo%long_name     = long_name
      dataInfo%standard_name = standard_name
      dataInfo%units         = units
      dataInfo%axis          = axis
      dataInfo%coordinates   = coordinates
      dataInfo%fill_value    = fill_value
      dataInfo%valid_max     = valid_max
      dataInfo%valid_min     = valid_min
      dataInfo%bit_size      = (/real_size,int_size,uint_size,str_size/)

      if (shave_bits .ge. 0) dataInfo%shave_bits = shave_bits

!     String size is specified as bytes.
!     Convert to bits.

      dataInfo%bit_size(4) = dataInfo%bit_size(4) * 8

      where (dataInfo%bit_size .lt. 0) dataInfo%bit_size = 0
      if (maxval(dataInfo%bit_size) .eq. 0) dataInfo%bit_size(1) = 32

      ELEMENT_COUNT = ELEMENT_COUNT - 1

      readDataInfo = 0

      end function readDataInfo

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
      print *, 'Usage: ldas2daac (-h | -help) [start date] [start time] ', &
               '[end date] [end time] [time step] [namelist] ', &
               '[LDAS input file] [DAAC output file]'

      print *, ' '
      print *, 'where'
      print *, ' '
      print *, '(-h | -help) - prints this summary.'
      print *, 'start date - initial date (ccyymmdd).'
      print *, 'start time - initial time (hhmmss).'
      print *, 'end date - ending date (ccyymmdd).'
      print *, 'end time - ending time (hhmmss).'
      print *, 'time step - time increment seconds.'
      print *, 'namelist - namelist describing the LDAS collection.'
      print *, 'LDAS input file - filename template for input LDAS file.'
      print *, 'DAAC output file - filename template for output DAAC file.'
      print *, ' '
      print *, '**Templates use the Unix Date command conventions (e.g. %Y%m%d)'

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

!       getArgs Error Handle
!       ====================

        case (GetArgsErrorHandle)

        select case (errorEvent)

          case (1)

            call usage(1)

          case (2)

            print *, 'getArgs (error#',code,'): invalid argument: ', &
                                                    arg(1:len_trim(arg))
            call usage(2)

          case (3)

            print *, 'getArgs (error#',code,'): incorrect number of arguments'
            call usage(3)

          case default

            print *, 'getArgs (error#',code,'): unknown error'

        end select

!       openNamelist Error Handle
!       =========================

        case (OpenNamelistErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'openNamelist (error#',code,'): error accessing ', &
                                          'namelist: ', arg(1:len_trim(arg))
          case default

            print *, 'openNamelist (error#',code,'): unknown error'

        end select

!       readFileInfo Error Handle
!       =========================

        case (ReadFileInfoErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'readFileInfo (error#',code,'): error reading ', &
                                                   '"file" namelist block'
          case default

            print *, 'readFileInfo (error#',code,'): unknown error'

        end select

!       readGroupInfo Error Handle
!       ==========================

        case (ReadGroupInfoErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'readGroupInfo (error#',code,'): error reading ', &
                                                    '"group" namelist block'
          case default

            print *, 'readGroupInfo (error#',code,'): unknown error'

        end select

!       readDataInfo Error Handle
!       =========================

        case (ReadDataInfoErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'readDataInfo (error#',code,'): error reading ', &
                                                    '"element" namelist block'
          case default

            print *, 'readDataInfo (error#',code,'): unknown error'

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

      end module interfaceMod
