      module merge_CorrectorMod

      use TypesMod
      use GridMod, GridError=>errorHandler
      use LFOBaseMod, LFOBaseError=>errorHandler

      private
      public :: getArgs
      public :: correctPrecip
      public :: UserInterfaceType
      public :: errorHandler

      integer :: IUMASK = 61

      integer, parameter :: CorrectPrecipErrorHandle = 1
      integer, parameter :: GetArgsErrorHandle = 2
      integer, parameter :: MergeCorrectionErrorHandle = 3

      type UserInterfaceType

        integer :: start_date,end_date,start_time

        character(len=1024) :: config
        character(len=1024) :: file1
        character(len=1024) :: file2
        character(len=1024) :: out_file

      end type UserInterfaceType

      integer :: errorCode = 0

      contains

!******************************************************************************
      integer function correctPrecip(idate)
!******************************************************************************
! English Name: Correct Precipitation
! -------------
!
! Purpose: Implements the precipitation corrector API. This corrector merges
! -------- data from two correction algorithms. 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The first filename (file1) references the primary corrected file.
! ------    Data from this file is retained on output where values are defined.
!           Values from the second file are used to replace values in the first
!           (primary) file where data is missing (undefined).
!
!        2. The starting hour is assumed to be 6:30Z of the specified day. This
!           is the convention used for CPCU corrected data. The hourly increment
!           is assumed to be 1-hour. See "getArgs()" for information on
!           optionally specifying the starting time.
!
!        3. This corrector corrects 1 entire day. For example: Jan 1, 2014 6:30Z
!           to January 2, 2014 5:30Z.
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
! exception            integer      OUT  Thrown Exceptions:
!
!                                        1: error retrieving command-line args
!                                        2: error reading corrected LFO data
!                                        3: error merging the corrected data
!                                        4: error writing the merged data
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/03/2015      J.Ardizzone  created.
!******************************************************************************
      implicit none
      integer :: this = CorrectPrecipErrorHandle

!     Argument List
!     -------------

      integer, intent(in) :: idate

!     Local Variables
!     ---------------

      integer :: rc
      integer :: itime
      integer, save :: heap = 0

      type (UserInterfaceType) :: args
      type (LFODataType), save, dimension(24) :: lfo1,lfo2

      correctPrecip = -1

!     Retrieve command-line arguments
!     and initialize.
!     ===============================

      rc = getArgs(args)
      if (isError(this,1,rc) .ne. 0) return

      itime = args%start_time

      heap = GRIDAllocate(heap)

!     Read in LFO corrected data
!     ==========================

      rc = LFOReadDay(args%file1,idate,itime,lfo1)
      if (isError(this,2,LFOBaseError(),arg=args%file1) .ne. 0) return

      rc = LFOReadDay(args%file2,idate,itime,lfo2)
      if (isError(this,2,LFOBaseError(),arg=args%file2) .ne. 0) return

      rc = mergeCorrection(lfo1,lfo2)
      if (isError(this,3,rc) .ne. 0) return

      print *, 'Writing corrected precipitation data for ',idate

      rc = LFOWriteFile(args%out_file,lfo1)
      if (isError(this,4,rc) .ne. 0) return

      call GRIDDeallocate(heap)

      correctPrecip = 0

      end function correctPrecip

!******************************************************************************
      integer function mergeCorrection(lfo1,lfo2)
!******************************************************************************
! English Name: Merge Correction
! -------------
!
! Purpose: Combines corrected precipitation data derived from two methods.
! -------- Missing (undefined) values in lfo1 are replaced with values from
!          lfo2.
!
! Language: Fortran 90
! ---------
!
! Notes: 
! ------
!
! Usage: rc = mergeCorrection(lfo1,lfo2)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! lfo1(:)          LFODataType    INOUT  On input, corrected precipitation
!                                        using method-1. On output, corrected
!                                        precipitation merged with data from
!                                        method-2. 
!
! lfo2(:)          LFODataType    INOUT  Corrected precipitation using method-2.
!
! rc                   integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
! exception                              Thrown Exception: none
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/05/2015      J.Ardizzone  created.
!******************************************************************************
      implicit none
      integer :: this = MergeCorrectionErrorHandle

!     Argument List
!     -------------

      type (LFODataType), dimension(:), intent(inout) :: lfo1
      type (LFODataType), dimension(:), intent(in) :: lfo2

!     Local Variables
!     ---------------

      integer :: rc,hour
      real :: undef1, undef2
      type (LFODataType) :: p1, p2

      mergeCorrection = -1

!     Fill undefined values in lfo1 with
!     values from lfo2.
!     ===================================

      undef1 = lfo1(1)%grid%undef
      undef2 = lfo2(1)%grid%undef

      do hour = 1, size(lfo1)

        p1 = lfo1(hour)
        p2 = lfo2(hour)

        where (p1%prectot .eq. undef1 .and. p2%prectot .ne. undef2)

          p1%prectot = p2%prectot
          p1%preccon = p2%preccon
          p1%precsno = p2%precsno

        endwhere

      end do
      
      mergeCorrection = 0

      end function mergeCorrection

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

      character (len=1024) :: file1, file2, out_file
      namelist /corrector/ file1, file2, out_file

      rc = 0
      getArgs = -1

!     Initialize
!     ==========

      file1     = ''
      file2     = ''
      out_file  = ''

!     Check Usage
!     ===========

      if (iargc() .lt. 3 .or. iargc() .gt. 4) rc = 1
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

      args%start_time = 63000

      if (iargc() .eq. 4) then 
        call getarg(4,str)
        read(unit=str,fmt='(i)') args%start_time
        if (args%start_time .gt. 235959) rc = 1
        if (isError(this,2,rc,arg=str)) return
      endif

!     Read in the configuration
!     =========================

      open (unit=5,file=args%config,status='old',iostat=rc)
      if (isError(this,3,rc,arg=args%config) .ne. 0) return

      read(5,nml=corrector,iostat=rc)
      if (isError(this,4,rc,arg=args%config) .ne. 0) return

      close (unit=5)

!     Save configuration (perform sanity check)
!     =========================================

      args%file1     = file1
      args%file2     = file2
      args%out_file  = out_file

      if (len_trim(file1) .eq. 0)     rc = isError(this,5,-1,arg='file1')
      if (len_trim(file2) .eq. 0)     rc = isError(this,5,-1,arg='file2')
      if (len_trim(out_file) .eq. 0)  rc = isError(this,5,-1,arg='out_file')

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
! arg                   string   OPT,IN  optional argument
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
!           01/03/2015      J.Ardizzone  created.
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

            print *, ''

          case (2)

            print *, 'correctPrecip (error#',code,'): error reading file: ', &
                      '"',trim(arg),'"'

          case (3)

            print *, 'correctPrecip (error#',code,'): error occurred during ', &
                     'merge'

          case (4)

            print *, 'correctPrecip (error#',code,'): error writing to output'

          case default

            print *, 'correctPrecip (error#',code,'): unknown error'

        end select

        case (GetArgsErrorHandle)
!       =========================

        select case (errorEvent)

          case (1)

            print *, 'Usage: correctPrecip [config] [sccyymmdd] [eccyymmdd]'
            print *, ' '

          case (2)

            print *, 'getArgs (error#',code,'): bad date/time format: ', &
                     'arg = ',trim(arg)

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

      end module merge_CorrectorMod
