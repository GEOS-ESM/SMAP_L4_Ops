      module LDAS_TAVG_Mod

      use time

      private

      public :: TAVGopen
      public :: TAVGwrite
      public :: errorHandler

      integer, parameter :: NUMTILES = 1653157
      integer, parameter :: TAVGOpenErrorHandle = 1
      integer, parameter :: TAVGReadErrorHandle = 2
      integer, parameter :: TAVGWriteErrorHandle = 3

      type FILE_HANDLE

        integer :: num_files = 0
        integer, dimension(3) :: unit_number = 0
        character (len=1024), dimension(3) :: file_name

      end type FILE_HANDLE

      integer :: errorCode = 0
      type (FILE_HANDLE) :: FH

      contains

!******************************************************************************
      integer function TAVGopen(file_template,idate,itime,offset)
!******************************************************************************
! English Name: Time Average Open
! -------------
!
! Purpose: Opens an LDAS time average file for the specified date/time and
! -------- offset. 
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Usage: rc = TAVGopen(file_template,idate,itime,offset)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! file_template         string       IN  filename template using the Unix
!                                        'date' command time token conventions.
!
! idate                integer       IN  date (ccyymmdd) associated with the
!                                        file to be opened.
!
! itime                integer       IN  time (hhmmss) associated with the
!                                        file to be opened.
!
! offset               integer   OPT,IN  offset in minutes to be added to the
!                                        specified date/time to get the actual
!                                        time of the file.
!
! rc                   integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
! exception            integer      OUT  Thrown exception:
!
!                                        0: no exception
!                                        1: max files exceeded
!                                        2: error opening file
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/15/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      integer, intent(in), optional :: offset
      character (len=*), intent(in) :: file_template

!     Local Variables
!     ---------------

      integer :: n,rc
      integer :: inc_min
      integer :: ndate
      integer :: ntime
      integer :: this = TAVGOpenErrorHandle

      rc = 0
      TAVGopen = -1

!     Return error if max files
!     exceeded.
!     =========================

      n = FH%num_files + 1
      FH%num_files = n

      if (FH%num_files .gt. 3) rc = 1
      if (isError(this,1,rc) .ne. 0) return

!     Resolve the file name using
!     the specified time parameters.
!     ==============================

      inc_min = 0
      if (present(offset)) inc_min = offset

      ndate = idate; ntime = itime
      call tm_inctime(ndate,ntime,0,inc_min,0)
      call tm_string(ndate,ntime,file_template,FH%file_name(n))

!     Open the file and exit
!     ======================

      FH%unit_number(n) = getUnit()
      
      open (unit=FH%unit_number(n), &
            file=FH%file_name(n), &
            form='unformatted', &
            convert='big_endian', &
            status='old', &
            iostat=rc)

      if (isError(this,2,rc,file=FH%file_name(n)) .ne. 0) return
      print *, 'Reading from: ', trim(FH%file_name(n))

      TAVGopen = 0

      end function TAVGopen

!******************************************************************************
      integer function TAVGwrite(file_template,idate,itime)
!******************************************************************************
! English Name: Time Average Write
! -------------
!
! Purpose: Calculates and writes out time averages for each data record
! -------- contained on the input data files (see TAVGopen).
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Usage: rc = TAVGwrite(file_template,idate,itime)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! file_template         string       IN  filename template using the Unix
!                                        'date' command time token conventions.
!
! idate                integer       IN  date (ccyymmdd) associated with the
!                                        time average data to be written.
!
! itime                integer       IN  time (hhmmss) associated with the
!                                        time average data to be written.
!
! rc                   integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
! exception            integer      OUT  Thrown exception:
!
!                                        0: no exception
!                                        1: error opening output file
!                                        2: error reading data
!                                        3: error writing data
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/14/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character (len=*), intent(in) :: file_template

!     Local Variables
!     ---------------

      real :: count
      integer :: n,iu,iuout,rc
      character (len=1024) :: filename
      integer :: this = TAVGWriteErrorHandle

      real, dimension(NUMTILES) :: in_buf
      real, dimension(NUMTILES) :: out_buf

      TAVGwrite = -1

!     Open the output file
!     ====================

      call tm_string(idate,itime,file_template,filename)

      iuout = getUnit()

      open (unit=iuout,file=filename,form='unformatted', &
            convert='big_endian',status='replace', iostat=rc)

      if (isError(this,1,rc,file=filename) .ne. 0) return
      print *, 'Writing to: ',trim(filename)

!     Sum parallel records from each
!     input file, average and write
!     the result.
!     ==============================

      do while (rc .eq. 0)

        rc = TAVGread(out_buf)
        if (rc .eq. 1) exit
        if (isError(this,2,rc) .ne. 0) return

        write(iuout,iostat=rc) out_buf
        if (isError(this,3,rc,file=filename) .ne. 0) return

      end do

      rc = TAVGclose()
      close(unit=iuout)

      TAVGwrite = 0

      end function TAVGwrite

!******************************************************************************
      integer function TAVGread(data_out)
!******************************************************************************
! English Name: Time Average Read
! -------------
!
! Purpose: Reads in parallel data records from multiple files and returns
! -------- the average of the records read.
!
! Language: Fortran 90
! ---------
!
! See Also: TAVGopen(), TAVGwrite()
! ---------
!
! Notes: 1. Fill values are not currently masked in this function.
! ------ 2. TAVGopen() must be called before invoking this function.
!
! Usage: rc = TAVGread(data_out)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! data_out(:)             real      OUT  returned array of mean values averaged
!                                        over the records read.
!
! rc                   integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception occurred
!
! exception            integer      OUT  Thrown exception:
!
!                                        0: no exception
!                                        1: error reading data
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           04/14/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      real, dimension(NUMTILES), intent(out) :: data_out

!     Local Variables
!     ---------------

      real :: count
      integer :: n,iu,rc
      real, dimension(NUMTILES) :: data_in
      integer :: this = TAVGReadErrorHandle

      TAVGread = -1

!     Read one record from each file
!     and sum over each file.
!     ==============================

      data_out = 0.0

      do n = 1,FH%num_files

        iu = FH%unit_number(n)

        read(iu,iostat=rc) data_in
        if (rc .lt. 0) exit
        if (isError(this,1,rc,file=FH%file_name(n)) .ne. 0) return

        data_out = data_out + data_in

      end do

!     Return the average over all
!     files.
!     ===========================

      count = max(FH%num_files,1)
      data_out = data_out / count

      TAVGread = 0
      if (rc .lt. 0) TAVGread = 1

      end function TAVGread

!******************************************************************************
      integer function TAVGclose()
!******************************************************************************

      implicit none

      integer :: n

      do n = 1,FH%num_files
        close(unit=FH%unit_number(n))
      end do

      FH%num_files = 0

      TAVGclose = 0

      end function TAVGclose

!******************************************************************************
      integer function getUnit()
!******************************************************************************

      implicit none 

!     get_file_unit returns a unit number that is not in use

      integer lu_max, lu, iostat
      logical opened
 
      lu_max = 97

      do lu = lu_max,1,-1
         inquire (unit=lu, opened=opened, iostat=iostat)
         if (iostat.ne.0) cycle
         if (.not.opened) exit
      end do
!
      getUnit = lu
      
      end function getUnit

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,file)
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
! file                  string   OPT,IN  command-line argument
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
!            4/14/2014      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: rc
      integer, intent(in) :: errorHandle
      integer, intent(in) :: errorEvent
      character(len=*), intent(in), optional :: file

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

        case (TAVGOpenErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'TAVGopen (error#',code,'): maximum number of files ', &
                     'exceeded'

          case (2)

            print *, 'TAVGopen (error#',code,'): error opening file: ', &
                     '"',file,'"'

          case default

            print *, 'TAVGopen (error#',code,'): unknown error'

        end select

        case (TAVGReadErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'TAVGread (error#',code,'): error reading file: ', &
                     '"',file,'"'

          case default

            print *, 'TAVGread (error#',code,'): unknown error'

        end select

        case (TAVGWriteErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'TAVGwrite (error#',code,'): error opening file: ', &
                     '"',file,'"'

          case (2)

            print *, 'TAVGwrite (error#',code,'): error reading data'

          case (3)

            print *, 'TAVGwrite (error#',code,'): error writing file: ', &
                     '"',file,'"'

          case default

            print *, 'TAVGwrite (error#',code,'): unknown error'

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

      end module LDAS_TAVG_Mod
