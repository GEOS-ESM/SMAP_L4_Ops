!******************************************************************************
      module LFOCorrFileMod
!******************************************************************************
! English Name: Corrected File Reader Module
! -------------
!
! Purpose: Implements the land forcing (LFO) I/O API for reading data
! -------- from precipitation corrected data files.
!
! Language: Fortran 90
! ---------
!
! See Also: LFOBaseMod, GRIDMod, ESMF CFIO
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! LFOReadFile()        integer      PUB  Reads land forcing data from a GEOS-5
!                                        SDF-type corrected file for the
!                                        specified date/time.
!
!   filename            string       IN  name of file containing land forcing
!                                        data.
!
!   idate              integer       IN  date of desired data as ccyymmdd
!
!   itime              integer       IN  time of desired data as hhmmss
!
!   data           LFODataType      OUT  returned land forcing data
!
!   LFOReadFile        integer      OUT  function return value:
!
!                                        0: success
!                                       -1: error occurred
!
! errorHandler()       integer      PUB  module error handler to be queried
!                                        after all invocations of the public
!                                        interface routines.
!
!                                        0: success
!                                       !0: error occurred
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           01/03/2014      J.Ardizzone  created.
!******************************************************************************

      use TypesMod
      use GridMod, GridError => errorHandler

      use ESMF_CFIOGridMOD
      use ESMF_CFIOVarInfoMOD
      use ESMF_CFIOFileMOD
      use ESMF_CFIOSdfMOD
      use ESMF_CFIOMOD

      private
      public :: LFOfactor
      public :: LFOReadFile
      public :: errorHandler

      real, parameter :: LFOfactor = 1.0

      integer, parameter :: LFOReadFileErrorHandle  = 1

      integer :: errorCode = 0

      contains

!******************************************************************************
      integer function LFOReadFile(filename,idate,itime,data)
!******************************************************************************
! English Name: Read File
! -------------
!
! Purpose: Reads land forcing data from a GEOS-5 SDF-type corrected file for
! -------- the specified date/time.
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Usage: rc = LFOReadFile(filename,idate,itime,data)
! ------ exception = errorHandler()
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! filename              string       IN  name of file containing land forcing
!                                        data.
!
! idate                integer       IN  date of desired data as ccyymmdd
!
! itime                integer       IN  time of desired data as hhmmss
!
! data             LFODataType      OUT  returned land forcing data
!
! rc                   integer      OUT  function return value:
!
!                                        0: success
!                                       -1: exception issued
!
! exception            integer      OUT  Thrown Exception:
!
!                                        1: Error creating CFIO object
!                                        2: Error creating CFIO object
!                                        3: Error creating CFIO object
!                                        4: Error retrieving file dimensions
!                                        5: Bad lat/lon dimensions        
!                                        6: error reading data
!                                        7: error deallocating data
!                                        8: error allocating memory
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
      integer :: this = LFOReadFileErrorHandle

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      type (LFODataType), intent(inout) :: data
      character(len=*), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      integer :: fmode = 1
      type(ESMF_CFIO) :: cfio
      integer, save :: heap = 0
      real*4, dimension(:,:), pointer, save :: buf => null()
      integer :: nx,ny,km,lm,ngatts

!     Initialize
!     ==========

      heap = GRIDAllocate(heap)

      LFOReadFile = -1
      call GRIDDeallocate(data%grid)
      if (isError(this,7,GRIDError()) .ne. 0) return

!     Access the input file for reading
!     =================================

      print *, trim(filename)

      cfio =  ESMF_CFIOCreate(cfioObjName='LFO', rc=rc)
      if (isError(this,1,rc) .ne. 0) return

      call ESMF_CFIOSet(cfio, fName=filename, rc=rc)
      if (isError(this,2,rc) .ne. 0) return

      call ESMF_CFIOFileOpen(cfio,fmode,rc=rc)
      if (isError(this,3,rc) .ne. 0) return

      call CFIO_DimInquire (cfio%fid, nx, ny, km, lm, cfio%mVars, ngatts, rc=rc)
      if (isError(this,4,rc) .ne. 0) return

      if (nx .le. 0 .or. ny .le. 0) rc = -1
      if (isError(this,5,rc) .ne. 0) return

!     Allocate memory for storing land forcing fields
!     ===============================================

      data%grid%nx = nx
      data%grid%ny = ny
      data%grid%dx = 360.0 / nx
      data%grid%dy = 180.0 / (ny - 1)
      data%grid%xStart = cfio%grids(1)%lon(1)
      data%grid%yStart = cfio%grids(1)%lat(1)
      data%grid%undef  = cfio%varObjs(1)%amiss
      data%grid%idate  = idate
      data%grid%itime  = itime
      
      rc = GRIDAllocate(data)
      if (isError(this,8,GRIDError()) .ne. 0) return

      if (associated(buf)) deallocate(buf)
      allocate(buf(nx,ny),stat=rc)
      if (isError(this,8,rc) .ne. 0) return

!     Read land forcing fields into data buffers
!     ==========================================

      call ESMF_CFIOVarRead(cfio,'PRECTOT',buf,date=idate, curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='PRECTOT') .ne. 0) return
      data%prectot = dble(buf)

      call ESMF_CFIOVarRead(cfio,'PRECSNO',buf,date=idate,curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='PRECSNO') .ne. 0) return
      data%precsno = dble(buf)

      call ESMF_CFIOVarRead(cfio,'PRECCON',buf,date=idate,curTime=itime,rc=rc)
      if (isError(this,6,rc,varName='PRECCON') .ne. 0) return
      data%preccon = dble(buf)

!     Close file and return
!     =====================

      call ESMF_CFIOFileClose(cfio)

      call GRIDCloseHeap()
      if (associated(buf)) deallocate(buf)

      LFOReadFile = 0
      return 

      end function LFOReadFile

!******************************************************************************
      integer function isError(errorHandle,errorEvent,rc,varName)
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
! varName               string   OPT,IN  variable name associated with an
!                                        error event.
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
      character(len=*), intent(in), optional :: varName

      character(len=2) :: code

      isError = 0
      if (rc .eq. 0) return

      write(unit=code,fmt='(i2)') errorEvent

!     Find appropriate error-handler and print message
!     associated with the event.
!     ================================================

      select case (errorHandle)

!       LFOReadFile Error Handle
!       ========================

        case (LFOReadFileErrorHandle)

        select case (errorEvent)

          case (1,2,3)

            print *, 'LFOReadFile (error#',code,'): Error creating CFIO object'

          case (4)

            print *, 'LFOReadFile (error#',code,'): Error retrieving file dimensions'

          case (5)

            print *, 'LFOReadFile (error#',code,'): bad lat/lon dimension'

          case (6)

            print *, 'LFOReadFile (error#',code,'): error reading data'
            print *, 'LFOReadFile: variable = ',varName

          case (7)

            print *, 'LFOReadFile (error#',code,'): error deallocating data'

          case (8)

            print *, 'LFOReadFile (error#',code,'): error allocating memory'

          case default

            print *, 'LFOReadFile (error#',code,'): unknown error'

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

      end module LFOCorrFileMod
