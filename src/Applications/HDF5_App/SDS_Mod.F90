      module SDS_Mod

      use typesMod

      public

      private :: EXCEPTION
      private :: isError
      private :: Itrim
      private :: Ftrim

      integer*1, parameter :: SDS_U8_FILL  = &
            b'11111110'

      integer*2, parameter :: SDS_U16_FILL = &
            b'1111111111111110'

      integer*4, parameter :: SDS_U32_FILL = &
            b'11111111111111111111111111111110'

      integer*8, parameter :: SDS_U64_FILL = &
            b'1111111111111111111111111111111111111111111111111111111111111110'

      integer*1, parameter :: SDS_I8_FILL  = -127
      integer*2, parameter :: SDS_I16_FILL = -32767
      integer*4, parameter :: SDS_I32_FILL = -2147483647
      integer*8, parameter :: SDS_I64_FILL = -9223372036854775807

      character        , parameter :: SDS_STR_NULL = char(0)
      character (len=2), parameter :: SDS_STR_FILL = '  '

!     real*4 :: SDS_F32_FILL = 1.0e+15
!     real*8 :: SDS_F64_FILL = 1.0e+15

      real*4 :: SDS_F32_FILL = -9999.0
      real*8 :: SDS_F64_FILL = -9999.0

      integer, parameter :: AllocateErrorHandle = 1

      interface SDStransfer

        module procedure SDStransferF64
        module procedure SDStransferF32
        module procedure SDStransferI32
        module procedure SDStransferI16
        module procedure SDStransferI8
        module procedure SDStransferStr

        module procedure SDStransferF64Arr
        module procedure SDStransferF32Arr
        module procedure SDStransferI32Arr
        module procedure SDStransferI16Arr
        module procedure SDStransferI8Arr
        module procedure SDStransferStrArr

      end interface

      interface assignment (=)
        module procedure SDSassignF64
        module procedure SDSassignF32
        module procedure SDSassignI32
        module procedure SDSassignI16
        module procedure SDSassignI8
      end interface

      integer :: EXCEPTION
      character (len=MAX_STR_LEN) :: SDSstring

      contains

!******************************************************************************
      subroutine SDSassignF32(element,array)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(inout) :: element
      real*4, dimension(:,:), intent(in) :: array

      integer :: i,j,k,r,c
      integer :: rows,cols,dim1,dim2

      cols = element%info%dims(1)
      rows = element%info%dims(2)
      dim1 = size(array,dim=1)
      dim2 = size(array,dim=2)
      
      do r = 1,rows
        j = min(r,dim2)
        do c = 1,cols
          i = min(c,dim1)
          k = (r - 1) * cols + c
          element%f32(k) = array(i,j)
        end do
      end do

      end subroutine SDSassignF32

!******************************************************************************
      subroutine SDSassignF64(element,array)
!******************************************************************************
      implicit none

      type (ElementDataType), intent(inout) :: element
      real*8, dimension(:,:), intent(in) :: array

      integer :: i,j,k,r,c
      integer :: rows,cols,dim1,dim2

      cols = element%info%dims(1)
      rows = element%info%dims(2)
      dim1 = size(array,dim=1)
      dim2 = size(array,dim=2)

      do r = 1,rows
        j = min(r,dim2)
        do c = 1,cols
          i = min(c,dim1)
          k = (r - 1) * cols + c
          element%f32(k) = array(i,j)
        end do
      end do

      end subroutine SDSassignF64

!******************************************************************************
      subroutine SDSassignI32(element,array)
!******************************************************************************
      implicit none

      type (ElementDataType), intent(inout) :: element
      integer*4, dimension(:,:), intent(in) :: array

      integer :: i,j,k,r,c
      integer :: rows,cols,dim1,dim2
      integer*4, dimension(:), pointer :: i32

      if (associated(element%i32)) i32 => element%i32
      if (associated(element%u32)) i32 => element%u32

      cols = element%info%dims(1)
      rows = element%info%dims(2)
      dim1 = size(array,dim=1)
      dim2 = size(array,dim=2)

      do r = 1,rows
        j = min(r,dim2)
        do c = 1,cols
          i = min(c,dim1)
          k = (r - 1) * cols + c
          i32(k) = array(i,j)
        end do
      end do

      end subroutine SDSassignI32

!******************************************************************************
      subroutine SDSassignI16(element,array)
!******************************************************************************
      implicit none

      type (ElementDataType), intent(inout) :: element
      integer*2, dimension(:,:), intent(in) :: array

      integer :: i,j,k,r,c
      integer :: rows,cols,dim1,dim2
      integer*2, dimension(:), pointer :: i16

      if (associated(element%i16)) i16 => element%i16
      if (associated(element%u16)) i16 => element%u16

      cols = element%info%dims(1)
      rows = element%info%dims(2)
      dim1 = size(array,dim=1)
      dim2 = size(array,dim=2)

      do r = 1,rows
        j = min(r,dim2)
        do c = 1,cols
          i = min(c,dim1)
          k = (r - 1) * cols + c
          i16(k) = array(i,j)
        end do
      end do

      end subroutine SDSassignI16

!******************************************************************************
      subroutine SDSassignI8(element,array)
!******************************************************************************
      implicit none

      type (ElementDataType), intent(inout) :: element
      integer*1, dimension(:,:), intent(in) :: array

      integer :: i,j,k,r,c
      integer :: rows,cols,dim1,dim2
      integer*1, dimension(:), pointer :: i8

      if (associated(element%i8)) i8 => element%i8
      if (associated(element%u8)) i8 => element%u8

      cols = element%info%dims(1)
      rows = element%info%dims(2)
      dim1 = size(array,dim=1)
      dim2 = size(array,dim=2)

      do r = 1,rows
        j = min(r,dim2)
        do c = 1,cols
          i = min(c,dim1)
          k = (r - 1) * cols + c
          i8(k) = array(i,j)
        end do
      end do

      end subroutine SDSassignI8

!******************************************************************************
      subroutine SDSdeallocate(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(inout) :: element

      if (associated(element%f32)) deallocate(element%f32)
      if (associated(element%f64)) deallocate(element%f64)
      if (associated(element%i8 )) deallocate(element%i8 )
      if (associated(element%i16)) deallocate(element%i16)
      if (associated(element%i32)) deallocate(element%i32)
      if (associated(element%int)) deallocate(element%int)
      if (associated(element%u8 )) deallocate(element%u8 )
      if (associated(element%u16)) deallocate(element%u16)
      if (associated(element%u32)) deallocate(element%u32)
      if (associated(element%buf)) deallocate(element%buf)

      end subroutine SDSdeallocate

!******************************************************************************
      integer function SDSallocate(element,real_size,int_size, &
                                                 uint_size,str_size)
!******************************************************************************

      implicit none

      integer, intent(in), optional :: int_size
      integer, intent(in), optional :: uint_size
      integer, intent(in), optional :: real_size
      integer, intent(in), optional :: str_size
      type (ElementDataType), intent(inout) :: element

      integer :: rc
      integer :: bytes
      integer :: rows,cols
      integer, dimension(4) :: bit_size
      integer :: this = AllocateErrorHandle

      SDSallocate = -1

      cols = element%info%dims(1)
      rows = element%info%dims(2)

      bit_size = element%info%bit_size

      if (present(real_size)) bit_size = (/real_size,0,0,0/)
      if (present(int_size))  bit_size = (/0,int_size,0,0/)
      if (present(uint_size)) bit_size = (/0,0,uint_size,0/)
      if (present(str_size))  bit_size = (/0,0,0,str_size*8/)

      if (bit_size(1) .eq. 32) allocate(element%f32(cols*rows),stat=rc)
      if (bit_size(1) .eq. 64) allocate(element%f64(cols*rows),stat=rc)

      if (bit_size(2) .eq. 32) allocate(element%i32(cols*rows),stat=rc)
      if (bit_size(2) .eq. 16) allocate(element%i16(cols*rows),stat=rc)
      if (bit_size(2) .eq.  8) allocate(element%i8 (cols*rows),stat=rc)

      if (bit_size(3) .eq. 32) allocate(element%u32(cols*rows),stat=rc)
      if (bit_size(3) .eq. 16) allocate(element%u16(cols*rows),stat=rc)
      if (bit_size(3) .eq.  8) allocate(element%u8 (cols*rows),stat=rc)

!     Always allocate a character buffer to be
!     used for workspace or string data types.
      
      bytes = (maxval(bit_size) * cols * rows) / 8
      if (bytes .ne. 0) allocate(element%buf(bytes),stat=rc)

      if (isError(this,1,rc) .ne. 0) return

      SDSallocate = 0

      end function SDSallocate

!******************************************************************************
      real*4 function SDStransferF32(value,default)
!******************************************************************************
      implicit none

      character (len=*), intent(in) :: value
      real*4, intent(in) :: default

      SDSstring = Ftrim(value)

      if (len_trim(SDSstring) .eq. 0) then
        SDStransferF32 = default
      else
        read(unit=SDSstring,fmt='(f)') SDStransferF32
      endif

      end function SDStransferF32

!******************************************************************************
      real*4 function SDStransferF32Arr(value,buf,default)
!******************************************************************************
      implicit none

      character (len=*), dimension(:), intent(in) :: value
      real*4, dimension(:), intent(out) :: buf
      real*4, intent(in) :: default

      integer :: i

      do i = 1,size(buf)
        buf(i) = SDStransfer(value(i),default)
      end do

      SDStransferF32Arr = size(buf)

      end function SDStransferF32Arr

!******************************************************************************
      real*8 function SDStransferF64(value,default)
!******************************************************************************
      implicit none

      character (len=*), intent(in) :: value
      real*8, intent(in) :: default

      SDSstring = Ftrim(value)

      if (len_trim(SDSstring) .eq. 0) then
        SDStransferF64 = default
      else
        read(unit=SDSstring,fmt='(f)') SDStransferF64
      endif

      end function SDStransferF64

!******************************************************************************
      real*8 function SDStransferF64Arr(value,buf,default)
!******************************************************************************
      implicit none

      character (len=*), dimension(:), intent(in) :: value
      real*8, dimension(:), intent(out) :: buf
      real*8, intent(in) :: default

      integer :: i

      do i = 1,size(buf)
        buf(i) = SDStransfer(value(i),default)
      end do

      SDStransferF64Arr = size(buf)

      end function SDStransferF64Arr

!******************************************************************************
      integer*1 function SDStransferI8(value,default)
!******************************************************************************
      implicit none

      character (len=*), intent(in) :: value
      integer*1, intent(in) :: default

      SDSstring = Itrim(value)

      if (len_trim(SDSstring) .eq. 0) then
        SDStransferI8 = default
      else
        read(unit=SDSstring,fmt='(i)') SDStransferI8
      endif

      end function SDStransferI8

!******************************************************************************
      integer*1 function SDStransferI8Arr(value,buf,default)
!******************************************************************************
      implicit none

      character (len=*), dimension(:), intent(in) :: value
      integer*1, dimension(:), intent(out) :: buf
      integer*1, intent(in) :: default 
      
      integer :: i

      do i = 1,size(buf)
        buf(i) = SDStransfer(value(i),default)
      end do

      SDStransferI8Arr = size(buf)

      end function SDStransferI8Arr

!******************************************************************************
      integer*2 function SDStransferI16(value,default)
!******************************************************************************
      implicit none
      
      character (len=*), intent(in) :: value
      integer*2, intent(in) :: default

      SDSstring = Itrim(value)

      if (len_trim(SDSstring) .eq. 0) then
        SDStransferI16 = default
      else
        read(unit=SDSstring,fmt='(i)') SDStransferI16
      endif

      end function SDStransferI16

!******************************************************************************
      integer*2 function SDStransferI16Arr(value,buf,default)
!******************************************************************************
      implicit none

      character (len=*), dimension(:), intent(in) :: value
      integer*2, dimension(:), intent(out) :: buf
      integer*2, intent(in) :: default

      integer :: i

      do i = 1,size(buf)
        buf(i) = SDStransfer(value(i),default)
      end do

      SDStransferI16Arr = size(buf)

      end function SDStransferI16Arr

!******************************************************************************
      integer*4 function SDStransferI32(value,default)
!******************************************************************************
      implicit none
      
      character (len=*), intent(in) :: value
      integer*4, intent(in) :: default

      SDSstring = Itrim(value)

      if (len_trim(SDSstring) .eq. 0) then
        SDStransferI32 = default
      else
        read(unit=SDSstring,fmt='(i)') SDStransferI32
      endif

      end function SDStransferI32

!******************************************************************************
      integer*4 function SDStransferI32Arr(value,buf,default)
!******************************************************************************
      implicit none

      character (len=*), dimension(:), intent(in) :: value
      integer*4, dimension(:), intent(out) :: buf
      integer*4, intent(in) :: default 
      
      integer :: i
      
      do i = 1,size(buf)
        buf(i) = SDStransfer(value(i),default)
      end do

      SDStransferI32Arr = size(buf)

      end function SDStransferI32Arr

!******************************************************************************
      character (len=MAX_STR_LEN) function SDStransferStr(value,str_null)
!******************************************************************************
      implicit none

      character (len=*), intent(in) :: value
      character, intent(in) :: str_null

      SDSstring = value

      SDStransferStr = value
      if (len_trim(SDSstring) .eq. 0) SDStransferStr = str_null

      end function SDStransferStr

!******************************************************************************
      integer*4 function SDStransferStrArr(value,buf,str_null)
!******************************************************************************
      implicit none

      character (len=*), dimension(:), intent(in) :: value
      character (len=*), dimension(:), intent(out) :: buf
      character, intent(in) :: str_null

      integer :: i

      do i = 1,size(buf)
        buf(i) = trim(value(i)) // str_null
      end do

      SDStransferStrArr = size(buf)

      end function SDStransferStrArr

!******************************************************************************
      character (len=MAX_STR_LEN) function Itrim(value)
!******************************************************************************
      implicit none

      integer :: i
      character (len=*), intent(in) :: value

      Itrim = value

      i = index(value,'.')
      if (i .gt. 0) Itrim = value(1:i-1)

      end function Itrim

!******************************************************************************
      character (len=MAX_STR_LEN) function Ftrim(value)
!******************************************************************************
      implicit none

      integer :: i
      character (len=*), intent(in) :: value

      Ftrim = value
      if (len_trim(value) .eq. 0) return

      i = index(value,'.')
      if (i .le. 0) Ftrim = trim(value) // '.0'

      end function Ftrim

!******************************************************************************
      logical function isCoord(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isCoord = .true.

      if (isLatitude(element))  return
      if (isLongitude(element)) return
      if (isTime(element))      return
      if (isRowIndex(element))  return
      if (isColIndex(element))  return
      if (isXProjCoord(element))  return
      if (isYProjCoord(element))  return

      isCoord = .false.

      end function isCoord

!******************************************************************************
      logical function isGrid(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isGrid = .false.

      if (isTime(element))      return
      if (isXProjCoord(element))  return
      if (isYProjCoord(element))  return

      isGrid = .true.

      end function isGrid

!******************************************************************************
      logical function isProjCoord(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isProjCoord = .true.

      if (isXProjCoord(element))  return
      if (isYProjCoord(element))  return

      isProjCoord = .false.

      end function isProjCoord

!******************************************************************************
      logical function isLatitude(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isLatitude = .true.

      if (trim(element%info%name) .eq. 'latitude') return
      if (trim(element%info%name) .eq. 'cell_lat') return

      isLatitude = .false.

      end function isLatitude

!******************************************************************************
      logical function isLongitude(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isLongitude = .true.

      if (trim(element%info%name) .eq. 'longitude') return
      if (trim(element%info%name) .eq. 'cell_lon') return

      isLongitude = .false.

      end function isLongitude

!******************************************************************************
      logical function isYProjCoord(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isYProjCoord = .true.

      if (trim(element%info%name) .eq. 'y') return
      if (trim(element%info%name) .eq. 'Y') return

      isYProjCoord = .false.

      end function isYProjCoord

!******************************************************************************
      logical function isXProjCoord(element)
!******************************************************************************
      
      implicit none

      type (ElementDataType), intent(in) :: element

      isXProjCoord = .true.

      if (trim(element%info%name) .eq. 'x') return
      if (trim(element%info%name) .eq. 'X') return
      
      isXProjCoord = .false.

      end function isXProjCoord

!******************************************************************************
      logical function isTime(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isTime = .true.

      if (trim(element%info%name) .eq. 'time') return

      isTime = .false.

      end function isTime

!******************************************************************************
      logical function isRowIndex(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isRowIndex = .true.
      
      if (trim(element%info%name) .eq. 'row') return
      if (trim(element%info%name) .eq. 'cell_row') return
      if (trim(element%info%name) .eq. 'cell_row_index') return

      isRowIndex = .false.

      end function isRowIndex

!******************************************************************************
      logical function isColIndex(element)
!******************************************************************************

      implicit none

      type (ElementDataType), intent(in) :: element

      isColIndex = .true.

      if (trim(element%info%name) .eq. 'column') return
      if (trim(element%info%name) .eq. 'cell_col') return
      if (trim(element%info%name) .eq. 'cell_column') return
      if (trim(element%info%name) .eq. 'cell_col_index') return

      isColIndex = .false.

      end function isColIndex

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

!       Allocate Error Handle
!       =====================

        case (AllocateErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'SDSallocate(error#',code,'): error allocating memory'


          case default

            print *, 'SDSallocate (error#',code,'): unknown error'

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

      end module SDS_Mod
