      module HDF5AttributeMod

      use hdf5
      use h5fortran_types

      private
      public :: h5Amake

      interface h5Amake

        module procedure h5AmakeString
        module procedure h5AmakeF32
        module procedure h5AmakeF64
        module procedure h5AmakeI8
        module procedure h5AmakeI16
        module procedure h5AmakeI32
        module procedure h5AmakeStringArr
        module procedure h5AmakeF32Arr
        module procedure h5AmakeF64Arr
        module procedure h5AmakeI8Arr
        module procedure h5AmakeI16Arr
        module procedure h5AmakeI32Arr

      end interface h5Amake

      contains

!******************************************************************************
      subroutine h5AmakeString(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with str,
!     using object attributes

      IMPLICIT NONE
      integer, intent(out) :: rc
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name,value
      INTEGER(HID_T) :: str_id, ann_space, title_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
      integer(SIZE_T) :: length,dim
!
      rc = 0
      length = LEN_TRIM(value)

      dim = length
      if (length .eq. 0) dim = 1

      CALL h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(str_id, dim, rc)
      if (rc .ne. 0) return
      call h5tset_strpad_f(str_id, H5T_STR_NULLTERM_F, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, ann_space, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, str_id, ann_space, title_id, rc)
      if (rc .ne. 0) return

      if (length .eq. 0) then
        CALL h5awrite_f(title_id, str_id, char(0), one, rc)
      else
        CALL h5awrite_f(title_id, str_id, value, one, rc)
      endif

      if (rc .ne. 0) return

      CALL h5tclose_f(str_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(ann_space, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(title_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeString

!******************************************************************************
      subroutine h5AmakeF32(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      real*4, intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name

      character (len=4) :: buf
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_REAL, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 4, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeF32

!******************************************************************************
      subroutine h5AmakeF64(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      real*8, intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name

      character (len=8) :: buf
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_DOUBLE, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 8, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeF64

!******************************************************************************
      subroutine h5AmakeI8(id, name, value,rc, sign)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*1, intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      INTEGER, INTENT(in), optional :: sign

      INTEGER :: sign_type
      character (len=1) :: buf
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 1, rc)
      if (rc .ne. 0) return
      CALL h5tset_sign_f(type_id, sign_type, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeI8

!******************************************************************************
      subroutine h5AmakeI16(id, name, value,rc, sign)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*2, intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      INTEGER, INTENT(in), optional :: sign

      INTEGER :: sign_type
      character (len=2) :: buf
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 2, rc)
      if (rc .ne. 0) return
      CALL h5tset_sign_f(type_id, sign_type, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeI16

!******************************************************************************
      subroutine h5AmakeI32(id, name, value,rc, sign)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*4, intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      INTEGER, INTENT(in), optional :: sign

      INTEGER :: sign_type
      character (len=4) :: buf
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 4, rc)
      if (rc .ne. 0) return
      CALL h5tset_sign_f(type_id, sign_type, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeI32

!******************************************************************************
      subroutine h5AmakeI64(id, name, value,rc, sign)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*8, intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      INTEGER, INTENT(in), optional :: sign

      INTEGER   :: sign_type
      character (len=8) :: buf
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
!
      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 8, rc)
      if (rc .ne. 0) return
      CALL h5tset_sign_f(type_id, sign_type, rc)
      if (rc .ne. 0) return
      CALL h5screate_f(H5S_SCALAR_F, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeI64

!******************************************************************************
      subroutine h5AmakeStringArr(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with str,
!     using object attributes

      IMPLICIT NONE
      integer, intent(out) :: rc
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), dimension(:), INTENT(in) :: value

      integer :: rank = 1
      integer(SIZE_T) :: length
      INTEGER(HID_T) :: space_id, attr_id, type_id, vltype_id
      INTEGER(HSIZE_T), dimension(1) :: dims
!
      rc = 0

      dims = size(value)
      length = len(value(1))

      CALL h5tcopy_f(H5T_NATIVE_CHARACTER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, length, rc)
      if (rc .ne. 0) return
!     call h5tvlen_create_f(type_id, vltype_id, rc)
!     if (rc .ne. 0) return
      call h5tset_strpad_f(type_id, H5T_STR_NULLTERM_F, rc)
      if (rc .ne. 0) return
      CALL h5screate_simple_f(rank, dims, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, value, dims, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeStringArr

!******************************************************************************
      subroutine h5AmakeF32Arr(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      real*4, dimension(:), intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name

      integer :: rank = 1
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T), dimension(1) :: dims
!
      rc = 0

      dims = size(value)

      CALL h5tcopy_f(H5T_NATIVE_REAL, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 4, rc)
      if (rc .ne. 0) return
      CALL h5screate_simple_f(rank, dims, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, value, dims, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeF32Arr

!******************************************************************************
      subroutine h5AmakeF64Arr(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with str,
!     using object attributes

      IMPLICIT NONE
      integer, intent(out) :: rc
      real*8, dimension(:), intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name

      integer :: rank = 1
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T), dimension(1) :: dims
!
      rc = 0

      dims = size(value)

      CALL h5tcopy_f(H5T_NATIVE_DOUBLE, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 8, rc)
      if (rc .ne. 0) return
      CALL h5screate_simple_f(rank, dims, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, value, dims, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeF64Arr

!******************************************************************************
      subroutine h5AmakeI8Arr(id, name, value,rc, sign)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*1, dimension(:), intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      INTEGER, INTENT(in), optional :: sign

      INTEGER :: sign_type
      integer, save :: rank = 1
      INTEGER(HSIZE_T), dimension(1) :: dims
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
      character, dimension(:), pointer, save :: buf => null()
!
      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign
!
      rc = 0
      dims = size(value)

!     Transfer 8-bit integer data to a
!     character buffer since there is no
!     HDF-5 Fortran API for this type.
!     ==================================

      if (associated(buf)) deallocate(buf)
      allocate(buf(dims(1)),stat=rc)
      if (rc .ne. 0) return

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 1, rc)
      if (rc .ne. 0) return
      CALL h5tset_sign_f(type_id, sign_type, rc)
      if (rc .ne. 0) return
      CALL h5screate_simple_f(rank, dims, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      deallocate(buf)

      end subroutine h5AmakeI8Arr

!******************************************************************************
      subroutine h5AmakeI16Arr(id, name, value,rc, sign)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*2, dimension(:), intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name
      INTEGER, INTENT(in), optional :: sign

      INTEGER :: sign_type
      integer, save :: rank = 1
      INTEGER(HSIZE_T), dimension(1) :: dims
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T) :: one(1) = (/1_HSIZE_T/)
      character, dimension(:), pointer, save :: buf => null()
!
      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign
!
      rc = 0
      dims = size(value)

!     Transfer 16-bit integer data to a
!     character buffer since there is no
!     HDF-5 Fortran API for this type.
!     ==================================

      if (associated(buf)) deallocate(buf)
      allocate(buf(dims(1)*2),stat=rc)
      if (rc .ne. 0) return

      buf = transfer(value,buf)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 2, rc)
      if (rc .ne. 0) return
      CALL h5tset_sign_f(type_id, sign_type, rc)
      if (rc .ne. 0) return
      CALL h5screate_simple_f(rank, dims, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, buf, one, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      deallocate(buf)

      end subroutine h5AmakeI16Arr

!******************************************************************************
      subroutine h5AmakeI32Arr(id, name, value,rc)
!******************************************************************************

!     Annote an object (group/dataset) id with a scalar
!     floating point value.

      IMPLICIT NONE
      integer, intent(out) :: rc
      integer*4, dimension(:), intent(in) :: value
      INTEGER(HID_T), INTENT(in) :: id
      CHARACTER(len=*), INTENT(in) :: name

      integer :: rank = 1
      INTEGER(HID_T) :: space_id, attr_id, type_id
      INTEGER(HSIZE_T), dimension(1) :: dims
!
      rc = 0

      dims = size(value)

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, rc)
      if (rc .ne. 0) return
      CALL h5tset_size_f(type_id, 4, rc)
      if (rc .ne. 0) return
      CALL h5screate_simple_f(rank, dims, space_id, rc)
      if (rc .ne. 0) return
      CALL h5acreate_f(id, name, type_id, space_id, attr_id, rc)
      if (rc .ne. 0) return
      CALL h5awrite_f(attr_id, type_id, value, dims, rc)
      if (rc .ne. 0) return

      CALL h5tclose_f(type_id, rc)
      if (rc .ne. 0) return
      CALL h5sclose_f(space_id, rc)
      if (rc .ne. 0) return
      CALL h5aclose_f(attr_id, rc)
      if (rc .ne. 0) return

      end subroutine h5AmakeI32Arr

      end module HDF5AttributeMod
