      use hdf5
      use h5fortran_types

      implicit none

      integer, parameter :: cols = 3856
      integer, parameter :: rows = 1624
      integer, parameter :: max_objs = 500
      integer, parameter :: maxdim = cols * rows

!     Local Variables
!     ---------------

      integer(HID_T) :: fid
      integer(HID_T) :: prp_id
      integer(HID_T) :: grp_id
      integer(HID_T) :: dset_id
      integer(HID_T) :: space_id
      integer(HID_T) :: attr_id
      integer(HID_T) :: str_id
      integer(HID_T) :: type_id
      integer(HID_T) :: time_id
      integer(HID_T), dimension(max_objs) :: obj_ids

      integer(SIZE_T) :: len
      integer(SIZE_T) :: obj_count

      integer(HSIZE_T), dimension(2) :: dims = (/cols,rows/)

      character (len=1024) :: name
      character (len=1024), dimension(45) :: names
      character (len=1024) :: filename
      character, dimension(maxdim*8) :: buf

      integer :: n
      integer :: rc

!     Retrieve the filename argument from the
!     command-line.
!     =======================================

      if (iargc() .ne. 1) stop 1
      call getarg(1,filename)
      
!     Initialize HDF5
!     ===============

      call h5open_f(rc)
      if (rc .ne. 0) stop 2

!     Open HDF5 file
!     ==============

      call h5pcreate_f(H5P_FILE_ACCESS_F, prp_id, rc)
      if (rc .ne. 0) stop 2
      call h5pset_fapl_stdio_f(prp_id, rc)
      if (rc .ne. 0) stop 2

      call h5fopen_f(filename,H5F_ACC_RDONLY_F,fid,rc,prp_id)
      if (rc .ne. 0) stop 2

!     Get a list of dataset elements
!     on the file
!     ==============================

      call h5Gopen_f(fid, 'Metadata/Source/L1C_TB', grp_id, rc)
      if (rc .ne. 0) stop 4

      call h5Aopen_f(grp_id, 'version', attr_id, rc)
      if (rc .ne. 0) stop 4

      call h5aget_type_f(attr_id, type_id, rc)
      if (rc .ne. 0) stop 5

      print *, grp_id, attr_id, type_id, H5T_NATIVE_INTEGER

      stop
      end
