      module HDF5FormatMod

      use time
      use typesMod
      use SDS_Mod, sdsError=>errorHandler
      use GPD_Mod, gridError=>errorHandler
      use HDF5AttributeMod

      use hdf5
      use h5fortran_types
      use h5ds

      private
      public :: DAACopen
      public :: DAACcreate
      public :: DAACwrite
      public :: DAACclose
      public :: errorHandler

      integer, parameter :: OpenErrorHandle = 1
      integer, parameter :: CreateErrorHandle = 2
      integer, parameter :: WriteGroupErrorHandle = 3
      integer, parameter :: WriteElementErrorHandle = 4
      integer, parameter :: WriteTimeErrorHandle = 5
      integer, parameter :: WriteAttributeErrorHandle = 6
      integer, parameter :: WriteCoordLinkErrorHandle = 7
      integer, parameter :: SetScaleErrorHandle = 8
      integer, parameter :: AttachScaleErrorHandle = 9

      type DAACDataType

        integer(HID_T) :: fid
        integer(HID_T) :: fapl_id
        integer(HID_T) :: dcpl_id
        integer(HID_T) :: grp_id
        integer(HID_T) :: dset_id
        integer(HID_T) :: space_id
        integer(HID_T) :: attr_id
        integer(HID_T) :: str_id
        integer(HID_T) :: type_id
        integer(HID_T) :: time_id
        integer(HID_T) :: lat_id
        integer(HID_T) :: lon_id

        integer :: num_groups
        integer(HID_T), dimension(MAX_DEPTH) :: groups

      end type DAACDataType

      interface DAACwrite

        module procedure DAACwriteGroup
        module procedure DAACwriteElement
        module procedure DAACwriteAttribute

      end interface DAACwrite

      interface DAACclose

        module procedure DAACcloseGroup
        module procedure DAACcloseFile

      end interface DAACclose

      integer :: EXCEPTION = 0

      type (DAACDataType) :: DAAC

      contains

!******************************************************************************
      integer function DAACcreate(filename,fileInfo)
!******************************************************************************
! English Name: DAAC Open
! -------------
!
! Purpose: Creates an HDF-5 file and writes file information.
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
! filename              string       IN  file name of HDF-5 file to be created.
!
! fileInfo        FileInfoType       IN  data structure containing information
!                                        about the file being created.
!
! DAACcreate           integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error initializing HDF
!                                        2: error creating file
!                                        3: error writing global attributes
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

      character(len=*), intent(in) :: filename
      type (FileInfoType), intent(in) :: fileInfo

!     Local Variables
!     ---------------

      integer :: i
      integer :: rc
      integer(HID_T) :: fid, fapl_id
      integer :: this = CreateErrorHandle
      type (GridParamDefType) :: gpd

!     Initialize HDF5
!     ===============

      call h5open_f(rc)
      if (isError(this,1,rc) .ne. 0) return

!     Open HDF5 file
!     ==============

      DAACcreate = -1

      call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, rc)
      if (rc .ne. 0) stop 1
      call h5pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, &
                                               H5F_LIBVER_LATEST_F, rc)
      if (rc .ne. 0) stop 2

      call h5fcreate_f(filename,H5F_ACC_TRUNC_F,fid,rc, &
                            creation_prp=H5P_DEFAULT_F, &
                            access_prp=fapl_id)
      if (rc .ne. 0) stop 3

      if (isError(this,2,rc,arg=filename) .ne. 0) return

!     The file ID itself is considered
!     to be the root group.

      DAAC%fid        = fid
      DAAC%fapl_id    = fapl_id
      DAAC%grp_id     = fid
      DAAC%groups(1)  = fid
      DAAC%num_groups = 1

!     Set up file properties
!     ======================

      call h5pcreate_f(H5P_DATASET_CREATE_F, DAAC%dcpl_id, rc)
      if (isError(this,4,rc) .ne. 0) return
!     call h5pset_szip_f(DAAC%dcpl_id, H5_SZIP_NN_OM_F, 8, rc)
!     if (isError(this,4,rc) .ne. 0) return

!     Set File Attributes
!     ===================

      call h5Amake(fid,'Title',fileInfo%title,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'Filename',filename,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'Source',fileInfo%source,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'Institution',fileInfo%institution,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'Conventions',fileInfo%conventions,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'History',fileInfo%history,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'Contact',fileInfo%contact,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'Comment',fileInfo%comment,rc)
      if (isError(this,3,rc) .ne. 0) return
      call h5Amake(fid,'References',fileInfo%references,rc)
      if (isError(this,3,rc) .ne. 0) return

!     Add Projection Definition
!     =========================

      gpd = GPDgetdef()
      rc  = writeElementGPD(gpd)

      DAACcreate = 0

      end function DAACcreate

!******************************************************************************
      integer function DAACopen(filename)
!******************************************************************************
!
! Purpose: Opens an HDF-5 file for read/write access.
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
! filename              string       IN  file name of HDF-5 file to be opened.
!
! DAACopen             integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error initializing HDF
!                                        2: error opening file
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

      character(len=*), intent(in) :: filename

!     Local Variables
!     ---------------

      integer :: rc
      integer(HID_T) :: fid, fapl_id
      integer :: this = OpenErrorHandle

      DAACopen = -1

!     Initialize HDF5
!     ===============

      call h5open_f(rc)
      if (isError(this,1,rc) .ne. 0) return

!     Open HDF5 file
!     ==============

      call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, rc)
      call h5pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, &
                                               H5F_LIBVER_LATEST_F, rc)

      call h5fopen_f(filename,H5F_ACC_RDWR_F,fid,rc, &
                              access_prp=fapl_id)


      if (isError(this,2,rc,arg=filename) .ne. 0) return

!     The file ID itself is considered
!     to be the root group.

      DAAC%fid        = fid
      DAAC%fapl_id    = fapl_id
      DAAC%grp_id     = fid
      DAAC%groups(1)  = fid
      DAAC%num_groups = 1

      DAACopen = 0

      end function DAACopen

!******************************************************************************
      integer function DAACwriteGroup(group)
!******************************************************************************
! English Name: DAAC Write Group
! -------------
!
! Purpose: Creates an HDF-5 group
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. DAACcreate() or DAACopen() is a prerequisite
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! group          GroupInfoType       IN  data structure containing information
!                                        about the group being created.
!
! DAACwriteGroup       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error creating group
!                                        2: error opening existing group
!                                        3: maximum number of group leaf nodes
!                                           exceeded (see MAX_DEPTH)
!
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

      type (GroupInfoType), intent(in) :: group

!     Local Variables
!     ---------------

      integer :: rc
      integer :: num_groups
      logical :: link_exists
      integer(HID_T) :: loc_id
      integer :: this = WriteGroupErrorHandle

      rc = 0
      DAACwriteGroup = -1

!     Simply exit if the root
!     group ("/") is requested.
!     -------------------------

      if (trim(group%name) .eq. '/') then

!       rc = DAACwriteCoordLink(DAAC%fid,group)
!       if (isError(this,3,rc,arg=group%name) .ne. 0) return

        DAACwriteGroup = 0
        return

      endif

!     Create an HDF5 group
!     ====================

      num_groups = DAAC%num_groups
      loc_id     = DAAC%groups(num_groups)

      if (num_groups .ge. MAX_DEPTH) rc = -1
      if (isError(this,3,rc,arg=group%name) .ne. 0) return

!     Create a new group as a descendant of
!     the most recent group. The root is
!     always the HDF file ID.
!     =====================================

      call h5lexists_f(loc_id, group%name, link_exists, rc)

      if (link_exists) then

        call h5Gopen_f(loc_id,group%name,DAAC%grp_id,rc)
        if (isError(this,2,rc,arg=group%name) .ne. 0) return

      else
        
        call h5Gcreate_f(loc_id,group%name,DAAC%grp_id,rc)
        if (isError(this,1,rc,arg=group%name) .ne. 0) return

      endif

!     Create links to the coordinate variables
!     ========================================

!     rc = DAACwriteCoordLink(DAAC%grp_id,group)
!     if (isError(this,3,rc,arg=group%name) .ne. 0) return

!     Update group master list and exit
!     =================================

      num_groups = DAAC%num_groups + 1

      DAAC%num_groups = num_groups
      DAAC%groups(num_groups) = DAAC%grp_id

      DAACwriteGroup = 0

      end function DAACwriteGroup

!******************************************************************************
      integer function DAACwriteCoordLink(id,group)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer(HID_T), intent(in) :: id
      type (GroupInfoType), intent(in) :: group

!     Local Variables
!     ---------------

      integer :: rc
      integer :: length, pos
      character (len=1024) :: pathname, name
      integer :: this = WriteCoordLinkErrorHandle

      rc = 0
      DAACwriteCoordLink = 0

      print *, '====> ', trim(group%name), trim(group%coordinates)

      length = len_trim(group%coordinates)
      if (length .eq. 0) return

      DAACwriteCoordLink = -1

      pos      = index(group%coordinates(1:length),' ')
      pathname = group%coordinates(1:pos-1)
      pos      = index(pathname,'/',back=.true.)
      name     = group%coordinates(pos+1:length)

      call h5lcreate_hard_f(DAAC%fid, pathname, id, 'latitude', rc)
      if (isError(this,1,rc,arg=group%name) .ne. 0) return

      pos      = index(group%coordinates(1:length),' ',back=.true.)
      pathname = group%coordinates(pos+1:length)
      pos      = index(pathname,'/',back=.true.)
      name     = group%coordinates(pos+1:length)

      call h5lcreate_hard_f(DAAC%fid, pathname, id, 'longitude', rc)
      if (isError(this,1,rc,arg=group%name) .ne. 0) return

      DAACwriteCoordLink = 0

      end function DAACwriteCoordLink

!******************************************************************************
      integer function DAACwriteElement(element)
!******************************************************************************
! English Name: Write Element
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated
! -------- attributes.
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
! element      ElementDataType       IN  data set to be written.
!
! writeElement         integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1:
!                                        2:
!
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer(HSIZE_T), dimension(3) :: chunks
      integer :: this = WriteElementErrorHandle

      DAACwriteElement = -1

      rank           = element%info%rank
      chunks(1:rank) = element%info%chunks(1:rank)

      call h5pset_chunk_f(DAAC%dcpl_id, rank, chunks, rc)
      if (isError(this,5,rc) .ne. 0) return

      if (isProjCoord(element)) then
        rc =  writeElementProj(element)
      elseif (associated(element%f32)) then
        rc = writeElementF32(element)
      elseif (associated(element%f64)) then
        rc = writeElementF64(element)
      elseif (associated(element%i32)) then
        rc = writeElementI32(element)
      elseif (associated(element%i16)) then
        rc = writeElementI16(element)
      elseif (associated(element%i8)) then
        rc = writeElementI8(element)
      elseif (associated(element%u32)) then
        rc = writeElementI32(element,sign=H5T_SGN_NONE_F)
      elseif (associated(element%u16)) then
        rc = writeElementI16(element,sign=H5T_SGN_NONE_F)
      elseif (associated(element%u8)) then
        rc = writeElementI8(element,sign=H5T_SGN_NONE_F)
      elseif (associated(element%buf)) then
        rc = writeElementStr(element)
      elseif (isTime(element)) then
        rc = writeElementTime(element)
      else
        rc = -1
        if (isError(this,4,rc,arg=element%info%name) .ne. 0) return
      endif

      if (rc .ne. 0) return

      if (isTime(element))      DAAC%time_id = DAAC%dset_id
      if (isLatitude(element))  DAAC%lat_id  = DAAC%dset_id
      if (isLongitude(element)) DAAC%lon_id  = DAAC%dset_id

      rc = DAACsetScale(element)
      if (rc .ne. 0) return

      rc = DAACattachScale(element)
      if (rc .ne. 0) return

      DAACwriteElement = 0

      end function DAACwriteElement

!******************************************************************************
      integer function DAACsetScale(element)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(in) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer(HID_T) :: dset_id
      integer :: this = SetScaleErrorHandle

      rc = 0
      DAACsetScale = -1

      if (isYProjCoord(element)) then 

        call h5Dopen_f(DAAC%fid, trim(element%info%name), dset_id, rc)
        if (isError(this,1,rc) .ne. 0) return
        call H5DSset_scale_f(dset_id, rc, 'y')
        if (isError(this,2,rc) .ne. 0) return

        call h5Dclose_f(dset_id,rc)

      elseif (isXProjCoord(element)) then

        call h5Dopen_f(DAAC%fid, trim(element%info%name), dset_id, rc)
        if (isError(this,1,rc) .ne. 0) return
        call H5DSset_scale_f(dset_id, rc, 'x')
        if (isError(this,2,rc) .ne. 0) return

        call h5Dclose_f(dset_id,rc)

      endif

      DAACsetScale = 0

      end function DAACsetScale

!******************************************************************************
      integer function DAACattachScale(element)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(in) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer(HID_T) :: dset_id, scale_id
      integer :: this = AttachScaleErrorHandle

      rc = 0
      DAACattachScale = 0

      if (.not. isGrid(element)) return

      DAACattachScale = -1

      print *, 'Attaching dimension scales to ', trim(element%info%name)

      call h5Dopen_f(DAAC%grp_id, trim(element%info%name), dset_id, rc)
      if (isError(this,1,rc) .ne. 0) return

!     Attach longitude dimension scale
!     ================================

      call h5Dopen_f(DAAC%fid, 'x', scale_id, rc)
      if (isError(this,1,rc) .ne. 0) return

      call h5DSattach_scale_f(dset_id, scale_id, 2, rc)
      if (isError(this,2,rc) .ne. 0) return

      call h5Dclose_f(scale_id,rc)

!     Attach latitude dimension scale
!     ===============================

      call h5Dopen_f(DAAC%fid, 'y', scale_id, rc)
      if (isError(this,1,rc) .ne. 0) return

      call h5DSattach_scale_f(dset_id, scale_id, 1, rc)
      if (isError(this,2,rc) .ne. 0) return

      call h5Dclose_f(scale_id,rc)

      DAACattachScale = 0
      call h5Dclose_f(dset_id,rc)

      end function DAACattachScale

!******************************************************************************
      integer function DAACwriteAttribute(attribute)
!******************************************************************************
! English Name: Write Attribute
! -------------
!
! Purpose: Writes an HDF attribute.
! -------- 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. DAACopen() or DAACcreate() are prerequisites
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! attribute  AttributeDataType       IN  attribute to be written
!
! DAACwriteAttribute   integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: error creating attribute
!                                        2: unknown attribute type
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

      real*4    :: f32
      real*8    :: f64
      integer*1 :: i8
      integer*2 :: i16
      integer*4 :: i32

      integer   :: rc
      integer   :: n
      logical   :: isString
      integer   :: this = WriteAttributeErrorHandle

      rc = 0
      DAACwriteAttribute = -1

      isString = (maxval(attribute%bit_size) .eq. 0)

!     Invoke the appropriate attribute method
!     based on the attribute type.
!     =======================================

      if (associated(attribute%f32)) then

        n = SDStransfer(attribute%str,attribute%f32,SDS_F32_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,attribute%f32,rc)

      elseif (associated(attribute%f64)) then

        n = SDStransfer(attribute%str,attribute%f64,SDS_F64_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,attribute%f64,rc)

      elseif (associated(attribute%i8)) then

        n = SDStransfer(attribute%str,attribute%i8,SDS_I8_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,attribute%i8,rc)

      elseif (associated(attribute%i16)) then

        n = SDStransfer(attribute%str,attribute%i16,SDS_I16_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,attribute%i16,rc)

      elseif (associated(attribute%i32)) then

        n = SDStransfer(attribute%str,attribute%i32,SDS_I32_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,attribute%i32,rc)

      elseif (associated(attribute%str)) then

        n = SDStransfer(attribute%str,attribute%str,SDS_STR_NULL)
        call h5Amake(DAAC%grp_id,attribute%name,attribute%str,rc)

      elseif (isString) then

        call h5Amake(DAAC%grp_id,attribute%name,attribute%value(1),rc)

      elseif (attribute%bit_size(1) .eq. 32) then

        f32 = SDStransfer(attribute%value(1),SDS_F32_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,f32,rc)

      elseif (attribute%bit_size(1) .eq. 64) then

        f64 = SDStransfer(attribute%value(1),SDS_F64_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,f64,rc)

      elseif (attribute%bit_size(2) .eq.  8) then

        i8  = SDStransfer(attribute%value(1),SDS_I8_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,i8 ,rc)

      elseif (attribute%bit_size(2) .eq. 16) then

        i16 = SDStransfer(attribute%value(1),SDS_I16_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,i16,rc)

      elseif (attribute%bit_size(2) .eq. 32) then

        i32 = SDStransfer(attribute%value(1),SDS_I32_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,i32,rc)

      elseif (attribute%bit_size(3) .eq.  8) then

        i8  = SDStransfer(attribute%value(1),SDS_U8_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,i8 ,rc,sign=H5T_SGN_NONE_F)

      elseif (attribute%bit_size(3) .eq. 16) then

        i16 = SDStransfer(attribute%value(1),SDS_U16_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,i16,rc,sign=H5T_SGN_NONE_F)

      elseif (attribute%bit_size(3) .eq. 32) then

        i32 = SDStransfer(attribute%value(1),SDS_U32_FILL)
        call h5Amake(DAAC%grp_id,attribute%name,i32,rc,sign=H5T_SGN_NONE_F)

      else

        rc = -1
        if (isError(this,2,rc,arg=attribute%long_name) .ne. 0) return

      endif

      if (isError(this,1,rc,arg=attribute%long_name) .ne. 0) return

      DAACwriteAttribute = 0

      end function DAACwriteAttribute

!******************************************************************************
      integer function writeElementTime(element)
!******************************************************************************
! English Name: Write Time
! -------------
!
! Purpose: Creates the time dimension element on the HDF-5 data set.
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
!           03/14/2013      J.Ardizzone  created.
!           05/06/2014      J.Ardizzone  changed reference time to exactly
!                                        noon on 1/1/2000.
!******************************************************************************
      implicit none

!     Argument List
!     -------------

      type (ElementDataType), intent(IN) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank = 1
      integer :: numSeconds
      integer :: ndate,ntime
      integer :: date,time,tstep
      integer :: hour,minute,second
      integer(HID_T) :: type_id,space_id
      integer(HSIZE_T), dimension(1) :: dims = 1
      integer :: this = WriteElementErrorHandle

!     Some confusion on J2000 Epoch offset.
!     integer :: secondsToNoon = 11 * 3600 + 58 * 60
      integer :: secondsToNoon = 12 * 3600

      real*8, dimension(2) :: range
      real*8, dimension(1) :: seconds

      character (len=19) :: delta_t = '0000-00-00 00:00:00'
      character (len=37) :: units = 'seconds since 2000-01-01 11:58:55.816'

!     Initialize
!     ==========

      writeElementTime = -1

      date  = element%info%date
      time  = element%info%time
      tstep = element%info%tstep

!     Compute seconds referenced to J2000 Epoch
!     =========================================

      numSeconds = tm_seconds(2000,date,time)
      numSeconds = numSeconds - secondsToNoon

!     Some confusion on J2000 Epoch offset.
!     seconds(1) = numSeconds - 55.816D0
      seconds(1) = dble(numSeconds)

      range      = seconds(1)

      ndate = 20000101; ntime = 0
      call tm_inctime(ndate,ntime,0,0,tstep)
      call tm_decode(ntime,hour,minute,second)
      write(unit=delta_t(12:13),fmt='(i2.2)') hour
      write(unit=delta_t(15:16),fmt='(i2.2)') minute
      write(unit=delta_t(18:19),fmt='(i2.2)') second

!     Create the HDF time element
!     ===========================

      CALL h5tcopy_f(H5T_NATIVE_DOUBLE, type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(type_id, 8, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims, space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, 'time', &
                     type_id, space_id, DAAC%dset_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,type_id,seconds,dims,rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'actual_range',range,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'delta_t',delta_t,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Tclose_f(type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(space_id, rc)

      writeElementTime = 0

      end function writeElementTime

!******************************************************************************
      integer function writeElementF32(element)
!******************************************************************************
! English Name: Write Element (32-bit Real)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for 32-bit floating point data. 
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
! element      ElementDataType       IN  data set to be written.
!
! writeElementF32      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer(HSIZE_T), dimension(3) :: dims
      integer :: this = WriteElementErrorHandle

      real*4 :: fill_value
      character (len=4) :: fill_buf
      real*4 :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementF32 = -1

      rank         = element%info%rank
      dims(1:rank) = element%info%dims(1:rank)

      element%buf = transfer(element%f32,element%buf)
      valid_min   = SDStransfer(element%info%valid_min, SDS_F32_FILL)
      valid_max   = SDStransfer(element%info%valid_max, SDS_F32_FILL)
      fill_value  = SDStransfer(element%info%fill_value,SDS_F32_FILL)
      fill_buf    = transfer(fill_value,fill_buf)

!     write(99) element%buf

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_REAL, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, 4, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_buf, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id,element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping','EASE2_global_projection',rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'DIMENSION_LABELS', (/'y', 'x'/), rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_min',valid_min,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_max',valid_max,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'_FillValue',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'missing_value',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'fmissing_value',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      if (len_trim(element%info%coordinates) .gt. 0) then
        call h5Amake(DAAC%dset_id,'coordinates',element%info%coordinates,rc)
        if (isError(this,3,rc,arg=element%info%name) .ne. 0) return
      endif

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementF32 = 0

      end function writeElementF32

!******************************************************************************
      integer function writeElementF64(element)
!******************************************************************************
! English Name: Write Element (64-bit Real)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for 64-bit floating point data. 
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
! element      ElementDataType       IN  data set to be written.
!
! writeElementF64      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer(HSIZE_T), dimension(3) :: dims
      integer :: this = WriteElementErrorHandle

      real*8 :: fill_value
      character (len=8) :: fill_buf
      real*8 :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementF64 = -1

      rank         = element%info%rank
      dims(1:rank) = element%info%dims(1:rank)

      element%buf = transfer(element%f64,element%buf)
      valid_min   = SDStransfer(element%info%valid_min, SDS_F64_FILL)
      valid_max   = SDStransfer(element%info%valid_max, SDS_F64_FILL)
      fill_value  = SDStransfer(element%info%fill_value,SDS_F64_FILL)
      fill_buf    = transfer(fill_value,fill_buf)

!     write(99) element%buf

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_DOUBLE, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, 8, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_buf, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id, &
                                     element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping','EASE2_global_projection',rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'DIMENSION_LABELS', (/'y', 'x'/), rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_min',valid_min,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_max',valid_max,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'_FillValue',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'missing_value',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'fmissing_value',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      if (len_trim(element%info%coordinates) .gt. 0) then
        call h5Amake(DAAC%dset_id,'coordinates',element%info%coordinates,rc)
        if (isError(this,3,rc,arg=element%info%name) .ne. 0) return
      endif

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementF64 = 0

      end function writeElementF64

!******************************************************************************
      integer function writeElementProj(element)
!******************************************************************************
! English Name: Write Element (Projection Coordinate)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for a projection coordinate.
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
! element      ElementDataType       IN  data set to be written.
!
! writeElementProj     integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/24/2018      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer(HSIZE_T), dimension(3) :: dims
      integer :: this = WriteElementErrorHandle

      real*8 :: fill_value
      character (len=8) :: fill_buf
      real*8 :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementProj = -1

      rank         = element%info%rank
      dims(1:rank) = element%info%dims(1:rank)

      element%buf = transfer(element%f64,element%buf)

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_DOUBLE, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, 8, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_buf, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id, &
                                     element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'standard_name',element%info%standard_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'axis',element%info%axis,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementProj = 0

      end function writeElementProj

!******************************************************************************
      integer function writeElementI8(element,sign)
!******************************************************************************
! English Name: Write Element (8-bit Integer)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for 8-bit integer data. 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. See optional "sign" argument for specifying unsigned integer types.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! element      ElementDataType       IN  data set to be written.
!
! sign                 integer   OPT,IN  HDF-5 sign type:
!
!                                        unsigned: H5T_SGN_NONE_F
!                                        signed: H5T_SGN_2_F (default)
!
! writeElementI8       integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer :: sign_type
      integer, intent(in), optional :: sign
      integer(HSIZE_T), dimension(3) :: dims
      integer*1, dimension(:), pointer :: i8
      integer :: this = WriteElementErrorHandle

      integer*1 :: default
      integer*1 :: fill_value
      character (len=1) :: fill_buf
      integer*1 :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementI8 = -1

      i8           => element%i8
      default      = SDS_I8_FILL
      rank         = element%info%rank
      dims(1:rank) = element%info%dims(1:rank)

      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign
      if (sign_type .eq. H5T_SGN_NONE_F) i8 => element%u8
      if (sign_type .eq. H5T_SGN_NONE_F) default = SDS_U8_FILL

      element%buf = transfer(i8,element%buf)
      valid_min   = SDStransfer(element%info%valid_min, default)
      valid_max   = SDStransfer(element%info%valid_max, default)
      fill_value  = SDStransfer(element%info%fill_value,default)
      fill_buf    = transfer(fill_value,fill_buf)

!     write(99) element%buf

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, 1, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_sign_f(DAAC%type_id, sign_type, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_buf, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id,element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping','EASE2_global_projection',rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'DIMENSION_LABELS', (/'y', 'x'/), rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_min',valid_min,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_max',valid_max,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'_FillValue',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'missing_value',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'fmissing_value',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      if (len_trim(element%info%coordinates) .gt. 0) then
        call h5Amake(DAAC%dset_id,'coordinates',element%info%coordinates,rc)
        if (isError(this,3,rc,arg=element%info%name) .ne. 0) return
      endif

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementI8 = 0

      end function writeElementI8

!******************************************************************************
      integer function writeElementI16(element,sign)
!******************************************************************************
! English Name: Write Element (16-bit Integer)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for 16-bit integer data. 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. See optional "sign" argument for specifying unsigned integer types.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! element      ElementDataType       IN  data set to be written.
!
! sign                 integer   OPT,IN  HDF-5 sign type:
!
!                                        unsigned: H5T_SGN_NONE_F
!                                        signed: H5T_SGN_2_F (default)
!
! writeElementI16      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer :: sign_type
      integer, intent(in), optional :: sign
      integer(HSIZE_T), dimension(3) :: dims
      integer*2, dimension(:), pointer :: i16
      integer :: this = WriteElementErrorHandle

      integer*2 :: default
      integer*2 :: fill_value
      character (len=2) :: fill_buf
      integer*2 :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementI16 = -1

      i16          => element%i16
      default      = SDS_I16_FILL
      rank         = element%info%rank
      dims(1:rank) = element%info%dims(1:rank)

      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign
      if (sign_type .eq. H5T_SGN_NONE_F) i16 => element%u16
      if (sign_type .eq. H5T_SGN_NONE_F) default = SDS_U16_FILL

      element%buf = transfer(i16,element%buf)
      valid_min   = SDStransfer(element%info%valid_min, default)
      valid_max   = SDStransfer(element%info%valid_max, default)
      fill_value  = SDStransfer(element%info%fill_value,default)
      fill_buf    = transfer(fill_value,fill_buf)

!     write(99) element%buf

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, 2, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_sign_f(DAAC%type_id, sign_type, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_buf, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id,element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping','EASE2_global_projection',rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'DIMENSION_LABELS', (/'y', 'x'/), rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_min',valid_min,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_max',valid_max,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'_FillValue',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'missing_value',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'fmissing_value',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      if (len_trim(element%info%coordinates) .gt. 0) then
        call h5Amake(DAAC%dset_id,'coordinates',element%info%coordinates,rc)
        if (isError(this,3,rc,arg=element%info%name) .ne. 0) return
      endif

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementI16 = 0

      end function writeElementI16

!******************************************************************************
      integer function writeElementI32(element,sign)
!******************************************************************************
! English Name: Write Element (32-bit Integer)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for 32-bit integer data. 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. See optional "sign" argument for specifying unsigned integer types.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! element      ElementDataType       IN  data set to be written.
!
! sign                 integer   OPT,IN  HDF-5 sign type:
!
!                                        unsigned: H5T_SGN_NONE_F
!                                        signed: H5T_SGN_2_F (default)
!
! writeElementI32      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer :: sign_type
      integer, intent(in), optional :: sign
      integer(HSIZE_T), dimension(3) :: dims
      integer*4, dimension(:), pointer :: i32
      integer :: this = WriteElementErrorHandle

      integer*4 :: default
      integer*4 :: fill_value
      character (len=4) :: fill_buf
      integer*4 :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementI32 = -1

      i32          => element%i32
      default      = SDS_I32_FILL
      rank         = element%info%rank
      dims(1:rank) = element%info%dims(1:rank)

      sign_type = H5T_SGN_2_F
      if (present(sign)) sign_type = sign
      if (sign_type .eq. H5T_SGN_NONE_F) i32 => element%u32
      if (sign_type .eq. H5T_SGN_NONE_F) default = SDS_U32_FILL

      element%buf = transfer(i32,element%buf)
      valid_min   = SDStransfer(element%info%valid_min, default)
      valid_max   = SDStransfer(element%info%valid_max, default)
      fill_value  = SDStransfer(element%info%fill_value,default)
      fill_buf    = transfer(fill_value,fill_buf)

!     write(99) element%buf

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_INTEGER, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, 4, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_sign_f(DAAC%type_id, sign_type, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_buf, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id,element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping','EASE2_global_projection',rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'DIMENSION_LABELS', (/'y', 'x'/), rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_min',valid_min,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_max',valid_max,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'_FillValue',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'missing_value',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'fmissing_value',fill_value,rc,sign=sign_type)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      if (len_trim(element%info%coordinates) .gt. 0) then
        call h5Amake(DAAC%dset_id,'coordinates',element%info%coordinates,rc)
        if (isError(this,3,rc,arg=element%info%name) .ne. 0) return
      endif

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementI32 = 0

      end function writeElementI32

!******************************************************************************
      integer function writeElementStr(element)
!******************************************************************************
! English Name: Write Element (String)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for data composed of character strings. 
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
! element      ElementDataType       IN  data set to be written.
!
! writeElementStr      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           03/18/2013      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (ElementDataType), intent(inout) :: element

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer(SIZE_T) :: length
      integer(HSIZE_T), dimension(3) :: dims
      integer :: this = WriteElementErrorHandle

      character (len=MAX_STR_LEN) :: fill_value
      character (len=MAX_STR_LEN) :: valid_min,valid_max

!     Initialize
!     ==========

      writeElementStr = -1

      rank         = element%info%rank
      length       = element%info%bit_size(4) / 8
      dims(1:rank) = element%info%dims(1:rank)

      valid_min   = SDStransfer(element%info%valid_min, SDS_STR_FILL)
      valid_max   = SDStransfer(element%info%valid_max, SDS_STR_FILL)
      fill_value  = SDStransfer(element%info%fill_value,SDS_STR_FILL)

!     write(99) element%buf

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_CHARACTER, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, length, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5tset_strpad_f(DAAC%type_id, H5T_STR_NULLTERM_F, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Pset_fill_value_f(DAAC%dcpl_id, DAAC%type_id, fill_value, rc)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, element%info%name, &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=element%info%name) .ne. 0) return

      call h5Dwrite_f(DAAC%dset_id,DAAC%type_id, &
                                     element%buf,dims(1:rank),rc)
      if (isError(this,2,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'long_name',element%info%long_name,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'units',element%info%units,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping','EASE2_global_projection',rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'DIMENSION_LABELS', (/'y', 'x'/), rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_min',valid_min,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'valid_max',valid_max,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'_FillValue',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'missing_value',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'fmissing_value',fill_value,rc)
      if (isError(this,3,rc,arg=element%info%name) .ne. 0) return

      if (len_trim(element%info%coordinates) .gt. 0) then
        call h5Amake(DAAC%dset_id,'coordinates',element%info%coordinates,rc)
        if (isError(this,3,rc,arg=element%info%name) .ne. 0) return
      endif

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementStr = 0

      end function writeElementStr

!******************************************************************************
      integer function writeElementGPD(gpd)
!******************************************************************************
! English Name: Write Element (Grid Product Definition)
! -------------
!
! Purpose: Writes an HDF scientific data set (element) and associated attributes
! -------- for data comprising a grid product definition.
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
! gpd         GridParamDefType       IN  GPD information.
!
! writeElementGPD      integer      OUT  function return value:
!
!                                        0: normal
!                                       -1: exception occurred
!
!                                        Thrown Exceptions
!
!                                        1: Error creating data space
!                                        2: Error writing the element
!                                        3: Error writing an attribute
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           04/26/2018      J.Ardizzone  created.
!******************************************************************************

!     Argument List
!     -------------

      type (GridParamDefType), intent(in) :: gpd

!     Local Variables
!     ---------------

      integer :: rc
      integer :: rank
      integer(SIZE_T) :: length
      integer(HSIZE_T), dimension(3) :: dims
      integer :: this = WriteElementErrorHandle

      real*8 :: standard_parallel
      real*8 :: false_northing
      real*8 :: false_easting
      real*8 :: central_lon
      
      standard_parallel = gpd%map_second_reference_latitude
      false_northing    = gpd%map_reference_latitude
      false_easting     = gpd%map_reference_longitude
      central_lon       = gpd%map_reference_longitude

!     Initialize
!     ==========

      writeElementGPD = -1

      rank         = 1
      length       = 1
      dims(1:rank) = 1

!     Create HDF-5 Element and 
!     associated attributes
!     ========================

      CALL h5tcopy_f(H5T_NATIVE_CHARACTER, DAAC%type_id, rc)
      if (isError(this,1,rc,arg=gpd%name) .ne. 0) return

      CALL h5tset_size_f(DAAC%type_id, length, rc)
      if (isError(this,1,rc,arg=gpd%name) .ne. 0) return

      call h5tset_strpad_f(DAAC%type_id, H5T_STR_NULLTERM_F, rc)
      if (isError(this,1,rc,arg=gpd%name) .ne. 0) return

      call h5Screate_simple_f(rank, dims(1:rank), DAAC%space_id, rc)
      if (isError(this,1,rc,arg=gpd%name) .ne. 0) return

      call h5Dcreate_f(DAAC%grp_id, 'EASE2_global_projection', &
           DAAC%type_id, DAAC%space_id, DAAC%dset_id, rc, dcpl_id=DAAC%dcpl_id)
      if (isError(this,1,rc,arg=gpd%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'grid_mapping_name',gpd%mapping_name,rc)
      if (isError(this,3,rc,arg=gpd%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'standard_parallel', standard_parallel, rc)
      if (isError(this,3,rc,arg=gpd%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'false_easting', false_easting,rc)
      if (isError(this,3,rc,arg=gpd%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'false_northing', false_northing, rc)
      if (isError(this,3,rc,arg=gpd%name) .ne. 0) return

      call h5Amake(DAAC%dset_id,'longitude_of_central_meridian',central_lon,rc)
      if (isError(this,3,rc,arg=gpd%name) .ne. 0) return

      call h5Tclose_f(DAAC%type_id, rc)
      call h5Dclose_f(DAAC%dset_id, rc)
      call h5Sclose_f(DAAC%space_id, rc)

      writeElementGPD = 0

      end function writeElementGPD

!******************************************************************************
      integer function DAACcloseGroup(group)
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      type (GroupInfoType), intent(out) :: group

!     Local Variables
!     ---------------

      integer :: rc
      integer :: num_groups

      rc = 0
      DAACcloseGroup = 0

!     The base "group" is the root which
!     has a location ID equal to the file
!     ID. This function will not close the
!     file and will exit when no descendant
!     groups exist.
!     =====================================

      group%num_elements = 0
      num_groups = DAAC%num_groups
      if (num_groups .le. 1) return

!     Close the lowest descendant of the current
!     group hierarchy.
!     ==========================================

      call h5Gclose_f(DAAC%groups(num_groups), rc)

      num_groups = num_groups - 1
      DAAC%num_groups = num_groups

!     Subtract one to get the total number
!     of remaining descendant groups since
!     the root group does not count as a
!     descendant.
!     ====================================

      group%name = ' '
      group%num_elements = num_groups - 1

      end function DAACcloseGroup

!******************************************************************************
      integer function DAACcloseFile()
!******************************************************************************

      implicit none

      integer :: rc
      integer :: num_groups

!     First close all descendant groups
!     of the root group (file id).
!     =================================

      num_groups      = DAAC%num_groups
      DAAC%num_groups = 0

      do while (num_groups .gt. 1)
        call h5Gclose_f(DAAC%groups(num_groups), rc)
        num_groups = num_groups - 1
      end do

      call h5pclose_f(DAAC%fapl_id,rc)
      call h5pclose_f(DAAC%dcpl_id, rc)
      call h5fclose_f(DAAC%groups(1), rc)
      call h5close_f(rc)

      DAACcloseFile = 0

      end function DAACcloseFile

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

!       Open Error Handle
!       =================

        case (OpenErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'DAACcreate (error#',code,'): error initializing HDF'

          case (2)

            print *, 'DAACopen (error#',code,'): error opening HDF file: ', &
                                                          '"',trim(arg),'"'

          case default

            print *, 'DAACopen (error#',code,'): unknown error'

        end select

!       Create Error Handle
!       ===================

        case (CreateErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'DAACcreate (error#',code,'): error initializing HDF'

          case (2)

            print *, 'DAACcreate (error#',code,'): error creating file: "', &
                                                               trim(arg),'"'

          case (3)

            print *, 'DAACcreate (error#',code,'): error writing attributes'

          case (4)

            print *, 'DAACcreate (error#',code,'): error creating ', &
                                                      'file properties'

          case default

            print *, 'DAACcreate (error#',code,'): unknown error'

        end select

!       Write Group Error Handle
!       ========================

        case (WriteGroupErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'DAACwriteGroup (error#',code,'): error creating ', &
                                                    'group: "',trim(arg),'"'
          case (2)

            print *, 'DAACwriteGroup (error#',code,'): error opening ', &
                                                    'group: "',trim(arg),'"'

          case (3)

            print *, 'DAACwriteGroup (error#',code,'): max groups ', &
                                               'exceeded: "',trim(arg),'"'

          case default

            print *, 'DAACwriteGroup (error#',code,'): unknown error'

        end select

!       Write Element Error Handle
!       ==========================

        case (WriteElementErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'DAACwriteElement (error#',code,'): error creating ', &
                                              'dataset for: "',trim(arg),'"'

          case (2)

            print *, 'DAACwriteElement (error#',code,'): error writing ', &
                                              'dataset for: "',trim(arg),'"'

          case (3)

            print *, 'DAACwriteElement (error#',code,'): error writing ', &
                                            'attribute for: "',trim(arg),'"'

          case (4)

            print *, 'DAACwriteElement (error#',code,'): data type ', &
                                          'not supported for: "',trim(arg),'"'

          case (5)

            print *, 'DAACcreate (error#',code,'): error setting ', &
                                                      'compression properties'

          case default

            print *, 'DAACwriteElement (error#',code,'): unknown error'

        end select

!       Write Attribute Error Handle
!       ============================

        case (WriteAttributeErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'DAACwriteAttribute (error#',code,'): error creating ', &
                                        'attribute for: "',trim(arg),'"'

          case (2)

            print *, 'DAACwriteAttribute (error#',code,'): unknown ', &
                                      'attribute type for: "',trim(arg),'"'

          case default

            print *, 'DAACwriteAttribute (error#',code,'): unknown error'

        end select

!       Write Time Error Handle
!       =======================

        case (WriteTimeErrorHandle)

        select case (errorEvent)

          case (1)

            print *, 'DAACwriteTime (error#',code,'): error creating ', &
                                                           'time dimension'

          case default

            print *, 'DAACwriteTime (error#',code,'): unknown error'

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

      end module HDF5FormatMod
