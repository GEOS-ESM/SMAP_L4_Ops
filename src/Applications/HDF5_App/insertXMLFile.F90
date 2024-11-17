!******************************************************************************
! English Name: Insert XML File
! -------------
!
! Purpose: Inserts the contents of an XML file into an existing HDF file
! -------- as a single string attribute.
!
! Language: Fortran 90
! ---------
!
! Notes:
! ------
!
! Usage: rc = insertXMLFile [HDF attribute] [XML File] [HDF File] [filesize]
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! HDF attribute         string       IN  Fully qualified pathname of the HDF
!                                        attribute to contain the XML string.
!                                        All group components of the pathname
!                                        must exist on the HDF output file.
!
! XML File              string       IN  filename of the file containing the
!                                        XML to be inserted into the HDF file
!                                        under the specified attribute name.
!
! HDF File              string       IN  filename of the output HDF file to
!                                        host the new XML attribute.
!
! filesize             integer       IN  size of the input XML file in bytes.
!
! rc                   integer      OUT  program exit status:
!
!                                        0: success
!                                        1: usage error
!                                        2: HDF file error 
!                                        3: HDF group path does not exist
!                                        4: XML file exceeds max size
!                                        5: XML file error
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Global Modeling and Assimilation Office - GMAO)
!
! Modified:       Date           Author  Description
! ---------   
!           05/31/2014      J.Ardizzone  created.
!******************************************************************************
      use hdf5
      use h5fortran_types
      use HDF5AttributeMod

      implicit none

      integer, parameter :: MAX_STRING_LENGTH = 8000000

!     Local Variables
!     ---------------

      integer :: pos
      integer :: reclen
      integer :: rc = 0
      integer :: filesize
      logical :: link_exists
      integer(HID_T) :: fid,grp_id

      character (len=MAX_STRING_LENGTH) :: xml
      character (len=1024) :: XML_file, HDF_file
      character (len=1024) :: pathname, path, name, str

!     Retrieve the command-line arguments
!     ===================================

      if (command_argument_count() .ne. 4) then
        print *, 'insertXMLFile [HDF pathname] [XML File] [HDF File] [filesize]'
        stop 1
      endif

      call get_command_argument(1,pathname)
      call get_command_argument(2,XML_file)
      call get_command_argument(3,HDF_file)
      call get_command_argument(4,str)

      read(unit=str,fmt='(i)') reclen

!     Parse the path and attribute name from
!     the pathname.
!     ======================================

      pos = index(pathname,'/',back=.true.)

      if (pos .le. 0) then
        name = pathname
        path = '/'
      else
        name = pathname(pos+1:len_trim(pathname))
        path = pathname(1:pos-1)
      endif

      if (len_trim(name) .le. 0) then
        print *, 'insertXMLFile: no attribute name specified: ',trim(pathname)
        stop 2
      endif

      if (len_trim(path) .le. 0) path = '/'

      print *, '"',trim(path),'" "',trim(name),'"'

!     Open the HDF file and retrieve the group id
!     ===========================================

      call h5open_f(rc)

      if (rc .ne. 0) then
        print *, 'insertXMLFile: error initializing HDF'
        stop 2
      endif

!     Open HDF5 file
!     ==============

      call h5fopen_f(HDF_file,H5F_ACC_RDWR_F,fid,rc)

      if (rc .ne. 0) then
        print *, 'insertXMLFile: error opening HDF file: "',trim(HDF_file),'"'
        stop 2
      endif

!     Retrieve Group ID
!     =================

      call h5lexists_f(fid, path, link_exists, rc)

      if (.not. link_exists) then
        print *, 'insertXMLFile: the group does not exist: "',trim(path),'"'
        stop 3
      endif

      call h5Gopen_f(fid, path, grp_id, rc)

      if (rc .ne. 0) then
        print *, 'insertXMLFile: HDF error accessing group: "',trim(path),'"'
        stop 2
      endif

!     Open and read the XML file
!     ==========================

      if (reclen .ge. MAX_STRING_LENGTH) then
        print *, 'insertXMLFile: XML file exceeds max size'
        stop 4
      endif

      open (unit=8, file=XML_file, form='unformatted', access='direct', &
            recl=reclen, status='old', iostat=rc)

      if (rc .ne. 0) then
        print *, 'insertXMLFile: unable to open XML file: "',trim(XML_file),'"'
        stop 5
      endif

      read(8,rec=1,iostat=rc) xml(1:reclen)

      if (rc .ne. 0) then
        print *, 'insertXMLFile: error reading XML file: "',trim(XML_file),'"'
        stop 5
      endif

      close (unit=8)

!     Write the XML string
!     ====================

      xml(reclen+1:reclen+1) = char(0)
      call h5Amake(grp_id, name, xml(1:reclen+1), rc)

      if (rc .ne. 0) then
        print *, 'insertXMLFile: error writing attribute: "',trim(pathname),'"'
        stop 2
      endif

!     Finalize and exit
!     =================

      call h5Gclose_f(grp_id,rc)
      call h5Fclose_f(fid,rc)

      stop
      end
