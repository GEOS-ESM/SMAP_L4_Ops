      use typesMod
      use insertMetaGroupsMod, MetaError=>errorHandler

      implicit none

!     Local Variables
!     ---------------

      integer :: rc
      character (len=MAX_STR_LEN) :: HDF5Fname

      type (CommandLineType)   :: args
      type (AttributeDataType) :: attribute

!     Retrieve input arguments.
!     =========================

      rc = getArgs(args)
      if (rc .eq. 1) stop
      if (MetaError() .ne. 0) stop 1

!     Begin Processing
!     ================

      rc = METAopen(args%HDF5fname)
      if (METAerror() .ne. 0) stop 2

      do while (METAread(attribute) .eq. 0)

        rc = METAwrite(attribute)
        if (METAError() .ne. 0) stop 3

        print *, trim(attribute%long_name)

      end do

      if (METAError() .ne. 0) stop 4

      rc = METAclose()

      stop
      end
