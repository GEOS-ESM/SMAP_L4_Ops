      use typesMod
      use time
      use interfaceMod, interfaceError=>errorHandler
      use LDAS_Mod, LDASError=>errorHandler
      use HDF5FormatMod, DAACError=>errorHandler

      implicit none

!     Local Variables
!     ---------------

      integer :: rc
      integer :: tstep
      integer :: idate,itime
      integer :: indate,intime,iedate,ietime

      character (len=MAX_STR_LEN) :: inFname
      character (len=MAX_STR_LEN) :: outFname
      character (len=MAX_STR_LEN) :: inFnameTmplt
      character (len=MAX_STR_LEN) :: outFnameTmplt

      type (CommandLineType) :: args
      type (GroupInfoType)   :: group
      type (FileInfoType)    :: fileInfo
      type (ElementDataType) :: element

!     Retrieve input arguments.
!     =========================

!     open(unit=99,file='/discover/nobackup/jardizzo/granule.bin', &
!          form='unformatted',status='replace')

      rc = getArgs(args)
      if (rc .eq. 1) stop
      if (interfaceError() .ne. 0) stop 1

      indate = args%startDate
      intime = args%startTime
      iedate = args%endDate
      ietime = args%endTime
      tstep  = args%timeStep

      inFnameTmplt  = args%LDASFnameTmplt
      outFnameTmplt = args%DAACFnameTmplt

!     Begin Processing
!     ================

      idate = indate
      itime = intime

      do while (tm_compare(idate,itime,iedate,ietime) .le. 0)

        fileInfo%date  = idate
        fileInfo%time  = itime
        fileInfo%tstep = tstep

        call tm_string(idate,itime,inFnameTmplt,inFname)
        call tm_string(idate,itime,outFnameTmplt,outFname)

        print *, ' '
        print *, idate,itime
        print *, trim(inFname)
        print *, trim(outFname)

        rc = LDASopen(inFname,fileInfo)
        if (LDASError() .ne. 0) stop 2

        rc = DAACcreate(outFname,fileInfo)
        if (DAACerror() .ne. 0) stop 3

        do while (LDASread(group) .eq. 0)

          if (LDASError() .ne. 0) stop 2

          rc = DAACwrite(group)
          if (DAACerror() .ne. 0) stop 3

          element%info%shave_bits = group%shave_bits

          do while (LDASread(element) .eq. 0)

            if (LDASError() .ne. 0) stop 2

            element%info%date  = idate
            element%info%time  = itime
            element%info%tstep = tstep

            rc = DAACwrite(element)
            if (DAACerror() .ne. 0) stop 3

            print *, trim(element%info%name),' (',trim(element%info%units),')'

            element%info%shave_bits = group%shave_bits

          end do

          rc = DAACclose(group)
          if (DAACerror() .ne. 0) stop 3

        end do

        rc = LDASclose()
        rc = DAACclose()

        call tm_inctime(idate,itime,0,0,tstep)

      end do

      stop
      end
