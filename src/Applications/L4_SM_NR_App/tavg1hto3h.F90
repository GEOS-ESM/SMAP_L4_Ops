      use time
      use LDAS_TAVG_Mod, TAVGerror=>errorHandler

      implicit none

!     Local Variables
!     ---------------

      integer :: rc
      integer :: idate,itime,edate,etime

      character (len=1024) :: str
      character (len=1024) :: in_template
      character (len=1024) :: out_template

!     Check Usage
!     ===========

      if (iargc() .ne. 3) then

        print *, 'Usage: tavg1hto3h [ccyymmdd] [input file template] ', &
                                                  '[output file template]'
        stop 1

      endif

!     Retrieve arguments
!     ==================

      call getarg(1,str)
      read(unit=str,fmt='(i8)') idate

      call getarg(2,in_template)
      call getarg(3,out_template)

      itime = 13000
      edate = idate
      etime = 223000

!     Generate 3-hourly time averages from hourly averages
!     ====================================================

      do while (tm_compare(idate,itime,edate,etime) .le. 0)

        rc = TAVGopen(in_template,idate,itime,offset = -60)
        if (TAVGerror() .ne. 0) stop 2
        rc = TAVGopen(in_template,idate,itime,offset =   0)
        if (TAVGerror() .ne. 0) stop 2
        rc = TAVGopen(in_template,idate,itime,offset =  60)
        if (TAVGerror() .ne. 0) stop 2

        rc = TAVGwrite(out_template,idate,itime)
        if (TAVGerror() .ne. 0) stop 3

        call tm_inctime(idate,itime,3,0,0)

      end do

      stop
      end
