      use time
      use GridMod, GridError=>errorHandler
      use LFOBaseMod, LFOBaseError=>errorHandler
#include "Corrector.h"

      implicit none

      integer :: rc
      integer :: idate,itime,endDate
      type (UserInterfaceType) :: args

      rc = getArgs(args)
      if (CorrectorError() .ne. 0) stop 1

      rc = LFOReadConfig(args%config)
      if (LFOBaseError() .ne. 0) stop 1

      idate   = args%start_date
      itime   = 0
      endDate = args%end_date

      do while (idate .le. endDate)

        rc = correctPrecip(idate)
        if (CorrectorError() .ne. 0) stop 1

        call tm_inctime(idate,itime,24,0,0)

      end do

      call GRIDDeallocate()

      stop
      end
