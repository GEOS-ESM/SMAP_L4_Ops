!******************************************************************************
      module time
!******************************************************************************
! English Name: Time
! -------------
!
! Purpose: Provides utilities for managing the date/time.
! -------- 
!
! Language: Fortran 90
! ---------
!
! Notes: 1. The tm_string() function recognizes the following time tokens
! ------    in addition to the Unix 'date' command tokens:
!
!           a.) offset token of the form: %sTxx
!
!           where
!
!      s - "+" or "-" for addition of subtraction
!      T - time token offset unit. This can be y,m,d,H,M or S for
!          year, month, day, hour, minute, second. No other tokens
!          are recognized for offsets.
!     xx - two digit integer representing the time increment/decrement
!
!  Offsets only apply to the token string immediately preceeding the offset.
!  Any non-token sequence acts as a separator. Brackets "{ }" can be used to
!  encapsulate string segments for an offset:
!
!       Examples:
!
!       timetag 20000228 0 "day:%d, year:%Y, month:%B%+d02"
!     >>day:28, year:2000, month:March
!
!       timetag 20000228 0 "{day:%d, year:%Y, month:%B}%+d02"
!     >>day:01, year:2000, month:March
!
!      b.) the "^" symbol can be used to lower the case of the resolved
!          time token or to remove a leading zero or blank.
!
!     examples:
!
!        timetag 20000228 0 "%d%B%Y"
!      >>28February2000
!
!        timetag 20000228 0 "%d^%B%Y"
!      >>28february2000
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! tm_string()       subroutine      PUB  resolves time tokens according to
!                                        a specified date/time. Tokens are
!                                        defined by the Unix "date" command. 
!                                        GrADS tokens are also resolvable (see 
!                                        optional "grads" argument). Available
!                                        tokens are:
!                                        Resolves time tokens in an input 
!                                        character string according to the 
!                                        specified date/time. See the "date" 
!                                        command for a list of valid tokens. 
!                                        Also, see note-1 for a description of 
!                                        additional time tokens for 
!                                        incrementing/decrementing the 
!                                        date/time.
!
!                                        GrADS tokens are also resolvable (see 
!                                        optional "grads" argument).
!
!   idate              integer       IN  date as ccyymmdd.
!   itime              integer       IN  time as hhmmss.
!   s1                  string       IN  character string containing time
!                                        tokens.
!   s2                  string      OUT  character string with time tokens
!                                        resolved.
!   grads     optional,logical       IN  GrADS flag: when set to true, GrADS
!                                        tokenization rules are used to resolve
!                                        the tokens.
!
! tm_inctime()      subroutine      PUB  increments/decrements the date/time.
!
!   idate              integer    INOUT  on input, current date as ccyymmdd.
!                                        on output, incremented date.
!   itime              integer    INOUT  on input, current time as hhmmss.
!                                        on output, incremented time.
!   inchrs             integer       IN  hours to offset date/time.
!   incmin             integer       IN  minutes to offset date/time.
!   incsec             integer       IN  seconds to offset date/time.
!
! tm_incr()         subroutine      PUB  increments/decrements the date/time.
!
!   idate              integer    INOUT  on input, current date as ccyymmdd.
!                                        on output, incremented date.
!   itime              integer    INOUT  on input, current time as hhmmss.
!                                        on output, incremented time.
!   years              integer   OPT,IN  number of years to incr/decr time.
!   months             integer   OPT,IN  number of months to incr/decr time.
!   days               integer   OPT,IN  number of days  to incr/decr time.
!   hours              integer   OPT,IN  number of hours to incr/decr time.
!   minutes            integer   OPT,IN  number of minutes to incr/decr time.
!   seconds            integer   OPT,IN  number of seconds to incr/decr time.
!
! tm_datetime()     subroutine      PUB  calculates the date/time given seconds
!                                        from some reference year.
!
!   refyear            integer       IN  reference year as ccyy.
!   seconds            integer       IN  number of seconds measured from
!                                        the reference year.
!   idate              integer      OUT  date as ccyymmdd.
!   itime              integer      OUT  time as hhmmss.
!
! tm_leapyear()       function      PUB  determines if the specified year
!                                        is a leap year.
!
!   year               integer       IN  year as ccyy.
!
!   tm_leapyear        logical      OUT  function return value:
!
!                                        tm_leapyear = .true. (year is leapyear)
!                                                    = .false. (not a leapyear)
!
! tm_synoptic()       function      PUB  calculates the synoptic time by 
!                                        rounding the date/time to the nearest
!                                        synoptic hour that is a multiple of 
!                                        the specified hourly increment.
!
!   inchrs             integer       IN  hourly increment of desired synoptic
!                                        bin (hh).
!   idate              integer    INOUT  on input, actual date as ccyymmdd.
!                                        on output, adjusted date corresponding
!                                        to the synoptic bin having vertices at
!                                        "inchrs" intervals starting at 00z.
!   itime              integer    INOUT  on input, actual time as hhmmss.
!                                        on output, adjusted time corresponding
!                                        to the synoptic bin having vertices at
!                                        "inchrs" intervals starting at 00z.
!
!   tm_synoptic           real      OUT  function return value:
!
!                                        tm_synoptic >= 0 (departure from 
!                                           synoptic time in fractional hours)
!
! tm_seconds()        function      PUB  calculates the date/time in seconds 
!                                        measured from the beginning of the 
!                                        specified reference year.
!
!   refyear            integer       IN  reference year as ccyy.
!   idate              integer       IN  date as ccyymmdd.
!   itime              integer       IN  time as hhmmss.
!
!   tm_seconds         integer      OUT  function return value:
!                                    
!                                        tm_seconds >= 0 (sec from refyear)
!                                                    < 0 (sec before refyear)
!
! tm_compare()        function      PUB  compares the date/time to some
!                                        reference date/time.
!
!   idate              integer       IN  date as ccyymmdd.
!   itime              integer       IN  time as hhmmss.
!   rdate              integer       IN  reference date as ccyymmdd.
!   rtime              integer       IN  reference time as hhmmss.
!
!   tm_compare         integer      OUT  function return value:
!
!                                        -1 (date/time is before the reference
!                                            date/time)
!                                         0 (date/time is the same as reference
!                                            date/time)
!                                         1 (date/time is after the reference
!                                            date/time)
!
! tm_y2k()            function      PUB  converts date with 2-digit year to 
!                                        include 4-digit year using the
!                                        following rule:
!
!                                        year <= 50 : year = year + 2000
!                                        year  > 50 : year = year + 1900
!                                        
!   idate              integer       IN  date as yymmdd.
!
!   tm_y2k             integer      OUT  function return value: date as
!                                        ccyymmdd.
!
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      integer, parameter :: SEC_IN_MIN = 60
      integer, parameter :: SEC_IN_HOUR = SEC_IN_MIN * 60
      integer, parameter :: SEC_IN_DAY = SEC_IN_HOUR * 24
      integer, parameter :: SEC_IN_YEAR = SEC_IN_DAY * 365

      contains
!******************************************************************************
      subroutine tm_string(idate,itime,s1,s2,grads)
!******************************************************************************
! English Name: Time String
! -------------
!
! Purpose: Resolves time tokens in an input character string according to
! -------- the specified date/time. See the "date" command for a list of
!          valid tokens. Also, see note-1 in the prolog section of this file
!          for a description of additional time tokens for 
!          incrementing/decrementing the date/time.
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Output string dimension must be large enough to hold expanded
! ------    time tokens.
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer       IN  date as ccyymmdd.
! itime                integer       IN  time as hhmmss.
! s1                    string       IN  character string containing time
!                                        tokens.
! s2                    string      OUT  character string with time tokens
!                                        resolved.
! grads       optional,logical       IN  GrADS flag: when set to true, GrADS
!                                        tokenization rules are used to resolve
!                                        the tokens.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           06/07/2007      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      character*(*), intent(in) :: s1
      character*(*), intent(out) :: s2
      logical, intent(in),optional :: grads

!     Local Variables
!     ---------------

      integer :: one = 1
      integer :: zero = 0
      integer :: ilen,iret
      integer :: timetag

      s2 = ' '
      ilen = len_trim(s1);

      if ( present(grads) .and. grads) then 
        iret = timetag(idate,itime,s2,s1,ilen,one);
      else
        iret = timetag(idate,itime,s2,s1,ilen,zero);
      endif

      end subroutine tm_string

!******************************************************************************
      subroutine tm_date(idate,itime,operator,operand)
!******************************************************************************
! English Name: Date
! -------------
!
! Purpose: Updates the date/time using the operator and operand to increment
! -------- or decrement the time by the specified number of units.
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer    INOUT  on input, date expressed as ccyymmdd.
!                                        on output, date after performing the
!                                        operation to incr or decr the time.
!
! itime                integer    INOUT  on input, time expressed as hhmmdd.  
!                                        on output, time after performing the
!                                        operation to incr or decr the time.
!
! operator              string       IN  token operator string in the form '%sT'
!                                        where:
!                                        
!                                        s - "+" or "-" for addition of 
!                                            subtraction
!
!                                        T - time token offset unit. This can 
!                                            be y,m,d,H,M or S for year, month,
!                                            day, hour, minute, second. No 
!                                            other tokens are recognized for 
!                                            offsets.
!
! operand              integer       IN  time increment. Please note that the
!                                        operator specifies the sign of the 
!                                        increment.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Synoptic Evaluation Group)
!
! Modified:       Date           Author  Description
! ---------   
!           06/08/2007      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(inout) :: idate
      integer, intent(inout) :: itime
      character(len=3), intent(in) :: operator
      integer, intent(in) :: operand

!     Local Variables
!     ---------------

      integer :: inc
      character(len=20) :: str
      character(len=5) :: operation
      character(len=20) :: tokens = '{%Y%m%d %H%M%S}     '

      inc = abs(operand)! To avoid infinite loop for negative value.
                        ! The sign is part of the operator: (eg %+m ).

      operation = operator // '99' ! 99 is the maximum increment for a 
                                   ! single operation.

      do while (inc .gt. 0)

        if (inc .lt. 99) write(unit=operation(4:5),fmt='(i2.2)') inc
        inc = inc - 99

        tokens(16:20) = operation
        call tm_string(idate,itime,tokens,str)
        read(unit=str(1:8),fmt='(i8)') idate
        read(unit=str(10:15),fmt='(i8)') itime

      end do

      return
      end subroutine tm_date

!******************************************************************************
      subroutine tm_inctime(idate,itime,inchrs,incmin,incsec)
!******************************************************************************
! English Name: Increment Time
! -------------
!
! Purpose: Increments/decrements the date/time.
! --------
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer    INOUT  on input, current date as ccyymmdd.
!                                        on output, incremented date.
! itime                integer    INOUT  on input, current time as hhmmss.
!                                        on output, incremented time.
! inchrs               integer       IN  hours to offset date/time.
! incmin               integer       IN  minutes to offset date/time.
! incsec               integer       IN  seconds to offset date/time.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(inout) :: idate
      integer, intent(inout) :: itime
      integer, intent(in)    :: inchrs
      integer, intent(in)    :: incmin
      integer, intent(in)    :: incsec

!     Local Variables
!     ---------------

      integer i
      integer nfeb,ndays(12)
      integer year,month,day,hour,min,sec

      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/
      equivalence (nfeb,ndays(2))

!     Separate Time Variables
!     =======================

      call tm_decode(idate,year,month,day)
      call tm_decode(itime,hour,min  ,sec)

!     Increment Time
!     ==============

      i   = sec + incsec
      sec = mod(i,60)
      min = min + (i / 60)
      if (sec .lt. 0) then
         sec = sec + 60
         min = min - 1
      endif

      i = min + incmin
      min = mod(i,60)
      hour = hour + (i / 60)
      if (min .lt. 0) then 
         min = min + 60
         hour = hour - 1
      endif

      i = hour + inchrs
      hour = mod(i,24)
      day  = day  + (i / 24)
      if (hour .lt. 0) then 
         hour = hour + 24
         day = day - 1
      endif

      do while (day .le. 0)
         month = month - 1
         if (month .eq. 0) then 
            month = 12
            year = year - 1
            nfeb = 28
            if (tm_leapyear(year)) nfeb = 29
         endif

         day = day + ndays(month)

      end do

      nfeb = 28
      if (tm_leapyear(year)) nfeb = 29
      do while (day .gt. ndays(month))
         day = day - ndays(month)
         month = month + 1
         if (month .gt. 12) then
            month = 1
            year = year + 1
            nfeb = 28
            if (tm_leapyear(year)) nfeb = 29
         endif
      end do

!     Pack the new date and time.
!     ===========================

      idate = year * 10000 + month * 100 + day
      itime = hour * 10000 + min   * 100 + sec

      return
      end subroutine tm_inctime

!******************************************************************************
      subroutine tm_incr(idate,itime,years,months,days, \
                                         hours,minutes,seconds)
!******************************************************************************
! English Name: Increment Time
! -------------
!
! Purpose: Increments/decrements the date/time.
! --------
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer    INOUT  on input, current date as ccyymmdd.
!                                        on output, incremented date.
! itime                integer    INOUT  on input, current time as hhmmss.
!                                        on output, incremented time.
! inchrs               integer       IN  hours to offset date/time.
! incmin               integer       IN  minutes to offset date/time.
! incsec               integer       IN  seconds to offset date/time.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(inout) :: idate
      integer, intent(inout) :: itime
      integer, intent(in), optional :: years
      integer, intent(in), optional :: months
      integer, intent(in), optional :: days
      integer, intent(in), optional :: hours
      integer, intent(in), optional :: minutes
      integer, intent(in), optional :: seconds

!     Local Variables
!     ---------------

      character(len=3) :: oper

      if (present(years)) then

        oper = '%+y'
        if (years .lt. 0) oper = '%-y'
        call tm_date(idate,itime,oper,years)

      endif

      if (present(months)) then

        oper = '%+m'
        if (months .lt. 0) oper = '%-m'
        call tm_date(idate,itime,oper,months)

      endif

      if (present(days)) then

        oper = '%+d'
        if (days .lt. 0) oper = '%-d'
        call tm_date(idate,itime,oper,days)

      endif

      if (present(hours)) then

        oper = '%+H'
        if (hours .lt. 0) oper = '%-H'
        call tm_date(idate,itime,oper,hours)

      endif

      if (present(minutes)) then

        oper = '%+M'
        if (minutes .lt. 0) oper = '%-M'
        call tm_date(idate,itime,oper,minutes)

      endif

      if (present(seconds)) then

        oper = '%+S'
        if (seconds .lt. 0) oper = '%-S'
        call tm_date(idate,itime,oper,seconds)

      endif

      return

      end subroutine tm_incr

!******************************************************************************
      subroutine tm_datetime(refyear,seconds,idate,itime)
!******************************************************************************
! English Name: Date/Time
! -------------
!
! Purpose: Calculates the date/time given seconds from some reference year.
! --------
!
! Language: Fortran 90
! ---------
!
! Notes: 1. Seconds may be less than zero.
! ------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! refyear              integer       IN  reference year as ccyy.
! seconds              integer       IN  number of seconds measured from
!                                        the reference year.
! idate                integer      OUT  date as ccyymmdd.
! itime                integer      OUT  time as hhmmss.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none
                                                                                
!     Argument List
!     -------------

      integer, intent(in)  :: refyear
      integer, intent(in)  :: seconds
      integer, intent(out) :: idate
      integer, intent(out) :: itime

!     Local Variables
!     ---------------

      integer ihour,imin,isec
      integer iyear,imon,iday
      integer oneyear,days(12)

      data days/31,28,31,30,31,30,31,31,30,31,30,31/

!     Determine the year.
!     ===================

      isec    = seconds
      iyear   = refyear
      oneyear = SEC_IN_YEAR
      if (tm_leapyear(iyear)) oneyear = oneyear + SEC_IN_DAY

      do while (isec .ge. oneyear)
         isec = isec - oneyear
         iyear = iyear + 1
         oneyear = SEC_IN_YEAR
         if (tm_leapyear(iyear)) oneyear = oneyear + SEC_IN_DAY
      end do

!     Determine the month.
!     ====================

      days(2) = 28
      if (tm_leapyear(iyear)) days(2) = 29

      imon = 1
      do while (isec .ge. days(imon)*SEC_IN_DAY)
        isec = isec - days(imon)*SEC_IN_DAY
        imon = imon + 1
      end do

!     Determine the day.
!     ==================

      iday = 1
      do while (isec .ge. SEC_IN_DAY)
        isec = isec - SEC_IN_DAY
        iday = iday + 1
      end do

!     Determine the hour.
!     ===================

      ihour = 0
      do while (isec .ge. SEC_IN_HOUR)
        isec = isec - SEC_IN_HOUR
        ihour = ihour + 1
      end do

!     Determine the minutes
!     =====================

      imin = 0
      do while (isec .ge. SEC_IN_MIN)
        isec = isec - SEC_IN_MIN
        imin = imin + 1
      end do

!     Pack the date/time.
!     ===================

      idate = iyear*10000 + imon*100 + iday
      itime = ihour*10000 + imin*100 + isec

      return
      end subroutine tm_datetime

!******************************************************************************
      logical function tm_leapyear(year)
!******************************************************************************
! English Name: Leap Year
! -------------
!
! Purpose: Determines if the specified year is a leap year.
! --------
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! year                 integer       IN  year as ccyy.
!
! tm_leapyear          logical      OUT  function return value:
!
!                                        tm_leapyear = .true. (year is leapyear)!                                                    = .false. (not a leapyear)
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: year

      if ( mod(year,4) .eq. 0 .and. mod(year,100) .ne. 0) then 
         tm_leapyear = .true.
      elseif (mod(year,400) .eq. 0) then
         tm_leapyear = .true.
      else
         tm_leapyear = .false.
      endif

      return
      end function tm_leapyear

!******************************************************************************
      real function tm_synoptic(inchrs,idate,itime)
!******************************************************************************
! English Name: Synoptic Time
! -------------
!
! Purpose: Calculates the synoptic time by rounding the date/time to
! -------- to nearest synoptic hour that is a multiple of the specified
!          hourly increment.
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! inchrs               integer       IN  hourly increment of desired synoptic
!                                        bin (hh).
! idate                integer    INOUT  on input, actual date as ccyymmdd.
!                                        on output, adjusted date corresponding
!                                        to the synoptic bin having vertices at
!                                        "inchrs" intervals starting at 00z.
! itime                integer    INOUT  on input, actual time as hhmmss.
!                                        on output, adjusted time corresponding
!                                        to the synoptic bin having vertices at
!                                        "inchrs" intervals starting at 00z.
!
! tm_synoptic             real      OUT  function return value:
!
!                                        tm_synoptic >= 0 (departure from
!                                           synoptic time in fractional hours)
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: inchrs
      integer, intent(inout) :: idate
      integer, intent(inout) :: itime

!     Local Variables
!     ---------------

      integer iyear,imon,iday
      integer ihour,imin,isec
      real midtime,hour,hinc,xrem

!     Extract temporal components.
!     ============================

      call tm_decode(idate,iyear,imon,iday)
      call tm_decode(itime,ihour,imin,isec)
      hour     = float(ihour) + float(imin)/60.0 + float(isec)/3600.0
      midtime  = float(inchrs) / 2.0
      hinc     = float(inchrs)

!     Determine the closest hourly bin.
!     =================================

      xrem = amod(hour,hinc)
      if (xrem .ge. midtime) then 
         hour = hour + (hinc - xrem)
         tm_synoptic = -(hinc - xrem)
      else
         hour = hour - xrem
         tm_synoptic = xrem
      endif

!     Set the new date/time.
!     ======================

      idate = iyear*10000 + imon*100 + iday
      itime = nint(hour)*10000
      call tm_inctime(idate,itime,0,0,0)

      return
      end function tm_synoptic

!******************************************************************************
      integer function tm_seconds(refyear,idate,itime)
!******************************************************************************
! English Name: Seconds
! -------------
!
! Purpose: Calculates the date/time in seconds measured from the
! -------- beginning of the specified reference year.
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! refyear              integer       IN  reference year as ccyy.
! idate                integer       IN  date as ccyymmdd.
! itime                integer       IN  time as hhmmss.
!
! tm_seconds           integer      OUT  function return value:
!
!                                        tm_seconds >= 0 (sec from refyear)
!                                                    < 0 (sec before refyear)
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: refyear
      integer, intent(in) :: idate
      integer, intent(in) :: itime

!     Local Variables
!     ---------------

      integer days(12)
      integer year,month,day
      integer hour,minute,second
      integer date,time,imonth,iyear,totsec

      data days/31,28,31,30,31,30,31,31,30,31,30,31/

      totsec = 0
      iyear  = refyear
      date   = idate
      time   = itime
      call tm_inctime(date,time,0,0,0)
      call tm_decode(date,year,month,day)
      call tm_decode(time,hour,minute,second)

!     Find seconds from reference year to beginning of current year.
!     ==============================================================

      if (iyear .le. year) then 
         do while (iyear .ne. year)
            totsec = totsec + SEC_IN_YEAR
            if (tm_leapyear(iyear)) totsec = totsec + SEC_IN_DAY
            iyear = iyear + 1
        end do
      else
         do while (iyear .ne. year)
            totsec = totsec - SEC_IN_YEAR
            if (tm_leapyear(iyear-1)) totsec = totsec - SEC_IN_DAY
            iyear = iyear - 1
        end do
      endif

!     Find seconds from reference year to current date/time.
!     ======================================================

      days(2) = 28
      if (tm_leapyear(iyear)) days(2) = 29

      imonth = 1
      do while (imonth .ne. month .and. imonth .le. 12)
         totsec = totsec + days(imonth) * SEC_IN_DAY
         imonth = imonth + 1
      end do

      totsec = totsec + (day - 1) * SEC_IN_DAY
      totsec = totsec + hour * SEC_IN_HOUR
      totsec = totsec + minute * SEC_IN_MIN
      totsec = totsec + second

      tm_seconds = totsec

      return
      end function tm_seconds

!******************************************************************************
      integer function tm_compare(idate,itime,rdate,rtime)
!******************************************************************************
! English Name: Compare date/time 
! -------------
!
! Purpose: Compares the date/time with some reference date/time.
! --------
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer       IN  date as ccyymmdd.
! itime                integer       IN  time as hhmmss.
! rdate                integer       IN  reference date as ccyymmdd.
! rtime                integer       IN  reference time as hhmmss.
!
! tm_compare           integer      OUT  function return value:
!
!                                        -1 (date/time is before the reference
!                                            date/time)
!                                         0 (date/time is the same as reference
!                                            date/time)
!                                         1 (date/time is after the reference
!                                            date/time)
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate
      integer, intent(in) :: itime
      integer, intent(in) :: rdate
      integer, intent(in) :: rtime

      tm_compare = 0
      if (idate .eq. rdate .and. itime .eq. rtime) return

      tm_compare = -1
      if (idate .lt. rdate) return
      if (idate .eq. rdate .and. itime .lt. rtime) return

      tm_compare = 1
      return
      end function tm_compare
!******************************************************************************
      integer function tm_y2k(idate)
!******************************************************************************
! English Name: Year 2000
! -------------
!
! Purpose: Converts date with 2-digit year to 4-digit year using the
! -------- following rule:
!
!             year <= 50 : year = year + 2000
!             year  > 50 : year = year + 1900
!
!          4-digit years are unchanged by this routine.
!
! Language: Fortran 90
! ---------
!
! Interface:              Type   Access  Description
! ----------                     Intent
!
! idate                integer       IN  date as yymmdd.
!
! tm_y2k               integer      OUT  function return value: date as
!                                        ccyymmdd.
!    
! Programmer: Joseph V. Ardizzone
! ----------- (NASA Goddard Space Flight Center)
!             (Sounder Research Team)
!
! Modified:       Date           Author  Description
! ---------   
!           12/27/2004      J.Ardizzone  created.
!******************************************************************************

      implicit none

!     Argument List
!     -------------

      integer, intent(in) :: idate

!     Local Variables
!     ---------------

      integer year

!     Make sure date has 4-digit year.
!     ================================

      year = idate / 10000

      if (idate .lt. 1000000 .and. year .gt. 50) then
         tm_y2k = idate + 19000000
      elseif (idate .lt. 1000000) then
         tm_y2k = idate + 20000000
      else
         tm_y2k = idate
      endif

      return
      end function tm_y2k

!**********************************************************************
      subroutine tm_decode(icode,item1,item2,item3)
!**********************************************************************
      item1 = icode / 10000
      item2 = (icode - (item1*10000)) / 100
      item3 = icode - (item1*10000 + item2*100)
      return
      end subroutine tm_decode

      end module time
