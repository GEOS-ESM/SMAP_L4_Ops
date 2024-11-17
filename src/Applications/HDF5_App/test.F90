      implicit none

      integer :: i,rc

      character (len=1024) :: name
      character (len=50000) :: long_value
      integer :: real_size, int_size, uint_size
      character (len=1024), dimension(2000) :: value

      namelist /meta/ name, value, real_size, int_size, uint_size, long_value

      open (unit=5,file='test.namelist',status='old',iostat=rc)

      do while (.true.) 

        value = ' '
        read(5,nml=meta,iostat=rc)
        if (rc .ne. 0) exit

        do i = 1,2000

          if (len_trim(value(i)) .gt. 0) then
            print *, i,':',value(i)(1:len_trim(value(i)))
          else
            exit
          endif

        end do

      end do

      close (unit=5)

      stop
      end


      
