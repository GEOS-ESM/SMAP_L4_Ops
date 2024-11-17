      module TypesMod

      public

      type GridInfoType

        real :: undef
        real :: dx,dy
        real :: xStart,yStart

        logical :: scaled

        integer :: id
        integer :: heap
        integer :: nx,ny
        integer :: i1,i2,j1,j2
        integer :: idate,itime

      end type GridInfoType

      type GenericDataType

        type (GridInfoType) :: grid
        real, pointer :: data(:,:) => null()

      end type GenericDataType

      type LFODataType

        type (GridInfoType) :: grid
        real, pointer :: prectot(:,:) => null()
        real, pointer :: precls(:,:) => null()
        real, pointer :: precsno(:,:) => null()
        real, pointer :: preccon(:,:) => null()
        real, pointer :: airtemp(:,:) => null()

      end type LFODataType

      type CPCUDataType

        type (GridInfoType) :: grid
        real, pointer :: data(:,:) => null()

      end type CPCUDataType

      type CMAPDataType

        type (GridInfoType) :: grid
        real, pointer :: data(:,:) => null()

      end type CMAPDataType

      type PrecipDataType

        type (GridInfoType) :: grid
        real, pointer :: data(:,:) => null()

      end type PrecipDataType

      type CPCUCorrDataType

        type (GridInfoType) :: grid
        real, pointer :: factor(:,:) => null()
        real, pointer :: residual(:,:) => null()
        real, pointer :: eod(:,:) => null()

      end type CPCUCorrDataType

      type TimeZoneDataType

        type (GridInfoType) :: grid
        integer :: localHour,utcTime
        real, dimension(:,:), pointer :: eod => null()

      end type TimeZoneDataType

      end module TypesMod
