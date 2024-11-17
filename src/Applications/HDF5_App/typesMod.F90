      module typesMod

      use hdf5
      use h5fortran_types

      public

      integer, parameter :: MAX_DEPTH = 100
      integer, parameter :: MAX_NUM_ATTR = 2000
      integer, parameter :: MAX_STR_LEN = 1024

      type FileInfoType

        integer :: date
        integer :: time
        integer :: tstep

        character (len=MAX_STR_LEN) :: title
        character (len=MAX_STR_LEN) :: history
        character (len=MAX_STR_LEN) :: conventions
        character (len=MAX_STR_LEN) :: institution
        character (len=MAX_STR_LEN) :: contact
        character (len=MAX_STR_LEN) :: references
        character (len=MAX_STR_LEN) :: source
        character (len=MAX_STR_LEN) :: comment
        character (len=MAX_STR_LEN) :: filename

      end type FileInfoType

      type GroupInfoType

        character (len=MAX_STR_LEN) :: name
        character (len=MAX_STR_LEN) :: coordinates
        integer :: num_elements
        integer :: shave_bits

      end type GroupInfoType

      type DataInfoType

        integer :: date
        integer :: time
        integer :: tstep

        character (len=MAX_STR_LEN)  :: name
        character (len=MAX_STR_LEN)  :: long_name
        character (len=MAX_STR_LEN)  :: standard_name
        character (len=MAX_STR_LEN)  :: units 
        character (len=MAX_STR_LEN)  :: axis
        character (len=MAX_STR_LEN)  :: coordinates

        character (len=MAX_STR_LEN)  :: fill_value
        character (len=MAX_STR_LEN)  :: valid_max
        character (len=MAX_STR_LEN)  :: valid_min

        integer :: rank
        integer :: shave_bits
        integer(HSIZE_T), dimension(3) :: dims
        integer(HSIZE_T), dimension(3) :: chunks
        integer, dimension(4) :: bit_size

      end type DataInfoType

      type ElementDataType

        type (DataInfoType) :: info
        
        real*4,    dimension(:), pointer :: f32
        real*8,    dimension(:), pointer :: f64
        integer*1, dimension(:), pointer :: i8
        integer*2, dimension(:), pointer :: i16
        integer*4, dimension(:), pointer :: i32
        integer*1, dimension(:), pointer :: u8
        integer*2, dimension(:), pointer :: u16
        integer*4, dimension(:), pointer :: u32
        integer,   dimension(:), pointer :: int
        character, dimension(:), pointer :: buf

      end type ElementDataType

      type AttributeDataType

        character (len=MAX_STR_LEN) :: name
        character (len=MAX_STR_LEN) :: long_name
        character (len=MAX_STR_LEN), dimension(MAX_NUM_ATTR) :: value

        real*4,    dimension(:), pointer :: f32 => null()
        real*8,    dimension(:), pointer :: f64 => null()
        integer*1, dimension(:), pointer :: i8  => null()
        integer*2, dimension(:), pointer :: i16 => null()
        integer*4, dimension(:), pointer :: i32 => null()
        integer*1, dimension(:), pointer :: u8  => null()
        integer*2, dimension(:), pointer :: u16 => null()
        integer*4, dimension(:), pointer :: u32 => null()
        integer  , dimension(:), pointer :: int => null()

        character (len=MAX_STR_LEN), dimension(:), pointer :: str => null()

        integer, dimension(3) :: bit_size

      end type AttributeDataType

      end module typesMod
