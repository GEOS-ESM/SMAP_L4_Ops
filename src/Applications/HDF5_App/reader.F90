      use hdf5
      use h5fortran_types

      implicit none

      integer, parameter :: cols = 3856
      integer, parameter :: rows = 1624
      integer, parameter :: max_objs = 500
      integer, parameter :: maxdim = cols * rows

!     Local Variables
!     ---------------

      integer(HID_T) :: fid
      integer(HID_T) :: prp_id
      integer(HID_T) :: grp_id
      integer(HID_T) :: dset_id
      integer(HID_T) :: space_id
      integer(HID_T) :: attr_id
      integer(HID_T) :: str_id
      integer(HID_T) :: type_id
      integer(HID_T) :: time_id
      integer(HID_T), dimension(max_objs) :: obj_ids

      integer(SIZE_T) :: len
      integer(SIZE_T) :: obj_count

      integer(HSIZE_T), dimension(2) :: dims = (/cols,rows/)

      character (len=1024) :: name
      character (len=1024), dimension(45) :: names
      character (len=1024) :: filename
      character, dimension(maxdim*8) :: buf

      integer :: n
      integer :: rc

!     Retrieve the filename argument from the
!     command-line.
!     =======================================

      if (iargc() .ne. 1) stop 1
      call getarg(1,filename)
      
!     Initialize HDF5
!     ===============

      call h5open_f(rc)
      if (rc .ne. 0) stop 2

!     Open HDF5 file
!     ==============

      call h5pcreate_f(H5P_FILE_ACCESS_F, prp_id, rc)
      if (rc .ne. 0) stop 2
      call h5pset_fapl_stdio_f(prp_id, rc)
      if (rc .ne. 0) stop 2

      call h5fopen_f(filename,H5F_ACC_RDONLY_F,fid,rc,prp_id)
      if (rc .ne. 0) stop 2

!     Get a list of dataset elements
!     on the file
!     ==============================

      call h5Gopen_f(fid, 'Geophysical Data Group', grp_id, rc)
      if (rc .ne. 0) stop 4

      call h5fget_obj_count_f(fid, H5F_OBJ_ALL_F, obj_count, rc)
      if (rc .ne. 0) stop 3
      call h5fget_obj_ids_f(fid, H5F_OBJ_ALL_F, max_objs, obj_ids, rc)
      if (rc .ne. 0) stop 3

      print *, obj_count, obj_ids(1)

      obj_count = 45
      names( 1) = 'baseflow_flux'
      names( 2) = 'cell_col'
      names( 3) = 'cell_row'
      names( 4) = 'heat_flux_ground'
      names( 5) = 'heat_flux_latent'
      names( 6) = 'heat_flux_sensible'
      names( 7) = 'height_lowatmmodlay'
      names( 8) = 'land_evapotranspiration_flux'
      names( 9) = 'land_fraction_saturated'
      names(10) = 'land_fraction_snow_covered'
      names(11) = 'land_fraction_unsaturated'
      names(12) = 'land_fraction_wilting'
      names(13) = 'latitude'
      names(14) = 'leaf_area_index'
      names(15) = 'longitude'
      names(16) = 'net_downward_longwave_flux'
      names(17) = 'net_downward_shortwave_flux'
      names(18) = 'overland_runoff_flux'
      names(19) = 'precipitation_total_surface_flux'
      names(20) = 'radiation_longwave_absorbed_flux'
      names(21) = 'radiation_shortwave_downward_flux'
      names(22) = 'sm_profile_pctl'
      names(23) = 'sm_profile_wetness'
      names(24) = 'sm_rootzone_pctl'
      names(25) = 'sm_rootzone_wetness'
      names(26) = 'sm_surface_pctl'
      names(27) = 'sm_surface_wetness'
      names(28) = 'snow_depth'
      names(29) = 'snow_mass'
      names(30) = 'snow_melt_flux'
      names(31) = 'snowfall_surface_flux'
      names(32) = 'soil_temp_layer1'
      names(33) = 'soil_temp_layer2'
      names(34) = 'soil_temp_layer3'
      names(35) = 'soil_temp_layer4'
      names(36) = 'soil_temp_layer5'
      names(37) = 'soil_temp_layer6'
      names(38) = 'soil_water_infiltration_flux'
      names(39) = 'specific_humidity_lowatmmodlay'
      names(40) = 'surface_pressure'
      names(41) = 'surface_temp'
      names(42) = 'temp_lowatmmodlay'
      names(43) = 'time'
      names(44) = 'vegetation_greenness_fraction'
      names(45) = 'windspeed_lowatmmodlay'

      do n = 1,obj_count

!       call h5fget_name_f(obj_ids(n), name, len, rc)
!       if (rc .ne. 0) stop 5

        name = names(n)

        call h5dopen_f(grp_id, name, dset_id, rc)
        if (rc .ne. 0) stop 5
        call h5dget_type_f(dset_id, type_id, rc)
        if (rc .ne. 0) stop 5
        call h5dget_space_f(dset_id, space_id, rc)
        if (rc .ne. 0) stop 5
        call h5dread_f(dset_id, type_id, buf, dims, rc)
        if (rc .ne. 0) stop 5

        call h5Tclose_f(type_id, rc)
        call h5Dclose_f(dset_id, rc)
        call h5Sclose_f(space_id, rc)

        print *, type_id,'"',trim(name),'"'

      end do

      stop
      end
