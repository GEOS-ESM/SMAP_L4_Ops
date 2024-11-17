#!/usr/bin/env python

'''
SMAP L4_C augmentation script

This script performs a set of augmentations to a SMAP L4_C granule designed
to improve usability by:

1. Improvements to adherence of CF 1.7 conventions
2. Addition of "dimension scales" and native coordinate variables

Author: Bruce Wallin
Email: bruce.wallin@nsidc.org
Modified: NTSG Tobias Kundig
revision: 2018-08-03T0738, Modified to work with NTSG systems and differences between L4_SM and L4_C
   	  hdf group/attribute structue
'''
import shutil
from path import Path

import sys
import h5py
import numpy as np
from toolz import first
from cartopy.crs import Projection, sgeom

if len(sys.argv[1:]) != 2:
    print 'Usage: ' + sys.argv[0] + ' [input fname] [output fname]'
    sys.exit(1)

GRANULE_FILEPATH = sys.argv[1]
AUGMENTED_FILEPATH = sys.argv[2]

ROOT_PATH = Path('/')

def flatten_array(arr, axis, flag_value=-9999):
    '''
    Given an array that is redundant along 'axis', flatten to remove that axis
    ignoring 'flag_value' values.
    '''
    def flatten_row(row):
        values = set(row)
        values.discard(flag_value)
        assert len(values) == 1
        return values.pop()

    flattened = np.apply_along_axis(flatten_row, axis, arr)

    return flattened


class EASE2_global(Projection):
    def __init__(self):
        proj4_params = [
            ('proj', 'cea'),
            ('lat_ts', 30),
            ('lon_0', 0),
            ('units', 'm'),
            ('x_0', 0),
            ('y_0', 0)
        ]
        super(EASE2_global, self).__init__(proj4_params=proj4_params, globe=None)

    @property
    def boundary(self):
        coords = ((self.x_limits[0], self.y_limits[0]),
                  (self.x_limits[1], self.y_limits[0]),
                  (self.x_limits[1], self.y_limits[1]),
                  (self.x_limits[0], self.y_limits[1]),
                  (self.x_limits[0], self.y_limits[0]))

        return sgeom.Polygon(coords).exterior

    @property
    def threshold(self):
        return 1e5

    @property
    def x_limits(self):
        return (-17367530.445161376, 17367530.445161376)

    @property
    def y_limits(self):
        return (-7314540.08900637, 7314540.08900637)


def associate_dim_scale(f, var, dim_path, dim_idx):
    '''
    Create and associate dimension scale to 'dim_path' on the 'dim_idx' indexed
    dimension of 'var' in dataset 'f'.
    '''
    if (('CLASS' in var.attrs and var.attrs['CLASS'] == b'DIMENSION_SCALE')
            or (str(dim_path) == var.name)):
        return

    var.dims.create_scale(f[str(dim_path)], dim_path.name)
    var.dims[dim_idx].label = dim_path.name
    var.dims[dim_idx].attach_scale(f[str(dim_path)])


def add_native_coordinate_variables(f):
    '''
    Add native coordinate variables to dataset 'f'
    '''
    ease = EASE2_global()
    wgs84 = ease.as_geodetic()

    # NTSG specific long lat location /GEO not /
    lons = flatten_array(f[str('/GEO')]['longitude'], axis=0)
    lats = flatten_array(f[str('/GEO')]['latitude'], axis=1)

    x = ease.transform_points(src_crs=wgs84, x=lons, y=np.full(lons.shape, lats[0]))[:, 0]
    y = ease.transform_points(src_crs=wgs84, x=np.full(lats.shape, lons[0]), y=lats)[:, 1]

    f[str(ROOT_PATH)]['x'] = x
    f[str(ROOT_PATH)]['y'] = y

    f[str(ROOT_PATH)]['x'].attrs['standard_name'] = 'projection_x_coordinate'
    f[str(ROOT_PATH)]['y'].attrs['standard_name'] = 'projection_y_coordinate'

    f[str(ROOT_PATH)]['x'].attrs['axis'] = 'X'
    f[str(ROOT_PATH)]['y'].attrs['axis'] = 'Y'

    f[str(ROOT_PATH
)]['x'].attrs['units'] = 'm'
    f[str(ROOT_PATH)]['y'].attrs['units'] = 'm'

    f[str(ROOT_PATH)]['x'].attrs['long_name'] = 'X coordinate of cell center in EASE 2.0 global projection'
    f[str(ROOT_PATH)]['y'].attrs['long_name'] = 'Y coordinate of cell center in EASE 2.0 global projection'


def add_grid_mapping_variable(f):
    '''
    Add CF grid mapping variable for EASE 2.0 global projection.
    '''
    projection_var = 'EASE2_global_projection'
    f[str(ROOT_PATH)][projection_var] = ''
    f[str(ROOT_PATH)][projection_var].attrs['grid_mapping_name'] = 'lambert_cylindrical_equal_area'
    f[str(ROOT_PATH)][projection_var].attrs['standard_parallel'] = 30.0
    f[str(ROOT_PATH)][projection_var].attrs['false_easting'] = 0.0
    f[str(ROOT_PATH)][projection_var].attrs['false_northing'] = 0.0
    f[str(ROOT_PATH)][projection_var].attrs['false_northing'] = 0.0
    f[str(ROOT_PATH)][projection_var].attrs['longitude_of_central_meridian'] = 0


def augment():
    '''
    Perform augmentation on sample file.
    '''
    shutil.copyfile(GRANULE_FILEPATH, AUGMENTED_FILEPATH)
    with h5py.File(AUGMENTED_FILEPATH, 'r+') as f:
        expected_shapes = {
            ('y',): (1624,),
            ('x',): (3856,),
            ('y', 'x'): (1624, 3856),
        }

        # Add ease grid coordinates
        add_native_coordinate_variables(f)
        add_grid_mapping_variable(f)

        # Attach ease grid coordinates as dimension scales to other variables
        def try_augment_variable(name, el):
            if not isinstance(el, h5py.Dataset):
                return
            # Check if shape of variable matches expected new dimensions
            try:
                dims_match = first(dims
                                   for dims, shape in expected_shapes.items()
                                   if el.shape == shape)
            except StopIteration:
                return

            el.attrs['grid_mapping'] = 'EASE2_global_projection'
            for dim_idx, dim_name in enumerate(dims_match):
                dim_path = ROOT_PATH / dim_name
                associate_dim_scale(f, el, dim_path, dim_idx)

        f.visititems(try_augment_variable)


augment()
