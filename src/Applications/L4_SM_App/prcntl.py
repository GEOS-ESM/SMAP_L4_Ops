#!/usr/bin/env python

import netCDF4 as nc4
import datetime as dt
import numpy as np
import argparse
import calendar
import struct
import time
import glob
import sys
import os

# global constants
gph_fields = ['sm_rootzone_wetness', 'sm_profile_wetness']
n_cdf = 99 # 1-99 percentile upper boundaries
std_no_perc = 0.05 # in wetness ; ~ 0.022 in vol/vol ~0.05*0.44;
nodata = -9999.0

def main():
    """
    # -------------------------------------------------------------------
    # def main()
    #    output: appends the percentile data to gph file
    #
    # Steps:
    # 1. Read tile-coord file for tile ids.
    # 2. Open cli file for reading, read tile ids and compare the order
    #    with that from tile-coord file.
    # 3. For each gph file:
    #    a. Read its time stamp (processing date) and 
    #       sm_rootzone/profile_wetness data
    #    b. Find pentads on either side of processing date and their
    #       contributing weights 
    #    c. For each variable (fields are sm_rootzone/profile_wetness)
    #         i. read corresponding cli data
    #        ii. compute percentile
    #       iii. append percentile data to the gph file
    # -------------------------------------------------------------------
    """
    t_total_start = time.time()

    # get command line args
    a = parse_args()
    tilecoord_file = a.tilefile
    gph_file_list = a.gphfile
    cli_file = a.clifile
    txt_out = 'out.prcntl'
    
    assert os.path.isfile(tilecoord_file)
    # check for 8 binary gph files
    if len(gph_file_list)!=8:
        print(('WARNING: [%d] gph bin files found '
              'instead of 8' % len(gph_file_list)))
    for gph_file in gph_file_list:
        assert os.path.isfile(gph_file)
        # input gph file should contain 40 records
        # i.e. it should have a size of
        # 40 * (1653157+2) * 4 = 269556320 B
        gph_file_size = os.stat(gph_file).st_size

     #  Removing the assert on file size since this makes the code less
     #  extensible. 
     #  assert gph_file_size==269556320, (
     #      'gph file [%s] size [%d] not consistent with '
     #      'expected size [269556320]' % (gph_file, gph_file_size))

    assert os.path.isfile(cli_file)

    # Step 1: read tilecoord bin file for tile ids
    tstart = time.time()
    gph_tile_id = get_tileid(tilecoord_file)
    tend = time.time()
    nTiles = len(gph_tile_id)
    print('nTiles:', nTiles)
    print('get_tileid: %.2f s' % (tend-tstart))
    sys.stdout.flush()

    # Step 2: open cli file for reading
    # tile_id which should match tile_id array from the tilecoord file
    tstart = time.time()
    nc4_fid = nc4.Dataset(cli_file, 'r')
    tend = time.time()
    print('nc4.Dataset(cli_file): %.2f s' % (tend-tstart))
    sys.stdout.flush()
    cli_tile_id = nc4_fid.variables['tile_id'][:]
    print(cli_tile_id)
    assert np.all(cli_tile_id==gph_tile_id), \
        'Order of tile_id in tilecoord file does ' \
        'NOT match that of tile_id in cli file'

    # cache cli data
    cache = dict()

    for gph_file in gph_file_list:
        print('\ngph file:', gph_file.split('/')[-1])
        # Step 3a: read gph file
        tstart = time.time()
        [prc_dt, gph_data] = get_gph(gph_file, nTiles)
        tend = time.time()
        print('get_gph: %.2f s' % (tend-tstart))
        print('date:', prc_dt.strftime('%Y/%m/%d, %H:%M'))
        sys.stdout.flush()

        # Step 3b: Find two pentads on either side of processing date
        # NOTE: pentads are 0-based
        # ## DEBUGGING: prc_dt leads to pentads 0 and 1
        # prc_dt = dt.datetime.strptime('20140104_0130z', '%Y%m%d_%H%Mz')
        # ## END DEBUGGING

        pt = tm_pentad(prc_dt)
        length = pt['length']

        [p1, p1c_dt]  = pentad_center(prc_dt - dt.timedelta(seconds=length/2.0))
        [p2, p2c_dt]  = pentad_center(prc_dt + dt.timedelta(seconds=length/2.0))

        delta_t = (p2c_dt - p1c_dt).total_seconds()

        print('pentads: %d, %d (0-based)' % (p1, p2))
        # weights
        w1 = (p2c_dt-prc_dt).total_seconds()/delta_t
        w2 = (prc_dt-p1c_dt).total_seconds()/delta_t
        assert abs(w1 - (1-w2))<1.0e-4, 'Weights add up to %f, not 1' % (w1+w2)
        print('weights: %f, %f' % (w1, w2))

        for field in gph_fields:
            print('field:', field)
            sys.stdout.flush()

            # Step 3ci: get weighted cli data for the current field
            nc4grp = field.replace('sm_','') + '_cli_stat'
            tstart = time.time()
            # NOTE: p1, p2 are 1-based whereas the arrays read
            # from the nc4 file are 0-based
            cli_data = get_weighted_cli(
                nc4_fid,
                nc4grp,
                [p1,p2],
                [w1,w2],
                cache
                )
            tend = time.time()
            print('  get_weighted_cli: %.2f s' % (tend-tstart))
            sys.stdout.flush()

            # Step 3cii: calculate percentile output
            tstart = time.time()
            prcntl = calc_prcntl(cli_data, gph_data[field])
            tend = time.time()
            print('  calc_prcntl: %.2f s' % (tend-tstart))
            sys.stdout.flush()

            # Step 3ciii: append percentile data to gph bin file
            tstart = time.time()
            append2gph(gph_file, prcntl)
            tend = time.time()
            print('  append2gph: %.2f s' % (tend-tstart))
            sys.stdout.flush()

    # close cli file
    nc4_fid.close()

    # total time
    print('-----\nTOTAL time taken:', time.time()-t_total_start)
        

def calc_prcntl(cli_data, gph_field_data):
    """
    # -------------------------------------------------------------------
    # This is the central part of the code that converts soil moisture
    # from wetness units to percentile units based on soil moisture
    # climatology inputs.
    # For each tile, we have the tile_data (value of the gph field for
    # the tile) and the corresponding cdf (an array of length 99,
    # weighted over two nearest pentads) and
    #    prcntl[tile] = where(cdf<tile_data)
    # Some special processing is needed for the case when the tile_data
    # is exactly equal to one (or more) cdf values.
    #
    # output: prcntl (numpy array containing prcntl data)
    # -------------------------------------------------------------------
    """
    nTiles = len(gph_field_data)
    prcntl = np.zeros(nTiles)
    for tile in range(nTiles):
        ## cli data for the current tile
        ## (cdf and its mean, stdv, min, max)
        cdf = cli_data['cdf'][tile,:] # array of length n_cdf
        cdf_min = cli_data['min'][tile]
        cdf_max = cli_data['max'][tile]
        cdf_stdv = cli_data['stdv'][tile]

        ## gph var data
        gph_tile_id = tile #tile_id_sndx[tile]
        tile_data = gph_field_data[gph_tile_id]

        ## some trivial cases
        if cdf_stdv<=std_no_perc:
            prcntl[gph_tile_id] = nodata
            continue
        elif tile_data<cdf_min:
            prcntl[gph_tile_id] = 0
            continue
        elif tile_data>cdf_max:
            prcntl[gph_tile_id] = 100
            continue
        else:
            pass

        eqndxs = np.where(cdf==tile_data)[0]
        if len(eqndxs)>0:
            ### multiple cdf values equal to tile_data
            neqs = len(eqndxs)
            if not isinstance(neqs, int):
                raise TypeError("the variable 'neqs' needs to be an integer")
            if neqs%2==1:
                #### neqs is odd, pick the middle one
                #### +1 shift since eqndxs is 0-based
                #### +1 shift to account for the missing 1st elt (0) in cdf
                prcntl[gph_tile_id] = eqndxs[neqs//2] + 2
            else:
                #### neqs is even
                #### +1 shift since eqndxs is 0-based
                #### +1 shift to account for the missing 1st elt (0) in cdf
                #### => +1+1-0.5 = 1.5
                prcntl[gph_tile_id] = eqndxs[neqs//2-1] + 1.5
        else:
            ### no cdf value equal to tile_data
            ndxs = np.where(cdf<tile_data)[0]
            if len(ndxs)>0:
                #### +1 shift since ndxs is 0-based
                #### +1 shift to account for the missing 1st elt (0) in cdf
                #### => +1+1-0.5 = 1.5
                prcntl[gph_tile_id] = ndxs[-1] + 1.5
            else:
                prcntl[gph_tile_id] = 0
                continue

    # return
    return prcntl

def tm_pentad(dattim):

  LEAP_DAY       = 60
  SECONDS_IN_DAY = 86400

  """
  Determine the pentad index (zero-based)
  ======================================= """

  offset = 0
  year   = dattim.year
  doy    = dattim.timetuple().tm_yday

  if calendar.isleap(year) and doy > LEAP_DAY:
    pentad = (doy - 2) / 5
    offset = (doy-2) % 5
  else:
    pentad = (doy - 1) / 5
    offset = (doy-1) % 5

  """
  Determine the start and end
  day for the pentad.
  =========================== """

  num_days = 5

  if calendar.isleap(year) and pentad == 11: num_days = 6
  if num_days == 6 and doy == LEAP_DAY+1: offset = 5

  start_dt = dattim - dt.timedelta(days=offset)
  start_dt = start_dt.replace(hour=0, minute=0, second=0, microsecond=0)
  end_dt   = start_dt + dt.timedelta(days=num_days,microseconds=-1)

  """
  Save information and return
  =========================== """

  info = {}

  info['index']    = pentad
  info['offset']   = (dattim - start_dt).total_seconds()
  info['datetime'] = dattim
  info['start']    = start_dt
  info['end']      = end_dt
  info['doy']      = doy
  info['length']   = num_days * SECONDS_IN_DAY

  return info

def pentad_center(DateTime):
    """
    # -------------------------------------------------------------------
    # def pentad_center(DateTime):
    #
    # input:  DateTime (a datetime.datetime object)
    # output: [pentad, center_dt], where
    #         index: index (0-based) of the pentad that DateTime
    #                belongs to
    #         center_dt: middle of the pentad (a datetime.datetime
    #                    object)
    # -------------------------------------------------------------------
    """

    pentad = tm_pentad(DateTime)

    index     = pentad['index']
    start_dt  = pentad['start']
    seconds   = pentad['length'] / 2.0
    center_dt = start_dt + dt.timedelta(seconds=seconds)

    return [index, center_dt]

def get_weighted_cli(fid_cli, field, pentads, weights, cache):
    """
    # -------------------------------------------------------------------
    # get_weighted_cli(cli_file, field, pentads, weights)
    #
    # read cli data and return weighted cli data for the input pentads
    # and corresponding weights. if a pentad is already cached, read the
    # cached data, else cache it.
    #
    #    input:  fid_cli - file id of open nc4 file containing cli data
    #              field - rootzone/profile_wetness_cli_stat
    #            pentads - two (0-based) pentads
    #            weights - interpolation weights of the pentads
    #              cache - cached cli data for accessed pentads
    #   output:     data - a dict with keys cdf, nData, mean etc.
    # -------------------------------------------------------------------
    """
    assert len(pentads)==len(weights)

    n_tiles = fid_cli.variables['tile_id'].shape[0]
    fieldata = fid_cli.groups[field]
    data = { # initialize
        'stdv': np.zeros(n_tiles),
        #'cdf': np.zeros((n_cdf,n_tiles)),
        'cdf': np.zeros((n_tiles,n_cdf)),
        'min': np.zeros(n_tiles),
        'max': np.zeros(n_tiles)
        }

    for p,w in zip(pentads,weights):
        # cache key
        ckey = str(p)+field
        # cache cli data if not present
        if not ckey in cache:
            cache[ckey] = dict()
            cache[ckey]['stdv'] = fieldata.variables['stdv'][p,:]
            cache[ckey]['cdf'] = fieldata.variables['percentile_UL'][p,:,:]
            cache[ckey]['min'] = fieldata.variables['min'][p,:]
            cache[ckey]['max'] = fieldata.variables['max'][p,:]
        # computed weighted cli
        data['stdv'] += w*cache[ckey]['stdv']
        data['cdf'] += w*cache[ckey]['cdf']
        data['min'] += w*cache[ckey]['min']
        data['max'] += w*cache[ckey]['max']

    return data


def get_gph(gph_file, nTiles):
    """
    # -------------------------------------------------------------------
    # get_gph(gph_file, nTiles)
    #
    # read binary gph file and return the variables 5 and 6
    # (sm_rootzone_wetness and sm_profile_wetness)
    #
    #    input:  gph_file - location of gph binary file
    #            nTiles - number of tiles
    #   output:  [prc_date, data, gph_sort_ndx] where
    #            prc_date - a datetime object parsed from the file name
    #            data - a dict with keys sm_rootzone/profile_wetness
    # -------------------------------------------------------------------
    """
    date = dt.datetime.strptime(
        gph_file.split('.')[-2],
        '%Y%m%d_%H%Mz'
        )
    
    fin = open(gph_file, 'rb')
    data = dict()

    for ifield in range(1,7): # NOTE: 1-indexed
        struct.unpack('<i', fin.read(4)) # fortran padding
        fmt = '<%df' % nTiles
        tmpData = struct.unpack(fmt, fin.read(4*nTiles))
        if 5==ifield: # sm_rootzone_wetness
            data['sm_rootzone_wetness'] = np.array(tmpData)
        elif 6==ifield: # sm_profile_wetness
            data['sm_profile_wetness'] = np.array(tmpData)
        else:
            pass
        struct.unpack('<i', fin.read(4)) # fortran padding

    fin.close()

    return [date, data]


def get_tileid(tilecoord_file):
    """
    # -------------------------------------------------------------------
    # get_tileid(tilecoord_file):
    #
    # read tilecoord_file and return an array containing tile_ids
    #
    #    input:  location of tile_coord file
    #   output:  tile_id - numpy array containing the ids of all tiles
    # -------------------------------------------------------------------
    """
    fin = open(tilecoord_file, 'rb')

    # read number of tiles
    struct.unpack('<i', fin.read(4)) # fortran padding
    nTiles = struct.unpack('<i', fin.read(4))[0]
    struct.unpack('<i', fin.read(4)) # fortran padding

    # read all tile_ids
    struct.unpack('<i', fin.read(4)) # fortran padding
    fmt = '<%di' % nTiles
    tile_id = np.array(struct.unpack(fmt, fin.read(4*nTiles))) # each tile_id is 4 bytes
    struct.unpack('<i', fin.read(4)) # fortran padding

    fin.close()

    print(tile_id)
    return tile_id


def append2gph(gph_file, prcntl):
    """
    # -------------------------------------------------------------------
    # Append percentile data to gph binary file
    # 4 bytes per integer/float
    # -------------------------------------------------------------------
    """
    fout = open(gph_file, 'ab')
    nTiles = len(prcntl)
    fout.write(struct.pack('<i', nTiles*4)) # padding
    fmt = '<%df' % nTiles
    fout.write(struct.pack(fmt, *prcntl))
    fout.write(struct.pack('<i', nTiles*4)) # padding
    fout.close()


def parse_args():
    """
    # -------------------------------------------------------------------
    # parse command line arguments and return a dict of options
    # -------------------------------------------------------------------
    """
    p = argparse.ArgumentParser(
        description= "Convert soil moisture from wetness units "
        "to percentile units based on soil moisture climatology inputs.")

    # arguments
    # ---------
    # positional
    p.add_argument('tilefile', help='tile-coord file (binary)')
    p.add_argument('clifile', help='cli file in NetCDF-4 format')
    p.add_argument('gphfile', 
                   nargs='+', 
                   help='list (separated by space) of gph files')

    a = p.parse_args() # vars converts to dict

    return a


if __name__ == "__main__":
    main()
