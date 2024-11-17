#! /usr/bin/env python
'''
###############################################################################
[ browse ]: SMAP imagery w/ a colorbar

Made with =[space}> by Brent Smith, 2015.
###############################################################################
'''
import os
import sys
import png
import fnmatch
import multiprocessing as mp
import itertools

import numpy as np
import h5py as h5
import matplotlib as mpl
# graphical backend
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors

# global field dictionary for variable selection
fields = {
    'NEE/nee_mean':{'cmap':'C_1','mi':-2,'ma':2},
    'GPP/gpp_mean':{'cmap':'C_2','mi':0,'ma':10},
    'RH/rh_mean':{'cmap':'C_3','mi':0,'ma':6},
    'EC/emult_mean':{'cmap':'SM_2','mi':0,'ma':100},
    'EC/emult_av':{'cmap':'SM_2','mi':0,'ma':100},
    'EC/frozen_area':{'cmap':'C_6','mi':0,'ma':100},
    'QA/nee_rmse_mean':{'cmap':'SM_2','mi':0,'ma':3},
    'Analysis_Data/sm_rootzone_analysis':{'cmap':'SMOS','mi':0,'ma':0.7},
    'Analysis_Data/sm_surface_analysis':{'cmap':'SMOS','mi':0,'ma':0.7},
    'Analysis_Data/soil_temp_layer1_analysis':{'cmap':'SM_2','mi':230,'ma':320},
    'Geophysical_Data/snow_mass':{'cmap':'SM_3','mi':0,'ma':200},
    'Analysis_Data/sm_rootzone_analysis_ensstd':{'cmap':'SM_2','mi':0,'ma':0.03},
    'Analysis_Data/sm_surface_analysis_ensstd':{'cmap':'SM_2','mi':0,'ma':0.03},
}

###############################################################################

def rc():
    '''custom plot settings, use mpl.rcdefaults() to default'''
    mpl.rc('lines', linewidth=0.5, antialiased=False)
    mpl.rc('patch', linewidth=0.5, facecolor='348ABD', edgecolor='eeeeee',
          antialiased=True)
    mpl.rc('axes', facecolor='black', edgecolor='black', linewidth=1.0)
    mpl.rc('font', family='monospace', size=10.0)
    mpl.rc('xtick', color='black')
    mpl.rc('xtick.major', size=4, pad=6)
    mpl.rc('xtick.minor', size=2, pad=6)
    mpl.rc('ytick', color='black')
    mpl.rc('ytick.major', size=4, pad=6)
    mpl.rc('ytick.minor', size=2, pad=6)
    mpl.rc('legend', fancybox=True, fontsize=10.0)
    mpl.rc('figure', figsize='12, 7.5', dpi=300, facecolor='white')
    mpl.rc('figure.subplot', hspace=0.5, left=0.07, right=0.95, bottom=0.1,
          top=0.95)

###############################################################################

def main_wrapper(args):
    return main(*args)

###############################################################################

def parse_args(args):
    '''command-line arguments'''
    import argparse
    parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter)
    required = parser.add_argument_group('required arguments')
    required.add_argument(
        '-f', '--file', required=True, help='GEOS-5 HDF5 file'
    )
    parser.add_argument(
        '-c', '--cmap', help='directory path containing cmap tables'
    )
    parser.add_argument(
        '-o', '--path', default=os.getcwd(),
        help='output location\n(default: %(default)s)\n\n'
    )
    parser.add_argument(
        '-s', '--smos', action='store_true', help='remove use of SMOS color table'
    )
    parser.add_argument(
        '-n', '--name', default=fields.keys(), nargs='*',
        help='field(s) to plot\n(default field are ops fields)'
    )
    args = parser.parse_args(args)
    if not os.path.exists(args.file):
        print('File does not exist.')
        sys.exit()
    if args.cmap:
        if not os.path.isdir(args.cmap):
            print('cmap directory does not exit.')
            sys.exit()
        if args.smos:
            fields['Analysis_Data/sm_rootzone_analysis']['cmap'] = 'SM_1'
            fields['Analysis_Data/sm_surface_analysis']['cmap'] = 'SM_1'
    # will attempt to create dne output folders
    if type(args.name) is not list:
        args.name = list(args.name)
    return args

###############################################################################

def main(f, field, ct, path):
    dt = h5.File(f, 'r')
    try:
        data = dt[field][:]
    except:
        print('Field not in granule: '+field)
        return
    if ct not in 'jet':
        if 'gpp' in field:
            with open(os.path.join(ct, 'L4_'+fields[field]['cmap']+'.txt')) as cm:
                cmap = cm.read().splitlines()
        else:
            with open(os.path.join(ct, 'L4_'+fields[field]['cmap']+'.txt')) as cm:
                cmap = cm.read().splitlines()
        tmp = []
        for line in cmap:
            r,g,b = line.split() #(' ')
            tmp += [1., int(b)/255., int(g)/255., int(r)/255.]
        tmp.reverse()
        cmap = [0,0,0,0] + tmp[:-4]
        cmap = [cmap[i:i+4] for i in range(0, len(cmap), 4)]
        cmap = mpl.colors.ListedColormap(cmap[1:][::-1], name='cmap')
    else:
        cmap = 'jet'

    try:
        data = np.ma.masked_values(data, dt[field].attrs.get('_FillValue'))
    except:
        data = np.ma.masked_equal(data, -9999.0)

    fig = plt.figure(figsize=(12,7.5), dpi=150)
    rc()
    ax = plt.Axes(fig, [0.05, 0.0, 0.9, 1.0])
    fig.add_axes(ax)

    plt.title(field.split('/')[-1].replace('_', ' ')+'\n'+(' '.join(os.path.basename(f).split('.')[0].split('_')).upper()), fontsize=14, fontname='monospace')
    if field not in fields:
        vmin = dt[field].attrs.get('valid_min')
        vmax = dt[field].attrs.get('valid_max')
    else:
        vmin = fields[field]['mi']
        vmax = fields[field]['ma']
    m = ax.imshow(data, cmap=cmap, vmin=vmin, vmax=vmax, interpolation='nearest')

    ax.set_xticklabels([])
    ax.set_yticklabels([])
    ax.set_xticks([])
    ax.set_yticks([])

    cax = plt.axes([0.05, 0.15, 0.9, 0.02])
    cbar = plt.colorbar(m, orientation='horizontal', cax=cax)
    cbar.set_label('[ '+dt[field].attrs.get('units')+' ]')
    try:
        os.makedirs(path)
    except OSError:
        if not os.path.isdir(path):
            raise
    plt.savefig(os.path.join(path, os.path.basename(f)[:-3]+'_'+field.split('/')[-1]+'.png'), bbox_inches='tight', dpi=150)
    plt.close()
    print os.path.basename(f)+': '+field.split('/')[-1]
    sys.stdout.flush()

###############################################################################
            
if __name__ == '__main__':
    args = parse_args(sys.argv[1:])
    if not args.cmap:
        args.cmap = 'jet'
    things = [(args.file, field, args.cmap, args.path) 
              for field in sorted(args.name)
              if ('T00' in args.file and 'soil_temp' not in field)
              or ('soil_temp' in field)
              or ('T22' in args.file and 'snow_mass' in field )
             ]
    pool = mp.Pool()
    pool.map(main_wrapper, things)
    pool.close()
    pool.join()
