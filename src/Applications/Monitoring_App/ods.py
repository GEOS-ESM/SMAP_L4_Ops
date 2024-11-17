#! /usr/bin/env python
'''
###############################################################################
[ ods ]: science monitoring ODS file creator from ObsFcstAna binary files

Made with =[space}> by Brent Smith, 2015.
###############################################################################
'''
import os
import sys
import struct
import fnmatch
import datetime as dt

import argparse
import numpy as np
from pprint import pprint
try:
#   sys.path.append('/gpfsm/dhome/dao_ops/GEOSadas-CURRENT/GEOSadas/src/GMAO_Shared/GMAO_ods')
    sys.path.append('/discover/nobackup/jstassi/GEOSadas/5260p3/src/GMAO_Shared/GMAO_ods')
    import pyods
except ImportError as err:
    print('Import Error: GMAO pyods package not found, please locate in ADAS.')
    print(err)
    sys.exit()

###############################################################################
def find(path=os.getcwd(), ext='.bin'):
    '''Recursive search function top-down.'''
    for (root, dirs, files) in os.walk(path):
        for f in fnmatch.filter(files, '*'+ext):
            yield os.path.join(root, f)

###############################################################################
def parse_args(args):
    parser = argparse.ArgumentParser()
    required = parser.add_argument_group('required arguments')
    required.add_argument(
        '-f', '--file', required=True, help='SMAP ObsFcstAna Binary File'
    )
    parser.add_argument(
        '-o', '--path', default=os.getcwd(),
        help='output location \n\n (default: %(default)s)'
    )
    args = parser.parse_args(args)
    if not os.path.exists(args.file):
        print('File does not exist.')
        sys.exit()
    if not os.path.exists(args.path):
        os.makedirs(args.path, 0777)
    return args

###############################################################################
def read_obsparam(file):
    '''reads binary data from obsparam file(s)'''
    data = []
    print ('\nread_obsparam: Reading from: '+os.path.basename(file))
    with open(file) as f:
        N_obs = int(f.readline())
        print ('N_obs: '+str(N_obs)+'\n')

        for i in range(N_obs):
            obs = {}
            obs['descr'] = f.readline().strip()[1:-1]
            obs['species'] = float(f.readline())
            obs['orbit'] = float(f.readline())
            obs['pol'] = float(f.readline())
            obs['N_ang'] = float(f.readline())

            obs['ang'] = [float(f.readline()) for i in range(int(obs['N_ang']))]

            obs['freq'] = float(f.readline())
            obs['FOB'] = float(f.readline())
            obs['assim'] = f.readline().strip()
            obs['scale'] = f.readline().strip()
            obs['getinnov'] = f.readline().strip()
            obs['bias_Npar'] = float(f.readline())
            obs['bias_trel'] = float(f.readline())
            obs['bias_tcut'] = float(f.readline())
            obs['nodata'] = float(f.readline())
            obs['varname'] = f.readline().strip()[1:-1]
            obs['units'] = f.readline().strip()[1:-1]
            obs['path'] = f.readline().strip()[1:-1]
            obs['name'] = f.readline().strip()[1:-1]
            obs['scalepath'] = f.readline().strip()[1:-1]
            obs['scalename'] = f.readline().strip()[1:-1]
            obs['errstd'] = float(f.readline())
            obs['std_normal_max'] = float(f.readline())
            obs['zeromean'] = f.readline().strip()
            obs['coarsen_pert'] = f.readline().strip()
            obs['xcorr'] = float(f.readline())
            obs['ycorr'] = float(f.readline())
            obs['adapt'] = float(f.readline())
            data.append(obs)
    return data

###############################################################################
def read_obsparam_new_1(file):
    '''reads binary data from obsparam file(s)'''
    data = []
    print ('\nread_obsparam_new_1: Reading from: '+os.path.basename(file))
    with open(file) as f:
        N_obs = int(f.readline())
        print ('N_obs: '+str(N_obs)+'\n')

        for i in range(N_obs):
            obs = {}
            obs['descr'] = f.readline().strip()[1:-1]
            obs['species'] = float(f.readline())
            obs['orbit'] = float(f.readline())
            obs['pol'] = float(f.readline())
            obs['N_ang'] = float(f.readline())

            obs['ang'] = [float(f.readline()) for i in range(int(obs['N_ang']))]

            obs['freq'] = float(f.readline())
            obs['FOV'] = float(f.readline())
            obs['FOV_units'] = f.readline().strip()[1:-1]
            obs['assim'] = f.readline().strip()
            obs['scale'] = f.readline().strip()
            obs['getinnov'] = f.readline().strip()
            obs['RTM_ID'] = float(f.readline())
            obs['bias_Npar'] = float(f.readline())
            obs['bias_trel'] = float(f.readline())
            obs['bias_tcut'] = float(f.readline())
            obs['nodata'] = float(f.readline())
            obs['varname'] = f.readline().strip()[1:-1]
            obs['units'] = f.readline().strip()[1:-1]
            obs['path'] = f.readline().strip()[1:-1]
            obs['name'] = f.readline().strip()[1:-1]
            obs['scalepath'] = f.readline().strip()[1:-1]
            obs['scalename'] = f.readline().strip()[1:-1]
            obs['errstd'] = float(f.readline())
            obs['std_normal_max'] = float(f.readline())
            obs['zeromean'] = f.readline().strip()
            obs['coarsen_pert'] = f.readline().strip()
            obs['xcorr'] = float(f.readline())
            obs['ycorr'] = float(f.readline())
            obs['adapt'] = float(f.readline())
            data.append(obs)
    return data

###############################################################################
def read_obsparam_new_2(file):
    '''reads binary data from obsparam file(s)'''
    data = []
    print ('\nread_obsparam_new_2: Reading from: '+os.path.basename(file))
    with open(file) as f:
        N_obs = int(f.readline())
        print ('N_obs: '+str(N_obs)+'\n')

        for i in range(N_obs):
            obs = {}
            obs['descr'] = f.readline().strip()[1:-1]
            obs['species'] = float(f.readline())
            obs['orbit'] = float(f.readline())
            obs['pol'] = float(f.readline())
            obs['N_ang'] = float(f.readline())

            obs['ang'] = [float(f.readline()) for i in range(int(obs['N_ang']))]

            obs['freq'] = float(f.readline())
            obs['FOV'] = float(f.readline())
            obs['FOV_units'] = f.readline().strip()[1:-1]
            obs['assim'] = f.readline().strip()
            obs['scale'] = f.readline().strip()
            obs['getinnov'] = f.readline().strip()
            obs['RTM_ID'] = float(f.readline())
            obs['bias_Npar'] = float(f.readline())
            obs['bias_trel'] = float(f.readline())
            obs['bias_tcut'] = float(f.readline())
            obs['nodata'] = float(f.readline())
            obs['varname'] = f.readline().strip()[1:-1]
            obs['units'] = f.readline().strip()[1:-1]
            obs['path'] = f.readline().strip()[1:-1]
            obs['name'] = f.readline().strip()[1:-1]
            obs['scalepath'] = f.readline().strip()[1:-1]
            obs['scalename'] = f.readline().strip()[1:-1]
            obs['flistpath'] = f.readline().strip()[1:-1]
            obs['flistname'] = f.readline().strip()[1:-1]
            obs['errstd'] = float(f.readline())
            obs['std_normal_max'] = float(f.readline())
            obs['zeromean'] = f.readline().strip()
            obs['coarsen_pert'] = f.readline().strip()
            obs['xcorr'] = float(f.readline())
            obs['ycorr'] = float(f.readline())
            obs['adapt'] = float(f.readline())
            data.append(obs)
    return data

###############################################################################
def read_ObsFcstAna(file):
    '''reads binary data from ObsFcstAna file(s)'''
    fdata = {}
    print ('\nReading from: '+os.path.basename(file))
    with open(file, 'rb') as f:
        f = f.read()

        # read N_obs and time stamp entry
        ftag, N_obs = struct.unpack('<2i',f[:8])
        print ('N_obs: '+str(N_obs))
        date_time = ['year', 'month', 'day', 'hour', 'min', 'sec', 'dofyr', 'pentad']
        date_time = dict(zip(date_time, struct.unpack('<8i', f[8:40])))

        f = f[44:]
        # since precision is int32 and float32, N_obs*4bytes is my chunk size
        fields = ['obs_assim', 'obs_species', 'obs_tilenum', 'obs_lon', 'obs_lat',
                  'obs_obs',   'obs_obsvar',  'obs_fcst', 'obs_fcstvar', 'obs_ana', 'obs_anavar']
        for name in fields:
            # skip preceeding ftag
            f = f[4:]
            chunk = f[:(N_obs*4)]

            if name in ['obs_assim', 'obs_species', 'obs_tilenum']:
                if name in 'obs_assim':
                    # boolean array
                    fdata[name] = np.int32(struct.unpack('<'+str(N_obs)+'i', chunk))
                    fdata[name][ fdata[name] != 0 ] = 1
                    fdata[name] = np.bool_(fdata[name])
                else:
                    # integer arrays
                    fdata[name] = np.int32(struct.unpack('<'+str(N_obs)+'i', chunk))
            else:
                # floating point arrays
                fdata[name] = np.float32(struct.unpack('<'+str(N_obs)+'f', chunk))
                fdata[name] = np.ma.masked_values(fdata[name], -9999.0)

            # skip remainder ftag
            f = f[4+(N_obs*4):]

        fdata['N_obs'] = N_obs
    return fdata

###############################################################################
def main(fcstana=None, path=os.getcwd()):
    # get data and description of data
    if not fcstana:
        fcstana = args.fcstana
    # find obs_param
    sn = os.path.basename(fcstana).split('.')[0]
    date = os.path.basename(fcstana).split('.')[-2].split('_')[0]
    param_dir = '/discover/nobackup/projects/gmao/smap/Operations/'+sn.split('_')[0]+'/'+sn
    if '0000z' in os.path.basename(fcstana):
        date = dt.datetime(int(date[:4]), int(date[4:6]), int(date[6:8]), 0, 0, 0)
        date = date-dt.timedelta(days=1)
        date = date.strftime('%Y%m%d')
    param = [x for x in find(param_dir, ext='.txt') if 'param' in x and date in x]

    if not param:
        return
    try:
        param = read_obsparam_new_2(param[0])
    except:
        try:
            param = read_obsparam_new_1(param[0])
        except:
            param = read_obsparam(param[0])
    # remap for kx key
    kx = sorted((x['descr'], x['species']) for x in param)
    kx_key = {
        'SMAP_L1C_Tbh_A':640,
        'SMAP_L1C_Tbh_D':641,
        'SMAP_L1C_Tbv_A':642,
        'SMAP_L1C_Tbv_D':643,
        'SMAP_L2AP_Tbh_A':644,
        'SMAP_L2AP_Tbh_D':645,
        'SMAP_L2AP_Tbv_A':646,
        'SMAP_L2AP_Tbv_D':647
    }

    # read data
    data = read_ObsFcstAna(fcstana)
    N_obs = data['N_obs']
    if N_obs < 1:
        return

    # start pyods creation
    ods = pyods.ODS(nobs=N_obs)
    try:
        exp, ens, o, stime, ext  = os.path.basename(fcstana).split('.')
    except:
        print('Problem getting filename fields: {expname}.ens_avg.ldas_ObsFcstAna.{date}.bin')
        return

    ods.lat[:] = data['obs_lat'].filled(1.e15)
    ods.lon[:] = data['obs_lon'].filled(1.e15)
    ods.obs[:] = data['obs_obs'].filled(1.e15)
    # kx mod of obs_species to key values
    for key,value in kx:
        x = data['obs_species']
        #if 'L2AP' in key:
        #    print x[x == int(value)]
        x[x == int(value)] = kx_key[key]
        data['obs_species'] = x
    ods.kx[:] = data['obs_species']
    ods.kt[:] = 40
    ods.ks[:] = np.arange(N_obs)+1
#    omf_mask = data['obs_obs'].mask or data['obs_fcst'].mask
#    oma_mask = data['obs_obs'].mask or data['obs_ana'].mask
#    ods.omf[:] = np.ma.masked_array(data['obs_obs']-data['obs_fcst'], mask=omf_mask).filled(1.e15)
#    ods.oma[:] = np.ma.masked_array(data['obs_obs']-data['obs_ana'], mask=oma_mask).filled(1.e15)
    ods.omf[:] = data['obs_obs']-data['obs_fcst']
    ods.oma[:] = data['obs_obs']-data['obs_ana']
    ods.oma[ods.oma == ods.obs] = 1.e15
    ods.omf[ods.omf == ods.obs] = 1.e15
    ods.qcx[:] = np.logical_not(data['obs_assim'])
    # xvec equiv to sigo = sqrt(obsvar)
    ods.xvec[:] = np.sqrt(data['obs_obsvar'])
    # xm we are assigning to be sigb = sqrt(fcstvar)
    ods.xm[:] = np.sqrt(data['obs_fcstvar'])

    syn_time = dt.datetime(int(stime[:4]), int(stime[4:6]), int(stime[6:8]),
                           int(stime[9:11]), int(stime[11:13]), 0)
    nymd = 10000*syn_time.year + 100*syn_time.month + syn_time.day
    nhms = 10000*syn_time.hour + 100*syn_time.minute + syn_time.second

    # pyods appends rather than overwrites
    f = os.path.join(path, '.'.join([exp, 'Tb', stime, 'ods']))
    if os.path.exists(f):
        os.remove(f)
    ods.write(f, nymd=nymd, nhms=nhms, nsyn=8)

###############################################################################
if __name__ == '__main__':
    args = parse_args(sys.argv[1:])
    main(fcstana=args.file, path=args.path)
#    files = [x for x in find('/discover/nobackup/projects/gmao/smap/SMAP_L4/L4_SM/V10002') if 'ObsFcstAna' in x]
#    for f in files:
#        main(f, path=os.path.join(os.getcwd(), 'corrected'))
    # requires an ObsFcstAna file
    # - more than one file?
    # - output specified?
    #main(sys.argv[1:])
