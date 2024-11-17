#!/usr/bin/env python
#
# original version by pchakrab, 2014
#
# updated to include ascending 9km Tbs - reichle, 14 Jul 2015

import argparse, os
from sys import stdout, exit
import numpy as np
import h5py as h5
from collections import OrderedDict
import time

# -----------------------------------------
# granule (LMC/GPH/LMC) headers and footers
# are defined towards the end of the file
# -----------------------------------------

#----------------------
# some global constants
#----------------------

# tolerance for comparing 64bit floats
tol_float64 = 10*np.finfo(np.float64).eps

# output formats
FORMAT_f = '%-51s,%-16s,%13.4f,%13.4f,%13.4f,%13.4f,%9d\n'
FORMAT_e = '%-51s,%-16s,%13.4e,%13.4e,%13.4e,%13.4e,%9d\n'


def main():

    """
    # --------------------------------------------------------------------------
    main program: WHAT DOES THIS DO??
    # --------------------------------------------------------------------------
    """

    comm_args = get_comm_line_args() # command line arguments
    land_frac_file = comm_args.landfrac
    lmc_file = comm_args.lmcfile
    gph_file = comm_args.gphfile
    aup_file = comm_args.aupfile

    # -----------------------------------------------------
    # Get land data - num_tile, land_frac, land_frac_isdata
    # land_frac_isdata is a logical array with value False
    #    where land_frac has NODATA value
    # Land data is needed for generating QA for all
    # granules (LMC/GPH/AUP) and so is the reqd 1st step
    # -----------------------------------------------------
    assert os.path.isfile(land_frac_file), 'file [%s] not found' % land_frac_file
    assert 'lmc' in os.path.basename(land_frac_file).lower()

    stdout.write('Reading cell_land_fraction data...')
    stdout.flush()
    t_start = time.time()
    lf_in = h5.File(land_frac_file, 'r')
    [num_tiles_9km, num_tiles_36km, land_frac, land_frac_isdata] = get_land_info(lf_in)
    lf_in.close()
    t_end = time.time()
    stdout.write('done. [%fs]\n' % (t_end-t_start))
    stdout.flush()

    # ------------------------------------
    # land model constants variables (lmc)
    # ------------------------------------
    if lmc_file:
        assert os.path.isfile(lmc_file), 'file [%s] not found' % lmc_file
        assert 'lmc' in os.path.basename(lmc_file).lower()
        t_start = time.time()
        stdout.write('Generating QA data for LMC granule...')
        stdout.flush()
        skip_vars = ['mwrtm_soilcls', 'mwrtm_vegcls']
        # get stats (mean, std etc.) of the variables
        lmc_in = h5.File(lmc_file, 'r')
        lmc_stats = get_var_info(
            lmc_in,                      # name of input h5 granule
            'Land-Model-Constants_Data', # h5 group name
            land_frac,                   # cell_land_fraction from LMC granule
            land_frac_isdata,            # False at a cell with NODATA value
            skip_vars,                   # variables to skip
            )
        lmc_in.close()
        # write stats info to qa file
        lmc_out_file = lmc_file.replace('h5', 'qa') # lmc qa file
        assert (not os.path.isfile(lmc_out_file)), '[%s] already exists' % lmc_out_file
        lmc_out = open(lmc_out_file, 'w')
        write_qa(lmc_out, lmc_file, lmc_stats, lmc_header, lmc_footer, num_tiles_9km)
        lmc_out.close()
        t_end = time.time()
        stdout.write('done. [%fs]\n' % (t_end-t_start))
        stdout.flush()

    # ---------------------------
    # geophysical variables (gph)
    # ---------------------------
    if gph_file:
        assert os.path.isfile(gph_file), 'file [%s] not found' % gph_file
        assert 'gph' in os.path.basename(gph_file).lower()
        t_start = time.time()
        stdout.write('Generating QA data for GPH granule...')
        stdout.flush()
        # get stats (mean, std etc.) of the variables
        gph_in = h5.File(gph_file, 'r')
        gph_stats = get_var_info(
            gph_in,                      # file-id of input h5 granule
            'Geophysical_Data',          # h5 group name
            land_frac,                   # cell_land_fraction from LMC granule
            land_frac_isdata             # False at a cell with NODATA value
            )
        gph_in.close()
        # write stats info to file
        gph_out_file = gph_file.replace('h5', 'qa') # gph qa file
        assert (not os.path.isfile(gph_out_file)), '[%s] already exists' % gph_out_file
        gph_out = open(gph_out_file, 'w')
        write_qa(gph_out, gph_file, gph_stats, gph_header, gph_footer, num_tiles_9km)
        gph_out.close()
        t_end = time.time()
        stdout.write('done. [%fs]\n' % (t_end-t_start))
        stdout.flush()

    # -------------------------------------
    # analysis update variables (aup)
    # aup variables need special processing
    # -------------------------------------
    if aup_file:
        assert os.path.isfile(aup_file), 'file [%s] not found' % aup_file
        assert 'aup' in os.path.basename(aup_file).lower()
        # aup tb
        t_start = time.time()
        stdout.write('Generating QA data for AUP granule:\n   brightness temperature...')
        stdout.flush()
        aup_in = h5.File(aup_file, 'r')
        auptb_stats = get_aup_tb_var_info(
            aup_in,                      # file-id of input h5 granule
            land_frac,                   # cell_land_fraction from LMC granule
            land_frac_isdata             # False at a cell with NODATA value
            )
        # not closing yet
        # write stats info to file
        aup_out_file = aup_file.replace('h5', 'qa') # aup qa file
        assert (not os.path.isfile(aup_out_file)), '[%s] already exists' % aup_out_file
        aup_out = open(aup_out_file, 'w')
        write_qa(aup_out, aup_file, auptb_stats, auptb_header, auptb_footer, num_tiles_9km, num_tiles_36km)
        t_end = time.time()
        stdout.write('done. [%fs]\n' % (t_end-t_start))
        stdout.flush()
        # aup gph
        t_start = time.time()
        stdout.write('   geophysical variables...')
        stdout.flush()
        aupgph_stats = get_aup_gph_var_info(
            aup_in,                      # file-id of input h5 granule
            land_frac,                   # cell_land_fraction from LMC granule
            land_frac_isdata             # False at a cell with NODATA value
            )
        aup_in.close()
        # write stats info to file that is already open
        write_qa(aup_out, aup_file, aupgph_stats, aupgph_header, aupgph_footer)
        aup_out.close()
        t_end = time.time()
        stdout.write('done. [%fs]\n' % (t_end-t_start))
        stdout.flush()

    return 0 # and we are done



def get_land_info(lmc_in):
    """
    # --------------------------------------------------------------------------
    # Compute land fraction of each gridcell and num of contributing gridcells
    #
    # Input:
    #      lmc_in           - file id of open input h5 LMC granule
    # Output:
    #      num_tiles_9km    - number of 9km land tiles
    #      num_tiles_36km   - number of 36km land tiles 
    #      land_frac        - contents of LandModelConstants_Data/cell_land_fraction
    #                         from LMC granule file
    #      land_frac_isdata - logical array with False at cells with NODATA value
    # --------------------------------------------------------------------------
    """

    # land fraction and number of tiles
    land_frac = np.float64(lmc_in['Land-Model-Constants_Data/cell_land_fraction'].value)
    land_frac_fillval = np.float64(lmc_in['Land-Model-Constants_Data/cell_land_fraction'].attrs.get('_FillValue'))
    land_frac_isdata = np.abs(land_frac-land_frac_fillval)>tol_float64

    # number of 9km tiles (it is important to store data in a float64 container)
    dzsf = np.float64(lmc_in['Land-Model-Constants_Data/clsm_dzsf'].value)
    FillValue = np.float64(lmc_in['Land-Model-Constants_Data/clsm_dzsf'].attrs.get('_FillValue'))
    mask_isdata = np.abs(dzsf-FillValue)>tol_float64
    num_tiles_9km = dzsf[mask_isdata].shape[0] # 9km tiles  
    # sum of a bool array returns num of True
    assert num_tiles_9km==np.sum(land_frac_isdata)

    # compute number of 36km tiles
    (nRows9km, nCols9km) = dzsf.shape
    num_tiles_36km = 0
    for iR in xrange(0,nRows9km,4):
        for jC in xrange(0,nCols9km,4):
            # here, var[iR:iR+4,jC:jC+4] is the 4x4 block
            mask_isdata_block = mask_isdata[iR:iR+4,jC:jC+4]
            if np.where(mask_isdata_block==True)[0].size > 0:
                num_tiles_36km += 1

    return [num_tiles_9km, num_tiles_36km, land_frac, land_frac_isdata]



def get_var_info(fid, H5GroupName, land_frac, land_frac_isdata, skipped=[]):
    """
    # --------------------------------------------------------------------------
    # Get mean, std etc. from the given LMC/GPH h5 granule
    #
    # Input:
    #      fid              - file id of open input h5 granule (LMC/GPH)
    #      H5GroupName      - h5 GROUP to read from
    #      land_frac        - numpy array containing the value of the variable
    #                         'LandModelConstants_Data/cell_land_fraction'
    #      land_frac_isdata - logical arr with False at cells with NODATA value
    #      skipped          - list of skipped variables (default empty list)
    # Output:
    #      VarStat          - an ordered dict with
    #                         key = variable name
    #                         value = dict with keys N, stats
    # --------------------------------------------------------------------------
    """

    VarStat = OrderedDict()

    # list of variables in the group h5group
    VariableList = fid[H5GroupName].keys()

    # iterate over all variables
    for variable in VariableList:
        # name
        #VarStat[var] = dict() # a nested dict (not ordered)
        # units
        v_unit = fid[H5GroupName][variable].attrs.get('units').strip()
        if v_unit=='1': v_unit='dimensionless'
        # VERY IMPORTANT: use dtype=np.float64 or higher
        #       to prevent loss of precision
        v_value = np.float64(fid[H5GroupName][variable].value)
        v_fillvalue = np.float64(fid[H5GroupName][variable].attrs.get('_FillValue'))
        mask_isdata = np.abs(v_value-v_fillvalue)>tol_float64
        v_value_isdata = v_value[mask_isdata]
        v_weight = land_frac[mask_isdata] # land_frac is used here
        v_N = v_value_isdata.shape[0]
        if variable in skipped:
            v_mean = v_std = -9999.0
            v_min = np.min(v_value_isdata)
            v_max = np.max(v_value_isdata)
        else:
            [v_mean, v_std, v_min, v_max] = compute_stats(v_value_isdata, v_weight)

        VarStat[variable] = {
            'N': v_N,
            'units': v_unit,
            'mean': v_mean,
            'std': v_std,
            'min': v_min,
            'max': v_max
            }

    return VarStat



def compute_stats(value, weight):
    """
    # --------------------------------------------------------------------------
    # Compute mean, std, min and max of the array 'value'.
    # For mean and std, 'value' is weighted by 'weight'
    #
    # Input:
    #      value            - array whose mean, std etc. we are computing
    #      weight           - weights for weighted mean, std
    # Output:
    #      the list [v_mean, v_std, v_min, v_max]
    # --------------------------------------------------------------------------    
    """

    assert value.shape==weight.shape, 'value and weight have different shapes??'

    stats = dict()

    v_N = value.shape[0]

    if v_N==0:
        v_mean = v_std = v_min = v_max = -9999.0
    else:
        # mean and std are weighted
        v_mean = np.sum(value*weight)/np.sum(weight)
        tmp = (value-v_mean)*(value-v_mean)
        v_std = np.sqrt(np.sum(tmp*weight)/np.sum(weight))
        # min, max are not weighted
        v_min = np.min(value)
        v_max = np.max(value)

    return [v_mean, v_std, v_min, v_max]



def get_aup_tb_var_info(aup_id, land_frac, land_frac_isdata):
    """
    # --------------------------------------------------------------------------
    # Get mean, std etc. from the given AUP granule
    #
    # Input:
    #      aup_id           - file id of input h5 aup granule
    #      land_frac        - numpy array containing the value of the
    #                         variable 'LandModelConstants_Data/cell_land_fraction'
    #      land_frac_isdata - logical array with False at cells with NODATA value
    # Output:
    #      VarStat          - an ordered dict with
    #                         key = variable name
    #                         value = dict with keys N, stats
    # --------------------------------------------------------------------------
    """

    VarStat = OrderedDict()

    # --------------------------------------------------------------------------
    # Brightness temperatures ("EnKF observation space")
    # from Observations_Data and Forecast_Data groups
    # Steps:
    # 1. create variable lists for h and v data
    # 2. compute masks for h and v data
    #    (a) read resolution and orbit flags
    # --------------------------------------------------------------------------

    # step 1: create variable list for h and v data
    tb = dict()
    tb['h'] = {
        'var_res': 'Observations_Data/tb_h_resolution_flag',
        'var_orb': 'Observations_Data/tb_h_orbit_flag',
        'var_list': [
            'Observations_Data/tb_h_obs',
            'Observations_Data/tb_h_obs_assim',
            'Forecast_Data/tb_h_forecast',
            'Observations_Data/tb_h_obs_errstd',
            'Forecast_Data/tb_h_forecast_ensstd',
            ]
        }
    tb['v'] = {
        'var_res': 'Observations_Data/tb_v_resolution_flag',
        'var_orb': 'Observations_Data/tb_v_orbit_flag',
        'var_list': [
            'Observations_Data/tb_v_obs',
            'Observations_Data/tb_v_obs_assim',
            'Forecast_Data/tb_v_forecast',
            'Observations_Data/tb_v_obs_errstd',
            'Forecast_Data/tb_v_forecast_ensstd',
            ]
        }
    
    for tb_key in tb.keys():
        tb_p = tb[tb_key] # p for polarization (h/v)
        # the resolution and orbit flags
        res_value = aup_id[tb_p['var_res']].value # uint32
        orb_value = aup_id[tb_p['var_orb']].value # uint32

        # resolution/orbit dimensions
        (nRows9km, nCols9km) = res_value.shape

        # masks: resolution_orbit
        orb_A = (orb_value==1)
        orb_D = (orb_value==2)
        res_36km = (res_value==1)
        mask_rb = dict() # rb = (r)esolution or(b)it
        mask_rb['36km'] = (res_36km) & ((orb_value==0) | (orb_A) | (orb_D))
        mask_rb['36km_A'] = (res_36km) & (orb_A)
        mask_rb['36km_D'] = (res_36km) & (orb_D)
        mask_rb['09km'] = (res_value==2) & ((orb_value==0) | (orb_A) | (orb_D))
        mask_rb['09km_D'] = (res_value==2) & (orb_D)
        mask_rb['09km_A'] = (res_value==2) & (orb_A)
        mask_keys_all = ['36km', '36km_A', '36km_D', '09km', '09km_A', '09km_D']

        # store data to enable computing
        # the minus and norm variables
        stored_vars = OrderedDict()

        # We need to explicitly reference the "assim" variable types in
        # order to build a mask to eliminate passive observations that
        # exist in the "forecast" types. Please note that the fill value
        # is assumed to be consistent in the assim and forecast fields.

        mask = dict()
        mask['h'] = np.float64(aup_id['Observations_Data/tb_h_obs_assim'].value)
        mask['v'] = np.float64(aup_id['Observations_Data/tb_v_obs_assim'].value)

        for variable in tb_p['var_list']:
            # get variable info
            v_name = variable.split('/')[-1]
            v_name_type = v_name.split('_')[-1]
            v_unit = aup_id[variable].attrs.get('units').strip()
            if v_unit=='1': v_unit='dimensionless'
            v_fillval = np.float64(aup_id[variable].attrs.get('_FillValue'))
            v_value = np.float64(aup_id[variable].value) # store in 64 bit arrays

            if (v_name.find('forecast') >= 0):
                v_isdata = np.abs(mask[tb_key]-v_fillval)>tol_float64
            else:
                v_isdata = np.abs(v_value-v_fillval)>tol_float64 # non-NODATA

            mask_rb_isdata = dict()

            for mask_key in mask_keys_all:
                # restrict masks to variable non-nondata values
                mask_rb_isdata[mask_key] = (mask_rb[mask_key]) & (v_isdata)

                # 36km masks need extra work - look at
                # 4x4 09km mask blocks and pick first element
                if mask_key.startswith('36km'):
                    # it is easier to append to lists
                    v_value_masked_l = list()
                    v_weight_l = list()
                    for iR in xrange(0,nRows9km,4):
                        for jC in xrange(0,nCols9km,4):
                            # here, var[iR:iR+4,jC:jC+4] is the 4x4 block
                            mask_block = mask_rb_isdata[mask_key][iR:iR+4,jC:jC+4]
                            WT = np.where(mask_block==True) # WT = (W)here (T)rue
                            if WT[0].size<=0:
                                continue
                            v_value_masked_l.append(v_value[iR+WT[0][0],jC+WT[1][0]])
                            l_f_block = land_frac[iR:iR+4,jC:jC+4]
                            l_f_isdata_block = land_frac_isdata[iR:iR+4,jC:jC+4]
                            v_weight_l.append(np.mean(l_f_block[l_f_isdata_block]))
                    # in the end we want np arrays
                    v_value_masked = np.array(v_value_masked_l)
                    v_weight = np.array(v_weight_l)

                # 09km_D mask does not need extra work
                else:
                    assert mask_key.startswith('09km')
                    mask_09km = mask_rb_isdata[mask_key]
                    v_value_masked = v_value[mask_09km]
                    v_weight = land_frac[mask_09km]

                # now store the variable, so we can compute
                # the 'minus' and 'norm' variable stats
                stored_vars[v_name + '_' + mask_key] = {
                    'unit': v_unit,
                    'value': v_value_masked,
                    'weight': v_weight
                    }
        
        # compute mean, std etc. and store in VarStat
        for svar in stored_vars.keys():
            sname = svar
            svalue = stored_vars[svar]['value']
            sweight = stored_vars[svar]['weight']
            sunit = stored_vars[svar]['unit']
            [smean, sstd, smin, smax] = compute_stats(svalue, sweight)
            VarStat[sname] = {
                'N': len(svalue),
                'units': sunit,
                'mean': smean,
                'std': sstd,
                'min': smin,
                'max': smax
                }

        # now do the minus/norm calculations
        for mask_key in mask_keys_all:
            var1 = 'tb_' + tb_key + '_obs_assim_' + mask_key
            var2 = 'tb_' + tb_key + '_forecast_' + mask_key
            # minus
            name_m = 'tb_' + tb_key + '_obs_assim_minus_forecast_' + mask_key
            value_m = stored_vars[var1]['value'] - stored_vars[var2]['value']
            weight_m = stored_vars[var1]['weight'] # or var2, doesn't matter
            [mean_m, std_m, min_m, max_m] = compute_stats(value_m, weight_m)
            unit_m = stored_vars[var1]['unit']
            VarStat[name_m] = {
                'N': len(value_m),
                'units': unit_m,
                'mean': mean_m,
                'std': std_m,
                'min': min_m,
                'max': max_m
                }
            # norm
            var3 = 'tb_' + tb_key + '_obs_errstd_' + mask_key
            var4 = 'tb_' + tb_key + '_forecast_ensstd_' + mask_key
            var3val = stored_vars[var3]['value']
            var4val = stored_vars[var4]['value']
            name_n = 'tb_' + tb_key + '_norm_obs_assim_minus_forecast_' + mask_key
            dnmntr =  np.sqrt(var3val*var3val + var4val*var4val)
            value_n = value_m/dnmntr
            weight_n = weight_m
            [mean_n, std_n, min_n, max_n] = compute_stats(value_n, weight_n)
            unit_n = unit_m + ' ' + unit_m + '-1'
            VarStat[name_n] = {
                'N': len(value_n),
                'units': unit_n,
                'mean': mean_n,
                'std': std_n,
                'min': min_n,
                'max': max_n
                }

    return VarStat



def get_aup_gph_var_info(aup_id, land_frac, land_frac_isdata):
    """
    # --------------------------------------------------------------------------
    # Get mean, std etc. from the given AUP granule
    #
    # Input:
    #      aup_id           - file id of input h5 aup granule
    #      land_frac        - numpy array containing the value of the variable
    #                         'LandModelConstants_Data/cell_land_fraction'
    #      land_frac_isdata - logical arr with False at cells with NODATA value
    # Output:
    #      VarStat          - an ordered dict with
    #                         key = variable name
    #                         value = dict with keys N, stats
    # --------------------------------------------------------------------------
    """

    VarStat = OrderedDict()

    # --------------------------------------------------------------------------
    # Geophysical variables ("EnKF state space")
    # Steps:
    # 1. Create list of variables
    # --------------------------------------------------------------------------
    aupgph_var_list = [
        # forecast
        'Forecast_Data/sm_surface_forecast',
        'Forecast_Data/sm_rootzone_forecast',
        'Forecast_Data/sm_profile_forecast',
        'Forecast_Data/surface_temp_forecast',
        'Forecast_Data/soil_temp_layer1_forecast',
        # analysis
        'Analysis_Data/sm_surface_analysis',
        'Analysis_Data/sm_rootzone_analysis',
        'Analysis_Data/sm_profile_analysis',
        'Analysis_Data/surface_temp_analysis',
        'Analysis_Data/soil_temp_layer1_analysis',
        'Analysis_Data/sm_surface_analysis_ensstd',
        'Analysis_Data/sm_rootzone_analysis_ensstd',
        'Analysis_Data/sm_profile_analysis_ensstd',
        'Analysis_Data/surface_temp_analysis_ensstd',
        'Analysis_Data/soil_temp_layer1_analysis_ensstd',
        ]

    # read variables in aupgph_var_list
    # from the h5 file and store them
    shape9km = aup_id['Forecast_Data/surface_temp_forecast'].value.shape
    stored_vars = {
        'analysis': OrderedDict(),
        'forecast': OrderedDict(),
        'analysis_minus_forecast': OrderedDict(),
        'mask_nnz': np.zeros(shape9km, dtype=np.bool8), # init to True
        }
    for variable in aupgph_var_list:
        v_name = variable.split('/')[-1]
        v_unit = aup_id[variable].attrs.get('units').strip()
        if v_unit=='1': v_unit='dimensionless'
        # VERY IMPORTANT: use dtype=np.float64 or higher
        #       to prevent loss of precision
        v_value = np.float64(aup_id[variable].value)
        v_fillvalue = np.float64(aup_id[variable].attrs.get('_FillValue'))
        v_isdata = np.abs(v_value-v_fillvalue)>tol_float64
        tmp_dict = {
            v_name: {
                'unit': v_unit,
                'value': v_value,
                'isdata': v_isdata,
                }
            }
        # analysis or forecast?
        ana_fcst = variable.split('/')[0].split('_')[0].lower()
        # store data
        stored_vars[ana_fcst].update(tmp_dict)

    # compute analysis_minus_forecast variables
    # and store them. also compute mask_nnz and store it
    for fcst_var_name in stored_vars['forecast']: # m for minus
        ana_var_name = fcst_var_name.replace('forecast', 'analysis')
        var_name_base = fcst_var_name.split('_forecast')[0]
        m_name = 'analysis_minus_forecast_' + var_name_base
        m_unit = stored_vars['forecast'][fcst_var_name]['unit']
        fcst_val = stored_vars['forecast'][fcst_var_name]['value']
        ana_val = stored_vars['analysis'][ana_var_name]['value']
        m_value = ana_val - fcst_val
        fcst_isdata = stored_vars['forecast'][fcst_var_name]['isdata']
        ana_isdata = stored_vars['analysis'][ana_var_name]['isdata']
        m_isdata = (fcst_isdata) & (ana_isdata) # IS THIS REALLY NECESSARY??
        # store data
        tmp_dict = {
            m_name: {
                'unit': m_unit,
                'value': m_value,
                'isdata': m_isdata,
                }
            }
        stored_vars['analysis_minus_forecast'].update(tmp_dict)
        # nnz mask
        if var_name_base.startswith('sm_'): my_zero = np.float64(1.0e-5)
        else:
            assert var_name_base.split('_')[1]=='temp', 'not a temp var'
            my_zero = np.float64(1.0e-3)
        stored_vars['mask_nnz'] = (stored_vars['mask_nnz']) | (np.abs(m_value)>my_zero)

    # compute mean, std etc. of stored_vars and store in VarStat
    for tYpe in ['forecast', 'analysis', 'analysis_minus_forecast']:
        for s_name in stored_vars[tYpe]: # s for stored
            # all data
            s_unit = stored_vars[tYpe][s_name]['unit']
            s_value = stored_vars[tYpe][s_name]['value']
            s_isdata = stored_vars[tYpe][s_name]['isdata']
            val_masked = s_value[s_isdata]
            wt_masked = land_frac[s_isdata]
            [s_mean, s_std, s_min, s_max] = compute_stats(val_masked, wt_masked)
            VarStat[s_name] = {
                'N': len(val_masked),
                'units': s_unit,
                'mean': s_mean,
                'std': s_std,
                'min': s_min,
                'max': s_max
                }
            # _masked data
            mask_isdata_nnz = (s_isdata) & (stored_vars['mask_nnz'])
            sm_name = s_name + '_masked'
            sm_unit = s_unit
            val_masked = s_value[mask_isdata_nnz]
            wt_masked = land_frac[mask_isdata_nnz]
            [sm_mean, sm_std, sm_min, sm_max] = compute_stats(val_masked, wt_masked)
            VarStat[sm_name] = {
                'N': len(val_masked),
                'units': sm_unit,
                'mean': sm_mean,
                'std': sm_std,
                'min': sm_min,
                'max': sm_max
                }
            
    return VarStat



def write_qa(fid, filename, stats, header, footer, num_tiles_9km=None, num_tiles_36km=None):
    """
    # --------------------------------------------------------------------------
    # Write qa information to file outfile
    #
    # Input:
    #      fid              - file id of opened output qa file
    #      stats            - OrderedDict containing var info
    #      header           - header string
    #      footer           - footer string
    #      num_tiles_9km    - num of contributing grid cells (9km res)
    #      num_tiles_36km   - num of contributing grid cells (36km res)
    # --------------------------------------------------------------------------
    """
    if num_tiles_9km:
        fid.write('Quality Assessment for SMAP L4_SM Granule <%s>\n' % os.path.basename(filename))
        fid.write('Number of L4_SM EASEv2  9 km land grid cells =  %d\n' % num_tiles_9km)
    if num_tiles_36km:
        fid.write('Number of L4_SM EASEv2 36 km land grid cells =  %d\n' % num_tiles_36km)
    fid.write('\n')

    # header
    fid.write(header)

    # variable info
    for var in stats.keys():
        v_N = stats[var]['N']
        v_units = stats[var]['units']
        v_mean = stats[var]['mean']
        v_std = stats[var]['std']
        v_min = stats[var]['min']
        v_max = stats[var]['max']

        # write to stdout
        fmt = FORMAT_f
        if v_units=='kg m-2 s-1' or v_units=='kg kg-1': fmt = FORMAT_e
        if var.startswith('analysis_minus_forecast'): fmt = FORMAT_e

        fid.write(fmt % (var, '['+v_units+']', v_mean, v_std, v_min, v_max, v_N))

    # footer
    fid.write(footer)



def get_comm_line_args():
    # --------------------------------------------------------------------------
    # process command line args
    # options are
    #    landfrac - positional
    #    lmcfile  - optional
    #    gphfile  - optional
    #    aupfile  - optional
    # --------------------------------------------------------------------------

    # get exp dir from command line
    # -----------------------------
    p = argparse.ArgumentParser(
        description='Generate QA files from three types of '
        'HDF5 granules - gph, aup, lmc.'
        )
    # positional
    p.add_argument(
        'landfrac',
        help='location of lmc (land model constants) granule',
        )
    # optional
    p.add_argument(
        '--lmcfile',
        help='location of lmc (land model constants) granule',
        )
    p.add_argument(
        '--gphfile',
        help='location of gph (geophysical) granule',
        )
    p.add_argument(
        '--aupfile',
        help='location of aup (analysis update) granule',
        )

    return p.parse_args()



# ---------------
# headers/footers
# ---------------
col_header_fmt = '%-51s,%-16s,%13s,%13s,%13s,%13s,%9s\n'

# lmc
lmc_header = '='*134 + '\n' + 'Land model constants\n' + '='*134 + '\n'
lmc_col_headings = ('Fieldname', 'Units (*1)', 'Mean (*2)',
                    'Std-dev (*2)', 'Min', 'Max', 'N (*3)')
lmc_header += col_header_fmt % lmc_col_headings
lmc_header += '-'*134 + '\n'
lmc_footer = '-'*134 + '\n'
lmc_footer += """See SMAP L4_SM Data Products Specification Document for additional information.

(*1) Units are valid for all statistics except N [dimensionless].
(*2) Mean and std-dev statistics are weighted by the land fraction of each grid cell.
(*3) N is the number of 9 km EASEv2 grid cells that contribute to the statistics.
"""
# gph
gph_header = '='*134 + '\n' + 'Geophysical variables\n' + '='*134 + '\n'
gph_col_headings = ('Fieldname', 'Units (*1)', 'Mean (*2)',
                    'Std-dev (*2)', 'Min', 'Max', 'N (*3)')
gph_header += col_header_fmt % gph_col_headings
gph_header += '-'*134 + '\n'
gph_footer = lmc_footer
# aup
## auptb
auptb_header = '='*134 + '\n' + 'Brightness temperatures ("EnKF observation space")\n' + '='*134 + '\n'
auptb_col_headings = ('Fieldname (*1)', 'Units (*2)', 'Mean (*3)',
                      'Std-dev (*3)', 'Min', 'Max', 'N (*4)')
auptb_header += '%-51s,%-16s,%13s,%13s,%13s,%13s,%9s\n' % auptb_col_headings
auptb_header += '-'*134 + '\n'
auptb_footer = '-'*134 + '\n'
auptb_footer += """See SMAP L4_SM Data Products Specification Document for additional information.

(*1) Fieldnames that contain "_h" and "_v" are for H-polarization and V-polarization brightness temperature (Tb) data, respectively.
     "tb_h_obs" and "tb_v_obs" are observed Tbs obtained from SMAP L1C_TB and L2_SM_AP files after quality control based on
     information from the same files.
     "tb_h_obs_assim" and "tb_v_obs_assim" are observed Tbs that were assimilated in the L4_SM system after land model-based
     quality control and climatological adjustment (scaling).
     Statistics for "tb_h_forecast" and "tb_v_forecast" are masked to "tb_h_obs_assim" and "tb_v_obs_assim", respectively.
     Fieldnames that contain "_36km" or "_09km" provide statistics for Tbs from SMAP L1C_TB files or L2_SM_AP files, respectively, 
     and corresponding model Tbs.
     Fieldnames that contain "_A" or "_D" provide statistics that are masked to Tbs from ascending or descending orbits, respectively.
     Some observations at very high latitudes may have resulted from averaging over both ascending and descending orbits.
     Fieldnames that contain "norm_obs_assim_minus_forecast" provide statistics for normalized Tb innovations, defined as
     (tb_obs_assim - tb_forecast)/sqrt(tb_obs_errstd^2 + tb_forecast_ensstd^2).
(*2) Units are valid for all statistics except N [dimensionless].
(*3) Mean and std-dev statistics are weighted by the land fraction in each grid cell.
(*4) N is the number of EASEv2 grid cells that contribute to the statistics.
     For fieldnames containing "_36km" and "_09km", N is the number of contributing 36 km and 9 km EASEv2 grid cells, respectively.

"""
## aupgph
aupgph_header = '='*134 + '\n' + 'Geophysical variables ("EnKF state space")\n' + '='*134 + '\n'
aupgph_col_headings = ('Fieldname (*5)', 'Units (*6)', 'Mean (*7)',
                       'Std-dev (*7)', 'Min', 'Max', 'N (*8)')
aupgph_header += '%-51s,%-16s,%13s,%13s,%13s,%13s,%9s\n' % aupgph_col_headings
aupgph_header += '-'*134 + '\n'
aupgph_footer = '-'*134 + '\n'
aupgph_footer += """See SMAP L4_SM Data Products Specification Document for additional information.

(*5) For fieldnames ending in "_masked", statistics are masked to areas with non-zero increments, where zero is defined to be within
     +/-10e-5 [m3 m-3] for soil moisture variables and +/-10e-3 [K] for temperature variables.
     Fieldnames starting with "analysis_minus_forecast" provide statistics for analysis increments.
(*6) Units are valid for all statistics except N [dimensionless].
(*7) Mean and std-dev statistics are weighted by the land fraction of each grid cell.
(*8) N is the number of 9 km EASEv2 grid cells that contribute to the statistics.
"""



if __name__ == "__main__":
    main()
