#!/bin/csh -fx
#=============
#PBS -N SPL4SM_V01000
#PBS -S /bin/csh
#PBS -W group_list=s1321
#PBS -l select=8:ncpus=16:mpiprocs=16:proc=sand
#PBS -o /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/output/SMAP_EASEv2_M09_GLOBAL/rc_out/Y2001/M07/SPL4SM_V01000.ldas_out.20010725_0000z.txt
#PBS -j oe
#PBS -q general
#PBS -l walltime=12:00:00
#=============

cd /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/output

#==============================================================
#=== Memory:                                                ===
limit vmemoryuse 18874368
limit  memoryuse 18874368
limit stacksize  unlimited
#==============================================================
#=== Intel 13:                                              ===
#=== ensure zero-diff across SandyBridge and Westmere nodes ===
setenv MKL_CBWR SSE4_2
#=== to speed up I/O do NOT use                             ===
#          setenv FORT_BUFFERED true
#=== this is now done via a compiler flag                   ===
#==============================================================
#=== MVAPICH2:                                              ===
setenv MV2_ON_DEMAND_THRESHOLD 8192
#==============================================================
source /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/bin/g5_modules

mpirun -np 128 /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/bin/LDASsa_assim_mpi.x \
-restart .true. \
-spin .false. \
-start_year 2001 \
-start_month 7 \
-start_day 25 \
-start_hour 0 \
-start_min 0 \
-start_sec 0 \
-end_year  2001 \
-end_month  7 \
-end_day 26 \
-end_hour 0 \
-end_min  0 \
-end_sec 0 \
-resolution SMAP_EASEv2_M09 \
-exp_domain SMAP_EASEv2_M09_GLOBAL \
-exp_id SPL4SM_V01000 \
-work_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/output \
-run_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/run \
-restart_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/output \
-restart_domain SMAP_EASEv2_M09_GLOBAL \
-restart_id SPL4SM_V01000 \
-tile_coord_file SMAP_EASEv2_M09_3856x1624.til \
-met_tag d591_rpit1_jan00__precCPCUG5RPFPITv1 \
-met_path /gpfsm/dnb33/jardizzo/SMAP_L4/SPL4SM_V01000/import/met_forcing/GEOS5_land_forcing/lfo_tavg/.. \
-force_dtstep 3600 \
-N_ens 24 \
-first_ens_id 0 \
-driver_inputs_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/run \
-driver_inputs_file LDASsa_V01_inputs_driver.nml \
-ens_prop_inputs_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/run \
-ens_prop_inputs_file LDASsa_V01_inputs_ensprop.nml \
-ens_upd_inputs_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/run \
-ens_upd_inputs_file LDASsa_V01_inputs_ensupd.nml \
-cat_bias_inputs_path /discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/run \
-cat_bias_inputs_file LDASsa_V01_inputs_catbias.nml > \
/discover/nobackup/jardizzo/SMAP_L4/SPL4SM_V01000/runModel/output/SMAP_EASEv2_M09_GLOBAL/rc_out/Y2001/M07/SPL4SM_V01000.ldas_log.20010725_0000z.txt
