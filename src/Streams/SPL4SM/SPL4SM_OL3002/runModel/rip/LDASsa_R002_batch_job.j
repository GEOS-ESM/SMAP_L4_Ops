#!/bin/csh -fx

#SBATCH --job-name=<L4_SM_STREAM_version>
#SBATCH --account=s1321
#SBATCH --time=2:00:00
#SBATCH --qos=daohi
#SBATCH --ntasks=112
#SBATCH --export=NONE
#SBATCH --constraint=hasw
#SBATCH --output=<L4_SM_STREAM_directoryID>/runModel/output/SMAP_EASEv2_M09_GLOBAL/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_log.%Y%m%d_0000z.txt
#SBATCH --error=<L4_SM_STREAM_directoryID>/runModel/output/SMAP_EASEv2_M09_GLOBAL/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_out.%Y%m%d_0000z.txt

limit stacksize unlimited
source <L4_SM_STREAM_directoryID>/bin/g5_modules
setenv MKL_CBWR SSE4_2 # ensure zero-diff across archs
setenv MV2_ON_DEMAND_THRESHOLD 8192 # MVAPICH2

mpirun -np 112 <L4_SM_STREAM_directoryID>/bin/LDASsa_assim_mpi.x \
-restart .true. \
-spin .false. \
-start_year <START_YEAR> \
-start_month <START_MONTH> \
-start_day <START_DAY> \
-start_hour <START_HOUR> \
-start_min <START_MIN> \
-start_sec <START_SEC> \
-end_year  <END_YEAR> \
-end_month  <END_MONTH> \
-end_day <END_DAY> \
-end_hour <END_HOUR> \
-end_min  <END_MIN> \
-end_sec <END_SEC> \
-resolution SMAP_EASEv2_M09 \
-exp_domain SMAP_EASEv2_M09_GLOBAL \
-exp_id <L4_SM_STREAM_version> \
-work_path <L4_SM_STREAM_directoryID>/runModel/output \
-run_path <L4_SM_STREAM_directoryID>/runModel/run \
-restart_path <L4_SM_STREAM_directoryID>/runModel/output \
-restart_domain SMAP_EASEv2_M09_GLOBAL \
-restart_id <L4_SM_STREAM_version> \
-tile_coord_file SMAP_EASEv2_M09_3856x1624.til \
-met_tag <L4_SM_MET_TAG> \
-met_path <L4_SM_MET_PATH> \
-force_dtstep 3600 \
-N_ens 24 \
-first_ens_id 0 \
-driver_inputs_path <L4_SM_STREAM_directoryID>/runModel/run \
-driver_inputs_file LDASsa_<L4_SM_DRIVER_version>_inputs_driver.nml \
-ens_prop_inputs_path <L4_SM_STREAM_directoryID>/runModel/run \
-ens_prop_inputs_file LDASsa_<L4_SM_ENSPROP_version>_inputs_ensprop.nml \
-ens_upd_inputs_path <L4_SM_STREAM_directoryID>/runModel/run \
-ens_upd_inputs_file LDASsa_<L4_SM_ENSUPD_version>_inputs_ensupd.nml \
-cat_bias_inputs_path <L4_SM_STREAM_directoryID>/runModel/run \
-cat_bias_inputs_file LDASsa_<L4_SM_CATBIAS_version>_inputs_catbias.nml

