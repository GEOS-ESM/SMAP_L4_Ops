#!/bin/csh -fx

#SBATCH --job-name=<L4_SM_STREAM_version>
#SBATCH --account=s1321
#SBATCH --time=00:30:00
#SBATCH --qos=daohi
#SBATCH --nodes=2 --ntasks-per-node=126
#SBATCH --constraint=mil
#SBATCH --output=<L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_log.%Y%m%d_0000z.txt
#SBATCH --error=<L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_out.%Y%m%d_0000z.txt

limit stacksize unlimited
setenv ARCH `uname`
#setenv ESMADIR  <L4_SM_STREAM_directoryID>/bin/GEOSldas/install
setenv ESMADIR  /discover/nobackup/dao_ops/jardizzo/SMAP/software/GEOSldas/install
#setenv ESMADIR  /home/qliu/smap/SMAP_Nature/SMAP_Nature_v11/OLv8_spin/build
#setenv ESMADIR  /discover/swdev/mathomp4/Models/GEOSldas-v18.0.3-IntelMPI/install-Aggressive-SLES15
setenv GEOSBIN  $ESMADIR/bin

unsetenv LD_LIBRARY_PATH
source $GEOSBIN/g5_modules

# OPENMPI flags
# Turn off warning about TMPDIR on NFS
setenv OMPI_MCA_shmem_mmap_enable_nfs_warning 0
# pre-connect MPI procs on mpi_init
setenv OMPI_MCA_mpi_preconnect_all 1
setenv OMPI_MCA_coll_tuned_bcast_algorithm 7
setenv OMPI_MCA_coll_tuned_scatter_algorithm 2
setenv OMPI_MCA_coll_tuned_reduce_scatter_algorithm 3
setenv OMPI_MCA_coll_tuned_allreduce_algorithm 3
setenv OMPI_MCA_coll_tuned_allgather_algorithm 4
setenv OMPI_MCA_coll_tuned_allgatherv_algorithm 3
setenv OMPI_MCA_coll_tuned_gather_algorithm 1
setenv OMPI_MCA_coll_tuned_barrier_algorithm 0
# required for a tuned flag to be effective
setenv OMPI_MCA_coll_tuned_use_dynamic_rules 1
# disable file locks
setenv OMPI_MCA_sharedfp "^lockedfile,individual"

# By default, ensure 0-diff across processor architecture by limiting MKL's freedom to pick algorithms.
# As of June 2021, MKL_CBWR=AVX2 is fastest setting that works for both haswell and skylake at NCCS.
# Change to MKL_CBWR=AUTO for fastest execution at the expense of results becoming processor-dependent.
#setenv MKL_CBWR "COMPATIBLE"
#setenv MKL_CBWR "AUTO"
setenv MKL_CBWR "AVX2"

setenv LD_LIBRARY_PATH ${BASEDIR}/${ARCH}/lib:${ESMADIR}/lib:${LD_LIBRARY_PATH}

module load nco

setenv RUN_CMD "$GEOSBIN/esma_mpirun -np "

# Initialize rc_out

mkdir -p <L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m
cd <L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m

rm <L4_SM_STREAM_version>.ldas_catbias_inputs.%Y%m%d_0000z.nml
rm <L4_SM_STREAM_version>.ldas_catparam.%Y%m%d_0000z.bin
rm <L4_SM_STREAM_version>.ldas_domdecomp.%Y%m%d_0000z.txt
rm <L4_SM_STREAM_version>.ldas_ensprop_inputs.%Y%m%d_0000z.nml
rm <L4_SM_STREAM_version>.ldas_ensupd_inputs.%Y%m%d_0000z.nml
rm <L4_SM_STREAM_version>.ldas_mwRTMparam.%Y%m%d_0000z.nc4
rm <L4_SM_STREAM_version>.ldas_obslog.%Y%m%d_0000z.txt
rm <L4_SM_STREAM_version>.ldas_LDAS_rc.%Y%m%d_0000z.txt
rm <L4_SM_STREAM_version>.ldas_CAP_rc.%Y%m%d_0000z.txt
rm <L4_SM_STREAM_version>.ldas_obsparam.%Y%m%d_0000z.txt
rm <L4_SM_STREAM_version>.ldas_smapL4SMlmc.%Y%m%d_0000z.bin

cat /dev/null > <L4_SM_STREAM_version>.ldas_log.%Y%m%d_0000z.txt
cat /dev/null > <L4_SM_STREAM_version>.ldas_out.%Y%m%d_0000z.txt

cp -p <L4_SM_MODEL_OUTPUT_DIR>/rc_out/<L4_SM_STREAM_version>.ldas_catparam.bin <L4_SM_STREAM_version>.ldas_catparam.%Y%m%d_0000z.bin

cp -p <L4_SM_BCS_RTM_OUT_DIR>/mwRTM_param.nc4 <L4_SM_STREAM_version>.ldas_mwRTMparam.%Y%m%d_0000z.nc4

cp -p <L4_SM_BCS_RTM_OUT_DIR>/vegopacity.bin <L4_SM_STREAM_version>.ldas_vegopacity.%Y%m%d_0000z.bin

# Set up GEOSldas run directory

cd <L4_SM_STREAM_directoryID>/runModel/run
mkdir -p ../data
rm -f ../data/*

<L4_SM_STREAM_directoryID>/bin/link_ldas.py LDASin.rc LDAS.rc ../data

# Initialize rc_out directory

cp -p LDAS.rc <L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_LDAS_rc.%Y%m%d_0000z.txt
cp -p CAP.rc <L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_CAP_rc.%Y%m%d_0000z.txt

mv LDASsa_<L4_SM_ENSPROP_version>_inputs_ensprop.nml LDASsa_SPECIAL_inputs_ensprop.nml
mv LDASsa_<L4_SM_ENSUPD_version>_inputs_ensupd.nml LDASsa_SPECIAL_inputs_ensupd.nml
mv LDASsa_<L4_SM_CATBIAS_version>_inputs_catbias.nml LDASsa_SPECIAL_inputs_catbias.nml

# Execute GEOSldas

$GEOSBIN/RmShmKeys_sshmpi.csh

set npes_nx = `grep NX: LDAS.rc | cut -d':' -f2 `
set npes_ny = `grep NY: LDAS.rc | cut -d':' -f2 `
@ numprocs = $npes_nx * $npes_ny

@ oserver_nodes = 0
@ writers = 0

if (! $?SLURM_NTASKS) then
  set total_npes = `wc -l $PBS_NODEFILE | awk '{print $1}'` 
else
  set total_npes = $SLURM_NTASKS
endif

if ($oserver_nodes == 0) then
  set oserver_options = ""
else
  set oserver_options = "--oserver_type multigroup --nodes_output_server $oserver_nodes  --npes_backend_pernode $writers"
endif

$RUN_CMD $total_npes $GEOSBIN/GEOSldas.x --npes_model $numprocs $oserver_options 
if( -e EGRESS.ldas ) then
   set rc = 0
   echo GEOSldas Run Status: $rc
else
   set rc = -1
   echo GEOSldas Run Status: $rc
   echo "ERROR: GEOSldas run FAILED, exit without post-processing"
   exit
endif

echo GEOSldas Run Status: $rc

# Move GEOSldas local output to stream directory.

<L4_SM_STREAM_directoryID>/bin/link_ldas.py LDASin.rc LDAS.rc ../data

mkdir -p <L4_SM_MODEL_OUTPUT_DIR>/cat/ens_avg/Y%Y/M%m

foreach t ( 0130 0430 0730 1030 1330 1630 1930 2230 )

  mv CURRENT.SMAP_L4_SM_gph.%Y%m%d_${t}z.bin <L4_SM_MODEL_OUTPUT_DIR>/cat/ens_avg/Y%Y/M%m/<L4_SM_STREAM_version>.ens_avg.ldas_tile_xhourly_out.%Y%m%d_${t}z.bin

end

mkdir -p <L4_SM_MODEL_OUTPUT_DIR>/ana/ens_avg/Y%Y/M%m

foreach t ( 0300 0600 0900 1200 1500 1800 2100 )

  mv <L4_SM_STREAM_version>.ens_avg.ldas_ObsFcstAna.%Y%m%d_${t}z.bin <L4_SM_MODEL_OUTPUT_DIR>/ana/ens_avg/Y%Y/M%m/<L4_SM_STREAM_version>.ens_avg.ldas_ObsFcstAna.%Y%m%d_${t}z.bin

  mv <L4_SM_STREAM_version>.ens_avg.ldas_tile_inst_smapL4SMaup.%Y%m%d_${t}z.bin <L4_SM_MODEL_OUTPUT_DIR>/ana/ens_avg/Y%Y/M%m/<L4_SM_STREAM_version>.ens_avg.ldas_tile_inst_smapL4SMaup.%Y%m%d_${t}z.bin

end

mkdir -p <L4_SM_MODEL_OUTPUT_DIR>/ana/ens_avg/Y%%Y/M%%m

mv <L4_SM_STREAM_version>.ens_avg.ldas_ObsFcstAna.%%Y%%m%%d_0000z.bin <L4_SM_MODEL_OUTPUT_DIR>/ana/ens_avg/Y%%Y/M%%m/<L4_SM_STREAM_version>.ens_avg.ldas_ObsFcstAna.%%Y%%m%%d_0000z.bin

mv <L4_SM_STREAM_version>.ens_avg.ldas_tile_inst_smapL4SMaup.%%Y%%m%%d_0000z.bin <L4_SM_MODEL_OUTPUT_DIR>/ana/ens_avg/Y%%Y/M%%m/<L4_SM_STREAM_version>.ens_avg.ldas_tile_inst_smapL4SMaup.%%Y%%m%%d_0000z.bin
