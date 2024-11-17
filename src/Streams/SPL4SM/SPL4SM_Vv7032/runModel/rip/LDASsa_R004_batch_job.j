#!/bin/csh -fx

#SBATCH --job-name=<L4_SM_STREAM_version>
#SBATCH --account=s1321
#SBATCH --time=01:00:00
#SBATCH --qos=daohi
#SBATCH --ntasks=120
#SBATCH --export=NONE
#SBATCH --constraint=sky
#SBATCH --output=<L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_log.%Y%m%d_0000z.txt
#SBATCH --error=<L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_out.%Y%m%d_0000z.txt

limit stacksize unlimited
setenv ARCH `uname`
setenv ESMADIR  <L4_SM_STREAM_directoryID>/bin/GEOSldas/install
setenv GEOSBIN  $ESMADIR/bin

unsetenv LD_LIBRARY_PATH
source $GEOSBIN/g5_modules

setenv MKL_CBWR "AUTO"
setenv I_MPI_DAPL_UD enable

setenv LD_LIBRARY_PATH ${BASEDIR}/${ARCH}/lib:${ESMADIR}/lib:${LD_LIBRARY_PATH}

if ( -e /etc/os-release ) then
module load nco/4.8.1
else
module load other/nco-4.6.8-gcc-5.3-sp3
endif

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

if( $?PBS_NODEFILE ) then
   sleep 10
   set nodes = `cat $PBS_NODEFILE | uniq`

   foreach node ($nodes)
     sshmpi $node $GEOSBIN/rmshmkeyhere.sh &
   end

   wait

endif   

$GEOSBIN/esma_mpirun -np 120 $GEOSBIN/GEOSldas.x

if( -e EGRESS.ldas ) then
   set rc = 0
else
   set rc = -1
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
