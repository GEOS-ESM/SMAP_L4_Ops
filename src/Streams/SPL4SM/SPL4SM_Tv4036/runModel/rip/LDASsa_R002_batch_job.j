#!/bin/csh -fx

#SBATCH --job-name=<L4_SM_STREAM_version>
#SBATCH --account=s1321
#SBATCH --time=2:00:00
#SBATCH --qos=daohi
#SBATCH --ntasks=112
#SBATCH --export=NONE
#SBATCH --constraint=hasw
#SBATCH --output=<L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_log.%Y%m%d_0000z.txt
#SBATCH --error=<L4_SM_MODEL_OUTPUT_DIR>/rc_out/Y%Y/M%m/<L4_SM_STREAM_version>.ldas_out.%Y%m%d_0000z.txt

limit stacksize unlimited
source <L4_SM_STREAM_directoryID>/bin/g5_modules
setenv OMPI_MCA_shmem_mmap_enable_nfs_warning 0

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
rm <L4_SM_STREAM_version>.ldas_obsparam.%Y%m%d_0000z.txt
rm <L4_SM_STREAM_version>.ldas_smapL4SMlmc.%Y%m%d_0000z.bin

cp -p <L4_SM_MODEL_OUTPUT_DIR>/rc_out/<L4_SM_STREAM_version>.ldas_catparam.bin <L4_SM_STREAM_version>.ldas_catparam.%Y%m%d_0000z.bin
cp -p <L4_SM_MODEL_OUTPUT_DIR>/rc_out/<L4_SM_STREAM_version>.ldas_mwRTMparam.nc4 <L4_SM_STREAM_version>.ldas_mwRTMparam.%Y%m%d_0000z.nc4

# Set up GEOSldas run directory

cd <L4_SM_STREAM_directoryID>/runModel/run
mkdir -p ../data
rm -f ../data/*

<L4_SM_STREAM_directoryID>/bin/link_ldas.py LDASin.rc LDAS.rc ../data

mv LDASsa_<L4_SM_ENSPROP_version>_inputs_ensprop.nml LDASsa_SPECIAL_inputs_ensprop.nml
mv LDASsa_<L4_SM_ENSUPD_version>_inputs_ensupd.nml LDASsa_SPECIAL_inputs_ensupd.nml
mv LDASsa_<L4_SM_CATBIAS_version>_inputs_catbias.nml LDASsa_SPECIAL_inputs_catbias.nml

# Execute GEOSldas

mpirun -map-by core --mca btl ^vader -np 112 <L4_SM_STREAM_directoryID>/bin/GEOSldas.x

if( -e EGRESS.ldas ) then
   set rc = 0
else
   set rc = -1
endif

echo GEOSldas Run Status: $rc

# Move GEOSldas local output to stream directory.

mkdir -p <L4_SM_MODEL_OUTPUT_DIR>/cat/ens_avg/Y%Y/M%m

foreach t ( 0130 0430 0730 1030 1330 1630 1930 2230 )

  cp -p CURRENT.SMAP_L4_SM_gph.%Y%m%d_${t}z.bin <L4_SM_MODEL_OUTPUT_DIR>/cat/ens_avg/Y%Y/M%m/<L4_SM_STREAM_version>.ens_avg.ldas_tile_xhourly_out.%Y%m%d_${t}z.bin

#mkdir -p <L4_SM_RST_DIR>/Y%%Y/M%%m

#cp -p <L4_SM_STREAM_version>.landassim_rseeds_checkpoint.%%Y%%m%%d_0000z.nc4 <L4_SM_RST_DIR>/Y%%Y/M%%m/<L4_SM_STREAM_version>.obspert_ldas_rst.%%Y%%m%%d_0000z.nc4

end
