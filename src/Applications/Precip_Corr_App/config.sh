#************************************
# Correct GEOS-5.7.2 Using Daily CPCU
#************************************

export applicationGroup=GEOS-5_7_2-CPCU

export namelist=GEOS-5_7_2-CPCU.namelist
export LFOinput=/discover/nobackup/projects/gmao/smap/SMAP_L4/GEOS-5/GEOSadas-5_7_2_p5_m1/e572p5_fp/diag/Y%y4/M%m2/D%d2/e572p5_fp.tavg1_2d_lfo_Nx.%y4%m2%d2_%h230z.nc4
export LFOoutput=/discover/nobackup/jardizzo/CPCU_test/e572p5_fp.tavg1_2d_lfo_Nx_corr.%y4%m2%d2_%h230z_4.nc4
export CPCUinput=/discover/nobackup/projects/gmao/smap/SMAP_L4/CPCU/v1.0/Y%y4/M%m2/PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.%y4%m2%d2.RT
export EODinput=/discover/nobackup/projects/gmao/smap/SMAP_L4/CPCU/v1.0/PRCP_CU_GAUGE_V1.0GLB_0.50deg_EOD.lnx

export LFOFILEFORMAT=SDF
runApp CPCUCorrectGEOS.x $namelist $sdate $edate $LFOinput $LFOoutput $CPCUinput $EODinput

#************************************
# Correct GEOS-5.7.2 Using Daily CPCU
#************************************

export applicationGroup=GEOSFP-CPCU

export namelist=$INSTALL_PATH/rip/GEOSFP-CPCU.namelist
export LFOinput=/gpfsm/dnb02/projects/p51/SMAP_L4/GEOS-5/FP/diag/Y%y4/M%m2/D%d2/GEOS.fp.asm.tavg1_2d_lfo_Nx.%y4%m2%d2_%h230.V01.nc4
export LFO_INST_FILENAME_TEMPLATE=/gpfsm/dnb02/projects/p51/SMAP_L4/GEOS-5/FP/diag/Y%y4/M%m2/D%d2/GEOS.fp.asm.inst1_2d_lfo_Nx.%y4%m2%d2_%h200.V01.nc4
export LFOoutput=/discover/nobackup/jardizzo/CPCU_test/GEOS.fp.asm.tavg1_2d_lfo_Nx_corr.%y4%m2%d2_%h230z.nc4
export CPCUinput=/gpfsm/dnb02/projects/p51/SMAP_L4/CPCU/v1.0/Y%y4/M%m2/PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.%y4%m2%d2.RT
export EODinput=/gpfsm/dnb02/projects/p51/SMAP_L4/CPCU/v1.0/PRCP_CU_GAUGE_V1.0GLB_0.50deg_EOD.lnx
export CPCU_MASK_FILENAME=/gpfsm/dnb02/projects/p51/SMAP_L4/CPCU/v1.0/Africa_G5FP_mask.dat

#export LFOFILEFORMAT=GRADS
export LFOFILEFORMAT=SDF
runApp CPCUCorrectGEOS.x $namelist $sdate $edate $LFOinput $LFOoutput $CPCUinput $EODinput
