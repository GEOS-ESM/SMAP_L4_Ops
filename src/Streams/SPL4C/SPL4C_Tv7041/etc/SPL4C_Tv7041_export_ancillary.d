#!/bin/sh

  bin_dir=`dirname $0`
  cd $bin_dir; bin_dir=`pwd`

  stream_dir=`dirname $bin_dir`
  stream_name=`basename $stream_dir`
  tmp=`dirname $stream_dir`
  stream_type=`basename $tmp`

  idate=$1
  edate=$2

  exec >/dev/null 2>&1

  EXPDIR=/datastage/smaplevel4/smapnsid/data_out/fp/PDR
  L4.pl -purge -st $stream_type -sn $stream_name -task export_ancillary

  while [ $idate -le $edate ]; do

  # dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MDL_LOG $idate
  # dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MDL_RIP $idate
  # dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MET_LOG $idate
  # dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MET_RIP $idate
  # dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MOD_LOG $idate
  # dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MOD_RIP $idate
    dtr.pl $stream_dir/export_ancillary/nsidc/SMAP_L4_C_ANC_MET $idate

    imon=`echo $idate | cut -c1-6`
    idate=`timetag $idate 0 %Y%m%d%+d01`

    numpdr=`/bin/ls $EXPDIR | wc -l`

    while [ $numpdr -gt 200 ]; do
      sleep 300
      L4.pl -purge -st $stream_type -sn $stream_name -task export_ancillary
      numpdr=`/bin/ls $EXPDIR | wc -l`
    done

  done

exit 0
