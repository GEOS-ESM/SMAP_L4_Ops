#!/bin/sh

while [ 1 ]; do

  correctPrecip.pl /discover/nobackup/projects/gmao/smap/Operations/PrecipCorr/CPCUG5FPv1
  sleep 3600

done

exit 0
