#!/bin/sh

exec >/dev/null 2>&1

while [ 1 ]; do

  correctPrecip.pl /discover/nobackup/projects/gmao/smap/Operations/PrecipCorr/CPCUG5FPv3
  sleep 3600

done

exit 0
