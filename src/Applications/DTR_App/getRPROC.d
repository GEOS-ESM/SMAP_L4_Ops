#!/bin/sh

  exec >/dev/null 2>&1

  while [ 1 ]; do

    dtr.pl /discover/nobackup/projects/gmao/smap/Operations/SMAP/RPROC/acquire
    sleep 3600

  done

exit 0
