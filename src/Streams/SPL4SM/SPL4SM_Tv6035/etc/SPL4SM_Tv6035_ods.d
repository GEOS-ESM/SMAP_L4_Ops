#!/bin/sh

  bin_dir=`dirname $0`
  cd $bin_dir; bin_dir=`pwd`

  stream_dir=`dirname $bin_dir`
  stream_name=`basename $stream_dir`
  stream_dir=`dirname $stream_dir`
  stream_type=`basename $stream_dir`

  odsdir=/discover/nobackup/projects/gmao/smap/SMAP_L4/L4_SM/Tv6035/obs

  date=20201113

  exec >/dev/null 2>&1

  while [ 1 ]; do

    clock=`L4.pl -list -st $stream_type -sn $stream_name | grep '^Clock:'`
    clock_date=`echo $clock | cut -d' ' -f3 | cut -d',' -f1`

    if [ $date -lt $clock_date ]; then

      output="$odsdir/Y%Y/M%m/D%d"

      L4.pl -ods -st $stream_type -sn $stream_name -date $date -pc 1 -o $output
      L4.pl -o2h -st $stream_type -sn $stream_name -date $date -pc 1 -auto

      date=`timetag $date 0 %Y%m%d%+d01`

    else

      sleep 300

    fi

  done

exit 0
