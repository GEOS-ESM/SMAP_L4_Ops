#!/bin/sh

  bin_dir=`dirname $0`
  cd $bin_dir; bin_dir=`pwd`

  stream_dir=`dirname $bin_dir`
  stream_name=`basename $stream_dir`
  stream_dir=`dirname $stream_dir`
  stream_type=`basename $stream_dir`

  date=20150331

  exec >/dev/null 2>&1

  while [ 1 ]; do

    clock=`L4.pl -list -st $stream_type -sn $stream_name | grep '^Clock:'`
    clock_date=`echo $clock | cut -d' ' -f3 | cut -d',' -f1`

    if [ $date -lt $clock_date ]; then

      L4.pl -run genISO -st $stream_type -sn $stream_name -date $date
      L4.pl -browse -st $stream_type -sn $stream_name -date $date -auto
      L4.pl -quads -st $stream_type -sn $stream_name -date $date -auto

      date=`timetag $date 0 %Y%m%d%+d01`

    else

      sleep 300

    fi

  done

exit 0
