#!/bin/sh

  bin_dir=`dirname $0`
  cd $bin_dir; bin_dir=`pwd`

  stream_dir=`dirname $bin_dir`
  stream_name=`basename $stream_dir`
  stream_dir=`dirname $stream_dir`
  stream_type=`basename $stream_dir`

  clock=`L4.pl -list -st $stream_type -sn $stream_name | grep '^Clock:'`
  last_date=`echo $clock | cut -d' ' -f3 | cut -d',' -f1`

  exec >/dev/null 2>&1

  while [ 1 ]; do

    L4.pl -run -st $stream_type -sn $stream_name

    clock=`L4.pl -list -st $stream_type -sn $stream_name | grep '^Clock:'`
    date=`echo $clock | cut -d' ' -f3 | cut -d',' -f1`

    if [ $date -eq $last_date ]; then
      sleep 3600
    else
      last_date=$date
    fi

  done

exit 0
