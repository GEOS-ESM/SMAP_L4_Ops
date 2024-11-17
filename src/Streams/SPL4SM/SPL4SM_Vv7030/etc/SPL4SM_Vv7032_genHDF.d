#!/bin/sh

  bin_dir=`dirname $0`
  cd $bin_dir; bin_dir=`pwd`

  stream_dir=`dirname $bin_dir`
  stream_name=`basename $stream_dir`
  stream_dir=`dirname $stream_dir`
  stream_type=`basename $stream_dir`

  date=20150331
  edate=20150413

  exec >/dev/null 2>&1

  while [ $date -le $edate ]; do

      L4.pl -run genISO -st $stream_type -sn $stream_name -date $date

      date=`timetag $date 0 %Y%m%d%+d01`

  done

exit 0
