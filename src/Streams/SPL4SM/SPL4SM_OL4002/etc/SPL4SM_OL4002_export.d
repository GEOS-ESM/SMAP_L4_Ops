#!/bin/sh

  bin_dir=`dirname $0`
  cd $bin_dir; bin_dir=`pwd`

  stream_dir=`dirname $bin_dir`
  stream_name=`basename $stream_dir`
  stream_dir=`dirname $stream_dir`
  stream_type=`basename $stream_dir`

  idate=$1
  edate=$2

  exec >/dev/null 2>&1

  while [ $idate -le $edate ]; do

    L4.pl -export -st $stream_type -sn $stream_name -task $3 -date $idate
    sleep 300

    L4.pl -purge -st $stream_type -sn $stream_name -task $3

    idate=`timetag $idate 0 %Y%m%d%+d01`

  done

exit 0
