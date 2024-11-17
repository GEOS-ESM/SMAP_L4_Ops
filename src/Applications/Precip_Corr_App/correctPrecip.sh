#!/bin/sh

  if [ $# -ne 3 ]; then
    echo "Usage: $0 [runGroup] [sccyymmdd] [eccyymmdd]" >& 2
    exit 1
  fi

  cwd=`pwd`
  BIN_PATH=`dirname $0`
  INSTALL_PATH=`dirname $BIN_PATH`
  cd $INSTALL_PATH

  export PATH=$BIN_PATH:$PATH
  export INSTALL_PATH=`pwd`

  cd $cwd

  runGroup=$1
  sdate=$2
  edate=$3

  export runGroup=$runGroup

  . $BIN_PATH/config.sh

exit 0
