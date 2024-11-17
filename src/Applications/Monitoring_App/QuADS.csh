#!/bin/csh

#source /gpfsm/dnb31/ebsmith2/dev/browse/v1.1/modules

set bindir = `dirname $0`
source $bindir/modules

set layers = "$bindir/etc/layers.temp"

if ( ! -f layers.temp ) then
  cp -p $layers .
endif

quads.py $*

exit 0
