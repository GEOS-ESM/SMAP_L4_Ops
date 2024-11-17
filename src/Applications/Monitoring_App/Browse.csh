#!/bin/csh

set bindir = `dirname $0`
source $bindir/modules

browse.py $*

exit 0
