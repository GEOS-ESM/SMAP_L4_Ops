#!/bin/csh

set bindir = `dirname $0`

#module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
#module use -a /home/dao_ops/gmao_packages/modules
#module load other/postgreSQL
#module load gpy/sles12-v1.1.0
#setenv SEMPERPY_CONFIG $bindir/o2h/config:$SEMPERPY_CONFIG

#setenv MODULEPATH /home/dao_ops/gmao_packages/modules:$MODULEPATH
setenv MODULEPATH /discover/nobackup/dao_ops/jardizzo/software/SLES15/GMAOpy/modules:$MODULEPATH
module load gpy/sles15-py3.11.0
setenv SEMPERPY_CONFIG $bindir/o2h/config:$SEMPERPY_CONFIG
echo SEMPERPY_CONFIG = $SEMPERPY_CONFIG

set def_file = $bindir/o2h/smap.def
set html_path = $bindir/o2h/html

$bindir/o2h/o2h.py $* -f $def_file -t $html_path

exit 0
