#!/bin/sh

i=13
while [ $i -le 22 ]; do

  XXXX=`expr $i + 10000 | cut -c2-5`

  sed s/XXXX/$XXXX/g restart.tmpl > SMAP_L4_SM_ANC_RST.dtr
  echo "" >> SMAP_L4_SM_ANC_RST.dtr

  dtr.pl `pwd` 20150331

  i=`expr $i + 1`

done

exit 0
