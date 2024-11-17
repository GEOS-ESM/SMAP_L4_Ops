#!/bin/sh

i=0
while [ $i -le 23 ]; do

  XXXX=`expr $i + 10000 | cut -c2-5`

  sed s/XXXX/$XXXX/g restart.tmpl
  echo ""

  i=`expr $i + 1`

done

exit 0
