#!/bin/sh

cat dtr.1

for orbit in 'A' 'D'; do

  i=1
  while [ $i -le 73 ]; do

    index=`expr $i + 100 | cut -c2-3`

    sed s/INDEX/$index/g dtr.2 | sed s/ORBIT/$orbit/g
    echo ""

    i=`expr $i + 1`

  done

done

exit 0
