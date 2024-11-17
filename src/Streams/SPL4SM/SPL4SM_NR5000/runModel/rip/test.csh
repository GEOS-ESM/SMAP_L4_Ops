#!/bin/csh

foreach i (`seq 1 1 23`)
  set t = `expr $i + 100 | cut -c2,3`00
  echo $t
end
