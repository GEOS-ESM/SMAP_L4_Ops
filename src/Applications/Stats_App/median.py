#! /usr/bin/env python

import sys
import re
import datetime as dt

#-rw-r--r-- 1 dao_ops g0620 4835211 2017-02-27 15:11 M02/D25/SMAP_L1C_TB_11059_A_20170225T170613_R14010_002.h5

total = 0.0
latency = []

with open(sys.argv[1]) as f:

    lines = f.readlines()

    for line in lines:

        pdate=re.match(r'.*\.([0-9]{14})',line)
        gdate=re.match(r'.*_([0-9]{8}T[0-9]{6})',line)

        p = pdate.group(1)
        g = gdate.group(1)

        ptime = dt.datetime.strptime(p,"%Y%m%d%H%M%S")
        gtime = dt.datetime.strptime(g,"%Y%m%dT%H%M%S")

        td = ptime - gtime
        latency.append(td.total_seconds())

latency.sort()
N     = len(latency)
mid   = N / 2
total = latency[mid]

days   = total//86400.
total -= days*86400.
hours  = total//3600.
total -= hours*3600.
minutes = total//60.

print N, mid, days, hours, minutes
