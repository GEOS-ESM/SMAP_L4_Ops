#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import datetime as dt

file = '/discover/nobackup/projects/gmao/smap/SMAP_L4/SMAP/OPS/L1C_TB/listing_counts'

with open(file, 'r') as f:
    lines = f.readlines()

y = []
y2 = []
for line in lines:

    line = line.strip().split()
    y.append(float(line[0]))
    y2.append(float(line[1]))

y = np.array(y)
y2 = np.array(y2)
#y  = np.ma.masked_where(y < 0.0, y)
#y2 = np.ma.masked_where(y2 < 0.0, y2)

#y[y < 0.0] = np.nan
#y2[y2 < 0.0] = np.nan

time_dt = dt.datetime(2015, 3, 31, 0)
end_dt = dt.datetime(2024, 7, 31, 0)

xl = range(0,3411,1)
xlab = []
labels = []
pyear = 0
i = 0 
while time_dt <= end_dt:

    year = time_dt.year
    if year != pyear:
        labels.append("%04d"%(year,))
        xlab.append(i)
        pyear = year

    i += 1
    time_dt += dt.timedelta(days=1)

#fig = plt.figure(figsize = (10, 5))

# creating the bar plot
plt.bar(xl, y2-y, color ='red') 

plt.xlabel("Time")
plt.axis([0, 3410, -30, 30])
plt.ylabel("N-diff")
plt.title("R19 Versus RXX Counts")
plt.xticks(xlab, labels)
#plt.show()
plt.savefig('granule_counts_bar.png',dpi=150)
