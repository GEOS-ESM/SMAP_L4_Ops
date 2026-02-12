#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import datetime as dt

file = 'viirs_counts.txt'

with open(file, 'r') as f:
    lines = f.readlines()

y = []
for line in lines:

    line = line.strip().split()
    y.append(float(line[0]))
    print(y)

x = range(0,459,1)

fig, ax = plt.subplots()
ax.plot(x, y, color='black', linewidth=1)

plt.title("VIIRS FPAR Granule Counts")
plt.ylabel('N')
plt.xlabel('Time')

time_dt = dt.datetime(2015, 3, 30, 0)
end_dt  = dt.datetime(2025, 3, 14, 0)

xl = range(0,459,1)
labels = []
pyear = 0
while time_dt <= end_dt:

    t = time_dt
    year = time_dt.year
    if year != pyear:
        labels.append("%04d"%(year,))
        pyear = year
        time_dt = dt.datetime(t.year, 1, 1, 0)
    else:
        labels.append("")

    time_dt += dt.timedelta(days=8)

labels = labels[0:459]
ax.set_xticks(xl)
ax.set_xticklabels(labels, fontsize=8)
ax.tick_params(axis='x', which='both',length=0)

plt.savefig('viirs_counts.png',dpi=150)
