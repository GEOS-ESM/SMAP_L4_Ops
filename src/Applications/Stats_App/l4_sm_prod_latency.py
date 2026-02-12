#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import datetime as dt

fname = '2023-2024.stats.txt'
with open(fname, 'r') as f:
    stats = f.readlines()

for i,v in enumerate(stats):
    v = float(v.strip())
    stats[i] = v

y = np.array(stats)
y  = np.ma.masked_where(y < 0.0, y)
y[y < 0.0] = np.nan

line = plt.plot(y)
plt.setp(line, color='black', linewidth=1)

plt.title("SMAP L4_SM Production Latency")
plt.grid(True)
plt.axis([0, len(stats)-1, 0, 10])
plt.ylabel('Days')
plt.xlabel('Day')

time_dt = dt.datetime(2023, 1, 1)
inc_day = dt.timedelta(hours=24)

xl = range(0,len(stats),1)
labels = []
xticks = []

for i in xl:
    year = time_dt.strftime("%Y")
    month = time_dt.strftime("%b")
    day = time_dt.strftime("%d")
    if day == '01' and month == 'Jan':
        labels.append("{}\n{}".format(month, year))
        xticks.append(i)
    elif day == '01':
        labels.append("{}".format(month))
        xticks.append(i)
    time_dt += inc_day
plt.xticks(xticks, labels)

ax = plt.axes()
ax.set_xticklabels(labels, fontsize=8)

plt.savefig('L4_SM_prod.png',dpi=150)
