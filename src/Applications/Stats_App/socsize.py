#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import datetime as dt

file = 'soc_size.txt'

with open(file, 'r') as f:
    lines = f.readlines()

y = []
for line in lines:

    line = line.strip().split()
    y.append(float(line[0]))
    print(y)

x = range(0,957,1)

fig, ax = plt.subplots()
ax.plot(x, y, color='black', linewidth=1)

plt.title("SPL4C_Vv8020 SOC Filesize")
plt.ylabel('GB')
plt.xlabel('Time')

time_dt = dt.datetime(2015, 3, 31, 0)
end_dt = dt.datetime(2017, 11, 11, 0)

xl = range(0,957,1)
labels = []
pyear = 0
while time_dt <= end_dt:

    year = time_dt.year
    if year != pyear:
        labels.append("%04d"%(year,))
        pyear = year
    else:
        labels.append("")

    time_dt += dt.timedelta(days=1)

ax.set_xticks(xl)
ax.set_xticklabels(labels, fontsize=8)
ax.tick_params(axis='x', which='both',length=0)

plt.savefig('soc_size.png',dpi=150)
