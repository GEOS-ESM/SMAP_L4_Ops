#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np

y2 = [2.68472222222,2.68819444444,2.68958333333,3.92430555556,2.98611111111,2.64236111111,2.68333333333,2.72708333333,2.72638888889,2.76666666667,3.87638888889,2.93680555556,2.67916666667,2.67638888889,2.67361111111,2.67777777778,4.59513888889,3.65555555556,2.75833333333,2.80069444444,2.78958333333]

y = [2.70020833333,2.69762731481,2.68265046296,3.93075231481,2.98872685185,2.646875,2.72037037037,2.71013888889,2.69140046296,2.75922453704,3.88211805556,2.94115740741,2.67525462963,2.70601851852,2.69383101852,2.6827662037,2.71331018519,2.70277777778,2.69199074074,2.80625,2.79188657407]

y = np.array(y)
y2 = np.array(y2)
y  = np.ma.masked_where(y < 0.0, y)
y2 = np.ma.masked_where(y2 < 0.0, y2)

y[y < 0.0] = np.nan
y2[y2 < 0.0] = np.nan

line = plt.plot(y)
plt.setp(line, color='red', linewidth=1.5)

line = plt.plot(y2)
plt.setp(line, color='black', linewidth=1.5)

plt.title("SMAP L4_SM Latency")
plt.grid(True)
plt.axis([0, 20, 0, 10])
plt.ylabel('Days')
plt.xlabel('August 2023')

xl = range(0,21,1)
labels = []
for i in xl:
    j = i + 1
    if j > 30: j = j - 30
    labels.append("%02d"%(j,))
plt.xticks(xl, labels)

x = range(0,21,1)
ax = plt.axes()
ax.fill_between(x, y, y2, color=(0.9, 0.9, 0.9))
ax.set_xticklabels(labels, fontsize=8)

plt.savefig('L4_SM.png',dpi=150)
