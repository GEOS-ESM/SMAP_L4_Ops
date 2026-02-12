#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np

y = [3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,5.0,6.0,10.0,17.0,24.0]

print (len(y))

line = plt.plot(y, marker='o', markersize=3)
plt.setp(line, color='red', linewidth=1.5)

plt.title("SMAP L1C_TB Granule Arrival Latency")
plt.grid(False)
plt.axis([0, 28, 0, 30])
#plt.ylabel('')
plt.xlabel('Arrival Order')

x = range(0,29,1)
labels = ['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29']
plt.xticks(x, labels)

ax = plt.axes()
ax.set_xticklabels(labels, fontsize=8)

plt.savefig('test.png',dpi=150)
