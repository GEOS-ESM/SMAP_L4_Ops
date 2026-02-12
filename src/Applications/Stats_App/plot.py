#! /usr/bin/env python

import matplotlib.pyplot as plt

soilm = [ 2.95277777778, 2.80486111111, 2.80625, 2.82222222222,
         2.73888888889, 2.78541666667, 2.87013888889 ]

carbon = [ 8.16111111111, 10.7770833333, 8.73055555556, 8.09722222222,
           8.84375, 10.1097222222, 9.16736111111 ]

line = plt.plot(soilm)
plt.setp(line, color='b', linewidth=2.0)
line = plt.plot(carbon)
plt.setp(line, color='g', linewidth=2.0)

plt.title("SMAP Level-4 Latency")
plt.grid(True)
plt.axis([0, 6, 0, 14])
plt.ylabel('Days')
plt.xlabel('2016')

x = range(0,7)
labels = ['Jun','Jul','Aug','Sep','Oct','Nov','Dec']
plt.xticks(x, labels)

plt.text(1.1,11,'Carbon (L4_C)',color='g')
plt.text(3.1,3.5,'Soil Moisture (L4_SM)',color='b')

#plt.show()
plt.savefig('test.png',dpi=200)
