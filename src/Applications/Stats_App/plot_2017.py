#! /usr/bin/env python

import matplotlib.pyplot as plt

soilm = [2.22992927378,2.15859977367,2.19958459137,2.32817020458,2.20556329939,2.32909794998,2.67361480022,2.15310193798,2.14544199263,2.19921078324,2.1824343772,2.06455140314]


carbon = [11.3122379673, 8.68273187301, 8.01172169028, 12.7817016308,10.4960988739,10.4892033368,7.92416938429,10.7274453854,10.253141864,9.48345282516,10.4364749105,9.04072637169]

line = plt.plot(soilm)
plt.setp(line, color='b', linewidth=2.0)
line = plt.plot(carbon)
plt.setp(line, color='g', linewidth=2.0)

plt.title("SMAP Level-4 Latency")
plt.grid(True)
plt.axis([0, 12, 0, 14])
plt.ylabel('Days')
plt.xlabel('2017')

x = range(0,12)
labels = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
plt.xticks(x, labels)

plt.text(1.1,7.5,'Carbon (L4_C)',color='g')
plt.text(3.1,3.5,'Soil Moisture (L4_SM)',color='b')

#plt.show()
plt.savefig('test.png',dpi=200)
