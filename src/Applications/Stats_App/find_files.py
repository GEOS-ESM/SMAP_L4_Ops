#! /usr/bin/env python

import sys
import re
import datetime as dt

total = 0.0
count = 0
buf   = {}
names = {}

for file in sys.stdin:

    file = file[0:-1]

    result  = re.match(r'.*(SP.+)\.([0-9]{8}T[0-9]{6})',file)
    key     = result.group(1) + '.' + result.group(2)

    result  = re.match(r'.*.([0-9]{8}T[0-9]{6})',file)
    data_dt = dt.datetime.strptime(result.group(1),"%Y%m%dT%H%M%S")

    result  = re.match(r'.*.([0-9]{14})',file)
    prod_dt = dt.datetime.strptime(result.group(1),"%Y%m%d%H%M%S")

    with open(file) as f:

        lines = f.readlines()
        line  = lines[2][0:-1]

        pattern = r'.*([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2})'
        result  = re.match(pattern, line)

        delivery_dt = dt.datetime.strptime(result.group(1),"%Y-%m-%dT%H:%M:%S")

#       td = prod_dt - data_dt
#       td = delivery_dt - prod_dt
        td = delivery_dt - data_dt

        if key in buf:
            if td.total_seconds() > buf[key]:
                buf[key] = td.total_seconds()
                names[key] = file
        else:
            buf[key] = td.total_seconds()
            names[key] = file

for k,v in names.items():
    print(v)

