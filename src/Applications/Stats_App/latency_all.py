#! /usr/bin/env python

import sys
import re
import datetime as dt

latency = {}
buffer = dict(production={}, delivery={}, total={})

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
        t = str(result.group(1))
        print(t)
        if t[-2:] == '60':
           t = t[0:-2] + '59'
        delivery_dt = dt.datetime.strptime(t,"%Y-%m-%dT%H:%M:%S")

        latency['production'] = prod_dt - data_dt
        latency['delivery'] = delivery_dt - prod_dt
        latency['total'] = delivery_dt - data_dt

        for k,buf in buffer.items():

            td = latency[k]

            if key in buf:
                if td.total_seconds() < buf[key]: buf[key] = td.total_seconds()
            else:
                buf[key] = td.total_seconds()

for k,buf in buffer.items():

    print(k.upper())

    total = 0.0
    count = 0
    outbuf = []

    for key in sorted(buf.keys()):
        value = buf[key]
        days  = value/86400.
        date  = key.split('.')[-1]
        day   = date[6:8]
    #   print "'"+day+"'"
        outbuf.append(str(days))
        print(days)

    print(','.join(outbuf))

    rmax = 0.0
    for key, value in buf.items():

    #   if value > 86400.0 * 5: print key
        count = count + 1
        total += value
        if value > rmax: rmax = value

    total = total / count

    total_days = total / 86400.0

    days   = total//86400.
    total -= days*86400.
    hours  = total//3600.
    total -= hours*3600.
    minutes = total//60.

    print(f'{k} latency => Mean: {count} {days} {hours} {minutes}')

    total = rmax

    total_days = total / 86400.0

    days   = total//86400.
    total -= days*86400.
    hours  = total//3600.
    total -= hours*3600.
    minutes = total//60.

    print(f'{k} latency => Maximum: {count} {days} {hours} {minutes}')
