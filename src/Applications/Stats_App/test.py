#! /usr/bin/env python

def get_days(latency):

    values = latency.split(',')
    values = [ float(v.split()[0].strip()) for v in values ]

    return values[0] + values[1] / 24.0 + values[2] / (60.0 * 24.0)

hash = {}

with open('monthly.summary') as f:
    lines = f.readlines()

for line in lines:

    text = line.rstrip().split(':')
    line_break = (len(text[0]) == 0)

    type = hash.get('Stream Type','')
    name = hash.get('Stream Name','')
    tname = hash.get('Transaction Name','')

    valid_block = (type == 'SPL4C' and
                   name == 'SPL4C_Vv2040'
                   and tname == 'ALL_TYPE')

    if line_break and valid_block:

        t1 = get_days(hash['Mean Production Latency'])
        t2 = get_days(hash['Mean Acquisition Latency'])

        print t1 + t2

    elif len(text) == 1:

        continue

    else:

        hash[text[0]] = text[1].strip()
