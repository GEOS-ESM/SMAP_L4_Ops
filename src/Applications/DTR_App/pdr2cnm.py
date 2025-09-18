#! /usr/bin/env python

import os
import sys
import json
import argparse

from messenger import PDR, CNM

# Retrieve command-line arguments.

parser = argparse.ArgumentParser(description='Convert PDRs to CNM messages')

parser.add_argument('-i', '--input', metavar='PDRs', nargs='+',
    help='PDR(s) to process.')
parser.add_argument('-o', '--odir', metavar='ODIR', type=str, required=True,
    help='CNM staging directory')

args = parser.parse_args()

for fname in args.input:
    pdr = PDR(fname)
    cnm = CNM(pdr)

    cnm.write()

#   with open('test.cnm', 'w') as f:
#       json.dump(cnm.message, f, indent=4) # indent=4 for pretty-printing
