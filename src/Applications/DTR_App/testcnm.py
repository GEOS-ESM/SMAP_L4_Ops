#! /usr/bin/env python

import os
import sys
import json
import argparse

from nesdis import PDR
from kinesis import kinesis_open
from cnm import PDRconvert, CNMSendType, CNMReceiveType

URI_DEFAULT = 'https://portal.nccs.nasa.gov/datastage/fp/'

# Retrieve command-line arguments.

parser = argparse.ArgumentParser(description='Convert PDRs to CNM messages')

parser.add_argument('-i', '--input', metavar='PDRs', nargs='+',
    help='PDR(s) to process.')
#parser.add_argument('-o', '--odir', metavar='ODIR', type=str, required=True,
#   help='CNM staging directory')

args = parser.parse_args()

cnms = CNMSendType()

for fname in args.input:
    pdr = PDR(fname)
    message = PDRconvert(pdr, URI_DEFAULT)

    with kinesis_open(cnms) as f:
        f.send(message)
