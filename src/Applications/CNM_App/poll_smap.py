#! /usr/bin/env python

import os
import sys
import glob
import argparse

from kinesis import *
from cnmtypes import *
from cnm import PDRconvert
from polling import PDR, PAN

EXPORT_DIR = '/datastage/smaplevel4/smapnsid'
CHANNELS_DEFAULT = ['fp', 'fpp', 'rproc', 'test']
URI_DEFAULT = 'https://portal.nccs.nasa.gov/datastage'

# Retrieve command-line arguments.

parser = argparse.ArgumentParser(description='SMAP-L4 CNM Polling App')

parser.add_argument('-c', '--channels', metavar='channels', nargs='+',
    default=CHANNELS_DEFAULT,
    help='SMAP-L4 export channels to poll')

args = parser.parse_args()
export_dir = EXPORT_DIR

for channel in args.channels:

    pdr_dir = os.path.join(export_dir, 'data_out', channel, 'PDR')
    cnms_dir = os.path.join(export_dir, 'data_out', channel, 'CNM-S')
    pan_dir = os.path.join(export_dir, 'a', 'data_in', channel, 'PAN')
    uri = os.path.join(URI_DEFAULT, channel)

    # Transmit new PDRs

    pdr_listing = glob.glob(os.path.join(pdr_dir, '*.PDR'))

    with kinesis_open(SMAPL4Type, 's') as f:

        for pdr_file in pdr_listing:

            print(pdr_file)
            name, ext  = os.path.splitext(os.path.basename(pdr_file))
            pan_file = os.path.join(pan_dir, name) + '.PAN'
            cnms_file = os.path.join(cnms_dir, name) + '.CNM-S'

         #  if os.path.exists(pan_file):
         #      continue

            os.makedirs(cnms_dir, mode=0o755, exist_ok=True)

            pdr = PDR(pdr_file)
            message = PDRconvert(pdr, uri)
         #  f.send(message)
            f.save(message, cnms_file)
            print(cnms_file)

# Acquire PDR receipts and create PAN files.

with kinesis_open(SMAPL4Type, 'r') as f:

    for message in f.receive():

        print(message)
        channel, name = message['collection'].split('/')
        print(channel, name)
        pan_dir = os.path.join(export_dir, 'a', 'data_in', channel, 'PAN')
        pan_file = os.path.join(pan_dir, name) + '.PAN'
        cnmr_dir = os.path.join(export_dir, 'a', 'data_in', channel, 'CNM-R')
        cnmr_file = os.path.join(cnmr_dir, name) + '.CNM-R'

        os.makedirs(pan_dir, mode=0o755, exist_ok=True)
        os.makedirs(cnmr_dir, mode=0o755, exist_ok=True)

        pan = PAN(message)
        pan.write(pan_file)
        f.save(message, cnmr_file)
