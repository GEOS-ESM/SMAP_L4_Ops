import glob
import os

from kinesis import *
from cnmtypes import *
from cnm import PDRconvert
from polling import PDR, PAN

CHANNELS_DEFAULT = ['fp', 'fpp', 'rproc', 'test']

for channel in self.channels:

    poll_dir = os.path.join(port, 'data_out', channel, 'PDR')

    pdr_dir = os.path.join(port, 'data_out', channel, 'PDR')
    cnms_dir = os.path.join(port, 'data_out', channel, 'CNM-S')
    pan_dir = os.path.join(port, 'a', 'data_in', channel, 'PAN')
    cnmr_dir = os.path.join(port, 'a', 'data_in', channel, 'CNM-R')

    # Transmit new PDRs

    pdr_listing = glob.glob(os.path.join(pdr_dir, '*.PDR'))

    with kinesis_open(SMAPL4Type, 's') as f:

        for pdr_file in pdr_listing:

            name, ext  = os.path.splitext(os.path.basename(pdr_file))
            pan_file = os.path.join(pan_dir, name) + '.PAN'

            if os.path.exists(pan_file):
                continue

            pdr = PDR(pdr_file)
            message = PDRconvert(pdr)
            f.send(message)

# Acquire processed PDR information

with kinesis_open(SMAPL4Type, 'r') as f:

    for message in f.receive():

        channel, fname = message['collection'].split('/')
        pan_dir = os.path.join(port, 'a', 'data_in', channel, 'PAN')

        pan = PAN(message)
        pan.write(fname)
