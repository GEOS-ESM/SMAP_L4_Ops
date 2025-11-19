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

def send(channels):
    """
    Send CNM-S messages.

    This method sends CNM-S messages for unexecuted PDR files located
    in the export directory for the specified channels. 

    Parameters
    ----------
    channels : list|string
        Export channel name(s) to query (e.g. 'fp','fpp','rproc','test')

    Returns
    -------
    No explicit return value

    """

    if not isinstance(channels, list):
        channels = [channels]

#   Create and broadcast CNM-S messages for unexecuted PDRs.

    for channel in channels:
    
        pdr_dir = os.path.join(EXPORT_DIR, 'data_out', channel, 'PDR')
        cnms_dir = os.path.join(EXPORT_DIR, 'data_out', channel, 'CNM-S')
        pan_dir = os.path.join(EXPORT_DIR, 'a', 'data_in', channel, 'PAN')
        uri = os.path.join(URI_DEFAULT, channel)
    
        pdr_listing = glob.glob(os.path.join(pdr_dir, '*.PDR'))
    
        with kinesis_open(SMAPL4Type, 's') as f:
    
            for pdr_file in pdr_listing:
    
                name, ext  = os.path.splitext(os.path.basename(pdr_file))
                pan_file = os.path.join(pan_dir, name) + '.PAN'
                cnms_file = os.path.join(cnms_dir, name) + '.CNM-S'
    
                if os.path.exists(pan_file) or os.path.exists(cnms_file):
                    continue
    
                os.makedirs(cnms_dir, mode=0o755, exist_ok=True)
    
                pdr = PDR(pdr_file)
                message = PDRconvert(pdr, uri)
    
                print(f'Exporting: {pdr_file}')
                f.send(message)
                f.save(message, cnms_file)

def receive(channels):

    """
    Receive CNM-R messages.

    This method will query CNM-R messages and create PAN files for PDRs
    that have not been finalized.

    Parameters
    ----------
    channels : list|string
        Export channel name(s) to query (e.g. 'fp','fpp','rproc','test')

    Returns
    -------
    No explicit return value

    """

    if not isinstance(channels, list):
        channels = [channels]

    # Acquire PDR receipts and create PAN files.
    
    with kinesis_open(SMAPL4Type, 'r') as f:
    
        for message in f.receive():
    
            try:
                channel, name = message['collection'].split('/')
            except:
                channel = 'unknown'
    
            if channel not in channels:
                print(f'Skipping message: "{message["collection"]}"')
                continue
    
            pdr_dir = os.path.join(EXPORT_DIR, 'data_out', channel, 'PDR')
            pdr_file = os.path.join(pdr_dir, name) + '.PDR'
            pan_dir = os.path.join(EXPORT_DIR, 'a', 'data_in', channel, 'PAN')
            pan_file = os.path.join(pan_dir, name) + '.PAN'
            cnmr_dir = os.path.join(EXPORT_DIR, 'a', 'data_in',
                                    channel, 'CNM-R')
            cnmr_file = os.path.join(cnmr_dir, name) + '.CNM-R'
    
            if os.path.exists(pan_file) or os.path.exists(cnmr_file):
                continue
    
            os.makedirs(pan_dir, mode=0o755, exist_ok=True)
            os.makedirs(cnmr_dir, mode=0o755, exist_ok=True)
    
            pan = PAN(message)
            pan.write(pan_file)
            f.save(message, cnmr_file)
    
            if pan.is_success():
                print(f'{pdr_file} is a success')
            else:
                print(f'{pdr_file} failed to ingest')

if __name__ == "__main__":

    """     
    Driver for executing SMAP L4 PDRs.
            
    This application is used to convert SMAP L4 PDRs into CNM-S messages and
    broadcast to the NSIDC Kinesis stream. It provides end-to-end functionality
    for modeling the PDR/PAN transfer protocol as a CNM messaging scheme.

    Notes
    -----
    Note-1 :
        See cnmtypes module for a list of configured CNM types. This application
        can be further abstracted to be fully driven by the content in the 
        cnmtypes module. 

    Note-2 :
        This application can be instantiated as multiple polling deamons using
        the command-line options. Polling can be stratified by polling channel,
        send operations, receive operations or any combination thereof.

    Parameters
    ----------
    channels : list
        Export channel name(s) to query (e.g. 'fp','fpp','rproc','test'). The
        default is to query all channels (see CHANNELS_DEFAULT).

    send : boolean
        Query and send unexecuted PDRs as CNM-S messages. This is the default
        if the send/receive options are unspecified.

    receive : boolean
        Query and receive CNM-R messages for open PDRs. This is the default
        if the send/receive options are unspecified.
            
    Returns
    ------- 
    value : integer
        0: normal
        non-zero: error occurred
            
    """         
        
    # Retrieve command-line arguments.
        
    parser = argparse.ArgumentParser(description='SMAP-L4 CNM Polling App')
        
    parser.add_argument('-c', '--channels', metavar='channels', nargs='+',
                        default=CHANNELS_DEFAULT,
                        help='SMAP-L4 export channels to poll')
        
    parser.add_argument('--send', action='store_true',
                        help='Send CNM-S messages')
    parser.add_argument('--receive', action='store_true',
                        help='Retrieve CNM-R messages')
    
    args = parser.parse_args()
    args.noargs = not (args.send or args.receive)

    if args.send or args.noargs:
        send(args.channels)

    if args.receive or args.noargs:
        receive(args.channels)

    sys.exit(0)
