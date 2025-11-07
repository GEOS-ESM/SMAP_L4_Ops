import glob
import os

class Pollster(object):

    def __init__(self, port, channels=None):

        self.channels = channels
        if not channels:
            self.channels = ['fp', 'fpp', 'rproc', 'test']

        self.port = port

    def poll(self):

        for channel in self.channels:

            poll_dir = os.path.join(port, 'data_out', channel, 'PDR')

            pdr_dir = os.path.join(port, 'data_out', channel, 'PDR')
            cnms_dir = os.path.join(port, 'data_out', channel, 'CNM-S')
            pan_dir = os.path.join(port, 'a', 'data_in', channel, 'PAN')
            cnmr_dir = os.path.join(port, 'a', 'data_in', channel, 'CNM-R')

            pdr_listing = glob.glob(os.path.join(pdr_dir, '*.PDR'))
            for pdr in pdr_listing:

                name, ext  = os.path.splitext(os.path.basename(pdr))
                pan_file = os.path.join(pan_dir, name) + '.PAN'

                if os.path.exists(pan_file):
                    continue

                pdr = PDR(pdr_file)
                cnm = CNMSender(pdr)
                cnm.write()
                cnm.send()
