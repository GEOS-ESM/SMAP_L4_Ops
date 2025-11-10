import os
import json
import time
import boto3
import datetime as dt

class PDR(object):

    def __init__(self, fname):

        bname, ext = os.path.splitext(os.path.basename(fname))
        nodes = bname.split('.')
        tnode = nodes[-1]
        collection = nodes[0]
        time_dt = dt.datetime.strptime(tnode, "%Y%m%d%H%M%S")

        with open(fname, 'r') as f:
            self.lines = f.readlines()

        self.filename = fname
        self.channel = os.path.dirname(fname).split(os.sep)[-2]
        self.line = None
        self.ptr = 0

        self.parse_record(self.__dict__)

        self.submissionTime = time_dt.strftime("%Y-%m-%dT%H:%M:%S.%fZ")
        self.identifier = os.path.join(self.channel, bname)
        self.collection = collection
        self.provider = self.ORIGINATING_SYSTEM

    def parse_record(self, d):

        while self.pop():

            line = self.line.strip()
            k = line.split('=')[0].strip()
            v = line.split('=')[1].strip().strip(';')

            if k.upper() == 'END_OBJECT':
                return

            if k.upper() == 'OBJECT' and v.upper() == 'FILE_GROUP':
                d['GROUPS'] = d.get('GROUPS', [])
                d['GROUPS'].append({})
                self.parse_record(d['GROUPS'][-1])
                continue

            if k.upper() == 'OBJECT' and v.upper() == 'FILE_SPEC':
                d['FILES'] = d.get('FILES', [])
                d['FILES'].append({})
                self.parse_record(d['FILES'][-1])
                continue

            d[k] = v

    def pop(self):

        if self.ptr >= len(self.lines):
            return False

        self.line = self.lines[self.ptr]
        self.ptr += 1

        return True

class PAN(object):

    def __init__(self, message):

        self.message = json.loads(message)

    def __write__(self):

        stream, fname = self.message['collection'].split('/')

        tnode = self.message['processCompleteTime'].split(':')
        sec = round(float(tnode[-1]))
        sec = f"{sec:02d}"
        time_stamp = ':'.join(tnode[0:-1] + [sec])

        response = self.message['response']
        disposition = response['status']

        with open(fname, 'w'):

            f.write('MESSAGE_TYPE = SHORTPAN;')

            if response['status'] == 'SUCCESS':
                f.write('DISPOSITION = SUCCESSFUL;')
            else:
                f.write('DISPOSITION = FTP/KFTP FAILURE;')

            f.write(f'TIME_STAMP = {time_stamp};')
