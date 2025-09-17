import os
import datetime as dt

URI_DEFAULT = 'https://portal.nccs.nasa.gov/datastage/data'

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

class CNM(object):

    def __init__(self, pdr, uri=URI_DEFAULT):

        self.uri = uri

        self.message = {}
        self.message['submissionTime'] = pdr.submissionTime
        self.message['identifier'] = pdr.identifier
        self.message['collection'] = pdr.collection
        self.message['provider'] = pdr.provider

        product = {}
        self.message['product'] = product

        files = []
        product['files'] = files

        for group in pdr.GROUPS:

            for file in group['FILES']:

                d = {}
                files.append(d)

                d['name'] = file['FILE_ID']
                d['uri'] = os.path.join(self.uri, file['FILE_ID'])
                d['type'] = file['FILE_TYPE']
                d['size'] = int(file['FILE_SIZE'])
                d['checksumType'] = file.get('FILE_CKSUM_TYPE', None)
                d['checksum'] = file.get('FILE_CKSUM_VALUE', None)

                name = d['name'].split('.')[0]
                product['dataVersion'] = name.split('_')[-1]
                product['name'] = name
