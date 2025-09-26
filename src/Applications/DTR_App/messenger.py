import os
import json
import time
import boto3
import datetime as dt

URI_DEFAULT = 'https://portal.nccs.nasa.gov/datastage/data'

KINESIS_REGION_NAME = 'us-west-2'
KINESIS_SEND_STREAM = 'smap-cnm-send'
KINESIS_RECEIVE_STREAM = 'smap-cnm-receive'
KINESIS_PARTITION_KEY = 'SMAPL4toNSIDC'

CNM_CACHE_DIR = '/datastage/smaplevel4/smapnsid/data_out'
CNM_SCHEMA_VERSION = '1.4'

FILE_TYPES = { 'science': 'data' }

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

    def __init__(self, uri=URI_DEFAULT,
                 region_name=KINESIS_REGION_NAME,
                 stream_name=KINESIS_SEND_STREAM,
                 partition_key=KINESIS_PARTITION_KEY):

        self.uri = uri
        self.region_name = region_name
        self.stream_name = stream_name
        self.partition_key = partition_key

class CNMSender(CNM):

    def __init__(self, pdr, **kwargs):

        kwargs['stream_name'] = kwargs.get('stream_name', KINESIS_SEND_STREAM)
        super().__init__(**kwargs)

        self.pdr_name = pdr.filename

        self.message = {}
        self.message['version'] = CNM_SCHEMA_VERSION
        self.message['submissionTime'] = pdr.submissionTime
        self.message['identifier'] = pdr.identifier
        self.message['collection'] = pdr.collection
        self.message['provider'] = pdr.provider

        product = {}
        self.message['product'] = product

        files = []
        product['files'] = files

        for group in pdr.GROUPS:

            product['dataVersion'] = group['DATA_VERSION']

            for file in group['FILES']:

                d = {}
                files.append(d)

                d['name'] = file['FILE_ID']
                d['uri'] = os.path.join(self.uri, file['FILE_ID'])

                ftype = file['FILE_TYPE'].lower()
                d['type'] = FILE_TYPES.get(ftype, file['FILE_TYPE'])
                d['size'] = int(file['FILE_SIZE'])

                checksumType = file.get('FILE_CKSUM_TYPE', None)
                if checksumType:
                    d['checksumType'] = checksumType

                checksum = file.get('FILE_CKSUM_VALUE', None)
                if checksum:
                    d['checksum'] = checksum

                name = d['name'].split('.')[0]
                product['name'] = name

    def send(self):

        kinesis_client = boto3.client('kinesis', region_name=self.region_name)

        try:
            response = kinesis_client.put_record(
                StreamName=self.stream_name,
                Data=json.dumps(self.message),
                PartitionKey=self.partition_key
            )
            print(f"Record sent successfully: {response}")
        except Exception as e:
            print(f"Error sending record: {e}")

    def write(self):

        name, ext = os.path.splitext(os.path.basename(self.pdr_name))
        path = os.path.join(CNM_CACHE_DIR, 'CNM-S')
        pathname = os.path.join(path, name) + '.CNM-S'
        os.makedirs(path, mode=0o755, exist_ok=True)

        with open(pathname, 'w') as f:
            json.dump(self.message, f, indent=4)

class CNMReceiver(CNM):

    def __init__(self, **kwargs):

        kwargs['stream_name'] = kwargs.get('stream_name', KINESIS_RECEIVE_STREAM)
        super().__init__(**kwargs)
        
    def receive(self):

        response = kinesis_client.describe_stream(StreamName=self.stream_name)
        shards = response['StreamDescription']['Shards']

        shard_id = shards[0]['ShardId']
        response = kinesis_client.get_shard_iterator(
            StreamName=stream_name,
            ShardId=shard_id,
            ShardIteratorType='TRIM_HORIZON'
        )
        shard_iterator = response['ShardIterator']

        while True:
            response = kinesis_client.get_records(
                ShardIterator=shard_iterator,
                Limit=100
            )
            records = response['Records']
            for record in records:
                # The data in Kinesis records is Base64 encoded
                decoded_data = record['Data'].decode('utf-8')
                print(f"Received record: {decoded_data}")

            shard_iterator = response['NextShardIterator']
            if not shard_iterator:
                # Shard has been closed or no more data is available
                break

            # Implement a delay to avoid exceeding API limits
            # (e.g., 5 transactions per second per shard for GetRecords)
            time.sleep(1)
