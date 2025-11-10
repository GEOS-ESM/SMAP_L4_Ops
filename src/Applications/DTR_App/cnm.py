import os
import json
import boto3

KINESIS_REGION_NAME = 'us-west-2'
KINESIS_SEND_STREAM = 'smap-cnm-send'
KINESIS_RECEIVE_STREAM = 'smap-cnm-receive'
KINESIS_PARTITION_KEY = 'SMAPL4toNSIDC'
CNM_SCHEMA_VERSION = '1.4'
FILE_TYPES = { 'science': 'data' }

class CNMSendType(object):

    def __init__(self, **kwargs):

        self.stream = kwargs.get('stream', KINESIS_SEND_STREAM)
        self.region = kwargs.get('region', KINESIS_REGION_NAME)
        self.partition = kwargs.get('partition', KINESIS_PARTITION_KEY)
        self.schema = kwargs.get('schema', CNM_SCHEMA_VERSION)

class CNMReceiveType(object):

    def __init__(self, **kwargs):

        self.stream = kwargs.get('stream', KINESIS_RECEIVE_STREAM)
        self.region = kwargs.get('region', KINESIS_REGION_NAME)
        self.partition = kwargs.get('partition', KINESIS_PARTITION_KEY)
        self.schema = kwargs.get('schema', CNM_SCHEMA_VERSION)

class CNM(object):

    def __init__(self, handle):

        self.stream = handle.stream
        self.region = handle.region
        self.partition = handle.partition
        self.schema = handle.schema
        self.kinesis_client = boto3.client('kinesis',region_name=self.region)

class CNMSender(CNM):

    def __init__(self, handle):

        super().__init__(handle)

    def send(self, message):

        try:
            response = self.kinesis_client.put_record(
                StreamName=self.stream,
                Data=json.dumps(message),
                PartitionKey=self.partition
            )
            print(f"Record sent successfully: {response}")
        except Exception as e:
            print(f"Error sending record: {e}")

class CNMReceiver(CNM):

    def __init__(self, handle):

        super().__init__(handle)
        
    def receive(self):

        response = self.kinesis_client.describe_stream(StreamName=self.stream)
        shards = response['StreamDescription']['Shards']

        shard_id = shards[0]['ShardId']
        response = kinesis_client.get_shard_iterator(
            StreamName=self.stream,
            ShardId=shard_id,
            ShardIteratorType='TRIM_HORIZON'
        )
        shard_iterator = response['ShardIterator']

        response = self.kinesis_client.get_records(
            ShardIterator=shard_iterator,
            Limit=100
        )
        records = response['Records']
        for record in records:
            # The data in Kinesis records is Base64 encoded
            decoded_data = record['Data'].decode('utf-8')
            yield decoded_data

    __iter__ = receive

def PDRconvert(pdr, uri):

    message = {}
    message['version'] = CNM_SCHEMA_VERSION
    message['submissionTime'] = pdr.submissionTime
    message['identifier'] = pdr.identifier
    message['collection'] = pdr.collection
    message['provider'] = pdr.provider

    product = {}
    message['product'] = product

    files = []
    product['files'] = files

    for group in pdr.GROUPS:

        product['dataVersion'] = group['DATA_VERSION']

        for file in group['FILES']:

            d = {}
            files.append(d)

            d['name'] = file['FILE_ID']
            d['uri'] = os.path.join(uri, file['FILE_ID'])

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

    return message
