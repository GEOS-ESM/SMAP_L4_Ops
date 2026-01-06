import os
import json
import boto3

CNM_SCHEMA_VERSION = '1.4'
FILE_TYPES = { 'science': 'data' }

class CNM(object):
    """
    Base class for Cloud Network Messaging (CNM). Provides common methods for
    send and receive messaging.

    Methods
    --------
    __init__ :
        Initializes kinesis stream parameters and opens client connection.
    close:
        Closes kinesis stream.
    save:
        Saves CNM messages to a file.

    """

    def __init__(self, stream=None, region=None, partition=None):

        self.stream = stream
        self.region = region
        self.partition = partition
        self.schema = CNM_SCHEMA_VERSION
        self.kinesis_client = boto3.client('kinesis',region_name=self.region)

    def close(self):

        self.kinesis_client.close()

    def save(self, message, file):

        with open(file, 'w') as f:
            json.dump(message, f, indent=2)

class CNMSender(CNM):

    """
    Provides methods for sending Cloud Network Messages (CNM-S).

    Methods
    --------
    send :
        Sends CNM-S message.

    """

    def __init__(self, **kwargs):

        super().__init__(**kwargs)

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

    """
    Provides methods for receiving Cloud Network Messages (CNM-R).

    Methods
    --------
    receive :
        Receives CNM-R messages.

    """

    def __init__(self, **kwargs):

        super().__init__(**kwargs)
        
    def receive(self):

        response = self.kinesis_client.describe_stream(StreamName=self.stream)
        shards = response['StreamDescription']['Shards']

        shard_id = shards[0]['ShardId']
        response = self.kinesis_client.get_shard_iterator(
            StreamName=self.stream,
            ShardId=shard_id,
            ShardIteratorType='TRIM_HORIZON'
        )
        shard_iterator = response['ShardIterator']

        while True:

            response = self.kinesis_client.get_records(
                ShardIterator=shard_iterator,
                Limit=10000
            )
            records = response['Records']
            if not records:
                break;

            for record in records:
                # The data in Kinesis records is Base64 encoded
                try:
                    decoded_data = record['Data'].decode('utf-8')
                except:
                    decoded_data = None

                if decoded_data:
                    message = json.loads(decoded_data)
                    yield message

            shard_iterator = response['NextShardIterator']

    __iter__ = receive

def PDRconvert(pdr, uri):
    """
    Converts a PDR object into a CNM-S message.

    Parameters
    ----------
    pdr : PDR object
        PDR object containing definitions to be converted to CNM-S.
    uri : string
        This is the URI that will be included in the CNM-S message. It must
        be the actual location of the data files in the message to be retrieved
        using HTTPS.

    Returns
    -------
    message : dict
        CNM-S message parameters stored as a dictionary.

    """

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
            product['name'] = name + '.h5'

    return message
