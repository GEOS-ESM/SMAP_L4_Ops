from cnm import *

class kinesis_open(object):

    def __init__(self, handle):

        self.handle = handle

    def __enter__(self):

        if isinstance(self.handle, CNMSendType):
            self.resource = CNMSender(self.handle)
        elif isinstance(self.handle, CNMReceiveType):
            self.resource = CNMReceiver(self.handle)
        else:
            self.resource = None

        return self.resource

    def __exit__(self, exc_type, exc_val, exc_tb):

        if exc_type:
            print(f"An exception occurred: {exc_val}")
        # Clean up the resource here
        self.resource = None
