from cnm import *

class kinesis_open(object):

    def __init__(self, handle, mode):

        self.handle = handle
        self.mode = mode

    def __enter__(self):

        if self.mode == 's':
            self.resource = CNMSender(**self.handle.send)
        elif self.mode == 'r':
            self.resource = CNMReceiver(**self.handle.receive)
        else:
            self.resource = None

        return self.resource

    def __exit__(self, exc_type, exc_val, exc_tb):

        if exc_type:
            print(f"An exception occurred: {exc_val}")
        # Clean up the resource here
        self.resource = None
        if self.resource:
            self.resource.close()
