from types import SimpleNamespace

SMAPL4SendType = dict(stream='smap-cnm-send',
                      region='us-west-2',
                      partition='SMAPL4toNSIDC'
                 )

SMAPL4ReceiveType = dict(stream='smap-cnm-receive',
                         region='us-west-2',
                         partition='SMAPL4toNSIDC'
                    )

SMAPL4Type = SimpleNamespace(send=SMAPL4SendType, receive=SMAPL4ReceiveType)
