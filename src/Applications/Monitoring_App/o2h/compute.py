from gmaopy.modules.obsstat import *

def insert_db(expver=None, dates=None, filename=None):
    # generalize these
    source = 'oper'
    database = 'ldas_ops'
    type = 'ob'

    return obsstat(
        source = source,
        database = database,
        expver = expver,
        overwrite = True,
        type = type,
        date = dates,
        domain = ['global','n.hem','s.hem','tropics'],
        ignore_missing_files = True,
        obs = [
            observation(
                variable = ['oma','omf','xvec','xm'],
                statistic = ['count','mean','rms','esigo','esigb','normcost'],
                filename = filename,
                usage = 'used',
            ),
            observation(
                variable = ['oma','omf'],
                statistic = ['count','mean','rms','esigo','esigb'],
                filename = filename,
                usage = 'passive',
            ),
            observation(
                variable = 'omf',
                statistic = 'count',
                filename = filename,
                usage = 'unused',
            ),
        ]
    )
