import os
import shutil
import psycopg2
import datetime as dt
from gmaopy.modules.obsstat import Dates
import calendar as c

def make(expver=None, dates=None, output=None, template=None):
    '''query db for data and modify template webpage'''
    yrmon = []
    for d in dates:
        ym = str(d)[:6]
        if ym not in yrmon:
            yrmon.append(ym)

    # monthly
    for ym in yrmon:
        print ym,' monthly files'
        y = str(ym)[:4]
        m = str(ym)[4:6]
        path = os.path.join(output, 'Y'+y)
        path = os.path.join(path, 'M'+m)
        shutil.copyfile(os.path.join(template, 'G1.html'), os.path.join(path, 'G1.html'))
        shutil.copyfile(os.path.join(template, 'G2.html'), os.path.join(path, 'G2.html'))
        shutil.copyfile(os.path.join(template, 'G3.html'), os.path.join(path, 'G3.html'))
        shutil.copyfile(os.path.join(template, 'G4.html'), os.path.join(path, 'G4.html'))
        shutil.copyfile(os.path.join(template, 'G5.html'), os.path.join(path, 'G5.html'))
        shutil.copyfile(os.path.join(template, 'G2.html'), os.path.join(path, 'G6.html'))
        shutil.copyfile(os.path.join(template, 'G3.html'), os.path.join(path, 'G7.html'))
        shutil.copyfile(os.path.join(template, 'G4.html'), os.path.join(path, 'G8.html'))
        shutil.copyfile(os.path.join(template, 'G5.html'), os.path.join(path, 'G9.html'))

        # peripherals
        shutil.copyfile(os.path.join(template, 'kttable.html'), os.path.join(path, 'kttable.html'))
        shutil.copyfile(os.path.join(template, 'kxtable.html'), os.path.join(path, 'kxtable.html'))
        shutil.copyfile(os.path.join(template, 'qcxtable.html'), os.path.join(path, 'qcxtable.html'))
        shutil.copyfile(os.path.join(template, 'qchtable.html'), os.path.join(path, 'qchtable.html'))

        with open(os.path.join(template, 'index.html'), 'r') as f:
            data = f.read()

        data = data.replace('{expver}', 'SPL4SM_'+expver)
        this_month = [str(x) for x in dates if ym in str(x)[:6]]
        begin = dt.datetime.strftime(dt.datetime(int(y), int(m), 1, 0), '%d%b%Y, %HZ')
        endd = this_month[-1]
        end = dt.datetime.strftime(dt.datetime(int(y), int(m), int(endd[6:8]), int(endd[8:10])), '%d%b%Y, %HZ')
        # I should intersect this with available synoptic times from the db, but let's assume they are all there
        this_month = [str(x) for x in Dates(int(y+m+'0100'), int(endd), 3)]
        data = data.replace('{begin}', begin)
        data = data.replace('{end}', end)

        # coverage plots
        cov = ['all']+[str(k) for k in range(640,644,1)]
        for k in cov:
            lines = []
            for d in this_month:
                print d,' coverage'
                year = int(d[:4])
                month = int(d[4:6])
                day = int(d[6:8])
                hour = int(d[8:10])
                z = dt.datetime.strftime(dt.datetime(year, month, day, hour), '%d%b%Y, %HZ')
                s = '<option value="cov/'+d+'_'+k+'_cov.png">'+z+'</option>'
                lines.append(s)
            data = data.replace('{'+k+'_cov}', '\n'.join(lines))
        with open(os.path.join(path, 'index.html'), 'w') as g:
            g.write(data)

    # db connect
    #con = None
    #try:
    #    con = psycopg2.connect('host=edb1 dbname=gmao_stats user=gmao_ops')
    #    cur = con.cursor()
    #except Exception as e:
    #    print e
    #    print 'Cannot connect to the database.'
    #    return

    #if con:
        #start = dates[0]
        #end = dates[-1]
        #period = [str(x) for x in Dates(start, end, 3)]

        #cur.execute("select distinct date from ldas_ops.v_view where expver='"+expver
        #    +"' and variable='omf' and statistic='sum' and domain_name='global' and date in "
        #    +"("+','.join(period)+") order by date asc")
        #rows = cur.fetchall()
        #rows = [str(x[0]) for x in rows]

        # we will assume that requested data is in database and imagery is generated too

    #    yrmon = []
    #    for d in dates:
    #        ym = str(d)[:6]
    #        if ym not in yrmon:
    #            yrmon.append(ym)

        
        # determine months
        # fill in coverages

    #if con:
    #    con.close()

    # loop over each month
    #yrmon = []
    #for d in dates:
    #    ym = str(d)[:6]
    #    if ym not in yrmon:
    #        yrmon.append(ym)
    # standard template files are in /html/
    #for ym in yrmon:
    #    with open('html/index.html','r') as i:
    #        input = i.readlines()
    #        for l,line in enumerate(input):
    #            input[l] = line.replace('{expver}', 'SPL4SM_'+expver)
    #        with open(os.path.join(os.path.join(output, ym), 'index.html'), 'w') as f:
    #            f.write(' ')
