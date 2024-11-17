import os
from gmaopy.modules.obsplot import *
from gmaopy.obs.odslevelplot import odslevelplot
from gmaopy.obs.odssummaryplot import odssummaryplot

def plot(expver=None, dates=None, filename=None, output=None):
    '''datahook to get values from db and also plot imagery'''
    import os
    import calendar as c

    # generalize these
    # source
    # database
    # type
    # frequency
    # domain?
    freq = 3

    # need to change date and only do per month
    yrmon = []
    for d in dates:
        ym = str(d)[:6]
        if ym not in yrmon:
            yrmon.append(ym)

    for i in yrmon:
        print 'plotting for date:',i
        mon = i[4:6]
        y = int(i[:4])
        m = int(i[4:6])
        start = int(i+'0100')
        end = int(i+str(c.monthrange(y,m)[1])+str(24-freq))
        month = Dates(start, end, freq)

        path = os.path.join(output, 'Y%02d' % y)
        path = os.path.join(path, 'M%02d' % m)

        print 'plotting monthly obs stats',i
        stats(expver=expver, dates=month, filename=filename, output=path)
        print ''
        print 'plotting monthly obs time series',i
        ts(expver=expver, dates=month, filename=filename, output=path)
        print ''
        
    for d in dates:
        try:
            print 'plotting obs coverage',d
            yr = 'Y%02d' % int(str(d)[:4])
            mn = 'M%02d' % int(str(d)[4:6])
            p = os.path.join(output, yr)
            p = os.path.join(p, mn)
            coverage(expver=expver, dates=d, filename=filename, output=p)
            check(expver=expver, dates=d, filename=filename, output=p)
            print ''
        except Exception as e:
            print e
            continue


def stats(expver=None, dates=None, filename=None, output=None):
    '''observational statistics plots'''

    # generalize these
    source = 'oper'
    database = 'ldas_ops'
    typ = 'ob'

    domain=['global','n.hem','tropics','s.hem']
    document(
        plot = [
            levelplot(
                {
                    'graphics.bar_color':['lime','darkorange','red'],
                    'graphics.alpha':1.0,
                    'xaxis.min':0,
                },
                plotdata = obsdata(
                    variable = 'oma',
                    statistic = 'count',
                    level = 'all',
                ),
                curve = [
                    stackedbar(
                        legend = 'used count',
                        plotdata = obsdata(usage='used')
                    ),
                    stackedbar(
                        legend = 'passive count',
                        plotdata = obsdata(usage='passive')
                    ),
                    stackedbar(
                        legend = 'unused count',
                        plotdata = obsdata(usage='unused')
                    ),
                ],
                graphics = graphics(
                    grid = False,
                ),
                has_legend = True,
                legend = plotlegend(
                    mode = None,
                    loc = 'outside topleft',
                    prop = plotfont(
                        size = 'x-small',
                    ),
                    columnspacing = 0.7,
                    handletextpad = 0.7,
                ),
            ),
            levelplot(
                {
                    'graphics.bar_color':['blue','red','cyan','gold'],
                    'graphics.alpha':1.0,
                },
                plotdata = obsdata(
                    statistic = ['rms','mean'],
                    level = 'all',
                    variable = ['omf','oma'],
                    usage = ['used','passive'],
                ),
                graphics = graphics(
                    grid = False,
                ),
                curve = shiftedbar(legend = '<statistic><variable>'),
                has_legend = True,
                legend = plotlegend(
                    mode = None,
                    loc = 'outside topleft',
                    prop = plotfont(
                        size = 'x-small',
                    ),
                    handletextpad = 0.7,
                ),
            ),
            levelplot(
                {
                    'graphics.bar_color':['blue','cyan','red'],
                    'graphics.alpha':1.0,
                    'xaxis.min':0,
                },
                plotdata = obsdata(level='all'),
                curve = [
                    shiftedbar(
                        plotdata = obsdata(
                            statistic = ['rms','mean'],
                            variable = 'xm',
                            usage = ['used'],
                        ),
                        legend = '<statistic>(sigb)',
                    ),
                    bar(
                        plotdata = obsdata(
                            statistic = 'rms',
                            variable = 'xvec',
                            usage = ['used'],
                        ),
                        legend = '<statistic>(sigo)',
                    ),
                    dot(
                        plotdata = obsdata(
                            statistic = 'esigo',
                            variable = 'esigo',
                            usage = ['used','passive'],
                        ),
                        graphics = item(
                            color = 'darkorange',
                            markersize = 8,
                        ),
                        legend = 'est(sigo)',
                    ),
                    dot(
                        plotdata = obsdata(
                            statistic = 'esigb',
                            variable = 'esigb',
                            usage = ['used','passive'],
                        ), 
                        graphics = item(
                            color = 'black',
                            markersize = 8,
                        ),
                        legend = 'est(sigb)',
                    ),
                ],
                graphics = graphics(
                    grid = False,
                ),
                has_legend = True,
                legend = plotlegend(
                    ncols = 5,
                    mode = None,
                    loc = 'outside topleft',
                    prop = plotfont(
                        size = 'x-small',
                    ),
                    handletextpad = 0.7,
                ),
            ),
        ],
        plotdata = obsdata(
            domain = domain,
            type = typ,
            expver = expver,
            database = database,
            date = dates,
            filename = filename
        ),
        has_title = True,
        title = ['', 'SPL4SM_<expver> <month>','<name> (<domain_name>)'],
        layout = [1,3],
        size = [10,8],
        interactive = False,
        output = os.path.join(output, '<name>_<domain_name>.png')
    )

def ts(expver=None, dates=None, filename=None, output=None):
    '''observational time series plots'''

    # generalize these
    source = 'oper'
    database = 'ldas_ops'
    typ = 'ob'

    import matplotlib as mpl
    mpl.rcParams['axes.grid'] = True

    domain=['global']
    document(
        plot = [
            timeseriesplot(
                {
                    'graphics.bar_color':['lime', 'darkorange', 'red'],
                    'graphics.alpha':1.0,
                    'graphics.bar_width_factor':1.0,
                    'graphics.edgewidth':0.001,
                    'graphics.edgecolor':['lime', 'darkorange', 'red'],
                    'yaxis.min':0,
                    'yaxis.other_side': True,
                    'xaxis.tick_label_size':'x-small',
                },
                plotdata = obsdata(
                    statistic = 'count',
                    variable = 'oma',
                ),
                legend = plotlegend(
                    columnspacing = 0.7,
                    handletextpad = 0.7,
                    location = 'outside topleft',
                    mode = None,
                    prop = plotfont(
                        size = 'x-small',
                    ),
                ),
                has_ytitle = False,
                curve = [
                    stackedbar(
                        plotdata = obsdata(usage='used'),
                        legend = 'used obs count',
                    ),
                    stackedbar(
                        plotdata = obsdata(usage='passive'),
                        legend = 'passive obs count',
                    ),
                    stackedbar(
                        plotdata = obsdata(usage='unused'),
                        legend = 'unused obs count',
                    ),
                ],
            ),
            timeseriesplot(
                {
                    'graphics.bar_color':['red', 'blue', 'gold', 'cyan'],
                    'graphics.alpha':1.0,
                    'graphics.bar_width_factor':1.0,
                    'graphics.edgewidth':0.001,
                    'graphics.edgecolor':['red', 'blue', 'gold', 'cyan'],
                    'yaxis.other_side': True,
                    'xaxis.tick_label_size':'x-small',
                },
                plotdata = obsdata(
                    statistic = ['rms','mean'],
                    variable = ['oma','omf'],
                ),
                legend = plotlegend(
                    columnspacing = 0.7,
                    handletextpad = 0.7,
                    location = 'outside topleft',
                    mode = None,
                    prop = plotfont(
                        size = 'x-small',
                    ),
                ),
                has_ytitle = False,
                curve = [
                    bar(
                        plotdata = obsdata(
                            usage = ['used'], #, 'passive'],
                        ),
                        legend = '<statistic> <variable>',
                    ),
                ],
            ),
        ],
        plotdata = obsdata(
            type = typ,
            expver = expver,
            database = database,
            filename = filename,
            date = dates,
            domain = domain,
            level = 'all',
        ),
        layout = [1,2],
        has_title = True,
        title = ['', 'SPL4SM_<expver> <month>','<name> (<domain_name>)'],
        size = [11,7],
        interactive = False,
        output = os.path.join(output, '<name>_ts.png')
    )

    mpl.rcParams['axes.grid'] = False

def coverage(expver=None, dates=None, filename=None, output=None):
    '''observational coverage plots'''
    import numpy as np
    # generalize these
    source = 'oper'
    database = 'ldas_ops'
    typ = 'ob'
    domain = ['global']

    cov = os.path.join(output, 'cov')

    with open(filename, 'r') as f:
        data = f.readlines()
        names = [(x.strip())[1:-1] for x in data if x and '=' not in x and '#' not in x]
    names = [x for x in names if x]+['']*4
    c = ['blue','cyan','gold','red','k','k','k','k']

    kx_all = [range(640,648,1)]
    for i in range(640,644,1):
        kx_all.append(i)
    for i,kx in enumerate(kx_all):
        if type(kx) is list:
            output = os.path.join(cov, '<date>_all_cov.png')
        else:
            kx = [kx]
            output = os.path.join(cov, '<date>_<kx>_cov.png')
        document(
            has_title = True,
            title = ['','<date_interval_with_time> <name>: <total_obs> observations', 'SPL4SM_<expver> (<domain_name>)'],
            plot = [
                coverageplot(
                    {
                        'symbol.alpha':0.75,
                    },
                    geography = geography(
                        projection='cyl',
                        boundary = boundary(linewidth=0.1),
                        coastlines = coastlines(linewidth=0.1),
                        plot_below = ['parallels','meridians','continents','coastlines'],
                        parallels = parallels(increment=30.,linewidth=0.1,dashes=[1,1],zorder=-1,labelstyle='+/-'),
                        meridians = meridians(increment=60.,linewidth=0.1,dashes=[1,1],zorder=-1,labelstyle='+/-'),
                    ),
                    plotdata = odsdata(
                        observer = odscounter(varname='total_obs', format='bigNumber')
                    ),
                    #title = ['Observation Locations'],
                    curve = [
                        curve(
                            {'graphics.color':c[j]},
                            plotdata = odsdata(
                                kt = 40,
                                kx = k,
                                usage = ['used'],
                                observer = odscounter(varname='total_used', format='bigNumber'),
                            ),
                            legend = '<total_used> (kx=<kx>)'
                        ) for j,k in enumerate(kx)
                    ],
                    has_xtitle = False,
                    has_legend = True,
                    legend = plotlegend(
                        mode = None,
                        loc = 'outside top',
                        prop = plotfont(
                            size = 'x-small',
                        ),
                        handletextpad = -0.2,
                    ),
                ),
                odssummaryplot(
                    {
                        'graphics.grid':False,
                        'xaxis.min':0,
                        'yaxis.absolute_max':1,
                        'yaxis.min':-1,
                    },
                    has_xtitle = True,
                    xtitle = 'Data Types',
                    plotdata = odsdata(
                        statistic = 'totalcount',
                        variable = 'omf',
                    ),
                    curve = [
                        stackedbar(
                            {
                                'graphics.color':c[j]
                            },
                            plotdata = odsdata(
                                usage = 'used',
                                kt = 40,
                                kx = k,
                                level = 'all',
                                name = '40',
                            )
                        ) for j,k in enumerate(kx)
                    ],
                    has_ytitle = True,
                    ytitle = 'kt=<kt>, Tb',
                ),
                odssummaryplot(
                    {
                        'graphics.grid':False,
                        'xaxis.min':0,
                    },
                    plotdata = odsdata(
                        statistic = 'totalcount',
                        variable = 'omf',
                    ),
                    curve = [
                        bar(
                            {
                                'annotation':'<name>',
                                'annotation_font.size':'x-small',
                                'graphics.color':c[j],
                            },
                            plotdata = odsdata(
                                usage = 'used',
                                kt = 40,
                                kx = k,
                                level = 'all',
                                name = str(k),
                            ),
                        ) for j,k in enumerate(kx)
                    ],
                    has_xtitle = True,
                    xtitle = 'Data Sources',
                ),
            ],
            layout = [8,8],
            layout_tiles = [
                1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,1,
                2,2,2,2,3,3,3,3,
                2,2,2,2,3,3,3,3,
            ],
            plotdata = odsdata(
                type = typ,
                domain = domain,
                expver = expver,
                date = dates,
                source = source,
                name = names[i],
            ),
            size = [10,7],
            interactive = False,
            output = output,
        )

def check(expver=None, dates=None, filename=None, output=None):
    from filehook import filehook
    # generalize these
    source = 'oper'
    database = 'ldas_ops'
    typ = 'ob'

    domain=['global']
    d = document(
        plot = [
            levelplot(
                plotdata = obsdata(
                    statistic = ['count','rms','mean'],
                    variable = 'omf',
                ),
                has_ytitle = False,
                curve = [
                    stackedbar(
                        plotdata = obsdata(usage='used',kt=40,kx=[640,641,642,643,644,645,646,647]),
                    ),
                ],
            ),
        ],
        plotdata = obsdata(
            type = typ,
            expver = expver,
            database = database,
#            filename = filename,
            date = dates,
            domain = domain,
            level = 'all',
        ),
        layout = [1,1],
        has_title = True,
        title = ['', 'SPL4SM_<expver> <month>','<name> (<domain_name>)'],
        size = [11,7],
        interactive = False,
        datahook = filehook('date','statistic','value',hidemissing=True,filename=os.path.join(output, str(dates)+'_check.txt')),
        output = os.path.join(output, str(dates)+'_chk.png')
    )

    with open(os.path.join(output, str(dates)+'_check.txt'),'r') as f:
        count, rms, mean = f.readlines()[1:]

    count = float(count.strip().split(',')[-1])
    rms = float(rms.strip().split(',')[-1])
    mean = float(mean.strip().split(',')[-1])
    print 'count >= 1000: ',count
    print 'rms > 20: ', rms
    print 'mean > 10: ',abs(mean)
    print ''

    if ((count >= 1000.) and (rms > 20. or abs(mean) > 10.)):
        with open(os.path.join(output, str(dates)+'_errs.txt'),'w') as f:
            f.write(str(dates)+' SPL4SM_'+expver+' obs2html Error Report\n')
            f.write('='*46+'\n\n')
            f.write('SMAP L4 (O-F) limits exceeded:\n')
            if count >= 1000.:
                f.write('Used Obs Count: %f\n' % count)
            if rms > 15:
                f.write('Root Mean Square: %f\n' % rms)
            if abs(mean) > 7.5:
                f.write('Abs(Mean): %f\n' % abs(mean))
            f.write('\n\n'+'-'*46+'\n')
            f.write('OPERATORS:\n')
            f.write('- Please HOLD PDRs for stream SPL4SM_'+expver+' from '+str(dates)[:-2]+', 0z.\n')
            f.write('- DO NOT EXPORT for this date/stream combination until further notice.\n')
            f.write('- Alert both OCs below.\n')
            f.write('-'*46+'\n\n')
            f.write('OCs:\n')
            f.write('Joe Ardizzone (joseph.v.ardizzone@nasa.gov)\n')
            f.write('Rolf Reichle (rolf.reichle@nasa.gov)')

        title = 'SMAP (O-F) Error: SPL4SM_'+expver+' ('+str(dates)+')'

        import subprocess
        try:
            cat = subprocess.Popen(['cat', os.path.join(output, str(dates)+'_errs.txt')], stdout=subprocess.PIPE)
            mail = subprocess.Popen(['/usr/bin/Mail', '-s '+title, 'oa@gmao.gsfc.nasa.gov'], stdin=cat.stdout, stdout=subprocess.PIPE)
            cat.stdout.close()
#            subprocess.check_output(
#                'cat '+os.path.join(output, str(dates)+'_errs.txt')+' | /usr/bin/Mail -s "'+title+'" brent.smith@nasa.gov',
#                stderr=subprocess.STDOUT, shell=True
#            )
        except subprocess.CalledProcessError, e:
            print 'mail check_output stdout: ',e.output
    try:
        os.remove(os.path.join(output, str(dates)+'_check.txt'))
        os.remove(os.path.join(output, str(dates)+'_chk.png'))
    except Exception as e:
        print 'Error cleaning up files for SMAP O-F check.'
        print e
