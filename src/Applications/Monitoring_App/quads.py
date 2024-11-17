#! /usr/bin/env python
'''
###############################################################################
[ QuADs ]: an erroneous data analyzer

Made with =[space}> by Brent Smith, 2015.
###############################################################################
'''
import os
import sys
import json
import fnmatch
import time as t

import numpy as np
try:
    import h5py as h5
except ImportError as err:
    print err
    try:
        import netCDF4 as nc4
    except ImportError as err:
        raise err
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from mpl_toolkits.basemap import Basemap

# SMAP-related imagery task
#import browse

# cmap for grey overlay
cdict = {
    'red': (
        (0.00, 0.33, 0.33), (0.01, 0.33, 0), (0.35, 0, 0),
        (0.66, 1, 1),       (0.89, 1, 1),    (0.99, 0.5, 0.33),
        (1.00, 0.33, 0.33)
    ),
    'green':(
        (0.00, 0.33, 0.33), (0.01, 0.33, 0), (0.125, 0, 0),
        (0.375, 1, 1),      (0.91, 0, 0),    (0.99, 0, 0.33),
        (1.00, 0.33, 0.33)
    ),
    'blue': (
        (0.00, 0.33, 0.33), (0.01, 0.33, 1), (0.11, 1, 1),
        (0.34, 1, 1),       (0.65, 0, 0),    (0.99, 0, 0.33),
        (1.00, 0.33, 0.33)
    ),
    'alpha':(
        (0.00, 0., 0.),     (0.01, 0., 1.0), (0.99, 1.0, 1), (1.00, 1, 1)
    )
}

###############################################################################
def find(path=os.getcwd(), ext='.h5'):
    '''Recursive search function top-down.'''
    for (root, dirs, files) in os.walk(path):
        for f in fnmatch.filter(files, '*'+ext):
            yield os.path.join(root, f)

def parse_args(args):
    '''
    ###########################################################################
    command-line arguments
    ###########################################################################
    '''
    import argparse
    parser = argparse.ArgumentParser()
    required = parser.add_argument_group('required arguments')
    required.add_argument(
        '-f', '--file', required=True, help='GEOS-5 HDF5 file(s) to analyze'
    )
    parser.add_argument(
        '-j', '--json', required=True, help='field/var json file'
    )
    parser.add_argument(
        '-o', '--path', default=os.getcwd(),
        help='output location \n\n (default: %(default)s)'
    )
    return parser.parse_args(args)
    # test file,path, and json exists

def rc():
    '''
    ###########################################################################
    creates custom mapping settings, use mpl.rcdefaults() to use default
    ###########################################################################
    '''
    mpl.rc('lines', linewidth=0.5, antialiased=True)
    mpl.rc('patch', linewidth=0.5, facecolor='348ABD', edgecolor='eeeeee',
           antialiased=True)
    mpl.rc('font', family='monospace', size=9.0)
    mpl.rc('axes', facecolor='white', edgecolor='bcbcbc', linewidth=1.0,
           grid=True, titlesize='x-large', labelsize='large', axisbelow=True,
           labelcolor='555555', color_cycle=['348ABD', '7A68A6', 'A60628',
           '467821', 'CF4457', '188487', 'E24A33'])
    mpl.rc('xtick', color='555555')
    mpl.rc('xtick.major', size=4, pad=6)
    mpl.rc('xtick.minor', size=2, pad=6)
    mpl.rc('ytick', color='555555')
    mpl.rc('ytick.major', size=4, pad=6)
    mpl.rc('ytick.minor', size=2, pad=6)
    mpl.rc('legend', fancybox=True)
    mpl.rc('figure', figsize='10.72, 6.7', dpi=72, facecolor='white')
    mpl.rc('figure.subplot', hspace=0.5, left=0.05, right=0.95, bottom=0.1,
           top=0.95)

def check(name, chk, z, data, field, d, h, path, base, args, lat, lon, mi, ma):
    '''
    ###########################################################################
    Custom script to perform output of reports and images.
    ###########################################################################
    '''
    # Tools.InitTxtFiles
    txt = os.path.join(path, 'QuADs_'+name+'.output'+d) # pid?
    html = os.path.join(path, 'QuADs_'+name+'_'+d+'.html')

    y,x = np.where(chk.mask == False)
    if not os.path.isfile(html):
        with open(html, 'a+') as file:
            out = '<html>\n'
            out += '<title> QuADs_'+name+'_'+d+' </title>\n'
            out += '<body>\n<p>\n'
            out += '<body bgcolor="#FFFFFF" text="#000000" link="#0000FF"'
            out += ' vlink="#006030">\n'
            out += '<p><center><h1><font size="6" color="#FF0000">'
            out += '"'+name+' Check" QuADs output for '+d+'\n'
            out += '</font></h1></center>\n'
            out += '<left>\n'
            file.write(out)

    png = os.path.join(path, base+'_'+field.split('/')[-1]+'_'+name+'Check_'+d+'_'+h[:2]+'Z')
    cbar = png+'_cbar'

    with open('layers.temp', 'r') as tmp:
        out = tmp.read()
    with open(png+'.html', 'w+') as file:
        tmp = png
        png = os.path.basename(png)
        file.write(out % (png, png, png, png, png, png, mi, ma))
        png = tmp

    with open(png+'.txt', 'a+') as file:
        file.write('point\ttitle\tdescription\ticon\n')

    with open(html, 'a+') as file:
        file.write('<br>Number of '+name+' points in file: '+str(int(np.size(x)))+'\n')

    mark = 0
    for i in range(np.size(x)):
        # __Write2Dreport
        out = ' '.join([
            args.file, field.split('/')[-1], '%10.4g\t'%data[y[i],x[i]],
            '(%6.4g %6.4g)'%(mi,ma), d,
            '[%dz][%6.2f][%6.2f]\n'%(int(h[:2]),lat[y[i],x[i]], lon[y[i],x[i]])
        ])
        with open(txt, 'a+') as file:
            file.write(out)
        if mark < 100:
            with open(html, 'a+') as file:
                file.write('<br>%.2d- '%(i)+out)
            with open(png+'.txt', 'a+') as file:
                out = '%6.2f,%6.2f\t'%(lat[y[i],x[i]],lon[y[i],x[i]])
                out += base+'\t'
                out += 'Latitude  = %6.4g<br>' % lat[y[i],x[i]]
                out += 'Longitude = %6.4g<br>' % lon[y[i],x[i]]
                out += 'Value     = %g<br>' % data[y[i],x[i]]
                out += name+'Check:<br>'
                out += '&nbsp;&nbsp;Min=%6.4g<br>' % mi
                out += '&nbsp;&nbsp;Max=%6.4g<br>' % ma
                out += 'Time = %.2d:%.2dZ\n' % (int(h[:2]), int(h[2:4]))
                file.write(out)
            mark += 1

    with open(txt, 'a+') as file:
        file.write('\n<br><a href="./'+os.path.basename(png)+'.html">'
                   + 'SEE IMAGE --'+' '*24+os.path.basename(png)+'</a>'
                   + '<br>'*3+'<hr>\n')
    with open(html, 'a+') as file:
        file.write('<br><a href="./'+os.path.basename(png)+'.html">'
                   + 'SEE IMAGE --'+' '*24+os.path.basename(png)+'</a>'
                   + '<br>'*3+'<hr>\n')

    # base
    fig = plt.figure(3, figsize=(15,4.76), dpi=100)
    fig.clf()
    plt.axes([0,0,1,1], frameon=False)
    plt.axis('off')
    ran = np.ma.fix_invalid(data, copy=1)
    im = plt.imshow(ran, cmap='jet', aspect='auto',
                    interpolation='nearest',
                    norm=colors.Normalize(vmin=mi, vmax=ma, clip=True))
    plt.savefig(png+'.png', transparent=True, dpi=100)
    plt.close()

    # cbar
    fig = plt.figure(4, figsize=(10,1), dpi=100)
    fig.clf()
    plt.axes([0.02, 0.5, 0.95, 1], frameon=False)
    plt.axis('off')
    delta = (ma-mi)/100.
    a = np.outer(np.arange(mi,ma,delta),1).T
    plt.pcolormesh(chk)
    plt.gca().set_visible(False)
    cmap = mpl.colors.LinearSegmentedColormap('my_quads', cdict)
    cb = plt.colorbar(cmap=cmap, orientation='horizontal', pad=0,
                 aspect=100)
    cb.set_label('[ '+z[field].attrs.get('units')+' ]', fontsize=10)
    plt.savefig(cbar+'.png', transparent=True, dpi=100)
    plt.close()

    # check
    fig = plt.figure(1, figsize=(15,4.76), dpi=100)
    fig.clf()
    ax = plt.axes([0,0,1,1], frameon=False)
    plt.axis('off')
    chk[chk.mask == False] = 0
    chk[chk.mask == True] = 1
    im = plt.imshow(chk, interpolation='nearest', cmap=cmap,
                    aspect='auto', vmin=0, vmax=1)
    plt.savefig(png+'_'+name+'.png', transparent=True, dpi=100)
    plt.close()

    if os.path.isfile(html):
        with open(html, 'a+') as file:
            file.write('</body>\n</html>')

def main():
    '''
    ###########################################################################
    main driver to get data, call checks, and produce maps/reports
    ###########################################################################
    '''
    args = parse_args(sys.argv[1:])
    #for f in sorted(x for x in find(os.path.abspath(os.path.dirname(args.file)))):
    #print(f)
    ####################################
    # file-specific pre-processing steps
    ####################################
    f = os.path.abspath(args.file)
    base = os.path.basename(f)

    d = base.split('_')[4].split('T')[0]
    h = base.split('_')[4].split('T')[1]
    print base

    try:
        z = h5.File(f, 'r')
    except:
        z = nc.Dataset(f, 'r')

    fields = json.load(open(args.json))
    ###########################################################################
    rc()

    for field in sorted(field for field in fields.keys() if field in z):
        dims = z[field].shape
        coords = z[field].attrs['coordinates'].split(' ')
        if 'lat' in coords[0]:
            lat = z[coords[0]][:]
            lon = z[coords[1]][:]
        else:
            lat = z[coords[1]][:]
            lon = z[coords[0]][:]
        
        if len(dims) > 2:
            return
        del(dims)

        
        if fields[field]:
            mi, ma = fields[field]
        else:
            # obtain min/max from granule
            mi = z[field].attrs['valid_min']
            ma = z[field].attrs['valid_max']
            if type(mi) is np.ndarray:
                mi = mi[0]
                ma = ma[0]
        print(' {0:50} {1:s}'.format(field, [mi, ma]))

        # data-specific steps
        try:
            data = z[field][:]
        except:
            continue
        try:
            fill = z[field].fillvalue
        except:
            fill = z[field].attrs['missing_value']

        ############
        # Main Check
        ############
        #ind = data[
        #    (data != fill) & (data <= ma) & (data >= mi) &
        #    (data != np.nan) & (data != 0.)
        #]
        #if ind.any():
        #    continue

        # constant field
        if len(np.unique(data)) == 1:
            continue

        data = np.ma.masked_values(data, fill)
        #data = data[(data != fill)] # flattens array, 1d and no mask

        # setup dirs
        path = os.path.join(args.path, 'Y'+d[:4]+'/M'+d[4:6]+'/D'+d[6:8])
        if not os.path.exists(path):
            os.makedirs(path, 0777)

        #############
        # Range Check
        #############
        if data[((data > ma) | (data < mi))].any():
            print('  Range Check failed: %02d:%02d'%(int(h[:2]), int(h[2:4])))

            ran = np.ma.masked_inside(data, mi, ma, copy=1)
            chk = check('Range', ran, z, data, field, d, h, path, base, args, lat, lon, mi, ma)

        ###########
        # NaN Check
        ###########
        if np.isnan(data).any():
            print('  NaN Check failed: %02d:%02d'%(int(h[:2]), int(h[2:4])))

            chk = check('NaN', data, z, data, field, d, h, path, base, args, lat, lon, mi, ma)
    z.close()

if __name__ == '__main__':
    sys.exit(main())
