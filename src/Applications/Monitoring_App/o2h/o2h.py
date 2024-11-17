#! /usr/bin/env python
'''
[o2h]: obs2html

ODS file sole dependence
'''
import os
import sys
import plots
import compute
import web
from gmaopy.modules.obsstat import Dates

def parse_args(args):
    '''command-line argument parser'''

    # need to add the following:
    # database name
    # location of ods/obs files

    import argparse
    parser = argparse.ArgumentParser()
    required = parser.add_argument_group('required arguments')

    # required
    required.add_argument(
        '-e', '--exp', required=True, help='Experiment Name'
    )
    required.add_argument(
        '-d', '--date', required=True,
        help='examples:\n\n1 syn-time: 1980010100\n\n1 day: 19800101'
             +'\n\nrange: 1980010100,1980010500'
    )
    required.add_argument(
        '-f', '--file', required=True,
        help='kt/kx definition file'
    )

    parser.add_argument(
        '-z', '--hertz', default=3,
        help='frequency of synoptic times'
    )
    parser.add_argument(
        '-o', '--output_path', default=os.getcwd(),
        help='Path for output imagery and files.'
    )
    parser.add_argument(
        '-t', '--template_path', default='',
        help='Path to template files.'
    )

    args = parser.parse_args(args)
    return args

def permissions(path=None):
    for r,d,f in os.walk(path):
        for m in d:
            os.chmod(os.path.join(r, m), 0o755)
        for m in f:
            os.chmod(os.path.join(r, m), 0o755)

if __name__ == '__main__':
    args = parse_args(sys.argv[1:])

    date, freq = args.date, args.hertz
    # determine period range
    if date and freq:
        # date range
        if ',' in date:
            dates = Dates(date.split(',')[0], date.split(',')[1], int(freq))
        # full day
        elif len(date) < 9:
            dates = Dates(date+'00', date+str(24-int(freq)), int(freq))
        # single synoptic time
        else:
            dates = [date]
    else:
        print 'Date argument incorrect input.'

    if not os.path.exists(args.output_path):
        print 'Output path does not exist.'
        sys.exit()
    args.output_path = os.path.abspath(args.output_path)

    # compute stats & db insert
    try:
        compute.insert_db(expver=args.exp, dates=dates, filename=args.file)
    except Exception as e:
        print e
        rc = 1

    # make output structure
    path = os.path.join(args.output_path, args.exp)
    if not os.path.exists(path):
        permissions(args.output_path)
        os.mkdir(path)
    for d in dates:
        y = 'Y%02d' % int(str(d)[:6][:4])
        m = 'M%02d' % int(str(d)[:6][4:6])
        yr = os.path.join(path, y)
        if not os.path.exists(yr):
            permissions(args.output_path)
            os.mkdir(yr)
        mn = os.path.join(yr, m)
        if not os.path.exists(mn):
            permissions(args.output_path)
            os.mkdir(mn)
        cov = os.path.join(mn, 'cov')
        if not os.path.exists(cov):
            permissions(args.output_path)
            os.mkdir(cov)

    # O-F check & plots
#   try:
    plots.plot(expver=args.exp, dates=dates, filename=args.file, output=path)
#   except Exception as e:
#       print e
#       rc = 1

    permissions(args.output_path)

    try:
        web.make(expver=args.exp, dates=dates, output=path, template=args.template_path)
    except Exception as e:
        print e
        rc = 1
    permissions(args.output_path)
