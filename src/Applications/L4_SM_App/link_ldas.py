#! /usr/bin/env python

import sys
import re
import os

def isafile(f):

    if os.path.islink(f): return False
    if os.path.isfile(f): return True

    return False

def movefile(src,dst):

    if os.path.isfile(dst): os.remove(dst)
    os.rename(src,dst)

# Check Usage
# ===========

if len(sys.argv)-1 != 3:
    print("Usage: ", sys.argv[0], "[input fname] [output fname] [outdir]")
    sys.exit(1)

infile  = sys.argv[1]
outfile = sys.argv[2]
dir     = sys.argv[3]

OBSPERT = 'LANDASSIM_OBSPERTRSEED_RESTART_FILE'

# Make output directory
# =====================

if not os.path.isdir(dir): os.mkdir(dir, 0o755)

# Clean output directory
# ======================

links = os.listdir(dir)
for link in links:
    if os.path.islink(link): os.remove(link)

# Read input lines
# ================

with open(infile, 'r') as f: lines = f.readlines()

# Substitute linked files for actual files on output.
# ===================================================

with open(outfile, 'w') as f:

    for line in lines:

        result     = re.match(r'(\w+FILE):\s*(\S+)',line)
        result_alt = re.match(r'(\w+FILE):\s*(\S+)\s+(\S+)',line)

        if result:

            param     = result.group(1)
            pathname  = result.group(2)
            path      = os.path.dirname(pathname)
            name      = os.path.basename(pathname)

            srcfile = pathname
            if result_alt: srcfile = result_alt.group(3)

            if path or result_alt:

                if '%s' in pathname:

                    for i in range(0,24):

                        ens = '%04d'%(i,)
                        
                        dst     = os.path.join(dir,name)
                        dst     = dst.replace('%s','_e'+ens)
                        src     = srcfile.replace('%s',ens)
                        if os.path.islink(dst): os.remove(dst)
                        if os.path.isfile(src): os.symlink(src,dst)
                        if isafile(dst): movefile(dst,src)

                else:

                    src  = srcfile
                    dst  = os.path.join(dir,name)
                    if os.path.islink(dst): os.remove(dst)
                    if os.path.isfile(src): os.symlink(src,dst)
                    if isafile(dst): movefile(dst,src)

                wrtparam = os.path.isfile(src)
                wrtparam = wrtparam or param != OBSPERT

                if wrtparam:
                    dst = os.path.join(dir,name)
                    f.write(param + ': ' + dst  + '\n')

            else:
                f.write(line)
                
        else:

            f.write(line)

sys.exit(0)
