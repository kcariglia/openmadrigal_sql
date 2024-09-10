""" configureSource.py loops over a small number of source files that need hard-coded paths:
    cedar.h, and isrim.f in the Madrigal distribution and edits them
    to use the site-specific definitions in madrigal.cfg.

    $Id: configureSource.py 7338 2021-03-26 14:57:40Z brideout $

"""
# standard python imports
import os, sys, os.path
import fnmatch
import shutil

# set MADROOT from environment
madrootval = os.environ['MADROOT']

f = open('MANIFEST')
lines = f.readlines()
f.close()

for line in lines:
    filename = line.strip()
    basename = os.path.basename(filename)
    
    if basename in ('isrim.f', 'irifun.f', 'irisub.f', 'cedar.h'):
        # replace MADROOT with real value
        f = open(filename)
        text = f.read()
        f.close()
        text = text.replace('MADROOT', madrootval)
        f = open(filename, 'w')
        f.write(text)
        f.close()
        print('Modified MADROOT in %s' % (filename))
        
    elif fnmatch.fnmatch(basename, "*ccir*.asc*") or fnmatch.fnmatch(basename, "*ursi*.asc*"):
        shutil.copy(filename, os.path.join(madrootval, 'bin'))
        print('Installing %s' % (filename))
        

