#!PYTHONEXE

"""configureHtml.py is a script to move documentation into /static/doc directory

Creates directory madroot/source/madpy/djangoMad/madweb/static/doc, and then copies
any files from MANIFEST doc/* to that directory

$Id: configureHtml.py 7045 2019-10-07 19:56:46Z brideout $
"""

import os, os.path, sys
import shutil

import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()
madroot = madDB.getMadroot()

docDir = os.path.join(madroot, 'source/madpy/djangoMad/madweb/static/doc')
try:
    os.makedirs(docDir)
except:
    pass

f = open(os.path.join(madroot, 'MANIFEST'))
lines = f.readlines()
f.close()
for line in lines:
    if line[0:4] == 'doc/':
        thisFile = line.strip()
        target = thisFile[4:]
        targetFile = os.path.join(madroot, thisFile)
        newDir = os.path.dirname(target)
        destDir = os.path.join(docDir, newDir)
        if len(newDir) > 0:
            try:
                os.makedirs(destDir)
            except:
                pass
        print(('copying %s to %s' % (targetFile, destDir)))
        shutil.copy(targetFile, destDir)
        