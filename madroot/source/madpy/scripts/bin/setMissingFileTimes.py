"""setMissingFileTimes.py adds date and time fields to fileTab.txt if missing for entire Madrigal database

$Id: setMissingFileTimes.py 7046 2019-10-07 19:57:14Z brideout $
"""

# standard python imports
import datetime
import os, os.path, sys

# Millstone imports
import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()
madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
madExpObj.sortByDateSite()
for i in range(madExpObj.getExpCount()):
    expDir = madExpObj.getExpDirByPosition(i)
    fileTab = os.path.join(expDir, 'fileTab.txt')
    if not os.access(fileTab, os.R_OK):
        continue
    madFileObj = madrigal.metadata.MadrigalMetaFile(madDB, fileTab)
    modified = False # keep track of whether we modified it
    for j in range(madFileObj.getFileCount()):
        filename = madFileObj.getFilenameByPosition(j)
        fullpath = os.path.join(expDir, filename)
        if not os.access(fullpath, os.R_OK):
            raise IOError('Registered file %s not found' % (fullpath))
        dt = madFileObj.getFileDatetimeByPosition(j)
        if dt is None:
            thisDT = datetime.datetime.utcfromtimestamp(os.path.getmtime(fullpath))
            madFileObj.setFileDatetimeByPosition(j, thisDT)
            modified = True
            
    if modified:
        # write new version
        madFileObj.writeMetadata()
        print(('Updated fileTab.txt in %s' % (expDir)))