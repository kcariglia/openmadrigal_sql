"""rebuildSummaries.py rebuilds all summary information in the Madrigal database

$Id: rebuildSummaries.py 7046 2019-10-07 19:57:14Z brideout $
"""

import madrigal.metadata
import madrigal.data

madDBObj = madrigal.metadata.MadrigalDB()

madrigalFiles = madDBObj.getFileList()

for i, madrigalFile in enumerate(madrigalFiles):
    print(('Working on file %i of %i: %s' % (i, len(madrigalFiles), madrigalFile)))
    madrigal.data.MadrigalFile(madrigalFile, madDBObj, forceRefresh=True)