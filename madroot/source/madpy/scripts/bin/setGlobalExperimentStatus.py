#!PYTHONEXE

""" setGlobalExperimentStatus.py is a script to set all local experiments to an experiment status

Will not change experiments with status -1

$Id: setGlobalExperimentStatus.py 7046 2019-10-07 19:57:14Z brideout $
"""

import sys
import os
import traceback

# catch any exception, and write an appropriate message admin
try:

    import madrigal.admin
    
except ImportError:
    
    # Fatal error - madpy library not found
    print("Unable to import the madrigal python library - please alert the sys admin!")
    sys.exit(0)



import madrigal.metadata

# create MadrigalDB obj
madDBObj = madrigal.metadata.MadrigalDB()
madAdminObj = madrigal.admin.MadrigalDBAdmin(madDBObj)


# check number, type of arguments
if len(sys.argv) != 2:
    print('usage: setGlobalExperimentStatus.py <status>')
    sys.exit(0)
    
newStatus = int(sys.argv[1])

if not newStatus in (0,1,2,3):
    print('newStatus must be 0,1,2 or 3, not %i' % (newStatus))
    sys.exit(0)


# loop through all Madrigal Experiments
madExpObj = madrigal.metadata.MadrigalExperiment(madDBObj)

for i in range(madExpObj.getExpCount()):
    thisDir = madExpObj.getExpDirByPosition(i)
    thisSecurity = madExpObj.getSecurityByPosition(i)
    if thisSecurity < 0:
        print(('skipping hidden directory %s' % (thisDir)))
        continue
    madAdminObj.changeExpStatus(thisDir, security=newStatus)
    print(('Set experiment %s' % (thisDir)))
    



