#!PYTHONEXE

#$Id: updateFileInExp.py 7046 2019-10-07 19:57:14Z brideout $

usage = """
updateFileInExp.py is a script used to update an existing Madrigal file in an
existing experiment.  Information such as the duration of the
experiment is updated by analyzing the file.  This script is use to replace
an existing Madrigal file.  Use addFileToExp.py to add a new file, and
changeFileStatus.py to change any file attribute.

Required arguments:

    --madFilename - full path to the new version of the Madrigal file. Basename will
                    be maintained.

    --expDir - full path to experiment directory. Example:
               "/opt/madrigal/experiments/1998/mlh/20jan98"
               
    --skipNotify - if this flag set (no arguments), registered users will NOT be notified
                   of this change.  The default is to email all registered users.
"""

import sys
import os, os.path
import getopt
import traceback

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['madFilename=',
               'expDir=',
               'skipNotify']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
madFilename = None
expDir = None
notify = True

for opt in optlist:
    if opt[0] == '--madFilename':
        madFilename = opt[1]
    elif opt[0] == '--expDir':
        expDir = opt[1]
    elif opt[0] == '--skipNotify':
        notify = False
    else:
        raise ValueError('Illegal option %s\n%s' % (opt[0], usage))

# check that all required arguments passed in
if madFilename == None:
    print('--madFilename argument required - must be full path to madrigal file')
    print(usage)
    sys.exit(0)

if expDir == None:
    print('--expDir argument required - must be full path to experiment directory')
    sys.exit(0)


adminObj = madrigal.admin.MadrigalDBAdmin()

adminObj.overwriteMadrigalFile(expDir,
                               madFilename,
                               notify=notify)

print('File %s successfully updated in experiment at %s' % (madFilename, expDir))
