import os, os.path, sys
import types
import getopt
import datetime

import madrigal.metadata
import madrigal.cedar

"""removeMadRecordsAfter.py removes all Madrigal records from a given
Madrigal file after the input time.

$Id: removeMadRecordsAfter.py 7046 2019-10-07 19:57:14Z brideout $
"""

usage = """python removeMadRecordsAfter.py --madfile=<original filename> --newfile=<new filename>
    --year=YYYY --month=MM --day=DD [--hour=HH] [--minute=MM] [--second=SS]"""

# parse command line
arglist = ''
longarglist = ['madfile=',
               'newfile=',
               'year=',
               'month=',
               'day=',
               'hour=',
               'minute=',
               'second=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

# default values
madfile = None # required
newfile = None # required
year = None # required
month = None # required
day = None # required
hour = 0
minute = 0
second = 0

for opt in optlist:
    if opt[0] == '--madfile':
        madfile = opt[1]
    elif opt[0] == '--newfile':
        newfile = opt[1]
    elif opt[0] == '--year':
        year = int(opt[1])
    elif opt[0] == '--month':
        month = int(opt[1])
    elif opt[0] == '--day':
        day = int(opt[1])
    elif opt[0] == '--hour':
        hour = int(opt[1])
    elif opt[0] == '--minute':
        minute = int(opt[1])
    elif opt[0] == '--second':
        second = int(opt[1])
        
    else:
        raise ValueError('Illegal option %s\n%s' % (opt[0], usage))

# check that all required arguments passed in
if madfile == None:
    print('--madfile argument required - must be name of existing madrigal file')
    print(usage)
    sys.exit(0)

if newfile == None:
    print('--newfile argument required - must be name of madrigal file to be created')
    print(usage)
    sys.exit(0)

if year == None:
    print('--year argument required')
    print(usage)
    sys.exit(0)

if month == None:
    print('--month argument required')
    print(usage)
    sys.exit(0)

if day == None:
    print('--day argument required')
    print(usage)
    sys.exit(0)

cutoffTime = datetime.datetime(year, month, day, hour, minute, second)

metaObj = madrigal.metadata.MadrigalDB()

# read the Madrigal file into memory
cedarObj = madrigal.cedar.MadrigalCedarFile(madfile)

# loop through each record, until record beyond cutoff found
for index in range(len(cedarObj)):
    record = cedarObj[index]
    # skip header and catalog records
    if record.getType() == 'data':
        # create startTime
        stList = record.getStartTimeList()
        st = datetime.datetime(stList[0],
                               stList[1],
                               stList[2],
                               stList[3],
                               stList[4],
                               stList[5])
        if st > cutoffTime:
           del cedarObj[index:]
           break

# write edited file
cedarObj.write('Madrigal', newfile)
