"""checkMadrigalTimesIncrease.py prints out any points where Madrigal records don't increase in
time even after being split with arraySplitParms.

$Id: checkMadrigalTimesIncrease.py 7230 2020-09-16 14:36:49Z brideout $
"""

# standard python imports
import os, sys, os.path
import argparse

# Madrigal imports
import madrigal.cedar

# script begins here
if __name__ == '__main__': 
    
    parser = argparse.ArgumentParser(description='checkMadrigalTimesIncrease.py prints out any points where Madrigal records do not increase in time.')
    parser.add_argument('file', help='Madrigal Hdf5 file to check')
    parser.add_argument('--splitParm', action='append', help='parm to split data with.  More than one allowed')

    args = parser.parse_args()
    
    dataDict = {}  # this dictionary is used to split data.  key = tuple of (splitParm, value) for each split parm,
                   # Value = list of start datetimes found so far
                   
    errCount = 0
                   
    cedarObj = madrigal.cedar.MadrigalCedarFile(args.file)
    for i, rec in enumerate(cedarObj):
        if rec.getType() != 'data':
            continue
        # get key
        key = []
        for splitParm in args.splitParm:
            value = rec.get1D(splitParm)
            key.append((splitParm, value))
        key = tuple(key)
        thisUt1 = rec.getStartDatetime()
        if key not in dataDict:
            dataDict[key] = [thisUt1]
        else:
            # check that times increase
            if thisUt1 <= dataDict[key][-1]:
                print('Time error.  Key=%s, ut1=%s, recno=%i' % (str(key), str(thisUt1), i))
                errCount += 1
            dataDict[key].append(thisUt1)
            
    if errCount == 0:
        print('No problems found')
    else:
        print('%i problems found' % (errCount))
            
            