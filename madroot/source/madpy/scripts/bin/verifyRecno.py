"""verifyRecno.py is a script to verify recno column is correct in a CEDAR Madrigal HDF5 file.

$Id: verifyRecno.py 7047 2019-10-07 19:58:21Z brideout $
"""

# standard python imports
import os, os.path, sys
import time
import shutil
import subprocess
import argparse
import datetime

# third party imports
import h5py
import numpy

# Madrigal imports
import madrigal.metadata

def is_sorted(a):
    for i in range(len(a) - 1):
         if a[i+1] < a[i]:
             print(('out of order at line %i' % (i)))
             return(False)
    return(True)


def checkFile(filename):
    """checkFile checks a CEDAR Madrigal HDF5 file for recno issues
    
    Input: filename - Madrigal CEDAR Hdf5 to check
    
    Returns True if recno okay, False if needs resetting
    """
    with h5py.File(filename) as f:
        table = f['Data']['Table Layout']
        recno = table['recno']
        if not is_sorted(recno):
            print('recno not sorted - unfixable')
            return(True)
        ut1_unix = table['ut1_unix']
        if not is_sorted(ut1_unix):
            print('ut1_unix not sorted - unfixable')
            return(True)
        ut2_unix = table['ut2_unix']
        if not is_sorted(ut2_unix):
            print('ut2_unix not sorted - unfixable')
            return(True)
        unique_recno = numpy.unique(recno)
        unique_ut1_unix = numpy.unique(ut1_unix)
        if len(unique_recno) != len(unique_ut1_unix):
            return(False)
        else:
            return(True)
        

def redoRecno(filename):
    f = h5py.File(filename, 'r+')
    table = f['Data']['Table Layout']
    recno = table['recno']
    new_recno = numpy.zeros(recno.shape, dtype=recno.dtype)
    ut1_unix = table['ut1_unix']
    # reset recno
    last_ut1_unix = None
    last_recno = None
    for i in range(len(recno)):
        if i == 0:
            last_ut1_unix = ut1_unix[i]
            last_recno = 0
        elif ut1_unix[i] != last_ut1_unix:
            last_recno += 1
            last_ut1_unix = ut1_unix[i]
        new_recno[i] = last_recno
            
    table['recno'] = new_recno
    f.close()
    
    # also reset summary file timestamp if it exists
    summaryFile = os.path.join(os.path.dirname(filename), 'overview', os.path.basename(filename) + '.summary')
    if os.access(summaryFile, os.R_OK):
        subprocess.check_call(['touch', summaryFile])
        
    
        
                
# script begins here
if __name__ == '__main__': 
    
    parser = argparse.ArgumentParser(description='verifyRecno.py checks and possibly corrects recno issues')
    parser.add_argument('--file', default=None, help='check a single file only')
    parser.add_argument('--fix', action='store_true', help='set fix to fix files. Default is to only report issues')
    parser.add_argument('--kinst', type=int, default=0, help='only work on files with a given kinst.  Default is all')
    parser.add_argument('--kindat', type=int, default=0, help='only work on files with a given kindat.  Default is all')
    parser.add_argument('--startDT', default=None, help='Only examine files after startDT YYYY-MM-DD')
    parser.add_argument('--endDT', default=None, help='Only examine files before endDT YYYY-MM-DD')
    
    args = parser.parse_args()
    
    # verify args
    if not args.file is None:
        if not os.access(args.file, os.R_OK):
            raise ValueError('Could not open file %s' % (args.file))
            
    if not args.startDT is None:
        args.startDT = datetime.datetime.strptime(args.startDT, '%Y-%m-%d')
        
    if not args.endDT is None:
        args.endDT = datetime.datetime.strptime(args.endDT, '%Y-%m-%d')
        
    # handle single file
    if not args.file is None:
        result = checkFile(args.file)
        if result:
            print(('File %s passed recno test' % (args.file)))
        else:
            print(('File %s failed recno test' % (args.file)))
            if args.fix:
                redoRecno(args.file)
                print('Fixed now')
                
        # all other arguments ignored
        sys.exit(0)
        
    # walk through all selected files
    madDB = madrigal.metadata.MadrigalDB()
    madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
    madExpObj.sortByDateSite()
    for i in range(madExpObj.getExpCount()):
        expStart = madExpObj.getExpStartDateTimeByPosition(i)[0:6]
        expStartDT = datetime.datetime(*expStart)
        if not args.endDT is None:
            if expStartDT > args.endDT:
                # no more exos to examine 
                break
        if not args.startDT is None:
            if expStartDT < args.startDT:
                continue
        if not args.kinst == 0:
            kinst = madExpObj.getKinstByPosition(i)
            if kinst != args.kinst:
                continue
        
        # all filters passed - work on this exp
        expDir = madExpObj.getExpDirByPosition(i)
        print(('Working on directory %s' % (expDir)))
        
        madFileObj = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
        for j in range(madFileObj.getFileCount()):
            if not args.kindat == 0:
                if args.kindat != madFileObj.getKindatByPosition(j):
                    continue
            basename = madFileObj.getFilenameByPosition(j)
            filename = os.path.join(expDir, basename)
            result = checkFile(filename)
            if result:
                print(('File %s passed recno test' % (filename)))
            else:
                print(('File %s failed recno test' % (filename)))
                if args.fix:
                    redoRecno(filename)
                    print('Fixed now')
            
        
    
    
            
    
    