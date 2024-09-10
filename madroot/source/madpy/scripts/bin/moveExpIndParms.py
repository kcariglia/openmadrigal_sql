"""moveExpIndParms.py is a script to move exps with ind parm issues to a separate Madrigal site for repair

$Id: moveExpIndParms.py 7046 2019-10-07 19:57:14Z brideout $
"""

# standard python imports
import os, os.path, sys
import time
import shutil
import subprocess
import argparse
import datetime
import configparser
import types
import traceback

# third party imports
import numpy
import h5py

# Madrigal imports
import madrigal.metadata
import madrigal.cedar
import madrigal.data


def checkFile(filename):
    """checkFile checks a CEDAR Madrigal HDF5 file for indParm issues
    
    Input: filename - Madrigal CEDAR Hdf5 to check
    
    Returns 0 if no 2D problems, 1 if has 2D parms and indParms, but no Array Layout,
        and 2 if has 2D parms but no indParms.
    """
    cedarObj = madrigal.cedar.MadrigalCedarFile(filename, maxRecords=2)
    if len(cedarObj.getIndSpatialParms()) == 0:
        if len(cedarObj.get2DParms()) > 0:
            print(('Found 2D parms %s' % (str(cedarObj.get2DParms()))))
            return(2)
    else:
        # check whether it has an array layout
        cmd = 'h5ls %s/Data' % (filename)
        result = subprocess.check_output(cmd.split())
        if result.find('Array') == -1:
            return(1)
        
    # no issues found
    return(0)



        
    
        
                
# script begins here
if __name__ == '__main__': 
    
    parser = argparse.ArgumentParser(description='moveExpIndParms.py move exps with ind parm issues to a separate Madrigal site for repair')
    parser.add_argument('--dest',  help='exp directory to copy the expDir to')
    parser.add_argument('--kinst', type=int, default=0, help='only work on files with a given kinst.  Default is all')
    parser.add_argument('--startDT', default=None, help='Only examine files after startDT YYYY-MM-DD')
    parser.add_argument('--endDT', default=None, help='Only examine files before endDT YYYY-MM-DD')
    
    args = parser.parse_args()
    
    # verify args
            
    if not args.startDT is None:
        args.startDT = datetime.datetime.strptime(args.startDT, '%Y-%m-%d')
    else:
        args.startDT = datetime.datetime(1950,1,1)
        
    if not args.endDT is None:
        args.endDT = datetime.datetime.strptime(args.endDT, '%Y-%m-%d')
    else:
        args.endDT = datetime.datetime.now()
        
    # make sure exp dir writable
    if not os.access(args.dest, os.W_OK):
        raise IOError('Cannot write to %s' % (args.dest))
    

        
    # walk through all selected files
    expTested = 0
    movedList = []
    madDB = madrigal.metadata.MadrigalDB()
    madroot = madDB.getMadroot()
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
        kinst = madExpObj.getKinstByPosition(i)
        if not args.kinst == 0:
            if kinst != args.kinst:
                continue
        
        # all filters passed - work on this exp
        expTested += 1
        expDir = madExpObj.getExpDirByPosition(i)
        print(('Working on directory %s' % (expDir)))
        
        try:
            madFileObj = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            print('no fileTab.txt')
            continue
        for j in range(madFileObj.getFileCount()):
            basename = madFileObj.getFilenameByPosition(j)
            filename = os.path.join(expDir, basename)
            file_basename, file_extension = os.path.splitext(basename)
            if not file_extension in ('.h5', '.hdf5', '.hdf'):
                result = 1 # this expDir not yet converted - copy it
            else:
                result = checkFile(filename)
            if result == 0:
                print(('File %s passed indParm test' % (filename)))
            else:
                # we need to move the whole experiment
                # strip experiments from expDir
                index = expDir.find('experiments')
                index2 = expDir[index:].find('/')
                subDir = expDir[index + index2 + 1 :]
                targetDir = os.path.join(args.dest, os.path.dirname(subDir))
                movedList.append(expDir)
                if not os.access(targetDir, os.W_OK):
                    os.makedirs(targetDir)
                try:
                    shutil.copytree(expDir, os.path.join(targetDir, os.path.basename(expDir)), True)
                except:
                    traceback.print_exc()
                print(('Copied %s to %s' % (expDir, os.path.join(targetDir, os.path.basename(expDir)))))
                break
                    
                 
    print('List of all copied experiments:')
    for f in movedList:
        print(f)
    print(('%i of %i copied' % (len(movedList), expTested)))
            
        
    
    
            
    
    