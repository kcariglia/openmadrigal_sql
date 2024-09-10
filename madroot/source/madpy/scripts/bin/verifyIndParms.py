"""verifyIndParms.py is a script to verify indParms set whenever there is 2D parms.

$Id: verifyIndParms.py 7047 2019-10-07 19:58:21Z brideout $
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
import madrigal.admin



def parseCachedIni(kinst, kindat, madroot):
        """parseCachedIni parses an ini file for information needed to create cached files
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
        
            kinst - the instrument kinst (integer)
            
            kindat - the data kindat (integer)
            
            iniFile - the ini file to use.  If None, uses default ini file $MADROOT/cachedFiles.ini
            
        Returns: a tuple with two items:
            1. a list of extra parameters (string mnemonics)
            2. a list of independent parms, or None if None
            
        Algorithm:
        
        1. If iniFile == None and no default file, returns ([], {})
        2. Searches ini file for section [%i] % (kinst).  If not found, returns ([], {})
        3. Searches right section for key %i_parms % (kindat).  If not found, searches for default_parms.
            If not found, extra parameters are []
        4. Searches right section for key %i_formats % (kindat).  If not found, searches for default_formats.
            If not found, alternate format dictionary is {}
        """
        thisIniFile = os.path.join(madroot, 'cachedFiles.ini')
            
        instSection = '%i' % (kinst)
            
        parser = configparser.SafeConfigParser()
        parser.read(thisIniFile)
        if not parser.has_section(instSection):
            raise ValueError('Need to add kinst %i to cachedFiles.ini' % (kinst))
        extraParms = []
        formatDict = {}
        skipArray = False
        
        # get extra parms
        if parser.has_option(instSection, '%i_parms' % (kindat)):
            extraParms = parser.get(instSection, '%i_parms' % (kindat))
            extraParms = extraParms.split(',')
        elif parser.has_option(instSection, 'default_parms'):
            extraParms = parser.get(instSection, 'default_parms')
            extraParms = extraParms.split(',')
            
        # make sure no empty parms snuck in
        finalExtraParms = []
        for extraParm in extraParms:
            if len(extraParm.strip()) > 0:
                finalExtraParms.append(extraParm.strip())
        
        # get format dict
        if parser.has_option(instSection, '%i_formats' % (kindat)):
            formatDict = parser.get(instSection, '%i_formats' % (kindat))
            formatDict = eval(formatDict)
        elif parser.has_option(instSection, 'default_formats'):
            formatDict = parser.get(instSection, 'default_formats')
            formatDict = eval(formatDict)
            
        # get skipArray
        if parser.has_option(instSection, '%i_skip_array' % (kindat)):
            if parser.get(instSection, '%i_skip_array' % (kindat)) in ('True', 'true', '1'):
                skipArray = True
        elif parser.has_option(instSection, 'default_skip_array'):
            if parser.get(instSection, 'default_skip_array') in ('True', 'true', '1'):
                skipArray = True
            
        try:
            return((finalExtraParms, formatDict['array'], skipArray))
        except:
            traceback.print_exc()
            raise IOError('problem find kinst %i, kindat %i' % (kinst, kindat))




def checkFile(filename, kinst, kindat, madroot):
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
        requiredParms, indParmList, skipArray = parseCachedIni(kinst, kindat, madroot)
        if skipArray:
            return(0)
        # check whether it has an array layout
        cmd = 'h5ls %s/Data' % (filename)
        result = subprocess.check_output(cmd.split())
        if result.find('Array') == -1:
            return(1)
        
    # no issues found
    return(0)


def redoIndParms(filename, kinst, kindat, madroot, parmObj, fix=True):
    requiredParms, indParmList, skipArray = parseCachedIni(kinst, kindat, madroot)
    if type(indParmList) == bytes:
        indParmList = [indParmList]
    elif type(indParmList) in (tuple, list):
        if len(indParmList) == 2:
            if type(indParmList[0]) in (tuple, list):
                indParmList = indParmList[0]
    if len(indParmList) == 0:
        raise ValueError('Got no ind parms for kinst %i, kindat %i' % (kinst, kindat))
    cedarObj = madrigal.cedar.MadrigalCedarFile(filename, maxRecords=2)
    twoDParms = cedarObj.get2DParms()
    # verify all indParms in 2DParms
    for indParm in indParmList:
        if indParm not in twoDParms:
            raise IOError('indParm %s not found in 2d parms for file %s' % (str(indParm), filename))
        
    if not fix:
        return
    
    # go ahead and fix the dffile
    f = h5py.File(filename, 'r+')
    
    metaGroup = f['Metadata']
    if 'Independent Spatial Parameters' in list(metaGroup.keys()):
        del metaGroup['Independent Spatial Parameters']
    
    # create new dataset
    longestMnemStr = 0
    longestDescStr = 0
    for parm in indParmList:
        if len(parm) + 1 > longestMnemStr:
            longestMnemStr = len(parm) + 1
        desc = parmObj.getSimpleParmDescription(parm)
        if len(desc) + 1 > longestDescStr:
            longestDescStr = len(desc) + 1
    indSpatialArr = numpy.recarray((len(indParmList),),
        dtype = [('mnemonic', '|S%i' % (longestMnemStr)),
                 ('description', '|S%i' % (longestDescStr))])
    
    for i, parm in enumerate(indParmList):
        indSpatialArr['mnemonic'][i] = parmObj.getParmMnemonic(parm)
        indSpatialArr['description'][i] = parmObj.getSimpleParmDescription(parm)
    metaGroup.create_dataset('Independent Spatial Parameters', data=indSpatialArr)
    
    # now modify _record_layout
    columnNames = cedarObj.getRecDType().names
    recLayoutDSet = metaGroup['_record_layout']
    newRecDSet = numpy.zeros(recLayoutDSet.shape, dtype=recLayoutDSet.dtype)
    for colname in columnNames:
        if colname in indParmList:
            newRecDSet[colname] = 3
        else:
            newRecDSet[colname] = recLayoutDSet[colname]
            
    # now overwrite
    metaGroup['_record_layout'][...] = newRecDSet
    
    f.close()
    
    # also reset summary file timestamp if it exists
    summaryFile = os.path.join(os.path.dirname(filename), 'overview', os.path.basename(filename) + '.summary')
    if os.access(summaryFile, os.R_OK):
        subprocess.check_call(['touch', summaryFile])
    
        
    
        
                
# script begins here
if __name__ == '__main__': 
    
    parser = argparse.ArgumentParser(description='verifyRecno.py checks indParms issues')
    parser.add_argument('--fix', action='store_true', help='set fix to fix files. Default is to only report issues')
    parser.add_argument('--delete', action='store_true', help='set delete to delete files to be fixed at next import. Default is to only report issues')
    parser.add_argument('--kinst', type=int, default=0, help='only work on files with a given kinst.  Default is all')
    parser.add_argument('--kindat', type=int, default=0, help='only work on files with a given kindat.  Default is all')
    parser.add_argument('--startDT', default=None, help='Only examine files after startDT YYYY-MM-DD')
    parser.add_argument('--endDT', default=None, help='Only examine files before endDT YYYY-MM-DD')
    parser.add_argument('--includeCorrupt', action='store_true', help="If set, do not skip exps and kinst listed as corrupt.  Default is to skip them.")
    
    args = parser.parse_args()
    
    corrupt_kinst_list = [211, 1560,71,73,6300,1320,8001,8002] # files have corruption beyond this problem
    corrupt_file_list = ['/opt/cedar/experiments3/madrigal3_tmp/experiments/1984/kir/29aug84/08290700.kra.hdf5',
                         '/opt/cedar/experiments3/madrigal3_tmp/experiments/1984/sod/29aug84/08290700.sra.hdf5',
                         '/opt/cedar/experiments3/madrigal3_tmp/experiments/1984/sod/30aug84/08300100.sra.hdf5',
                         '/opt/cedar/experiments3/madrigal3_tmp/experiments/2011/pkf/06may11/pkf110506.557.001.hdf5',
                         '/opt/cedar/experiments3/madrigal3_tmp/experiments/2011/pkf/06may11/pkf110506.630.001.hdf5']
    
    corrupt_kinst_kindat_list = [(30,13210),
                                 (6320,17087),
                                 (80, 15520),
                                 (80, 19120),
                                 (95, 6801),]
    
    if args.includeCorrupt:
        corrupt_kinst_list = [211,]
        corrupt_file_list = []
        corrupt_kinst_kindat_list = []
    
    
    # verify args
            
    if not args.startDT is None:
        args.startDT = datetime.datetime.strptime(args.startDT, '%Y-%m-%d')
    else:
        args.startDT = datetime.datetime(1950,1,1)
        
    if not args.endDT is None:
        args.endDT = datetime.datetime.strptime(args.endDT, '%Y-%m-%d')
    else:
        args.endDT = datetime.datetime.now()
        
    if args.fix and args.delete:
        raise ValueError('Cannot set both fix and delete - exclusive.')

        
    # walk through all selected files
    failedList = []
    madDB = madrigal.metadata.MadrigalDB()
    madroot = madDB.getMadroot()
    madParmObj = madrigal.data.MadrigalParameters(madDB)
    madAdminObj = madrigal.admin.MadrigalDBAdmin(madDB)
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
            
        if kinst in corrupt_kinst_list:
            print(('skipping because corrupt kinst %i' % (kinst)))
            continue
        
        # all filters passed - work on this exp
        expDir = madExpObj.getExpDirByPosition(i)
        if not os.access(os.path.join(expDir, 'fileTab.txt'), os.R_OK):
            continue
        print(('Working on directory %s' % (expDir)))
        
        madFileObj = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
        for j in range(madFileObj.getFileCount()):
            kindat = madFileObj.getKindatByPosition(j)
            if (kinst, kindat) in corrupt_kinst_kindat_list:
                print(('Skipping kinst %i, kindat %i because corrupt combination' % (kinst, kindat)))
                continue
            if not args.kindat == 0:
                if args.kindat != kindat:
                    continue
            basename = madFileObj.getFilenameByPosition(j)
            filename = os.path.join(expDir, basename)
            if filename in corrupt_file_list:
                continue
            file_basename, file_extension = os.path.splitext(basename)
            if not file_extension in ('.h5', '.hdf5', '.hdf'):
                continue
            # ignore history files
            if madFileObj.getCategoryByPosition(j) != 1:
                continue
            result = checkFile(filename, kinst, kindat, madroot)
            if result == 0:
                print(('File %s passed indParm test' % (filename)))
            elif result == 1 and not args.delete:
                # just verify its fixable
                try:
                    redoIndParms(filename, kinst, kindat, madroot, madParmObj, fix=False)
                    print(('File %s passed indParm test, but missing array layout' % (filename)))
                    failedList.append(filename)
                except:
                    print(('File %s failed' % (filename)))
                    traceback.print_exc()
                    failedList.append(filename)
            elif result == 2 and not args.delete:
                # just verify its fixable
                try:
                    redoIndParms(filename, kinst, kindat, madroot, madParmObj, fix=False)
                except:
                    traceback.print_exc()
                print(('File %s failed indParm test' % (filename)))
                failedList.append(filename)
                if args.fix:
                    redoIndParms(filename, kinst, kindat, madroot, madParmObj)
                    print(('fixed %s' % (filename)))

            elif result in (1, 2) and args.delete:
                # just verify its fixable for the record
                failedList.append(filename)
                if result == 1:
                    print(('File %s has indParms, but not array - being deleted' % (filename)))
                else:
                    print(('File %s failed indParm test - being deleted' % (filename)))
                madAdminObj.removeMadrigalFile(os.path.dirname(filename), 
                                               os.path.basename(filename))
                    
                 
    print('List of all failed files:')
    for f in failedList:
        print(f)
            
        
    
    
            
    
    