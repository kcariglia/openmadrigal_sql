#!PYTHONEXE

"""createCachedHdf5Files.py is a script that will walk all the files in a Madrigal database, and make sure all
files in the old Cedar 2.X format have cached hdf5 versions.  Used only when updating from Madrigal 3 to 
Madrigal 3.

$Id: createCachedHdf5Files.py 7387 2021-10-20 14:22:23Z brideout $
"""

usage = """createCachedHdf5Files.py [--inst=<instList>  --path=<expPath> --includeNonDefault --ini=<iniFile> --mad3 --overwrite
--includeGeo --numCPU=<numCPU> -h --help --removeSummary --skipMad3Download]
By default all instruments will be included.  Use --inst=<comma delimited kinst list> to only include some instruments.
By default, all experiment directories will be included.  Use --path to limit to a particular directory and all subdirectories.
By default only default files will be cached.  Use --includeNonDefault to include all files.
By default, extra parameters and formats are added by the ini file $MADROOT/cachedFiles.ini.  Use
    --ini=<iniFile> to specify an alternative ini file.  See madrigal.data.MadrigalFile._parseCachedIni for description of
    the ini file format.  Set --includeGeo to also convert geophysical files
Use --overwrite to overwrite all Hdf5 cached files
Use --mad3 to overwrite all non Madrigal3 Hdf5 files
numCPU by default is the maximum of (1, numCPUs available - 2).  Use --numCPU to override, but still will not be highter than default.
Use --removeSummary to remove summary files before creating Hdf5 files
Use --skipMad3Download to not try to dowload file from madrigal3.haystack.mit.edu
-h or --help - print usage and exit
"""

import os, os.path, sys
import getopt
import time, datetime
import traceback
import multiprocessing
import warnings
import random

import h5py

import madrigal.metadata
import madrigal.data
import madrigalWeb.madrigalWeb




def downloadMad3File(filename, mad3Url, expDirNum):
    """downloadMad3File downloads the appropriate Madrigal 3 Hdf5 associated with filename if possible
    from mad3Url. Also downloads the summary file.
    
    Inputs:
        filename - full path to filename on present Madrigal2 server
        mad3Url - url of Madrigal 3 CEDAR Madrigal server to get hdf5 version from
        expDirNum - either '' or '3' - suggestion as to what experiment directory to try first.
    
    Returns True if success, False if not.
    """
    user_fullname = 'Bill Rideout' 
    user_email = 'brideout@haystack.mit.edu' 
    user_affiliation = 'MIT' 
    format='hdf5'
    madroot = '/opt/madrigal3'
    madWebObj = madrigalWeb.madrigalWeb.MadrigalData(mad3Url)
    expDir = os.path.dirname(filename)
    basename = os.path.basename(filename)
    
    # take into account that CEDAR madrigal server has both experiments and experiments3
    remoteFileList = [os.path.join(madroot, filename[filename.find('experiments'):]) + '.hdf5']
    remoteFileList.append(remoteFileList[-1].replace('experiments/', 'experiments3/'))
    
    remoteSummaryFileList = [os.path.join(madroot, expDir[expDir.find('experiments'):], 'overview', basename + '.hdf5.summary')]
    remoteSummaryFileList.append(remoteSummaryFileList[-1].replace('experiments/', 'experiments3/'))
    
    if expDirNum == '3':
        # reverse both lists to try experiments3 first
        remoteFileList.reverse()
        remoteSummaryFileList.reverse()
    
    destDir = os.path.join(os.path.dirname(filename), 'overview')
    destination = os.path.join(destDir, os.path.basename(filename) + '.hdf5')
    summDest = destination + '.summary'
    
    for i in range(len(remoteFileList)):
        remoteFile = remoteFileList[i]
        remoteSummaryFile = remoteSummaryFileList[i]
        try:
            madWebObj.downloadFile(remoteFile, destination, user_fullname, user_email, user_affiliation, 
                                   format)
            madWebObj.downloadFile(remoteSummaryFile, summDest, user_fullname, user_email, user_affiliation, 
                                   format)
            return(True)
        except:
            if i < len(remoteFileList) - 1:
                continue
            else:
                print(('Failed to download any of %s' % (str(remoteFileList))))
                return(False)
            
            
    
    
    
    

def createHdfFile(args):
    # this code cannot raise an error
    try:
        with warnings.catch_warnings():
            # we know we are calling deprecated code - surpress warnings
            warnings.simplefilter("ignore")
            filename, iniFile, overwrite, mad3, removeSummary, skipMad3Download, expDirNum,  quiet, mad3Url = args
            madDB = madrigal.metadata.MadrigalDB()
            # skip if already Hdf5
            fileName, fileExtension = os.path.splitext(filename)
            if fileExtension in ('.h5', '.hdf5', '.hdf'):
                if not quiet:
                    print(('skipping %s because already Hdf5' % (filename)))
                return
            hdf5Name = os.path.join(os.path.dirname(filename), 'overview', os.path.basename(filename) + '.hdf5')
            if os.access(hdf5Name, os.R_OK) and not overwrite:
                if not mad3:
                    return
                else:
                    # check if already Madrigal3
                    try:
                        f = h5py.File(hdf5Name, 'r')
                        if '_record_layout' in list(f['Metadata'].keys()):
                            f.close()
                            if not quiet:
                                print(('skipping %s because cached file already Madrigal3' % (filename)))
                            return
                        else:
                            print(('overwriting cached file for %s because not Madrigal3' % (filename)))
                            f.close()
                            os.remove(hdf5Name)
                    except:
                        traceback.print_exc()
                        try:
                            f.close()
                        except:
                            pass
                        print(('Problem with cached file for %s - removing' % (filename)))
                        os.remove(hdf5Name)
                        
            if removeSummary:
                summaryFile = os.path.join(os.path.dirname(filename), 'overview', 
                                           os.path.basename(filename) + '.summary')
                try:
                    if not quiet:
                        print(('removing summary %s' % (summaryFile)))
                    os.remove(summaryFile)
                except:
                    pass
                
            # first try to download file from mad3Url if not skipMad3Download
            result = False
            if not skipMad3Download:
                result = downloadMad3File(filename, mad3Url, expDirNum)
                if result:
                    print(('Downloaded cached and summary file for %s from Mad3 CEDAR Madrigal site' % (filename)))
            
            if not result:
                # this Hdf5 file needs to be created
                print(('creating hdf5 file for %s' % (filename)))
                try:
                    madFileObj = madrigal.data.MadrigalFile(filename, madDB)
                    madFileObj.getCachedHdf5(iniFile, overwrite, showWarnings=True)
                except:
                    traceback.print_exc()
                    
    except:
        print('Unexpected error')
        traceback.print_exc()


### main script begins here ###
if __name__ == '__main__':

    instList = None
    includeNonDefault = 0
    includeGeo = False
    iniFile = None
    overwrite = False
    mad3 = False
    expPath = None
    removeSummary = False
    skipMad3Download = False
    numCPU = multiprocessing.cpu_count()-2
    quiet = False
    
    mad3Url = 'https://cedar.openmadrigal.org'
    
    try:
        opts, args = getopt.getopt(sys.argv[1:], "h", ["inst=", "path=", "includeNonDefault", "overwrite", "ini=", 
                                                       "mad3", "includeGeo", "numCPU=", "help", "removeSummary",
                                                       "skipMad3Download", "quiet"])
    except getopt.GetoptError as err:
        print(str(err)) 
        sys.exit(2)
    for o, a in opts:
        if o == "--inst":
            instItems = a.split(',')
            instList = []
            for inst in instItems:
                try:
                    instList.append(int(inst))
                except:
                    print(('--inst must be a comma delimited list of kinst (integers), not %s' % (a)))
                    raise
        elif o == "--path":
            expPath = a
            if len(expPath) > 1 and expPath[-1] == '/':
                # strip off /
                expPath = expPath[:-1]
            if not os.access(expPath, os.R_OK):
                raise IOError('Unable to access path %s' % (expPath))
        elif o in ("-h", "--help"):
            print(usage)
            sys.exit(-1)
        elif o == '--includeNonDefault':
            includeNonDefault = 1
        elif o == '--includeGeo':
            includeGeo = True
        elif o == '--overwrite':
            overwrite = True
        elif o == '--mad3':
            mad3 = True
        elif o == '--ini':
            iniFile = a
        elif o == '--numCPU':
            numCPU = int(a)
            if numCPU < 1:
                raise ValueError('numCPU must be positive, not %i' % (numCPU))
        elif o == '--removeSummary':
            removeSummary = True
        elif o == '--skipMad3Download':
            skipMad3Download = True
        elif o == '--quiet':
            quiet = True
        else:
            assert False, "unhandled option"
            
    # get a list of all files to test for caching
    madDB = madrigal.metadata.MadrigalDB()
    fileList = madDB.getFileList(kinstList=instList, includeNonDefault=includeNonDefault,
                                 path=expPath)
    
    # possibly skip geophysical files
    geoList = [120, 210, 211, 212]
    
    expDict = {} # expDict - dict with keys = kinst, value = list of tuples of (exp sDT, eDT, expDirNum)
                 # created only if needed as set by skipMad3Download
    
    filesToProcess = [] # the list to pass into the multiprocessing module to handle
    numCPU = min(max(1, multiprocessing.cpu_count()-2), numCPU)
    pool = multiprocessing.Pool(processes=numCPU) 
    print(('Creating Cached Hdf5 files using %i cpu\'s' % (numCPU)))
    
    hdf5Exts = ('.h5', '.hdf5', '.hdf')
    madWebObj = madrigalWeb.madrigalWeb.MadrigalData(mad3Url)
    
    print('This next step may take a few hours....')
    for thisFile in fileList:
        # check expPath
        if expPath:
            if thisFile.find(expPath) == -1:
                continue
            
        # skip Hdf5 files here so things are faster for an almost competely converted Madrigal site
        base, ext = os.path.splitext(thisFile)
        if ext in hdf5Exts:
            continue
            
        expTab = os.path.join(os.path.dirname(thisFile), 'expTab.txt')
        madExpObj = madrigal.metadata.MadrigalExperiment(madDB, expTab)
        kinst = madExpObj.getKinstByPosition(0)
        if kinst in geoList and not includeGeo:
            continue
        
        if not skipMad3Download:
            if kinst not in list(expDict.keys()):
                expList = madWebObj.getExperiments(kinst, 1950, 1, 1, 0, 0, 0, 2020, 12, 31, 23, 59, 59)
                expList.sort()
                data = []
                for exp in expList:
                    sDT = datetime.datetime(exp.startyear, exp.startmonth, exp.startday,
                                            exp.starthour, exp.startmin, exp.startsec)
                    eDT = datetime.datetime(exp.endyear, exp.endmonth, exp.endday,
                                            exp.endhour, exp.endmin, exp.endsec)
                    url = exp.url
                    if url.find('experiments3') != -1:
                        expDirNum = '3'
                    elif url.find('experiments2') != -1:
                        expDirNum = '2'
                    else:
                        expDirNum = ''
                    data.append((sDT, eDT, expDirNum))
                expDict[kinst] = data
            
            sList = madExpObj.getExpStartDateTimeByPosition()[:6]
            sDT = datetime.datetime(*sList)
            eList = madExpObj.getExpEndDateTimeByPosition()[:6]
            eDT = datetime.datetime(*eList)
            mDT = sDT + (eDT - sDT)
            
            # loop through the experiments to get right experiment directory
            expDirNum = None
            for sDT, eDT, thisNum in expDict[kinst]:
                if sDT <= mDT and mDT <= eDT:
                    expDirNum = thisNum
                    break
        else:
            expDirNum = None
                
            
        filesToProcess.append((thisFile, iniFile, overwrite, mad3, removeSummary, skipMad3Download, expDirNum, quiet, mad3Url))
    
        
    # to better balance the load, apply random shuffle
    random.shuffle(filesToProcess)
    
    if len(filesToProcess):
        pool.map(createHdfFile, filesToProcess, 5)
    print('All HDF5 cached files successfully created')
