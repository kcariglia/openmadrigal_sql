#!PYTHONEXE

"""createCachedFiles.py is a script that will create cached text or netCDF4 files to speed up
user downloads.

$Id: createCachedFiles.py 7119 2020-06-22 20:28:21Z brideout $
"""

usage = """createCachedFiles.py [--excludeText --excludeNetCDF4 --inst=<instList> --kindat=<kindatList>  --path=<expPath> 
--includeNonDefault  --overwrite --includeGeo --listOnly --numCPU=<numCPU> -h --help]
By default, both text and netCDF4 files created.  Use --excludeText or --excludeNetCDF4 to only create one type.
By default all instruments will be included.  Use --inst=<comma delimited kinst list> to only include some instruments.
By default all kinds of data will be included.  Use --kindat=<comma delimited kindat list> to only include some kindats
By default, all experiment directories will be included.  Use --path to limit to a particular directory and all subdirectories.
By default only default files will be cached.  Use --includeNonDefault to include all files.
Use --overwrite to overwrite all Hdf5 cached files. Default is to skip existing cached files.
Set --includeGeo to also convert geophysical files. Default is to skip them.
Set --listOnly to simply print cached files to be created
numCPU by default is the maximum of (1, numCPUs available - 2).  Use --numCPU to override, but still will not be highter than default.
-h or --help - print usage and exit
"""

# standard python imports
import os, os.path, sys
import getopt
import time
import traceback
import multiprocessing
import subprocess

# madrigal imports
import madrigal.metadata
import madrigal.cedar

    

def createCachedFiles(args):
    """createCachedFiles is called for each file to be checked
    
    args = (filename, excludeText, excludeNetCDF4, overwrite, listOnly)
    # this code cannot raise an error
    """
    try:
        filename, excludeText, excludeNetCDF4, overwrite, listOnly = args
        if not listOnly:
            print('working on %s' % (filename))
        sys.stdout.flush()
        madDB = madrigal.metadata.MadrigalDB()
        basename = os.path.basename(filename)
        expDir = os.path.dirname(filename)
        
        if not excludeText:
            cachedFile = os.path.join(expDir, 'overview', basename + '.txt')
            if not os.access(cachedFile + '.gz', os.R_OK) or overwrite:
                if not listOnly:
                    madrigal.cedar.convertToText(filename, cachedFile)
                    subprocess.check_call(['gzip', '-f', cachedFile])
                else:
                    print('ascii cache needed for %s' % (filename))
                
        if not excludeNetCDF4:
            cachedFile = os.path.join(expDir, 'overview', basename + '.nc')
            if not os.access(cachedFile, os.R_OK) or overwrite:
                if not listOnly:
                    if os.access(cachedFile, os.R_OK):
                        os.remove(cachedFile)
                    try:
                        madrigal.cedar.convertToNetCDF4(filename, cachedFile)
                    except IOError:
                        cedarObj = madrigal.cedar.MadrigalCedarFile(filename)
                        cedarObj.write('netCDF4', cachedFile)
                else:
                    print('netCDF4 cache needed for %s' % (filename))
                    
    except:
        print('Unexpected error')
        traceback.print_exc()


### main script begins here ###
if __name__ == '__main__':

    excludeText = False
    excludeNetCDF4 = False
    instList = None
    kindatList = None
    includeNonDefault = 0
    includeGeo = False
    overwrite = False
    expPath = None
    listOnly = False
    numCPU = multiprocessing.cpu_count()-2
    
    try:
        opts, args = getopt.getopt(sys.argv[1:], "h", ["excludeText", "excludeNetCDF4", "inst=", "kindat=", "path=", 
                                                       "includeNonDefault", "overwrite", "includeGeo", "numCPU=", 
                                                       "listOnly", "help"])
    except getopt.GetoptError as err:
        print(str(err)) 
        sys.exit(2)
    for o, a in opts:
        if o == '--excludeText':
            excludeText = True
        elif o == '--excludeNetCDF4':
            excludeNetCDF4 = True
        elif o == "--inst":
            instItems = a.split(',')
            instList = []
            for inst in instItems:
                try:
                    instList.append(int(inst))
                except:
                    print(('--inst must be a comma delimited list of kinst (integers), not %s' % (a)))
                    raise
        elif o == "--kindat":
            kindatItems = a.split(',')
            kindatList = []
            for kindat in kindatItems:
                try:
                    kindatList.append(int(kindat))
                except:
                    print(('--kindat must be a comma delimited list of kindat codes (integers), not %s' % (a)))
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
        elif o == '--listOnly':
            listOnly = True
        elif o == '--numCPU':
            numCPU = int(a)
            if numCPU < 1:
                raise ValueError('numCPU must be positive, not %i' % (numCPU))
        else:
            assert False, "unhandled option"
            
    if excludeText and excludeNetCDF4:
        print('Nothing to be done since both text and netCDF4 cached files excluded')
        sys.exit(0)
            
    # get a list of all files to test for caching
    madDB = madrigal.metadata.MadrigalDB()
    fileList = madDB.getFileList(kinstList=instList, kindatList=kindatList, includeNonDefault=includeNonDefault,
                                 path=expPath)
    
    # possibly skip geophysical files
    geoList = [120, 210, 211, 212]
    
    filesToProcess = [] # the list to pass into the multiprocessing module to handle
    numCPU = min(max(1, multiprocessing.cpu_count()-2), numCPU)
    pool = multiprocessing.Pool(processes=numCPU) 
    print(('Creating cached files using %i cpu\'s' % (numCPU)))
    
    print('This next step may take a few hours....')
    for thisFile in fileList:
        # check expPath
        if expPath:
            if thisFile.find(expPath) == -1:
                continue
            
            
        expTab = os.path.join(os.path.dirname(thisFile), 'expTab.txt')
        madExpObj = madrigal.metadata.MadrigalExperiment(madDB, expTab)
        kinst = madExpObj.getKinstByPosition(0)
        if kinst in geoList and not includeGeo:
            continue
            
        filesToProcess.append((thisFile, excludeText, excludeNetCDF4, overwrite, listOnly))
     
        if len(filesToProcess) > 200:
            pool.map(createCachedFiles, filesToProcess)
            filesToProcess = []
            
    # get all remaining
    if len(filesToProcess):
        pool.map(createCachedFiles, filesToProcess)
    print('All cached files successfully created')
