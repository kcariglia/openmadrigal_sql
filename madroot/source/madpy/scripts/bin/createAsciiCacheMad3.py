"""createAsciiCacheMad3.py creates gzipped ascii cache files for given kinst to improve performance
for Madrigal 3 sites.

$Id: createAsciiCacheMad3.py 7119 2020-06-22 20:28:21Z brideout $
"""
usage = 'python createAsciiCacheMad3.py <kinst>'

# standard python imports
import multiprocessing
import os, os.path, sys
import subprocess

# Madrigal imports
import madrigal.metadata
import madrigal.cedar

def createAsciiCache(args):
    """createAsciiCache is the method called by the multiprocessing module to create a single cached file
    
    Input args contains:
        fileName - full path to file to be cached
        madDB - madrigal.MadrigalDB object
    """
    fileName, madDB = args
    print(('caching %s' % (fileName)))
    madCedarObj = madrigal.cedar.MadrigalCedarFile(fileName)
    dirname = os.path.dirname(fileName)
    basename = os.path.basename(fileName)
    outputFile = os.path.join(dirname, 'overview', basename + '.txt')
    madCedarObj.writeText(outputFile)
    subprocess.check_call(['gzip', '-9', outputFile])
    
### main script begins here ###
if __name__ == '__main__':
    
    # main
    numCPUs = 1 # 20
    
    if len(sys.argv) != 2:
        print(usage)
        sys.exit(-1)
    kinst = int(sys.argv[1])
    
    madDB = madrigal.metadata.MadrigalDB()
            
    pool = multiprocessing.Pool(processes=numCPUs)
    
    args = [] # list of args to pass into multiprocssing
    
    madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
    for i in range(madExpObj.getExpCount()):
        if madExpObj.getKinstByPosition(i) != kinst:
            continue
        expDir = madExpObj.getExpDirByPosition(i)
        madFileObj = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
        for j in range(madFileObj.getFileCount()):
            if madFileObj.getCategoryByPosition(j) != 1:
                continue
            basename = madFileObj.getFilenameByPosition(j)
            # see if cached file already exists
            cachedFile = os.path.join(expDir, 'overview', basename + '.txt.gz')
            if os.access(cachedFile, os.R_OK):
                continue
            fileName = os.path.join(expDir, basename)
            args.append((fileName, madDB))
    
    pool.map(createAsciiCache, args)
        