"""rebuildSummariesMP.py rebuilds all summary information in the Madrigal database using multiprocessing,
then copies each new summary file

$Id: rebuildSummariesMP.py 7119 2020-06-22 20:28:21Z brideout $
"""
# standard python imports
import os, os.path, sys
import multiprocessing
import subprocess

import madrigal.metadata
import madrigal.data

numCpus = 20
baseExpDir = '/data/madrigal/'
remoteStr = 'madrigal:/opt/madrigal3'

def rebuild(args):
    """method called by multiprocessing.pool.map
    """
    madrigalFile = args[0]
    madrigal.data.MadrigalFile(madrigalFile)
    # now copy summary file to madrigal
    dirname = os.path.dirname(madrigalFile)
    basename = os.path.basename(madrigalFile) + '.summary'
    destExpPath = dirname[len(baseExpDir):]
    sourceFile = os.path.join(dirname, 'overview', basename)
    destFile = os.path.join(remoteStr, destExpPath, 'overview', basename)
    cmd = 'scp %s %s' % (sourceFile, destFile)
    subprocess.check_call(cmd.split())
    print(('completed %s' % (madrigalFile)))
    
### main script begins here ###
if __name__ == '__main__':

    madDBObj = madrigal.metadata.MadrigalDB()
    madrigalFiles = madDBObj.getFileList()
    print(('%i files to check' % (len(madrigalFiles))))
    args = [[madFile] for madFile in madrigalFiles]
    pool = multiprocessing.Pool(processes=numCpus)
    pool.map(rebuild, args)
