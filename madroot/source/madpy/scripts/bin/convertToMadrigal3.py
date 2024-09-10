#!PYTHONEXE

"""convertToMadrigal3.py [--numCPU=<numCPU>][-q --quiet][--skipCache][--skipMad3Download] converts a CEDAR 2.x database to Madrigal 3.0.

Steps:
    1. Runs createCachedHdf5Files.py with --overwrite --includeNonDefault to make sure every Hdf5 file in overview is up-to-date
        and ready to be switched to be the main file. If --numCPU given, that argument passed to createCachedHdf5Files.py
    2. For each experiment, and then for each non-Hdf5 file in that experiment:
        3. Remove that file, and copy it to <expDir>/deprecated
        4. Move the corresponding Hdf5 file from overview into experiment, adding it with the same status and category as
            removed file.  If that file does not exist (because createCachedHdf5Files.py failed to create it), log that to stdout.
    3. Updates siteTab.txt table to add Madrigal version field to end (3.0)
            
$Id: convertToMadrigal3.py 7405 2021-12-17 15:59:53Z brideout $
"""
# standard python imports
import os, os.path, sys
import subprocess
import shutil
import glob
import re
import getopt
import time
import traceback
import multiprocessing
import random

# madrigal imports
import madrigal.metadata
import madrigal.admin
import madrigal.data


def convertRecordPlots(plotDir):
    """convertRecordPlots converts all record plots to png with correct numbering
    """
    gsCmdTemplate = 'gs -dNOPAUSE -dBATCH -q -sDEVICE=png256 -r120 -g1230x1230 -sOutputFile=%s %s'
    convertCmdTemplate = 'convert %s %s'
    reStr = '[0-9][0-9][0-9][0-9][0-9]'
    pngFiles = glob.glob(os.path.join(plotDir, '*[0-9][0-9][0-9][0-9][0-9]*.png'))
    pngFiles.sort()
    if len(pngFiles) > 0 and pngFiles[0].find('00000') != -1:
        # already converted
        return
    # loop through possible file types
    image_exts_dict = {'png': 'convert',
                       'jpg':'convert', 
                       'jpeg':'convert', 
                       'gif': 'convert',
                       'eps': 'gs',
                       'ps': 'gp'}
    for image_ext in list(image_exts_dict.keys()):
        image_ext_type = image_exts_dict[image_ext]
        plotFiles = glob.glob(os.path.join(plotDir, '*[0-9][0-9][0-9][0-9][0-9]*.%s' % (image_ext)))
        if len(plotFiles)> 0:
            print(('converting *.%s files in %s' % (image_ext, plotDir)))
            plotFiles.sort()
            for i, plotFile in enumerate(plotFiles):
                basename = os.path.basename(plotFile)
                items = re.split(reStr, basename)
                targetBasename = items[0] + '%05i' % (i) + items[1]
                targetBasename = targetBasename[:targetBasename.rfind('.')+1] + 'png'
                target = os.path.join(plotDir, targetBasename)
                if image_ext_type == 'gs':
                    cmd = gsCmdTemplate % (target, plotFile)
                elif image_ext_type == 'convert':
                    cmd = convertCmdTemplate % (plotFile, target)
                try:
                    subprocess.check_call(cmd.split())
                except:
                    print(('cmd <%s> failed' % (cmd)))
                    traceback.print_exc()
                    
                # remove old file
                os.remove(plotFile)
            break
                
                
def processExperiment(args):
    """ method called by multiprocesing pool to handle a single experiment 
    args = expDir, madFile, problemExpList, quiet
    """
    # put the entire method in a try block to get full traceback
    try:
        t = time.time()
        madDB = madrigal.metadata.MadrigalDB()
        madAdmin = madrigal.admin.MadrigalDBAdmin(madDB)
        expDir, problemExpQueue, quiet = args
        madFile = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
        deprecatedDir = os.path.join(expDir, 'deprecated')
        if not quiet:
            print(('working on dir %s' % (expDir)))
        try:
            os.mkdir(deprecatedDir)
        except:
            pass
        
        # the first pass is to get a list of all files already registered, in case of a partial conversion
        # also makes sure file times are all registered
        filesInExp = []
        modified = False
        for j in range(madFile.getFileCount()):
            filesInExp.append(madFile.getFilenameByPosition(j))
            if madFile.getFileDatetimeByPosition(j) is None:
                madFile.setFileDatetimeByPosition(j, None)
                modified = True # so we know we need to rewrite it
        if modified:
            madFile.writeMetadata()
            
        if not quiet:
            for s in filesInExp:
                print(s)
            
        filesToRemove = [] # remove files only after adding files
        
        for j in range(madFile.getFileCount()):
            filename = madFile.getFilenameByPosition(j)
            fileName, fileExtension = os.path.splitext(filename)
            overviewFile = os.path.join(expDir, 'overview', filename + '.summary')
            if fileExtension in ('.h5', '.hdf5', '.hdf'):
                if not os.path.exists(overviewFile):
                    if not quiet:
                        print(('adding overview to %s' % (filename)))
                    madrigal.data.MadrigalFile(os.path.join(expDir, filename), madDB, acceptOldSummary=True)
                if not quiet:
                    print(('skipping %s because already Hdf5' % (filename)))
                continue
            
            if not quiet:
                print(('working on file %s' % (filename)))
            if filename + '.hdf5' not in filesInExp:
                status = madFile.getStatusByPosition(j)
                category = madFile.getCategoryByPosition(j)
                infile = os.path.join(expDir, filename)
                outfile = os.path.join(deprecatedDir, filename)
                try:
                    shutil.copyfile(infile,outfile)
                except FileNotFoundError:
                    print(('Mad2 file %s not found! Adding this exp to problem list' % (os.path.join(expDir, filename))))
                    problemExpQueue.put(expDir)
                    continue
                hdf5File = os.path.join(expDir, 'overview', filename + '.hdf5')
                if not os.path.exists(hdf5File):
                    print(('Hdf5 file %s not found! Adding this exp to problem list' % (hdf5File)))
                    problemExpQueue.put(expDir)
                    continue
                    
                try:
                    # load Hdf5 first before deleting original file
                    madDataObj = madrigal.data.MadrigalFile(hdf5File, madDB, saveSummary=False)
                    kindats = madDataObj.getKindatList()
                    madAdmin.addMadrigalFile(expDir, hdf5File, 0, status, category, kindat=kindats[0],
                                             acceptOldSummary=True, notify=False)
                    filesToRemove.append(filename)
                    # now make sure summary file updated
                    newMadFile = os.path.join(expDir, os.path.basename(hdf5File))
                    madNewDataObj = madrigal.data.MadrigalFile(newMadFile, madDB)
                    if not quiet:
                        print(('%s added' % (hdf5File)))
                except:
                    print(('problem with file %s' % (hdf5File)))
                    traceback.print_exc()
                    problemExpQueue.put(expDir)
                    continue
                    
                
                # check if record plots exists
                recordsDir = os.path.join(expDir, 'plots', filename, 'records')
                orgCount = glob.glob(os.path.join(recordsDir, '*.*'))
                newRecordsDir = os.path.join(expDir, 'plots', filename+'.hdf5', 'records')
                newCount = glob.glob(os.path.join(newRecordsDir, '*.*'))
                if os.path.exists(recordsDir) and orgCount > newCount:
                    if not quiet:
                        print(('converting record plots in %s' % (recordsDir)))
                    convertRecordPlots(recordsDir)
                    oldFilenameDir = os.path.join(expDir, 'plots', filename)
                    newFilenameDir = os.path.join(expDir, 'plots', filename + '.hdf5')
                    # just to be sure its doesn't exist
                    cmd = 'rm -rf %s' % (newFilenameDir)
                    subprocess.check_call(cmd.split())
                    
                    shutil.move(oldFilenameDir, newFilenameDir)
            else:
                if not quiet:
                    print(('simply deleting %s because already exists as hdf5' % (filename)))
                filesToRemove.append(filename)
                
                
        for filename in filesToRemove:
            try:
                madAdmin.removeMadrigalFile(expDir, filename, allowMissing=True)
                if not quiet:
                    print(('removed %s' % (filename)))
            except:
                traceback.print_exc()
                target = os.path.join(expDir, filename)
                if not quiet:
                    print(('Will remove the file %s manually if possible' % (target)))
                try:
                    os.remove(target)
                except:
                    pass
                
        if not quiet:
            print(('Processing expDir %s  took %f seconds' % (expDir, time.time()-t)))
    
    except Exception as e:
        print(('Exception raised with expDir %s and file %s' % (str(expDir), str(madFile))))
        traceback.print_exc()
        

### main script begins here ###
if __name__ == '__main__':
                    
    t1 = time.time()
    numCPUCmd = ''
    numCPU = multiprocessing.cpu_count()-2
    if numCPU < 1:
        numCPU = 1
    if numCPU > 2:
        numCPU = 2
    quiet = False
    skipCache = False
    skipMad3Download = False
            
    try:
        opts, args = getopt.getopt(sys.argv[1:], "q", ["numCPU=", "quiet", "skipCache", "skipMad3Download"])
    except getopt.GetoptError as err:
        print(str(err)) 
        sys.exit(2)
    for o, a in opts:
        if o == '--numCPU':
            numCPU = min(int(a), numCPU)
            if numCPU < 1:
                raise ValueError('numCPU must be positive, not %i' % (numCPU))
            numCPUCmd = '--numCPU=%i' % (numCPU)
        elif o == '-q' or o == '--quiet':
            quiet = True
        elif o == '--skipCache':
            skipCache = True
        elif o == '--skipMad3Download':
            skipMad3Download = True
        else:
            assert False, "unhandled option"
    
    
    madDB = madrigal.metadata.MadrigalDB()
    madAdmin = madrigal.admin.MadrigalDBAdmin(madDB)
    
    if skipMad3Download:
        skipDownloadStr = '--skipMad3Download'
    else:
        skipDownloadStr = ''
    
    cmd = '%s/bin/createCachedHdf5Files.py --includeNonDefault  --mad3 %s --includeGeo %s' % (madDB.getMadroot(), 
                                                                                          skipDownloadStr, numCPUCmd)
    if not skipCache:
        subprocess.check_call(cmd.split())
    
    m = multiprocessing.Manager()
    problemExpQueue = m.Queue()
    pool = multiprocessing.Pool(processes=numCPU)
    expsToProcess = [] # argument expDir,  to processExp
    
    madExp = madrigal.metadata.MadrigalExperiment(madDB)
    for i in range(madExp.getExpCount()):
        expDir = madExp.getExpDirByPosition(i)
        try:
            madFile = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            print(('no files in experiments %s' % (expDir)))
            continue
        expsToProcess.append((expDir, problemExpQueue, quiet))
        
    random.shuffle(expsToProcess)
        
    pool.map(processExperiment, expsToProcess, 5)
    print('pooled calls to processExperiment done - calling updateMaster')
    
    madAdmin.updateMaster()
            
            
    print('Set site version to 3.0 if needed')
    mdSiteObj = madrigal.metadata.MadrigalSite(madDB)
    siteID = madDB.getSiteID()
    if siteID != '3.0':
        mdSiteObj.setSiteVersionBySiteID(siteID, '3.0')
        mdSiteObj.writeMetadata()
        
    if problemExpQueue.qsize() == 0:
        print('Conversion without problems')
    else:
        print('The following experiments had missing Hdf5 files')
        for i in range(problemExpQueue.qsize()):
            print((problemExpQueue.get()))
    
    print(('Total time to run convertToMadrigal %f secs' % (time.time() - t1)))
            
            
