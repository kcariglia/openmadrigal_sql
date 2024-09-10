"""correctGpsTecRecno.py is a script that corrects the recno column in GPS TEC data caused by splitting records
with the same time due to the 16 bit limitation of the old CEDAR database format

$Id: correctGpsTecRecno.py 7045 2019-10-07 19:56:46Z brideout $
"""
# standard python imports
import os, os.path, sys
import shutil

# third party imports
import h5py

# madrigal imports
import madrigal.metadata

def fixFile(gps_file):
    """fixFile will normalize the recno column in a gps file if needed
    """
    # first. test if it needs to be fixed
    with h5py.File(gps_file, 'r') as f:
        recno = f['Data']['Table Layout']['recno']
        if recno[-1] <= 288:
            # temp only
            print(('skipping %s because already correct' % (gps_file)))
            return
        
    # for safety, create a copy to edit in case something goes wrong
    basename = os.path.basename(gps_file)
    tmpFile = os.path.join('/tmp', basename)
    shutil.copy(gps_file, tmpFile)
    
    with h5py.File(tmpFile, 'a') as f:
        recno = f['Data']['Table Layout']['recno']
        ut1 = f['Data']['Table Layout']['ut1_unix']
        # loop along ut1, reset recno
        this_recno = 0
        last_ut1 = ut1[0]
        for i in range(ut1.shape[0]):
            ut1_value = ut1[i]
            if ut1_value > last_ut1:
                this_recno += 1
                last_ut1 = ut1_value
            recno[i] = this_recno
        f['Data']['Table Layout']['recno'] = recno
            
    # no errors, copy it back
    shutil.copy(tmpFile, gps_file)
    os.remove(tmpFile)
    print(('corrected %s' % (basename)))
    
    
madDB = madrigal.metadata.MadrigalDB()
madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
for i in range(madExpObj.getExpCount()):
    kinst = madExpObj.getKinstByPosition(i)
    if kinst != 8000:
        continue
    expDir = madExpObj.getExpDirByPosition(i)
    madFileObj = madrigal.metadata.MadrigalMetaFile(madDB, os.path.join(expDir, 'fileTab.txt'))
    for j in range(madFileObj.getFileCount()):
        kindat = madFileObj.getKindatByPosition(j)
        if kindat != 3500:
            continue
        filename = madFileObj.getFilenameByPosition(j)
        # try to fix
        fixFile(os.path.join(expDir, filename))
        
    