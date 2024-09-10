#!PYTHONEXE

"""configureExperiments is a script to allow experiments to be imported from
other Madrigal installations or moved between experiments[0-9]* directories.  Verifies both siteId and cgiUrl.
Checks that all three geophysical files are in experiments directory

Replaces the tcl version of the same name

No arguments.

For each non-hidden local experiment, will make the following changes if needed:

    1. expTab.txt -> expId: set to new siteId x 10,000,000
    2. expTab.txt -> url: set to new url
    3. expTab.txt -> siteId: set to new siteId
    4. fileTab.txt -> fileId: set to new siteId x 10,000,000

$Id: configureExperiments.py 7045 2019-10-07 19:56:46Z brideout $
"""

import os, os.path, sys

import madrigal.metadata

print('configureExperiments is converting any experiment from a different' + \
      ' Madrigal site into a local experiment. Will also convert experiments moved into different experiments[0-9]* directories.')

madDB = madrigal.metadata.MadrigalDB()
siteId = madDB.getSiteID()

thisId = siteId*10000000
cgiUrl = madDB.getTopLevelUrl()
cgiUrlTest = os.path.join(cgiUrl, 'madtoc')

# be sure experiments/stage exists and is world-writable
madroot = madDB.getMadroot()
stageDir = os.path.join(madroot, 'experiments/stage')
if not os.access(stageDir, os.R_OK):
    os.makedirs(stageDir)
try:
    os.chmod(stageDir, 0o777)
except:
    pass

hasGeo = False
hasDst = False
hasImf = False

expConvertedNum = 0

# get a list of every Madrigal experiment directory
madExpDirs = madDB.getExpList()

for thisDir in madExpDirs:

    # create experiment object
    try:
        madExpObj = madrigal.metadata.MadrigalExperiment(madDB,
                                                         os.path.join(thisDir,
                                                                      'expTab.txt'))
    except:
        print('WARNING - error with experiment %s expTab.txt file' % (thisDir))
        continue
    
    # if siteId is and cgiUrl is correct, continue
    # create list of valid urls
    validUrlList = []
    startIndex = thisDir.find('/experiments')
    if thisDir.find('/experiments/') != -1:
        # it is valid to skip experiment part of url
        validUrlList.append(cgiUrlTest + thisDir[startIndex+len('/experiments'):])
        if thisDir.find('1950/gpi/01jan50') != -1:
            hasGeo = True
        if thisDir.find('1957/dst/01jan57') != -1:
            hasDst = True
        if thisDir.find('1963/imf/27nov63') != -1:
            hasImf = True
    else:
        if thisDir.find('1950/gpi/01jan50') != -1:
            raise IOError('The experiment 1950/gpi/01jan50 must be located in the experiments directory only!')
        if thisDir.find('1957/dst/01jan57') != -1:
            raise IOError('The experiment 1957/dst/01jan57 must be located in the experiments directory only!')
        if thisDir.find('1963/imf/27nov63') != -1:
            raise IOError('The experiment 1963/imf/27nov63 must be located in the experiments directory only!')
    validUrlList.append(cgiUrlTest + thisDir[startIndex:])
    if madExpObj.getExpSiteIdByPosition(0) == siteId and \
       madExpObj.getExpUrlByPosition(0) in validUrlList:
        continue

    print('Modifying exp %s to be local' % (thisDir))
    expConvertedNum += 1
    
    # get baseDir
    baseDir = thisDir[len(madroot):]
    if baseDir[0] == '/':
        baseDir = baseDir[1:]

    # modify expTab.txt
    madExpObj.setExpIdByPosition(0, thisId)
    thisUrl = madExpObj.getExpUrlByPosition(0)
    newUrl = os.path.join(cgiUrl, 'madtoc', baseDir)
    madExpObj.setExpUrlByPosition(0, newUrl)
    madExpObj.setExpSiteIdByPosition(0, siteId)
    madExpObj.writeMetadata()

    if os.access(os.path.join(thisDir,'fileTab.txt'), os.R_OK):
            
        try:
            madFileObj = madrigal.metadata.MadrigalMetaFile(madDB,
                                                            os.path.join(thisDir,
                                                                         'fileTab.txt'))
            if madFileObj.getFileCount() == 0:
                raise IOError('')
        except:
            print('WARNING - error with experiment %s fileTab.txt' % (thisDir))
            continue

        # modify fileTab.txt
        madFileObj.setExpIdByPosition(0, thisId)
        madFileObj.writeMetadata()
        
if not hasGeo:
    raise IOError('The experiment 1950/gpi/01jan50 not found in the experiments directory!')
if not hasDst:
    raise IOError('The experiment 1957/dst/01jan57 not found in the experiments directory!')
if not hasImf:
    raise IOError('The experiment 1963/imf/27nov63 not found in the experiments directory!')

if expConvertedNum == 0:
    print('No non-local experiments found')
else:
    cmd = 'chmod -Rf %s' % (os.path.join(madroot, 'experiments*'))
    print('configureExperiments done')
    

