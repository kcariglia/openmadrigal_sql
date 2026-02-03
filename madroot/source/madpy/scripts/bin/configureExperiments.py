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

$Id: configureExperiments.py 7741 2025-01-09 14:46:26Z kcariglia $
"""

import os, os.path, sys
import re
import numpy
import madrigal.metadata

import datetime
s = datetime.datetime.now()

print('configureExperiments is converting any experiment from a different' + \
      ' Madrigal site into a local experiment. Will also convert experiments moved into different experiments[0-9]* directories.')

madDB = madrigal.metadata.MadrigalDB()
siteId = madDB.getSiteID()

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


def fast_scandir(dirname):
    # from StackOverflow
    # https://stackoverflow.com/questions/973473/getting-a-list-of-all-subdirectories-in-the-current-directory

    subfolders = [f.path for f in os.scandir(dirname) if (f.is_dir())]
    for dirname in list(subfolders):
        subfolders.extend(fast_scandir(dirname))
    return subfolders


hasGeo = False
hasDst = False
hasImf = False

expConvertedNum = 0

expDirs = madDB.getExperimentDirs() # regex list of all matching expdirs
allExpUrls = madDB.getExpUrls()
allExpDirs = []

expObj = madrigal.metadata.MadrigalExperiment(madDB)
# get all expIDs and SQL-assigned IDs associated with experiments for this site
idsAndIdxs = expObj.getAllExpIDs(siteId)

if not idsAndIdxs:
    # special case, first installation when db is empty
    maxId = (siteId * 10000000) + 1
else:
    expIDs = [i[0] for i in idsAndIdxs]
    maxId = int(numpy.max(expIDs))
    maxId += 1


for dir in expDirs:
    __dirConvStr1 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[a-zA-Z0-9\-_]*$'
    __dirConvStr2 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[0-3][0-9][a-z][a-z0-9][a-z0-9][0-9][0-9].?$'
    pthpattern1 = re.compile(madroot + __dirConvStr1)
    pthpattern2 = re.compile(madroot + __dirConvStr2)

    subdirs = fast_scandir(dir)
    subdirs = [d for d in subdirs if (((pthpattern1.search(d) is not None) or (pthpattern2.search(d) is not None))
                                       and ('overview' not in d) and ('deprecated' not in d) and ('plots' not in d))]
    allExpDirs += subdirs


# this is in case of needing to copy expdirs
# later, when adding exp, should add to metadata.db AND create txt files

# algorithm: 
# format expdir -> expurl (local)
# use madDB.getExpUrls
# if this url not in getExpUrls, add new exp using expTab text (and file info by fileTab text)
urltemplate = madDB.getTopLevelUrl() + "/madtoc/"

if (urltemplate + "experiments/1957/dst/01jan57") in allExpUrls:
    hasDst = True
if (urltemplate + "experiments/1950/gpi/01jan50") in allExpUrls:
    hasGeo = True
if (urltemplate + "experiments/1963/imf/27nov63") in allExpUrls:
    hasImf = True

dirsneeded = [dir for dir in allExpDirs if ((urltemplate + dir[dir.find("experiments"):]) not in allExpUrls)]

if len(dirsneeded) > 0:
    expText = ""
    fileText = ""
    for thisDir in dirsneeded:
        # new experiment (and files) to add

        # check if we need to add test exps
        if "experiments/1957/dst/01jan57" in thisDir:
            hasDst = True
        if "experiments/1950/gpi/01jan50" in thisDir:
            hasGeo = True
        if "experiments/1963/imf/27nov63" in thisDir:
            hasImf = True

        with open(os.path.join(thisDir, "expTab.txt"), "r") as f:
            eText = f.read()
            if '\n' != eText[-1]:
                # need newline separator
                eText += '\n'
            expLines = eText.split('\n')
            splitList = [line.split(',') for line in expLines]
            # must account for local metadata
            tList = []
            for line in splitList:
                if len(line) > 1:
                    # reset expID
                    line[0] = maxId
                    # reset expUrl
                    line[1] = urltemplate + thisDir[thisDir.find("experiments"):]
                    # reset siteID
                    line[3] = siteId
                    line = [str(i) for i in line]
                    tList.append(','.join(line))
            eText = '\n'.join(tList)
            if not eText.endswith('\n'):
                eText += '\n'
            expText += eText

        with open(os.path.join(thisDir, "fileTab.txt"), "r") as f:
            fText = f.read()
            if '\n' != fText[-1]:
                # need newline separator
                fText += '\n'
            fileLines = fText.split('\n')
            splitList = [line.split(',') for line in fileLines]
            # must reset expID
            tList = []
            for line in splitList:
                if len(line) > 1:
                    line[1] = maxId
                    line = [str(i) for i in line]
                    tList.append(','.join(line))
            fText = '\n'.join(tList)
            if not fText.endswith('\n'):
                fText += '\n'
            fileText += fText
        maxId += 1
            
    madDB.addExperimentsMetadata(expText)
    madDB.addFilesMetadata(fileText)

e = datetime.datetime.now()
print("finished adding {} new exps after {} s".format(len(dirsneeded), (e-s).seconds))
expConvertedNum = len(dirsneeded)
        
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
    

e = datetime.datetime.now()
print("configureExperiments took {} s".format((e-s).seconds))