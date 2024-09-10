"""checkSiteIs3.py simply checks that the Madrigal administrator has set the siteTab.txt file to indicate this site
is at least Madrigal 3.

Also verifies that soft links exist between $MADROOT/experiments[0-9]* and 
$MADROOT/source/madpy/djangoMad/madweb/static

Exits with error if not set to at least 3.0

$Id: checkSiteIs3.py 7524 2023-05-16 13:31:14Z brideout $
"""

# standard python imports
import os, os.path, sys
import packaging.version
import subprocess

# Madrigal imports
import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()
siteId = madDB.getSiteID()

madSiteObj = madrigal.metadata.MadrigalSite(madDB)

version = madSiteObj.getSiteVersion(siteId)

if packaging.version.parse(version) < packaging.version.parse('3.0'):
    print(('INSTALLATION FAILED: siteTab.txt needs to be updated so that this site (%i) is set to at least 3.0' % (siteId)))
    sys.exit(-1)
    
print('checking if any soft links to experiments directory(s) needed...')
expDirs = madDB.getExperimentDirs()
madroot = madDB.getMadroot()
needed = False
for expDir in expDirs:
    basename = os.path.basename(expDir)
    targetDir = os.path.join(madroot, 'source/madpy/djangoMad/madweb/static', basename)
    if not os.access(targetDir, os.R_OK):
        # create it
        cmd = 'ln -s %s %s' % (os.path.join(madroot, basename), targetDir)
        subprocess.check_call(cmd.split())
        print(('created soft link from %s to %s' % (os.path.join(madroot, basename), targetDir)))
        needed = True
        
if not needed:
    print('No soft links needed')
        