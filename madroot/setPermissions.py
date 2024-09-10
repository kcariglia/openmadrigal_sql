"""setPermissions.py loops over all files in the Madrigal distribution and changes their
permissions to user and group write - also set some other file permissions - used during install

$Id: setPermissions.py 7459 2022-10-06 19:15:05Z brideout $
"""

# standard python imports
import os, sys, os.path
import glob
import subprocess
import traceback


print("Make all files in distribution world readable, and ")
print("user and group writeable")

f = open('MANIFEST')
filenames = f.read().split()
f.close()

for filename in filenames:
    print("Processing %s" % (filename))
    cmd = 'chmod a+r,ug+w %s' % (filename)
    try:
        subprocess.check_call(cmd.split())
    except:
        traceback.print_exc()
        
cmd = "chmod -R -f 0777 metadata/userdata"
os.system(cmd)

cmd = "chmod ugo+w metadata/userdata/users.xml.template"
subprocess.check_call(cmd.split())

accessFiles = glob.glob("metadata/userdata/access_*.log")
for accessFile in accessFiles:
    cmd = "chmod -f  a+w %s" % (accessFile)
    os.system(cmd)