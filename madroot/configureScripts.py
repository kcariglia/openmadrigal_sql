""""configureScripts loops over all scripts in source/madpy/scripts/bin
and edits them to use the site-specific
definitions in madrigal.cfg and copies the resulting configured file to
MADROOT/bin. 

# $Id: configureScripts.py 7463 2022-10-12 13:51:46Z brideout $
"""
# standard python imports
import os, sys, os.path, stat


def testPerm(fname):
    """testPerm returns True if permission should be changed, False is not.
    Wants permission to be 775 or 777
    """
    st = os.stat(fname)
    perm = oct(st.st_mode)
    user = int(perm[-3])
    group = int(perm[-2])
    other = int(perm[-1])
    if user != 7 or group != 7:
        return(True)
    elif other not in (7,5):
        return(True)
    return(False)


# Extract configuration variables from madrigal.cfg
f = open('madrigal.cfg')
lines = f.readlines()
f.close()
madDict = {} # dict of keywords, values
for line in lines:
    if line[0] == '#':
        continue
    items = line.split()
    if len(items) < 3:
        continue
    if items[1] != '=':
        continue
    madDict[items[0]] = ' '.join(items[2:])
    
# add PYTHONEXE
madroot = madDict['MADROOT']
madDict['PYTHONEXE'] = os.path.join(madroot, 'bin/python')

# Get the variable values and sort so that longest variable names
# are checked first
keys = sorted(madDict, key=len)
keys.reverse()

f = open('MANIFEST')
lines = f.readlines()
f.close()

# change files only if one of the following strings match
matchStrings = ["source/madpy/scripts/bin",
                "source/madpy/madrigalWeb/global",
                "source/madpy/madrigalWeb/test",
                "configureExperiments"]

for line in lines:
    filename = line.strip()
    found = False
    for match in matchStrings:
        if filename.find(match) != -1:
            found = True
            break
    if found:
        if filename.find("configureExperiments") != -1:
            dest = os.path.join(madroot, "configureExperiments")
        else:
            dest = os.path.join(madroot, 'bin', os.path.basename(filename))
        f = open(filename)
        text = f.read()
        f.close()
        for key in keys:
            if text.find(key) != -1:
                text = text.replace(key, madDict[key])
        f = open(dest, 'w')
        f.write(text)
        f.close()
        print('%s -> %s' % (filename, dest))
        
        if testPerm(dest):
            os.chmod(dest, 
                     stat.S_IRWXU |
                     stat.S_IRWXG |
                     stat.S_IROTH |
                     stat.S_IXOTH)

        

