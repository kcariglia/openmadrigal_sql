"""checkConfig.py verifies madrigal.cfg has been edited where needed
Also verifies siteTab.txt has this siteid

$Id: checkConfig.py 7337 2021-03-26 14:10:17Z brideout $
"""

# standard python imports
import os, sys, os.path
import shutil

# first step is to copy metadata files over that do not already exist

# siteTab.txt
if not os.access("metadata/siteTab.txt", os.R_OK):
    shutil.copy("metadata/siteTab.txt.original",  "metadata/siteTab.txt")
    print("Copied siteTab.txt.original to siteTab.txt")
else:
    print("siteTab.txt already exists - new version ignored.  May be updated by updatedMaster later.")
 
 # instTab.txt

if not os.access("metadata/instTab.txt", os.R_OK):
    shutil.copy("metadata/instTab.txt.original",  "metadata/instTab.txt")
    print("Copied instTab.txt.original to instTab.txt")
else:
    print("instTab.txt already exists - new version ignored.  May be updated by updatedMaster later.")
 
 # instType.txt
if not os.access("metadata/instType.txt", os.R_OK):
    shutil.copy("metadata/instType.txt.original",  "metadata/instType.txt")
    print("Copied instType.txt.original to instType.txt")
else:
    print("instType.txt already exists - new version ignored.  May be updated by updatedMaster later.")
 
 # typeTab.txt
if not os.access("metadata/typeTab.txt", os.R_OK):
    shutil.copy("metadata/typeTab.txt.original",  "metadata/typeTab.txt")
    print("Copied typeTab.txt.original to typeTab.txt")
else:
    print("typeTab.txt already exists - new version ignored.  May be updated by updatedMaster later.")
 

# lines im madrigal.cfg required to be verified
MADROOT_test = False
MADSERVER_test = False
MADSERVERROOT_test = False
SITEID_test = False
HTMLSTYLE_test = False
INDEXHEAD_test = False
CONTACT_test = False
pwd = os.getcwd()

f = open('madrigal.cfg')
lines = f.readlines()
f.close()



for line in lines:
    # MADROOT check
    if line[0:9] == "MADROOT =":
        MADROOT = line[9:].strip()
        MADROOT_test = True
        if MADROOT != pwd:
            print("MADROOT in madrigal.cfg is $MADROOT, but PWD is %s" % (pwd))
            print("Fix madrigal.cfg before trying installation again")
            sys.exit(-1)
    
    # MADSERVER check
    elif line[0:11] == "MADSERVER =":
        MADSERVER = line[11:].strip()
        MADSERVER_test = True
        if MADSERVER.find('invalid') != -1:
            print("madrigal.cfg MADSERVER entry must be edited - cannot be %s" % (MADSERVER))
            print("Fix madrigal.cfg before trying installation again")
            sys.exit(-1)
    
    # MADSERVERROOT check
    elif line[0:15] == "MADSERVERROOT =":
        MADSERVERROOT = line[15:].strip()
        MADSERVERROOT_test = True
    
    # SITEID check
    elif line[0:8] == "SITEID =":
        SITEID = line[8:].strip()
        SITEID_test = True
        if SITEID.find('99999') != -1:
            print("madrigal.cfg SITEID entry must be edited - cannot be 99999")
            print("Fix madrigal.cfg before trying installation again")
            sys.exit(-1)

    
    # HTMLSTYLE check
    elif line[0:11] == "HTMLSTYLE =":
        HTMLSTYLE = line[11:].strip()
        HTMLSTYLE_test = True
    
    # INDEXHEAD check
    elif line[0:11] == "INDEXHEAD =":
        INDEXHEAD = line[11:].strip()
        INDEXHEAD_test = True
        if INDEXHEAD.find('CHANGEME') != -1:
            print("madrigal.cfg INDEXHEAD entry must be edited - cannot include CHANGEME")
            print("Fix madrigal.cfg before trying installation again")
            sys.exit(-1)
    
    # CONTACT check
    elif line[0:9] == "CONTACT =":
        CONTACT = line[9:].strip()
        CONTACT_test = True
        if CONTACT.find('CHANGEME') != -1:
            print("madrigal.cfg CONTACT entry must be edited - cannot include CHANGEME")
            print("Fix madrigal.cfg before trying installation again")
            sys.exit(-1)
    
   

# check that all lines were verified

if not MADROOT_test:
    print("madrigal.cfg does not contain required variable MADROOT")
    sys.exit(-1)
elif  not MADSERVER_test:
    print("madrigal.cfg does not contain required variable MADSERVER")
    sys.exit(-1)
elif not MADSERVERROOT_test:
    print("madrigal.cfg does not contain required variable MADSERVERROOT")
    sys.exit(-1)
elif not SITEID_test:
    print("madrigal.cfg does not contain required variable SITEID")
    sys.exit(-1)
elif not HTMLSTYLE_test:
    print("madrigal.cfg does not contain required variable HTMLSTYLE")
    sys.exit(-1)
elif not INDEXHEAD_test:
    print("madrigal.cfg does not contain required variable INDEXHEAD")
    sys.exit(-1)
elif not CONTACT_test:
    print("madrigal.cfg does not contain required variable CONTACT")
    sys.exit(-1)


# now verify that metadata/siteTab.txt has matching entry
SITEID_OKAY = False
f = open('metadata/siteTab.txt')
lines = f.readlines()
f.close()
for line in lines:
    items = line.split(',')
    if len(items) < 3:
        continue
    if items[0] == SITEID:
        SITEID_OKAY = True
        break


if  not SITEID_OKAY:
    print("siteTab.txt does not contain an entry for site id %s" % (SITEID))
    print("Edit metadata/siteTab.txt before trying installation again")
    sys.exit(-1)
