#!PYTHONEXE

#$Id: createExpWithFile.py 7045 2019-10-07 19:56:46Z brideout $

usage = """
createExpWithFile.py is a script used to create a new Madrigal experiment
based on an already existing file.  Information such as the duration of the
experiment is obtained by analyzing the file.

With Madrigal 3, accepts either old CEDAR database format files, or CEDAR Madrigal 
Hdf5 files.

Required arguments:

    --madFilename - full path to the complete Madrigal file. Basename will
                    be maintained. If old CEDAR databse format, .hdf5 will 
                    be appended

    --expTitle - experiment title. Use quotes if title contains spaces.

    
    --permission - 0 for public, 1 for private (restricted to certain IP range)
                   (both the experiment and the file will set)

    --fileDesc - file decription

Optional arguments:

    --instCode - instrument code. If this argument missing, instrument code is
                 taken from file, but error is thrown if more than one kinst found.

    --category - 1=default, 2=variant, or 3=history If this argument is missing,
                 1 (default) used.

    --dirName  - directory name to use for experiment.  If not given, the directory
                 name will be the default name DDmmmYY[optChar].  Cannot contain "/"

    --optChar  - optional character to be added to experiment directory if no dirName
                 given.  If dirName argument given, this argument ignored.  optChar
                 is used if the default directory name DDmmmYY is used for
                 more than one experiment created for a given instrument on a given day.
                 For example, if --optChar=h for a MLH experiment on September 12, 2005,
                 then the experiment directory created would be experiments/2005/mlh/12sep05h.

    --kindat  -  Set file kindat independently from one (or more) in file
    
    --experimentsDirNum - the number to be appended to the experiments directory, if experiments
                      directory being used is of the form experiments[0-9]* instead of just
                      experiments.  For example, if experimentsDirNum is 7, then the experiment
                      would be created in MADROOT/experiments7 instead of MADROOT/experiments.
                      Default is to create in experiments directory.
                      
    --PI - set Principal Investigator for this experiment
    
    --PIEmail - set PI email for this experiment
                      
    --fileAnalyst - set file analyst name for this file.  This will default to blank.
    
    --fileAnalystEmail - set file analyst email for this file.  This will default to blank.
    
    --createCachedText - create a cached text file in overview. Default is no cached file.
        Improves speed when text version is downloaded; uses disk space.
    
    --createCachedNetCDF4 - create a cached netCDF4 file in overview. Default is no cached file.
        Improves speed when netCDF4 version is downloaded; uses disk space.
"""

import sys
import os, os.path
import getopt
import traceback

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['madFilename=',
               'expTitle=',
               'permission=',
               'fileDesc=',
               'instCode=',
               'category=',
               'dirName=',
               'optChar=',
               'kindat=',
               'experimentsDirNum=',
               'PI=',
               'PIEmail=',
               'fileAnalyst=',
               'fileAnalystEmail=',
               'createCachedText',
               'createCachedNetCDF4']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
madFilename = None
expTitle = None
permission = None
fileDesc = None
instCode = None
category = 1
dirName = None
kindat = None
optChar = ''
experimentsDirNum = None
PI = ''
PIEmail = ''
fileAnalyst = ''
fileAnalystEmail = ''
createCachedText = False
createCachedNetCDF4 = False

for opt in optlist:
    if opt[0] == '--madFilename':
        madFilename = opt[1]
    elif opt[0] == '--expTitle':
        expTitle = opt[1]
    elif opt[0] == '--permission':
        permission = int(opt[1])
    elif opt[0] == '--fileDesc':
        fileDesc = opt[1]
    elif opt[0] == '--instCode':
        instCode = int(opt[1])
    elif opt[0] == '--category':
        category = opt[1]
    elif opt[0] == '--dirName':
        dirName = opt[1]
    elif opt[0] == '--kindat':
        kindat = int(opt[1])   
    elif opt[0] == '--optChar':
        optChar = opt[1]
        if len(optChar) != 1:
            raise ValueError('optChar argument must contain exactly one character, not %s' % (optChar))
    elif opt[0] == '--experimentsDirNum':
        experimentsDirNum = int(opt[1])
    elif opt[0] == '--PI':
        PI = opt[1]
    elif opt[0] == '--PIEmail':
        PIEmail = opt[1]
    elif opt[0] == '--fileAnalyst':
        fileAnalyst = opt[1]
    elif opt[0] == '--fileAnalystEmail':
        fileAnalystEmail = opt[1]
    elif opt[0] == '--createCachedText':
        createCachedText = True
    elif opt[0] == '--createCachedNetCDF4':
        createCachedNetCDF4 = True
    else:
        raise ValueError('Illegal option %s\n%s' % (opt[0], usage))

# check that all required arguments passed in
if madFilename == None:
    print('--madFilename argument required - must be full path to madrigal file')
    print(usage)
    sys.exit(0)

if expTitle == None:
    print('--expTitle argument required - must be experiment title')
    sys.exit(0)

if permission == None:
    print('--permission argument required - must be 0 for public, 1 for private')
    sys.exit(0)

if fileDesc == None:
    print('--fileDesc argument required - must be file description')
    sys.exit(0)

adminObj = madrigal.admin.MadrigalDBAdmin()

expDir = adminObj.createMadrigalExperiment(madFilename,
                                           expTitle,
                                           permission,
                                           fileDesc,
                                           instCode,
                                           category,
                                           optChar,
                                           dirName,
                                           kindat,
                                           experimentsDirNum,
                                           PI=PI,
                                           PIEmail=PIEmail,
                                           fileAnalyst=fileAnalyst,
                                           fileAnalystEmail=fileAnalystEmail,
                                           createCachedText=createCachedText,
                                           createCachedNetCDF4=createCachedNetCDF4,
                                           updateToMad3=True)

print('New experiment successfully created at %s with file %s - run updateMaster to register' % (expDir,
                                                                  madFilename))
