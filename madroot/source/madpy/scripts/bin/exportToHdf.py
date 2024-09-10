#!PYTHONEXE

#$Id: exportToHdf.py 7359 2021-03-31 20:54:00Z brideout $

usage = """
exportToHdf.py is a script used to convert a Madrigal 2 file to Madrigal 3 hdf5 format.

Required arguments:

    --cedarFilename - full path of existing Madrigal file. 

    --hdf5Filename - full path of hdf5 file to write.

Optional arguments - set these to add layouts, parameters or filters.  Default is to use cachedFiles.ini:
    
    --independentSpatialParms - a comma separated list of parameters as mnemonics
        that represent independent spatial variables.  Causes array layout to be added to 
        output Hdf5 file.  If not given, uses $MADROOT/cachedFiles.ini

    --arraySplittingParms - a comma separated list of parameters as mnemonics used to split
        arrays into subarrays.  For example, beamcode would split data with separate beamcodes
        into separate arrays. The number of separate arrays will be up to the product of the number of 
        unique values found for each parameter, with the restriction that combinations with no records will
        not create a separate array. .  If not given, uses $MADROOT/cachedFiles.ini
    
    --extraParameters - These parameters will be added to the output file if 
                        they are not already in the input file. Comma-delimited.  Default is no
                        extra parameter
                        
    --filter - Filter argument as in isprint command as string (eg, 'ti.500,2000') Only one allowed.
               Default is no filtering
               
    --status - use to set status of file.  Default is 1 (default).  Use -1 to get from fileTab (error raised
            if not available)

Example:
    exportToHdf --cedarFilename=/opt/madrigal/experiments/1998/mlh/20jan98/mil20100112.001
                --hdf5Filename=/home/user/data/mil20100112.hdf5
                --independentSpatialParms=range
                --arraySplittingParms=kinst,pl,mdtyp
                --extraParameters=ti,te
                --filter=ti,500,1000
"""

import sys
import os, os.path
import getopt
import traceback
import madrigal.data

# parse command line
arglist = 'h'
longarglist = ['cedarFilename=',
               'hdf5Filename=',
               'independentSpatialParms=',
               'arraySplittingParms=',
               'extraParameters=',
               'filter=',
               'status='
               'help']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
cedarFilename = None
hdf5Filename = None
independentSpatialParms = None
arraySplittingParms = None
extraParameters = []
filter = None
status = '1'

for opt in optlist:
    if opt[0] == '--cedarFilename':
        cedarFilename = opt[1]
    elif opt[0] == '--hdf5Filename':
        hdf5Filename = opt[1]
    elif opt[0] == '--independentSpatialParms':
        independentSpatialParms = opt[1].split(',')
    elif opt[0] == '--arraySplittingParms':
        arraySplittingParms = opt[1].split(',')
    elif opt[0] == '--extraParameters':
        extraParameters = opt[1].split(',')
    elif opt[0] == '--filter':
        filter = opt[1]
    elif opt[0] == '--status':
        status = opt[1]
        if status == '-1':
            status = None
    elif opt[0] in ('-h', '--help'):
        print(usage)
        sys.exit(0)
        
    else:
        raise ValueError('Illegal option %s\n%s' % (opt[0], usage))

# check that all required arguments passed in
if cedarFilename == None:
    print('--cedarFilename argument required - must be full path of existing madrigal file')
    print(usage)
    sys.exit(-1)

if hdf5Filename == None:
    print('--hdf5Filename argument required - must be full path of hdf5 file to write')
    sys.exit(-1)
    
fileObj = madrigal.data.MadrigalFile(cedarFilename)

# read the cachedFiles.ini to see if information needed
kinst = fileObj.getKinstList()[0]
kindat = fileObj.getKindatList()[0]
x,y,x = fileObj._parseCachedIni(kinst, kindat)
iniExtraParms, altFormatDict, skipArray = fileObj._parseCachedIni(kinst, kindat)
iniSpatialParms = []
iniSplittingParms=[]
if 'array' in altFormatDict:
    value = altFormatDict['array']
    if type(value)  in (bytes, str):
        iniSpatialParms = [value]
    elif len(value) == 2 and type(value[0]) in (tuple, list):
        iniSpatialParms = value[0]
        iniSplittingParms = value[1]
    else:
        iniSpatialParms = value
if independentSpatialParms is None:
    independentSpatialParms = iniSpatialParms
if arraySplittingParms is None:
    arraySplittingParms = iniSplittingParms

fileObj.exportToHdf(output = hdf5Filename,
                    independentSpatialParms = independentSpatialParms,
                    arraySplittingParms = arraySplittingParms,
                    extraParameters = extraParameters,
                    filter = filter, skipArray = skipArray, status=status)

print()
print('The file %s has been converted to hdf5 format: %s' % (cedarFilename, hdf5Filename))
