"""convertParcods.py converts to old parcods.tab file to a comma delimited parmCodes.txt file

By removing column delimited layout, all limits on number of codes are removed.  Also removes deprecated
scaling factor, which has no meaning in modern Hdf5 format, and additional increment parameters.

    The following is the column layout of parcods.tab
       0-7     Code
       10-48   Description   Note: DESC_LEN   = 40
       50-60   Int16Desc     Note: DESC16_LEN = 12
       62-68   ScaleFactor
       70-77   Units         Note: UNIT_LEN   =  9
       81-100  Mnemonic      Note: MNEM_LEN   = 21
       105-112 Format
       114-115 Width
       118-120 CatId
       122-122 hasDesc  - does this mnemonic have an html description?
       124-124 hasErrDesc - does this error mnemonic have an html description?
       
Throws error if any repeated mnemonics, or non-zero codes.  Moves parameters listed as duplicates
in MADROOT/metadata/parcods_duplicates.txt to the end, and notes the main parameter

Output file has the following comma-limited fields:
    1. code
    2. description
    3. units
    4. mnemonic
    5. format
    6. width
    7. categoryId
    8. hasDesc
    9. hasErrDesc
   10. isDuplicateOfCode (optional field)
    
Parameters that are duplicates of others will be converted by the API to the main parameter.
If users insist on using the duplicate version, a warning will be printed.  The duplicate
parameters will be listed last under a comment lines that warns they are deprecated.

$Id: convertParcods.py 7045 2019-10-07 19:56:46Z brideout $
"""

usage = 'python convertParcods.py <original parcods.txt> <outputDir>'

import os, os.path, sys

if len(sys.argv) != 3:
    print(usage)
    sys.exit(-1)
    
inputFile = sys.argv[1]
outputDir = sys.argv[2]
foundMnemonics = []
foundCodes = []

if not inputFile.endswith('parcods.tab'):
    print(('Input file must be parcods.tab, not <%s>' % (inputFile)))
    
# get duplicate dict
def get_duplicate_dict(dup_file_name):
    """get_duplicate_dict returns a dict with key = main code, value = list of duplicate codes
    based in input file dup_file_name
    """
    f = open(dup_file_name)
    lines = f.readlines()
    f.close()
    ret_dict = {}
    for line in lines:
        if line[0] == '#':
            continue
        items = line.split(':')
        if len(items) != 2:
            continue
        main_code = int(items[0])
        values = [int(code) for code in items[1].split(',')]
        ret_dict[main_code] = values
        
    return(ret_dict)

def get_main_code(dup_dict, code):
    """get_main_code returns the main code assocated with code, if code is a duplicate
    as defined in dup_dict (see get_duplicate_dict for definition of dup_dict)
    """
    for key in dup_dict:
        if code in dup_dict[key]:
            return(key)
        
    return(None)
        
    
dup_dict = get_duplicate_dict(os.path.join(os.path.dirname(inputFile), 'parcods_duplicates.txt'))
    
f = open(inputFile)
lines = f.readlines()
f.close()

header = """# This file defines all CEDAR parameters.  The columns are:
#    1. code (a unique positive integer)
#    2. description (parameter description.  Must not contain a comma)
#    3. units (units description)
#    4. mnemonic (a unique char string without spaces, all caps)
#    5. format (how to format the data in a report, for example (%9.0f)
#    6. width (number of spaces for data - must be greater than format)
#    7. categoryId (category id of this parameter as defined in madCatTab.txt)
#    8. hasDesc (1 if there is a link to this parameter in doc/parmDesc.html,0 if not)
#    9. hasErrDesc (1 if there is a link to the error version of this parameter in doc/parmDesc.html,0 if not)
#    10. isDuplicateOfCode (This field should not be used for new parms - optional field for old version of
#         CEDAR database format with 16 bit integers when there were occasionally multiple parms for
#         dynamic ranges reasons - now irrelevant with Hdf5 format.)
#
#  If you edit this file to add a new parameter, please run $MADROOT/source/madpy/scripts/bin/checkParmsCodes.py <new_version>
#  to check for problems.  Please also email new version to brideout@haystack.mit.edu .
#
"""

dup_text = """#
# The following parameters are deprecated because they are duplicates of the main parameter, 
# whose code is listed at the end of the line.  They exist only because the original Cedar
# database format used 16 bit integers to store data. Please do not use them.
#
"""

fout = open(os.path.join(outputDir, 'parmCodes.txt'), 'w')
fout.write(header)
for line in lines:
    if len(line.strip()) == 0:
        continue
    code = int(line[0:8].strip())
    if code > 0 and code in foundCodes:
        raise ValueError('duplicate code in line <%s>' % (line))
    description = line[10:49].strip()
    if description.find(',') != -1:
        description = description.replace(',', ';')
    if description.lower().find('additional increment') != -1:
        continue
    units = line[70:78].strip()
    if units.find(',') != -1:
        units = units.replace(',', ';')
    mnemonic = line[81:101].upper().strip()
    if mnemonic.find(',') != -1:
        raise ValueError('Illegal comma in mnemonic in line <%s>' % (line))
    if mnemonic in foundMnemonics:
        raise ValueError('duplicate mnemonic in line <%s>' % (line))
    format = line[105:113].strip()
    if format.find(',') != -1:
        raise ValueError('Illegal comma in format in line <%s>' % (line))
    if format == '%c':
        # use string format
        format = '%1s'
    width = int(line[114:116].strip())
    categoryId = int(line[118:121].strip())
    hasDesc = int(line[122:123].strip())
    hasErrDesc = int(line[124:125].strip())
    main_code = get_main_code(dup_dict, code)
    if main_code == None:
        fout.write('%i,%s,%s,%s,%s,%i,%i,%i,%i\n' % (code, description, units, mnemonic, format,
                                                     width,categoryId, hasDesc, hasErrDesc))
    else:
        dup_text += '%i,%s,%s,%s,%s,%i,%i,%i,%i,%i\n' % (code, description, units, mnemonic, format,
                                                     width,categoryId, hasDesc, hasErrDesc, main_code)
    foundMnemonics.append(mnemonic)
    if code > 0:
        foundCodes.append(code)
        
fout.write(dup_text)
fout.close()

    