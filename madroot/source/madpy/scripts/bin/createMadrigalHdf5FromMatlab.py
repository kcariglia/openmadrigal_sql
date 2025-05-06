#!PYTHONEXE

"""createMadrigalHdf5FromMatlab.py reads in Matlab file with all information for a CEDAR file
and creates an output file.

$Id: createMadrigalHdf5FromMatlab.py 6540 2018-07-05 19:28:51Z brideout $
"""

usage = """"python createMadrigalHdf5FromMatlab.py <input_mat_file> [output_Madrigal_file]
If output_Madrigal_file not given, uses filename set in .mat file
"""

# standard python imports
import os, os.path, sys
import datetime

# third party imports
import scipy.io
import numpy

# Madrigal imports
import madrigal.cedar

if len(sys.argv) not in (2, 3):
    print(usage)
    sys.exit()
    
matlabFile = sys.argv[1]

# read in all local variables from *.mat file
mat_dict = scipy.io.loadmat(matlabFile)
if len(sys.argv) == 2:
    filename = mat_dict['filename'][0]
else:
    filename = sys.argv[2]
# convert all parm names with __plus__ to +
oneDParms = [str(parm[0][0]).replace('__plus__', '+') for parm in mat_dict['oneDParms']]
independent2DParms = [str(parm[0][0]).replace('__plus__', '+') for parm in mat_dict['independent2DParms']]
twoDParms = [str(parm[0][0]).replace('__plus__', '+') for parm in mat_dict['twoDParms']]
arraySplittingParms = [str(parm[0][0]).replace('__plus__', '+') for parm in mat_dict['arraySplittingParms']]
data = mat_dict['data']
principleInvestigator = mat_dict['principleInvestigator']
if len(principleInvestigator):
    principleInvestigator = str(principleInvestigator[0])
else:
    principleInvestigator = None
expPurpose = mat_dict['expPurpose']
if len(expPurpose):
    expPurpose = str(expPurpose[0])
else:
    expPurpose = None
expMode = mat_dict['expMode']
if len(expMode):
    expMode = str(expMode[0])
else:
    expMode = None
cycleTime = mat_dict['cycleTime']
if len(cycleTime):
    cycleTime = float(cycleTime[0])
else:
    cycleTime = None
correlativeExp = mat_dict['correlativeExp']
if len(correlativeExp):
    correlativeExp = str(correlativeExp[0])
else:
    correlativeExp = None
sciRemarks = mat_dict['sciRemarks']
if len(sciRemarks):
    sciRemarks = str(sciRemarks[0])
else:
    sciRemarks = None
instRemarks = mat_dict['instRemarks']
if len(instRemarks):
    instRemarks = str(instRemarks[0])
else:
    instRemarks = None
kindatDesc = mat_dict['kindatDesc']
if len(kindatDesc):
    kindatDesc = str(kindatDesc[0])
else:
    kindatDesc = None
analyst = mat_dict['analyst']
if len(analyst):
    analyst = str(analyst[0])
else:
    analyst = None
comments = mat_dict['comments']
if len(comments):
    comments = str(comments[0])
else:
    comments = None
history = mat_dict['history']
if len(history):
    history = str(history[0])
else:
    history = None
skipArray = bool(mat_dict['skipArray'])

all2DParms = independent2DParms + twoDParms # in python these are combined


recnoArr = numpy.unique(data[:,0])

newFile = madrigal.cedar.MadrigalCedarFile(filename, True, skipArray=skipArray)

# add each record individually
for recno in recnoArr:
    indices = numpy.where(data[:,0] == recno)[0]
    numRows = len(indices)
    kinst = int(data[indices[0],1])
    kindat = int(data[indices[0],2])
    ut1_unix = data[indices[0],3]
    ut2_unix = data[indices[0],4]
    sDT = datetime.datetime.fromtimestamp(ut1_unix, datetime.UTC)
    eDT = datetime.datetime.fromtimestamp(ut2_unix, datetime.UTC)
    
    newRec = madrigal.cedar.MadrigalDataRecord(kinst,
                 kindat,
                 sDT.year,sDT.month,sDT.day,sDT.hour,sDT.minute,sDT.second, 0,
                 eDT.year,eDT.month,eDT.day,eDT.hour,eDT.minute,eDT.second, 0,
                 oneDParms,
                 all2DParms,
                 numRows,
                 ind2DList=independent2DParms)
    
    # set 1D parms
    numSkip = 5 # 5 std parms in data to be skipped
    for i, parm in enumerate(oneDParms):
        newRec.set1D(parm, data[indices[0], numSkip+i]) 
        
    # set independent parms
    numSkip = 5 + len(oneDParms)
    for i, parm in enumerate(independent2DParms):
        newRec.set2DParmValues(parm, data[indices, numSkip+i])
        
    # set dependent 2D parms
    numSkip = 5 + len(oneDParms) + len(independent2DParms)
    for i, parm in enumerate(twoDParms):
        newRec.set2DParmValues(parm, data[indices, numSkip+i])
        
    # done - append record
    newFile.append(newRec)
    
# write result
newFile.write(arraySplittingParms=arraySplittingParms)

# add catalog and header
creatorObj = madrigal.cedar.CatalogHeaderCreator(filename)
creatorObj.createCatalog(principleInvestigator,
                         expPurpose,
                         expMode,
                         cycleTime,
                         correlativeExp,
                         sciRemarks,
                         instRemarks)
creatorObj.createHeader(kindatDesc, 
                        analyst, 
                        comments, 
                        history)
creatorObj.write()
    
