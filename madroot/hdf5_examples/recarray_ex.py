"""This is a very simple example of how to create an hdf5 Madrigal export file via madrigalWeb API

It uses madrigalWeb, but needs to use madrigal.cedar for the header and catalog records

$Id: recarray_ex.py 3331 2011-03-02 20:23:46Z brideout $
"""

import os, os.path, sys
import datetime

import numpy
import h5py

import madrigalWeb.madrigalWeb

# hardcoded arguments - this needs to be improved
filename = '/opt/madrigal/experiments/2010/mlh/22feb10/mlh100222g.002'
madUrl = 'http://madrigal.haystack.mit.edu/madrigal'
hdf5Filename = '/Users/brideout/Documents/tmp/mlh100222.hdf5'
user_fullname = 'Bill Rideout'
user_email = 'brideout@haystack.mit.edu'
user_affiliation = 'MIT'


madWebObj = madrigalWeb.madrigalWeb.MadrigalData(madUrl)

# open output HDF5 file
f = h5py.File(hdf5Filename)

# first step - find out what parameters are in the file
madParmList = madWebObj.getExperimentFileParameters(filename)

# next, we search through the parameters to find out five things:
#  1. Which parameters do we want in the HDF5 output?
#  2. What is the longest parameter mnemonic string?
#  3. What is the longest parameter description string?
#  4. What is the longest parameter units string?
#  5. What is the longest parameter category string?
# We need to know the longest strings because strings must all be the same length.
parmsWantedList = [] 
longestMnemStr = 0
longestDescStr = 0
longestUnitsStr = 0
longestCategoryStr = 0
for parm in madParmList:
    if parm.isMeasured or parm.mnemonic.lower() in ('year', 'month', 'day', 'hour', 'min', 'sec', 'recno'):
        if parm.isAddIncrement:
            # these parameters are not needed in HDF5 output - only the main parameter is used
            # because it already includes the additional increment
            continue
        parmsWantedList.append(parm)
        if len(parm.mnemonic) > longestMnemStr:
            longestMnemStr = len(parm.mnemonic)
        if len(parm.description) > longestDescStr:
            longestDescStr = len(parm.description)
        if len(parm.units) > longestUnitsStr:
            longestUnitsStr = len(parm.units)
        if len(parm.category) > longestCategoryStr:
            longestCategoryStr = len(parm.category)
            
# next task - create a numpy recarray that describes all the parameters in the file.  The columns will be
#  1. mnemonic (string) Example 'dti'
#  2. description (string) Example: 'F10.7 Multiday average observed (Ott)'
#  3. isError (int) 1 if error parameter, 0 if not
#  4. units (string) Example 'W/m2/Hz'
#  5. category (string) Example: 'Time Related Parameter'

parmArr = numpy.recarray((len(parmsWantedList),),
                         dtype = [('mnemonic', '|S%i' % (longestMnemStr)),
                                  ('description', '|S%i' % (longestDescStr)),
                                  ('isError', 'int'),
                                  ('units', '|S%i' % (longestUnitsStr)),
                                  ('category', '|S%i' % (longestCategoryStr))])


# set all the values
for i in range(len(parmsWantedList)):
    parm = parmsWantedList[i]
    parmArr['mnemonic'][i] = parm.mnemonic
    parmArr['description'][i] = parm.description
    parmArr['isError'][i] = parm.isError
    parmArr['units'][i] = parm.units
    parmArr['category'][i] = parm.category
    
# write parameter description to top level
dset = f.create_dataset('Parameter Descriptions', data=parmArr)

# next, we need to deal with the actual data in the file

# create parms string with comma-delimited mnemonics for isprint
parmsStr = 'year,month,day,hour,min,sec,recno,'  # these parameters are always used
for i in range(len(parmsWantedList)):
    parm = parmsWantedList[i]
    if parm.mnemonic.lower() in ('year','month','day','hour','min','sec','recno'):
        continue
    parmsStr += parm.mnemonic.lower()
    if i < len(parmsWantedList) - 1:
        parmsStr += ','
        
data = madWebObj.isprint(filename, parmsStr, '', user_fullname, user_email, user_affiliation)
data = data.strip()
lines = data.split('\n')
numRows = len(lines)

# the next step is to create a numpy.recarray to hold all this data

# first build the needed dtype dynamically
dataDtype = [('year', int), ('month', int), ('day', int), ('hour', int), ('min', int), ('sec', int), ('recno', int)]
for i in range(len(parmsWantedList)):
    parm = parmsWantedList[i]
    if parm.mnemonic.lower() in ('year','month','day','hour','min','sec','recno'):
        continue
    dataDtype.append((parm.mnemonic.lower(), 'float'))
    
dataArr = numpy.recarray((numRows,), dtype = dataDtype)

# next put all the data into dataArr
for i in range(len(lines)):
    items = lines[i].split() # len(items) = 7 + len(parmsWantedList)
    for j, key in enumerate(('year','month','day','hour','min','sec','recno')):
        dataArr[key][i] = int(items[j])
        
    for j in range(len(parmsWantedList)):
        parm = parmsWantedList[j]
        if parm.mnemonic.lower() in ('year','month','day','hour','min','sec','recno'):
            continue
        try:
            value = float(items[j+7])
        except:
            value = numpy.nan
        dataArr[parm.mnemonic.lower()][i] = value
        
# write data to top level
dset = f.create_dataset('Data', data=dataArr)

# add catalog and header text
# here I simply read it from a file, but should really be read using madrigal.cedar

catalogFile = open('/Users/brideout/Documents/tmp/cat_mlh100222g.001')
headerFile = open('/Users/brideout/Documents/tmp/head_mlh100222g.001')

catalogLines = catalogFile.readlines()
headerLines = headerFile.readlines()

catalogFile.close()
headerFile.close()

numLines = len(catalogLines) + len(headerLines)

# create a recarray to hold this text
textArr = numpy.recarray((numLines,), dtype=[('File Notes', '|S80')])
lineCount = 0
for catLine in catalogLines:
    textArr['File Notes'][lineCount] = catLine
    lineCount += 1
for headLine in headerLines:
    textArr['File Notes'][lineCount] = headLine
    lineCount += 1
    
# write data to top level
dset = f.create_dataset('Experiment Notes', data=textArr)

# get experiment summary information
madExp = madWebObj.getExperiments(30, 2010, 2, 22, 0, 0, 0, 2010, 2, 22, 23, 59, 59, 1)[0]
madExpFile = madWebObj.getExperimentFiles(madExp.id)[0]
madInstList = madWebObj.getAllInstruments()
for item in madInstList:
    if item.code == 30:
        madInst = item
        
instrumentName = madExp.instname
expName =  madExp.name
startDate = datetime.datetime(madExp.startyear, madExp.startmonth, madExp.startday,
                              madExp.starthour, madExp.startmin, madExp.startsec)
startDateStr = startDate.strftime('%Y-%m-%d %H:%M:%S UT')
endDate = datetime.datetime(madExp.endyear, madExp.endmonth, madExp.endday,
                            madExp.endhour, madExp.endmin, madExp.endsec)
endDateStr = endDate.strftime('%Y-%m-%d %H:%M:%S UT')
cedarFileName = madExpFile.name
kindatDesc = madExpFile.kindatdesc
statusDesc = madExpFile.status
instLat = madInst.latitude
instLon = madInst.longitude
instAlt = madInst.altitude

# create an expSummary numpy recarray
summArr = numpy.recarray((1,), dtype = [('instrument', '|S%i' % (len(instrumentName))),
                                        ('experiment name', '|S%i' % (len(expName))),
                                        ('start time', '|S%i' % (len(startDateStr))),
                                        ('end time', '|S%i' % (len(endDateStr))),
                                        ('Cedar file name', '|S%i' % (len(cedarFileName))),
                                        ('kind of data file', '|S%i' % (len(kindatDesc))),
                                        ('status description', '|S%i' % (len(statusDesc))),
                                        ('instrument latitude', 'float'),
                                        ('instrument longitude', 'float'),
                                        ('instrument altitude', 'float')])
summArr['instrument'][0] = instrumentName
summArr['experiment name'][0] = expName
summArr['start time'][0] = startDateStr
summArr['end time'][0] = endDateStr
summArr['Cedar file name'][0] = cedarFileName
summArr['kind of data file'][0] = kindatDesc
summArr['status description'][0] = statusDesc
summArr['instrument latitude'][0] = instLat
summArr['instrument longitude'][0] = instLon
summArr['instrument altitude'][0] = instAlt

# write data to top level
dset = f.create_dataset('Experiment Summary', data=summArr)


f.close()
