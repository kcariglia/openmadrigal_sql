"""testCedarReadWrite.py is a regression test that checks that a Cedar Madrigal file can go back and forth
from Madrigal (Hdf5) to Ascii.

By default test standard files; but can be used as a module to test any Madrigal 3 Hdf5 file

$Id: testCedarReadWrite.py 7555 2023-07-13 19:58:39Z brideout $
"""           

# standard python imports
import os, os.path, sys
import time, datetime
import subprocess
import traceback

# third party imports
import netCDF4
import numpy

# Millstone imports
import madrigal.metadata
import madrigal.cedar
import madrigal.data
import madrigal.isprint
import madrigal.derivation

def populateCedarWithAscii(cedarObj, textFile, dtype, oneDParms, twoDParms, indParms):
    """populateCedarWithAscii populates a MadrigalCedarFile object with data from a text file
    
    Inputs:
        cedarObj - a madrigal.cedar.MadrigalCedarFile to populate
        textFile - the text file to load
        dtype - a numpy data type.  Lists all columns in text file in order
        oneDParms - list of which parms are 1D
        twoDparms - list of which parms are 2D
        indParms - list of which parms are independent parameters
    """
    # create needed objects
    madInstObj = madrigal.metadata.MadrigalInstrument()
    
    madParmObj = madrigal.data.MadrigalParameters()
    
    # first pass creates a dictionary with key = line number where record starts, value = number of row
    lineDict = {}
    line_num = -1
    rows = 0
    last_recno = None
    last_first_line = None
    with open(textFile, 'r') as f:
        while(True):
            line = f.readline()
            if len(line) == 0:
                break
            line_num += 1
            items = line.split()
            if len(line) < 11:
                continue
            try:
                recno = int(items[6])
            except:
                # probably header line
                continue
            if not last_recno is None:
                if recno != last_recno:
                    lineDict[last_first_line] = rows
                    last_recno = recno
                    last_first_line = line_num
                    rows = 0
            else:
                last_recno = recno
                last_first_line = line_num
            rows += 1
            
        # add last record
        if last_first_line not in lineDict:
            lineDict[last_first_line] = rows
            
    # second pass - now actually populate the cedar object
    line_num = -1
    present_row = 0
    num_rows = None
    madRec = None
    with open(textFile, 'r') as f:
        while(True):
            line = f.readline()
            if len(line) == 0:
                break
            line_num += 1
            items = line.split()
            if len(line) < 11:
                continue
            try:
                recno = int(items[6])
            except:
                # probably header line 
                continue
            kindat = int(items[7])
            kinst = int(items[8])
            if line_num in list(lineDict.keys()):

                # append complete record is any
                if not madRec is None:
                    cedarObj.append(madRec)
                    cedarObj.dump()
                present_row = 0
                num_rows = lineDict[line_num]
                # get times
                sDT = datetime.datetime.fromtimestamp(float(items[9]), datetime.UTC)
                eDT = datetime.datetime.fromtimestamp(float(items[10]), datetime.UTC)
                # create Madrigal record
                madRec = madrigal.cedar.MadrigalDataRecord(kinst=kinst,
                    kindat=kindat,
                    sYear=sDT.year,sMonth=sDT.month,sDay=sDT.day,
                    sHour=sDT.hour,sMin=sDT.minute,sSec=sDT.second,sCentisec=sDT.microsecond/10000,
                    eYear=eDT.year,eMonth=eDT.month,eDay=eDT.day,
                    eHour=eDT.hour,eMin=eDT.minute,eSec=eDT.second,eCentisec=eDT.microsecond/10000,
                    oneDList=oneDParms,
                    twoDList=twoDParms,
                    nrow=num_rows,
                    madInstObj=madInstObj,
                    madParmObj=madParmObj, 
                    ind2DList=indParms)

                # set 1D parms - only need to do it once
                for column, parm in enumerate(dtype.names):
                    thisType = dtype[parm]
                    if 'f' in str(thisType):
                        try:
                            value = float(items[column])
                        except:
                            value = items[column]
                    elif 'i' in str(thisType):
                        try:
                            value = int(items[column])
                        except:
                            value = items[column]
                    else:
                        value = items[column]
                        
                    if madParmObj.getParmMnemonic(parm).lower() in oneDParms:
                        madRec.set1D(madParmObj.getParmMnemonic(parm).lower(), value)
                        
            # now set 2D values
            for column, parm in enumerate(dtype.names):
                thisType = dtype[parm]
                if 'f' in str(thisType):
                    try:
                        value = float(items[column])
                    except:
                        value = items[column]
                elif 'i' in str(thisType):
                    try:
                        value = int(items[column])
                    except:
                        value = items[column]
                else:
                    value = items[column]
                    
                if madParmObj.getParmMnemonic(parm).lower() in twoDParms:
                    madRec.set2D(madParmObj.getParmMnemonic(parm).lower(), present_row, value)
                    
            present_row += 1
            
    
    # at end, append last record and close
    cedarObj.append(madRec)
    cedarObj.dump()
    cedarObj.close()
    
    
def compareNetcdf4Ascii(netCDF4File, textFile, parms, oneDParms, arraySplitParms, indParms):
    """compareNetcdf4Ascii compares a netCDF4 file to an ascii file
    
    Inputs:
        netCDF4File - netCDF4 file
        textFile - text file to compare to
        parms - ordered list of parms to compare to
        oneDParms - list of one D Parms
        arraySplitParms -  list of array split parms
        indParms - indpendent parms
        
    Returns True if no differences found; prints first difference if difference and returns False
    """
    rootgrp = netCDF4.Dataset(netCDF4File)
    # get groups
    nc_groups = rootgrp.groups
    if len(nc_groups) == 0:
        group_dict = None
    else:
        group_dict = {} # key - group name, values = values of araySplitParms
        for g in nc_groups:
            group_dict[g] = [None for item in arraySplitParms]
            items = g.split('_')
            for item in items:
                if item.find('=') == -1:
                    continue
                subitems = item.split('=')
                for i, parm in enumerate(arraySplitParms):
                    if subitems[0] == parm:
                        group_dict[g][i] = float(subitems[1])
                        break
        
    
    with open(textFile) as f:
        while(True):
            line = f.readline()
            if len(line) == 0:
                break
            items = line.split()
            if len(items) < 11:
                continue
            try:
                year = int(items[0])
            except:
                continue # header line
            # parse each line into a dictionary
            dataDict = {}
            for i, item in enumerate(items):
                try:
                    value = int(item)
                except ValueError:
                    try:
                        value = float(item)
                    except:
                        value = item
                dataDict[parms[i]] = value
                
            # figure out section
            if group_dict is None:
                g = ''
            else:
                this_group = None
                for g in list(group_dict.keys()):
                    values = group_dict[g]
                    match = True
                    for i, p in enumerate(arraySplitParms):
                        if type(p) == bytes:
                            pString = p.decode('utf8')
                        else:
                            pString = p
                        if values[i] is not None:
                            if not numpy.isclose(dataDict[pString], values[i]):
                                match = False
                                break
                    if match:
                        this_group = g
                        break
                    
                if this_group is None:
                    raise ValueError('found no group for line %s' % (line))
            
            # get indices for this row
            ts = numpy.array(rootgrp['/%s/timestamps' % (g)])
            
            ts_index = numpy.where(numpy.abs(dataDict['ut1_unix'] - ts) < 0.1)
            if len(ts_index) != 1:
                raise ValueError('did not find ts %f' % (dataDict['ut1_unix']))
            
            indices = [ts_index[0][0]]
            
            # add indParms if any
            for p in indParms:
                d = numpy.array(rootgrp['/%s/%s' % (g, p)])
                this_index = numpy.where(dataDict[p] == d)
                if len(this_index[0]) != 1:
                    raise ValueError('did not find %s %f' % (p, dataDict[p]))
                indices.append(this_index[0][0])
                
            # finally, we are ready to start comparing data for this line
            for p in parms[11:]:
                data = rootgrp['/%s/%s' % (g, p)]
                shape = data.shape
                if len(shape) == 1:
                    if p not in indParms:
                        value = data[indices[0]]
                    else:
                        value = data[indices[1+indParms.index(p)]]
                elif len(shape) == 2:
                    value = data[indices[0]][indices[1]]
                elif len(shape) == 3:
                    value = data[indices[0]][indices[1]][indices[2]]
                elif len(shape) == 4:
                    value = data[indices[0]][indices[1]][indices[2]][indices[3]]
                else:
                    raise ValueError('shape %s' % (str(shape)))
                if not numpy.isclose(value, dataDict[p]):
                    if not(numpy.isnan(value) and numpy.isnan(dataDict[p])):
                        print(('at line %s got error with parm %s: %s versus %s' % (line, p, str(value), str(dataDict[p]))))
                        return(False)
            
    
    return(True)


def regressionTestFile(inputFile):
    t = time.time()
    overall_success = True # until proven otherwise
    
    # verify we are in the right directory
    pwd = os.getcwd()
    base_dir = os.path.basename(pwd)
    if base_dir != 'regressionTests':
        raise IOError('pwd directory must be regressionTests, not %s' % (pwd))
    
    if not os.access(inputFile, os.R_OK):
        raise ValueError('Cannot read input file %s' % (inputFile))
    
    # make sure tmp exists and is empty
    if not os.access('tmp', os.W_OK):
        os.mkdir('tmp')
    else:
        os.system('rm -f tmp/*')
        
    cedarObj = madrigal.cedar.MadrigalCedarFile(inputFile)
    print(('time to import into MadrigalCedarFile %f seconds' % (time.time()-t)))
    t = time.time()
    
    # get parms and dimensions to allow reloading into CEDAR Madrigal format
    dtype = cedarObj.getDType()
    oneDParms = cedarObj.get1DParms()
    twoDParms = cedarObj.get2DParms()
    indParms = cedarObj.getIndSpatialParms()
    kinst = cedarObj.getKinstList()[0]
    kindatList = cedarObj.getKindatList()
    kindat = cedarObj.getKindatList()[0]
    arraySplitParms = cedarObj.getArraySplitParms()
    if len(kindatList) > 0:
        if 'kindat' not in arraySplitParms:
            arraySplitParms.append('kindat')
    
    # make sure all parms are most modern name
    madParmObj = madrigal.data.MadrigalParameters()
    oneDParms = [madParmObj.getParmMnemonic(p).lower() for p in oneDParms]
    twoDParms = [madParmObj.getParmMnemonic(p).lower() for p in twoDParms]
    indParms = [madParmObj.getParmMnemonic(p).lower() for p in indParms]
    if not arraySplitParms is None:
        arraySplitParms = [madParmObj.getParmMnemonic(p).lower() for p in arraySplitParms]
        
    # use isprint to write file to ascii with set parms
    parm_list = [madParmObj.getParmMnemonic(parm).lower() for parm in dtype.names]
    madFilter = madrigal.derivation.MadrigalFilter('recno', [[numpy.nan, 2],])
    text1 = 'tmp/test1.txt'
    madrigal.isprint.Isprint(inputFile, text1, parm_list, [madFilter], summary='plain')
    print(('time to export with isprint %f seconds' % (time.time()-t)))
    
    hdf_test1 = os.path.join(pwd, 'tmp', 'test1.hdf5')
    cedarObj2 = madrigal.cedar.MadrigalCedarFile(hdf_test1, createFlag=True,
                                                 arraySplitParms=arraySplitParms)
    
    populateCedarWithAscii(cedarObj2, text1, dtype, oneDParms, twoDParms, indParms)
    print(('time at create Cedar %f' % (time.time()-t)))
    
    
    text2 = 'tmp/test2.txt'
    madrigal.isprint.Isprint(hdf_test1, text2, parm_list, [])
    print(('time at second export to text %f seconds' % (time.time()-t)))
    
    # diff two files
    os.chdir(pwd)
    cmd = 'diff %s %s' % (os.path.join(pwd, text1), os.path.join(pwd, text2))
    outs = None
    proc = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        outs, errs = proc.communicate(timeout=15)
    except:
        traceback.print_exc()
        outs, errs = proc.communicate(timeout=15)
        overall_success = False
    if outs is not None and len(outs) > 0:
        overall_success = False   
    
    netCdf1 = 'tmp/mlh_test1.nc'
    madrigal.cedar.convertToNetCDF4(hdf_test1, netCdf1)
    print(('time at export to netCDF4 %f seconds' % (time.time()-t)))
    
    result = compareNetcdf4Ascii(netCdf1, text2, parm_list, oneDParms, arraySplitParms, indParms)
    print(('time at end %f seconds' % (time.time()-t)))
    
    if not result:
        overall_success = False
    
    if (overall_success):
        print('SUCCESS')
    else:
        print('FAILED')
        
    return(overall_success)
        
if __name__ == '__main__':
    # default is to run regression test against standard files, but module can be used to test any Madrigal 3 hdf5   
    
    inputFiles = ['../../../../experiments/1998/mlh/20jan98/mlh980120g.002.hdf5',
              '../../../../experiments/1995/jro/01feb95/jic950201g.001.hdf5',
              '../../../../experiments/1997/aro/06jan97/are970106g.001.hdf5',
              '../../../../experiments/1997/lyr/08apr97/lyr970408g.001.hdf5',
              '../../../../experiments/1998/jro/27apr98/jro980427d.001.hdf5',
              ]

    overall_success = True
    for f in inputFiles:
        print(('Testing %s' % (f)))
        try:
            if not regressionTestFile(f):
                overall_success = False
        except:
            traceback.print_exc()
            sys.exit(-1)
            
    print('running test of mergeCedarFiles')
    input1Hdf5 = '../../../../experiments/2017/mlh/14aug17/mlh170814a.000.hdf5'
    input2Hdf5 = '../../../../experiments/2017/mlh/14aug17/mlh170814b.000.hdf5'
    outputHdf5 = '../../../../experiments/2017/mlh/14aug17/mlh170814o.000.hdf5'
    
    for skipArray in (True, False):
        for includeDuplicate in (True, False):
            print('testing mergeCedarFiles skipArray %s and includeDuplicate %s' % (str(skipArray), str(includeDuplicate)))
            try:
                madrigal.cedar.mergeCedarFiles(input1Hdf5, input2Hdf5, outputHdf5, includeDuplicate=includeDuplicate, 
                                               skipArray=skipArray)
            except:
                overall_success = False
                traceback.print_exc()
            
            try:
                os.remove(outputHdf5)
            except:
                pass
                
    if (overall_success):
        print('SUCCESS')
    else:
        print('FAILED')
            
            
    
    