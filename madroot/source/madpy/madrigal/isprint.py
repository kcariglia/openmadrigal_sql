"""isprint.py is a module that creates new output files based on an input file and lists of parameters and filters.

$Id: isprint.py 7044 2019-10-07 19:13:16Z brideout $
"""
# standard python imports
import os, os.path, sys
import datetime
import math
import tempfile
import shlex
import shutil
import random
import subprocess

# third party imports
import numpy

# Madrigal imports
import madrigal.cedar
import madrigal.derivation
import madrigal._derive

# public methods

def isRequest1D(existingFile, desiredParmList):
    """isRequest1D returns True if desiredParmList parms do not contain any measured or derivable 2D parms
    """
    non1DSet = madrigal.derivation.getUnderivableParms(existingFile.get1DParms(), desiredParmList)
    if len(non1DSet) == 0:
        return(True)
    # may also be True if all in non1DSet underivable
    underivableSet = madrigal.derivation.getUnderivableParms(existingFile.get1DParms() + existingFile.get2DParms(), 
                                                             desiredParmList)
    if len(non1DSet) == len(underivableSet):
        return(True)
    else:
        return(False)
    

class Isprint:
    
    def __init__(self, orgFilename, output, desiredParmList, filterList,
                 indSpatialParms=None,
                 summary='summary', showHeaders=False, 
                 missing=None, assumed=None, knownbad=None, ignoreEmptyFile=True):
        """
        Takes the input Cedar Hdf5 file and writes modified version
        
        Inputs:
            orgFilename - full path to original Cedar Hdf5 file
            output - output filename - If None, writes text file to stdout.  If given and extension is one of
                 <.hdf5, .h5, .hdf>, then will save as Hdf5 file.  If extension is .nc, will be saved as netCDF4. In
                 all other cases, will be saved as ascii text.  If hdf5 or netCDF4, arguments (header, summary, badval,
                 assumed, and knownbad) ignored if given.
            desiredParmList - a list of ordered desired parameters as lower case mnemonics
            filterList - a list of 0 or more madrigal.derivation.MadrigalFilter objects
            indSpatialParms - a list of lower case mnemonics to use as independent spatial parameters.  If None
                (the default), use independent spatial parameters from original file.
            
            (The following arguments are ignored if not ascii format)
            
            summary - type of summary line to print at top.  Allowed values are:
                'plain' - text only mnemonic names, but only if not showHeaders
                'html' - mnemonic names wrapped in standard javascript code to allow descriptive popups
                'summary' - print overview of file and filters used. Also text only mnemonic names, 
                    but only if not showHeaders (default)
                None - no summary line
                
            showHeaders - if True, print header in format for each record.  If False, the default,
                do not.
                
            missing, assumed, knownbad - how to print Cedar special values.  Default is None for
                all, so that value printed in value in numpy table as per spec.
                
            (The following argument is ignored if ascii format)
            
            ignoreEmptyFile - if False, raise IOError when trying to create an Hdf5 or netCDF4
                file with no records.  If True (the default), then simply create 0 byte file. Ignored
                if output not hdf5 or netCDF4
        """
        self._numRec = 1000 # sets the number of records to load from the file at once. RAM/speed tradeoff
        
        # if orgFilename more than 2 MB in size, uncompress it first to speed things up
        if os.path.getsize(orgFilename) > 2E6:
            is_tmp = True
            orgFilename = self._getTempUncompressed(orgFilename)
        else:
            is_tmp = False
        
        startDT, endDT = self._getStartEndTimes(filterList)
        maxRecno = self._getMaxRecno(filterList) # sets a value only if recno used as a filter and max value given
        totalRecsLoaded = self._numRec
        
        if output is not None:
            fileName, fileExtension = os.path.splitext(output)
            if fileExtension.lower() in ('.hdf5', '.h5', '.hdf'):
                format = 'hdf5'
                tmpOutput = output
            elif fileExtension.lower() in ('.nc'):
                format = 'netCDF4'
                tmpOutput = fileName + '.hdf5'
            else:
                format = 'text'
                tmpOutput = None
        else:
            format = 'text'
            tmpOutput = None
        
        
        # this can fail if time filters too restrictive
        try:
            existingFile = madrigal.cedar.MadrigalCedarFile(orgFilename, startDatetime=startDT,
                                                            endDatetime=endDT, maxRecords=self._numRec)
        except ValueError:
            if is_tmp:
                os.remove(orgFilename)
            if format == 'text':
                # add message to text file
                if output is not None:
                    f = open(output, 'a')
                else:
                    f = sys.stdout
                f.write('\nNo records were selected with the filters selected\n')
                if output is not None:
                    f.close()
            elif not ignoreEmptyFile:
                raise IOError('No records selected with input time filters - unable to create file')
            
            else:
                # create empty file
                f = open(output, 'w')
                f.close()
            return
        
        # make sure some records loaded
        found = False
        try:
            oneD = existingFile.get1DParms()
            found = True
        except ValueError:
            while(True):
                result = existingFile.loadNextRecords(numRecords=self._numRec)
                if result[0] > 0:
                    found = True
                    break
                if result[1]:
                    break
                
        if not found:
            # no records with time restriction
            if is_tmp:
                os.remove(orgFilename)
            if format == 'text':
                # add message to text file
                if output is not None:
                    f = open(output, 'a')
                else:
                    f = sys.stdout
                f.write('\nNo records were selected with the filters selected\n')
                if output is not None:
                    f.close()
            elif not ignoreEmptyFile:
                raise IOError('No records selected with input time filters - unable to create file')
            
            else:
                # create empty file
                f = open(output, 'w')
                f.close()
            return
        

        # see if we need to extract any indSpatialParms or arraySplitParms from file
        if format in ('hdf5', 'netCDF4'):
            # check is this a one-D request
            is1D = isRequest1D(existingFile, desiredParmList)
            if not is1D:
                if indSpatialParms is None:
                    indSpatialParms = existingFile.getIndSpatialParms()
                arraySplitParms = existingFile.getArraySplitParms()
                for parm in indSpatialParms + arraySplitParms:
                    if type(parm) in (bytes, numpy.bytes_):
                        parm = parm.decode("ascii")
                    if parm.lower() not in desiredParmList:
                        desiredParmList.append(parm.lower())
            else:
                indSpatialParms = None
                arraySplitParms = None
        else:
            # not relevant to text files
            indSpatialParms = None
            arraySplitParms = None
            
        numRec = self._numRec
        
        madDevObj = madrigal.derivation.MadrigalDerivation(existingFile, desiredParmList, filterList, fullFilename=tmpOutput,
                                                           indParms=indSpatialParms, arraySplitParms=arraySplitParms)
        
        if len(madDevObj._madDerivationPlan.underivableFilterList) > 0:
            if format == 'text':
                # add message to text file
                if output is not None:
                    f = open(output, 'a')
                else:
                    f = sys.stdout
                f.write('\nNo records were selected with the filters above\n')
                if output is not None:
                    f.close()
            elif not ignoreEmptyFile:
                raise IOError('No records selected with input time filters')
            
            else:
                # create empty file
                f = open(output, 'w')
                f.close()
            if is_tmp:
                os.remove(orgFilename)
            return
        
        newFile = madDevObj.getNewCedarFile()
        
        isFirstWrite = True
        
        shouldBreak = False
        
        while (True):
            
            # output newFile in text or hdf5
            if format == 'text':
                newFile.writeText(newFilename=output, missing=missing, assumed=assumed, knownbad=knownbad, filterList=filterList, 
                                  summary=summary, showHeaders=showHeaders, selectParms=desiredParmList, append=True,
                                  firstWrite=isFirstWrite)
                # no need for another summary
                isFirstWrite = False
                
            else:
                newFile.dump()
                
            if shouldBreak:
                break
                
            numRec, isComplete = madDevObj.loadRecords(self._numRec)
            
            totalRecsLoaded += numRec
            if not maxRecno is None:
                if totalRecsLoaded > maxRecno:
                    break
                
            if isComplete:
                shouldBreak = True

                
        if madDevObj.getNumRecsAccepted() == 0:
            if format == 'text':
                # add message to text file
                if output is not None:
                    f = open(output, 'a')
                else:
                    f = sys.stdout
                f.write('\nNo records were selected with the filters above\n')
                if output is not None:
                    f.close()
            elif not ignoreEmptyFile:
                raise IOError('No records selected with input time filters')
            
            else:
                # create empty file
                f = open(output, 'w')
                f.close()
            if is_tmp:
                os.remove(orgFilename)
            return
            
        # if ascii, done
        if format == 'text':
            if is_tmp:
                os.remove(orgFilename)
            return
        
        else:
            newFile.close() # create Hdf5 array layout if needed
            
        if format == 'netCDF4':
            # convert from Hdf5 to netCDF4
            madrigal.cedar.convertToNetCDF4(tmpOutput, output)
            os.remove(tmpOutput)
            
        if is_tmp:
            os.remove(orgFilename)
            
        
        
    def _getStartEndTimes(self, filterList):
        """_getStartEndTimes is a private method that returns a tuple of (startDatetime, endDatetime)
        based on any ut1 or ut1_unix filters found in filterList. Implemented to speed up loading large files
        for only a subset of data. Returns (None, None) if no ut1 or ut1_unix filters
        
        Inputs - filterList - a list of 0 or more madrigal.derivation.MadrigalFilter objects
        
        Returns: tuple of (startDatetime, endDatetime)
        based on any ut1 filters found in filterList. Implemented to speed up loading large files
        for only a subset of data. Returns (None, None) if no ut1 or ut1_unix filters
        """
        if len(filterList) == 0:
            return((None, None))
        
        startDT = None
        endDT = None
        for filt in filterList:
            if filt.mnemonic1 == 'ut1' and filt.mnemonic2 is None:
                if len(filt.rangeList) == 1:
                    if not filt.rangeList[0][0] is None:
                        if not math.isnan(filt.rangeList[0][0]):
                            if startDT is None:
                                startDT = filt.rangeList[0][0]
                            elif startDT < filt.rangeList[0][0]:
                                startDT = filt.rangeList[0][0]
                    if not filt.rangeList[0][1] is None:
                        if not math.isnan(filt.rangeList[0][1]):
                            if endDT is None:
                                endDT = filt.rangeList[0][1]
                            elif endDT > filt.rangeList[0][1]:
                                endDT = filt.rangeList[0][1]
                            
                    if not startDT is None:
                        dtList = madrigal._derive.getDateFromUt(startDT)[0:6]
                        startDT = datetime.datetime(*dtList)
                    if not endDT is None:
                        dtList = madrigal._derive.getDateFromUt(endDT)[0:6]
                        endDT = datetime.datetime(*dtList)
                    return((startDT, endDT))
                
            elif filt.mnemonic1 == 'ut1_unix' and filt.mnemonic2 is None:
                if len(filt.rangeList) == 1:
                    if not filt.rangeList[0][0] is None:
                        if not math.isnan(filt.rangeList[0][0]):
                            if startDT is None:
                                startDT = filt.rangeList[0][0]
                            elif startDT < filt.rangeList[0][0]:
                                startDT = filt.rangeList[0][0]
                    if not filt.rangeList[0][1] is None:
                        if not math.isnan(filt.rangeList[0][1]):
                            if endDT is None:
                                endDT = filt.rangeList[0][1]
                            elif endDT > filt.rangeList[0][1]:
                                endDT = filt.rangeList[0][1]
                            
                    if not startDT is None:
                        startDT = datetime.datetime.fromtimestamp(startDT, datetime.UTC)
                    if not endDT is None:
                        endDT = datetime.datetime.fromtimestamp(endDT, datetime.UTC)
                    return((startDT, endDT))
            
        return((None, None))
        
    
    
    def _getMaxRecno(self, filterList):
        """_getMaxRecno is a private method that returns a the max recno if recno used as a filter.
        Implemented to speed up loading large files
        for only a subset of data. Returns None if no recno filter
        
        Inputs - filterList - a list of 0 or more madrigal.derivation.MadrigalFilter objects
        
        Returns: maxRecno, or None if no recno filter
        """
        if len(filterList) == 0:
            return(None)
        
        maxRecno = None
        for filt in filterList:
            if filt.mnemonic1 == 'recno' and filt.mnemonic2 is None:
                if len(filt.rangeList) == 1:
                    if not filt.rangeList[0][1] is None:
                        if not math.isnan(filt.rangeList[0][1]):
                            maxRecno = int(filt.rangeList[0][1])
                            
        return(maxRecno)
    
    
    def _getTempUncompressed(self, orgFilename):
        """_getTempUncompressed returns the full path to a temp version of the orgFilename where the Table Layout
        has been compressed.  If h5repack fails, simply copies intact version to temp name
        """
        tempDir = tempfile.gettempdir()
        tempFile = os.path.join(tempDir, '%i_%s' % (random.randint(0,1000000), os.path.basename(orgFilename)))
        cmd = 'h5repack -f Data/Table\ Layout:GZIP=0 %s %s' % (orgFilename, tempFile)
        try:
            subprocess.check_call(shlex.split(cmd))
        except subprocess.CalledProcessError:
            shutil.copy(orgFilename, tempFile)
            
        return(tempFile)
        
            
            
class MadCalculatorGrid:
    """MadCalculatorGrid is the class that runs the Madrigal derivation engine when there is no pre-existing file
    
    Called grid because it calculates points at each unique combination of input latitudes, longitudes, and altitudes.
    That is, number of points = len(latList) x len(lonList) x len(altList)
    """
    
    def __init__(self, output, desiredParmList, dtList, latList, lonList, altList, 
                 oneDParmDict=None, twoDParmDict=None,
                 summary='summary', showHeaders=False, 
                 missing=None, assumed=None, knownbad=None):
        """__init__ runs the madCalculatorGrid engine and creates destFile without existing input file.
        
        Inputs:
        
            output - output filename - If None, writes text file to stdout.  If given and extension is one of
                 <.hdf5, .h5, .hdf>, then will save as Hdf5 file.  If extension is .nc, will be saved as netCDF4. In
                 all other cases, will be saved as ascii text.  If hdf5 or netCDF4, arguments (header, summary, badval,
                 assumed, and knownbad) ignored if given.
                 
            desiredParmList - a list of ordered desired parameters as lower case mnemonics
            
            dtList - a list of datetimes in UT, one for each record.  Length must be one or more.
            
        
            latList - a list of latitudes. May be zero length if a pure 1D calcuation. If one or more
                in length, each value must be unique.
            
            latList - a list of longitudes. May be zero length if a pure 1D calcuation. If one or more
                in length, each value must be unique.  Must be at least length 1 if latList length not 0.
                
            altList - a list of altitudes. May be zero length if a pure 1D calcuation. If one or more
                in length, each value must be unique.  Must be at least length 1 if latList length not 0.
                
            oneDParmDict - dict with keys = lower case one D parm mnemonics, values = parm values.  Length must be
                length of dtList.
                
            twoDParmDict - dict with keys = lower case two D parm mnemonics, values = 4D numpy array of values.  Shape must be
                (length of dtList, len of latList, len of lonList, len of altList).
                
            (The following arguments are ignored if not ascii format)
                
            summary - type of summary line to print at top.  Allowed values are:
                'plain' - text only mnemonic names, but only if not showHeaders
                'html' - mnemonic names wrapped in standard javascript code to allow descriptive popups
                'summary' - print overview of file and filters used. Also text only mnemonic names, 
                    but only if not showHeaders (default)
                None - no summary line
                
            showHeaders - if True, print header in format for each record.  If False, the default,
                do not.
                
            missing, assumed, knownbad - how to print Cedar special values.  Default is None for
                all, so that value printed in value in numpy table as per spec.
        
        """
        # tmpFile used as input file
        tmpFile = madrigal.derivation.createBaseMadrigalFileGrid(dtList, latList, lonList, altList,
                                                                 oneDParmDict, twoDParmDict)
        
        Isprint(tmpFile, output, desiredParmList, [],
                None, summary, showHeaders, 
                missing, assumed, knownbad)
        
        try:
            os.remove(tmpFile)
        except:
            pass
        
        
class MadCalculatorList:
    """MadCalculatorList is a second class that runs the Madrigal derivation engine when there is no pre-existing file
    
    Called list because it calculates points at zip(latList, lonList, altList)
    That is, number of points = len(latList)
    """
    
    def __init__(self, output, desiredParmList, dtList, latList, lonList, altList, 
                 oneDParmDict=None, twoDParmDict=None,
                 summary='summary', showHeaders=False, 
                 missing=None, assumed=None, knownbad=None):
        """__init__ runs the madCalculatorList engine and creates destFile without existing input file.
        
        Inputs:
        
            output - output filename - If None, writes text file to stdout.  If given and extension is one of
                 <.hdf5, .h5, .hdf>, then will save as Hdf5 file.  If extension is .nc, will be saved as netCDF4. In
                 all other cases, will be saved as ascii text.  If hdf5 or netCDF4, arguments (header, summary, badval,
                 assumed, and knownbad) ignored if given.
                 
            desiredParmList - a list of ordered desired parameters as lower case mnemonics
            
            dtList - a list of datetimes in UT, one for each record.  Length must be one or more.
            
        
            latList - a list of latitudes. May be zero length if a pure 1D calcuation. 
            
            latList - a list of longitudes. May be zero length if a pure 1D calcuation. Len must = len(latList)
                
            altList - a list of altitudes. May be zero length if a pure 1D calcuation. Len must = len(latList)
                
            oneDParmDict - dict with keys = lower case one D parm mnemonics, values = parm values.  Length must be
                length of dtList.
                
            twoDParmDict - dict with keys = lower case two D parm mnemonics, values numpy float array with shape 
                (len(dtList), len(latList))
                
            (The following arguments are ignored if not ascii format)
                
            summary - type of summary line to print at top.  Allowed values are:
                'plain' - text only mnemonic names, but only if not showHeaders
                'html' - mnemonic names wrapped in standard javascript code to allow descriptive popups
                'summary' - print overview of file and filters used. Also text only mnemonic names, 
                    but only if not showHeaders (default)
                None - no summary line
                
            showHeaders - if True, print header in format for each record.  If False, the default,
                do not.
                
            missing, assumed, knownbad - how to print Cedar special values.  Default is None for
                all, so that value printed in value in numpy table as per spec.
        
        """
        # tmpFile used as input file
        tmpFile = madrigal.derivation.createBaseMadrigalFileList(dtList, latList, lonList, altList,
                                                                 oneDParmDict, twoDParmDict)
        
        Isprint(tmpFile, output, desiredParmList, [],
                None, summary, showHeaders, 
                missing, assumed, knownbad)
        
        try:
            os.remove(tmpFile)
        except:
            pass