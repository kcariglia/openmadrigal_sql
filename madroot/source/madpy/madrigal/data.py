"""data is the module that interfaces to madrigal data files, or to Cedar standards about data.

This module includes the api to get information from a single madrigal file, and the api to
get information about the Cedar standard (such as parameter and category definitions).  

$Id: data.py 7457 2022-10-06 19:08:35Z brideout $
"""
# standard python imports
import os, sys
import string
import copy
import time
import datetime
import configparser
import subprocess
import itertools
import math
import warnings
import tempfile
import re

# third party imports
import numpy
import h5py

# Madrigal imports
import madrigal._Madrec
import madrigal.metadata
import madrigal.admin
import madrigal.ui.web
import madrigal.cedar
import madrigal.derivation


class _Parameter:
    """_Parameter is a private class to hold internal information for Madrigal Parameter
    """
    
    def __init__(self, code, description, units, mnemonic, format, width, categoryId,
                 hasDescription, hasErrDescription, position, main_code=None):
        """Inputs:
        
            code - parameter code (integer) - may not be negative
            description - mnemonic description (string)
            units - parameter units (string)
            mnemonic - mnemonic (string, no spaces)
            format - format string (used for data display)
            width - width to give value (used for data display)
            categoryId - id of parameter category (see madCatTab.txt)
            hasDescription - true if parameter has a description
            hasErrDescription - true if error parameter has a description
            position - position in input file
            main_code - if duplicate parameter, which code is the main code. If not, = None
        """
        self.code = int(code)
        self.description = description
        self.units = units
        self.mnemonic = mnemonic
        self.format = format
        self.width = int(width)
        self.categoryId = int(categoryId)
        self.hasDescription = bool(hasDescription)
        self.hasErrDescription = bool(hasErrDescription)
        self.position = int(position)
        if main_code != None:
            self.main_code = int(main_code)
        else:
            self.main_code = None
        
        

class MadrigalFile:
    """MadrigalFile is an object that provides access to information in a single Madrigal File.

    This object provides access to a single Madrigal file.  

    Usage example:

        import os, madrigal.data
    
        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

        test = madrigal.data.MadrigalFile(filepath)

        print test.toString()

        print test.getMaxValidAltitude()

    Non-standard Python modules used:
    
    None

    MadrigalError exception thrown if:
    
        1.  No data records found in file

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 26, 2001

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu June 4, 2002 to use summary information in header records
    if available.

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu Jan 24, 2003 to use the high level maddata module.

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu Jan 24, 2003 to use overview/[filename].summary file
    if available.

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu Feb 15, 2005 to add more summary data.
    
    Modified by "Miguel Urco":mailto:miguel.urco@jro.igp.gob.pe May 04, 2011 to handle hdf5 files.
    """

    # _getSummary return argument positions (from self._getSummary)
    _kinstListPosition        = 0
    _kindatListPosition       = 1
    _parameterListPosition    = 2
    _missingListPosition      = 3
    _maxPulseLenPosition      = 4
    _minPulseLenPosition      = 5
    _maxValidAltitudePosition = 6
    _minValidAltitudePosition = 7
    _maxLatitudePosition      = 8
    _minLatitudePosition      = 9
    _maxLongitudePosition     = 10
    _minLongitudePosition     = 11
    _earliestTimePosition     = 12
    _latestTimePosition       = 13
    _param1dListPosition      = 14
    _param2dListPosition      = 15
    _paramIndListPosition     = 16
    
    
    # cedar special values
    missingVal  = numpy.nan
    assumedVal  = -1.0
    knownbadVal = -2.0
    

    def __init__(self, initFile, madDB=None, saveSummary=True, forceRefresh=False, acceptOldSummary=False):
        """__init__ initializes MadrigalFile by finding all summary data.

        Inputs: self, String representing the full path to the madrigal file.

            Existing MadrigalDB object, by default = None.
            
            saveSummary - if True (default), persist summary information to /overview.
                If false, do not
                
            forceRefresh - if True, recreate summary file even if it exists.  If False
                (the default) do not recreate if it exists.
                
            acceptOldSummary - if True, do not recreate existing summary file even if older than main file,
                and touch the summary file to set its timestamp.
                If False (the default), recreate summary file if older than main file.
        
        Returns: void

        Affects: Initializes self._summary, which is a list of summary data about the file.  The
        information is first searched for in the file
        overview/[filename].summary.  If that fails, the file is analyzed using
        self._getSummary, which will write its results to overview/[filename].summary.  All
        public functions simply return this summarized data.

        Exceptions: MadrigalError thrown if no data record in file.
        """

        # get metadata dir
        if madDB == None:
            self._madDB = madrigal.metadata.MadrigalDB()
        else:
            self._madDB = madDB

        self._filename = initFile
        
        #create needed MadrigalParameters object:
        self._madParmObj = MadrigalParameters(self._madDB)

        # read metadata about instrument
        self._instMetadata = madrigal.metadata.MadrigalInstrument(self._madDB)

        # read metadata about files
        expDir = os.path.dirname(initFile)
        # we no longer assume fileTab.txt exists, so we need to check whether this
        # expDir actually matches the naming convention in order to properly initialize
        # the MadrigalMetaFile object
        validExpDir = False

        # first check that path to initFile contains expDir
        dirConvStr1 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[a-zA-Z0-9\-_]*$'
        dirConvStr2 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[0-3][0-9][a-z][a-z0-9][a-z0-9][0-9][0-9].?$'

        found = re.search(dirConvStr1, expDir)
        if not found:
            found = re.search(dirConvStr2, expDir)
            if not found:
                pass
        
        if found:
            validExpDir = True
            expDir = found.group(0) # should be exactly 1 expDir

        if validExpDir:
            self._fileMetadata = madrigal.metadata.MadrigalMetaFile(self._madDB, os.path.join(expDir, 'fileTab.txt'))
            self._fileCategory = self._fileMetadata.getCategoryByFilename(self._filename)
        else:
            self._fileMetadata = madrigal.metadata.MadrigalMetaFile(self._madDB)
        
        # read metadata about kindats
        self._madKindatObj = madrigal.metadata.MadrigalKindat(self._madDB)
            
        # first try to get file summary data from overview/[filename].summary
        if not forceRefresh:
            self._summary = self._getExistingSummary(acceptOldSummary)
        else:
            self._summary = None
        
        self._stdParms = ['year', 'month', 'day', 'hour', 'min', 'sec',
                 'recno', 'kindat', 'kinst', 'ut1_unix', 'ut2_unix']

        # if tempSummary = None, revert to complete file read via self._getSummary
        if self._summary is None:
            # for now, we need to check if this is a deprecated CEDAR 2.X file, or Hdf5.  Will be removed later
            fileName, fileExtension = os.path.splitext(self._filename)
            if fileExtension not in ('.h5', '.hdf5', '.hdf'):
                self._summary = self._getDeprecatedSummary()
            else:
                self._summary = self._getSummary()
                
            # write summary file to avoid this step in the future
            if saveSummary:
                self._writeSummary()
            
	# end init
    
        
    def getStandardParms(self, upper=False):
        """getStandardParms returns a list of standard parameters in every Madrigal Hdf5 file (formerly in prolog)
        
        Input:
         upper - if False (the default), return mnemonics in lower case. Otherwise, upper case
        
        """
        if not upper:
            return(self._stdParms)
        else:
            return[parm.upper() for parm in self._stdParms]
        

    def getKinstList(self):
        """getKinstList returns a list of integers of all kinst values in file.

        Inputs: self
        
        Returns: a list of integers of all kinst values in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._kinstListPosition]
    

    def getKinstListStr(self):
        """getKinstListStr returns a comma-separated string with the names of kinst values in file.

        Inputs: self
        
        Returns: a comma-separated string with the names of kinst values in file.

        Affects: Nothing

        Exceptions: None
        """

        kinstList = self._summary[self._kinstListPosition]

        kinstStr = ''
        
        # first kinst has no preceeding comma
        isFirstKinst = 1
        
        for inst in kinstList:
            if not isFirstKinst:
                kinstStr = kinstStr + ', '
            kinstStr = kinstStr + self._instMetadata.getInstrumentName(inst)
            isFirstKinst = 0
            
        return kinstStr


    def getKindatList(self):
        """getKindatList returns a list of integers of all kindat values in file.

        Inputs: self
        
        Returns: a list of integers of all kindat values in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._kindatListPosition]


    def getMeasuredParmList(self):
        """getMeasuredParmList returns a list of integers of all parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._parameterListPosition]


    def getMeasured1dParmList(self):
        """getMeasured1dParmList returns a list of integers of all 1d parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all 1d parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._param1dListPosition]
    

    def getMeasured2dParmList(self):
        """getMeasured2dParmList returns a list of integers of all 2d parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all 2d parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._param2dListPosition]
    
    
    def getMeasuredIndParmList(self):
        """getMeasuredIndParmList returns a list of integers of all independent 2d parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all independent 2d parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._paramIndListPosition]


    def getMissingParmList(self):
        """getMissingParmList returns a list of integers, one for each parameters stored in file.

        Inputs: self
        
        Returns: a list of integers, one for each parameters stored in file. If 1, that parameter
        was found to missing from at least one record in the file.  If -1, not missing in any
        data record.
        
        No longer relevant to Madrigal 3, since parameters cannot be missing.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._missingListPosition]
    

    def getMeasDervBothParmLists(self, parmList, measParmList, derivedParmList, allParmList, sureParmList, parmObj=None):
        """getMeasDervBothParmLists sets up four lists: measured parms, derived parms, both, and sure parms given a parm list to verify.

        Inputs: parmList: A list of parameters (integers or mnemonics to be considered)

            measParmList: an empty python list.  Will be filled with an list of all measured parameters (mnemonics)
            found in file when function returns. Standard parms are included in measParmList

            derivedParmList: an empty python list.  Will be filled with an list of all parameters (mnemonics) in parmList
            that can be derived from file when function returns.

            allParmList: an empty python list.  Will be filled with an list of all parameters in
            measParmList or derivedParmList when function returns.

            sureParmList: an empty python list.  Will be filled with an list of all parameters from the measured list
            that are never missing, and parameters that can be derived from those.  These parameters can then be derived
            for every record (excluding the fact that the value of the parameter in the record may be "missing").
            
            parmObj: use a different MadrigalParameters object (only for conversion mad2 -> mad3)
        
        Returns: void (see Affects below)

        Affects: adds items to measParmList, derivedParmList, and allParmList.  All items will be mnemonics.

        Exceptions: None

        Usage example:

            import os, madrigal.data

            filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

            test = madrigal.data.MadrigalFile(filepath)

            measParmList = []
            
            derivedParmList = []
            
            allParmList = []

            sureParmList = []


            test.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Short'),
                                          measParmList,
                                          derivedParmList,
                                          allParmList,
                                          sureParmList)
                                          
            #print lists
            
            print 'Measured parms are: ' + str(measParmList)
            
            print 'Derived parms are: ' + str(derivedParmList)
            
            print 'All good parms are: ' + str(allParmList)

            print 'Parameters sure to exist are: ' + str(sureParmList)
        """
        
        if parmObj is None:
            parmObj = self._madParmObj

        # be sure input is in the form of mnemonics
        mnemParmList = parmObj.getParmMnemonicList(parmList)

        # get list of parameters in file in mnemonic form
        fileParmList = parmObj.getParmMnemonicList(self._summary[self._parameterListPosition])
        fileParmList = self.getStandardParms(True) + fileParmList

        # divide all parameters into either measured (and all) list, or to be verified list
        # every parameter in file is put in measList
        toBeVerifiedList = []


        for parm in mnemParmList:
            if parm not in fileParmList:
                toBeVerifiedList.append(parm)
        
        # add measured parameters to measParmList and tempAllList
        index = 0
        for parm in fileParmList:
            if parm not in measParmList:
                measParmList.append(parm)
            if parm not in allParmList:
                allParmList.append(parm)
            if parm not in sureParmList:
                sureParmList.append(parm)
            index += 1

        # get list of derivable parameters
        derivableParmsList = madrigal.derivation.getDerivableParms(fileParmList)

            
        for item in toBeVerifiedList:
            if item in derivableParmsList:
                if item not in derivedParmList:
                    derivedParmList.append(item)
                if item not in allParmList:
                    allParmList.append(item)
                if item not in sureParmList:
                    sureParmList.append(item)


        

    def getMaxPulseLength(self):
        """getMaxPulseLength returns a double representing maximum pulse length in microseconds in file.

        Inputs: self
        
        Returns: a double representing maximum pulse length in microseconds in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._maxPulseLenPosition]

    def getMinPulseLength(self):
        """getMinPulseLength returns a double representing minimum pulse length in microseconds in file.

        Inputs: self
        
        Returns: a double representing minimum pulse length in microseconds in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._minPulseLenPosition]


    def getMaxValidAltitude(self):
        """getMaxValidAltitude returns a double representing maximum valid altitude in km in file.

        Inputs: self
        
        Returns: a double representing maximum valid altitude in km in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._maxValidAltitudePosition]


    def getMinValidAltitude(self):
        """getMinValidAltitude returns a double representing minimum valid altitude in km in file.

        Inputs: self
        
        Returns: a double representing minimum valid altitude in km in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._minValidAltitudePosition]


    def getMaxLatitude(self):
        """getMaxLatitude returns a double representing maximum latitude in degrees in file.

        Inputs: self
        
        Returns: a double representing maximum latitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._maxLatitudePosition]
    

    def getMinLatitude(self):
        """getMinLatitude returns a double representing minimum latitude in degrees in file.

        Inputs: self
        
        Returns: a double representing minimum latitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._minLatitudePosition]


    def getMaxLongitude(self):
        """getMaxLongitude returns a double representing maximum longitude in degrees in file.

        Inputs: self
        
        Returns: a double representing maximum longitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._maxLongitudePosition]
    

    def getMinLongitude(self):
        """getMinLongitude returns a double representing minimum longitude in degrees in file.

        Inputs: self
        
        Returns: a double representing minimum longitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._minLongitudePosition]


    def getEarliestTime(self):
        """getEarliestTime returns a list of 6 numbers representing the earliest time in the file.

        Inputs: self
        
        Returns: a list of 6 numbers representing the earliest time in the file.  The format is
                [Year, Month, Day, Hour, Minute, Second]

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._earliestTimePosition]


    def getLatestTime(self):
        """getLatestTime returns a list of 6 numbers representing the latest time in the file.

        Inputs: self
        
        Returns: a list of 6 numbers representing the latest time in the file.  The format is
                [Year, Month, Day, Hour, Minute, Second]

        Affects: Nothing

        Exceptions: None
        """

        return self._summary[self._latestTimePosition]


    def getCatalogHeaderStr(self):
        """getCatalogHeaderStr returns a string formatted for printing containing all catalog and header records.

        Input: None

        Returns: a string formatted for printing containing all catalog and header records. Returns '' if no
        catalog or header records.
        """
        retStr = ''
        
        with h5py.File(self._filename, 'r') as f:
            
            try:
                cat = f['Metadata']['Experiment Notes']
                if len(cat) > 0:
                    retStr += 'Experiment Notes:\n'
                for line in cat:
                    if type(line[0]) in (bytes, numpy.bytes_):
                        retStr += '%s\n' % (line[0].decode('utf-8'))
                    else:
                        retStr += '%s\n' % (line[0])
            except:
                pass
            
            try:
                expParms = f['Metadata']['Experiment Parameters']
                if len(expParms) > 0:
                    retStr += '\nExperiment Parameters:\n'
                for line in expParms:
                    if type(line[0]) in (bytes, numpy.bytes_):
                        retStr += '%s: %s\n' % (line[0].decode('utf-8'), line[1].decode('utf-8'))
                    else:
                        retStr += '%s: %s\n' % (line[0], line[1])
            except:
                pass
                
            try:
                dataParms = f['Metadata']['Data Parameters']
                if len(dataParms) > 0:
                    retStr += '\nData Parameters:\n'
                for line in dataParms:
                    if type(line[0]) in (bytes, numpy.bytes_):
                        retStr += '%s: %s, units: %s\n' % (line[0].decode('utf-8'), line[1].decode('utf-8'), 
                                                           line[3].decode('utf-8'))
                    else:
                        retStr += '%s: %s, units: %s\n' % (line[0], line[1], 
                                                           line[3])
            except:
                pass
            
            try:
                parms = f['Metadata']['Independent Spatial Parameters']
                if len(parms) > 0:
                    retStr += '\nIndependent Spatial Parameters:\n'
                for line in parms:
                    if type(line[0]) in (bytes, numpy.bytes_):
                        retStr += 'mnemonic: %s: description: %s\n' % (line[0].decode('utf-8'), line[1].decode('utf-8'))
                    else:
                        retStr += 'mnemonic: %s: description: %s\n' % (line[0], line[1])
            except:
                pass
            
            
            try:
                parms = f['Metadata']['Parameters Used to Split Array Data']
                if len(parms) > 0:
                    retStr += '\nParameters Used to Split Array Data:\n'
                for line in parms:
                    if type(line[0]) in (bytes, numpy.bytes_):
                        retStr += 'mnemonic: %s: description: %s\n' % (line[0].decode('utf-8'), line[1].decode('utf-8'))
                    else:
                        retStr += 'mnemonic: %s: description: %s\n' % (line[0], line[1])
            except:
                pass
            
                
            
                
        return(retStr)
        
    
        

    def toString(self):
        """toString returns a simple string representation of a MadrigalFile object.

        Inputs: None
        
        Returns: String describing a simple representation of a MadrigalFile object.

        Affects: Nothing

        Exceptions: None
        """

        output =  "Object type: MadrigalFile\n"
        output += "Filename = "                     + self._filename + "\n"
        output += "Kinst list = "                   + str(self.getKinstList()) + "\n"
        output += "Kindat list = "                  + str(self.getKindatList()) + "\n"
        output += "Measured parm list = "           + str(self.getMeasuredParmList()) + "\n"
        output += "Missing parm list = "            + str(self.getMissingParmList()) + "\n"
        output += "Maximum pulse length (sec) = "   + str(self.getMaxPulseLength()) + "\n"
        output += "Minimum pulse length (sec) = "   + str(self.getMinPulseLength()) + "\n"
        output += "Maximum valid altitude (km) = "  + str(self.getMaxValidAltitude()) + "\n"
        output += "Minimum valid altitude (km) = "  + str(self.getMinValidAltitude()) + "\n"
        output += "Maximum latitude (degrees) = "   + str(self.getMaxLatitude()) + "\n"
        output += "Minimum latitude (degrees) = "   + str(self.getMinLatitude()) + "\n"
        output += "Maximum longitude (degrees) = "  + str(self.getMaxLongitude()) + "\n"
        output += "Minimum longitude (degrees) = "  + str(self.getMinLongitude()) + "\n"
        output += "Earliest time list = "           + str(self.getEarliestTime()) + "\n"
        output += "Latest time list = "             + str(self.getLatestTime()) + "\n"
        output += "Kinst string = "                 + self.getKinstListStr() + "\n"
        output += "Measured 1d parm list = "        + str(self.getMeasured1dParmList()) + "\n"
        output += "Measured 2d parm list = "        + str(self.getMeasured2dParmList()) + "\n"
        output += "Measured ind 2d parm list = "    + str(self.getMeasuredIndParmList()) + "\n"

        return output
    
    
    def __str__(self):
        return(self.toString())
    
    
    def _getSummary(self):
        """_getSummary creates a new summary from examoning an Hdf5 file, and returns a list of the following:

              1. List of integers of all KINST values found
              2. List of integers of all KINDAT values found
              3. List of integers of all parameters in file
              4. List of integers, one for each parameter above,
                 =1 if that parameter was found to be missing from some records,
                 =-1 if never missing
              5. double of maximum pulse length (microsec) found (NaN if none)
              6. double of minimum pulse length (microsec) found (NaN if none)
              7. double of maximum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              8. double of minimum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              9. double of maximum latitude (deg) found (NaN if none)
             10. double of minimum latitude (deg) found (NaN if none)
             11. double of maximum longitude (deg) found (NaN if none)
             12. double of minimum longitude (deg) found (NaN if none)
             13. List of 6 integers representing earliest
                 date found in file [year, month, day, hour, min, sec]
             14. List of 6 integers representing latest
                 date found in file [year, month, day, hour, min, sec]
             15. List of integers of all 1-D parameters in file
             16. List of integers of all 2-D parameters in file
             17. List of integers of all independent 2-D parameters in file
        """
        searchParms = ['pl', 'gdalt', 'gdlat', 'glon']
        retList = []
        # to reduced possible large memory footprint, load 100 records at time
        isFirst = True
        minMaxDict = {}
        madCedarObj = madrigal.cedar.MadrigalCedarFile(self._filename, maxRecords=100)
        num = 100
        firstKinst = madCedarObj.getKinstList()[0]
        isComplete = False
        while(True):
            if not isFirst:
                num, isComplete = madCedarObj.loadNextRecords(100)
            if isFirst:
                parms = madCedarObj.getRecDType().names
                derivableParms = [parm.lower() for parm in madrigal.derivation.getDerivableParms(parms, kinst=firstKinst)]
                parmsToDerive = []
                oneDParms = madCedarObj.get1DParms()
                twoDParms = madCedarObj.get2DParms()
                indParms = madCedarObj.getIndSpatialParms()
                for parm in searchParms:
                    if parm in list(parms) + derivableParms:
                        if parm not in parmsToDerive:
                            parmsToDerive.append(parm)
                isFirst = False
            if num == 0:
                break
            
            # use derivation model to find max and min values
            madDerObj = madrigal.derivation.MadrigalDerivation(madCedarObj, parmsToDerive)
            madDerFile = madDerObj.getNewCedarFile()
            
            for parm in searchParms:
                if parm in parmsToDerive:
                    minValue, maxValue = madDerFile.getMaxMinValues(parm, verifyValid=True)
                else: 
                    minValue, maxValue = numpy.nan, numpy.nan
                if parm not in minMaxDict:
                    minMaxDict[parm] = [minValue, maxValue]
                else:
                    orgMin, orgMax = minMaxDict[parm]
                    minMaxDict[parm] = [min(minValue, orgMin), max(maxValue, orgMax)]
            
            if isComplete:
                break
            
        retList.append([int(kinst) for kinst in madCedarObj.getKinstList()])
        retList.append([int(kindat) for kindat in madCedarObj.getKindatList()])
        retList.append([int(self._madParmObj.getParmCodeFromMnemonic(parm)) for parm in parms])
        retList.append([-1 for parm in parms])
        
        for parm in searchParms:
            minValue, maxValue = minMaxDict[parm]
            retList.append(maxValue)
            retList.append(minValue)
            
        eDT = madCedarObj.getEarliestDT()
        retList.append([eDT.year, eDT.month, eDT.day, eDT.hour, eDT.minute, eDT.second])
        
        sDT = madCedarObj.getLatestDT()
        retList.append([sDT.year, sDT.month, sDT.day, sDT.hour, sDT.minute, sDT.second])
        
        retList.append([int(self._madParmObj.getParmCodeFromMnemonic(parm)) for parm in oneDParms])
        retList.append([int(self._madParmObj.getParmCodeFromMnemonic(parm)) for parm in twoDParms])
        retList.append([int(self._madParmObj.getParmCodeFromMnemonic(parm)) for parm in indParms])
        
        return(retList)


    def _getExistingSummary(self, acceptOldSummary):
        """_getExistingSummary returns a list of strings summarizing a file via header/cat records or overview file if possible.

        If all the required information is not found in the first two records of the file, the file
        overview/[filename].summary is used or [filename].summary in the same directory as the filename.  If that fails, returns None.

        Inputs: acceptOldSummary - if True, do not recreate existing summary file even if older than main file, and touch if older.
                If False, recreate summary file if older than main file.
        
        Returns: A list of values summarizing the MadrigalFile.  These values are:
              1. List of integers of all KINST values found
              2. List of integers of all KINDAT values found
              3. List of integers of all parameters in file
              4. List of integers, one for each parameter above,
                 =1 if that parameter was found to be missing from some records,
                 =-1 if never missing
              5. double of maximum pulse length (microsec) found (NaN if none)
              6. double of minimum pulse length (microsec) found (NaN if none)
              7. double of maximum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              8. double of minimum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              9. double of maximum latitude (deg) found (NaN if none)
             10. double of minimum latitude (deg) found (NaN if none)
             11. double of maximum longitude (deg) found (NaN if none)
             12. double of minimum longitude (deg) found (NaN if none)
             13. List of 6 integers representing earliest
                 date found in file [year, month, day, hour, min, sec]
             14. List of 6 integers representing latest
                 date found in file [year, month, day, hour, min, sec]
             15. List of integers of all 1-D parameters in file
             16. List of integers of all 2-D parameters in file
             17. List of integers of all independent 2-D parameters in file
        

            If all required information not found, returns None.  The following
            items are not required, and default to missingVal or empty list: minPulseLen, minValidAltitude,
            maxLatitude, minLatitude, maxLongitude, minLongitude, param1dList, param2dList, paramIndList

        Affects: Nothing

        Exceptions: None
        """

        summaryFilename = os.path.join(os.path.dirname(self._filename), 'overview')
        summaryFilename = os.path.join(summaryFilename, os.path.basename(self._filename) + '.summary')
        if not os.path.exists(summaryFilename):
            # modify to look in same directory
            summaryFilename = os.path.join(self._filename + '.summary')
        if os.path.exists(summaryFilename):
            # check that summaryFile is newer than data file, otherwise it might be out of date
            if os.stat(summaryFilename).st_mtime + 10 < os.stat(self._filename).st_mtime:
                if not acceptOldSummary:
                    return(None)
                else:
                    # touch summary file
                    os.utime(summaryFilename, None)
                
            summaryFile = open(summaryFilename)
            summaryStr = summaryFile.read()
            summaryFile.close()
            try:
                tempSummary = self._parseSummary(summaryStr)
            except:
                tempSummary = None
            return tempSummary
        else:
            return None



    def _parseSummary(self, summaryStr):
        """_parseSummary returns a list of values summarizing a file based on overview file if possible.

        If all the required information is not found in the string, returns None. 

        Inputs: summaryStr read from a summary file.
        
        Returns: A list of values summarizing the MadrigalFile.  These values are:
              1. List of integers of all KINST values found
              2. List of integers of all KINDAT values found
              3. List of integers of all parameters in file
              4. List of integers, one for each parameter above,
                 =1 if that parameter was found to be missing from some records,
                 =-1 if never missing
              5. double of maximum pulse length (microsec) found (NaN if none)
              6. double of minimum pulse length (microsec) found (NaN if none)
              7. double of maximum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              8. double of minimum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              9. double of maximum latitude (deg) found (NaN if none)
             10. double of minimum latitude (deg) found (NaN if none)
             11. double of maximum longitude (deg) found (NaN if none)
             12. double of minimum longitude (deg) found (NaN if none)
             13. List of 6 integers representing earliest
                 date found in file [year, month, day, hour, min, sec]
             14. List of 6 integers representing latest
                 date found in file [year, month, day, hour, min, sec]
             15. List of integers of all 1-D parameters in file
             16. List of integers of all 2-D parameters in file
             17. List of integers of all independent parameters in file
        

            If all required information not found, returns None.  The following
            items are not required, and default to missingVal or empty list: minPulseLen, minValidAltitude,
            maxLatitude, minLatitude, maxLongitude, minLongitude, param1dList, param2dList, paramIndList

        Affects: Nothing

        Exceptions: None
        """

        kinstList            = None
        kindatList           = None
        parameterList        = None
        missingList          = None
        maxPulseLen          = None
        minPulseLen          = None
        maxValidAltitude     = None
        minValidAltitude     = None
        maxLatitude          = None
        minLatitude          = None
        maxLongitude         = None
        minLongitude         = None
        earliestYear         = None
        earliestMonth        = None
        earliestDay          = None
        earliestHour         = None
        earliestMin          = None
        earliestSec          = None
        earliestTime         = None
        latestYear           = None
        latestMonth          = None
        latestDay            = None
        latestHour           = None
        latestMin            = None
        latestSec            = None
        latestTime           = None
        param1dList          = []
        param2dList          = []
        paramIndList         = []

        
        # split string into lists of strings by separating lines
        summaryStrLineList = summaryStr.split('\n')

        # finally break each line into a list of words
        summaryList = []

        for line in summaryStrLineList:
            summaryList.append(line.split())

        # get info
        for line in summaryList:
            
            # ignore empty lines
            if len(line) == 0:
                continue

            #kinst
            if line[0].lower() == 'kinste':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if kinstList is None:
                    kinstList = [int(line[1])]
                else:
                    kinstList.append(int(line[1]))

            # max pulse length
            if line[0].lower() == 'pl2':
                try:
                    maxPulseLen =float(line[1])
                except:
                    return None

            # min pulse length
            if line[0].lower() == 'pl1':
                try:
                    minPulseLen = float(line[1])
                except:
                    return None

            # max valid altitude
            if line[0].lower() == 'alt2':
                try:
                    maxValidAltitude = float(line[1])
                except:
                    return None

            # min valid altitude
            if line[0].lower() == 'alt1':
                try:
                    minValidAltitude = float(line[1])
                except:
                    return None

            # max latitude
            if line[0].lower() == 'gglat2':
                try:
                    maxLatitude = float(line[1])
                except:
                    return None

            # min latitude
            if line[0].lower() == 'gglat1':
                try:
                    minLatitude = float(line[1])
                except:
                    return None

            # max longitude
            if line[0].lower() == 'gglon2':
                try:
                    maxLongitude = float(line[1])
                except:
                    return None

            # min longitude
            if line[0].lower() == 'gglon1':
                try:
                    minLongitude = float(line[1])
                except:
                    return None

            # date
            if line[0].lower() == 'ibyre':
                try:
                    earliestYear = int(line[1])
                except:
                    return None

            if line[0].lower() == 'ieyre':
                try:
                    latestYear = int(line[1])
                except:
                    return None

            if line[0].lower() == 'ibdte':
                try:
                    earliestMonth = int(line[1][:-2])
                    earliestDay = int(line[1][-2:])
                except:
                    return None

            if line[0].lower() == 'iedte':
                try:
                    latestMonth = int(line[1][:-2])
                    latestDay = int(line[1][-2:])
                except:
                    return None

            if line[0].lower() == 'ibhme':
                try:
                    if len(line[1][:-2]) == 0:
                        earliestHour = 0
                    else:
                        earliestHour = int(line[1][:-2])
                    earliestMin = int(line[1][-2:])
                except:
                    return None

            if line[0].lower() == 'iehme':
                try:
                    if len(line[1][:-2]) == 0:
                        latestHour = 0
                    else:
                        latestHour = int(line[1][:-2])
                    latestMin = int(line[1][-2:])
                except:
                    return None

            if line[0].lower() == 'ibcse':
                try:
                    if len(line[1][:-2]) == 0:
                        earliestSec = 0
                    else:
                        earliestSec = int(line[1][:-2])
                except:
                    return None

            if line[0].lower() == 'iecse':
                try:
                    if len(line[1][:-2]) == 0:
                        latestSec = 0
                    else:
                        latestSec = int(line[1][:-2])
                except:
                    return None


            #kindat
            if line[0].lower() == 'kindat':
                try:
                    # test if its an integer - note - skip first 0
                    int(line[2])
                except:
                    return None
                if kindatList is None:
                    kindatList = [int(line[2])]
                else:
                    kindatList.append(int(line[2]))
                

            # parameters
            if line[0][0:3].lower() == 'kod':
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                if parameterList is None:
                    parameterList = [int(line[2])]
                else:
                    if int(line[2]) not in parameterList:
                        parameterList.append(int(line[2]))

            if line[0][0:4].lower() == 'kods':
                # 1d parameter
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                param1dList.append(int(line[2]))

            if line[0][0:4].lower() == 'kodm':
                # 2d parameter
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                param2dList.append(int(line[2]))
                    
            if line[0][0:4].lower() == 'kodi':
                # ind 2d parameter
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                paramIndList.append(int(line[2]))

            # missing parameters
            if line[0].lower() in ('cmissing', 'missing'):
                # line may be empty
                if missingList == None:
                    missingList = []

                tempList = []
                if len(line) > 1:
                    for item in line[1].split(','):
                        tempList.append(int(item))
                    
                # now populate missing List
                for parm in parameterList:
                    if parm in tempList:
                        missingList.append(1)
                    else:
                        missingList.append(-1)


        earliestTime = [earliestYear, earliestMonth, earliestDay,
                        earliestHour, earliestMin, earliestSec]

        latestTime = [latestYear, latestMonth, latestDay,
                       latestHour, latestMin, latestSec]

        # set any needed defaults
        if minPulseLen is None:
            minPulseLen = self.missingVal

        if minValidAltitude is None:
            minValidAltitude = self.missingVal

        if maxLatitude is None:
            maxLatitude = self.missingVal

        if minLatitude is None:
            minLatitude = self.missingVal

        if maxLongitude is None:
            maxLongitude = self.missingVal

        if minLongitude is None:
            minLongitude = self.missingVal
            
        if paramIndList is None:
            paramIndList = []
            
        # verify that all non-default values are set
        if None in (kinstList, kindatList, parameterList, missingList,
                    maxPulseLen, maxValidAltitude, earliestYear,
                    earliestMonth, earliestDay, earliestHour, earliestMin,
                    earliestSec, latestYear, latestMonth, latestDay, latestHour,
                    latestMin, latestSec):
            return(None)
        else:
            return([kinstList, kindatList, parameterList, missingList, maxPulseLen,
                    minPulseLen,  maxValidAltitude, minValidAltitude, maxLatitude,
                    minLatitude, maxLongitude, minLongitude, earliestTime, latestTime,
                    param1dList, param2dList, paramIndList])
            
            
    def _getDeprecatedSummary(self):
        """_getDeprecatedSummary creates a new summary from examoning a deprecated CEDAR 2.x file, and 
            returns a list of the following:

              1. List of integers of all KINST values found
              2. List of integers of all KINDAT values found
              3. List of integers of all parameters in file
              4. List of integers, one for each parameter above,
                 =1 if that parameter was found to be missing from some records,
                 =-1 if never missing
              5. double of maximum pulse length (microsec) found (NaN if none)
              6. double of minimum pulse length (microsec) found (NaN if none)
              7. double of maximum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              8. double of minimum valid altitude found (km above sea level)
                 (Valid means a real 2D value exists at that altitude)
                  (NaN if none)
              9. double of maximum latitude (deg) found (NaN if none)
             10. double of minimum latitude (deg) found (NaN if none)
             11. double of maximum longitude (deg) found (NaN if none)
             12. double of minimum longitude (deg) found (NaN if none)
             13. List of 6 integers representing earliest
                 date found in file [year, month, day, hour, min, sec]
             14. List of 6 integers representing latest
                 date found in file [year, month, day, hour, min, sec]
             15. List of integers of all 1-D parameters in file
             16. List of integers of all 2-D parameters in file
             17. List of integers of all independent 2-D parameters in file (empty list always,
                     since not defined in old CEDAR file format),
        """
        tempSummary = madrigal._Madrec.getSummary(self._filename)

        retList = []

        # append kinstList to retList
        retList.append(tempSummary[self._kinstListPosition])
        # append kindatList to retList
        retList.append(tempSummary[self._kindatListPosition])
        # append parmList to retList
        retList.append(tempSummary[self._parameterListPosition])
        # append missingList to retList
        retList.append(tempSummary[self._missingListPosition])

        retList.append(float(tempSummary[self._maxPulseLenPosition]))

        retList.append(float(tempSummary[self._minPulseLenPosition]))

        # append maximum valid altitude
        retList.append(float(tempSummary[self._maxValidAltitudePosition]))

        # append minimum valid altitude
        retList.append(float(tempSummary[self._minValidAltitudePosition]))

        # append maximum valid latitude
        retList.append(float(tempSummary[self._maxLatitudePosition]))

        # append minimum valid latitude
        retList.append(float(tempSummary[self._minLatitudePosition]))

        # append maximum valid longitude
        retList.append(float(tempSummary[self._maxLongitudePosition]))

        # append minimum valid longitude
        retList.append(float(tempSummary[self._minLongitudePosition]))

        # append earliestList to retList
        retList.append(tempSummary[self._earliestTimePosition])
        # append latestList to retList
        retList.append(tempSummary[self._latestTimePosition])
            
        # append parm1dList to retList
        retList.append(tempSummary[self._param1dListPosition])
        # append parm2dList to retList
        retList.append(tempSummary[self._param2dListPosition])
        
        # finally append empty list of ind parms since not in CEDAR file format
        retList.append([])
        
        return(retList)



    def _writeSummary(self):
        """_writeSummary writes a summary file to overview/[filename].summary.

        Uses data from self._summary. 

        Inputs: None.
        
        Returns: None

        Affects: writes a summary file to overview/[filename].summary

        Exceptions: If file cannot be written
        """
        
        summaryFilename = os.path.dirname(self._filename) + '/overview'

        # if overview directory does not yet exist, create it
        if os.path.exists(summaryFilename) == 0:
            try:
                os.mkdir(summaryFilename)
            except OSError:
                try:
                    adminObj =  madrigal.admin.MadrigalNotify()
                    adminObj.sendAlert('Unable to create summary file %s - please check permissions' % (summaryFilename), 'Madrigal permission error')
                except:
                    pass
                return
            try:
                os.chmod(summaryFilename, 0o777)
            except:
                pass
        
        summaryFilename += '/' + os.path.basename(self._filename) + '.summary'
        try:
            summaryFile = open(summaryFilename, 'w')
        except OSError:
            try:
                adminObj =  madrigal.admin.MadrigalNotify()
                adminObj.sendAlert('Unable to create summary file %s - please check permissions' % (summaryFilename), 'Madrigal permission error')
            except:
                pass
            return

        # write kinst
        for kinst in self._summary[self._kinstListPosition]:
            summaryFile.write('KINSTE  ' + str(kinst) + '\n')

        # write kindat
        for kindat in self._summary[self._kindatListPosition]:
            summaryFile.write('KINDAT  0  ' + str(kindat) + '\n')

        # write 1d parameters
        for parm in self._summary[self._param1dListPosition]:
            summaryFile.write('KODS  0  ' + str(parm) + '\n')

        # write 2d parameters
        for parm in self._summary[self._param2dListPosition]:
            summaryFile.write('KODM  0  ' + str(parm) + '\n')
            
        # write ind 2d parameters
        for parm in self._summary[self._paramIndListPosition]:
            summaryFile.write('KODI  0  ' + str(parm) + '\n')
            
        # write missing parameters - even if list empty
        summaryFile.write('CMISSING ')
        count = 0
        first = 1
        for parm in self._summary[self._missingListPosition]:
            if parm == 1:
                if first != 1:
                    summaryFile.write(',')
                first = 0
                summaryFile.write(str(self._summary[self._parameterListPosition][count]))
            count += 1
        summaryFile.write('\n')

        # write max pulse length in microseconds as an int
        # round up if needed
        if numpy.isnan(self._summary[self._maxPulseLenPosition]):
            pl2Str = 'nan'
        elif math.modf(self._summary[self._maxPulseLenPosition]*1000000)[0] > 0.0:
            pl2Str = str(1 + int(self._summary[self._maxPulseLenPosition]*1000000))
        else:
            pl2Str = str(int(self._summary[self._maxPulseLenPosition]*1000000))
        summaryFile.write('PL2     ' + pl2Str + '\n')

        # write min pulse length in microseconds as an int
        # round up if needed
        if numpy.isnan(self._summary[self._minPulseLenPosition]):
            pl1Str = 'nan'
        elif math.modf(self._summary[self._minPulseLenPosition]*1000000)[0] > 0.0:
            pl1Str = str(1 + int(self._summary[self._minPulseLenPosition]*1000000))
        else:
            pl1Str = str(int(self._summary[self._minPulseLenPosition]*1000000))
        summaryFile.write('PL1     ' + pl1Str + '\n')

        # write max valid alt as an int
        # round up if needed
        if numpy.isnan(self._summary[self._maxValidAltitudePosition]):
            alt2Str = 'nan'
        elif math.modf(self._summary[self._maxValidAltitudePosition])[0] > 0.0:
            alt2Str = str(1 + int(self._summary[self._maxValidAltitudePosition]))
        else:
            alt2Str = str(int(self._summary[self._maxValidAltitudePosition]))
        summaryFile.write('ALT2    ' + alt2Str + '\n')

        # write min valid alt as an int
        # round up if needed
        if numpy.isnan(self._summary[self._minValidAltitudePosition]):
            alt1Str = 'nan'
        elif math.modf(self._summary[self._minValidAltitudePosition])[0] > 0.0:
            alt1Str = str(1 + int(self._summary[self._minValidAltitudePosition]))
        else:
            alt1Str = str(int(self._summary[self._minValidAltitudePosition]))
        summaryFile.write('ALT1    ' + alt1Str + '\n')

        # write max latitude as an int
        # round up if needed
        if numpy.isnan(self._summary[self._maxLatitudePosition]):
            gglat2Str = 'nan'
        elif math.modf(self._summary[self._maxLatitudePosition])[0] > 0.0:
            gglat2Str = str(1 + int(self._summary[self._maxLatitudePosition]))
        else:
            gglat2Str = str(int(self._summary[self._maxLatitudePosition]))
        summaryFile.write('GGLAT2    ' + gglat2Str + '\n')

        # write min valid latitude as an int
        # round up if needed
        if numpy.isnan(self._summary[self._minLatitudePosition]):
            gglat1Str = 'nan'
        elif math.modf(self._summary[self._minLatitudePosition])[0] > 0.0:
            gglat1Str = str(1 + int(self._summary[self._minLatitudePosition]))
        else:
            gglat1Str = str(int(self._summary[self._minLatitudePosition]))
        summaryFile.write('GGLAT1    ' + gglat1Str + '\n')

        # write max longitude as an int
        # round up if needed
        if numpy.isnan(self._summary[self._maxLongitudePosition]):
            gglon2Str = 'nan'
        elif math.modf(self._summary[self._maxLongitudePosition])[0] > 0.0:
            gglon2Str = str(1 + int(self._summary[self._maxLongitudePosition]))
        else:
            gglon2Str = str(int(self._summary[self._maxLongitudePosition]))
        summaryFile.write('GGLON2    ' + gglon2Str + '\n')

        # write min valid longitude as an int
        # round up if needed
        if numpy.isnan(self._summary[self._minLongitudePosition]):
            gglon1Str = 'nan'
        elif math.modf(self._summary[self._minLongitudePosition])[0] > 0.0:
            gglon1Str = str(1 + int(self._summary[self._minLongitudePosition]))
        else:
            gglon1Str = str(int(self._summary[self._minLongitudePosition]))
        summaryFile.write('GGLON1    ' + gglon1Str + '\n')

        # write beginning time
        summaryFile.write('IBYRE   ' + str(self._summary[self._earliestTimePosition][0]) + '\n')
        md = 100*self._summary[self._earliestTimePosition][1] + self._summary[self._earliestTimePosition][2]
        summaryFile.write('IBDTE   ' + str(md) + '\n')
        hm = 100*self._summary[self._earliestTimePosition][3] + self._summary[self._earliestTimePosition][4]
        summaryFile.write('IBHME   ' + str(hm) + '\n')
        summaryFile.write('IBCSE   ' + str(100*self._summary[self._earliestTimePosition][5]) + '\n')

        # write ending time
        summaryFile.write('IEYRE   ' + str(self._summary[self._latestTimePosition][0]) + '\n')
        md = 100*self._summary[self._latestTimePosition][1] + self._summary[self._latestTimePosition][2]
        summaryFile.write('IEDTE   ' + str(md) + '\n')
        hm = 100*self._summary[self._latestTimePosition][3] + self._summary[self._latestTimePosition][4]
        summaryFile.write('IEHME   ' + str(hm) + '\n')
        summaryFile.write('IECSE   ' + str(100*self._summary[self._latestTimePosition][5]) + '\n')
        
        summaryFile.close()

        # set permissions wide open to avoid problems if possible
        try:
            os.chmod(summaryFilename, 0o666)
        except:
            pass


    def _setToOne(self, x):
        """ Private function used to initialize a list to ones"""
        return 1
    
    
    """The remaining ~1000 lines of this class exists solely to convert a Madrigal 2.X installation to Madrigal 3.0.  These
    methods should only be used during installation of Madrogal 3.0.  All will issue DeprecationWarnings, which 
    should be surpressed by the installation script.
    """
    
    
    def _execute_isprint(self,cmd):
        """_execute_isprint is a convenience function that executes a call to isprint.py and returns the filepath
        to a file containing the output from isprint.py. This function removes blank lines from isprint.py output.
        
        Inputs:
        
            cmd - a string of the isprint.py command to execute.
                
        Returns: the path to the file containing the output of the executed isprint.py command.
        
        Affects: Writes a file into /tmp

        Exceptions: Parameter required is not in the file nor can it be derived.
                    No records selected with the filters above
        """
        with tempfile.NamedTemporaryFile(delete=True,dir='/tmp') as tf:
            tempfile_name = tf.name

        start_rec = 0
        num_lines = 0
        with open(tempfile_name,'a') as f:
            while True:
                thisCmd = cmd + ' filter=recno,%i,%i ' % (start_rec, start_rec+999) # isprint filters are INCLUSIVE
                with subprocess.Popen(thisCmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as proc:
                    temp = proc.stdout.read()

                lines = temp.decode('utf-8').split('\n')
                lines = [x + '\n' for x in lines if x != '']  # isprint always starts with a blank line and ends with one after the split
                                                              # also want to add \n so we can write data to file

                if 'No records were selected with the filters above' in lines[0]:
                    if num_lines == 0:
                        raise madrigal.admin.MadrigalError('<%s> isprint_deprecated.py error: ' % (cmd)  + str(line), None)
                    else:
                        break

                num_lines += len(lines)
                f.writelines(lines)

                start_rec += 1000

        return tempfile_name



    def exportToHdf(self, output = None, independentSpatialParms = [], arraySplittingParms=[], extraParameters = [], 
                    filter = None, showWarnings=False, status=None, skipArray=False):
        """exportToHdf will write the Madrigal 2.X Cedar format file self._filename to the Madrigal 3 hdf5 format. 
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0. Issues UserWarning.
        
        Inputs:
        
            output - hdf5 file to write.  If None (the default),
                write to self._filename + 'hdf5'
                
            independentSpatialParms - a list of parameters as mnemonics
                that represent independent spatial variables.  Causes array layout to be added to 
                output Hdf5 file.  Default is empty list, and no array layout.

            arraySplittingParms - a list of parameters as mnemonics used to split
                arrays into subarrays.  For example, beamcode would split data with separate beamcodes
                into separate arrays. The number of separate arrays will be up to the product of the number of 
                unique values found for each parameter, with the restirction that combinations with no records will
                not create a separate array. Default is empty list, and no array splitting.  This argument ignored
                if no independentSpatialParms argument
            
            extraParameters - These parameters will be added to the output file if they are not already in the input file.             
            
            filter - Filter argument as in isprint command as string (eg, 'ti.500,2000').  Just one filter allowed.
            
            showWarnings - if True, print warnings about problems to stdout. If False (the default), do not
            
            status - if None, get status from fileTab.txt.  Else use status passed in
            
            skipArray - if False (the default), create array if non-empty independentSpatialParms.  If True, never create array layout
        
        Affects: Writes Hdf5 output file

        Exceptions: Parameter required is not in the file nor can it be derived.
                    No records selected with the filters above
        """
        warnings.warn("This method should only be called during installation of Madrigal 3 or conversion of old Madrigal 2 format file", UserWarning)
        
        # verify independentSpatialParms exist if 2D parma
        if len(self.getMeasured2dParmList()) > 0:
            if len(independentSpatialParms) == 0:
                raise IOError('Cannot create a Madrigal 3 file with 2D parameters if no independent spatial parameters - update cachedFiles.ini for kinst %i kindat %i' \
                    % (self.getKinstList()[0], self.getKindatList()[0]))
            
        
        # open output HDF5 file
        if output == None: 
            output = os.path.splitext(self._filename)[0] + '.hdf5'
        try:
            # we need to make sure this file is closed and then deleted if an error
            f = None # used if next line fails
            f = h5py.File(output, 'w')
            dataGroup = f.create_group("Data")
            metaGroup = f.create_group("Metadata")
            
            # create needed Madrigal objects
            parmObj = madrigal.data._DeprecatedMadrigalParameters(self._madDB)
            
            parmsWantedList, uniqueValuesDict, longestStrList, twoDParmList = self._analyzeHdf5Parms(independentSpatialParms, 
                                                                                                     arraySplittingParms, extraParameters,
                                                                                                     parmObj, showWarnings=showWarnings)
            longestMnemStr, longestDescStr, longestUnitsStr, longestCategoryStr = longestStrList
            
                    
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
            
            # also create recarrays for independentSpatialParms and arraySplittingParms
            if len(independentSpatialParms):
                indSpatialArr = numpy.recarray((len(independentSpatialParms),),
                                     dtype = [('mnemonic', '|S%i' % (longestMnemStr)),
                                              ('description', '|S%i' % (longestDescStr))])
                
            if len(independentSpatialParms) and len(arraySplittingParms):
                arraySplitArr = numpy.recarray((len(arraySplittingParms),),
                                     dtype = [('mnemonic', '|S%i' % (longestMnemStr)),
                                              ('description', '|S%i' % (longestDescStr))])
            
            # set all the values
            for i in range(len(parmsWantedList)):
                parm = parmsWantedList[i]
                parmArr['mnemonic'][i] = parmObj.getParmMnemonic(parm)
                parmArr['description'][i] = parmObj.getSimpleParmDescription(parm)
                parmArr['isError'][i] = parmObj.isError(parm)
                parmArr['units'][i] = parmObj.getParmUnits(parm)
                parmArr['category'][i] = parmObj.getParmCategory(parm)
            
            # write parameter description to top level
            metaGroup.create_dataset('Data Parameters', data=parmArr)
            
            # Independent Spatial Parameters
            if len(independentSpatialParms):
                for i in range(len(independentSpatialParms)):
                    parm = independentSpatialParms[i]
                    indSpatialArr['mnemonic'][i] = parmObj.getParmMnemonic(parm)
                    indSpatialArr['description'][i] = parmObj.getSimpleParmDescription(parm)
                metaGroup.create_dataset('Independent Spatial Parameters', data=indSpatialArr)
                
            # Parameters Used to Split Array Data
            if len(independentSpatialParms) and len(arraySplittingParms):
                for i in range(len(arraySplittingParms)):
                    parm = arraySplittingParms[i]
                    arraySplitArr['mnemonic'][i] = parmObj.getParmMnemonic(parm)
                    arraySplitArr['description'][i] = parmObj.getSimpleParmDescription(parm)
                metaGroup.create_dataset('Parameters Used to Split Array Data', data=arraySplitArr)
            
            # first build the needed dtypes dynamically - dataDtype is for the main table layout dataset, while
            # recDtype is for metadata/_record_layout - one line for each record - all data is int 
            # 3 for independent 2D,2 for 2D, 1 for 1D
            dataDtype = []
            recDtype = []
            
            for parm in parmsWantedList:
                dataDtype.append((parm.lower(), float))
                recDtype.append((parm.lower(), 'int')) # all information in this table in integer form
    
            # next, we need to deal with the actual data in the file - using an isprint_deprecated.py cmd twice,
            # first time to count the data, the next to read it.
            parmsStr = ""
            for parm in parmsWantedList:
                parmsStr += parm.lower() + ' '
            
            cmd = self._madDB.getMadroot() + '/bin/isprint_deprecated.py '
            cmd += 'file=%s ' %(self._filename)
            cmd += 'summary=f '
            cmd += 'header=f '
            if filter != None:
                cmd += 'filter=%s ' %(filter)
            cmd += parmsStr

            # execute the command and cache results to file to reduce RAM usage and
            # limit the number of times we have to run the isprint command (it is slower
            # than file I/O)
            tempfile_name = self._execute_isprint(cmd)

            lineCount, totalRecordCount = self._getRecordLineCount(tempfile_name,parmsWantedList,cmd)
            self.lineCount = lineCount
            self.totalRecordCount = totalRecordCount

            # the next step is to create a numpy.recarray to hold all (or part) of this data
            MAX_RECORDS = 100000 # used to reduce memory footprint of temp numpy array
            numpyIndex = 0 # index of the temporary numpy array holding some or all of the data
            dataArr = numpy.recarray((min(lineCount, MAX_RECORDS),), dtype = dataDtype)
            recordIndex = 0
            recArr = numpy.recarray((1,), dtype=recDtype)


            # fi, fe = subObj.communicate()

            # next put all the data into dataArr
            recordCount = -1 # used instead of recno so that counts always start at 0
            lastRecno = -1 # keep track of changes in recno to count records
            # lines = fi.decode('utf-8').split('\n')
            # for line in lines:
            with open(tempfile_name,'r') as f:
                while True:
                    line = f.readline()
                    if line == '':
                        break
                    line = line.strip('\n')

                    items = line.split() # len(items) = len(parmsWantedList)
                    if len(items) == 0:
                        continue
                    elif len(items) != len(parmsWantedList):
                        raise madrigal.admin.MadrigalError('isprint_deprecated.py error: ' + str(line), None)
                    # keep track of what record we are working on - may differ from recno itself
                    recno = int(items[6])
                    if recno != lastRecno:
                        recordCount += 1
                        lastRecno = recno
                    for j in range(len(parmsWantedList)):
                        parm = parmsWantedList[j]
                        if parm.lower() == 'recno':
                            # this is the one parameter where the isprint_deprecated.py data could be wrong, 
                            # because records could have been filtered out
                            dataArr[parm.lower()][recordIndex] = recordCount
                            continue
                        
                        try:
                            value = float(items[j])
                            if numpy.isnan(value):
                                items[j] = 'missing'
                                raise ValueError('')
                            # add this value to uniqueValuesDict if needed
                            if parm.lower() in list(uniqueValuesDict.keys()):
                                # see if we can use an integer instead of a float
                                if parmObj.isInteger(parm):
                                    keyValue = int(value)
                                else:
                                    keyValue = value
                                if keyValue not in uniqueValuesDict[parm.lower()]:
                                    uniqueValuesDict[parm.lower()].append(keyValue)
                                    uniqueValuesDict[parm.lower()].sort()
                        except ValueError:
                            if items[j].lower() == 'missing':
                                value = numpy.nan
                            elif parmObj.isError(parm) and items[j].lower() == 'assumed':
                                value = -1.0
                            elif parmObj.isError(parm) and items[j].lower() == 'knownbad':
                                value = -2.0
                            elif parmObj.isInteger(parm) and len(items[j]) == 1:
                                # assume its a character
                                if items[j] == '_': # used to indicate missing
                                    value = numpy.nan
                                else:
                                    value = float(ord(items[j][0]))
                            else:
                                raise ValueError('Unknown value %s' % (items[j]))
                        dataArr[parm.lower()][recordIndex] = value
                    recordIndex += 1
        
                    
                    if recordIndex % MAX_RECORDS == 0:
                        # need to create multiple numpy arrays to reduce memory footprint
                        if numpyIndex == 0:
                            tableName = 'Table Layout'
                            dset = dataGroup.create_dataset(tableName, data=dataArr, maxshape=(lineCount,), compression='gzip')
                        else:
                            dset.resize((min(lineCount, (numpyIndex+1) * MAX_RECORDS),))
                        # set all values from dataArr
                        dset[numpyIndex * MAX_RECORDS : (numpyIndex+1) * MAX_RECORDS] = dataArr
                        numpyIndex += 1
                        recordIndex = 0
                        dataArr = numpy.recarray((min(lineCount - (numpyIndex * MAX_RECORDS), MAX_RECORDS),), 
                                                 dtype = dataDtype)
        
            os.remove(tempfile_name)
                    
            # write data to top level
            if numpyIndex > 0 and dataArr.shape[0] > 0:
                dset.resize((lineCount,))
                # set all values from dataArr
                dset[numpyIndex * MAX_RECORDS : lineCount] = dataArr
            elif dataArr.shape[0] > 0:
                dset = dataGroup.create_dataset('Table Layout', data=dataArr, compression='gzip')
                
            self._setRecordMetadata(recArr, dset, independentSpatialParms, twoDParmList)
            recDset = metaGroup.create_dataset('_record_layout', data=recArr)
            recDset.attrs['description'] = 'This is meant to be internal data.  For each Madrigal record and parameter, it has a 2 if its a 2D parameter, 1 if its a 1D parameter, and 0 if there is no data.'
            
            self._setMetadata(metaGroup, status)
            
            del dataArr
            
            # Add array layout to hdf5 file if needed
            if len(independentSpatialParms) > 0 and not skipArray:
                
                # put one or more arrays in "Array Layout" Group
                arrGroup = dataGroup.create_group("Array Layout")
                
                if len(list(uniqueValuesDict.keys())) == 0:
                    self._createHdf5Array(arrGroup, independentSpatialParms, parmObj,
                                          cmd, parmsWantedList, longestMnemStr, longestDescStr, 
                                          longestUnitsStr, longestCategoryStr, {}, extraParameters,
                                          twoDParmList)
                else:
                    # we need to call this method once for each unique combination of unique values in uniqueValuesDict
                    mnemList = list(uniqueValuesDict.keys())
                    mnemList.sort()
                    rangeList = [] # a list of indices in each menemonic
                    for mnem in mnemList:
                        rangeList.append(list(range(len(uniqueValuesDict[mnem]))))
                        
                    for indexList in itertools.product(*rangeList):
                        thisMnemDict = {}
                        for j, thisIndex in enumerate(indexList):
                            thisMnemDict[mnemList[j]] = uniqueValuesDict[mnemList[j]][thisIndex]
                        
                        # some combination may not exist
                        try:
                            self._createHdf5Array(arrGroup, independentSpatialParms, parmObj,
                                                  cmd, parmsWantedList, longestMnemStr, longestDescStr, 
                                                  longestUnitsStr, longestCategoryStr, thisMnemDict,
                                                  extraParameters, twoDParmList)
                        except  madrigal.admin.MadrigalError:
                            if showWarnings:
                                print(('no data for dict %s' % (str(thisMnemDict))))
                
            f.close()
            
        except:
            # on any error, close and delete file, then reraise error
            if f:
                f.close()
            if os.path.exists(output):
                os.remove(output)
            if os.path.exists(tempfile_name):
                os.remove(tempfile_name)
            raise
        
        
    def _analyzeHdf5Parms(self, independentSpatialParms, arraySplittingParms, extraParameters, parmObj, showWarnings=False):
        """_analyzeHdf5Parms gets metadata about which parameters are needed and their description strings.
        It is a helper method for exportToHdf.
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
            independentSpatialParms - a list of parameters as mnemonics
                that represent independent spatial variables.
            arraySplittingParms - a list of parameters as mnemonics used to split
                arrays into subarrays.  
            extraParameters - These parameters will be added to the output file if they are not already in the input file.
            parmObj - madrigal.data._DeprecatedMadrigalParameters object
            showWarnings - if True, show warnings
        
        Returns:
         a tuple with items:
             1. parmsWantedList - list of all parameters wanted in the output file when isprint_deprecated.py called
             2. uniqueValuesDict - dictionary with keys = mnemonics to split arrays into, and value = sorted list of values of that mnemonic
             3. longestStrList - a list of lengths of the longest string length for (mnemonics, parm description,
                  units, category)
             4. a list of 2D measured parms in file in lower case mnemonic
        """
        defaultParms = ['year', 'month', 'day', 'hour', 'min', 'sec', 'recno', 'kindat', 'kinst', 'ut1_unix', 'ut2_unix']
        parmsWantedList = defaultParms[:]
        
        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []
        
        twoDParmList = [parmObj.getParmMnemonic(p).lower() for p in self.getMeasured2dParmList()]
        meas1DParmList = self.getMeasured1dParmList()
        meas2DParmList = self.getMeasured2dParmList() + self.getMeasuredIndParmList()
        oneDDerivable, twoDDerivable = madrigal.derivation.get1D2DDerivableParms(meas1DParmList, meas2DParmList)
        
        
        self.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Comprehensive'),
                                      measParmList,
                                      derivedParmList,
                                      allParmList,
                                      sureParmList,
                                      parmObj=parmObj)
        
        # Add independent spatial parameters to parmsWantedList 
        failedIndParms = []
        for parm in independentSpatialParms:
            parmMnem = self._madParmObj.getParmMnemonic(parm)
            if parmMnem.lower() in parmsWantedList:
                continue
            if parmMnem.upper() not in allParmList:
                print(('The independent spatial parameter required, %s, is not in the file nor can it be derived - will be ignored' % (parmMnem.upper())))
                failedIndParms.append(parm)
                continue
            parmsWantedList.append(parmMnem.lower())
            
        for badParm in failedIndParms:
            independentSpatialParms.remove(badParm)
            
        # make sure splitting parms are in parmsWantedList and uniqueValuesDict
        uniqueValuesDict = {} # dictionary with keys = mnemonics to split arrays into, and value = sorted list of values of that mnemonic
        notFoundList = []
        for parm in arraySplittingParms:
            parmMnem = self._madParmObj.getParmMnemonic(parm)
            if parmMnem.upper() not in allParmList:
                print(('The array splitting parameter required, %s, is not in the file nor can it be derived - will be dropped' % (parmMnem.upper())))
                notFoundList.append(parm)
                continue
            uniqueValuesDict[parmMnem.lower()] = []  # we will find the unique values during the first pass through the data
            if parmMnem.lower() in parmsWantedList:
                continue
            else:
                parmsWantedList.append(parmMnem.lower())   
                
        for thisParm in notFoundList:
            arraySplittingParms.remove(thisParm)      
        
        # Add all parameters stored in the file to parmsWantedList        
        for parm in measParmList:
            if parmObj.isAddIncrement(parm) or (self._madParmObj.getParmMnemonic(parm).lower() in parmsWantedList):
                continue
            parmsWantedList.append(self._madParmObj.getParmMnemonic(parm).lower())
        
        # Add extra parameters to parmsWantedList
        for parm in extraParameters:
            parmMnem = self._madParmObj.getParmMnemonic(parm)
            
            if parmMnem.lower() in parmsWantedList:
                continue
            
            if parmMnem.upper() not in allParmList:
                if showWarnings:
                    print(('The parameter required, %s, is not in the file %s nor can it be derived' % (parmMnem.upper(), self._filename)))
                continue
            
            parmsWantedList.append(parmMnem.lower())
            if parmMnem.upper() in twoDDerivable and parmMnem.lower() not in twoDParmList:
                twoDParmList.append(parmMnem.lower())
            
        # local variables to find longest string length
        longestMnemStr = 0
        longestDescStr = 0
        longestUnitsStr = 0
        longestCategoryStr = 0  
        
        for parm in parmsWantedList:         
            if len(parmObj.getParmMnemonic(parm)) > longestMnemStr:
                longestMnemStr = len(parmObj.getParmMnemonic(parm))
            if len(parmObj.getSimpleParmDescription(parm)) > longestDescStr:
                longestDescStr = len(parmObj.getSimpleParmDescription(parm))
            if len(parmObj.getParmUnits(parm)) > longestUnitsStr:
                longestUnitsStr = len(parmObj.getParmUnits(parm))
            if len(parmObj.getParmCategory(parm)) > longestCategoryStr:
                longestCategoryStr = len(parmObj.getParmCategory(parm))
                
        return((parmsWantedList, uniqueValuesDict, (longestMnemStr, longestDescStr, 
                                                    longestUnitsStr, longestCategoryStr),
                twoDParmList))
        
        
        
    def _getRecordLineCount(self, isprint_output_file, parmsWantedList, cmd):
        """_getRecordLineCount returns a tuple of (lineCount, recordCount, cmdUsed) in the file with any possible filter applied
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
            parmsWantedList - list of all parameters wanted in the output file when isprint_deprecated.py called
            filter - Filter argument as in isprint_deprecated.py command as string (eg, 'ti.500,2000').  Just one filter allowed.
        """
        
        # the first pass is just to get a line count
        # readline is used instead of readlines to reduce the memory footprint of large files
        # subObj = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        # fi, fe = subObj.communicate()
        lineCount = 0
        recordCount = 0
        lastRecno = -1 # keep track of changes in recno to count records

        with open(isprint_output_file,'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    break
                line = line.strip('\n')

                items = line.split() # len(items) = len(parmsWantedList)
                if len(items) == 0:
                    continue
                elif len(items) != len(parmsWantedList):
                    raise madrigal.admin.MadrigalError('<%s> isprint_deprecated.py error: ' % (cmd) + str(line), None)
                recno = int(items[6])
                if recno != lastRecno:
                    recordCount += 1
                    lastRecno = recno
                lineCount += 1
        
        return((lineCount, recordCount))
    
    
    
    def _setMetadata(self, metaGroup, status=None):
        """_setMetadata is responsible for writing the metadata to the Hdf5 file
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
            metaGroup - the H5py metadata group object
            
            status - if None, get status from fileTab.txt.  Else use status passed in
        """
        # add catalog and header text
        catheadLines = self._getMad2CatalogHeaderStr()
        catheadLines = catheadLines.strip()
        catheadLines = catheadLines.split('\n')
        
        numLines = len(catheadLines)
        
        # create a recarray to hold this text
        textArr = numpy.recarray((numLines,), dtype=[('File Notes', '|S80')])
        for i in range(len(catheadLines)):
            textArr['File Notes'][i] = catheadLines[i]
            
        # write data to top level
        dset = metaGroup.create_dataset('Experiment Notes', data=textArr)
        
        inst = self.getKinstList()[0]
        instrumentName = self.getKinstListStr()
        if instrumentName is None:
            instrumentName = 'unknown'
        categoryStr = self._instMetadata.getCategory(inst)
        if categoryStr is None:
            categoryStr = 'unknown'
        piStr = self._instMetadata.getContactName(inst)
        piEmailStr = self._instMetadata.getContactEmail(inst)
        if piStr is None:
            piStr = 'unknown'
        if piEmailStr is None:
            piEmailStr = 'unknown'
        delimiter = ','
        kinstCodes = []
        for code in self.getKinstList():
            kinstCodes.append(str(code))
        instrumentCodes = delimiter.join(kinstCodes)
        
        tmpDate = self.getEarliestTime()
        startDate = datetime.datetime(tmpDate[0], tmpDate[1], tmpDate[2], tmpDate[3], tmpDate[4], tmpDate[5])
        startDateStr = startDate.strftime('%Y-%m-%d %H:%M:%S UT')
        
        tmpDate = self.getLatestTime()
        endDate = datetime.datetime(tmpDate[0], tmpDate[1], tmpDate[2], tmpDate[3], tmpDate[4], tmpDate[5])
        endDateStr = endDate.strftime('%Y-%m-%d %H:%M:%S UT')
        
        cedarFileName = self._filename + '.hdf5'

        if status is None:
            fileTab = os.path.join(os.path.dirname(self._filename), 'fileTab.txt')
            fileMetadata = madrigal.metadata.MadrigalMetaFile(self._madDB, fileTab)
            statusDesc = fileMetadata.getStatusByFilename(self._filename)
            if statusDesc is None:
                statusDesc = ''
        else:
            statusDesc = status
        instLat = self._instMetadata.getLatitude(inst)
        instLon = self._instMetadata.getLongitude(inst)
        instAlt = self._instMetadata.getAltitude(inst)
        
        # create kindat description based on all kindats
        kindatList = self.getKindatList()
        kindatDesc = ''
        kindatListStr = ''
        if len(kindatList) > 1:
            kindatDesc = 'This experiment has %i kinds of data.  They are:' % (len(kindatList))
            for i, kindat in enumerate(kindatList):
                thisKindatDesc = self._madKindatObj.getKindatDescription(kindat, inst)
                if not thisKindatDesc:
                    raise IOError('kindat %i undefined - please add to typeTab.txt' % (kindat))
                thisKindatDesc = thisKindatDesc.strip()
                kindatDesc += ' %i) %s (code %i)' % (i+1, thisKindatDesc, kindat)
                kindatListStr += '%i' % (kindat)
                if i < len(kindatList) - 1:
                    kindatDesc += ', '
                    kindatListStr += ', '
        else:
            kindatDesc = self._madKindatObj.getKindatDescription(kindatList[0], inst)
            if not kindatDesc:
                raise IOError('kindat for %s undefined - please add to typeTab.txt' % (str((kindatList[0], inst))))
            kindatDesc = kindatDesc.strip()
            kindatListStr += '%i' % (kindatList[0])
            
        # create an expSummary numpy recarray  
        # find max string length
        maxStrLen = max(len(instrumentName), len(instrumentCodes), len(kindatListStr), len(startDateStr), 
                        len(endDateStr), len(cedarFileName), len(kindatDesc), len(statusDesc),
                        len(categoryStr), len(piStr), len(piEmailStr)) 

        summArr = numpy.recarray((14,), dtype = [('name', 'S%i' % (20)),
                                                ('value', 'S%i' % (maxStrLen+2))])
        
        summArr['name'][0] = 'instrument'
        summArr['name'][1] = 'instrument code(s)'
        summArr['name'][2] = 'kind of data file'
        summArr['name'][3] = 'kindat code(s)'
        summArr['name'][4] = 'start time'
        summArr['name'][5] = 'end time'
        summArr['name'][6] = 'Cedar file name'
        summArr['name'][7] = 'status description'
        summArr['name'][8] = 'instrument latitude'
        summArr['name'][9] = 'instrument longitude'
        summArr['name'][10] = 'instrument altitude'
        summArr['name'][11] = 'instrument category'
        summArr['name'][12] = 'instrument PI'
        summArr['name'][13] = 'instrument PI email'
                      
        summArr['value'][0] = self._getOnlyPrintableAscii(instrumentName)
        summArr['value'][1] = self._getOnlyPrintableAscii(instrumentCodes)
        summArr['value'][2] = self._getOnlyPrintableAscii(kindatDesc)
        summArr['value'][3] = self._getOnlyPrintableAscii(kindatListStr)
        summArr['value'][4] = self._getOnlyPrintableAscii(startDateStr)
        summArr['value'][5] = self._getOnlyPrintableAscii(endDateStr)
        summArr['value'][6] = self._getOnlyPrintableAscii(cedarFileName)
        summArr['value'][7] = self._getOnlyPrintableAscii(statusDesc)
        summArr['value'][8] = self._getOnlyPrintableAscii(str(instLat))
        summArr['value'][9] = self._getOnlyPrintableAscii(str(instLon))
        summArr['value'][10] = self._getOnlyPrintableAscii(str(instAlt))
        summArr['value'][11] = self._getOnlyPrintableAscii(categoryStr)
        summArr['value'][12] = self._getOnlyPrintableAscii(piStr)
        summArr['value'][13] = self._getOnlyPrintableAscii(piEmailStr)
        
        # write data to top level
        dset = metaGroup.create_dataset('Experiment Parameters', data=summArr)
                
                
            
            
    def _createHdf5Array(self, arrHighLevelGroup, independentSpatialParms, parmObj, cmd, parmsWantedList,
                         longestMnemStr, longestDescStr, longestUnitsStr, longestCategoryStr,
                         extraDict, extraParameters, twoDParmList=[]):
        """
        _createHdf5Array adds an array section to the data group
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
            arrHighLevelGroup - the h5py.Group object for array data.  If more than one array layout, it will
                hold groups with each array layout. If only one, it will directly hold the datasets.
            independentSpatialParms - a list of parameters as mnemonics that represent independent spatial variables.
            parmObj - a madrigal.metadata._DeprecatedMadrigalParameter object
            cmd - the isprint_deprecated.py command used for the table data
            parmsWantedList - a list of lower case mnemonics 
            longestMnemStr - length of longest mnemonic (used to lay out strings)
            longestDescStr - length of longest mnemonic description (used to lay out strings)
            longestUnitsStr - length of longest units string (used to lay out strings)
            longestCategoryStr - length of longest category string (used to lay out strings)
            arrayName - name of array group. Default is "Array Layout"
            extraDict - a dictionary with extra filters.  Keys are mnemonics to filter with, 
                values are only value to accept.  May be empty dictionary.
            extraParameters - extra parameters to add to array if possible
            twoDParmList - parameters know to be 2D.  Default is empty list
        """
        xparm = 'ut1_unix'
        rec_parm = 'recno'
        
        yparmList = independentSpatialParms
        
        # returns a list of strings
        LayoutDescription = self._getLayoutDescription('array')
        
        maxLen = 0 # maxLen of any line in LayoutDescription
        yParmDesc = ''
        for i in range(len(yparmList)):
            yParmDesc += parmObj.getSimpleParmDescription(yparmList[i])
            if i < len(yparmList) - 1:
                yParmDesc += ', '
        for i, layoutStr in enumerate(LayoutDescription):
            if layoutStr == LayoutDescription[-1]:
                LayoutDescription[i] = layoutStr % (str(yparmList), yParmDesc)
            elif layoutStr.find('%s') != -1:
                # Dynamically set the right yparm in the description
                LayoutDescription[i] = layoutStr % (str(yparmList))
            maxLen = max(maxLen, len(LayoutDescription[i]))
        
        # create a recarray to hold this text
        textArr = numpy.recarray((len(LayoutDescription),), dtype=[('Layout Description', '|S%i' % (maxLen + 1))])
        for i in range(len(LayoutDescription)):
            textArr['Layout Description'][i] = LayoutDescription[i]
        
        
        # next read number of timestamps and spatial positions
        xList = []
        recnoList = []
        yDict = {} # key = yparm, value = list of all values found
        
        # reject any data without the needed yparm
        for thisYparm in yparmList:
            cmd += ' filter=%s,, ' % (thisYparm)
            
        # reject any data not in this array
        for key in list(extraDict.keys()):
            # handle float matching issue by incementing by oner part in 10^4
            if math.fmod(extraDict[key], 1.0) != 0.0:
                # not an integer
                lowerLimit = extraDict[key] - 1.0E-4*extraDict[key]
                upperLimit = extraDict[key] + 1.0E-4*extraDict[key]
            else:
                lowerLimit = extraDict[key]
                upperLimit = extraDict[key]
            cmd += ' filter=%s,%g,%g ' % (key,lowerLimit, upperLimit)

        # the first pass is just to get the x values and y values
        # readline is used instead of readlines to reduce the memory footprint of large files
        prevTime = None # used to cound time changes
        prevRecno = None # used to recno changes

        # get the isprint output and cache it so we don't have to call isprint more than once
        tempfile_name = self._execute_isprint(cmd)

        with open(tempfile_name,'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    break
                line = line.strip('\n')
            
                items = line.split() # len(items) = len(parmsWantedList)
                if len(items) == 0:
                    continue
                elif len(items) != len(parmsWantedList):
                    print('Unable to create array layout because of isprint_deprecated.py error: ' + str(line))
                    return
                    
                for j in range(len(parmsWantedList)):
                    parm = parmsWantedList[j]
                    if parm.lower() == rec_parm:
                        value = int(float(items[j]))
                        if value != prevRecno:
                            recnoList.append(value)
                            prevRecno = value
                        continue
                    elif parm.lower() == xparm:
                        value = int(float(items[j]))
                        if value != prevTime:
                            xList.append(value)
                            prevTime = value
                        continue
                    
                    if parm.lower() in yparmList:
                        try:
                            value = float(items[j])
                        except:
                            continue
                        if parm.lower() not in list(yDict.keys()):
                            yDict[parm.lower()] = []
                        if value not in yDict[parm.lower()]:
                            yDict[parm.lower()].append(value)
                        continue
        
        xList = sorted(xList)
        for key in list(yDict.keys()):
            yDict[key].sort()
        
        max_x_dimension = len(xList)
        max_y_dimension_list = []
        total_ind_parm_combinations = 1 # give up if greater than 1000000
        for thisYParm in yparmList:
            max_y_dimension_list.append(len(yDict[thisYParm]))
            total_ind_parm_combinations *= len(yDict[thisYParm])
            
        if total_ind_parm_combinations > 1000000:
            print(('Too many ind parm combinations %i - abandoning array layout' % (total_ind_parm_combinations)))
            return(None)
        
        # build dictionary of indexes into xList
        xListDict = {}
        for i in range(len(xList)):
            xListDict[xList[i]] = i
        
        # build dictonary of indexes into yDict
        yListDict = {} # key is yparm, value is dict with key = y value, value = y index
        for thisYParm in list(yDict.keys()):
            yListDict[thisYParm] = {}
            for i in range(len(yDict[thisYParm])):
                yListDict[thisYParm][yDict[thisYParm][i]] = i

        # Create 1D array
        # first build the needed dtype dynamically
        parm1DList = []
        data1Dtype = []
        for parm in self.getMeasured1dParmList():
            if parmObj.isAddIncrement(parm):
                continue
            mnemonic = self._madParmObj.getParmMnemonic(parm).lower()
            parm1DList.append(mnemonic)
            data1Dtype.append((mnemonic, 'float'))
        
        # Create 2D array
        parm2DList = []
        data2Dtype = []
        for parm in self.getMeasured2dParmList():
            if parmObj.isAddIncrement(parm):
                continue
            mnemonic = self._madParmObj.getParmMnemonic(parm).lower()
            if mnemonic in yparmList:
                continue
            parm2DList.append(mnemonic)
            data2Dtype.append((mnemonic, 'float'))
            
        # add derived parameters from parmsWantedList to correct (1D or 2D) list
        for parm in extraParameters:
            if parm.lower() in parm1DList or parm.lower() in parm2DList:
                continue
            if parm.lower() in twoDParmList:
                parm2DList.append(parm.lower())
                data2Dtype.append((parm.lower(), 'float'))
            elif parm.upper() in madrigal._Madrec.getDerivableParms(parm1DList):
                parm1DList.append(parm.lower())
                data1Dtype.append((parm.lower(), 'float'))
            else:
                parm2DList.append(parm.lower())
                data2Dtype.append((parm.lower(), 'float'))
                
        # the next step is to create a numpy.recarray to hold all of this data
        # its possible there is no 1D data
        if len(data1Dtype) > 0:
            data1DArr = numpy.recarray((max_x_dimension,), dtype = data1Dtype)
        if len(data2Dtype) > 0:
            data2DArr = numpy.recarray(max_y_dimension_list + [max_x_dimension], dtype = data2Dtype)
        else:
            # skip creating an array layout
            return(None)
        
        # initializing numpy array
        if len(data1Dtype) > 0:
            for parm in parm1DList:
                data1DArr[parm][:] = numpy.NAN
        for parm in parm2DList:
            data2DArr[parm][:] = numpy.NAN
        
        
        
        # next put all the data into data1DArr
        presentTime = None
        xIndex = parmsWantedList.index(xparm)
        yIndexList = []
        for thisYParm in yparmList:
            yIndexList.append(parmsWantedList.index(thisYParm))

        with open(tempfile_name,'r') as f:
            while True:
                line = f.readline()
                if line == '':
                    break
                line = line.strip('\n')

                items = line.split() # len(items) = len(parmsWantedList)
                if len(items) == 0:
                    continue
                elif len(items) != len(parmsWantedList):
                    raise madrigal.admin.MadrigalError('isprint_deprecated.py error: ' + str(line), None)
                
                xdata = int(float(items[xIndex]))
                ydataList = []
                for i in range(len(yparmList)):
                    try:
                        ydataList.append(float(items[yIndexList[i]]))
                    except ValueError:
                        raise ValueError('To create hdf5 file yparm %s cannot be missing, but is missing in line %s' % (yparm, str(line)))
                
                if xdata != presentTime:
                    newTimeFound = True
                else:
                    newTimeFound = False
                    
                presentTime = xdata
                
                i = xListDict[xdata]
                yIndex = []
                for j, thisYParm in enumerate(yparmList):
                    yIndex.append([yListDict[thisYParm][ydataList[j]]])
                yIndex.append([i])
                
                for k in range(len(parmsWantedList)):
                    parm = parmsWantedList[k]
                    
                    if (parm.lower() not in parm1DList) and (parm.lower() not in parm2DList):
                        continue
                    
                    try:
                        value = float(items[k])
                    except:
                        if items[k].lower() == 'missing':
                            value = numpy.nan
                        elif parmObj.isError(parm) and items[k].lower() == 'assumed':
                            value = -1.0
                        elif parmObj.isError(parm) and items[k].lower() == 'knownbad':
                            value = -2.0
                        else:
                            print(('Warning: Unknown value %s' % (items[k])))
                            value = numpy.nan
                        
                    if parm.lower() in parm1DList:
                        if newTimeFound:
                            data1DArr[parm.lower()][i] = value
                        continue
                    
                    if parm.lower() in parm2DList:
                        data2DArr[parm.lower()][tuple(yIndex)] = value
                        continue
        
        os.remove(tempfile_name)

        # now that we are past all possible exceptions, we can create groups
        if len(list(extraDict.keys())) == 0:
            # there is only one array layout - no need for a subgroup
            arrGroup = arrHighLevelGroup
        else:
            groupName = 'Array with '
            keys = list(extraDict.keys())
            keys.sort()
            for key in keys:
                groupName += '%s=%s ' % (key, str(extraDict[key]))
                if key != keys[-1]:
                    groupName += 'and '
            arrGroup = arrHighLevelGroup.create_group(groupName)
        oneDGroup = arrGroup.create_group("1D Parameters")
        twoDGroup = arrGroup.create_group("2D Parameters")
        
        dset1DDict = {}
        for parm in parm1DList:
            dset1D = oneDGroup.create_dataset(parm, data=data1DArr[parm], compression='gzip')
            dset1DDict[parm] = dset1D
        
        dset2DDict = {}
        for parm in parm2DList:
            dset2D = twoDGroup.create_dataset(parm, data=data2DArr[parm], compression='gzip')
            dset2DDict[parm] = dset2D
        
        # Save timestamps and yparm
        timestamps = numpy.recarray((len(xList),),dtype=[('timestamps',int)])
        timestamps = numpy.asarray(xList)
        yaxisList = []
        for thisYParm in yparmList:
            yaxisList.append(numpy.asarray(yDict[thisYParm]))
            
        # set all the values
        if len(data1Dtype) > 0:
            parm1DArr = numpy.recarray((len(parm1DList),),
                             dtype = [('mnemonic', '|S%i' % (longestMnemStr)),
                                      ('description', '|S%i' % (longestDescStr)),
                                      ('isError', 'int'),
                                      ('units', '|S%i' % (longestUnitsStr)),
                                      ('category', '|S%i' % (longestCategoryStr))])
        
        for i in range(len(parm1DList)):
            parm = parm1DList[i]
            parm1DArr['mnemonic'][i] = parmObj.getParmMnemonic(parm)
            parm1DArr['description'][i] = parmObj.getSimpleParmDescription(parm)
            parm1DArr['isError'][i] = parmObj.isError(parm)
            parm1DArr['units'][i] = parmObj.getParmUnits(parm)
            parm1DArr['category'][i] = parmObj.getParmCategory(parm)
        
        # set all the values
        parm2DArr = numpy.recarray((len(parm2DList),),
                         dtype = [('mnemonic', '|S%i' % (longestMnemStr)),
                                  ('description', '|S%i' % (longestDescStr)),
                                  ('isError', 'int'),
                                  ('units', '|S%i' % (longestUnitsStr)),
                                  ('category', '|S%i' % (longestCategoryStr))])
        
        for i in range(len(parm2DList)):
            parm = parm2DList[i]
            parm2DArr['mnemonic'][i] = parmObj.getParmMnemonic(parm)
            parm2DArr['description'][i] = parmObj.getSimpleParmDescription(parm)
            parm2DArr['isError'][i] = parmObj.isError(parm)
            parm2DArr['units'][i] = parmObj.getParmUnits(parm)
            parm2DArr['category'][i] = parmObj.getParmCategory(parm)
            
        dset = arrGroup.create_dataset('Layout Description', data=textArr)
            
        dset = arrGroup.create_dataset("timestamps", data=timestamps)
        for i, thisYParm in enumerate(yparmList):
            dset = arrGroup.create_dataset(thisYParm, data=yaxisList[i])
        if len(data1Dtype) > 0:
            dset = oneDGroup.create_dataset('Data Parameters', data=parm1DArr)
        dset = twoDGroup.create_dataset('Data Parameters', data=parm2DArr)
            
                    
        
        
    def getCachedHdf5(self, iniFile=None, overwrite=False, showWarnings=False):
        """getCachedHdf5 will get the full path to the cached version of the HDF5 file.  It will create
        the file if it does not already exist.
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs: 
            iniFile - ini file to use to create hdf5 with, if one needs to be created. If None, uses default ini 
                file $MADROOT/cachedFiles.ini
            overwrite - if True, overwrite existing hdf5 file.  If False (the default), use existing hdf5 file
            showWarnings - if True, print warnings about problems to stdout. If False (the default), do not
        Returns: full path to cached Hdf5 file
        """
        basename = os.path.basename(os.path.basename(self._filename)) + '.hdf5'
        filePath = os.path.join(os.path.dirname(self._filename), 'overview', basename)
        if os.path.exists(filePath) and not overwrite:
            return(filePath)
        
        # need to create the file 
        try:
            os.makedirs(os.path.dirname(filePath))
        except:
            pass
        kinst = self.getKinstList()[0]
        kindat = self.getKindatList()[0]
        extraParms, altFormatDict, skipArray = self._parseCachedIni(kinst, kindat, iniFile)
        independentSpatialParms = []
        arraySplittingParms=[]
        if 'array' in altFormatDict:
            value = altFormatDict['array']
            if type(value)  in (bytes, str):
                independentSpatialParms = [value]
            elif len(value) == 2 and type(value[0]) in (tuple, list):
                independentSpatialParms = value[0]
                arraySplittingParms = value[1]
            else:
                independentSpatialParms = value
        self.exportToHdf(filePath, independentSpatialParms, arraySplittingParms, extraParms, showWarnings=showWarnings,
                         skipArray=skipArray)
        try:
            os.chmod(filePath, 0o777)
        except:
            pass

        return(filePath)
                                
    

    
    
    def _parseCachedIni(self, kinst, kindat, iniFile=None):
        """_parseCachedIni parses an ini file for information needed to create cached files
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
        
            kinst - the instrument kinst (integer)
            
            kindat - the data kindat (integer)
            
            iniFile - the ini file to use.  If None, uses default ini file $MADROOT/cachedFiles.ini
            
        Returns: a tuple with three items:
            1. a list of extra parameters (string mnemonics)
            2. a dictionary of alternate formats (see exportToHdf)
            3. a bool of whether to skip array or not
            
        Algorithm:
        
        1. If iniFile == None and no default file, returns ([], {})
        2. Searches ini file for section [%i] % (kinst).  If not found, returns ([], {})
        3. Searches right section for key %i_parms % (kindat).  If not found, searches for default_parms.
            If not found, extra parameters are []
        4. Searches right section for key %i_formats % (kindat).  If not found, searches for default_formats.
            If not found, alternate format dictionary is {}
        """
        skipArray = False
        if not iniFile:
            thisIniFile = os.path.join(self._madDB.getMadroot(), 'cachedFiles.ini')
            if not os.path.exists(thisIniFile):
                return(([], {}, skipArray))
        else:
            thisIniFile = iniFile
            
        instSection = '%i' % (kinst)
            
        parser = configparser.SafeConfigParser()
        parser.read(thisIniFile)
        if not parser.has_section(instSection):
            return(([], {}, skipArray))
        extraParms = []
        formatDict = {}
        
        
        # get extra parms
        if parser.has_option(instSection, '%i_parms' % (kindat)):
            extraParms = parser.get(instSection, '%i_parms' % (kindat))
            extraParms = extraParms.split(',')
        elif parser.has_option(instSection, 'default_parms'):
            extraParms = parser.get(instSection, 'default_parms')
            extraParms = extraParms.split(',')
            
        # make sure no empty parms snuck in
        finalExtraParms = []
        for extraParm in extraParms:
            if len(extraParm.strip()) > 0:
                finalExtraParms.append(extraParm.strip())
        
        # get format dict
        if parser.has_option(instSection, '%i_formats' % (kindat)):
            formatDict = parser.get(instSection, '%i_formats' % (kindat))
            formatDict = eval(formatDict)
        elif parser.has_option(instSection, 'default_formats'):
            formatDict = parser.get(instSection, 'default_formats')
            formatDict = eval(formatDict)
            
        # get skipArray
        if parser.has_option(instSection, '%i_skip_array' % (kindat)):
            if parser.get(instSection, '%i_skip_array' % (kindat)) in ('True', 'true', '1'):
                skipArray = True
        elif parser.has_option(instSection, 'default_skip_array'):
            if parser.get(instSection, 'default_skip_array') in ('True', 'true', '1'):
                skipArray = True
            
        return((finalExtraParms, formatDict, skipArray))
    
    
    
    def _getOnlyPrintableAscii(self, inputStr):
        """_getOnlyPrintableAscii returns a string with all non printable characters replaced by spaces
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        """
        retStr = ''
        for c in inputStr:
            if c not in string.printable:
                retStr += ' '
            else:
                retStr += c
        return(retStr)
    

    def _getLayoutDescription(self, format):
        """_getLayoutDescription returns a description of the layout selected.
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.

        Inputs: 
            format: Alternates format available
        
        Returns: 
            LayoutDescription: A list of strings summarizing the Layout Description

        Affects: Nothing

        Exceptions: None
        """
        
        LayoutDescription = ""
        
        if format == "array":
            LayoutDescription = """
                This data layout contains reshaped data from the Table Layout. The reshaped data is stored as an array, with time and
                %s parameters in different dimensions. It creates an array for each parameter found in file.
                
                This layout contains:
                - "1D parameters" group: contains one 1D-array for each 1d parameter
                                  stored in the file. Time-dependent only parameters.
                - "2D parameters" group: contains one 2D-array for each 2d parameter
                                  stored in the file. Time and %s are dependent parameters.
                                  Every 2D array has one row for each time value and
                                  one column for each %s value.
                - timestamps:     Time vector in seconds from 1/1/1970.
                
                - %s :          The y parameter for this file: %s"""
                
        return LayoutDescription.split('\n')   
    
    
    def _setRecordMetadata(self, recArr, dset, independentSpatialParms, twoDParmList):
        """_setRecordMetadata sets metadata in recArr based on raw data dset.
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.
        
        Inputs:
            recArr - a numpy recarray with the same columns as in dset and a single row since it must be the same for
            all records.  recArr is all integers, with one row for every Madrigal record.  
            The values are limited to: 
                3 - if its an independent vector parameter,
                2 - if it a dependent 2D parameter in that record, and 
                1 - if a 1D parameter in that record
            
            dset - a h5py dataset with one row for every 2D record in the Madrigal file
            
            independentSpatialParms - a list of independent spatial parameters
            
            twoDParmList - list on known 2D parms in lower case mnemonic form
            
        Effects: sets values in recArr to be 3, 2, or 1
        """
        recnoArr = dset['recno']
        recnoValues = list(set(recnoArr))
        recnoValues.sort()
        
        # loop until you find a record where no parameters have all NaN data.  That record is skipped
        # because its not clear if its 1D or 2D
        parmDimDict = {}
        for recno in recnoValues:
            accept = True
            # get the subset of dset with this recno
            thisRec = dset[recnoArr == recno]
            for parm in thisRec.dtype.names:
                thisData = thisRec[parm]
                if parm in independentSpatialParms:
                    parmDimDict[parm] = 3
                elif parm in twoDParmList:
                    parmDimDict[parm] = 2
                # check for data is all nan, or else all the same
                elif len(numpy.nonzero(numpy.select([thisData == thisData], [1]))) == 0:
                    # only true if all data is nan - skip this record
                    if parm not in parmDimDict:
                        parmDimDict[parm] = 0 # will be set to 1 if not set by some other record
                    accept = False
                    break
                elif len(set(numpy.nan_to_num(thisData))) > 1:
                    # some value must vary
                    parmDimDict[parm] = 2
                else:
                    parmDimDict[parm] = 1 # all values the same
            
            if accept:
                # we can now set all values
                for parm in list(parmDimDict.keys()):
                    recArr[0][parm] = parmDimDict[parm]
                # apply rule that either the parm or its error is 2D, the other must be 2D also
                for parm in list(parmDimDict.keys()):
                    if self._madParmObj.isError(parm):
                        main_parm = parm[1:]
                        if main_parm in parmDimDict:
                            if recArr[0][parm] == 1 and recArr[0][main_parm] == 2:
                                recArr[0][parm] = 2
                            if recArr[0][main_parm] == 1 and recArr[0][parm] == 2:
                                recArr[0][main_parm] = 2
                return
            
        # if we arrived here there were parameters with unambiguous dimensions - assume they are 1D
        for parm in list(parmDimDict.keys()):
            if parmDimDict[parm] == 0:
                parmDimDict[parm] = 1
            recArr[0][parm] = parmDimDict[parm]
            
        # finally able rule that either the parm or its error is 2D, the other must be 2D also
        for parm in list(parmDimDict.keys()):
            if self._madParmObj.isError(parm):
                main_parm = parm[1:]
                if main_parm in parmDimDict:
                    if recArr[0][parm] == 1 and recArr[0][main_parm] == 2:
                        recArr[0][parm] = 2
                    if recArr[0][main_parm] == 1 and recArr[0][parm] == 2:
                        recArr[0][main_parm] = 2
            
            
    def _getMad2CatalogHeaderStr(self):
        """_getMad2CatalogHeaderStr returns a string formatted for printing containing all catalog and header records
        for a Madrigal 2 Cedar file.
        
        This method exists only to convert an old Madrigal 2.x installation to Madrigal 3.  It should never be called
        after installation of Madrigal 3.0.

        Input: None

        Returns: a string formatted for printing containing all catalog and header records. Returns '' if no
        catalog or header records.
        """
        retStr = ''
        
        catList, headList = madrigal._Madrec.cedarCatalogHeaderList(self._filename)

        for cat in catList:
            retStr += 'Catalog information from record %i:\n\n' % (cat)
            retStr += madrigal._Madrec.cedarGetInformation(self._filename, cat)
            retStr += '\n'

        for head in headList:
            retStr += 'Header information from record %i:\n\n' % (head)
            retStr += madrigal._Madrec.cedarGetInformation(self._filename, head)
            retStr += '\n'

        return retStr
                
            


class MadrigalParameters:
    """MadrigalParameters is an object that provides information about Madrigal parameters.

    This class provides access to the Cedar/Madrigal standards for parameters
    (such as getMnemonic, getDescription, getCodeFromMnemonic) and
    categories.  It will also examine an expression (string) and return the parameter mnemonics it contains.

    Usage example:

        import madrigal.data.MadrigalParameters
    
        test = madrigal.data.MadrigalParameters()

        parcode = test.getParmCodeFromMnemonic("YEAR")

        print parcode
        
    Now directly parses parmCodes.txt to remove Int16-based C library.  Stores parameter data in two
    separate dictionaries: 1) self._mnemDict - key = upper case mnemonic, value = _Parameter object,
    2) self._codeDict, key = code, value =  _Parameter object.  Note self._codeDict does not have parameters
    with code = 0.  Also stores a list of duplicate codes and duplicate mnemonics that should be intercepted
    and replaced with the main one in self._dupCodes and self._dupMnems


    Non-standard Python modules used:
    None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 27, 2001
    Added getMnemonicListFromExpression Jul. 16, 2002
    

    """


    def __init__(self, madDB = None):
        """__init__ initializes MadrigalParameters by getting some basic information from MadrigalDB.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self._binDir.  Populates self._mnemDict - key = upper case mnemonic, 
        value = _Parameter object.  Also populates self._codeDict, key = code, value =  _Parameter object.  
        Note self._codeDict does not have parameters with code = 0

        Exceptions: None.
        """
        self._mnemDict = {}
        self._codeDict = {}
        self._dupCodes = []
        self._dupMnems = []

        # get metadata dir
        if madDB == None:
            thisMadDB = madrigal.metadata.MadrigalDB()
        else:
            thisMadDB = madDB

        self._binDir = thisMadDB.getBinDir()
        self._madDB  = thisMadDB
        
        self._madCatObj = madrigal.metadata.MadrigalParmCategory(self._madDB)
        
        # parse parmCodes.txt
        f = open(os.path.join(self._madDB.getMadroot(), 'metadata/parmCodes.txt'))
        lines = f.readlines()
        f.close()
        
        position = -1 # keep track of line number
        for line in lines:
            if len(line.strip()) == 0:
                continue
            if line[0] == '#':
                continue
            position += 1
            items = line.split(',')
            code = int(items[0])
            description = items[1]
            units = items[2]
            mnemonic = items[3].upper()
            format = items[4]
            width = int(items[5])
            categoryId = int(items[6])
            hasDesc = int(items[7])
            hasErrDesc = int(items[8])
            if len(items) > 9:
                main_code = int(items[9])
                self._dupCodes.append(code)
                self._dupMnems.append(mnemonic)
            else:
                main_code = None
            parmObj = _Parameter(code, description, units, mnemonic, format, width, 
                                 categoryId, hasDesc, hasErrDesc, position, main_code)
            if mnemonic in self._mnemDict:
                raise ValueError('Found duplicate mnemonic in line %s' % (line))
            self._mnemDict[mnemonic] = parmObj
            if code > 0 and code in self._codeDict:
                raise ValueError('Found duplicate code in line %s' % (line))
            if code > 0:
                self._codeDict[code] = parmObj
            
            
                



    def _sortList(self, parmList):
        """ Private function that returns a new list of parameters sorted as isprint sorts parameters. """

        # create new list with negative parameters replaced by abs(parm) + 0.5 if positive parameter in list,
	# otherwise insert it as 100000 + abs(parm) so it goes to the end of the list (this is how isprint works)
        sortList = []
        for parm in parmList:
            if type(parm) in [str, bytes]:
                fixedParm = int(parm)
                if fixedParm < 0:
                    if abs(fixedParm) in parmList:
                        sortList.append(abs(fixedParm) + 0.5)
                    else:
                        sortList.append(100000 + abs(parm))
            elif parm < 0:
                if abs(parm) in parmList:
                    sortList.append(abs(parm) + 0.5)
                else:
                    sortList.append(100000 + abs(parm))
            else:
                sortList.append(parm)

        sortList.sort()

        # convert back to negative numbers
        listIndex = 0
        for parm in sortList:
            if type(parm) == float:
                sortList[listIndex] = -1 * int(parm)
            elif parm > 100000:
                sortList[listIndex] = 100000 - parm
            listIndex = listIndex + 1

        return sortList


    def _reorder(self, validList, sortedParmList, parmList):
        """ Private function that returns a new valid list sorted in the same parameter order as parmList. """

        newValidList = []

        for parm in parmList:
            i = sortedParmList.index(parm)
            newValidList.append(validList[i])

        return newValidList
    
    
    def getSortedMnemonicList(self):
        """getSortedMnemonicList returns a list of mnemonics sorted based on their
        order in parmCodes.txt
        """
        mnemList = list(self._mnemDict.keys())
        mnemList.sort(key=self._mnemSorter)
        return(mnemList)


    def getParmType(self, mnemonic):
        """ getParmType returns 1 if mnemonic is a standard parameter, 0 if an error parameter, or -1 if not found.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer in string form)

        Returns: 1 if mnemonic is a standard parameter, 0 if an error parameter, or -1 if not found

        Affects: none

        Exceptions: If non-string passed in.
        """
        try:
            code = int(mnemonic)
            if code > 0 and code in list(self._codeDict.keys()):
                return(1)
            elif code < 0 and abs(code) in list(self._codeDict.keys()):
                return(0)
            else:
                return(-1)
        except ValueError:
            if mnemonic.upper() in self._mnemDict:
                return(1)
            elif mnemonic.upper()[0] == 'D' and mnemonic[1:].upper() in self._mnemDict:
                return(0)
            else:
                return(-1)


    def getParmScaleFactor(self, mnemonic):
        """ getParmScaleFactor returns scale factor as double of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: scale factor as double of given mnemonic

        Affects: none

        Exceptions: If mnemonic not found.
        
        With Madrigal 3.0, this method is now deprecated
        """
        warnings.warn("deprecated", DeprecationWarning)
        return(0.0)


   
    def getParmCodeFromMnemonic(self, mnemonic):
        """ getParmCodeFromMnemonic converts a string to the cedar code (integer).
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer in string form)

        Returns: integer (cedar code)

        Affects: none

        Exceptions: MadrigalError thrown if code not found.
        """
        # if its an integer in string form, return the integer
        try:
            retValue = int(mnemonic)
            if retValue in self._dupCodes:
                retValue = self._getMainCodeFromDuplicateCode(retValue)
            return retValue
        except ValueError:
            pass
        
        mnem = mnemonic.upper()
        if mnem in self._dupMnems:
            mnem = self._getMainMnemFromDuplicateMnem(mnem)
        if mnem in list(self._mnemDict.keys()):
            return(self._mnemDict[mnem].code)
        elif mnem[0] == 'D' and mnem[1:] in self._mnemDict:
            return(-1 * self._mnemDict[mnem[1:]].code)
        elif mnem[:8] == 'UNKNOWN_':
            return(int(mnem[8:]))
        else:
            raise ValueError('Mnemonic: ' + str(mnemonic) + ' not a legal mnemonic.')



    def getParmCategory(self, parm):
        """ getParmCategory returns a category (String) given a cedar parameter (integer or mnemonic string).
        
        Inputs: a cedar code (integer)

        Returns: a category string, or empty string if not found

        Affects: none

        Exceptions: none
        """
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        if parm.upper() in self._mnemDict:
            catIndex = self._mnemDict[parm.upper()].categoryId
        elif parm.upper()[0] == 'D' and parm[1:].upper() in self._mnemDict:
            catIndex = self._mnemDict[parm[1:].upper()].categoryId
        else:
            return('')
        
        result = self._madCatObj.getCategoryDesc(catIndex)
        if not result is None:
            return(result)
        else:
            return('')
        


    def hasHtmlDesc(self, parm):
        """ hasHtmlDesc returns 1 if that parameter has a html description in parmDesc.html.
        
        Inputs: a Madrigal mnemonic (string) or parameter

        Returns: 1 if that parameter has a html description in parmDesc.html, 0 if not or not found

        Affects: none

        Exceptions: 
        """
        mnem = self.getParmMnemonic(parm)
        if mnem in self._mnemDict:
            return(self._mnemDict[mnem].hasDescription)
        elif mnem[0] == 'D' and mnem[1:] in self._mnemDict:
            return(self._mnemDict[mnem[1:]].hasErrDescription)
        else:
            return(0)


    def getParmDescription(self, parm):
        """ getParmDescription returns a description including units and possible links (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: a description string including units and possible links

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
        
        if parm.upper() in self._mnemDict:
            retValue = '%s - %s' % (self._mnemDict[parm.upper()].description,
                                    self._mnemDict[parm.upper()].units)
        elif parm.upper()[0] == 'D' and parm[1:].upper() in self._mnemDict:
            retValue = 'Error in %s - %s' % (self._mnemDict[parm[1:].upper()].description,
                                             self._mnemDict[parm[1:].upper()].units)
        else:
            retValue = ''

        # append link to parmDesc.html if exists
        if self.hasHtmlDesc(parm):
            retValue += ' %s/docs/name/parmDesc.html#' % (self._madDB.getTopLevelUrl()) + parm
            
        return(retValue)


    def getSimpleParmDescription(self, parm):
        """ getSimpleParmDescription returns a description without units or links (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: a description string without units or links

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        if parm.upper() in self._mnemDict:
            retValue = self._mnemDict[parm.upper()].description
        elif parm.upper()[0] == 'D' and parm[1:].upper() in self._mnemDict:
            retValue = 'Error in ' + self._mnemDict[parm[1:].upper()].description
        else:
            retValue = ''

        return(retValue)


    def getParmUnits(self, parm):
        """ getParmUnits returns units  (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: units  (String)

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        if parm.upper() in self._mnemDict:
            retValue = self._mnemDict[parm.upper()].units
        elif parm.upper()[0] == 'D' and parm[1:].upper() in self._mnemDict:
            retValue = self._mnemDict[parm[1:].upper()].units
        else:
            retValue = ''

        return(retValue)



        

    def isError(self, parm):
        """isError returns True if parm is an error parameter, False otherwise
        
        Inputs: a parameter (integer or mnemonic)

        Returns: True if parm is an error parameter, False otherwise

        Affects: none

        Exceptions: none
        """
        # convert to code
        type = self.getParmType(parm)

        if type == 0:
            return(True)
        else:
            return(False)
        
        
    def getParmFormat(self, mnemonic):
        """ getParmFormat returns format string from parcods.tab of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: format string from parmCodes.txt of given mnemonic, or default of '%9.2f'

        Affects: none

        Exceptions: If mnemonic not found.
        """
        # make sure its a mnemonic
        parm = self.getParmMnemonic(mnemonic)
        
        if parm.upper() in self._mnemDict:
            retValue = self._mnemDict[parm.upper()].format
        elif parm.upper()[0] == 'D' and parm[1:].upper() in self._mnemDict:
            retValue = self._mnemDict[parm[1:].upper()].format
        else:
            retValue = '%9.2f'

        return(retValue)
    
    
    def isInteger(self, mnemonic):
        """isInteger returns True if parameter is an integer as determined by the format string.
        """
        format = self.getParmFormat(mnemonic)
        if len(format) == 0:
            return(False) # unknown is not integer
        if format[-1] in ('i',):
            return(True)
        else:
            return(False)
        
        
    def isString(self, mnemonic):
        """isString returns True if parameter is a string as determined by the format string.
        """
        format = self.getParmFormat(mnemonic)
        if len(format) == 0:
            return(False) # unknown is not string
        if format[-1] in ('s','S'):
            return(True)
        else:
            return(False)
        
        
    def getStringLen(self, mnemonic):
        """getStringLen returns the number of characters in the string data
        
        Expects forma in form %<num>s, and returns num
        """
        if not self.isString(mnemonic):
            raise ValueError('Cannot call getStringLen for non-string parameter %s' % (str(mnemonic)))
        format = self.getParmFormat(mnemonic)
        return(int(format[1:-1]))
        
    
    
    def getParmWidth(self, mnemonic):
        """ getParmWidth returns format string from parmCodes.txt of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: format string from parcods.tab of given mnemonic, or default 9 if not found

        Affects: none

        Exceptions: If mnemonic not found.
        """
        # make sure its a mnemonic
        parm = self.getParmMnemonic(mnemonic)
        
        if parm.upper() in self._mnemDict:
            retValue = self._mnemDict[parm.upper()].width
        elif parm.upper()[0] == 'D' and parm[1:].upper() in self._mnemDict:
            retValue = self._mnemDict[parm[1:].upper()].width
        else:
            retValue = 9

        return(retValue)
    
    

    def getParmDescriptionList(self, parmList):
        """ getParmDescriptionList returns a list of descriptions (String) given a list of parameters (integer or mnemonic).
        
        Inputs: a list of parameters (integer or mnemonic)

        Returns: a list of descriptions (String) given a list of parameters (integer or mnemonic).  

        Affects: none

        Exceptions: none
        """

        returnList = []

        for parm in parmList:
            returnList.append(self.getParmDescription(parm).strip())

        return(returnList)


    def getParmMnemonic(self, code):
        """ getParmMnemonic returns a mnemonic (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter: integer, an integer in string form, or a mnemonic string

        Returns: a mnemonic string.  If integer not found, returns integer in string form.

        Affects: none

        Exceptions: none
        """
        if type(code) in [str, bytes]:
            # try to convert to an integer, if can't, assume its already a mnemonic
            try:
                intCode = int(code)
                code = intCode
                if code > 0:
                    if code in self._dupCodes:
                        code = self._getMainCodeFromDuplicateCode(code)
                elif code * -1 in self._dupCodes:
                    code = self._getMainCodeFromDuplicateCode(code*-1) * -1
            except:
                code = code.upper()
                if self.getParmType(code) == 1:
                    if code in self._dupMnems:
                        code = self._getMainMnemFromDuplicateMnem(code)
                elif self.getParmType(code) == 0:
                    if code[1:] in self._dupMnems:
                        code = 'D' + self._getMainMnemFromDuplicateMnem(code[1:])
                return(code)
        elif type(code) in [str, bytes]:
                        # try to convert to an integer, if can't, assume its already a mnemonic
            try:
                intCode = int(code)
                code = intCode
                if code > 0:
                    if code in self._dupCodes:
                        code = self._getMainCodeFromDuplicateCode(code)
                elif code * -1 in self._dupCodes:
                    code = self._getMainCodeFromDuplicateCode(code*-1) * -1
            except:
                code = code.upper()
                if self.getParmType(code) == 1:
                    if code in self._dupMnems:
                        code = self._getMainMnemFromDuplicateMnem(code)
                elif self.getParmType(code) == 0:
                    if code[1:] in self._dupMnems:
                        code = 'D' + self._getMainMnemFromDuplicateMnem(code[1:])
                return(code)
        elif code > 0:
            if code in self._dupCodes:
                code = self._getMainCodeFromDuplicateCode(code)
        elif code * -1 in self._dupCodes:
            code = self._getMainCodeFromDuplicateCode(code*-1) * -1

        # now it should already be an integer
        # if unknown code, return int as string
        if code > 0 and code in list(self._codeDict.keys()):
            return(self._codeDict[code].mnemonic)
        elif code < 0 and abs(code) in list(self._codeDict.keys()):
            return('D' + self._codeDict[abs(code)].mnemonic)
        else:
            return(str(code))
        
        
    def getParmIndex(self, mnemonic):
        """ getParmIndex returns a index (float) given a parameter (integer or mnemonic) representing position in the
        parmCodes.txt file.  Error parameters are std parm + 0.5
        
        Inputs: a parameter: integer, an integer in string form, or a mnemonic string

        Returns: a index (float) given a parameter (integer or mnemonic) representing position.  If parameter not
        found, returns 1000000 + code + 0.5 if error

        Affects: none

        Exceptions: None
        """
        mnem = self.getParmMnemonic(mnemonic)
        
        try:
            code = int(mnem)
            if code < 0:
                return(1000000.0 + code + 0.5)
            else:
                return(1000000.0 + code)
        except ValueError:
            pass

        if not self.isError(mnem):
            return(float(self._mnemDict[mnem].position))
        else:
            return(float(self._mnemDict[mnem[1:]].position) + 0.5)


    def getParmMnemonicList(self, codeList):
        """ getParmMnemonicList returns a list of upper case mnemonics (String) given a list of cedar codes (integer, integer as string, or mnemonic string).
        
        Inputs: a list of cedar codes (integer, integer as string, or mnemonic string)

        Returns: a list of upper case mnemonics (String) given a list of cedar codes (integer).  If illegal value,
        returns str(code) for that item

        Affects: none

        Exceptions: none
        """

        returnList = []

        for code in codeList:
            mnemonic = self.getParmMnemonic(code).strip()
            if mnemonic == 'Illegal Parameter Code':
                mnemonic = str(code)
            returnList.append(mnemonic.upper())


        return returnList
    

    def normalizeParmList(self, parmList):
        """ normalizeParmList returns an ordered list of parameters with all mnemonics changed to integers.
        
        Inputs: parmList - the list of parameters (integers or mnemonics) to convert

        Returns:  a new parmList that is ordered (negitive values are placed directly after the same positive values)
        and all parameters are converted to integers

        Affects: None

        Exceptions: none
        """

        #create a copy of the parmList
        newParmList = copy.deepcopy(parmList)

        # convert all parameters to integers
        itemCount = 0
        for parm in newParmList:
            if type(parm) in (bytes, str):
                # check if its already an integer
                try:
                    newParm = int(parm)
                    newParmList[itemCount] = newParm
                except:
                    # must be a mnemonic
                    newParmList[itemCount] = self.getParmCodeFromMnemonic(parm)

            # otherwise it must already be an integer
            else:
                newParmList[itemCount] = parm
                    
            itemCount = itemCount + 1

        #put in correct order
        return self._sortList(newParmList)


    def getMadCategoryIndex(self, category):
        """ getMadCategoryIndex returns the index (id) of a given category.
        
        Inputs: a Madrigal category (string)

        Returns: an integer representing the index (order).  Returns -32767
                 if not found.

        Affects: none

        Exceptions: none
        """
        catList = self._madCatObj.getCategoryList()
        
        for catDesc, catId in catList:
            if catDesc == category:
                return(catId)
        return(-32767)


    def getCategoryDict(self, parmList):
        """ getCategoryDict returns a python dict with key = category index, item = category name and ordered parameters        
        Inputs: parmList - the list of parameters (integers or mnemonics) 

        Returns:  a python dict, with key = category index. Each item is a list of two items.
        The first item is the category name (string).  The second item is a list of parameter mnemonics
        from parmList belonging in that category.  Ordering is alphabetical, except that an error parameter
        immediately follows its non-error parameter.

        Affects: None

        Exceptions: none
        """
        catDict = {}

        # create a parm List in mnemonic form
        mnemParmList = self.getParmMnemonicList(parmList)

        for parm in mnemParmList:
            category = self.getParmCategory(parm)
            catId = self.getMadCategoryIndex(category)

            # check if category already included
            try:
                if not parm in catDict[catId][1]:
                    catDict[catId][1].append(parm)
                    
            except KeyError:
                # create new item
                catDict[catId] = [category, [parm]]

        # now order all parm lists
        for id in list(catDict.keys()):
            self.orderParms(catDict[id][1])

        
        return catDict


    def orderParms(self, parmList):
        """ orderParms sorts mnemonic parameters based on order in parcods.tab file.
        
        Input: parmList - a list of parameter mnemonics

        Error parameters directly follow standard parameter.
        """
        parmList.sort(key=self._mnemSorter)

   

    def getMnemonicListFromExpression(self, expressionStr):
        """ getMnemonicListFromExpression returns a list of unique cedar mnemonics in a python logical expression.
        
        Inputs: expressionStr - a string containing a valid python logical expression with cedar mnemonics as
        variables.  Expression can contain any python logical operator (and, or, not, ==, <, >, <=, >=, !=), any
        operator, any number, and valid python math function, and any variable that is a valid cedar mnemonic.  A substring
        is assumed to be a cedar mnemonic if it begins with a letter, contains only alphanumerics, period, plus, or
        underscore, and is not immediately followed by a open parenthesis. Each potential cedar mnemonic is
        verified, and an exception is thrown if it is not valid.  The validity of the entire expression is then
        verified by replacing all the valid cedar mnemonics by "1.0" and executing the resulting expression.  If any
        exception besides divide by zero or value error occurs, an exception is thrown.  Otherwise, the list of cedar
        mnemonics found is returned.

        Returns:  a list of unique cedar mnemonics (upper case).

        Affects: None

        Exceptions: Error thrown if any non-valid mnemonic found, or if expression throws an exception when run
        (except divide by zero or value error).
        """

        # convert expressionStr to lower case, as required for functions
        expressionStr = expressionStr.lower()

        # list of found mnemonics
        foundMnemonics = []

        # create a test string with all mnemonics replaced by "1.0"
        testStr = ''

        # search expressionStr for possible mnemonics, and
        # fill testStr
        inMnemonic = 0
        thisMnemonic = ''
        for char in expressionStr + ' ': 
            # see if a new mnemonic might have begun
            if inMnemonic == 0 and char.isalpha():
                inMnemonic = 1
                thisMnemonic = char
            # else see if a new mnemonic is done
            elif inMnemonic == 1 and ((not char.isalnum())  and char != '.' and char != '_' and char != '+'):
                inMnemonic = 0
                # if char is '(', its a function - ignore it
                if char == '(':
                    testStr = testStr + thisMnemonic + char
                    continue
                # if its just a logical operator or 'e', ignore it
                if thisMnemonic in ('and', 'or', 'not', 'e'):
                    testStr = testStr + thisMnemonic + char
                    continue
                else:
                    # verify its a valid mnemonic
                    try:
                        self.getParmCodeFromMnemonic(thisMnemonic)
                        # now convert to upper case
                        thisMnemonic = thisMnemonic.upper()
                        # append it if its unique
                        if thisMnemonic not in foundMnemonics:
                            foundMnemonics.append(thisMnemonic)
                        testStr = testStr + '1.0' + char
                    except:
                        # not valid - throw an error
                        raise ValueError('The expression ' + expressionStr + \
                              ' contains an illegal mnemonic: ' + thisMnemonic)
            # else see if its another char to add to the present mnemonic
            elif inMnemonic == 1:
                thisMnemonic = thisMnemonic + char
            else:
                testStr = testStr + char
                continue

        # now try to evaluate testStr to see if its a reasonable logical expression
        try:
            obj = eval(testStr)
            
        except ZeroDivisionError:
            # this is not a problem
            pass
        
        except ValueError:
            # this is not a problem
            pass
        
        except:
            # some other error occurred - this is a problem
            raise ValueError('The expression "' + expressionStr + '" contains an error: ' + str(sys.exc_info()[1]))


        return foundMnemonics


    def getStdExpression(self, expressionStr):
        """ getStdExpression returns an expression in standard form (upper case mnemonic, all else lower case).
        
        Inputs: expressionStr - a string containing a valid python logical expression with cedar mnemonics as
        variables.  Expression can contain any python logical operator (and, or, not, ==, <, >, <=, >=, !=), any
        operator, any number, and valid python math function, and any variable that is a valid cedar mnemonic.  A substring
        is assumed to be a cedar mnemonic if it begins with a letter, contains only alphanumerics, period, plus, or
        underscore, and is not immediately followed by a open parenthesis. Each potential cedar mnemonic is
        verified, and an exception is thrown if it is not valid.  

        Returns:  an expression (string) in standard form (upper case mnemonic, all else lower case).

        Affects: None

        Exceptions: Error thrown if any non-valid mnemonic found.
        """

        # convert expressionStr to lower case, as required for functions
        expressionStr = expressionStr.lower()

        # create a new stdExpStr to be returned 
        stdExpStr = ''

        # search expressionStr for possible mnemonics
        inMnemonic = 0
        thisMnemonic = ''
        for char in expressionStr + ' ': 
            # see if a new mnemonic might have begun
            if inMnemonic == 0 and char.isalpha():
                inMnemonic = 1
                thisMnemonic = char
            # else see if a new mnemonic is done
            elif inMnemonic == 1 and ((not char.isalnum())  and char != '.' and char != '_' and char != '+'):
                inMnemonic = 0
                # if char is '(', its a function - ignore it
                if char == '(':
                    stdExpStr = stdExpStr + thisMnemonic + char
                    continue
                # if its just a logical operator or 'e', ignore it
                if thisMnemonic in ('and', 'or', 'not', 'e'):
                    stdExpStr =  stdExpStr + thisMnemonic + char
                    continue
                else:
                    # verify its a valid mnemonic
                    try:
                        self.getParmCodeFromMnemonic(thisMnemonic)
                        # now convert to upper case
                        thisMnemonic = thisMnemonic.upper()
                        stdExpStr = stdExpStr + thisMnemonic + char
                    except:
                        # not valid - throw an error
                        raise ValueError('The expression ' + expressionStr + \
                              ' contains an illegal mnemonic: ' + thisMnemonic)
            # else see if its another char to add to the present mnemonic
            elif inMnemonic == 1:
                thisMnemonic = thisMnemonic + char
            else:
                stdExpStr = stdExpStr + char
                continue

        return stdExpStr


                        
    def getParametersForInstruments(self, instrumentList, parmListName='Comprehensive'):
        """getParametersForInstruments returns a list of unique Madrigal mnemonics associated with a list of instruments.

        This method's purpose is to return a list of parameters appropriate for a user to select from
        given that a certain list of instruments is under consideration.  This method will return a list
        of all measured parameters found in data files associated with those instruments, and also all
        parameters in the parmNameList that can be derived from those measured parameters.  The passed
        in parmNameList must be a valid name of a parameter list found in the madrigal.ui.web.MadrigalWebFormat
        class, and defaults to the "Comprehensive" list of parameters used in the madDataBrowse web page.
        
        Inputs: instrumentList - a python list on instruments as integers (kinst values).
        
                parmListName - a name (string) of a list of parameters in the MadrigalWebFormat class.
                Defaults to "Comprehensive"

        Returns:  an ordered list of unique Madrigal mnemonics.

        Affects: None

        Exceptions: None.
        """

        # list of found mnemonics
        foundMnemonics = []

        # create MadrigalInstrumentParameters object
        instParmsObj = madrigal.metadata.MadrigalInstrumentParameters(self._madDB)

        # create MadrigalWebFormat object
        webFormatObj = madrigal.ui.web.MadrigalWebFormat()

        # check that passed-in instrumentList is really a list
        if type(instrumentList) == int:
            instrumentList = [instrumentList]
        elif instrumentList == None:
            instrumentList = []

        # loop through each instrument
        for inst in instrumentList:
            # get measured parameters associated with that instrument
            measParms = instParmsObj.getParameters(inst)
            # if None, skip instrument
            if measParms == None:
                continue
            # if unique, add them to foundMnemonics
            for parm in measParms:
                if not parm.upper() in foundMnemonics:
                    foundMnemonics.append(parm.upper())

        # now add whatever parameters can be derived from these measured parameters
        preferedParmList = webFormatObj.getFormat(parmListName)

        derivableParmList = madrigal.derivation.getDerivableParms(foundMnemonics)


        # add all unique, allowed parameters from derivedParmList
        for item in derivableParmList:
            
            # add if unique and in preferedParmList
            if not item in foundMnemonics:
                if not item.lower() in preferedParmList:
                    continue
                foundMnemonics.append(item)
            

        return foundMnemonics


    def getIsprintHeader(self, mnemonicList):
        """ getIsprintHeader returns a string with mnemonics as it would appear at the top of isprint.
        
        Inputs: mnemonic:  a list of Madrigal mnemonics (string or integer)

        Returns: a string with mnemonics as it would appear at the top of isprint

        Affects: none

        Exceptions: If any mnemonic not found.
        """
        retStr = ''

        for mnem in mnemonicList:
            strLength = self.getParmWidth(self.getParmMnemonic(mnem))
            newFormatStr = '%'
            newFormatStr += '%is' % (strLength)
            retStr += newFormatStr % (self.getParmMnemonic(mnem).upper())

        return retStr[4:]
    
    
    def _mnemSorter(self, mnem1):
        """_mnemSorter is a helper function that compares two parameter mnemonics based on their
        order in parmCodes.txt.  Error parameters always directly follow the standard parameter
        """
        return(self.getParmIndex(mnem1))
    
    
    def _getMainCodeFromDuplicateCode(self, duplicate_code):
        """_getMainCodeFromDuplicateCode returns the main code given a duplicate code.
        
        Raises ValueError if no main code found
        """
        parm = self._codeDict[duplicate_code]
        if parm.main_code == None:
            raise ValueError('Code %i is not a duplicate, cannot call _getMainCodeFromDuplicateCode' % (duplicate_code))
        return(parm.main_code)
    
    def _getMainMnemFromDuplicateMnem(self, duplicate_mnem):
        """_getMainMnemFromDuplicateMnem returns the main mnem given a duplicate mnem.
        
        Raises ValueError if no main mnem found
        """
        parm = self._mnemDict[duplicate_mnem]
        if parm.main_code == None:
            raise ValueError('Mnem %s is not a duplicate, cannot call _getMainMnemFromDuplicateMnem' % (duplicate_mnem))
        return(self.getParmMnemonic(parm.main_code))
    
    
    
class _DeprecatedMadrigalParameters:
    """_DeprecatedMadrigalParameters is a private object that provides information about 
    Deprecated Madrigal 2.6 parameters.  Needed only to support converting old Cedar format files
    to Madrigal 3.0 Hdf5 files

    This class provides access to the Cedar/Madrigal standards for parameters
    (such as getMnemonic, getDescription, getCodeFromMnemonic) and
    categories.  It will also examine an expression (string) and return the parameter mnemonics it contains.

    Usage example:

        import madrigal.data.MadrigalParameters
    
        test = madrigal.data.MadrigalParameters()

        parcode = test.getParmCodeFromMnemonic("YEAR")

        print parcode

    Non-standard Python modules used:
    None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 27, 2001
    Added getMnemonicListFromExpression Jul. 16, 2002

    """


    def __init__(self, madDB = None):
        """__init__ initializes MadrigalParameters by getting some basic information from MadrigalDB.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self._binDir.

        Exceptions: None.
        """

        # get metadata dir
        if madDB == None:
            thisMadDB = madrigal.metadata.MadrigalDB()
        else:
            thisMadDB = madDB

        self._binDir = thisMadDB.getBinDir()
        self._madDB  = thisMadDB


    def getParmType(self, mnemonic):
        """ getParmType returns 1 if mnemonic is a standard parameter, 0 if an error parameter, or -1 if not found.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer in string form)

        Returns: 1 if mnemonic is a standard parameter, 0 if an error parameter, or -1 if not found

        Affects: none

        Exceptions: If non-string passed in.
        """
        return madrigal._Madrec.madGetParType(mnemonic)


    def getParmScaleFactor(self, mnemonic):
        """ getParmScaleFactor returns scale factor as double of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: scale factor as double of given mnemonic

        Affects: none

        Exceptions: If mnemonic not found.
        """
        code = self.getParmCodeFromMnemonic(mnemonic)
        return madrigal._Madrec.cedarGetParScaleFactor(code)


   
    def getParmCodeFromMnemonic(self, mnemonic):
        """ getParmCodeFromMnemonic converts a string to the cedar code (integer).
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer in string form)

        Returns: integer (cedar code)

        Affects: none

        Exceptions: MadrigalError thrown if code not found.
        """
        # if its an integer in string form, return the integer
        try:
            retValue = int(mnemonic)
            return retValue
        except:
            pass
        
        retValue = madrigal._Madrec.cedarGetParCodeFromMnemonic(mnemonic)

        if retValue == -1:
            raise madrigal.admin.MadrigalError('Mnemonic: ' + str(mnemonic) + ' not a legal mnemonic.', None)

        return retValue


    def getParmCategory(self, parm):
        """ getParmCategory returns a category (String) given a cedar parameter (integer or mnemonic string).
        
        Inputs: a cedar code (integer)

        Returns: a category string

        Affects: none

        Exceptions: none
        """
        # if its a string form of an integer, convert it to an integer
        if type(parm) in [str, bytes]:
            try:
                int(parm)
                parm = int(parm)
            except ValueError:
                pass
            
        if type(parm) in [str, bytes]:
            retValue = madrigal._Madrec.madGetParMnemType(parm)
        else:
            retValue = madrigal._Madrec.cedarGetParCodeType(parm)

        return retValue


    def getParmDescription(self, parm):
        """ getParmDescription returns a description including units and possible links (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: a description string including units and possible links

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        retValue = madrigal._Madrec.madGetParDescription(parm)

        # append link to parmDesc.html if exists
        if self.hasHtmlDesc(parm):
            retValue += '<br><br>Click <a href=/%s/docs/name/parmDesc.html#' % (self._madDB.getTopLevelUrl()) + parm
            retValue += '>here</a> for a more detailed description of ' + parm + '.'

        return retValue


    def getSimpleParmDescription(self, parm):
        """ getSimpleParmDescription returns a description without units or links (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: a description string without units or links

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        retValue = madrigal._Madrec.madGetSimpleParDescription(parm).strip()

        return retValue


    def getParmUnits(self, parm):
        """ getParmUnits returns units  (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: units  (String)

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        retValue = madrigal._Madrec.madGetParUnits(parm).strip()

        return retValue


    def hasAddIncrement(self, parm):
        """hasAddIncrement returns True if parm has additional increment parameter, False otherwise
        
        Inputs: a parameter (integer or mnemonic)

        Returns: True if parm has additional increment parameter, False otherwise

        Affects: none

        Exceptions: none
        """
        # convert to code
        try:
            code = int(parm)
        except:
            code = self.getParmCodeFromMnemonic(parm)

        if code > 0:
            testCode = code + 1 # additional increment parameter always one higher
        else:
            testCode = code - 1

        desc = self.getSimpleParmDescription(testCode)

        if desc.lower().find('additional increment') != -1:
            return(True)
        else:
            return(False)


    def isAddIncrement(self, parm):
        """isAddIncrement returns True if parm is an additional increment parameter, False otherwise
        
        Inputs: a parameter (integer or mnemonic)

        Returns: True if parm is an additional increment parameter, False otherwise

        Affects: none

        Exceptions: none
        """
        # convert to code
        try:
            code = int(parm)
        except:
            code = self.getParmCodeFromMnemonic(parm)

        desc = self.getSimpleParmDescription(code)

        if desc.lower().find('additional increment') != -1:
            return(True)
        else:
            return(False)

    def isError(self, parm):
        """isError returns True if parm is an error parameter, False otherwise
        
        Inputs: a parameter (integer or mnemonic)

        Returns: True if parm is an error parameter, False otherwise

        Affects: none

        Exceptions: none
        """
        # convert to code
        try:
            code = int(parm)
        except:
            code = self.getParmCodeFromMnemonic(parm)

        if code < 0:
            return(True)
        else:
            return(False)
        
        
    def getParmFormat(self, mnemonic):
        """ getParmFormat returns format string from parcods.tab of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: format string from parcods.tab of given mnemonic

        Affects: none

        Exceptions: If mnemonic not found.
        """
        # make sure its a mnemonic
        mnemonic = self.getParmMnemonic(mnemonic)
        return madrigal._Madrec.madGetParFormat(mnemonic)
    
    
    def isInteger(self, mnemonic):
        """isInteger returns True if parameter is an integer as determined by the format string.
        """
        try:
            for item in ('i', 'c'):
                if self.getParmFormat(mnemonic).find(item) != -1:
                    return(True)
            return(False)
        except:
            return(False)


    def getParmMnemonic(self, code):
        """ getParmMnemonic returns a mnemonic (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter: integer, an integer in string form, or a mnemonic string

        Returns: a mnemonic string.  If integer not found, returns integer in string form.

        Affects: none

        Exceptions: none
        """
        if type(code) in [str, bytes]:
            # try to convert to an integer, if can't, assume its already a mnemonic
            try:
                intCode = int(code)
                code = intCode
            except:
                return code.upper()

        # otherwise it should already be an integer
        # if unknown code, return int as string
        try:
            retValue = madrigal._Madrec.cedarGetParMnemonic(code)
        except:
            retValue = str(code)

        return retValue
    
    
    def getParmMnemonicList(self, codeList):
        """ getParmMnemonicList returns a list of upper case mnemonics (String) given a list of cedar codes (integer, integer as string, or mnemonic string).
        
        Inputs: a list of cedar codes (integer, integer as string, or mnemonic string)

        Returns: a list of upper case mnemonics (String) given a list of cedar codes (integer).  If illegal value,
        returns str(code) for that item

        Affects: none

        Exceptions: none
        """

        returnList = []

        for code in codeList:
            mnemonic = self.getParmMnemonic(code).strip()
            if mnemonic == 'Illegal Parameter Code':
                mnemonic = str(code)
            returnList.append(mnemonic.upper())

        return(returnList)


    
if __name__ == '__main__':  
    os.environ.setdefault('MADROOT','/usr/local/madrigal')
    try:
        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'
        #filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mlh980120g.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print('File analysis took ' + str(t2-t1) + ' seconds.')
        
        print(test.toString())

        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1995/jro/01feb95/jic950201g.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print('File analysis took ' + str(t2-t1) + ' seconds.')

        print(test.toString())

        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1997/aro/06jan97/are970106g.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print('File analysis took ' + str(t2-t1) + ' seconds.')

        print(test.toString())

#        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1990/mui/23apr90/mui900423a.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print('File analysis took ' + str(t2-t1) + ' seconds.')

        print(test.toString())

        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []

        t1 = time.time()


        test.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Comprehensive'),
                                      measParmList,
                                      derivedParmList,
                                      allParmList,
                                      sureParmList)
        t2 = time.time()

        print('Parameter analysis took ' + str(t2-t1) + ' seconds.')

        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []
        
        t1 = time.time()


        test.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Comprehensive'),
                                      measParmList,
                                      derivedParmList,
                                      allParmList,
                                      sureParmList)
        t2 = time.time()

        print('Parameter analysis without verification of derived parameters took ' + str(t2-t1) + ' seconds.')
        
        #print lists
        print('Measured parms are: ' + str(measParmList))
        print('Derived parms are: ' + str(derivedParmList))
        print('All good parms are: ' + str(allParmList))
        print('Sure parms are: ' + str(sureParmList))
    
        # test hdf5 file conversion
        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1995/jro/01feb95/jic950201g.001'
        outputfile = "/tmp/jic950201g.hdf5"
        
        test = MadrigalFile(filepath)
        
        t1 = time.time()
        
        test.exportToHdf(outputfile, independentSpatialParms=['range'], extraParameters=['ut1'])

        t2 = time.time()

        print('File conversion took ' + str(t2-t1) + ' seconds.')
        
    except madrigal.admin.MadrigalError as e:

        print(e.getExceptionStr())

    sys.exit()
    #test MadrigalParameters
    madDB = madrigal.metadata.MadrigalDB()
    
    print('madDB loaded')

    test = MadrigalParameters(madDB)

    paramList = ['YEAR', 20, 21, 34, 'gdalt', 120, 121, 125, 126, 130, 132, 133, 140, -120, 'dgdalt', 142, 143, 160, 170, 204, 206, 208,
                 210, 213, 216, 218, 220, 222,  226, -121, 246, 310,
                 340, 354, 356, 402, 411, 420, 430, 461, 482, 483, 505]

    print(str(test.getCategoryDict(allParmList)))

    shortParmList = [120, 125, 126]

    strFile = os.environ.get('MADROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'


    print('Cedar code for diplat is:' + str(test.getParmCodeFromMnemonic('diplat')))

    print('Cedar scale factor for diplat is:' + str(test.getParmScaleFactor('diplat')))

    try:
        
        print('Cedar code for BillR is:' + str(test.getParmCodeFromMnemonic('BillR')))

    except madrigal.admin.MadrigalError as e:
        
        print(e.getExceptionStr())
	
    print('Mnemonic for 75 is: ' + test.getParmMnemonic(75))

    print('Mnemonic for 110 is: ' + test.getParmMnemonic(110))

    print('Mnemonic list for [110, 120] is: ' + str(test.getParmMnemonicList([110, 120])))

    print('Description for 110 is: ' + test.getParmDescription(110))

    print('Description list for [110, 120] is: ' + str(test.getParmDescriptionList([110, 120])))

    print('Converting GDALT POPL DPOPL TI DTI TE DTE VO DVO to int string:')

    strList = 'GDALT POPL DPOPL TI DTI TE DTE VO DVO'.split()

    strInt = ''
    for mnemStr in strList:
        parmInt = test.getParmCodeFromMnemonic(mnemStr)
        strInt = strInt + str(parmInt) + ' '

    print(strInt)
    
    # test getMnemonicListFromExpression
    print('Mnemonics found in "po+ <= 1000 and range != log10(f10.7) and range > 1e-10" are:')
    print(test.getMnemonicListFromExpression('po+ <= 1000 and range != log10(f10.7) and range > 1e-10'))
    print('Standard expression is: ')
    print(test.getStdExpression('po+ <= 1000 and range != log10(f10.7) and range > 1e-10'))

    print(test.getMnemonicListFromExpression('1<Kp<6'))

    # test getParametersForInstruments
    print('The following parameters are appropriate for instruments 30 and 80:')
    print(test.getParametersForInstruments([80, 30]))

    # test getParmCategory
    print(test.getParmCategory(9))
    print(test.getParmCategory('BYear'))

    # test hasHtmlDesc
    print("Does ut1 have an html description = " + str(test.hasHtmlDesc('uT1')))

    # test getParmType
    print("Type of 1234 = " + str(test.getParmType('1234')))
    print("Type of -1234 = " + str(test.getParmType('-1234')))
    print("Type of gdalt = " + str(test.getParmType('gdalt')))
    print("Type of dgdalt = " + str(test.getParmType('dgdalt')))
    print("Type of dgdalt2 = " + str(test.getParmType('dgdalt2')))

    # test getSimpleParmDescription and getParmUnits
    print('The follow are the simple description and units for ti:')
    print('"%s"' % (test.getSimpleParmDescription('ti')))
    print('"%s"' % (test.getParmUnits('ti')))