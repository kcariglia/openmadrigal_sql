# -*- coding: utf-8 -*-

"""The metadata module provides access to all metadata about one particular madrigal database.

This metadata is presently read from the files in the metadata directory, and from the
madrigal.cfg file.  If that file cannot be found, hard-coded values set during installation
are used instead.  If the madrigal.cfg file is found at the location specified by either
the madroot enviroment variable or at the hard-coded value of madroot set at installation, then
the parameters are read from the madrigal.cfg file.  Note that madroot with caps is only written
once in this file before installation, since it will be automatically replaced, so it is referred
to by MAD_ROOT or MAD+ROOT.

$Id: metadata_original.py 7655 2024-06-27 20:20:49Z kcariglia $
"""
# standard python imports
import io
import configparser
import os, sys, traceback
import os.path
import shutil
import time
import datetime
import calendar
import fnmatch
import types
import re
import random
import copy
import glob
import packaging.version
import bisect

# third party imports
import numpy
import h5py
import filelock

# Madrigal imports
import madrigal.admin
import madrigal._derive


# helper functions
def getMadrigalUTFromDT(dt):
    """getMadrigalUTFromDT returns the number of seconds since midnight UT 1950-01-01 
    for datetime dt
    """
    t1950 = 631152000.0 # offset from Unix time
    return(calendar.timegm(dt.timetuple()) + t1950)

def getMadrigalUTFromDate(year, month, day, hour, minute, second, microsecond):
    """getMadrigalUTFromDate returns the number of seconds since midnight UT 1950-01-01 
    for date specified in arguments
    """
    dt = datetime.datetime(year, month, day, hour, minute, second, microsecond)
    return(getMadrigalUTFromDT(dt))

def getUnixUTFromDT(dt):
    """getUnixUTFromDT returns the number of float seconds since midnight UT 1970-01-01 
    for datetime dt
    """
    return(calendar.timegm(dt.timetuple()) + dt.microsecond/1.0E6)

def getUnixUTFromDate(year, month, day, hour, minute, second, microsecond):
    """getUnixUTFromDate returns the number of seconds since midnight UT 1970-01-01 
    for date specified in arguments
    """
    dt = datetime.datetime(year, month, day, hour, minute, second, microsecond)
    return(getUnixUTFromDT(dt))


class MadrigalDB:
    """MadrigalDB is an object that provides access to an entire Madrigal database.

    This object provides complete high-level access to an entire Madrigal database.
    Presently, all its information comes from madrigal.cfg, or another path passed in by the
    user.  If env variable madroot is not set, or if madrigal.cfg cannot be opened, the
    default values automatically editted during installation are used

    Usage example::

        import madrigal.metadata
    
        try:
        
            test =  madrigal.metadata.MadrigalDB()
            
        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()
            
        else:
        
            print test.toString()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1.  If the constructor is called with an configuration file path as an argument, and that file cannot be
            read.
        
        2.  If the configuration file cannot be parsed by ConfigParser.
    
        3.  If any of the following keys cannot be found in the configuration file:
    
            *__MAD_SERVER

            *__MAD_SERVERROOT
            
            *__SITE_ID

            *__HTML_STYLE

            *__INDEX_HEAD

            *__CON_TACTLINK


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Oct. 4, 2001

    """

    # lines that are edited during installation
    __hardCodeMadRoot          = 'MADROOT'
    __hardCodeMadServer        = 'MADSERVER'
    __hardCodeMadServerRoot    = 'MADSERVERROOT'
    __hardCodeSiteId           = 'SITEID'
    __hardCodeHtmlStyle        = 'HTMLSTYLE'
    __hardCodeIndexHead        = 'INDEXHEAD'
    __hardCodeContact          = 'CONTACT'
    __hardCodeMailServer       = 'MAILSERVER'

    #constants
    __webProtocol  = "http"
    """ Change the above string to use another web protocol such as https. """

    __binDir       = "/bin"
    """ Sets the relative path from madrigal root to the bin directory. """

    __metadataDir  = "/metadata"
    """ Sets the relative path from madrigal root to the metadata directory. """

    __experimentDir  = "/experiments"
    """ Sets the relative path from madrigal root to the experiment directory. """

    __MAD_ROOT      = "MAD" + "ROOT"
    """ Sets the name of the environment variable that points to the madrigal root directory. """

    __confFileName = "madrigal.cfg"
    """ Sets the name of the default madrigal configuration file. """

    __MAD_SERVER    = "MAD" + "SERVER"
    """ Sets the key name in the configuration file to find the main madrigal url. """

    __MAD_SERVERROOT    = "MAD" + "SERVERROOT"
    """ Sets the key name in the configuration file to find the top level directory. """

    __SITE_ID       = "SITE" + "ID"
    """ Sets the key name in the configuration file to find the site id. """

    __HTML_STYLE   = "HTML" + "STYLE"
    """ Sets the key name in the configuration file to find the html body style tag. """

    __INDEX_HEAD   = "INDEX" + "HEAD"
    """ Sets the key name in the configuration file to find the heading in the top level madrigal page. """

    __CON_TACTLINK   = "CON" + "TACT"
    """ Sets the key name in the configuration file to find the contact link line. """

    __MAIL_SERVER   = "MAIL" + "SERVER"
    """ Sets the key name in the configuration file to find the mailserver. """

    __PYTHON_EXE   = "PYTHON" + "EXE"
    """ Sets the key name in the configuration file to find the python executable. """
    
    __PLOT_BUTTON_LABEL = "PLOT" + "BUTTON" + "LABEL"
    """Set the label on the plot/docs button to something different if desired """


    def __init__(self, initFile=None):
        """__init__ initializes the MadrigalDB by reading from $MAD_ROOT/madrigal.cfg (or initString).

        Inputs: String representing the full path to the configuration file. Default is None, in
                which case configuration file is $(__MAD_ROOT)/__confFileName (now madrigal.cfg).
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown if error (see class description for specific conditions).

        Notes:

            Given the file $MAD_ROOT/madrigal.cfg (these names are set by constants above), or initFile,
            this constructor goes about loading some basic data about the database, including information such as:
            
            -the database utility directory
            
            -the www home base
            
            Implemented using ConfigParser module.  This reads ini type files.  The only difference between
            ini files and madrigal.cfg is that ini files are broken into sections of the form:
            
            [sectionTitle]
            
            key1 = value1
            
            key2 = value2
            
            Since madrigal.cfg (or another initFile) has no section header, one section head called "madrigal"
            is appended to the beginning of the file.
        """

        # if madroot not set, use hard coded madroot
        self.__madRootDir = os.environ.get(self.__MAD_ROOT)
        if (self.__madRootDir == None):
            self.__madRootDir = self.__hardCodeMadRoot

        # Set configuration file
        if (initFile == None):
            self.__confFilePath = self.__madRootDir + "/" + self.__confFileName
        else:
            self.__confFilePath = initFile

        # open configuration file
        try:
            self.__confFile = open(self.__confFilePath, "r")
            
        except IOError:
            # can't read from file - use all hard-coded values
            self.__initFromHardCode()
            self.__finishInit()
            return
        
        # create Parser using standard module ConfigParser
        self.__parser = configparser.ConfigParser()
        
        # read conf file into a StringIO with "[madrigal]\n" section heading prepended
        strConfFile = io.StringIO("[madrigal]\n" + self.__confFile.read())

        # parse StringIO configuration file
        try:
            self.__parser.read_file(strConfFile)
        except:
            raise madrigal.admin.MadrigalError("Unable to parse configuration file " + self.__confFilePath,
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # read information from configuration file
        self.__readConfFile()

        # close conf file
        self.__confFile.close()

        self.__finishInit()

        # end __init__


    def __finishInit(self):
        """__finishInit is a private helper function that finishes initialization by combining attributes to form new ones.
        
        Inputs: None
        
        Returns: Void.

        Affects: Initializes class member variables that are combinations of existing ones.

        Exceptions: None.
        """
            
        # combine information 
        # Set the database utility directory
        self.__databaseUtilityDirectory = self.__madRootDir + self.__binDir

        # Set the wwwHomeBase
        if self.__mainUrl.find('http') == -1:
            self.__wwwHomeBase = self.__webProtocol + "://" + self.__mainUrl
        else:
            self.__wwwHomeBase = self.__mainUrl

        # Set the top level url
        if self.__topLevel.strip() not in ('', '.'):
            self.__topLevelUrl = self.__wwwHomeBase + '/' + self.__topLevel
        else:
            self.__topLevelUrl = self.__wwwHomeBase

        # Set contactEmail by stripping ContactLink line
        # Look for colon at end of Mailto:
        begIndexEmail = self.__contactlink.find(':')
        # look for "> at end of email address
        endIndexEmail = self.__contactlink.find('">')
        # if not found, try '>
        if endIndexEmail == -1:
            endIndexEmail = self.__contactlink.find('\'>')
        # if still not found, try > alone
        if endIndexEmail == -1:
            endIndexEmail = self.__contactlink.find('>')
        #check that both were found
        if begIndexEmail != -1 and endIndexEmail != -1:
            self.__contactEmail = self.__contactlink[begIndexEmail + 1 : endIndexEmail]
            if not self.__isValidEmail(self.__contactEmail):
                self.__contactEmail = None
        elif self.__isValidEmail(self.__contactlink):
            # user just entered an email
            self.__contactEmail = self.__contactlink
            # make contactlink proper link
            self.__contactlink = '<A HREF="MAILTO:%s”>%s</A>' % (self.__contactEmail, self.__contactEmail)
        else:
            self.__contactEmail = None

        # set pythonexe to default
        self.__pythonexe = self.__databaseUtilityDirectory + '/python'


        # end __init__


    def __readConfFile(self):
        """__readConfFile is a private helper function that reads information from the parsed config file.
        
        Inputs: None
        
        Returns: Void.

        Affects: Initializes class member variables that are found in the config file.

        Exceptions: MadrigalError thrown if any key not found.
        """

        # get the main url info
        try:
            self.__mainUrl = self.__parser.get("madrigal", self.__MAD_SERVER)
        except:
            raise madrigal.admin.MadrigalError("Unable to find key " + self.__MAD_SERVER + \
                                              " in configuration file due to ConfigParser error",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # get the top level info
        try:
            self.__topLevel = self.__parser.get("madrigal", self.__MAD_SERVERROOT)
        except:
            raise madrigal.admin.MadrigalError("Unable to find key " + self.__MAD_SERVERROOT + \
                                              " in configuration file due to ConfigParser error",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # get the site id
        try:
            self.__siteIdValue = self.__parser.get("madrigal", self.__SITE_ID)
        except:
            raise madrigal.admin.MadrigalError("Unable to find key " + self.__SITE_ID + \
                                              " in configuration file due to ConfigParser error",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # get the htmlstyle
        try:
            self.__htmlstyle = self.__parser.get("madrigal", self.__HTML_STYLE)
        except:
            raise madrigal.admin.MadrigalError("Unable to find key " + self.__HTML_STYLE + \
                                              " in configuration file due to ConfigParser error",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # get the indexhead
        try:
            self.__indexhead = self.__parser.get("madrigal", self.__INDEX_HEAD)
        except:
            raise madrigal.admin.MadrigalError("Unable to find key " + self.__INDEX_HEAD + \
                                              " in configuration file due to ConfigParser error",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # get the contactlink
        try:
            self.__contactlink = self.__parser.get("madrigal", self.__CON_TACTLINK)
        except:
            raise madrigal.admin.MadrigalError("Unable to find key " + self.__CON_TACTLINK + \
                                              " in configuration file due to ConfigParser error",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # get the mailserver
        try:
            self.__mailserver = self.__parser.get("madrigal", self.__MAIL_SERVER)
        except:
            # no error in this case - simply default to localhost
            # this avoids an error if the old version of madrigal.cfg is present
            self.__mailserver = 'localhost'
            
        # get the plot button label if set
        try:
            self.__plotbuttonlabel = self.__parser.get("madrigal", self.__PLOT_BUTTON_LABEL)
        except:
            # no error in this case - simply default to Plots/Docs
            self.__plotbuttonlabel = 'Plots/Docs'

        
        # end __readConfFile


    def __initFromHardCode(self):
        """__initFromHardCode is a private helper function that reads information from the automatically editted lines.
        
        Inputs: None
        
        Returns: Void.

        Affects: Initializes class member variables that are found in the constants at the top of this file.
        These constants were set during installation.

        Exceptions: None.
        """

        self.__mainUrl      = self.__hardCodeMadServer
        self.__topLevel     = self.__hardCodeMadServerRoot
        self.__siteIdValue  = int(self.__hardCodeSiteId)
        self.__htmlstyle    = self.__hardCodeHtmlStyle
        self.__indexhead    = self.__hardCodeIndexHead
        self.__contactlink  = self.__hardCodeContact
        
        # mailserver may be empty
        if len(self.__hardCodeMailServer) > 0:
            self.__mailserver = self.__hardCodeMailServer
        else:
            self.__mailserver = 'localhost'


    # public methods

    def getDatabaseUtilityDirectory(self):
        """getDatabaseUtilityDirectory returns the full path to the database utility directory.

        
        Inputs: None
        
        Returns: String representing the full path to the database utility directory. (eg,
        /opt/madrigal/bin)

        Affects: Nothing

        Exceptions: None
        """

        return self.__databaseUtilityDirectory


    def getWWWHomeBase(self):
        """getWWWHomeBase returns the url to the main database website(eg, http://haystack.mit.edu).
        
        Inputs: None
        
        Returns: String representing the url to the main database website.

        Affects: Nothing

        Exceptions: None
        """

        return self.__wwwHomeBase


    def getMadServer(self):
        """getMadServer returns the full name of the madrigal server (eg, haystack.mit.edu).
        
        Inputs: None
        
        Returns: String representing the url to the main database website.

        Affects: Nothing

        Exceptions: None
        """

        return self.__mainUrl


    def getTopLevelUrl(self):
        """getTopLevelUrl returns the full url of the top level directory in main database website.
        
        Inputs: None
        
        Returns: String representing the full url to the top level directory in main database website.
        (eg, http://haystack.mit.edu/madrigal)

        Affects: Nothing

        Exceptions: None
        """

        return self.__topLevelUrl


    def getRelativeTopLevel(self):
        """getRelativeTopLevel returns the relative url of the top level directory in main database website.
        
        Inputs: None
        
        Returns: String representing the relative url to the top level directory in main database website.
        (eg, madrigal)

        Affects: Nothing

        Exceptions: None
        """

        return self.__topLevel




    def getSiteID(self):
        """getSiteID returns the site id number.

        Inputs: None
        
        Returns: The site id (integer) of the madrigal installation.

        Affects: Nothing

        Exceptions: If non-integer found
        """
        
        try:
            return int(self.__siteIdValue)
        except:
            raise madrigal.admin.MadrigalError("Site id not an integer in madrigal configuration file " + \
                                              self.__confFile,
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getMadrootEnvVarName(self):
        """getMadrootEnvVarName returns the name of the environment variable __MAD_ROOT (presently = MAD_ROOT).

        Inputs: None
        
        Returns: The name of the environment variable __MAD_ROOT.

        Affects: Nothing

        Exceptions: None
        """

        return self.__MAD_ROOT


    def getMadroot(self):
        """getMadroot returns the value of the environment variable __MAD_ROOT.

        Inputs: None
        
        Returns: The value of the environment variable __MAD_ROOT.

        Affects: Nothing

        Exceptions: None
        """

        return self.__madRootDir
    

    def getMetadataDir(self):
        """getMetadataDir returns the metadata directory.

        Inputs: None
        
        Returns: The full metadata directory path. (eg. /opt/madrigal/metadata)

        Affects: Nothing

        Exceptions: None
        """

        return self.__madRootDir + self.__metadataDir
    

    def getExperimentDir(self):
        """getExperimentDir returns the main experiment directory. No longer guarenteed
        to be unique,but will hold test files and geophysical experiments.

        Inputs: None
        
        Returns: The full experiment directory path. (eg. /opt/madrigal/experiments)

        Affects: Nothing

        Exceptions: None
        """

        return self.__madRootDir + self.__experimentDir
    
    
    def getExperimentDirs(self):
        """getExperimentDirs returns a list of full paths of all valid experiment directories. 

        Inputs: None
        
        Returns:  list of full paths of all valid experiment directories. Valid experiment directories
        are those of the form of the regular expression $MADROOT/experiments[0-9]*

        Affects: Nothing

        Exceptions: None
        """
        reStr = '/experiments[0-9]*$'
        possibleExpDirs = glob.glob(os.path.join(self.__madRootDir, 'experiments*'))
        retList = []
        for possibleExpDir in possibleExpDirs:
            result = re.search(reStr, possibleExpDir)
            if result:
                retList.append(possibleExpDir)
        return(retList)
            
    

    def getBinDir(self):
        """getBinDir returns the madrigal bin directory.

        Inputs: None
        
        Returns: The madrigal bin directory path. (eg /opt/madrigal/bin)

        Affects: Nothing

        Exceptions: None
        """

        return self.__madRootDir + self.__binDir
    

    def getHtmlStyle(self):
        """getHtmlSyle returns the default html body tag for the site.

        Inputs: None
        
        Returns: The default html body tag for the site.
        (eg. <BODY BGCOLOR=#FFFF88 LINK=#008000 VLINK=#003366>)

        Affects: Nothing

        Exceptions: None
        """
        
        return self.__htmlstyle
    
    
    def getBackgroundColor(self):
        """getBackgroundColor returns the string representing background color from getHtmlStyle
        
        Inputs: None
        
        Returns the color string from the body tag in madrigal.cfg. For example, will return
            '#FFFF88; if HTMLSTYLE is <BODY BGCOLOR=#FFFF88 LINK=#008000 VLINK=#003366>
            Returns empty string if no BGCOLOR=
        """
        line = self.getHtmlStyle().upper()
        items = line.split()
        for item in items:
            index = item.find('BGCOLOR=')
            if index != -1:
                return(item[len('BGCOLOR='):])
            
        return('') # not found
                
        
    

    def getIndexHead(self):
        """getIndexHead returns the heading of the top level madrigal page.

        Inputs: None
        
        Returns: The heading of the top level madrigal page.
        (eg. Welcome to the Madrigal Database <BR> at Ishtar)

        Affects: Nothing

        Exceptions: None
        """
        
        return self.__indexhead
    

    def getContactLink(self):
        """getContactLink returns contact email link tag (see getContactEmail for the email alone).

        Inputs: None
        
        Returns: The contact email link tag.
        (eg. <A HREF="MAILTO:brideout@haystack.mit.edu">madrigal@haystack</A><BR>)

        Affects: Nothing

        Exceptions: None
        """
        
        return self.__contactlink
    

    def getContactEmail(self):
        """getContactEmail returns the email address of the site administrator.

        Inputs: None
        
        Returns: The email address of the site administrator.

        Affects: Nothing

        Exceptions: None
        """
        if not self.__contactEmail is None:
            return(self.__contactEmail)
        else:
            madSite = madrigal.metadata.MadrigalSite()
            return(madSite.getSiteEmail(self.getSiteID()))
            
    

    def getMailserver(self):
        """getMailserver returns the mailserver name.

        Inputs: None
        
        Returns: The mailserver name.  If this heading is not found in madrigal.cfg, no
        error is thrown - simply defaults to localhost

        Affects: Nothing

        Exceptions: None
        """
        
        return self.__mailserver


    def getPythonExecutable(self):
        """getPythonExecutable returns the full path to the python executable.

        Inputs: None
        
        Returns: the full path to the python executable.  If this heading is not
        found in madrigal.cfg, no error is thrown - simply defaults to
        madroot/bin/python

        Affects: Nothing

        Exceptions: None
        """
        
        return self.__pythonexe
    
    
    def getPlotButtonLabel(self):
        """getPlotButtonLabel returns the Plot button label.

        Inputs: None
        
        Returns: the Plot button label

        Affects: Nothing

        Exceptions: None
        """
        
        return self.__plotbuttonlabel


        

    def getLocalRulesOfRoad(self):
        """getLocalRulesOfRoad returns the local rules of the road.

        Inputs: None
        
        Returns: If the file madroot/local_rules_of_the_road.txt exists, returns the text of that.
        Else returns a default rules_of_road statement

        Affects: Nothing

        Exceptions: None
        """
        default_rules_of_road = 'Use of the Madrigal Database is generally subject to the ' + \
            'CEDAR Rules-of-the-Road . ' + \
	    'Prior permission to access the data is not required.  However, the user is required to establish ' + \
	    'early contact with any organization whose data are involved in the project to discuss the ' + \
	    'intended usage. Data are often subject to limitations which are not immediately evident to ' + \
	    'new users.  Before they are formally submitted, draft copies of all reports and publications ' + \
	    'must be sent to the contact scientist at all data-supplying organizations along with an offer ' + \
	    'of co-authorship to scientists who have provided data. This offer may be declined.  The ' + \
	    'Database and the organizations that contributed data must be acknowledged in all reports and ' + \
	    'publications, and whenever this data is made available through another database. If you have ' + \
	    'any questions about appropriate use of these data, contact <a href="mailto:%s">%s</a>' % (self.getContactEmail(), self.getContactEmail())

        localRules = None

        try:
            f = open(os.path.join(self.getMadroot(), 'local_rules_of_the_road.txt'))
            localRules = f.read()
            f.close()
        except:
            pass

        if localRules == None:
            return default_rules_of_road
        else:
            return localRules
        
        
    def getFullPathFromPartial(self, partialPath):
        """getFullPathFromPartial returns the full path to a file or directory based on
        a partial path.
        
        Follows the rule that if partialPath begins with [/]experiments, then the full
        path is madroot + partialPath.  Otherwise (pre madrigal 2.6) full path is
        madroot + experiments + partialPath
        
        Input: parial path (eg, 2010/mlh/15jan10 or experiments10/2010/mlh/15jan10)
        
        Returns: full path (eg /opt/madrigal/experiments10/2010/mlh/15jan10)
        """
        if partialPath[:12].find('experiments') != -1:
            fullPath = os.path.join(self.getMadroot(), partialPath)
        else:
            fullPath = os.path.join(self.getMadroot(), 'experiments', partialPath)
        return(fullPath)


    def getExpList(self,
                   expName = None,
                   kinstList = None,
                   startDate = None,
                   endDate = None,
                   startDayOfYear = None,
                   endDayOfYear = None,
                   showIgnoredExperiments = False,
                   enforcePathConvention = False):
        """getExpList returns a list of full experiment directory names that match the search arguments.

        Inputs:

            expName: a string defining what experiment names to accept (case insensitive).
            Use of unix file matching characters * and ? are allowed.  If None (default),
            any experiment name allowed.

            kinstList: a list of kinst (kind of instrument codes) integers that will be
            accepted.  If None (default) or if list contains 0, all kinst values are accepted.

            startDate: a datetime date.  If None (default), do not reject any experiments.

            endDate: a datetime date.  If None (default), do not reject any experiments.

            startDayOfYear: a Julian day number (1-366) after which to accept experiments from
            any given year.  This can be used for example to only allow files from Spring,
            which has a start day of year of 80 (March 21). If None (default), do not
            reject any experiments.

            endDayOfYear: a Julian day number (1-366) before which to accept experiments from
            any given year.  This can be used for example to only allow files from Spring,
            which has a end day of year of 172 (June 21). If None (default), do not
            reject any experiments.

            showIgnoredExperiments: if False (default), ignore experiments where expTab.txt state code
            is set to ignore.  If True, include those experiments.

            enforcePathConvention: if True, only return experiments whose path is of the form
            convention 1999/mlh/20jan98d (YYYY/<three lower case letters>/DDmmmYY<optional
            single character>. If False (the default), only required path in the form 1999/mlh/*.

        
        Returns: a list of full experiment directory names that match the search arguments

        Affects: Nothing

        Exceptions: None
        """
        if kinstList != None:
            if 0 in kinstList:
                kinstList = None

        expList = []
        
        # get all experiment directories
        expDirList = self.getExperimentDirs()

        # walk the experiments directory to find all files meeting criteria

        for thisExpDir in expDirList:
                for root, dirs, files in os.walk(thisExpDir):
                        self.__getExperiments((expName,
                          kinstList,
                          startDate,
                          endDate,
                          startDayOfYear,
                          endDayOfYear,
                          expList,
                          showIgnoredExperiments,
                          enforcePathConvention), root, dirs + files)

        return expList
    
        

    def getFileList(self,
                    expName = None,
                    kinstList = None,
                    kindatList = None,
                    startDate = None,
                    endDate = None,
                    startDayOfYear = None,
                    endDayOfYear = None,
                    publicAccessOnly = 3,
                    enforcePathConvention = 0,
                    includeNonDefault = 0,
                    includeNonMadrigal = 0,
                    appendKinst = 0,
                    appendStartTime = 0,
                    includeRealtime = 0,
                    path = None):
        """getFileList returns a list of full file names that match the search arguments.

        Inputs:

            expName: a string defining what experiment names to accept (case insensitive).
            Use of unix file matching characters * and ? are allowed.  If None (default),
            any experiment name allowed.

            kinstList: a list of kinst (kind of instrument codes) integers that will be
            accepted.  If None (default) or if list contains 0, all kinst values are accepted.
            
            kindatList: a list of kindat (kind of data codes) integers that will be
            accepted.  If None (default) or if list contains 0, all kindat values are accepted.

            startDate: a python date (see time module - actually a tuple of nine integers)
            after which to accept files.  If None (default), do not reject any files.

            endDate: a python date (see time module - actually a tuple of nine integers)
            before which to accept files.  If None (default), do not reject any files.

            startDayOfYear: a Julian day number (1-366) after which to accept files from
            any given year.  This can be used for example to only allow files from Spring,
            which has a start day of year of 80 (March 21). If None (default), do not
            reject any files.

            endDayOfYear: a Julian day number (1-366) before which to accept files from
            any given year.  This can be used for example to only allow files from Spring,
            which has a end day of year of 172 (June 21). If None (default), do not
            reject any files.

            publicAccessOnly: if 1, only return files marked in fileTab.txt and expTab.txt
            as public.  If 0, return all non-archive files. If 2, return public
            and public archive experiments with public files.  If 3, return all files,
            including public, private, archived, and private archived. (the default)

            enforcePathConvention: if 1, only return experiments whose path is of the form
            convention 1999/mlh/20jan98d (YYYY/<three lower case letters>/DDmmmYY<optional
            single character>. If 0 (the default), only require paths to be in the form 1999/mlh/*.

            includeNonDefault: if 1, also include data files that are not default files
            in they match all other criteria. If 0 (the default), exclude non-default.

            includeNonMadrigal: if 1, include all experiment files that are not in
            fileTab.txt.  If 0, (the default) limit data files to those in fileTab.txt.

            appendKinst: if 1, append kind of instrument integer to each file name, so that
            what is returned is a list of (file name, inst code) tuples. If 0 (the default),
            do not append instrument code; return a list of file names.

            appendStartTime: if 1, append start time in seconds since 1950 to each file name, so that
            what is returned is a list of (file name, startTime) tuples. If 0 (the default),
            do not append startTimes.

            includeRealtime: if 1, include realtime files even if includeNonDefault = 0.  If 0
            (default), do not include realtime if includeNonDefault = 0.
            
            path if not None (the default), search only experiment directory set by path.  If None,
            search add experiments.
        
        Returns: a list of full path file names (strings) that match the search arguments

        Affects: Nothing

        Exceptions: None
        """
        if kinstList != None:
            if 0 in kinstList:
                kinstList = None

        if kindatList != None:
            if 0 in kindatList:
                kindatList = None

        fileList = []
        
        # get all experiment directories
        if path is None:
            expDirList = self.getExperimentDirs()
        else:
            expDirList = [path]

        # walk the experiments directory to find all files meeting criteria
 
        for thisExpDir in expDirList:
                for root, dirs, files in os.walk(thisExpDir):
                          self.__getFiles((expName,
                                           kinstList,
                                           kindatList,
                                           startDate,
                                           endDate,
                                           startDayOfYear,
                                           endDayOfYear,
                                           fileList,
                                           publicAccessOnly,
                                           enforcePathConvention,
                                           includeNonDefault,
                                           includeNonMadrigal,
                                           appendKinst,
                                           appendStartTime,
                                           includeRealtime), 
                                           root, dirs + files)

        return fileList


    def getFileListFromMetadata(self,
                                expName = None,
                                kinstList = None,
                                kindatList = None,
                                startDate = None,
                                endDate = None,
                                startDayOfYear = None,
                                endDayOfYear = None,
                                publicAccessOnly = 3,
                                includeNonDefault = 0,
                                appendKinst = 0,
                                appendStartTime = 0,
                                includeRealtime = 0):
        """getFileListFromMetadata returns a list of full file names that match the search arguments using metadata only.

        This method is very similar to getFileList, except that it assumes the metadata is correct, and doesn't
        search the actual experiment directories.  It is therefore faster, but less robust.

        Inputs:

            expName: a string defining what experiment names to accept (case insensitive).
            Use of unix file matching characters * and ? are allowed.  If None (default),
            any experiment name allowed.

            kinstList: a list of kinst (kind of instrument codes) integers that will be
            accepted.  If None (default) or if list contains 0, all kinst values are accepted.

            kindatList: a list of kindat (kind of data codes) integers that will be
            accepted.  If None (default) or if list contains 0, all kindat values are accepted.

            startDate: a python date (see time module - actually a tuple of nine integers)
            after which to accept files.  If None (default), do not reject any files.

            endDate: a python date (see time module - actually a tuple of nine integers)
            before which to accept files.  If None (default), do not reject any files.

            startDayOfYear: a Julian day number (1-366) after which to accept files from
            any given year.  This can be used for example to only allow files from Spring,
            which has a start day of year of 80 (March 21). If None (default), do not
            reject any files.

            endDayOfYear: a Julian day number (1-366) before which to accept files from
            any given year.  This can be used for example to only allow files from Spring,
            which has a end day of year of 172 (June 21). If None (default), do not
            reject any files.

            publicAccessOnly: if 1, only return files marked in fileTab.txt and expTab.txt
            as public.  If 0, return all non-archive files. If 2, return public
            and public archive experiments with public files.  If 3, return all files,
            including public, private, archived, and private archived. (the default)

            includeNonDefault: if 1, also include data files that are not default files
            in they match all other criteria. If 0 (the default), exclude non-default.

            appendKinst: if 1, append kind of instrument integer to each file name, so that
            what is returned is a list of (file name, inst code) tuples. If 0 (the default),
            do not append instrument code; return a list of file names.

            appendStartTime: if 1, append start time in seconds since 1950 to each file name, so that
            what is returned is a list of (file name, startTime) tuples. If 0 (the default),
            do not append startTimes.

            includeRealtime: if 1, include realtime files even if includeNonDefault = 0.  If 0
            (default), do not include realtime if includeNonDefault = 0.
        
        Returns: a list of full path file names (strings) that match the search arguments, and possibly kinst and
        starttime.

        Affects: Nothing

        Exceptions: None
        """
        if kinstList != None:
            if 0 in kinstList:
                kinstList = None

        if kindatList != None:
            if 0 in kindatList:
                kindatList = None

        # the list to be returned
        fileList = []

        # load a Madrigal experiment object
        madExpObj = MadrigalExperiment(self)

        # load a Madrigal metafile object
        madFileObj = MadrigalMetaFile(self)

        # convert input arguments to needed forms
        if expName != None:
            expNameArg = expName.replace(' ', '_')

        if startDate != None:
            startDateTime = madrigal.metadata.getMadrigalUTFromDate(startDate[0],
                                                           startDate[1],
                                                           startDate[2],
                                                           startDate[3],
                                                           startDate[4],
                                                           startDate[5],
                                                           0)

        if endDate != None:
            endDateTime = madrigal.metadata.getMadrigalUTFromDate(endDate[0],
                                                         endDate[1],
                                                         endDate[2],
                                                         endDate[3],
                                                         endDate[4],
                                                         endDate[5],
                                                           0)

        # loop through all experiments to create four lists:
        #  acceptedExpIdList, acceptedExpKinstList, acceptedExpStartTimeList, and acceptedExpDirList
        acceptedExpIdList = []
        acceptedExpIdDict = {}
        acceptedExpKinstList = []
        acceptedExpStartTimeList = []
        acceptedExpDirList = []
        position = -1
        while 1:

            position += 1 # first position is zero
            
            thisName = madExpObj.getExpNameByPosition(position)

            #check if we're at the end
            if thisName == None:
                break

            # check experiment access
            thisSecurity = madExpObj.getSecurityByPosition(position)
            if thisSecurity == -1:
                continue
            elif publicAccessOnly == 0:
                if thisSecurity not in (0,1):
                    continue
            elif publicAccessOnly == 1:
                if thisSecurity != 0:
                    continue
            elif publicAccessOnly == 2:
                if thisSecurity not in (0,2):
                    continue
            
            # apply expName filter if desired
            if expName != None:


                # since we are using file name matching, all spaces are
                # converted to underscores
                thisName = thisName.replace(' ', '_')

                # try to match (case insensitive)
                if not fnmatch.fnmatch(thisName.lower(), expNameArg.lower()):
                    continue

            # apply kinstList filter
            thisKinst = madExpObj.getKinstByPosition(position)
            
            if kinstList != None:
                if thisKinst not in kinstList:
                    continue

            # startDate filter - we always need thisStartTime whether or not filter used
            thisStartDate = madExpObj.getExpStartDateTimeByPosition(position)

            thisStartTime = madrigal.metadata.getMadrigalUTFromDate(thisStartDate[0],
                                                           thisStartDate[1],
                                                           thisStartDate[2],
                                                           thisStartDate[3],
                                                           thisStartDate[4],
                                                           thisStartDate[5],
                                                           0)
            
            if endDate != None:
                if thisStartTime > endDateTime:
                    continue

                
            # endDate filter
            if endDate != None:
                thisEndDate = madExpObj.getExpEndDateTimeByPosition(position)

                thisEndTime = madrigal.metadata.getMadrigalUTFromDate(thisEndDate[0],
                                                             thisEndDate[1],
                                                             thisEndDate[2],
                                                             thisEndDate[3],
                                                             thisEndDate[4],
                                                             thisEndDate[5],
                                                             0)

                if startDate != None:
                    if thisEndTime < startDateTime:
                        continue


            # apply startDayOfYear filter
            if startDayOfYear != None:

                thisStartDate = madExpObj.getExpStartDateTimeByPosition(position)

                if thisStartDate[7] < startDayOfYear:
                    # index 7 refers to Day of year
                    continue


            # apply endDayOfYear filter
            if endDayOfYear != None:

                thisEndDate = madExpObj.getExpEndDateTimeByPosition(position)

                if thisEndDate[7] > endDayOfYear:
                    # index 7 refers to Day of year
                    continue

            # this experiment has made it through all filters, append its id, kinst, startTime, and dir
            acceptedExpIdList.append(madExpObj.getExpIdByPosition(position))
            acceptedExpIdDict[madExpObj.getExpIdByPosition(position)] = len(acceptedExpIdList) - 1
            acceptedExpKinstList.append(thisKinst)
            acceptedExpStartTimeList.append(thisStartTime)
            # find the directory based on url
            dir = self.getMadroot()
            if dir[-1] != '/':
                dir += '/'
            url = madExpObj.getExpUrlByPosition(position)
            index = url.find('/madtoc/')
            partialExpDir = url[index+8:]
            # added default experiments if not there already
            if partialExpDir.find('experiments') == -1:
                dir += 'experiments/'
            dir += url[index+8:]
            acceptedExpDirList.append(dir)
            
        sortedAcceptedExpIdList = sorted(acceptedExpIdList)

        # now loop through the file object to find all files
        position = -1
        while 1:

            position += 1 # first position is zero
            
            thisExpId = madFileObj.getExpIdByPosition(position)

            #check if we're at the end
            if thisExpId == None:
                break
            
            # skip this file if expId not in acceptedExpIdList
            if sortedAcceptedExpIdList[bisect.bisect_left(sortedAcceptedExpIdList, thisExpId)] != thisExpId:
                continue
		
	        # apply kindatList filter
            if kindatList != None:
                thisKindat = madFileObj.getKindatByPosition(position)
                if thisKindat not in kindatList:
                    continue

            # check file access
            if madFileObj.getAccessByPosition(position) != 0:
                if publicAccessOnly in (1,2):
                    continue

            # apply includeNonDefault filter
            if includeNonDefault == 0:
                category = madFileObj.getCategoryByPosition(position)
                if includeRealtime == 0:
                    if category != 1:
                        # not default
                        continue
                else:
                    if category not in (1,4):
                        # not default or realtime
                        continue
                

            # this file has been accepted by all filters - first, get its experiment index
            expIndex = acceptedExpIdDict[thisExpId]

            thisFilename = acceptedExpDirList[expIndex] + '/' + madFileObj.getFilenameByPosition(position)

            # append result to fileList according to appendKinst and appendStartTime
            if appendKinst == 0 and appendStartTime == 0:
                fileList.append(thisFilename)
            elif appendKinst == 1 and appendStartTime == 0:
                fileList.append((thisFilename, acceptedExpKinstList[expIndex]))
            elif appendKinst == 0 and appendStartTime == 1:
                fileList.append((thisFilename, acceptedExpStartTimeList[expIndex]))
            else:
                fileList.append((thisFilename, acceptedExpKinstList[expIndex], acceptedExpStartTimeList[expIndex]))

        return fileList
            


    def setFileAccess(self, expDirectory, accessMode):
        """setFileAccess sets all fileTab.txt  and expTab.txt files in all subdirectories of expDirectory to be public or private.

        Inputs:

            expDirectory:  The full path to a directory in the experiment directory.  That is, it
            may be madroot/experiments[0-9]* or any directory under it.

            accessMode: either 0 for public access, or 1 for private access.
        
        Returns: None

        Affects: sets all fileTab.txt files in all subdirectories of expDirectory to be public or private.

        Exceptions: If accessMode is not 1 or 0.
        """
        if (accessMode != 0 and accessMode != 1):
            raise madrigal.admin.MadrigalError('MadrigalDB.setFileAccess called with accessMode = ' + \
                   str(accessMode) + ', must be either 0 or 1', None)



        # walk the experiments directory to find all files meeting criteria
           
        for root, dirs, files in os.walk(expDirectory):
            self.__setFileAccess(accessMode, root, dirs + files)
        

    def tarExperiments(self,
                       tarFileName,
                       startDate = None,
                       endDate = None,
                       excludePrivData = 0,
                       ignoreDirCon = 1,
                       includeNonDefData = 0,
                       onlyData = 0,
                       filetype = 0,
                       verbose = 0):
        """tarExperiments creates a tar file containing files from madroot/experiments[0-9]*.

        Note:  this method sometimes requires the modification of the fileTab.txt
        files found in the experiments directory.  This is because some data files might be
        excluded, so that the fileTab.txt file will no longer be accurate.  Because of this,
        all files to be tar'ed will be copied to /tmp/temp<random num>/experiments,
        where the fileTab.txt files will be modified.  When done, this temp dir will be deleted.

        Inputs:

            tarFileName:  The full path to a tar file to be created.

            startDate: a python date (see time module - actually a tuple of nine integers)
            after which to accept files.  If None (default), do not reject any files.

            endDate: a python date (see time module - actually a tuple of nine integers)
            before which to accept files.  If None (default), do not reject any files.
            
            excludePrivData: if 1, allow data marked as private to be omitted (and the line
            from the fileTab.txt to be removed).  If 0 (the default), all data, public and
            private, will be included.

            ignoreDirCon: if 1, ignore convention that directory must be in form
            1999/mlh/03sep99 (the default).  If 0 , reject non-standard directories.

            includeNonDefData:  if 1, include all files listed in fileTab.txt. If 0 (the default),
            reject non-default files, and modify fileTab.txt to remove non-default listings.

            onlyData: if 1, reject all files not listed in fileTab.txt.  If 0 (the default),
            accept all files in a directory not mentioned in fileTab.txt.

            filetype: format to save data files as.  Default 0 is to leave present format unchanged.
            <type> is an integer as follows:

                type = 0  Leave present format unchanged (default)
            
                type = 1  Madrigal
                
                type = 2  Blocked Binary
                
                type = 3  Cbf
                
                type = 4  Unblocked binary
                
                type = 5  Ascii

            verbose: if 1, print to std out the list of files included (relative path).  If 0,
            (the default) print nothing.
        
        Returns: None

        Affects: created tar file tarFileName of selected files from madroot/experiments. 

        Exceptions: If unable to read any experiment file.
        """
        
        if ignoreDirCon == 1:
            enforcePathConvention = 0
        else:
            enforcePathConvention = 1

        if onlyData == 1:
            includeNonMadrigal = 0
        else:
            includeNonMadrigal = 1

        # create a random temp dir
        tempDir = '/tmp/temp' + str(random.randrange(1,10000000))

        if verbose == 1:
            print ('Creating list of files to tar...')

        # get list of files to tar
        tarFileList = self.getFileList(None,
                                       None,
                                       None,
                                       startDate,
                                       endDate,
                                       None,
                                       None,
                                       excludePrivData,
                                       enforcePathConvention,
                                       includeNonDefData,
                                       includeNonMadrigal)
        
        # now create a new list of filenames, using relative paths
        relTarFileList = []
        for file in tarFileList:
            newFilename = file.split(self.getMadroot() + '/')[1]
            relTarFileList.append(newFilename)

        # copy all these files to tempDir
        if verbose == 1:
            print('The following is a list of files included:')
        for file in relTarFileList:
            # make sure dir exists
            try:
                os.makedirs(tempDir + '/' + os.path.dirname(file))
            except:
                pass

            # now copy file
            shutil.copyfile(self.getMadroot() + '/' + file, tempDir + '/' + file)
            if verbose == 1:
                print('\t' + file)

            if onlyData:
                # now check if that data file needs to be converted to another filetype
                # since onlyData is set, convert every file
                if filetype != 0:
                    # copy data file to another location
                    shutil.copyfile(tempDir + '/' + file, tempDir + '/' + file + '.backup')
                    # run mergeCedarFiles
                    execStr = self.getMadroot() + '/bin/mergeCedarFiles -i ' + \
                              tempDir + '/' + file + '.backup 1 100000000 -o ' + \
                              tempDir + '/' + file + \
                              ' -t ' + str(filetype)
                    os.system(execStr)
                    os.remove(tempDir + '/' + file + '.backup')

        if verbose == 1:
            print('Modifying fileTab.txt files if needed...')

        # modify fileTab.txt files, if required
        if (not onlyData) and (excludePrivData or not includeNonDefData):
            # first loop through each fileTab.txt file in list just to check permissions
            for filename in relTarFileList:
                if os.path.basename(filename) != 'fileTab.txt':
                    continue
                
                # make sure fileTab.txt is writable
                if not os.access(tempDir + '/' + filename, os.W_OK):
                    raise madrigal.admin.MadrigalError('Unable to tar experiments because denied write permission ' + \
                                                       'for ' + str(filename), None)
                
                # make sure the directory is writable
                if not os.access(os.path.dirname(tempDir + '/' + filename), os.W_OK):
                    raise madrigal.admin.MadrigalError('Unable to tar experiments because denied write permission ' + \
                                                       'for ' + str(os.path.dirname(filename)), None)

            # no exceptions raised - all permissions are okay
            # this time loop through and modify the fileTab.txt files
            for filename in relTarFileList:
                if os.path.basename(filename) != 'fileTab.txt':
                    continue

                # create a MadrigalMetaFile object 
                fileMeta = MadrigalMetaFile(self, tempDir + '/' + filename)

                # loop through each file name to see if its in tarFileList:
                fileNum = 0
                while 1:
                    madFilename = fileMeta.getFilenameByPosition(fileNum)
                    if madFilename == None:
                        break
                    # get madRelFilename 
                    madRelFilename = os.path.dirname(filename) + '/' + madFilename
                    # if its not in relTarFileList, delete it from fileMeta
                    if not madRelFilename in relTarFileList:
                        fileMeta.deleteRowByFilename(madFilename)
                        fileNum = 0
                        continue
                    else:
                        # we know madRelFilename is a data file since its in fileTab.txt -
                        # now check if that data file needs to be converted to another filetype
                        if filetype != 0:
                            # copy data file to another location
                            shutil.copyfile(tempDir + '/' + madRelFilename, tempDir + '/' + madRelFilename + '.backup')
                            # run mergeCedarFiles
                            execStr = self.getMadroot() + '/bin/mergeCedarFiles -i ' + \
                                      tempDir + '/' + madRelFilename + '.backup 1 100000000 -o ' + \
                                      tempDir + '/' + madRelFilename + \
                                      ' -t ' + str(filetype)
                            os.system(execStr)
                            os.remove(tempDir + '/' + madRelFilename + '.backup')
                    # get next madFilename
                    fileNum = fileNum + 1

                # if fileMeta not empty, write it out
                if fileMeta.getFileCount() > 0:
                    fileMeta.writeMetadata()

                # else if its empty, simply delete it
                else:
                    os.remove(tempDir + '/' + filename)

        # done modifying fileTab.txt - ready to tar
        # need to change working directory to tempDir to get relative paths in tar.
        # Since this directory will soon be deleted, will need to change it back to
        # whatever it is now when we're done
        pwd = os.getcwd()
        
        # check if tarFileName is absolute or relative to pwd
        if tarFileName[0] != '/':
            tarFileName = pwd + '/' + tarFileName

        if verbose == 1:
            print('Creating tar file...')
            
        os.chdir(tempDir)
        os.system('tar -cf ' + tarFileName + ' experiments*')
        os.chdir(pwd)

        if verbose == 1:
            print('Removing temp files...')

        # finally remove temp dir
        try:
            shutil.rmtree(tempDir)
        except:
            raise madrigal.admin.MadrigalError('In tarExperiments could not remove dir ' + tempDir,
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))
                
                                                       
                
    def listFileTimes(self, expDir=None, relative=True):
        """listFileTimes returns a list of (filename, datetime_ut) of all files in the experiment directory
        
        Inputs:
        
            expDir - the particular subdirectory of an experiment directory to list.  If None (the default),
                will list all files for all experiment directories.  May be an absolute path, or may start with
                experiments[0-9]*.
                
            relative - if True (the default) give the path relative to the experiments[0-9]* directory.  If False,
                give full path
                
        Returns: a list of tuples, where each tuple has 1.  the file path, and 2. a UT datetime object of the
            last file modification.
            
        Exceptions: raised if expDir is not a valid experiments directory or subdirectory
        """
        expDirs = self.getExperimentDirs()
        
        if expDir == None:
            dirsToExamine = expDirs
        else:
            # verify a valid directory
            found = False
            if expDir[-1] == '/':
                expDir = expDir[:-1] # strip trailing /
            if expDir[0:11] == 'experiments':
                # convert to absolute path
                expDir = os.path.join(self.getMadroot(), expDir)
            for thisDir in expDirs:
                if expDir.find(thisDir) != -1:
                    found = True
            if found:
                dirsToExamine = [expDir]
            else:
                raise ValueError('expDir %s not a valid experiment directory' % (expDir))
            
        retList = []
        
        for dirToExamine in dirsToExamine:
            for root, dirs, files in os.walk(dirToExamine):
                for name in files:
                    fullname = os.path.join(root, name)
                    relIndex = fullname[len(self.__madRootDir):].find('/experiments')
                    relativeName = fullname[relIndex + 1 + (len(self.__madRootDir)):]
                    ts = os.stat(fullname).st_mtime
                    if relative:
                        retList.append((relativeName, datetime.datetime.utcfromtimestamp(ts)))
                    else:
                        retList.append((fullname, datetime.datetime.utcfromtimestamp(ts)))
                    
        return(retList)
    
    
    def getKinstKindatConfig(self, kinst, kindat, iniFile=None):
        """getKinstKindatConfig gets information for the MADROOT/cachedFiles.ini needed to create Madrigal3 files if not specified.
        
        Used primarily to get independent spatial parameters and array splitting parms for given files for old loading programs that don't specify them.
        
        Inputs:
        
            kinst - the instrument kinst (integer)
            
            kindat - the data kindat (integer)
            
            iniFile - the ini file to use.  If None, uses default ini file $MADROOT/cachedFiles.ini
            
        Returns: a tuple with three items:
            1. a list of extra parameters (string mnemonics)
            2. a list of independent spatial parameters
            3. a list of array splitting parameters
            
        Algorithm:
        
        1. If iniFile == None and no default file, returns ([], [], [])
        2. Searches ini file for section [%i] % (kinst).  If not found, returns ([], [], [])
        3. Searches right section for key %i_parms % (kindat).  If not found, searches for default_parms.
            If not found, extra parameters are []
        4. Searches right section for key %i_formats % (kindat).  If not found, searches for default_formats.
            If not found, indepedent spatial parms and extra splitting parameters are [] and [].  If found,
            then parses dictionary, and returns key 'array'.  If value has only one item, returns it as a one item
            list of independent spatial parameters.  If two items, they are ind spatial parms and array splitting parms.
        """
        if not iniFile:
            thisIniFile = os.path.join(self.getMadroot(), 'cachedFiles.ini')
            if not os.access(thisIniFile, os.R_OK):
                return(([], [], []))
        else:
            thisIniFile = iniFile
            
        instSection = '%i' % (kinst)
            
        parser = configparser.ConfigParser()
        parser.read(thisIniFile)
        if not parser.has_section(instSection):
            return(([], [], []))
        extraParms = []
        indSpatialParms = []
        arraySplitParms = []
        
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
        formatDict = None
        if parser.has_option(instSection, '%i_formats' % (kindat)):
            formatDict = parser.get(instSection, '%i_formats' % (kindat))
            formatDict = eval(formatDict)
        elif parser.has_option(instSection, 'default_formats'):
            formatDict = parser.get(instSection, 'default_formats')
            formatDict = eval(formatDict)
        if not formatDict is None:
            if 'array' in formatDict:
                value = formatDict['array']
                if type(value) in (list, tuple):
                    indSpatialParms = value[0]
                    arraySplitParms = value[1]
                else:
                    indSpatialParms = [value]
                    
                    
        return((finalExtraParms, indSpatialParms, arraySplitParms))
        
        

    def toString(self):
        """toString returns a simple string representation of a MadrigalDB object.

        Inputs: None
        
        Returns: String describing a simple representation of a MadrigalDB object.

        Affects: Nothing

        Exceptions: None
        """

        output =  "Object type: MadrigalDB\n"
        output += "Database utility directory = "   + self.getDatabaseUtilityDirectory() + "\n"
        output += "WWW home base = "                + self.getWWWHomeBase() + "\n"
        output += "Server name = "                  + self.getMadServer() + "\n"
        output += "Top level url = "                + self.getTopLevelUrl() + "\n"
        output += "Relative Top level url = "       + self.getRelativeTopLevel() + "\n"
        output += "Site ID = "                      + str(self.getSiteID()) + "\n"
        output += "MAD_ROOT env. variable name = "   + self.getMadrootEnvVarName() + "\n"
        output += "MAD_ROOT env. variable value = "  + self.getMadroot() + "\n"
        output += "Madrigal metadata dir = "        + self.getMetadataDir() + "\n"
        output += "Madrigal bin dir = "             + self.getBinDir() + "\n"
        output += "Madrigal html body style = "     + self.getHtmlStyle() + "\n"
        output += "Madrigal top level heading = "   + self.getIndexHead() + "\n"
        output += "Madrigal contact link = "        + self.getContactLink() + "\n"
        output += "Madrigal contact email = "       + str(self.getContactEmail()) + "\n"
        output += "Madrigal mailserver = "          + self.getMailserver() + "\n"
        output += "Madrigal python exe = "          + self.getPythonExecutable() + "\n"

        return output
    
    
    def isTestExperiment(self, url, siteId=None):
        """isTestExperiment returns True if the given experiment url is a test of this Madrigal
        server.  Url can be either real or form in expTab.txt, or can be the experiment directory.
        If siteId not given, use local site id.
        """
        # Skip test experiments
        if siteId == None:
            siteId = self.getSiteID()
        if url.find('1998/mlh/20jan98') != -1 and siteId != 1:
            return(True)
        if url.find('1997/aro/06jan97') != -1 and siteId != 7:
            return(True)
        if url.find('1997/lyr/08apr97') != -1 and siteId != 2:
            return(True)
        if url.find('1997/son/06jan97') != -1 and siteId != 3:
            return(True)
        if url.find('1995/jro/01feb95') != -1 and siteId != 6:
            return(True)
        if url.find('1998/jro/27apr98') != -1 and siteId != 6:
            return(True)
        return(False)
    
    
    def createGroupIdWithList(self, user_fullname, user_email, user_affiliation, fileList):
        """createGroupIdWithList writes to $MADROOT/metadata/cedarGroupId.hdf5, the file that records citations to groups of files
        
            Inputs:
                user_fullname, user_email, user_affiliation - strings identifying user
                fileList is a list of urls for individual files in the full form 'https://w3id.org/cedar?experiment_list=experiments/2013/mlh/16mar13&file_list=mlh130316g.004.hdf5'
                    or without the static part, 'experiments/2013/mlh/16mar13&file_list=mlh130316g.004.hdf5'
                    
            Returns:
                The group id created, which will be the next higher, or if none exist, 1000
    
            This method is for the user who manually picks files.
        """
        if len(fileList) == 0:
            # nothing to be done
            return(-1)
        hdf5File = os.path.join(self.getMadroot(), 'metadata/cedarGroupId.hdf5')
        dtype = [('group_id', int),('user_fullname', numpy.bytes_, 256),
                 ('user_email', numpy.bytes_, 256),
                 ('user_affiliation', numpy.bytes_, 256),
                 ('url', numpy.bytes_, 512)]
        
        # lock the file to make sure single write access
        lock_path = hdf5File + '.lock'
        lock = filelock.FileLock(lock_path, timeout=10)
        with lock:
            # get id to add
            if not os.access(hdf5File, os.R_OK):
                new_id = 1000
            else:
                with h5py.File(hdf5File, 'r') as f:
                    ids = f['group_id_dset']['group_id']
                    new_id = numpy.max(ids) + 1
                    
            # create data to append
            this_recarray = numpy.recarray((len(fileList),), dtype=dtype)
            for i, thisFile in enumerate(fileList):
                if thisFile[:len('experiments')] == 'experiments':
                    thisFile = 'https://w3id.org/cedar?experiment_list=' + thisFile
                this_recarray[i] = (new_id, user_fullname, user_email, user_affiliation, thisFile)
                    
            with h5py.File(hdf5File, 'a') as f:
                
                if 'group_id_dset' in f.keys():
                    group_id_recarray = f['group_id_dset']
                    group_id_recarray.resize((group_id_recarray.shape[0] + len(this_recarray),))
                    group_id_recarray.write_direct(this_recarray,None,numpy.s_[-1*len(this_recarray):])
                else:
                    # new file
                    f.create_dataset('group_id_dset', data=this_recarray, 
                                     chunks=True, maxshape=(None,))
                
                
        return(new_id)
    
    
    
    def getListFromGroupId(self, group_id):
        """getListFromGroupId returns a list of files associated with group_id in $MADROOT/metadata/cedarGroupId.hdf5, the file that records citations to groups of files
        
            Inputs:
                group_id - group id from group permanent url (integer)
                    
            Returns:
                A list of files with that group_id, each in the form 'https://w3id.org/cedar?experiment_list=experiments/2013/mlh/16mar13&file_list=mlh130316g.004.hdf5'
                If group_id not found, returns empty list
        """
        hdf5File = os.path.join(self.getMadroot(), 'metadata/cedarGroupId.hdf5')
        if not os.access(hdf5File, os.R_OK):
            raise IOError('File %s not readable' % (hdf5File))
        # lock the file to make sure single write access
        lock_path = hdf5File + '.lock'
        lock = filelock.FileLock(lock_path, timeout=10)
        with lock:
            with h5py.File(hdf5File, 'a') as f:
                ids = f['group_id_dset']['group_id']
                indices = numpy.argwhere(ids == group_id)
                urls = [url[0].decode('utf-8') for url in f['group_id_dset']['url'][indices]]
                return(urls)
                


    def __str__(self):
        """ __str__ simply calls toString """
        return (self.toString())
    

    def __isValidEmail(self, emailStr):
        """ __isValidEmail is a private helper function that does some checking to ensure a valid email address was found.

        Inputs: emailStr - email address string to verify
        
        Returns: 1 if no problems, 0 if not valid.

        Affects: Nothing

        Exceptions: None
        """
        emailStr = emailStr.strip()

        if emailStr.find(' ') != -1:
            return 0

        if emailStr.find('"') != -1:
            return 0

        if emailStr.find('<') != -1:
            return 0

        if emailStr.find('>') != -1:
            return 0

        if emailStr.find('/') != -1:
            return 0

        if emailStr.find(':') != -1:
            return 0
        
        if emailStr.find('@') == -1:
            return(0)

        # otherwise okay
        return 1


    def __getExperiments(self, arg, dirname, names):
        """ __getExperiment is a private helper function called by os.path.walk in getExpList.

        __getExperiments is called for each sub-directory in the experiments directory.  Its purpose
        is to add any experiment directory paths from that directory that match the search criteria
        to the expList.

        Inputs:

            arg: a tuple containing all the filter arguments, plus the fileList
            to be appended to.  The tuple elements are:

                expName: a string defining what experiment names to accept (case insensitive).
                Use of unix file matching characters * and ? are allowed.  If None,
                any experiment name allowed.

                kinstList: a list of kinst (kind of instrument codes) integers that will be
                accepted.  If None, all kinst values are accepted.

                startDate: a python datetime in UTC.  If None, do not reject any files.

                endDate: a python datetime in UTC.  If None, do not reject any files.

                startDayOfYear: a Julian day number (1-366) after which to accept files from
                any given year.  This can be used for example to only allow files from Spring,
                which has a start day of year of 80 (March 21). If None, do not
                reject any files.

                endDayOfYear: a Julian day number (1-366) before which to accept files from
                any given year.  This can be used for example to only allow files from Spring,
                which has a end day of year of 172 (June 21). If None, do not
                reject any files.

                expList:  the list of valid experiment paths to which is appended any newly-found
                valid experiment directories
                
                showIgnoredExperiments: if False (default), ignore experiments where expTab.txt state code
                is set to ignore.  If True, include those experiments.

                enforcePathConvention: if True, only return experiments whose path is of the form
                convention 1999/mlh/20jan98d (YYYY/<three lower case letters>/DDmmmYY<optional
                single character>. If False (the default), only required path in the form 1999/mlh/*.

                
            dirname: the directory name of the present directory

            names:  the list of filenames in the present directory
        
        Returns: None.

        Affects: Adds full file paths of any data files found that meet all filter criteria

        Exceptions: None
        """
        

        # constants - arg order
        __expName           = 0
        __kinstList         = 1
        __startDate         = 2
        __endDate           = 3
        __startDayOfYear    = 4
        __endDayOfYear      = 5
        __expList           = 6
        __showIgnored       = 7
        __enforcePath       = 8


        # regular expression that enforces dir path convention
        # note that the __dirConvStr2 is the old convention that experiment directories be in the form DDmmmYY[char]
        __dirConvStr1 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[a-zA-Z0-9\-_]*$'
        __dirConvStr2 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[0-3][0-9][a-z][a-z0-9][a-z0-9][0-9][0-9].?'
        
        # ignore any directory without expTab.txt
        if not 'expTab.txt' in names:
            return
            
        # get exp metadata
        expMeta = MadrigalExperiment(self, dirname + '/' + 'expTab.txt')

        # apply expName filter
        if arg[__expName] != None:

            expName = expMeta.getExpNameByPosition()

            # since we are using file name matching, all spaces are
            # converted to underscores
            expName = expName.replace(' ', '_')
            expNameArg = arg[__expName].replace(' ', '_')

            # try to match (case insensitive)
            if not fnmatch.fnmatch(expName.lower(), expNameArg.lower()):
                # stop going down tree
                names[:] = []
                return

        # apply kinstList filter
        expKinst = expMeta.getKinstByPosition()
        
        if arg[__kinstList] != None:
            if expKinst not in arg[__kinstList]:
                # stop going down tree
                names[:] = []
                return

        # apply startDate filter

        expStartDate = expMeta.getExpStartDateTimeByPosition()
        
        if expStartDate is None:
                raise IOError('Error getting expStartDate from dir %s' % (dirname))

        time1 = datetime.datetime(expStartDate[0],
                                  expStartDate[1],
                                  expStartDate[2],
                                  expStartDate[3],
                                  expStartDate[4],
                                  expStartDate[5])
        
        if arg[__endDate] != None:

            if time1 > arg[__endDate]:
                # stop going down tree
                names[:] = []
                return

            
        # apply endDate filter
        if arg[__startDate] != None:

            expEndDate = expMeta.getExpEndDateTimeByPosition()

            time1 = datetime.datetime(expEndDate[0],
                                      expEndDate[1],
                                      expEndDate[2],
                                      expEndDate[3],
                                      expEndDate[4],
                                      expEndDate[5]) 

            if time1 < arg[__startDate]:
                # stop going down tree
                names[:] = []
                return


        # apply startDayOfYear filter
        if arg[__startDayOfYear] != None:

            expStartDate = expMeta.getExpStartDateTimeByPosition()

            if expStartDate[7] < arg[__startDayOfYear]:
                # index 7 refers to Day of year
                # stop going down tree
                names[:] = []
                return


        # apply endDayOfYear filter
        if arg[__endDayOfYear] != None:

            expEndDate = expMeta.getExpEndDateTimeByPosition()

            if expEndDate[7] > arg[__endDayOfYear]:
                # index 7 refers to Day of year
                # stop going down tree
                names[:] = []
                return

        # apply ignore filter
        if expMeta.getSecurityByPosition() == -1 and not arg[__showIgnored]:
            # stop going down tree
            names[:] = []
            return

            
        # now reject paths not matching present path convention if not enforcePathConvention
        if not arg[__enforcePath]:
            reDirPath = re.compile(self.getMadroot() + __dirConvStr1)
            match = reDirPath.search(dirname)
            if match == None:
                return

        # reject paths not matching old path convention if enforcePathConvention
        if arg[__enforcePath]:
            reDirPath = re.compile(self.getMadroot() + __dirConvStr2)
            match = reDirPath.search(dirname)
            if match == None:
                return
            
        # experiment is okay
        arg[__expList].append(dirname)

        


    def __getFiles(self, arg, dirname, names):
        """ __getFiles is a private helper function called by os.path.walk in getFileList.

        __getFiles is called for each sub-directory in the experiments directory.  Its purpose
        is to add any file paths from that directory that match the search criteria
        to the fileList.

        Inputs:

            arg: a tuple containing all the filter arguments, plus the fileList
            to be appended to.  The tuple elements are:

                expName: a string defining what experiment names to accept (case insensitive).
                Use of unix file matching characters * and ? are allowed.  If None,
                any experiment name allowed.

                kinstList: a list of kinst (kind of instrument codes) integers that will be
                accepted.  If None, all kinst values are accepted.

                kindatList: a list of kindat (kind of data codes) integers that will be
                accepted.  If None, all kindat values are accepted.

                startDate: a python date/time in UTC (see time module - actually a tuple of nine integers)
                after which to accept files.  If None, do not reject any files.

                endDate: a python date/time in UTC (see time module - actually a tuple of nine integers)
                before which to accept files.  If None, do not reject any files.

                startDayOfYear: a Julian day number (1-366) after which to accept files from
                any given year.  This can be used for example to only allow files from Spring,
                which has a start day of year of 80 (March 21). If None, do not
                reject any files.

                endDayOfYear: a Julian day number (1-366) before which to accept files from
                any given year.  This can be used for example to only allow files from Spring,
                which has a end day of year of 172 (June 21). If None, do not
                reject any files.

                fileList:  the list of valid file paths to which is appended any newly-found
                valid file paths
                
                publicAccessOnly: if 1, only return files marked in fileTab.txt and expTab.txt
                as public.  If 0, return all non-archive files. If 2, return public
                and public archive experiments with public files.  If 3, return all files,
                including public, private, archived, and private archived. 

                enforcePathConvention: if 1, only return experiments whose path is of the form
                convention 1999/mlh/20jan98d (YYYY/<three lower case letters>/DDmmmYY<optional
                single character>. If 0 (the default), only require paths to be in the form 1999/mlh/*.

                includeNonDefault: if 1, also include data files that are not default files
                in they match all other criteria. If 0 (the default), exclude non-default.

                includeNonMadrigal: if 1, include all experiment files that are not in
                fileTab.txt.  If 0, (the default) limit data files to those in fileTab.txt.

                appendKinst: if 1, append kind of instrument integer to each file name, so that
                what is returned is a list of (file name, inst code) tuples. If 0 (the default),
                do not append instrument code; return a list of file names.

                appendStartTime: if 1, append start time in seconds since 1950 to each file name, so that
                what is returned is a list of (file name, startTime) tuples. If 0 (the default),
                do not append startTimes.

                includeRealtime: if 1, include realtime files even if includeNonDefault = 0.  If 0
                (default), do not include realtime if includeNonDefault = 0.
                
            dirname: the directory name of the present directory

            names:  the list of filenames in the present directory
        
        Returns: None.

        Affects: Adds full file paths of any data files found that meet all filter criteria

        Exceptions: None
        """

        # constants - arg order
        __expName           = 0
        __kinstList         = 1
        __kindatList        = 2
        __startDate         = 3
        __endDate           = 4
        __startDayOfYear    = 5
        __endDayOfYear      = 6
        __fileList          = 7
        __pubAccessOnly     = 8
        __enforcePath       = 9
        __includeNonDef     = 10
        __includeNonMad     = 11
        __appendKinst       = 12
        __appendStartTime   = 13
        __includeRealtime   = 14

        # regular expression that enforces dir path convention
        # note that the __dirConvStr2 is the old convention that experiment directories be in the form DDmmmYY[char]
        __dirConvStr1 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[a-zA-Z0-9\-_]*$'
        __dirConvStr2 = '/experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[0-3][0-9][a-z][a-z0-9][a-z0-9][0-9][0-9].?'

        # ignore any directory without expTab.txt and fileTab.txt if not includeNonMadrigal
        if 'expTab.txt' in names:
            self.__hasExpTab = 1
        else:
            self.__hasExpTab = 0

        if 'fileTab.txt' in names:
            self.__hasFileTab = 1
        else:
            self.__hasFileTab = 0
            
        if arg[__includeNonMad] == 0:
            if not self.__hasExpTab or not self.__hasFileTab:
                return


        # apply all filters relating to expTab.txt if self.__hasExpTab
        if self.__hasExpTab:
            
            # get exp metadata
            expMeta = MadrigalExperiment(self, dirname + '/' + 'expTab.txt')

            # apply expName filter
            if arg[__expName] != None:

                expName = expMeta.getExpNameByPosition()

                # since we are using file name matching, all spaces are
                # converted to underscores
                expName = expName.replace(' ', '_')
                expNameArg = arg[__expName].replace(' ', '_')

                # try to match (case insensitive)
                if not fnmatch.fnmatch(expName.lower(), expNameArg.lower()):
                    # stop going down tree
                    names[:] = []
                    return

            # apply kinstList filter
            expKinst = expMeta.getKinstByPosition()
            
            if arg[__kinstList] != None:
                if expKinst not in arg[__kinstList]:
                    # stop going down tree
                    names[:] = []
                    return

            # apply date filters

            expStartDate = expMeta.getExpStartDateTimeByPosition()
            
            if expStartDate is None:
                raise IOError('Error getting expStartDate from dir %s' % (dirname))

            expStartUt = madrigal.metadata.getMadrigalUTFromDate(expStartDate[0],
                                                        expStartDate[1],
                                                        expStartDate[2],
                                                        expStartDate[3],
                                                        expStartDate[4],
                                                        expStartDate[5],
                                                        0)

            expEndDate = expMeta.getExpEndDateTimeByPosition()

            expEndUt = madrigal.metadata.getMadrigalUTFromDate(expEndDate[0],
                                                      expEndDate[1],
                                                      expEndDate[2],
                                                      expEndDate[3],
                                                      expEndDate[4],
                                                      expEndDate[5],
                                                      0)

            if arg[__startDate] != None:

                time1 = madrigal.metadata.getMadrigalUTFromDate(arg[__startDate][0],
                                                       arg[__startDate][1],
                                                       arg[__startDate][2],
                                                       arg[__startDate][3],
                                                       arg[__startDate][4],
                                                       arg[__startDate][5],
                                                       0)
                if time1 > expEndUt:
                    # stop going down tree
                    names[:] = []
                    return

            # save time1 in case we need to appendStartTime
            thisStartTime = expStartUt

                
            # apply endDate filter
            if arg[__endDate] != None:


                time2 = madrigal.metadata.getMadrigalUTFromDate(arg[__endDate][0],
                                                       arg[__endDate][1],
                                                       arg[__endDate][2],
                                                       arg[__endDate][3],
                                                       arg[__endDate][4],
                                                       arg[__endDate][5],
                                                       0)

                if time2 < expStartUt:
                    # stop going down tree
                    names[:] = []
                    return


            # apply startDayOfYear filter
            if arg[__startDayOfYear] != None:

                expStartDate = expMeta.getExpStartDateTimeByPosition()

                if expStartDate[7] < arg[__startDayOfYear]:
                    # index 7 refers to Day of year
                    # stop going down tree
                    names[:] = []
                    return


            # apply endDayOfYear filter
            if arg[__endDayOfYear] != None:

                expEndDate = expMeta.getExpEndDateTimeByPosition()

                if expEndDate[7] > arg[__endDayOfYear]:
                    # index 7 refers to Day of year
                    # stop going down tree
                    names[:] = []
                    return

            # check experiment access
            thisSecurity = expMeta.getSecurityByPosition()
            if thisSecurity == -1:
                # stop going down tree
                names[:] = []
                return
            elif arg[__pubAccessOnly] == 0:
                if thisSecurity not in (0,1):
                    # stop going down tree
                    names[:] = []
                    return
            elif arg[__pubAccessOnly] == 1:
                if thisSecurity != 0:
                    # stop going down tree
                    names[:] = []
                    return
            elif arg[__pubAccessOnly] == 2:
                if thisSecurity not in (0,2):
                    # stop going down tree
                    names[:] = []
                    return

        else:
            # no expTab.txt - set kinst to None
            expKinst = None
            
        # now reject paths not matching present path convention if enforcePathConvention == 0
        if arg[__enforcePath] == 0:
            reDirPath = re.compile(self.getMadroot() + __dirConvStr1)
            match = reDirPath.search(dirname)
            if match == None:
                return

        # reject paths not matching old path convention if enforcePathConvention != 0
        if arg[__enforcePath] != 0:
            reDirPath = re.compile(self.getMadroot() + __dirConvStr2)
            match = reDirPath.search(dirname)
            if match == None:
                return
            
        # experiment is okay, now start looking at individual files
        if self.__hasFileTab:
            fileMeta = MadrigalMetaFile(self, dirname + '/' + 'fileTab.txt')

            # get numFiles
            numFiles = fileMeta.getFileCount()

            # loop through each file, and add it if okay
            fileCount = 0
            while fileCount < numFiles:
                
                # skip non-default and non-realtime files if not includeNonDef
                if fileMeta.getCategoryByPosition(fileCount) not in (1,4) and arg[__includeNonDef] == 0:
                    fileCount = fileCount + 1
                    continue

                # skip realtime files if not includeNonDef and not includeRealtime
                if fileMeta.getCategoryByPosition(fileCount) == 4 and arg[__includeNonDef] == 0 and arg[__includeRealtime] == 0:
                    fileCount = fileCount + 1
                    continue

                # check if file has right kindat
                if arg[__kindatList] != None:
                    if fileMeta.getKindatByPosition(fileCount) not in arg[__kindatList]:
                        fileCount = fileCount + 1
                        continue

                # check file access
                if fileMeta.getAccessByPosition(fileCount) != 0:
                    if arg[__pubAccessOnly] in (1,2):
                        fileCount = fileCount + 1
                        continue

                # the file needs to be added
                filename = fileMeta.getFilenameByPosition(fileCount)
                if arg[__appendKinst] == 0 and arg[__appendStartTime] == 0:
                    # return only file names
                    arg[__fileList].append(dirname + '/' + filename)
                elif arg[__appendStartTime] == 0:
                    # return file name, inst code tuple
                    arg[__fileList].append((dirname + '/' + filename, expKinst))
                elif arg[__appendKinst] == 0:
                    # return file name, start time tuple
                    arg[__fileList].append((dirname + '/' + filename, thisStartTime))
                else:
                    # append both
                    arg[__fileList].append((dirname + '/' + filename, expKinst, thisStartTime))
                fileCount = fileCount + 1

            # now add all non-madrigal files if includeNonMad
            if arg[__includeNonMad] == 1:
                for name in names:
                    # skip dir names
                    if os.path.isdir(dirname + '/' + name):
                        continue
                    # if its not in fileTab.txt, add it
                    if fileMeta.getExpIdByFilename(name) == None:
                        if arg[__appendKinst] == 0 and arg[__appendStartTime] == 0:
                            # return only file names
                            arg[__fileList].append(dirname + '/' + name)
                        elif arg[__appendStartTime] == 0:
                            # return file name, inst code tuple
                            arg[__fileList].append((dirname + '/' + name, expKinst))
                        elif arg[__appendKinst] == 0:
                            # return file name, start time tuple
                            arg[__fileList].append((dirname + '/' + name, thisStartTime))
                        else:
                            # append both
                            arg[__fileList].append((dirname + '/' + filename, expKinst, thisStartTime))

        # else the file has no fileTab.txt and includeNonMad is true - include all files
        elif arg[__includeNonMad] == 1:
            for name in names:
                # skip dir names
                if os.path.isdir(dirname + '/' + name):
                    continue
                if arg[__appendKinst] == 0 and arg[__appendStartTime] == 0:
                    # return only file names
                    arg[__fileList].append(dirname + '/' + name)
                elif arg[__appendStartTime] == 0:
                    # return file name, inst code tuple
                    arg[__fileList].append((dirname + '/' + name, expKinst))
                elif arg[__appendKinst] == 0:
                    # return file name, start time tuple
                    arg[__fileList].append((dirname + '/' + name, thisStartTime))
                else:
                    # append both
                    arg[__fileList].append((dirname + '/' + filename, expKinst, thisStartTime))
                

    def __setFileAccess(self, arg, dirname, names):
        """setFileAccess is a private helper function called by os.path.walk in setFileAccess.

        Inputs:

            arg: either 0 for public access, or 1 for private access.

            dirname: the directory name of the present directory

            names:  the list of filenames in the present directory
        
        Returns: None

        Affects: sets all fileTab.txt and expTab.txt files in dirname to be public or private, depending on arg.

        Exceptions: None.
        """

        for name in names:
            if name == 'fileTab.txt':
                try:
                    fileMetaObj = madrigal.metadata.MadrigalMetaFile(self, dirname + '/' + name)
                    fileMetaObj.setAccess(arg)
                    
                except madrigal.admin.MadrigalError as e:
                    print(e.getExceptionStr()) 
                    
            elif name == 'expTab.txt':
                try:
                    expObj = madrigal.metadata.MadrigalExperiment(self, dirname + '/' + name)
                    expObj.setSecurityByPosition(0, arg)
                    expObj.writeMetadata()
                    
                except madrigal.admin.MadrigalError as e:
                    print(e.getExceptionStr()) 

                

class MadrigalSite:
    """MadrigalSite is an object that provides access to Madrigal site info from the metadata.

    This object provides access to all Madrigal site information in the metadata file siteTab.txt.

    Usage example::

        import madrigal.metadata
        import madrigal.admin

        try:
    
            siteObj = madrigal.metadata.MadrigalSite()

            print siteObj.getSiteName(1)

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
	
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 9, 2001

    """

    #constants
    __siteMetadataFile  = "siteTab.txt"
    _defaultVersion = '2.6'

    # column positions
    __siteIDCol         =  0
    __siteNameCol       =  1
    __madServerCol      =  2
    __madDocRootCol     =  3
    __madCGICol         =  4
    __madServletCol     =  5
    __contactNameCol    =  6
    __contactAddr1Col   =  7
    __contactAddr2Col   =  8
    __contactAddr3Col   =  9
    __contactCityCol    =  10
    __contactStateCol   =  11
    __contactZipCol     =  12
    __contactCountryCol =  13
    __contactPhoneCol   =  14
    __contactEmailCol   =  15
    __siteVersionCol    =  16
    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalSite by reading from siteTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/siteTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get site metadata file
        if (initFile == None):
            self.__filename = self.__siteMetadataFile
        else:
            self.__filename = initFile
            
        # MadrigalExperiment can now legal have two different lengths - with or without version
        allowedLens = (self.__contactEmailCol+1, self.__siteVersionCol+1)

        self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB, allowedLens).getList()
        
        

    def getSiteName(self, siteID):
        """getSiteName returns the site name that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site name that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__siteNameCol]

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # not found
        return(None)


    def getSiteServer(self, siteID):
        """getSiteServer returns the site server (e.g., www.haystack.mit.edu) that matches siteID argument, or None if not found.

        Inputs: siteID integer to get site server.
        
        Returns: the site server that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__madServerCol]

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # not found
        return(None)


    def getSiteDocRoot(self, siteID):
        """getSiteDocRoot returns the relative document root (e.g. madrigal)  that matches siteID argument, or None if not found.

        Inputs: siteID integer to get document root path.
        
        Returns: the document root path that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__madDocRootCol]

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # not found
        return(None)



    def getSiteRelativeCGI(self, siteID):
        """getSiteRelativeCGI returns the relative cgi path (e.g.cgi-bin/madrigal)  that matches siteID argument, or None if not found.
        
        For Madrigal 3.0 sites and later, this returns getSiteDocRoot, and the cgi field is ignored.

        Inputs: siteID integer to get relative cgi path.
        
        Returns: the relative cgi path that matches siteID argument, or None if not found. Not meaningful for Madrigal 3.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        
        version = self.getSiteVersion(siteID)
        if packaging.version.parse(version) >= packaging.version.parse('3.0'):
            return(self.getSiteDocRoot(siteID))
        

        # this code only runs for Madrigal 2.0
        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__madCGICol]

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # not found
        return None
    
    
    def getSiteContactName(self, siteID):
        """getSiteContactName returns the site contact name that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact name that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactNameCol]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    
    
    def getSiteAddress1(self, siteID):
        """getSiteAddress1 returns the site address 1 that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact address 1 that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactAddr1Col]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    
    
    def getSiteAddress2(self, siteID):
        """getSiteAddress2 returns the site address 2 that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact address 2 that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactAddr2Col]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSiteAddress3(self, siteID):
        """getSiteAddress3 returns the site address 3 that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact address 3 that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactAddr3Col]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSiteCity(self, siteID):
        """getSiteCity returns the site city that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact city that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactCityCol]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSiteState(self, siteID):
        """getSiteState returns the site state that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact state that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactStateCol]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSitePostalCode(self, siteID):
        """getSitePostalCode returns the site postal code that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact postal code that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactZipCol]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSiteCountry(self, siteID):
        """getSiteCountry returns the site country that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact country that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactCountryCol]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSiteTelephone(self, siteID):
        """getSiteTelephone returns the site telephone that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact telephone that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactPhoneCol]
            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        # not found
        return(None)
    

    def getSiteEmail(self, siteID):
        """getSiteEmail returns the site email address that matches siteID argument, or None if not found.

        Inputs: siteID integer to get Site email address.
        
        Returns: the site email address that matches siteID argument, or None if not found.  To list multiple
        email addresses in this field separate them with semicolons (since commas are delimiters).  getSiteEmail
        will automatically replace semicolons with commas, as required by multiple email addresses.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    return site[self.__contactEmailCol].replace(';', ',')

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # not found
        return(None)


    def getSiteList(self):
        """getSiteList returns a list of all site ids and names.

        Inputs: None.
        
        Returns: a list of all site ids and names.  Each item in the list
        is a tuple of the form (Site id (integer), site name (string)).  Example item:
        (1,'Millstone')

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        retList = []

        for site in self.__fileList:
            try:
                item = (int(site[self.__siteIDCol]),
                        site[self.__siteNameCol])
                retList.append(item)
                
            except:
                raise madrigal.admin.MadrigalError('Error in siteTab.txt parsing metadata row: ' + str(inst),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        return retList
    
    
    def getSiteVersion(self, siteID):
        """getSiteVersion returns the site version string that matches siteID argument, or None if not found.

        Inputs: siteID integer
        
        Returns: the site version that matches siteID argument, or None if not found.  If file does
            not have this field, returns default value of 2.6.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        for site in self.__fileList:
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    if len(site) >= self.__siteVersionCol + 1:
                        return site[self.__siteVersionCol]
                    else:
                        return(self._defaultVersion)

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # not found
        return(None)
    
    
    
    def setSiteVersionBySiteID(self, siteID, version):
        """setSiteVersionBySiteID sets the site Madrigal version (string) as period delimited integers 
        at given siteID.

        Inputs:

            siteID - siteID of site in list.

            version - string Madrigal version as period delimited integers (eg, 2.6)
        
        Returns: None.

        Affects: sets the Madrigal version as period delimited integers (eg, 2.6) at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        siteID not found.
        
        This method added in Madrigal 3.0
        """
        # verify no illegal commas
        if version.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setSiteVersionByPosition with args %s: %s' %  \
                                               (str(position, version),
                                                [traceback.format_exc()]))
            
        # parse test
        version = version.strip()
        try:
            siteID = int(siteID)
            if len(version) == 0:
                raise ValueError('')
            for item in version.split('.'):
                int(item)
        except:
            raise madrigal.admin.MadrigalError('Error in setSiteVersionByPosition with args %s: %s' %  \
                                               (str(position, version),
                                                [traceback.format_exc()]))
            
        rightPosition = None
        for position, site in enumerate(self.__fileList):
            # find matching siteid
            try:
                if (int(site[self.__siteIDCol]) == siteID):
                    rightPosition = position

            except:
                raise madrigal.admin.MadrigalError('Error parsing metadata row in siteTab.txt: ' + str(site),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        if rightPosition is None:
            raise madrigal.admin.MadrigalError('siteId %i not found in siteTab.txt' % (siteID))
        
        if len(self.__fileList[position]) == self.__siteVersionCol:
            # not yet set
            self.__fileList[position].append(version)
        else:
            self.__fileList[position][self.__siteVersionCol] = version
              
                
                
    def writeMetadata(self, newFullPath=None):
        """writeMetadata writes a new version of the siteTab.txt file.
        
        Inputs: newFullPath:  a new path to write the siteTab.txt file to, if
        not the same as the original metadata file opened.  Defaults to None, which overwrites
        metadata file that was read from.
        
        Returns: None.

        Affects: Writes updated version of metadata file.

        Exceptions: If unable to write file
        """

        # create string to hold file
        metaFileStr = ''
        delimiter = ','

        for lineList in self.__fileList:
            metaFileStr += delimiter.join(lineList) + '\n'

        # try to write file, if not, raise exception

        try:

            if newFullPath == None:
                if (len(os.path.dirname(self.__filename)) != 0):
                    newFullPath = self.__filename
                else:
                    newFullPath = self.__madDB.getMetadataDir() + "/" + self.__filename
            newFile = open(newFullPath, "w")
            newFile.write(metaFileStr)
            newFile.close()

        except:

            raise madrigal.admin.MadrigalError("Unable to write metadata file " + \
                                               str(newFullPath),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))



class MadrigalInstrument:
    """MadrigalInstrument is an object that provides access to Madrigal instrument info from the metadata.

    This object provides access to all Madrigal instrument information in the metadata files instTab.txt
    and instType.Tab.
    
    key = self.__instKinstCol (used in searches)

    Usage example::

        import madrigal.metadata
        
        import madrigal.admin

        try:
    
            test = madrigal.metadata.MadrigalInstrument()

            print test.getInstrumentName(30)

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 9, 2001

    """

    #constants
    __instMetadataFile  = "instTab.txt"

    # column positions
    __instKinstCol      =  0
    __instMnemonicCol   =  1
    __instNameCol       =  2
    __latitudeCol       =  3
    __longitudeCol      =  4
    __altitudeCol       =  5
    __contactNameCol    =  6
    __contactAddr1Col   =  7
    __contactAddr2Col   =  8
    __contactAddr3Col   =  9
    __contactCityCol    =  10
    __contactStateCol   =  11
    __contactZipCol     =  12
    __contactCountryCol =  13
    __contactPhoneCol   =  14
    __contactEmailCol   =  15
    __categoryCol       =  16

    # instType.txt file
    __inst2MetadataFile  = "instType.txt"

    # column positions
    __inst2CategoryIdCol    =  0
    __inst2CategoryDescCol  =  1
    

    def __init__(self, madDB=None, initFile=None, init2File=None):
        """__init__ initializes MadrigalInstrument by reading from instTab.txt (or initFile)
        and instType.txt file (or init2File).

        Inputs:

            madDB - Existing MadrigalDB object, by default = None.

            initFile - String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/instTab.txt.

            init2File - String representing the full path to the metadata file instType.txt. Default is
            None, in which case file read is MadrigalDB.getMetadataDir()/instType.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        Note that the instTab.txt file was updated with the release of the madrigal python api, and this
        function will throw an error if used with the old file.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get instrument metadata file
        if (initFile == None):
            self.__filename = self.__instMetadataFile
        else:
            self.__filename = initFile
            
        genericObj = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB, key=self.__instKinstCol)

        self.__fileList = genericObj.getList()
        self._dict = genericObj.getDict()

        # get instrument metadata file 2
        if (init2File == None):
            self.__filename2 = self.__inst2MetadataFile
        else:
            self.__filename2 = initFile

        self.__fileList2 = madrigal.metadata.MadrigalMetadata(self.__filename2, self.__madDB).getList()


    def getInstrumentName(self, kinst):
        """getInstrumentName returns the instrument name that matches kinst argument, or None if not found.

        Inputs: kinst integer to get instrument name.
        
        Returns: the instrument name that matches kinst argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        return(self.__fileList[position][self.__instNameCol])


    def getInstrumentMnemonic(self, kinst):
        """getInstrumentMnemonic returns the 3 char instrument mnemonic that matches kinst argument, or None if not found.

        Inputs: kinst integer to get instrument mnemonic.
        
        Returns: the instrument mnemonic that matches kinst argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        return(self.__fileList[position][self.__instMnemonicCol])


    def getLatitude(self, kinst):
        """getLatitude returns the latitude as a float that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get latitude.
        
        Returns: the latitude as a float that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        try:
            return(float(self.__fileList[position][self.__latitudeCol]))
        except ValueError:
            return(None)


    def getLongitude(self, kinst):
        """getLongitude returns the longitude as a float that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get longitude.
        
        Returns: the longitude as a float that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        try:
            return(float(self.__fileList[position][self.__longitudeCol]))
        except ValueError:
            return(None)
	
	
    def getAltitude(self, kinst):
        """getAltitude returns the altitude as a float that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get altitude.
        
        Returns: the altitude in km above sea level as a float that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        try:
            return(float(self.__fileList[position][self.__altitudeCol]))
        except ValueError:
            return(None)
    
    
    def getContactName(self, kinst):
        """getContactName returns the contact name as a string that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get contact name.
        
        Returns: the contact name as a string that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        
        This method added in Madrigal 2.6
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        return(self.__fileList[position][self.__contactNameCol])
    
    
    def getContactAddress1(self, kinst):
        """getContactAddress1 returns the contact address 1 field (institution) as a string that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get contact name.
        
        Returns: the contact address 1 as a string that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        
        This method added in Madrigal 3
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        return(self.__fileList[position][self.__contactAddr1Col])
    
    
    
    def getContactEmail(self, kinst):
        """getContactEmail returns the contact email as a string that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get contact email.
        
        Returns: the contact email as a string that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        
        This method added in Madrigal 2.6
        """
        try:
            position = self._dict[str(kinst)]
        except KeyError:
            return(None)
        return(self.__fileList[position][self.__contactEmailCol])


    def getCategory(self, kinst):
        """getCategory returns the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Inputs: kinst integer to get altitude.
        
        Returns: the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        categoryId = None
        try:
            position = self._dict[str(kinst)]
            categoryId = int(self.__fileList[position][self.__categoryCol])
        except KeyError:
            categoryId = None

        if categoryId == None:
            return(None)

        # now loop until categoryId found
        for inst2 in self.__fileList2:
            try:
                if (int(inst2[self.__inst2CategoryIdCol]) == categoryId):
                    return(inst2[self.__inst2CategoryDescCol])
            except:
                raise madrigal.admin.MadrigalError('Error in instType.txt parsing metadata row: ' + str(inst2),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        # type not found
        return(None)



    def getCategoryId(self, kinst):
        """getCategory returns the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Inputs: kinst integer to get altitude.
        
        Returns: the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        try:
            position = self._dict[str(kinst)]
            categoryId = int(self.__fileList[position][self.__categoryCol])
        except KeyError:
            categoryId = None
        return(categoryId)

    

    def getInstrumentList(self):
        """getInstrumentList returns a list of all instrument names, mnemonics, and their kinst values.

        Inputs: None.
        
        Returns: a list of all instrument names, mnemonics, and their kinst values.  Each item in the list
        is a tuple of the form (Instrument Name (string), mnemonic (string), kinst (integer)).  Example item:
        ('Millstone Hill UHF Steerable Antenna', 'mlh', 31)

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        retList = []

        for inst in self.__fileList:
            try:
                item = (inst[self.__instNameCol],
                        inst[self.__instMnemonicCol],
                        int(inst[self.__instKinstCol]))
                retList.append(item)
                
            except:
                raise madrigal.admin.MadrigalError('Error in instTab.txt parsing metadata row: ' + str(inst),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        return retList


    def getOrderedInstrumentList(self):
        """getOrderedInstrumentList returns a list of all (instrument names, mnemonics, kinst, categories, categoryId),
        ordered by categoryId and then kinst.

        Inputs: None.
        
        Returns: a list of tuples of (instrument name, mnemonic, kinst, category, categoryId) ordered by categoryId and
        then kinst.    Example item:
        ('Millstone Hill UHF Steerable Antenna', 'mlh', 31, 'Incoherent Scatter Radars', 1)

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        retList = []

        for inst in self.__fileList:
            try:
                kinst = int(inst[self.__instKinstCol])
                item = (inst[self.__instNameCol],
                        inst[self.__instMnemonicCol],
                        kinst,
                        self.getCategory(kinst),
                        self.getCategoryId(kinst))
                retList.append(item)
                
            except:
                raise madrigal.admin.MadrigalError('Error in instTab.txt parsing metadata row: ' + str(inst),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        retList.sort(key=self.__instrumentSort) 
        return retList


    def getOrderedInstrumentListWithData(self, isTrusted, localOnly=False,
                                         localExpObj=None, globalExpObj=None,
                                         allowArchive=False, requireFiles=False,
                                         requireFilesOrPlots=False):
        """getInstrumentList returns information about which instruments have local and global data.

        Inputs:

                isTrusted - True if client is trusted, False otherwise

                localOnly - if False (the default), will return information about both local and global data.
                    If True, then global data ignored.

                localExpObj - an MadrigalExperiment object for the local data.  If None (the default),
                    will be created.

                globalExpObj - an MadrigalExperiment object for the global data.  If None (the default),
                    will be created if not localOnly.

                allowArchive - if True, allow experiments marked as security==2(public arcive), and if
                    isTrusted, allow security==3(private archive)
                    
                requireFiles - if True, only include experiments with Madrigal data files.  If False
                    (default), not not require that.
                    
                requireFilesOrPlots - if True, only include experiments with Madrigal data files or plots.  
                    If False (default), not not require that.
        
        
        Returns: an ordered tuple of two items:
            1. a list of tuples of (instName, localStartYear, localEndYear, globalStartYear,
               globalEndYear, kinst, categoryId), ordered by by categoryId, then kinst.  If
               localOnly, globalStartYear and globalEndYear = 0.  If not local only, localStartYear
               and localEndYear will be zero if no local data for that instrument.
            2. categoryDict - key = categoryId, value = category Description

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        if (not localOnly) and (globalExpObj==None):
            globalExpObj = madrigal.metadata.MadrigalExperiment(self.__madDB,
                                                                os.path.join(self.__madDB.getMadroot(),
                                                                             'metadata/expTabAll.txt'))
            
        if localExpObj == None:
            localExpObj = madrigal.metadata.MadrigalExperiment(self.__madDB)

        orderInstList = []
        categoryDict = {}
        
        # next task - create local dict and global dict of key=kinst, value = (startYear, endYear) tuple
        localInstDict = {}
        globalInstDict = {}
        
        # Skip test experiments
        testExp = ('1998/mlh/20jan98', '1997/aro/06jan97', '1997/lyr/08apr97',
                   '1997/son/06jan97', '1995/jro/01feb95', '1998/jro/27apr98')
        
        for i in range(localExpObj.getExpCount()):
            thisUrl = localExpObj.getExpUrlByPosition(i)
            if thisUrl[-16:] in testExp:
                continue
            thisSecurity = localExpObj.getSecurityByPosition(i)
            if thisSecurity == -1:
                # blocked to all
                continue
            if thisSecurity == 1 and not isTrusted:
                # no access
                continue
            if thisSecurity == 2 and not allowArchive:
                # no access
                continue
            if thisSecurity == 3 and ((not isTrusted) or (not allowArchive)):
                # no access
                continue
            
            kinst = localExpObj.getKinstByPosition(i)
            startYear = localExpObj.getExpStartDateTimeByPosition(i)[0]
            endYear = localExpObj.getExpEndDateTimeByPosition(i)[0]
            if kinst in localInstDict:
                oldStartYear, oldEndYear = localInstDict[kinst]
                if startYear >= oldStartYear and endYear <= oldEndYear:
                    # no new info
                    continue
            
            expDir = localExpObj.getExpDirByPosition(i)
                
            if requireFilesOrPlots:
                links = localExpObj.getExpLinksByPosition(i)
                if len(links) == 0:
                    try:
                        size = os.path.getsize(os.path.join(expDir, 'fileTab.txt'))
                        if size < 3:
                            continue
                    except:
                        continue
            
            if requireFiles:
                try:
                    size = os.path.getsize(os.path.join(expDir, 'fileTab.txt'))
                    if size < 3:
                        continue
                except:
                    continue
            
            if kinst in localInstDict:
                localInstDict[kinst] = (min(startYear, oldStartYear), max(endYear, oldEndYear))
            else:
                localInstDict[kinst] = (startYear, endYear)


        if not localOnly:
            for i in range(globalExpObj.getExpCount()):
                kinst = globalExpObj.getKinstByPosition(i)
                startYear = globalExpObj.getExpStartDateTimeByPosition(i)[0]
                endYear = globalExpObj.getExpEndDateTimeByPosition(i)[0]
                thisUrl = globalExpObj.getExpUrlByPosition(i)
                thisSecurity = globalExpObj.getSecurityByPosition(i)
                thisSite = globalExpObj.getExpSiteIdByPosition(i)
                if thisUrl[-16:] in testExp:
                    continue
                if kinst not in globalInstDict:
                    globalInstDict[kinst] = (startYear, endYear)
                    continue
                oldStartYear, oldEndYear = globalInstDict[kinst]
                if startYear >= oldStartYear and endYear <= oldEndYear:
                    # no new info
                    continue
                if thisSecurity != 0 and self.__madDB.getSiteID() != thisSite:
                    # no search remote sites for non-public experiments allowed
                    continue
                if requireFiles:
                    if globalExpObj.getExpIdByPosition(i) not in expIdList:
                        continue
                globalInstDict[kinst] = (min(startYear, oldStartYear), max(endYear, oldEndYear))
        
        # populate orderInstList and categoryDict
        instList = self.getOrderedInstrumentList() 
        
        for inst in instList:
            if not localOnly:
                if inst[2] in globalInstDict:
                    if inst[4] not in categoryDict:
                        categoryDict[inst[4]] = inst[3]
                    globalStartYear, globalEndYear = globalInstDict[inst[2]]
                    try:
                        localStartYear, localEndYear = localInstDict[inst[2]]
                    except KeyError:
                        localStartYear, localEndYear = (0, 0)
                    orderInstList.append((inst[0], localStartYear, localEndYear,
                                               globalStartYear, globalEndYear, inst[2], inst[4]))
            else:
                if inst[2] in localInstDict:
                    if inst[4] not in categoryDict:
                        categoryDict[inst[4]] = inst[3]
                    globalStartYear = 0
                    globalEndYear = 0
                    localStartYear, localEndYear = localInstDict[inst[2]]
                    orderInstList.append((inst[0], localStartYear, localEndYear,
                                               globalStartYear, globalEndYear, inst[2], inst[4]))

        return ((orderInstList, categoryDict))


    def __instrumentSort(self, thisInst):
        """instrumentSort is a private method used to sort tuples of instrument data
        """
        return((thisInst[4], thisInst[2]))


class MadrigalInstrumentParameters:
    """MadrigalInstrumentParameters is an object that provides access to the metadata file that summarizes the parameters associated with each instrument.

    This object provides access to all Madrigal instrument parameter information in the metadata file instParmTab.txt.
    The metadata file instParmTab.txt lists, for any given instrument, all the measured parameters found in all the
    data files in the database associated with that instrument.

    This class also contains a method to rebuild the table instParmTab.txt by examining every data file in the database.
    This is presumably a slow process and should be done in the background.

    Usage example::

        import madrigal.metadata
        
        import madrigal.admin

        try:
    
            test = madrigal.metadata.MadrigalInstrumentParameters()

            print test.getParameters(30)

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 17, 2002

    """

    #constants
    __instParmMetadataFile  = "instParmTab.txt"

    # column positions
    __instParmKinstCol  =  0
    __instParmListCol   =  1

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalInstrumentParameters by reading from instParmTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/instParmTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        Note that the instParmTab.txt file was new with the release of the madrigal python api, and this
        function will throw an error if file not there.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get instrument parameter metadata file
        if (initFile == None):
            self.__filename = self.__instParmMetadataFile
        else:
            self.__filename = initFile

        self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB).getList()


        
    def getParameters(self, kinst):
        """getParameters returns a list of parameters in mnemonic form (strings or unknown integers as strings) that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get parameters.  If 0, get parameters from all instruments.
        
        Returns: a list of mnemonic strings or unknown integer strings, or None if kinst not found or blank.

        Affects: None

        Exceptions: if error in metadata file
        """
        
        parmList = []

        for inst in self.__fileList:
            # find matching kinst
            try:
                if (int(inst[self.__instParmKinstCol]) == kinst or kinst == 0):
                    if len(inst[self.__instParmListCol]) == 0:
                        continue
                    tempList = inst[self.__instParmListCol].split()
                    for parm in tempList:
                        if not parm.lower() in parmList:
                            if parm.find('-32767') == -1:
                                parmList.append(parm.lower())
                
            except:
                raise madrigal.admin.MadrigalError('Error in instTab.txt parsing metadata row: ' + str(inst),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        if len(parmList) == 0:
            return(None)
        return parmList


    def rebuildInstParmTable(self, completeRebuildFlag = 0):
        """rebuildInstParmTable rebuilds the instParmTab.txt metadata file.

        The table instParmTab.txt is a listing of every measured parameter found in any data file for a given
        instrument. It now will also import data from other Madrigal sites instParmTab.txt files.
        Since these files are constantly updated, this table needs to be updated on a regular
        basis.  This methods works in one of two ways, depending on the value of completeRebuildFlag and whether
        a file called instParmLastUpdate.txt exists in the metadata directory.

        If completeRebuildFlag = 1 or instParmLastUpdate.txt does not exist, the method rebuildInstParmTable
        loops through each instrument in the instTab.txt.  For each
        instrument, it loops through every data file associated with that instrument.  For every data file, it
        gets the list of parameters in that file, and adds them to the list for that instrument if they are unique.
        Since this process involves every file in the database, it may take a great deal of time and should
        be run in the background.

        If completeRebuildFlag = 0 and instParmLastUpdate.txt does exist, the method rebuildInstParmTable
        first stores all the existing parameters from the instParmTab.txt.  It reads the date of the last update
        from instParmLastUpdate.txt, and only reads data files newer than that date that are associated with
        each instrument. For every new data file, it gets the list of parameters in that file, and adds
        them to the list for that instrument if they are unique.  This makes rebuildInstParmTable faster,
        but possibly keeps invalid parameters if experiments are ever deleted.

        Finally, the instParmTab.txt file of every other site is obtained via getMetadata, and those parameters are
        also added.

        Inputs: completeRebuildFlag: if 0 (the default), only add parameters from new files, where new means
        newer than the date in the instParmLastUpdate.txt file (stored as a float).  If 1, rebuild the table completely.  This will
        eliminate any parameters no longer found in files, but will take longer.  If no instParmLastUpdate.txt
        file is found, the table is always rebuilt completely.
        
        Returns: None.

        Affects: Writes file instParmTab.txt in metadata directory

        Exceptions: If unable to write instParmTab.txt file or the instParmLastUpdate.txt file.
        """
        # get full file name
        filename = self.__madDB.getMetadataDir() + '/' + self.__filename
        
        # throw an exception if instParmTab.txt file not writable
        if not os.access(filename, os.W_OK):
            raise madrigal.admin.MadrigalError('Unable to write: ' + str(filename), None)

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.__madDB.getMadroot()

	# now its safe to import madrigal.data since MAD + ROOT set
        import madrigal.data

	# get date of files to examine
        if completeRebuildFlag == 0:
            # try to open and read instParmLastUpdate.txt
            try:
                dateFile = open(self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt', 'r')
                lastUpdateTime = float(dateFile.read())
                dateFile.close()
                
            except:
                # if failure, set time to 1/1/1970 and set completeRebuildFlag = 1
                lastUpdateTime = time.mktime((1970,1,1,0,0,0,0,0,0))
                completeRebuildFlag = 1
        else:
            # set time to 1/1/1971
            lastUpdateTime = time.mktime((1971,1,1,0,0,0,0,0,0))

        # now write new version of instParmLastUpdate.txt
        # if not writable, exception thrown
        try:
            dateFile = open(self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt', 'w')
        except:
            raise madrigal.admin.MadrigalError('Unable to write: ' + \
                                               self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt',
                                               None)
        newUpdateTime = time.time()
        dateFile.write(str(newUpdateTime))
        dateFile.close()
        # finally, make sure new instParmLastUpdate.txt is world-writable
        try:                
            os.chmod(self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt', 0o666)
        except:
            pass

        # create a needed MadrigalParameters obj
        madParmObj = madrigal.data.MadrigalParameters(self.__madDB)

        # create a needed MadrigalSite obj
        madSiteObj = madrigal.metadata.MadrigalSite(self.__madDB)

        # create a string to hold all the text for the new file
        newFileStr = ''
        
        # create a dictionary with all instrument codes as keys
        instObj = madrigal.metadata.MadrigalInstrument(self.__madDB)
        instList = instObj.getInstrumentList()

        instCodeDict = {}
        for inst in instList:
            if inst[2] == 0:
                continue
            if inst[2] in list(instCodeDict.keys()):
                continue
            if completeRebuildFlag == 1:
                # start with empty list
                instCodeDict[inst[2]]= []
            else:
                # start with present list
                tempParmList = []
                oldParmList = self.getParameters(inst[2])
                if oldParmList != None:
                    for item in oldParmList:
                        # check that its not -32767
                        try:
                            if madParmObj.getParmCodeFromMnemonic(item) == -32767:
                                continue
                        except ValueError:
                            print(('skipping parm %s since no longer valid' % (str(item))))
                            continue
                        tempParmList.append(madParmObj.getParmCodeFromMnemonic(item))
                    instCodeDict[inst[2]] = tempParmList
                else:
                    instCodeDict[inst[2]] = []


        # get a list of all default and realtime files 
        filelist = self.__madDB.getFileListFromMetadata(appendKinst = 1,
                                                        includeRealtime = 1) # appendKinst, allow realtime


        # loop through each file - file is tuple of
        # (file name, inst code)
        for file in filelist:
            # check whether file should be skipped
            try:
                if lastUpdateTime > os.stat(file[0])[8]:
                    continue
            except:
                continue
            
            # try to create MadrigalFile object for that file
            try:
                print('\t\tAdding parameters to instParmTab.txt from ' + str(file))
                madFileObj = madrigal.data.MadrigalFile(file[0], self.__madDB)
                fileParmList = madFileObj.getMeasuredParmList()
                # append unique parameters
                for parm in fileParmList:
                    if not parm in instCodeDict[file[1]]:
                        instCodeDict[file[1]].append(parm)
            # if a problem with file, skip it
            except:
                continue

 

        # now create a new file string from instCodeDict
        # create a sorted list of keys
        keyList = list(instCodeDict.keys())
        keyList.sort()
        # step through each key
        delimiter = ' '
        for key in keyList:
            # sort that instrument's list
            instParmList = madParmObj.normalizeParmList(instCodeDict[key])
            # convert from codes to mnemonics
            instParmList = madParmObj.getParmMnemonicList(instParmList)

            # append that instrument's data to newFileStr
            newFileStr = newFileStr + str(key) + ','
            newFileStr = newFileStr + delimiter.join(instParmList).lower() + '\n'

        # if no data found, throw error
        if len(newFileStr) == 0:
            raise madrigal.admin.MadrigalError('No data found for: ' + str(filename), None)
            

        # write new instParmTab.txt
        newFile = open(filename, 'w')
        newFile.write(newFileStr)
        newFile.close
                

        
    
class MadrigalKindat:
    """MadrigalKindat is an object that provides access to Madrigal kind of data info from the metadata.

    This object provides access to all Madrigal kind of data information in the metadata file typeTab.txt.

    Usage example::

        import madrigal.metadata
        import madrigal.admin

        try:
    
            test = madrigal.metadata.MadrigalKindat()

            print test.getKindatDescription(3001)

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 9, 2001

    """

    #constants
    __typeMetadataFile  = "typeTab.txt"

    # column positions
    __typeCodeCol   =  0
    __typeDescCol   =  1

    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalKindat by reading from typeTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/typeTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get kindat metadata file
        if (initFile == None):
            self.__filename = self.__typeMetadataFile
        else:
            self.__filename = initFile

        self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB).getList()


    def getKindatDescription(self, code, kinst=None):
        """getKindatDescription returns the kindat description that matches code argument, or None if not found.

        Inputs: 
        
            code integer to get kindat description. or integer as string
            
            kinst - used to look up kindat description in form '%i_%i' % (kinst, code).  If None,
                (the default) only return exact code match as integer.  If given, first try to match
                '%i_%i' % (kinst, code).  If not found, then try just code. May be integer or string integer
        
        Returns: the kindat description that matches code argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        code = int(code)
        
        if kinst != None:
            kinst = int(kinst)
            for i, type in enumerate(self.__fileList):
                # find matching kinst and code 
                try:
                    items = type[self.__typeCodeCol].split('_')
                    if len(items) == 2:
                        thisKinst = int(items[0])
                        thisKindat = int(items[1])
                        if thisKinst == kinst and thisKindat == code:
                            return type[self.__typeDescCol]
                    
                except:
                    raise madrigal.admin.MadrigalError('Error in typeTab.txt parsing metadata row %i: ' % (i) + str(type),
                                                       traceback.format_exception(sys.exc_info()[0],
                                                                                  sys.exc_info()[1],
                                                                                  sys.exc_info()[2]))
                    
        # just search for code
        for i, type in enumerate(self.__fileList):
            try:
                items = type[self.__typeCodeCol].split('_')
                if len(items) == 1:
                    thisKindat = int(items[0])
                    if thisKindat == code:
                        return type[self.__typeDescCol]
                
            except:
                raise madrigal.admin.MadrigalError('Error in typeTab.txt parsing metadata row %i: ' % (i) + str(type),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))

        # not found
        return(None)


    def getKindatList(self):
        """getKindatList returns a list of all kindat descriptions and codes.

        Inputs: None.
        
        Returns: a list of all kindat descriptions and codes.  Each item in the list
        is a tuple of the form (Kindat description (string), kindat code (integer or string in
        form '%i_%i' % (kinst, code))).  Example item:
        ('INSCAL Basic Derived Parameters', 3001)

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        retList = []

        for i, kindat in enumerate(self.__fileList):
            
            try:
                if kindat[self.__typeCodeCol].find('_') != -1:
                    raise ValueError('')
                # works if its a standard code - int no longer raises an error if underscore found
                item = (kindat[self.__typeDescCol],
                        int(kindat[self.__typeCodeCol]))
                retList.append(item)
                
            except:
                try:
                    # works for kinst_kindat
                    item = (kindat[self.__typeDescCol],
                            kindat[self.__typeCodeCol])
                    retList.append(item)
                    
                except:
                    raise madrigal.admin.MadrigalError('Error in typeTab.txt parsing metadata row %i: ' % (i) + str(kindat),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        return retList



class MadrigalInstrumentKindats:
    """MadrigalInstrumentKindats is an object that provides access to the metadata file that summarizes the kindat codes associated with each instrument.

    This object provides access to all Madrigal instrument kindat information in the metadata file instKindatTab.txt.
    The metadata file instKindatTab.txt lists, for any given instrument and year, all the kindat codes found in all the
    data files in the local database associated with that instrument.

    This class also contains a method to rebuild the table instKindatTab.txt by examining all the metadata in the database.
    This is presumably a somewhat slow process and should be done in the background.

    Usage example::

        import madrigal.metadata
        
        import madrigal.admin

        try:
    
            test = madrigal.metadata.MadrigalInstrumentKindats()

            print test.getKindatListForInstruments([20,30])

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Aug. 15, 2002

    """

    #constants
    __instKindatMetadataFile  = "instKindatTab.txt"

    # column positions
    __instKindatKinstCol  =  0
    __instKindatListCol   =  1
    __instKindatYearCol   =  2

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalInstrumentKindats by reading from instKindatTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/instKindatTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        Note that the instKindatTab.txt file was new with the release of the madrigal python api, and this
        function will throw an error if file not there.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get instrument kindat metadata file
        if (initFile == None):
            self.__filename = self.__instKindatMetadataFile
        else:
            self.__filename = initFile

        self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB).getList()


    def getKindatListForInstruments(self, kinstList):
        """getKindatListForInstruments returns a list of kindat codes as integers for the given instrument list.
        A kindat code is returned if it appears in any year

        Inputs: kinstList: a list of kinst integers to get associated kindat list. Also accepts a single integer.
        
        Returns: a list of kindat codes as integers associated with the given instrument list.

        Affects: None

        Exceptions: if error in metadata file
        """
        retKindatList = []
        
        # if kinstList is just a single integer, convert it into a list
        if type(kinstList) == int:
            kinstList = [kinstList]

        # loop through instKindatTab.txt
        for inst in self.__fileList:
            # find matching kinst
            try:
                if (int(inst[self.__instKindatKinstCol]) in kinstList):
                    if len(inst[self.__instKindatListCol]) == 0:
                        continue
                    tempList = inst[self.__instKindatListCol].split()
                    for parm in tempList:
                        if not int(parm) in retKindatList:
                           retKindatList.append(int(parm))
                
            except:
                raise madrigal.admin.MadrigalError('Error in instKindatTab.txt parsing metadata row: ' + str(inst),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        return(retKindatList)
    
    
    
    def getKindatListForInstrumentYear(self, kinst, year):
        """getKindatListForInstrumentYear returns a list of kindat codes as integers for the given instrument and year.

        Inputs: kinst, year
        
        Returns: a list of kindat codes as integers associated with the given instrument  and year,
            or None if not found.

        Affects: None

        Exceptions: if error in metadata file
        """
        retKindatList = []
        

        # loop through instKindatTab.txt
        for inst in self.__fileList:
            # find matching kinst
            try:
                if int(inst[self.__instKindatKinstCol]) != kinst:
                    continue
                if int(inst[self.__instKindatYearCol]) != year:
                    continue
                return([int(kindat) for kindat in inst[self.__instKindatListCol].split()])
                
            except:
                raise madrigal.admin.MadrigalError('Error in instKindatTab.txt parsing metadata row: ' + str(inst),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def rebuildInstKindatTable(self):
        """rebuildInstKindatTable rebuilds the instKindatTab.txt metadata file.

        The table instKindatTab.txt is a listing of every kindat found for a given
        instrument for any given year.  Since these files are constantly updated, this table needs to be updated on a regular
        basis. Data from other Madrigal sites is also imported into this table.

        How it works: For each instrument in instTab.txt, this method first creates a list
        of all the experiments that used that instrument.  It then loops
        through the file table.  For each file listing, if the file is the
        default one, and its experiment id is in the list of experiments just
        created, the kindat is added to the list if its not already there.
        Data from all other Madrigal sites is then added via getMetadata.  It then writes
        the metadata file in the form: 10, 1001 1002 1003, where the first column in the
        instrument code and the second column is a space delimited list of kindat codes.

        Inputs: None.
        
        Returns: None.

        Affects: Writes file instKindatTab.txt in metadata directory

        Exceptions: If unable to write instKindatTab.txt file.
        """
        
        # get full file name
        filename = self.__madDB.getMetadataDir() + '/' + self.__filename
        
        # throw an exception if instKindatTab.txt file not writable
        if not os.access(filename, os.W_OK):
            raise madrigal.admin.MadrigalError('Unable to write: ' + str(filename), None)


        # get experiment metadata
        madExp = MadrigalExperiment(self.__madDB)


        kindatDict = {} # key = year, value = list of kindats

        for i in range(madExp.getExpCount()):
            kinst = madExp.getKinstByPosition(i)
            if kinst not in list(kindatDict.keys()):
                kindatDict[kinst] = {}
            expDir = madExp.getExpDirByPosition(i)
            # get year range
            timeTuple = madExp.getExpStartDateTimeByPosition(i)
            syear = timeTuple[0]
            timeTuple = madExp.getExpEndDateTimeByPosition(i)
            eyear = timeTuple[0]
            for year in range(syear, eyear+1):
                if year not in list(kindatDict[kinst].keys()):
                    kindatDict[kinst][year] = []
                try:
                    madFileObj = madrigal.metadata.MadrigalMetaFile(self.__madDB,
                                                                    os.path.join(expDir, 'fileTab.txt'))
                except:
                    continue
            
                for j in range(madFileObj.getFileCount()):
                    kindat = madFileObj.getKindatByPosition(j)
                    if kindat not in kindatDict[kinst][year]:
                        kindatDict[kinst][year].append(kindat)
                    
        # create newFileStr from instCodeDict
        newFileStr = ''
        kinstList = list(kindatDict.keys())
        kinstList.sort()
        delimiter = ' '

        for kinst in kinstList:
            yearList = list(kindatDict[kinst].keys())
            yearList.sort()
            for year in yearList:
                kindatDict[kinst][year].sort()
                kindatStrList = [str(kindat) for kindat in kindatDict[kinst][year]]
                kindatStr = delimiter.join(kindatStrList)
                newFileStr += '%i,%s,%i\n' % (kinst, kindatStr, year)

        # finally, write new instKindatTab.txt
        newFile = open(filename, 'w')
        newFile.write(newFileStr)
        newFile.close


    
class MadrigalExperiment:
    """MadrigalExperiment is an object that provides access to Madrigal experiment info from the metadata.

    This object provides access to all Madrigal experiment information in the metadata file expTab.txt.

    Usage example::

        import madrigal.metadata
        
        import madrigal.admin

        try:
    
            test = madrigal.metadata.MadrigalExperiment()

            print test.getExperimentName(3001)

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Apr. 17, 2002

    """

    #constants
    __expMetadataFile  = "expTab.txt"

    # column positions
    __expIdCol           =  0
    __expUrlCol          =  1
    __expNameCol         =  2
    __expSiteIdCol       =  3
    __expStartDateCol    =  4
    __expStartTimeCol    =  5
    __expEndDateCol      =  6
    __expEndTimeCol      =  7
    __expKinstCol        =  8
    __expSecurityCol     =  9
    __expPICol           =  10
    __expPIEmailCol      =  11
    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalExperiment by reading from expTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/expTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get experiment metadata file
        if (initFile == None):
            self.__filename = self.__expMetadataFile
        else:
            self.__filename = initFile
            
        # MadrigalExperiment can now legal have two different lengths - with or without PI and email
        allowedLens = (self.__expSecurityCol+1, self.__expPIEmailCol+1)
        
        genericObj = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB, allowedLens, key=self.__expIdCol)

        self.__fileList = genericObj.getList()
        self._dict = genericObj.getDict()


    def getExpIdByPosition(self, position = 0):
        """getExpIdByPosition returns the experiment id of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment id (integer), or None if position >= number of experiments.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__expIdCol])

            except:
                raise madrigal.admin.MadrigalError('Error in expTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)


    def setExpIdByPosition(self, position, expId):
        """setExpIdByPosition sets the experiment id of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expId - the new experiment id to use
        
        Returns: None.

        Affects: sets the experiment id of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        
        try:
            self.__fileList[position][self.__expIdCol] = str(int(expId))
            self._dict[position] = str(int(expId))

        except:
            raise madrigal.admin.MadrigalError('Error in setExpIdByPosition with args %s: %s' %  \
                                               (str((position, expId)),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2])))


    def getExpUrlByPosition(self, position = 0):
        """getExpUrlByPosition returns the experiment url of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment url, or None if position >= number of experiments.

        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:
            return self.__fileList[position][self.__expUrlCol]

        else:
            return(None)


    def getExpUrlByExpId(self, expId):
        """getExpUrlByExpId returns the experiment url for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the experiment url (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpUrlByPosition(position))

    
    
    def getRealExpUrlByPosition(self, position = 0):
        """getRealExpUrlByPosition returns the real experiment url of the experiment at given position.
        
        The url in the metadata may contain /madtoc/ for historical reasons.  This method converts that url to the real one.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the real experiment url, or None if position >= number of experiments.

        Affects: None

        Exceptions: 
        """
        thisUrl = self.getExpUrlByPosition(position)
        if not thisUrl:
            return(None)
        index = thisUrl.find('/madtoc/')
        if index == -1:
            return(thisUrl)
        if thisUrl.find('experiments') != -1:
            realUrl = os.path.join(self.__madDB.getTopLevelUrl(), 'showExperiment?experiment_list=' + thisUrl[index+8:])
        else:
            realUrl = os.path.join(self.__madDB.getTopLevelUrl(), 'showExperiment?experiment_list=' + 'experiments', thisUrl[index+8:])
        return(realUrl)


    def getRealExpUrlByExpId(self, expId):
        """getRealExpUrlByExpId returns the real experiment url for a given experiment id.
        
        The url in the metadata contains /madtoc/ for historical reasons.  This method converts that url to the real one.

        Inputs: Experiment Id (integer).
        
        Returns: the real experiment url (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getRealExpUrlByPosition(position))
    
    
    def getExpPathByPosition(self, position = 0):
        """getExpPathByPosition returns the experiment path of the experiment at given position.
        
        Experiment path is the path to the experiment from the madroot directory, and always begins "experiments"
        
        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the path to the experiment from the madroot directory, or None if position >= number of experiments.

        Affects: None

        Exceptions: 
        """
        thisUrl = self.getExpUrlByPosition(position)
        if not thisUrl:
            return(None)
        index = thisUrl.find('/madtoc/')
        if index != -1:
            return(thisUrl[index+8:])
        index = thisUrl.find('experiments')
        if index == -1:
            return(None)
        return(thisUrl[index:])


    def getExpPathByExpId(self, expId):
        """getRealExpUrlByExpId returns the experiment path of the experiment for a given experiment id.
        
        The url in the metadata may contain /madtoc/ for historical reasons.  This method converts that url to the real one.

        Inputs: Experiment Id (integer).
        
        Returns: the path to the experiment from the madroot directory (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpPathByPosition(position))


    def setExpUrlByPosition(self, position, expUrl):
        """setExpUrlByPosition sets the experiment url of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expUrl - the new experiment url to use
        
        Returns: None.

        Affects: sets the experiment url of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        # verify no illegal commas
        if expUrl.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setExpUrlByPosition with args %s: %s' %  \
                                               (str(position, expUrl),
                                                [traceback.format_exc()]))
        
        try:
            self.__fileList[position][self.__expUrlCol] = str(expUrl)

        except:
            raise madrigal.admin.MadrigalError('Error in setExpUrlByPosition with args %s: %s' %  \
                                               (str(position, expUrl),
                                                [traceback.format_ex()]))

    

    def getExpDirByPosition(self, position = 0):
        """getExpDirByPosition returns the full experiment directory of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the full experiment directory, or None if position >= number of experiments.  Uses
        experiment url to determine directory.

        Affects: None

        Exceptions: None
        """
        if len(self.__fileList) > position:
            url = self.__fileList[position][self.__expUrlCol]
            # find the directory based on url
            maddir = self.__madDB.getMadroot()
            index = url.find('/madtoc/')
            partialExpDir = url[index+8:]
            # added default experiments if not there already
            if partialExpDir.find('experiments') == -1:
                maddir = os.path.join(maddir, 'experiments', url[index+8:])
            else:
                maddir = os.path.join(maddir, url[index+8:])
            return(maddir)

        else:
            return(None)


    def getExpDirByExpId(self, expId):
        """getExpDirByExpId returns the full experiment directory for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the full experiment directory (string).  Returns None if experiment id not found.
        Uses experiment url to determine directory.

        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpDirByPosition(position))


        
    def getExpNameByPosition(self, position = 0):
        """getExpNameByPosition returns the experiment name of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment name, or None if position >= number of experiments.

        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:
            return self.__fileList[position][self.__expNameCol]

        else:
            return(None)


    def getExpNameByExpId(self, expId):
        """getExpNameByExpId returns the experiment name for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the experiment name (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpNameByPosition(position))


    def setExpNameByPosition(self, position, expName):
        """setExpNameByPosition sets the experiment name of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expName - the new experiment name to use
        
        Returns: None.

        Affects: sets the experiment name of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        # verify no illegal commas
        if expName.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setExpNameByPosition with args %s: %s' %  \
                                               (str(position, expName),
                                                [traceback.format_exc()]))
        
        try:
            self.__fileList[position][self.__expNameCol] = str(expName)

        except:
            raise madrigal.admin.MadrigalError('Error in setExpNameByPosition with args %s: %s' %  \
                                               (str(position, expName),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2])))


    def getExpSiteIdByExpId(self, expId):
        """getExpSiteIdByExpId returns the site id (int) for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the site id for this experiment.  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpSiteIdByPosition(position))


    def getExpSiteIdByPosition(self, position = 0):
        """getExpSiteIdByPosition returns the experiment site id of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment site id (integer), or None if position >= number of experiments.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__expSiteIdCol])

            except:
                raise madrigal.admin.MadrigalError('Error in expTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)


    def setExpSiteIdByPosition(self, position, expSiteId):
        """setExpSiteIdByPosition sets the experiment site id of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expSiteId - the new experiment site id to use
        
        Returns: None.

        Affects: sets the experiment site id of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        
        try:
            self.__fileList[position][self.__expSiteIdCol] = str(int(expSiteId))

        except:
            raise madrigal.admin.MadrigalError('Error in setExpSiteIdByPosition with args %s: %s' %  \
                                               (str(position, expSiteId),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2])))


    def getExpStartDateTimeByPosition(self, position = 0):
        """getExpStartDateTimeByPosition returns the starting date/time of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment start date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if position >= number of experiments.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:
            # create time from year, month, day, hour, min, sec, weekday, julian day, daylight savings
            startTime =  [int(self.__fileList[int(position)][self.__expStartDateCol][0:4]),
                          int(self.__fileList[int(position)][self.__expStartDateCol][4:6]),
                          int(self.__fileList[int(position)][self.__expStartDateCol][6:8]),
                          int(self.__fileList[int(position)][self.__expStartTimeCol][0:2]),
                          int(self.__fileList[int(position)][self.__expStartTimeCol][2:4]),
                          int(self.__fileList[int(position)][self.__expStartTimeCol][4:6]),
                          0,
                          0,
                          0]

            # handle hour = 24 case
            if startTime[3] == 24:
                startTime[3] = 23
                startTime[4] = 59
                startTime[5] = 59
            
            # we still need day of year
            utcTime = madrigal.metadata.getMadrigalUTFromDate(startTime[0],
                                                     startTime[1],
                                                     startTime[2],
                                                     startTime[3],
                                                     startTime[4],
                                                     startTime[5],
                                                     0)

            utcDate = madrigal._derive.getDateFromUt(utcTime)

            startTime[7] = utcDate[7]

            # return python time tuple, missing only day of week and DST flag
            return startTime

        else:
            return(None)



    def getExpEndDateTimeByPosition(self, position = 0):
        """getExpEndDateTimeByPosition returns the ending date/time of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment end date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if position >= number of experiments.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """
        position = int(position)

        if len(self.__fileList) > position:
            # create time from year, month, day, hour, min, sec, weekday, julian day, daylight savings
            endTime =  [int(self.__fileList[position][self.__expEndDateCol][0:4]),
                        int(self.__fileList[position][self.__expEndDateCol][4:6]),
                        int(self.__fileList[position][self.__expEndDateCol][6:8]),
                        int(self.__fileList[position][self.__expEndTimeCol][0:2]),
                        int(self.__fileList[position][self.__expEndTimeCol][2:4]),
                        int(self.__fileList[position][self.__expEndTimeCol][4:6]),
                        0,
                        0,
                        0]

            # handle hour = 24 case
            if endTime[3] == 24:
                endTime[3] = 23
                endTime[4] = 59
                endTime[5] = 59
            
            # we still need day of year
            utcTime = madrigal.metadata.getMadrigalUTFromDate(endTime[0],
                                                     endTime[1],
                                                     endTime[2],
                                                     endTime[3],
                                                     endTime[4],
                                                     endTime[5],
                                                     0)

            utcDate = madrigal._derive.getDateFromUt(utcTime)

            endTime[7] = utcDate[7]

            # return python time tuple, missing only day of week and DST flag
            return endTime

        else:
            return(None)



    def getExpStartDateTimeByExpId(self, expId):
        """getExpStartDateTimeByExpId returns the starting date/time of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the experiment start date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if experiment id not found.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpStartDateTimeByPosition(position))



    def getExpEndDateTimeByExpId(self, expId):
        """getExpEndDateTimeByExpId returns the ending date/time of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the experiment end date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if experiment id not found.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getExpEndDateTimeByPosition(position))

        

    def setExpStartDateTimeByPosition(self, startDateTime, position = 0):
        """setExpStartDateTimeByPosition sets a new MadrigalExperiment start date and time by position.

        Inputs:

            startDateTime - a python datetime object to set the exp start date and time to.

            position - which experiment row to change - defaults to 0
        
        Returns: None.
        
        Affects: sets exp start date and time in self.__fileList.

        Exceptions: None
        """
        self.__fileList[position][self.__expStartDateCol] = '%04i%02i%02i' % (startDateTime.year,
                                                                              startDateTime.month,
                                                                              startDateTime.day)

        self.__fileList[position][self.__expStartTimeCol] = '%02i%02i%02i' % (startDateTime.hour,
                                                                              startDateTime.minute,
                                                                              startDateTime.second)


    def setExpEndDateTimeByPosition(self, endDateTime, position = 0):
        """setExpEndDateTimeByPosition sets a new MadrigalExperiment end date and time by position.

        Inputs:

            endDateTime - a python datetime object to set the exp end date and time to.

            position - which experiment row to change - defaults to 0
        
        Returns: None.
        
        Affects: sets exp end date and time in self.__fileList.

        Exceptions: None
        """
        self.__fileList[position][self.__expEndDateCol] = '%04i%02i%02i' % (endDateTime.year,
                                                                            endDateTime.month,
                                                                            endDateTime.day)

        self.__fileList[position][self.__expEndTimeCol] = '%02i%02i%02i' % (endDateTime.hour,
                                                                            endDateTime.minute,
                                                                            endDateTime.second)



    def getKinstByPosition(self, position = 0):
        """getKinstByPosition returns the kinst (kind of instrument code) of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the kinst (instrument code) of the file at given position as an integer.  
        Returns None if position >= number of files.
        
        Affects: None

        Exceptions: Thrown if kinst column cannot be parsed into an integer
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__expKinstCol])

            except:
                raise madrigal.admin.MadrigalError('Error in expTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)



    def getKinstByExpId(self, expId):
        """getKinstByExpId returns the kinst (integer) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the experiment kinst (integer).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getKinstByPosition(position))

       
    def setExpKinstByPosition(self, position, expKinst):
        """setExpKinstByPosition sets the experiment kinst of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expKinst - the new experiment kinst to use
        
        Returns: None.

        Affects: sets the experiment kinst of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        
        try:
            self.__fileList[position][self.__expKinstCol] = str(int(expKinst))

        except:
            raise madrigal.admin.MadrigalError('Error in setExpKinstByPosition with args %s: %s' %  \
                                               (str(position, expKinst),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2])))


    def getSecurityByPosition(self, position = 0):
        """getSecurityByPosition returns the security code of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the security code (integer) of the file at given position as an integer.  
        Returns None if position >= number of files.
        
        Affects: None

        Exceptions: Thrown if security column cannot be parsed into an integer
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__expSecurityCol])

            except:
                raise madrigal.admin.MadrigalError('Error in expTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)


    def getSecurityByExpId(self, expId):
        """getSecurityByExpId returns the security code (integer) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the security code (integer).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getSecurityByPosition(position))


    def setSecurityByPosition(self, position, securityCode):
        """setSecurityByPosition sets the security code (integer) of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            securityCode - the new experiment security code (integer) to use
        
        Returns: None.

        Affects: sets the security code of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        
        try:
            self.__fileList[position][self.__expSecurityCol] = str(int(securityCode))

        except:
            raise madrigal.admin.MadrigalError('Error in setSecurityByPosition with args %s: %s' %  \
                                               (str(position, securityCode),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2])))
            
            
    def getPIByPosition(self, position = 0):
        """getPIByPosition returns the principal investigator of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the principal investigator's name (string) of the file at given position as a string.  
        Returns None if position >= number of files.  Since not all expTab.txt files may have this
        column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """

        if len(self.__fileList) > position:

            try:
                pi = self.__fileList[position][self.__expPICol]
                if len(pi) > 0:
                    return(pi)
                else:
                    return(None)

            except:
                return(None)
        else:
            return(None)


    def getPIByExpId(self, expId):
        """getPIByExpId returns the principal investigator (string) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the principal investigator's name (string).  Returns None if experiment id not found. 
        Since not all expTab.txt files may have this column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getPIByPosition(position))


    def setPIByPosition(self, position, PI):
        """setPIByPosition sets the principal investigator (string) of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            PI - the new experiment principal investigator's name (string) to use
        
        Returns: None.

        Affects: sets the principal investigator of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        
        This method added in Madrigal 2.6
        """
        # verify no illegal commas
        if PI.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setPIByPosition with args %s: %s' %  \
                                               (str(position, PI),
                                                [traceback.format_exc()]))
        
        if len(self.__fileList[position]) == self.__expPICol:
            # add PI and blank email
            self.__fileList[position].append(str(PI))
            self.__fileList[position].append('')
        else:
            try:
                self.__fileList[position][self.__expPICol] = str(PI)

            except:
                raise madrigal.admin.MadrigalError('Error in setPIByPosition with args %s: %s' %  \
                                                   (str(position, PI),
                                                    [traceback.format_exc()]))
                
    
    def getPIEmailByPosition(self, position = 0):
        """getPIEmailByPosition returns the principal investigator email of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the principal investigator's email (string) of the file at given position as a string.  
        Returns None if position >= number of files.  Since not all expTab.txt files may have this
        column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """

        if len(self.__fileList) > position:

            try:
                piEmail = self.__fileList[position][self.__expPIEmailCol]
                if len(piEmail) > 0:
                    return(piEmail)
                else:
                    return(None)

            except:
                return(None)
        else:
            return(None)


    def getPIEmailByExpId(self, expId):
        """getPIEmailByExpId returns the principal investigator email (string) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the principal investigator's email (string).  Returns None if experiment id not found. 
        Since not all expTab.txt files may have this column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        return(self.getPIEmailByPosition(position))


    def setPIEmailByPosition(self, position, PIEmail):
        """setPIEmailByPosition sets the principal investigator email (string) of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            PIEmail - the new experiment principal investigator's email (string) to use
        
        Returns: None.

        Affects: sets the principal investigator email of the experiment at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        
        This method added in Madrigal 2.6
        """
        # verify no illegal commas
        if PIEmail.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setPIEmailByPosition with args %s: %s' %  \
                                               (str(position, PIEmail),
                                                [traceback.format_exc()]))
        
        if len(self.__fileList[position]) == self.__expPICol:
            # add blank PI and email
            self.__fileList[position].append('')
            self.__fileList[position].append(str(PIEmail))
        else:
            try:
                self.__fileList[position][self.__expPIEmailCol] = str(PIEmail)

            except:
                raise madrigal.admin.MadrigalError('Error in setPIEmailByPosition with args %s: %s' %  \
                                                   (str(position, PIEmail),
                                                    [traceback.format_exc()]))


    def getExpLinksByExpId(self, expId):
        """getExpLinksByExpId returns a list of (title, url) tuples containing all links for this experiment.
        
        Inputs:

            Inputs: Experiment Id (integer).
        
        Returns: a list of (title, url) tuples containing all links for this experiment.

        In order to be a link, a file must be in the experiment directory in the form *.html,
        */index.html, or */*/index.html.  The title is parsed from the title in the head; if not found,
        returns 'No title' as title.  The follow file extensions are also links if found in the
        main experiment directory: ['ps', 'eps', 'png', 'jpg', 'jpeg', 'pdf', 'gif'].  For these
        files, the title is simply the basename.
        
        Now sorted according to basename of url.
        
        Affects: None

        Exceptions: None
        """
        try:
            position = self._dict[str(expId)]
        except KeyError:
            return(None)
        retList = sorted(self.getExpLinksByPosition(position),
                         key=lambda list_item: os.path.basename(list_item[1]))
        return(retList)
        
        
    
    def getExpLinksByPosition(self, position = 0):
        """getExpLinksByExpId returns a list of (title, url) tuples containing all links for this experiment.

        Inputs:

            Inputs: position - position of experiment in list (first position is zero).
        
        Returns: a list of (title, url) tuples containing all links for this experiment.

        In order to be a link, a file must be in the experiment directory in the form *.html,
        */index.html, or */*/index.html.  The title is parsed from the title in the head; if not found,
        returns 'No title' as title.  The follow file extensions are also links if found in the
        main experiment directory: ['ps', 'eps', 'png', 'jpg', 'jpeg', 'pdf', 'gif', 'tar.gz', 'notes.txt'].  For these
        files, the title is simply the basename.
        
        Affects: None

        Exceptions: None
        """
        retList = []
        allowedExtensions = ['ps', 'eps', 'png', 'jpg', 'jpeg', 'pdf', 'gif', 'tar.gz', 'txt']
        
        # find the experiment directory based on url
        topdir = self.__madDB.getMadroot()
        url = self.getExpUrlByPosition(position)
        if url == None:
            return retList
        index = url.find('/madtoc/')
        partialExpDir = url[index+8:]
        # added default experiments if not there already
        if partialExpDir.find('experiments') == -1:
            expdir = os.path.join(topdir, 'experiments', url[index+8:])
        else:
            expdir = os.path.join(topdir, url[index+8:])

        # now create a list of html files
        htmlFiles = []
        l1 = glob.glob(os.path.join(expdir, '*.html'))
        l2 = glob.glob(os.path.join(expdir, '*/index.html'))
        l3 = glob.glob(os.path.join(expdir, '*/*/index.html'))

        for item in l1:
            htmlFiles.append(item)
        for item in l2:
            htmlFiles.append(item)
        for item in l3:
            htmlFiles.append(item)
            
        for htmlFile in htmlFiles:
            if os.path.basename(htmlFile) == 'notes.html':
                continue
            # get title if possible
            f = open(htmlFile)
            text = f.read()
            f.close()
            i1 = text.upper().find('<TITLE>') + len('<TITLE>')
            i2 = text.upper().find('</TITLE>')
            if i1 > -1 and i2 > -1:
                title = text[i1:i2]
            else:
                title = 'No title'
            # get url
            i1 = htmlFile.find('experiments')
            url = os.path.join(self.__madDB.getTopLevelUrl(), 'static', htmlFile[i1:])
            retList.append((title, url))
            
        # Append custom data files
        allowedExtraAuxFilePatterns = ['EISCAT*.hdf5']
        for allowedAuxPattern in allowedExtraAuxFilePatterns:
            addList = glob.glob(os.path.join(expdir, allowedAuxPattern))
            if (len(addList) > 0):
                for addf in addList:
                    i1 = addf.find('experiments')
                    url = os.path.join(self.__madDB.getTopLevelUrl(), 'static', addf[i1:])
                    retList.append((os.path.basename(addf), url))
            
        
        for extension in allowedExtensions:
            plotList = glob.glob(os.path.join(expdir, '*.%s' % (extension)))
            for plot in plotList:
                plot2 = plot # used only for displaying notes.txt as html
                if os.path.basename(plot) == 'notes.html':
                    continue
                if extension == 'txt' and os.path.basename(plot) != 'notes.txt':
                    continue
                elif extension == 'txt':
                    # check if it needs to be converted to notes.html
                    f = open(plot, 'r')
                    text = f.read()
                    if text.lower().find('</') != -1 or text.lower().find('<br>') != -1:
                        # assume html format, create a copy as notes.html
                        plot2 = plot.replace('notes.txt', 'notes.html')
                        if not os.access(plot2, os.R_OK):
                            try:
                                shutil.copy(plot, plot2)
                            except:
                                continue
                        
                i1 = plot.find('experiments')
                url = os.path.join(self.__madDB.getTopLevelUrl(), 'static', plot2[i1:])
                retList.append((os.path.basename(plot), url))

        return retList



    def getExpCount(self):
        """getExpCount returns number of experiments in MadrigalExperiment object
        """
        return(len(self.__fileList))


    def sortByDateSite(self):
        """sortByDateSite will resort self.__fileList so that experiments are listed first by experiment
        end date, and then by site
        """
        self.__fileList.sort(key=self.__compareDateSite__)
        # rebuild self._dict 
        self._dict = {}
        for i, items in enumerate(self.__fileList):
            self._dict[items[self.__expIdCol]] = i
        
        
    def getStartPosition(self, startDT):
        """getStartPosition returns the position of the first experiment with a start datetime after
        endDT after sorting by end time.
        
            Inputs: startDT - start datetime
            
            Returns: returns the position of the first experiment with a start datetime after
        startDT.
        """
        self.sortByDateSite()
        top = len(self.__fileList) - 1
        bottom = 0
        while(top >= bottom):
            if top == bottom:
                return(max(top-1, 0))
            middle = int((top+bottom)/2)
            thisEDTList = self.getExpEndDateTimeByPosition(middle)
            thisEDT = datetime.datetime(*thisEDTList[0:6])
            if thisEDT < startDT:
                bottom = middle + 1
                continue
            if middle == 0:
                return(0)
            prevEDTList = self.getExpEndDateTimeByPosition(middle-1)
            prevEDT = datetime.datetime(*prevEDTList[0:6])
            if prevEDT < startDT:
                return(middle-1)
            else:
                top = middle -1
     



    def __compareDateSite__(self, first):
        """__compareDateSite__ is a private method to help sort by end date, then start date, and then site.

        first - an item from self.__fileList
        
        returns a tuple of (endDT, startDT, site) to be used for sorting
        """
        endDT = datetime.datetime.strptime(first[self.__expEndDateCol] + first[self.__expEndTimeCol],
                                           '%Y%m%d%H%M%S')
        startDT = datetime.datetime.strptime(first[self.__expStartDateCol] + first[self.__expStartTimeCol],
                                           '%Y%m%d%H%M%S')
        site = first[self.__expSiteIdCol]
        return((endDT, startDT, site))
    


    def getLine(self, position):
        """getLine returns the line at a given position.  Returns None if position > number of lines.

        Inputs:  position - position in file.  First line = 0
        """
        delimiter = ','

        if position >= len(self.__fileList):
            return(None)

        return(delimiter.join(self.__fileList[position]) + '\n')
  
        
    def writeMetadata(self, newFullPath=None):
        """writeMetadata writes a new version of the expTab.txt file.
        
        Inputs: newFullPath:  a new path to write the expTab.txt file to, if
        not the same as the original metadata file opened.  Defaults to None, which overwrites
        metadata file that was read from.
        
        Returns: None.

        Affects: Writes updated version of metadata file.

        Exceptions: If unable to write file
        """

        # create string to hold file
        metaFileStr = ''
        delimiter = ','

        for lineList in self.__fileList:
            metaFileStr += delimiter.join(lineList) + '\n'

        # try to write file, if not, raise exception

        try:

            if newFullPath == None:
                if (len(os.path.dirname(self.__filename)) != 0):
                    newFullPath = self.__filename
                else:
                    newFullPath = self.__madDB.getMetadataDir() + "/" + self.__filename
            newFile = open(newFullPath, "w")
            newFile.write(metaFileStr)
            newFile.close()

        except:

            raise madrigal.admin.MadrigalError("Unable to write metadata file " + \
                                               str(newFullPath),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))


    def __str__(self):
        """return possibly modified file as a string in same format as writeMetadata
        """
        # create string to hold file
        metaFileStr = ''
        delimiter = ','

        for lineList in self.__fileList:
            metaFileStr += delimiter.join(lineList) + '\n'
            
        return(metaFileStr)






class MadrigalMetaFile:
    """MadrigalMetaFile is an object that provides access to Madrigal file info from the metadata.

    This object provides access to all Madrigal experiment information in the metadata file fileTab.txt.

    Usage example::

        import madrigal.metadata
        
        import madrigal.admin

        try:
    
            test = madrigal.metadata.MadrigalMetaFile()

            print test.getFileCount()

        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  May. 7, 2002

    """

    #constants
    __fileMetadataFile  = "fileTab.txt"

    # column positions
    __fileNameCol           =  0
    __fileExpIdCol          =  1
    __fileKindatCol         =  2
    __fileCategoryCol       =  3
    __fileSizeCol           =  4
    __fileHasCatalogCol     =  5
    __fileHasHeaderCol      =  6
    __fileModDateCol        =  7
    __fileModTimeCol        =  8
    __fileStatusCol         =  9
    __fileAccessCol         =  10
    __fileAnalystCol        =  11
    __fileAnalystEmailCol   =  12
    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalMetaFile by reading from fileTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/fileTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get file metadata file
        if (initFile == None):
            self.__filename = self.__fileMetadataFile
        else:
            self.__filename = initFile

        # MadrigalMetafile can now legal have two different lengths - with or without file analyst and email
        allowedLens = (self.__fileAccessCol+1, self.__fileAnalystEmailCol+1)
        
        genericObj = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB, allowedLens, key=self.__fileNameCol)

        self.__fileList = genericObj.getList()
        self._dict = genericObj.getDict()
        
        # check that at least first row has right number of columns
        if len(self.__fileList) > 0:
            # analyst and analyst email columns may or may not exist
            if len(self.__fileList[0]) not in allowedLens:
                raise madrigal.admin.MadrigalError('Error in count of first row of ' + str(self.__filename), None)



    def getFileCount(self):
        """getFileCount returns the number of files (rows) in the metadata file.

        Inputs: None
        
        Returns: the number of files (rows) in the metadata file.

        Affects: None

        Exceptions: None
        """

        return len(self.__fileList)


    def getFilenameByPosition(self, position = 0):
        """getFilenameByPosition returns the filename of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the filename of the file at given position as a string.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            return self.__fileList[position][self.__fileNameCol]

        else:
            return(None)


    def getExpIdByPosition(self, position = 0):
        """getExpIdByPosition returns the experiment id (integer) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the experiment id (integer) of the file at given position as an integer.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: Thrown if kinst exp id cannot be parsed into an integer
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__fileExpIdCol])

            except:
                raise madrigal.admin.MadrigalError('Error in fileTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)


    def setExpIdByPosition(self, position, expId):
        """setExpIdByPosition sets the experiment id of the file at given position.

        Inputs:

            position - position of file in list (first position is zero).

            expId - the new experiment id to use
        
        Returns: None.

        Affects: sets the experiment id of the file at given position

        Exceptions: MadrigalError if any item in row cannot be cast to correct format, or
        position not found.
        """
        
        try:
            self.__fileList[position][self.__fileExpIdCol] = str(int(expId))

        except:
            raise madrigal.admin.MadrigalError('Error in setExpIdByPosition with args %s: %s' %  \
                                               (str(position), str(expId)),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2]))


    def getKindatByPosition(self, position = 0):
        """getKindatByPosition returns the kindat (kind of data code) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the kinst (kind of instrument code) of the file at given position as an integer.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: Thrown if kinst column cannot be parsed into an integer
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__fileKindatCol])

            except:
                raise madrigal.admin.MadrigalError('Error in fileTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)


    def getCategoryByPosition(self, position = 0):
        """getCategoryByPosition returns the category (eg., 1=default, 2=variant, 3=history, 4=real-time) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the category (eg., 1=default, 2=variant, 3=history, 4=real-time) of the file at given position as an integer.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: Thrown if category column cannot be parsed into an integer
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__fileCategoryCol])

            except:
                raise madrigal.admin.MadrigalError('Error in fileTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)


    def getCategoryByFilename(self, filename):
        """getCategoryByFilename returns the category (eg., 1=default, 2=variant, 3=history, 4=real-time) of the file with the given filename.

        Inputs: filename - name of file to search for.
        
        Returns: the category (eg., 1=default, 2=variant, 3=history, 4=real-time) of the first row found with the
        given filename.  Since
        filename may not be unique (although it usually is), the first match found is used.  If
        no matches found, returns None.
        
        Affects: None

        Exceptions: Thrown if category column cannot be parsed into an integer
        """
        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(int(self.__fileList[position][self.__fileCategoryCol]))



    def getHasCatalogByPosition(self, position = 0):
        """getHasCatalogByPosition returns True if the file at given position has any catalog records, False otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  True if the file at given position has any catalog records, False otherwise 
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            if int(self.__fileList[position][self.__fileHasCatalogCol]) == 0:
                return False
            else:
                return True

        else:
            return(None)


    def getHasCatalogByFilename(self,filename):
        """getHasCatalogByFilename returns true if the file with the given name has any catalog records, False otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: true if the file with the given name has any catalog records, False otherwise.  Returns none if name
        not found
        
        Affects: None

        Exceptions: None.
        """

        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(int(self.__fileList[position][self.__fileHasCatalogCol]))


    def setHasCatalogByPosition(self, position, hasCatalog):
        """setHasCatalogByPosition sets the value of hasCatalog for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            hasCatalog - 1 or True for yes, 0 or False for no
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if hasCatalog not in (0, 1, True, False):
            raise ValueError('Illegal value for hasCatalog in setHasCatalogByPosition: %s' % (str(hasCatalog)))

        if len(self.__fileList) > position:

            if hasCatalog in (1, True):
                self.__fileList[position][self.__fileHasCatalogCol] = '1'
            else:
                self.__fileList[position][self.__fileHasCatalogCol] = '0'

        else:
            raise ValueError('setHasCatalogByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def getHasHeaderByPosition(self, position = 0):
        """getHasHeaderByPosition returns True if the file at given position has any header records, False otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  True if the file at given position has any header records, False otherwise 
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            if int(self.__fileList[position][self.__fileHasHeaderCol]) == 0:
                return False
            else:
                return True

        else:
            return(None)


    def getHasHeaderByFilename(self,filename):
        """getHasHeaderByFilename returns true if the file with the given name has any header records, False otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: true if the file with the given name has any header records, False otherwise.  Returns none if name
        not found
        
        Affects: None

        Exceptions: Thrown if filename not found.
        """

        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(int(self.__fileList[position][self.__fileHasHeaderCol]))



    def setHasHeaderByPosition(self, position, hasHeader):
        """setHasHeaderByPosition sets the value of hasHeader for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            hasHeader - 1 or True for yes, 0 or False for no
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if hasHeader not in (0, 1, True, False):
            raise ValueError('Illegal value for hasHeader in setHasHeaderByPosition: %s' % (str(hasHeader)))

        if len(self.__fileList) > position:

            if hasHeader in (1, True):
                self.__fileList[position][self.__fileHasHeaderCol] = '1'
            else:
                self.__fileList[position][self.__fileHasHeaderCol] = '0'

        else:
            raise ValueError('setHasHeaderByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def getStatusByPosition(self, position = 0):
        """getStatusByPosition returns the status description of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the status description of the file at given position as a string.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            return self.__fileList[position][self.__fileStatusCol]

        else:
            return(None)


    def getStatusByFilename(self,filename):
        """getStatusByFilename returns the status description of the file with the given name.

        Inputs: filename - name of file to search for.
        
        Returns: the status description of the file with the given name.  Returns none if name
        not found
        
        Affects: None

        Exceptions: Thrown if filename not found.
        """

        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(self.__fileList[position][self.__fileStatusCol])



    def getAccessByPosition(self, position = 0):
        """getAccessByPosition returns the access (0=public, 1=private) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the access level (0=public, 1=private) of the file at given position as an integer.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: Thrown if access column cannot be parsed into an integer
        """

        if len(self.__fileList) > position:

            try:
                return int(self.__fileList[position][self.__fileAccessCol])

            except:
                raise madrigal.admin.MadrigalError('Error in fileTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        else:
            return(None)
        
        
    def getFileDatetimeByPosition(self, position = 0):
        """getFileDatetimeByPosition returns a datetime of the file at given position, or None if not set.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: a datetime of the file at given position, or None if not set.  
        Returns None if position >= number of experiments.
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            dateStr = self.__fileList[position][self.__fileModDateCol]
            if len(dateStr) < 8:
                return(None)
            timeStr = self.__fileList[position][self.__fileModTimeCol]
            if len(timeStr) < 6:
                return(None)
            
            return(datetime.datetime.strptime('%s %s' % (dateStr, timeStr), '%Y%m%d %H%M%S'))

        else:
            return(None)


    def getFileDatetimeByFilename(self,filename):
        """getFileDatetimeByFilename returns a datetime of the file with the given filenme, or None if not set.

        Inputs: filename - name of file to search for.
        
        Returns: a datetime of the file with the given filenme, or None if not set.  Returns none if name
        not found
        
        Affects: None

        Exceptions: Thrown if filename not found.
        """

        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(self.getFileDatetimeByPosition(position))

    
    
    def setFileDatetimeByPosition(self, position, dt):
        """setFileDatetimeByPosition sets the file datetime for the given position

        Inputs:

            position - position of file in list (first position is zero).

            dt - datetime (UT) of file being added.  If None, use file datetime.
                Will raise exception if dt is None and fileTab.txt in the metadata
                version, not the expDir version.
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if dt is None:
            basename = self.getFilenameByPosition(position)
            expDir = os.path.dirname(self.__filename)
            if len(expDir) < 2:
                raise IOError('Cannot set datetime to None using the metadata version of fileTab.txt')
            dataFile = os.path.join(expDir, basename)
            if not os.access(dataFile, os.R_OK):
                raise IOError('Cannot access %s' % (dataFile))
            dt = datetime.datetime.utcfromtimestamp(os.path.getmtime(dataFile))
            
            
        dateStr = dt.strftime('%Y%m%d')
        timeStr = dt.strftime('%H%M%S')

        if len(self.__fileList) > position:

            self.__fileList[position][self.__fileModDateCol] = dateStr
            self.__fileList[position][self.__fileModTimeCol] = timeStr

        else:
            raise ValueError('setFileDatetimeByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def deleteRowByFilename(self, filename):
        """deleteRowByFilename deletes a row with a given filename.

        Inputs: filename - name of file to search for.
        
        Returns: None.
        
        Affects: Removes item from self.__fileList if filename found

        Exceptions: Thrown if filename not found.
        """
        
        for file in self.__fileList:

            if filename == file[self.__fileNameCol]:

                self.__fileList.remove(file)
                return

                
        # no matches found
        raise madrigal.admin.MadrigalError('Could not delete file ' + filename + ' from ' + self.__filename, None)



    def getExpIdByFilename(self, filename):
        """getExpIdByFilename returns the first experiment id (integer) with the given filename.

        Inputs: filename - name of file to search for.
        
        Returns: the experiment id (integer) of the first row found with the given filename.  Since
        filename may not be unique (although it usually is), the first match found is used.  If
        no matches found, returns None.
        
        Affects: None

        Exceptions: Thrown if exp id cannot be parsed into an integer
        """
        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(int(self.__fileList[position][self.__fileExpIdCol]))



    def getKindatByFilename(self, filename):
        """getKindatByFilename returns the first kindat (integer) with the given filename.

        Inputs: filename - name of file to search for.
        
        Returns: the kindat (integer) of the first row found with the given filename.  Since
        filename may not be unique (although it usually is), the first match found is used.  If
        no matches found, returns None.
        
        Affects: None

        Exceptions: Thrown if kindat cannot be parsed into an integer
        """
        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        
        return(int(self.__fileList[position][self.__fileKindatCol]))



    def setAccessByPosition(self, position, access):
        """setAccessByPosition sets the value of access for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            access - 0 of False for public, 1 or True for private
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if access not in (0, 1, True, False):
            raise ValueError('Illegal value for access in setAccessByPosition: %s' % (str(access)))

        if len(self.__fileList) > position:

            if access in (1, True):
                self.__fileList[position][self.__fileAccessCol] = '1'
            else:
                self.__fileList[position][self.__fileAccessCol] = '0'

        else:
            raise ValueError('setAccessByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def setAccess(self, accessType):
        """setAccess sets the access column to all 0's (public) or all 1's (private).

        Inputs: accessType - either 0 to set to public access, or 1 to set to private access.
        
        Returns: None.
        
        Affects: Overwrite fileTab.txt file with access column set to all 0's (public)
        or all 1's (private).

        Exceptions: Thrown if file cannot be written, if accessType is not 0 or 1
        """
        if (accessType != 0 and accessType != 1):
            raise madrigal.admin.MadrigalError('MadrigalMetaFile.setAccess called with arg = ' + \
                                               str(accessType) + ', must be either 0 or 1', None)

        for row in self.__fileList:
            row[self.__fileAccessCol] = str(accessType)

        self.writeMetadata()


    def setKindatByPosition(self, position, kindat):
        """setKindatByPosition sets the value of kindat for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            kindat - integer kindat value
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        kindat = int(kindat)
        
        if len(self.__fileList) > position:

            self.__fileList[position][self.__fileKindatCol] = str(kindat)

        else:
            raise ValueError('setKindatByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))



    def setCategoryByPosition(self, position, category):
        """setCategoryByPosition sets the value of category for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            category - 1=default, 2=variant, 3=history, 4=real-time
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if int(category) not in (1, 2, 3, 4):
            raise ValueError('Illegal value for category in setCategoryByPosition: %s' % (str(category)))

        if len(self.__fileList) > position:

            self.__fileList[position][self.__fileCategoryCol] = str(category)

        else:
            raise ValueError('setCategoryByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def setStatusByPosition(self, position, status):
        """setStatusByPosition sets the value of status string for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            status - string describing status
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        statusTypes = [bytes, str]
        if type(status) not in statusTypes:
            raise ValueError('Illegal value for status in setStatusByPosition: %s' % (str(status)))

        # check that string does not illegally contain a comma
        if status.find(',') != -1:
            raise ValueError('status string in fileTab.txt cannot contain a comma: <%s> is illegal' % (status))

        if len(self.__fileList) > position:

            self.__fileList[position][self.__fileStatusCol] = status

        else:
            raise ValueError('setStatusByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def getAnalystByPosition(self, position = 0):
        """getAnalystByPosition returns file analyst name if there, None otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  file analyst name if there, None otherwise. Since not all fileTab.txt files may have this column, 
        returns None if the column does not exist.  Returns None if position >= number of experiments.  
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            try:
                return(self.__fileList[position][self.__fileAnalystCol])
            except:
                return(None)

        else:
            return(None)


    def getAnalystByFilename(self, filename):
        """getAnalystByFilename returns file analyst name if there, None otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: file analyst name if there, None otherwise. Since not all fileTab.txt files may have this column, 
        returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        """
        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        try:
            return(self.__fileList[position][self.__fileAnalystCol])
        except:
            return(None)



    def setAnalystByPosition(self, position, analyst):
        """setAnalystByPosition sets the file analyst name for the given position

        Inputs:

            position - position of file in list (first position is zero).

            analyst - name of file analyst
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        # verify no illegal commas
        if analyst.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setAnalystByPosition with args %s: %s' %  \
                                               (str(position), analyst),
                                                [traceback.format_exc()])

        if len(self.__fileList) > position:

            if len(self.__fileList[position]) == self.__fileAccessCol+1:
                # append two extra columns
                self.__fileList[position].append(str(analyst))
                self.__fileList[position].append('')
            else:
                self.__fileList[position][self.__fileAnalystCol] = str(analyst)

        else:
            raise ValueError('setAnalystByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))


    def getAnalystEmailByPosition(self, position = 0):
        """getAnalystEmailByPosition returns file analyst email if there, None otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  file analyst email if there, None otherwise. Since not all fileTab.txt files may have this column, 
        returns None if the column does not exist.  Returns None if position >= number of experiments.  
        
        Affects: None

        Exceptions: None
        """

        if len(self.__fileList) > position:

            try:
                return(self.__fileList[position][self.__fileAnalystEmailCol])
            except:
                return(None)

        else:
            return(None)


    def getAnalystEmailByFilename(self, filename):
        """getAnalystEmailByFilename returns file analyst email if there, None otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: file analyst email if there, None otherwise. Since not all fileTab.txt files may have this column, 
        returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        """
        filename = os.path.basename(filename)
        try:
            position = self._dict[filename]
        except KeyError:
            return(None)
        try:
            return(self.__fileList[position][self.__fileAnalystEmailCol])
        except:
            return(None)



    def setAnalystEmailByPosition(self, position, analystEmail):
        """setAnalystEmailByPosition sets the file analyst email for the given position

        Inputs:

            position - position of file in list (first position is zero).

            analystEmail - email of file analyst
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        # verify no illegal commas
        if analystEmail.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setAnalystEmailByPosition with args %s: %s' %  \
                                               (str(position, analystEmail),
                                                [traceback.format_exc()]))

        if len(self.__fileList) > position:

            if len(self.__fileList[position]) == self.__fileAccessCol + 1:
                # append two extra columns
                self.__fileList[position].append('')
                self.__fileList[position].append(str(analystEmail))
            else:
                self.__fileList[position][self.__fileAnalystEmailCol] = str(analystEmail)

        else:
            raise ValueError('setAnalystEmailByPosition called for position %i beyond length %i' % (position, len(self.__fileList)))




    def getMetadataSummaryByFilename(self, filename, madExpObj = None, madInstObj = None, madKindatObj = None):
        """getMetadataSummaryByFilename returns a string summarizing the file's metadata given a filename.

        Inputs: filename - name of file to search for.

            The next three inputs are other metadata objects.  If these objects already exist, performance will
            be improved by passing them in rather than recreating them.  If they do not exist, they will be
            created:
        
            madExpObj - a MadrigalExperiment object to get experiment metadata from.  If None (the default),
            a new MadrigalExperiment object is created.
            
            madInstObj - a MadrigalInstrument object to get experiment metadata from.  If None (the default),
            a new MadrigalInstrument object is created.
            
            madKindatObj - a MadrigalKindat object to get experiment metadata from.  If None (the default),
            a new MadrigalKindat object is created.
        
        Returns: A string summarizing the metadata about the file.  The format is::

                Start Date and Time: 01/06/1997  14:07  
                End Date and Time:   01/10/1997  23:45
                Instrument: Millstone Hill Incoherent Scatter Radar
                Experiment name: World Day - Mesosphere/Lower-Thermosphere Coupling Study
                Kind of data: INSCAL (8.0) Basic Derived Parameters
        
        Affects: None

        Exceptions: Thrown if any parsing error in metadata.
        """

        # build return String
        retStr = ''

        # create any needed metadata objects

        if madExpObj == None:
            madExpObj = MadrigalExperiment(self.__madDB)

        if madInstObj == None:
            madInstObj = MadrigalInstrument(self.__madDB)

        if madKindatObj == None:
            madKindatObj = MadrigalKindat(self.__madDB)

        # get the experiment id - if None, return message to update Metadata
        expId = self.getExpIdByFilename(filename)

        if expId == None:
            return '\tMetadata for file ' + str(filename) + \
                   ' not found.  Please notify Madrigal administrator that metadata ' + \
                   'needs to be updated.'

        # get the experiment start date - if not found, return message to update Metadata
        starttime = madExpObj.getExpStartDateTimeByExpId(expId)

        if starttime == None:
            return '\tMetadata for file ' + str(filename) + \
                   ' not found.  Please notify Madrigal administrator that metadata ' + \
                   'needs to be updated.'
        

        retStr = retStr + '\tExperiment start:  ' + time.asctime(starttime) + '\n'

        # endtime
        endtime = madExpObj.getExpEndDateTimeByExpId(expId)
        retStr = retStr + '\tExperiment end:    ' + time.asctime(endtime) + '\n'

        # get instrument name
        kinst = madExpObj.getKinstByExpId(expId)
        # if kinst not found, metadata is corrupt
        if kinst == None:
            return 'Could not find kinst for experiment id = ' + str(expId)
        instName = madInstObj.getInstrumentName(kinst)
        retStr = retStr + '\tInstrument name:   ' + str(instName) + '\n'

        # get experiment name
        expName = madExpObj.getExpNameByExpId(expId)
        retStr = retStr + '\tExperiment name:   ' + str(expName) + '\n'

        # get kind of data
        kindat = self.getKindatByFilename(filename)
        kindatName = madKindatObj.getKindatDescription(kindat, kinst)
        retStr = retStr + '\tKind of data:      ' + str(kindatName) + '\n'
        
        return retStr
    
    
    def getFileDOIUrlByPosition(self, position):
        """getFileDOIUrlByPosition returns the full url to the file on the archive site.
        
        Meant to be used as a referencable url, so always refers to cedar.openmadigal.org
        
        Returns permanent URL to file, or None if not found
        """
        
        # get expDir
        if self.__fileMetadataFile != self.__filename:
            # local copy
            expDir = os.path.dirname(self.__filename)
        else:
            expId = self.getExpIdByPosition(position)
            if expId is None:
                return(None)
            # need a MadrigalExperiment object
            madExpObj = madrigal.metadata.MadrigalExperiment(self.__madDB)
            expDir = madExpObj.getExpDirByExpId(expId)
            if expDir is None:
                return(None)
        
        basename = self.getFilenameByPosition(position)
        if basename is None:
            return(None)
        
        # change so that it starts with experiments
        expDir = expDir[expDir.find('experiments'):]
        
        url_template = 'https://w3id.org/cedar?experiment_list=%s&file_list=%s'
        return(url_template % (expDir, basename))
        
        
    
    def getFileDOIUrlByFilename(self, filename):
        """getFileDOIUrlByFilename returns the full url to the file on the archive site.
        
        Meant to be used as a referencable url, so always refers to cedar.openmadigal.org
        
        Returns permanent URL to file, or None if not found
        """
        # get expDir
        if self.__fileMetadataFile != self.__filename:
            # local copy
            expDir = os.path.dirname(self.__filename)
        else:
            expId = self.getExpIdByFilename(filename)
            if expId is None:
                return(None)
            # need a MadrigalExperiment object
            madExpObj = madrigal.metadata.MadrigalExperiment(self.__madDB)
            expDir = madExpObj.getExpDirByExpId(expId)
            if expDir is None:
                return(None)
        
        # change so that it starts with experiments
        expDir = expDir[expDir.find('experiments'):]
        
        url_template = 'https://w3id.org/cedar?experiment_list=%s&file_list=%s'
        return(url_template % (expDir, filename))
        
        



    def getLine(self, position):
        """getLine returns the line at a given position.  Returns None if position > number of lines.

        Inputs:  position - position in file.  First line = 0
        """
        delimiter = ','

        if position >= len(self.__fileList):
            return(None)

        return(delimiter.join(self.__fileList[position]) + '\n')
        

    def writeMetadata(self, newFullPath=None):
        """writeMetadata writes a new version of the fileTab.txt file.
        
        Inputs: newFullPath:  a new path to write the fileTab.txt file to, if
        not the same as the original metadata file opened.  Defaults to None, which overwrites
        metadata file that was read from.
        
        Returns: None.

        Affects: Writes updated version of metadata file.

        Exceptions: If unable to write file
        """

        # create string to hold file
        metaFileStr = ''
        delimiter = ','

        for lineList in self.__fileList:
            metaFileStr += delimiter.join(lineList) + '\n'

        # try to write file, if not, raise exception

        try:

            if newFullPath == None:
                if (len(os.path.dirname(self.__filename)) != 0):
                    newFullPath = self.__filename
                else:
                    newFullPath = self.__madDB.getMetadataDir() + "/" + self.__filename
            newFile = open(newFullPath, "w")
            newFile.write(metaFileStr)
            newFile.close()

        except:

            raise madrigal.admin.MadrigalError("Unable to write metadata file " + \
                                               str(newFullPath),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))
        
    def __str__(self):
        """return possibly modified file as a string in same format as writeMetadata
        """
        # create string to hold file
        metaFileStr = ''
        delimiter = ','

        for lineList in self.__fileList:
            metaFileStr += delimiter.join(lineList) + '\n'
            
        return(metaFileStr)
    
    
class MadrigalParmCategory:
    """MadrigalParmCategory is an object that provides access to Madrigal parameter category info from the metadata.

    This object provides access to all Madrigal kind of data information in the metadata file madCatTab.txt.

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jan. 18, 2013

    """

    #constants
    _categoryMetadataFile  = "madCatTab.txt"

    # column positions
    _categoryCodeCol   =  0
    _categoryDescCol   =  1

    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalParmCategory by reading from madCatTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/madCatTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get category metadata file
        if (initFile == None):
            self.__filename = self._categoryMetadataFile
        else:
            self.__filename = initFile

        self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB).getList()


    def getCategoryDesc(self, code):
        """getCategoryDesc returns the category description that matches code argument, or None if not found.

        Inputs: 
        
            code integer to get category description. or integer as string
        
        Returns: the category description that matches code argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        code = int(code)
        
        for i, category in enumerate(self.__fileList):
            # find matching  code 
            try:
                if code == int(category[self._categoryCodeCol]):
                    return(category[self._categoryDescCol])
                
            except:
                raise madrigal.admin.MadrigalError('Error in madCatTab.txt parsing metadata row %i: ' % (i) + str(category),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))

        # not found
        return(None)


    def getCategoryList(self):
        """getCategoryList returns a list of all category descriptions and codes.

        Inputs: None.
        
        Returns: a list of all category descriptions and codes.  Each item in the list
        is a tuple of the form (Category description (string),  code (integer)).  Example item:
        ('INSCAL Basic Derived Parameters', 3001)

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        retList = []

        for i, category in enumerate(self.__fileList):
            item = (category[self._categoryDescCol],
                    int(category[self._categoryCodeCol]))
            retList.append(item)
            
        return(retList)
    
    
class MadrigalInstrumentData:
    """MadrigalInstrumentData is an object that provides access to Madrigal instrument data info from the metadata.

    This object provides access to all Madrigal kind of data information in the metadata files instData.txt
    and instDataPriv.txt.  Those files summarize years data is available by instrument.

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 2, 2015

    """

    #constants
    _instDataMetadataFile  = "instData.txt"

    # column positions
    _siteIDCol   =  0
    _kinstCol    =  1
    _yearsCol    = 2

    

    def __init__(self, madDB=None, priv=False, initFile=None, madInstObj=None):
        """__init__ initializes MadrigalInstrumentData by reading from instData.txt (or instDataPriv.txt).

        Inputs: madDB - Existing MadrigalDB object, by default = None.

                priv - if True, use instDataPriv.txt instead of instData.txt.  If False (the default),
                    use instData.txt.
                    
                initFile - String representing the full path to the metadata file. Default is None, in
                    which case file read depends on priv argument. priv arg ingored if this not None
                    
                madInstObj - m madrigal.metadata.MadrigalInstrument object.  If None, one is created.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown  by MadrigalMetadata if file not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB
            
        if priv:
            self._instDataMetadataFile  = "instDataPriv.txt"

        # get instData metadata file
        if (initFile == None):
            self.__filename = self._instDataMetadataFile
        else:
            self.__filename = initFile
            
        if madInstObj is not None:
            self._madInstObj = madInstObj
        else:
            self._madInstObj = madrigal.metadata.MadrigalInstrument(self.__madDB)

        self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB).getList()


    def getCategories(self, local=False):
        """getCategories returns the a list of (category id, category desc) tuples.

        Inputs: 
        
            local - if False, return all categories for which there is data anywhere in Madrigal.
                If True, only return local categories.
        
        Returns:  a list of (category id, category desc) tuples

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        if local:
            siteID = self.__madDB.getSiteID()
            
        localDict = {} # key is category id, value is category desc str.  Will be converted to list before returned
        
        for i, data in enumerate(self.__fileList):
            # find matching  code 
            try:
                if local:
                    if int(data[self._siteIDCol]) != siteID:
                        continue
                kinst = int(data[self._kinstCol])
                categoryID = self._madInstObj.getCategoryId(kinst)
                if categoryID is None:
                    continue
                if categoryID not in list(localDict.keys()):
                    localDict[categoryID] = self._madInstObj.getCategory(kinst)
                    
            except:
                raise madrigal.admin.MadrigalError('Error in instData.txt parsing metadata row %i: ' % (i) + str(category),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        categoryIDKeys = list(localDict.keys())
        categoryIDKeys.sort()
        retList = [(catID, localDict[catID]) for catID in categoryIDKeys]
        return(retList)
        


    def getInstruments(self, categoryID=0, local=False):
        """getInstruments returns the a list of (kinst, instrument desc, siteID) tuples.

        Inputs: 
        
            categoryID - category id to return instruments with data for. If 0, return all
            local - if False, return all instruments with that category for which there is data 
                anywhere in Madrigal. If True, only return local instruments, in which case siteID
                is always the local siteID.
        
        Returns:  a list of (kinst, instrument desc, siteID) tuples

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        siteID = self.__madDB.getSiteID()
            
        localDict = {} # key is kinst, value is tuple if (instrument desc str, siteID).  
                       #  Will be converted to list before returned
        
        for i, data in enumerate(self.__fileList):
            # find matching  code 
            try:
                kinst = int(data[self._kinstCol])
                thisSiteID = int(data[self._siteIDCol])
                thisCategoryID = self._madInstObj.getCategoryId(kinst)
                if thisCategoryID != categoryID and categoryID != 0:
                    continue
                if local:
                    if thisSiteID != siteID:
                        continue

                if kinst not in list(localDict.keys()):
                    localDict[kinst] = (self._madInstObj.getInstrumentName(kinst), thisSiteID)
                else:
                    if thisSiteID == siteID:
                        # local data overrides remote
                        localDict[kinst] = (self._madInstObj.getInstrumentName(kinst), thisSiteID)
                    
            except:
                raise madrigal.admin.MadrigalError('Error in instData.txt parsing metadata row %i: ' % (i) + str(categoryID),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        kinstKeys = list(localDict.keys())
        kinstKeys.sort()
        retList = [(kinst, localDict[kinst][0], localDict[kinst][1]) for kinst in kinstKeys]
        return(retList)
    
    
    def getInstrumentYears(self, kinst):
        """getInstrumentYears returns the a list of years (int) for instrument.

        Inputs: 
        
            kinst - the instrument id
        
        Returns:  an ordered list of years as integers.  If none found, raises error

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        retList = []
        for i, data in enumerate(self.__fileList):
            # find matching  code 
            try:
                thisKinst = int(data[self._kinstCol])
                if thisKinst != kinst:
                    continue
                yearsStr = data[self._yearsCol]

                retList = [int(year) for year in yearsStr.split()]
                retList.sort()
                    
            except:
                raise madrigal.admin.MadrigalError('Error in instData.txt parsing metadata row %i: ' % (i) + str(category),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
                
        if len(retList) == 0:
            raise madrigal.admin.MadrigalError('No data found for kinst %i' % (kinst), '')
        
        return(retList)
    
    

class MadrigalMetadata:
    """MadrigalMetadata is a private class that parses a Madrigal metadata file.


    This private class is used by all classes that need to parse a Madrigal
    metadata file.  If the class is called with the name of the metadata file only,
    the metadata file is assumed to be at $MAD_ROOT/metadata.  If a full path name is
    given that includes a directory separator, then that is used instead.  The getList
    method returns a list with one item for each line in the file.  That item for each
    line is simply a list of strings found in the line.  The following is an example
    metadata file and the list the method getList would return.

    Metadata file example::

        Tom, Dick,,Harry
        ,,Joe,
        Sally, Jane,Joe, Dick

    The list returned by getList example::

        [['Tom', 'Dick', '', 'Harry'],
        ['', '', 'Joe', ''],
        ['Sally', 'Jane', 'Joe', 'Dick']]

        
    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. Unable to open metadata file.
	
        2. All lines in metadata file do not have same number of items

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 8, 2001

    """

    # constants

    # file delimiter - presently a comma
    __DELIMITER = ','
    

    def __init__(self, metadataFileName, madDB=None, allowedLenList=None, key=None):
        """__init__ initializes the MadrigalCategoryList by reading from __privateList.

        Inputs: String metadataFileName - if not a full path, then MadrigalDB.getMetadataDir
            is included.

            Existing MadrigalDB object, by default = None.
            
            allowedLenList - a list of allowed lengths (integers) for data lines.  If None (the default),
            then the rule is that all lengths must be the same.
            
            key - which position in the file to use as a key.  Creates a attribute called self._dict,
            which is a dictionary with keys = item in key position, value = line index.  Used for
            faster lookup by key.  If None (the default), self._dict = None
        
        Returns: void

        Affects: Initializes private member variable __categoryList.

        Exceptions: None.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB
        
        # create empty list to hold parsed data
        self.__fileList = []

        # used to check that every line has same number of words if allowedLenList == None
        self.__numWords = 0
        self._allowedLenList = allowedLenList
        
        if not key is None:
            self._dict = {}
        else:self._dict = None
        
        # get real filename 
        self.__fileName = self.__getFullPath(metadataFileName)

        # open configuration file
        try:
            self.__file = open(self.__fileName, "r")
            
        except IOError:
            raise madrigal.admin.MadrigalError("Unable to open metadata file " + self.__fileName,
                                                       traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
                                                       

        # loop over each line in file, creating a list for each
        line = self.__file.readline()
        
        count = 0
        while (len(line.strip())):
            self.__parseLine(line, key, count)
            line = self.__file.readline()
            count += 1

        # close metadata file
        self.__file.close()
            

    def __parseLine(self, line, key=None, count=None):
        """__parseLine adds a list of items in line to __fileList.

        Inputs: 
            Line of file to parse (String).
            key - position to add key for self._dict, value = line position
                If None (the default), ignore
            count - if None, ignore.  Else add to self._dict as value (see key above)
        
        
        Returns: void

        Affects: Adds a list of items in line to self.__fileList.

        Exceptions: None.
        """
        list = line.split(self.__DELIMITER)

        # create new list with leading and trailing whitespace stripped

        strippedList = []
        
        for word in list:
            strippedList.append(word.strip())

        self.__fileList.append(strippedList)
        
        if not self._dict is None:
            self._dict[list[key]] = count

        # check correct number of words
        if self._allowedLenList:
            if len(strippedList) not in self._allowedLenList:
                raise madrigal.admin.MadrigalError("Wrong number of items found in metadata file " + \
                                                   self.__fileName + " at line " + \
                                                   str(len(self.__fileList)),
                                                   [traceback.format_exc()])
        elif self.__numWords == 0:
            self.__numWords = len(strippedList)
            
        elif self.__numWords != len(strippedList):
            raise madrigal.admin.MadrigalError("Wrong number of items found in metadata file " + \
                                                       self.__fileName + " at line " + \
                                                       str(len(self.__fileList)),
                                                       [traceback.format_exc()])

    def __getFullPath(self, filename):
        """getFullPath returns the full path name of the metafile.
        
        Inputs: filename passed in as argument
        
        Returns: The full path name of the metafile.  If the filename argument already is
        a full path, filename is returned unchanged.  If the filename is simply the name
        of a metadata file, then $MAD_ROOT/metadata is appended

        Affects: Nothing

        Exceptions: None
        """

        if (len(os.path.dirname(filename)) != 0):
            return filename

        fullName = self.__madDB.getMetadataDir() + "/" + filename

        # normalize in case we run on a system without / as a separator

        return os.path.normpath(fullName)


    
    # public methods

    def getList(self):
        """getList returns the list of lists of items in each line in the metafile.
        
        Inputs: None
        
        Returns: The list of lists of items in each line in the metafile.  That is, each
        item in the returned list is itself a list, representing a single line in the
        metafile. That single line's list is the list of items in that line.

        Affects: Nothing

        Exceptions: None
        """

        return(self.__fileList)
    
    
    def getDict(self):
        """getDict returns self_dict, which will be a dict with keys= key column set, value = line number,
        of None if no key passed into constructor
        """
        return(self._dict)
    

    def toString(self):
        """toString returns a simple string representation of a MadrigalMetadata object.

        Inputs: None
        
        Returns: String describing a simple representation of a __MadrigalMetadat object.

        Affects: Nothing

        Exceptions: None
        """

        return str(self.__fileList)


