# -*- coding: utf-8 -*-

"""The metadata module provides access to all metadata about one particular madrigal database.

This metadata is presently read from the files in the metadata directory, and from the
madrigal.cfg file.  If that file cannot be found, hard-coded values set during installation
are used instead.  If the madrigal.cfg file is found at the location specified by either
the madroot enviroment variable or at the hard-coded value of madroot set at installation, then
the parameters are read from the madrigal.cfg file.  Note that madroot with caps is only written
once in this file before installation, since it will be automatically replaced, so it is referred
to by MAD_ROOT or MAD+ROOT.

As of (whenever I finish this), metadata text files are used to populate the metadata.db SQLite
database. Old metadata text files are kept for posterity and backwards compatibility.

$Id: metadata_original.py 7740 2025-01-09 14:44:56Z kcariglia $
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
import madrigal.metadata
import packaging.version
import sqlite3

# third party imports
import filelock
import h5py
import numpy
import pandas

# Madrigal imports
import madrigal.admin
import madrigal._derive

METADB = "metadata.db"

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


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.getMetadataDir(), METADB))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

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

        # use input args to format condition strings
        # for lists: where <column name> in <arg list>
        # first, get all expIDs necessary
        # then get files based on expIDs

        expConditions = []
        fileConditions = []

        if expName:
            thisCond = "name=\"{}\"".format(expName)
            expConditions.append(thisCond)

        if kinstList and (0 not in kinstList):
            if len(kinstList) == 1:
                thisCond = "kinst={}".format(kinstList[0])
                expConditions.append(thisCond)
            else:
                thisCond = "kinst in {}".format(tuple(kinstList))
                expConditions.append(thisCond)

        if kindatList and (0 not in kindatList):
            thisCond = "kindat in {}".format(tuple(kindatList))
            fileConditions.append(thisCond)

        expQuery = "SELECT id"
        if appendKinst:
            expQuery += ", kinst"

        if appendStartTime:
            expQuery += ", sdt"

        if startDate:
            startDate = datetime.datetime(startDate[0], startDate[1], startDate[2], startDate[3],
                                          startDate[4], startDate[5], tzinfo=datetime.timezone.utc)
            sDate = startDate.strftime("%Y%m%d%H%M%S")
            if startDayOfYear:
                startDayOfYear = datetime.datetime.strptime(startDayOfYear, "%j")
                jDate = startDayOfYear.strftime("%m%d")
                sDate = sDate[:4] + jDate + sDate[8:]
            thisCond = "sdt >= {}".format(sDate)
            expConditions.append(thisCond)
        elif startDayOfYear:
            startDayOfYear = datetime.datetime.strptime(startDayOfYear, "%j")
            jDate = "____" + startDayOfYear.strftime("%m%d") + "______"
            thisCond = "sdt LIKE {}".format(jDate)
            expConditions.append(thisCond)
            
        if endDate:
            endDate = datetime.datetime(endDate[0], endDate[1], endDate[2], endDate[3],
                                        endDate[4], endDate[5], tzinfo=datetime.timezone.utc)
            eDate = endDate.strftime("%Y%m%d%H%M%S")
            if endDayOfYear:
                endDayOfYear = datetime.datetime.strptime(endDayOfYear, "%j")
                jDate = endDayOfYear.strftime("%m%d")
                eDate = eDate[:4] + jDate + eDate[8:]
            thisCond = "edt <= {}".format(eDate)
            expConditions.append(thisCond)
        elif endDayOfYear:
            endDayOfYear = datetime.datetime.strptime(endDayOfYear, "%j")
            jDate = "____" + endDayOfYear.strftime("%m%d") + "______"
            thisCond = "edt LIKE {}".format(jDate)
            expConditions.append(thisCond)

        match publicAccessOnly:
            case 0:
                thisCond = "security in (0, 1)"
                expConditions.append(thisCond)
            case 1:
                expCond = "security=0"
                expConditions.append(expCond)
                fileCond = "permission=0"
                fileConditions.append(fileCond)
            case 2:
                expCond = "security in (0, 2)"
                expConditions.append(expCond)
                fileCond = "permission=0"
                fileConditions.append(fileCond)

        if not includeNonDefault:
            if includeRealtime:
                thisCond = "category in (1, 4)"
                fileConditions.append(thisCond)
            else:
                thisCond = "category=1"
                fileConditions.append(thisCond)

        # make sure to get local experiments only
        expQuery += " FROM expTab WHERE sid={} ".format(self.getSiteID())
        if expConditions:
            expQuery += " AND "
            for e in range(len(expConditions)):
                expQuery += expConditions[e]

                if e < (len(expConditions) - 1):
                    expQuery += " AND "

        try:
            self.__initMetaDBConnector()
            res = self.__cursor.execute(expQuery)
            resList = res.fetchall()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem running expQuery in getFileListFromMetadata", 
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))

        if not resList:
            # didn't find anything
            return(resList)
        
        epoch_1950 = datetime.datetime(1950, 1, 1, 0, 0, 0, tzinfo=datetime.timezone.utc)

        # ids is dict with key: id, value: index
        # kinsts/times are dicts with key: index, value: kinst/time
        ids = {resList[i][0]:i for i in range(len(resList))}
        kinsts = {}
        times = {}
        if not appendKinst and not appendStartTime:
            # resList only contains expIDs
            pass # already got expIDs
        elif appendKinst and not appendStartTime:
            # resList contains: [expID, kinst]
            kinsts = {i:resList[i][1] for i in range(len(resList))}
        elif appendStartTime and not appendKinst:
            # resList contains [expID, sDT]
            times = {i:(datetime.datetime(int(resList[i][1][0:4]),
                          int(resList[i][1][4:6]),
                          int(resList[i][1][6:8]),
                          int(resList[i][1][8:10]),
                          int(resList[i][1][10:12]),
                          int(resList[i][1][12:14]), tzinfo=datetime.timezone.utc) - epoch_1950).seconds for i in range(len(resList))}
        else:
            # resList contains [expID, kinst, sDT]
            kinsts = {i:resList[i][1] for i in range(len(resList))}
            times = {i:(datetime.datetime(int(resList[i][2][0:4]),
                          int(resList[i][2][4:6]),
                          int(resList[i][2][6:8]),
                          int(resList[i][2][8:10]),
                          int(resList[i][2][10:12]),
                          int(resList[i][2][12:14]), tzinfo=datetime.timezone.utc) - epoch_1950).seconds for i in range(len(resList))}
        
        # dont wanna query fileTab one expID at a time,
        # instead get results for all found expIDs then use
        # index of expID in ids list to get corresponding
        # kinst and start time

        fileQuery = "SELECT fname, eid FROM fileTab"
        if len(ids.keys()) > 1:
            eidCond = "eid in {}".format(tuple(ids.keys()))
        else:
            eidCond = "eid={}".format(list(ids.keys())[0])
        fileConditions.append(eidCond)

        if fileConditions:
            fileQuery += " WHERE "
            for e in range(len(fileConditions)):
                fileQuery += fileConditions[e]

                if e < (len(fileConditions) - 1):
                    fileQuery += " AND "

        try:
            self.__initMetaDBConnector()
            res = self.__cursor.execute(fileQuery)
            resList = res.fetchall()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem running fileQuery in getFileListFromMetadata",
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))

        # resList is [(fname, expID)]
        expObj = MadrigalExperiment(self)
        if not appendKinst and not appendStartTime:
            finalList = [os.path.join(self.getMadroot(), expObj.getExpDirByExpId(item[1]), os.path.basename(item[0])) for item in resList]
            return(finalList)
        elif appendKinst and not appendStartTime:
            finalList = [[os.path.join(self.getMadroot(), expObj.getExpDirByExpId(item[1]), os.path.basename(item[0])), kinsts[ids[item[1]]]] for item in resList]
            return(finalList)
        elif appendStartTime and not appendKinst:
            finalList = [[os.path.join(self.getMadroot(), expObj.getExpDirByExpId(item[1]), os.path.basename(item[0])), times[ids[item[1]]]] for item in resList]
            return(finalList)
        else:
            finalList = [[os.path.join(self.getMadroot(), expObj.getExpDirByExpId(item[1]), os.path.basename(item[0])), kinsts[ids[item[1]]], times[ids[item[1]]]] for item in resList]
            return(finalList)
            

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
                        retList.append((relativeName, datetime.datetime.fromtimestamp(ts, tz=datetime.timezone.utc)))
                    else:
                        retList.append((fullname, datetime.datetime.fromtimestamp(ts, tz=datetime.timezone.utc)))
                    
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
                

    def getTableStr(self, tblName):
        """
        Returns a string representation of a table in metadata.db given by tblName, 
        reminiscent of old tblName.txt metadata files. 

        Inputs: string tblName (one of the following: 'instParmTab', 'typeTab',
                  'parmCodes', 'madCatTab', 'instType',
                  'instTab', 'siteTab', 'fileTab', 'expTab')
        Returns: comma/newline delimited string representation of table given by tblName
        """
        tables = ['instParmTab', 'typeTab',
                  'parmCodes', 'madCatTab', 'instType',
                  'instTab', 'siteTab', 'fileTab', 'expTab']
        
        if tblName not in tables:
            raise madrigal.admin.MadrigalError("Table name {} does not exist".format(tblName),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        if "expTab" in tblName:
            query = "SELECT id, url, name, sid, sdt, edt, kinst, security, pi, piemail FROM {}".format(tblName)
        elif "fileTab" in tblName:
            query = "SELECT fname, eid, kindat, category, fsize, catrec, headrec, amoddate, amodtime, status, permission, fanalyst, fanalystemail FROM {}".format(tblName)
        else:
            query = "SELECT * FROM {}".format(tblName)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            textList = []
            for line in resList:
                line = [str(l) for l in line]
                if "expTab" in tblName:
                    # separate start/end date/time to remain consistent with old expTab format
                    line = line[:4] + [line[4][:8], line[4][8:]] + [line[5][:8], line[5][8:]] + line[6:]
                textList.append(','.join(line))
            tblText = '\n'.join(textList)
            return(tblText)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem accessing table: {}".format(tblName),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def getExpUrls(self):
        """
        Returns all experiment urls present in expTab. Used to check for duplicate expTab entries.
        Represented as a dictionary {url:url} for improved search performance.
        
        Inputs: None
        Returns: Dictionary of all expTab urls, {url:url}
        """
        query = "SELECT url FROM expTab"

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            urls = {item[0]:item[0] for item in resList}
            return(urls)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem getting expUrls",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __getFnameEIDCombos(self):
        """
        Private helper function to be used with updateMaster.
        Returns all filename and experiment ID combinations present in fileTab. Used to check for
        duplicate fileTab entries. Represented as a dictionary {(fname, eid):(fname, eid)} for
        improved search performance. 
        Also returns dictionary of experiment IDs {eid:eid}.

        Inputs: None
        Returns: Dictionary of filename and expID combos, {(fname, eid):(fname, eid)} and dictionary
        of expIDs {eid:eid}
        """
        query = "SELECT fname, eid FROM fileTab"
        equery = "SELECT id FROM expTab"

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            combos = {i:i for i in resList}
            result = self.__cursor.execute(equery)
            resList = result.fetchall()
            eids = {i[0]:i[0] for i in resList}
            self.__closeMetaDBConnector()
            
            return(combos, eids)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem getting fname + expID combos",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __cleanExpText(self, expText):
        """
        Private helper function to be used with updateMaster.
        Parses text read from any expTab.txt file and formats into a
        list of tuples containing experiment metadata, to be inserted
        into metadata.db.

        Note: won't work for initial database creation.

        Inputs: comma/newline delimited string read from any expTab.txt file
        Returns: list of tuples containing experiment data
        """
        expUrls = self.getExpUrls()
        expLines = expText.split('\n')
        splitList = [line.split(',') for line in expLines]
        # append empty strings if no pi info
        noPiLines = [(line + ["", ""]) for line in splitList if (len(line) == 10)]
        otherLines = [line for line in splitList if (len(line) == 12)]
        expList = otherLines + noPiLines

        expData = [tuple([expList[i][0], # eid
                        expList[i][1],   # url
                        expList[i][2],   # ename
                        expList[i][3],   # site id
                        expList[i][4] + f'{expList[i][5]:>06}', # start dt
                        expList[i][6] + f'{expList[i][7]:>06}', # end dt
                        expList[i][8],   # kinst
                        expList[i][9],   # security
                        expList[i][10],  # pi
                        expList[i][11]]) # piemail
                        for i in range(len(expList)) if ((expList[i][1] not in expUrls) )] # and ((expList[i][9] == '0') or (expList[i][9] == '2'))

        # criteria: url does not already exist #and security == 0 | 2
        return(expData)
    

    def __cleanFileText(self, fileText):
        """
        Private helper function to be used with updateMaster. 
        Parses text read from any fileTab.txt file and formats into a
        list of tuples containing file metadata, to be inserted into metadata.db.

        Note: won't work for initial database creation.

        Inputs: comma/newline delimited string read from any fileTab.txt file
        Returns: list of tuples containing file data
        """
        fnameEIDCombos, eids = self.__getFnameEIDCombos()
        fileLines = fileText.split('\n')
        splitList = [line.split(',') for line in fileLines]
        # append empty strings if no file analyst info
        noAnalyst = [tuple(line + ["", ""]) for line in splitList if (len(line) == 11)]
        otherLines = [tuple(line) for line in splitList if (len(line) == 13)]
        fileList = otherLines + noAnalyst

        # criteria: (fname, eid) does NOT exist, but eid does exist and permission = 0 (public)
        fileData = [line for line in fileList if (((line[0], int(line[1])) not in fnameEIDCombos) and ((int(line[1]) in eids)))] #  and (line[10] == '0')
        return(fileData)
    

    def addExperimentsMetadata(self, expText):
        """
        Add experiment metadata to expTab from string data passed in expText.

        Inputs: comma/newline delimited string expText as read from any expTab.txt file
        Returns: None
        """
        expData = self.__cleanExpText(expText)
        if not expData:
            return
        template = """INSERT INTO expTab('id', 'url', 'name', 'sid', 'sdt', 'edt', 'kinst', 'security', 'pi', 'piemail') VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

        try:
            self.__initMetaDBConnector()
            self.__cursor.executemany(template, expData)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem adding experiment metadata: {}".format(expData),
                                          traceback.format_exception(sys.exc_info()[0],
                                                                    sys.exc_info()[1],
                                                                    sys.exc_info()[2]))
            
        
    def addFilesMetadata(self, fileText):
        """
        Add file metadata to fileTab from string data passed in fileText.

        Inputs: comma/newline delimited string fileText as read from any fileTab.txt file
        Returns: None
        """
        fileData = self.__cleanFileText(fileText)
        if not fileData:
            return
        template = """INSERT INTO fileTab(fname, eid, kindat, category, fsize, catrec, headrec, amoddate, amodtime, status, permission, fanalyst, fanalystemail) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""
            
        try:
            self.__initMetaDBConnector()
            self.__cursor.executemany(template, fileData)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem adding file metadata: {}".format(fileData),
                                          traceback.format_exception(sys.exc_info()[0],
                                                                    sys.exc_info()[1],
                                                                    sys.exc_info()[2]))
        
    def updateInstType(self, text):
        """
        Check text for updates to instType-- if updates are present, add to metadata.db.

        Note that instType is a "special" table insofar as there is no MadrigalInstrumentCategory class--
        use MadrigalDB (or directly access metadata.db (not recommended though).)

        Inputs: comma/newline delimited string text as read from any instType.txt file
        Returns: None
        """ 
        # search by instType description because category int key not guaranteed to be unique
        # among other madrigal sites
        qtemplate = "SELECT * FROM instType"
        updatetemplate = """INSERT INTO instType VALUES(?, ?)"""

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(qtemplate)
            resList = result.fetchall()

            categoryDescs = {item[1]:item[1] for item in resList}
            maxCatID = numpy.max([item[0] for item in resList])
            newCatID = maxCatID + 1

            for line in text:
                line = line.rstrip()
                line = line.split(',')

                if line[1] not in categoryDescs:
                    self.__cursor.execute(updatetemplate, (newCatID, line[1]))
                    self.__connector.commit()
                    newCatID += 1

            self.__closeMetaDBConnector()
            print("instType updated successfully")
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem updating instType",
                                          traceback.format_exception(sys.exc_info()[0],
                                                                    sys.exc_info()[1],
                                                                    sys.exc_info()[2]))
        

    def validateParmCodes(self):
        """
        Validate parmCodes metadata. 
        Parameters with code = 0 cannot be stored in a CEDAR file and are intended to be derivable parameters only.
        Mnemonics already guaranteed to be unique via parmCodes primary key. 
        All nonzero parameter codes must be unique. 

        Affects: Nothing
        Returns: Bool (True if valid, False otherwise)
        """
        isValid = True

        query = "SELECT code, mnem, category FROM parmCodes"

        try:
            self.__initMetaDBConnector()
            res = self.__cursor.execute(query)
            resList = res.fetchall()
            self.__closeMetaDBConnector()

            # rearrange results into dict of key: code, value: [(mnem, category)]
            parmDict = {}
            for parmData in resList:
                if parmData[0] in parmDict.keys():
                    # if this parm code is not 0, then it's a duplicate
                    if int(parmData[0]) == 0:
                        # must be derivable or test parm
                        parmDict[parmData[0]].append((parmData[1], parmData[2]))
                    else:
                        # duplicate parm
                        print(f"Found invalid duplicate parameter code {parmData[0]}")
                        isValid = False
                else:
                    # new parm for parmDict
                    parmDict[parmData[0]] = []

            return(isValid)

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Unable to validate parmCodes",
                                          traceback.format_exception(sys.exc_info()[0],
                                                                sys.exc_info()[1],
                                                                sys.exc_info()[2]))


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
        # if 'expTab.txt' in names:
        #     self.__hasExpTab = 1
        # else:
        #     self.__hasExpTab = 0

        # if 'fileTab.txt' in names:
        #     self.__hasFileTab = 1
        # else:
        #     self.__hasFileTab = 0
            
        # if arg[__includeNonMad] == 0:
        #     if not self.__hasExpTab or not self.__hasFileTab:
        #         return


        # NOTE 2 SELF; probably get rid of includeNonMadrigal arg?


        # apply all filters relating to expTab.txt if self.__hasExpTab
        try:
            
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

        except:
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
        try:
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

            # what exactly is a non-madrigal file?

            # now add all non-madrigal files if includeNonMad
            # if arg[__includeNonMad] == 1:
            #     for name in names:
            #         # skip dir names
            #         if os.path.isdir(dirname + '/' + name):
            #             continue
            #         # if its not in fileTab.txt, add it
            #         if fileMeta.getExpIdByFilename(name) == None:
            #             if arg[__appendKinst] == 0 and arg[__appendStartTime] == 0:
            #                 # return only file names
            #                 arg[__fileList].append(dirname + '/' + name)
            #             elif arg[__appendStartTime] == 0:
            #                 # return file name, inst code tuple
            #                 arg[__fileList].append((dirname + '/' + name, expKinst))
            #             elif arg[__appendKinst] == 0:
            #                 # return file name, start time tuple
            #                 arg[__fileList].append((dirname + '/' + name, thisStartTime))
            #             else:
            #                 # append both
            #                 arg[__fileList].append((dirname + '/' + filename, expKinst, thisStartTime))

        # else the file has no fileTab.txt and includeNonMad is true - include all files
        except madrigal.admin.MadrigalError as e:
            print(e.getExceptionStr())
        
        # arg[__includeNonMad] == 1:
        #     for name in names:
        #         # skip dir names
        #         if os.path.isdir(dirname + '/' + name):
        #             continue
        #         if arg[__appendKinst] == 0 and arg[__appendStartTime] == 0:
        #             # return only file names
        #             arg[__fileList].append(dirname + '/' + name)
        #         elif arg[__appendStartTime] == 0:
        #             # return file name, inst code tuple
        #             arg[__fileList].append((dirname + '/' + name, expKinst))
        #         elif arg[__appendKinst] == 0:
        #             # return file name, start time tuple
        #             arg[__fileList].append((dirname + '/' + name, thisStartTime))
        #         else:
        #             # append both
        #             arg[__fileList].append((dirname + '/' + filename, expKinst, thisStartTime))
                

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
                    fileMetaObj.writeMetadata()
                    
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

    This object provides access to all Madrigal site information, as read from siteTab in metadata.db.

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
    __siteMetadataFile  = METADB
    _defaultVersion = '3.0'
    __tblName = "siteTab"

    # column names
    __siteIDCol         =  "id"
    __siteNameCol       =  "name"
    __madServerCol      =  "server"
    __madDocRootCol     =  "docdir"
    __madCGICol         =  "cgidir"
    __madServletCol     =  "servlet"
    __contactNameCol    =  "cname"
    __contactAddr1Col   =  "cadr1"
    __contactAddr2Col   =  "cadr2"
    __contactAddr3Col   =  "cadr3"
    __contactCityCol    =  "ccity"
    __contactStateCol   =  "cstate"
    __contactZipCol     =  "ccode"
    __contactCountryCol =  "ccountry"
    __contactPhoneCol   =  "cphone"
    __contactEmailCol   =  "cemail"
    __siteVersionCol    =  "version"
    

    def __init__(self, madDB=None):
        """__init__ initializes MadrigalSite by reading from metadata.db.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown if metadata.db not opened or parsed successfully.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get site metadata file
        self.__filename = self.__siteMetadataFile


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getSiteName(self, siteID):
        """getSiteName returns the site name that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site name that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__siteNameCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[name]] = resList
            return(name)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Name for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))



    def getSiteServer(self, siteID):
        """getSiteServer returns the site server (e.g., www.haystack.mit.edu) that matches siteID argument, or None if not found.

        Inputs: siteID integer to get site server.
        
        Returns: the site server that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__madServerCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[server]] = resList
            return(server)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Server for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))



    def getSiteDocRoot(self, siteID):
        """getSiteDocRoot returns the relative document root (e.g. madrigal)  that matches siteID argument, or None if not found.

        Inputs: siteID integer to get document root path.
        
        Returns: the document root path that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__madDocRootCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[docroot]] = resList
            return(docroot)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("DocRoot for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))



    def getSiteRelativeCGI(self, siteID):
        """getSiteRelativeCGI returns the relative cgi path (e.g.cgi-bin/madrigal)  that matches siteID argument, or None if not found.
        
        For Madrigal 3.0 sites and later, this returns getSiteDocRoot, and the cgi field is ignored.

        Inputs: siteID integer to get relative cgi path.
        
        Returns: the relative cgi path that matches siteID argument, or None if not found. Not meaningful for Madrigal 3.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        version = self.getSiteVersion(siteID)
        if packaging.version.parse(version) >= packaging.version.parse('3.0'):
            return(self.getSiteDocRoot(siteID))
        
        query = "SELECT " + self.__madCGICol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cgidir]] = resList
            return(cgidir)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("CGIDir for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def getSiteContactName(self, siteID):
        """getSiteContactName returns the site contact name that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact name that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactNameCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cname]] = resList
            return(cname)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Contact name for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def getSiteAddress1(self, siteID):
        """getSiteAddress1 returns the site address 1 that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact address 1 that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactAddr1Col + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cadr1]] = resList
            return(cadr1)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Address1 for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

    
    def getSiteAddress2(self, siteID):
        """getSiteAddress2 returns the site address 2 that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact address 2 that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactAddr2Col + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cadr2]] = resList
            return(cadr2)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Address2 for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSiteAddress3(self, siteID):
        """getSiteAddress3 returns the site address 3 that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact address 3 that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactAddr3Col + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cadr3]] = resList
            return(cadr3)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Address3 for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSiteCity(self, siteID):
        """getSiteCity returns the site city that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact city that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactCityCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[ccity]] = resList
            return(ccity)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("City for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSiteState(self, siteID):
        """getSiteState returns the site state that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact state that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactStateCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cstate]] = resList
            return(cstate)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("State for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSitePostalCode(self, siteID):
        """getSitePostalCode returns the site postal code that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact postal code that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactZipCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[ccode]] = resList
            return(ccode)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Zip code for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSiteCountry(self, siteID):
        """getSiteCountry returns the site country that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact country that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactCountryCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[ccountry]] = resList
            return(ccountry)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Country for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSiteTelephone(self, siteID):
        """getSiteTelephone returns the site telephone that matches siteID argument, or None if not found.

        Inputs: siteID integer to get SiteName.
        
        Returns: the site contact telephone that matches siteID argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactPhoneCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cphone]] = resList
            return(cphone)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Phone for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    

    def getSiteEmail(self, siteID):
        """getSiteEmail returns the site email address that matches siteID argument, or None if not found.

        Inputs: siteID integer to get Site email address.
        
        Returns: the site email address that matches siteID argument, or None if not found.  To list multiple
        email addresses in this field separate them with semicolons (since commas are delimiters).  getSiteEmail
        will automatically replace semicolons with commas, as required by multiple email addresses.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__contactEmailCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cemail]] = resList
            return(cemail)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Email for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getSiteList(self):
        """getSiteList returns a list of all site ids and names.

        Inputs: None.
        
        Returns: a list of all site ids and names.  Each item in the list
        is a tuple of the form (Site id (integer), site name (string)).  Example item:
        (1,'Millstone')

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__siteIDCol + ", " + self.__siteNameCol + " FROM " + self.__tblName
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            return(resList)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Error getting site list", 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def getSiteVersion(self, siteID):
        """getSiteVersion returns the site version string that matches siteID argument, or None if not found.

        Inputs: siteID integer
        
        Returns: the site version that matches siteID argument, or None if not found.  If file does
            not have this field, returns default value of 2.6.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__siteVersionCol + " FROM " + self.__tblName + " WHERE " + self.__siteIDCol + "={}".format(siteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[version]] = resList

            if not version:
                return(None)
            
            return(str(version))
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Version for siteID {} not found".format(siteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def setSiteVersionBySiteID(self, siteID, version):
        """setSiteVersionBySiteID sets the site Madrigal version (string) as period delimited integers 
        at given siteID.

        Inputs:

            siteID - siteID of site in list.

            version - string Madrigal version as period delimited integers (eg, 2.6)
        
        Returns: None.

        Affects: sets the Madrigal version as period delimited integers (eg, 2.6) for given site
        Exceptions: MadrigalError if any problems accessing metadata.db, or
        siteID not found.
        
        This method added in Madrigal 3.0
        """
        update = ("UPDATE " + self.__tblName + " SET " + self.__siteVersionCol + "={} WHERE " + self.__siteIDCol + "={}").format(version, siteID)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setSiteVersionBySiteID with args %s: %s' %  \
                                               (str(siteID, version)),
                                                [traceback.format_exc()])
                
                
    def writeMetadata(self, newFullPath=None):
        """writeMetadata writes a new version of the siteTab.txt file.
        
        Inputs: newFullPath:  a new path to write the siteTab.txt file to, if
        not the same as the original metadata file opened.  Defaults to None, which overwrites
        metadata file that was read from.
        
        Returns: None.

        Affects: Writes updated version of metadata file.

        Exceptions: If unable to write file
        """

        try:
            siteStr = self.__madDB.getTableStr(self.__tblName)

            if newFullPath:
                with open(newFullPath, "w") as f:
                    f.write(siteStr)
            else:
                with open(self.__madDB.getMetadataDir() + "/siteTab.txt", "w") as f:
                    f.write(siteStr)

        except:
            raise madrigal.admin.MadrigalError("Unable to write metadata file " + \
                                               str(newFullPath),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))
        
        

    def updateSiteTab(self, text):
        """
        Updates siteTab with parsed input text, formatted as it would be in siteTab.txt.
        Generally for use with checkOpenMadrigalMetadata in updateMaster.

        Inputs: text - comma/newline delimited site metadata, formatted as it would be in siteTab.txt
        Returns: None
        """ 
        qtemplate = "SELECT " + self.__siteIDCol + " FROM " + self.__tblName
        updatetemplate = """INSERT INTO siteTab VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(qtemplate)
            resList = result.fetchall()

            currentIDs = {item[0]:item[0] for item in resList}

            for line in text:
                line = line.rstrip()
                line = line.split(',')

                if line[0] not in currentIDs:
                    if len(line) == 16:
                        # no site version, use default (3.0)
                        line.append('3.0')
                    if len(line) == 17:
                        self.__cursor.execute(updatetemplate, line)
                        self.__connector.commit()

            self.__closeMetaDBConnector()
            print("siteTab updated successfully")
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem updating siteTab",
                                          traceback.format_exception(sys.exc_info()[0],
                                                                    sys.exc_info()[1],
                                                                    sys.exc_info()[2]))




class MadrigalInstrument:
    """MadrigalInstrument is an object that provides access to Madrigal instrument info from the metadata.

    This object provides access to all Madrigal instrument information in the instTab
    and instType metadata tables. 

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
    __instMetadataFile  = METADB
    __tblName           = "instTab"
    __tbl2Name          = "instType"

    # column positions
    __instKinstCol      =  "kinst"
    __instMnemonicCol   =  "mnem"
    __instNameCol       =  "name"
    __latitudeCol       =  "lat"
    __longitudeCol      =  "lon"
    __altitudeCol       =  "alt"
    __contactNameCol    =  "cname"
    __contactAddr1Col   =  "cadr1"
    __contactAddr2Col   =  "cadr2"
    __contactAddr3Col   =  "cadr3"
    __contactCityCol    =  "ccity"
    __contactStateCol   =  "cstate"
    __contactZipCol     =  "ccode"
    __contactCountryCol =  "ccountry"
    __contactPhoneCol   =  "cphone"
    __contactEmailCol   =  "cemail"
    __categoryCol       =  "category"
    __descCol           =  "desc"


    def __init__(self, madDB=None):
        """__init__ initializes MadrigalInstrument by reading from instTab in metadata.db.

        Inputs:

            madDB - Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown if any problems accessing metadata.db.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get instrument metadata file
        self.__filename = self.__instMetadataFile


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getInstrumentName(self, kinst):
        """getInstrumentName returns the instrument name that matches kinst argument, or None if not found.

        Inputs: kinst integer to get instrument name.
        
        Returns: the instrument name that matches kinst argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__instNameCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[name]] = resList
            return(name)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Name for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getInstrumentMnemonic(self, kinst):
        """getInstrumentMnemonic returns the 3 char instrument mnemonic that matches kinst argument, or None if not found.

        Inputs: kinst integer to get instrument mnemonic.
        
        Returns: the instrument mnemonic that matches kinst argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__instMnemonicCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[mnem]] = resList
            return(mnem)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Mnemonic for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getLatitude(self, kinst):
        """getLatitude returns the latitude as a float that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get latitude.
        
        Returns: the latitude as a float that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__latitudeCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[lat]] = resList
            return(float(lat))
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Latitude for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getLongitude(self, kinst):
        """getLongitude returns the longitude as a float that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get longitude.
        
        Returns: the longitude as a float that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__longitudeCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[lon]] = resList
            return(float(lon))
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Longitude for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
	
	
    def getAltitude(self, kinst):
        """getAltitude returns the altitude as a float that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get altitude.
        
        Returns: the altitude in km above sea level as a float that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__altitudeCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[alt]] = resList
            return(float(alt))
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Altitude for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def getContactName(self, kinst):
        """getContactName returns the contact name as a string that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get contact name.
        
        Returns: the contact name as a string that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        
        This method added in Madrigal 2.6
        """
        query = "SELECT " + self.__contactNameCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cname]] = resList
            return(cname)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Contact name for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def getContactAddress1(self, kinst):
        """getContactAddress1 returns the contact address 1 field (institution) as a string that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get contact name.
        
        Returns: the contact address 1 as a string that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        
        This method added in Madrigal 3
        """
        query = "SELECT " + self.__contactAddr1Col + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cadr1]] = resList
            return(cadr1)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Contact address1 for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    def getContactEmail(self, kinst):
        """getContactEmail returns the contact email as a string that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get contact email.
        
        Returns: the contact email as a string that matches kinst argument, or None if not found or blank.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        
        This method added in Madrigal 2.6
        """
        query = "SELECT " + self.__contactEmailCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[cemail]] = resList
            return(cemail)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Contact email for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getCategory(self, kinst):
        """getCategory returns the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Inputs: kinst integer to get altitude.
        
        Returns: the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query1 = "SELECT " + self.__categoryCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query1)
            resList = result.fetchall()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[categoryID]] = resList

            query2 = "SELECT " + self.__descCol + " FROM " + self.__tbl2Name + " WHERE " + self.__categoryCol + "={}".format(categoryID)

            result = self.__cursor.execute(query2)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)
            [[desc]] = resList
            return(desc)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Category for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getCategoryId(self, kinst):
        """getCategory returns the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Inputs: kinst integer to get altitude.
        
        Returns: the instrument category that matches kinst argument as a string, or
        None if kinst not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__categoryCol + " FROM " + self.__tblName + " WHERE " + self.__instKinstCol + "={}".format(kinst)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            # should be exactly one item in resList now
            [[category]] = resList
            return(int(category))
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("CategoryID for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getInstrumentList(self):
        """getInstrumentList returns a list of all instrument names, mnemonics, and their kinst values.

        Inputs: None.
        
        Returns: a list of all instrument names, mnemonics, and their kinst values.  Each item in the list
        is a tuple of the form (Instrument Name (string), mnemonic (string), kinst (integer)).  Example item:
        ('Millstone Hill UHF Steerable Antenna', 'mlh', 31)

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__instNameCol + ", " + self.__instMnemonicCol + ", " + self.__instKinstCol + " FROM " + self.__tblName
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            
            if not resList:
                return(None)

            return(resList)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Error getting instrument list", 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getOrderedInstrumentList(self):
        """getOrderedInstrumentList returns a list of all (instrument names, mnemonics, kinst, categories, categoryId),
        ordered by categoryId and then kinst.

        Inputs: None.
        
        Returns: a list of tuples of (instrument name, mnemonic, kinst, category, categoryId) ordered by categoryId and
        then kinst.    Example item:
        ('Millstone Hill UHF Steerable Antenna', 'mlh', 31, 'Incoherent Scatter Radars', 1)

        Affects: None

        Exceptions: MadrigalError if any problems reading metadata.db
        """

        try:
            instList = self.getInstrumentList()

            retList = [(inst[0], inst[1], inst[2], self.getCategory(inst[2]), self.getCategoryId(inst[2])) for inst in instList]

            if not retList:
                return(None)

            retList.sort(key=self.__instrumentSort) 
            return(retList)
            
        except:
            raise madrigal.admin.MadrigalError("Error getting ordered instrument list", 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def updateInstTab(self, text):
        """
        Updates instTab with parsed input text, formatted as it would be in instTab.txt.
        Generally for use with checkOpenMadrigalMetadata in updateMaster.

        Inputs: text - comma/newline delimited site metadata, formatted as it would be in instTab.txt
        Returns: None
        """ 
        qtemplate = "SELECT " + self.__instKinstCol + " FROM " + self.__tblName
        updatetemplate = """INSERT INTO instTab VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"""

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(qtemplate)
            resList = result.fetchall()

            currentKinsts = {item[0]:item[0] for item in resList}

            for line in text:
                line = line.rstrip()
                line = line.split(',')

                if line[0] not in currentKinsts:
                    self.__cursor.execute(updatetemplate, line)
                    self.__connector.commit()

            self.__closeMetaDBConnector()
            print("instTab updated successfully")
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem updating siteTab",
                                          traceback.format_exception(sys.exc_info()[0],
                                                                    sys.exc_info()[1],
                                                                    sys.exc_info()[2]))


    def __instrumentSort(self, thisInst):
        """instrumentSort is a private method used to sort tuples of instrument data
        """
        return((thisInst[4], thisInst[2]))
    


class MadrigalInstrumentParameters:
    """MadrigalInstrumentParameters is an object that provides access to the metadata table that summarizes the parameters associated with each instrument.

    This object provides access to all Madrigal instrument parameter information in metadata.db.
    The metadata table instParmTab lists, for any given instrument, all the measured parameters found in all the
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
    __instParmMetadataFile  = METADB
    __tblName               = "instParmTab"

    # column positions
    __instParmKinstCol  =  "kinst"
    __instParmListCol   =  "parm"

    def __init__(self, madDB=None):
        """__init__ initializes MadrigalInstrumentParameters by reading from metadata.db.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown if any problems accessing metadata.db.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get instrument parameter metadata file
        self.__filename = self.__instParmMetadataFile


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

        
    def getParameters(self, kinst):
        """getParameters returns a list of parameters in mnemonic form (strings or unknown integers as strings) that matches kinst argument, or None if not found or blank.

        Inputs: kinst integer to get parameters.  If 0, get parameters from all instruments.
        
        Returns: a list of mnemonic strings or unknown integer strings, or None if kinst not found or blank.

        Affects: None

        Exceptions: if error accessing metadata.db
        """
        
        try:
            self.__initMetaDBConnector()
            if kinst != 0:
                query = "SELECT " + self.__instParmListCol + " FROM " + self.__tblName + " WHERE " + self.__instParmKinstCol + "={}".format(kinst)
                result = self.__cursor.execute(query)
                resList = result.fetchall()
                self.__closeMetaDBConnector()
                
                if not resList:
                    return(None)

                retList = [item[0] for item in resList]
                return(retList)
            else:
                query = "SELECT " + self.__instParmListCol + " FROM " + self.__tblName
                result = self.__cursor.execute(query)
                resList = result.fetchall()
                self.__closeMetaDBConnector()
                
                if not resList:
                    return(None)

                retList = list(numpy.unique([item[0] for item in resList]))
                return(retList)
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Parameters for kinst {} not found".format(kinst), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def rebuildInstParmTable(self, completeRebuildFlag = 0):
        """rebuildInstParmTable rebuilds the instParmTab metadata table.

        The table instParmTab is a listing of every measured parameter found in any data file for a given
        instrument. It now will also import data from other Madrigal sites instParmTab tables.
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

        Finally, the instParmTab table of every other site is obtained via getMetadata, and those parameters are
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
        # filename = self.__madDB.getMetadataDir() + '/instParmTab.txt'
        
        # # throw an exception if instParmTab.txt file not writable
        # if not os.access(filename, os.W_OK):
        #     raise madrigal.admin.MadrigalError('Unable to write: ' + str(filename), None)
        # we should already be connected to metadata.db

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.__madDB.getMadroot()

	# now its safe to import madrigal.data since MAD + ROOT set
        import madrigal.data

	# get date of files to examine
        if completeRebuildFlag == 0:
            # try to open and read instParmLastUpdate.txt
            try:
                with open(self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt', 'r') as dateFile:
                    lastUpdateTime = float(dateFile.read())
                
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
            with open(self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt', 'w') as dateFile:
                newUpdateTime = time.time()
                dateFile.write(str(newUpdateTime))
        except:
            raise madrigal.admin.MadrigalError('Unable to write: ' + \
                                               self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt',
                                               None)
        
        # finally, make sure new instParmLastUpdate.txt is world-writable
        try:                
            os.chmod(self.__madDB.getMetadataDir() + '/instParmLastUpdate.txt', 0o666)
        except:
            pass

        # create a needed MadrigalParameters obj
        madParmObj = madrigal.data.MadrigalParameters(self.__madDB)

        # create a string to hold all the text for the new file
        # newFileStr = ''
        
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
                traceback.print_exc()
                continue
            
            # try to create MadrigalFile object for that file
            try:
                print('\t\tAdding parameters to instParmTab from ' + str(file))
                madFileObj = madrigal.data.MadrigalFile(file[0], self.__madDB)
                fileParmList = madFileObj.getMeasuredParmList()
                # append unique parameters
                for parm in fileParmList:
                    if not parm in instCodeDict[file[1]]:
                        instCodeDict[file[1]].append(parm)
            # if a problem with file, skip it
            except:
                traceback.print_exc()
                continue

        template = "INSERT INTO " + self.__tblName + " VALUES(?, ?)"

        # now create a new file string from instCodeDict
        # create a sorted list of keys
        keyList = list(instCodeDict.keys())
        keyList.sort()
        delimiter = ' '
        # step through each key
        try:
            self.__initMetaDBConnector()
            for key in keyList:
                # sort that instrument's list
                instParmList = madParmObj.normalizeParmList(instCodeDict[key])
                # convert from codes to mnemonics
                instParmList = madParmObj.getParmMnemonicList(instParmList)

                for parm in instParmList:
                    self.__cursor.execute(template, (key, parm))

                # append that instrument's data to newFileStr
                #newFileStr = newFileStr + str(key) + ','
                #newFileStr = newFileStr + delimiter.join(instParmList).lower() + '\n'

            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem rebuilding instParmTab', 
                                               traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


        
    
class MadrigalKindat:
    """MadrigalKindat is an object that provides access to Madrigal kind of data info from the metadata.

    This object provides access to all Madrigal kind of data information in the metadata table typeTab.

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
    __typeMetadataFile  = METADB
    __tblName           = "typeTab"

    # column positions
    __typeCodeCol   =  "kindat"
    __typeDescCol   =  "desc"

    

    def __init__(self, madDB=None):
        """__init__ initializes MadrigalKindat by reading from metadata.db.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown if problems accessing metadata.db.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get kindat metadata file
        self.__filename = self.__typeMetadataFile


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getKindatDescription(self, code, kinst=None):
        """getKindatDescription returns the kindat description that matches code argument, or None if not found.

        Inputs: 
        
            code integer to get kindat description. or integer as string
            
            kinst - used to look up kindat description in form '%i_%i' % (kinst, code).  If None,
                (the default) only return exact code match as integer.  If given, first try to match
                '%i_%i' % (kinst, code).  If not found, then try just code. May be integer or string integer
        
        Returns: the kindat description that matches code argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db.
        """
        kcode = None
        if kinst:
            kcode = str(kinst) + '_' + str(code)

        query = "SELECT " + self.__typeDescCol + " FROM " + self.__tblName + " WHERE " + self.__typeCodeCol + "=\"{}\""

        try:
            self.__initMetaDBConnector()

            result = self.__cursor.execute(query.format(kcode))
            resList = result.fetchall()

            if not resList:
                # try again with code instead of kcode
                result = self.__cursor.execute(query.format(code))
                resList = result.fetchall()

                if not resList:
                    return(None)
            
            self.__closeMetaDBConnector()
            [[desc]] = resList
            return(desc)
        
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Description for kindat {} not found".format(code), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getKindatList(self):
        """getKindatList returns a list of all kindat descriptions and codes.

        Inputs: None.
        
        Returns: a list of all kindat descriptions and codes.  Each item in the list
        is a tuple of the form (Kindat description (string), kindat code (integer or string in
        form '%i_%i' % (kinst, code))).  Example item:
        ('INSCAL Basic Derived Parameters', 3001)

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self.__typeDescCol + ", " + self.__typeCodeCol + " FROM " + self.__tblName

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
            
            
            return(resList)
        
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Unable to get kindat list", 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

    
class MadrigalExperiment:
    """MadrigalExperiment is an object that provides access to Madrigal experiment info from the metadata.

    This object provides access to all Madrigal experiment information in the metadata table expTab.

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

        1. MadrigalMetadata fails to open or read metadata database
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Apr. 17, 2002

    """

    #constants
    __expMetadataFile  = METADB
    __tblName          = "expTab"

    # column positions
    __expIdCol               =  'id'
    __expUrlCol              =  'url'
    __expNameCol             =  'name'
    __expSiteIdCol           =  'sid'
    __expStartDateTimeCol    =  'sdt'
    __expEndDateTimeCol      =  'edt'
    __expKinstCol            =  'kinst'
    __expSecurityCol         =  'security'
    __expPICol               =  'pi'
    __expPIEmailCol          =  'piemail'
    __expIdxCol              =  'idx'
    __index                  =  None # stays None unless initFile is not None
    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalExperiment by reading from metadata.db.

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
        self.__filename = self.__expMetadataFile

        if initFile:
            # get row position of this experiment within expTab
            self.__setIdxFromInit(initFile)
    

    def __setIdxFromInit(self, initFile):
        """
        __setIdxFromInit sets __index private variable used to refer to a specific (unique)
        experiment. Index is pulled from expTab entry where url contains expDir (from initFile path).
        Assumed to be a local experiment.

        Inputs: initFile, referring to expTab.txt (doesn't need to exist-- only need expDir from path to this file)

        Returns: void

        Affects: sets __index private variable used to refer to this specific experiment

        Exceptions: MadrigalError thrown if path to initFile contains malformed expDir, or
        if expDir not found
        """
        # first check that path to initFile contains expDir
        dir = os.path.dirname(initFile)
        dirConvStr1 = 'experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[a-zA-Z0-9\-_]*$'
        dirConvStr2 = 'experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[0-3][0-9][a-z][a-z0-9][a-z0-9][0-9][0-9].?'
        metaMatch = 'metadata'

        found = re.search(dirConvStr1, dir)
        if not found:
            found = re.search(dirConvStr2, dir)
            if not found:
                metaCheck = re.search(metaMatch, dir)
                if metaCheck:
                    # initFile points to main metadata file
                    return
                raise madrigal.admin.MadrigalError("Malformed expDir found: {}".format(dir),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        expDir = found.group(0) # should be exactly 1 expDir
       
        # make sure to use local experiment if initFile is also local
        if self.__madDB.getMadroot() in initFile:
            expDir = self.__madDB.getTopLevelUrl() + "/madtoc/" + expDir
        elif initFile.startswith("/experiments") or initFile.startswith("experiments"):
            # assume local experiment
            expDir = self.__madDB.getTopLevelUrl() + "/madtoc/" + expDir

        # now match found expDir to expUrl to find the correct index
        query = "SELECT " + self.__expIdxCol + ", " + self.__expUrlCol + " FROM " + self.__tblName + " WHERE " + self.__expUrlCol + " LIKE \"%{}%\"".format(expDir)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [idx] = [item[0] for item in resList if expDir in item[1]]
            self.__index = idx
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Could not find index for expDir: {}".format(expDir),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))



    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables (__connector and __cursor) to
        connect to metadata.db

        Exceptions: MadrigalError thrown if unable to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db

        Exceptions: MadrigalError thrown if unable to disconnect from metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getExpIdByPosition(self, position = 0):
        """getExpIdByPosition returns the experiment id of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment id (integer), or None if position >= number of experiments.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = "SELECT " + self.__expIdCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol +"={}".format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[id]] = resList
            return(id)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("No expID found at position {}".format(position),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def setExpIdByPosition(self, position, expId):
        """setExpIdByPosition sets the experiment id of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expId - the new experiment id to use
        
        Returns: None.

        Affects: sets the experiment id of the experiment at given position

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expIdCol + "={} WHERE " + self.__expIdxCol + "={}").format(expId, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setExpIdByPosition with position: {} id: {}'.format(position, expId),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2]))


    def getExpUrlByPosition(self, position = 0):
        """getExpUrlByPosition returns the experiment url of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment url, or None if position >= number of experiments.

        Affects: None

        Exceptions: None
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expUrlCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[url]] = resList
            return(url)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getExpUrlByExpId(self, expId):
        """getExpUrlByExpId returns the experiment url for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the experiment url (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expUrlCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[url]] = resList
            return(url)
        except:
            self.__closeMetaDBConnector()
            return(None)

    
    def getRealExpUrlByPosition(self, position = 0):
        """getRealExpUrlByPosition returns the real experiment url of the experiment at given position.
        
        The url in the metadata may contain /madtoc/ for historical reasons.  This method converts that url to the real one.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the real experiment url, or None if position >= number of experiments.

        Affects: None

        Exceptions: 
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

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
        query = ("SELECT " + self.__expIdxCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[position]] = resList
        except:
            self.__closeMetaDBConnector()
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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

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
        """getExpPathByExpId returns the experiment path of the experiment for a given experiment id.
        
        The url in the metadata may contain /madtoc/ for historical reasons.  This method converts that url to the real one.

        Inputs: Experiment Id (integer).
        
        Returns: the path to the experiment from the madroot directory (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expIdxCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[position]] = resList
        except:
            self.__closeMetaDBConnector()
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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expUrlCol + "=\"{}\" WHERE " + self.__expIdxCol + "={}").format(expUrl, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setExpUrlByPosition with args %s: %s' %  \
                                               (str(position, expUrl),
                                                [traceback.format_exc()]))
    

    def getExpDirByPosition(self, position = 0):
        """getExpDirByPosition returns the full experiment directory of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the full experiment directory, or None if position >= number of experiments.  Uses
        experiment url to determine directory.

        Affects: None

        Exceptions: None
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        url = self.getExpUrlByPosition(position)
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


    def getExpDirByExpId(self, expId):
        """getExpDirByExpId returns the full experiment directory for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the full experiment directory (string).  Returns None if experiment id not found.
        Uses experiment url to determine directory.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expIdxCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[position]] = resList
        except:
            self.__closeMetaDBConnector()
            return(None)
        return(self.getExpDirByPosition(position))


        
    def getExpNameByPosition(self, position = 0):
        """getExpNameByPosition returns the experiment name of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment name, or None if position >= number of experiments.

        Affects: None

        Exceptions: None
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expNameCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[name]] = resList
            return(name)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getExpNameByExpId(self, expId):
        """getExpNameByExpId returns the experiment name for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the experiment name (string).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expNameCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[name]] = resList
            return(name)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def setExpNameByPosition(self, position, expName):
        """setExpNameByPosition sets the experiment name of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expName - the new experiment name to use
        
        Returns: None.

        Affects: sets the experiment name of the experiment at given position

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expNameCol + "=\"{}\" WHERE " + self.__expIdxCol + "={}").format(expName, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setExpNameByPosition with args %s: %s' %  \
                                               (str(position), expName),
                                                [traceback.format_exc()])


    def getExpSiteIdByExpId(self, expId):
        """getExpSiteIdByExpId returns the site id (int) for a given experiment id.

        Inputs: Experiment Id (integer).
        
        Returns: the site id for this experiment.  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expSiteIdCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[site]] = resList
            return(site)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getExpSiteIdByPosition(self, position = 0):
        """getExpSiteIdByPosition returns the experiment site id of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment site id (integer), or None if position >= number of experiments.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expSiteIdCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[site]] = resList
            return(site)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def setExpSiteIdByPosition(self, position, expSiteId):
        """setExpSiteIdByPosition sets the experiment site id of the experiment at given position.

        Inputs:

            position - position of experiment in list (first position is zero).

            expSiteId - the new experiment site id to use
        
        Returns: None.

        Affects: sets the experiment site id of the experiment at given position

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expSiteIdCol + "={} WHERE " + self.__expIdxCol + "={}").format(expSiteId, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expStartDateTimeCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
                
            [[startDTStr]] = resList
        except:
            self.__closeMetaDBConnector()
            return(None)

        # create time from year, month, day, hour, min, sec, weekday, julian day, daylight savings
        startTime =  [int(startDTStr[0:4]),
                          int(startDTStr[4:6]),
                          int(startDTStr[6:8]),
                          int(startDTStr[8:10]),
                          int(startDTStr[10:12]),
                          int(startDTStr[12:14]),
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
        return tuple(startTime)



    def getExpEndDateTimeByPosition(self, position = 0):
        """getExpEndDateTimeByPosition returns the ending date/time of the experiment at given position.

        Inputs: position of experiment in list (first position is zero).  Defaults to first.
        
        Returns: the experiment end date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if position >= number of experiments.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1
            
        query = ("SELECT " + self.__expEndDateTimeCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
                
            [[endDTStr]] = resList
        except:
            traceback.print_exc()
            self.__closeMetaDBConnector()
            return(None)

        # create time from year, month, day, hour, min, sec, weekday, julian day, daylight savings
        endTime =  [int(endDTStr[0:4]),
                          int(endDTStr[4:6]),
                          int(endDTStr[6:8]),
                          int(endDTStr[8:10]),
                          int(endDTStr[10:12]),
                          int(endDTStr[12:14]),
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
        return tuple(endTime)



    def getExpStartDateTimeByExpId(self, expId):
        """getExpStartDateTimeByExpId returns the starting date/time of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the experiment start date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if experiment id not found.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expStartDateTimeCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
                
            [[startDTStr]] = resList
        except:
            self.__closeMetaDBConnector()
            return(None)

        # create time from year, month, day, hour, min, sec, weekday, julian day, daylight savings
        startTime =  [int(startDTStr[0:4]),
                          int(startDTStr[4:6]),
                          int(startDTStr[6:8]),
                          int(startDTStr[8:10]),
                          int(startDTStr[10:12]),
                          int(startDTStr[12:14]),
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
        return tuple(startTime)



    def getExpEndDateTimeByExpId(self, expId):
        """getExpEndDateTimeByExpId returns the ending date/time of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the experiment end date/time in the standard python form of 9 item tuple in UTC.  
        Returns None if experiment id not found.  Since mktime does not go before 1970, I use
        date.c methods, and miss only day of week and DST flag
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expEndDateTimeCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
                
            [[endDTStr]] = resList
        except:
            self.__closeMetaDBConnector()
            return(None)

        # create time from year, month, day, hour, min, sec, weekday, julian day, daylight savings
        endTime =  [int(endDTStr[0:4]),
                          int(endDTStr[4:6]),
                          int(endDTStr[6:8]),
                          int(endDTStr[8:10]),
                          int(endDTStr[10:12]),
                          int(endDTStr[12:14]),
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
        return tuple(endTime)

        

    def setExpStartDateTimeByPosition(self, startDateTime, position = 0):
        """setExpStartDateTimeByPosition sets a new MadrigalExperiment start date and time by position.

        Inputs:

            startDateTime - a python datetime object to set the exp start date and time to.

            position - which experiment row to change - defaults to 0
        
        Returns: None.
        
        Affects: sets exp start date and time in metadata.db

        Exceptions: None
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        updatedSDTStr = startDateTime.strftime("%Y%m%d%H%M%S")
        update = ("UPDATE " + self.__tblName + " SET " + self.__expStartDateTimeCol + "={} WHERE " + self.__expIdxCol + "={}").format(updatedSDTStr, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            return(None)


    def setExpEndDateTimeByPosition(self, endDateTime, position = 0):
        """setExpEndDateTimeByPosition sets a new MadrigalExperiment end date and time by position.

        Inputs:

            endDateTime - a python datetime object to set the exp end date and time to.

            position - which experiment row to change - defaults to 0
        
        Returns: None.
        
        Affects: sets exp end date and time in metadata.db

        Exceptions: None
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        updatedEDTStr = endDateTime.strftime("%Y%m%d%H%M%S")
        update = ("UPDATE " + self.__tblName + " SET " + self.__expEndDateTimeCol + "={} WHERE " + self.__expIdxCol + "={}").format(updatedEDTStr, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getKinstByPosition(self, position = 0):
        """getKinstByPosition returns the kinst (kind of instrument code) of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the kinst (instrument code) of the file at given position as an integer.  
        Returns None if position >= number of files.
        
        Affects: None

        Exceptions: Thrown if kinst column cannot be parsed into an integer
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expKinstCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
                
            [[kinst]] = resList
            return(kinst)

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error getting kinst from metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))



    def getKinstByExpId(self, expId):
        """getKinstByExpId returns the kinst (integer) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the experiment kinst (integer).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expKinstCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
                
            [[kinst]] = resList
            return(kinst)

        except:
            self.__closeMetaDBConnector()
            return(None)

       
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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expKinstCol + "={} WHERE " + self.__expIdxCol + "={}").format(expKinst, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expSecurityCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[security]] = resList
            return(security)

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error getting security for metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))



    def getSecurityByExpId(self, expId):
        """getSecurityByExpId returns the security code (integer) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the security code (integer).  Returns None if experiment id not found.
        
        Affects: None

        Exceptions: None
        """
        query = ("SELECT " + self.__expSecurityCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[security]] = resList
            return(security)

        except:
            self.__closeMetaDBConnector()
            return(None)


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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expSecurityCol + "={} WHERE " + self.__expIdxCol + "={}").format(securityCode, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setSecurityByPosition with args %s: %s' %  \
                                               (str(position, securityCode),
                                                traceback.format_exception(sys.exc_info()[0],
                                                                           sys.exc_info()[1],
                                                                           sys.exc_info()[2])))
            
            
    def getPIByPosition(self, position = 0):
        """getPIByPosition returns the principal investigator of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the principal investigator's name (string) of the file at given position as a string.  
        Returns None if position >= number of files.  Since not all experiments may have this
        column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expPICol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[pi]] = resList
            if len(pi) > 0:
                return(pi)
            else:
                return(None)

        except:
            self.__closeMetaDBConnector()
            return(None)
        


    def getPIByExpId(self, expId):
        """getPIByExpId returns the principal investigator (string) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the principal investigator's name (string).  Returns None if experiment id not found. 
        Since not all experiments may have this column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """
        query = ("SELECT " + self.__expPICol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[pi]] = resList
            
            if len(pi) > 0:
                    return(pi)
            else:
                return(None)
        
        except:
            self.__closeMetaDBConnector()
            return(None)


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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expPICol + "=\"{}\" WHERE " + self.__expIdxCol + "={}").format(PI, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setPIByPosition with args %s: %s' %  \
                                               (str(position), PI),
                                                [traceback.format_exc()])
                
    
    def getPIEmailByPosition(self, position = 0):
        """getPIEmailByPosition returns the principal investigator email of the experiment at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the principal investigator's email (string) of the file at given position as a string.  
        Returns None if position >= number of files.  Since not all experiments may have this
        column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__expPIEmailCol + " FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[piEmail]] = resList
            if len(piEmail) > 0:
                return(piEmail)
            else:
                return(None)

        except:
            self.__closeMetaDBConnector()
            return(None)
        


    def getPIEmailByExpId(self, expId):
        """getPIEmailByExpId returns the principal investigator email (string) of the experiment for a given experiment id.
        
        Inputs: Experiment Id (integer).
        
        Returns: the principal investigator's email (string).  Returns None if experiment id not found. 
        Since not all experiments may have this column, returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        
        This method added in Madrigal 2.6
        """
        query = ("SELECT " + self.__expPIEmailCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[piEmail]] = resList
            if len(piEmail) > 0:
                return(piEmail)
            else:
                return(None)
        except:
            self.__closeMetaDBConnector()
            return(None)


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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__expPIEmailCol + "=\"{}\" WHERE " + self.__expIdxCol + "={}").format(PIEmail, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in setPIEmailByPosition with args %s: %s' %  \
                                               (str(position), PIEmail),
                                                [traceback.format_exc()])


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
        query = ("SELECT " + self.__expIdxCol + " FROM " + self.__tblName + " WHERE " + self.__expIdCol + "={}").format(expId)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[position]] = resList
        except:
            self.__closeMetaDBConnector()
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
        if self.__index:
            position = self.__index
        elif (position == 0):
            # if index not set, position should default to 1
            position = 1

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
        """getExpCount returns the total number of experiments in metadata.db.

           Inputs - None
           Returns - (int) total number of experiments
        """
        if self.__index:
            # this MadrigalExperiment object initialized from single exp
            return 1
        
        query = "SELECT COUNT(*) FROM " + self.__tblName
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()

            if not resList:
                return(None)
            [[numExps]] = resList
            return(numExps)
        except:
            raise madrigal.admin.MadrigalError("Problem getting experiment count", 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    # REMOVE ME 
    # def getStartPosition(self, startDT):
    #     """getStartPosition returns the position of the first experiment with a start datetime after
    #     endDT after sorting by end time.
        
    #         Inputs: startDT - start datetime
            
    #         Returns: returns the position of the first experiment with a start datetime after
    #     startDT.
    #     """
    #     self.sortByDateSite() # this should cause an error
    #     top = len(self.__fileList) - 1
    #     bottom = 0
    #     while(top >= bottom):
    #         if top == bottom:
    #             return(max(top-1, 0))
    #         middle = int((top+bottom)/2)
    #         thisEDTList = self.getExpEndDateTimeByPosition(middle)
    #         thisEDT = datetime.datetime(*thisEDTList[0:6])
    #         if thisEDT < startDT:
    #             bottom = middle + 1
    #             continue
    #         if middle == 0:
    #             return(0)
    #         prevEDTList = self.getExpEndDateTimeByPosition(middle-1)
    #         prevEDT = datetime.datetime(*prevEDTList[0:6])
    #         if prevEDT < startDT:
    #             return(middle-1)
    #         else:
    #             top = middle -1
     

    def getAllExpIDs(self, localSiteID):
        """
        Returns a list of tuples of all experiment IDs and indexes for experiments associated
        with the given site ID. 

        Inputs - site ID
        Returns - experiment IDs associated with site ID [(expID, expIdx)]
        """
        query = ("SELECT " + self.__expIdCol + ", " + self.__expIdxCol
                 + " FROM " + self.__tblName + " WHERE " + self.__expSecurityCol
                + " IN (0, 1, 2, 3) AND " + self.__expSiteIdCol + "={}").format(localSiteID)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                self.__closeMetaDBConnector()
                return(None)
            
            return(resList)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem getting expIDs for siteID {}".format(localSiteID), 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))



    # REMOVE ME
    # def __compareDateSite__(self, first):
    #     """__compareDateSite__ is a private method to help sort by end date, then start date, and then site.

    #     first - an item from self.__fileList
        
    #     returns a tuple of (endDT, startDT, site) to be used for sorting
    #     """
    #     endDT = datetime.datetime.strptime(first[self.__expEndDateCol] + first[self.__expEndTimeCol],
    #                                        '%Y%m%d%H%M%S')
    #     startDT = datetime.datetime.strptime(first[self.__expStartDateCol] + first[self.__expStartTimeCol],
    #                                        '%Y%m%d%H%M%S')
    #     site = first[self.__expSiteIdCol]
    #     return((endDT, startDT, site))
    

    # REMOVE ME
    # def getLine(self, position):
    #     """getLine returns the line at a given position.  Returns None if position > number of lines.

    #     Inputs:  position - position in file.  First line = 0
    #     """
    #     delimiter = ','

    #     if position >= len(self.__fileList):
    #         return(None)

    #     return(delimiter.join(self.__fileList[position]) + '\n')
  
        
    def writeMetadata(self, newFullPath=None):
        """writeMetadata writes a new version of the expTab.txt file.
        
        Inputs: newFullPath:  a new path to write the expTab.txt file to, if
        not the same as the original metadata file opened.  Defaults to None, which overwrites
        metadata file that was read from.
        
        Returns: None.

        Affects: Writes updated version of metadata file.

        Exceptions: If unable to write file
        """
        try:
            if not self.__index:
                fStr = self.__madDB.getTableStr(self.__tblName)

                if newFullPath:
                    with open(newFullPath, "w") as f:
                        f.write(fStr)
                else:
                    with open(self.__madDB.getMetadataDir() + "/expTab.txt", "w") as f:
                        f.write(fStr)
            else:
                # assume a local metadata obj
                localQuery = ("SELECT id, url, name, sid, sdt, edt, kinst, security, pi, piemail FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(self.__index)
                expDir = self.getExpDirByExpId(self.getExpIdByPosition())

                try:
                    self.__initMetaDBConnector()
                    result = self.__cursor.execute(localQuery)
                    resList = result.fetchall()
                    self.__closeMetaDBConnector()

                    textList = []
                    for line in resList:
                        line = [str(l) for l in line]
                        line = line[:4] + [line[4][:8], line[4][8:]] + [line[5][:8], line[5][8:]] + line[6:]
                        textList.append(','.join(line))
                    tblText = '\n'.join(textList)

                    with open(os.path.join(expDir, "expTab.txt"), "w") as f:
                        f.write(tblText)

                except:
                    self.__closeMetaDBConnector()
                    raise madrigal.admin.MadrigalError('Problem getting local metadata for exp at {}'.format(expDir),
                                                    traceback.format_exception(sys.exc_info()[0],
                                                                                sys.exc_info()[1],
                                                                                sys.exc_info()[2]))


        except:
            raise madrigal.admin.MadrigalError("Unable to write metadata file " + \
                                               str(newFullPath),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))
        

    def validateExp(self):
        """
        Validate that this is, in fact, a legitimate experiment. Remove (both content of expDir and from metadata)
        if not. Assumes that this MadrigalExperiment object is referring to a single experiment, rather 
        than all experiments.

        Checks the following:
            - exp dir is accessible
            - exp dir contains at least one .h5/.hdf5 file

        Used to completely remove experiment from Madrigal, assuming you have manually deleted the 
        associated experiment files first.

        Returns: True (valid experiment), False (invalid experiment)
        """
        if self.__index:
            position = self.__index
        else:
            raise ValueError("Attempting to use validateExp for all Madrigal experiments-- must initialize MadrigalExperiment object from single experiment")
        
        thisExpDir = self.getExpDirByPosition(position)
        todelete = False

        if not os.path.exists(thisExpDir):
            if os.path.islink(thisExpDir):
                raise ValueError(f"Broken symlink found for expDir {thisExpDir}")
            todelete = True
        else:
            possibleFiles = []
            possibleFiles += glob.glob(os.path.join(thisExpDir, "*.h5"))
            possibleFiles += glob.glob(os.path.join(thisExpDir, "*.hdf5"))
            if len(possibleFiles) == 0:
                todelete = True

        if todelete:
            self.__removeExp()
            
            

    def __removeExp(self):
        """
        Private function to remove this experiment from metadata. WARNING: removes all associated files in experiment too.

        Inputs - None
        Returns - None
        """
        if self.__index:
            position = self.__index
        else:
            raise ValueError("Attempting to remove exp without assigned index")

        thisExpName = self.getExpNameByPosition(position)
        thisExpDate = self.getExpStartDateTimeByPosition(position)
        thisFileObj = MadrigalMetaFile(self.__madDB, self.getExpDirByPosition() + "/fileTab.txt")
        count = thisFileObj.getFileCount()

        for i in range(count):
            thisFileObj.deleteRowByFilename(thisFileObj.getFilenameByPosition(i))

        update = ("DELETE FROM " + self.__tblName + " WHERE " + self.__expIdxCol + "={}").format(position)
        
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
            print(f"Successfully removed experiment {thisExpName} starting at {thisExpDate}")
        except:
            # no matches found
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Could not delete experiment at position ' + str(position), None)
        

        
        

    def __str__(self):
        """return possibly modified file as a string in same format as writeMetadata
        """
        return(self.__madDB.getTableStr(self.__tblName))



class MadrigalMetaFile:
    """MadrigalMetaFile is an object that provides access to Madrigal file info from the metadata.

    This object provides access to all Madrigal experiment information in the metadata table fileTab.

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
    __fileMetadataFile  = METADB
    __tblName           = "fileTab"

    # column positions
    __fileNameCol           =  'fname'
    __fileExpIdCol          =  'eid'
    __fileKindatCol         =  'kindat'
    __fileCategoryCol       =  'category'
    __fileSizeCol           =  'fsize'
    __fileHasCatalogCol     =  'catrec'
    __fileHasHeaderCol      =  'headrec'
    __fileModDateCol        =  'amoddate'
    __fileModTimeCol        =  'amodtime'
    __fileStatusCol         =  'status'
    __fileAccessCol         =  'permission'
    __fileAnalystCol        =  'fanalyst'
    __fileAnalystEmailCol   =  'fanalystemail'
    __fileIdxCol            =  'idx'
    __indexList             =  None # stays None unless initFile is not None
    

    def __init__(self, madDB=None, initFile=None):
        """__init__ initializes MadrigalMetaFile by reading from fileTab.txt (or initFile).

        Inputs: Existing MadrigalDB object, by default = None.

            String representing the full path to the metadata file. Default is None, in
            which case file read is MadrigalDB.getMetadataDir()/fileTab.txt.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown any problems accessing metadata.db.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get file metadata file
        self.__filename = self.__fileMetadataFile

        if initFile:
            self.__setIdxFromInit(initFile)


    def __setIdxFromInit(self, initFile):
        """
        __setIdxFromInit sets __index private variable used to refer to a specific (unique)
        experiment. Index is pulled from expTab entry where url contains expDir (from initFile path).

        Inputs: initFile, referring to fileTab.txt (doesn't need to exist-- only need expDir from path to this file)

        Returns: void

        Affects: sets __index private variable used to refer to this specific experiment

        Exceptions: MadrigalError thrown if path to initFile contains malformed expDir, or
        if expDir not found
        """
        # first check that path to initFile contains expDir
        dir = os.path.dirname(initFile)
        dirConvStr1 = 'experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[a-zA-Z0-9\-_]*$'
        dirConvStr2 = 'experiments[0-9]*/[0-9][0-9][0-9][0-9]/[a-z][a-z0-9][a-z0-9]/[0-3][0-9][a-z][a-z0-9][a-z0-9][0-9][0-9].?'

        found = re.search(dirConvStr1, dir)
        if not found:
            found = re.search(dirConvStr2, dir)
            if not found:
                raise madrigal.admin.MadrigalError("Malformed expDir found: {}".format(dir),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        expDir = found.group(0)

        # make sure to use local experiment if initFile is also local
        if self.__madDB.getMadroot() in initFile:
            expDir = self.__madDB.getTopLevelUrl() + "/madtoc/" + expDir
        elif initFile.startswith("/experiments") or initFile.startswith("experiments"):
            # assume local experiment
            expDir = self.__madDB.getTopLevelUrl() + "/madtoc/" + expDir

        # now match found expDir to expUrl to find the correct index
        query = "SELECT id, url FROM expTab WHERE url LIKE \"%{}%\"".format(expDir)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()

            # assumes exactly 1 expDir
            [idx] = [item[0] for item in resList if expDir in item[1]]

            # use expID to get indicies for files in this exp
            query1 = ("SELECT " + self.__fileIdxCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={}").format(idx)
            result = self.__cursor.execute(query1)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            self.__indexList = [item[0] for item in resList]
            
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Could not find index for expDir: {}".format(dir),
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getFileCount(self):
        """If self.__indexList set, getFileCount returns the total number of files in metadata.db.
        Otherwise, return the total number of files associated with this experiment.

        Inputs: None
        
        Returns: the number of files (rows) in metadata.db or number of files in this experiment.

        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            return(len(self.__indexList))
        
        query = "SELECT COUNT(*) FROM " + self.__tblName
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()

            if not resList:
                return(None)
            [[numFiles]] = resList
            return(numFiles)
        except:
            raise madrigal.admin.MadrigalError("Problem getting file count", 
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getFilenameByPosition(self, position = 0):
        """getFilenameByPosition returns the filename of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the filename of the file at given position as a string.  
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1
            
        query = ("SELECT " + self.__fileNameCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[fname]] = resList
            return(fname)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getExpIdByPosition(self, position = 0):
        """getExpIdByPosition returns the experiment id (integer) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the experiment id (integer) of the file at given position as an integer.  
        
        Affects: None

        Exceptions: Thrown if kinst exp id cannot be parsed into an integer
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileExpIdCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[eid]] = resList
            return(eid)

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem getting expID for file at position ' + str(position),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))


    def setExpIdByPosition(self, position, expId):
        """setExpIdByPosition sets the experiment id of the file at given position.

        Inputs:

            position - position of file in list (first position is zero).

            expId - the new experiment id to use
        
        Returns: None.

        Affects: sets the experiment id of the file at given position

        Exceptions: MadrigalError if any problems accessing metadata.db.
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileExpIdCol + "={} WHERE " + self.__fileIdxCol + "={}").format(expId, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem setting expID by position: pos %s: id %s' %  \
                                                (str(position), str(expId)),
                                                 traceback.format_exception(sys.exc_info()[0],
                                                                            sys.exc_info()[1],
                                                                            sys.exc_info()[2]))


    def getKindatByPosition(self, position = 0):
        """getKindatByPosition returns the kindat (kind of data code) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the kinst (kind of instrument code) of the file at given position as an integer.  
        
        Affects: None

        Exceptions: Thrown if kinst column cannot be parsed into an integer
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileKindatCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[kindat]] = resList
            return(kindat)

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem getting kindat for position ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))


    def getCategoryByPosition(self, position = 0):
        """getCategoryByPosition returns the category (eg., 1=default, 2=variant, 3=history, 4=real-time) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the category (eg., 1=default, 2=variant, 3=history, 4=real-time) of the file at given position as an integer.  
        
        Affects: None

        Exceptions: Thrown if category column cannot be parsed into an integer
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileCategoryCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[category]] = resList
            return(int(category))

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem getting category for position ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))


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
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()
        filename = os.path.basename(filename)
        query = ("SELECT " + self.__fileCategoryCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[category]] = resList
            return(int(category))
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getHasCatalogByPosition(self, position = 0):
        """getHasCatalogByPosition returns True if the file at given position has any catalog records, False otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  True if the file at given position has any catalog records, False otherwise 
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileHasCatalogCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[catrec]] = resList

            if int(catrec) == 0:
                return False
            else:
                return True
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getHasCatalogByFilename(self,filename):
        """getHasCatalogByFilename returns true if the file with the given name has any catalog records, False otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: true if the file with the given name has any catalog records, False otherwise.  Returns none if name
        not found
        
        Affects: None

        Exceptions: None.
        """
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()

        filename = os.path.basename(filename)
        query = ("SELECT " + self.__fileHasCatalogCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[catrec]] = resList

            if int(catrec) == 0:
                    return False
            else:
                return True

        except:
            self.__closeMetaDBConnector()
            return(None)


    def setHasCatalogByPosition(self, position, hasCatalog):
        """setHasCatalogByPosition sets the value of hasCatalog for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            hasCatalog - 1 or True for yes, 0 or False for no
        
        Returns:  None.
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        if hasCatalog not in (0, 1, True, False):
            raise ValueError('Illegal value for hasCatalog in setHasCatalogByPosition: %s' % (str(hasCatalog)))

        if hasCatalog in (1, True):
            hasCatalog = '1'
        else:
            hasCatalog = '0'

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileHasCatalogCol + "={} WHERE " + self.__fileIdxCol + "={}").format(hasCatalog, position)

        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getHasHeaderByPosition(self, position = 0):
        """getHasHeaderByPosition returns True if the file at given position has any header records, False otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  True if the file at given position has any header records, False otherwise 
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileHasHeaderCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[headrec]] = resList

            if int(headrec) == 0:
                return False
            else:
                return True
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getHasHeaderByFilename(self,filename):
        """getHasHeaderByFilename returns true if the file with the given name has any header records, False otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: true if the file with the given name has any header records, False otherwise.  Returns none if name
        not found
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()

        filename = os.path.basename(filename)
        query = ("SELECT " + self.__fileHasHeaderCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[headrec]] = resList

            if int(headrec) == 0:
                return False
            else:
                return True
        except:
            self.__closeMetaDBConnector()
            return(None)


    def setHasHeaderByPosition(self, position, hasHeader):
        """setHasHeaderByPosition sets the value of hasHeader for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            hasHeader - 1 or True for yes, 0 or False for no
        
        Returns:  None.
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        if hasHeader not in (0, 1, True, False):
            raise ValueError('Illegal value for hasHeader in setHasHeaderByPosition: %s' % (str(hasHeader)))

        if hasHeader in (1, True):
            hasHeader = '1'
        else:
            hasHeader = '0'

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileHasHeaderCol + "={} WHERE " + self.__fileIdxCol + "={}").format(hasHeader, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getStatusByPosition(self, position = 0):
        """getStatusByPosition returns the status description of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the status description of the file at given position as a string.  
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileStatusCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[status]] = resList
            return(status)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getStatusByFilename(self,filename):
        """getStatusByFilename returns the status description of the file with the given name.

        Inputs: filename - name of file to search for.
        
        Returns: the status description of the file with the given name.  Returns none if name
        not found
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()

        filename = os.path.basename(filename)
        query = ("SELECT " + self.__fileStatusCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[status]] = resList
            return(status)
        except:
            self.__closeMetaDBConnector()
            return(None)



    def getAccessByPosition(self, position = 0):
        """getAccessByPosition returns the access (0=public, 1=private) of the file at given position.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: the access level (0=public, 1=private) of the file at given position as an integer.  
        
        Affects: None

        Exceptions: Thrown if access column cannot be parsed into an integer
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileAccessCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[permission]] = resList
            return(int(permission))

        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Error in fileTab.txt parsing metadata row: ' + str(position),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        
        
    def getFileDatetimeByPosition(self, position = 0):
        """getFileDatetimeByPosition returns a datetime of the file at given position, or None if not set.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns: a datetime of the file at given position, or None if not set.  
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileModDateCol + ", " + self.__fileModTimeCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[dateStr, timeStr]] = resList
            thisdt = datetime.datetime.strptime('%s %s' % (dateStr, timeStr), '%Y%m%d %H%M%S')
            thisdt = thisdt.replace(tzinfo=datetime.timezone.utc)
            return(thisdt)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getFileDatetimeByFilename(self,filename):
        """getFileDatetimeByFilename returns a datetime of the file with the given filenme, or None if not set.

        Inputs: filename - name of file to search for.
        
        Returns: a datetime of the file with the given filenme, or None if not set.  Returns none if name
        not found
        
        Affects: None

        Exceptions: Thrown if filename not found.
        """
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()

        filename = "*" + os.path.basename(filename) + "*"
        query = ("SELECT " + self.__fileModDateCol + ", " + self.__fileModTimeCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + " GLOB \"{}\"").format(expID, filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[dateStr, timeStr]] = resList
            thisdt = datetime.datetime.strptime('%s %s' % (dateStr, timeStr), '%Y%m%d %H%M%S')
            thisdt = thisdt.replace(tzinfo=datetime.timezone.utc)
            return(thisdt)
        except:
            self.__closeMetaDBConnector()
            return(None)

    
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
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        if dt is None:
            basename = self.getFilenameByPosition(position)
            expDir = os.path.dirname(self.__filename)
            if len(expDir) < 2:
                raise IOError('Cannot set datetime to None using the metadata version of fileTab.txt')
            dataFile = os.path.join(expDir, basename)
            if not os.access(dataFile, os.R_OK):
                raise IOError('Cannot access %s' % (dataFile))
            dt = datetime.datetime.fromtimestamp(os.path.getmtime(dataFile), tz=datetime.timezone.utc)
            
            
        dateStr = dt.strftime('%Y%m%d')
        timeStr = dt.strftime('%H%M%S')

        try:
            update = ("UPDATE " + self.__tblName + " SET " + self.__fileModDateCol 
                    + "=\"{}\", " + self.__fileModTimeCol + "=\"{}\" WHERE "
                    + self.__fileIdxCol + "={}").format(dateStr, timeStr, position)

            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()

        except:
            self.__closeMetaDBConnector()
            raise ValueError('Problem in setFileDatetimeByPosition')


    def deleteRowByFilename(self, filename):
        """deleteRowByFilename deletes a row with a given filename.

        Inputs: filename - name of file to search for.
        
        Returns: None.
        
        Affects: Removes item from self.__fileList if filename found

        Exceptions: Thrown if filename not found.
        """
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByFilename(filename)
        update = ("DELETE FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)
        
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
            print(f"Successfully removed file {filename} from metadata")
        except:
            # no matches found
            self.__closeMetaDBConnector()
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

        query = ("SELECT " + self.__fileExpIdCol + " FROM " + self.__tblName + " WHERE " + self.__fileNameCol + "=\"{}\"").format(filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[eid]] = resList
            return(eid)
        except:
            self.__closeMetaDBConnector()
            return(self.getExpIdByPosition())



    def getKindatByFilename(self, filename):
        """getKindatByFilename returns the first kindat (integer) with the given filename.

        Inputs: filename - name of file to search for.
        
        Returns: the kindat (integer) of the first row found with the given filename.  Since
        filename may not be unique (although it usually is), the first match found is used.  If
        no matches found, returns None.
        
        Affects: None

        Exceptions: Thrown if kindat cannot be parsed into an integer
        """
        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()
        filename = os.path.basename(filename)

        query = ("SELECT " + self.__fileKindatCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[kindat]] = resList
            return(kindat)
        
        except:
            self.__closeMetaDBConnector()
            return(None)


    def setAccessByPosition(self, position, access):
        """setAccessByPosition sets the value of access for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            access - 0 of False for public, 1 or True for private
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        if access not in (0, 1, True, False):
            raise ValueError('Illegal value for access in setAccessByPosition: %s' % (str(access)))
        
        if access in (1, True):
            access = '1'
        else:
            access = '0'

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileAccessCol + "={} WHERE " + self.__fileIdxCol + "={}").format(access, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise ValueError('Problem with setAccessByPosition')


    def setAccess(self, accessType):
        """setAccess sets the access column to all 0's (public) or all 1's (private).

        Inputs: accessType - either 0 to set to public access, or 1 to set to private access.
        
        Returns: None.
        
        Affects: Overwrite fileTab with access column set to all 0's (public)
        or all 1's (private).

        Exceptions: Thrown if file cannot be written, if accessType is not 0 or 1
        """
        if not self.__indexList:
            raise madrigal.admin.MadrigalError('setAccess called with a local MadrigalMetaFile object', None)
        if (accessType != 0 and accessType != 1):
            raise madrigal.admin.MadrigalError('MadrigalMetaFile.setAccess called with arg = ' + \
                                               str(accessType) + ', must be either 0 or 1', None)
        
        expID = self.getExpIdByPosition()

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileAccessCol + "={} WHERE " + self.__fileExpIdCol + "={}").format(str(accessType), expID)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise ValueError("Could not update fileTab access")


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

        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1
        
        update = ("UPDATE " + self.__tblName + " SET " + self.__fileKindatCol + "={} WHERE " + self.__fileIdxCol + "={}").format(kindat, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()

        except:
            self.__closeMetaDBConnector()
            raise ValueError('Problem with setKindatByPosition')


    def setCategoryByPosition(self, position, category):
        """setCategoryByPosition sets the value of category for the file at the given position.

        Inputs:

            position - position of file in list (first position is zero).

            category - 1=default, 2=variant, 3=history, 4=real-time
        
        Returns:  None.
        
        Affects: None

        Exceptions: If position beyond length.
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        if int(category) not in (1, 2, 3, 4):
            raise ValueError('Illegal value for category in setCategoryByPosition: %s' % (str(category)))

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileCategoryCol + "={} WHERE " + self.__fileIdxCol + "={}").format(category, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()

        except:
            self.__closeMetaDBConnector()
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
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        statusTypes = [bytes, str]
        if type(status) not in statusTypes:
            raise ValueError('Illegal value for status in setStatusByPosition: %s' % (str(status)))

        # check that string does not illegally contain a comma
        if status.find(',') != -1:
            raise ValueError('status string in fileTab.txt cannot contain a comma: <%s> is illegal' % (status))

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileStatusCol + "=\"{}\" WHERE " + self.__fileIdxCol + "={}").format(status, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()

        except:
            self.__closeMetaDBConnector()
            raise ValueError('Problem with setStatusByPosition')


    def getAnalystByPosition(self, position = 0):
        """getAnalystByPosition returns file analyst name if there, None otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  file analyst name if there, None otherwise. Since not all files may have this column, 
        returns None if the column does not exist. 
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        query = ("SELECT " + self.__fileAnalystCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[analyst]] = resList
            return(analyst)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getAnalystByFilename(self, filename):
        """getAnalystByFilename returns file analyst name if there, None otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: file analyst name if there, None otherwise. Since not all files may have this column, 
        returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        """
        filename = os.path.basename(filename)

        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()

        query = ("SELECT " + self.__fileAnalystCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[analyst]] = resList
            return(analyst)
        except:
            self.__closeMetaDBConnector()
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
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        # verify no illegal commas
        if analyst.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setAnalystByPosition with args %s: %s' %  \
                                               (str(position), analyst),
                                                [traceback.format_exc()])

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileAnalystCol + "=\"{}\" WHERE " + self.__fileIdxCol + "={}").format(analyst, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()
        except:
            self.__closeMetaDBConnector()
            raise ValueError('Problem with setAnalystByPosition')


    def getAnalystEmailByPosition(self, position = 0):
        """getAnalystEmailByPosition returns file analyst email if there, None otherwise.

        Inputs: position of file in list (first position is zero).  Defaults to first.
        
        Returns:  file analyst email if there, None otherwise. Since not all files may have this column, 
        returns None if the column does not exist.  
        
        Affects: None

        Exceptions: None
        """
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1
        
        query = ("SELECT " + self.__fileAnalystEmailCol + " FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(position)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[email]] = resList
            return(email)
        except:
            self.__closeMetaDBConnector()
            return(None)


    def getAnalystEmailByFilename(self, filename):
        """getAnalystEmailByFilename returns file analyst email if there, None otherwise.

        Inputs: filename - name of file to search for.
        
        Returns: file analyst email if there, None otherwise. Since not all files may have this column, 
        returns None if the column does not exist.
        
        Affects: None

        Exceptions: None
        """
        filename = os.path.basename(filename)

        if self.__indexList:
            pass
        else:
            # we should never delete files based on filename only,
            # idx must be set
            raise madrigal.admin.MadrigalError(f"Unable to delete file {filename}, MadrigalMetaFile index not set", None)

        expID = self.getExpIdByPosition()

        query = ("SELECT " + self.__fileAnalystEmailCol + " FROM " + self.__tblName + " WHERE " + self.__fileExpIdCol + "={} AND " + self.__fileNameCol + "=\"{}\"").format(expID, filename)
        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[email]] = resList
            return(email)
        except:
            self.__closeMetaDBConnector()
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
        if self.__indexList:
            position = self.__indexList[position]
        elif (position == 0):
            # if indexList not set, position should default to 1
            position = 1

        # verify no illegal commas
        if analystEmail.find(',') != -1:
            raise madrigal.admin.MadrigalError('Error in setAnalystEmailByPosition with args %s: %s' %  \
                                               (str(position, analystEmail),
                                                [traceback.format_exc()]))

        update = ("UPDATE " + self.__tblName + " SET " + self.__fileAnalystEmailCol + "=\"{}\" WHERE " + self.__fileIdxCol + "={}").format(analystEmail, position)
        try:
            self.__initMetaDBConnector()
            self.__cursor.execute(update)
            self.__connector.commit()
            self.__closeMetaDBConnector()

        except:
            self.__closeMetaDBConnector()
            raise ValueError(f'problem setting analyst email at position {position}')




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

        # get the experiment id - if None, return message to update Metadata
        expId = self.getExpIdByFilename(filename)

        if expId == None:
            return '\tMetadata for file ' + str(filename) + \
                   ' not found.  Please notify Madrigal administrator that metadata ' + \
                   'needs to be updated.'

        # create any needed metadata objects

        if madExpObj == None:
            madExpObj = MadrigalExperiment(self.__madDB)

        if madInstObj == None:
            madInstObj = MadrigalInstrument(self.__madDB)

        if madKindatObj == None:
            madKindatObj = MadrigalKindat(self.__madDB)

        

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
        # don't need to reset position here
        
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


    # REMOVE ME
    # def getLine(self, position):
    #     """getLine returns the line at a given position.  Returns None if position > number of lines.

    #     Inputs:  position - position in file.  First line = 0
    #     """
    #     delimiter = ','

    #     if position >= len(self.__fileList):
    #         return(None)

    #     return(delimiter.join(self.__fileList[position]) + '\n')
        

    def writeMetadata(self, newFullPath=None):
        """writeMetadata writes a new version of the fileTab.txt file.
        
        Inputs: newFullPath:  a new path to write the fileTab.txt file to, if
        not the same as the original metadata file opened.  Defaults to None, which overwrites
        metadata file that was read from.
        
        Returns: None.

        Affects: Writes updated version of metadata file.

        Exceptions: If unable to write file
        """
        try:
            if not self.__indexList:
                fStr = self.__madDB.getTableStr(self.__tblName)

                if newFullPath:
                    with open(newFullPath, "w") as f:
                        f.write(fStr)
                else:
                    with open(self.__madDB.getMetadataDir() + "/fileTab.txt", "w") as f:
                        f.write(fStr)
            else:
                # assume a local metadata obj
                if len(self.__indexList) == 1:
                    localQuery = ("SELECT fname, eid, kindat, category, fsize, catrec, headrec, amoddate, amodtime, status, permission, fanalyst, fanalystemail FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + "={}").format(self.__indexList[0])
                else:
                    localQuery = ("SELECT fname, eid, kindat, category, fsize, catrec, headrec, amoddate, amodtime, status, permission, fanalyst, fanalystemail FROM " + self.__tblName + " WHERE " + self.__fileIdxCol + " IN {}").format(tuple(self.__indexList))
                expObj = MadrigalExperiment()
                expDir = expObj.getExpDirByExpId(self.getExpIdByPosition())

                try:
                    self.__initMetaDBConnector()
                    result = self.__cursor.execute(localQuery)
                    resList = result.fetchall()
                    self.__closeMetaDBConnector()

                    textList = []
                    for line in resList:
                        line = [str(l) for l in line]
                        textList.append(','.join(line))
                    tblText = '\n'.join(textList)

                    with open(os.path.join(expDir, "fileTab.txt"), "w") as f:
                        f.write(tblText)

                except:
                    self.__closeMetaDBConnector()
                    raise madrigal.admin.MadrigalError('Problem getting local metadata for exp at {}'.format(expDir),
                                                    traceback.format_exception(sys.exc_info()[0],
                                                                                sys.exc_info()[1],
                                                                                sys.exc_info()[2]))


        except:
            raise madrigal.admin.MadrigalError('Problem writing local metadata for exp at {}'.format(expDir),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))
        
        
    def __str__(self):
        """return possibly modified file as a string in same format as writeMetadata
        """  
        return(self.__madDB.getTableStr(self.__tblName))
    
    
class MadrigalParmCategory:
    """MadrigalParmCategory is an object that provides access to Madrigal parameter category info from the metadata.

    This object provides access to all Madrigal kind of data information in the metadata table madCatTab.

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jan. 18, 2013

    """

    #constants
    _categoryMetadataFile  = METADB
    __tblName              = "madCatTab"

    # column positions
    _categoryCodeCol   =  'code'
    _categoryDescCol   =  'catname'
    _minCodeCol        =  'mincode'
    _maxCodeCol        =  'maxcode'


    def __init__(self, madDB=None):
        """__init__ initializes MadrigalParmCategory by reading from metadata.db.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: MadrigalError thrown if any problems accessing metadata.db.
        """

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # get category metadata file
        self.__filename = self._categoryMetadataFile


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metadata.db
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection to metadata.db
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadata.db",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getCategoryDesc(self, code):
        """getCategoryDesc returns the category description that matches code argument, or None if not found.

        Inputs: 
        
            code integer to get category description. or integer as string
        
        Returns: the category description that matches code argument, or None if not found.

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        code = int(code)
        query = ("SELECT " + self._categoryDescCol + " FROM " + self.__tblName + " WHERE " + self._categoryCodeCol + "={}").format(code)

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            [[desc]] = resList
            return(desc)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem getting description for category code {}'.format(code),
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))


    def getCategoryList(self):
        """getCategoryList returns a list of all category descriptions and codes.

        Inputs: None.
        
        Returns: a list of all category descriptions and codes.  Each item in the list
        is a tuple of the form (Category description (string),  code (integer)).  Example item:
        ('INSCAL Basic Derived Parameters', 3001)

        Affects: None

        Exceptions: MadrigalError if any problems accessing metadata.db
        """
        query = "SELECT " + self._categoryDescCol + ", " + self._categoryCodeCol + " FROM " + self.__tblName

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query)
            resList = result.fetchall()
            self.__closeMetaDBConnector()
            resList = [(i[0], int(i[1])) for i in resList]
            return(resList)
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem getting category list',
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))    
    

class MadrigalInstrumentData:
    """MadrigalInstrumentData is an object that provides access to Madrigal instrument data info from the metadata.

    This object provides access to all Madrigal kind of data information in the metadata files instData.txt
    and instDataPriv.txt.  Those files summarize years data is available by instrument.

    ^^^^^ this is not true anymore, fix these docs when you verify ui performance is ok

    Non-standard Python modules used:
    None

    MadrigalError exception thrown if:

        1. MadrigalMetadata fails to open or parse metadata file
        
        2. Columns expected to be ints or floats cannot be converted

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 2, 2015

    """

    #constants
    _instDataMetadataFile  = METADB

    # column positions
    _siteIDCol   =  'sid'
    _kinstCol    =  'kinst'
    _kindatCol   =  'kindat'
    _yearsCol    =  'year'

    

    def __init__(self, madDB=None, priv=False, madInstObj=None):
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
            
        #if priv:
        #    self._instDataMetadataFile  = "instDataPriv.txt"
        self.priv = priv

        # get instData metadata file
        #if (initFile == None):
        self.__filename = self._instDataMetadataFile
        #else:
        #    self.__filename = initFile
            
        if isinstance(madInstObj, madrigal.metadata.MadrigalInstrument):
            self._madInstObj = madInstObj
        else:
            self._madInstObj = madrigal.metadata.MadrigalInstrument(self.__madDB)

        #self.__fileList = madrigal.metadata.MadrigalMetadata(self.__filename, self.__madDB).getList()


    def __initMetaDBConnector(self):
        """
        __initMetaDBConnector initializes the sqlite3 connector to read from the metadata database.

        Inputs: None

        Returns: Void

        Affects: Initializes private class member variables to connect to metaDB
        """
        try:
            self.__connector = sqlite3.connect(os.path.join(self.__madDB.getMetadataDir(), self.__filename))
            self.__cursor = self.__connector.cursor()
        except:  
            raise madrigal.admin.MadrigalError("Unable to connect to metadataDB",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
        

    def __closeMetaDBConnector(self):
        """
        __closeMetaDBConnector closes the connection to the sqlite3 database connector.

        Inputs: None

        Returns: Void

        Affects: Closes connection 
        """
        try:
            self.__connector.close()
        except:  
            raise madrigal.admin.MadrigalError("Problem closing connection to metadataDB",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))


    def getKindatListForInstruments(self, kinstList):
        """getKindatListForInstruments returns a list of kindat codes as integers for the given instrument list.
        A kindat code is returned if it appears in any year

        Inputs: kinstList: a list of kinst integers to get associated kindat list. Also accepts a single integer.
        
        Returns: a list of kindat codes as integers associated with the given instrument list.

        Affects: None

        Exceptions: if error in metadata file
        """
        # NOTE 2 SELF: the way this is currently written may be 
        # quite inefficient, ideally you would want to cache these 
        # results somewhere or something like that

        # if kinstList is just a single integer, look for this kinst only
        if (type(kinstList) == int) or (len(kinstList) == 1):
            if (len(kinstList) == 1):
                kinstList = kinstList[0]
            query1 = "SELECT id FROM expTab WHERE kinst={}".format(kinstList)
        else:
            query1 = "SELECT id FROM expTab WHERE kinst IN {}".format(tuple(kinstList))


        query2 = "SELECT kindat FROM fileTab WHERE eid IN {}"

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query1)
            resList = result.fetchall()

            if not resList:
                return(None)

            resList = [item[0] for item in resList]
            result = self.__cursor.execute(query2.format(tuple(resList)))
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
            
            resList = [item[0] for item in resList]
            return(list(set(resList)))
        
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError("Problem getting kindat list",
                                              traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    
    
    
    def getKindatListForInstrumentYear(self, kinst, year):
        """getKindatListForInstrumentYear returns a list of kindat codes as integers for the given instrument and year.

        Inputs: kinst, year
        
        Returns: a list of kindat codes as integers associated with the given instrument  and year,
            or None if not found.

        Affects: None

        Exceptions: if error in metadata file
        """
        # NOTE 2 SELF: the way this is currently written may be 
        # quite inefficient, ideally you would want to cache these 
        # results somewhere

        query1 = "SELECT id FROM expTab WHERE kinst={} AND ((sdt LIKE '{}%%%%%%%%%%') OR (edt LIKE '{}%%%%%%%%%%'))"
        query2 = "SELECT kindat FROM fileTab WHERE eid IN {}"

        try:
            self.__initMetaDBConnector()
            result = self.__cursor.execute(query1.format(kinst, year, year))
            resList = result.fetchall()

            if not resList:
                return(None)

            resList = [item[0] for item in resList]
            result = self.__cursor.execute(query2.format(tuple(resList)))
            resList = result.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                return(None)
            
            resList = [item[0] for item in resList]
            return(list(set(resList)))
        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem getting kindat list for kinst {} and year {}'.format(kinst, year),
                                                           traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))

    

    def rebuildInstDataTable(self):
        """
        
        lets try something completely different

        InstData is now effectively an array of 3 dictionaries:

        KinstDict - key: kinst, value: (instDesc, yearList)
        SiteDict - key: siteID, value: [kinstList]
        CategoryDict - key: catID, value: [kinstList]


        3 dictionaries will be converted to pandas.dataframes, which
        will be exported to different groups in an hdf5 file



        Inputs: None.
        
        Returns: None.

        Affects: Writes file instKindatTab.txt in metadata directory

        Exceptions: If unable to write instKindatTab.txt file.
        """
        localSite = self.__madDB.getSiteID()
        expQuery = "SELECT kinst, sid, security, substring(sdt, 0, 5), substring(edt, 0, 5) FROM expTab"

        kinstDict = {}
        siteDict = {}
        catDict = {}
        kinstDict_priv = {}
        siteDict_priv = {}
        catDict_priv = {}

        try:
            self.__initMetaDBConnector()
            res = self.__cursor.execute(expQuery)
            resList = res.fetchall()
            self.__closeMetaDBConnector()

            if not resList:
                raise("Unable to build instData table")
            
            # this may take a bit...
            for expData in resList:
                thiskinst = int(expData[0])
                thissite = int(expData[1])
                security = int(expData[2])
                syear = int(expData[3])
                eyear = int(expData[4])
                thiscategory = self._madInstObj.getCategoryId(thiskinst)

                # skip all non-local archived data
                if thissite != localSite and security in (2,3):
                    continue

                if security < 0:
                    # test exp, skip
                    continue

                if security not in (0, 2):
                    # private data only

                    # build kinstDict
                    if thiskinst not in kinstDict_priv:
                        kinstDict_priv[thiskinst] = [self._madInstObj.getInstrumentName(thiskinst), []]
                    kinstDict_priv[thiskinst][1].append(syear)
                    kinstDict_priv[thiskinst][1].append(eyear)

                    # build siteDict
                    if thissite not in siteDict_priv:
                        siteDict_priv[thissite] = [[]]
                    siteDict_priv[thissite][0].append(thiskinst)

                    # build catDict
                    if thiscategory not in catDict_priv:
                        catDict_priv[thiscategory] = [[]]
                    catDict_priv[thiscategory][0].append(thiskinst)


                else:
                    # public data, populate all needed dicts

                    # build kinstDict
                    if thiskinst not in kinstDict:
                        kinstDict[thiskinst] = [self._madInstObj.getInstrumentName(thiskinst), []]
                    kinstDict[thiskinst][1].append(syear)
                    kinstDict[thiskinst][1].append(eyear)

                    if thiskinst not in kinstDict_priv:
                        kinstDict_priv[thiskinst] = [self._madInstObj.getInstrumentName(thiskinst), []]
                    kinstDict_priv[thiskinst][1].append(syear)
                    kinstDict_priv[thiskinst][1].append(eyear)


                    # build siteDict
                    if thissite not in siteDict:
                        siteDict[thissite] = [[]]
                    siteDict[thissite][0].append(thiskinst)

                    if thissite not in siteDict_priv:
                        siteDict_priv[thissite] = [[]]
                    siteDict_priv[thissite][0].append(thiskinst)


                    # build catDict
                    if thiscategory not in catDict:
                        catDict[thiscategory] = [[]]
                    catDict[thiscategory][0].append(thiskinst)

                    if thiscategory not in catDict_priv:
                        catDict_priv[thiscategory] = [[]]
                    catDict_priv[thiscategory][0].append(thiskinst)
                    

            # remove duplicate years
            for kinst in kinstDict.keys():
                kinstDict[kinst][1] = sorted(list(set(kinstDict[kinst][1])))
            for kinst in kinstDict_priv.keys():
                kinstDict_priv[kinst][1] = sorted(list(set(kinstDict_priv[kinst][1])))
            
            # remove duplicate kinsts
            for siteID in siteDict.keys():
                siteDict[siteID][0] = list(set(siteDict[siteID][0]))
            for siteID in siteDict_priv.keys():
                siteDict_priv[siteID][0] = list(set(siteDict_priv[siteID][0]))
            for category in catDict.keys():
                catDict[category][0] = list(set(catDict[category][0]))
            for category in catDict_priv.keys():
                catDict_priv[category][0] = list(set(catDict_priv[category][0]))

            kinstDF = pandas.DataFrame.from_dict(kinstDict)
            kinstDF_priv = pandas.DataFrame.from_dict(kinstDict_priv)
            siteDF = pandas.DataFrame.from_dict(siteDict)
            siteDF_priv = pandas.DataFrame.from_dict(siteDict_priv)
            catDF = pandas.DataFrame.from_dict(catDict)
            catDF_priv = pandas.DataFrame.from_dict(catDict_priv)

            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
            privInstDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")

            kinstDF.to_hdf(instDataFile, key="kinst")
            siteDF.to_hdf(instDataFile, key="site")
            catDF.to_hdf(instDataFile, key="category")

            kinstDF_priv.to_hdf(privInstDataFile, key="kinst")
            siteDF_priv.to_hdf(privInstDataFile, key="site")
            catDF_priv.to_hdf(privInstDataFile, key="category")


        except:
            self.__closeMetaDBConnector()
            raise madrigal.admin.MadrigalError('Problem rebuilding instData',
                                                   traceback.format_exception(sys.exc_info()[0],
                                                                              sys.exc_info()[1],
                                                                              sys.exc_info()[2]))
        

    def getInstrumentsForWeb(self):
        """getInstrumentsForWeb gets all (kinst, siteID) tuples needed
        for getSingleRedirectList.

        Inputs: None

        Returns: list of (kinst, siteID) tuples
        
        """
        if self.priv:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")
        else:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
            
        siteDF = pandas.read_hdf(instDataFile, key="site")
        siteDict = siteDF.to_dict()

        kinstList = []

        for site in siteDict.keys():
            print(siteDict[site][0])
            kinstList += [(kinst, site) for kinst in siteDict[site][0]]

        return(kinstList)
    

    def getInstrumentsForFTP(self):
        """getInstrumentsForFTP gets all (instDesc, kinst) tuples needed
        for views.ftp.

        Inputs: None

        Returns: list of (instDesc, kinst) tuples
        
        """
        if self.priv:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")
        else:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
            
        siteDF = pandas.read_hdf(instDataFile, key="site")
        kinstDF = pandas.read_hdf(instDataFile, key="kinst")
        siteDict = siteDF.to_dict()
        kinstDict = kinstDF.to_dict()

        localSiteID = self.__madDB.getSiteID()

        kinstList = []

        for kinst in siteDict[localSiteID][0]:
            kinstList.append((kinstDict[kinst][0], kinst))
        
        return(kinstList)
    

    def getInstrumentsFor(self, categoryID=0, local=False):
        """
        
        
        getInstruments returns a list of (kinst, instrument desc, siteID) tuples.

        Inputs: 
        
            categoryID - category id to return instruments with data for. If 0, return all
            local - if False, return all instruments with that category for which there is data 
                anywhere in Madrigal. If True, only return local instruments, in which case siteID
                is always the local siteID.
        
        Returns:  a list of (kinst, instrument desc, siteID) tuples

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        if self.priv:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")
        else:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
            
        siteDF = pandas.read_hdf(instDataFile, key="site")
        kinstDF = pandas.read_hdf(instDataFile, key="kinst")
        categoryDF = pandas.read_hdf(instDataFile, key="category")
        siteDict = siteDF.to_dict()
        kinstDict = kinstDF.to_dict()
        catDict = categoryDF.to_dict()

        localSiteID = self.__madDB.getSiteID()
        


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

            if self.priv:
                instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")
            else:
                instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
                
            siteDF = pandas.read_hdf(instDataFile, key="site")
            siteDict = siteDF.to_dict()
            
            siteID = self.__madDB.getSiteID()
            kinstList = siteDict[siteID][0]
            catList = [(self._madInstObj.getCategoryId(kinst), self._madInstObj.getCategory(kinst)) for kinst in kinstList]
            return(catList)

        else:
            query = "SELECT * FROM instType"


            try:
                self.__initMetaDBConnector()
                result = self.__cursor.execute(query)
                resList = result.fetchall()
                self.__closeMetaDBConnector()

                return(resList)
            except:
                self.__closeMetaDBConnector()
                raise madrigal.admin.MadrigalError('Problem getting categories',
                                                    traceback.format_exception(sys.exc_info()[0],
                                                                                sys.exc_info()[1],
                                                                                sys.exc_info()[2]))
        
            
        


    def getInstruments(self, categoryID=0, local=False):
        """
        
        
        getInstruments returns a list of (kinst, instrument desc, siteID) tuples.

        Inputs: 
        
            categoryID - category id to return instruments with data for. If 0, return all
            local - if False, return all instruments with that category for which there is data 
                anywhere in Madrigal. If True, only return local instruments, in which case siteID
                is always the local siteID.
        
        Returns:  a list of (kinst, instrument desc, siteID) tuples

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """

        if self.priv:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")
        else:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
            
        siteDF = pandas.read_hdf(instDataFile, key="site")
        kinstDF = pandas.read_hdf(instDataFile, key="kinst")
        categoryDF = pandas.read_hdf(instDataFile, key="category")
        siteDict = siteDF.to_dict()
        kinstDict = kinstDF.to_dict()
        catDict = categoryDF.to_dict()

        localSiteID = self.__madDB.getSiteID()

        # case 1: categoryID != 0 AND local
        if (categoryID != 0) and local:
            kinsts = list(set(siteDict[localSiteID][0] + catDict[categoryID][0]))
            retList = [(kinst, kinstDict[kinst][0], localSiteID) for kinst in kinsts]
            return(retList)
        
        # case 2: categoryID != 0 
        if (categoryID != 0) and (not local):
            retList = []
            for site in siteDict.keys():
                kinsts = list(set(siteDict[site][0] + catDict[categoryID][0]))
                retList += [(kinst, kinstDict[kinst][0], site) for kinst in kinsts]
            return(retList)

        # case 3: categoryID == 0 AND local
        if (categoryID == 0) and local:
            retList = [(kinst, kinstDict[kinst][0], localSiteID) for kinst in siteDict[localSiteID][0]]
            return(retList)

        # case 4: categoryID == 0
        if (categoryID == 0) and (not local):
            retList = []
            for site in siteDict.keys():
                retList += [(kinst, kinstDict[kinst][0], site) for kinst in siteDict[site][0]]
            return(retList)







        # NOTE 2 SELF: the way this is currently written may be 
        # quite inefficient, ideally you would want to cache these 
        # results somewhere


        # GETTING CATEGORY IS BROKEN
        # FIX ME    


        # want kinst and sid from expTab
        # then want kinst where category = catid from instTab
        # finally get intersecting kinsts

        # siteID = self.__madDB.getSiteID()

        # query1 = "SELECT kinst, sid FROM expTab"
        # if local:
        #     cond = (" WHERE sid={}").format(siteID)
        #     query1 += cond

        # query2 = "SELECT kinst, name FROM instTab"
        # if categoryID != 0:
        #     query2 += " WHERE category={}".format(categoryID)
            

        # # first get matching kinst list
        # try:
        #     self.__initMetaDBConnector()
        #     result = self.__cursor.execute(query1)
        #     resList1 = result.fetchall()

        #     # key: kinst, value: site id
        #     kinstdict = {}
        #     for items in resList1:
        #         if items[0] not in kinstdict.keys():
        #             kinstdict[items[0]] = []
        #             kinstdict[items[0]].append(items[1])
        #         else:
        #             if items[1] not in kinstdict[items[0]]:
        #                 kinstdict[items[0]].append(items[1])

        #     result = self.__cursor.execute(query2)
        #     resList2 = result.fetchall()
        #     # key: kinst, value: inst name
        #     resList2 = {i[0]:i[1] for i in resList2}
        #     self.__closeMetaDBConnector()

        #     #print(resList2[30])
        #     #print(kinstdict[30])
        #     # retList = [(kinst, resList2[kinst], kinstdict[kinst]) for kinst in kinstdict.keys()]
        #     retList = []

        #     if categoryID != 0:
        #         for kinst in set(kinstdict.keys()).intersection(set(resList2.keys())):
        #             for site in kinstdict[kinst]:
        #                 retList.append((kinst, resList2[kinst], site))
        #     else:
        #         for kinst in kinstdict.keys():
        #             for site in kinstdict[kinst]:
        #                 retList.append((kinst, resList2[kinst], site))
            
        #     return(retList)
        # except:
        #     self.__closeMetaDBConnector()
        #     raise madrigal.admin.MadrigalError('Problem getting instruments',
        #                                            traceback.format_exception(sys.exc_info()[0],
        #                                                                       sys.exc_info()[1],
        #                                                                       sys.exc_info()[2]))
        
    
    def getInstrumentYears(self, kinst):
        """getInstrumentYears returns the a list of years (int) for instrument.

        Inputs: 
        
            kinst - the instrument id
        
        Returns:  an ordered list of years as integers.  If none found, raises error

        Affects: None

        Exceptions: MadrigalError if any item in row cannot be cast to correct format
        """
        if self.priv:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instDataPriv.hdf5")
        else:
            instDataFile = os.path.join(self.__madDB.getMetadataDir(), "instData.hdf5")
            
        kinstDF = pandas.read_hdf(instDataFile, key="kinst")
        kinstDict = kinstDF.to_dict()

        return(kinstDict[kinst][1])


