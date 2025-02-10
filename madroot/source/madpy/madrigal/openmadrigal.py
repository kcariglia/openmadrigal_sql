"""The openmadrigal module provides access to all OpenMadrigal installations via http and to OpenMadrigal Subversion.

$Id: openmadrigal.py 7044 2019-10-07 19:13:16Z brideout $
"""


import os, os.path, sys
import urllib
import re


import madrigal.admin
import madrigal.metadata


class OpenMadrigal:
    """OpenMadrigal is an object that provides access to all Open Madrigal installations via http and to OpenMadrigal Subversion.


    Usage example::

        import madrigal.openmadrigal
    
        try:
        
            test =  madrigal.openmadrigal.OpenMadrigal()

            # get the metadata file fileTab.txt from Madrigal site with id = 1

            test.getFileTab(1)
            
        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()
            


    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Aug. 14, 2002

    """


    #constants
    _webProtocol  = "https"
    """ Change the above string to use another web protocol such as https. """
    
    _openMadrigalUrl = 'https://cedar.openmadrigal.org/'
    
    
    # the following constants are used by cgi script getMetadata
    __expTab     = 0
    __fileTab    = 1
    __dataTab    = 2
    __instTab    = 3
    __parcods    = 4
    __siteTab    = 5
    __typeTab    = 6
    __instKindat = 7
    __instParm   = 8
    __madCat     = 9
    __instType   = 10


    def __init__(self, madDB=None):
        """__init__ initializes OpenMadrigal by setting or creating a MadrigalDB object.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: None.
        """
        
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # create the needed MadrigalSite object
        self.__madSite = madrigal.metadata.MadrigalSite(self.__madDB)


    def __getMetadata(self, siteId, metadataType):
        """ __getMetadata is a private helper function called to get metadata files via the web.
        
        Inputs: siteId - integer identifying Madrigal site.
            metadataType - constant defined by getMetadata cgi script
        
        Returns: The desired metadata file as a string, or None if not successful

        Affects: None.

        Exceptions: None.
        """
        # get site server name
        serverName = self.__madSite.getSiteServer(siteId)
        if serverName == None:
            return None

        # get site relative cgi path
        relativeCgi = self.__madSite.getSiteRelativeCGI(siteId)
        if relativeCgi == None:
            return None

        # create url string
        if not relativeCgi in ('', '.'):
            urlStr = serverName + '/' + \
                     relativeCgi + '/getMetadata?fileType=' + str(metadataType)
        else:
            urlStr = serverName +  \
                     '/getMetadata?fileType=' + str(metadataType)
        if urlStr.find('http') == -1:
            if siteId not in (1, 8, 22):
                urlStr = self._webProtocol + '://' + urlStr
            else:
                # Millstone, Beijing, and Illinois sites still uses http
                urlStr = "http" + '://' + urlStr
        
        
        try:
            file = urllib.request.urlopen(urlStr)
            fileStr = file.read()
            if type(fileStr) == bytes:
                fileStr = fileStr.decode('utf8')
            return fileStr
        
        except:
            return None
        
        
    def _limitColumns(self, text, numCols, delimiter=','):
        """_limitColumns is a private method that takes metadata text and returns text with a set number of columns.
        
        Used to remove optional metadata until all Madrigal sites can handle it.
        
        Inputs:
        
            text:  metadata text to modify by limiting to a set number of columns
            
            numCols: number of columns to limit metadata to
            
            delimiter: delimiter to use for splitting the metadata.  Default is comma
            
        Returns:
        
            Modified metadata text
        """
        newText = ''
        
        lines = text.split('\n')
        for line in lines:
            if len(line.strip()) == 0:
                continue
            items = line.split(delimiter)
            if len(items) < numCols:
                raise IOError('In line <%s> needed %i items, found only %i' % (line, numCols, len(items)))
            newLine = delimiter.join(items[:numCols])
            newText += '%s\n' % (newLine)
            
        return(newText)

    

    # public methods
    
    def getOpenMadrigalUrl(self):
        """returns self._openMadrigalUrl
        """
        return(self._openMadrigalUrl)
    
    

    def getMetadataFromOpenMadrigal(self, filename):
        """getMetadataFromOpenMadrigal returns a shared metadata file from OpenMadrigal server
        as a string

        
        Inputs:

            filename - metadata file to download, relative to metadata
        
        Returns: File contents as a string. 

        Affects: Nothing
        """
        urlStr = self._openMadrigalUrl + 'getOpenMadrigalSharedFiles?filename=%s' % (filename)
        try:
            file = urllib.request.urlopen(urlStr)
            fileStr = file.read()
            return(fileStr.decode('utf8'))
        
        except:
            return('')
        

    

    def getExpMetadata(self, siteId, numCols=None):
        """getExpMetadata returns the expTab.txt file from siteId as a string.

        
        Inputs: 

            siteId - the siteId to retrieve the expTab.txt file from
            
            numCols - if None, do not limit the number of columns.  If numCols, then
                limit metadata to numCols in length.
        
        Returns: the expTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__expTab)
        
        if numCols:
            fileStr = self._limitColumns(fileStr, numCols)

        if type(fileStr) == bytes:
            fileStr = fileStr.decode('utf8')

        return(fileStr)


    def getFileMetadata(self, siteId, numCols=None):
        """getFileMetadata returns the fileTab.txt file from siteId as a string.

        
        Inputs: 

            siteId - the siteId to retrieve the expTab.txt file from
            
            numCols - if None, do not limit the number of columns.  If numCols, then
                limit metadata to numCols in length.
        
        Returns: the fileTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__fileTab)
        
        if numCols:
            fileStr = self._limitColumns(fileStr, numCols)
        
        if type(fileStr) == bytes:
            fileStr = fileStr.decode('utf8')

        return(fileStr)


    def getDataMetadata(self, siteId):
        """getDataMetadata returns the dataTab.txt file from siteId as a string.

        This file is deprecated with Madrigal 2.5 and may not exist.

        
        Inputs: None
        
        Returns: the dataTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__dataTab)

        return fileStr


    def getInstMetadata(self, siteId):
        """getInstMetadata returns the instTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the instTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__instTab)

        return fileStr


    def getParcodsMetadata(self, siteId):
        """getParcodsMetadata returns the parcods.tab (or parmCodes.txt if Madrigal3) file from siteId as a string.

        
        Inputs: None
        
        Returns: the parcods.tab file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__parcods)

        return fileStr


    def getSiteMetadata(self, siteId):
        """getSiteMetadata returns the siteTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the siteTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__siteTab)

        return fileStr


    def getTypeMetadata(self, siteId):
        """getTypeMetadata returns the typeTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the typeTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__typeTab)

        return fileStr


    def getMadCatMetadata(self, siteId):
        """getMadCatMetadata returns the madCatTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the madCatTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__madCat)

        return fileStr


    def getInstTypeMetadata(self, siteId):
        """getInstTypeMetadata returns the instType.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the instType.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__instType)

        return fileStr
    
    
    def getLatestSubversionVersion(self, fullPath):
        """getLatestSubversionVersion returns the latest Subversion version of the file given by fullPath as a string.

        Inputs: fullPath - full path to the Madrigal file in Subversion relative to trunk
        (example: 'madroot/metadata/siteTab.txt')
        
        Returns: the latest Subversion version of the file given by fullPath as a string,
        or None if not found

        Affects: Nothing

        Exceptions: None
        
        Simply calls equivalent method getLatestCvsVersion
        """
        return(self.getLatestCvsVersion(fullPath))


    def getLatestCvsVersion(self, fullPath):
        """getLatestCvsVersion returns the latest Subversion version of the file given by fullPath as a string.
        
        CVS is no longer the Madrigal repository. Equivalent to getLatestSubversionVersion.

        Inputs: fullPath - full path to the Madrigal file in Subversion relative to trunk
        (example: 'madroot/metadata/siteTab.txt')
        
        Returns: the latest Subversion version of the file given by fullPath as a string,
        or None if not found

        Affects: Nothing

        Exceptions: None
        """
        try:
            urlStr = self._openMadrigalUrl + 'getLatestMetadataVersion?fullPath=%s' % (fullPath)

            f = urllib.request.urlopen(urlStr)
            fileStr = f.read()
            f.close()

            if type(fileStr) == bytes:
                fileStr = fileStr.decode('utf8')
            return(fileStr)


        except:
            return None


    def getAllRevisionNumbers(self, fullPath):
        """getAllRevisionNumbers a list of all revision numbers for a given file in Subversion in
        order from latest to earliest.

        Inputs: fullPath - full path to the Madrigal file in Subversion relative to trunk
        (example: 'madroot/metadata/siteTab.txt')
        
        Returns: a list of all revision numbers for a given file in Subversion in
        order from latest to earliest.  Empty list if file not found

        Affects: Nothing

        Exceptions: None
        """
        try:
            urlStr = self._openMadrigalUrl + 'getAllMetadataVersions?fullPath=%s' % (fullPath)

            f = urllib.request.urlopen(urlStr)
            fileStr = (f.read()).decode('utf8')
            f.close()
            
            retList = [item.strip() for item in fileStr.split('\n') if len(item.strip()) > 0 ]
            
            return(retList)

        except:
            return []
        
        
    def getSubversionVersion(self, fullPath, revision):
        """getSubversionVersion returns the Subversion version of the file given by fullPath and revision.

        Inputs:

            fullPath - full path to the Madrigal file in Subversion
            (example: 'madroot/metadata/siteTab.txt')

            revision - revision string (example '1445')
        
        Returns: the Subversion version of the file given by fullPath and revision,
        or None if not found

        Affects: Nothing

        Exceptions: None
        
        Simply calls equivalent method getCvsVersion
        """
        return(self.getCvsVersion(fullPath, revision))
    


    def getCvsVersion(self, fullPath, revision):
        """getCvsVersion returns the Subversion version of the file given by fullPath and revision.
        
        CVS is no longer the Madrigal repository. Equivalent to getSubversionVersion.

        Inputs:

            fullPath - full path to the Madrigal file in Subversion
            (example: 'madroot/metadata/siteTab.txt')

            revision - revision string (example '1445')
        
        Returns: the Subversion version of the file given by fullPath and revision,
        or None if not found

        Affects: Nothing

        Exceptions: None
        """
        try:
            urlStr = self._openMadrigalUrl + 'getMetadataVersion?fullPath=%s&version=%s' % (fullPath, str(revision))

            finalFile = urllib.request.urlopen(urlStr)
            text = finalFile.read()
            finalFile.close()
            return(text.decode('utf8'))

        except:
            return None

        
        

if __name__ == '__main__':

    test = OpenMadrigal()

    print('This is expTab.txt:')
    
    print(test.getExpMetadata(998) + '\n')

    print('This is fileTab.txt:')
    
    print(test.getFileMetadata(998) + '\n')

    print('This is dataTab.txt:')
    
    print(test.getDataMetadata(998) + '\n')

    print('This is instTab.txt:')
    
    print(test.getInstMetadata(998) + '\n')

    print('This is parcods.tab:')
    
    print(test.getParcodsMetadata(998) + '\n')

    print('This is siteTab.txt:')
    
    print(test.getSiteMetadata(998) + '\n')

    print('This is typeTab.txt:')
    
    print(test.getTypeMetadata(998) + '\n')
    
    print(test.getLatestSubversionVersion('madroot/metadata/siteTab.txt'))

    print(test.getLatestCvsVersion('madroot/metadata/siteTab.txt'))
    
    print(test.getAllRevisionNumbers('madroot/metadata/siteTab.txt'))
    
    print(test.getSubversionVersion('madroot/metadata/siteTab.txt', 1808))
    
    print(test.getCvsVersion('madroot/metadata/siteTab.txt', 1808))
