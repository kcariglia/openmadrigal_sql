#!PYTHONEXE

"""checkMetadata.py is a script run by the central Madrigal admin to ensure source control and distribution
metadata files siteTab.txt, instTab.txt, and instType.txt are in sync.

$Id: checkMetadata.py 7523 2023-05-16 13:30:45Z brideout $
"""


import sys
import os
import time
import traceback
import packaging.version


# catch any exception, and write an appropriate message admin
try:
    # check if pythonlibpath env variable exists
    # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
    temp = os.environ.get('PYTHON' + 'LIBPATH')
    if temp != None:
        sys.path.append(temp)
        
    # append path madroot/lib (needed only if python not installed by setup)
    sys.path.append('MADROOT/lib/python')

    # prepare to handle MadrigalError
    from madrigal.admin import *

except ImportError:
    
    # Fatal error - madpy library not found
    print("Unable to import the madrigal python library - please alert the sys admin!")
    sys.exit(0)

# try to run script, and report all errors to Madrigal sys admin
try:

    import madrigal.metadata
    import madrigal.openmadrigal

    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # if madroot not set, set it now
    if os.environ.get('MAD' + 'ROOT') == None:
        os.environ['MAD' + 'ROOT'] = madDBObj.getMadroot()

    # create MadrigalSite object
    madSite = madrigal.metadata.MadrigalSite(madDBObj)

    # create OpenMadrigal object
    openMad = madrigal.openmadrigal.OpenMadrigal(madDBObj)

    siteList = madSite.getSiteList()

    # first examine file siteTab.txt
    cvsSiteFile = openMad.getLatestCvsVersion('madroot/metadata/siteTab.txt')
    if cvsSiteFile == None:
        messStr += 'problem with cvs and siteTab.txt\n<br>\n'
        cvsSiteFile = ''
        
    cvsSiteFile3 = openMad.getLatestCvsVersion('madroot/metadata3/siteTab.txt')
    if cvsSiteFile3 == None:
        messStr += 'problem with cvs and mad3 siteTab.txt\n<br>\n'
        cvsSiteFile3 = ''

    # diff against every version from siteList
    for site in siteList:
        version = madSite.getSiteVersion(site[0])
        if packaging.version.parse(version) >= packaging.version.parse('3.0'):
            thisVersion = 3
        else:
            thisVersion = 2
        siteFile = openMad.getSiteMetadata(site[0])
        if siteFile == None:
            messStr += 'File siteTab.txt not retrieved from site ' + site[1] + '\n<br>\n'
        else:
            if thisVersion == 2:
                if siteFile.strip() != cvsSiteFile.strip():
                    messStr += 'File siteTab.txt differs from source control at site ' + site[1] + '\n<br>\n'
            else:
                if siteFile.strip() != cvsSiteFile3.strip():
                    messStr += 'File mad3 siteTab.txt differs from source control at site ' + site[1] + '\n<br>\n'

    # next examine file instTab.txt
    cvsTypeFile = openMad.getLatestCvsVersion('madroot/metadata/instTab.txt')
    if cvsTypeFile == None:
        messStr += 'problem with cvs and instTab.txt\n<br>\n'

    # diff against every version from siteList
    for site in siteList:
        typeFile = openMad.getInstMetadata(site[0])
        if typeFile == None:
            messStr += 'File instTab.txt not retrieved from site ' + site[1] + '\n<br>\n'
        else:
            if typeFile.strip() != cvsTypeFile.strip():
                messStr += 'File instTab.txt differs from source control at site ' + site[1] + '\n<br>\n'

    # next examine file instTypeTab.txt
    cvsTypeFile = openMad.getLatestCvsVersion('madroot/metadata/instType.txt')
    if cvsTypeFile == None:
        messStr += 'problem with cvs and instType.txt\n<br>\n'

    # diff against every version from siteList
    for site in siteList:
        typeFile = openMad.getInstTypeMetadata(site[0])
        if typeFile == None:
            messStr += 'File instType.txt not retrieved from site ' + site[1] + '\n<br>\n'
        else:
            if typeFile.strip() != cvsTypeFile.strip():
                messStr += 'File instType.txt differs from source control at site ' + site[1] + '\n<br>\n'

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by checkMetadata')


except MadrigalError as e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in checkMetadata.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running checkMetadata.py' )

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]).find('exceptions.SystemExit') != -1:
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running checkMetadata.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running checkMetadata.py')


# end script

