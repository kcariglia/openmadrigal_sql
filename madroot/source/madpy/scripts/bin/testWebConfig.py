#!PYTHONEXE

"""testWebConfig.py is a test script designed to detect whether madroot/lib needs to be added to the web server
configuration file.

$Id$
"""

import os, os.path, sys
import traceback
import urllib.request, urllib.error, urllib.parse

import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()
madroot = madDB.getMadroot()

message = """\nPROBLEM FOUND: You either need to restart your web server, or you
need to configure the web server to search for libraries in %s/lib.  
For apache, you need to add a line like:

SetEnv LD_LIBRARY_PATH /usr/local/lib:%s/lib

to the http.conf file.  After you have made this change, rerun this test as follows:

%s/bin/python %s/source/madpy/scripts/bin/testWebConfig.py

and verify you get a message that indicates success.
""" % (madroot, madroot, madroot, madroot)

# create test url
url = os.path.join(madDB.getTopLevelUrl(), 'getInstrumentsService.py')

try:
    f = urllib.request.urlopen(url)
    text = f.read()
    f.close()
except:
    print(message)
    sys.exit(-1)
    
if type(text) == bytes:
    text = text.decode('utf-8')
    
if text.find('Internal Server Error') != -1:
    print(message)
else:
    print('SUCCESS: Web configuration okay')
    