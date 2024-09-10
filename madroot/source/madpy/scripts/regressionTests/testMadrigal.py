"""testMadrigal.py is the regression test for the entire python madrigal library.
The admin module is not tested, since the testAdminScripts.py regression test covers it.

This script assumes the standard test experiments have been installed.

Add --verbose flag to have detailed results printed out

Aborts if any errors found.

$Id: testMadrigal.py 7663 2024-07-18 18:52:45Z kcariglia $
"""

import os, os.path, sys
import shutil
import traceback
import datetime
import glob

import numpy

import madrigal._Madrec
import madrigal.metadata
import madrigal.data
import madrigal.cedar
import madrigal.openmadrigal
import madrigal.ui.madrigalPlot
import madrigal.ui.userData

verbose = False
for arg in sys.argv[1:]:
    if arg == '--verbose':
        verbose = True

print('The following is a regression test of the Madrigal library.\n')

try:
    # if anything goes wrong, clean up anyway

    madDBObj = madrigal.metadata.MadrigalDB()
    madroot = madDBObj.getMadroot()

    expBase = os.path.join(madroot, 'experiments')
    binDir = os.path.join(madroot, 'bin')
    testExp = os.path.join(expBase, '1998/mlh/20jan98')
    testFile = os.path.join(testExp, 'mil980120g.002')
    testCat = os.path.join(testExp, 'mil980120g.c.003')


  


    #### test of madIsrRecordSummary ####
    print('#### Testing madIsrRecordSummary class ####\n')

    # create some dummy data
    TiData = numpy.array([[500.0,100.0], [550.0,150.0], [600.0,200.0], [520.0,250.0]])
    TiError = numpy.array([50.0, 60.0, 70.0, 80.0])
    TeData = numpy.array([[520.0,100.0], [600.0,150.0], [650.0,200.0], [700.0,250.0]])
    TeError = numpy.array([50.0, 60.0, 70.0, 80.0])
    VoData = numpy.array([[10.0,100.0], [0.0,150.0], [-10.0,200.0], [-5.0,250.0]])
    VoError = numpy.array([3.0, 4.0, 3.0, 5.0])
    NelData = numpy.array([[11.0,100.0], [11.2,150.0], [11.5,200.0], [11.7,250.0]])
    NelError = numpy.array([0.4, 0.3, 0.2, 0.5])
    description = 'The text here would\nsummarize this file'
    
    try:
        os.remove('/tmp/junk_isr.png')
    except:
        pass

    histPlotObj = madrigal.ui.madrigalPlot.madIsrRecordSummary(TiData,TiError,
                                                               TeData,TeError,
                                                               VoData,VoError,
                                                               NelData,NelError,
                                                               False,description,
                                                               '/tmp/junk_isr.png',
                                                               altResl=(5,10))
    
    if verbose:
        print('madIsrRecordSummary created plot  /tmp/junk_isr.png')
        
    print('#### Test of madIsrRecordSummary class succeeded ####\n')

   


except:
    traceback.print_exc()
    print('FAILURE')


### final clean up ###
try:
    os.remove('/tmp/junk.tar')
    os.remove(os.path.join(madroot, 'metadata/userdata/fred.xml'))
    # restore users.xml
    shutil.copy(os.path.join(madroot, 'metadata/userdata/users.xml.backup'),
                os.path.join(madroot, 'metadata/userdata/users.xml'))
except:
    pass






