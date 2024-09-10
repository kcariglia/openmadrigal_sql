"""testUserScripts.py is a regression test that will test the following scripts that
perform user tasks:

    isprint
    createRecordPlots.py
    setAccess
    tarExperiments
    mergeCedarFile
    mergeCedarFilesInTime
    printCedarRecords
    removeCedarRecords
    summarizeCedarFile

This script assumes to standard test experiments have been installed.

Aborts if any errors found.

$Id: testUserScripts.py 7174 2020-08-26 13:12:26Z brideout $
"""

import os, os.path, sys
import shutil
import traceback
import subprocess

import madrigal.metadata

print('The following is a regression test of the Madrigal user scripts.\n')

tmpDir = '/tmp/testDir'

try:
    # if anything goes wrong, clean up anyway

    madDBObj = madrigal.metadata.MadrigalDB()
    madroot = madDBObj.getMadroot()

    expBase = os.path.join(madroot, 'experiments')
    binDir = os.path.join(madroot, 'bin')

    # get test file
    testFile = os.path.join(expBase, '1998/mlh/20jan98/mlh980120g.002.hdf5') 

    #### test of isprint ####
    program = 'isprint'
    print('Testing %s ...' % (program))
    
    cmd = os.path.join(binDir, program)
    cmd += ' file=%s ' % (testFile)
    cmd += ' date1=1/21/1998 time1=14:00:00 '
    cmd += ' date2=1/21/1998 time2=15:00:00 '
    cmd += ' z=100,500 az=-170,170 el=10,90 '
    cmd += ' plen=,5e-3 filter=ti,0,2000 '
    cmd += ' gdalt gdlat glon ti dti kp '
    print('Command to be tested: <%s>' % (cmd))
    result = subprocess.check_output(cmd.split())
    result = result.decode('utf-8')
    resultLine = 'Millstone Hill UHF Steerable Antenna: 1998-01-21 1400:11-1401:09'
    if result.find(resultLine) != -1:
        print('Test of %s succeeded.\n' % (program))
    else:
        print('Error in %s - regression test aborting...' % (program))
        raise ValueError('')


    #### test of createRecordPlots.py ####
    program = 'createRecordPlots.py'
    print('Testing %s ...' % (program))


    cmd = os.path.join(binDir, program)
    cmd += ' %s ' % (testFile)
    print('Command to be tested: <%s>' % (cmd))
    print('This test may take some time to run ...')
    result = subprocess.check_output(cmd.split())
    if type(result) in (bytes, ):
        result = result.decode('utf-8')
    resultLine = '1110'
    print(result[:1000])
    if result.find(resultLine) != -1:
        print('Test of %s succeeded.\n' % (program))
    else:
        print('Error in %s - regression test aborting...' % (program))
        raise ValueError('')


    #### test of setAccess ####
    program = 'setAccess'
    print('Testing %s ...' % (program))

    cmd = os.path.join(binDir, program)
    cmd += ' %s public ' % (os.path.dirname(testFile))
    print('Command to be tested: <%s>' % (cmd))
    result = subprocess.check_output(cmd.split())
    if type(result) in (bytes, ):
        result = result.decode('utf-8')
    resultLine = 'Access set'
    if result.find(resultLine) != -1:
        print('Test of %s succeeded.\n' % (program))
    else:
        print('Error in %s - regression test aborting...' % (program))
        raise ValueError('')



    #### test of tarExperiments ####
    program = 'tarExperiments'
    print('Testing %s ...' % (program))
    try:
        os.mkdir(tmpDir)
    except:
        pass

    cmd = os.path.join(binDir, program)
    cmd += ' -start 19980119 -end 19980122 '
    cmd += ' -excludePrivData '
    cmd += ' -f %s' % (os.path.join(tmpDir, 'test.tar'))
    print('Command to be tested: <%s>' % (cmd))
    print('This test may take some time to run ...')
    result = subprocess.check_output(cmd.split())
    if type(result) in (bytes, ):
        result = result.decode('utf-8')
    resultLine = 'Running gzip on tarfile...'
    if result.find(resultLine) != -1:
        print('Test of %s succeeded.\n' % (program))
    else:
        print('Error in %s: result=%s - regression test aborting...' % (program, str(result)))
        raise ValueError('')
    

    print('SUCCESS')

except ValueError:
    traceback.print_exc()
    print('FAILURE')
except:
    traceback.print_exc()
    print('FAILURE')


### final clean up ###
os.system('rm -rf %s' % (tmpDir))





