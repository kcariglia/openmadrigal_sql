"""testDerivation.py is a regression test of the derivation engine.  First version compared Madrigal 2.6
and Madrigal 3.0

Assumes it is run from the regressionTest directory
"""
# standard python imports
import os, os.path
import time

# Madrigal imports
import madrigal.derivation
import madrigal.cedar

pwd = os.getcwd()
testFilename = os.path.join(pwd, '../bin/testScripts/mlh120113g.001.h5')
madCedarFile = madrigal.cedar.MadrigalCedarFile(testFilename)
measParms = list(madCedarFile.getDType().names)
measParms.sort()
derivableParms = madrigal.derivation.getDerivableParms(measParms, kinst=30)
derivableParms.sort()
print(('%i derivable parms found' % (len(derivableParms))))
delimiter = ' '
print((delimiter.join(derivableParms)))




# to limit size, only get first three records
filt1 = madrigal.derivation.MadrigalFilter('recno', [(float('nan'), 2)])
t = time.time()
madDerObj = madrigal.derivation.MadrigalDerivation(madCedarFile, derivableParms, [filt1])
madDerCedarFile = madDerObj.getNewCedarFile()
print(('derivation took %f secs' % (time.time() - t)))

# write to test file
madDerCedarFile.writeText('./derivation.txt', selectParms=derivableParms, showHeaders=True)