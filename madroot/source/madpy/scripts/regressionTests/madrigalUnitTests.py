"""madrigalUnitTest.py is a unit test script for the Madrigal python library

First developed for the Madrigal 3.0 release

$Id: madrigalUnitTests.py 7655 2024-06-27 20:20:49Z kcariglia $
"""

# standard python modules
import unittest
import os, sys, os.path
import datetime
import glob
import shutil
import traceback
import types

# Madrigal imports
import madrigal.metadata
import madrigal.data
import madrigal.openmadrigal
import madrigal.ui.web

# third party imports
import django
import numpy

madDB = madrigal.metadata.MadrigalDB()
sys.path.append(os.path.join(madDB.getMadroot(), 'source/madpy/djangoMad'))
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "djangoMad.settings")
django.setup()

# helper methods
def readConfigFile(configFile, key):
    """readConfigFile returns everything after key = in configFile
    """
    with open(configFile) as f:
        lines = f.readlines()
        for line in lines:
            if line[:len(key)] == key:
                index = line.find('=') + 1
                return(line[index:].strip())
        
        raise IOError('key <%s> not found in %s' % (key, configFile))

class TestMadrigalDB(unittest.TestCase):
    """TestMadrigalDB performs unit tests on madrigal.metadata.MadrigalDB and on madrigal.metadata helper methods
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madroot = self.madDB.getMadroot()
        self.configFile = os.path.join(self.madroot, 'madrigal.cfg')
        
    def test_getMadrigalUTFromDT(self):
        dt = datetime.datetime(1998,1,20,13,52,43)
        result = madrigal.metadata.getMadrigalUTFromDT(dt)
        self.assertEqual(result, 1516456363)
        
    def test_getMadrigalUTFromDate(self):
        result = madrigal.metadata.getMadrigalUTFromDate(1998,1,20,13,52,43,0)
        self.assertEqual(result, 1516456363)
        
    def test_getUnixUTFromDT(self):
        dt = datetime.datetime(1998,1,20,13,52,43)
        result = madrigal.metadata.getUnixUTFromDT(dt)
        self.assertEqual(result, 1516456363 - 631152000)
        
    def test_getUnixUTFromDate(self):
        result = madrigal.metadata.getUnixUTFromDate(1998,1,20,13,52,43,0)
        self.assertEqual(result, 1516456363 - 631152000)
        
    def test_getDatabaseUtilityDirectory(self):
        self.assertEqual(self.madDB.getDatabaseUtilityDirectory(), os.path.join(self.madroot, 'bin'))
        
    def test_getWWWHomeBase(self):
        url = self.madDB.getWWWHomeBase()
        testUrlList = ['http://' + readConfigFile(self.configFile, 'MADSERVER'), readConfigFile(self.configFile, 'MADSERVER')]
        self.assertIn(url, testUrlList)
        
    def test_getMadServer(self):
        result = readConfigFile(self.configFile, 'MADSERVER')
        self.assertEqual(self.madDB.getMadServer(), result)
        
    def test_getTopLevelUrl(self):
        url1 = self.madDB.getWWWHomeBase()
        relative = readConfigFile(self.configFile, 'MADSERVERROOT')
        if relative not in ('', '.'):
            url1 = os.path.join(url1, readConfigFile(self.configFile, 'MADSERVERROOT'))
        url2 = self.madDB.getTopLevelUrl()
        self.assertEqual(url1, url2)
        
    def test_getRelativeTopLevel(self):
        url1 = self.madDB.getRelativeTopLevel()
        result =  readConfigFile(self.configFile, 'MADSERVERROOT')
        self.assertEqual(url1, result)
        
    def test_getSiteID(self):
        siteID = self.madDB.getSiteID()
        result = int(readConfigFile(self.configFile, 'SITEID'))
        self.assertEqual(siteID, result)
        
    def test_getMadrootEnvVarName(self):
        name = self.madDB.getMadrootEnvVarName()
        self.assertEqual(name, 'MADROOT')
        
    def test_getMadroot(self):
        result = readConfigFile(self.configFile, 'MADROOT')
        self.assertEqual(self.madroot, result)
        
    def test_getMetadataDir(self):
        self.assertEqual(self.madDB.getMetadataDir(), 
                         os.path.join(self.madroot, 'metadata'))
        
    def test_getExperimentDir(self):
        self.assertEqual(self.madDB.getExperimentDir(), 
                         os.path.join(self.madroot, 'experiments'))
        
    def test_getExperimentDirs(self):
        # verify only one
        globStr = self.madDB.getExperimentDir() + '*'
        count = 0
        items = (glob.glob(globStr))
        for item in items:
            if os.path.isdir(item):
                count += 1
        if count != 1:
            raise ValueError('regression tests assumes a site with only one experiment dir')
        self.assertEqual(self.madDB.getExperimentDirs()[0], 
                         os.path.join(self.madroot, 'experiments'))
        
    def test_getBinDir(self):
        self.assertEqual(self.madDB.getBinDir(),
                         self.madDB.getDatabaseUtilityDirectory())
        
    def test_getHtmlStyle(self):
        result = readConfigFile(self.configFile, 'HTMLSTYLE')
        self.assertEqual(self.madDB.getHtmlStyle(), result)
        
    def test_getBackgroundColor(self):
        newResult = None
        result = readConfigFile(self.configFile, 'HTMLSTYLE')
        items = result.split()
        for item in items:
            if item.upper().find('BGCOLOR') != -1:
                newResult = item[8:].strip()
        self.assertEqual(newResult, self.madDB.getBackgroundColor())
        
    def test_getIndexHead(self):
        result = readConfigFile(self.configFile, 'INDEXHEAD')
        self.assertEqual(self.madDB.getIndexHead(), result)
        
    def test_getContactLink(self):
        result = readConfigFile(self.configFile, 'CONTACT')
        self.assertEqual(self.madDB.getContactLink(), result)
        
    def test_getContactLink(self):
        result = readConfigFile(self.configFile, 'CONTACT')
        subStr = result[result.find(':')+1:]
        email = subStr[:subStr.find('"')]
        self.assertEqual(self.madDB.getContactEmail(), email)
        
    def test_getMailserver(self):
        result = readConfigFile(self.configFile, 'MAILSERVER')
        self.assertEqual(self.madDB.getMailserver(), result)
        
    def test_getPythonExecutable(self):
        pythonexe = self.madDB.getPythonExecutable()
        self.assertEqual(pythonexe, os.path.join(self.madDB.getBinDir(), 'python'))
        
    def test_getPlotButtonLabel(self):
        plotbuttonlabel = self.madDB.getPlotButtonLabel()
        self.assertEqual(plotbuttonlabel, 'Plots/Docs')
        
    def test_getLocalRulesOfRoad(self):
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
        'any questions about appropriate use of these data, contact <a href="mailto:%s">%s</a>' % \
        (self.madDB.getContactEmail(), self.madDB.getContactEmail())
        
        try:
            with open(os.path.join(self.madroot, 'local_rules_of_the_road.txt')) as f:
                result = f.read()
        except IOError:
            result = default_rules_of_road
        self.assertEqual(self.madDB.getLocalRulesOfRoad(), result)
        
    def test_getFullPathFromPartial(self):
        result = os.path.join(self.madroot, 'experiments', '1998/mlh/20jan98')
        self.assertEqual(self.madDB.getFullPathFromPartial('1998/mlh/20jan98'), result)
        
    def test_getExpList(self):
        result = os.path.join(self.madroot, 'experiments/1998/mlh/20jan98')
        exps = self.madDB.getExpList(kinstList=[30], startDate=datetime.datetime(1998,1,20), 
                                      endDate=datetime.datetime(1998,1,21,23,59,59))
        self.assertEqual(exps[0], result)
        
    def test_getFileList(self):
        result = os.path.join(self.madroot, 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        files = self.madDB.getFileList(kinstList=[30], kindatList=[3408,3410], startDate=[1998,1,20,0,0,0,0,0,0], 
                                       endDate=[1998,1,21,23,59,59,0,0,0], includeNonDefault=1)
        self.assertIn(result, files)
        
    def test_getFileListFromMetadata(self):
        result = os.path.join(self.madroot, 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        files = self.madDB.getFileListFromMetadata(kinstList=[30], kindatList=[3408, 3410], startDate=[1998,1,20,0,0,0,0,0,0], 
                                                   endDate=[1998,1,21,23,59,59,0,0,0], includeNonDefault=1)
        self.assertIn(result, files)
        
    def test_setFileAccess(self):
        expDir = os.path.join(self.madroot, 'experiments/1998/mlh/20jan98')
        madMetaFile = madrigal.metadata.MadrigalMetaFile(self.madDB, os.path.join(expDir, "fileTab.txt"))
        # set to private
        self.madDB.setFileAccess(expDir, 1)
        # f = open(os.path.join(expDir, 'fileTab.txt'))
        # lines = f.readlines()
        # f.close()
        # items = lines[0].split(',')
        # permission = int(items[10])
        permission = madMetaFile.getAccessByPosition()
        self.assertEqual(permission, 1)
        # set back to public
        self.madDB.setFileAccess(expDir, 0)
        # f = open(os.path.join(expDir, 'fileTab.txt'))
        # lines = f.readlines()
        # f.close()
        # items = lines[0].split(',')
        # permission = int(items[10])
        permission = madMetaFile.getAccessByPosition()
        self.assertEqual(permission, 0)
        
    def test_tarExperiments(self):
        tarFile = '/tmp/junk.tar'
        try:
            os.remove(tarFile)
        except:
            pass
        self.madDB.tarExperiments(tarFile, startDate=[1998,1,20,0,0,0,0,0,0],
                                  endDate=[1998,1,21,23,59,59,0,0,0])
        result = os.access(tarFile, os.R_OK)
        os.remove(tarFile)
        self.assertEqual(result, True)
        
    def test_listFileTimes(self):
        expDir = os.path.join(self.madroot, 'experiments/1998/mlh/20jan98')
        fileTimes = self.madDB.listFileTimes(expDir)
        found = False
        for filename, filetime in fileTimes:
            if os.path.basename(filename) == 'mlh980120g.002.hdf5':
                ts = os.stat(os.path.join(expDir, 'mlh980120g.002.hdf5')).st_mtime
                dt =  datetime.datetime.fromtimestamp(ts, datetime.UTC)
                self.assertEqual(dt, filetime)
                found = True
                break
        if not found:
            raise ValueError('Did not find mlh980120g.002.hdf5')
        
    def test_getKinstKindatConfig(self):
        extraParms, indParms, splitParms = self.madDB.getKinstKindatConfig(30, 3410)
        result = [['gdlat', 'glon', 'gdalt', 'ne', 'dne'], ['range'], ['pl', 'mdtyp', 'kinst']]
        self.assertEqual(extraParms, result[0])
        self.assertEqual(indParms, result[1])
        self.assertEqual(splitParms, result[2])
        
    def test_toString(self):
        result = self.madDB.toString()
        self.assertEqual(result[:len('Object type')], 'Object type')
        
    def test_isTestExperiment(self):
        exp = '1995/jro/01feb95'
        self.assertEqual(self.madDB.isTestExperiment(exp), True)
        
    def test_groupId(self):
        url = 'https://w3id.org/cedar?experiment_list=experiments/2013/mlh/16mar13&file_list=mlh130316g.004.hdf5'
        id =self.madDB.createGroupIdWithList('Bill Rideout', 'brideout@mit.edu', 'MIT', [url])
        result = self.madDB.getListFromGroupId(id)
        self.assertIn(url, result)
        
        
        
class TestMadrigalSite(unittest.TestCase):
    """TestMadrigalSite performs unit tests on madrigal.metadata.MadrigalSite
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madSite = madrigal.metadata.MadrigalSite(self.madDB)
        
    def test_getSiteName(self):
        self.assertEqual(self.madSite.getSiteName(1), 'Millstone')
        
    def test_getSiteServer(self):
        self.assertEqual(self.madSite.getSiteServer(1), 'millstonehill.haystack.mit.edu')
        
    def test_getSiteDocRoot(self):
        self.assertEqual(self.madSite.getSiteDocRoot(3), 'madrigal')
        
    def test_getSiteContactName(self):
        self.assertEqual(self.madSite.getSiteContactName(1), 'Bill Rideout')
        
    def test_getSiteAddress1(self):
        self.assertEqual(self.madSite.getSiteAddress1(1), 'MIT Haystack Observatory')
        
    def test_getSiteAddress2(self):
        self.assertEqual(self.madSite.getSiteAddress2(1), 'Route 40')
        
    def test_getSiteAddress3(self):
        self.assertEqual(self.madSite.getSiteAddress3(1), '')
        
    def test_getSiteCity(self):
        self.assertEqual(self.madSite.getSiteCity(1), 'Westford')
        
    def test_getSiteState(self):
        self.assertEqual(self.madSite.getSiteState(1), 'MA')
        
    def test_getSitePostalCode(self):
        self.assertEqual(self.madSite.getSitePostalCode(1), '01886')
        
    def test_getSiteCountry(self):
        self.assertEqual(self.madSite.getSiteCountry(1), 'USA')
        
    def test_getSiteTelephone(self):
        self.assertEqual(self.madSite.getSiteTelephone(1), '1-781-981-5624')
        
    def test_getSiteEmail(self):
        self.assertEqual(self.madSite.getSiteEmail(1), 'brideout@mit.edu')
        
    def test_getSiteList(self):
        siteList = self.madSite.getSiteList()
        # convert to dict
        siteDict = {int(key): value for key, value in siteList}
        self.assertEqual(siteDict[1], 'Millstone')
        
    def test_getSiteVersion(self):
        siteVersion = self.madSite.getSiteVersion(1)
        # verify its a list of numbers
        numberList = [int(value) for value in siteVersion.split('.')]
        self.assertTrue(type(numberList[0]) == int and type(numberList[1]) == int)
        
    def test_setSiteVersionBySiteID(self):
        self.madSite.setSiteVersionBySiteID(1,'3.0')
        # make sure an error not raised
        self.assertEqual(1,1)
        
    def test_writeMetadata(self):
        testFile = '/tmp/siteTab.txt'
        try:
            os.remove(testFile)
        except:
            pass
        self.madSite.writeMetadata(testFile)
        self.assertTrue(os.access(testFile, os.R_OK))
        
        
class TestMadrigalInstrument(unittest.TestCase):
    """TestMadrigalInstrument performs unit tests on madrigal.metadata.MadrigalInstrument
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madInst = madrigal.metadata.MadrigalInstrument(self.madDB)
        
    def test_getInstrumentName(self):
        self.assertEqual(self.madInst.getInstrumentName(30), 'Millstone Hill IS Radar')
        
    def test_getInstrumentMnemonic(self):
        self.assertEqual(self.madInst.getInstrumentMnemonic(30), 'mlh')
        
    def test_getLatitude(self):
        self.assertAlmostEqual(self.madInst.getLatitude(30), 42.619)
        
    def test_getLongitude(self):
        self.assertAlmostEqual(self.madInst.getLongitude(30), 288.51)
        
    def test_getAltitude(self):
        self.assertAlmostEqual(self.madInst.getAltitude(30), 0.146)
        
    def test_getContactName(self):
        self.assertEqual(self.madInst.getContactName(30), 'Phil Erickson')
        
    def test_getContactEmail(self):
        self.assertEqual(self.madInst.getContactEmail(30), 'perickson@haystack.mit.edu')
        
    def test_getContactAddress1(self):
        self.assertEqual(self.madInst.getContactAddress1(30), 'MIT/Haystack Observatory')
        
    def test_getCategory(self):
        self.assertEqual(self.madInst.getCategory(30), 'Incoherent Scatter Radars')
        
    def test_getCategoryId(self):
        self.assertEqual(self.madInst.getCategoryId(30), 1)
        
    def test_getInstrumentList(self):
        instList = self.madInst.getInstrumentList()
        self.assertIn(('Millstone Hill IS Radar', 'mlh', 30), instList)
        
    def test_getOrderedInstrumentList(self):
        instList = self.madInst.getOrderedInstrumentList()
        self.assertIn(('Millstone Hill IS Radar', 'mlh', 30, 'Incoherent Scatter Radars', 1), instList)
        
        
class TestMadrigalInstrumentParameters(unittest.TestCase):
    """TestMadrigalInstrumentParameters performs unit tests on madrigal.metadata.MadrigalInstrumentParameters
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madInstParms = madrigal.metadata.MadrigalInstrumentParameters(self.madDB)
        
    def test_getParameters(self):
        parms = self.madInstParms.getParameters(30)
        testParms = ['az1', 'az2', 'el1', 'el2', 'pl', 'sn', 'chisq', 'systmp', 'power', 'tfreq', 'popl', 'dpopl', 'ti', 'dti', 'tr', 'dtr', 'vo']
        for testParm in testParms:
            self.assertTrue((testParm in parms) or (testParm.upper() in parms))
            
    def test_rebuildInstParmTable(self):
        self.madInstParms.rebuildInstParmTable()
        # passes as long as no exception raised
        self.assertEqual(1, 1)
        
        
class TestMadrigalKindat(unittest.TestCase):
    """TestMadrigalKindat performs unit tests on madrigal.metadata.MadrigalKindat
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madKindat = madrigal.metadata.MadrigalKindat(self.madDB)
        
    def test_getKindatDescription(self):
        self.assertEqual(self.madKindat.getKindatDescription(str(3201)), 'HYCOR Basic Derived Parameters - INSCAL (1.0)')
        
    def test_getKindatList(self):
        kindatList = self.madKindat.getKindatList()
        testItem = ('HYCOR Basic Derived Parameters - INSCAL (1.0)', str(3201))
        self.assertIn(testItem, kindatList)
        
        
class TestMadrigalInstrumentKindats(unittest.TestCase):
    """TestMadrigalInstrumentKindats performs unit tests on madrigal.metadata.MadrigalInstrumentKindats
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madInstKindats = madrigal.metadata.MadrigalInstrumentData(self.madDB)
        
    def test_getKindatListForInstruments(self):
        kindats = self.madInstKindats.getKindatListForInstruments([30])
        self.assertIn(3410, kindats)
        
    def test_getKindatList(self):
        kindats = self.madInstKindats.getKindatListForInstrumentYear(30, 1998)
        testItem = ('HYCOR Basic Derived Parameters - INSCAL (1.0)', 3201)
        self.assertIn(3410, kindats)
        
        
    """
    inst kindat table now conglomerated into instData

    def test_rebuildInstKindatTable(self):
        try:
            self.madInstKindats.rebuildInstKindatTable()
            self.assertEqual(0,0)
        except:
            # should not get here
            self.fail('rebuildInstKindatTable raised exception')"""
            
            
class TestMadrigalExperiment(unittest.TestCase):
    """TestMadrigalExperiment performs unit tests on madrigal.metadata.MadrigalExperiment
    """
    
    @classmethod
    def setUpClass(cls):

        # 
        # self.testUrl = 'http://test/madtoc/experiments/1998/mlh/20jan98'
        # self.madDB = madrigal.metadata.MadrigalDB()
        # self.madExp = madrigal.metadata.MadrigalExperiment(self.madDB)
        # #self.madExp.setExpIdByPosition(0, 999)
        # #self.madExp.setExpUrlByPosition(0, self.testUrl)
        # self.realUrl = self.madDB.getTopLevelUrl() + '/showExperiment?experiment_list='
        # self.realUrl += self.testUrl[self.testUrl.find('experiments'):]
        # self.expPath = self.testUrl[self.testUrl.find('experiments'):]
        # self.expName = 'Test Experiment 123'
        # self.madExp.setExpNameByPosition(0, self.expName)
        # self.siteId = 99
        # self.madExp.setExpSiteIdByPosition(0, self.siteId)
        # # write it out
        # self.madExp.writeMetadata('/tmp/expTab.txt')
        # self.madExp = madrigal.metadata.MadrigalExperiment(self.madDB, '/tmp/expTab.txt')
        try:
            # set values for unit testing
            cls.madDB = madrigal.metadata.MadrigalDB()
            madroot = cls.madDB.getMadroot()
            expBase = os.path.join(madroot, 'experiments')
            binDir = os.path.join(madroot, 'bin')
            testUrl = 'http://test/madtoc/experiments2/1998/mlh/20jan98BillsTestExp'

            cls.realUrl = cls.madDB.getTopLevelUrl() + '/showExperiment?experiment_list='
            cls.realUrl += testUrl[testUrl.find('experiments'):]
            cls.expPath = testUrl[testUrl.find('experiments'):]
            cls.siteId = cls.madDB.getSiteID()
            cls.expName = 'Bill Rideout test experiment'
            cls.kindat = 3408

            # get test file
            testFile = os.path.join(expBase, '1998/mlh/20jan98/mlh980120g.002.hdf5')
            shutil.copy(testFile, '/tmp')
            # create test experiment
            os.makedirs(expBase + "2")
            cmd = os.path.join(binDir, 'createExpWithFile.py')
            cmd += ' --madFilename=/tmp/mlh980120g.002.hdf5 '
            cmd += f' --expTitle="{cls.expName}" '
            cmd += ' --permission=0 '
            cmd += ' --fileDesc="This is just a test" '
            cmd += ' --instCode=30 '
            cmd += f' --kindat={cls.kindat} '
            cmd += ' --dirName=20jan98BillsTestExp '
            cmd += ' --experimentsDirNum=2 '
            cmd += ' --PI="Bill Rideout" '
            cmd += ' --PIEmail="brideout@haystack.mit.edu" '
            cmd += ' --fileAnalyst="John Doe" '
            cmd += ' --fileAnalystEmail=jdoe@haystack.mit.edu '
            cmd += ' --createCachedText '
            cmd += ' --createCachedNetCDF4 '
            
            result = os.system(cmd)
            if result == 0:
                pass
            else:
                print('Error - could not create test experiment')
                raise ValueError('')
            
            # because we just created a new experiment,
            # we know that its expID must be the max possible
            # expID local to this site
            mainExpObj = madrigal.metadata.MadrigalExperiment(cls.madDB)
            idsAndIdxs = mainExpObj.getAllExpIDs(cls.siteId)
            expIDs = [i[0] for i in idsAndIdxs]
            cls.thisExpID = int(numpy.max(expIDs))

            cls.madExp = madrigal.metadata.MadrigalExperiment(cls.madDB, cls.expPath + '/expTab.txt')
        except Exception as e:
            traceback.print_exc()
            cls.tearDownClass(cls)
        
    """
    deprecated because we dont want to let users set their own exp ids

    def test_getExpIdByPosition(self):
        # set in setIp to 999
        self.assertEqual(self.madExp.getExpIdByPosition(0), 999)"""
        
    """
    deprecated because we dont want to let users set their own exp urls
    (if they want to redo that, they should probably use some masking
    mechanism via their apache config)
    
    def test_getExpUrlByPosition(self):
        # set in setUp 
        self.assertEqual(self.madExp.getExpUrlByPosition(0), self.testUrl)
        
    def test_getExpUrlByExpId(self):
        self.assertEqual(self.madExp.getExpUrlByExpId(999), self.testUrl)"""
        
        
    def test_getRealExpUrlByPosition(self):
        self.assertEqual(self.madExp.getRealExpUrlByPosition(0), self.realUrl)
        
    def test_getRealExpUrlByExpId(self):
        self.assertEqual(self.madExp.getRealExpUrlByExpId(self.thisExpID), self.realUrl)
        
    def test_getExpPathByPosition(self):
        self.assertEqual(self.madExp.getExpPathByPosition(0), self.expPath)
        
    def test_getExpPathByExpId(self):
        self.assertEqual(self.madExp.getExpPathByExpId(self.thisExpID), self.expPath)
        
    def test_getExpPathByPosition(self):
        self.assertEqual(self.madExp.getExpPathByPosition(0), self.expPath)
        
    def test_getExpDirByPosition(self):
        self.assertEqual(self.madExp.getExpDirByPosition(0), 
                         os.path.join(self.madDB.getMadroot(), self.expPath))
        
    def test_getExpDirByExpId(self):
        self.assertEqual(self.madExp.getExpDirByExpId(self.thisExpID), 
                         os.path.join(self.madDB.getMadroot(), self.expPath))
        
    def test_getExpNameByPosition(self):
        self.assertEqual(self.madExp.getExpNameByPosition(0), self.expName)
        
    def test_getExpNameByExpId(self):
        self.assertEqual(self.madExp.getExpNameByExpId(self.thisExpID), self.expName)
        
    def test_getExpSiteIdByPosition(self):
        self.assertEqual(self.madExp.getExpSiteIdByPosition(0), self.siteId)
        
    def test_getExpSiteIdByExpId(self):
        self.assertEqual(self.madExp.getExpSiteIdByExpId(self.thisExpID), self.siteId)

    def test_expTabCreated(self):
        expTabModTime = datetime.datetime.fromtimestamp(os.stat(os.path.join(self.madDB.getMadroot(), self.expPath + '/expTab.txt')).st_mtime, tz=datetime.timezone.utc)
        now = datetime.datetime.now(tz=datetime.timezone.utc)
        delta = datetime.timedelta(minutes=10)
        # assert test expTab.txt is less than 10 minutes old
        self.assertLess(now - expTabModTime, delta)
        
        
    @classmethod
    def tearDownClass(cls):
        try:
            # remove exp here
            expBase = os.path.join(madDB.getMadroot(), 'experiments2')
            thisExp = os.path.join(expBase, '1998/mlh', '20jan98BillsTestExp')
            os.system('rm -rf %s' % (thisExp))
            expObj = madrigal.metadata.MadrigalExperiment(madDB, thisExp + "/expTab.txt")
            expObj.validateExp()
            os.system('rm -r %s' % (expBase))
        except Exception as e:
            traceback.print_exc()
        
        
class TestMadrigalMetaFile(unittest.TestCase):
    """TestMadrigalMetaFile performs unit tests on madrigal.metadata.MadrigalMetaFile
    """
    @classmethod
    def setUpClass(cls):

        try:
            # set values for unit testing
            cls.madDB = madrigal.metadata.MadrigalDB()
            madroot = cls.madDB.getMadroot()
            expBase = os.path.join(madroot, 'experiments')
            binDir = os.path.join(madroot, 'bin')
            testUrl = 'http://test/madtoc/experiments2/1998/mlh/20jan98BillsTestExp'

            cls.realUrl = cls.madDB.getTopLevelUrl() + '/showExperiment?experiment_list='
            cls.realUrl += testUrl[testUrl.find('experiments'):]
            cls.expPath = testUrl[testUrl.find('experiments'):]
            cls.siteId = cls.madDB.getSiteID()
            cls.expName = 'Bill Rideout test experiment'
            cls.kindat = 3408

            # get test file
            testFile = os.path.join(expBase, '1998/mlh/20jan98/mlh980120g.002.hdf5')
            shutil.copy(testFile, '/tmp')
            # create test experiment
            os.makedirs(expBase + "2")
            cmd = os.path.join(binDir, 'createExpWithFile.py')
            cmd += ' --madFilename=/tmp/mlh980120g.002.hdf5 '
            cmd += f' --expTitle="{cls.expName}" '
            cmd += ' --permission=0 '
            cmd += ' --fileDesc="This is just a test" '
            cmd += ' --instCode=30 '
            cmd += f' --kindat={cls.kindat} '
            cmd += ' --dirName=20jan98BillsTestExp '
            cmd += ' --experimentsDirNum=2 '
            cmd += ' --PI="Bill Rideout" '
            cmd += ' --PIEmail="brideout@haystack.mit.edu" '
            cmd += ' --fileAnalyst="John Doe" '
            cmd += ' --fileAnalystEmail=jdoe@haystack.mit.edu '
            cmd += ' --createCachedText '
            cmd += ' --createCachedNetCDF4 '
            
            result = os.system(cmd)
            if result == 0:
                pass
            else:
                print('Error - could not create test experiment')
                raise ValueError('')
            
            # because we just created a new experiment,
            # we know that its expID must be the max possible
            # expID local to this site
            mainExpObj = madrigal.metadata.MadrigalExperiment(cls.madDB)
            idsAndIdxs = mainExpObj.getAllExpIDs(cls.siteId)
            expIDs = [i[0] for i in idsAndIdxs]
            cls.thisExpID = int(numpy.max(expIDs))
            
            cls.madFile = madrigal.metadata.MadrigalMetaFile(cls.madDB, cls.expPath + '/fileTab.txt')

            # self.madDB = madrigal.metadata.MadrigalDB()
            # self.madFile = madrigal.metadata.MadrigalMetaFile(self.madDB)
            # self.madFile.setExpIdByPosition(0, 999)
            cls._test_filename = "mlh980120g.002.hdf5"
            # self.madFile.setKindatByPosition(0, 1000)
            cls.madFile.setCategoryByPosition(0, 1)
            cls.madFile.setHasCatalogByPosition(0, 1)
            cls.madFile.setHasHeaderByPosition(0, 1)
            cls.madFile.setStatusByPosition(0, 'Final')
            cls._test_dt = datetime.datetime(1998,1,20, tzinfo=datetime.timezone.utc)
            cls.madFile.setFileDatetimeByPosition(0, cls._test_dt)
            cls.madFile.setAccessByPosition(0, 1)
            cls.madFile.setAnalystByPosition(0, 'Bill Rideout')
            cls.madFile.setAnalystEmailByPosition(0, 'brideout@mit.edu')
            # # write it out
            # self.tempDir = '/tmp/experiments/test'
            # if not os.access(self.tempDir, os.R_OK):
            #     os.makedirs(self.tempDir)
            # filePath = os.path.join(self.tempDir, 'fileTab.txt')
            # self.madFile.writeMetadata(filePath)
            # self.madFile = madrigal.metadata.MadrigalMetaFile(self.madDB, filePath)
        except Exception as e:
            traceback.print_exc()
            cls.tearDownClass(cls)
        
        
    def test_getFileCount(self):
        self.assertGreater(self.madFile.getFileCount(), 0)
        
    def test_getFilenameByPosition(self):
        self.assertIsNotNone(self._test_filename)
        self.assertEqual(self.madFile.getFilenameByPosition(0), self._test_filename)
        
    def test_getExpIdByPosition(self):
        self.assertEqual(self.madFile.getExpIdByPosition(0), self.thisExpID)
        
    def test_getKindatByPosition(self):
        self.assertEqual(self.madFile.getKindatByPosition(0), self.kindat)
        
    def test_getKindatByFilename(self):
        self.assertEqual(self.madFile.getKindatByFilename(self._test_filename), self.kindat)
        
    def test_getCategoryByPosition(self):
        self.assertEqual(self.madFile.getCategoryByPosition(0), 1)
        
    def test_getCategoryByFilename(self):
        self.assertEqual(self.madFile.getCategoryByFilename(self._test_filename), 1)
        
    def test_getHasCatalogByPosition(self):
        self.assertEqual(self.madFile.getHasCatalogByPosition(0), 1)
        
    def test_getHasCatalogByFilename(self):
        self.assertEqual(self.madFile.getHasCatalogByFilename(self._test_filename), 1)
        
    def test_getHasHeaderByPosition(self):
        self.assertEqual(self.madFile.getHasHeaderByPosition(0), 1)
        
    def test_getHasHeaderByFilename(self):
        self.assertEqual(self.madFile.getHasHeaderByFilename(self._test_filename), 1)
        
    def test_getStatusByPosition(self):
        self.assertEqual(self.madFile.getStatusByPosition(0), 'Final')
        
    def test_getStatusByFilename(self):
        self.assertEqual(self.madFile.getStatusByFilename(self._test_filename), 'Final')
        
    def test_getAccessByPosition(self):
        self.assertEqual(self.madFile.getAccessByPosition(0), 1)
        
    def test_getFileDatetimeByPosition(self):
        self.assertEqual(self.madFile.getFileDatetimeByPosition(0), self._test_dt)
        
    def test_getFileDatetimeByFilename(self):
        self.assertEqual(self.madFile.getFileDatetimeByFilename(self._test_filename), self._test_dt)
        
    def test_getExpIdByFilename(self):
        self.assertEqual(self.madFile.getExpIdByFilename(self._test_filename), self.thisExpID)
        
    def test_getAnalystByPosition(self):
        self.assertEqual(self.madFile.getAnalystByPosition(0), 'Bill Rideout')
        
    def test_getAnalystByfilename(self):
        self.assertEqual(self.madFile.getAnalystByFilename(self._test_filename), 'Bill Rideout')
        
    def test_getAnalystEmailByPosition(self):
        self.assertEqual(self.madFile.getAnalystEmailByPosition(0), 'brideout@mit.edu')
        
    def test_getAnalystEmailByfilename(self):
        self.assertEqual(self.madFile.getAnalystEmailByFilename(self._test_filename), 'brideout@mit.edu')
        
    def test_getMetadataSummaryByFilename(self):
        self.assertGreater(len(self.madFile.getMetadataSummaryByFilename(self._test_filename)), 0)
        
    def test_getFileDOIUrlByPosition(self):
        url = self.madFile.getFileDOIUrlByPosition(0)
        self.assertNotEqual(url.find('https://w3id.org/cedar?experiment_list=experiments'), -1)
        
    def test_getFileDOIUrlByFilename(self):
        url = self.madFile.getFileDOIUrlByPosition(0)
        self.assertEqual(self.madFile.getFileDOIUrlByFilename(self._test_filename), url)
        
    def test_str(self):
        self.assertGreater(len(str(self.madFile)), 0)

    def test_fileTabCreated(self):
        fileTabModTime = datetime.datetime.fromtimestamp(os.stat(os.path.join(self.madDB.getMadroot(), self.expPath + '/fileTab.txt')).st_mtime, tz=datetime.timezone.utc)
        now = datetime.datetime.now(tz=datetime.timezone.utc)
        delta = datetime.timedelta(minutes=10)
        # assert test fileTab.txt is less than 10 minutes old
        self.assertLess(now - fileTabModTime, delta)
        
    @classmethod
    def tearDownClass(cls):
        try:
           # remove exp here
            expBase = os.path.join(madDB.getMadroot(), 'experiments2')
            thisExp = os.path.join(expBase, '1998/mlh', '20jan98BillsTestExp')
            os.system('rm -rf %s' % (thisExp))
            expObj = madrigal.metadata.MadrigalExperiment(madDB, thisExp + "/expTab.txt")
            expObj.validateExp()
            os.system('rm -r %s' % (expBase))
        except Exception as e:
            traceback.print_exc()
        
        
class TestMadrigalParmCategory(unittest.TestCase):
    """TestMadrigalParmCategory performs unit tests on madrigal.metadata.MadrigalParmCategory
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madParmCat = madrigal.metadata.MadrigalParmCategory(self.madDB)
        
    def test_getCategoryDesc(self):
        self.assertEqual(self.madParmCat.getCategoryDesc(1), 'Geographic Coordinate')
        
    def test_getCategoryList(self):
        self.assertIn(('Geographic Coordinate', 1), self.madParmCat.getCategoryList())
        
        
class TestMadrigalInstrumentData(unittest.TestCase):
    """TestMadrigalInstrumentData performs unit tests on madrigal.metadata.MadrigalInstrumentData
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madInstData = madrigal.metadata.MadrigalInstrumentData(self.madDB)
        self.siteId = self.madDB.getSiteID()
        
    def test_getCategories(self):
        self.assertIn((2, 'Geophysical Indices'), self.madInstData.getCategories())
        
    def test_getInstruments(self):
        self.assertIn((212, 'DST Index', self.siteId), self.madInstData.getInstruments())
        
    def test_getInstrumentYears(self):
        self.assertIn(1957, self.madInstData.getInstrumentYears(212))


class TestMadrigalFile(unittest.TestCase):
    """TestMadrigalFile performs unit tests on madrigal.data.MadrigalFile
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        testFile = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        self.madFile = madrigal.data.MadrigalFile(testFile, self.madDB)
        
    def test_getStandardParms(self):
        self.assertIn('ut1_unix', self.madFile.getStandardParms())  
        
    def test_getKinstList(self):
        self.assertIn(31, self.madFile.getKinstList())
        
    def test_getKinstListStr(self):
        self.assertNotEqual(-1, self.madFile.getKinstListStr().find('Millstone Hill UHF Zenith Antenna'))
    
    def test_getKindatList(self):
        self.assertIn(3408, self.madFile.getKindatList()) 
        
    def test_getMeasuredParmList(self):
        self.assertIn(505, self.madFile.getMeasuredParmList()) 
        
    def test_getMeasured1dParmList(self):
        self.assertIn(402, self.madFile.getMeasured1dParmList())
        
    def test_getMeasured2dParmList(self):
        self.assertIn(505, self.madFile.getMeasured2dParmList())
        
    def test_getMeasuredIndParmList(self):
        self.assertIn(120, self.madFile.getMeasuredIndParmList())
    
    def test_getMeasDervBothParmLists(self):
        madWebObj = madrigal.ui.web.MadrigalWebFormat()
        parmList = madWebObj.getFormat('Comprehensive')
        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []
        self.madFile.getMeasDervBothParmLists(parmList, measParmList, derivedParmList, allParmList, sureParmList)
        self.assertIn('TI', measParmList)
        self.assertIn('BMAG', derivedParmList)
        self.assertIn('BMAG', allParmList)
        self.assertIn('DTI', sureParmList)
        
    def test_getMaxPulseLength(self):
        self.assertEqual(2000.0, self.madFile.getMaxPulseLength())
        
    def test_getMinPulseLength(self):
        self.assertEqual(300.0, self.madFile.getMinPulseLength())
        
    def test_getMaxValidAltitude(self):
        self.assertEqual(2110.0, self.madFile.getMaxValidAltitude())
        
    def test_getMinValidAltitude(self):
        self.assertEqual(67.0, self.madFile.getMinValidAltitude())
        
    def test_getMaxPulseLength(self):
        self.assertEqual(2000.0, self.madFile.getMaxPulseLength())
        
    def test_getMaxLatitude(self):
        self.assertEqual(65.0, self.madFile.getMaxLatitude())
        
    def test_getMinLatitude(self):
        self.assertEqual(27.0, self.madFile.getMinLatitude())
        
    def test_getMaxLongitude(self):
        self.assertEqual(-70.0, self.madFile.getMaxLongitude())
        
    def test_getMinLongitude(self):
        self.assertEqual(-72.0, self.madFile.getMinLongitude())
        
    def test_getEarliestTime(self):
        self.assertEqual([1998, 1, 20, 13, 52, 43], self.madFile.getEarliestTime())
        
    def test_getLatestTime(self):
        self.assertEqual([1998, 1, 21, 16, 38, 33], self.madFile.getLatestTime())
        
    def test_getCatalogHeaderStr(self):
        self.assertNotEqual(-1, self.madFile.getCatalogHeaderStr().find('CMODEXP More details are available from: http:'))
        
    def test_toString(self):
        self.assertNotEqual(-1, self.madFile.toString().find('Minimum longitude (degrees)'))
        
    
class TestMadrigalParameters(unittest.TestCase):
    """TestMadrigalParameters performs unit tests on madrigal.data.MadrigalParameters
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madParm = madrigal.data.MadrigalParameters(self.madDB)
        
    def test_getSortedMnemonicList(self):
        self.assertEqual('BYEAR', self.madParm.getSortedMnemonicList()[0]) 
        
    def test_getParmType(self):
        self.assertEqual(1, self.madParm.getParmType('TI')) 
        self.assertEqual(0, self.madParm.getParmType('DTI')) 
        
    def test_getParmCodeFromMnemonic(self):
        self.assertEqual(550, self.madParm.getParmCodeFromMnemonic('TI')) 
        self.assertEqual(-550, self.madParm.getParmCodeFromMnemonic('DTI'))
        
    def test_getParmCategory(self):
        self.assertEqual('I. S. Radar Basic Parameter', self.madParm.getParmCategory('TI'))
        
    def test_hasHtmlDesc(self):
        self.assertEqual(0, self.madParm.hasHtmlDesc('TI')) 
        self.assertEqual(1, self.madParm.hasHtmlDesc('BXGSM'))
        
    def test_getParmDescription(self):
        self.assertEqual('Ion temperature (Ti) - K', self.madParm.getParmDescription('TI'))
        
    def test_getSimpleParmDescription(self):
        self.assertEqual('Ion temperature (Ti)', self.madParm.getSimpleParmDescription('TI'))
        
    def test_getParmUnits(self):
        self.assertEqual('K', self.madParm.getParmUnits('TI'))
        
    def test_isError(self):
        self.assertEqual(False, self.madParm.isError('TI')) 
        self.assertEqual(True, self.madParm.isError('DTI'))
        
    def test_getParmFormat(self):
        self.assertEqual('%9.1f', self.madParm.getParmFormat('TI'))
    
    def test_isInteger(self):
        self.assertEqual(False, self.madParm.isInteger('TI')) 
        self.assertEqual(True, self.madParm.isInteger('YEAR'))
    
    def test_isString(self):
        self.assertEqual(False, self.madParm.isString('TI')) 
        self.assertEqual(True, self.madParm.isString('GPS_SITE'))
    
    def test_getStringLen(self):
        self.assertEqual(4, self.madParm.getStringLen('GPS_SITE')) 
        
    def test_getParmWidth(self):
        self.assertEqual(14, self.madParm.getParmWidth('PL_FQ_UP'))
        
    def test_getParmDescriptionList(self):
        self.assertEqual(['Ion temperature (Ti) - K'], self.madParm.getParmDescriptionList(['Ti']))
    
    def test_getParmMnemonic(self):
        self.assertEqual('TI', self.madParm.getParmMnemonic('550')) 
        self.assertEqual('TI', self.madParm.getParmMnemonic(550))
        self.assertEqual('TI', self.madParm.getParmMnemonic('Ti'))
        
    def test_getParmIndex(self):
        self.assertEqual(0.0, self.madParm.getParmIndex('BYEAR'))
        self.assertEqual(0.5, self.madParm.getParmIndex('DBYEAR'))
        
    def test_getParmMnemonicList(self):
        self.assertEqual(['TI', 'DTI'], self.madParm.getParmMnemonicList([550,-550]))
        
    def test_normalizeParmList(self):
        self.assertEqual([120,-120,550,-550], self.madParm.normalizeParmList(['DRANGE', 'DTI', 'TI', 'RANGE']))
        
    def test_getMadCategoryIndex(self):
        self.assertEqual(6, self.madParm.getMadCategoryIndex('I. S. Radar Basic Parameter'))
        
    def test_getCategoryDict(self):
        self.assertEqual({6: ['I. S. Radar Basic Parameter', ['TI', 'DTI']]}, 
                         self.madParm.getCategoryDict(['TI', 'DTI']))
        
    def test_orderParms(self):
        parmList = ['DRANGE', 'DTI', 'TI', 'RANGE']
        self.madParm.orderParms(parmList)
        self.assertEqual(['RANGE', 'DRANGE', 'TI', 'DTI'], parmList)
        
    def test_getMnemonicListFromExpression(self):
        self.assertEqual(['DTI', 'TI'], self.madParm.getMnemonicListFromExpression('DTI/Ti < 0.1'))
        
    def test_getStdExpression(self):
        self.assertEqual('DTI/TI < 0.1 ', self.madParm.getStdExpression('DTi/Ti < 0.1'))
        
    def test_getParametersForInstruments(self):
        self.assertIn('TI', self.madParm.getParametersForInstruments([30]))
        
    def test_getIsprintHeader(self):
        self.assertEqual(' DRANGE        DTI         TI      RANGE', 
                          self.madParm.getIsprintHeader(['DRANGE', 'DTI', 'TI', 'RANGE']))
        
        
class TestOpenMadrigal(unittest.TestCase):
    """TestOpenMadrigal performs unit tests on madrigal.openmadrigal.OpenMadrigal
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.openMad = madrigal.openmadrigal.OpenMadrigal(self.madDB)
        
    def test_getOpenMadrigalUrl(self):
        self.assertEqual('https://cedar.openmadrigal.org/', self.openMad.getOpenMadrigalUrl()) 
    
    def test_getOpenMadrigalUrl(self):
        self.assertIn('Millstone Hill UHF Zenith Antenna', self.openMad.getMetadataFromOpenMadrigal('instTab.txt')) 
    
    def test_getExpMetadata(self):
        self.assertIn('experiments/1998/mlh/20jan98', self.openMad.getExpMetadata(1))
        
    def test_getFileMetadata(self):
        self.assertIn('mlh980120g.002', self.openMad.getFileMetadata(1))
        
    def test_getInstMetadata(self):
        self.assertIn('Jicamarca IS Radar', self.openMad.getInstMetadata(1))
        
    def test_getParmsMetadata(self):
        self.assertIn('SUNRISE_HOUR', self.openMad.getParcodsMetadata(1))
        
    def test_getSiteMetadata(self):
        self.assertIn('Millstone', self.openMad.getSiteMetadata(1))
        
    def test_getTypeMetadata(self):
        self.assertIn('HYCOR Basic Derived Parameters', self.openMad.getTypeMetadata(1))  
        
    def test_getMadCatMetadata(self):
        self.assertIn('Interplanetary Magnetic Field', self.openMad.getMadCatMetadata(1)) 
        
    def test_getInstTypeMetadata(self):
        self.assertIn('Individual Ground Based Satellite Receivers', self.openMad.getInstTypeMetadata(1))
        
    def test_getLatestSubversion(self):
        self.assertIn('Millstone', self.openMad.getLatestSubversionVersion('madroot/metadata/siteTab.txt'))
        
    def test_getAllRevisionNumbers(self):
        self.assertIn('95d7ee6f03d1f43e06bccd0cb4b25dd42241495a', self.openMad.getAllRevisionNumbers('madroot/metadata/siteTab.txt'))
        
    def test_getSubversionVersion(self):
        self.assertIn('Millstone', self.openMad.getSubversionVersion('madroot/metadata/siteTab.txt', '95d7ee6f03d1f43e06bccd0cb4b25dd42241495a')) 
        
        
class TestWeb(unittest.TestCase):
    """TestWeb performs unit tests on madrigal.ui.web.MadrigalWeb
    """
    
    def setUp(self):
        self.madDB = madrigal.metadata.MadrigalDB()
        self.madWeb = madrigal.ui.web.MadrigalWeb(self.madDB)
        
    def test_getRulesOfTheRoad(self):
        self.assertIn('before using this data in a report or publication', self.madWeb.getRulesOfTheRoad())
        
    def test_isTrusted(self):
        self.assertIn(self.madWeb.isTrusted(), (0,1))
        
    def test_logDataAccess(self):
        # get todays year
        now = datetime.datetime.now()
        thisYear = '%04i' % (now.year)
        logFile = os.path.join(self.madDB.getMadroot(), 'metadata/userdata', 'access_%s.log' % (thisYear))
        originalLogSize = os.path.getsize(logFile)
        fullFilename = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        self.madWeb.logDataAccess(fullFilename, 'Bill Rideout', 'brideout@mit.edu', 'MIT')
        self.assertGreater(os.path.getsize(logFile), originalLogSize)
        
    def test_filterLog(self):
        tmpFile = '/tmp/junkFilter.log'
        self.madWeb.filterLog(tmpFile, [30])
        logSize = os.path.getsize(tmpFile)
        os.remove(tmpFile)
        self.assertGreater(logSize, 0)
        
    def test_createGlobalIsprintCmd(self):
        # set up arguments
        languages = ('python', 'Matlab', 'IDL') # run all three since different code
        madrigalUrl = self.madDB.getTopLevelUrl()
        parmList = ['Ti', 'Te', 'UT1_unix']
        output = '/tmp/junk'
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        start_datetime = datetime.datetime(1998,1,1)
        end_datetime = datetime.datetime(1998,2,1)
        instCode = 30
        filterList = ['filter=te,,2000']
        kindatList = [13300]
        expName = '*world*'
        fileDesc = '*final*'
        seasonalStartDate = '01/01'
        seasonalEndDate = '04/30'
        format = 'netCDF4'
        for language in languages:
            self.assertIn('brideout@mit.edu', self.madWeb.createGlobalIsprintCmd(language, madrigalUrl, parmList, 
                                                                                 output, user_fullname, user_email, 
                                                                                 user_affiliation, start_datetime, 
                                                                                 end_datetime, instCode, filterList, 
                                                                                 kindatList, expName, fileDesc, 
                                                                                 seasonalStartDate, seasonalEndDate,
                                                                                 format))
            
            
    def test_createGlobalDownloadCmd(self):
        # set up arguments
        languages = ('python', 'Matlab', 'IDL') # run all three since different code
        madrigalUrl = self.madDB.getTopLevelUrl()
        output = '/tmp'
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        start_datetime = datetime.datetime(1998,1,1)
        end_datetime = datetime.datetime(1998,2,1)
        instCode = 30
        kindatList = [13300]
        expName = '*world*'
        fileDesc = '*final*'
        format = 'netCDF4'
        for language in languages:
            self.assertIn('brideout@mit.edu', self.madWeb.createGlobalDownloadCmd(language, madrigalUrl, 
                                                                                 output, format, user_fullname, user_email, 
                                                                                 user_affiliation, start_datetime, 
                                                                                 end_datetime, instCode, 
                                                                                 kindatList, expName, fileDesc))
            
            
            
    def test_generateGlobalIsprintScriptFromForm(self):
        form1 = {'instruments':30,
                 'start_date': datetime.datetime(1998,1,1),
                 'end_date': datetime.datetime(1998,3,1),
                 'format_select': 'ascii',
                 'directory_select': 'File',
                 'language_select': 'Matlab',
                 'kindat_select': [13300],
                 'expName': '*world*',
                 'fileDesc': '*final*',
                 'seasonalStartDay': 1,
                 'seasonalStartMonth': 1,
                 'seasonalEndDay': 30, 
                 'seasonalEndMonth': 4}
        form2 = {'parameters': ['Ti', 'Te', 'UT1_unix']}
        form3 = {'parm_1': 'Ti',
                 'parm_1_lower': 100,
                 'parm_1_upper': 2000}
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        self.assertIn('brideout@mit.edu', self.madWeb.generateGlobalIsprintScriptFromForm(form1, form2, form3, 
                                                                                          user_fullname,
                                                                                          user_email, user_affiliation))
        
        
    def test_generateDownloadFileScriptFromForm(self):
        form1 = {'instruments':30,
                 'start_date': datetime.datetime(1998,1,1),
                 'end_date': datetime.datetime(1998,3,1),
                 'format_select': 'ascii',
                 'language_select': 'Matlab',
                 'kindat_select': [13300],
                 'expName': '*world*',
                 'fileDesc': '*final*'}
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        self.assertIn('brideout@mit.edu', self.madWeb.generateDownloadFileScriptFromForm(form1, user_fullname,
                                                                                         user_email, user_affiliation))
        
        
    def test_getSingleRedirectList(self):
        self.assertIn('https://cedar.openmadrigal.org/', str(self.madWeb.getSingleRedirectList()))
        
    def test_getMonths(self):
        self.assertIn('January', str(self.madWeb.getMonths(30, 1998)))
        
    def test_getDays(self):
        self.assertIn(datetime.date(1998,1,20), self.madWeb.getDays(30, 1998, 1))
        
    def test_getExperimentList(self):
        self.assertIn('World Day - Storm El Scan', str(self.madWeb.getExperimentList([30], datetime.datetime(1998,1,1),
                                                                                 datetime.datetime(1998,2,1), True)))
        
    def test_getExpsOnDate(self):
        self.assertIn('World Day - Storm El Scan', str(self.madWeb.getExpsOnDate(30, 1998, 1, 20)))
        
    def test_getFileFromExpDir(self):
        expDir = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98')
        self.assertIn('mlh980120g.002.hdf5', str(self.madWeb.getFileFromExpDir(expDir, 30)))
        
    def test_getExpInfoFromExpID(self):
        madExp = madrigal.metadata.MadrigalExperiment(self.madDB, os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/expTab.txt'))
        expID = None
        for i in range(madExp.getExpCount()):
            if madExp.getKinstByPosition(i) != 30:
                continue
            sDT = madExp.getExpStartDateTimeByPosition(i)
            if sDT[0] != 1998 or sDT[1] != 1 or sDT[2] != 20:
                continue
            expID = madExp.getExpIdByPosition(i)
            break
        self.assertIn('experiment_list=experiments/1998/mlh/20jan98', str(self.madWeb.getExpInfoFromExpID(expID)))
        
    
    def test_getExpIDFromExpPath(self):
        madExp = madrigal.metadata.MadrigalExperiment(self.madDB, os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/expTab.txt'))
        expID = None
        for i in range(madExp.getExpCount()):
            if madExp.getKinstByPosition(i) != 30:
                continue
            sDT = madExp.getExpStartDateTimeByPosition(i)
            if sDT[0] != 1998 or sDT[1] != 1 or sDT[2] != 20:
                continue
            expID = madExp.getExpIdByPosition(i)
            break
        self.assertEqual(expID, self.madWeb.getExpIDFromExpPath('experiments/1998/mlh/20jan98'))
        
    def test_getInfoFromFile(self):
        filePath = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        self.assertIn('World Day - Storm El Scan', str(self.madWeb.getInfoFromFile(filePath)))
        
    def test_getFileFromExpID(self):
        madExp = madrigal.metadata.MadrigalExperiment(self.madDB, os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/expTab.txt'))
        expID = None
        for i in range(madExp.getExpCount()):
            if madExp.getKinstByPosition(i) != 30:
                continue
            sDT = madExp.getExpStartDateTimeByPosition(i)
            if sDT[0] != 1998 or sDT[1] != 1 or sDT[2] != 20:
                continue
            expID = madExp.getExpIdByPosition(i)
            break
        self.assertIn('mlh980120g.002.hdf5', str(self.madWeb.getFileFromExpID(expID)))
        
    def test_getSiteInfo(self):
        self.assertIn('https://cedar.openmadrigal.org', str(self.madWeb.getSiteInfo()))
        
    def test_downloadFileAsIs(self):
        madExp = madrigal.metadata.MadrigalExperiment(self.madDB)
        expID = None
        for i in range(madExp.getExpCount()):
            if madExp.getKinstByPosition(i) != 30:
                continue
            sDT = madExp.getExpStartDateTimeByPosition(i)
            if sDT[0] != 1998 or sDT[1] != 1 or sDT[2] != 20:
                continue
            expID = madExp.getExpIdByPosition(i)
            break
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        self.assertIn('mlh980120g.002.hdf5', str(self.madWeb.downloadFileAsIs(expID, 'mlh980120g.002.hdf5', user_fullname, 
                                                                              user_email, user_affiliation)))
        
    def test_downloadFullFileAsIs(self):
        filePath = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        self.assertIn('mlh980120g.002.hdf5', str(self.madWeb.downloadFullFileAsIs(filePath, 'hdf5',  user_fullname, 
                                                                                  user_email, user_affiliation)))
        
    def test_downloadMultipleFiles(self):
        filePath = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        self.assertIn('madrigalFiles_', self.madWeb.downloadMultipleFiles([filePath], 'hdf5',  user_fullname,
                                                                           user_email, user_affiliation))
        
    def test_printFileAsIs(self):
        filePath = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        testStr = os.path.join(self.madDB.getMadroot(), 'experiments/stage')
        self.assertIn(testStr, self.madWeb.printFileAsIs(filePath, user_fullname, 
                                                         user_email, user_affiliation))
        
    def test_listRecords(self):
        filePath = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        self.assertIn('0: 1998-01-20', self.madWeb.listRecords(filePath))
        
    def test_downloadIsprintFileFromIsprintForm(self):
        filePath = os.path.join(self.madDB.getMadroot(), 'experiments/1998/mlh/20jan98/mlh980120g.002.hdf5')
        user_fullname = 'Bill Rideout'
        user_email = 'brideout@mit.edu'
        user_affiliation = 'MIT'
        isprintForm = {'fullFilename': filePath,
                       'parameters': ['Ti', 'Te', 'UT1_unix'],
                       'start_date': datetime.datetime(1998,1,20,20),
                       'end_date': datetime.datetime(1998,1,20,20,15),
                       'formats': 'netCDF4',
                       'parm_1': 'Ti',
                       'parm_1_lower': '100',
                       'parm_1_upper': '2000',
                       'parm_2': 'None',
                       'parm_2_lower': '',
                       'parm_2_upper': '',
                       'parm_3': 'None',
                       'parm_3_lower': '',
                       'parm_3_upper': ''}
        testStr = os.path.join(self.madDB.getMadroot(), 'experiments/stage')
        self.assertIn(testStr, self.madWeb.downloadIsprintFileFromIsprintForm(isprintForm, user_fullname, 
                                                                              user_email, user_affiliation))
        
    def test_runMadrigalCalculatorFromForm(self):
        calcForm = {'parameters': ['mlt', 'sdwht'],
                    'min_latitude': 20,
                    'max_latitude': 30,
                    'delta_latitude': 5,
                    'min_longitude': 20,
                    'max_longitude': 30,
                    'delta_longitude': 5,
                    'min_altitude': 100,
                    'max_altitude': 200,
                    'delta_altitude': 50,
                    'datetime': datetime.datetime(2010,3,19,12)}
        self.assertIn('30.00      30.00     100.00', self.madWeb.runMadrigalCalculatorFromForm(calcForm))
        
    def test_runLookerFromForm1(self):
        lookerForm = {'looker_options': 1,
                      'instruments': 30,
                      'start_lat': 20,
                      'stop_lat': 40,
                      'step_lat': 5,
                      'start_lon': 20,
                      'stop_lon': 40,
                      'step_lon': 5,
                      'start_alt': 100,
                      'stop_alt': 200,
                      'step_alt': 50,
                      }
        self.assertIn('35.00   35.00  200.00', self.madWeb.runLookerFromForm(lookerForm))
        
    def test_runLookerFromForm3(self):
        lookerForm = {'looker_options': 3,
                      'year': 2000,
                      'start_lat': 20,
                      'stop_lat': 40,
                      'step_lat': 5,
                      'start_lon': 20,
                      'stop_lon': 40,
                      'step_lon': 5,
                      'start_alt': 100,
                      'stop_alt': 200,
                      'step_alt': 50,
                      }
        self.assertIn('35.00   40.00  150.00', self.madWeb.runLookerFromForm(lookerForm))
        
    def test_runLookerFromForm4(self):
        lookerForm = {'looker_options': 4,
                      'instruments': 30,
                      'start_az': 20,
                      'stop_az': 40,
                      'step_az': 5,
                      'start_el': 20,
                      'stop_el': 40,
                      'step_el': 5,
                      'start_range': 100,
                      'stop_range': 200,
                      'step_range': 50,
                      }
        self.assertIn('35.00   40.00  100.00', self.madWeb.runLookerFromForm(lookerForm))
        
    def test_runLookerFromForm5(self):
        lookerForm = {'looker_options': 5,
                      'instruments': 30,
                      'year': 2000,
                      'fl_az': 20,
                      'fl_el': 20,
                      'fl_range': 100,
                      'start_alt': 100,
                      'stop_alt': 200,
                      'step_alt': 50,
                      }
        self.assertIn('43.', self.madWeb.runLookerFromForm(lookerForm))
        
    def test_runLookerFromForm8(self):
        lookerForm = {'looker_options': 8,
                      'datetime': datetime.datetime(2010,3,19,12),
                      'start_lat': 20,
                      'stop_lat': 40,
                      'step_lat': 5,
                      'start_lon': 20,
                      'stop_lon': 40,
                      'step_lon': 5,
                      'start_alt': 100,
                      'stop_alt': 200,
                      'step_alt': 50,
                      'gdlat': 40,
                      'glon': -70,
                      'gdalt': 500,
                      'pList': ['SZEN']
                      }
        self.assertIn('35.00      30.00     100.00      44.11', self.madWeb.runLookerFromForm(lookerForm))
        
    def test_cleanStage(self):
        self.madWeb.cleanStage()
        
    def test_modifyBasename(self):
        self.assertEqual('mlh980120g_002.hdf5', self.madWeb.modifyBasename('mlh980120g_001.hdf5'))
        self.assertEqual('mlh980120g_1.hdf5', self.madWeb.modifyBasename('mlh980120g.hdf5'))
        self.assertEqual('mlh980120g_7.hdf5', self.madWeb.modifyBasename('mlh980120g_6.hdf5'))
        self.assertEqual('mlh980120g_10.hdf5', self.madWeb.modifyBasename('mlh980120g_9.hdf5'))
        
    def test_globalFileSearch(self):
        # set up arguments
        url = 'https://w3id.org/cedar?experiment_list=experiments/1998/mlh/20jan98&file_list=mlh980120g.002.hdf5'
        start_datetime = datetime.datetime(1998,1,1)
        end_datetime = datetime.datetime(1998,2,1)
        instCode = 30
        kindatList = ['*combined*']
        expName = '*world*'
        fileDesc = '*final*'
        result = self.madWeb.global_file_search(start_datetime, end_datetime, instCode,
                                                kindatList, seasonalStartDate='01/10',
                                                seasonalEndDate='02/12', expName=expName,
                                                fileDesc=fileDesc, returnCitation=True)
        self.assertIn(url, result)
        
 
    
    
testCaseList = (TestMadrigalDB,
                TestMadrigalSite,
                TestMadrigalInstrument,
                TestMadrigalInstrumentParameters,
                TestMadrigalKindat,
                TestMadrigalInstrumentKindats,
                TestMadrigalExperiment,
                TestMadrigalMetaFile,
                TestMadrigalParmCategory,
                TestMadrigalInstrumentData,
                TestMadrigalFile,
                TestMadrigalParameters,
                TestOpenMadrigal,
                TestWeb)
        
suite = unittest.TestSuite()
for testCase in testCaseList:
    tests = unittest.TestLoader().loadTestsFromTestCase(testCase)
    suite.addTests(tests)
unittest.TextTestRunner(verbosity=2).run(suite)