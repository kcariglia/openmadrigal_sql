"""testWebServices.py makes calls to the web services bin scripts:

Now tests:
    madCalculator2.py
    madCalculator3.py
    
$Id: testWebServices.py 7239 2020-10-02 19:25:23Z brideout $
"""

import os, os.path, sys
import subprocess
import urllib.request, urllib.error, urllib.parse

import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()

madroot = madDB.getMadroot()

pythonExe = os.path.join(madroot, 'bin/python')

cgiUrl = madDB.getTopLevelUrl()


# test madCalculator2Service
url = os.path.join(cgiUrl, 'madCalculator2Service.py')
url += '?year=2001&month=3&day=19'
url += '&hour=12&min=30&sec=20'
url += '&lats=45,46,47,48.5'
url += '&longs=-70,-71,-72,-73'
url += '&alts=145,200,250,300.5'
url += '&parms=bmag,pdcon,ne_model'
url += '&oneD=kinst,31.0&oneD=elm,45.0'
url += '&twoD=ti,1000,1000,1000,1000'
url += '&twoD=te,1100,1200,1300,1400'
url += '&twoD=ne,1.0e10,1.0e10,1.0e10,1.0e10'

print('\nmadCalculator2Service.py url:')
print(('%s\n' % (url)))

f = urllib.request.urlopen(url)
text = f.read()
if type(text) == bytes:
    text = text.decode('utf-8')
print(text)
f.close()

### madCalculator3.py

url = os.path.join(cgiUrl, 'madCalculator3Service.py')
url += '?year=2001,2001&month=3,3&day=19,20'
url += '&hour=12,12&min=30,40&sec=20,0'
url += '&numPos=4,5&lats=45,46,47,48.5,46,47,48.2,49,50'
url += '&longs=-70,-71,-72,-73,-70,-71,-72,-73,-74'
url += '&alts=145,200,250,300.5,200,250,300,350,400'
url += '&parms=bmag,pdcon,ne_model'
url += '&oneD=kinst,31.0,31.0&oneD=elm,45.0,50.0'
url += '&twoD=ti,1000,1000,1000,1000,1000,1000,1000,1000,1000'
url += '&twoD=te,1100,1200,1300,1400,1500,1000,1100,1200,1300'
url += '&twoD=ne,1.0e10,1.0e10,1.0e10,1.0e10,1.0e10,1.0e10,1.0e10,1.0e10,1.0e10'

print('\nmadCalculator3Service.py url:')
print(('%s\n' % (url)))

f = urllib.request.urlopen(url)
text = f.read()
if type(text) == bytes:
    text = text.decode('utf-8')
print(text)
f.close()

### globalFileSearchService.py

url = os.path.join(cgiUrl, 'globalFileSearchService.py')
url += '?startDate=1998-01-01&endDate=1998-01-31&inst=Millstone*&kindat=Combined*&expName=world*&seasonalEndDate=02/15&fileDesc=final&returnCitation=True'

print('\globalFileSearchService.py url:')
print(('%s\n' % (url)))

f = urllib.request.urlopen(url)
text = f.read()
if type(text) == bytes:
    text = text.decode('utf-8')
print(text)
f.close()



