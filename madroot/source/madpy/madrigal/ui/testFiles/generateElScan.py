"""generateElScan.py generates some false el scans for testing
"""

import os, os.path, sys
import copy
import traceback

import madrigalWeb.madrigalWeb

slatgd = 42.0
slon = -71.0
saltgd = 0.3

az = 90.0

madWeb = madrigalWeb.madrigalWeb.MadrigalData('http://localhost/madrigal')

f = open('fake.txt', 'w')

parms = 'gdlatr,gdlonr,galtr,range,az1,az2,el1,el2,nel_iri'
oneDParmList=['kinst', 'az1', 'az2', 'el1', 'el2']
oneDParmValues = [31, az, az]
year = 2004
month = 1
day = 1
hour = 1
minute = 1
second = 1

for el in [10,20,30,40,50,60,70]:
    latList = []
    lonList = []
    altList = []
    oneDParmValues2 = copy.copy(oneDParmValues)
    oneDParmValues2.append(el - 5.0)
    oneDParmValues2.append(el + 5.0)
    for thisRange in range(100,700,50):
        result = madWeb.radarToGeodetic(slatgd, slon, saltgd, az, el, thisRange)
        lat = result[0][0]
        lon = result[0][1]
        alt = result[0][2]
        latList.append(lat)
        lonList.append(lon)
        altList.append(alt)
    data = madWeb.madCalculator2(year, month, day, hour, minute, second, latList, lonList, altList, parms, oneDParmList, oneDParmValues2)
    for line in data:
        try:
            f.write('%s %s %s %s %s %s %s %s %s \n' % tuple(line[3:]))
        except:
            traceback.print_exc()
            print((tuple(line[3:])))
    
f.close()