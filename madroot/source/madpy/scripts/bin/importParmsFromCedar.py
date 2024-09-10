import os,os.path,sys

"""importParmsFromCedar.py imports parameter information from Cedar file
parameters_list.txt into parcods.tab for a range of parameters.

Also checks uniqueness of mnemonics

$Id: importParmsFromCedar.py 7046 2019-10-07 19:57:14Z brideout $
"""

startRange = 3100
endRange = 3200

cedarFile = open('/home/jupiter/brideout/madroot/metadata/parameters_list.txt')
parFile = open('/home/jupiter/brideout/madroot/metadata/parcods.tab')

cedarLines = cedarFile.readlines()
parLines = parFile.readlines()

cedarFile.close()
parFile.close()

cedarDict = {} # key = code, value = desc
cedarMnemDict = {} # key = code, value = mnemonic
cedarScaleDict = {} # key = code, value = scale factor
cedarUnitsDict = {} # key = code, value = units string

parMnemList = [] # a list of all parameter mnemonic

for cedarLine in cedarLines:
    try:
        cedarCode = int(cedarLine[0:8])
    except:
        continue
    if not (cedarCode >= startRange and cedarCode < endRange):
        continue
    cedarDesc = cedarLine[9:48]
    cedarMnem = cedarLine[67:].strip().lower()
    cedarScale = cedarLine[51:57].lower()
    cedarUnits = cedarLine[58:67].strip()
    if cedarUnits == '':
        cedarUnits = 'N/A'
    cedarDict[cedarCode] = cedarDesc
    cedarMnemDict[cedarCode] = cedarMnem
    cedarScaleDict[cedarCode] = cedarScale
    cedarUnitsDict[cedarCode] = cedarUnits

# rewrite parcods.tab
f = open('/home/jupiter/brideout/madroot/metadata/parcods.tab.new', 'w')


for parLine in parLines:
    parCode = int(parLine[0:8])
    parMnem = parLine[81:101].strip().lower()
    if parMnem in parMnemList or 'd'+parMnem in parMnemList:
        raise ValueError('duplicate parameter mnem <%s>' % (parMnem))
    parMnemList.append(parMnem)
    parMnemList.append('d'+parMnem)
    
    if not (parCode >= startRange and parCode < endRange):
        print('skipping code %i' % (parCode))
        f.write(parLine)
        continue
    
    parDesc = parLine[10:49]

    # try to replace
    try:
        newParDesc = cedarDict[parCode]
        print('replacing desc <%s> with <%s>' % (parDesc, newParDesc))
        newParMnem = cedarMnemDict[parCode]
        newParmScale = cedarScaleDict[parCode]
        newParmUnits = cedarUnitsDict[parCode]
        if newParMnem != parMnem:
            if newParMnem in parMnemList or 'd'+newParMnem in parMnemList:
                raise ValueError('duplicate parameter mnem <%s>' % (newParMnem))
            parMnemList.append(newParMnem)
            parMnemList.append('d'+newParMnem)
            print('replacing mnem <%s> with <%s>' % (parMnem, newParMnem))
        
        f.write('%s%s%s%s%s%s%s%s' % (parLine[0:10],
                                      newParDesc,
                                      parLine[49:63],
                                      newParmScale,
                                      ' '*(9-len(newParmUnits)) + newParmUnits,
                                      parLine[78:81],
                                      newParMnem.upper() + ' '*(20-len(newParMnem)),
                                      parLine[101:]))
    except KeyError:
        f.write(parLine)

f.close()
        
