import os,sys

"""testParcods.py checks for duplicate mnemonics in parcods.py

Usage: python testParcods.py <parcods file>
"""

print 'Checking parcods.tab for duplicate mnemonics...'

f = open(sys.argv[1])
lines = f.readlines()
f.close()

mnemList = []

for line in lines:
    thisMnem = line[81:101]
    if thisMnem in mnemList:
        print 'Duplicate mnemonic: %s' % (thisMnem)
    else:
        mnemList.append(thisMnem)
