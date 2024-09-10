"""testRequirements.py is a python script that tests that all modules listed in required_modules.txt are installed.

If so, succeeds. If not, prints error and returns -1 return code

$Id: testRequirements.py 6167 2017-08-29 17:50:19Z brideout $
"""

import sys

python_version = sys.version_info.major
if python_version < 3:
    print('python 3 (or greater) required')
    sys.exit(-1)

f = open('required_modules.txt')
for line in f.readlines():
    if line[0] == '#':
        continue
    try:
        exec('import %s' % (line.strip()))
        result = True
    except:
        result = False
    if not result:
        print('You need to install the module %s on your default python before installing Madrigal.' % (line.strip()))
        sys.exit(-1)
        
# verify django at least version 
django_version = django.get_version()
items = django_version.split('.')
if int(items[0]) < 2:
    print('django must be at least 2.x, your version is %s' % (django_version))
    sys.exit(-1)
        
print('All required python modules found.')
sys.exit(0)