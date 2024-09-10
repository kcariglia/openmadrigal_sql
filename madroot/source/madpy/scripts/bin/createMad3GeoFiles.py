"""createMad3GeoUpdate.py is a script to create Madrigal 3 geophysical files from Madrigal 2 ones

$Id: createMad3GeoFiles.py 7045 2019-10-07 19:56:46Z brideout $
"""
import os, os.path, sys
import datetime
import shutil
import subprocess

# for now, uses old versions

old_dir = '/usr/local/apache2/htdocs/madrigal/distributionFiles'
mad3_dir = '/usr/local/apache2/htdocs/madrigal/distributionFiles/mad3'

fileTabDict = {1950: 'echo "geo500101g.002,10000002,30007,1,0,0,0,0,0,0,0" > /tmp/mad3/experiments/1950/gpi/01jan50/fileTab.txt',
               1957: 'echo "dst570101g.002,10000001,30006,1,0,0,0,0,0,0,0" > /tmp/mad3/experiments/1957/dst/01jan57/fileTab.txt',
               1963: 'echo "imf631127g.002,10000001,30012,1,0,0,0,0,0,0,0" > /tmp/mad3/experiments/1963/imf/27nov63/fileTab.txt'}

cmdDict = {1950: '/opt/madrigal3/bin/exportToHdf.py --cedarFilename=/tmp/mad3/experiments/1950/gpi/01jan50/geo500101g.002 --hdf5Filename=/opt/madrigal3/experiments/1950/gpi/01jan50/geo500101g.002.hdf5',
           1957: '/opt/madrigal3/bin/exportToHdf.py --cedarFilename=/tmp/mad3/experiments/1957/dst/01jan57/dst570101g.002 --hdf5Filename=/opt/madrigal3/experiments/1957/dst/01jan57/dst570101g.002.hdf5',
           1963: '/opt/madrigal3/bin/exportToHdf.py --cedarFilename=/tmp/mad3/experiments/1963/imf/27nov63/imf631127g.002 --hdf5Filename=/opt/madrigal3/experiments/1963/imf/27nov63/imf631127g.002.hdf5'}
    
        
def parse_status_file(fileDir):
    """parse_status_file parses the file status.dat in fileDir, and returns the
    following list of dates:
    
        last_update
        last_kp
        last_f107
        last_dst
        last_imf
    """
    f = open(fileDir + '/status.dat')
    lines = f.readlines()
    for line in lines:
        items = line.split()
        if line.find('last_update') != -1:
            last_update = datetime.datetime(int(items[1]),
                                            int(items[2]),
                                            int(items[3]))
        elif line.find('last_kp') != -1:
            last_kp = datetime.datetime(int(items[1]),
                                        int(items[2]),
                                        int(items[3]))
        elif line.find('last_f107') != -1:
            last_f107 = datetime.datetime(int(items[1]),
                                         int(items[2]),
                                         int(items[3]))
        elif line.find('last_dst') != -1:
            last_dst = datetime.datetime(int(items[1]),
                                         int(items[2]),
                                         int(items[3]))
        elif line.find('last_imf') != -1:
            last_imf = datetime.datetime(int(items[1]),
                                         int(items[2]),
                                         int(items[3]))

    return (last_update, last_kp, last_f107, last_dst, last_imf)


last_update, last_kp, last_f107, last_dst, last_imf = parse_status_file(old_dir)
last_mad3_update, last_mad3_kp, last_mad3_f107, last_mad3_dst, last_mad3_imf = parse_status_file(mad3_dir)

if last_update <= last_mad3_update:
    print('no action needed')
    sys.exit(0)
    
print('updating mad3 geophysical files')
tmpDir = '/tmp/mad3'
try:
    os.makedir(tmpDir)
except:
    pass
shutil.copy(os.path.join(old_dir, 'geofil.tar.gz'), tmpDir)
cmd = 'tar -xzf %s' % (os.path.join(tmpDir, 'geofil.tar.gz'))
print(('about to run <%s>' % (cmd)))
subprocess.check_call(cmd.split())
    
# update all three files
for year in (1950, 1957, 1963):
    print(('working on %i' % (year)))
    os.system(fileTabDict[year])
    print(('about to run <%s>' % (cmdDict[year])))
    subprocess.check_call(cmdDict[year].split())
    
os.chdir('/opt/madrigal3/experiments')

outputFile = os.path.join(mad3_dir, 'geofil.tar')
cmd = 'tar -cf %s 1950/gpi 1957/dst 1963/imf' % (outputFile)
subprocess.check_call(cmd.split())
os.system('gzip %s' % (outputFile))
os.system('cp /usr/local/apache2/htdocs/madrigal/distributionFiles/status.dat /usr/local/apache2/htdocs/madrigal/distributionFiles/mad3/')

os.system('rm -rf %s' % (tmpDir))
