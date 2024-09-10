#!PYTHONEXE

"""madCalculator3.py is a script run to return a text output of derived Madrigal parameters for series of times and a random array of gdlat, glon, and gdalt points.

It is similar to madCalculator2.py, but it allows multiple times to be requested at once, where each time
has an independent array of points.  It was written to improve performance in a case where madCalculator2
had to be called many times for many different times.  Only the requested parameters may not change from one
time to the next.

It is presently used by the cgi script
"madCalculator3Service.py":../services/madCalculator3Service.py.html,

It has the following input arguments:

     --date=<MM/DD/YYYY,MM/DD/YYYY,...> (comma-separated list of dates - required)
                              Example with 2 times:  --date=03/19/2001,03/20/2001

     --time=<HH:MM:SS,HH:MM:ss,...>  (comma separated list of times - len must equal date list - required)
                              Example with 2 times:  --time=12:30:20,12:40:00
                              
     --numPos=<number of position list> (comma separated list of number of spatial positions.
                                         len must equal date list - required)
                                         Example with 2 times:  --numPos=4,5

     --lats=<latitude array>  string with comma delimited latitudes.
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              -90 to 90. (required).
                              Example with 2 times and numPos=4,5: --lats=45,46,47,48.5,46,47,48.2,49,50,51

     --longs=<longitude array>  string with comma delimited longitudes.
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              -180 to 180. (required).
                              Example with 2 times and numPos=4,5: --longs=145,146,147,148.5,146,147,148.2,149,150,151

     --alts=<altitude array>  string with comma delimited altitudes.
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              >= 0  (required)
                              Example with 2 times and numPos=4,5: --alts=2145,2146,2147,2148.5,2146,2147,2148.2,2149,2150,2151

     --parms=<comma delimited string of Madrigal parameters desired> (required)
                              Example: --parms=bmag,pdcon

     --showHeader (optional)  Prints header line before data

     --oneD=<parm>,<value array>  (optional - 0 or more allowed) This argument allows the user to
                                  set any number of one-D parameters to be used in the calculation.
                                  Value must be parameter name, comma, list of comma-separated values as double.
                                  len must equal date list
                                  Example with two times:  --oneD=kinst,31.0,31.0  --oneD=elm,45.0,50.0
                            
     --twoD=<parm>,<values>  (optional - 0 or more allowed) This argument allows the user to
                             set any number of two-D parameters to be used in the calculation.
                              Value must be parameter name, comma, comma-separated values 
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              Example 2 times and numPos=4,5:  --twoD=te;1000,1100,1200,1000,1000,1100,1200,1000,1200  

Returns comma-delimited data, one line for each location.  Separate times are delimited by line

Time YYYY-MM-DD HH:MM:SS


Data lines have the following fields:

1. latitude
2. longitude
3. altitude
4. Values for each Madrigal parameter listed in argument parms, separated by whitespace

If --showHeader given, also prints header line before data

This script, and the corresponding cgi script
"madCalculator3Service.py":../services/madCalculator3Service.py.html are available to any scripting
language that wants to access Madrigal derived parameters.

$Id: madCalculator3.py 7046 2019-10-07 19:57:14Z brideout $
"""

import sys
import os
import time
import traceback
import getopt

def isnan( x ):
    rx = repr(float(x))
    if rx.lower().find('nan') != -1:
        return 1
    else:
        return 0

usage = """madCalculator3.py is a script run to return a text output of derived Madrigal parameters for a time and a collection of gdlat, glon, and gdalt values.

It has the following input arguments:

     --date=<MM/DD/YYYY,MM/DD/YYYY,...> (comma-separated list of dates - required)
                              Example with 2 times:  --date=03/19/2001,03/20/2001

     --time=<HH:MM:SS,HH:MM:ss,...>  (comma separated list of times - len must equal date list - required)
                              Example with 2 times:  --time=12:30:20,12:40:00
                              
     --numPos=<number of position list> (comma separated list of number of spatial positions.
                                         len must equal date list - required)
                                         Example with 2 times:  --numPos=4,5

     --lats=<latitude array>  string with comma delimited latitudes.
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              -90 to 90. (required).
                              Example with 2 times and numPos=4,5: --lats=45,46,47,48.5,46,47,48.2,49,50,51

     --longs=<longitude array>  string with comma delimited longitudes.
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              -180 to 180. (required).
                              Example with 2 times and numPos=4,5: --longs=145,146,147,148.5,146,147,148.2,149,150,151

     --alts=<altitude array>  string with comma delimited altitudes.
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              >= 0  (required)
                              Example with 2 times and numPos=4,5: --alts=2145,2146,2147,2148.5,2146,2147,2148.2,2149,2150,2151

     --parms=<comma delimited string of Madrigal parameters desired> (required)
                              Example: --parms=bmag,pdcon

     --showHeader (optional)  Prints header line before data

     --oneD=<parm>,<value array>  (optional - 0 or more allowed) This argument allows the user to
                                  set any number of one-D parameters to be used in the calculation.
                                  Value must be parameter name, comma, list of comma-separated values as double.
                                  len must equal date list
                                  Example with two times:  --oneD=kinst,31.0,31.0  --oneD=elm,45.0,50.0
                            
     --twoD=<parm>,<values>  (optional - 0 or more allowed) This argument allows the user to
                             set any number of two-D parameters to be used in the calculation.
                              Value must be parameter name, comma, comma-separated values 
                              Order is all points from first time, then second time, etc.
                              Total must be sum of numPos
                              Example 2 times and numPos=4,5:  --twoD=te;1000,1100,1200,1000,1000,1100,1200,1000,1200  

Returns comma-delimited data, one line for each location.  Separate times are delimited by line

Time YYYY-MM-DD HH:MM:SS


Data lines have the following fields:

1. latitude
2. longitude
3. altitude
4. Values for each Madrigal parameter listed in argument parms, separated by whitespace

If --showHeader given, also prints header line before data
"""


# catch any exception, and write an appropriate message admin
try:
    # check if pythonlibpath env variable exists
    # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
    temp = os.environ.get('PYTHON' + 'LIBPATH')
    if temp != None:
        sys.path.append(temp)
        
    # append path madroot/lib (needed only if python not installed by setup)
    sys.path.append('MADROOT/lib/python')

    # prepare to handle MadrigalError
    from madrigal.admin import *

except ImportError:
    
    # Fatal error - madpy library not found
    print("Unable to import the madrigal python library - please alert the sys admin!")
    sys.exit(-1)

# try to run script, and report all errors to Madrigal sys admin, and also print
try:

    import madrigal.ui.report
    import madrigal.data

    parmObj = madrigal.data.MadrigalParameters()

    # parse command line
    arglist = ''
    longarglist = ['date=',
                   'time=',
                   'numPos=',
                   'lats=',
                   'longs=',
                   'alts=',
                   'parms=',
                   'showHeader',
                   'oneD=',
                   'twoD=']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    dates = []
    numPos = None
    times = []
    latList = []
    longList = []
    altList = []
    parms = None
    showHeader = 0
    oneDParmList = []
    oneDParmValues = []
    twoDParmList = []
    twoDParmValues = []
    
    # we must find numPos before we can parse anything else, so the first time we just look for numPos
    for opt in optlist:
        if opt[0] == '--numPos':
            totalPos = 0
            totalTimes = 0
            items = opt[1].split(',')
            numPos = []
            for item in items:
                totalTimes += 1
                numPos.append(int(item))
                totalPos += int(item)
    
    if numPos == None:
        print(usage)
        raise ValueError('--numPos argument required')

    # now parse all the rest
    for opt in optlist:
        if opt[0] == '--date':
            items = opt[1].split(',')
            if len(items) != totalTimes:
                raise ValueError('Number of dates %i must equal len of numPos list %i' % (len(items), totalTimes))
            for item in items:
                dates.append(item)
            if len(dates) != len(numPos):
                raise ValueError('len(dates)  %i != len(numPos) %i' % (len(dates), len(numPos)))
            
        elif opt[0] == '--time':
            items = opt[1].split(',')
            if len(items) != totalTimes:
                raise ValueError('Number of times %i must equal len of numPos list %i' % (len(items), totalTimes))
            for item in items:
                times.append(item)
            if len(times) != len(numPos):
                raise ValueError('len(times)  %i != len(numPos) %i' % (len(times), len(numPos)))
            
        elif opt[0] == '--numPos':
            continue
        
        elif opt[0] == '--lats':
            items = opt[1].split(',')
            if len(items) != totalPos:
                raise ValueError('Number of lats %i must equal total numPos %i' % (len(items), totalPos))
            index = 0
            for pos in numPos:
                latList.append([])
                for i in range(pos):
                    value = float(items[index])
                    latList[-1].append(value)
                    if value < -90.001 or value > 90.001:
                        raise ValueError('Illegal latitude %f' % (value))
                    index += 1

        elif opt[0] == '--longs':
            items = opt[1].split(',')
            if len(items) != totalPos:
                raise ValueError('Number of longs %i must equal total numPos %i' % (len(items), totalPos))
            index = 0
            for pos in numPos:
                longList.append([])
                for i in range(pos):
                    value = float(items[index])
                    longList[-1].append(value)
                    if longList[-1][-1] > 180.0:
                        longList[-1][-1] -= 360.0
                    if longList[-1][-1] < -180.001 or longList[-1][-1] > 180.001:
                        raise ValueError('Illegal longitude %f' % (longList[-1][-1]))
                    index += 1

        elif opt[0] == '--alts':
            items = opt[1].split(',')
            if len(items) != totalPos:
                raise ValueError('Number of alts %i must equal total numPos %i' % (len(items), totalPos))
            index = 0
            for pos in numPos:
                altList.append([])
                for i in range(pos):
                    value = float(items[index])
                    altList[-1].append(value)
                    if value < -0.001:
                        raise ValueError('Illegal altitude %f' % (value))
                    index += 1
                    
        elif opt[0] == '--parms':
            parms = opt[1]
            
        elif opt[0] == '--showHeader':
            showHeader = 1
            
        elif opt[0] == '--oneD':
            items = opt[1].split(',')
            if len(items) - 1 != totalTimes:
                raise ValueError('Number of oneD %i must equal len of numPos list %i' % (len(items)-1, totalTimes))
            index = 0
            initialize1D = False
            if len(oneDParmList) == 0:
                # need to populated it with one list for each time
                initialize1D = True
            oneDParmList.append(parmObj.getParmMnemonic(items[0]))
            if initialize1D:
                for i in range(totalTimes):
                    oneDParmValues.append([])
            for i, item in enumerate(items[1:]):
                oneDParmValues[i].append(float(item))
            
        elif opt[0] == '--twoD':
            items = opt[1].split(',')
            if len(items) - 1 != totalPos:
                raise ValueError('Number of twoD %i must equal total numPos %i' % (len(items)-1, totalPos))
            initialize2D = False
            if len(twoDParmList) == 0:
                # need to populated it with one list for each time
                initialize2D = True
            twoDParmList.append(parmObj.getParmMnemonic(items[0]))
            if initialize2D:
                for i in range(totalTimes):
                    twoDParmValues.append([])
            # add a new list for this 2D parameter
            for i in range(totalTimes):
                twoDParmValues[i].append([])
            # now add the actual values
            index = 1
            for i, pos in enumerate(numPos):
                for j in range(pos):
                    twoDParmValues[i][-1].append(float(items[index]))
                    index += 1
                
    
        else:
            raise ValueError('Illegal option %s\n%s' % (opt[0], usage))
        
    # make sure oneDParmList and twoDParmList got initialized
    if len(oneDParmList) == 0:
        for i in range(totalTimes):
            oneDParmValues.append([])
            
    if len(twoDParmList) == 0:
        for i in range(totalTimes):
            twoDParmValues.append([])

    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # verify input values
    if len(dates) == 0:
        raise ValueError('Date argument --date required!\n%s' % (usage))
    
    # loop through each time
    for index in range(totalTimes):
        
        date = dates[index]
        timeStr = times[index]
        lats = latList[index]
        longs = longList[index]
        alts = altList[index]
        
        # print header line
        print(('TIME %s %s' % (date, timeStr)))

        dateFields = date.split('/')
        if len(dateFields) != 3:
            raise ValueError('Date argument must be in form MM/DD/YYYY: %s' % (date))
        month = int(dateFields[0])
        day = int(dateFields[1])
        year = int(dateFields[2])
    
        if month < 1 or month > 12:
            raise ValueError('Date argument must be in form MM/DD/YYYY: %s' % (date))
    
        if day < 1 or day > 31:
            raise ValueError('Date argument must be in form MM/DD/YYYY: %s' % (date))
    
        if timeStr != None:
    
            timeFields = timeStr.split(':')
            if len(timeFields) != 3:
                raise ValueError('time argument must be in form HH:MM:SS: %s' % (timeStr))
            hour = int(timeFields[0])
            min = int(timeFields[1])
            sec = int(timeFields[2])
    
            if hour < 0 or hour > 24:
                raise ValueError('Time argument must be in form HH:MM:SS: %s' % (timeStr))
    
            if min < 0 or min > 60:
                raise ValueError('Time argument must be in form HH:MM:SS: %s' % (timeStr))
    
            if sec < 0 or sec > 61:
                raise ValueError('Time argument must be in form HH:MM:SS: %s' % (timeStr))
    
        else:
            # set default times
            hour = 0
            min = 0
            sec = 0
        
    
        if parms == None:
            raise ValueError('parms argument required!\n%s' % (usage))
    
        reportObj = madrigal.ui.report.MadrigalReport(madDBObj)
    
        parmList = parms.split(',')
    
        reportObj.looker2(parmList,
                          lats,
                          longs,
                          alts,
                          year,
                          month,
                          day,
                          hour,
                          min,
                          sec,
                          showHeader,
                          oneDParmList,
                          oneDParmValues[index],
                          twoDParmList,
                          twoDParmValues[index])
    
        
    
        # if messStr not empty, send message to admin
        if len(messStr) > 0:
            # create MadrigalNotify object
            notifyObj = MadrigalNotify(madDBObj)
    
            notifyObj.sendAlert(messStr, 'Problem detected by madCalculator')


except MadrigalError as e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in madCalculator3.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    print(errStr)

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running madCalculator3.py' )

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]).find('exceptions.SystemExit') != -1:
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running madCalculator3.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    print(errStr)

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running madCalculator3.py')


# end script

