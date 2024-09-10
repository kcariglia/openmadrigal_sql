#!PYTHONEXE

"""madCalculator2.py is a script run to return a text output of derived Madrigal parameters for a time and a random array of gdlat, glon, and gdalt points.

It is similar to madCalculator.py, but it does not use a grid of lat, lon, and alts.  Instead, each input
point is independent.  It also allows input 2D parameters to be input, along with 1D.

It is presently used by the cgi script
"madCalculator2Service.py":../services/madCalculator2Service.py.html,

It has the following input arguments:

     --date=<MM/DD/YYYY> (required)

     --time=<HH:MM:SS>  (optional - if no HH:MM:SS given, 00:00:00 assumed)

     --lats=<latitude array>  string with comma delimited latitudes, -90 to 90 (required)

     --longs=<longitude array>  string with comma delimited longitudes, -180 to 180 (required).
            Length must equal --lat 

     --alts=<altitude>  string with comma delimited altitudes, >= 0 (required)
             Length must equal --lat 

     --parms=<comma delimited string of Madrigal parameters desired> (required)

     --showHeader (optional)  Prints header line before data

     --oneD=<parm>,<value>  (optional - 0 or more allowed) This argument allows the user to
                            set any number of one-D parameters to be used in the calculation.
                            Value must be parameter name, comma, value as double.
                            Example:  --oneD=kinst,31.0  --oneD=elm=45.0
                            
     --twoD=<parm>,<values>  (optional - 0 or more allowed) This argument allows the user to
                            set any number of two-D parameters to be used in the calculation.
                            Value must be parameter name, comma, comma-separated values.
                            Number of values must equal the number of points (or number of lats)
                            Example:  --twoD=te,1000,1100,1200  --twoD=ti,1000,1000,1000
                               where there are 3 inputs points

Returns comma-delimited data, one line for each location,
with the following fields:

1. latitude
2. longitude
3. altitude
4. Values for each Madrigal parameter listed in argument parms, separated by whitespace

If --showHeader given, also prints header line before data

This script, and the corresponding cgi script
"madCalculator2Service.py":../services/madCalculator2Service.py.html are available to any scripting
language that wants to access Madrigal derived parameters.

$Id: madCalculator2.py 7046 2019-10-07 19:57:14Z brideout $
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

usage = """madCalculator2.py is a script run to return a text output of derived Madrigal parameters for a time and a collection of gdlat, glon, and gdalt values.

It has the following input arguments:
     --date=<MM/DD/YYYY> (required)
     --time=<HH:MM:SS>  (optional - if no HH:MM:SS given, 00:00:00 assumed)
     --lats=<latitude array>  string with comma delimited latitudes, -90 to 90 (required)
     --longs=<longitude array>  string with comma delimited longitudes, -180 to 180 (required).
            Length must equal --lat 
     --alts=<altitude>  string with comma delimited altitudes, >= 0 (required)
             Length must equal --lat 
     --parms=<comma delimited string of Madrigal parameters desired> (required)
     --showHeader (optional)  Prints header line before data
     --oneD=<parm>,<value>  (optional - 0 or more allowed) This argument allows the user to
                            set any number of one-D parameters to be used in the calculation.
                            Value must be parameter name, comma, value as double.
                            Example:  --oneD=kinst,31.0  --oneD=elm=45.0
     --twoD=<parm>,<values>  (optional - 0 or more allowed) This argument allows the user to
                            set any number of two-D parameters to be used in the calculation.
                            Value must be parameter name, comma, comma-separated values.
                            Number of values must equal the number of points (or number of lats)
                            Example:  --twoD=te,1000,1100,1200  --twoD=ti,1000,1000,1000
                            where there are 3 inputs points

Returns comma-delimited data, one line for each position,
with the following fields:

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
                   'lats=',
                   'longs=',
                   'alts=',
                   'parms=',
                   'showHeader',
                   'oneD=',
                   'twoD=']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    date = None
    timeStr = None
    lats = []
    longs = []
    alts = []
    parms = None
    showHeader = 0
    oneDParmList = []
    oneDParmValues = []
    twoDParmList = []
    twoDParmValues = []

    for opt in optlist:
        if opt[0] == '--date':
            date = opt[1]
        elif opt[0] == '--time':
            timeStr = opt[1]
        elif opt[0] == '--lats':
            items = opt[1].split(',')
            for item in items:
                lats.append(float(item))
                if lats[-1] < -90.001 or lats[-1] > 90.001:
                    raise ValueError('Illegal latitude %f' % (lats[-1]))
        elif opt[0] == '--longs':
            items = opt[1].split(',')
            for item in items:
                longs.append(float(item))
                if longs[-1] > 180.0:
                    longs[-1] -= 360.0
                if longs[-1] < -180.001 or longs[-1] > 180.001:
                    raise ValueError('Illegal longitude %f' % (longs[-1]))
        elif opt[0] == '--alts':
            items = opt[1].split(',')
            for item in items:
                alts.append(float(item))
                if alts[-1] < -0.001:
                    raise ValueError('Illegal altitude %f' % (alts[-1]))
        elif opt[0] == '--parms':
            parms = opt[1]
        elif opt[0] == '--showHeader':
            showHeader = 1
        elif opt[0] == '--oneD':
            items = opt[1].split(',')
            if len(items) != 2:
                raise ValueError('oneD must be in form --oneD=<parm>,<value>, not %s' % (opt[1]))
            oneDParmList.append(parmObj.getParmMnemonic(items[0]))
            oneDParmValues.append(float(items[1]))
        elif opt[0] == '--twoD':
            items = opt[1].split(',')
            if len(items) < 2:
                raise ValueError('twoD must be in form --twoD=<parm>,<values>, not %s' % (opt[1]))
            twoDParmList.append(parmObj.getParmMnemonic(items[0]))
            twoDParmValues.append([])
            for item in items[1:]:
                twoDParmValues[-1].append(float(item))
    
        else:
            raise ValueError('Illegal option %s\n%s' % (opt[0], usage))

    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # verify input values
    if date == None:
        raise ValueError('Date argument required!\n%s' % (usage))

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
                      oneDParmValues,
                      twoDParmList,
                      twoDParmValues)

    

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by madCalculator')


except MadrigalError as e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in madCalculator2.py</h1>'

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
                         'Error running madCalculator2.py' )

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]).find('exceptions.SystemExit') != -1:
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running madCalculator2.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    print(errStr)

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running madCalculator2.py')


# end script

