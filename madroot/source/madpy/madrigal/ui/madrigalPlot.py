"""madrigalPlot is the module that produces plots of Madrigal data.

Presently based on madplotlib: http://matplotlib.sourceforge.net/

$Id: madrigalPlot.py 7618 2023-10-02 13:39:00Z brideout $

"""
import sys,os
import os.path
import traceback
import types
import datetime
import bisect
import math
import colorsys
import subprocess

import numpy

import madrigal.metadata
import madrigal._derive


def defineHomeEnvVariable():
    """defineHomeEnvVariable makes sure HOME env variable is defined, as required by matplotlib.

    If not defined, sets HOME to MADROOT/metadata/userdata
    """
    try:
        os.environ['HOME']
        return
    except KeyError:
        import madrigal.metadata
        madDB = madrigal.metadata.MadrigalDB()
        os.environ['HOME'] = os.path.join(madDB.getMadroot(), 'metadata/userdata')


defineHomeEnvVariable()

import matplotlib
# set rendering
matplotlib.use('Agg')
import matplotlib.pylab
import matplotlib.colors
import matplotlib.cm
import numpy.ma
import numpy



def convertToAbsoluteTimeStr(xticks, noTime=False, noDate=False, timezone=0):
    """convertToAbsoluteTimeStr converts a list of strings containing seconds since 1/1/1950 to datetime string.
    
    Input: xticks - a list of strings containing seconds since 1/1/1950
    
    Returns: a list of strings formated as YYYY-MM-DD HH-MM-SS.  If noTime, format as YYYY-MM-DD
    """
    datetime1950 = datetime.datetime(1950,1,1,0,0,0)
    
    newList = []
    seconds = int(xticks[0]) - timezone
    iniDoy = datetime1950 + datetime.timedelta(0, seconds)
    iniDoy = int(iniDoy.strftime('%j'))
    
    for item in xticks:
        seconds = int(item) - timezone
        newDatetime = datetime1950 + datetime.timedelta(0, seconds)
        if noTime:
            newList.append(newDatetime.strftime('%Y-%m-%d'))
        elif noDate:
            curDoy = int(newDatetime.strftime('%j'))
            currHour = newDatetime.hour
            currMin = newDatetime.minute
            currHour += (curDoy - iniDoy)*24
            
            newList.append('%02d:%02d' %(currHour,currMin))

        else:
            newList.append(newDatetime.strftime('%Y-%m-%d %H:%M:%S'))
    
    return newList


def get_vo_cmap():
    """get_vo_cmap is a function to return a colormap optimized to show sign changes in the middle of the range.
    """
    LUTSIZE = matplotlib.rcParams['image.lut']

    _vo_cm_data =     {'red':   ((0, 0.75, 0.75), (0.4, 0.0, 0.0), (0.5, 0.0, 0.0),   (0.6, 1.0, 1.0),   (1.0, 1.0, 1.0)),
                       'green': ((0, 1.0, 1.0), (0.4, 0.5, 0.5), (0.5, 0.25, 0.25), (0.6, 0.25, 0.25), (1.0, 1.0, 1.0)),
                       'blue':  ((0, 1.0, 1.0), (0.4, 1.0, 1.0), (0.5, 0.5, 0.5),   (0.6, 0.0, 0.0),   (1.0, 0.5, 0.5))}


    vo_cm = matplotlib.colors.LinearSegmentedColormap('vo_cm',_vo_cm_data, LUTSIZE)

    return vo_cm


def getIsprintString(filename, parms, filterStr):
    """getIsprintString returns the output of isprint given inputs
    
    Inputs:
        filename - Madrigal filename 
        parms - comma-delimited parameters
        filterStr - arguments to pass to isprint cmd
    """
    madDB = madrigal.metadata.MadrigalDB()
    cmd = '%s/bin/isprint file=%s ' % (madDB.getMadroot(), filename)
    for parm in parms.split(','):
        cmd += '%s ' % (parm)
    cmd += filterStr
    cmd += ' header=f summary=f '
    pipe = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE).stdout
    result = pipe.read()
    delimiter = '\n'
    if type(result) in (bytes, numpy.bytes_):
        result = result.decode('utf-8')
    lines = result.split(delimiter)
    return(delimiter.join(lines[1:]).strip())



class madScatterPlot:
    """madScatterPlot is the class the produces two dimensional scatter plots of x versus y.


    """
    def __init__(self, isprintText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None,
                 noTime = False,
                 noDate = False,
                 yMinimum = None,
                 yMaximum = None,
                 timezone = 0):
        """__init__ writes a madScatter plot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
			  to the experiment beginning (UTH) or absolute (UT1).
			  The second must be the parameter to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float. 
            
            titleStr - plot title (string) - should describe parameter being plotted
            
            xLabelStr - x label string
	    
	        yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
			    
    	    useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
    	                      since beginning of experiment (UTH).  If useAbsoluteTime is true, first
    			      parameter in isprintText must be UT1, if false, it must be UTH.
    
    	    startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
    	                in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
    	                startTime must be in units of UTH (hours since midnight UT of first day of
    	                experiment). Default is None, which means start at lowest time found.
    
    	    endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
    	              in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
    	              endTime must be in units of UTH (hours since midnight UT of first day of
    	              experiment). Default is None, which means end at largest time found.
    
    	    maxNumPoints - maximum number of points to plot.  If not None, truncate isprintText if
    	                   needed to have at most maxNumPoints lines.
        
            noTime - if True and useAbsoluteTime True, then dates without times printed on x axis.
            
            noDate - if True and useAbsoluteTime True, then times without dates printed on x axis.
    
            yMinimum - set y minimum.  If default=None, use data minimum
    
            yMaximum - set y maximum.  If default=None, use data maximum
            
            timezone - The offset of the local (non-DST) timezone, in seconds west of UTC 
                        (negative in most of Western Europe, positive in the US, zero in the UK).
                        
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, since numpy doesn't handle NaN
        self.__parameter_count = 2

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))
        
        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumPoints != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumPoints)
               
        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_missing, split_data))
            array_data = numpy.asarray(float_data)
            array_data = numpy.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')
        
        # find min, max of x (time)
        if startTime == None:
            xMin = array_data[0,0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = array_data[-1,0]
        else:
            xMax = endTime
        
	    
        # find min, max of y, not including missing
        yMin = None
        yMax = None
        for y in array_data[:,1]:
            if y == self.__missing:
                continue
            if yMin == None:
                yMin = y
            elif yMin > y:
                yMin = y
            if yMax == None:
                yMax = y
            elif yMax < y:
                yMax = y
		
        if yMin == None:
            raise ValueError('No valid y data found')

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(9,3), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(10,6), facecolor = 'w')
	    
        if useAbsoluteTime:
             # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.8, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.66, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.14, 0.18, 0.83, 0.7]) 
      
        matplotlib.pylab.scatter(array_data[:,0],array_data[:,1])

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)
        matplotlib.pylab.grid(True)
            
        if useAbsoluteTime:
            # if plot is less than 24 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim() 
            xmin = xmin / (60*60)
            xmax = xmax / (60*60)
            if noDate == True:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                step = int((xmax-xmin-1)/6)+1
                xRange =  list(range(0,int(newXmax-xmin),step))
                for i in xRange:
                    tmp = (xmin + i)*60*60
                    newXLocs.append(tmp)
                
                if (xmax - xmin) < 1: newXLocs.append(xmax*60*60)
                
                newXLabels = convertToAbsoluteTimeStr(newXLocs, noDate=noDate, timezone=timezone)
                matplotlib.pylab.xticks(newXLocs, newXLabels)
            
            else:
                locs, labels = matplotlib.pylab.xticks()
                if len(locs) > 5:
                    # truncate by len(locs) / 5
                    scale = 1 + int(len(locs) / 5)
                    new_locs = []
                    for i in range(len(locs)):
                        if i % scale == 0:
                            new_locs.append(locs[i])
                    locs = new_locs
                newXTicks = convertToAbsoluteTimeStr(locs)
                matplotlib.pylab.xticks(locs, newXTicks, rotation=15)
	    
        matplotlib.pylab.xticks(fontsize=fontSize)

        if yMinimum != None and yMaximum != None:
            matplotlib.pylab.ylim(yMinimum, yMaximum)
        elif yMinimum != None and yMaximum == None:
            matplotlib.pylab.ylim(yMin=yMinimum)
        elif yMinimum == None and yMaximum != None:
            matplotlib.pylab.ylim(yMax=yMaximum)
            
        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()
          
        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()


    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText
        

    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

        

class madLineTimePlot:
    """madLineTimePlot is the class the produces line plots of one or more parameters versus time.


    """
    def __init__(self, isprintText,
                 yParmList,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None,
                 noTime = False,
                 yMinimum = None,
                 yMaximum = None,
                 noDate = False,
                 timezone = 0):
        """__init__ writes a madLineTimePlot plot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
			  to the experiment beginning (UTH) or absolute (UT1).
			  The the following must be the parameters to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float.

            yParmList - a list of y parameters (strings).  Length must == num columns in isprintText - 1
            
            titleStr - plot title (string) - should describe parameter being plotted
            
            xLabelStr - x label string
            
            yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
			    
    	    useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
    	                      since beginning of experiment (UTH).  If useAbsoluteTime is true, first
    			      parameter in isprintText must be UT1, if false, it must be UTH.
    
    	    startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
    	                in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
    	                startTime must be in units of UTH (hours since midnight UT of first day of
    	                experiment). Default is None, which means start at lowest time found.
    
    	    endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
    	              in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
    	              endTime must be in units of UTH (hours since midnight UT of first day of
    	              experiment). Default is None, which means end at largest time found.
    
    	    maxNumPoints - maximum number of points to plot.  If not None, truncate isprintText if
    	                   needed to have at most maxNumPoints lines.
    
    	    noTime - if True and useAbsoluteTime True, then dates without times printed on x axis.
    	    
    	    noDate - if True and useAbsoluteTime True, then times without dates printed on x axis.
    
    	    yMinimum - set y minimum.  If default=None, use data minimum
    
    	    yMaximum - set y maximum.  If default=None, use data maximum
    	    
    	    timezone - The offset of the local (non-DST) timezone, in seconds west of UTC 
                        (negative in most of Western Europe, positive in the US, zero in the UK).
            
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, since numpy doesn't handle NaN
        self.__parameter_count = 1 + len(yParmList)
        self.__colorList = 'gbrcmykw' # a list of colors for the lines

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))
        
        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumPoints != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumPoints)
               
        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_missing, split_data))
            array_data = numpy.asarray(float_data)
            array_data = numpy.reshape(array_data,(-1,self.__parameter_count))
            threshold = self.__missing*0.9999
            array_data = numpy.ma.masked_where(array_data > threshold, array_data)
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')
        
        # find min, max of x (time)
        if startTime == None:
            xMin = array_data[0,0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = array_data[-1,0]
        else:
            xMax = endTime
        
	    
        # find yMin just to ensure there is data
        yMin = None
        for column in range(len(yParmList)):
            for y in array_data[:,1+column]:
                if y == self.__missing:
                    continue
                if yMin == None:
                    yMin = y
                elif yMin > y:
                    yMin = y
		
        if yMin == None:
            raise ValueError('No valid y data found')

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(8,3), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
	    
        if useAbsoluteTime:
             # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.8, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.9, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.1, 0.18, 0.87, 0.7]) 
        else:
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.15, 0.71, 0.73])
      
        for column in range(len(yParmList)):
            color = self.__colorList[column % len(self.__colorList)]
            matplotlib.pylab.plot(array_data[:,0],array_data[:,1 + column], '%so-' % (color), ms=5, mew=0.5)

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.grid(True)

        if useAbsoluteTime:
            
            # if plot is less than 24 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim() 
            xmin = xmin / (60*60)
            xmax = xmax / (60*60)
            if noDate == True:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                step = int((xmax-xmin-1)/6)+1
                xRange =  list(range(0,int(newXmax-xmin),step))
                for i in xRange:
                    tmp = (xmin + i)*60*60
                    newXLocs.append(tmp)
                
                if (xmax - xmin) < 1: newXLocs.append(xmax*60*60)
                
                newXLabels = convertToAbsoluteTimeStr(newXLocs, noDate=noDate, timezone=timezone)
                matplotlib.pylab.xticks(newXLocs, newXLabels)
                
            else:
                locs, labels = matplotlib.pylab.xticks()
                if len(locs) > 5:
                    # truncate by len(locs) / 5
                    scale = 1 + int(len(locs) / 5)
                    new_locs = []
                    for i in range(len(locs)):
                        if i % scale == 0:
                            new_locs.append(locs[i])
                    locs = new_locs
                newXTicks = convertToAbsoluteTimeStr(locs, noTime)
                matplotlib.pylab.xticks(locs, newXTicks, rotation=15)
        else:
            # if plot is less than 48 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim()
            if xmax < 49:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                for i in range(0,int(4+newXmax),4):
                    newXLocs.append(i)
                    newXLabels.append(str(i))
                matplotlib.pylab.xticks(newXLocs, newXLabels)

        matplotlib.pylab.xticks(fontsize=fontSize)

        if yMinimum != None and yMaximum != None:
            matplotlib.pylab.ylim(yMinimum, yMaximum)
        elif yMinimum != None and yMaximum == None:
            matplotlib.pylab.ylim(yMin=yMinimum)
        elif yMinimum == None and yMaximum != None:
            matplotlib.pylab.ylim(yMax=yMaximum)

        matplotlib.pylab.title(titleStr, fontsize=fontSize)
        matplotlib.pylab.legend(yParmList, numpoints=1)
          
        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()

    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText
        
	
    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

    def writeToFile(self, fullFilename):
        pass

    def displayToScreen(self):
        pass

    def getFigureHandle(self):
        pass
    
    
    
class mad2YTimePlot:
    """mad2YTimePlot is the class the produces line plots of two parameters with different Y axes versus time.


    """
    def __init__(self, isprintText,
                 yParmList,
                 titleStr,
                 xLabelStr,
                 fullFilename,
                 size = 'small',
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None,
                 noTime = False,
                 yMinimum = None,
                 yMaximum = None,
                 noDate = False,
                 timezone = 0):
        """__init__ writes a madLineTimePlot plot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
              to the experiment beginning (UTH) or absolute (UT1).
              The following must be two parameters to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float.

            yParmList - a list of two y parameters (strings)
            
            titleStr - plot title (string) - should describe parameter being plotted
            
            xLabelStr - x label string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
                
            useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
                              since beginning of experiment (UTH).  If useAbsoluteTime is true, first
                      parameter in isprintText must be UT1, if false, it must be UTH.
    
            startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
                        in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
                        startTime must be in units of UTH (hours since midnight UT of first day of
                        experiment). Default is None, which means start at lowest time found.
    
            endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
                      in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
                      endTime must be in units of UTH (hours since midnight UT of first day of
                      experiment). Default is None, which means end at largest time found.
    
            maxNumPoints - maximum number of points to plot.  If not None, truncate isprintText if
                           needed to have at most maxNumPoints lines.
    
            noTime - if True and useAbsoluteTime True, then dates without times printed on x axis.
            
            noDate - if True and useAbsoluteTime True, then times without dates printed on x axis.
    
            yMinimum - set y minimum of first parm.  If default=None, use data minimum
    
            yMaximum - set y maximum of first.  If default=None, use data maximum
            
            timezone - The offset of the local (non-DST) timezone, in seconds west of UTC 
                        (negative in most of Western Europe, positive in the US, zero in the UK).
            
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, since numpy doesn't handle NaN
        self.__parameter_count = 1 + len(yParmList)
        self.__colorList = 'gbrcmykw' # a list of colors for the lines

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))
        
        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumPoints != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumPoints)
               
        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_missing, split_data))
            array_data = numpy.asarray(float_data)
            array_data = numpy.reshape(array_data,(-1,self.__parameter_count))
            threshold = self.__missing*0.9999
            array_data = numpy.ma.masked_where(array_data > threshold, array_data)
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')
        
        # find min, max of x (time)
        if startTime == None:
            xMin = array_data[0,0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = array_data[-1,0]
        else:
            xMax = endTime
        
        
        # find yMin just to ensure there is data
        yMin = None
        for column in range(len(yParmList)):
            for y in array_data[:,1+column]:
                if y == self.__missing:
                    continue
                if yMin == None:
                    yMin = y
                elif yMin > y:
                    yMin = y
        
        if yMin == None:
            raise ValueError('No valid y data found')


        fig, ax1 = matplotlib.pylab.subplots()
        # select the plot size
        if size == 'small':
            fig.set_size_inches(8, 3)
        elif size == 'wide':
            fig.set_size_inches(10,4)
        elif size == 'large':
            fig.set_size_inches(12,6)
        
        if useAbsoluteTime:
             # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.8, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.9, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.1, 0.18, 0.87, 0.7]) 
        else:
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.15, 0.71, 0.73])
      
        # plot first
        color = self.__colorList[0]
        ax1.plot(array_data[:,0],array_data[:,1], '%so-' % (color))
        ax1.set_ylabel(yParmList[0], color=color)
        
        ax2 = ax1.twinx()
        color = self.__colorList[1]
        ax2.plot(array_data[:,0],array_data[:,2], '%so-' % (color))
        ax2.set_ylabel(yParmList[1], color=color)

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.grid(True)

        if useAbsoluteTime:
            
            # if plot is less than 24 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim() 
            xmin = xmin / (60*60)
            xmax = xmax / (60*60)
            if noDate == True:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                step = int((xmax-xmin-1)/6)+1
                xRange =  list(range(0,int(newXmax-xmin),step))
                for i in xRange:
                    tmp = (xmin + i)*60*60
                    newXLocs.append(tmp)
                
                if (xmax - xmin) < 1: newXLocs.append(xmax*60*60)
                
                newXLabels = convertToAbsoluteTimeStr(newXLocs, noDate=noDate, timezone=timezone)
                matplotlib.pylab.xticks(newXLocs, newXLabels)
                
            else:
                locs, labels = matplotlib.pylab.xticks()
                if len(locs) > 5:
                    # truncate by len(locs) / 5
                    scale = 1 + int(len(locs) / 5)
                    new_locs = []
                    for i in range(len(locs)):
                        if i % scale == 0:
                            new_locs.append(locs[i])
                    locs = new_locs
                newXTicks = convertToAbsoluteTimeStr(locs, noTime)
                matplotlib.pylab.xticks(locs, newXTicks, rotation=15)
        else:
            # if plot is less than 48 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim()
            if xmax < 49:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                for i in range(0,int(4+newXmax),4):
                    newXLocs.append(i)
                ax1.set_xticks(newXLocs)
                ax2.set_xticks(newXLocs)

        matplotlib.pylab.xticks(fontsize=fontSize)

        if yMinimum != None or yMaximum != None:
            ax1.set_ylim(yMinimum, yMaximum)

        matplotlib.pylab.title(titleStr, fontsize=fontSize)
          
        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()

    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText
        
    
    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing




                 
class madXYScatterPlot:
    """madXYScatterPlot is the class the produces XY scatter plots.


        Change History:

            Written by "Brandon Scott Fines":mailto:bfines@haystack.mit.edu Aug 2, 2006
            Written by "Bill Rideout":mailto:brideout@haystack.mit.edu Aug 2, 2006

    """

    def __init__(self,inputText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size='small',
                 lowBound=None,
                 highBound=None,
                 maxNumPoints=None,
                 yLegend=None):

        """
        Inputs:

            inputText - string of values to be plotted. The formatting is as
                        follows:

                        xval yval
                        xval yval
                        xval yval

            titleStr - title of the Plot

            xLabelStr - label for the x axis

            yLabelStr - label for the y axis

            fullFilename - full file path to save the resulting picture to

            size - size of plot to be saved. Must be 'small','wide', or 'large'.
                defaults to 'small'.

            lowBound - lower bound on the x-axis value. If no bound is specified,
                    the lowest value found in inputText will be used.

            highBound - upper bound on the x-axis value. If no bound is specified,
                    the highest value found in inputText will be used.

            maxNumPoints - maximum number of points to be plotted.

        Outputs: None

        Affects: Creates a scatter plot using matplotlib and writes that to the
                    file designated by the variable 'fullFilename'

        Exceptions: ValueError if lowBound or highBound cannot be converted to
                    a float value

        Non-standard python modules used:

            matplotlib
        """

        #verify input
        if size not in ('small','wide','large'):
            raise ValueError('size must be "small","wide", or "large", not %s'%(str(size)))

        if size in ('small','wide'):
            fontSize = 12
        elif size in 'large':
            fontSize = 18

        if maxNumPoints !=None:
            inputText = self.__truncateInput(inputText, maxNumPoints)


        if lowBound !=None:
            #check that it is a number
            try:
                lowBound = float(lowBound)
            except ValueError:
                raise ValueError('lowBound not a number')
            try:
                highBound = float(highBound)
            except:
                raise ValueError('lowBound not a number')     
                

        #convert the input data into numeric arrays
        x=[]
        y=[]

        split_data = inputText.split()
        #send x-values to x, y-values to y
        i=0
        while i <len(split_data)-1:
            try:
                newX = float(split_data[i])
                newY = float(split_data[i+1])
                x.append(newX)
                y.append(newY)
            except:
                pass

            i=i+2
        
        xlow =None
        xhigh = None
        # set min and max in x if specified
        if lowBound !=None and highBound !=None:
            xlow = lowBound
            xhigh = highBound
        elif lowBound !=None and highBound==None:
            #find the upper bound in x
            for i in x:
                if xhigh==None:
                    xhigh = i
                if i>xhigh:
                    xhigh = i
            xlow = lowBound
        elif lowBound==None and highBound !=None:
            #find the lower bound in x
            for i in x:
                if xlow==None:
                    xlow = i
                if i<xlow:
                    xlow = i
            xhigh=highBound
        else:
            #find lower and upper bounds
            for i in x:
                if xlow==None:
                    xlow = i
                elif i<xlow:
                    xlow = i
                if xhigh==None:
                    xhigh = i
                elif i>xhigh:
                    xhigh = i

        #find upper and lower bounds in y
        ylow = None
        yhigh = None
        for i in y:
            if ylow==None:
                ylow = i
            elif i<ylow:
                ylow = i
            if yhigh==None:
                yhigh = i
            elif i>yhigh:
                yhigh = i



        #check for good numbers
        if ylow == None:
            raise ValueError('no valid y data found')

        #select plot sizes
        if size=='small':
            matplotlib.pylab.figure(1,figsize=(6,4),facecolor='w')
        elif size=='wide':
            matplotlib.pylab.figure(1,figsize=(10,4),facecolor='w')
        elif size=='large':
            matplotlib.pylab.figure(1,figsize=(12,6),facecolor='w')


        #draw scatter plot
        #matplotlib.pylab.scatter(x,y)
        matplotlib.pylab.plot(x,y, 'gd',ms=5,mew=0.5)


        thisFont = {'size' : fontSize}
        matplotlib.pylab.rc('font', **thisFont) #pass in font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr,fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr,fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xlow,xhigh)
        matplotlib.pylab.title(titleStr,fontsize=fontSize)
        matplotlib.pylab.grid(True)
        
        if yLegend != None: matplotlib.pylab.legend(yLegend,numpoints=1)

        #save the figure
        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()
        
class madXYwithErrorPlot:
    """madXYwithErrorPlot is the class the produces two dimensional scatter plots of x versus y more its error.
    Written by "Miguel Urco":mailto:murco@jro.igp.gob.pe Jul 2, 2009

    """
    def __init__(self, isprintText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None,
                 noTime = False,
                 noDate = False,
                 yMinimum = None,
                 yMaximum = None,
                 timezone = 0):
        """__init__ writes a madScatter plot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First of three parameters
                          must be UTH or UT1, depending on whether time scale should be relative
                          to the experiment beginning (UTH) or absolute (UT1).
                          The second must be the parameter to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float. 
            
            titleStr - plot title (string) - should describe parameter being plotted
            
            xLabelStr - x label string
        
            yLabelStr - y label string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
                
            useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
                            since beginning of experiment (UTH).  If useAbsoluteTime is true, first
                            parameter in isprintText must be UT1, if false, it must be UTH.

            startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
                    in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
                    startTime must be in units of UTH (hours since midnight UT of first day of
                    experiment). Default is None, which means start at lowest time found.

            endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
                  in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
                  endTime must be in units of UTH (hours since midnight UT of first day of
                  experiment). Default is None, which means end at largest time found.

            maxNumPoints - maximum number of points to plot.  If not None, truncate isprintText if
                       needed to have at most maxNumPoints lines.
            
            noDate - if True and useAbsoluteTime True, then times without dates printed on x axis.
    
            yMinimum - set y minimum.  If default=None, use data minimum
    
            yMaximum - set y maximum.  If default=None, use data maximum
            
            timezone - The offset of the local (non-DST) timezone, in seconds west of UTC 
                        (negative in most of Western Europe, positive in the US, zero in the UK).
                        
        Returns: void

        Affects: None
        
        Now removes all lines with missing
        """
        self.__missing = 1.0E30 # special value, since numpy doesn't handle NaN
        self.__parameter_count = 3

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))
        
        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18
            
        # filter out missing lines
        newIsprint = ''
        lines = isprintText.split('\n')
        for line in lines:
            try:
                items = line.split()
                float(items[0])
                float(items[1])
                float(items[2])
                newIsprint += line + ' '
            except:
                continue
        isprintText = newIsprint

        if maxNumPoints != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumPoints)
               
        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_missing, split_data))
            array_data = numpy.asarray(float_data)
            array_data = numpy.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')
        
        # find min, max of x (time)
        if startTime == None:
            xMin = array_data[0,0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = array_data[-1,0]
        else:
            xMax = endTime
        
        
        # find min, max of y, not including missing
        yMin = None
        yMax = None

        for y in array_data[:,1]:
            if y == self.__missing:
                continue
            if yMin == None:
                yMin = y
            elif yMin > y:
                yMin = y
            if yMax == None:
                yMax = y
            elif yMax < y:
                yMax = y

        if yMin == None:
            raise ValueError('No valid y data found')

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(8,3), facecolor = 'w')
            matplotlib.pylab.axes([0.11, 0.18, 0.78, 0.7])
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(10,6), facecolor = 'w')
        
        if useAbsoluteTime:
             # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.8, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.66, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.1, 0.18, 0.87, 0.7]) 
      
        matplotlib.pylab.errorbar(array_data[:,0],array_data[:,2],array_data[:,1],ecolor='red',fmt='s',mfc='red',mec='green',ms=1,mew=1)

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)
        matplotlib.pylab.grid(True)

        if yMinimum != None and yMaximum != None:
            matplotlib.pylab.ylim(yMinimum, yMaximum)
        elif yMinimum != None and yMaximum == None:
            matplotlib.pylab.ylim(yMin=yMinimum)
        elif yMinimum == None and yMaximum != None:
            matplotlib.pylab.ylim(yMax=yMaximum)

        if useAbsoluteTime:
            # if plot is less than 24 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim()
            xmin = xmin / (60*60)
            xmax = xmax / (60*60)
            if noDate == True:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0)+1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                step = int((xmax-xmin-1)/6)+1
                xRange =  list(range(0,int(newXmax-xmin),step))
                for i in xRange:
                    tmp = (xmin + i)*60*60
                    newXLocs.append(tmp)
                
                if (xmax - xmin) < 1: newXLocs.append(xmax*60*60)
                
                newXLabels = convertToAbsoluteTimeStr(newXLocs, noDate=noDate, timezone=timezone)
                matplotlib.pylab.xticks(newXLocs, newXLabels)
            else:
                locs, labels = matplotlib.pylab.xticks()
                if len(locs) > 5:
                    # truncate by len(locs) / 5
                    scale = 1 + int(len(locs) / 5)
                    new_locs = []
                    for i in range(len(locs)):
                        if i % scale == 0:
                            new_locs.append(locs[i])
                    locs = new_locs
                newXTicks = convertToAbsoluteTimeStr(locs)
                matplotlib.pylab.xticks(locs, newXTicks, rotation=15)
        
        matplotlib.pylab.xticks(fontsize=fontSize)

        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()
          
        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()
        
    def __truncateInput(self,inputStr,maxLines):
        """__truncateInput truncates inputStr to have maxLines at most
        """
        inputList = inputStr.split('\n')
        if len(inputList)<maxLines:
            return inputStr
        else:
            dropNumber = int(len(inputList)/maxLines)
            newline = '\n'
            newInputText = newline.join(inputList[::dropNumber])

            return newInputText
        
    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

class madPcolorPlot:
    """madPcolorPlot is the class that produces pcolor plots of x versus y with z intensity.

    Assumes the x axis is time.

    Usage example::

    	obj = madPcolorPlot(isprintText,
                        'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2003 - Alt code',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Altitude (km)',
                        './isprint.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12,
			smoothAltitude = False)    

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Mar. 31, 2005
    """


    def __init__(self, isprintText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 minColormap = None,
                 maxColormap = None,
                 smoothAltitude = True,
                 insertDataGap = 5,
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 sortTimeFlag = False,
                 maxNumTimes = None,
                 maxNumAlt = None,
                 truncateIsprint = False,
                 colorMap = matplotlib.cm.jet,
                 yMinimum = None,
                 yMaximum = None,
                 altYTitle = None,
                 altYLabels = None,
                 noTime = False,
                 noDate = False,
                 timezone = 0,
                 background = 'w',
                 peakAltStr = None):
        
        """__init__ writes a madPColorPlot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
			  to the experiment beginning (UTH) or absolute (UT1).
			  The second must be gdalt, and third parameter to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float. 
            
            titleStr - plot title (string) - should describe parameter being plotted
	    
    	    xLabelStr - x label string
    	    
    	    yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.
	    
    	    smoothAltitude - if True, extrapolate between existing data between altitudes to fill
    	                     in missing data; if False, leave missing
    			     
    	    insertDataGap - this parameter sets the threshold for inserting a data gap.  The time intervals
    	                    being plotted are ordered, and the time gap larger than 90% of the rest is determined.
    			    Any time interval more than insertDataGap times bigger is then considered missing
    			    data.  Defaults to five.  If None, no gaps are ever inserted.  For data with close
    			    to uniform time intervals, no gaps will be inserted.
    			    
    	    useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
    	                      since beginning of experiment (UTH).  If useAbsoluteTime is true, first
    			      parameter in isprintText must be UT1, if false, it must be UTH.
        
            startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
	                in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	                startTime must be in units of UTH (hours since midnight UT of first day of
	                experiment). Default is None, which means start at lowest time found.

    	    endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
    	              in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
    	              endTime must be in units of UTH (hours since midnight UT of first day of
    	              experiment). Default is None, which means end at largest time found.
    
    	    sortTimeFlag - if true, check that time is correctly sorted.  If false (the default),
    	                   assume time already sorted
    
    	    maxNumTimes - if not None, decimate the number of times in the isprint string to
    	                  maxNumTimes.  If None (the default), plot all times.
    
    	    maxNumAlt - if not None, reduce the number of altitudes to maxNumAlt.  If None (the default),
    	                plot all altitudes.
    
    	    truncateIsprint - if True, and both maxNumTimes and maxNumAlt not = None, then truncate
    	                       the number of isprint lines to be maxNumTimes * maxNumAlt
    
    	    colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet
    
    	    yMinimum - minumum y value.  If None (default), set by data y minimum.
    
    	    yMaximum - maximum y value.  If None (default), set by data y maximum.
    
    	    altYTitle - title of right (second) y axis.  If None (the default),
    	                 no Y axis on the right side.
    
    	    altYLabels - a list of Y labels (strings) for the right axis.  If None (the default),
    	                 no Y labels on the right axis.

            noDate - if True and useAbsoluteTime True, then times without dates printed on x axis.
            
            timezone - The offset of the local (non-DST) timezone, in seconds west of UTC 
                        (negative in most of Western Europe, positive in the US, zero in the UK).
                        
            peakAltStr - if not None (the default), plot the peak altitude with x.  Format is a
                str with each line (time in same format as data, peak altitude)
                  
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, used to create a masked array
        self.__parameter_count = 3

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))

        if size in ('small'):
            fontSize = 10
        elif size in ('wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if truncateIsprint and maxNumTimes != None and maxNumAlt != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumTimes * maxNumAlt)

        # since matplotlib pcolor wants a regular grid, we create a grid with all altitudes

        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_missing, split_data))
            array_data = numpy.asarray(float_data)
            array_data = numpy.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')

        if sortTimeFlag:
            array_data = self.sortArrayInTime(array_data)

        if maxNumTimes != None and maxNumTimes != 0 and not truncateIsprint:
            array_data = self.decimateTimes(array_data, int(maxNumTimes), insertDataGap)
        
        # The first pass through is to obtain the number and range of the  x and y variables
        
        xList = []
        yList = []
        zList = []
        zMin = None
        zMax = None

        # loop over all the lines of data in the array
        for j in range(len(array_data)):
            try:
                
                x = array_data[j][0]
                y = array_data[j][1]
                z = array_data[j][2]

                if x not in xList:
                    xList.append(x)

                if y not in yList:
                    yList.append(y)

                if z != self.__missing:
                    zList.append(z)

                if zMin != None:
                    if z < zMin and z != self.__missing:
                        zMin = z
                elif z != self.__missing:
                    zMin = z

                if zMax != None:
                    if z > zMax and z != self.__missing:
                        zMax = z
                elif z != self.__missing:
                    zMax = z

            except:
                continue
		
        if zMin == None:
            raise ValueError('No valid z data found')

        # if both minColormap and maxColormap == None, use autoscaling
        if minColormap == None and maxColormap == None:
            zList.sort()
            d10 = zList[int(len(zList)*0.10)]
            d90 = zList[int(len(zList)*0.90)]

            zMin = d10 - (d90-d10) * 0.75
            zMax = d90 + (d90-d10) * 0.75

        # now sort the X and Y axis lists and pull their length
        
        xList.sort()
        if startTime == None:
            xMin = xList[0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = xList[-1]
        else:
            xMax = endTime
        yList.sort()
        max_x_dimension = len(xList)
        max_y_dimension = len(yList)

        if yMinimum == None:
            yMinimum = yList[0]
        if yMaximum == None:
            yMaximum = yList[-1]

        self.truncateAlt = False
        if maxNumAlt != None:
            if max_y_dimension > maxNumAlt:
                self.truncateAlt = True

        # build dictonary of indexes into xList
        self.xListDict = {}
        for i in range(len(xList)):
            self.xListDict[xList[i]] = i

        # if self.truncateAlt == False, build dictonary of indexes into yList,
        # else truncate y values by builing a list of maxNumAlt ranges
        if self.truncateAlt == False:
            self.yListDict = {}
            for i in range(len(yList)):
                self.yListDict[yList[i]] = i
        else:
            self.yListRanges = []
            for i in range(maxNumAlt):
                self.yListRanges.append(yList[int(i*(len(yList)/maxNumAlt))])
            max_y_dimension = maxNumAlt

        # now build arrays to handle the X axis label, Y axis label, and the Z data

        X = numpy.zeros((max_x_dimension, max_y_dimension), numpy.float32)
        Y = numpy.zeros((max_x_dimension, max_y_dimension), numpy.float32)
        Z = numpy.ones((max_x_dimension, max_y_dimension), numpy.float32)

        # all parameter values default to missing
        Z = Z * self.__missing

        # fill the X and Y arrays

        for i in range(max_x_dimension):
            for j in range(max_y_dimension):
                X[i][j] = float(xList[i])
                if self.truncateAlt:
                    Y[i][j] = float(yList[int(j*(len(yList)/maxNumAlt))])
                else:
                    Y[i][j] = float(yList[j])

    
        # Now load up the data array Z with the array_data measurements as a function of x and y
        previousIndex = None
        previousValue = None
        presentTime = None
        newTimeFound = True

        for k in range(len(array_data)):
            try:

                xdata = array_data[k][0]
                ydata = array_data[k][1]
                zdata = array_data[k][2]

                if zdata == self.__missing:
                    continue

                if xdata != presentTime:
                    newTimeFound = True
                else:
                    newTimeFound = False
                presentTime = xdata

                # now find the right place in the array for this data point
                i = self.xListDict[xdata]
                j = self.__getYIndex(ydata)

                Z[i][j] = zdata
		
                # now see if we need to fill in any gaps
                if (not newTimeFound) and smoothAltitude:
                    if previousIndex < j - 1:
                        # fill in all missing points
                        for l in range(previousIndex + 1, j):
                            # simply average between the points based on index
                            thisValue = previousValue + (zdata - previousValue)*(float(l-previousIndex)/float(j-previousIndex))
                            Z[i][l] = thisValue
			   
                previousIndex = j
                previousValue = zdata
		
                
            except:
                continue
		
        # insert missing data to represent gaps if needed
        if insertDataGap != None:
            # first find the time interval greater than 90% of others
            timeIntervalList = []
            for i in range(len(xList) - 1):
                timeIntervalList.append(xList[i+1] - xList[i])
            timeIntervalList.sort()
            index = int(len(timeIntervalList)*0.9)
            maxInterval = timeIntervalList[index]
	    
            for i in range(len(xList) - 1):
                if xList[i+1] - xList[i] > maxInterval * insertDataGap:
                    Z[i,:] = self.__missing

        # insert missing data to represent gaps if needed in Altitude
        if insertDataGap != None and not smoothAltitude:
            # first find the time interval greater than 90% of others
            altitudeIntervalList = []
            for i in range(len(yList) - 1):
                altitudeIntervalList.append(yList[i+1] - yList[i])
            altitudeIntervalList.sort()
            index = int(len(altitudeIntervalList)*0.9)
            maxInterval = altitudeIntervalList[index]
        
            for j in range(len(yList) - 1):
                if yList[j+1] - yList[j] > maxInterval * insertDataGap and j < max_y_dimension:
                    Z[:,j] = self.__missing
                    
        # make Z a masked array
        Znew = numpy.ma.masked_inside(Z, 0.99*self.__missing, 1.01*self.__missing)

        # set up plotting parameters
        if minColormap == None:
            minColormap = zMin
        if maxColormap == None:
            maxColormap = zMax

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(8,3), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(14,5), facecolor = 'w')
	    
        if useAbsoluteTime:
            # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.1, 0.85, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.97, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.1, 0.18, 0.97, 0.7])
        else:
            if size == 'small':
                matplotlib.pylab.axes([0.1, 0.18, 0.97, 0.7])
            elif size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.9, 0.73])

        matplotlib.pylab.pcolor(X,Y,Znew, edgecolors='none', vmin=minColormap, vmax=maxColormap, cmap = colorMap,
                                edgecolor='face')
        matplotlib.pylab.colorbar()

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.ylim(yMinimum, yMaximum)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)

        if useAbsoluteTime:
            # if plot is less than 49 hours
            xmin, xmax = matplotlib.pylab.xlim()
            xmin = numpy.floor(xmin / (60*60))
            xmax = numpy.ceil(xmax / (60*60))
            if noDate == True:
                newXLabels = []
                if (xmax - xmin) <= 1:
                    newXLocs = []
                    xRange = numpy.arange(0,1,0.25)
                    for i in xRange:
                        tmp=(xmin+i)*60*60
                        newXLocs.append(tmp)
                elif (xmax - xmin) <= 2:
                    newXLocs = []
                    xRange = numpy.arange(0,2,0.5)
                    for i in xRange:
                        tmp=(xmin+i)*60*60
                        newXLocs.append(tmp)
                else:
                    newXLocs = []
                    step = int((xmax-xmin-1)/6)+1
                    xRange =  list(range(0,int(xmax-xmin),step))
                    for i in xRange:
                        tmp = (xmin + i)*60*60
                        newXLocs.append(tmp)

                #newXLocs.append(xmax*60*60)                                                
                newXLabels = convertToAbsoluteTimeStr(newXLocs, noDate=noDate, timezone=timezone)
                matplotlib.pylab.xticks(newXLocs, newXLabels)
            else:
                locs, labels = matplotlib.pylab.xticks()
                if len(locs) > 5:
                    # truncate by len(locs) / 5
                    scale = 1 + int(len(locs) / 5)
                    new_locs = []
                    for i in range(len(locs)):
                        if i % scale == 0:
                            new_locs.append(locs[i])
                    locs = new_locs
                newXTicks = convertToAbsoluteTimeStr(locs)
                matplotlib.pylab.xticks(locs, newXTicks, rotation=15)

        else:
            # if plot is less than 48 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim()
            if xmax < 49 and (startTime is None or endTime is None):
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                for i in range(0,int(4+newXmax),4):
                    newXLocs.append(i)
                    newXLabels.append(str(i))
                matplotlib.pylab.xticks(newXLocs, newXLabels)


        matplotlib.pylab.xticks(fontsize=fontSize)

        # add second y-axis if desired
        if altYTitle != None and altYLabels != None:
            ax2 = matplotlib.pylab.twinx()
            matplotlib.pylab.ylabel(altYTitle)
            ax2.yaxis.tick_right()
            matplotlib.pylab.yticks(list(range(len(altYLabels))), altYLabels)
            matplotlib.pylab.yticks(fontsize=fontSize)

        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()
        if peakAltStr:
            # plot peak altitudes
            data = peakAltStr.split()
            xdata = [float(item) for i, item in enumerate(data) if i % 2 == 0]
            ydata = [float(item) for i, item in enumerate(data) if i % 2 == 1]
            matplotlib.pylab.plot(xdata, ydata, 'b+')
          
        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()



    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

    def __getYIndex(self, yvalue):
        """__getYIndex returns the correct index into the y dimension for a given y value.

        Input: yvalue - value of y parameter

        Returns: the correct index into the y dimension

        Algorithm: if self.truncateAlt == False, simple use the dictionary self.yListDict.  Else
        loop through self.yListRanges and return the first greater than the requested value
        """
        if self.truncateAlt == False:
            return self.yListDict[yvalue]
        else:
            i = bisect.bisect_left(self.yListRanges, yvalue)
            if i >= len(self.yListRanges):
                i = len(self.yListRanges) - 1
            return i
        
            
    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newIsprintText = ''
            for i in range(0,len(isprintList),dropNumber):
                newIsprintText += isprintList[i] + '\n'

            return newIsprintText


        
    def displayToScreen(self):
        " to implement this takes a reworking away from pylab to use the underlying matplotlib code "
        pass 

    def getFigureHandle(self):
        return self.__figure


    def getAverage(self, X):
        """returns the average of items in a float array.  Does not including missing data.
        If all data missing, returns self.__missing
        """
        count = 0
        total = 0.0
        for i in range(X.shape[0]):
            if X[i] != self.__missing:
                count += 1
                total += X[i]

        if count == 0:
            return self.__missing
        else:
            return total / float(count)


    def sortArrayInTime(self, array_data):
        """sortArrayInTime sorts a two-dimensional array so that the first element in each row (time) is in ascending order.

        Input: array_data - two-dimensional array to be sorted by rearranging rows so
               that the first element in each row (time) is in ascending order

        Returns: new_array
        """

        sortIndex = numpy.argsort(array_data[:,0])[0]

        # if already sorted, just return original array
        if sortIndex == numpy.sort(sortIndex):
            return array_data
        
        new_array = numpy.zeros(array_data.shape, array_data.dtype)

        for i in range(len(sortIndex)):
            new_array[sortIndex[i],:] = array_data[i,:]


        return new_array


    def decimateTimes(self, array_data, maxNumTimes, insertDataGap):
        """decimateTimes decimates array_data to have at most maxNumTimes times.

        Input: array_data - two-dimensional array to be decimated by deleting times and missing data.

               maxNumTimes: int representing the maximum number of times to keep in array_data

               insertDataGap - this parameter sets the threshold for inserting a data gap.  The time intervals
	                    being plotted are ordered, and the time gap larger than 90% of the rest is determined.
			    Note that this parameter is used here to stop the truncation of isprint lines that
			    will eventually be considered edge lines.

        Returns: new array built from decimated array_data

        """

        # get the number of times in array_data, and make a list of all unique times
        numTimes = 0
        uniqueTimes = []
        time_array = array_data[:, 0]
        for i in range(len(time_array)):
            if i == 0:
               numTimes =  1
               uniqueTimes.append(time_array[i])
            elif time_array[i-1] != time_array[i]:
                uniqueTimes.append(time_array[i])
                numTimes +=  1

        if numTimes <= maxNumTimes:
            return array_data
        

        # insert missing data to represent gaps if needed
        gapTimes = []
        if insertDataGap != None:
            # first find the time interval greater than 90% of others
            timeIntervalList = []
            for i in range(len(time_array) - 1):
                if time_array[i+1] == time_array[i]:
                    continue
                timeIntervalList.append(time_array[i+1] - time_array[i])
            timeIntervalList.sort()
            index = int(len(timeIntervalList)*0.9)
            maxInterval = timeIntervalList[index]
	    
            for i in range(len(time_array) - 1):
                if time_array[i+1] - time_array[i] > maxInterval * insertDataGap:
                    gapTimes.append(time_array[i+1])
                    gapTimes.append(time_array[i])

        # get the number of times to skip each time
        numSkip = numTimes//maxNumTimes

        # get the number of rows in the new_array
        numRows = 0
        numTimes = 0
        useThisTime = False
        for i in range(len(time_array)):
            if i == 0:
                numTimes =  1
            elif time_array[i-1] != time_array[i]:
                numTimes +=  1
                if numTimes % (numSkip + 1) == 0 or time_array[i] in gapTimes:
                    useThisTime = True
                    if array_data[i, -1] != self.__missing:
                        numRows += 1
                else:
                    useThisTime = False
            else:
                if useThisTime:
                    if array_data[i, -1] != self.__missing:
                        numRows += 1

        # create new_array
        new_array = numpy.zeros((numRows, array_data.shape[1]), array_data.dtype)

        # copy selected rows to new_array
        numRows = 0
        numTimes = 0
        useThisTime = False
        for i in range(len(time_array)):
            if i == 0:
                numTimes =  1
            elif time_array[i-1] != time_array[i]:
                numTimes +=  1
                if numTimes % (numSkip + 1) == 0 or time_array[i] in gapTimes:
                    useThisTime = True
                    if array_data[i, -1] != self.__missing:
                        new_array[numRows,:] = array_data[i,:]
                        numRows += 1
                else:
                    useThisTime = False
            else:
                if useThisTime:
                    if array_data[i, -1] != self.__missing:
                        new_array[numRows,:] = array_data[i,:]
                        numRows += 1

        return new_array



class madPcolorScan:
    """madPcolorScan is the class that produces pcolor scans.

    Usage example::

    	obj = madPcolorScan(isprintText,
                        'Nel (log(m^-3)) - 26 June 2006 13:49:43-14:07:36',
                        'Longitude',
                        'Latitude',
                        './isprint.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12)    

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 20, 2006
    """


    def __init__(self, isprintText,
                 xGridSize,
                 yGridSize,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 xMinimum = None,
                 xMaximum = None,
                 yMinimum = None,
                 yMaximum = None,
                 minColormap = None,
                 maxColormap = None,
                 colorMap = matplotlib.cm.jet,
                 maxNumLines = None):
        """__init__ writes a madPcolorScan to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be the X axis value, and the second must be the Y axis value.
			  The third column is the value (intensity). Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float.

            xGridSize - grid size for x data (for example 0.1 for 0.1 degree longitude grid)

            yGridSize - grid size for x data (for example 0.1 for 0.1 degree latitude grid)
            
            titleStr - plot title (string) - should describe parameter being plotted
	    
    	    xLabelStr - x label string
    	    
    	    yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.

            xMinimum = minumum x value.  If None (default), uses lowest x value found.

            xMaximum = maximum x value.  If None (default), uses highest x value found.

            yMinimum = minumum y value.  If None (default), uses lowest y value found.

            yMaximum = maximum y value.  If None (default), uses highest y value found.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.

            colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

	    maxNumLine - max number of lines in isprintText before truncating.  If None, no truncation

	              
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, used to create a masked array
        self.__parameter_count = 3
        self.__xGridSize = int(xGridSize)
        self.__yGridSize = int(yGridSize)
        self.__parseCount = 0 # used to tell which column self.__filter_input is working on

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))

        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumLines != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumLines)

        # since matplotlib pcolor wants a regular grid, we create a grid with all altitudes

        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_input, split_data))
            array_data = numpy.asarray(float_data)
            array_data = numpy.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')

        array_data = self.sortArrayInX(array_data)

        
        # The first pass through is to obtain the number and range of the  x and y variables
        
        xList = []
        yList = []
        zList = []
        zMin = None
        zMax = None

        # loop over all the lines of data in the array
        for j in range(len(array_data)):
            try:
                
                x = array_data[j][0]
                y = array_data[j][1]
                z = array_data[j][2]

                if x not in xList:
                    xList.append(x)

                if y not in yList:
                    yList.append(y)

                if z != self.__missing:
                    zList.append(z)

                if zMin != None:
                    if z < zMin and z != self.__missing:
                        zMin = z
                elif z != self.__missing:
                    zMin = z

                if zMax != None:
                    if z > zMax and z != self.__missing:
                        zMax = z
                elif z != self.__missing:
                    zMax = z

            except:
                continue

		
        if zMin == None:
            raise ValueError('No valid z data found')

        # if both minColormap and maxColormap == None, use autoscaling
        if minColormap == None and maxColormap == None:
            zList.sort()
            d10 = zList[int(len(zList)*0.10)]
            d90 = zList[int(len(zList)*0.90)]

            zMin = d10 - (d90-d10) * 0.75
            zMax = d90 + (d90-d10) * 0.75

        # now sort the X and Y axis lists and pull their length
        
        xList.sort()
        xMin = xList[0]
        xMax = xList[-1]
        yList.sort()
        yMin = yList[0]
        yMax = yList[-1]
        max_x_dimension = len(xList)
        max_y_dimension = len(yList)


        # build dictonary of indexes into xList
        self.xListDict = {}
        for i in range(len(xList)):
            self.xListDict[xList[i]] = i

        # build dictonary of indexes into yList,
        self.yListDict = {}
        for i in range(len(yList)):
            self.yListDict[yList[i]] = i


        # now build arrays to handle the X axis label, Y axis label, and the Z data

        X = numpy.zeros((max_x_dimension, max_y_dimension), numpy.float32)
        Y = numpy.zeros((max_x_dimension, max_y_dimension), numpy.float32)
        Z = numpy.ones((max_x_dimension, max_y_dimension), numpy.float32)

        # all parameter values default to missing
        Z = Z * self.__missing

        # fill the X and Y arrays

        for i in range(max_x_dimension):
            for j in range(max_y_dimension):
                X[i][j] = float(xList[i])
                Y[i][j] = float(yList[j])

    
        # Now load up the data array Z with the array_data measurements as a function of x and y
        previousIndex = None
        previousValue = None
        presentTime = None
        newTimeFound = True

        for k in range(len(array_data)):
            try:

                xdata = self.__round(array_data[k][0], self.__xGridSize)
                ydata = self.__round(array_data[k][1], self.__yGridSize)
                zdata = array_data[k][2]

                if zdata == self.__missing:
                    continue

                if xdata != presentTime:
                    newTimeFound = True
                else:
                    newTimeFound = False
                presentTime = xdata

                # now find the right place in the array for this data point
                i = self.xListDict[xdata]
                j = self.yListDict[ydata]

                Z[i][j] = zdata
			   
                previousIndex = j
                previousValue = zdata
		
                
            except:
                continue
		

        # make Z a masked array
        Znew = numpy.ma.masked_inside(Z, 0.99*self.__missing, 1.01*self.__missing)

        # set up plotting parameters
        if xMinimum == None:
            xMinimum = xMin
        if xMaximum == None:
            xMaximum = xMax
        if yMinimum == None:
            yMinimum = yMin
        if yMaximum == None:
            yMaximum = yMax
        if minColormap == None:
            minColormap = zMin
        if maxColormap == None:
            maxColormap = zMax

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(6,4), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
	    

        matplotlib.pylab.pcolor(X,Y,Znew, edgecolors='none', vmin=minColormap, vmax=maxColormap, cmap = colorMap)
        matplotlib.pylab.colorbar()

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.xlim(xMinimum, xMaximum)
        matplotlib.pylab.ylim(yMinimum, yMaximum)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)


        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()

        try:
            matplotlib.pylab.savefig(fullFilename)
        except:
            matplotlib.pylab.clf()
            raise
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()


    def __round(self, value, increment):
        """__round returns a value to the nearest increment 
        """
        return value - math.fmod(value, increment)



    def __filter_input(self,x):
        """__filter_input is called in map to convert missing strings to self.__missing, and to
        round x and y vales to the nearest xGridSize or yGridSize
        """
        try:
            value = float(x)
            if self.__parseCount % 3 == 0:
                # x value
                value = self.__round(value, self.__xGridSize)
            elif self.__parseCount % 3 == 1:
                # y value
                value = self.__round(value, self.__yGridSize)
                
        except:
            value = self.__missing

        self.__parseCount += 1
        return value
        
            
    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newIsprintText = ''
            for i in range(0,len(isprintList),dropNumber):
                newIsprintText += isprintList[i] + '\n'

            return newIsprintText


        
    def displayToScreen(self):
        " to implement this takes a reworking away from pylab to use the underlying matplotlib code "
        pass 

    def getFigureHandle(self):
        return self.__figure


    def getAverage(self, X):
        """returns the average of items in a float array.  Does not including missing data.
        If all data missing, returns self.__missing
        """
        count = 0
        total = 0.0
        for i in range(X.shape[0]):
            if X[i] != self.__missing:
                count += 1
                total += X[i]

        if count == 0:
            return self.__missing
        else:
            return total / float(count)


    def sortArrayInX(self, array_data):
        """sortArrayInX sorts a two-dimensional array so that the first element in each row (x) is in ascending order.

        Input: array_data - two-dimensional array to be sorted by rearranging rows so
               that the first element in each row (x) is in ascending order

        Returns: new_array
        """

        sortIndex = numpy.argsort(array_data[:,0])[0]

        # if already sorted, just return original array
        if sortIndex == numpy.sort(sortIndex):
            return array_data
        
        new_array = numpy.zeros(array_data.shape, array_data.dtype)

        for i in range(len(sortIndex)):
            new_array[sortIndex[i],:] = array_data[i,:]


        return new_array
    
    
class madPcolorWedgeScan:
    """madPcolorWedgeScan is the class that produces pcolor scans where data is drawn as wedge shapes.

    Usage example::

        obj = madPcolorScan(isprintText,
                        'Nel (log(m^-3)) - 26 June 2006 13:49:43-14:07:36',
                        'Longitude',
                        'Latitude',
                        './isprint.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12)    

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 20, 2006
    """


    def __init__(self, isprintText,
                 scanType,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 xMinimum = None,
                 xMaximum = None,
                 yMinimum = None,
                 yMaximum = None,
                 minColormap = None,
                 maxColormap = None,
                 colorMap = matplotlib.cm.jet):
        """__init__ writes a madPcolorWedgeScan to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. The 9 parameters
                are (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2, <parameter value>).  Any
                missing data should be written as "missing" or other string that
                cannot be converted to a float. gdlatr, gdlonr, galtr are the station location

            scanType - must be 'az' or 'el_lat' (for north or south el scans), 'el_lon'
                (for east or west el scans) or 'el_gcdist' (for all other el scans.
                For az - x is long, y is lat
                for el_* y is always alt, and x is either lat, lon, or great circle distance
            
            titleStr - plot title (string) - should describe parameter being plotted
        
            xLabelStr - x label string
        
            yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.

            xMinimum = minumum x value.  If None (default), uses lowest value found.

            xMaximum = maximum x value.  If None (default), uses highest value found.

            yMinimum = minumum y value.  If None (default), uses lowest value found.

            yMaximum = maximum y value.  If None (default), uses highest value found.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.

            colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

        Returns: void

        Affects: Writes fullFilename to disk
        """

        # verify input
        if scanType not in ('az', 'el_lat', 'el_lon', 'el_gcdist'):
            raise ValueError('scanType must be "az", "el_lat", "el_lon", "el_gcdist", not %s' % (str(scanType)))
        
        if size not in ('small', 'wide', 'large'):
            raise ValueError('size must be "small", "wide", or "large", not %s' % (str(size)))

        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        isprintLines = isprintText.split('\n')
        
        if xMaximum == None or xMinimum == None or yMaximum == None or yMinimum or maxColormap == None or minColormap == None:
            newLonMax, newLonMin, newLatMax, newLatMin, newAltMax, newGreatCDist, newParmMax, newParmMin = self._getLimits(isprintLines)
            # set values
            xMaximum, xMinimum, yMaximum, yMinimum = self._setLimits(scanType, xMaximum, xMinimum, yMaximum, yMinimum,
                                                                     newLonMax, newLonMin, newLatMax, newLatMin, newAltMax, 
                                                                     newGreatCDist, newParmMax, newParmMin)
            
            if maxColormap == None:
                maxColormap = newParmMax
            if minColormap == None:
                minColormap = newParmMin
            
        
        # set up wedge generator
        dataGen = self._generateWedges(isprintLines, scanType)
        
        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(6,5), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
            
        try:
            while True: 
                data = next(dataGen)
                x = [data[1][0], data[2][0], data[3][0], data[4][0]]
                y = [data[1][1], data[2][1], data[3][1], data[4][1]]
                dataRatio = (data[0] - minColormap) / (maxColormap - minColormap)
                colorStr = matplotlib.colors.rgb2hex(colorMap(dataRatio)[:3])
                thisPlot = matplotlib.pyplot.fill(x, y, colorStr, edgecolor='none')
        except StopIteration:
            pass

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs
        
        scalableMappable = matplotlib.cm.ScalarMappable(cmap=colorMap)
        scalableMappable.set_array(numpy.array([minColormap, maxColormap]))
        scalableMappable.set_clim(minColormap, maxColormap)
        def get_alpha(*args, **kw):
            return(1.0)
        scalableMappable.get_alpha = get_alpha


        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.xlim(xMinimum, xMaximum)
        matplotlib.pylab.ylim(yMinimum, yMaximum)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)
        matplotlib.pylab.colorbar(mappable=scalableMappable)
        matplotlib.pylab.gcf().subplots_adjust(bottom=0.15)

        try:
            matplotlib.pylab.savefig(fullFilename)
        except:
            matplotlib.pylab.clf()
            raise

        matplotlib.pylab.clf()
        
        
    def getGreatCircleDist(self, gdlatr, glonr, gdlat, glon):
        """getGreatCircleDist return the great circle distance in.  Taken from madDeriveMethods c code.
        
        Inputs: gdlatr, glonr: reference point on earth's surface
                gdlat, glon: second point on earth's surface
        """
        dlon = glonr/57.2958 - glon/57.2958
        dlat = gdlatr/57.2958 - gdlat/57.2958;
        a = math.sin(dlat/2.0)*math.sin(dlat/2.0) + math.cos(gdlatr/57.2958)*math.cos(gdlat/57.2958) * math.sin(dlon/2.0)*math.sin(dlon/2.0)
    
        if math.sqrt(a) < 1.0:
            c = 2.0 * math.asin(math.sqrt(a))
        else:
            c = 2.0 * math.asin(1.0)
    
        return(c * 6371.2)
        
        
    def _getLimits(self, isprintLines):
        """returns (newLonMax, newLonMin, newLatMax, newLatMin, newAltMax, newGreatCDist, newParmMax, newParmMin) 
        given isprint lines with
        parameters (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2, <parameter value>)
        
        We only look at the end points of ranges and the station location
        """
        # set initial values to station location
        items = isprintLines[1].split()
        slat = float(items[0])
        slon = float(items[1])
        if slon > 180.0:
            slon -= 360.0
        salt = float(items[2])
        maxLon = slon
        minLon = slon
        maxLat = slat
        minLat = slat
        maxAlt = salt
        maxGreatCDist = 0.0
        maxParm = -1.0e-30
        minParm = 1.0e30
        
        lastAz = None
        lastEl = None
        lastRange = None
        
        for i, isprintLine in enumerate(isprintLines):
            items = isprintLine.split()
            if len(items) < 9:
                continue
            range = float(items[3])
            az1 = float(items[4])
            az2 = float(items[5])
            thisAz = (az1 + az2)/2
            el1 = float(items[6])
            el2 = float(items[7])
            thisEl = (el1 + el2)/2
            try:
                parm = float(items[8])
                if parm > maxParm:
                    maxParm = parm
                if parm < minParm:
                    minParm = parm
            except ValueError:
                continue
            
            # see if we're at an end point
            if lastAz != None:
                if abs(lastAz - thisAz) > 0.001 or abs(lastEl - thisEl) > 0.001:
                    # the last point was an end point
                    gdlat,glon,gdalt = madrigal._derive.radarToGeodetic(slat,slon,salt,lastAz,lastEl,lastRange)
                    if gdlat > maxLat:
                        maxLat = gdlat
                    if gdlat < minLat:
                        minLat = gdlat
                    if glon > maxLon:
                        maxLon = glon
                    if glon < minLon:
                        minLon = glon
                    if gdalt > maxAlt:
                        maxAlt = gdalt
                    greatCDist = self.getGreatCircleDist(slat, slon, gdlat, glon)
                    if greatCDist > maxGreatCDist:
                        maxGreatCDist = greatCDist
                        
            lastAz = thisAz
            lastEl = thisEl
            lastRange = range
            
        # check last point
        gdlat,glon,gdalt = madrigal._derive.radarToGeodetic(slat,slon,slat,lastAz,lastEl,lastRange)
        if gdlat > maxLat:
            maxLat = gdlat
        if gdlat < minLat:
            minLat = gdlat
        if glon > maxLon:
            maxLon = glon
        if glon < minLon:
            minLon = glon
        if gdalt > maxAlt:
            maxAlt = gdalt
        greatCDist = self.getGreatCircleDist(slat, slon, gdlat, glon)
        if greatCDist > maxGreatCDist:
            maxGreatCDist = greatCDist
            
        return((maxLon, minLon, maxLat, minLat, maxAlt, maxGreatCDist, maxParm, minParm))
    
    
    def _setLimits(self, scanType, xMaximum, xMinimum, yMaximum, yMinimum,
                   newLonMax, newLonMin, newLatMax, newLatMin, newAltMax,
                   newGreatCDist, newParmMax, newParmMin):
        """ _setLimits sets xMaximum, xMinimum, yMaximum, yMinimum based on values
        found in self._getLimits and scanType.  If any value in (xMaximum, xMinimum, yMaximum, yMinimum)
        is not None, it will not be changed. """
        newXMax = xMaximum
        newXMin = xMinimum
        newYMax = yMaximum
        newYMin = yMinimum
        if scanType == 'az':
            if xMaximum == None:
                newXMax = newLonMax
            if xMinimum == None:
                newXMin = newLonMin
            if yMaximum == None:
                newYMax = newLatMax
            if yMinimum == None:
                newYMin = newLatMin
        elif scanType == 'el_lat':
            if xMaximum == None:
                newXMax = newLatMax
            if xMinimum == None:
                newXMin = newLatMin
            if yMaximum == None:
                newYMax = newAltMax
            if yMinimum == None:
                newYMin = 0.0
        elif scanType == 'el_lon':
            if xMaximum == None:
                newXMax = newLonMax
            if xMinimum == None:
                newXMin = newLonMin
            if yMaximum == None:
                newYMax = newAltMax
            if yMinimum == None:
                newYMin = 0.0
        elif scanType == 'el_gcdist':
            if xMaximum == None:
                newXMax = newGreatCDist
            if xMinimum == None:
                newXMin = 0.0
            if yMaximum == None:
                newYMax = newAltMax
            if yMinimum == None:
                newYMin = 0.0
        return((newXMax, newXMin, newYMax, newYMin))
    
    
    def _generateWedges(self, isprintLines, scanType):
        """_generateWedges is a python generator that return a data wedge in the form
        (dataValue, (point1_x, point1_y), (point2_x, point2_y), 
        (point3_x, point3_y), (point4_x, point4_y))
        
        Inputs:
        
            isprintLines - a list of line with 9 parameters that
                are (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2, <parameter value>).  Any
                missing data should be written as "missing" or other string that
                cannot be converted to a float. gdlatr, gdlonr, galtr are the station location.
                No wedge is returned for a missing data line
                
            scanType - must be 'az' or 'el_lat' (for north or south el scans), 'el_lon'
                (for east or west el scans) or 'el_gcdist' (for all other el scans.
                For az - x is long, y is lat
                for el_* y is always alt, and x is either lat, lon, or great circle distance
        """
        # state variables
        lastAz1 = None
        lastAz2 = None
        lastEl1 = None
        lastEl2 = None
        lastRange = None
        lastDiffRangeLow = None
        lastValue = None
        
        for isprintLine in isprintLines:
            items = isprintLine.split()
            if len(items) < 9:
                continue
            gdlatr = float(items[0])
            gdlonr = float(items[1])
            galtr = float(items[2])
            range = float(items[3])
            az1 = float(items[4])
            az2 = float(items[5])
            el1 = float(items[6])
            el2 = float(items[7])
            valueStr = items[8]
            if lastAz1 != None and ( abs(lastAz1-az1) > 0.001 or abs(lastEl1-el1) > 0.001 ):
                # new radar line found, send last point if needed
                if lastValue != None:
                    dataWedge = self._getWedge(gdlatr, gdlonr, galtr, lastRange, 
                                               lastDiffRangeLow, lastDiffRangeLow, lastAz1, lastAz2,
                                               lastEl1, lastEl2, lastValue, scanType)
                    yield(dataWedge)
                # this is the first point in the line, so no lastDiffRangeLow
                lastDiffRangeLow = None
            elif lastAz1 != None and ( abs(lastAz1-az1) < 0.001 and abs(lastEl1-el1) < 0.001 ):
                # continuing radar line
                if lastValue != None:
                    lastDiffRangeLow = (range - lastRange)/2.0
                    dataWedge = self._getWedge(gdlatr, gdlonr, galtr, lastRange, 
                                               lastDiffRangeLow, (range-lastRange)/2.0, lastAz1, lastAz2,
                                               lastEl1, lastEl2, lastValue, scanType)
                    yield(dataWedge)
                else:
                    lastDiffRangeLow = (range - lastRange)/2.0
            # reset state variables
            lastAz1 = az1
            lastAz2 = az2
            lastEl1 = el1
            lastEl2 = el2
            lastRange = range
            try:
                lastValue = float(valueStr)
            except:
                lastValue = None
                
        # finally, send last wedge if needed
        if lastValue != None:
            dataWedge = self._getWedge(gdlatr, gdlonr, galtr, lastRange, 
                                       lastDiffRangeLow, lastDiffRangeLow, lastAz1, lastAz2,
                                       lastEl1, lastEl2, lastValue, scanType)
            yield(dataWedge)
            


    def _getWedge(self, gdlatr, gdlonr, galtr, range,
                  diffRange1, diffRange2, az1, az2,
                  el1, el2, value, scanType):
        """_getWedge returns return a data wedge in the form
        (dataValue, (point1_x, point1_y), (point2_x, point2_y), 
        (point3_x, point3_y), (point4_x, point4_y)).  The x and y dimensions depend on
        scanType:
            az: x: lon, y: lat
            el_lat: x: lat, y: alt
            el_lon: x: lon, y: alt
            el_gcdist: x: great circle distance, y: alt
            
        Inputs:
            gdlatr, gdlonr, galtr - location of radar
            range, diffRange1, diffRange2 - range of center of wedge, and distance in and out from there
            az1, az2, el1, el2 - start and end aximuth and elevation.  Which is used depends on scanType
            value - float data value
            scanType - az, el_lat, el_lon, el_gcdist
        """
        if scanType == 'az':
            point1_y,point1_x,gdalt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range-diffRange1)
            point2_y,point2_x,gdalt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az2,el1,range-diffRange1)
            point3_y,point3_x,gdalt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az2,el1,range+diffRange2)
            point4_y,point4_x,gdalt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range+diffRange2)
            
        elif scanType == 'el_lat':
            point1_x,glon,point1_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range-diffRange1)
            point2_x,glon,point2_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el2,range-diffRange1)
            point3_x,glon,point3_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el2,range+diffRange2)
            point4_x,glon,point4_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range+diffRange2)
            
        elif scanType == 'el_lon':
            gdlat,point1_x,point1_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range-diffRange1)
            gdlat,point2_x,point2_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el2,range-diffRange1)
            gdlat,point3_x,point3_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el2,range+diffRange2)
            gdlat,point4_x,point4_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range+diffRange2)
            
        elif scanType == 'el_gcdist':
            gdlat,glon,point1_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range-diffRange1)
            point1_x = self.getGreatCircleDist(gdlatr, gdlonr, gdlat, glon)
            gdlat,glon,point2_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el2,range-diffRange1)
            point2_x = self.getGreatCircleDist(gdlatr, gdlonr, gdlat, glon)
            gdlat,glon,point3_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el2,range+diffRange2)
            point3_x = self.getGreatCircleDist(gdlatr, gdlonr, gdlat, glon)
            gdlat,glon,point4_y = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range+diffRange2)
            point4_x = self.getGreatCircleDist(gdlatr, gdlonr, gdlat, glon)
            
        return((value, (point1_x, point1_y), (point2_x, point2_y), (point3_x, point3_y), (point4_x, point4_y)))
    
    
class madPcolorWedgeKmlScan:
    """madPcolorWedgeKmlScan is the class that produces pcolor scan kml files where data is drawn as wedge shapes.

    Usage example::

        obj = madPcolorWedgeKmlScan(isprintText,
                        'az',
                        './isprint.kml',
                        minColormap = 9,
                        maxColormap = 12)    

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec 15, 2010
    """


    def __init__(self, isprintText,
                 scanType,
                 fullFilename,
                 minColormap = None,
                 maxColormap = None,
                 instName = None,
                 instDesc = None,
                 instLat = None,
                 instLon = None,
                 instAlt = None,
                 colorMap = None):
        """__init__ writes a madPcolorWedgeKmlScan to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. The 9 parameters
                are (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2, <parameter value>).  Any
                missing data should be written as "missing" or other string that
                cannot be converted to a float. gdlatr, gdlonr, galtr are the station location

            scanType - must be 'az' or 'el_lat' (for north or south el scans), 'el_lon'
                (for east or west el scans) or 'el_gcdist' (for all other el scans.
                For az - x is long, y is lat
                for el_* y is always alt, and x is either lat, lon, or great circle distance

            fullFilename - full path of file containing kml file to be saved.  Extension must
                           be kml, or exception thrown.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.
                          
                          
            instName, instDesc, instLat, instLon, instAlt - used to create a placemark for the radar.  If
                instLat or instLon missing, no placemark added.
                
            colorMap - A matplotlib colormap to use to create colors.  If None, then blue minimum and red maximum

        Returns: void

        Affects: Writes fullFilename to disk
        """

        # verify input
        if scanType not in ('az', 'el_lat', 'el_lon', 'el_gcdist'):
            raise ValueError('scanType must be "az", "el_lat", "el_lon", "el_gcdist", not %s' % (str(scanType)))

        isprintLines = isprintText.split('\n')
        
        newLonMax, newLonMin, newLatMax, newLatMin, newAltMax, newGreatCDist, newParmMax, newParmMin = self._getLimits(isprintLines)
        
        # get look point
        lookLat = (newLatMax + newLatMin)/2.0
        lookLon = (newLonMax + newLonMin)/2.0
        lookAlt = newAltMax * 10.0
        
        if maxColormap == None:
            maxColormap = newParmMax
        if minColormap == None:
            minColormap = newParmMin
            
        
        # set up wedge generator
        dataGen = self._generateWedges(isprintLines, scanType)
        
        text = self._createKmlText(dataGen, maxColormap, minColormap, lookLat, lookLon, lookAlt, scanType,
                                   instName, instDesc, instLat, instLon, instAlt, colorMap)
        
        with open(fullFilename, 'w') as f:
            f.write(text)
        
        
        
    def getGreatCircleDist(self, gdlatr, glonr, gdlat, glon):
        """getGreatCircleDist return the great circle distance in.  Taken from madDeriveMethods c code.
        
        Inputs: gdlatr, glonr: reference point on earth's surface
                gdlat, glon: second point on earth's surface
        """
        dlon = glonr/57.2958 - glon/57.2958
        dlat = gdlatr/57.2958 - gdlat/57.2958;
        a = math.sin(dlat/2.0)*math.sin(dlat/2.0) + math.cos(gdlatr/57.2958)*math.cos(gdlat/57.2958) * math.sin(dlon/2.0)*math.sin(dlon/2.0)
    
        if math.sqrt(a) < 1.0:
            c = 2.0 * math.asin(math.sqrt(a))
        else:
            c = 2.0 * math.asin(1.0)
    
        return(c * 6371.2)
        
        
    def _getLimits(self, isprintLines):
        """returns (newLonMax, newLonMin, newLatMax, newLatMin, newAltMax, newGreatCDist, newParmMax, newParmMin) 
        given isprint lines with
        parameters (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2, <parameter value>)
        
        We only look at the end points of ranges and the station location
        """
        # set initial values to station location
        items = isprintLines[1].split()
        slat = float(items[0])
        slon = float(items[1])
        if slon > 180.0:
            slon -= 360.0
        salt = float(items[2])
        maxLon = slon
        minLon = slon
        maxLat = slat
        minLat = slat
        maxAlt = salt
        maxGreatCDist = 0.0
        maxParm = -1.0e-30
        minParm = 1.0e30
        
        lastAz = None
        lastEl = None
        lastRange = None
        
        for i, isprintLine in enumerate(isprintLines):
            items = isprintLine.split()
            if len(items) < 9:
                continue
            range = float(items[3])
            az1 = float(items[4])
            az2 = float(items[5])
            thisAz = (az1 + az2)/2
            el1 = float(items[6])
            el2 = float(items[7])
            thisEl = (el1 + el2)/2
            try:
                parm = float(items[8])
                if parm > maxParm:
                    maxParm = parm
                if parm < minParm:
                    minParm = parm
            except ValueError:
                continue
            
            # see if we're at an end point
            if lastAz != None:
                if abs(lastAz - thisAz) > 0.001 or abs(lastEl - thisEl) > 0.001:
                    # the last point was an end point
                    gdlat,glon,gdalt = madrigal._derive.radarToGeodetic(slat,slon,salt,lastAz,lastEl,lastRange)
                    if gdlat > maxLat:
                        maxLat = gdlat
                    if gdlat < minLat:
                        minLat = gdlat
                    if glon > maxLon:
                        maxLon = glon
                    if glon < minLon:
                        minLon = glon
                    if gdalt > maxAlt:
                        maxAlt = gdalt
                    greatCDist = self.getGreatCircleDist(slat, slon, gdlat, glon)
                    if greatCDist > maxGreatCDist:
                        maxGreatCDist = greatCDist
                        
            lastAz = thisAz
            lastEl = thisEl
            lastRange = range
            
        # check last point
        gdlat,glon,gdalt = madrigal._derive.radarToGeodetic(slat,slon,slat,lastAz,lastEl,lastRange)
        if gdlat > maxLat:
            maxLat = gdlat
        if gdlat < minLat:
            minLat = gdlat
        if glon > maxLon:
            maxLon = glon
        if glon < minLon:
            minLon = glon
        if gdalt > maxAlt:
            maxAlt = gdalt
        greatCDist = self.getGreatCircleDist(slat, slon, gdlat, glon)
        if greatCDist > maxGreatCDist:
            maxGreatCDist = greatCDist
            
        return((maxLon, minLon, maxLat, minLat, maxAlt, maxGreatCDist, maxParm, minParm))
    
    
    
    def _generateWedges(self, isprintLines, scanType):
        """_generateWedges is a python generator that return a data wedge in the form
        (dataValue, (point1_lat, point1_lon, point1_alt), (point2_lat, point2_lon, point2_alt), 
        (point3_lat, point3_lon, point3_alt), (point4_lat, point4_lon, point4_alt))
        
        Inputs:
        
            isprintLines - a list of line with 9 parameters that
                are (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2, <parameter value>).  Any
                missing data should be written as "missing" or other string that
                cannot be converted to a float. gdlatr, gdlonr, galtr are the station location.
                No wedge is returned for a missing data line
                
            scanType - must be 'az' or 'el_lat' (for north or south el scans), 'el_lon'
                (for east or west el scans) or 'el_gcdist' (for all other el scans.
                For az - x is long, y is lat
                for el_* y is always alt, and x is either lat, lon, or great circle distance
        """
        # state variables
        lastAz1 = None
        lastAz2 = None
        lastEl1 = None
        lastEl2 = None
        lastRange = None
        lastDiffRangeLow = None
        lastValue = None
        
        for isprintLine in isprintLines:
            items = isprintLine.split()
            if len(items) < 9:
                continue
            gdlatr = float(items[0])
            gdlonr = float(items[1])
            galtr = float(items[2])
            range = float(items[3])
            az1 = float(items[4])
            az2 = float(items[5])
            el1 = float(items[6])
            el2 = float(items[7])
            valueStr = items[8]
            if lastAz1 != None and ( abs(lastAz1-az1) > 0.001 or abs(lastEl1-el1) > 0.001 ):
                # new radar line found, send last point if needed
                if lastValue != None:
                    dataWedge = self._getWedge(gdlatr, gdlonr, galtr, lastRange, 
                                               lastDiffRangeLow, lastDiffRangeLow, lastAz1, lastAz2,
                                               lastEl1, lastEl2, lastValue)
                    yield(dataWedge)
                # this is the first point in the line, so no lastDiffRangeLow
                lastDiffRangeLow = None
            elif lastAz1 != None and ( abs(lastAz1-az1) < 0.001 and abs(lastEl1-el1) < 0.001 ):
                # continuing radar line
                if lastValue != None:
                    lastDiffRangeLow = (range - lastRange)/2.0
                    dataWedge = self._getWedge(gdlatr, gdlonr, galtr, lastRange, 
                                               lastDiffRangeLow, (range-lastRange)/2.0, lastAz1, lastAz2,
                                               lastEl1, lastEl2, lastValue)
                    yield(dataWedge)
                else:
                    lastDiffRangeLow = (range - lastRange)/2.0
            # reset state variables
            lastAz1 = az1
            lastAz2 = az2
            lastEl1 = el1
            lastEl2 = el2
            lastRange = range
            try:
                lastValue = float(valueStr)
            except:
                lastValue = None
                
        # finally, send last wedge if needed
        if lastValue != None:
            dataWedge = self._getWedge(gdlatr, gdlonr, galtr, lastRange, 
                                       lastDiffRangeLow, lastDiffRangeLow, lastAz1, lastAz2,
                                       lastEl1, lastEl2, lastValue)
            yield(dataWedge)
            


    def _getWedge(self, gdlatr, gdlonr, galtr, range,
                  diffRange1, diffRange2, az1, az2,
                  el1, el2, value):
        """_getWedge returns return a data wedge in the form
        (dataValue, (point1_lat, point1_lon, point1_alt), (point2_lat, point2_lon, point2_alt), 
        (point3_lat, point3_lon, point3_alt), (point4_lat, point4_lon, point4_alt)).  

            
        Inputs:
            gdlatr, gdlonr, galtr - location of radar
            range, diffRange1, diffRange2 - range of center of wedge, and distance in and out from there
            az1, az2, el1, el2 - start and end aximuth and elevation.  Which is used depends on scanType
            value - float data value
        """
        point1_lat,point1_lon,point1_alt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range-diffRange1)
        point2_lat,point2_lon,point2_alt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az2,el2,range-diffRange1)
        point3_lat,point3_lon,point3_alt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az2,el2,range+diffRange2)
        point4_lat,point4_lon,point4_alt = madrigal._derive.radarToGeodetic(gdlatr, gdlonr, galtr,az1,el1,range+diffRange2)
            
        return((value, (point1_lat,point1_lon,point1_alt), (point2_lat,point2_lon,point2_alt), 
                (point3_lat,point3_lon,point3_alt), (point4_lat,point4_lon,point4_alt)))
        
        
        
    def _createKmlText(self, dataGen, maxColormap, minColormap, lookLat, lookLon, lookAlt, scanType,
                       instName, instDesc, instLat, instLon, instAlt, colorMap):
        """_createKmlText creates kml text
        
        Inputs:
        
            dataGen - list in form (dataValue, (point1_lat, point1_lon, point1_alt), (point2_lat, point2_lon, point2_alt), 
                                    (point3_lat, point3_lon, point3_alt), (point4_lat, point4_lon, point4_alt))
            maxColormap 
            minColormap
            lookLat
            lookLon
            lookAlt
            scanType
            instName, instDesc, instLat, instLon, instAlt - - used to create a placemark for the radar.  If
                instLat or instLon missing, no placemark added.
            colorMap - A matplotlib colormap to use to create colors.  If None, then blue minimum and red maximum
        """
        
        # need to fill in id and color
        styleTemplate = """
        <Style id="p%i">
            <PolyStyle>
                <color>%s</color>
                <fill>1</fill>
                <outline>0</outline>
            </PolyStyle>
        </Style>
        """
        
        # need to fill in id and coordinates
        placemarkTemplate = """
        <Placemark>
            <styleUrl>#%s</styleUrl>
            <Polygon>
                <altitudeMode>absolute</altitudeMode>
                <outerBoundaryIs>
                    <LinearRing>
                        <coordinates> %s </coordinates>
                    </LinearRing>
                </outerBoundaryIs>
            </Polygon>
        </Placemark>
        """
        
        instTemplate = """<Placemark>
              <name>%s</name>
              <description>%s</description>
              <Point>
                <coordinates>%f,%f,%f</coordinates>
              </Point>
        </Placemark>
        """
        
        # need to fill in styles and placemarks
        documentTemplate = """<?xml version="1.0" encoding="UTF-8"?>
        <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
        <Document>
        <name>ScanKmlFile</name>
            <LookAt>
                <longitude>%f</longitude>
                <latitude>%f</latitude>
                <altitude>%f</altitude>
                <heading>%f</heading>
                <tilt>50</tilt>
                <range>%f</range>
                <altitudeMode>relativeToGround</altitudeMode>
            </LookAt>
            
        %s
        %s
        %s
        </Document>
        </kml>
        """
        
        retStr  = ''
        
        styleStr = ''
        
        # create 100 colors
        for i in range(100):
            if not colorMap:
                colorStr = self._getKmlRGB(i/100.0)
            else:
                matLabColorStr = matplotlib.colors.rgb2hex(colorMap(i/100.0)[:3])
                red = matLabColorStr[1:3]
                green = matLabColorStr[3:5]
                blue = matLabColorStr[5:7]
                colorStr = 'ff' + blue + green + red # this is the google earth standard for opaque color
            styleStr += styleTemplate % (i, colorStr)
            
        placemarkStr = ''
        # loop through data adding strings to placemark string
        while(True):
            try:
                p = next(dataGen)
            except StopIteration:
                break
            data = p[0]
            if data > maxColormap:
                styleIndex = 99
            elif data < minColormap:
                styleIndex = 0
            else:
                styleIndex = int(99.0 * ((data-minColormap)/(maxColormap-minColormap)))

            styleId = 'p%i' % (styleIndex)
            coordStr = ('%f,%f,%f %f,%f,%f %f,%f,%f %f,%f,%f %f,%f,%f' ) % \
                        (p[1][1], p[1][0], p[1][2]*1000.0,
                         p[2][1], p[2][0], p[2][2]*1000.0,
                         p[3][1], p[3][0], p[3][2]*1000.0,
                         p[4][1], p[4][0], p[4][2]*1000.0,
                         p[1][1], p[1][0], p[1][2]*1000.0)
            placemarkStr += placemarkTemplate % (styleId, coordStr)
            
            
        if scanType in ('el_lat'):
            heading = 90.0
            distance = 200.0
        elif scanType in ('el_lon', 'el_gcdist'):
            heading = 0.0
            distance = 200.0
        else:
            heading = 0.0
            distance = 1000.0
            
            
        # create instStr
        instStr = ''
        if instLat != None and instLon != None:
            if instAlt == None:
                instAlt = 0.0
            if instName == None:
                instName = 'Radar'
            if instDesc == None:
                instDesc = 'This is the location of the radar that produced this scan.'
            instStr += instTemplate % (instName, instDesc, instLon, instLat, instAlt)
        
        
        retStr += documentTemplate % (lookLon, lookLat, lookAlt*100.0, heading, lookAlt*distance, styleStr, instStr, placemarkStr)
        
        return(retStr)
    
    
    def _getKmlRGB(self, value, transparent=False, wrap=False):
        """_getKmlRGB returns a Kml color string given a value between 0 qnd 1.
        
        If transparent == False, solid color.  Otherwise transpanency set to 50%
        
        If wrap, colors at zero and 1 wrap.  Otherwise, extremes red and blue
        
        If invert, use value = 1 - value
        """
        if value < 0.0 or value > 1.0:
            raise ValueError('value must be between 0 and 1, not %f' % (value))
        
        if wrap:
            r,g,b = colorsys.hsv_to_rgb(value, 0.9, 0.9)
        else:
            r,g,b = colorsys.hsv_to_rgb(value*0.67, 0.9, 0.9)
        
        if not transparent:
            transStr = 'ff'
        else:
            transStr = '80'
        
        retStr = '%s%s%s%s' % (transStr,
                               hex(int(r*255))[-2:],
                               hex(int(g*255))[-2:],
                               hex(int(b*255))[-2:])
        
        return(retStr)
    
            

class scanPlotter:
    """scanPlotter is the class that produces a series of scan plots for a single Madrigal file

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 26, 2006
    """
    def __init__(self, madFile,
                 madDBObj=None):
        """__init__ initializes a scanPlotter object.

        Inputs:

            madFile - the Madrigal file to be analyzed.  Must contain SCNTYP and CYCN parameters.

            madDBObj - a madrigal.metadata.MadrigalDB object.  If None (default),
            one created

	              
        Returns: void

        Affects: sets self.madFile, self.madDBObj
        """
        if os.access(madFile, os.R_OK):
            self.madFile = madFile
        else:
            raise IOError('unable to read %s' % (str(madFile)))

        if madDBObj == None:
            self.madDBObj = madrigal.metadata.MadrigalDB()
        else:
            self.madDBObj = madDBObj


    def plotAllScans(self,
                     scanType,
                     fullFilenameTemplate,
                     plotParm,
                     size = 'small',
                     xMinimum = None,
                     xMaximum = None,
                     yMinimum = None,
                     yMaximum = None,
                     xGridSize = None,
                     yGridSize = None,
                     minColormap = None,
                     maxColormap = None,
                     colorMap = matplotlib.cm.jet,
                     maxNumLines = None,
                     filterStr = ''):
        """plotAllScans creates a series of az or el scans.

        Inputs:

            scanType - must be 'az' or 'el'

            fullFilename - full path of file containing pcolor plot to be saved. Each image
            created will have _#.png appended, with # starting at 0

            plotParm - mnemonic of parameter to be plotted.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.

            xMinimum = minumum x value.  If None (default), uses lowest x value found for each scan.

            xMaximum = maximum x value.  If None (default), uses highest x value found for each scan.

            yMinimum = minumum y value.  If None (default), uses lowest y value found for each scan.

            yMaximum = maximum y value.  If None (default), uses highest y value found for each scan.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.

            colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

    	    maxNumLine - max number of lines in isprintText before truncating.  If None, no truncation
    	    
    	    filterStr - a filter string to pass to isprint.  Defaults to no filtering
    
    	Returns - a list of tuples, where each tuple has two items: 1. time string,
    	          2. metadata string in form for scanTab.txt.  List length = number
    	          of plots created
	"""
        
        if scanType not in ('az', 'el'):
            raise ValueError('scantype must be az or el, not %s' % (str(scanType)))

        self.scanType = scanType

        retList = []

        # handle ion velocity
        if plotParm.lower() == 'vo':
            cmap = madrigal.ui.madrigalPlot.get_vo_cmap()
        else:
            cmap = matplotlib.cm.jet

        # create isprint string
        parms = 'ut1,scntyp,cycn,%s,gdalt,gdlat,glon,azm,elm,gcdist' % (plotParm.strip())
        isprintStr = getIsprintString(self.madFile, parms, filterStr)

        try:
            scanList = self.createScanInfoList(isprintStr)
        except:
            traceback.print_exc()
            return (retList)

        # loop through each scan
        scanFailureCount = 0 # keep track of how many scans fail
        for i in range(len(scanList)):
            scanCount = i - scanFailureCount
            scanInfo = scanList[i]
            try:
                startDateStr = self.getDateStrFromUT(scanInfo[1])
            except:
                # no scans may have been found
                continue
            endDateStr = self.getTimeStrFromUT(scanInfo[4])
            # create title
            
            if scanType == 'el':
                if scanInfo[3] > scanInfo[6]:
                    ind = 'down'
                    direction = 1
                else:
                    ind = 'up'
                    direction = 0
                titleStr = 'El %s scan (%s) %s-%s, az=%i' % (plotParm,
                                                             ind,
                                                             startDateStr,
                                                             endDateStr,
                                                             int(scanInfo[2]))
                timeStr = '%s-%s' % (startDateStr, endDateStr)
                metaStr = 'El scan: startTime %i el %f az %f stopTime %i el %f az %f dir %i' % (scanInfo[1],
                                                                                                scanInfo[3],
                                                                                                scanInfo[2],
                                                                                                scanInfo[4],
                                                                                                scanInfo[6],
                                                                                                scanInfo[5],
                                                                                                direction)
                retList.append((timeStr,metaStr))
                
                if scanInfo[7] == 'Gdlat':
                    xLabelStr = 'Geodetic latitude'
                    xGridSize = 1.0
                elif scanInfo[7] == 'Glon':
                    xLabelStr = 'Longitude'
                    xGridSize = 1.0
                elif scanInfo[7] == 'Gcdist':
                    xLabelStr = 'Ground distance in km'
                    xGridSize = 100.0

                xLabelStr = ''
                yLabelStr = 'Altitude (km)'
                if xGridSize == None:
                    xGridSize = 1.0
                if yGridSize == None:
                    yGridSize = 20.0
                
            else:
                if scanInfo[2] > scanInfo[5]:
                    ind = 'ccw'
                    direction = 1
                else:
                    ind = 'cw'
                    direction = 0
                titleStr = 'Az %s scan (%s) %s-%s, el=%i' % (plotParm,
                                                             ind,
                                                             startDateStr,
                                                             endDateStr,
                                                             int(scanInfo[3]))
                timeStr = '%s-%s' % (startDateStr, endDateStr)
                metaStr = 'Az scan: startTime %i az %f el %f stopTime %i az %f el %f dir %i' % (scanInfo[1],
                                                                                                scanInfo[2],
                                                                                                scanInfo[3],
                                                                                                scanInfo[4],
                                                                                                scanInfo[5],
                                                                                                scanInfo[6],
                                                                                                direction)

                retList.append((timeStr,metaStr))

                xLabelStr = 'Longitude'
                yLabelStr = 'Geodetic latitude'
                xGridSize = 1.0
                yGridSize = 1.0

            name = '%s_%06i.png' % (fullFilenameTemplate, scanCount)

            try:
                madPcolorScan(scanInfo[0],
                              xGridSize,
                              yGridSize,
                              titleStr,
                              xLabelStr,
                              yLabelStr,
                              name,
                              minColormap = minColormap,
                              maxColormap = maxColormap,
                              colorMap = cmap)
            except:
                print('Problem creating scan %s' % (str(retList[-1])))
                traceback.print_exc()
                scanFailureCount += 1
                retList.remove(retList[-1])

        return (retList)
    
    
    def plotAllWedgeScans(self,
                     scanType,
                     fullFilenameTemplate,
                     plotParm,
                     size = 'small',
                     xMinimum = None,
                     xMaximum = None,
                     yMinimum = None,
                     yMaximum = None,
                     minColormap = None,
                     maxColormap = None,
                     colorMap = matplotlib.cm.jet,
                     maxNumLines = None,
                     filterStr = '',
                     addTitle = '',
                     includeKml = False,
                     radarName = None,
                     radarDesc = None):
        """plotAllWedgeScans creates a series of az or el scans.  Similar to plotAllScans, except produces
        fancier wedge plots with uniform scalling.

        Inputs:

            scanType - must be 'az' or 'el'

            fullFilename - full path of file containing pcolor plot to be saved. Each image
            created will have _#.png appended, with # starting at 0

            plotParm - mnemonic of parameter to be plotted.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.

            xMinimum = minumum x value.  If None (default), uses lowest x value found for each scan.

            xMaximum = maximum x value.  If None (default), uses highest x value found for each scan.

            yMinimum = minumum y value.  If None (default), uses lowest y value found for each scan.

            yMaximum = maximum y value.  If None (default), uses highest y value found for each scan.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.

            colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

            maxNumLine - max number of lines in isprintText before truncating.  If None, no truncation
            
            filterStr - a filter string to pass to isprint.  Defaults to no filtering
            
            addTitle - a string with additional title.  If empty string (the default), no additional title string.
            
            includeKml - if True, create a corresponding kml file for every png file.  Same names
                    as png files, except extension = .kml.  If False, do not.
                    
            radarName - name to label radar placemark if kml generated.
            
            radarDesc - description text for radar placemark if kml generated
    
        Returns - a list of tuples, where each tuple has two items: 1. time string,
                  2. metadata string in form for scanTab.txt.  List length = number
                  of plots created
    """
        
        if scanType not in ('az', 'el'):
            raise ValueError('scantype must be az or el, not %s' % (str(scanType)))

        self.scanType = scanType
        
        if scanType == 'az':
            wedgeScanType = 'az'

        retList = []

        # handle ion velocity
        if plotParm.lower() == 'vo':
            cmap = madrigal.ui.madrigalPlot.get_vo_cmap()
        else:
            cmap = matplotlib.cm.jet

        # create isprint string
        parms = 'ut1,scntyp,cycn,%s,gdalt,gdlat,glon,azm,elm,gcdist,gdlatr,gdlonr,galtr,range,az1,az2,el1,el2' % (plotParm.strip())

        filterStr += ' filter=%s,, ' % (plotParm.strip())
        isprintStr = getIsprintString(self.madFile, parms, filterStr)

        try:
            scanList = self.createWedgeScanInfoList(isprintStr)
        except:
            traceback.print_exc()
            return (retList)
        
        gdlatr, gdlonr, galtr = self._getInstLocation(isprintStr)
        
        # create limit dictionary used to keep all scans of the same type with the same limits
        limitDict = self._getLimits(scanList, xMinimum, xMaximum, yMinimum, yMaximum)

        # loop through each scan
        scanFailureCount = 0 # keep track of how many scans fail
        for i in range(len(scanList)):
            scanCount = i - scanFailureCount
            scanInfo = scanList[i]
            try:
                startDateStr = self.getDateStrFromUT(scanInfo[1])
            except:
                # no scans may have been found
                continue
            endDateStr = self.getTimeStrFromUT(scanInfo[4])
            # create title
            
            if scanType == 'el':
                if scanInfo[3] > scanInfo[6]:
                    ind = 'down'
                    direction = 1
                else:
                    ind = 'up'
                    direction = 0
                if int(scanInfo[2]) == int(scanInfo[5]):
                    # no change in az
                    titleStr = 'El %s scan (%s) %s-%s, az=%i %s' % (plotParm,
                                                                    ind,
                                                                    startDateStr,
                                                                    endDateStr,
                                                                    int(scanInfo[2]),
                                                                    addTitle)
                else:
                    # az changed
                    titleStr = 'El (%i-%i) %s scan %s-%s, az=%i-%i %s' % (int(scanInfo[3]),
                                                                          int(scanInfo[6]),
                                                                          plotParm,
                                                                          startDateStr,
                                                                          endDateStr,
                                                                          int(scanInfo[2]),
                                                                          int(scanInfo[5]),
                                                                          addTitle)
                    
                timeStr = '%s-%s' % (startDateStr, endDateStr)
                metaStr = 'El scan: startTime %i el %f az %f stopTime %i el %f az %f dir %i' % (scanInfo[1],
                                                                                                scanInfo[3],
                                                                                                scanInfo[2],
                                                                                                scanInfo[4],
                                                                                                scanInfo[6],
                                                                                                scanInfo[5],
                                                                                                direction)
                retList.append((timeStr,metaStr))
                
                if scanInfo[7] == 'Gdlat':
                    xLabelStr = 'Geodetic latitude'
                    wedgeScanType = 'el_lat'
                elif scanInfo[7] == 'Glon':
                    xLabelStr = 'Longitude'
                    wedgeScanType = 'el_lon'
                elif scanInfo[7] == 'Gcdist':
                    xLabelStr = 'Ground distance in km'
                    wedgeScanType = 'el_gcdist'

                xLabelStr = ''
                yLabelStr = 'Altitude (km)'
                
            else:
                if scanInfo[2] > scanInfo[5]:
                    ind = 'ccw'
                    direction = 1
                else:
                    ind = 'cw'
                    direction = 0
                titleStr = 'Az %s scan (%s) %s-%s, el=%i %s' % (plotParm,
                                                             ind,
                                                             startDateStr,
                                                             endDateStr,
                                                             int(scanInfo[3]),
                                                             addTitle)
                timeStr = '%s-%s' % (startDateStr, endDateStr)
                metaStr = 'Az scan: startTime %i az %f el %f stopTime %i az %f el %f dir %i' % (scanInfo[1],
                                                                                                scanInfo[2],
                                                                                                scanInfo[3],
                                                                                                scanInfo[4],
                                                                                                scanInfo[5],
                                                                                                scanInfo[6],
                                                                                                direction)

                retList.append((timeStr,metaStr))

                xLabelStr = 'Longitude'
                yLabelStr = 'Geodetic latitude'

            name = '%s_%06i.png' % (fullFilenameTemplate, scanCount)

            try:
                madPcolorWedgeScan(scanInfo[0],
                                   wedgeScanType,
                                   titleStr,
                                   xLabelStr,
                                   yLabelStr,
                                   name,
                                   xMinimum = limitDict[wedgeScanType][0],
                                   xMaximum = limitDict[wedgeScanType][1],
                                   yMinimum = limitDict[wedgeScanType][2],
                                   yMaximum = limitDict[wedgeScanType][3],
                                   minColormap = minColormap,
                                   maxColormap = maxColormap,
                                   colorMap = cmap)
                success = True
            except:
                print('Problem creating scan %s' % (str(retList[-1])))
                traceback.print_exc()
                scanFailureCount += 1
                retList.remove(retList[-1])
                success = False
                
            if includeKml and success:
                kmlName = name[:-3] + 'kml'
                try:
                    madPcolorWedgeKmlScan(scanInfo[0],
                                          wedgeScanType,
                                          kmlName,
                                          minColormap = minColormap,
                                          maxColormap = maxColormap,
                                          instName = radarName,
                                          instDesc = radarDesc,
                                          instLat = gdlatr,
                                          instLon = gdlonr,
                                          instAlt = galtr,
                                          colorMap = cmap)
                except:
                    print('Problem creating kml scan')
                    traceback.print_exc()
                                          
                

        return (retList)
    



    def getDateStrFromUT(self, ut):
        """getDateStrFromUT returns a date string formated as YYYY-MM-DD HH:MM:SS from a ut time
        (seconds since 1/1/1950)
        """
        timeList = madrigal._derive.getDateFromUt(float(ut))
        return '%04i-%02i-%02i %02i:%02i:%02i' % (timeList[0],
                                                  timeList[1],
                                                  timeList[2],
                                                  timeList[3],
                                                  timeList[4],
                                                  timeList[5])


    def getTimeStrFromUT(self, ut):
        """getTimeStrFromUT returns a time string formated as HH:MM:SS from a ut time
        (seconds since 1/1/1950)
        """
        timeList = madrigal._derive.getDateFromUt(float(ut))
        return '%02i:%02i:%02i' % (timeList[3],
                                   timeList[4],
                                   timeList[5])



    def createScanInfoList(self, isprintStr):
        """createScanInfoList creates a list of tuples, which each tuple representing a single scan, and
        values of:

        (isprintString, startUT, startAz, startEl, endUT, endAz, endEl, type,
        minGdlat, maxGdlat,minGlon, maxGdlon,minGdalt, maxGdalt, minGcdist, maxGcdist).  Isprint string
        has values (x,y,plotParm), where for az scan x=lon, y=lat, and for el scan y=alt, x=lat if starting
        az within 15 degrees of north or south, x=lon if starting az within 15 degrees of east or west,
        of x=gcdist if other az. Type is Gdlat or Glon or Gcdist for el scans, None for az scans.

        Input isprint string has parameters ut1,scntyp,cycn,<parm>,gdalt,gdlat,glon,azm,elm,gcdist

        Azimuth scans are split whenever direction or elevation changes.  For elevation scans, there will be zero or one 
        north-south scans, zero or one east-west scans, and zero or more off azimuth scans.  An off azimuth
        scan is not within 15 degrees of north, south, east or west.  Off azimuth scans are not combined with
        scans 180 degrees in the other direction, because the x axis is ground distance, which does not reverse
        sign.
        """
        isprintItems = isprintStr.split()

        retList = []

        # loop variables
        thisIsprintStr = []
        cycNum = None
        startUT = []
        startAz = []
        startEl = []
        endUT = []
        endAz = []
        endEl = []
        elScan = []
        minGdlat = []
        maxGdlat = []
        minGlon = []
        maxGlon = []
        minGdalt = []
        maxGdalt = []
        minGcdist = []
        maxGcdist = []
        thisAzDirection = None # used to detect new azimuth scans
        lastAzDirection = None # used to detect new azimuth scans
        lastEl = None # used to detect new azimuth scans
        thisAz = None
        lastUt = None # keep track of new ut
        isNewUt = None # true only when ut changes
        inAzScan = False
        

        for i in range(len(isprintItems)):
            item = isprintItems[i]
            mod = i % 10
            if mod == 0:
                thisUt = float(item)
                if lastUt == None:
                    lastUt = thisUt
            elif mod == 1:
                thisScntyp = int(item)
            elif mod == 2:
                thisCycn = int(float(item))
                if cycNum == None:
                    cycNum = thisCycn
            elif mod == 3:
                thisParm = item
            elif mod == 4:
                thisGdalt = float(item)
            elif mod == 5:
                thisGdlat = float(item)
            elif mod == 6:
                thisGlon = float(item)
            elif mod == 7:
                if thisAz != None and lastUt < thisUt and inAzScan:
                    if thisAz > float(item):
                        thisAzDirection = 'cw'
                    elif thisAz < float(item):
                        thisAzDirection = 'ccw'
                thisAz = float(item)
            elif mod == 8:
                thisEl = float(item)
            elif mod == 9:
                thisGcdist = float(item)
                
                # check whether this is a new time
                if lastUt < thisUt:
                    isNewUt = True
                    lastUt = thisUt
                else:
                    isNewUt = False

                # if new cycle, close out value if data exists
                if thisCycn != cycNum:
                    for i in range(len(thisIsprintStr)):
                        if len(thisIsprintStr[i]) == 0:
                            continue
                        retList.append((thisIsprintStr[i],
                                        startUT[i], startAz[i], startEl[i], endUT[i], endAz[i], endEl[i], elScan[i],
                                        minGdlat[i], maxGdlat[i], minGlon[i], maxGlon[i], minGdalt[i], maxGdalt[i],
                                        minGcdist[i], maxGcdist[i]))
                    thisIsprintStr = []
                    cycNum = None
                    startUT = []
                    startAz = []
                    startEl = []
                    endUT = []
                    endAz = []
                    endEl = []
                    elScan = []
                    minGdlat = []
                    maxGdlat = []
                    minGlon = []
                    maxGlon = []
                    minGdalt = []
                    maxGdalt = []
                    minGcdist = []
                    maxGcdist = []
                    thisAzDirection = None
                    lastAzDirection = None
                    lastEl = None
                    inAzScan = False

                # end of line - process it
                if self.scanType == 'el' and thisScntyp == 3:
                    # add this elevation scan point
                    # check type of el scan
                    inAzScan = False
                    if -15.0 <= thisAz <= 15.0 or -165.0 >= thisAz or thisAz >= 165.0:
                        thisElScan = 'Gdlat'
                    elif 75.0 <= thisAz <= 105.0 or -105.0 <= thisAz <= -75.0:
                        thisElScan = 'Glon'
                    else:
                        thisElScan = 'Gcdist'
                    # get index of this scan, None if a new scan
                    index = None
                    for i, item in enumerate(elScan):
                        if item == 'Gdlat' and thisElScan == 'Gdlat':
                            index = i
                            break
                        elif item == 'Glon' and thisElScan == 'Glon':
                            index = i
                            break
                        elif item == 'Gcdist' and thisElScan == 'Gcdist' and int(thisAz) == int(startAz[i]):
                            index = i
                            break
                    if index == None:
                        # a new scan has been found
                        thisIsprintStr.append('')
                        startUT.append(thisUt)
                        startAz.append(thisAz)
                        startEl.append(thisEl)
                        endUT.append(None)
                        endAz.append(None)
                        endEl.append(None)
                        elScan.append(thisElScan)
                        minGdlat.append(None)
                        maxGdlat.append(None)
                        minGlon.append(None)
                        maxGlon.append(None)
                        minGdalt.append(None)
                        maxGdalt.append(None)
                        minGcdist.append(None)
                        maxGcdist.append(None)
                        index = len(thisIsprintStr) - 1
                            
                    endUT[index] = thisUt
                    endAz[index] = thisAz
                    endEl[index] = thisEl
                    if thisElScan == 'Gdlat':
                        thisIsprintStr[index] += '%f %f %s\n' % (thisGdlat, thisGdalt, thisParm)
                    elif thisElScan == 'Glon':
                        thisIsprintStr[index] += '%f %f %s\n' % (thisGlon, thisGdalt, thisParm)
                    else:
                        thisIsprintStr[index] += '%f %f %s\n' % (thisGcdist, thisGdalt, thisParm)

                elif self.scanType == 'az' and thisScntyp == 2:
                    # az scan
                    inAzScan = True
                    # add this azimuth scan point
                    # check if this is a new scan
                    newAzScan = False
                    if lastEl == None:
                        newAzScan = True
                    # check for elevation change
                    elif abs(lastEl - int(thisEl)) > 3:
                        newAzScan = True
                    # check for direction change
                    if lastAzDirection != None:
                        if lastAzDirection != thisAzDirection:
                            newAzScan = True
                            thisAzDirection = None
                    if newAzScan:
                        # a new azimuth scan has been found
                        thisIsprintStr.append('')
                        startUT.append(thisUt)
                        startAz.append(thisAz)
                        startEl.append(thisEl)
                        endUT.append(None)
                        endAz.append(None)
                        endEl.append(None)
                        elScan.append(None)
                        minGdlat.append(None)
                        maxGdlat.append(None)
                        minGlon.append(None)
                        maxGlon.append(None)
                        minGdalt.append(None)
                        maxGdalt.append(None)
                        minGcdist.append(None)
                        maxGcdist.append(None)
                        index = len(thisIsprintStr) - 1
                        lastEl = int(thisEl)
                        lastAzDirection = None
                    else:
                        # this is at least the second line of the scan
                        if not thisAzDirection is None:
                            lastAzDirection = thisAzDirection
                            
                    endUT[-1] = thisUt
                    endAz[-1] = thisAz
                    endEl[-1] = thisEl
                    thisIsprintStr[-1] += '%f %f %s\n' % (thisGlon, thisGdlat, thisParm)
                    
                else:
                    # not a scan line
                    inAzScan = False
                    continue

                if minGdlat[index] == None:
                    minGdlat[index] = thisGdlat
                    maxGdlat[index] = thisGdlat
                    minGlon[index] = thisGlon
                    maxGlon[index] = thisGlon
                    minGdalt[index] = thisGdalt
                    maxGdalt[index] = thisGdalt
                    minGcdist[index] = thisGcdist
                    maxGcdist[index] = thisGcdist
                else:
                    if minGdlat[index] > thisGdlat:
                        minGdlat[index] = thisGdlat
                    if maxGdlat[index] < thisGdlat:
                        maxGdlat[index] = thisGdlat
                    if minGlon[index] > thisGlon:
                        minGlon[index] = thisGlon
                    if maxGlon[index] < thisGlon:
                        maxGlon[index] = thisGlon
                    if minGdalt[index] > thisGdalt:
                        minGdalt[index] = thisGdalt
                    if maxGdalt[index] < thisGdalt:
                        maxGdalt[index] = thisGdalt
                    if minGcdist[index] > thisGcdist:
                        minGcdist[index] = thisGcdist
                    if maxGcdist[index] < thisGcdist:
                        maxGcdist[index] = thisGcdist

        # end, close out last cycle
        for i in range(len(thisIsprintStr)):
            if len(thisIsprintStr[i]) == 0:
                continue
            retList.append((thisIsprintStr[i],
                            startUT[i], startAz[i], startEl[i], endUT[i], endAz[i], endEl[i], elScan[i],
                            minGdlat[i], maxGdlat[i], minGlon[i], maxGlon[i], minGdalt[i], maxGdalt[i],
                            minGcdist[i], maxGcdist[i]))

        return(retList)
    
    
    def createWedgeScanInfoList(self, isprintStr):
        """createWedgeScanInfoList creates a list of tuples, which each tuple representing a single scan, and
        values of:

        (isprintString, startUT, startAz, startEl, endUT, endAz, endEl, type,
        minGdlat, maxGdlat,minGlon, maxGdlon,minGdalt, maxGdalt, minGcdist, maxGcdist).  

        Input isprint string has parameters ut1,scntyp,cycn,<parm>,gdalt,gdlat,glon,azm,elm,gcdist,
        gdlatr,gdlonr,galtr,range,az1,az2,el1,el2

        Azimuth scans are split whenever direction or elevation changes.  For elevation scans, there will be zero or more 
        north-south scans, zero or more east-west scans, and zero or more off azimuth scans.  A south to north
        elevation scan or a west to east elevation scan is called clockwise, and a switch from clockwise
        to counterclockwise will create a new elevation scan. An off azimuth
        scan is not within 15 degrees of north, south, east or west.  Off azimuth scans are not combined with
        scans 180 degrees in the other direction, because the x axis is ground distance, which does not reverse
        sign.
        """
        isprintItems = isprintStr.split()

        retList = []

        # loop variables
        thisIsprintStr = []
        cycNum = None
        startUT = []
        startAz = []
        startEl = []
        endUT = []
        endAz = []
        endEl = []
        elScan = []
        minGdlat = []
        maxGdlat = []
        minGlon = []
        maxGlon = []
        minGdalt = []
        maxGdalt = []
        minGcdist = []
        maxGcdist = []
        thisAzDirection = None # used to detect new azimuth scans
        lastAzDirection = None # used to detect new azimuth scans
        thisElDirection = None # used to detect new elevation scans (cw or ccw)
        lastElDirection = None # used to detect new elevation scans (cw or ccw)
        lastEl = None # used to detect new azimuth scans
        thisAz = None
        thisEl = None
        lastUt = None # keep track of new ut
        isNewUt = None # true only when ut changes
        inAzScan = False
        inElScan = False
        

        for i in range(len(isprintItems)):
            item = isprintItems[i]
            mod = i % 18
            if mod == 0:
                thisUt = float(item)
                if lastUt == None:
                    lastUt = thisUt
            elif mod == 1:
                thisScntyp = int(item)
            elif mod == 2:
                thisCycn = int(float(item))
                if cycNum == None:
                    cycNum = thisCycn
            elif mod == 3:
                thisParm = item
            elif mod == 4:
                thisGdalt = float(item)
            elif mod == 5:
                thisGdlat = float(item)
            elif mod == 6:
                thisGlon = float(item)
            elif mod == 7:
                if thisAz != None and lastUt < thisUt and inAzScan:
                    if thisAz > float(item):
                        thisAzDirection = 'cw'
                    elif thisAz < float(item):
                        thisAzDirection = 'ccw'
                thisAz = float(item)
            elif mod == 8:
                if thisEl != None and inElScan:
                    if abs(float(item) - thisEl) > 1.0:
                        if thisElScan == 'Gdlat':
                            if -165.0 >= thisAz or thisAz >= 165.0:
                                # southern scan
                                if float(item) > thisEl:
                                    thisElDirection = 'cw'
                                else:
                                    thisElDirection = 'ccw'
                            else:
                                # northern scan
                                if float(item) < thisEl:
                                    thisElDirection = 'cw'
                                else:
                                    thisElDirection = 'ccw'
                        elif thisElScan == 'Glon':
                            if -105.0 <= thisAz <= -75.0:
                                # western scan
                                if float(item) > thisEl:
                                    thisElDirection = 'cw'
                                else:
                                    thisElDirection = 'ccw'
                            else:
                                # eastern scan
                                if float(item) < thisEl:
                                    thisElDirection = 'cw'
                                else:
                                    thisElDirection = 'ccw'
                        elif thisElScan == 'Gcdist':
                            if float(item) > thisEl:
                                thisElDirection = 'up'
                            else:
                                thisElDirection = 'down'

                thisEl = float(item)
                
            elif mod == 9:
                thisGcdist = float(item)
            elif mod == 10:
                thisGdlatr = float(item)
            elif mod == 11:
                thisGdlonr = float(item)
            elif mod == 12:
                thisGaltr = float(item)
            elif mod == 13:
                thisRange = float(item)
            elif mod == 14:
                thisAz1 = float(item)
            elif mod == 15:
                thisAz2 = float(item)
            elif mod == 16:
                thisEl1 = float(item)
            elif mod == 17:
                thisEl2 = float(item)
                
                # check whether this is a new time
                if lastUt < thisUt:
                    isNewUt = True
                    lastUt = thisUt
                else:
                    isNewUt = False

                # if new cycle or elevation issue, close out value if data exists
                if (thisCycn != cycNum) or \
                    (thisElDirection == 'cw' and lastElDirection == 'ccw') or \
                    (thisElDirection == 'ccw' and lastElDirection == 'cw') or \
                    (thisElDirection == 'down' and lastElDirection == 'up') or \
                    (thisElDirection == 'up' and lastElDirection == 'down') or \
                    (thisElDirection != None and not inElScan) or \
                    (thisAzDirection != None and not inAzScan):
                    
                    for i in range(len(thisIsprintStr)):
                        if len(thisIsprintStr[i]) == 0:
                            continue
                        retList.append((thisIsprintStr[i],
                                        startUT[i], startAz[i], startEl[i], endUT[i], endAz[i], endEl[i], elScan[i],
                                        minGdlat[i], maxGdlat[i], minGlon[i], maxGlon[i], minGdalt[i], maxGdalt[i],
                                        minGcdist[i], maxGcdist[i]))
                    thisIsprintStr = []
                    cycNum = None
                    startUT = []
                    startAz = []
                    startEl = []
                    endUT = []
                    endAz = []
                    endEl = []
                    elScan = []
                    minGdlat = []
                    maxGdlat = []
                    minGlon = []
                    maxGlon = []
                    minGdalt = []
                    maxGdalt = []
                    minGcdist = []
                    maxGcdist = []
                    thisAzDirection = None
                    lastAzDirection = None
                    thisElDirection = None # used to detect new elevation scans (cw or ccw)
                    lastElDirection = None 
                    lastEl = None
                    inAzScan = False
                    inElScan = False

                # end of line - process it
                if self.scanType == 'el' and thisScntyp == 3:
                    # add this elevation scan point
                    # check type of el scan
                    inAzScan = False
                    inElScan = True
                    if (-15.0 <= thisAz <= 15.0 or -165.0 >= thisAz or thisAz >= 165.0) and \
                        abs(thisAz1 - thisAz2) < 0.5:
                        thisElScan = 'Gdlat'
                    elif (75.0 <= thisAz <= 105.0 or -105.0 <= thisAz <= -75.0) and \
                        abs(thisAz1 - thisAz2) < 0.5:
                        thisElScan = 'Glon'
                    else:
                        thisElScan = 'Gcdist'
                    # get index of this scan, None if a new scan
                    index = None
                    for i, item in enumerate(elScan):
                        
                        if item == 'Gdlat' and thisElScan == 'Gdlat':
                            index = i
                            break
                        elif item == 'Glon' and thisElScan == 'Glon':
                            index = i
                            break
                        elif item == 'Gcdist' and thisElScan == 'Gcdist' and int(thisAz) == int(startAz[i]):
                            index = i
                            break
                        elif item == 'Gcdist' and thisElScan == 'Gcdist' and i == len(elScan)-1:
                            index = i
                            break
                    
                    if index == None:
                        # a new scan has been found
                        thisIsprintStr.append('')
                        startUT.append(thisUt)
                        startAz.append(thisAz)
                        startEl.append(thisEl)
                        endUT.append(None)
                        endAz.append(None)
                        endEl.append(None)
                        elScan.append(thisElScan)
                        minGdlat.append(None)
                        maxGdlat.append(None)
                        minGlon.append(None)
                        maxGlon.append(None)
                        minGdalt.append(None)
                        maxGdalt.append(None)
                        minGcdist.append(None)
                        maxGcdist.append(None)
                        index = len(thisIsprintStr) - 1
                        
                    lastElDirection = thisElDirection  
                    endUT[index] = thisUt
                    endAz[index] = thisAz
                    endEl[index] = thisEl
                    # gdlatr, gdlonr, galtr, range, az1, az2, el1, el2,plotParm
                    thisIsprintStr[index] += '%f %f %f %f %f %f %f %f %s\n' % (thisGdlatr, thisGdlonr, thisGaltr,
                                                                               thisRange, thisAz1, thisAz2,
                                                                               thisEl1, thisEl2, thisParm)

                elif self.scanType == 'az' and thisScntyp == 2:
                    # az scan
                    inAzScan = True
                    inElScan = False
                    # add this azimuth scan point
                    # check if this is a new scan
                    newAzScan = False
                    if lastEl == None:
                        newAzScan = True
                    # check for elevation change
                    elif abs(lastEl - int(thisEl)) > 3:
                        newAzScan = True
                    # check for direction change
                    if lastAzDirection != None:
                        if lastAzDirection != thisAzDirection:
                            newAzScan = True
                            thisAzDirection = None
                    if newAzScan:
                        # a new azimuth scan has been found
                        thisIsprintStr.append('')
                        startUT.append(thisUt)
                        startAz.append(thisAz)
                        startEl.append(thisEl)
                        endUT.append(None)
                        endAz.append(None)
                        endEl.append(None)
                        elScan.append(None)
                        minGdlat.append(None)
                        maxGdlat.append(None)
                        minGlon.append(None)
                        maxGlon.append(None)
                        minGdalt.append(None)
                        maxGdalt.append(None)
                        minGcdist.append(None)
                        maxGcdist.append(None)
                        index = len(thisIsprintStr) - 1
                        lastEl = int(thisEl)
                        lastAzDirection = None
                    else:
                        # this is at least the second line of the scan
                        if not thisAzDirection is None:
                            lastAzDirection = thisAzDirection
                            
                    endUT[-1] = thisUt
                    endAz[-1] = thisAz
                    endEl[-1] = thisEl
                    thisIsprintStr[-1] += '%f %f %f %f %f %f %f %f %s\n' % (thisGdlatr, thisGdlonr, thisGaltr,
                                                                               thisRange, thisAz1, thisAz2,
                                                                               thisEl1, thisEl2, thisParm)
                    
                else:
                    # not a scan line
                    inAzScan = False
                    inElScan = False
                    continue

                if minGdlat[index] == None:
                    minGdlat[index] = thisGdlat
                    maxGdlat[index] = thisGdlat
                    minGlon[index] = thisGlon
                    maxGlon[index] = thisGlon
                    minGdalt[index] = thisGdalt
                    maxGdalt[index] = thisGdalt
                    minGcdist[index] = thisGcdist
                    maxGcdist[index] = thisGcdist
                else:
                    if minGdlat[index] > thisGdlat:
                        minGdlat[index] = thisGdlat
                    if maxGdlat[index] < thisGdlat:
                        maxGdlat[index] = thisGdlat
                    if minGlon[index] > thisGlon:
                        minGlon[index] = thisGlon
                    if maxGlon[index] < thisGlon:
                        maxGlon[index] = thisGlon
                    if minGdalt[index] > thisGdalt:
                        minGdalt[index] = thisGdalt
                    if maxGdalt[index] < thisGdalt:
                        maxGdalt[index] = thisGdalt
                    if minGcdist[index] > thisGcdist:
                        minGcdist[index] = thisGcdist
                    if maxGcdist[index] < thisGcdist:
                        maxGcdist[index] = thisGcdist

        # end, close out last cycle
        for i in range(len(thisIsprintStr)):
            if len(thisIsprintStr[i]) == 0:
                continue
            retList.append((thisIsprintStr[i],
                            startUT[i], startAz[i], startEl[i], endUT[i], endAz[i], endEl[i], elScan[i],
                            minGdlat[i], maxGdlat[i], minGlon[i], maxGlon[i], minGdalt[i], maxGdalt[i],
                            minGcdist[i], maxGcdist[i]))

        return(retList)
    
    
    def _getLimits(self, scanList, xMinimum, xMaximum, yMinimum, yMaximum):
        """_getLimits returns a dictionary with keys that may include ('az', 'el_lat', 'el_lon', 'el_gcdist'),
        though not all will neccessarily be there.  Values are overall tuple of
        (xMinimum, xMaximum, yMinimum, yMaximum). These values are set by the input
        arguments unless it is None, in which case the scanList limits set the values.
        
        Inputs: 
        
            scanList: a list of tuples, which each tuple representing a single scan, and
                has values of: (isprintString, startUT, startAz, startEl, endUT, endAz, endEl, type,
                minGdlat, maxGdlat,minGlon, maxGdlon,minGdalt, maxGdalt, minGcdist, maxGcdist).  Isprint string
                has values (gdlatr, gdlonr, galtr, range, az1, az2, el1, el2,plotParm),. Type is Gdlat or Glon or 
                Gcdist for el scans, None for az scans.
                
            xMinimum, xMaximum, yMinimum, yMaximum - as passed into scanPlotter
        """
        retDict = {}
        
        xMinAz = None
        xMaxAz = None
        yMinAz = None
        yMaxAz = None
        xMinElLat = None
        xMaxElLat = None
        yMinElLat = None
        yMaxElLat = None
        xMinElLon = None
        xMaxElLon = None
        yMinElLon = None
        yMaxElLon = None
        xMinElGcdist = None
        xMaxElGcdist = None
        yMinElGcdist = None
        yMaxElGcdist = None
        minValue = None
        maxValue = None
        
        for scanItem in scanList:
            if scanItem[7] == None: # az scan
                if xMinAz == None:
                    xMinAz = scanItem[10]
                elif xMinAz > scanItem[10]:
                    xMinAz = scanItem[10]
                if xMaxAz == None:
                    xMaxAz = scanItem[11]
                elif xMaxAz < scanItem[11]:
                    xMaxAz = scanItem[11]
                if yMinAz == None:
                    yMinAz = scanItem[8]
                elif yMinAz > scanItem[8]:
                    yMinAz = scanItem[8]
                if yMaxAz == None:
                    yMaxAz = scanItem[9]
                elif yMaxAz < scanItem[9]:
                    yMaxAz = scanItem[9]
            elif scanItem[7] == 'Gdlat': # el_lat scan
                if xMinElLat == None:
                    xMinElLat = scanItem[8]
                elif xMinElLat > scanItem[8]:
                    xMinElLat = scanItem[8]
                if xMaxElLat == None:
                    xMaxElLat = scanItem[9]
                elif xMaxElLat < scanItem[9]:
                    xMaxElLat = scanItem[9]
                if yMinElLat == None:
                    yMinElLat = scanItem[12]
                elif yMinElLat > scanItem[12]:
                    yMinElLat = scanItem[12]
                if yMaxElLat == None:
                    yMaxElLat = scanItem[13]
                elif yMaxElLat < scanItem[13]:
                    yMaxElLat = scanItem[13]
            elif scanItem[7] == 'Glon': # el_lon scan
                if xMinElLon == None:
                    xMinElLon = scanItem[10]
                elif xMinElLon > scanItem[10]:
                    xMinElLon = scanItem[10]
                if xMaxElLon == None:
                    xMaxElLon = scanItem[11]
                elif xMaxElLon < scanItem[11]:
                    xMaxElLon = scanItem[11]
                if yMinElLon == None:
                    yMinElLon = scanItem[12]
                elif yMinElLon > scanItem[12]:
                    yMinElLon = scanItem[12]
                if yMaxElLon == None:
                    yMaxElLon = scanItem[13]
                elif yMaxElLon < scanItem[13]:
                    yMaxElLon = scanItem[13]
            elif scanItem[7] == 'Gcdist': # el_gcdist scan
                if xMinElGcdist == None:
                    xMinElGcdist = scanItem[14]
                elif xMinElGcdist > scanItem[14]:
                    xMinElGcdist = scanItem[14]
                if xMaxElGcdist == None:
                    xMaxElGcdist = scanItem[15]
                elif xMaxElGcdist < scanItem[15]:
                    xMaxElGcdist = scanItem[15]
                if yMinElGcdist == None:
                    yMinElGcdist = scanItem[12]
                elif yMinElGcdist > scanItem[12]:
                    yMinElGcdist = scanItem[12]
                if yMaxElGcdist == None:
                    yMaxElGcdist = scanItem[13]
                elif yMaxElGcdist < scanItem[13]:
                    yMaxElGcdist = scanItem[13]
                    
        # fill out the dictionary
        if xMinAz != None:
            if xMinimum == None:
                xMin = xMinAz
            else:
                xMin = xMinimum
            if xMaximum == None:
                xMax = xMaxAz
            else:
                xMax = xMaximum
            if yMinimum == None:
                yMin = yMinAz
            else:
                yMin = yMinimum
            if yMaximum == None:
                yMax = yMaxAz
            else:
                yMax = yMaximum
            retDict['az'] = (xMin, xMax, yMin, yMax)
            
        if xMinElLat != None:
            if xMinimum == None:
                xMin = xMinElLat
            else:
                xMin = xMinimum
            if xMaximum == None:
                xMax = xMaxElLat
            else:
                xMax = xMaximum
            if yMinimum == None:
                yMin = yMinElLat
            else:
                yMin = yMinimum
            if yMaximum == None:
                yMax = yMaxElLat
            else:
                yMax = yMaximum
            retDict['el_lat'] = (xMin, xMax, yMin, yMax)
            
        if xMinElLon != None:
            if xMinimum == None:
                xMin = xMinElLon
            else:
                xMin = xMinimum
            if xMaximum == None:
                xMax = xMaxElLon
            else:
                xMax = xMaximum
            if yMinimum == None:
                yMin = yMinElLon
            else:
                yMin = yMinimum
            if yMaximum == None:
                yMax = yMaxElLon
            else:
                yMax = yMaximum
            retDict['el_lon'] = (xMin, xMax, yMin, yMax)
            
        if xMinElGcdist != None:
            if xMinimum == None:
                xMin = xMinElGcdist
            else:
                xMin = xMinimum
            if xMaximum == None:
                xMax = xMaxElGcdist
            else:
                xMax = xMaximum
            if yMinimum == None:
                yMin = yMinElGcdist
            else:
                yMin = yMinimum
            if yMaximum == None:
                yMax = yMaxElGcdist
            else:
                yMax = yMaximum
            retDict['el_gcdist'] = (xMin, xMax, yMin, yMax)
            
        return(retDict)
    
    
    def _getInstLocation(self, isprintStr):
        """_getInstLocation returns a tuple of instrument location (gdlatr, gdlonr, galtr)

        Input isprint string has parameters ut1,scntyp,cycn,<parm>,gdalt,gdlat,glon,azm,elm,gcdist,
        gdlatr,gdlonr,galtr,range,az1,az2,el1,el2
        """
        isprintItems = isprintStr.split()
        gdlatr = float(isprintItems[10])
        gdlonr = float(isprintItems[11])
        if gdlonr > 180.0:
            gdlonr -= 360.0
        galtr = float(isprintItems[12])
        return((gdlatr, gdlonr, galtr))
        

    
class madHistogram:
    """madHistogram is the class that produces Histogram plots of Madrigal
        Data.

        Usage example::

            obj = madHistogram(isprintText,
                                'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2005 - Alt. code',
                                'Hours since midnight UT Oct. 30,2003'
                                'Altitude',
                                './isprint.png'
                                size = 'large')

        Inputs:

            isprintText - a string giving isprint output without headers. Any missing data should
                            be written as 'missing' or other string that cannot be converted into
                            a float.  Only one column.

            TitleStr - plot title(string) - should describe the plot being made

            xLabelStr - Label for x axis

            fullFilename - full path of file containing histogram plot to be saved. Extension must be .jpeg
                            or .png, or exception thrown

            size - size of plot to be saved. Must be 'small','wide', or 'large'. Defaults to small

            maxNumPoints - maximum number of points to be considered. Defaults to None, so all points in string shown


            numBins - number of bins to be used to store the data. More bins means closer resolution in Histogram.
                        Default is 30

            orientation - whether histogram shows horizontal or vertical. Must be written as 'horizontal' or
                            'vertical', or exception thrown--defaults to 'vertical'

            isNorm - whether histogram should be normalized or not. Default is 0, or False

            isBottom - If true, sets the lowest passed value as zero

            sdevs - number of standard deviations to take into account. When 0 is passed to it, this option is ignored and all data
                    is included in the distribution; otherwise, the range of values accepted is sdevs*(standard deviation of sample)

        Outputs:

            A .png file is written to the fullFilename path given, resulting in a Histogram drawn by matplotlib

            
        Change History:

            Written by "Brandon Scott Fines":mailto:bfines@haystack.mit.edu Aug. 1,2006
            Written by "Bill Rideout":mailto:brideout@haystack.mit.edu Aug. 1,2006
 
    """
    def __init__(self,isprintText,
                 titleStr,
                 xLabelStr,
                 fullFilename,
                 size = 'small',
                 maxNumPoints = None,
                 numBins = 30,
                 Orientation='vertical',
                 isNorm =0,
                 isBottom=0,
                 sdevs=0):
        """__init__ writes a madHistogram string to file

        """
        self.__missing = 1.0E30 #special value, since numpy doesn't Handle NaN
        self.__parameter_count = 2

        #verify input
        if not size in ('small','wide','large'):
            raise ValueError('size must be "small","wide" or "large", not %s'%(str(size)))

        if size in ('small','wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        # remove all non-float values
        items = isprintText.split()
        keptItems = []
        for item in items:
            try:
                float(item)
                keptItems.append(item)
            except:
                continue
        delimiter = ' '
        isprintText = delimiter.join(keptItems)

        if maxNumPoints !=None:
            isprintText = self._truncateIsprint(isprintText, maxNumPoints)

        #convert the input data into numeric arrays of floats assuming no headers and filter the missing values

        try:
            split_data = isprintText.split()
            float_data = list(map(self.__filter_missing,split_data))
            array_data = numpy.asarray(float_data)
        except:
            traceback.print_exc()
            raise ValueError('input text is not parseable')

        #if needed, throw away outliers
        if sdevs !=0:
            array_data = self.removeOutliers(sdevs,array_data)

        #select the plot size
        if size=='small':
            matplotlib.pylab.figure(1,figsize=(6,4), facecolor = 'w')
        elif size=='wide':
            matplotlib.pylab.figure(1,figsize=(10,4),facecolor='w')
        elif size=='large':
            matplotlib.pylab.figure(1,figsize=(12,6),facecolor='w')


        matplotlib.pylab.hist(array_data,bins = numBins,normed=isNorm,bottom=isBottom,orientation = Orientation)

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont) #pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.title(titleStr,fontsize=fontSize)

        #self.__figure = matplotlib.pylab.gcf()

        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()

    def __truncateIsprintStr(self,isprintText, maxLines):
        """__truncateIsprintStr truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList)<maxLines:
            return isprintText
        else:
            dropNumber = int(1+len(isprintList)/maxlines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText

    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

    def removeOutliers(self,ndevs,values=[]):

        
        #find the mean value in values
        sums = 0.0
        count = 0
        for i in values:
            sums = sums+i
            count = count+1


        mean = sums/count
        

        #calculate a standard deviation
        innersum = 0
        for i in values:
            innersum = innersum + math.pow((i-mean),2)

        sdev = math.sqrt((1.0/count)*innersum)

        #throw away all data points falling outside three standard deviations of the mean, then return the resulting list
        retArray = []
        ndist = mean-ndevs*sdev
        pdist = mean+ndevs*sdev
        for i in values:
            if (i>=ndist) and (i<=pdist):
                retArray.append(i)

        return retArray

    #end eliminateOutliers()


class madIsrRecordSummary:
    """madIsrRecordSummary is the class that produces a summary plot of a single ISR record.


    """
    def __init__(self, TiData,
                 TiError,
                 TeData,
                 TeError,
                 VoData,
                 VoError,
                 NelData,
                 NelError,
                 isPopl,
                 description,
                 fullFilename,
                 useRange = False,
                 altResl = None):
        """__init__ writes a madIsrRecordSummary  to a file.

        Inputs:

            TiData - an Nx2 numeric array of Ti (col 0) and altitude in km (col 1)

            TiError - an N length array of error in Ti
            
            TeData - an Nx2 numeric array of Te (col 0) and altitude in km (col 1)

            TeError - an N length array of error in Te

            VoData - an Nx2 numeric array of Vo (col 0) and altitude in km (col 1)

            VoError - an N length array of error in Vo

            NelData - an Nx2 numeric array of (Popl or Nel in m^-3) (col 0) and altitude in km (col 1)

            NelError - an 2xN length array of lower, upper error in Popl or Nel
            
            isPopl - true if Nel contains Popl data, false if Nel data

            description - text to put in fourth panel of plot - use carriage returns to separate lines

            fullFilename - full filename to save output png file.

            useRange - if False (the default), y axis = Altitude.  If True, y axis = Range.

            altResl - tuple with (altitude resolution at 100, 300 in km).  If not None, show altitude resolution on each graph.
        
        
        Returns: void

        Affects: None
        """
        if useRange:
            yAxisStr = 'Range (km)'
        else:
            yAxisStr = 'Altitude (km)'
            
        # upper left - Ti and Te
        fig = matplotlib.pylab.figure(figsize=(10,10))
        axis = matplotlib.pylab.subplot(2,2,1)
        if TiData.shape[0] > 0 and TeData.shape[0] > 0:
            axis.errorbar(TiData[:,0], TiData[:,1], xerr=TiError, fmt='bo-')   
            axis.errorbar(TeData[:,0], TeData[:,1], xerr=TeError, fmt='rD-')
            matplotlib.pylab.text(0.65, 0.9, 'Ti: blue circle\nTe: red diamond',
                                  horizontalalignment='left',
                                  verticalalignment='top',
                                  transform = axis.transAxes,
                                  fontsize=10)
            matplotlib.pylab.text(0.5,0.5, 'Preliminary data\n\n\nfrom the\n\n\nMadrigal database',
                                  horizontalalignment='center',
                                  verticalalignment='center',
                                  fontsize=18,
                                  transform = axis.transAxes, alpha=0.1)
            matplotlib.pylab.xlabel('Temperature (K)')
            matplotlib.pylab.ylabel(yAxisStr)
            matplotlib.pylab.grid(True)
            # set limits based on data only
            tiMax = numpy.nanmax(TiData[:,0])
            tiMin = numpy.nanmin(TiData[:,0])
            teMax = numpy.nanmax(TeData[:,0])
            teMin = numpy.nanmin(TeData[:,0])
            lowerLimit, upperLimit = matplotlib.pylab.xlim()    
            
            # alt resolution
            if altResl != None:
                altResl100, altResl300 = altResl
                if altResl100 != altResl300:
                    altLabel = 'Alt res 100:\n%i km' % (int(altResl100))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.7*(upperLimit - lowerLimit)
                    startY = max(0.03, (100-y_min)/(y_max-y_min))
                    reslPercentage = altResl100/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.73, startY+0.04, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                    altLabel = 'Alt res 300:\n%i km' % (int(altResl300))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.7*(upperLimit - lowerLimit)
                    startY = max(0.15, (300-y_min)/(y_max-y_min))
                    reslPercentage = altResl300/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.73, startY+0.06, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                else:
                    altLabel = 'Alt res: \n%i km' % (int(altResl100))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.8*(upperLimit - lowerLimit)
                    startY = 0.15
                    reslPercentage = altResl100/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.83, 0.2, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                    
                
            matplotlib.pylab.xlim(lowerLimit, upperLimit)
            
                
                                         

        # upper right - Vo and 10*Vo
        axis = matplotlib.pylab.subplot(2,2,2)
        if VoData.shape[0] > 0:
            axis.errorbar(VoData[:,0], VoData[:,1], xerr=VoError, fmt='bo-')
            axis.errorbar(10.0*VoData[:,0], VoData[:,1], xerr=10.0*VoError, fmt='rD-', alpha=0.3)
            # set x limits to be even around 0
            limit = 1000.0
            matplotlib.pylab.xlim(-1.0*limit, limit)
            matplotlib.pylab.title('Vo: blue circle, 10*Vo: red diamond')
            matplotlib.pylab.text(0.5,0.5, 'Preliminary data\n\n\nfrom the\n\n\nMadrigal database',
                                  horizontalalignment='center',
                                  verticalalignment='center',
                                  fontsize=18,
                                  transform = axis.transAxes, alpha=0.1)
            matplotlib.pylab.xlabel('Line of sight drift (m/s)')
            matplotlib.pylab.ylabel(yAxisStr)
            matplotlib.pylab.grid(True)
            lowerLimit, upperLimit = matplotlib.pylab.xlim()

            # alt resolution
            if altResl != None:
                altResl100, altResl300 = altResl
                if altResl100 != altResl300:
                    altLabel = 'Alt res 100:\n%i km' % (int(altResl100))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.7*(upperLimit - lowerLimit)
                    startY = max(0.03, (100-y_min)/(y_max-y_min))
                    reslPercentage = altResl100/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.73, startY+0.04, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                    altLabel = 'Alt res 300:\n%i km' % (int(altResl300))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.7*(upperLimit - lowerLimit)
                    startY = max(0.15, (300-y_min)/(y_max-y_min))
                    reslPercentage = altResl300/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.73, startY+0.06, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                else:
                    altLabel = 'Alt res: \n%i km' % (int(altResl100))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.8*(upperLimit - lowerLimit)
                    startY = 0.15
                    reslPercentage = altResl100/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.83, 0.2, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                
            matplotlib.pylab.xlim(-1.0*limit, limit)

            

        # lower left - Nel or Popl
        axis = matplotlib.pylab.subplot(2,2,3)
        if isPopl:
            labelStr = 'Log Power (m^-3)'
        else:
            labelStr = 'Log Electron density (m^-3)'
        nelMax = 12.5
        nelMin = 9.5
        if NelData.shape[0] > 0:
            # check that not every point equal
            if numpy.nanmax(NelData[:,0]) == numpy.nanmin(NelData[:,0]):
                pass
            else:
                axis.errorbar(NelData[:,0], NelData[:,1], xerr=NelError, fmt='bo-')
                matplotlib.pylab.xlabel(labelStr)
                matplotlib.pylab.ylabel(yAxisStr)
                matplotlib.pylab.grid(True)
                matplotlib.pylab.xlim(math.floor(nelMin), math.ceil(nelMax))
                matplotlib.pylab.text(0.5,0.5, 'Preliminary data\n\n\nfrom the\n\n\nMadrigal database',
                                  horizontalalignment='center',
                                  verticalalignment='center',
                                  fontsize=18,
                                  transform = axis.transAxes, alpha=0.1)
            lowerLimit, upperLimit = matplotlib.pylab.xlim()
                
            # alt resolution
            if altResl != None:
                altResl100, altResl300 = altResl
                if altResl100 != altResl300:
                    altLabel = 'Alt res 100:\n%i km' % (int(altResl100))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.7*(upperLimit - lowerLimit)
                    startY = max(0.03, (100-y_min)/(y_max-y_min))
                    reslPercentage = altResl100/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.73, startY+0.04, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                    altLabel = 'Alt res 300:\n%i km' % (int(altResl300))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.7*(upperLimit - lowerLimit)
                    startY = max(0.15, (300-y_min)/(y_max-y_min))
                    reslPercentage = altResl300/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.73, startY+0.06, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)
                else:
                    altLabel = 'Alt res: \n%i km' % (int(altResl100))
                    y_min, y_max = matplotlib.pylab.ylim()
                    startX = lowerLimit + 0.8*(upperLimit - lowerLimit)
                    startY = 0.15
                    reslPercentage = altResl100/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.83, 0.2, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)

                    matplotlib.pylab.xlim(math.floor(nelMin), math.ceil(nelMax))
        

        # lower right - text
        axis = matplotlib.pylab.subplot(2,2,4)
        matplotlib.pylab.text(0.05, 0.9, description,
                              horizontalalignment='left',
                              verticalalignment='top',
                              transform = axis.transAxes,
                              fontsize=9, fontweight='bold')
        matplotlib.pylab.xticks([])
        matplotlib.pylab.yticks([])
        
        # title
        desc_items = description.split('\n')
        title = ' '.join(desc_items[:3])
        fig.suptitle(title)
        
        

        matplotlib.pylab.subplots_adjust(wspace = 0.25)
        

        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()
        matplotlib.pylab.close()

        
if __name__ == '__main__':

    import time

    # test madPcolorScan

    # az scan
    with open('testFiles/isprint_az_scan.txt') as f:
        isprintText = f.read()

    print('creating az scan at /tmp/isprint_az_scan.png')
    obj = madPcolorScan(isprintText,
                        1.0, 1.0,
                        'Az scan - NEL 2006-06-26 13:49:43-14:07:36',
                        'Longitude',
                        'Latitude',
                        '/tmp/isprint_az_scan.png',
                        'small',
                        -100,-50,40,60,
                        9,12)
    
    # el scan
    with open('testFiles/isprint_el_scan.txt') as f:
        isprintText = f.read()
    
    print('creating el scan at /tmp/isprint_el_scan.png')
    obj = madPcolorScan(isprintText,
                        1.0, 40.0,
                        'El scan - NEL 2006-06-26 13:49:43-14:07:36',
                        'Latitude',
                        'Altitude (km)',
                        '/tmp/isprint_el_scan.png',
                        'small')

    # test scanPlotter
    print('creating az scan series using mlh060622a.000')
    scanObj = scanPlotter('/home/grail/brideout/madroot/experiments/2006/mlh/22jun06/mlh060622a.000')
    scanObj.plotAllScans('az','/tmp/junkAz', 'Nel', filterStr='filter=nel,,11.0')
    
    t = time.time()

    # test madIsrRecordSummary
    TiData = numpy.array([[800, 150],[900, 200],[910, 300], [945, 500], [980, 600]])
    TiError = numpy.array([50,100,50,100,50])
    TeData = numpy.array([[850, 150],[1000, 200],[1800, 300], [2000, 500], [2000, 600]])
    TeError = numpy.array([50,100,50,100,50])
    VoData = numpy.array([[-30, 150],[0, 200],[10, 300], [35, 500], [10, 600]])
    VoError = numpy.array([5,10,5,10,5])
    NelData = numpy.array([[11, 150],[11.5, 200],[11.3, 300], [11, 500], [10.5, 600]])
    NelError = numpy.array(((2.0,0.5),(2.0,1.0), (2.0,0.5), (2.0,1.0), (2.0,0.5)))
    isPopl = True
    text = 'Kinst = Millstone Zenith Antenna\nMore stuff here\nand more\nand more'
    fullFilename = '/tmp/junk.png'
    madIsrRecordSummary(TiData,TiError,TeData,TeError,VoData,VoError,NelData,numpy.transpose(NelError),isPopl,text,fullFilename)


    
    
    with open('testFiles/isprint_uth_fof2.txt') as f:
        isprintText = f.read()
    
    print('creating scatterplot at /tmp/isprint_fof2.png')
    obj = madScatterPlot(isprintText,
                        'Fof2 - Millstone Hill - Oct. 30, 2003',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Fof2',
                        '/tmp/isprint_fof2.png',
                        size = 'large',
			useAbsoluteTime = False)
			

    with open('testFiles/isprint_nel.txt') as f:
        isprintText = f.read()

    print('creating pcolor plot at /tmp/isprint_nel.png')
    obj = madPcolorPlot(isprintText,
                        'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2003 - Alt code',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Altitude (km)',
                        '/tmp/isprint_nel.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12,
			smoothAltitude = True,
			insertDataGap = 5,
			useAbsoluteTime = False,
			startTime = None,
			endTime = None,
			sortTimeFlag = False,
			maxNumTimes = None,
			maxNumAlt = 50,
                        truncateIsprint = True,
                        colorMap = matplotlib.cm.spring,
                        altYTitle = 'Latitude',
                        altYLabels = ('40', '41', '42', '43', '44', '45'))
    
			
    print('took %i secs' % (time.time() - t))
