"""Forms for madweb application

$Id: forms.py 7621 2023-10-05 13:23:00Z brideout $
"""

# standard python imports
import os.path
import datetime
import itertools

# django imports
import django.forms
import django.utils.html
import django.utils.safestring
import django.template.defaulttags

# third party imports
import numpy

# Madrigal imports
import madrigal.metadata
import madrigal.ui.web

# temp only
import logging

@django.template.defaulttags.register.filter
def get_item(dictionary, key):
    return(dictionary.get(key))

@django.template.defaulttags.register.filter
def modulo(num, val):
    return(num % val == 0)
    
class HorizontalRadioSelect(django.forms.RadioSelect):
    template_name = 'madweb/horizontal_select.html'
    


def getSelection(keyword, args, kwargs):
    """getSelection returns '0' if keyword not a key in either args[0] or kwargs, 
    otherwise the value
    
    args, kwargs - arguments as passed into SingleExpDefaultForm __init__
    """
    if len(args) == 0 and len(list(kwargs.keys())) == 0:
        return('0') # default case when no data passed in
    elif len(args) > 0:
        # args[0] is data dict argument to bind data
        if keyword in args[0]:
            return(args[0][keyword])
        else:
            return('0')
    elif keyword in kwargs:
        return(kwargs[keyword])
    elif 'data' in kwargs:
        if keyword in kwargs['data']:
            return(kwargs['data'][keyword])
        else:
            return('0')
    else:
        return('0')
    

def getIsGlobal(args, kwargs):
    """getIsGlobal is a helper function returns True if 'isGlobal' not found in either args[0] or kwargs, 
    otherwise the bool of the value
    
    args, kwargs - arguments as passed into SingleExpDefaultForm __init__
    """
    if len(args) == 0 and len(list(kwargs.keys())) == 0:
        return(True) # default case when no data passed in
    elif len(args) == 0 and 'initial' in kwargs:
        return(True)
    elif len(args) > 0:
        # args[0] is data dict argument to bind data
        if 'isGlobal' in args[0]:
            if args[0]['isGlobal'] == '0':
                return(False)
            else:
                return(bool(args[0]['isGlobal']))
        else:
            return(False)
    elif 'data' in kwargs:
        if 'isGlobal' in kwargs['data']:
            if kwargs['data']['isGlobal'] == '0':
                return(False)
            else:
                return(bool(kwargs['data']['isGlobal']))
        else:
            return(False)
    elif 'isGlobal' in kwargs:
        if kwargs['isGlobal'] == '0':
                return(False) 
        else:
            return(bool(kwargs['isGlobal']))
    else:
        return(False)
    

def getCategoryList(args, kwargs, madInstData, forList=False):
    """getCategoryList is a helper function that returns the categories choices in 
    SingleExpDefaultForm.categories at runtime
    
    Inputs:
        args, kwargs - arguments as passed into SingleExpDefaultForm __init__.  Used only to 
            determine isGlobal
        madInstData - madrigal.metadata.MadrigalInstrumentData object
        forList - if False (the default) category list if for single interface, If True, for List interface
    """
    if not forList:
        local = not getIsGlobal(args, kwargs)
    else:
        local = False
    categories = madInstData.getCategories(local)
    if forList:
        catList = [('0', 'All instrument categories'),]
    else:
        catList = [('0', 'Choose instrument type: '),]
    for catID, catDesc in categories:
        catList.append((str(catID), catDesc))
    return(catList)

    
    
def getInstrumentList(args, kwargs, madInstData, header='Select an instrument: ', local=None, includeYears=False):
    """getInstrumentList is a helper function that returns the instrument choices in 
    SingleExpDefaultForm.categories at runtime
    
    Inputs:
        args, kwargs - arguments as passed into SingleExpDefaultForm __init__.  Used to 
            determine categoryId and local
        madInstData - madrigal.metadata.MadrigalInstrumentData object
        header - text of first item in selection
        local - if None (the default), set local flag via args and kwargs.  If boolean,
            set local flag to arg.
        includeYear - if True, include data years in description.  If False (the default), do not.
    """
    if local is None:
        local = not getIsGlobal(args, kwargs)
    else:
        local = bool(local)
    if header == 'Select an instrument: ':
        categoryId = int(getSelection('categories', args, kwargs))
    else:
        categoryId = 0
    instruments = madInstData.getInstruments(categoryId, local)
    instList = [('0', header),]
    for kinst, instDesc, siteID in instruments:
        if includeYears:
            instYears = madInstData.getInstrumentYears(kinst)
            instList.append((str(kinst), '%s [%i-%i]' % (instDesc, instYears[0], instYears[-1])))
        else:
            instList.append((str(kinst), instDesc))
    return(instList)


def getYearList(args, kwargs, madInstData):
    """getYearList is a helper function that returns the year choices in 
    SingleExpDefaultForm.categories at runtime
    
    Inputs:
        args, kwargs - arguments as passed into SingleExpDefaultForm __init__.  Used to 
            determine categoryId and local and kinst
        madInstData - madrigal.metadata.MadrigalInstrumentData object
    """
    local = not getIsGlobal(args, kwargs)
    kinst = int(getSelection('instruments', args, kwargs))
    years = madInstData.getInstrumentYears(kinst)
    yearsList = [('0', 'Select a year: '),]
    for thisYear in reversed(years):
        yearsList.append((str(thisYear), str(thisYear)))
    return(yearsList)


def getMonthList(args, kwargs, madWebObj):
    """getMonthList is a helper function that returns the month choices in 
    SingleExpDefaultForm.categories at runtime.  Value is (month number, 
    month name)
    
    Inputs:
        args, kwargs - arguments as passed into SingleExpDefaultForm __init__.  Used to 
            determine and kinst and year
        madWebObj - madrigal.ui.web.MadrigalWeb object
    """
    kinst = int(getSelection('instruments', args, kwargs))
    year = int(getSelection('years', args, kwargs))
    monthList = [('0', 'Select a month: '),]
    addedMonthList = madWebObj.getMonths(kinst, year)
    if len(addedMonthList) == 0:
        addedMonthList = madWebObj.getMonths(kinst, year, optimize=False)
    monthList += addedMonthList
    return([(str(monthNumber), monthName) for monthNumber, monthName in monthList])


def getDayList():
    """always returns 1 ... 31
    """
    dayList = [(i, str(i)) for i in range(1,32)]
    return(dayList)


def getExpList(args, kwargs, madWebObj):
    """getExpList is a helper function that returns the experiment choices in 
    SingleExpDefaultForm.categories at runtime.  Value is (expId, 
    expDesc, expDir, pi_name, pi_email)
    
    Inputs:
        args, kwargs - arguments as passed into SingleExpDefaultForm __init__.  Used to 
            determine and kinst, year, month, day
        madWebObj - madrigal.ui.web.MadrigalWeb object
    """
    kinst = int(getSelection('instruments', args, kwargs))
    year = int(getSelection('years', args, kwargs))
    month = int(getSelection('months', args, kwargs))
    day = int(getSelection('days', args, kwargs))
    expList = madWebObj.getExpsOnDate(kinst, year, month, day)
    if len(expList) == 0:
        expList = madWebObj.getExpsOnDate(kinst, year, month, day, optimize=False)
    return(expList)


def handle_registration(kwargs, user_email, expUrl, kinst, madDB):
    """handle_registration causes the user to register or unregister interest in getting emails
    when a particular experiment or instrument is updated
    
    Inputs:
        kwargs - dictionary as passed into form
        user_email - users email address
        expUrl - experiment url (part after /madtoc/)
        kinst - instrument code
        madDB - madrigal.metadata.MadrigalDB object
    """
    # first find out if this exp or inst is already registered
    madUserObj = madrigal.ui.userData.MadrigalUserData(madDB)
    expsRegistered = madUserObj.getRegisteredExperiments(user_email)
    if expUrl in expsRegistered:
        thisExpRegistered = True
    else:
        thisExpRegistered = False
    instsRegistered = madUserObj.getRegisteredInstruments(user_email)
    if kinst in instsRegistered:
        thisInstRegistered = True
    else:
        thisInstRegistered = False
        
    # check registration status, update if needed, and let form know how to print this
    # 0 - no registration, 1 - exp registered, 2 - inst registered
    if 'registerExp' in kwargs:
        if not thisExpRegistered:
            madUserObj.registerExperiment(user_email, expUrl)
        return('1')
    elif 'registerInst' in kwargs:
        if not thisInstRegistered:
            madUserObj.registerInstrument(user_email, kinst)
        return('2')
    elif 'unregisterExp' in kwargs:
        if thisExpRegistered:
            madUserObj.unregisterExperiment(user_email, expUrl)
        return('0')
    elif 'unregisterInst' in kwargs:
        if thisInstRegistered:
            madUserObj.unregisterInstrument(user_email, kinst)
        return('0')
    elif thisExpRegistered:
        return('1')
    elif thisInstRegistered:
        return('2')
    else:
        return('0')
    
    
def getFormatChoices(basename, expID):
    """getFormatChoices returns a list with 3 tuples, where each tuple
    is 1. filename with correct extension, 2. Format
    
    Inputs:
        basename - basename of Madrigal Hdf5 file
        expID - needed to determine if cached file available in near real time
    """
    madWebObj = madrigal.ui.web.MadrigalWeb()
    formats = madWebObj.getFormatsAvailable(expID, basename)
    fileName, fileExtension = os.path.splitext(basename)
    retList = []
    retList.append((basename, 'Hdf5')) # file is assumed to be in Hdf5 format
    if 'ascii' in formats:
        retList.append((fileName + '.txt', 'Space-delimited ascii'))
    if 'netCDF4' in formats:
        retList.append((fileName + '.nc', 'netCDF4'))
    return(retList)


class RegisterForm(django.forms.Form):
    """RegisterForm is the form class that supports the Register page
    """
    def __init__(self, *args, **kwargs):
        super(RegisterForm, self).__init__(*args, **kwargs)
        self.fields['user_fullname'] = django.forms.CharField(label='Full name', min_length=2, max_length=256)
        self.fields['user_email'] = django.forms.EmailField(label='Email')
        self.fields['user_affiliation'] = django.forms.CharField(label='Affliation (type "None" if individual)', min_length=2, max_length=256)



class SingleExpDefaultForm(django.forms.Form):
    """SingleExpDefaultForm is a Form class for the default fields in the Single Experiment interface
    (formally the Simple interface)
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpDefaultForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        isTrusted = bool(kwargs['initial']['isTrusted'])
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB, isTrusted)
        user_email = getSelection('user_email', args, kwargs)
        request = getSelection('request', args, kwargs)
        self.fields['isGlobal'] = django.forms.BooleanField(widget = django.forms.CheckboxInput(attrs={"onChange":'populateCat(this)'}),
                                                            required=False, label='Use all Madrigal sites: ',
                                                            initial=getIsGlobal(args, kwargs))
        
        
        categoriesSelection = getSelection('categories', args, kwargs)
        self.fields['categories'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateInst(this)'}), 
                                                             choices=getCategoryList(args, kwargs, madInstData), 
                                                             initial=categoriesSelection,
                                                             label='Instrument category:')
        
        # the following fields may or may not be populated.  All are also available as individual classes
        # to allow AJAX calls to not need to create this full object
        
        if categoriesSelection != '0':
            instrumentSelection = getSelection('instruments', args, kwargs)
            self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateYear(this)'}),
                                                                  choices=getInstrumentList(args, kwargs, madInstData),
                                                                  initial=instrumentSelection,
                                                                  required=False, label='Instrument:')
        else:
            return # no need to create any further fields
        
        if instrumentSelection != '0':
            yearSelection = getSelection('years', args, kwargs)
            self.fields['years'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateMonth(this)'}),
                                                            choices=getYearList(args, kwargs, madInstData),
                                                            initial=yearSelection,
                                                            required=False, label='Year:')
            
        else:
            return # no need to create any further fields
        
        if yearSelection != '0':
            madWebObj = madrigal.ui.web.MadrigalWeb(madDB, request)
            monthSelection = getSelection('months', args, kwargs)
            self.fields['months'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateCalendar(this)'}),
                                                        choices=getMonthList(args, kwargs, madWebObj),
                                                        initial=monthSelection, label='Month:')
            
        else:
            return # no need to create any further fields
        
        
        if monthSelection != '0':
            daySelection = getSelection('days', args, kwargs)
            # this field is only used to help display the calendar
            self.fields['days'] = django.forms.ChoiceField(required=False, choices=getDayList())
            
        else:
            return # no need to create any further fields
        
        if daySelection != '0':
            expFullList = getExpList(args, kwargs, madWebObj)
            expIdSelection = None
            if len(expFullList) > 1:
                expList = [('0', 'Select one of multiple experiments')] + [(items[0], items[1]) for items in expFullList]
                expIdSelection = getSelection('experiment_list', args, kwargs)
                self.fields['experiment_list'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateFile(this)'}),
                                                                          choices=expList, initial=expIdSelection,
                                                                          required=False, label='Select experiment:')
                
                if expIdSelection == '0':
                    # no need to create any further fields
                    return
            
            if expIdSelection is None:
                expIdSelection = expFullList[0][0]
                expDir = expFullList[0][2]
            else:
                expDir = None
                for expId, thisExpDesc, thisExpDir in expFullList:
                    if int(expIdSelection) == int(expId):
                        expDir = thisExpDir
                        break

            fileList = madWebObj.getFileFromExpDir(expDir, int(instrumentSelection))
            fileList = [('0', 'Select file')] + fileList
            fileSelection = getSelection('file_list', args, kwargs)
            self.fields['file_list'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'changeFile(this)'}),
                                                                choices=fileList, initial=fileSelection,
                                                                required=False, label='Select file:')
            self.fields['exp_id'] = django.forms.CharField(initial=str(expIdSelection), 
                                                           widget=django.forms.HiddenInput(attrs={'value': str(expIdSelection)}),
                                                           required=False,
                                                           label=str(expIdSelection))
            
            pi_name, pi_email, expUrl, kinst, expDesc, kinstDesc = madWebObj.getExpInfoFromExpID(int(expIdSelection))
            
            self.fields['exp_desc'] = django.forms.CharField(initial=str(expDesc), 
                                                             widget=django.forms.HiddenInput(attrs={'value': str(expDesc)}),
                                                             required=False,
                                                             label=str(expDesc))
            self.fields['user_email'] = django.forms.CharField(initial=str(user_email), 
                                                               widget=django.forms.HiddenInput(attrs={'value': str(user_email)}),
                                                               required=False,
                                                               label=str(user_email))
            self.fields['inst_desc'] = django.forms.CharField(initial=str(kinstDesc), 
                                                               widget=django.forms.HiddenInput(attrs={'value': str(kinstDesc)}),
                                                               required=False,
                                                               label=str(kinstDesc))
            self.fields['pi_name'] = django.forms.CharField(initial=pi_name, 
                                                           widget=django.forms.HiddenInput(attrs={'value': pi_name}),
                                                           required=False,
                                                           label=pi_name)
            self.fields['pi_email'] = django.forms.CharField(initial=pi_email, 
                                                            widget=django.forms.HiddenInput(attrs={'value': pi_email}),
                                                            required=False,
                                                            label=pi_email)
            
            # handle any needed registration or unregistration
            register = handle_registration(args[0], user_email, expUrl, kinst, madDB)
            self.fields['register'] = django.forms.CharField(initial=register, 
                                                             widget=django.forms.HiddenInput(attrs={'value': register}),
                                                             required=False,
                                                             label=register)
            
            
        else:
            return # no need to create any further fields
        
        if fileSelection != '0':
            self.fields['file_buttons'] = django.forms.CharField(initial='', 
                                                                widget=django.forms.HiddenInput(attrs={'value': ''}),
                                                                required=False,
                                                                label='')
        
        
        else:
            return # no need to create any further fields
        
        
            
        
        
        

        
        
class SingleExpInstForm(django.forms.Form):
    """SingleExpInstForm is a Form class for the instrument select field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpInstForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        isTrusted = bool(kwargs['initial']['isTrusted'])
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB, isTrusted)
        instrumentSelection = getSelection('instruments', args, kwargs)
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateYear(this)'}),
                                                              choices=getInstrumentList(args, kwargs, madInstData),
                                                              initial=instrumentSelection,
                                                              label='Instrument:')
        
class SingleExpYearForm(django.forms.Form):
    """SingleExpYearForm is a Form class for the year select field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpYearForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        isTrusted = bool(kwargs['initial']['isTrusted'])
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB, isTrusted)
        yearSelection = getSelection('years', args, kwargs)
        self.fields['years'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateMonth(this)'}),
                                                        choices=getYearList(args, kwargs, madInstData),
                                                        initial=yearSelection, label='Year:')
        
        
class SingleExpMonthForm(django.forms.Form):
    """SingleExpMonthForm is a Form class for the month select field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpMonthForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        request = getSelection('request', args, kwargs)
        madWebObj = madrigal.ui.web.MadrigalWeb(madDB, request)
        monthSelection = getSelection('months', args, kwargs)
        self.fields['months'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'populateCalendar(this)'}),
                                                        choices=getMonthList(args, kwargs, madWebObj),
                                                        initial=monthSelection, label='Month:')
        
        
class SingleExpCalendarForm(django.forms.Form):
    """SingleExpCalendarForm is a Form class for the calendar field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpCalendarForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        request = getSelection('request', args, kwargs)
        madWebObj = madrigal.ui.web.MadrigalWeb(madDB, request)
        
        
class SingleExpFileForm(django.forms.Form):
    """SingleExpFileForm is a Form class for the file select field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpFileForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        request = getSelection('request', args, kwargs)
        madWebObj = madrigal.ui.web.MadrigalWeb(madDB, request)
        expID = getSelection('experiment_list', args, kwargs)
        try:
            expID = int(expID)
        except ValueError:
            # convert expPath to expID
            expID = madWebObj.getExpIDFromExpPath(expID, True)
        user_email = getSelection('user_email', args, kwargs)
        include_non_default = getSelection('includeNonDefault', args, kwargs)
        if include_non_default == '0':
            include_non_default = False
        fileList = madWebObj.getFileFromExpID(expID, include_non_default)
        fileList = [('0', 'Select file')] + fileList
        
        fileSelection = getSelection('file_list', args, kwargs)
        self.fields['file_list'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={"onChange":'changeFile(this)'}),
                                                            choices=fileList, initial=fileSelection,
                                                            required=False, label='Select file:')
        if int(expID) != 0:
            self.fields['exp_id'] = django.forms.CharField(initial=str(expID), 
                                                           widget=django.forms.HiddenInput(attrs={'value': str(expID)}),
                                                           required=False,
                                                           label=str(expID))
            pi_name, pi_email, expUrl, kinst, expDesc, kinstDesc = madWebObj.getExpInfoFromExpID(int(expID))
            
            self.fields['exp_desc'] = django.forms.CharField(initial=str(expDesc), 
                                                             widget=django.forms.HiddenInput(attrs={'value': str(expDesc)}),
                                                             required=False,
                                                             label=str(expDesc))
            self.fields['user_email'] = django.forms.CharField(initial=str(user_email), 
                                                               widget=django.forms.HiddenInput(attrs={'value': str(user_email)}),
                                                               required=False,
                                                               label=str(user_email))
            self.fields['inst_desc'] = django.forms.CharField(initial=str(kinstDesc), 
                                                               widget=django.forms.HiddenInput(attrs={'value': str(kinstDesc)}),
                                                               required=False,
                                                               label=str(kinstDesc))
            self.fields['pi_name'] = django.forms.CharField(initial=pi_name, 
                                                           widget=django.forms.HiddenInput(attrs={'value': pi_name}),
                                                           required=False,
                                                           label=pi_name)
            self.fields['pi_email'] = django.forms.CharField(initial=pi_email, 
                                                            widget=django.forms.HiddenInput(attrs={'value': pi_email}),
                                                            required=False,
                                                            label=pi_email)
            self.fields['includeNonDefault'] = django.forms.BooleanField(widget = django.forms.CheckboxInput(attrs={"onChange":'reloadFiles(this)'}),
                                                            required=False, label='Show non-default files: ',
                                                            initial=include_non_default)
            # handle any needed registration or unregistration
            register = handle_registration(args[0], user_email, expUrl, kinst, madDB)
            self.fields['register'] = django.forms.CharField(initial=register, 
                                                             widget=django.forms.HiddenInput(attrs={'value': register}),
                                                             required=False,
                                                             label=register)
            
            if fileSelection != '0':
                self.fields['file_buttons'] = django.forms.CharField(initial='', 
                                                                        widget=django.forms.HiddenInput(attrs={'value': ''}),
                                                                        required=False,
                                                                        label='')
                self.fields['basename'] = django.forms.CharField(initial=str(fileSelection), 
                                                                   widget=django.forms.HiddenInput(attrs={'value': str(fileSelection)}),
                                                                   required=False,
                                                                   label=str(fileSelection))
            
            
            
            
            
class SingleExpButtonsForm(django.forms.Form):
    """SingleExpButtonsForm is a Form class for the file buttons field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpButtonsForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
        request = getSelection('request', args, kwargs)
        madWebObj = madrigal.ui.web.MadrigalWeb(madDB, request)
        expID = getSelection('experiment_list', args, kwargs)
        try:
            expID = int(expID)
        except ValueError:
            # convert expPath to expID
            expID = madWebObj.getExpIDFromExpPath(expID, True)
        
        fileSelection = getSelection('file_list', args, kwargs)

        expDir = madExpObj.getExpDirByExpId(int(expID))
        filesize = os.path.getsize(os.path.join(expDir, str(fileSelection)))
        
        self.fields['file_buttons'] = django.forms.CharField(initial='', 
                                                                widget=django.forms.HiddenInput(attrs={'value': ''}),
                                                                required=False,
                                                                label='')
        self.fields['exp_id'] = django.forms.CharField(initial=str(expID), 
                                                           widget=django.forms.HiddenInput(attrs={'value': str(expID)}),
                                                           required=False,
                                                           label=str(expID))
        self.fields['basename'] = django.forms.CharField(initial=str(fileSelection), 
                                                           widget=django.forms.HiddenInput(attrs={'value': str(fileSelection)}),
                                                           required=False,
                                                           label=str(fileSelection))
        self.fields['filesize'] = django.forms.IntegerField(initial=filesize, 
                                                           widget=django.forms.HiddenInput(attrs={'value': filesize}),
                                                           required=False,
                                                           label=str(filesize))
        
        
class SingleExpPlotsForm(django.forms.Form):
    """SingleExpPlotsForm is a Form class for the file data/show plots field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpPlotsForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
        request = getSelection('request', args, kwargs)
        madWebObj = madrigal.ui.web.MadrigalWeb(madDB, request)
        expID = getSelection('experiment_list', args, kwargs)
        try:
            expID = int(expID)
        except ValueError:
            # convert expPath to expID
            expID = madWebObj.getExpIDFromExpPath(expID, True)
        plotList = madExpObj.getExpLinksByExpId(expID)
        if len(plotList) == 0:
            plotList = [('No plots available', '')]
        
        self.fields['plot_list'] = django.forms.ChoiceField(widget = django.forms.Select(),
                                                            choices=plotList,
                                                            required=False)
        
        
class SingleExpDownloadAsIsForm(django.forms.Form):
    """SingleExpDownloadAsIsForm is a Form class for the download as is field in the Single Experiment interface.
    Use this because its faster to create than the full SingleExpDefaultForm
    """
    def __init__(self, *args, **kwargs):
        super(SingleExpDownloadAsIsForm, self).__init__(*args, **kwargs)
        basename = getSelection('format_select', args, kwargs)
        expID = int(getSelection('expID', args, kwargs))
        formatChoices = getFormatChoices(basename, expID)
        
        self.fields['format_select'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(attrs={"onChange":'changeFormat(this)'}),
                                                                choices=formatChoices,
                                                                required=False)
        

    
    
class IsprintChoiceField(django.forms.TypedMultipleChoiceField):
    """IsprintChoiceField is subclass of TypedMultipleChoiceField
    """
    def __init__(self, *args, **kwargs):
        """__init__ allows all the arguments of TypedMultipleChoiceField, plus extra
        keyword arguments:
            madWebObject  madrigal.ui.web.MadrigalWeb object
        """
        self.isDerivedDict = kwargs.pop('isDerivedDict')
        self.parmDescDict = kwargs.pop('parmDescDict')
        try:
            self.separateProlog = kwargs.pop('separateProlog')
        except:
            self.separateProlog = False     
        
        kwargs['widget'] = IsprintWidget(attrs={'isDerivedDict':self.isDerivedDict,
                                                'parmDescDict':self.parmDescDict,
                                                'separateProlog':self.separateProlog})
        super(IsprintChoiceField, self).__init__(*args, **kwargs)
        self.widget.set_parm_lists(self.isDerivedDict, self.parmDescDict, self.separateProlog)
        

        
        
class IsprintWidget(django.forms.CheckboxSelectMultiple):
    """IsprintWidget is a subclass of CheckboxSelectMultiple with additional parameters passed in
    to modify rendering
    """
    def __init__(self, *args, **kwargs):
        """__init__ allows all the arguments of CheckboxSelectMultiple, plus extra
        keyword arguments:
            madWebObject  madrigal.ui.web.MadrigalWeb object
        """
        super(IsprintWidget, self).__init__(*args, **kwargs)
        self.renderer = django.forms.CheckboxSelectMultiple
        self.template_name = 'madweb/parameter_multiple.html'
        
        
    def set_parm_lists(self, isDerivedDict, parmDescDict, separateProlog=False):
        """set_parm_lists sets class variables used by the html renderer
        
        measParmList - parms in the file
        derivedParmList - parms derivable
        allParmList - above lists combined
        separateProlog - if true, create separate div tags so prolog parms can be hidden or not.
            Default is False, in which case no div tags are used
        """
        self.renderer.isDerivedDict = isDerivedDict
        self.renderer.parmDescDict = parmDescDict
        self.renderer.iterator = itertools.count()
        self.renderer.separateProlog = separateProlog

        
        
        
class IsprintForm(django.forms.Form):
    """IsprintForm is the form for the enhanced isprint page
    """
    def __init__(self, *args, **kwargs):
        
        madFileObj = getSelection('madFileObj', args, kwargs)
        type = getSelection('type', args, kwargs)
        formatChoices = [('ascii', 'Space-delimited ascii')]
        if type == 'download':
            formatChoices.insert(0, ('netCDF4', 'netCDF4'))
            formatChoices.insert(0, ('Hdf5', 'Hdf5'))
            separateProlog = True
        else:
            separateProlog = False
            
        madDB = getSelection('madDB', args, kwargs)
        madParmObj = getSelection('madParmObj', args, kwargs)
        derivedParmList = getSelection('derivedParmList', args, kwargs)
        allParmList = getSelection('allParmList', args, kwargs)
        allParmDescList = getSelection('allParmDescList', args, kwargs)
        self.parmList = list(zip(allParmList, allParmDescList))
        super(IsprintForm, self).__init__(*args, **kwargs)
        
        madCatObj = madrigal.metadata.MadrigalParmCategory(madDB)
        
        catList = madCatObj.getCategoryList()
        catDict = madParmObj.getCategoryDict(allParmList)
        
        choices = []
        isDerivedDict = {}
        parmDescDict = {}
        for catDesc, catID in catList:
            if catID not in list(catDict.keys()):
                continue
            theseParms = []
            for parm in catDict[catID][1]:
                theseParms.append((parm, parm))
                if parm in derivedParmList:
                    isDerivedDict[parm] = True
                else:
                    isDerivedDict[parm] = False
                parmDescDict[parm] = madParmObj.getParmDescription(parm)
            choices.append((catDesc, theseParms))
            
        choices_with_null = [('None', 'None')] + choices
        
        earliestTime = madFileObj.getEarliestTime()
        latestTime = madFileObj.getLatestTime()
        earliestDT = datetime.datetime(*earliestTime)
        latestDT = datetime.datetime(*latestTime)
        earliestStr = earliestDT.strftime('%Y-%m-%d %H:%M:%S')
        latestStr = latestDT.strftime('%Y-%m-%d %H:%M:%S')
        
        self.fields['fullFilename'] = django.forms.CharField(required=False, widget=django.forms.HiddenInput())
        self.fields['type'] = django.forms.CharField(required=False, widget=django.forms.HiddenInput())
        
        # format fields
        self.fields['formats'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(attrs={"onChange":'changeFormat(this)'}),
                                                          choices=formatChoices, initial='ascii',
                                                          required=False, label='Select output format:')
        
        self.fields['showHeaders'] = django.forms.BooleanField(required=False, label='Show headers:', 
                                                               help_text="Select this to show a header line before each record")
        
        self.fields['missing'] = django.forms.CharField(required=False,
                                                        label='Missing value string: ',
                                                        help_text='Modify this field to display something other than NaN for missing or assumed or known bad data.')
        
        # form fields
        self.fields['parameters'] = IsprintChoiceField(choices=choices,
                                                       required=False,
                                                       initial=['YEAR', 'MIN'],
                                                       isDerivedDict=isDerivedDict,
                                                       parmDescDict=parmDescDict,
                                                       separateProlog=separateProlog,
                                                       label = "")
        
        # time selection
        self.fields['start_date'] = django.forms.DateTimeField(input_formats=['%Y-%m-%dT%H:%M:%S'],
                                                               label='Start datetime',
                                                               help_text='Modify this field to remove all records before this start time.  Initial start datetime is the time of the first record.',
                                                               widget=django.forms.DateTimeInput(format='%Y-%m-%dT%H:%M:%S'),
                                                               required=False)

        self.fields['end_date'] = django.forms.DateTimeField(input_formats=['%Y-%m-%dT%H:%M:%S'],
                                                             label='End datetime',
                                                             help_text='Modify this field to remove all records after this end time.  Initial end datetime is the time of the last record.',
                                                             widget=django.forms.DateTimeInput(format='%Y-%m-%dT%H:%M:%S'),
                                                             required=False)                                              
        
        self.fields['parm_1'] = django.forms.ChoiceField(required=False,
                                                          choices=choices_with_null)
        self.fields['parm_1_lower'] = django.forms.CharField(required=False, initial='')
        self.fields['parm_1_upper'] = django.forms.CharField(required=False, initial='')
        
        self.fields['parm_2'] = django.forms.ChoiceField(required=False,
                                                          choices=choices_with_null)
        self.fields['parm_2_lower'] = django.forms.CharField(required=False, initial='')
        self.fields['parm_2_upper'] = django.forms.CharField(required=False, initial='')
        
        self.fields['parm_3'] = django.forms.ChoiceField(required=False,
                                                          choices=choices_with_null)
        self.fields['parm_3_lower'] = django.forms.CharField(required=False, initial='')
        self.fields['parm_3_upper'] = django.forms.CharField(required=False, initial='')
        
        # add optional fields
        if 'GDALT' in allParmList:
            min_alt_value = getSelection('min_alt', args, kwargs)
            max_alt_value = getSelection('max_alt', args, kwargs)
            if min_alt_value != max_alt_value:
                self.fields['min_alt'] = django.forms.CharField(required=False,
                                                                label='Min altitude:',
                                                                help_text='Modify this field to remove all data with altitudes below this level.')
                self.fields['max_alt'] = django.forms.CharField(required=False,
                                                                label='Max altitude:',
                                                                help_text='Modify this field to remove all data with altitudes above this level.')
            
        if 'AZM' in allParmList:
            self.fields['min_az'] = django.forms.CharField(required=False,
                                                           label='Min azimuth:',
                                                           help_text='Modify this field to remove all azimuths (-180 to 180) below this value. You can also have two separate azimuth ranges by using the Azimuth 2 fields.')
            self.fields['max_az'] = django.forms.CharField(required=False,
                                                            label='Max azimuth:',
                                                            help_text='Modify this field to remove all azimuths (-180 to 180) above this value. You can also have two separate azimuth ranges by using the Azimuth 2 fields.')
            self.fields['min_az2'] = django.forms.CharField(required=False,
                                                            label='Min azimuth 2:',
                                                            help_text='Modify this field to have a second allowed azimuth range. This would set the lower limit of the second range.')
            self.fields['max_az2'] = django.forms.CharField(required=False,
                                                            label='Max azimuth 2:',
                                                            help_text='Modify this field to have a second allowed azimuth range. This would set the upper limit of the second range.')
            
        if 'ELM' in allParmList:
            self.fields['min_el'] = django.forms.CharField(required=False,
                                                           label='Min elevation:',
                                                           help_text='Modify this field to remove all elevations (0 to 90) below this value. You can also have two separate elevations ranges by using the Elevations 2 fields.')
            self.fields['max_el'] = django.forms.CharField(required=False,
                                                            label='Max elevation:',
                                                            help_text='Modify this field to remove all elevations (0 to 90) above this value. You can also have two separate elevations ranges by using the Elevations 2 fields.')
            self.fields['min_el2'] = django.forms.CharField(required=False,
                                                            label='Min elevation 2:',
                                                            help_text='Modify this field to have a second allowed elevation range. This would set the lower limit of the second range.')
            self.fields['max_el2'] = django.forms.CharField(required=False,
                                                            label='Max elevation 2:',
                                                            help_text='Modify this field to have a second allowed elevation range. This would set the upper limit of the second range.')
            
        if 'PL' in allParmList:
            min_pl_value = getSelection('min_pl', args, kwargs)
            max_pl_value = getSelection('max_pl', args, kwargs)
            self.fields['min_pl'] = django.forms.CharField(required=False,
                                                           label='Min pulse len (microsec): ',
                                                           help_text='Modify this field to remove all pulse lengths in microsecs below this value.')
            self.fields['max_pl'] = django.forms.CharField(required=False,
                                                            label='Max pulse len (microsec): ',
                                                            help_text='Modify this field to remove all pulse lengths in microsecs above this value.')
            
    
    def clean_fullFilename(self):
        fullFilename = self.cleaned_data['fullFilename']
        # make sure the file exists
        if not os.access(fullFilename, os.R_OK):
            raise django.forms.ValidationError('Invalid filename: %(value)s cannot be opened',
                                               code='io_error',
                                               params={'value': fullFilename})
            
        return(fullFilename)
    
    
    def clean_formats(self):
        formats = self.cleaned_data['formats']
        # make sure the format valid
        if formats not in ('Hdf5', 'netCDF4', 'ascii'):
            raise django.forms.ValidationError('Invalid format: %(value)s not legal format',
                                               code='invalid',
                                               params={'value': formats})
            
        return(formats)
    
    
    def clean_min_alt(self):
        min_alt = self.cleaned_data['min_alt']
        if len(min_alt) != 0:
            try:
                min_alt_value = float(min_alt)
            except:
                raise django.forms.ValidationError('Invalid minimum altitude: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': min_alt})
            if min_alt_value < 0.0:
                raise django.forms.ValidationError('Invalid minimum altitude: %(value)s cannot be less than 0.0 kilometers',
                                                   code='invalid',
                                                   params={'value': min_alt})
        return(min_alt)
    
    def clean_max_alt(self):
        max_alt = self.cleaned_data['max_alt']
        if len(max_alt) != 0:
            try:
                max_alt_value = float(max_alt)
            except:
                raise django.forms.ValidationError('Invalid maximum altitude: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': max_alt})
            if max_alt_value < 0.0:
                raise django.forms.ValidationError('Invalid maximum altitude: %(value)s cannot be less than 0.0 kilometers',
                                                   code='invalid',
                                                   params={'value': max_alt})
        return(max_alt)
    
    
    def clean_min_az(self):
        min_az = self.cleaned_data['min_az']
        if len(min_az) != 0:
            try:
                min_az_value = float(min_az)
            except:
                raise django.forms.ValidationError('Invalid minimum azimuth: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': min_az})
            if min_az_value < -180.0:
                raise django.forms.ValidationError('Invalid minimum azimuth: %(value)s cannot be less than -180 degrees',
                                                   code='invalid',
                                                   params={'value': min_az})
            if min_az_value > 180.0:
                raise django.forms.ValidationError('Invalid minimum azimuth: %(value)s cannot be more than 180 degrees',
                                                   code='invalid',
                                                   params={'value': min_az})
        return(min_az)
    
    
    def clean_max_az(self):
        max_az = self.cleaned_data['max_az']
        if len(max_az) != 0:
            try:
                max_az_value = float(max_az)
            except:
                raise django.forms.ValidationError('Invalid maximum azimuth: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': max_az})
            if max_az_value < -180.0:
                raise django.forms.ValidationError('Invalid maximum azimuth: %(value)s cannot be less than -180 degrees',
                                                   code='invalid',
                                                   params={'value': max_az})
            if max_az_value > 180.0:
                raise django.forms.ValidationError('Invalid maximum azimuth: %(value)s cannot be more than 180 degrees',
                                                   code='invalid',
                                                   params={'value': max_az})
        return(max_az)
    
    
    def clean_min_el(self):
        min_el = self.cleaned_data['min_el']
        if len(min_el) != 0:
            try:
                min_el_value = float(min_el)
            except:
                raise django.forms.ValidationError('Invalid minimum elevation: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': min_el})
            if min_el_value < 0.0:
                raise django.forms.ValidationError('Invalid minimum elevation: %(value)s cannot be less than 0 degrees',
                                                   code='invalid',
                                                   params={'value': min_el})
            if min_el_value > 90.0:
                raise django.forms.ValidationError('Invalid minimum elevation: %(value)s cannot be more than 90 degrees',
                                                   code='invalid',
                                                   params={'value': min_el})
        return(min_el)
    
    
    def clean_max_el(self):
        max_el = self.cleaned_data['max_el']
        if len(max_el) != 0:
            try:
                max_el_value = float(max_el)
            except:
                raise django.forms.ValidationError('Invalid maximum elevation: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': max_el})
            if max_el_value < 0.0:
                raise django.forms.ValidationError('Invalid maximum elevation: %(value)s cannot be less than 0 degrees',
                                                   code='invalid',
                                                   params={'value': max_el})
            if max_el_value > 90.0:
                raise django.forms.ValidationError('Invalid maximum elevation: %(value)s cannot be more than 90 degrees',
                                                   code='invalid',
                                                   params={'value': max_el})
        return(max_el)
    
    
    def clean_min_az2(self):
        min_az2 = self.cleaned_data['min_az2']
        if len(min_az2) != 0:
            try:
                min_az2_value = float(min_az2)
            except:
                raise django.forms.ValidationError('Invalid minimum azimuth 2: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': min_az2})
            if min_az2_value < -180.0:
                raise django.forms.ValidationError('Invalid minimum azimuth 2: %(value)s cannot be less than -180 degrees',
                                                   code='invalid',
                                                   params={'value': min_az2})
            if min_az2_value > 180.0:
                raise django.forms.ValidationError('Invalid minimum azimuth 2: %(value)s cannot be more than 180 degrees',
                                                   code='invalid',
                                                   params={'value': min_az2})
        return(min_az2)
    
    
    def clean_max_az2(self):
        max_az2 = self.cleaned_data['max_az2']
        if len(max_az2) != 0:
            try:
                max_az2_value = float(max_az2)
            except:
                raise django.forms.ValidationError('Invalid maximum azimuth 2: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': max_az2})
            if max_az2_value < -180.0:
                raise django.forms.ValidationError('Invalid maximum azimuth 2: %(value)s cannot be less than -180 degrees',
                                                   code='invalid',
                                                   params={'value': max_az2})
            if max_az2_value > 180.0:
                raise django.forms.ValidationError('Invalid maximum azimuth 2: %(value)s cannot be more than 180 degrees',
                                                   code='invalid',
                                                   params={'value': max_az2})
        return(max_az2)
    
    
    def clean_min_el2(self):
        min_el2 = self.cleaned_data['min_el2']
        if len(min_el2) != 0:
            try:
                min_el2_value = float(min_el2)
            except:
                raise django.forms.ValidationError('Invalid minimum elevation 2: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': min_el2})
            if min_el2_value < 0.0:
                raise django.forms.ValidationError('Invalid minimum elevation 2: %(value)s cannot be less than 0 degrees',
                                                   code='invalid',
                                                   params={'value': min_el2})
            if min_el2_value > 90.0:
                raise django.forms.ValidationError('Invalid minimum elevation 2: %(value)s cannot be more than 90 degrees',
                                                   code='invalid',
                                                   params={'value': min_el2})
        return(min_el2)
    
    
    def clean_max_el2(self):
        max_el2 = self.cleaned_data['max_el2']
        if len(max_el2) != 0:
            try:
                max_el2_value = float(max_el2)
            except:
                raise django.forms.ValidationError('Invalid maximum elevation 2: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': max_el2})
            if max_el2_value < 0.0:
                raise django.forms.ValidationError('Invalid maximum elevation 2: %(value)s cannot be less than 0 degrees',
                                                   code='invalid',
                                                   params={'value': max_el2})
            if max_el2_value > 90.0:
                raise django.forms.ValidationError('Invalid maximum elevation 2: %(value)s cannot be more than 90 degrees',
                                                   code='invalid',
                                                   params={'value': max_el2})
        return(max_el2)
    
    
    def clean_min_pl(self):
        min_pl = self.cleaned_data['min_pl']
        if len(min_pl) != 0:
            try:
                min_pl_value = float(min_pl)
            except:
                raise django.forms.ValidationError('Invalid lower limit for pulse length: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': min_pl})
        return(min_pl)
    
    
    def clean_max_pl(self):
        max_pl = self.cleaned_data['max_pl']
        if len(max_pl) != 0:
            try:
                max_pl_value = float(max_pl)
            except:
                raise django.forms.ValidationError('Invalid upper limit for pulse length: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': max_pl})
        return(max_pl)
    
    
    def clean_parm_1_lower(self):
        parm_1_lower = self.cleaned_data['parm_1_lower']
        if len(parm_1_lower) != 0:
            try:
                parm_1_lower_value = float(parm_1_lower)
            except:
                raise django.forms.ValidationError('Invalid lower limit for parm 1: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': parm_1_lower})
        return(parm_1_lower)
    
    
    def clean_parm_1_upper(self):
        parm_1_upper = self.cleaned_data['parm_1_upper']
        if len(parm_1_upper) != 0:
            try:
                parm_1_upper_value = float(parm_1_upper)
            except:
                raise django.forms.ValidationError('Invalid upper limit for parm 1: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': parm_1_upper})
        return(parm_1_upper)
    
    
    def clean_parm_2_lower(self):
        parm_2_lower = self.cleaned_data['parm_2_lower']
        if len(parm_2_lower) != 0:
            try:
                parm_2_lower_value = float(parm_2_lower)
            except:
                raise django.forms.ValidationError('Invalid lower limit for parm 2: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': parm_2_lower})
        return(parm_2_lower)
    
    
    def clean_parm_2_upper(self):
        parm_2_upper = self.cleaned_data['parm_2_upper']
        if len(parm_2_upper) != 0:
            try:
                parm_2_upper_value = float(parm_2_upper)
            except:
                raise django.forms.ValidationError('Invalid upper limit for parm 2: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': parm_2_upper})
        return(parm_2_upper)
    
    
    def clean_parm_3_lower(self):
        parm_3_lower = self.cleaned_data['parm_3_lower']
        if len(parm_3_lower) != 0:
            try:
                parm_3_lower_value = float(parm_3_lower)
            except:
                raise django.forms.ValidationError('Invalid lower limit for parm 3: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': parm_3_lower})
        return(parm_3_lower)
    
    
    def clean_parm_3_upper(self):
        parm_3_upper = self.cleaned_data['parm_3_upper']
        if len(parm_3_upper) != 0:
            try:
                parm_3_upper_value = float(parm_3_upper)
            except:
                raise django.forms.ValidationError('Invalid upper limit for parm 3: %(value)s cannot be converted to a float',
                                                   code='invalid',
                                                   params={'value': parm_3_upper})
        return(parm_3_upper)
    
    
    
    
    def clean(self):
        """clean in the Django method to validate things in a form that require looking at multiple fields
        """
        # rule 1 - start_date < end_date
        if self.cleaned_data['start_date'] > self.cleaned_data['end_date']:
            raise django.forms.ValidationError('Error - start datetime greater than end datetime.')
        
        # rule 2 - min_alt <= max_alt
        try:
            min_alt = float(self.cleaned_data['min_alt'])
            max_alt = float(self.cleaned_data['max_alt'])
        except:
            min_alt = 0.0
            max_alt = 1.0
        if min_alt > max_alt:
            raise django.forms.ValidationError('Error - Minimum altitude greater than maximum altitude.')
        
        # rule 3 - min_az <= max_az
        try:
            min_az = float(self.cleaned_data['min_az'])
            max_az = float(self.cleaned_data['max_az'])
        except:
            min_az = 0.0
            max_az = 1.0
        if min_az > max_az:
            raise django.forms.ValidationError('Error - Minimum azimuth greater than maximum azimuth.')
        
        # rule 4 - min_el <= max_el
        try:
            min_el = float(self.cleaned_data['min_el'])
            max_el = float(self.cleaned_data['max_el'])
        except:
            min_el = 0.0
            max_el = 1.0
        if min_el > max_el:
            raise django.forms.ValidationError('Error - Minimum elevation greater than maximum elevation.')
        
        # rule 5 - min_az2 <= max_az2
        try:
            min_az2 = float(self.cleaned_data['min_az2'])
            max_az2 = float(self.cleaned_data['max_az2'])
        except:
            min_az2 = 0.0
            max_az2 = 1.0
        if min_az2 > max_az2:
            raise django.forms.ValidationError('Error - Minimum azimuth 2 greater than maximum azimuth 2.')
        
        # rule 6 - min_el2 <= max_el2
        try:
            min_el2 = float(self.cleaned_data['min_el2'])
            max_el2 = float(self.cleaned_data['max_el2'])
        except:
            min_el2 = 0.0
            max_el2 = 1.0
        if min_el2 > max_el2:
            raise django.forms.ValidationError('Error - Minimum elevation 2 greater than maximum elevation 2.')
        
        # rule 7 - min_pl <= max_pl
        try:
            min_pl = float(self.cleaned_data['min_pl'])
            max_pl = float(self.cleaned_data['max_pl'])
        except:
            min_pl = 0.0
            max_pl = 1.0
        if min_pl > max_pl:
            raise django.forms.ValidationError('Error - Minimum pulse length greater than maximum pulse length.')
        
        # rule 8 - parm_1_lower <= parm_1_upper
        try:
            parm_1_lower = float(self.cleaned_data['parm_1_lower'])
            parm_1_upper = float(self.cleaned_data['parm_1_upper'])
        except:
            parm_1_lower = 0.0
            parm_1_upper = 1.0
        if parm_1_lower > parm_1_upper:
            raise django.forms.ValidationError('Error - parm 1 lower limit greater than upper limit.')
        
        # rule 9 - parm_2_lower <= parm_2_upper
        try:
            parm_2_lower = float(self.cleaned_data['parm_2_lower'])
            parm_2_upper = float(self.cleaned_data['parm_2_upper'])
        except:
            parm_2_lower = 0.0
            parm_2_upper = 1.0
        if parm_2_lower > parm_2_upper:
            raise django.forms.ValidationError('Error - parm 2 lower limit greater than upper limit.')
        
        # rule 10 - parm_3_lower <= parm_3_upper
        try:
            parm_3_lower = float(self.cleaned_data['parm_3_lower'])
            parm_3_upper = float(self.cleaned_data['parm_3_upper'])
        except:
            parm_3_lower = 0.0
            parm_3_upper = 1.0
        if parm_3_lower > parm_3_upper:
            raise django.forms.ValidationError('Error - parm 3 lower limit greater than upper limit.')
        
        
class ListExpForm(django.forms.Form):
    """ListExpForm is a Form class for the default fields in the List Experiment interface
    """
    def __init__(self, *args, **kwargs):
        super(ListExpForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        isTrusted = bool(kwargs['initial']['isTrusted'])
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB, isTrusted)
        isGlobal = True # default load
        now = datetime.datetime.now()
        endDateTime = datetime.datetime(now.year, 12, 31, 23, 59, 59)
        self.fields['isGlobal'] = django.forms.BooleanField(widget = django.forms.CheckboxInput(attrs={"onChange":'changeGlobal(this.form)'}),
                                                            required=False, label='Use all Madrigal sites: ',
                                                            initial=isGlobal)
        
        categoriesSelection = '0'
        self.fields['categories'] = django.forms.MultipleChoiceField(widget=django.forms.SelectMultiple(attrs={"onChange":"updateInstruments(this.form)"}),
                                                                     choices=getCategoryList(args, kwargs, madInstData, True), 
                                                                     initial='0',
                                                                     label='Choose instrument category(s):')
        
        self.fields['instruments'] = django.forms.MultipleChoiceField(widget = django.forms.SelectMultiple(),
                                                                      choices=getInstrumentList(args, kwargs, madInstData, 'All instruments', local=False, includeYears=True),
                                                                      initial='0', label='Choose instrument(s)')
        
        self.fields['showDefault'] = django.forms.BooleanField(widget = django.forms.CheckboxInput(),
                                                               label='Show only default files: ',
                                                               initial=True, required=False)
        
        # time selection
        self.fields['start_date'] = django.forms.SplitDateTimeField(input_date_formats=['%Y-%m-%d'], input_time_formats=['%H:%M:%S'],
                                                                  label='Start date',
                                                                  help_text='Modify this field to select experiments after this start time.',
                                                                  initial=datetime.datetime(1950,1,1))
        
        self.fields['end_date'] = django.forms.SplitDateTimeField(input_date_formats=['%Y-%m-%d'], input_time_formats=['%H:%M:%S'],
                                                                  label='End date',
                                                                  help_text='Modify this field to select experiments before this end time.',
                                                                  initial=endDateTime) 
        

        
        # attributes to support javascript
        local_categories = madInstData.getCategories(True)
        global_categories = madInstData.getCategories(False)
        local_category_ids = [cat[0] for cat in local_categories]
        self.categories = [] # each item a tuple of (category description, category id, global only js bool)
        self.instruments = [] # each item a tiple of (instrument desc, category id, kinst, local start year, local end year, global sy, global ey)
        for id, desc in global_categories:
            if id in local_category_ids:
                localBool = 'false'
            else:
                localBool = 'true'
            self.categories.append((desc, id, localBool))
            for kinst, instDesc, siteId in madInstData.getInstruments(id):
                if siteId == madDB.getSiteID():
                    localInst = True
                else:
                    localInst = False
                yearList = madInstData.getInstrumentYears(kinst)
                if localInst:
                    self.instruments.append((instDesc, id, kinst, yearList[0], yearList[-1], yearList[0], yearList[-1]))
                else:
                    self.instruments.append((instDesc, id, kinst, 0, 0, yearList[0], yearList[-1]))
                    
                    
    def clean(self):
        """clean in the Django method to validate things in a form that require looking at multiple fields
        """
        # rule 1 - start_date < end_date
        if self.cleaned_data['start_date'] > self.cleaned_data['end_date']:
            raise django.forms.ValidationError('Error - start datetime greater than end datetime.')
        
        
class DownloadAsIsScriptForm(django.forms.Form):
    """DownloadAsIsScriptForm is a Form class for the default fields in the download files as is script generator interface
    """
    def __init__(self, *args, **kwargs):
        super(DownloadAsIsScriptForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        isTrusted = bool(kwargs['initial']['isTrusted'])
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB, isTrusted)
        madKindatObj = madrigal.metadata.MadrigalKindat(madDB)
        madInstKindatObj = madrigal.metadata.MadrigalInstrumentKindats(madDB)
        now = datetime.datetime.now()
        endDateTime = datetime.datetime(now.year, 12, 31, 23, 59, 59)
        kwargs['isGlobal'] = '0'
        formatChoices = (('hdf5', 'Hdf5'), ('ascii', 'Space-delimited ascii'), ('netCDF4', 'netCDF4'))
        languageChoices = (('python', 'python'), ('Matlab', 'Matlab'), ('IDL', 'IDL'))
        
        categoriesSelection = '0'
        self.fields['categories'] = django.forms.ChoiceField(widget=django.forms.Select(attrs={"onChange":"updateInstruments(this.form)"}),
                                                            choices=getCategoryList(args, kwargs, madInstData), 
                                                            initial='0',
                                                            label='Choose an instrument category if desired:')
        
        self.fields['instruments'] = django.forms.ChoiceField(widget=django.forms.Select(attrs={"onChange":"updateKindats(this.form)"}),
                                                              choices=getInstrumentList(args, kwargs, madInstData, local=True, includeYears=True),
                                                              initial='0', label='Choose one instrument')
        
        # time selection
        self.fields['start_date'] = django.forms.DateField(input_formats=['%Y-%m-%d'],
                                                                    label='Start date',
                                                                    help_text='Modify this field to select experiments after this start time.',
                                                                    initial=datetime.datetime(1950,1,1))
        
        
        self.fields['end_date'] = django.forms.DateField(input_formats=['%Y-%m-%d'],
                                                                  label='End date',
                                                                  help_text='Modify this field to select experiments before this end time.',
                                                                  initial=endDateTime)
        
        self.fields['format_select'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(),
                                                                choices=formatChoices,
                                                                initial=formatChoices[0][0],
                                                                label='File format to download:')
        
        self.fields['language_select'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(),
                                                                    choices=languageChoices,
                                                                    initial=languageChoices[0][0],
                                                                    label='Choose scripting language:')
        
        choices=(('0', 'Select an instrument first to see list'),)
        if len(args) > 0:
            if 'kindat_select' in args[0]:
                choices = [(kindat, kindat) for kindat in args[0].getlist('kindat_select')]
        self.fields['kindat_select'] = django.forms.MultipleChoiceField(choices=choices, 
                                                                        initial='0',
                                                                        label='Choose one or more kinds of data:',
                                                                        required=False)
        self.fields['kindat_select'].widget.attrs['style']="max-width:100%;"
        
        self.fields['expName'] = django.forms.CharField(max_length=256, required=False)
        
        self.fields['fileDesc'] = django.forms.CharField(max_length=256, required=False)
        

        
        # attributes to support javascript
        local_categories = madInstData.getCategories(True)
        local_category_ids = [cat[0] for cat in local_categories]
        self.categories = [] # each item a tuple of (category description, category id, global only js bool)
        self.instruments = [] # each item a tiple of (instrument desc, category id, kinst, local start year, local end year)
        self.kindats = [] # each item is a tuple of (kindat, kindatDesc, kinst)
        for id, desc in local_categories:
            self.categories.append((desc, id))
            for kinst, instDesc, siteId in madInstData.getInstruments(id):
                if siteId != madDB.getSiteID():
                    continue
                yearList = madInstData.getInstrumentYears(kinst)
                self.instruments.append((instDesc, id, kinst, yearList[0], yearList[-1]))
                kindatList = madInstKindatObj.getKindatListForInstruments([kinst])
                for kindat in kindatList:
                    self.kindats.append((kindat, madKindatObj.getKindatDescription(kindat, kinst),
                                         kinst))
                    
                    
    def clean(self):
        """clean is the Django method to validate things in a form that require looking at multiple fields
        """
        # rule 1 - start_date < end_date
        if self.cleaned_data['start_date'] > self.cleaned_data['end_date']:
            raise django.forms.ValidationError('Error - start datetime greater than end datetime.')
        
        
class DownloadAdvancedScriptForm(django.forms.Form):
    """DownloadAdvancedScriptForm is a Form class for the default fields in the download advanced script generator interface
    """
    def __init__(self, *args, **kwargs):
        super(DownloadAdvancedScriptForm, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        isTrusted = bool(kwargs['initial']['isTrusted'])
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB, isTrusted)
        madKindatObj = madrigal.metadata.MadrigalKindat(madDB)
        madInstKindatObj = madrigal.metadata.MadrigalInstrumentKindats(madDB)
        now = datetime.datetime.now()
        endDateTime = datetime.datetime(now.year, 12, 31, 23, 59, 59)
        kwargs['isGlobal'] = '0'
        formatChoices = (('hdf5', 'Hdf5'), ('ascii', 'Space-delimited ascii'), ('netCDF4', 'netCDF4'))
        directoryChoices = (('Directory', 'Directory'), ('File', 'File'))
        languageChoices = (('python', 'python'), ('Matlab', 'Matlab'), ('IDL', 'IDL'))
        
        categoriesSelection = '0'
        self.fields['categories'] = django.forms.ChoiceField(widget=django.forms.Select(attrs={"onChange":"updateInstruments(this.form)"}),
                                                            choices=getCategoryList(args, kwargs, madInstData), 
                                                            initial='0',
                                                            label='Choose an instrument category if desired:')
        
        self.fields['instruments'] = django.forms.ChoiceField(widget=django.forms.Select(attrs={"onChange":"updateParmsKindats(this.form)"}),
                                                              choices=getInstrumentList(args, kwargs, madInstData, local=True, includeYears=True),
                                                              initial='0', label='Choose one instrument')
        
        # time selection
        self.fields['start_date'] = django.forms.DateField(input_formats=['%Y-%m-%d'],
                                                                    label='Start date',
                                                                    help_text='Modify this field to select experiments after this start time.',
                                                                    initial=datetime.datetime(1950,1,1))
        
        self.fields['end_date'] = django.forms.DateField(input_formats=['%Y-%m-%d'],
                                                                  label='End date',
                                                                  help_text='Modify this field to select experiments before this end time.',
                                                                  initial=endDateTime)
        
        self.fields['format_select'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(attrs={"onChange":'changeFormat(this)'}),
                                                                choices=formatChoices,
                                                                initial=formatChoices[0][0],
                                                                label='File format to download:')
        
        self.fields['directory_select'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(),
                                                                   choices=directoryChoices,
                                                                   initial=directoryChoices[0][0],
                                                                   label='If ascii, download result to:')
        
        self.fields['language_select'] = django.forms.ChoiceField(widget = HorizontalRadioSelect(),
                                                                    choices=languageChoices,
                                                                    initial=languageChoices[0][0],
                                                                    label='Choose scripting language:')
        
        choices=(('0', 'Select an instrument first to see list'),)
        if len(args) > 0:
            if 'kindat_select' in args[0]:
                choices = [(kindat, kindat) for kindat in args[0].getlist('kindat_select')]
        self.fields['kindat_select'] = django.forms.MultipleChoiceField(choices=choices, 
                                                                        initial='0',
                                                                        label='Choose one or more kinds of data:',
                                                                        required=False)
        self.fields['kindat_select'].widget.attrs['style']="max-width:100%;"
        
        self.fields['expName'] = django.forms.CharField(max_length=256, required=False)
        
        self.fields['fileDesc'] = django.forms.CharField(max_length=256, required=False)
        
        self.fields['seasonalStartDay'] = django.forms.IntegerField(min_value=1, max_value=31, initial=1)
        self.fields['seasonalStartMonth'] = django.forms.IntegerField(min_value=1, max_value=12, initial=1)
        self.fields['seasonalEndDay'] = django.forms.IntegerField(min_value=1, max_value=31, initial=31)
        self.fields['seasonalEndMonth'] = django.forms.IntegerField(min_value=1, max_value=12, initial=12)
        
        
        # attributes to support javascript
        local_categories = madInstData.getCategories(True)
        local_category_ids = [cat[0] for cat in local_categories]
        self.categories = [] # each item a tuple of (category description, category id, global only js bool)
        self.instruments = [] # each item a tiple of (instrument desc, category id, kinst, local start year, local end year)
        self.kindats = [] # each item is a tuple of (kindat, kindatDesc, kinst)
        for id, desc in local_categories:
            self.categories.append((desc, id))
            for kinst, instDesc, siteId in madInstData.getInstruments(id):
                if siteId != madDB.getSiteID():
                    continue
                yearList = madInstData.getInstrumentYears(kinst)
                self.instruments.append((instDesc, id, kinst, yearList[0], yearList[-1]))
                kindatList = madInstKindatObj.getKindatListForInstruments([kinst])
                for kindat in kindatList:
                    self.kindats.append((kindat, madKindatObj.getKindatDescription(kindat, kinst),
                                         kinst))
                    
                    
    def clean(self):
        """clean is the Django method to validate things in a form that require looking at multiple fields
        """
        # rule 1 - start_date < end_date
        if self.cleaned_data['start_date'] > self.cleaned_data['end_date']:
            raise django.forms.ValidationError('Error - start datetime greater than end datetime.')
        
        # rule 2 - seasonal start must be before seasonal end
        if ((self.cleaned_data['seasonalStartDay'] + 31*self.cleaned_data['seasonalStartMonth']) > \
            (self.cleaned_data['seasonalEndDay'] + 31*self.cleaned_data['seasonalEndMonth'])):
            raise django.forms.ValidationError('Error - seasonal start after seasonal end.')
        
        
        
class AdvScriptParmsForm(django.forms.Form):
    """AdvScriptParmsForm is the form for the parameters
    """
    def __init__(self, *args, **kwargs):
        
        kinst = int(getSelection('instruments', args, kwargs))
        if kinst == 0:
            raise ValueError('kinst should never be zero')
        madDB = madrigal.metadata.MadrigalDB()
        madParmObj = madrigal.data.MadrigalParameters(madDB)
        parmList = madParmObj.getParametersForInstruments([kinst])
        self.parmList = []
        for parm in parmList:
            self.parmList.append((parm, madParmObj.getSimpleParmDescription(parm)))
            
        madCatObj = madrigal.metadata.MadrigalParmCategory(madDB)
        
        catList = madCatObj.getCategoryList()
        catDict = madParmObj.getCategoryDict(parmList)
        
        choices = []
        isDerivedDict = {}
        parmDescDict = {}
        for catDesc, catID in catList:
            if catID not in list(catDict.keys()):
                continue
            theseParms = []
            for parm in catDict[catID][1]:
                theseParms.append((parm, parm))
                isDerivedDict[parm] = False
                parmDescDict[parm] = madParmObj.getParmDescription(parm)
            choices.append((catDesc, theseParms))
            
        super(AdvScriptParmsForm, self).__init__(*args, **kwargs)

        self.fields['parameters'] = IsprintChoiceField(choices=choices,
                                                       initial=['YEAR', 'MIN'],
                                                       required=False,
                                                       isDerivedDict=isDerivedDict,
                                                       parmDescDict=parmDescDict,
                                                       separateProlog=False,
                                                       label = "")
        
        
class AdvScriptParmsFiltersForm(django.forms.Form):
    """AdvScriptParmsFiltersForm is the form for the parameter filters in the scrip generator
    """
    def __init__(self, *args, **kwargs):
        
        kinst = int(getSelection('instruments', args, kwargs))
        if kinst == 0:
            raise ValueError('kinst should never be zero')
        madDB = madrigal.metadata.MadrigalDB()
        madParmObj = madrigal.data.MadrigalParameters(madDB)
        parmList = madParmObj.getParametersForInstruments([kinst])
        self.parmList = []
        for parm in parmList:
            self.parmList.append((parm, madParmObj.getSimpleParmDescription(parm)))
            
        super(AdvScriptParmsFiltersForm, self).__init__(*args, **kwargs)

        choices = [(parm, parm) for parm in parmList]
        choices_with_null = [('None', 'None')] + choices
        
        self.fields['parm_1'] = django.forms.ChoiceField(required=False,
                                                          choices=choices_with_null)
        self.fields['parm_1_lower'] = django.forms.CharField(required=False, initial='')
        self.fields['parm_1_upper'] = django.forms.CharField(required=False, initial='')
        
        self.fields['parm_2'] = django.forms.ChoiceField(required=False,
                                                          choices=choices_with_null)
        self.fields['parm_2_lower'] = django.forms.CharField(required=False, initial='')
        self.fields['parm_2_upper'] = django.forms.CharField(required=False, initial='')
        
        self.fields['parm_3'] = django.forms.ChoiceField(required=False,
                                                          choices=choices_with_null)
        self.fields['parm_3_lower'] = django.forms.CharField(required=False, initial='')
        self.fields['parm_3_upper'] = django.forms.CharField(required=False, initial='')
        
        
        
class MadCalculatorForm(django.forms.Form):
    """MadCalculatorForm is the form for the madCalculator page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        madDB = madrigal.metadata.MadrigalDB()
        madParmObj = madrigal.data.MadrigalParameters(madDB)
        super(MadCalculatorForm, self).__init__(*args, **kwargs)
        fullDerivedParmList = madrigal.derivation.getDerivableParms(['gdalt', 'gdlat', 'glon'])
        
        # removed unwanted time and prolog parameters
        rejectedCats = ('Time Related Parameter', 'Prolog Parameters', 'Radar Instrument Operation Parameter',
                        'Madrigal Hdf5 Prolog Parameters')
        # define time parameters that do make sense to calculate
        neededTimeParms = ('APLT', 'CONJ_SUNRISE_H', 'CONJ_SUNSET_H',
                           'SUNRISE_H', 'SUNSET_H', 'MLT')
        
        # skip parms related to inst location
        instLocationParms = ('AZM','ELM','GALTR','GDLONR','GDLATR','RANGE', 'ASPECT',
                             'GDALT', 'GDLAT', 'GLON', 'CXR', 'CYR', 'CZR')
        
        madCatObj = madrigal.metadata.MadrigalParmCategory(madDB)
        
        catList = madCatObj.getCategoryList()
        catDict = madParmObj.getCategoryDict(fullDerivedParmList)
        choices = []
        isDerivedDict = {}
        self.parmDescDict = {}
        for catDesc, catID in catList:
            if catDesc in rejectedCats[1:]:
                continue
            if catID not in list(catDict.keys()):
                continue
            theseParms = []
            for parm in catDict[catID][1]:
                if parm in instLocationParms:
                    continue
                if catDesc in rejectedCats and parm not in neededTimeParms:
                    continue
                if not parm in fullDerivedParmList:
                    continue
                theseParms.append((parm, parm))
                isDerivedDict[parm] = True
                self.parmDescDict[parm] = madParmObj.getParmDescription(parm)
            choices.append((catDesc, theseParms))
            
        
        
        # form fields
        self.fields['min_latitude'] = django.forms.FloatField(initial=-90.0, min_value=-90.0,
                                                              max_value=90.0)
        self.fields['max_latitude'] = django.forms.FloatField(initial=90.0, min_value=-90.0,
                                                              max_value=90.0)
        self.fields['delta_latitude'] = django.forms.FloatField(initial=45, min_value=1.0E-6)
        
        self.fields['min_longitude'] = django.forms.FloatField(initial=-180.0, min_value=-180.0,
                                                              max_value=180.0)
        self.fields['max_longitude'] = django.forms.FloatField(initial=180.0, min_value=-180.0,
                                                              max_value=180.0)
        self.fields['delta_longitude'] = django.forms.FloatField(initial=90, min_value=1.0E-6)
        
        self.fields['min_altitude'] = django.forms.FloatField(initial=0.0, min_value=0.0)
        self.fields['max_altitude'] = django.forms.FloatField(initial=600.0, min_value=0.0)
        self.fields['delta_altitude'] = django.forms.FloatField(initial=200, min_value=1.0E-6)
        
        # time selection
        now = datetime.datetime.utcnow()
        self.fields['datetime'] = django.forms.SplitDateTimeField(input_date_formats=['%Y-%m-%d'], input_time_formats=['%H:%M:%S'],
                                                                  label='Select UT datetime',
                                                                  help_text='Select the UT time at which to run this calcuation',
                                                                  initial=datetime.datetime(now.year,1,1))
        
        self.fields['parameters'] = IsprintChoiceField(choices=choices,
                                                       required=False,
                                                       isDerivedDict=isDerivedDict,
                                                       parmDescDict=self.parmDescDict,
                                                       separateProlog=False,
                                                       label = "")
                
        
    def clean(self):
        """clean is the Django method to validate things in a form that require looking at multiple fields
        """
        max_len = 1.0E5
        try:
            a1 = numpy.arange(self.cleaned_data['min_latitude'], self.cleaned_data['max_latitude'], self.cleaned_data['delta_latitude'])
            if len(a1) > max_len:
                raise django.forms.ValidationError('Too many latitudes: %i.' % (len(a1)))
        except ZeroDivisionError:
            raise django.forms.ValidationError('Infinite latitudes')
        
        try:
            a2 = numpy.arange(self.cleaned_data['min_longitude'], self.cleaned_data['max_longitude'], self.cleaned_data['delta_longitude'])
            if len(a1) > max_len:
                raise django.forms.ValidationError('Too many longitudes: %i.' % (len(a1)))
        except ZeroDivisionError:
            raise django.forms.ValidationError('Infinite longitudes')
        
        try:
            a3 = numpy.arange(self.cleaned_data['min_altitude'], self.cleaned_data['max_altitude'], self.cleaned_data['delta_altitude'])
            if len(a1) > max_len:
                raise django.forms.ValidationError('Too many altitudes: %i.' % (len(a1)))
        except ZeroDivisionError:
            raise django.forms.ValidationError('Infinite altitudes')
        
        total = len(a1) * len(a2) * len(a3)
        if total > max_len:
            raise django.forms.ValidationError('Too many calculations: %i' % (total))
        
        

class GetMetadataForm(django.forms.Form):
    """GetMetadataForm is the form for the getMetadata page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(GetMetadataForm, self).__init__(*args, **kwargs)
        
        fileTypeChoices = (("0", "Experiment Table"),
                           ("1", "File Table"),
                           ("3", "Instrument Table"),
                           ("4", "Parameter Table"),
                           ("5", "Site Table"),
                           ("6", "Type Table"),
                           ("7", "Instrument Kindat Table"),
                           ("8", "Instrument Parameter Table"),
                           ("9", "Madrigal categories table"),
                           ("10", "Instrument categories table"),)
        
        self.fields['fileType'] = django.forms.ChoiceField(widget = django.forms.RadioSelect,
                                                           choices=fileTypeChoices,
                                                           initial="0",
                                                           label="Choose the metadata file type to download")
        
        
class LookerSelectForm(django.forms.Form):
    """LookerSelectForm is the form for the looker_form page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerSelectForm, self).__init__(*args, **kwargs)
        
        lookerOptionChoices = (("1", "Geodetic latitude, longitude and altitude of the points"),
                               ("2", "Apex latitude, longitude and altitude of the points"),
                               ("3", "Geodetic latitude, longitude and altitude of the points"),
                               ("4", "Azimuth, elevation and range of the points from a specified instrument (output includes aspect angle)"),
                               ("5", "Azimuth, elevation, range from specified instrument"),
                               ("6", "Geodetic latitude, longitude, altitude of a point on the field line"),
                               ("7", "Apex latitude, longitude of the field line"),
                               ("8", "Geodetic latitude, longitude and altitude of the points"))
        
        self.fields['looker_options'] = django.forms.ChoiceField(widget = django.forms.RadioSelect,
                                                           choices=lookerOptionChoices,
                                                           initial="1")
        
        
class LookerGeodeticRadar(django.forms.Form):
    """LookerGeodeticRadar is the form for the geodetic to radar page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerGeodeticRadar, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB)
        dict1 = {'isGlobal': True, 'categories': "0"}
        
        self.fields['looker_options'] = django.forms.CharField(initial="1", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "1"}))
        
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={'size':'3'}),
                                                              choices=getInstrumentList([], dict1, madInstData, 'Set location manually', local=False),
                                                              required=False, label='Instrument: ')
        
        self.fields['inst_lat'] = django.forms.FloatField(initial=0.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_lon'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_lat'] = django.forms.FloatField(initial=30.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lat'] = django.forms.FloatField(initial=50.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lat'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_lon'] = django.forms.FloatField(initial=-100.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lon'] = django.forms.FloatField(initial=-80.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lon'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        
class LookerGeomagRadar(django.forms.Form):
    """LookerGeomagRadar is the form for the geomagnetic to radar page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerGeomagRadar, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB)
        dict1 = {'isGlobal': True, 'categories': "0"}
        
        self.fields['looker_options'] = django.forms.CharField(initial="2", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "2"}))
        
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={'size':'3'}),
                                                              choices=getInstrumentList([], dict1, madInstData, 'Set location manually', local=False),
                                                              required=False, label='Instrument: ')
        
        self.fields['inst_lat'] = django.forms.FloatField(initial=0.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_lon'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_lat'] = django.forms.FloatField(initial=30.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lat'] = django.forms.FloatField(initial=50.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lat'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_lon'] = django.forms.FloatField(initial=-100.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lon'] = django.forms.FloatField(initial=-80.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lon'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        now = datetime.datetime.utcnow()
        self.fields['year'] = django.forms.FloatField(initial=float(now.year), min_value=1950.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
class LookerGeomagFromGeodetic(django.forms.Form):
    """LookerGeomagFromGeodetic is the form for the geomagnetic from geodetic page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerGeomagFromGeodetic, self).__init__(*args, **kwargs)
        
        self.fields['looker_options'] = django.forms.CharField(initial="3", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "3"}))
        
        self.fields['start_lat'] = django.forms.FloatField(initial=30.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lat'] = django.forms.FloatField(initial=50.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lat'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_lon'] = django.forms.FloatField(initial=-100.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lon'] = django.forms.FloatField(initial=-80.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lon'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        now = datetime.datetime.utcnow()
        self.fields['year'] = django.forms.FloatField(initial=float(now.year), min_value=1950.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
class LookerGeomagFromRadar(django.forms.Form):
    """LookerGeomagFromRadar is the form for the geomagnetic from radar page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerGeomagFromRadar, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB)
        dict1 = {'isGlobal': True, 'categories': "0"}
        
        self.fields['looker_options'] = django.forms.CharField(initial="4", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "4"}))
        
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={'size':'3'}),
                                                              choices=getInstrumentList([], dict1, madInstData, local=False)[1:],
                                                              required=False, label='Instrument: ')
        
        self.fields['inst_lat'] = django.forms.FloatField(initial=0.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_lon'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_az'] = django.forms.FloatField(initial=-180.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_az'] = django.forms.FloatField(initial=180.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_az'] = django.forms.FloatField(initial=45.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_el'] = django.forms.FloatField(initial=0.0, min_value=0.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_el'] = django.forms.FloatField(initial=90.0, min_value=0.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_el'] = django.forms.FloatField(initial=30.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_range'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_range'] = django.forms.FloatField(initial=600.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_range'] = django.forms.FloatField(initial=200.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        now = datetime.datetime.utcnow()
        self.fields['year'] = django.forms.FloatField(initial=float(now.year), min_value=1950.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        
class LookerFieldLineFromRadar(django.forms.Form):
    """LookerFieldLineFromRadar is the form for the field line from radar page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerFieldLineFromRadar, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB)
        dict1 = {'isGlobal': True, 'categories': "0"}
        
        self.fields['looker_options'] = django.forms.CharField(initial="5", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "5"}))
        
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={'size':'3'}),
                                                              choices=getInstrumentList([], dict1, madInstData, 'Set location manually', local=False),
                                                              required=False, label='Instrument: ')
        
        self.fields['inst_lat'] = django.forms.FloatField(initial=0.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_lon'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_az'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_el'] = django.forms.FloatField(initial=45.0, min_value=0.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_range'] = django.forms.FloatField(initial=1000.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=1000.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        now = datetime.datetime.utcnow()
        self.fields['year'] = django.forms.FloatField(initial=float(now.year), min_value=1950.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        


class LookerFieldLineFromGeodetic(django.forms.Form):
    """LookerFieldLineFromGeodetic is the form for the field line from geodetic page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerFieldLineFromGeodetic, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB)
        dict1 = {'isGlobal': True, 'categories': "0"}
        
        self.fields['looker_options'] = django.forms.CharField(initial="6", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "6"}))
        
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={'size':'3'}),
                                                              choices=getInstrumentList([], dict1, madInstData, 'Set location manually', local=False),
                                                              required=False, label='Instrument: ')
        
        self.fields['inst_lat'] = django.forms.FloatField(initial=0.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_lon'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_lat'] = django.forms.FloatField(initial=45.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_lon'] = django.forms.FloatField(initial=-90.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=1000.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        now = datetime.datetime.utcnow()
        self.fields['year'] = django.forms.FloatField(initial=float(now.year), min_value=1950.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        
class LookerFieldLineFromApex(django.forms.Form):
    """LookerFieldLineFromApex is the form for the field line from apex coordinates page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerFieldLineFromApex, self).__init__(*args, **kwargs)
        madDB = madrigal.metadata.MadrigalDB()
        madInstData = madrigal.metadata.MadrigalInstrumentData(madDB)
        dict1 = {'isGlobal': True, 'categories': "0"}
        
        self.fields['looker_options'] = django.forms.CharField(initial="7", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "7"}))
        
        self.fields['instruments'] = django.forms.ChoiceField(widget = django.forms.Select(attrs={'size':'3'}),
                                                              choices=getInstrumentList([], dict1, madInstData, 'Set location manually', local=False),
                                                              required=False, label='Instrument: ')
        
        self.fields['inst_lat'] = django.forms.FloatField(initial=0.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_lon'] = django.forms.FloatField(initial=0.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['inst_alt'] = django.forms.FloatField(initial=0.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_apex_lat'] = django.forms.FloatField(initial=65.0, min_value=-00.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['fl_apex_lon'] = django.forms.FloatField(initial=-90.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=1000.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        now = datetime.datetime.utcnow()
        self.fields['year'] = django.forms.FloatField(initial=float(now.year), min_value=1950.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        
class LookerConjugateFromGeodetic(django.forms.Form):
    """LookerConjugateFromGeodetic is the form for the geomagnetic/conjugate from geodetic page
    """
    def __init__(self, *args, **kwargs):
        """
        """
        super(LookerConjugateFromGeodetic, self).__init__(*args, **kwargs)
        
        parmChoices = [("MAGCONJLAT", 'Magnetic conjugate latitude'),
                       ("MAGCONJLON", 'Magnetic conjugate longitude'),
                       ("SZEN", 'Solar zenith angle'),
                       ("SZENC", 'Magnetic conjugate solar zenith angle'),
                       ("SDWHT", 'Shadow height (km)'),
                       ("MAGCONJSDWHT", 'Magnetic conjugate shadow height (km)')]
        parmInitial = [key for key, value in parmChoices]
        
        self.fields['looker_options'] = django.forms.CharField(initial="8", 
                                                           widget=django.forms.HiddenInput(attrs={'value': "8"}))
        
        self.fields['start_lat'] = django.forms.FloatField(initial=30.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lat'] = django.forms.FloatField(initial=50.0, min_value=-90.0, max_value=90.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lat'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_lon'] = django.forms.FloatField(initial=-100.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_lon'] = django.forms.FloatField(initial=-80.0, min_value=-180.0, max_value=180.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_lon'] = django.forms.FloatField(initial=10.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['start_alt'] = django.forms.FloatField(initial=100.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['stop_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        self.fields['step_alt'] = django.forms.FloatField(initial=500.0, min_value=0.0,
                                                               widget=django.forms.TextInput(attrs={'size':5}))
        
        # time selection
        now = datetime.datetime.utcnow()
        self.fields['datetime'] = django.forms.SplitDateTimeField(input_date_formats=['%Y-%m-%d'], input_time_formats=['%H:%M:%S'],
                                                                  label='Select UT datetime',
                                                                  help_text='Select the UT time at which to run this calcuation',
                                                                  initial=datetime.datetime(now.year,1,1))
        
        self.fields['pList'] = django.forms.MultipleChoiceField(widget=django.forms.CheckboxSelectMultiple(),
                                                                choices=parmChoices, initial=parmInitial)
        
