#!PYTHONEXE

"""registrationAdmin.py is an script for Madrigal administrators to register
or unregister users from receiving emails for experiments or instruments.

$Id: registrationAdmin.py 7046 2019-10-07 19:57:14Z brideout $
"""

usage = """registrationAdmin.py [--unregister | --register]
[--inst=<kinst> | --exp=<expDir>] userEmail

Either unregister or register must be set.
Either --inst=<instrument code> or --exp=<expDir> must be set.
expDir is in form <experiments/2010/mlh/18jan10>
"""

import os, os.path, sys
import getopt

import madrigal.ui.userData


# parse command line
unregister = None
register = None
inst = None
expDir = None

try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["unregister", "register",
                                                  "inst=", "exp="])
except getopt.GetoptError as err:
    print(str(err)) # will print something like "option -a not recognized"
    print(usage)
    sys.exit(2)
for o, a in opts:
    if o == "--unregister":
        unregister = True
        if register != None:
            raise ValueError("cannot set both register and unregister")
    elif o == "--register":
        register = True
        if unregister != None:
            raise ValueError("cannot set both register and unregister")
    elif o == "--inst":
        inst = int(a)
        if expDir != None:
            raise ValueError("cannot set both inst and exp")
    elif o == "--exp":
        expDir = a
        if inst != None:
            raise ValueError("cannot set both inst and exp")
    else:
        assert False, "unhandled option"
if len(args) != 1:
    print(usage)
    print('userEmail argument required')
    sys.exit(-1)
email = args[0]
if email.find('@') == -1:
    print(usage)
    print(('userEmail argument <%s> seems invalid' % (email)))
    sys.exit(-1)
        
if register == None and unregister == None:
    raise ValueError("must set either register or unregister")

if inst == None and expDir == None:
    raise ValueError("must set either inst or exp")

madUserObj = madrigal.ui.userData.MadrigalUserData()

if register:
    if inst:
        madUserObj.registerInstrument(email, inst)
    elif expDir:
        madUserObj.registerExperiment(email, expDir)
elif unregister:
    if inst:
        madUserObj.unregisterInstrument(email, inst)
    elif expDir:
        madUserObj.unregisterExperiment(email, expDir)
