#!PYTHONEXE

"""isTrusted.py returns 1 if user trusted, 0 if not.

Simply calls madrigal.ui.web.MadrigalWeb.isTrusted.  Need only to be called from
tcl cgi scripts.

$Id: isTrusted.py 7046 2019-10-07 19:57:14Z brideout $
"""

import os, os.path, sys

import madrigal.ui.web

webObj = madrigal.ui.web.MadrigalWeb()
print(webObj.isTrusted())
