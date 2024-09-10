"""
WSGI config for djangoMad project.

It exposes the WSGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/1.7/howto/deployment/wsgi/

Modified to work when python in a virtual environment
"""

python_home = '/home/madrigal/midasop/anaconda3/envs/cedar'

import sys
import site

# Calculate path to site-packages directory.

python_version = '.'.join(map(str, sys.version_info[:2]))
site_packages = python_home + '/lib/python%s/site-packages' % python_version

# Add the site-packages directory.

site.addsitedir(site_packages)

import os, os.path
import madrigal.metadata

madDB = madrigal.metadata.MadrigalDB()
os.environ['PYTHON_EGG_CACHE'] = os.path.join(madDB.getMadroot(), 'eggs')
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "djangoMad.settings_production")

from django.core.wsgi import get_wsgi_application
application = get_wsgi_application()
