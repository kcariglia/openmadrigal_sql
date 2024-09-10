#!/usr/bin/env python
import os
import sys

"""as created by django-admin-2.7.py startproject djangoMad

$Id: manage.py 4678 2015-01-19 16:29:43Z brideout $
"""

if __name__ == "__main__":
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "djangoMad.settings")

    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)
