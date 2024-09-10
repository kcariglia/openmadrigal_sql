#!/usr/bin/env python
import os
import sys

"""as created by django-admin-2.7.py startproject djangoMad

$Id: manage_cedar.py 5769 2016-08-15 19:58:13Z brideout $
"""

if __name__ == "__main__":
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "djangoMad.settings_cedar")

    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)
