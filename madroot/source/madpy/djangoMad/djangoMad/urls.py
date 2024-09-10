from django.urls import include, re_path
from django.contrib import admin
import madweb.views

import madrigal.metadata

urlpatterns = [
    re_path(r'^', include('madweb.urls')),
    re_path(r'^$', madweb.views.index),
]

madDB = madrigal.metadata.MadrigalDB()
madserverroot = madDB.getRelativeTopLevel()

if len(madserverroot) > 0 and madserverroot != '.':
    urlpatterns = [re_path(r'^%s/' % (madserverroot), include(urlpatterns))]
