# -*- coding: utf-8 -*-
__author__ = 'sandlbn'

from django.urls import re_path
from .views import CalendarJsonListView, CalendarView

urlpatterns = [
    re_path(
        r'^json/$',
        CalendarJsonListView.as_view(),
        name='calendar_json'
    ),
    re_path(
        r'^$',
        CalendarView.as_view(),
        name='calendar'
    ),
]
