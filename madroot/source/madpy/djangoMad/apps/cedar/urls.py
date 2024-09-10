'''
urls for docs app
 
@author: Bill Rideout
@contact: brideout@haystack.mit.edu

$Id: urls.py 7510 2023-05-15 20:59:00Z brideout $
'''


# django imports
from django.urls import re_path
from . import views

urlpatterns = [ re_path(r'^getLogAdmin.py', 
                views.get_log_admin, name='getLogAdmin'),
               re_path(r'^getLatestMetadataVersion$', 
                views.get_latest_metadata_version, name='getLatestMetadataVersion'),
               re_path(r'^getAllMetadataVersions$', 
                views.get_all_metadata_versions, name='getAllMetadataVersions'),
               re_path(r'^getMetadataVersion$', 
                views.get_metadata_version, name='getMetadataVersion'),
               re_path(r'^getOpenMadrigalSharedFiles$', 
                views.get_open_madrigal_shared_files, name='getOpenMadrigalSharedFiles'),
               re_path(r'^getMadrigalVideos/?$', 
                views.get_madrigal_videos, name='getMadrigalVideos'),
               re_path(r'^expNotes.py/?$', 
                views.get_exp_notes, name='get_exp_notes'),
               re_path(r'^compareToArchive.py$', 
                views.compare_to_archive, name='compare_to_archive'),
               re_path(r'^openmadrigal/?$', 
                views.open_madrigal, name='open_madrigal'),
               re_path(r'^madrigalAdmin/?$', 
                views.madrigal_admin, name='madrigal_admin'),
               re_path(r'^madrigalDownload/?$', 
                views.madrigal_download, name='madrigal_download'),
               re_path(r'^getCitationGroup$', 
                views.get_citation_group_service, name='get_citation_group_service'),
               re_path(r'^createCitationGroupWithList$', 
                views.create_citation_group_with_list, name='create_citation_group_with_list'),
               re_path(r'^getCitationGroupWithFilters$', 
                views.get_citation_group_with_filters, name='get_citation_group_with_filters'),
            ]