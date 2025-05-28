#!/usr/bin/env python

#
# This is a python DistUtils setup file for the
# Madrigal Python API.
#
# This Python module exists as an API to the madrigal
# database.  Much of its functionality comes from
# the underlying C API layer written by John Holt
# and Bill Rideout.
# 
# To build : 
# 
# 1.  Be sure $MADROOT is set (at MLH, set to /opt/madrigal)
# 2.  python setup.py build
#
# To install : 
# 
# 1.  Be sure $MADROOT is set (at MLH, set to /opt/madrigal)
# 2.  python setup.py install
#
import os, sys, os.path
import configparser
import io
import numpy
from setuptools import setup, Extension


# function to edit madrigal/metadata_original.py, substituting in values
# from madrigal.cfg in order to create metadata.py
# Also modifies __bgReport_original.py to create __bgReport.py
def createMetadataPyFile():

    # get data from madrigal.cfg
    madConfFile = open(os.path.join(os.environ['MADROOT'], 'madrigal.cfg'), 'r')

    # create Parser using standard module ConfigParser
    parser = configparser.ConfigParser()

    # read madrigal.cfg into a StringIO with "[madrigal]\n" section heading prepended
    strMadConfFile = io.StringIO("[madrigal]\n" + madConfFile.read())

    # parse StringIO madrigal.cfg
    parser.read_string(strMadConfFile.getvalue())

    # open metadata_original.py
    metadataOrgFile = open('madrigal/metadata_original.py', 'r')

    # read file into string
    metadataStr = io.StringIO(metadataOrgFile.read()).getvalue()

    # substitute all values - order from longest to shortest for MADSERVER
    metadataStr = metadataStr.replace('MADSERVERROOT', parser.get("madrigal", 'MADSERVERROOT'))
    metadataStr = metadataStr.replace('MADSERVER', parser.get("madrigal", 'MADSERVER'))
    metadataStr = metadataStr.replace('MADROOT', parser.get("madrigal", 'MADROOT'))
    metadataStr = metadataStr.replace('SITEID', parser.get("madrigal", 'SITEID'))
    metadataStr = metadataStr.replace('HTMLSTYLE', parser.get("madrigal", 'HTMLSTYLE'))
    metadataStr = metadataStr.replace('INDEXHEAD', parser.get("madrigal", 'INDEXHEAD'))
    metadataStr = metadataStr.replace('CONTACT', parser.get("madrigal", 'CONTACT'))
    metadataStr = metadataStr.replace('MAILSERVER', parser.get("madrigal", 'MAILSERVER'))

    # substitute MAXGLOBALQUERIES only if it exists, otherwise set to blank
    if parser.has_option("madrigal", 'MAXGLOBALQUERIES'):
        metadataStr = metadataStr.replace('MAXGLOBALQUERIES', parser.get("madrigal", 'MAXGLOBALQUERIES'))
    else:
        metadataStr = metadataStr.replace('MAXGLOBALQUERIES', '2')

    # substitute MAXTEMPREPORTS only if it exists, otherwise set to blank
    if parser.has_option("madrigal", 'MAXTEMPREPORTS'):
        metadataStr = metadataStr.replace('MAXTEMPREPORTS', parser.get("madrigal", 'MAXTEMPREPORTS'))
    else:
        metadataStr = metadataStr.replace('MAXTEMPREPORTS', '2')


    # write new string to metadata.py
    metadataNewFile = open('madrigal/metadata.py', 'w')
    metadataNewFile.write(metadataStr)
    metadataNewFile.close()
    metadataOrgFile.close()

    # close madrigal.cfg
    madConfFile.close()



# call the function above
createMetadataPyFile()

setup(name="madrigal",
        version="3.2.9",
        description="Madrigal Python API - now uses python 3",
        author="Bill Rideout",
        author_email="brideout@mit.edu",
        url="https://cedar.openmadrigal.org",
        license='MIT',
        py_modules=['madrigal.admin', 'madrigal.cedar', 'madrigal.data', 'madrigal.derivation',
                    'madrigal.isprint', 'madrigal.metadata', 'madrigal.openmadrigal',
                    'madrigal.ui.madrigalPlot', 'madrigal.ui.userData', 'madrigal.ui.web'],
        ext_modules=[Extension("madrigal/_Madrec",
                              ["madrigal/_Madrec.c"],
                              include_dirs=[os.environ['MADROOT'] + "/source/madc/include",
                                            os.environ['MADROOT'] + "/source",
                                            numpy.get_include()],
                              library_dirs =[os.environ['MADROOT'] +"/lib"],
                              runtime_library_dirs = [os.environ['MADROOT'] + "/lib"],
                              libraries=["madrec","geo"]
                              ),
                     Extension("madrigal/_derive",
                              ["madrigal/_derive.c"],
                              include_dirs=[os.environ['MADROOT'] + "/source/madc/include",
                                            os.environ['MADROOT'] + "/source",
                                            numpy.get_include()],
                              library_dirs =[os.environ['MADROOT'] +"/lib"],
                              runtime_library_dirs = [os.environ['MADROOT'] + "/lib"],
                              libraries=["madrec","geo"] 
                              )
                    ]

           )
