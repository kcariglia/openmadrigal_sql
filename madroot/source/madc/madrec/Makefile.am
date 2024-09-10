# $Id: Makefile.am 7713 2024-09-09 16:09:23Z brideout $
#
# Makefile.am for madrec library and test programs
#

AM_CFLAGS = -I../include

lib_LTLIBRARIES = libmadrec.la


# ----------------------------------------------------------------------
#       Madrec library source files
# ----------------------------------------------------------------------

libmadrec_la_SOURCES = madrec.c cedar.c cedarIO.c date.c geometry.c \
          maddata.c madDeriveEngine.c madDeriveMethods.c solpos.c \
          madrecF77.c maddataF77.c 
	  


# test programs only
check_PROGRAMS = testMadrec 
testMadrec_LDADD = -lmadrec -L../../madf/geolib -lgeo $(FLIBS)

