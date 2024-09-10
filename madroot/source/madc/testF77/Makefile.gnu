# $Id: Makefile.gnu 3304 2011-01-17 15:25:59Z brideout $
#
# ----------------------------------------------------------------------
#       Gnu makefile for test of Madrigal Fortran API (Distribution version)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#       Site-dependent definitions - change these as needed
# ----------------------------------------------------------------------

# Madrigal root directory
MADROOT =  /home/grail/brideout/madR/madroot

# Solaris Make program
MAKE = /opt/local/bin/make

# GNU Fortran compiler
FC = g77

# GNU C compiler
CC = gcc

# Library directory
LIBDIR = $(MADROOT)/lib/gnu

# Binary directory
BINDIR = $(MADROOT)/bin/gnu

# ----------------------------------------------------------------------
#       End of site-dependent definitions
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#       Print Command
# ----------------------------------------------------------------------

PRINT = lp

# ----------------------------------------------------------------------
#       Library directives
# ----------------------------------------------------------------------

LDLIBS = -L$(LIBDIR) -lmadrec -lm -lnsl -lgeo

# ----------------------------------------------------------------------
#       Fortran compiler options
# ----------------------------------------------------------------------

FFLAGS = -O -fPIC -Wall -Wuninitialized

# ----------------------------------------------------------------------
#       Link editor options 
# ----------------------------------------------------------------------

LDFLAGS = -Xlinker -R$(LIBDIR)


# ----------------------------------------------------------------------
#       Libraries and Programs
# ----------------------------------------------------------------------

all: clean tmaddataF77 tmadrecF77 tmaddata_nofile_F77


tmaddataF77: tmaddataF77.o
	$(LINK.f) tmaddataF77.o $(LDLIBS) -o $@

tmadrecF77: tmadrecF77.o
	$(LINK.f) tmadrecF77.o $(LDLIBS) -o $@
	
tmadrec_nofile_F77: tmadrec_nofile_F77.o
	$(LINK.f) tmadrec_nofile_F77.o $(LDLIBS) -o $@

clean cleanup:
	rm -f tmaddataF77 tmadrecF77 tmaddata_nofile_F77  core *.o *.BAK *.CKP *%

