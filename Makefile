#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2006 Jul 05  JMA  --JIVE  more changes for better installation stuff
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build
# 2008 Feb 18  JMA  --MPIfR updates for generalizing build
# 2013 May     AGW  -- improved generalized building a whole lot!

# customize the following four lines to your configuration
# note that we make no use of the real parseltongue - its just a place
# holder for python - this will be cleaned up 
export SHELL=/bin/bash
export INSTALLDIR = /home/bhugo/Documents/ALBUS


# ancient legacy stuff
export CURRENT_PYTHON = python3.8

# modify the following line to point to where Python include files are kept
export PYTHONINCLUDEDIR = /usr/include/python3.8

# Version numbering for the libiri library
export VER_MAJOR = 1
export VER_MINOR = 0
export VER_SUBSUB = 0

# use debugging by default? - under linux Ubuntu 10.04 there seems to be some 
# problems with gfortran and multiprocessing. Under ubuntu 12.04 things seem
# to work OK

export DEBUG=0

# do make a shared library by default
export SHARED = 1

# define the compiler
export CC = gcc
export F77 = gfortran -std=legacy 
export F77_COMPILER_LIB = m 
export F77_COMPILER_LIB = gfortran
export F77_RECL_UNIT = bytes
export HAVE_G2C_H_FILE=0
export C++ = g++

export CPP = cpp -P

export INCLUDES = -I. -I$(INSTALLDIR)/include -I./include -I/home/bhugo/Documents/ALBUS_ionosphere/include
export DEFINES = -DINSTALLDIR=\"$(INSTALLDIR)\"
ifeq ($(HAVE_G2C_H_FILE),1)
  DEFINES += -DHAVE_G2C_H_FILE=$(HAVE_G2C_H_FILE)
endif

ifeq ($(OSTYPE),linux)
  ifeq ($(DEBUG),0)
    CFLAGS = -O2 -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -O2 -Wall $(INCLUDES) $(DEFINES)

  else
    CFLAGS = -g -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -g -Wall $(INCLUDES) $(DEFINES)

  endif
endif
ifeq ($(OSTYPE),Solaris)
  export F77 = f77
  export MAKE = gmake
  ifeq ($(DEBUG),0)
    CFLAGS = -pipe -O2 -msupersparc -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -O $(INCLUDES) $(DEFINES) 
  else
    CFLAGS = -g -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -g $(INCLUDES) $(DEFINES) 
  endif
endif

ifeq ($(SHARED),0)
	EXT = a
else
	CFLAGS += -fPIC
	FFLAGS += -fPIC
	EXT = so
endif

export CFLAGS
export FFLAGS
export EXT




#export MAKEDEPENDREAL = .dependencies.mk
#export MAKEDEPENDTEMP = .dependencies.mk.temp
#export MAKEDEPEND = gcc -MM -MF $(MAKEDEPENDTEMP)








# set up a postfix to use for file and directory names
ifeq ($(DEBUG),0)
  POSTFIX=$(OSTYPE)
else
  POSTFIX=$(OSTYPE)_debug
endif
export POSTFIX






export RM = /bin/rm -f
export RMDIR = /bin/rm -rf
export CP = /bin/cp 
export MKDIR = /bin/mkdir -p
export MV = /bin/mv


SUBDIRS = \
	C++ \
	FORTRAN \
	Python \
	include

.PHONY: subdirs $(SUBDIRS)






all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ 

C: include

C++: C FORTRAN include

Python: C C++ FORTRAN include






# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean install
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)
	$(foreach i,$(SUBDIRS), $(MAKE) -C $i $@;)


clean:
	$(RM) $(OBJS) $(PROG)
	$(foreach i,$(SUBDIRS), $(MAKE) -C $i $@;)
#note: first delete old directory and make a new one
	$(RMDIR)  $(INSTALLDIR)
	$(MKDIR)  $(INSTALLDIR)


distclean: clean
	$(RM) *~
	$(foreach i,$(SUBDIRS), $(MAKE) -C $i $@;)


install: $(SUBDIRS)
	$(foreach i,$(SUBDIRS), $(MAKE) -C $i $@;)
	chmod -R ugo+rX $(INSTALLDIR)/bin
	chmod -R ugo+rX $(INSTALLDIR)/include
	chmod -R ugo+rX $(INSTALLDIR)/lib
	chmod -R ugo+rX $(INSTALLDIR)/libdata
	$(MKDIR) $(INSTALLDIR)/man
	chmod -R ugo+rX $(INSTALLDIR)/man
	chmod -R ugo+rX $(INSTALLDIR)/share





# make sure we can compile C++ stuff
.SUFFIXES:
.SUFFIXES: .cxx $(SUFFIXES)









# declare our own default compilation rules
.cxx.o:
	$(CC) $(CFLAGS) -c $<

.F.o:
	$(F77) $(FFLAGS) -c $<

.f.o:
	$(F77) $(FFLAGS) -c $<



# If you are going to use MAKEDEPEND with GCC, uncomment the following
#include .dependencies.mk

