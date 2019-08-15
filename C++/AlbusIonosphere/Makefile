#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places

SHELL = /bin/csh -f

# Version numbering for the libjmavex library
VER_MAJOR = 1
VER_MINOR = 0
VER_SUBSUB = 0

LIBRARY_NAME = Albus_Ionosphere


# do not use debugging by default
DEBUG=0
# do make a shared library by default
SHARED = 1
EXT = so



# define the compiler
CC = gcc
F77 = g77
C++ = g++

CPP = cpp -P

#MAKEDEPENDREAL = .dependencies.mk
#MAKEDEPENDTEMP = .dependencies.mk.temp
#MAKEDEPEND = gcc -MM -MF $(MAKEDEPENDTEMP)




INCLUDES = -I. -I$(INSTALLDIR)/include 
DEFINES = 

LIBRARIES = -L$(INSTALLDIR)/lib -lmim -lpim -liri -ljmavex -lvexplus -lvex -lsofa -lfl -lg2c -lm








ifeq ($(OSTYPE),linux)
  ifeq ($(DEBUG),0)
    CFLAGS = -march=i686 -O2 -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -march=i686 -O2 -Wall $(INCLUDES) $(DEFINES) -fno-second-underscore 
  else
    CFLAGS = -march=i686 -O2 -g -Wall $(INCLUDES) $(DEFINES) 
    FFLAGS = -march=i686 -O2 -g -Wall $(INCLUDES) $(DEFINES) -fno-second-underscore 
  endif
endif
ifeq ($(OSTYPE),Solaris)
  F77 = f77
  MAKE = gmake
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







# set up a postfix to use for file and directory names
ifeq ($(DEBUG),0)
  POSTFIX=$(OSTYPE)
else
  POSTFIX=$(OSTYPE)_debug
endif







RM = /bin/rm -f
CP = /bin/cp 





HEADERS = 



SRCS = \
	AlbusIonosphere.cxx

OBJS = \
	AlbusIonosphere.o

PROG = \
	AlbusIonosphere


LIBS = \
	lib$(LIBRARY_NAME).$(EXT)


all: $(PROG)








AlbusIonosphere: $(OBJS) 
	$(C++) $(CFLAGS) -o $@ $(OBJS) $(LIBRARIES)

install: AlbusIonosphere
	$(CP) AlbusIonosphere $(INSTALLDIR)/bin







# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG)

distclean: 
	$(RM) $(OBJS) $(PROG)
	$(RM) *~




# make sure we can compile C++ stuff
.SUFFIXES:
.SUFFIXES: .cxx $(SUFFIXES)









# declare our own default compilation rules
.cxx.o:
	$(CC) $(CFLAGS) -c $<

.F.o:
	$(F77) $(FFLAGS) -c $<












# If you are going to use MAKEDEPEND with GCC, uncomment the following
include .dependencies.mk



# The following stuff is for automatic generation of dependencies
# Some MAKEDEPEND programs need it.  I tend to use gcc to make
# it's own dependencies file.
# DO NOT DELETE THIS LINE -- make depend depends on it.
