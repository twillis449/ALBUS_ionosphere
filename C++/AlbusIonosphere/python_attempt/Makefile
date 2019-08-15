#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2006 Jul 05  JMA  --JIVE  more changes for better installation stuff
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build


SRCS = \
	AlbusIonosphere.cxx

OBJS = \

PROG = \

LIBS = \
	AlbusIonosphere





all: $(LIBS)
.PHONY : all 

AlbusIonosphere: $(SRCS)
	echo "Nothing to do here, skipping."








# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean install
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG)
	/bin/rm -rf build

distclean: clean
	$(RM) *~


install: $(LIBS) 
	echo $(INSTALLDIR) $(VER_MAJOR) $(VER_MINOR) | $(CURRENT_PYTHON) setup.py build install --prefix=$(INSTALLDIR) --install-platlib=$(INSTALLDIR)/lib --install-purelib=$(INSTALLDIR)/share/python




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



# The following stuff is for automatic generation of dependencies
# Some MAKEDEPEND programs need it.  I tend to use gcc to make
# it's own dependencies file.
# DO NOT DELETE THIS LINE -- make depend depends on it.
