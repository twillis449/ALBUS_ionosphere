#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2005 Sep 20  JMA  --JIVE set up for vexplus stuff
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build

INCLUDES += -I../vex2005 -I$(INSTALLDIR)/include
CFLAGS += -I../vex2005 -I$(INSTALLDIR)/include
HEADERS = \
	vexplus.h

SRCS = \
	vexplus.cxx 

OBJS = \
	vexplus.o

PROG = \

LIBS = \
	libvexplus.$(EXT)



all: $(PROG) $(LIBS)
.PHONY : all



libvexplus.a: $(OBJS)
	$(RM) $@
	ar rcs $@ $(OBJS)

libvexplus.so: $(OBJS)
	$(RM) $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR) $@ 
	gcc -shared -Wl,-soname,$@.$(VER_MAJOR) -o $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $(OBJS)
	ln -s $@.$(VER_MAJOR).$(VER_MINOR).$(VER_SUBSUB) $@.$(VER_MAJOR)
	ln -s $@.$(VER_MAJOR) $@ 


install: libvexplus.so
	$(CP) libvexplus.* $(INSTALLDIR)/lib
	$(CP) $(HEADERS) $(INSTALLDIR)/include





# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
.PHONY : depend clean distclean install
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) $(OBJS) $(PROG) libvexplus.*

distclean: clean
	$(RM) *~




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
