#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build


HEADERS = \
	$(INSTALLDIR)/include/JMA_code.h \
	$(INSTALLDIR)/include/JMA_math.h \
	$(INSTALLDIR)/include/f2c.h


all: $(HEADERS)


$(INSTALLDIR)/include/JMA_code.h: JMA_code.h
	$(MKDIR) $(INSTALLDIR)/include
	$(CP) JMA_code.h $@

$(INSTALLDIR)/include/JMA_math.h: JMA_math.h
	$(CP) JMA_math.h $@

$(INSTALLDIR)/include/f2c.h: f2c.h
	$(CP) f2c.h $@




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

distclean: clean
	$(RM) *~
	$(foreach i,$(SUBDIRS), $(MAKE) -C $i $@;)


install: $(SUBDIRS)
	$(foreach i,$(SUBDIRS), $(MAKE) -C $i $@;)





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
