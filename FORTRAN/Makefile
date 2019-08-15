#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2006 Jul 05  JMA  --JIVE  more changes for better installation stuff
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build



SUBDIRS = \
	SOFA \
	IRI \
	PIM/PIM_1.7

.PHONY: subdirs $(SUBDIRS)






all: subdirs



subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ 
	$(MAKE) -C $@ install

PIM/PIM_1.7: SOFA






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
