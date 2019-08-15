#
# JMA_code.rules
#
# Example Makefile for JMA coding
# 2004 Feb 13  James M Anderson  --NRAO  reorganize from various places
# 2008 Feb 15  JMA  --MPIfR updates for generalizing build







SRCS = \
	environ.f \
	lme.f \
	lmetounf.f \
	strnglib.f \

OBJS = \
	environ.o \
	lme.o \
	lmetounf.o \
	strnglib.o \

PROG = \
	lmetounf



all: $(PROG)


lmetounf: $(OBJS)
	$(F77) $(FFLAGS) -o $@ $(OBJS)








# This will probably work best with C and C++ code.  Modify
# If you want to do this for FORTRAN too.
depend:
	$(RM) $(MAKEDEPENDREAL)
	$(foreach i,$(SRCS), $(MAKEDEPEND) $(CFLAGS) $i;cat $(MAKEDEPENDTEMP) >> $(MAKEDEPENDREAL);)
	$(RM) $(MAKEDEPENDTEMP)


clean:
	$(RM) *.o lmetounf

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

.f.o:
	$(F77) $(FFLAGS) -c $<












# If you are going to use MAKEDEPEND with GCC, uncomment the following
#include .dependencies.mk



# The following stuff is for automatic generation of dependencies
# Some MAKEDEPEND programs need it.  I tend to use gcc to make
# it's own dependencies file.
# DO NOT DELETE THIS LINE -- make depend depends on it.
